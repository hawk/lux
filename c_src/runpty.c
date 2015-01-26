/*
 * Copyright 2012-2015 Tail-f Systems AB
 *
 * See the file "LICENSE" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 * Run a program in the slave end of a pseudo tty. Used to run our
 * interactive tests with.
 */

#ifdef __linux__
#define _GNU_SOURCE
#endif
#ifdef __NetBSD__
#define _NETBSD_SOURCE /* needed for pty funcs */
#endif

#include <stdio.h>
#include <stdlib.h>
#include <fcntl.h>
#include <signal.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/select.h>
#include <sys/wait.h>
#include <sys/ioctl.h>
#ifdef __sun__
#include <stropts.h>
#define NEED_STREAMS
#endif
#ifdef USE_OPENPTY
#include <util.h>
#endif

static int quit = 0;
void sighdlr(int sig) {
    switch (sig) {
    case SIGCHLD:
        break;
    default:
        quit++;
        break;
    }
    return;
}

#ifdef USE_OPENPTY
static char *openmaster(int *master, int *slave)
{
    int m, s;
    static char path[1024];
    if (openpty(&m, &s, path, NULL, NULL) < 0) return NULL;
    *master = m;
    *slave = s;
    return path;
}
#else
static char *openmaster(int *master, int *slave)
{
    int m;
    char *path;
    if ((m = posix_openpt(O_RDWR | O_NOCTTY)) < 0) return NULL;
    if (grantpt(m) < 0) goto fail;
    if (unlockpt(m) < 0) goto fail;
    if ((path = ptsname(m)) == NULL) goto fail;
    *master = m;
    *slave = -1;
    return path;
fail:
    close(m);
    return NULL;
}
#endif


int main(int argc, char *argv[])
{
    int in = fileno(stdin), out = fileno(stdout);
    int master;
    int slave;
    char *slavepath;
    pid_t child;
    int status;

    if (argc < 2) {
        exit(1);
    }

    if ((slavepath = openmaster(&master, &slave)) == NULL) {
        perror("failed to open pty");
        exit(1);
    }

    /* QNX requires euid 0 to get a pty
       - get rid of any setuid-ness now */
    setuid(getuid());


    if ((child = fork()) < 0) {
        perror("fork failed");
        exit(1);
    }

    if (child == 0) {
        /* child */
        close(master);
        if (setsid() < 0) {
            perror("child failed to setsid()");
            exit(1);
        }
        if (slave < 0 && (slave = open(slavepath, O_RDWR)) < 0) {
            perror("open slave pty in child failed");
            exit(1);
        }
#ifdef NEED_STREAMS
        if (ioctl(slave, I_FIND, "ptem") == 0 &&
            ioctl(slave, I_PUSH, "ptem") < 0) {
            perror("failed to push STREAMS module 'ptem'");
            exit(1);
        }
        if (ioctl(slave, I_FIND, "ldterm") == 0 &&
            ioctl(slave, I_PUSH, "ldterm") < 0) {
            perror("failed to push STREAMS module 'ldterm'");
            exit(1);
        }
#endif
#if defined(TIOCSCTTY) && !defined(__QNX__)
        if (ioctl(slave, TIOCSCTTY, NULL) < 0) {
            perror("TIOCSCTTY failed in child");
            exit(1);
        }
#endif
        dup2(slave, 0);
        dup2(slave, 1);
        dup2(slave, 2);
        if (slave > 2) close(slave);
        execvp(argv[1], argv+1);
        perror("exec in child failed");
        exit(1);
    }

    if (slave >= 0) {
        close(slave);
    }
    signal(SIGINT, sighdlr);
    signal(SIGTERM, sighdlr);

    for (;;) {
        fd_set fdset;
        char buf[BUFSIZ*4]; /* *4 appears to fix bug on mac os x */

        FD_ZERO(&fdset);
        FD_SET(in, &fdset);
        FD_SET(master, &fdset);

        if (select(master+1, &fdset, NULL, NULL, NULL) < 0) {
            if (quit) goto quit;
        }

        if (FD_ISSET(in, &fdset)) {
            int r = read(in, buf, sizeof(buf));
            if (r < 1) goto quit;
            if (write(master, buf, r) != r) goto quit;
        }

        if (FD_ISSET(master, &fdset)) {
            int r = read(master, buf, sizeof(buf));
            if (r < 1) goto quit;
            if (write(out, buf, r) != r) goto quit;
        }
    }

quit:
    close(master);

    if (waitpid(child, &status, WNOHANG) == 0) {
        /* Child hasn't terminated, give it some time and if it still
         * hasn't quit, kill it*/
        struct timeval tv;
        tv.tv_sec = 5; tv.tv_usec = 0;
        signal(SIGCHLD, sighdlr);
        /* Wait for SIGCHLD or timeout */
        if (select(0, NULL, NULL, NULL, &tv) == 0) {
            kill(child, SIGKILL);
        }
        waitpid(child, &status, 0);
    }

    if (WIFEXITED(status))
        exit(WEXITSTATUS(status));

    if (WIFSIGNALED(status))
        fprintf(stderr, "Child terminated by signal %d\n", WTERMSIG(status));

    exit(1);
}
