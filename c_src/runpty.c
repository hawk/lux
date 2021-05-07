/*
 * Copyright 2012-2021 Tail-f Systems AB
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
#include <stdarg.h>
#include <fcntl.h>
#include <signal.h>
#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/select.h>
#include <sys/wait.h>
#include <sys/ioctl.h>
#include <errno.h>

#include <termios.h>

#ifdef __sun__
#include <stropts.h>
#define NEED_STREAMS
#endif
#ifdef USE_OPENPTY
#include <util.h>
#endif

enum dbglevel_t {
    silent = 1,
    debug  = 2,
    trace  = 3
};

static int dbgfd = -1;
static enum dbglevel_t dbglevel =

#ifdef TRACE
    trace;
#else
#    ifdef DEBUG
         debug;
#    else
         silent;
#    endif
#endif

enum dbgoutmode_t {
    raw,
    meta
};

static enum dbgoutmode_t dbgoutmode = meta;

static void dbg_dump(const char *buf, int len)
{
    if (dbglevel > silent) {
        if (write(dbgfd, buf, len) < 0) {
            perror("dump to log failed");
            exit(1);
        }
        dbgoutmode = raw;
    }
}

static void dbg_printf(const char *fmt, ...)
{
    if (dbglevel > silent) {
        va_list args;

        if (dbgoutmode == raw) dprintf(dbgfd, "\n");
        dprintf(dbgfd, "[runpty] ");
        va_start(args, fmt);
        vdprintf(dbgfd, fmt, args);
        va_end(args);
        dbgoutmode = meta;
    }
}

static void dbg_exit(int status)
{
    if (dbglevel > silent) {
        dbg_printf("EXIT %d\n", status);
        close(dbgfd);
    }
    exit(status);
}

static int quit = 0;
void signal_handler(int sig) {
    dbg_printf("GOT SIGNAL %d\n", sig);
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
    dbg_printf("CLOSE master at line %d\n", __LINE__);
    close(m);
    return NULL;
}
#endif

struct termios prev_state;

static int unset_onlcr(int fd)
{
    struct termios  b;
    unsigned int iflag, lflag, cflag, oflag;

    if (tcgetattr(fd, &prev_state) < 0) return -1;

    iflag = prev_state.c_iflag;
    lflag = prev_state.c_lflag;
    cflag = prev_state.c_cflag;
    oflag = prev_state.c_oflag;

    b = prev_state;

    oflag = oflag & ~ONLCR;

    b.c_iflag = iflag;
    b.c_lflag = lflag;
    b.c_cflag = cflag;
    b.c_oflag = oflag;

    b.c_cc[VMIN] = 1;
    b.c_cc[VTIME] = 0;

    if (tcsetattr(fd, TCSAFLUSH, &b) < 0) return -1;

    return 0;
}


static int prev_fd_flags;
static int prev_outfd_flags;

static void set_nonblocking(int fd, int outfd)
{
    /* configure socket for non-blocking io */
    prev_fd_flags = fcntl(fd, F_GETFL, 0);
    fcntl(fd, F_SETFL, prev_fd_flags | O_NONBLOCK);

    prev_outfd_flags = fcntl(outfd, F_GETFL, 0);
    fcntl(outfd, F_SETFL, prev_outfd_flags | O_NONBLOCK);
}

void restore_blocking(int fd, int outfd)
{
    /* restore blocking io */
    fcntl(fd, F_SETFL, prev_fd_flags);
    fcntl(outfd, F_SETFL, prev_outfd_flags);
}

#define QUIT(reason)                                                    \
    {                                                                   \
        dbg_printf("QUIT at line %d due to '%s' fail: %s\n",            \
                   __LINE__, reason, strerror(errno));                  \
        goto quit;                                                      \
    }

#define EXIT(reason)                                                    \
    {                                                                   \
        dbg_printf("EXIT at line %d\n", __LINE__);                      \
        fprintf(stderr, "\nrunpty error: %s: %s\n", reason, strerror(errno)); \
        dbg_printf("%s: %s\n", reason, strerror(errno));                \
        dbg_exit(1);                                                    \
    }

int main(int argc, char *argv[])
{
    int in = fileno(stdin);
    int out = fileno(stdout);
    long inr = 0;
    long inw = 0;
    long outr = 0;
    long outw = 0;
    fd_set readfdset;
    fd_set writefdset;
    char inbuf[BUFSIZ*4];  /* *4 appears to fix bug on mac os x */
    char outbuf[BUFSIZ*4]; /* *4 appears to fix bug on mac os x */
    int master;
    int slave;
    char *slavepath;
    pid_t child;
    int status;
    long incnt = 0;
    long outcnt = 0;

    char *dbgmode = getenv("LUX_RUNPTY_MODE");
    char *logdir = getenv("LUX_EXTRA_LOGS");
    char *shellname = getenv("LUX_SHELLNAME");
    char *dbgfile = NULL;
    char *logtype = NULL;

    if (dbgmode) {
        if (strcmp(dbgmode, "trace") == 0) {
            dbglevel = trace;
            logtype = ".trace";
        } else if (strcmp(dbgmode, "debug") == 0) {
            dbglevel = debug;
            logtype = ".debug";
        } else if (strcmp(dbgmode, "silent") == 0) {
            dbglevel = silent;
            logtype = "";
        }
    }

    if (dbglevel > silent) {
        if (logdir) {
            mkdir(logdir, 0755);
        } else {
            logdir = getcwd(NULL, 0);
        }
        if (shellname) {
            if (asprintf(&dbgfile, "%s/runpty%s.%s",
                         logdir, logtype, shellname) < 0) {
                fprintf(stderr, "\nrunpty error: "
                        "concatenate shell log filename failed");
                exit(1);
            }
        } else {
            if (asprintf(&dbgfile, "%s/runpty%s", logdir, logtype) < 0) {
                fprintf(stderr, "\nrunpty error: "
                        "concatenate log filename failed");
                exit(1);
            }
        }
        dbgfd = open(dbgfile,
                     O_WRONLY | O_CREAT | O_TRUNC,
                     S_IRUSR | S_IWUSR | S_IRGRP | S_IROTH);
        if (dbgfd < 0 ) {
            perror("open runpty failed");
            exit(1);
        }
    }

    if (argc < 2) {
        EXIT("too few arguments");
    }

    if ((slavepath = openmaster(&master, &slave)) == NULL) {
        EXIT("failed to open pty");
    }

    /* QNX requires euid 0 to get a pty - get rid of any setuid-ness now */
    if (setuid(getuid()))
        ; /* Ignore return value */

    if ((child = fork()) < 0) {
        EXIT("fork failed");
    }

    if (child == 0) {
        /* child */
        close(master);
        if (setsid() < 0) {
            EXIT("child failed to setsid()");
        }
        if (slave < 0 && (slave = open(slavepath, O_RDWR)) < 0) {
            EXIT("open slave pty in child failed");
        }
#ifdef NEED_STREAMS
        if (ioctl(slave, I_FIND, "ptem") == 0 &&
            ioctl(slave, I_PUSH, "ptem") < 0) {
            EXIT("failed to push STREAMS module 'ptem'");
        }
        if (ioctl(slave, I_FIND, "ldterm") == 0 &&
            ioctl(slave, I_PUSH, "ldterm") < 0) {
            EXIT("failed to push STREAMS module 'ldterm'");
        }
#endif
#if defined(TIOCSCTTY) && !defined(__QNX__)
        if (ioctl(slave, TIOCSCTTY, NULL) < 0) {
            EXIT("TIOCSCTTY failed in child");
        }
#endif
        dup2(slave, 0);
        dup2(slave, 1);
        dup2(slave, 2);
        if (slave > 2) {
            dbg_printf("CLOSE slave at line %d\n", __LINE__);
            close(slave);
        }
        unset_onlcr(1);

        execvp(argv[1], argv+1);
        EXIT("exec in child failed");
    }

    if (slave >= 0) {
        dbg_printf("CLOSE slave at line %d\n", __LINE__);
        close(slave);
    }

    set_nonblocking(in, master);

    dbg_printf("Wait for SIGINT (%d) or timeout\n", SIGINT);
    signal(SIGINT, signal_handler);
    dbg_printf("Wait for SIGTERM (%d) or timeout\n", SIGTERM);
    signal(SIGTERM, signal_handler);

    for (;;) {
        FD_ZERO(&readfdset);
        FD_ZERO(&writefdset);

        if (inw == inr) {
            dbg_printf("IN(%ld): Waiting for more\n", 0);//incnt);
            FD_SET(in, &readfdset);
        } else {
            dbg_printf("IN(%d): Written %d of %d bytes, waiting for %d more\n",
                       incnt, inw, inr, inr-inw);
            FD_SET(master, &writefdset);
        }
        if (outw == outr) {
            dbg_printf("OUT(%d): Waiting for more\n", outcnt);
            FD_SET(master, &readfdset);
        } else {
            ssize_t left = outr-outw;
            dbg_printf("OUT(%d): Written %d of %d bytes, waiting for %d more\n",
                       outcnt, outw, outr, left);
            FD_SET(out, &writefdset);
        }

        if (select(master+1, &readfdset, &writefdset, NULL, NULL) < 0) {
            if (quit) QUIT("select");
        }

        if (FD_ISSET(in, &readfdset)) {
            incnt++;
            inr = read(in, inbuf, sizeof(inbuf));
            dbg_printf("IN(%d): Read %d (%d) bytes\n",
                       incnt, inr, (int)sizeof(inbuf));
            if (inr < 1) QUIT("read in");
            inw = write(master, inbuf, inr);
            dbg_printf("IN(%d): Wrote %d bytes\n", incnt, inw);
            if (inw < 0 && (errno == EAGAIN || errno == EWOULDBLOCK)) inw = 0;
            if (inw < 0) QUIT("write in");
        }
        if (FD_ISSET(master, &writefdset)) {
            int left = inr-inw;
            ssize_t w = write(master, inbuf+inw, left);
            dbg_printf("IN(%d): Wrote %d of %d bytes\n", incnt, w, left);
            if (w < 1) QUIT("write in");
            inw += w;
            dbg_printf("IN(%d): Written in total %d of %d\n", incnt, inw, inr);
        }
        if (FD_ISSET(master, &readfdset)) {
            outcnt++;
            outr = read(master, outbuf, sizeof(outbuf));
            dbg_printf("OUT(%d): Read %d (%d) bytes\n",
                       outcnt, outr, (int)sizeof(outbuf));
            if (outr < 1) QUIT("read out");
            outw = write(out, outbuf, outr);
            dbg_printf("OUT(%d): Wrote %d bytes\n", outcnt, outw);

            if (dbglevel >= trace) {
                if (outbuf[0] == '\r') dbg_printf("<<<NEWLINE CR FIRST>>>\n");
                dbg_dump(outbuf, outw);
                if (outbuf[outw-1] == '\r')
                    dbg_printf("<<<NEWLINE CR LAST>>>\n");
            }
            if (outw < 0 && (errno == EAGAIN || errno == EWOULDBLOCK)) outw = 0;
            if (outw < 0) QUIT("write out");
        }
        if (FD_ISSET(out, &writefdset)) {
            ssize_t left = outr-outw;
            ssize_t w = write(out, outbuf+outw, left);
            dbg_printf("OUT(%d): Wrote %d of %d bytes\n", outcnt, w, left);
            if (dbglevel >= trace) {
                dbg_dump(outbuf+outw, w);
                if (outbuf[outw+w-1] == '\r')
                    dbg_printf("<<<NEWLINE CR2 LAST>>>\n");
            }
            if (w < 1) QUIT("write out");
            outw += w;
            dbg_printf("OUT(%d): Written in total %d of %d\n",
                       outcnt, outw, outr);
        }
    }

quit:
    dbg_printf("CLOSE master at line %d\n", __LINE__);
    close(master);

    if (waitpid(child, &status, WNOHANG) == 0) {
        /* Child hasn't terminated, give it some time
         * and if it still hasn't quit, kill it */

        struct timeval tv;
        tv.tv_sec = 5; tv.tv_usec = 0;
        signal(SIGCHLD, signal_handler);
        dbg_printf("Wait for SIGCHLD (%d) or timeout\n", SIGCHLD);
        if (select(0, NULL, NULL, NULL, &tv) == 0) {
            dbg_printf("KILL CHILD\n");
            kill(child, SIGKILL);
        }
        waitpid(child, &status, 0);
    }

    restore_blocking(in, master);

    if (WIFEXITED(status)) {
        dbg_printf("Child exited with status %d\n",
                   WEXITSTATUS(status));
        dbg_exit(WEXITSTATUS(status));
    }

    if (WIFSIGNALED(status)) {
        fprintf(stderr, "runpty error: Child terminated by signal %d\n",
                WTERMSIG(status));
        dbg_printf("Child terminated by signal %d\n",
                   WTERMSIG(status));
        dbg_exit(1);
    }
    dbg_exit(1);
}
