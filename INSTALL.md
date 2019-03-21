Installation
============

On MacOS, Lux can be installed with `brew install hawk/homebrew-hawk/lux`.
It will install Erlang and whatewer else Lux needs.

TLDR;

Prerequisites
-------------

The following software is required:

* The tool **Lux** is implemented with **[Erlang/OTP][]** and its
  runtime system must be installed in order to build the tool. Install
  `Erlang/OTP` from [source][Erlang/OTP] or use [pre-built packages][]:

>     sudo apt-get install erlang

or

>     brew install erlang

* On BSD based systems, GNU Make is required.

* The documentation is pre-built. Re-generation of the documentation
  requires **[Markdown][]**.

Instructions
------------

On systems lacking `brew`, Lux is `downloaded` from GitHub with

>     git clone git@github.com:hawk/lux.git
>     cd lux

The `configure file is generated with

>     autoconf

By default lux is installed as `standalone` with a bundled `Erlang`
runtime system meaning that it does not rely on a separately installed
Erlang. The standalone installation is done with

>     ./configure

In most cases it does probably more sense to use the already installed
erlang runtime system and `NOT bundle it with Lux`. This allows test
programs to use Erlang applications which not is self-contained/standalone.

>     ./configure --disable-standalone

Once the system is configures it needs to be built and (possibly) installed.

>     make
>     make install

This will imply that **Lux** will be installed on `/usr/local/lux` and
that custom architecture configuration will be read from
`/usr/local/lux/lib/lux-$(VSN)/priv`.

Install on specific directory `/foo/bar` with

>     ./configure --disable-standalone
>     make
>     DESTDIR=/foo/bar make install

alternatively

>     ./configure --disable-standalone --prefix=/foo/bar
>     make
>     make install

Install on directory `/foo/bar` and read custom architecture
configuration from `/etc/lux` with

>     ./configure --disable-standalone
>     make
>     DESTDIR=/foo/bar ETCDIR=/etc/lux make install

alternatively

>     ./configure --disable-standalone --prefix=/foo/bar --sysconfdir=/etc/lux
>     make
>     make install

Obscure platforms
-----------------

On "obscure platforms" which have `Erlang/OTP` but lacks `autotools`,
make etc. it is still possible to build with

>     bin/lux --make

and install with

>     bin/lux --install DestDir

The given InstallDir will contain the lux tool as well as a stripped
Erlang runtime system. It is possible to move the entire standalone
system from InstallDir to another location without any
re-installation.

The standalone tool can be started with

>     DestDir/bin/lux

Re-build the documentation
--------------------------

Simply do

>     cd doc
>     make
>     open ../lux.html
