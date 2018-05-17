Installation
============

Prerequisites
-------------

The following software is required:

* The tool **Lux** is implemented with **[Erlang/OTP][]** and its
  runtime system must be installed in order to build the tool. Install
  `Erlang/OTP` from [source][Erlang/OTP] or use [pre-built packages][]:
>     sudo apt-get install erlang

  Once `Lux` has been installed, it will be self-contained and does
  not need a separate `Erlang/OTP` runtime system any more.

* On BSD based systems, GNU Make is required.

* The documentation is pre-built. Re-generation of the documentation
  requires **[Markdown][]**.

Instructions
------------

If you have cloned the source from `github.com` and want to build the
tool using `configure` and `make` there is no `configure` script. Then
you need to create it with

>     autoconf

Vanilla configure, build and install with

>     ./configure
>     make
>     make install

This will imply that **Lux** will be installed on `/usr/local/lux` and
that custom architecture configuration will be read from
`/usr/local/lux/lib/lux-$(VSN)/priv`.

Install on specific directory `/foo/bar` with

>     ./configure
>     make
>     DESTDIR=/foo/bar make install

alternatively

>     ./configure --prefix=/foo/bar
>     make
>     make install

Install on directory `/foo/bar` and read custom architecture
configuration from `/etc/lux` with

>     ./configure
>     make
>     DESTDIR=/foo/bar ETCDIR=/etc/lux make install

alternatively

>     ./configure --prefix=/foo/bar --sysconfdir=/etc/lux
>     make
>     make install

Obscure platforms
-----------------

On "obscure platforms" which have `Erlang/OTP` but lacks
`autotools`, make etc. it is still possible to build with

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
