Installation
============

On MacOS, Lux can be installed with `brew install hawk/homebrew-hawk/lux`.
It will install Erlang and whatewer else Lux needs.

TLDR;

Prerequisites
-------------

The following software is required:

* On BSD based systems, GNU Make is required.

* The tool **Lux** is implemented with **[Erlang/OTP][]** and its
  runtime system must be installed in order to build the tool. Install
  `Erlang/OTP` from [source][Erlang/OTP] or use [pre-built packages][]:

>     brew tap hawk/homebrew-hawk
>     brew install erlang

           or

>     sudo apt-get install erlang

* By installing the `erlang` package most of the Erlang apps needed by
  Lux will be installed automatically. But there are additional Erlang
  packages which may be needed when using more exotic features, such
  as debugging and developing Lux itself:

  - `--internal_debug` requires `debugger`+`wx`
  - `--suite_trace`    requires `runtime_tools`
  - `--event_trace`    requires `runtime_tools`+`et`+`wx`
  - `--reltool`        requires `reltool`

* A standalone installation (using `--install`) requires `reltool`.

* Building Lux using `--make` requires `tools`.

* Testing of Lux itself requires `tools`.

* `--history` require may `inets`. But only when the logs are referred
  to by using URL's. Using local files does not require `inets`.

* The documentation is pre-built. Re-generation of the documentation
  requires **[Markdown][]**.

Instructions
------------

Lux can be `downloaded` from GitHub with

>     git clone git@github.com:hawk/lux.git
>     cd lux

Lux is built with

>     autoconf
>     ./configure
>     make

When this is done you have a system which can run Lux with

>     bin/lux <SOME PARAMS>

But you may also install Lux somewhere by using

>     make install

By default (that is when ./configure has been invoked without
parameters), Lux will be installed under /usr/local. It is effectively
the same as invoking

>     ./configure --prefix=/usr/local --exec_prefix=/usr/local --bindir=/usr/local/bin --sysconfdir=/usr/local/etc

`make install` does also accept various parameters which overrides the
ones given to `./configure`. Such as

>     make prefix=/usr/local exec_prefix=/usr/local bindir=/usr/local/bin sysconfdir=/usr/local/etc install

and those parameters may be combined with

>     make DESTDIR=/my/staging/area install

Standalone installation
-----------------------

When building Lux an Erlang/OTP system must be available.

By default that Erlang/OTP system is also used when running Lux.

But it is possible to perform an `standalone installation` of Lux
where the Lux installation is bundled with Erlang/OTP. This means that
you may in fact uninstall the Erlang/OTP system used for building Lux
and still be able to run Lux as it is self-contained with its own
Erlang/OTP runtime system. A standalone installation is performed with

>     mkdir -p <TARGETDIR>
>     bin/lux --install <TARGETDIR>

The installed standalone system may be re-located if needed.

Obscure platforms
-----------------

On "obscure platforms" which have `Erlang/OTP` but lacks `autotools`,
make etc. it may still possible to build with

>     bin/lux --make

Re-build the documentation
--------------------------

Simply do

>     cd doc
>     make
>     open ../lux.html
