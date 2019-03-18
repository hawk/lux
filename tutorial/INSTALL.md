Installation of tutorial
========================

Install LUX on Mac
------------------

> brew tap hawk/homebrew-hawk
> brew install lux

Install LUX from source
-----------------------

> git clone -b euc git@github.com:hawk/lux.git
> cd lux
> autoconf
> ./configure
> make
> export "PATH=$PWD/bin:$PATH"

See ../../lux/INSTALL.md for more details about how to install LUX.

Run tutorial demo tests
-----------------------

>     cd tutorial
>     make
