Name: fpc-extra
Version: 0.99.12
Release: 1
ExclusiveArch: i386
Copyright: GPL
Group: Development/Languages
Source: fpc-extra-0.99.12-src.tar.gz
Requires: fpc = 0.99.12
Summary: Free Pascal Compiler Extra Packages
Packager: Michael Van Canneyt (michael@tfdec1.fys.kuleuven.ac.be)
URL: http://tfdec1.fys.kuleuven.ac.be/~michael/fpc/fpc.html

%description	
The Free Pascal Compiler is a Turbo Pascal 7.0 and Delphi compatible 32bit
Pascal Compiler. It comes with fully TP 7.0 compatible run-time library.
Some extensions are added to the language, like function overloading. Shared
libraries can be linked and created. Basic Delphi support is already
implemented (classes,exceptions,ansistrings). This package contains some
extra units: ncurses,inet,mysql,ibase,postgres,uncgi

%define unitdir /usr/lib/fpc/0.99.12/rtl

%prep
%setup -c

%build
# Don't load the system makefile.fpc
export FPCMAKE=
export FPCDIR=
NEWPP=/usr/bin/ppc386
	make -C packages all PP=${NEWPP} RELEASE=1 UNITDIR=%{unitdir}

%install
# Don't load the system makefile.fpc
export FPCMAKE=
export FPCDIR=
NEWPP=/usr/bin/ppc386
	make -C packages install PP=${NEWPP}

%clean
	make -C packages clean

%files
/usr/lib/fpc/0.99.12/units/ibase.ppu
/usr/lib/fpc/0.99.12/units/ibase.o
/usr/lib/fpc/0.99.12/units/mysql_com.ppu
/usr/lib/fpc/0.99.12/units/mysql_version.ppu
/usr/lib/fpc/0.99.12/units/mysql.ppu
/usr/lib/fpc/0.99.12/units/mysql_com.o
/usr/lib/fpc/0.99.12/units/mysql_version.o
/usr/lib/fpc/0.99.12/units/mysql.o
/usr/lib/fpc/0.99.12/units/uncgi.ppu
/usr/lib/fpc/0.99.12/units/uncgi.o
/usr/lib/fpc/0.99.12/units/utmp.ppu
/usr/lib/fpc/0.99.12/units/utmp.o
/usr/lib/fpc/0.99.12/units/inet.ppu
/usr/lib/fpc/0.99.12/units/inet.o
/usr/lib/fpc/0.99.12/units/dllist.ppu
/usr/lib/fpc/0.99.12/units/postgres.ppu
/usr/lib/fpc/0.99.12/units/dllist.o
/usr/lib/fpc/0.99.12/units/postgres.o
/usr/lib/fpc/0.99.12/units/ncurses.ppu
/usr/lib/fpc/0.99.12/units/ncurses.o
/usr/lib/fpc/0.99.12/units/x.ppu
/usr/lib/fpc/0.99.12/units/xresource.ppu
/usr/lib/fpc/0.99.12/units/xlib.ppu
/usr/lib/fpc/0.99.12/units/xutil.ppu
/usr/lib/fpc/0.99.12/units/forms.ppu
/usr/lib/fpc/0.99.12/units/xlib.o
/usr/lib/fpc/0.99.12/units/forms.o
/usr/lib/fpc/0.99.12/units/svgalib.ppu
/usr/lib/fpc/0.99.12/units/vgamouse.ppu
/usr/lib/fpc/0.99.12/units/svgalib.o
