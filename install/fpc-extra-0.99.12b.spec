Name: fpc-extra
Version: 0.99.12b
Release: 1
ExclusiveArch: i386
Copyright: GPL
Group: Development/Languages
Source: fpc-extra-0.99.12b-src.tar.gz
Requires: fpc = 0.99.12b
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

%define rtldir /usr/lib/fpc/0.99.12/rtl/linux
%define unitdir /usr/lib/fpc/0.99.12/units/linux

%prep
%setup -c

%build
# Don't load the system makefile.fpc
export FPCMAKE=
export FPCDIR=
NEWPP=/usr/bin/ppc386
	make -C packages all PP=${NEWPP} RELEASE=1 RTLDIR=%{rtldir}

%install
# Don't load the system makefile.fpc
export FPCMAKE=
export FPCDIR=
NEWPP=/usr/bin/ppc386
	make -C packages install PP=${NEWPP} RELEASE=1 RTLDIR=%{rtldir}

%clean
	make -C packages clean

%files
%{unitdir}/ibase.ppu
%{unitdir}/ibase.o
%{unitdir}/mysql_com.ppu
%{unitdir}/mysql_version.ppu
%{unitdir}/mysql.ppu
%{unitdir}/mysql_com.o
%{unitdir}/mysql_version.o
%{unitdir}/mysql.o
%{unitdir}/uncgi.ppu
%{unitdir}/uncgi.o
%{unitdir}/utmp.ppu
%{unitdir}/utmp.o
%{unitdir}/inet.ppu
%{unitdir}/inet.o
%{unitdir}/dllist.ppu
%{unitdir}/postgres.ppu
%{unitdir}/dllist.o
%{unitdir}/postgres.o
%{unitdir}/ncurses.ppu
%{unitdir}/ncurses.o
%{unitdir}/x.ppu
%{unitdir}/xresource.ppu
%{unitdir}/xlib.ppu
%{unitdir}/xutil.ppu
%{unitdir}/forms.ppu
%{unitdir}/xlib.o
%{unitdir}/forms.o
%{unitdir}/svgalib.ppu
%{unitdir}/vgamouse.ppu
%{unitdir}/svgalib.o
