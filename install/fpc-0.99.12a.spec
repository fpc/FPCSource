Name: fpc
Version: 0.99.12a
Release: 1
ExclusiveArch: i386
Copyright: GPL
Group: Development/Languages
Source: fpc-0.99.12a-src.tar.gz
Summary: Free Pascal Compiler
Packager: Michael Van Canneyt (michael@tfdec1.fys.kuleuven.ac.be)
URL: http://tfdec1.fys.kuleuven.ac.be/~michael/fpc/fpc.html

%define fpcdir /usr/lib/fpc/0.99.12
%define docdir /usr/doc/fpc-0.99.12

%description	
The Free Pascal Compiler is a Turbo Pascal 7.0 and Delphi compatible 32bit
Pascal Compiler. It comes with fully TP 7.0 compatible run-time library.
Some extensions are added to the language, like function overloading. Shared
libraries can be linked and created. Basic Delphi support is already
implemented (classes,exceptions,ansistrings). This package contains
the commandline compiler and runtime library.

%prep
%setup -c

%build
export FPCMAKE=
export FPCDIR=
NEWPP=`pwd`/compiler/ppc386
	make compiler_cycle
	make utils_all PP=${NEWPP}

%install
export FPCMAKE=
export FPCDIR=
NEWPP=`pwd`/compiler/ppc386
	make compiler_install PP=${NEWPP}
	make rtl_install PP=${NEWPP}
	make utils_install PP=${NEWPP}

	make base_install PP=${NEWPP}
	make demo_install PP=${NEWPP} DOCINSTALLDIR=%{docdir}
	make man_install PP=${NEWPP}
	
%clean
	make compiler_clean
	make rtl_clean
	make utils_clean

%post
FPCDIR=%{fpcdir}

# create link
ln -sf $FPCDIR/ppc386 /usr/bin/ppc386

# Create config
$FPCDIR/samplecfg $FPCDIR

# update ld.so cache
ldconfig

%files
%{fpcdir}/ppc386
%{fpcdir}/samplecfg
%{fpcdir}/msg
%{fpcdir}/rtl
/usr/bin/ppudump
/usr/bin/ppumove
/usr/bin/ppdep
/usr/bin/h2pas
/usr/bin/ptop
/usr/man/man1/ppc386.1
/usr/man/man1/ptop.1
/usr/man/man1/ppudump.1
/usr/man/man1/ppumove.1
/usr/man/man1/ppdep.1
/usr/man/man5/ppc386.cfg.5
/usr/man/man5/ptop.cfg.5
%{docdir}/demo
