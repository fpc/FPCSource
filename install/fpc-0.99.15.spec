Name: fpc
Version: 0.99.15
Release: 1
ExclusiveArch: i386
Copyright: GPL
Group: Development/Languages
Source: fpc-0.99.15-src.tar.gz
Summary: Free Pascal Compiler
Packager: Peter Vreman (peter@freepascal.org)
URL: http://www.freepascal.org/

%define fpcversion 0.99.15
%define fpcdir /usr/lib/fpc/%{fpcversion}
%define docdir /usr/doc/fpc-%{fpcversion}

%description	
The Free Pascal Compiler is a Turbo Pascal 7.0 and Delphi compatible 32bit
Pascal Compiler. It comes with fully TP 7.0 compatible run-time library.
Some extensions are added to the language, like function overloading. Shared
libraries can be linked and created. Basic Delphi support is already
implemented (classes,exceptions,ansistrings).
This package contains commandline compiler and utils. Provided units are
the runtime library (RTL), free component library (FCL), gtk,ncurses,zlib,
mysql,postgres,ibase bindings.

%prep
%setup -c

%build
export FPCDIR=
NEWPP=`pwd`/compiler/ppc386
	make compiler_cycle
	make fcl_all PP=${NEWPP}
	make api_all PP=${NEWPP}
	make packages_all PP=${NEWPP}
	make utils_all PP=${NEWPP}

%install
export FPCDIR=
NEWPP=`pwd`/compiler/ppc386
NEWPPUFILES=`pwd`/utils/ppufiles
	make compiler_install PP=${NEWPP} PPUFILES=${NEWPPUFILES}
	make rtl_install PP=${NEWPP} PPUFILES=${NEWPPUFILES}
	make fcl_install PP=${NEWPP} PPUFILES=${NEWPPUFILES}
	make api_install PP=${NEWPP} PPUFILES=${NEWPPUFILES}
	make packages_install PP=${NEWPP} PPUFILES=${NEWPPUFILES}
	make utils_install PP=${NEWPP} PPUFILES=${NEWPPUFILES}

	make demo_install PP=${NEWPP} DOCINSTALLDIR=%{docdir}
	make doc_install PP=${NEWPP} DOCINSTALLDIR=%{docdir}
	make man_install PP=${NEWPP}
	
%clean
	make compiler_clean
	make rtl_clean
	make fcl_clean
	make api_clean
	make packages_clean
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
/usr/bin/ppufiles 
/usr/bin/ppudump 
/usr/bin/ppumove 
/usr/bin/ppdep 
/usr/bin/ptop 
/usr/bin/rstconv 
/usr/bin/data2inc 
/usr/bin/bin2obj
/usr/bin/delp
/usr/bin/plex 
/usr/bin/pyacc
/usr/bin/h2pas
/usr/bin/fprcp
%{fpcdir}
/usr/lib/fpc/lexyacc/yylex.cod 
/usr/lib/fpc/lexyacc/yyparse.cod
/usr/man/man1/delp.1
/usr/man/man1/ppdep.1
/usr/man/man1/ppumove.1
/usr/man/man1/ppudump.1
/usr/man/man1/ppufiles.1
/usr/man/man1/fpcmake.1
/usr/man/man1/ptop.1
/usr/man/man1/h2pas.1
/usr/man/man1/plex.1
/usr/man/man1/pyacc.1
/usr/man/man1/ppc386.1
/usr/man/man5/ppc386.cfg.5
/usr/man/man5/fpcmake.5
/usr/man/man5/ptop.cfg.5
%{docdir}/examples
%{docdir}/README
%{docdir}/NEWS
%{docdir}/faq.html
%{docdir}/faq.txt
