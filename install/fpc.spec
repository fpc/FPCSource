Name: fpc
Version: 1.0.6
Release: 1
ExclusiveArch: i386 i586 i686
Copyright: GPL
Group: Development/Languages
Source: fpc-1.0.6-src.tar.gz
Summary: Free Pascal Compiler
Packager: Peter Vreman (peter@freepascal.org)
URL: http://www.freepascal.org/
BuildRoot: /tmp/fpc-build
BuildRequires: fpc

%define fpcversion 1.0.6
%define fpcdir /usr/lib/fpc/%{fpcversion}
%define docdir /usr/doc/fpc-%{fpcversion}

%define builddocdir %{buildroot}%{docdir}

%description	
The Free Pascal Compiler is a Turbo Pascal 7.0 and Delphi compatible 32bit
Pascal Compiler. It comes with fully TP 7.0 compatible run-time library.
Some extensions are added to the language, like function overloading. Shared
libraries can be linked. Basic Delphi support is already implemented (classes,
exceptions,ansistrings,RTTI). This package contains commandline compiler and
utils. Provided units are the runtime library (RTL), free component library
(FCL), gtk,ncurses,zlib, mysql,postgres,ibase bindings.

%prep
%setup -c

%build
NEWPP=`pwd`/compiler/ppc386
	make compiler_cycle FPC_VERSION=`ppc386 -iV`
	make rtl_clean rtl_smart FPC=${NEWPP}
	make packages_smart FPC=${NEWPP}
	make fcl_smart FPC=${NEWPP}
	make utils_all FPC=${NEWPP}

%install
	rm -rf %{buildroot}
	
NEWPP=`pwd`/compiler/ppc386
INSTALLOPTS="FPC=${NEWPP} INSTALL_PREFIX=%{buildroot}/usr INSTALL_DOCDIR=%{builddocdir}"
	make compiler_distinstall ${INSTALLOPTS}
	make rtl_distinstall ${INSTALLOPTS}
	make packages_distinstall ${INSTALLOPTS}
	make fcl_distinstall ${INSTALLOPTS}
	make utils_distinstall ${INSTALLOPTS}

	make demo_install ${INSTALLOPTS} INSTALL_SOURCEDIR=%{builddocdir}
	make doc_install ${INSTALLOPTS}
	make man_install ${INSTALLOPTS}
	
%clean
	make compiler_clean
	make rtl_clean
	make packages_clean
	make fcl_clean
	make utils_clean

	rm -rf %{buildroot}
	
%post
FPCDIR=%{fpcdir}

# create link
ln -sf $FPCDIR/ppc386 /usr/bin/ppc386

# Create config
$FPCDIR/samplecfg $FPCDIR

# update ld.so cache
#ldconfig


%files
/usr
