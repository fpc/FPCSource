Name: fpc
Version: %FPCVERSION%
Release: 0
ExclusiveArch: i386 i586 i686 ppc amd64 x86_64
Copyright: GPL
Group: Development/Languages
Source: %{name}-%{version}-src.tar.gz
Summary: Free Pascal Compiler
Packager: Peter Vreman (peter@freepascal.org)
URL: http://www.freepascal.org/
BuildRoot: %{_tmppath}/fpc-build
BuildRequires: fpc

%define fpcdir %{_libdir}/fpc/%{version}
%define docdir %{_docdir}/fpc-%{version}
%define exampledir %{docdir}/examples

%ifarch ppc
%define ppcname ppcppc
%else
%ifarch x86_64
%define ppcname ppcx64
%else
%ifarch amd64
%define ppcname amd64
%else
%define ppcname ppc386
%endif
%endif
%endif

%define builddocdir %{buildroot}%{docdir}
%define buildmandir %{buildroot}%{_mandir}
%define buildbindir %{buildroot}%{_bindir}
%define buildlibdir %{buildroot}%{_libdir}
%define buildexampledir %{buildroot}%{exampledir}

# The normal redhat rpm scripts does not recognize properly, what files to strip
# Hook our own strip command
%define __strip %{_builddir}/%{name}-%{version}/smart_strip.sh


%description
The Free Pascal Compiler is a Turbo Pascal 7.0 and Delphi compatible 32bit
Pascal Compiler. It comes with fully TP 7.0 compatible run-time library.
Some extensions are added to the language, like function overloading. Shared
libraries can be linked. Basic Delphi support is already implemented (classes,
exceptions,ansistrings,RTTI). This package contains commandline compiler and
utils. Provided units are the runtime library (RTL), free component library
(FCL), gtk,ncurses,zlib, mysql,postgres,ibase bindings.

###############################################################################
# fpc.rpm
#

%prep
%setup -c

%build
FPCDIR=
NEWPP=`pwd`/compiler/%{ppcname}
NEWFPDOC=`pwd`/utils/fpdoc/fpdoc
	make compiler_cycle
	make rtl_clean rtl_smart FPC=${NEWPP}
	make packages_base_smart FPC=${NEWPP}
	make fcl_smart FPC=${NEWPP}
	make fv_smart FPC=${NEWPP}
	make packages_extra_smart FPC=${NEWPP}
	make ide_all FPC=${NEWPP}
	make utils_all FPC=${NEWPP}
if [ -z ${NODOCS} ]; then
	make -C docs pdf FPC=${NEWPP} FPDOC=${NEWFPDOC}
fi

%install
if [ %{buildroot} != "/" ]; then
	rm -rf %{buildroot}
fi

FPCDIR=
NEWPP=`pwd`/compiler/%{ppcname}
INSTALLOPTS="FPC=${NEWPP} INSTALL_PREFIX=%{buildroot}/usr INSTALL_LIBDIR=%{buildlibdir} \
		INSTALL_DOCDIR=%{builddocdir} INSTALL_BINDIR=%{buildbindir} \
		INSTALL_EXAMPLEDIR=%{buildexampledir}"
	make compiler_distinstall ${INSTALLOPTS}
	make rtl_distinstall ${INSTALLOPTS}
	make packages_distinstall ${INSTALLOPTS}
	make fcl_distinstall ${INSTALLOPTS}
	make fv_distinstall ${INSTALLOPTS}
	make ide_distinstall ${INSTALLOPTS}
	make utils_distinstall ${INSTALLOPTS}

	make demo_install ${INSTALLOPTS} INSTALL_SOURCEDIR=%{buildexampledir}
	make doc_install ${INSTALLOPTS}
	make man_install ${INSTALLOPTS} INSTALL_MANDIR=%{buildmandir}
	
if [ -z ${NODOCS} ]; then
	make -C docs pdfinstall ${INSTALLOPTS} INSTALL_DOCDIR=%{builddocdir}
fi

	# create link
	ln -sf %{fpcdir}/%{ppcname} %{buildroot}%{_bindir}/%{ppcname}

        # Workaround:
        # newer rpm versions do not allow garbage
        # delete lexyacc
        rm -rf %{buildroot}/usr/lib/fpc/lexyacc


%clean
	make compiler_clean
	make rtl_clean
	make packages_clean
	make fcl_clean
	make fv_clean
	make ide_clean
	make utils_clean
if [ -z ${NODOCS} ]; then
	make -C docs clean
fi

if [ %{buildroot} != "/" ]; then
	rm -rf %{buildroot}
fi

%post
# Create a version independent config
%{fpcdir}/samplecfg %{_libdir}/fpc/\$fpcversion

%files
%defattr(-, root, root)
%{_bindir}/*
%{fpcdir}
%doc %{docdir}/NEWS
%doc %{docdir}/README
%doc %{docdir}/readme.ide
%doc %{docdir}/faq*
%doc %{exampledir}/*
%{_mandir}/*/*
