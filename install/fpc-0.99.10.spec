Name: fpc
Version: 0.99.10
Release: 1
ExclusiveArch: i386
Copyright: GPL
Group: Development/Languages
Source: fpc-0.99.10-src.tar.gz
Summary: Free Pascal Compiler
Packager: Michael Van Canneyt (michael@tfdec1.fys.kuleuven.ac.be)
URL: http://tfdec1.fys.kuleuven.ac.be/~michael/fpc/fpc.html

%description	
The Free Pascal Compiler is a Turbo Pascal 7.0 and Delphi II compatible
32bit Pascal Compiler. It comes with fully TP 7.0 compatible run-time library.
Some extensions are added to the language, like function overloading. Shared
and static libraries can be linked and created. Linking with C libraries is
easy, so interfaces can be written fast. 

%define package fpc-%{PACKAGE_VERSION}
%define bindir /usr/bin
%define libdir /usr/lib/fpc
%define vlibdir %{libdir}/%{PACKAGE_VERSION}
%define unitdir %{vlibdir}/linuxunits

%prep
%setup -c

%build
make -C compiler cycle RELEASE=1
make -C rtl/utils all RELEASE=1

%install
make -C compiler install BININSTALLDIR=%{bindir} LIBINSTALLDIR=%{vlibdir}
make -C rtl/linux install UNITINSTALLDIR=%{unitdir}
make -C rtl/linux libinstall UNITINSTALLDIR=%{unitdir} PPUMOVE=../utils/ppumove
make -C rtl/utils install BININSTALLDIR=%{bindir} UNITINSTALLDIR=%{unitdir}

%clean
make -C compiler clean
make -C rtl/utils clean
make -C rtl/linux libsclean

%post
%{vlibdir}/samplecfg %{vlibdir} `dirname \`find /usr/lib/gcc-lib/ -name libgcc.a -print | grep -v egcs \``
ldconfig

%files
%{bindir}/ppc386
%{bindir}/ppudump
%{bindir}/ppumove
%{bindir}/h2pas
%{vlibdir}
/usr/lib/libfpc.so
%dir %{libdir}
