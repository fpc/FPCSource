Name: fpc-docs
Version: 1.1
Release: 0
Copyright: GPL
Group: Development/Languages
Source: %{name}-%{version}-src.tar.gz
Summary: Free Pascal Compiler Documentation
Packager: Peter Vreman (peter@freepascal.org)
URL: http://www.freepascal.org/
BuildRoot: %{_tmppath}/fpc-build

%define fpcdir %{_libdir}/fpc/%{version}
%define docdir %{_docdir}/fpc-%{version}

%define builddocdir %{buildroot}%{docdir}

%description	
The Free Pascal Compiler is a Turbo Pascal 7.0 and Delphi compatible 32bit
Pascal Compiler. It comes with fully TP 7.0 compatible run-time library.
Some extensions are added to the language, like function overloading. Shared
libraries can be linked and created. Basic Delphi support is already
implemented (classes,exceptions,ansistrings).
This package contains the documentation in PDF format

%prep
%setup -c

%build
	make -C docs pdf

%install
if [ %{buildroot} != "/" ]; then
	rm -rf %{buildroot}
fi

	make -C docs pdfinstall DOCINSTALLDIR=%{builddocdir}

%clean
	make -C docs clean
if [ %{buildroot} != "/" ]; then
	rm -rf %{buildroot}
fi

%files
%defattr(-, root, root)
%doc %{docdir}/*
