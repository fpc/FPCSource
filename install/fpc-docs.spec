Name: fpc-docs
Version: 1.0.6
Release: 1
Copyright: GPL
Group: Development/Languages
Source: fpc-docs-1.0.6-src.tar.gz
Summary: Free Pascal Compiler Documentation
Packager: Peter Vreman (peter@freepascal.org)
URL: http://www.freepascal.org/
BuildRoot: /tmp/fpc-docs-build

%define fpcversion 1.0.6
%define fpcdir /usr/lib/fpc/%{fpcversion}
%define docdir /usr/doc/fpc-%{fpcversion}

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
	rm -rf %{buildroot}
	make -C docs pdfinstall DOCINSTALLDIR=%{builddocdir}

%clean
	make -C docs clean
	rm -rf %{buildroot}

%files
/usr
