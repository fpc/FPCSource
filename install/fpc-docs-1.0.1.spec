Name: fpc-docs
Version: 1.0.1
Release: 1
ExclusiveArch: i386
Copyright: GPL
Group: Development/Languages
Source: fpc-docs-1.0.1-src.tar.gz
Summary: Free Pascal Compiler Documentation
Packager: Peter Vreman (peter@freepascal.org)
URL: http://www.freepascal.org/

%define fpcversion 1.0.1
%define fpcdir /usr/lib/fpc/%{fpcversion}
%define docdir /usr/doc/fpc-%{fpcversion}

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
	make -C docs pdfinstall DOCINSTALLDIR=%{docdir}

%clean
	make -C docs clean

%files
%{docdir}/*.pdf

