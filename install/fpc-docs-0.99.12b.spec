Name: fpc-docs
Version: 0.99.12b
Release: 1
ExclusiveArch: i386
Copyright: GPL
Group: Development/Languages
Source: fpc-docs-0.99.12b-src.tar.gz
Summary: Free Pascal Compiler Documentation
Packager: Michael Van Canneyt (michael@tfdec1.fys.kuleuven.ac.be)
URL: http://tfdec1.fys.kuleuven.ac.be/~michael/fpc/fpc.html

%description	
The Free Pascal Compiler is a Turbo Pascal 7.0 and Delphi compatible 32bit
Pascal Compiler. It comes with fully TP 7.0 compatible run-time library.
Some extensions are added to the language, like function overloading. Shared
libraries can be linked and created. Basic Delphi support is already
implemented (classes,exceptions,ansistrings). This package contains
the documentation in HTML format

%define docdir /usr/doc/fpc-0.99.12

%prep
%setup -c

%build
	make -C docs pdf

%install
	make -C docs pdfinstall

%clean
	make -C docs clean

%files
%{docdir}/*.pdf

