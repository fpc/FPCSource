###############################################################################
# fpc-docs.rpm
#
# Note: This file will be attached to the fpc.spec

%package docs
Group: Development/Languages
Summary: Free Pascal Compiler - Documentation
%description docs
The Free Pascal Compiler is a Turbo Pascal 7.0 and Delphi compatible 32bit
Pascal Compiler. It comes with fully TP 7.0 compatible run-time library.
Some extensions are added to the language, like function overloading. Shared
libraries can be linked. Basic Delphi support is already implemented (classes,
exceptions,ansistrings,RTTI). This package contains commandline compiler and
utils. Provided units are the runtime library (RTL), free component library
(FCL), gtk,ncurses,zlib, mysql,postgres,ibase bindings.
This package contains the documentation in PDF format

%files docs
%defattr(-, root, root)
%doc %{docdir}/*.pdf
