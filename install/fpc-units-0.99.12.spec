Name: fpc-units
Version: 0.99.12a
Release: 1
ExclusiveArch: i386
Copyright: GPL
Group: Development/Languages
Source: fpc-units-0.99.12a-src.tar.gz
Requires: fpc = 0.99.12a
Summary: Free Pascal Compiler Extra Units
Packager: Michael Van Canneyt (michael@tfdec1.fys.kuleuven.ac.be)
URL: http://tfdec1.fys.kuleuven.ac.be/~michael/fpc/fpc.html

%description	
The Free Pascal Compiler is a Turbo Pascal 7.0 and Delphi compatible 32bit
Pascal Compiler. It comes with fully TP 7.0 compatible run-time library.
Some extensions are added to the language, like function overloading. Shared
libraries can be linked and created. Basic Delphi support is already
implemented (classes,exceptions,ansistrings).
This package contains:
 - FCL (Free Component Library)
 - API/Free Vision
 - GTK 1.2.x interface

%define rtldir /usr/lib/fpc/0.99.12/rtl/linux
%define unitdir /usr/lib/fpc/0.99.12/units/linux

%prep
%setup -c

%build
# Don't load the system makefile.fpc
export FPCMAKE=
export FPCDIR=
NEWPP=/usr/bin/ppc386
	make -C fcl all PP=${NEWPP} RELEASE=1 UNITDIR=%{rtldir}
	make -C gtk all PP=${NEWPP} RELEASE=1 UNITDIR=%{rtldir}
	make -C api all PP=${NEWPP} RELEASE=1 UNITDIR=%{rtldir}
	make -C fv all PP=${NEWPP} RELEASE=1 UNITDIR=%{rtldir}

%install
# Don't load the system makefile.fpc
export FPCMAKE=
export FPCDIR=
NEWPP=/usr/bin/ppc386
	make -C fcl install PP=${NEWPP}
	make -C gtk install PP=${NEWPP}
	make -C api install PP=${NEWPP}
	make -C fv install PP=${NEWPP}

%clean
	make -C fcl clean
	make -C gtk clean
	make -C api clean
	make -C fv clean

%files

%{unitdir}/common.ppu 
%{unitdir}/callspec.ppu 
%{unitdir}/video.ppu 
%{unitdir}/keyboard.ppu 
%{unitdir}/mouse.ppu 
%{unitdir}/filectrl.ppu 
%{unitdir}/filesys.ppu

%{unitdir}/common.o 
%{unitdir}/callspec.o 
%{unitdir}/video.o 
%{unitdir}/keyboard.o 
%{unitdir}/mouse.o 
%{unitdir}/filectrl.o 
%{unitdir}/filesys.o

%{unitdir}/validate.ppu 
%{unitdir}/history.ppu 
%{unitdir}/commands.ppu 
%{unitdir}/drivers.ppu 
%{unitdir}/helpctx.ppu 
%{unitdir}/memory.ppu 
%{unitdir}/objtypes.ppu 
%{unitdir}/views.ppu 
%{unitdir}/resource.ppu 
%{unitdir}/msgbox.ppu 
%{unitdir}/dialogs.ppu 
%{unitdir}/menus.ppu 
%{unitdir}/app.ppu 
%{unitdir}/histlist.ppu 
%{unitdir}/colortxt.ppu 
%{unitdir}/gadgets.ppu 
%{unitdir}/colorsel.ppu 
%{unitdir}/inplong.ppu 
%{unitdir}/stddlg.ppu 
%{unitdir}/mousedlg.ppu 
%{unitdir}/outline.ppu 
%{unitdir}/textview.ppu 
%{unitdir}/calc.ppu 
%{unitdir}/asciitab.ppu 
%{unitdir}/calendar.ppu 
%{unitdir}/helpfile.ppu 
%{unitdir}/editors.ppu

%{unitdir}/validate.o 
%{unitdir}/drivers.o 
%{unitdir}/helpctx.o 
%{unitdir}/memory.o 
%{unitdir}/views.o 
%{unitdir}/resource.o 
%{unitdir}/msgbox.o 
%{unitdir}/dialogs.o 
%{unitdir}/menus.o 
%{unitdir}/app.o 
%{unitdir}/histlist.o 
%{unitdir}/colortxt.o 
%{unitdir}/gadgets.o 
%{unitdir}/colorsel.o 
%{unitdir}/inplong.o 
%{unitdir}/stddlg.o 
%{unitdir}/mousedlg.o 
%{unitdir}/outline.o 
%{unitdir}/textview.o 
%{unitdir}/calc.o 
%{unitdir}/asciitab.o 
%{unitdir}/calendar.o 
%{unitdir}/helpfile.o 
%{unitdir}/editors.o

%{unitdir}/classes.ppu 
%{unitdir}/inifiles.ppu 
%{unitdir}/ezcgi.ppu

%{unitdir}/classes.o 
%{unitdir}/inifiles.o 
%{unitdir}/ezcgi.o

%{unitdir}/glib.ppu 
%{unitdir}/gmodule.ppu 
%{unitdir}/gdk.ppu 
%{unitdir}/gtk.ppu

%{unitdir}/glib.o 
%{unitdir}/gdk.o 
%{unitdir}/gtk.o
