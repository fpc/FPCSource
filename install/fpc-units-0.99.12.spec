Name: fpc-units
Version: 0.99.12
Release: 1
ExclusiveArch: i386
Copyright: GPL
Group: Development/Languages
Source: fpc-units-0.99.12-src.tar.gz
Requires: fpc = 0.99.12
Summary: Free Pascal Compiler Extra Units
Packager: Michael Van Canneyt (michael@tfdec1.fys.kuleuven.ac.be)
URL: http://tfdec1.fys.kuleuven.ac.be/~michael/fpc/fpc.html

%description	
The Free Pascal Compiler is a Turbo Pascal 7.0 and Delphi compatible 32bit
Pascal Compiler. It comes with fully TP 7.0 compatible run-time library.
Some extensions are added to the language, like function overloading. Shared
libraries can be linked and created. Basic Delphi support is already
implemented (classes,exceptions,ansistrings). This package contains
the FCL (Free Component Library), API/Free Vision and GTK 1.2.x interface.

%define unitdir /usr/lib/fpc/0.99.12/rtl

%prep
%setup -c

%build
# Don't load the system makefile.fpc
export FPCMAKE=
export FPCDIR=
NEWPP=/usr/bin/ppc386
	make -C fcl all PP=${NEWPP} RELEASE=1 UNITDIR=%{unitdir}
	make -C gtk all PP=${NEWPP} RELEASE=1 UNITDIR=%{unitdir}
	make -C api all PP=${NEWPP} RELEASE=1 UNITDIR=%{unitdir}
	make -C fv all PP=${NEWPP} RELEASE=1 UNITDIR=%{unitdir}

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

/usr/lib/fpc/0.99.12/units/common.ppu 
/usr/lib/fpc/0.99.12/units/callspec.ppu 
/usr/lib/fpc/0.99.12/units/video.ppu 
/usr/lib/fpc/0.99.12/units/keyboard.ppu 
/usr/lib/fpc/0.99.12/units/mouse.ppu 
/usr/lib/fpc/0.99.12/units/filectrl.ppu 
/usr/lib/fpc/0.99.12/units/filesys.ppu

/usr/lib/fpc/0.99.12/units/common.o 
/usr/lib/fpc/0.99.12/units/callspec.o 
/usr/lib/fpc/0.99.12/units/video.o 
/usr/lib/fpc/0.99.12/units/keyboard.o 
/usr/lib/fpc/0.99.12/units/mouse.o 
/usr/lib/fpc/0.99.12/units/filectrl.o 
/usr/lib/fpc/0.99.12/units/filesys.o

/usr/lib/fpc/0.99.12/units/validate.ppu 
/usr/lib/fpc/0.99.12/units/history.ppu 
/usr/lib/fpc/0.99.12/units/commands.ppu 
/usr/lib/fpc/0.99.12/units/drivers.ppu 
/usr/lib/fpc/0.99.12/units/helpctx.ppu 
/usr/lib/fpc/0.99.12/units/memory.ppu 
/usr/lib/fpc/0.99.12/units/objtypes.ppu 
/usr/lib/fpc/0.99.12/units/views.ppu 
/usr/lib/fpc/0.99.12/units/resource.ppu 
/usr/lib/fpc/0.99.12/units/msgbox.ppu 
/usr/lib/fpc/0.99.12/units/dialogs.ppu 
/usr/lib/fpc/0.99.12/units/menus.ppu 
/usr/lib/fpc/0.99.12/units/app.ppu 
/usr/lib/fpc/0.99.12/units/histlist.ppu 
/usr/lib/fpc/0.99.12/units/colortxt.ppu 
/usr/lib/fpc/0.99.12/units/gadgets.ppu 
/usr/lib/fpc/0.99.12/units/colorsel.ppu 
/usr/lib/fpc/0.99.12/units/inplong.ppu 
/usr/lib/fpc/0.99.12/units/stddlg.ppu 
/usr/lib/fpc/0.99.12/units/mousedlg.ppu 
/usr/lib/fpc/0.99.12/units/outline.ppu 
/usr/lib/fpc/0.99.12/units/textview.ppu 
/usr/lib/fpc/0.99.12/units/calc.ppu 
/usr/lib/fpc/0.99.12/units/asciitab.ppu 
/usr/lib/fpc/0.99.12/units/calendar.ppu 
/usr/lib/fpc/0.99.12/units/helpfile.ppu 
/usr/lib/fpc/0.99.12/units/editors.ppu

/usr/lib/fpc/0.99.12/units/validate.o 
/usr/lib/fpc/0.99.12/units/drivers.o 
/usr/lib/fpc/0.99.12/units/helpctx.o 
/usr/lib/fpc/0.99.12/units/memory.o 
/usr/lib/fpc/0.99.12/units/views.o 
/usr/lib/fpc/0.99.12/units/resource.o 
/usr/lib/fpc/0.99.12/units/msgbox.o 
/usr/lib/fpc/0.99.12/units/dialogs.o 
/usr/lib/fpc/0.99.12/units/menus.o 
/usr/lib/fpc/0.99.12/units/app.o 
/usr/lib/fpc/0.99.12/units/histlist.o 
/usr/lib/fpc/0.99.12/units/colortxt.o 
/usr/lib/fpc/0.99.12/units/gadgets.o 
/usr/lib/fpc/0.99.12/units/colorsel.o 
/usr/lib/fpc/0.99.12/units/inplong.o 
/usr/lib/fpc/0.99.12/units/stddlg.o 
/usr/lib/fpc/0.99.12/units/mousedlg.o 
/usr/lib/fpc/0.99.12/units/outline.o 
/usr/lib/fpc/0.99.12/units/textview.o 
/usr/lib/fpc/0.99.12/units/calc.o 
/usr/lib/fpc/0.99.12/units/asciitab.o 
/usr/lib/fpc/0.99.12/units/calendar.o 
/usr/lib/fpc/0.99.12/units/helpfile.o 
/usr/lib/fpc/0.99.12/units/editors.o

/usr/lib/fpc/0.99.12/units/classes.ppu 
/usr/lib/fpc/0.99.12/units/inifiles.ppu 
/usr/lib/fpc/0.99.12/units/ezcgi.ppu

/usr/lib/fpc/0.99.12/units/classes.o 
/usr/lib/fpc/0.99.12/units/inifiles.o 
/usr/lib/fpc/0.99.12/units/ezcgi.o

/usr/lib/fpc/0.99.12/units/glib.ppu 
/usr/lib/fpc/0.99.12/units/gmodule.ppu 
/usr/lib/fpc/0.99.12/units/gdk.ppu 
/usr/lib/fpc/0.99.12/units/gtk.ppu

/usr/lib/fpc/0.99.12/units/glib.o 
/usr/lib/fpc/0.99.12/units/gdk.o 
/usr/lib/fpc/0.99.12/units/gtk.o
