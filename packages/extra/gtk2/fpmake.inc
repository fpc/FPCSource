    StartPackage('gtk2');
    {$IF defined(ALLPACKAGES)}
    Directory:='extra/gtk2';
    {$ELSEIF defined(EXTRAPACKAGES)}
    Directory:='gtk2';
    {$ENDIF}
    OS:=[linux,freebsd,win32];
    T:=Targets.AddUnit('buildgtk2');
    T.Install:=False;
    T:=Targets.AddUnit('glib/glib2');
    T:=Targets.AddUnit('atk/atk');
    T:=Targets.AddUnit('pango/pango');
    T:=Targets.AddUnit('gtk+/gdk-pixbuf/gdk2pixbuf');
    T:=Targets.AddUnit('gtk+/gdk/gdk2');
    T:=Targets.AddUnit('gtk+/gtk/gtk2');
    T:=Targets.AddUnit('libglade/libglade2');
    T:=Targets.AddUnit('gtkglext/gtkglext');
    T:=Targets.AddUnit('gtkglext/gdkglext');
    EndPackage;
