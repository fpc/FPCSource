    StartPackage('gtk');
    {$IF defined(ALLPACKAGES)}
    Directory:='extra/gtk';
    {$ELSEIF defined(EXTRAPACKAGES)}
    Directory:='gtk';
    {$ENDIF}
    OS:=[linux,netbsd,freebsd,openbsd,win32,os2,emx,darwin];
    T:=Targets.AddUnit('glib/glib');
    T:=Targets.AddUnit('glib/gmodule');
    T:=Targets.AddUnit('gdk/gdk');
    T:=Targets.AddUnit('gtk/gtk');
    T:=Targets.AddUnit('gdk/gdkpixbuf');
    T:=Targets.AddUnit('gtkgl/gtkglarea');
    T:=Targets.AddExampleUnit('gtkgl/gtkgldemo');
    EndPackage;
