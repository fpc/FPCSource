    StartPackage('imlib');
    {$IF defined(ALLPACKAGES)}
    Directory:='extra/imlib';
    {$ELSEIF defined(EXTRAPACKAGES)}
    Directory:='imlib';
    {$ENDIF}
    OS:=[linux,netbsd,freebsd,openbsd,os2,emx,darwin];
    Dependencies.Add('gtk');
    Dependencies.Add('x11');
    T:=Targets.AddUnit('gdk_imlib');
      T.Directory:='gdk_imlib';
    T:=Targets.AddUnit('imlib');
      T.Directory:='imlib';
    EndPackage;
