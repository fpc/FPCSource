    StartPackage('libpng');
    {$IF defined(ALLPACKAGES)}
    Directory:='extra/libpng';
    {$ELSEIF defined(EXTRAPACKAGES)}
    Directory:='libpng';
    {$ENDIF}
    OS:=[linux,netbsd,freebsd,openbsd,os2,emx,darwin];
    Dependencies.Add('zlib');
    T:=Targets.AddUnit('png');
    EndPackage;
