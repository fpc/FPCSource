    StartPackage('forms');
    {$IF defined(ALLPACKAGES)}
    Directory:='extra/forms';
    {$ELSEIF defined(EXTRAPACKAGES)}
    Directory:='forms';
    {$ENDIF}
    OS:=[linux,netbsd,freebsd,openbsd,darwin];
    Dependencies.Add('x11');
    T:=Targets.AddUnit('xforms');
    T:=Targets.AddProgram('fd2pascal');
    EndPackage;
