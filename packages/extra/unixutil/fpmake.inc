    StartPackage('unixutil');
    {$IF defined(ALLPACKAGES)}
    Directory:='extra/unixutil';
    {$ELSEIF defined(EXTRAPACKAGES)}
    Directory:='unixutil';
    {$ENDIF}
    OS:=[linux];
    Dependencies.Add('libc');
    Dependencies.Add('fcl');
    T:=Targets.AddUnit('unixutils');
    T.ResourceStrings:=True;
    EndPackage;
