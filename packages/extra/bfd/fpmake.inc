    StartPackage('bfd');
    {$IF defined(ALLPACKAGES)}
    Directory:='extra/bfd';
    {$ELSEIF defined(EXTRAPACKAGES)}
    Directory:='bfd';
    {$ENDIF}
    OS:=[linux,netbsd,freebsd,openbsd,darwin];
    T:=Targets.AddUnit('bfd');
    EndPackage;
