    StartPackage('cairo');
    {$IF defined(ALLPACKAGES)}
    Directory:='extra/cairo';
    {$ELSEIF defined(EXTRAPACKAGES)}
    Directory:='cairo';
    {$ENDIF}
    OS:=[linux, freebsd, netbsd, openbsd, darwin, solaris];
    T:=Targets.AddUnit('cairo');
    EndPackage;
