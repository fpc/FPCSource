    StartPackage('aspell');
    {$IF defined(ALLPACKAGES)}
    Directory:='extra/aspell';
    {$ELSEIF defined(EXTRAPACKAGES)}
    Directory:='pcp';
    {$ENDIF}
    OS:=[linux,netbsd,freebsd,openbsd,solaris,darwin];
    T:=Targets.AddUnit('aspell');
    T:=Targets.AddUnit('scheck');
    EndPackage;
