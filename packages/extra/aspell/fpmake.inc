    StartPackage('aspell');
    {$IF defined(ALLPACKAGES)}
    Directory:='extra/aspell';
    {$ELSEIF defined(EXTRAPACKAGES)}
    Directory:='pcp';
    {$ENDIF}
    OS:=[linux,netbsd,freebsd,openbsd,win32,solaris,darwin];
    T:=Targets.AddUnit('aspell');
    EndPackage;
