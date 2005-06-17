    StartPackage('libasync');
    {$IF defined(ALLPACKAGES)}
    Directory:='base/libasync';
    {$ELSEIF defined(BASEPACKAGES)}
    Directory:='libasync';
    {$ENDIF}
    OS:=[linux,netbsd,openbsd,freebsd,darwin];
    T:=Targets.AddUnit('libasync');
    if (Defaults.OS=linux) then
      T.Directory:='unix';
    if (Defaults.OS=freebsd) then
      T.Directory:='unix';
    if (Defaults.OS=darwin) then
      T.Directory:='unix';
    if (Defaults.OS=netbsd) then
      T.Directory:='unix';
    if (Defaults.OS=openbsd) then
      T.Directory:='unix';
    EndPackage;
