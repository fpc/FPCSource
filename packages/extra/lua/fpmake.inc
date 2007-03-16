    StartPackage('lua');
    {$IF defined(ALLPACKAGES)}
    Directory:='extra/lua';
    {$ELSEIF defined(EXTRAPACKAGES)}
    Directory:='lua';
    {$ENDIF}
    OS:=[linux,windows];
    CPU:=[i386];
    T:=Targets.AddUnit('lua');
    T:=Targets.AddUnit('lualib');
    T.Dependencies.Add('lua');
    T:=Targets.AddUnit('lauxlib');
    T.Dependencies.Add('lua');
    EndPackage;
