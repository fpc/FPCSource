    StartPackage('regexpr');
    {$IF defined(ALLPACKAGES)}
    Directory:='base/regexpr';
    {$ELSEIF defined(BASEPACKAGES)}
    Directory:='regexpr';
    {$ENDIF}
    Version:='2.0.0';
    Options:='-S2';
    T:=Targets.AddUnit('regexpr');
    T:=Targets.AddExampleunit('testreg1');
    EndPackage;
