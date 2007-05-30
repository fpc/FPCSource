    StartPackage('fpmake');
    Version:='2.0.0';
    Description:='Free Pascal Make Tool';
    T:=Targets.AddUnit('fpmkunit');
    T.Directory:='src';
    S:=Sources.AddSrcFiles('src/*');
    S:=Sources.AddSrcFiles('fpmake.inc');
    S:=Sources.AddSrcFiles('fpmake.pp');
    EndPackage;
