    StartPackage('fpmkunit');
    Version:='2.0.0';
    Description:='Free Pascal Make Tool';
    T:=Targets.AddUnit('fpmkunit');
      T.Directory:='src';
    EndPackage;
