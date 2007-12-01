    StartPackage('hash');
    Version:='2.0.0';
    T:=Targets.AddUnit('md5');
      T.Directory:='src';
    T:=Targets.AddUnit('crc');
      T.Directory:='src';
    T:=Targets.AddUnit('ntlm');
      T.Directory:='src';
    T:=Targets.AddUnit('uuid');
      T.Directory:='src';
    T:=Targets.AddUnit('unixcrypt');
    T:=Targets.AddExampleunit('mdtest');
      T.Directory:='tests';
    EndPackage;

