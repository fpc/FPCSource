    StartPackage('hash');
    Version:='2.0.0';
    T:=Targets.AddUnit('src/md5.pp');
    T:=Targets.AddUnit('src/crc.pp');
    T:=Targets.AddUnit('src/ntlm.pas');
    T:=Targets.AddUnit('src/uuid.pas');
    T:=Targets.AddUnit('src/unixcrypt.pas');
    T.OS:=[Linux];
    T:=Targets.AddExampleunit('tests/mdtest.pas');
    EndPackage;

