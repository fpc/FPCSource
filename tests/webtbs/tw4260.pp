{ Source provided for Free Pascal Bug Report 4260 }
{ Submitted by "Rodrigo Robles" on  2005-08-07 }
{ e-mail: rodrigo_augusto_robles@yahoo.com.br }
program teste_ansireplacetext;

uses strutils;


var
str : string;

procedure docheck(const s : string);
  begin
    if s<>str then
      halt(1);
  end;

BEGIN
str:=ansireplacetext('123456789','456','ABC');
docheck('123ABC789');
str:=ansireplacetext('123456789','123','ABC');
docheck('ABC456789');
str:=ansireplacetext('123456789','789','ABC');
docheck('123456ABC');
str:=ansireplacetext('123456789','123456789','ABC');
docheck('ABC');


str:=ansireplacetext('123abc789','abc','ABC');
docheck('123ABC789');
str:=ansireplacetext('XYZ456789','xyz','ABC');
docheck('ABC456789');
str:=ansireplacetext('123456xyz','XYZ','ABC');
docheck('123456ABC');
str:=ansireplacetext('xxxxyzxxx','xxxxyzxxx','ABC');
docheck('ABC');

writeln('ok');

END.