{ Old file: tbs0026.pp }
{  tests for a wrong unused. var. warning              OK 0.9.4 }

const
  HexTbl : array[0..15] of char=('0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F');
function HexB(b:byte):string;
begin
  HexB[0]:=#2;
  HexB[1]:=HexTbl[b shr 4];
  HexB[2]:=HexTbl[b and $f];
end;



function HexW(w:word):string;
begin
  HexW:=HexB(w shr 8)+HexB(w and $ff);
end;



begin
  HexW($fff);
end.
