{ Old file: tbs0121.pp }
{ cardinal -> byte conversion not work (and crashes)    OK 0.99.6 (FK) }

{$R+}
var

  c : cardinal;
  i : integer;
  w : word;
  b : byte;
  si : shortint;

begin
  w:=c;
  i:=c;
  b:=c;
  b:=si;
end.
