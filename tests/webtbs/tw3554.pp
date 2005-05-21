{$R+}

function d(a,b:string):Integer;
begin
  d := Ord(a[1])-Ord(b[1]);
end;

var
  c: SmallInt;
  ch : char;
begin
  ch:='A';
  c := Byte(ch)-Byte('B');
  WriteLn(c);
  WriteLn(d('a','b'));
end.
