type 
  TwoChar = Array[0..1] of char;
  Empty = Record
	  End;
const
  asd : TwoChar = ('a','b');

procedure Tester(i:TwoChar; a: Empty;l : longint);
begin
  i[0]:=i[1];
  Writeln('l = ',l,' @l = ',hexstr(longint(@l),8),' @a = ',hexstr(longint(@a),8));
end;

var
  a : Empty;
  l : longint;
begin
  l:=6;
  Writeln(Sizeof(asd));
  Tester(asd,a,l);
  Writeln(asd);
end.
