{ Old file: tbs0299.pp }
{ passing Array[0..1] of char by value to proc leads to problems OK 0.99.13 (PM)
passing Array[0..1] of char by value to proc leads to problems }

type
  TwoChar = Array[0..1] of char;
  Empty = Record
          End;
const
  asd : TwoChar = ('a','b');

procedure Tester(i:TwoChar; a: Empty;l : longint;var ll : longint);
begin
  i[0]:=i[1];
  Writeln('l = ',l,' @l = ',hexstr(longint(@l),8),' @a = ',hexstr(longint(@a),8));
  inc(ll);
end;

var
  a : Empty;
  l,ll : longint;
begin
  l:=6;
  ll:=15;
  Writeln(Sizeof(asd));
  Tester(asd,a,l,ll);
  Writeln(asd);
  if (ll<>16) then
    Begin
      Writeln('Error with passing value parameter of type array [1..2] of char');
      Halt(1);
    end;
end.
