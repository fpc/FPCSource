{ %fail }
{ %opt=-Se1w2 -vw }

var
  pc: pchar;
  m,m1: longword;
begin
  m:=1;
  pc^:=char(m-(m1*10)+byte('0'));
end.
