unit testop2;

interface

type
  op2 = record
    x,y : longint;
  end;

operator + (const a,b : op2) c : op2;

implementation

operator + (const a,b : op2) c : op2;
begin
  c.x:=a.x+b.x;
  c.y:=a.y+b.y;
end;

end.