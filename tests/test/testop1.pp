unit testop1;

interface

type
  op1 = record
    x,y : longint;
  end;

operator + (const a,b : op1) c : op1;

implementation

operator + (const a,b : op1) c : op1;
begin
  c.x:=a.x+b.x;
  c.y:=a.y+b.y;
end;

end.