unit toperator4;

interface

type
  op3 = record
    x,y : real;
  end;

operator + (const a,b : op3) c : op3;

implementation

operator + (const a,b : op3) c : op3;
begin
  c.x:=a.x+b.x;
  c.y:=a.y+b.y;
end;

end.
