unit toperator2;

interface

type
  op1 = record
    x,y : longint;
  end;

operator + (const a,b : op1) c : op1;

implementation

uses
  toperator3;

operator + (const a,b : op1) c : op1;
begin
  c.x:=a.x+b.x;
  c.y:=a.y+b.y;
end;

procedure test_op2;
var
  a,b,c : op2;
begin
  a.x:=44;
  a.y:=67;
  b.x:=-34;
  b.y:=-57;
  c:=a+b;
  if (c.x<>10) or (c.y<>10) then
    Halt(1);
end;

begin
  test_op2;
end.
