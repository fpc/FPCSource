unit toperator3;

interface

type
  op2 = record
    x,y,z : longint;
  end;

operator + (const a,b : op2) c : op2;

implementation

uses
  toperator2,toperator4;

operator + (const a,b : op2) c : op2;
begin
  c.x:=a.x+b.x;
  c.y:=a.y+b.y;
end;

procedure test_op3;
var
  a,b,c : op3;
begin
  a.x:=44.0;
  a.y:=67.0;
  b.x:=-34.0;
  b.y:=-57.0;
  c:=a+b;
  if (c.x<>10.0) or (c.y<>10.0) then
    Halt(1);
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

procedure test_op1;
var
  a,b,c : op1;
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
  test_op1;
  test_op2;
  test_op3;
end.
