Program Example2;

{ Program to demonstrate the Addr function. }

Const Zero : integer = 0;

Var p : pointer;
    i : Integer;

begin
  p:=Addr(p);     { P points to itself }
  p:=Addr(I);     { P points to I }
  p:=Addr(Zero);  { P points to 'Zero' }
end.
