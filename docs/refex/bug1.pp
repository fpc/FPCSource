Program Example2;

{ Program to demonstrate the Addr function. }

Const Zero = 0;

Var p : pointer;

begin
  p:=Addr(Zero);  { P points to 'Zero' ??? }
end.
