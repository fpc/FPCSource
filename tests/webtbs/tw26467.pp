{ %OPT=-Sew}
{$INLINE ON}
{$ASSERTIONS ON}
program test;

procedure TestFunc();
begin
   Assert(True);
end;

procedure TestFuncInline(); inline;
begin
   Assert(True);
end;

begin
   TestFunc();
   TestFuncInline(); // Warning: unreachable code
end.
