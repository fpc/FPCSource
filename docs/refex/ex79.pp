program example79;

{ Program to demonstrate the setjmp, longjmp functions }

procedure dojmp(var env : jmp_buf; value : longint);

begin
  value:=2;
  Writeln ('Going to jump !');
  { This will return to the setjmp call,
    and return value instead of 0 }
  longjmp(env,value);
end;

var env : jmp_buf;

begin
  if setjmp(env)=0 then
    begin
    writeln ('Passed first time.');
    dojmp(env,2);
    end
  else
    writeln ('Passed second time.');
end.
