{ Fibonacci Numbers }

program fibo;
uses SysUtils;

function fib(N : integer) : longint;
begin
    if N < 2 then fib := 1
    else fib := fib(N-2) + fib(N-1);
End;

var
    NUM : integer;
    f : longint;

begin
    if ParamCount = 0 then
        NUM := 1
    else
        NUM := StrToInt(ParamStr(1));

    if NUM < 1 then NUM := 1;
    f := fib(NUM);
    WriteLn( IntToStr(f) );
end.
