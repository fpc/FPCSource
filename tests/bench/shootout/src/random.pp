{ Random Number Generator }

program random;
uses SysUtils;

const
    IM = 139968;
    IA =   3877;
    IC =  29573;

var 
    LAST, NUM, i : longint;
    result : real;

function gen_random(n : integer) : real;
begin    
    LAST := (LAST * IA + IC) mod IM;
    gen_random := n * LAST / IM;
end;

begin
    if ParamCount = 0 then
        NUM := 1
    else
        NUM := StrToInt(ParamStr(1));
    if NUM < 1 then NUM := 1;
    LAST := 42;
    for i:= 1 to NUM do
    begin
        result := gen_random(100);
    end;
    WriteLn( result:10:9 );
end.
