{ Nested Loops }




program nestedloop;
uses SysUtils;

var n, a, b, c, d, e, f : integer;
var x : longint;

begin
    if ParamCount = 0 then
        n := 1
    else
        n := StrToInt(ParamStr(1));
    if n < 1 then n := 1;
    x := 0;
    For a := 1 to n Do
    For b := 1 to n Do
    For c := 1 to n Do
    For d := 1 to n Do
    For e := 1 to n Do
    For f := 1 to n Do
    Inc(x);
    WriteLn( IntToStr(x) );
end.
