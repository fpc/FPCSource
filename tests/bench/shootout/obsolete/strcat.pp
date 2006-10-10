{ String Concatenation }

program strcat;

uses SysUtils;
var
    NUM, i : longint;
    str : string;

begin
    if ParamCount = 0 then NUM := 1
    else NUM := StrToInt(ParamStr(1));
    if NUM < 1 then NUM := 1;

    str := '';
    For i := 1 To NUM Do
        str := str + 'hello'#13;
    WriteLn( Longint(Length(str)) );
    WriteLn( str );
end.
