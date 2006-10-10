{ Sieve of Erathostenes }

program sieve;
uses SysUtils;

var
    NUM, i, k, count : integer;
    flags : array[0..8192] of integer;

begin
    if ParamCount = 0 then
        NUM := 1
    else
        NUM := StrToInt(ParamStr(1));

    if NUM < 1 then NUM := 1;

    while NUM > 0 do
    begin
        Dec(NUM);
        count := 0;
        for i := 0 to 8192 do
        begin
            flags[i] := i;
        end;
        for i := 2 to 8192 do
        begin
            if flags[i] <> -1 then
            begin
                k := i+i;
                while k <= 8192 do
                begin
                    flags[k] := -1;
                    Inc(k, i);
                end;
                Inc(count);
            end;
        end;
    end;
    WriteLn('Count: ' + IntToStr(Count));
end.
