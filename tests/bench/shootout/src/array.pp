{ Array Access }

Program ary3;

uses SysUtils, Classes;

var
    n, i, k, last : longint;
    X, Y : TList;
begin
    if ParamCount = 0 then
        n := 1
    else
        n := StrToInt(ParamStr(1));

    if n < 1 then n := 1;

    last := n - 1;
    X := TList.Create;
    X.Capacity := n;
    For i := 0 To last do
        X.Add( Pointer(i+1) );

    Y := TList.Create;
    Y.Capacity := n;
    For i := 0 To last do
        Y.Add( Pointer(0) );


    For k := 0 To 999 do
    begin
        For i := last downto 0 do
        begin
            Y.Items[i] := Pointer(longint(Y.Items[i]) + longint(X.Items[i]));
        end;
    end;
    Writeln (IntToStr(longint(Y.Items[0])), ' ', IntToStr(longint(Y.Items[last])));
end.
