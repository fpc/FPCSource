{$mode delphi}
program test;

type
        booleanVoidFun = function : boolean;
        boolean1IntFun = function(i : integer) : boolean;

var
        af : array[1..10] of booleanVoidFun;
        ag : array[1..10] of boolean1IntFun;

        b : boolean;
        i : integer;

function alwaysTrue : boolean;
begin
        alwaysTrue := true;
end;

function maybeTrue(q : integer) : boolean;
begin
        maybeTrue := (q = 0);
end;

begin
        for i := 1 to 10 do begin
                af[i] := alwaysTrue;
                ag[i] := maybeTrue;
        end;

        b := af[1]; { can be fixed by using b := af[1]() }
        b := ag[1](0);
end.
