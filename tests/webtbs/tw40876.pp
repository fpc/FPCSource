{ %NORUN }

program tw40876;

{$Mode Delphi}

uses
    SysUtils;

type
    TTest<const A: UInt64> = record
    public
        function ToString(B: UInt64): UnicodeString;
    end;

// There should be at least one method for linking to fail
function TTest<A>.ToString(B: UInt64): UnicodeString;
begin
    Result := (A + B).ToString;
end;

type
    TMyTest = TTest<1234>;

var A: TMyTest;
begin
    WriteLn(A.ToString(23456));
end.

