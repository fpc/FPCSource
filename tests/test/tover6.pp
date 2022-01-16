{ %FAIL }

unit tover6;

{$mode objfpc}{$H+}

interface

procedure Test(aArg: LongInt);
function Test(aArg: LongInt): LongInt;

implementation

procedure Test(aArg: LongInt);
begin

end;

function Test(aArg: LongInt): LongInt;
begin

end;

end.

