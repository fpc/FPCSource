program tb0675;

{$mode objfpc}

function Test(a: Single): LongInt;
begin
  Result := 1;
end;

{$ifdef FPC_HAS_TYPE_DOUBLE}
function Test(a: Double): LongInt;
begin
  Result := 2;
end;
{$endif}

function Test2(a: Single): LongInt;
begin
  Result := 1;
end;

{$ifdef FPC_HAS_TYPE_DOUBLE}
function Test2(a: Double): LongInt;
begin
  Result := 2;
end;
{$endif}

{$ifdef FPC_HAS_TYPE_EXTENDED}
function Test2(a: Extended): LongInt;
begin
  Result := 3;
end;
{$endif}

var
  a: Currency;
begin
  if Test(a) <> 2 then
    Halt(1);
{$ifdef FPC_HAS_TYPE_EXTENDED}
  if Test2(a) <> 3 then
    Halt(2);
{$endif}
end.
