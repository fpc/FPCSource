program tb0688;

function Test(var aArg: QWordBool): LongInt;
begin
  Test := LongInt(aArg);
end;

function Test(var aArg: Boolean64): LongInt;
begin
  Test := LongInt(aArg);
end;

var
  b64: Boolean64;
  qb: QWordBool;
begin
  b64 := True;
  if Test(b64) <> 1 then
    Halt(1);
  b64 := False;
  if Test(b64) <> 0 then
    Halt(2);
  qb := True;
  if Test(qb) <> -1 then
    Halt(3);
  qb := False;
  if Test(qb) <> 0 then
    Halt(4);
  qb := QWordBool($12341234);
  if Test(qb) <> $12341234 then
    Halt(5);
  qb := QWordBool($100000000);
  if Test(qb) <> 0 then
    Halt(6);
end.
