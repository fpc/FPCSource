{ Strange bug while concatenating TBytes arrays }
program tw41087;

{$mode objfpc}
{$modeswitch arrayoperators}

type
  TBytes = array of Byte;

function U8(v: Byte): TBytes;
begin
  SetLength(Result, 1);
  Result[0] := v;
end;

var
  r: TBytes;
begin
  { three function results in one + chain must not alias the last one }
  r := U8($11) + U8($22) + U8($33);
  if (Length(r) <> 3) or (r[0] <> $11) or (r[1] <> $22) or (r[2] <> $33) then
    halt(1);
end.
