type
  TRecord = record
    w1,w2,w3,w4 : word;
  end;

var
  w1 : word;
  b1 : byte;
  rec : TRecord;
  ErrorCount : longint;

begin
  ErrorCount:=0;
  b1:=$57;
  w1:=$2D57;
  rec.w1:=w1;
  if (byte(rec.w1)<>b1) then
    inc(ErrorCount,3);
  if (rec.w1=b1) then
    inc(ErrorCount,4);
  halt(ErrorCount);
end.
