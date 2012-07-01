var
  q1: QWord;
begin
  q1:=$1020304050607080;
  if (q1 shl 0) <> q1 then
    halt(1);
  if (q1 shr 0) <> q1 then
    halt(2);
end.
