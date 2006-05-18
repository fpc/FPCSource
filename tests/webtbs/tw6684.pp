Var
  D : LongInt;
  d1,d2 : int64;

Begin
  D := -100;
  {$OVERFLOWCHECKS on}
  Writeln(2 * D);
  d1:=d*2;
  {$OVERFLOWCHECKS off}
  Writeln(2 * D);
  d2:=d*2;
  if d1<>-200 then
    halt(1);
  if d2<>-200 then
    halt(1);
  writeln('ok');
End.
