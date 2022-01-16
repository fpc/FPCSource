program tb0619;

var
  ss: ShortString;
  us: UnicodeString;
  ws: WideString;
  as: AnsiString;
  i: LongInt;
begin
  ss := 'Test';
  Delete(ss, 2, 1);
  if ss <> 'Tst' then
    Halt(1);
  Insert('Foo', ss, 2);
  if ss <> 'TFoost' then
    Halt(2);

  us := 'Test';
  Delete(us, 2, 1);
  if us <> 'Tst' then
    Halt(3);
  Insert('Foo', us, 2);
  if ss <> 'TFoost' then
    Halt(4);

  ws := 'Test';
  Delete(ws, 2, 1);
  if ws <> 'Tst' then
    Halt(5);
  Insert('Foo', ws, 2);
  if ss <> 'TFoost' then
    Halt(6);

  as := 'Test';
  Delete(as, 2, 1);
  if as <> 'Tst' then
    Halt(7);
  Insert('Foo', as, 2);
  if ss <> 'TFoost' then
    Halt(8);

  ss := 'Test';
  Insert(#$41, ss, 2);
  if ss <> 'TAest' then
    Halt(9);
end.
