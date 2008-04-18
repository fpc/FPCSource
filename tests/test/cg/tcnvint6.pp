
procedure error(n: longint);
begin
  writeln('Test failed: ', n);
  Halt(n);
end;

var
  b: byte;
  w: word;
  c: cardinal;
  shi: shortint;
  si: smallint;
  i64: int64;
begin
  b:=$ff;
  Inc(b,$ff);
  if shortint(b)<>-2 then
    error(1);
  if smallint(b)<>$fe then
    error(2);
  if word(b)<>$fe then
    error(3);
  if longint(b)<>$fe then
    error(4);
  if cardinal(b)<>$fe then
    error(5);
  if int64(b)<>$fe then
    error(6);
{$ifdef FPC}
  if qword(b)<>$fe then
    error(7);
{$endif FPC}

  w:=$8000;
  if shortint(w)<>0 then
    error(8);

  w:=$ffff;
  Inc(w,$ffff);
  if shortint(w)<>-2 then
    error(11);
  if byte(w)<>$fe then
    error(12);
  if smallint(w)<>-2 then
    error(13);
  if longint(w)<>$fffe then
    error(14);
  if cardinal(w)<>$fffe then
    error(15);
  if int64(w)<>$fffe then
    error(16);
{$ifdef FPC}
  if qword(w)<>$fffe then
    error(17);
{$endif FPC}

  c:=$ffffffff;
  Inc(c,$ffffffff);
  if int64(c)<>$fffffffe then
    error(21);
{$ifdef FPC}
  if qword(c)<>$fffffffe then
    error(22);
{$endif FPC}

  shi:=-1;
  if word(shi)<>$ffff then
    error(31);
  if cardinal(shi)<>$ffffffff then
    error(32);
  i64:=cardinal(shi);
  if i64<>$ffffffff then
    error(33);
{$ifdef FPC}
  if qword(shi)<>$ffffffffffffffff then
    error(34);
{$endif FPC}

  si:=-1;
  if word(si)<>$ffff then
    error(35);
  if cardinal(si)<>$ffffffff then
    error(36);
{$ifdef FPC}
  if qword(si)<>$ffffffffffffffff then
    error(37);
{$endif FPC}

  writeln('Test OK.');
end.
