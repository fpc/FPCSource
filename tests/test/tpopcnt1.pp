var
  b : byte;
  si : shortint;
  w : word;
  i : integer;
  d : dword;
  li : longint;
  q : qword;
  i64 : int64;

begin
  { 8 Bit }

  b:=$a4;
  if popcnt(b)<>3 then
    halt(1);

  b:=$0;
  if popcnt(b)<>0 then
    halt(1);

  b:=$20;
  if popcnt(b)<>1 then
    halt(1);

  writeln('popcnt(<byte>); passed');

{
  si:=$54;
  if popcnt(si)<>3 then
    halt(1);

  si:=$0;
  if popcnt(si)<>0 then
    halt(1);

  si:=$20;
  if popcnt(si)<>1 then
    halt(1);
}

  { 16 Bit }

  w:=$a4a4;
  if popcnt(w)<>6 then
    halt(1);

  w:=$0;
  if popcnt(w)<>0 then
    halt(1);

  w:=$2020;
  if popcnt(w)<>2 then
    halt(1);

  writeln('popcnt(<word>); passed');

{
  i:=$5454;
  if popcnt(i)<>6 then
    halt(1);

  i:=$0;
  if popcnt(i)<>0 then
    halt(1);

  i:=$2020;
  if popcnt(i)<>2 then
    halt(1);
}

  { 32 Bit }

  d:=$a4a4a4a4;
  if popcnt(d)<>12 then
    halt(1);

  d:=$0;
  if popcnt(d)<>0 then
    halt(1);

  d:=$20402040;
  if popcnt(d)<>4 then
    halt(1);

  writeln('popcnt(<dword>); passed');

{
  li:=$54545454;
  if popcnt(li)<>12 then
    halt(1);

  li:=$0;
  if popcnt(li)<>0 then
    halt(1);

  li:=$20402080;
  if popcnt(li)<>4 then
    halt(1);
}

  { 64 Bit }

  q:=qword($a4a4a4a4a4a4a4a4);
  if popcnt(q)<>24 then
    halt(1);

  q:=$0;
  if popcnt(q)<>0 then
    halt(1);

  q:=$2040204080804001;
  if popcnt(q)<>8 then
    halt(1);

  q:=qword($a4a4a4a400000000);
  if popcnt(q)<>12 then
    halt(1);

  writeln('popcnt(<qword>); passed');

{
  i64:=$5454545454545454;
  if popcnt(i64)<>24 then
    halt(1);

  i64:=$0;
  if popcnt(i64)<>0 then
    halt(1);

  i64:=$2040208020402080;
  if popcnt(li)<>8 then
    halt(1);
}

  writeln('ok');
end.

