begin
  { 8 Bit }

  if popcnt(byte($a4))<>3 then
    halt(1);

  if popcnt(byte($0))<>0 then
    halt(1);

  if popcnt(byte($20))<>1 then
    halt(1);

  writeln('popcnt(<byte>); passed');

{
  if popcnt(shortint($54))<>3 then
    halt(1);

  if popcnt(shortint($0))<>0 then
    halt(1);

  if popcnt(shortint($20))<>1 then
    halt(1);
}

  { 16 Bit }

  if popcnt(word($a4a4))<>6 then
    halt(1);

  if popcnt(word($0))<>0 then
    halt(1);

  if popcnt(word($2020))<>2 then
    halt(1);

  writeln('popcnt(<word>); passed');

{
  if popcnt(integer($5454))<>6 then
    halt(1);

  if popcnt(integer($0))<>0 then
    halt(1);

  if popcnt(integer($2020))<>2 then
    halt(1);
}

  { 32 Bit }

  if popcnt(dword($a4a4a4a4))<>12 then
    halt(1);

  if popcnt(dword($0))<>0 then
    halt(1);

  if popcnt(dword($20402040))<>4 then
    halt(1);

  writeln('popcnt(<dword>); passed');

{
  if popcnt(longint($54545454))<>12 then
    halt(1);

  if popcnt(longint($0))<>0 then
    halt(1);

  if popcnt(longint($20402080))<>4 then
    halt(1);
}

  { 64 Bit }

  if popcnt(qword($a4a4a4a4a4a4a4a4))<>24 then
    halt(1);

  if popcnt(qword($0))<>0 then
    halt(1);

  if popcnt(qword($2040204080804001))<>8 then
    halt(1);

  if popcnt(qword($a4a4a4a400000000))<>12 then
    halt(1);

  writeln('popcnt(<qword>); passed');

{
  if popcnt(int64($5454545454545454))<>24 then
    halt(1);

  if popcnt(int64($0))<>0 then
    halt(1);

  if popcnt(int64($2040208020402080))<>8 then
    halt(1);
}

  writeln('ok');
end.

