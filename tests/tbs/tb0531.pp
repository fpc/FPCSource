procedure testshort;
var
  s1,s2: shortint;
  l: longint;
begin
  s1 := -1;
  s1 := s1 xor -1;
  l := -65536;
  l := l + s1;
  if (l <> -65536) then
    halt(1);

  s1 := 127;
  s1 := s1 or -128;
  l := -65536;
  l := l + s1;
  if (l <> -65536-1) then
    halt(2);


  s1 := -1;
  s1 := s1 xor -128;
  l := -65536;
  l := l + s1;
  if (l <> -65536+127) then
    halt(3);

  s1 := 127;
  s1 := s1 or -128;
  l := -65536;
  l := l + s1;
  if (l <> -65536-1) then
    halt(4);


  s1 := -1;
  s2 := -128;
  s1 := s1 xor s2;
  l := 0;
  l := l + s1;
  if l <> 127 then
    halt(5);
  
  s1 := 126;
  s2 := -128;
  s1 := s1 or s2;
  l := 0;
  l := l + s1;
  if l <> -2 then
    halt(6);
end;


procedure testsmall;
var
  s1,s2: smallint;
  l: longint;
begin
  s1 := -1;
  s1 := s1 xor -1;
  l := -65536;
  l := l + s1;
  if (l <> -65536) then
    halt(1+6);

  s1 := 32767;
  s1 := s1 or -32678;
  l := -65536;
  l := l + s1;
  if (l <> -65536-1) then
    halt(2+6);


  s1 := -1;
  s1 := s1 xor -32768;
  l := -65536;
  l := l + s1;
  if (l <> -65536+32767) then
    halt(3+6);

  s1 := 32767;
  s1 := s1 or -32768;
  l := -65536;
  l := l + s1;
  if (l <> -65536-1) then
    halt(4+6);


  s1 := -1;
  s2 := -32768;
  s1 := s1 xor s2;
  l := 0;
  l := l + s1;
  if l <> 32767 then
    halt(5+6);
  
  s1 := 32766;
  s2 := -32768;
  s1 := s1 or s2;
  l := 0;
  l := l + s1;
  if l <> -2 then
    halt(6+6);
end;

begin
  testshort;
  testsmall;
end.
