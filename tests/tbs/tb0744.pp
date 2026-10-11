{ an explicit typecast of a char constant to a distinct type of the char type of the other width
  has that type, as a typecast of a variable or of a constant of the same width has }
program tb0744;

{$mode objfpc}

type
  tletter = type widechar;
  taletter = type ansichar;

function p(c: widechar): longint; overload;
begin
  result := 1;
end;

function p(c: tletter): longint; overload;
begin
  result := 2;
end;

function p(c: ansichar): longint; overload;
begin
  result := 3;
end;

function p(c: taletter): longint; overload;
begin
  result := 4;
end;

var
  a: ansichar;
  w: widechar;
begin
  a := 'a';
  w := 'a';
  { a constant of the other width }
  if p(tletter(ansichar('a'))) <> 2 then
    halt(1);
  if p(tletter('a')) <> 2 then
    halt(2);
  if p(tletter(#65)) <> 2 then
    halt(3);
  if p(taletter(widechar('a'))) <> 4 then
    halt(4);
  { a constant of the same width, and variables }
  if p(tletter(widechar('a'))) <> 2 then
    halt(5);
  if p(taletter(ansichar('a'))) <> 4 then
    halt(6);
  if p(taletter('a')) <> 4 then
    halt(7);
  if p(tletter(a)) <> 2 then
    halt(8);
  if p(tletter(w)) <> 2 then
    halt(9);
  if p(taletter(w)) <> 4 then
    halt(10);
  if p(taletter(a)) <> 4 then
    halt(11);
  { the value is the converted one }
  if ord(tletter(ansichar('a'))) <> 97 then
    halt(12);
  if ord(taletter(widechar('a'))) <> 97 then
    halt(13);
  { a typecast to the char type itself still has that type }
  if p(widechar(ansichar('a'))) <> 1 then
    halt(14);
  if p(ansichar(widechar('a'))) <> 3 then
    halt(15);
end.
