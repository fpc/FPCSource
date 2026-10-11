{ a widechar constant typecast to a distinct ansichar type has that type also where the
  conversion is not folded: with an utf-8 source code page it is done at run time }
program tb0747;

{$mode objfpc}
{$codepage utf8}

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

begin
  if p(taletter(widechar('a'))) <> 4 then
    halt(1);
  if ord(taletter(widechar('a'))) <> 97 then
    halt(2);
  if p(tletter(ansichar('a'))) <> 2 then
    halt(3);
  if p(ansichar(widechar('a'))) <> 3 then
    halt(4);
end.
