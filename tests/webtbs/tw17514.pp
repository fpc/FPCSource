{$ifdef fpc}
{$mode objfpc}{$h+}
{$endif}

var
  a: ansistring;
  s: shortstring;
{$ifdef fpc}
  u: unicodestring;
{$endif}
  w: widestring;
  code: Integer;

begin
  code := 0;
  a := 'foo bar';
  delete(a, 4, maxint);
  if a <> 'foo' then
    code := code or 1;
  s := 'foo bar';
  delete(s, 4, maxint);
  if s <> 'foo' then
    code := code or 2;
  w := 'foo bar';
  delete(w, 4, maxint);
  if w <> 'foo' then
    code := code or 4;
{$ifdef fpc}
  u := 'foo bar';
  delete(u, 4, maxint);
  if u <> 'foo' then
    code := code or 8;
{$endif}
  Halt(code);
end.