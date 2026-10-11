{ %OPT=-vw }
{ %EXPECTMSGS=4110 }

{ a char constant of the other width outside of a char subrange is the warning it is for a
  constant of the same width, with range checking off }
program tb0746;

{$mode objfpc}
{$R-}
{ a single-byte source code page: under utf8 the wide-to-ansi direction is not folded (tb0747) }
{$codepage cp1252}

type
  tlower = 'a'..'z';

var
  l: tlower;
begin
  l := widechar('A');
  if ord(l) <> 65 then
    halt(1);
end.
