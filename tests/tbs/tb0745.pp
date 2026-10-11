{ %OPT=-vw }
{ %EXPECTMSGS=4110 }

{ an explicit typecast of a char constant of the other width to a char subrange it is outside
  of is the warning it is for a constant of the same width, also with range checking on }
program tb0745;

{$mode objfpc}
{$R+}
{ a single-byte source code page: under utf8 the wide-to-ansi direction is not folded (tb0747) }
{$codepage cp1252}

type
  tlower = 'a'..'z';

var
  l: tlower;
begin
  l := tlower(widechar('A'));
  if ord(l) <> 65 then
    halt(1);
end.
