{ %fail }
{ %expectmsgs=4109 }

{ a char constant of the other width outside of a char subrange is a range check error with
  range checking on, as one of the same width is }
program tb0334;

{$mode objfpc}
{$R+}
{ a single-byte source code page: under utf8 the wide-to-ansi direction is not folded (tb0747) }
{$codepage cp1252}

type
  tlower = 'a'..'z';

var
  l: tlower;
begin
  l := widechar('A');
  writeln(ord(l));
end.
