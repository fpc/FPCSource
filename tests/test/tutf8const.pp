{ UTF-8 string constants converted to UnicodeString and to another code page keep their last character }
program tutf8const;

{$mode objfpc}{$h+}
{$codepage utf8}

type
  T1252String = type AnsiString(1252);

var
  lUni: UnicodeString;
  l1252: T1252String;

begin
  lUni:='abc';
  if (Length(lUni)<>3) or (lUni[3]<>'c') then
    halt(1);
  lUni:='déf';
  if (Length(lUni)<>3) or (Ord(lUni[2])<>$E9) or (lUni[3]<>'f') then
    halt(2);
  l1252:='abc';
  if (Length(l1252)<>3) or (l1252[3]<>'c') then
    halt(3);
  l1252:='déf';
  if (Length(l1252)<>3) or (Ord(l1252[2])<>$E9) or (l1252[3]<>'f') then
    halt(4);
  writeln('ok');
end.
