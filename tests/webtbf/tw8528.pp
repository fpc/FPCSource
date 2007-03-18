{ %fail }

program test;

{$mode objfpc}{$H+}

const
  AllowedCharSet: set of Byte = [48..60];

var
  s: string;
begin
  s := 'test0';
  if s[5] in AllowedCharSet then
    Writeln('huh?');
end.
