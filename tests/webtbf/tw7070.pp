{ %fail }

program varistr;

{$ifdef fpc}
{$mode delphi}
{$h+}
{$endif}

var
  str: string;
begin
  str := 'something';

  if not str = 'hello' then
    writeln('test')
end.

