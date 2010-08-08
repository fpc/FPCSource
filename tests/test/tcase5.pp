{ comparsion of one-symbol strings in ranges }

{$H+}
var
  my_str: string;
  i: integer;

begin
  my_str := 'c';
  i := -1;

  case my_str of
    'a'..'b': i := 1;
    'c'..'d': i := 2;
    else i := 0;
  end;

  if (i <> 2) then begin
    writeln('Error');
    Halt(1);
  end;
end.
