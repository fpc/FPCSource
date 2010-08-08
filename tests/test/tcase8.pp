{ test for range with one-symbol string as left bound }

{$H+}
var
  my_str: string;
  i: integer;

begin
  my_str := 'cab';
  i := -1;

  case my_str of
    'a'..'daa': i := 1;
    else i := 0;
  end;

  if (i <> 1) then begin
    writeln('Error');
    Halt(1);
  end;
end.
