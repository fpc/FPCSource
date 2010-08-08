{ simple test for range }

{$H+}
var
  my_str: string;
  i: integer;

begin
  my_str := 'cab';
  i := -1;

  case my_str of
    'aba'..'daa': i := 1;
    else i := 0;
  end;

  if (i <> 1) then begin
    writeln('Error');
    Halt(1);
  end;
end.
