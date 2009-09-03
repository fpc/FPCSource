{%FAIL}

{ left bound is greater; comparsion with empty string. Fails }

{$H+}
var
  my_str: string;
  i: integer;

begin
  my_str := 'ababaca';
  i := -1;

  case my_str of
    'aba'..'': i := 1;
    else i := 0;
  end;
end.
