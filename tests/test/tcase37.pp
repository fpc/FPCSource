{%FAIL}

{ left bound is greater; comparsion with empty string. Fails }

{$H-}
var
  my_str: string;
  my_str_wide: widestring;
  my_str_ansi: ansistring;
  my_str_uni: unicodestring;
  i: integer;

begin
  my_str := 'ababaca';
  my_str_wide := 'ababaca';
  my_str_ansi := 'ababaca';
  my_str_uni := 'ababaca';
  i := -1;

  case my_str of
    'aba'..'': i := 1;
    else i := 0;
  end;

  case my_str_wide of
    'aba'..'': i := 1;
    else i := 0;
  end;

  case my_str_ansi of
    'aba'..'': i := 1;
    else i := 0;
  end;

  case my_str_uni of
    'aba'..'': i := 1;
    else i := 0;
  end;
end.
