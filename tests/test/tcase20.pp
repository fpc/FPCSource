{%FAIL}

{ left bound is greater; fails }

{$H+}
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
    'abba'..'ababaca': i := 1;
    else i := 0;
  end;

  case my_str_wide of
    'abba'..'ababaca': i := 1;
    else i := 0;
  end;

  case my_str_ansi of
    'abba'..'ababaca': i := 1;
    else i := 0;
  end;

  case my_str_uni of
    'abba'..'ababaca': i := 1;
    else i := 0;
  end;
end.
