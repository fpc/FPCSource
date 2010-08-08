{ comparsion of one-symbol strings in ranges }

{$H-}
var
  my_str: string;
  my_str_wide: widestring;
  my_str_ansi: ansistring;
  my_str_uni: unicodestring;
  i: integer;

begin
  my_str := 'c';
  my_str_wide := 'c';
  my_str_ansi := 'c';
  my_str_uni := 'c';
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


  case my_str_wide of
    'a'..'b': i := 1;
    'c'..'d': i := 2;
    else i := 0;
  end;

  if (i <> 2) then begin
    writeln('Error');
    Halt(1);
  end;


  case my_str_ansi of
    'a'..'b': i := 1;
    'c'..'d': i := 2;
    else i := 0;
  end;

  if (i <> 2) then begin
    writeln('Error');
    Halt(1);
  end;


  case my_str_uni of
    'a'..'b': i := 1;
    'c'..'d': i := 2;
    else i := 0;
  end;

  if (i <> 2) then begin
    writeln('Error');
    Halt(1);
  end;
end.
