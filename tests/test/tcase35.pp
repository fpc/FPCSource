{%FAIL}

{ duplicate labels in different cases, one of them is range }

{$H-}
var
  my_str: string;
  my_str_wide: string;
  my_str_ansi: string;
  my_str_uni: string;
  i: integer;

begin
  my_str := 'cab';
  my_str_wide := 'cab';
  my_str_ansi := 'cab';
  my_str_uni := 'cab';
  i := -1;

  case my_str of
    'a'..'b': i := 1;
    'c': i := 2;
    'c'..'d': i := 3;
    else i := 0;
  end;

  if (i <> 3) then begin
    writeln('Error');
    Halt(1);
  end;


  case my_str_wide of
    'a'..'b': i := 1;
    'c': i := 2;
    'c'..'d': i := 3;
    else i := 0;
  end;

  if (i <> 3) then begin
    writeln('Error');
    Halt(1);
  end;


  case my_str_ansi of
    'a'..'b': i := 1;
    'c': i := 2;
    'c'..'d': i := 3;
    else i := 0;
  end;

  if (i <> 3) then begin
    writeln('Error');
    Halt(1);
  end;


  case my_str_uni of
    'a'..'b': i := 1;
    'c': i := 2;
    'c'..'d': i := 3;
    else i := 0;
  end;

  if (i <> 3) then begin
    writeln('Error');
    Halt(1);
  end;
end.
