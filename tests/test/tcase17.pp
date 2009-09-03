{ comparsion of empty string }

{$H+}
var
  my_str: string;
  my_str_wide: string;
  my_str_ansi: string;
  my_str_uni: string;
  i: integer;

begin
  my_str := '';
  my_str_wide := '';
  my_str_ansi := '';
  my_str_uni := '';
  i := -1;

  case my_str of
    'a'..'b': i := 1;
    'c': i := 2;
    'd'..'eee': i := 3;
    ''..'': i := 4;
    else i := 0;
  end;

  if (i <> 4) then begin
    writeln('Error');
    Halt(1);
  end;


  case my_str_wide of
    'a'..'b': i := 1;
    'c': i := 2;
    'd'..'eee': i := 3;
    ''..'': i := 4;
    else i := 0;
  end;

  if (i <> 4) then begin
    writeln('Error');
    Halt(1);
  end;


  case my_str_ansi of
    'a'..'b': i := 1;
    'c': i := 2;
    'd'..'eee': i := 3;
    ''..'': i := 4;
    else i := 0;
  end;

  if (i <> 4) then begin
    writeln('Error');
    Halt(1);
  end;


  case my_str_uni of
    'a'..'b': i := 1;
    'c': i := 2;
    'd'..'eee': i := 3;
    ''..'': i := 4;
    else i := 0;
  end;

  if (i <> 4) then begin
    writeln('Error');
    Halt(1);
  end;
end.
