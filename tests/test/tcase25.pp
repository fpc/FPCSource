{ test for range with one-symbol string as left bound }

{$H+}
var
  my_str: string;
  my_str_wide: widestring;
  my_str_ansi: ansistring;
  my_str_uni: unicodestring;
  i: integer;

begin
  my_str := 'cab';
  my_str_wide := 'cab';
  my_str_ansi := 'cab';
  my_str_uni := 'cab';
  i := -1;

  case my_str of
    'a'..'daa': i := 1;
    else i := 0;
  end;

  if (i <> 1) then begin
    writeln('Error');
    Halt(1);
  end;


  case my_str_wide of
    'a'..'daa': i := 1;
    else i := 0;
  end;

  if (i <> 1) then begin
    writeln('Error');
    Halt(1);
  end;


  case my_str_ansi of
    'a'..'daa': i := 1;
    else i := 0;
  end;

  if (i <> 1) then begin
    writeln('Error');
    Halt(1);
  end;


  case my_str_uni of
    'a'..'daa': i := 1;
    else i := 0;
  end;

  if (i <> 1) then begin
    writeln('Error');
    Halt(1);
  end;
end.
