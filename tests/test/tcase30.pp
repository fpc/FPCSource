{ comparsion with empty string as bound of 'needed' range }

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
    ''..'ababaca': i := 1;
    else i := 0;
  end;

  if (i <> 1) then begin
    writeln('Error');
    Halt(1);
  end;


  case my_str_wide of
    ''..'ababaca': i := 1;
    else i := 0;
  end;

  if (i <> 1) then begin
    writeln('Error');
    Halt(1);
  end;


  case my_str_ansi of
    ''..'ababaca': i := 1;
    else i := 0;
  end;

  if (i <> 1) then begin
    writeln('Error');
    Halt(1);
  end;


  case my_str_uni of
    ''..'ababaca': i := 1;
    else i := 0;
  end;

  if (i <> 1) then begin
    writeln('Error');
    Halt(1);
  end;
end.
