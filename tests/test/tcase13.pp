{ the last range should be converted to single case and give 'expected' value }

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
    'a'..'b': i := 1;
    'c'..'caa': i := 2;
    'cab'..'cab': i := 3;
    else i := 0;
  end;

  if (i <> 3) then begin
    writeln('Error');
    Halt(1);
  end;
  

  case my_str_wide of
    'a'..'b': i := 1;
    'c'..'caa': i := 2;
    'cab'..'cab': i := 3;
    else i := 0;
  end;

  if (i <> 3) then begin
    writeln('Error_wide ', i);
    Halt(1);
  end;
  
  
  case my_str_ansi of
    'a'..'b': i := 1;
    'c'..'caa': i := 2;
    'cab'..'cab': i := 3;
    else i := 0;
  end;

  if (i <> 3) then begin
    writeln('Error_ansi');
    Halt(1);
  end;
  

  case my_str_uni of
    'aa': i := 1;
    'ca'..'caa': i := 2;
    'cab': i := 3;
    else i := 0;
  end;

  if (i <> 3) then begin
    writeln('Error');
    Halt(1);
  end;
end.
