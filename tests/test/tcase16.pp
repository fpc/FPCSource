{ comparsion of one-symbol strings as ranges and single cases }

{$H+}
var
  my_str: string;
  my_str_wide: string;
  my_str_ansi: string;
  my_str_uni: string;
  i: integer;

begin
  my_str := 'b';
  my_str_wide := 'b';
  my_str_ansi := 'b';
  my_str_uni := 'b';
  i := -1;

  case my_str of
    'a'..'b': i := 1;
    'c': i := 2;
    'd'..'eee': i := 3;
    else i := 0;
  end;

  if (i <> 1) then begin
    writeln('Error');
    Halt(1);
  end;


  case my_str_wide of
    'a'..'b': i := 1;
    'c': i := 2;
    'd'..'eee': i := 3;
    else i := 0;
  end;

  if (i <> 1) then begin
    writeln('Error');
    Halt(1);
  end;


  case my_str_ansi of
    'a'..'b': i := 1;
    'c': i := 2;
    'd'..'eee': i := 3;
    else i := 0;
  end;

  if (i <> 1) then begin
    writeln('Error');
    Halt(1);
  end;


  case my_str_uni of
    'a'..'b': i := 1;
    'c': i := 2;
    'd'..'eee': i := 3;
    else i := 0;
  end;

  if (i <> 1) then begin
    writeln('Error');
    Halt(1);
  end;
end.
