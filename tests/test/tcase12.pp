{ test for simple comparsion }

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
    'abba': i := 1;
    'ababac': i := 2;
    'ababacaa': i := 3;
    'ababaca ': i := 4;
    ' ababaca': i := 5;
    ' ababac': i := 6;
    'ababaca': i := 7;
    else i := 0;
  end;

  writeln(i);
  if (i <> 7) then begin
    writeln('Error');
    Halt(1);
  end;


  case my_str_wide of
    'abba': i := 1;
    'ababac': i := 2;
    'ababacaa': i := 3;
    'ababaca ': i := 4;
    ' ababaca': i := 5;
    ' ababac': i := 6;
    'ababaca': i := 7;
    else i := 0;
  end;

  writeln(i);
  if (i <> 7) then begin
    writeln('Error');
    Halt(1);
  end;


  case my_str_ansi of
    'abba': i := 1;
    'ababac': i := 2;
    'ababacaa': i := 3;
    'ababaca ': i := 4;
    ' ababaca': i := 5;
    ' ababac': i := 6;
    'ababaca': i := 7;
    else i := 0;
  end;

  writeln(i);
  if (i <> 7) then begin
    writeln('Error');
    Halt(1);
  end;


  case my_str_uni of
    'abba': i := 1;
    'ababac': i := 2;
    'ababacaa': i := 3;
    'ababaca ': i := 4;
    ' ababaca': i := 5;
    ' ababac': i := 6;
    'ababaca': i := 7;
    else i := 0;
  end;

  writeln(i);
  if (i <> 7) then begin
    writeln('Error');
    Halt(1);
  end;
end.
