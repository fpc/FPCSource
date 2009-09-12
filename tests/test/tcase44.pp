{ test for range with one-symbol string as left bound }
{ sequence of cases in cases                          }

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
    'a'..'daa':
      begin
        case my_str[1] of
          'a' : i := 0;
          'b' : i := 0;
          'c' : i := 1;
        end;
        case my_str[2] of
          'a' : i := i * 1;
          'b' : i := 0;
          'c' : i := 0;
        end;
        case my_str[3] of
          'a' : i := 0;
          'b' : i := i * 1;
          'c' : i := 0;
        end;
      end;
    else i := 0;
  end;

  if (i <> 1) then begin
    writeln('Error');
    Halt(1);
  end;


  case my_str_wide of
    'a'..'daa':
      begin
        case my_str_wide[1] of
          'a' : i := 0;
          'b' : i := 0;
          'c' : i := 1;
        end;
        case my_str_wide[2] of
          'a' : i := i * 1;
          'b' : i := 0;
          'c' : i := 0;
        end;
        case my_str_wide[3] of
          'a' : i := 0;
          'b' : i := i * 1;
          'c' : i := 0;
        end;
      end;
    else i := 0;
  end;

  if (i <> 1) then begin
    writeln('Error');
    Halt(1);
  end;


  case my_str_ansi of
    'a'..'daa':
      begin
        case my_str_ansi[1] of
          'a' : i := 0;
          'b' : i := 0;
          'c' : i := 1;
        end;
        case my_str_ansi[2] of
          'a' : i := i * 1;
          'b' : i := 0;
          'c' : i := 0;
        end;
        case my_str_ansi[3] of
          'a' : i := 0;
          'b' : i := i * 1;
          'c' : i := 0;
        end;
      end;
    else i := 0;
  end;

  if (i <> 1) then begin
    writeln('Error');
    Halt(1);
  end;


  case my_str_uni of
    'a'..'daa':
      begin
        case my_str_uni[1] of
          'a' : i := 0;
          'b' : i := 0;
          'c' : i := 1;
        end;
        case my_str_uni[2] of
          'a' : i := i * 1;
          'b' : i := 0;
          'c' : i := 0;
        end;
        case my_str_uni[3] of
          'a' : i := 0;
          'b' : i := i * 1;
          'c' : i := 0;
        end;
      end;
    else i := 0;
  end;

  if (i <> 1) then begin
    writeln('Error');
    Halt(1);
  end;

  writeln('OK!');
end.
