var
  S :String;
begin
  S := 'H';
  case S of
    'HH','H': WriteLn('1');
  end;
  S := 'HH';
  case S of
    'HH': WriteLn('2');
  end;
  case S of
    'HH','H': WriteLn('3');
  end;
  case S of
    'H','HH': WriteLn('4');
  end;
  S := 'A';
  case S of
    'HH': WriteLn('2');
  end;
  case S of
    'HH','H': WriteLn('3');
  end;
  case S of
    'H','HH': WriteLn('4');
  end;
end.
