{$mode objfpc}
uses
  sysutils;
var
  d : tdatetime;
begin
  try
    d:=strtodate('03'+dateseparator+'03'+dateseparator+'2033');
  except
    halt(1);
  end;
  writeln(1);
  try
    d:=strtodate('2.2/2');
    halt(1);
  except
  end;
  writeln(2);
  try
    d:=strtodate('33'+dateseparator+'33'+dateseparator+'33');
    halt(1);
  except
  end;
  writeln('ok');
end.
