var
  s: string;
  res: boolean;
begin
  res:=false;
  s:='Tes 1';
  case s of
  'Tes 1','Tes 2','Tes 3': res:=true;
  end;
  if not res then
    halt(1);

  res:=false;
  s:='Tes 2';
  case s of
  'Tes 1','Tes 2','Tes 3': res:=true;
  end;
  if not res then
    halt(2);

  res:=false;
  s:='Tes 3';
  case s of
  'Tes 1','Tes 2','Tes 3': res:=true;
  end;
  if not res then
    halt(3);
end.
