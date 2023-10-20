{ %NORUN }

program tw40410;

{$mode delphi}

procedure _Test0<T>(const AParam: T);
begin
end;

procedure _Test<T>(const AValue1, AValue2: T);
begin
  try
    _Test0<T>(AValue1);
  except
    _Test0<T>(AValue2);
  end;
end;

begin
end.
