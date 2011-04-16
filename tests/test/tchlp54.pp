{ this example tests combinations of class and helpers hierarchies }
program tchlp54;

{$ifdef fpc}
  {$mode delphi}
{$endif}
{$apptype console}

type
  TTest1 = class
  end;

  TTest2 = class(TTest1)
    class function Test3: Integer;
  end;

  TTest3 = class(TTest2)
    class function Test1: Integer;
    class function Test2: Integer;
  end;

  TTest4 = class(TTest3)
  end;

  TTest1Helper = class helper for TTest1
    class function Test1: Integer;
    class function Test3: Integer;
    class function Test4: Integer;
  end;

  TTest3Helper = class helper for TTest3
    class function Test2: Integer;
    class function Test4: Integer;
  end;

  TTest4Helper = class helper(TTest1Helper) for TTest4
    class function DoTest1: Integer;
    class function DoTest2: Integer;
    class function DoTest3: Integer;
    class function DoTest4: Integer;
  end;

class function TTest2.Test3: Integer;
begin
  Result := 1;
end;

class function TTest3.Test1: Integer;
begin
  Result := 1;
end;

class function TTest3.Test2: Integer;
begin
  Result := 1;
end;

class function TTest1Helper.Test1: Integer;
begin
  Result := 2;
end;

class function TTest1Helper.Test3: Integer;
begin
  Result := 2;
end;

class function TTest1Helper.Test4: Integer;
begin
  Result := 1;
end;

class function TTest3Helper.Test2: Integer;
begin
  Result := 2;
end;

class function TTest3Helper.Test4: Integer;
begin
  Result := 2;
end;

class function TTest4Helper.DoTest1: Integer;
begin
  Result := Test1;
end;

class function TTest4Helper.DoTest2: Integer;
begin
  Result := Test2;
end;

class function TTest4Helper.DoTest3: Integer;
begin
  Result := Test3;
end;

class function TTest4Helper.DoTest4: Integer;
begin
  Result := Test4;
end;

var
  res: Integer;
begin
  res := TTest4.DoTest1;
  Writeln('TTest4.DoTest1: ', res);
  if res <> 2 then
    Halt(1);
  res := TTest4.DoTest2;
  Writeln('TTest4.DoTest2: ', res);
  if res <> 2 then
    Halt(2);
  res := TTest4.DoTest3;
  Writeln('TTest4.DoTest3: ', res);
  if res <> 2 then
    Halt(3);
  res := TTest4.DoTest4;
  Writeln('TTest4.DoTest4: ', res);
  if res <> 1 then
    Halt(4);
  Writeln('ok');
end.
