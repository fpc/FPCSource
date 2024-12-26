program toperator96;

{$mode objfpc}
{$modeswitch advancedrecords}

type
  ITestIntf = interface
  ['{4E8F3222-928F-427C-91D9-C499F8A73693}']
    procedure Test;
  end;

  TTestObject = class(TInterfacedObject, ITestIntf)
    procedure Test;
  end;

  TTestObject2 = class
  end;

  TOvldType = (otNone, otTObject, otIUnknown, otPointer);

  TTest1 = record
    t: TOvldType;
    class operator := (aArg: TObject): TTest1;
    class operator := (aArg: IUnknown): TTest1;
    class operator := (aArg: Pointer): TTest1;
  end;

  TTest2 = record
    t: TOvldType;
    class operator := (aArg: IUnknown): TTest2;
    class operator := (aArg: Pointer): TTest2;
  end;

  TTest3 = record
    t: TOvldType;
    class operator := (aArg: TObject): TTest3;
    class operator := (aArg: Pointer): TTest3;
  end;

  TTest4 = record
    t: TOvldType;
    class operator := (aArg: TObject): TTest4;
    class operator := (aArg: IUnknown): TTest4;
  end;

procedure TTestObject.Test;
begin
end;

class operator TTest1. := (aArg: TObject): TTest1;
begin
  Result.t := otTObject;
end;

class operator TTest1. := (aArg: IUnknown): TTest1;
begin
  Result.t := otIUnknown;
end;

class operator TTest1. := (aArg: Pointer): TTest1;
begin
  Result.t := otPointer;
end;

class operator TTest2. := (aArg: IUnknown): TTest2;
begin
  Result.t := otIUnknown;
end;

class operator TTest2. := (aArg: Pointer): TTest2;
begin
  Result.t := otPointer;
end;

class operator TTest3. := (aArg: TObject): TTest3;
begin
  Result.t := otTObject;
end;

class operator TTest3. := (aArg: Pointer): TTest3;
begin
  Result.t := otPointer;
end;

class operator TTest4. := (aArg: TObject): TTest4;
begin
  Result.t := otTObject;
end;

class operator TTest4. := (aArg: IUnknown): TTest4;
begin
  Result.t := otIUnknown;
end;

var
  o: TTestObject;
  o2: TTestObject2;
  t1: TTest1;
  t2: TTest2;
  t3: TTest3;
  t4: TTest4;
  i: IUnknown;
begin
  o := TTestObject.Create;
  o2 := TTestObject2.Create;
  i := o;

  t1 := o;
  Writeln('Test1 o: ', t1.t);
  if t1.t <> otTObject then
    Halt(1);

  t2 := o;
  Writeln('Test2 o: ', t2.t);
  if t2.t <> otIUnknown then
    Halt(2);

  t3 := o;
  Writeln('Test3 o: ', t3.t);
  if t3.t <> otTObject then
    Halt(3);

  t4 := o;
  Writeln('Test4 o: ', t4.t);
  if t4.t <> otTObject then
    Halt(4);

  t1 := i;
  Writeln('Test1 i: ', t1.t);
  if t1.t <> otIUnknown then
    Halt(5);

  t2 := i;
  Writeln('Test2 i: ', t2.t);
  if t2.t <> otIUnknown then
    Halt(6);

  t3 := i;
  Writeln('Test3 i: ', t3.t);
  if t3.t <> otPointer then
    Halt(7);

  t4 := i;
  Writeln('Test4 i: ', t4.t);
  if t4.t <> otIUnknown then
    Halt(8);

  t1 := o2;
  Writeln('Test1 o2: ', t1.t);
  if t1.t <> otTObject then
    Halt(9);

  t2 := o2;
  Writeln('Test2 o2: ', t2.t);
  if t2.t <> otPointer then
    Halt(10);

  t3 := o2;
  Writeln('Test3 o2: ', t3.t);
  if t3.t <> otTObject then
    Halt(11);

  t4 := o2;
  Writeln('Test4 o2: ', t4.t);
  if t4.t <> otTObject then
    Halt(12);

  i := Nil;
  o2.Free;
end.
