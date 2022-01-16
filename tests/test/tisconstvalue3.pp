program tisconstvalue3;

{$IFDEF FPC}
  {$mode Delphi}
{$ENDIF}

type
  TMyClass = class
  const
    PI = 3.14;
  private
    FNumber: Integer;
  public
    function DoMathAndReturn(const AValue: Integer): Integer;
  published
    property MyNumber: Integer read FNumber;
  end;

  TClassOf = class of TMyClass;

  function TMyClass.DoMathAndReturn(const AValue: Integer): Integer;
  begin
    Result := FNumber * 2;
  end;

function WorldCopy(AInput: String): String;
begin
  if IsConstValue(AInput) then
    Halt(9);

  Result := 'Hello ' + AInput;
end;

function WorldConst(const AInput: String): String;
begin
  if IsConstValue(AInput) then
    Halt(10);

  Result := 'Hello ' + AInput;
end;

function WorldVar(var AInput: String): String;
begin
  if IsConstValue(AInput) then
    Halt(11);

  Result := 'Hello ' + AInput;
end;

function WorldOut(out AInput: String): String;
begin
  AInput := 'Test';
  if IsConstValue(AInput) then
    Halt(12);

  Result := 'Hello ' + AInput;
end;

var
  MyClass: TMyClass;
  MyString: String;

const
  SomeClass: TClass = TMyClass;

begin
  if IsConstValue(TMyClass) then
    Halt(1);

  MyClass := TMyClass.Create;
  try
    if IsConstValue(MyClass) then
      Halt(3);

    if IsConstValue(MyClass.MyNumber) then
      Halt(4);

    if not IsConstValue(MyClass.PI) then
      Halt(5);

    if IsConstValue(MyClass.DoMathAndReturn(5)) then
      Halt(6);

    if IsConstValue(@MyClass) then
      Halt(7);
  finally
    MyClass.Free;
  end;

  if IsConstValue(@WorldCopy) then
    Halt(8);

  WorldCopy('World');
  WorldConst('World');
  MyString := 'World';
  WorldVar(MyString);
  WorldOut(MyString);

  if IsConstValue(WorldCopy('World')) then
    Halt(13);

  if IsConstValue(MyString) then
    Halt(14);

  if IsConstValue(@MyString) then
    Halt(15);

  UniqueString(MyString);
  if IsConstValue(MyString) then
    Halt(16);

  if not IsConstValue('Hello') then
    Halt(17);

  if not IsConstValue(3.14) then
    Halt(17);

  if not IsConstValue(12345) then
    Halt(18);

  if not IsConstValue(5 <> 2) then
    Halt(19);

  if not IsConstValue(5 - 5 = 0) then
    Halt(20);

  if IsConstValue(SomeClass) then
    Halt(21);

  Writeln('Ok');
end.
