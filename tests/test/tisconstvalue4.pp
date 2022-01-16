{ %FAIL }
program tisconstvalue4;

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

begin
  // Error: type identifier not allowed here
  if IsConstValue(TClassOf) then
    Halt(1);

  Writeln('Ok');
end.

