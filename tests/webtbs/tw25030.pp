program tw25030;

{$MODE DELPHI}

type

  { TInteger32Boolean }

  TInteger32Boolean = record
  public
    Value: Integer;
    const
      FALSE = 0;
      TRUE = 1;

    class operator Implicit(const Operand: TInteger32Boolean): Boolean;
    class operator Implicit(Operand: Boolean): TInteger32Boolean;
    class operator LogicalNot(const Operand: TInteger32Boolean): TInteger32Boolean;
  end;

{ TInteger32Boolean }

class operator TInteger32Boolean.Implicit(const Operand: TInteger32Boolean): Boolean;
begin
  Result := Operand.Value <> FALSE;
end;

class operator TInteger32Boolean.Implicit(Operand: Boolean): TInteger32Boolean;
begin
  if Operand then
    Result.Value := TRUE
  else
    Result.Value := FALSE;
end;

class operator TInteger32Boolean.LogicalNot(const Operand: TInteger32Boolean): TInteger32Boolean;
begin
  if Operand then
    Result.Value := FALSE
  else
    Result.Value := TRUE;
end;

var
  IntegerBoolean: TInteger32Boolean;

begin
  IntegerBoolean := True;
  if IntegerBoolean.Value<>1 then
    halt(1);

  if not IntegerBoolean then
    halt(2);
end.