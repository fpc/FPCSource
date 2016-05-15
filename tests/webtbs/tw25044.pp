{ %NORUN }

program tw25044;

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

type
  TBase = class(TObject)
  end;

  TDescendant = class(TBase)
  end;

  T123456789012345678901 = record
  public
    Value: TDescendant;

    class operator Implicit(Operand: T123456789012345678901): TBase;
    class operator Implicit(Operand: T123456789012345678901): TDescendant;
  end;

{ T123456789012345678901 }

class operator T123456789012345678901.Implicit(Operand: T123456789012345678901): TBase;
begin
  Result := Operand.Value;
end;

class operator T123456789012345678901.Implicit(Operand: T123456789012345678901): TDescendant;
begin
  Result := Operand.Value;
end;

begin
end.
