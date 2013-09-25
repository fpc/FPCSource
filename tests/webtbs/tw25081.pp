program tw25081;

{$APPTYPE CONSOLE}

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

type
  TLargeCardinal = record
  public
    Low:  Cardinal;
    High: Cardinal;

    class operator Inc(const Operand: TLargeCardinal): TLargeCardinal;
    class operator Dec(const Operand: TLargeCardinal): TLargeCardinal;
  end;

{ TLargeCardinal }

class operator TLargeCardinal.Dec(const Operand: TLargeCardinal): TLargeCardinal;
begin
  Result := Operand;
  Dec(Result.Low);

  if Result.Low = $FFFFFFFF then
    Dec(Result.High);
end;

class operator TLargeCardinal.Inc(const Operand: TLargeCardinal): TLargeCardinal;
begin
  Result := Operand;
  Inc(Result.Low);

  if Result.Low = 0 then
    Inc(Result.High);
end;

var
  Value: TLargeCardinal;

begin
  Value.Low  := $FFFFFFFF;
  Value.High := 0;

  Inc(Value);

  if (Value.Low <> 0) or (Value.High <> 1) then
    Halt(1);

  Dec(Value);

  if (Value.Low <> $FFFFFFFF) or (Value.High <> 0) then
    Halt(1);
end.
