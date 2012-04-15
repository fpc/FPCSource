{ based on the file attached to bug 20627, but modified for usage in FPC's
  testsuite }

{$MODE delphi}

type
  TWrapper<TValue> = class
  strict private
    FValue: TValue;
  public
    type
      TWrapperState = class
      strict private
        FValue: TValue;
      public
        property Value: TValue read FValue write FValue;
        function GetValueSize: Integer;
          { The compiler will report that forward declaration
            TWrapper$LongInt.TWrapperState.GetValueSize is not resolved. }
      end;
  public 
    property Value: TValue read FValue write FValue;
    function CaptureState: TWrapperState;
  end;

function TWrapper<TValue>.CaptureState: TWrapperState;
begin
  Result := TWrapperState.Create;
  Result.Value := FValue;
end;

function TWrapper<TValue>.TWrapperState.GetValueSize: Integer;
begin
  Result := SizeOf(FValue);
end;


begin
  with TWrapper<Integer>.Create do begin
    Value := 123;
    with CaptureState do begin
      if GetValueSize <> SizeOf(Integer) then
        Halt(1);
      Free;
    end;
    Free;
  end;
end.
