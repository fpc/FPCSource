{ %fail }

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

type
  TWrapper = record
    class operator Implicit(value: Integer): TWrapper;
  end;

class operator TWrapper.Implicit(value: Integer): TWrapper;
begin
end;

procedure TestProcedure(const wr: TWrapper = TWrapper(0));
  { This crashes the compiler, and shouldn’t be allowed }
begin
end;

begin
end.
