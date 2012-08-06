{ %fail }

{$IFDEF FPC}
  {$MODE DELPHI}
{$ENDIF}

type
  TWrapper = record end;

procedure TestProcedure(const wr: TWrapper = 0);
  { This shouldn’t be allowed; try also 0.0, '0', nil, False, reNone, etc. }
begin
end;

begin
end.
