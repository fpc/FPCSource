{ %opt=-Oodfa -Sew }
{ %fail }
{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}

program FunctionResultAnalysisWrong;
uses
  SysUtils;

var
  I : longint;

function Test5: Integer;
begin
  if i > 0 then
    Result := 0;
end; {SHOULD generate "Warning: Function result does not seem to be set", does in delphi, not in FPC }


begin
  i:=1;
end.
