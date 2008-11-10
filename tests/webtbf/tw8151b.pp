{ %opt=-Oodfa -Sew -vw }
{ %fail }
{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}

program FunctionResultAnalysisWrong;
uses
  SysUtils;

var
  I : longint;

function Test3: Integer;
begin
  if i > 0 then
    raise Exception.Create('')
end; {SHOULD generate "Warning: Function result does not seem to be set", and does in both delphi and FPC}

begin
  i:=1;
end.
