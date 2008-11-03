{ %opt=-Oodfa -Sew }
{ %norun }
{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}

program FunctionResultAnalysisWrong;
uses
  SysUtils;

var
  I : longint;

function Test1: Integer;
begin
  raise Exception.Create('');
end; {should NOT generate "Warning: Function result does not seem to be set" (doesn't in delphi, does in FPC) }

function Test2: Integer;
begin
  if i > 0 then
    raise Exception.Create('')
  else
    raise Exception.Create('');
end; {should NOT generate "Warning: Function result does not seem to be set" (doesn't in delphi, does in FPC) }

function Test4: Integer;
begin
  if i > 0 then
    raise Exception.Create('')
  else
    Result := 0;
end; {should NOT generate "Warning: Function result does not seem to be set" (doesn't in delphi and FPC) }

begin
  i:=1;
end.
