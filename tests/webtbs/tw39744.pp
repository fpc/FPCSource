{ %OPT=-Oodfa -Sew }
program project2;

{$mode delphi}
{$modeswitch anonymousfunctions}
{$modeswitch functionreferences}

type
  TFuncRef = reference to function (A, B: Integer): Integer;

function CallFunc(const F: TFuncRef; A, B: Integer): Integer; //overload;
begin
  Result := F(A*100, B*100);
end;

function TestRef(A, B, C: Integer): Integer; noinline;
begin
  Result := CallFunc(function (A, B: Integer): Integer begin
      Result := A + B + C;
    end, A, B);
end;

begin
  if TestRef(1, 2, 3) <> 303 then
    Halt(1);
end.
