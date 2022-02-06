{ %FAIL }

program tanonfunc48;

{$mode objfpc}
{$modeswitch anonymousfunctions}
{$modeswitch functionreferences}

{ var parameters can't be captured }

type
  TProc = reference to procedure;

procedure Test(var aArg);
var
  p: TProc;
begin
  p := procedure
       begin
         PLongInt(@aArg)^ := 42;
       end;
end;

begin

end.
