{ %FAIL }

program tanonfunc46;

{$mode objfpc}
{$modeswitch anonymousfunctions}
{$modeswitch functionreferences}

{ var parameters can't be captured }

type
  TProc = reference to procedure;

procedure Test(var aArg: LongInt);
var
  p: TProc;
begin
  p := procedure
       begin
         aArg := 42;
       end;
end;

begin

end.
