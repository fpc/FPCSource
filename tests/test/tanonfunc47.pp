{ %FAIL }

program tanonfunc47;

{$mode objfpc}
{$modeswitch anonymousfunctions}
{$modeswitch functionreferences}

{ out parameters can't be captured }

type
  TProc = reference to procedure;

procedure Test(out aArg: LongInt);
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
