{ %FAIL }

program tanonfunc49;

{$mode objfpc}
{$modeswitch anonymousfunctions}
{$modeswitch functionreferences}

{ out parameters can't be captured }

type
  TProc = reference to procedure;

procedure Test(out aArg);
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
