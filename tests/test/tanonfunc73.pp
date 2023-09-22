{ %NORUN }

program tanonfunc73;

{$mode objfpc}
{$modeswitch anonymousfunctions}
{$modeswitch functionreferences}

type
  TProc = reference to procedure;

procedure Test;
var
  o: TObject;
  p: TProc;
begin
  with o do begin
    p := procedure
         begin
           Writeln('Hello World');
         end;
  end;
end;

var
  o: TObject;
  p: TProc;
begin
  with o do begin
    p := procedure
         begin
           Writeln('Hello World');
         end;
  end;
end.
