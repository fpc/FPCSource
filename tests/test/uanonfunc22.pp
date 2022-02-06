unit uanonfunc22;

{$mode objfpc}
{$modeswitch anonymousfunctions}
{$modeswitch functionreferences}

interface

type
  tproc = reference to procedure;

procedure foo;
procedure bar(p: tproc);

implementation

procedure foo;
var
  i: Integer;
begin
  bar(procedure
    begin
      i := 123;
    end);
  if i <> 123 then
    halt(1);
end;

procedure bar(p: tproc);
begin
  p();
end;

end.

