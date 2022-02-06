unit uanonfunc21;

{$mode objfpc}
{$modeswitch functionreferences}

interface

type
  tproc = reference to procedure;

procedure bar(p: tproc);

implementation

procedure bar(p: tproc);
begin
  p();
end;

end.

