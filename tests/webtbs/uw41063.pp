unit uw41063;

{$mode objfpc}{$h+}
{$modeswitch functionreferences}

interface

type
  TMyClass = class
    FProcRef: reference to procedure;
    procedure proc;
  end;

implementation

procedure TMyClass.proc;
begin
  FProcRef := @proc;
end;

end.

