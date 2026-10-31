unit reloadclass_control;

{$mode objfpc}
{$inline on}

interface

uses reloadclass_base, reloadclass_child;

const Factor = 1;

procedure Test(Value: TBase);

implementation

procedure Test(Value: TBase);
begin
  Accept(Value);
  if Inlined(Value)<>Value then
    Halt(1);
end;

end.