program tanonfunc34;

{$mode objfpc}
{$modeswitch anonymousfunctions}
{$modeswitch functionreferences}

{ test generic anonymous method reference }

type
  generic TProc<T> = reference to procedure(Arg: T);

procedure Foo;
var
  p: specialize TProc<Integer>;
  acc: Integer;
begin
  p := procedure(Arg: Integer)
  begin
    Inc(acc, Arg);
  end;
  acc := 1;
  p(2);
  if acc <> 3 then
    halt(1);
end;

begin
  Foo;
end.

