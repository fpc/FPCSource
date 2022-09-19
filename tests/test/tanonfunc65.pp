program tanonfunc65;

{$mode delphi}
{$modeswitch anonymousfunctions}
{$modeswitch functionreferences}

{ test generic anonymous method reference 

  same as tanonfunc34 but with mode delphi
}

type
  TProc<T> = reference to procedure(Arg: T);

procedure Foo;
var
  p: TProc<Integer>;
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

