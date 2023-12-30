{ %FAIL }

program tanonfunc74;

{$mode objfpc}
{$modeswitch anonymousfunctions}
{$modeswitch functionreferences}
{$modeswitch nestedprocvars}

{ verify that nested procedures aren't accessible from anonymous functions
  in the captured procedure }

type
  tproc = reference to procedure;

procedure baz(p: tproc);
begin
  p();
end;

procedure foo;

  procedure bar;
  begin
  end;

type
  TNested = procedure is nested;

begin
  bar;
  baz(procedure var n: TNested; begin n := @bar; end);
end;

begin
  foo;
end.


