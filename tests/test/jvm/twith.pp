program twith;

{$mode delphi}

type
  twithbase = class
  end;

   twithchild = class(twithbase)
    procedure test; virtual;
  end;

procedure twithchild.test;
begin
end;


function func: twithbase;
begin
  result:=twithchild.create;
end;


begin
  with twithchild(func) do
    test;
end.
