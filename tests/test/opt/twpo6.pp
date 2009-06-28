{ %wpoparas=devirtcalls,optvmts }
{ %wpopasses=1 }

{$mode objfpc}
{$m+}

{ check that multiple descendents properly mark parent class method as
  non-optimisable
}

type
  tbase = class
    procedure test; virtual;
  end;

  tchild1 = class(tbase)
    procedure test; override;
  end;

  tchild2 = class(tbase)
   published
    procedure test; override;
  end;

procedure tbase.test;
begin
  halt(1);
end;

var
  a: longint;

procedure tchild1.test;
begin
  if a<>1 then
    halt(2);
end;

procedure tchild2.test;
begin
  if a<>2 then
    halt(3);
end;

var
  bb: tbase;
begin
  bb:=tchild1.create;
  if (bb is tchild2) then
    halt(1);
end.
