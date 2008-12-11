{ %wpoparas=devirtcalls,optvmts }
{ %wpopasses=1 }

{$mode objfpc}

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
  a:=1;
  bb.test;
  a:=2;
  bb.free;
  bb:=tchild2.create;
  bb.test;
  bb.free;
end.
