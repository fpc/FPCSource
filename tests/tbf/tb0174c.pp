{ %fail }
{$mode objfpc}

type
  tobject1 = class
  strict protected
    spro : integer;
  strict private
    spriv : integer;
  public
    procedure p1;
  end;


  tobject2 = class(tobject1)
    procedure p2;
  end;

procedure tobject1.p1;
  begin
    spro:=1;
    spriv:=2;
  end;

procedure tobject2.p2;
  begin
    spro:=3;
  end;

var
  o1 : tobject1;
  o2 : tobject2;

begin
  o1:=tobject1.create;
  o2:=tobject2.create;
  o1.spriv:=1;
  o1.free;
  o2.free;
end.


