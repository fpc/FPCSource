{ %version=1.1}
{ %opt=-vw }
{$mode objfpc}
type
  to1 = class
    procedure p1;
    procedure p2;virtual;
    procedure p3;
  end;

  to2 = class(to1)
    procedure p1;
    procedure p2;virtual;reintroduce;
    procedure p3;virtual;
  end;

procedure to1.p1;
  begin
  end;


procedure to1.p2;
  begin
  end;


procedure to1.p3;
  begin
  end;


procedure to2.p1;
  begin
  end;


procedure to2.p2;
  begin
  end;


procedure to2.p3;
  begin
  end;


begin
end.
