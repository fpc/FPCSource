{ %OPT=-Oodfa -Sew -vw }
type
  to1 = object
    procedure Init;
  end;

procedure to1.Init;
  begin
  end;


var
  o1,o2 : to1;

begin
  o1.Init;
  o2:=o1;
end.
