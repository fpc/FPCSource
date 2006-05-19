program project1;

{$mode objfpc}{$H+}

type
  generic TGenTest1<T> = class
   public
    procedure One(const a: T);
    function Two: T;
  end;

procedure TGenTest1.One(const a: T);
begin

end;

function TGenTest1.Two: T; // fails here
begin
end;

begin
end.
