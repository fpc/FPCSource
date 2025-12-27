{ %NORUN }

program tb0726;

{$mode objfpc}{$H+}

type
  generic TTest1<T> = class
    procedure Test;
  end;

  generic TTest2<T> = class
    procedure Test;
  end;

procedure TTest1.Test;
begin
end;

procedure Test;
type
  TArr = packed array [0..1] of Single;
  TTest1Arr = specialize TTest1<TArr>;
  TTest2Arr = specialize TTest2<TArr>;
var
  a: TTest1Arr;
  b: TTest2Arr;
begin
  a := TTest1Arr.Create;
  a.Free;
  b := TTest2Arr.Create;
  b.Free;
end;

procedure TTest2.Test;
begin
end;

begin

end.
