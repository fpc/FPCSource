{ %fail }

 unit tw34821;

{$mode objfpc}{$H+}

interface

type TStrBuilder = class
  procedure append(); inline;
end;

type TTest = object
  procedure xyz;
end;

implementation

procedure TStrBuilder.append();
begin
  TTest.xyz;
end;

procedure TTest.xyz;
begin
end;

end.

