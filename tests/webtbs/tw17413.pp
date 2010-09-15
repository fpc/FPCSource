{$mode objfpc}
{$H+}

uses SysUtils;

type
  TVector3Single = array [0..2] of Single;

function Vector3Single(const X, Y, Z: Single): TVector3Single;
begin
  Result[0] := X;
  Result[1] := Y;
  Result[2] := Z;
end;

var
  res1, res2, res3: single;

procedure RenderFromViewEverything;
var
  Normal: TVector3Single;

  procedure DoTexCoordVertex(const Vertex: TVector3Single);
  begin
    Writeln('Normal: ', Normal[0]:1:1, ' ', Normal[1]:1:1, ' ', Normal[2]:1:1);
    if (normal[0]<>res1) or
       (normal[1]<>res2) or
       (normal[2]<>res3) then
      halt(1);
  end;

begin
  res1:=123;
  res2:=456;
  res3:=789;
  Normal := Vector3Single(123, 456,  789);
  DoTexCoordVertex(Vector3Single(111, 222,  333));

  res1:=987;
  res2:=654;
  res3:=321;
  Normal := Vector3Single(987, 654, 321);
  DoTexCoordVertex(Vector3Single(444, 555, 666));
end;

begin
  RenderFromViewEverything;
end.
