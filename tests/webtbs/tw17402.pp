{$mode objfpc}
{$H+}

uses SysUtils;

type
  TVRMLGeometryNode = class
  public
    function TrianglesCount(ProxyGeometry: TVRMLGeometryNode): Cardinal; virtual;
  end;

  TNodeCylinder = class(TVRMLGeometryNode)
  public
  end;

  TNodeIndexedFaceSet = class(TVRMLGeometryNode)
  public
    function TrianglesCount(ProxyGeometry: TVRMLGeometryNode): Cardinal; override;
  end;

function TVRMLGeometryNode.TrianglesCount(ProxyGeometry: TVRMLGeometryNode): Cardinal;
begin
  Writeln(ClassName, ': Default TrianglesCount implementation, passes the call to the Proxy or must be overridden');
  if ProxyGeometry <> nil then
    Result := ProxyGeometry.TrianglesCount(nil) else
    raise Exception.CreateFmt('%s: TrianglesCount not overridden, and node without a Proxy', [ClassName]);
end;

function TNodeIndexedFaceSet.TrianglesCount(ProxyGeometry: TVRMLGeometryNode): Cardinal;
begin
  Result := 2;
end;

var
  C: TNodeCylinder;
  I: TNodeIndexedFaceSet;
begin
  C := TNodeCylinder.Create;
  I := TNodeIndexedFaceSet.Create;
  try
    Writeln(C.TrianglesCount(I));
  finally
    FreeAndNil(C);
    FreeAndNil(I);
  end;
end.
