{ %norun }
{$mode objfpc}{$h+}
{$implicitexceptions off}

{ Test compilation of leaf function with managed parameter/local and implicit exceptions disabled. }
type
  TCodeTreeNodeDesc = word;

  TCodeTreeNode = class
    Parent: TCodeTreeNode;
    Desc: TCodeTreeNodeDesc;
    function GetNodeOfTypes(Descriptors: array of TCodeTreeNodeDesc): TCodeTreeNode;
  end;


function TCodeTreeNode.GetNodeOfTypes(Descriptors: array of TCodeTreeNodeDesc
  ): TCodeTreeNode;
var
  i: Integer;
begin
  Result:=Self;
  while (Result<>nil) do begin
    for i:=Low(Descriptors) to High(Descriptors) do
      if Result.Desc=Descriptors[i] then exit;
    Result:=Result.Parent;
  end;
end;


procedure test;
var
  s: string;
begin
  pointer(s):=nil;
end;  


begin
end.
