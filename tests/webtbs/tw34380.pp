{ Code extracted from fpc-image fpcolhash unit }

{$mode objfpc}

uses
  sysutils;

type
  PColHashMainNode = ^TColHashMainNode;
  TColHashMainNode = packed record
    childs : array[0..16] of pointer; { can be either another MainNode or a SubNode }
  end;

  TFPColorHashTable = class (TObject)
    function AllocateMainNode : PColHashMainNode;
  end;

function TFPColorHashTable.AllocateMainNode : PColHashMainNode;
var tmp : PColHashMainNode;
    i : byte;
begin
  Result:=nil;
  tmp:=getmem(sizeof(TColHashMainNode));
  if tmp=nil then raise Exception.Create('Out of memory');
  for i:=0 to high(tmp^.childs) do
    tmp^.childs[i]:=nil;
  Result:=tmp;
end;

begin
end.
