program stored;
{$mode objfpc}{$h+}
uses
  SysUtils, Classes;

const
  ShowTheException = true; //set this to false for halt(128) instead of exception

type
  TGLNode = class (TCollectionItem)
  private
    FCoords : array[0..5] of double;
    procedure SetCoordinate(aIndx: Integer; AValue: double);
  protected
    function StoreCoordinate(aIndx: Integer) : Boolean;
  published
    property X: double index 0 read FCoords[0] write SetCoordinate stored StoreCoordinate;
    property Y: double index 1 read FCoords[1] write SetCoordinate stored StoreCoordinate;
    property Z: double index 2 read FCoords[2] write SetCoordinate stored StoreCoordinate;
    property X2: double index 3 read FCoords[3] write SetCoordinate stored true;
    property Y2: double index 4 read FCoords[4] write SetCoordinate stored true;
    property Z2: double index 5 read FCoords[5] write SetCoordinate stored true;
  end;

  { TNodeContainer }

  TNodeContainer = class (TComponent)
    private
      FNodes: TCollection;
      procedure SetNodes(const AValue: TCollection);
    public
      constructor Create(AOwner: TComponent); virtual;
    published
      property Nodes : TCollection read FNodes write SetNodes;
  end;

{ TNodeContainer }

procedure TNodeContainer.SetNodes(const AValue: TCollection);
begin
  if FNodes=AValue then exit;
  FNodes:=AValue;
end;

constructor TNodeContainer.create(AOwner: TComponent);
begin
  inherited create(AOwner);
  fNodes:=TCollection.Create(TGLNode);
end;

{ TGLNode }

procedure TGLNode.SetCoordinate(aIndx: Integer; AValue: double);
begin
  if (aIndx in [0..2]) or ShowTheException then
    FCoords[aIndx]:=AValue
  else begin
    writeln('SetCoordinate called with index=',aIndx);
    halt(128);
  end;
end;

function TGLNode.StoreCoordinate(aIndx: Integer): Boolean;
begin
  if (aIndx in [0..2])  or ShowTheException then
    result:=(PtrUInt((@FCoords[aIndx])^)<>0)
  else begin
    writeln('StoreCoordinate called with index=',aIndx);
    halt(128);
  end;
end;

var gNodes  : TNodeContainer;
    gFile   : TFileStream;
    i : word;

begin
  gNodes:=TNodeContainer.create(nil);
  for i := 1 to 3 do begin
    with (gNodes.Nodes.Add as TGLNode) do begin
      PtrUInt((@x)^):=$FF80 or i;
      PtrUInt((@y)^):=$FFA0 or i;
      PtrUInt((@z)^):=$FFC0 or i;
      PtrUInt((@x2)^):=$FF80 or i;
      PtrUInt((@y2)^):=$FFA0 or i;
      PtrUInt((@z2)^):=$FFC0 or i;
    end;
  end;
  gFile:=TFileStream.Create('testfile.tmp',fmCreate);
  gFile.WriteComponent(gNodes);
  gFile.Free;
  gNodes.Nodes.Clear;
  gFile:=TFileStream.Create('testfile.tmp',fmOpenRead);
  gFile.ReadComponent(gNodes);
  gFile.Free;
  for i := 1 to 3 do begin
    with (gNodes.Nodes.Items[i-1] as TGLNode) do begin
      if PtrUInt((@x)^) <> ($FF80 or i) then begin writeln('Node ',i,' X-Value is wrong: ',hexStr(PtrUInt((@x)^),sizeof(PtrUInt)*2),' but should be ',hexStr($FF80 or i,sizeof(PtrUInt)*2)); halt(128); end;
      if PtrUInt((@y)^) <> ($FFA0 or i) then begin writeln('Node ',i,' Y-Value is wrong: ',hexStr(PtrUInt((@y)^),sizeof(PtrUInt)*2),' but should be ',hexStr($FFA0 or i,sizeof(PtrUInt)*2)); halt(128); end;
      if PtrUInt((@z)^) <> ($FFC0 or i) then begin writeln('Node ',i,' Z-Value is wrong: ',hexStr(PtrUInt((@z)^),sizeof(PtrUInt)*2),' but should be ',hexStr($FFC0 or i,sizeof(PtrUInt)*2)); halt(128); end;
      if PtrUInt((@x2)^) <> ($FF80 or i) then begin writeln('Node ',i,' X-Value is wrong: ',hexStr(PtrUInt((@x)^),sizeof(PtrUInt)*2),' but should be ',hexStr($FF80 or i,sizeof(PtrUInt)*2)); halt(128); end;
      if PtrUInt((@y2)^) <> ($FFA0 or i) then begin writeln('Node ',i,' Y-Value is wrong: ',hexStr(PtrUInt((@y)^),sizeof(PtrUInt)*2),' but should be ',hexStr($FFA0 or i,sizeof(PtrUInt)*2)); halt(128); end;
      if PtrUInt((@z2)^) <> ($FFC0 or i) then begin writeln('Node ',i,' Z-Value is wrong: ',hexStr(PtrUInt((@z)^),sizeof(PtrUInt)*2),' but should be ',hexStr($FFC0 or i,sizeof(PtrUInt)*2)); halt(128); end;
    end;
  end;
  writeln('ok. done.');
  DeleteFile('testfile.tmp');
end.
