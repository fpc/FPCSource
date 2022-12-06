program tw40019;

{$mode delphi}{$H+}

type
  TArray4IntegerType = array[0..3] of Longint;

  TPoint = record
       X : Longint; Y : Longint;
  end;

  TRect = record
       case Longint of
         0: (Left,Top,Right,Bottom : Longint);
         1: (TopLeft,BottomRight : TPoint);
         2: (Vector:TArray4IntegerType);
  end;

  THead=class
  end;

  TDrawCellInfo=class
      m1,m2:integer;
  end;

  TCanvas=class
    procedure dosmth(arect:TRect);
  end;

  TCellState=set of (A,B,C,D);
  TDrawCellState=set of (xA,xB,xC,xD);

  { TGridObject }

  TGridObject=class
    fCellInfo:TDrawCellInfo;
    procedure GetCellInfo(ACol, ARow:integer; AState:TCellState; AColHead, ARowHead:THead; ACellInfo:TDrawCellInfo);
    procedure DrawCell(APrevCol, APrevRow, ACol, ARow, ANextCol, ANextRow: Integer;
      AColHead, ARowHead: THead;
      AState: TCellState; ARect: TRect; DrawSelection: Boolean; ACanvas: TCanvas);overload;virtual;
    procedure DrawCell(CellInfo: TDrawCellInfo; APrevCol, APrevRow, ACol, ARow, ANextCol, ANextRow: Integer;
      AColHead, ARowHead: THead;
      AState: TCellState; ARect: TRect; DrawSelection: Boolean; ACanvas: TCanvas);

  end;

  procedure TGridObject.GetCellInfo(ACol, ARow: integer; AState: TCellState;
    AColHead, ARowHead: THead; ACellInfo: TDrawCellInfo);
  begin
    AColHead:=ARowHead;
    ACellInfo.m1:=ACellInfo.m2;
    end;

  procedure TGridObject.DrawCell(APrevCol, APrevRow, ACol, ARow, ANextCol, ANextRow: Integer;
      AColHead, ARowHead: THead;
      AState: TCellState; ARect: TRect; DrawSelection: Boolean; ACanvas: TCanvas);
var
  ACellInfo: TDrawCellInfo;
begin
  if ARect.left<>1 then
    halt(1);
  if ARect.right<>2 then
    halt(2);
  if ARect.top<>3 then
    halt(3);
  if ARect.bottom<>4 then
    halt(4);
  ACellInfo := fCellInfo;
  GetCellInfo(ACol, ARow, AState, AColHead, ARowHead, ACellInfo);
  if not Assigned(ACellInfo) then
    ACellInfo := fCellInfo;
  DrawCell(ACellInfo,APrevCol,APrevRow,ACol,ARow,ANextCol,ANextRow,
    AColHead,ARowHead,AState,ARect,false,nil);
end;

procedure TGridObject.DrawCell(CellInfo: TDrawCellInfo; APrevCol, APrevRow, ACol, ARow, ANextCol, ANextRow: Integer;
      AColHead, ARowHead: THead;
      AState: TCellState; ARect: TRect; DrawSelection: Boolean; ACanvas: TCanvas);
var
  //CellInfo: TDrawCellInfo;
  FrameFlags, FrameFlagsOuter, RegionIndex: Integer;



begin
  if drawSelection then Acanvas.dosmth(Arect);
end;

procedure TCanvas.dosmth(arect:TRect);
begin
    if arect.left<>1 then
      halt(21);
    if arect.right<>2 then
      halt(22);
    if arect.top<>3 then
      halt(23);
    if arect.bottom<>4 then
      halt(24);
  arect.left:=arect.top+arect.right
end;

var go:TGridObject;
    r: TRect;
begin
  go:=TGridObject.Create;
  r.left:=1;
  r.right:=2;
  r.top:=3;
  r.bottom:=4;
  go.DrawCell(nil,0,0,0,0,0,0,nil,nil,[],r,true,TCanvas.create);
end.

