unit blocks;

{$mode objfpc}

interface

uses gdk,gtk,classes;

type
  TBlockList = Class;
  TBreakOut = Class;

  TGraphicalObject  = Class(TObject)
    FRect : TGdkRectangle;
  Public
    Function Contains(X,Y : Integer) : Boolean;
    Property Left : SmallInt Read FRect.x Write Frect.x;
    Property Top : SmallInt  Read FRect.y Write Frect.y;
    Property Width : Word  Read Frect.Width Write Frect.Width;
    Property Height : Word  Read Frect.Height Write Frect.Height;
  end;

  TBlock = Class(TGraphicalObject)
  Private
    FMaxHits : Integer;
    FBlockList : TBlockList;
    FGC : PGDKGC;
    FColor : PGDKColor;
    FNeedRedraw : Boolean;
    Procedure CreateGC;
    Function DrawingArea : PGtkWidget;
    Function PixMap : PgdkPixMap;
  Public
    Procedure Draw;
    Function Hit : Boolean;
    Constructor Create (ABlockList : TBlockList);
    Property Color : PGDKColor Read FColor Write FColor;
  end;

  TSprite = Class(TGraphicalObject)
    FPreviousTop,
    FPreviousLeft : Integer;
    FDrawingArea : PGtkWidget;
    FDrawPixMap : PgdkPixmap;
    FPixMap : PgdkPixMap;
    FBitMap : PGdkBitMap;
  Protected
    Procedure CreateSpriteFromData(SpriteData : PPGchar);
    Procedure CreatePixMap; Virtual; Abstract;
    Procedure SavePosition;
  Public
    Constructor Create(DrawingArea: PGtkWidget);
    Procedure Draw;
    Function GetChangeRect (Var Rect : TGDkRectAngle) : Boolean;
    Property PixMap : PgdkPixMap Read FPixMap;
    Property BitMap : PGdkBitMap Read FBitMap;
  end;

  TPad = Class (TSprite)
  Private
    FSlope,
    FSpeed,FCurrentSpeed : Integer;
  Protected
    Procedure CreatePixMap; override;
    Procedure InitialPosition;
  Public
    Constructor Create(DrawingArea: PGtkWidget);
    Procedure Step;
    Procedure GoLeft;
    Procedure GoRight;
    Procedure Stop;
    Property CurrentSpeed : Integer Read FCurrentSpeed;
    Property Speed : Integer Read FSpeed Write FSpeed;
    Property Slope : Integer Read FSlope Write FSlope;
  end;

  TBall = Class (TSprite)
  Private
    FBreakOut : TBreakOut;
    FCurrentSpeedX,
    FCurrentSpeedY : Integer;
    FSpeedfactor : Integer;
  Protected
    Procedure CreatePixMap; override;
    Procedure SetSpeed(Value : Integer);
  Public
    Constructor Create(BreakOut : TBreakOut);
    Procedure Step;
    Procedure IncSpeed (Value: Integer);
    Procedure FlipSpeed (FlipX,FlipY : Boolean);
    Property CurrentSpeedX : Integer Read FCurrentSpeedX Write SetSpeed;
    Property CurrentSpeedY : Integer Read FCurrentSpeedY;
    Property SpeedFactor : INteger Read FSpeedFactor Write FSpeedFactor;
  end;

  TBlockList = Class (TList)
    FTotalRows,FTotalColums,FStartRow,FBlockRows,FSpacing : Byte;
    FBreakOut : TBreakOut;
    FColor : PGDKColor;
    Function DRawingArea : PGTKWidget;
    FPixMap : PGDKPixmap;
  Public
    Constructor Create(BreakOut : TBreakOut);
    Destructor Destroy; override;
    Procedure CheckCollision (Ball: TBall);
    Procedure DrawBlocks;
    Procedure DrawBlocks(Const Area : TGdkRectangle);
    Procedure CreateBlocks;
    Procedure FreeBlocks;
    Property TotalRows : Byte Read FTotalRows Write FTotalRows;
    Property TotalColumns : Byte Read FTotalColums Write FTotalColums;
    Property StartRow : Byte Read FStartRow Write FStartRow;
    Property BlockRows : Byte Read FBlockRows Write FBlockRows;
    Property BlockSpacing : Byte Read FSpacing Write FSpacing;
    Property PixMap : PGDKPixMap Read FPixMap Write FPixMap;
  end;


  TBreakOut = Class(TObject)
  Private
    FLevel : Integer;
    FBalls : Integer;
    FBGGC : PGDKGC;
    FBackGroundColor : PGDKColor;
    FPad : TPad;
    FBall : TBall;
    FBlockList : TBlockList;
    FDrawingArea : PGTKWidget;
    FPixMap : PGDKPixMap;
    Procedure DrawBackGround (Area : TGdkrectAngle);
    Procedure DrawBoard(Exposed : PGdkEventExpose);
    Procedure CreateGC;
    Procedure CreatePixMap;
    Procedure CopyPixMap(Area : TGdkRectangle);
    Procedure CheckCollision;
    Procedure FreeBall;
    Procedure NextLevel;
    Procedure NextBall;
    Procedure GameOver;
    Procedure LostBall;
    Procedure Redrawgame;
  Public
    Constructor Create (DrawingArea : PGtkWidget);
    Procedure Draw(Exposed : PGDKEventExpose);
    Procedure Step;
    Property BlockList : TBlockList Read FBlockList;
    Property Pad : TPad Read FPad;
    Property Level : Integer Read Flevel;
    Property Balls : Integer Read FBalls Write FBalls;
  end;

Const
  HitAccelleration   = 1;
  LevelAccelleration = 2;
  FMaxXspeed = 90;


implementation

{ ---------------------------------------------------------------------
    TGraphicalObject implementation
  ---------------------------------------------------------------------}

Function TGraphicalObject.Contains(X,Y : Integer) : Boolean;

begin
  Result:=((X>=Left) and (X<Left+Width)) and
          ((Y>=top) and (Y<Top+Width));
end;

{ ---------------------------------------------------------------------
    TBlock implementation
  ---------------------------------------------------------------------}


Constructor TBlock.Create (ABlockList : TBlockList);

begin
  Inherited Create;
  FBlockList:=ABlockList;
  FMaxHits:=1;
end;

Function TBlock.DrawingArea : PGtkWidget;

begin
  Result:=FBlockList.FBreakout.FDrawingArea;
end;

Function TBlock.PixMap : PgdkPixMap;

begin
  Result:=FBlockList.PixMap;
end;

Procedure TBlock.CreateGC;

begin
  FGC:=gdk_gc_new(DrawingArea^.Window);
  gdk_gc_set_foreground(FGC,FColor);
  gdk_gc_set_fill(FGC,GDK_SOLID);
  FNeedRedraw:=True;
end;

Procedure TBlock.Draw;

begin
  if FGC=Nil then
    CreateGC;
  if FNeedRedraw Then
    begin
    gdk_draw_rectangle(PGDKDrawable(Pixmap),FGC,-1,Left,Top,Width,Height);
    FNeedRedraw:=False;
   end;
end;

Function TBlock.Hit : Boolean;

begin
  Dec(FMaxHits);
  Result:=FMaxHits=0;
  If Result then
    begin
    FBlockList.FBreakOut.DrawBackground(FRect);
    FBlockList.Remove(Self);
    Free;
    end;
end;

{ ---------------------------------------------------------------------
    TBlockList implementation
  ---------------------------------------------------------------------}

Constructor TBlockList.Create(BreakOut : TBreakOut);

begin
  FBreakOut:=BreakOut;
end;

Function TBlockList.DrawingArea : PGtkWidget;

begin
  Result:=FBreakOut.FDrawingArea;
end;

Destructor TBlockList.Destroy;

begin
  If FColor<>Nil then
    FreeMem(FColor);
  FreeBlocks;
end;

Procedure TBlockList.DrawBlocks;

Var
  I : Longint;

begin
  If Count=0 then
    CreateBlocks;
  For I:=0 to Count-1 do
    TBlock(Items[i]).draw;
end;

Procedure TBlockList.DrawBlocks (Const Area : TGdkRectangle);

Var
  i : longint;
  inters : TgdkRectangle;

begin
  For I:=0 to Count-1 do
    With TBlock(Items[i]) do
      FNeedRedraw:=gdk_rectangle_intersect(@area,@Frect,@inters)<>0;
  DrawBlocks;
end;

Function AllocateColor(R,G,B : Integer; Widget : PGtkWidget) : PGdkColor;

begin
  Result:=New(PgdkColor);
  With Result^ do
    begin
    Pixel:=0;
    Red:=R;
    Blue:=B;
    Green:=G;
    end;
  gdk_colormap_alloc_color(gtk_widget_get_colormap(Widget),Result,true,False);
end;

Procedure TBlockList.CreateBlocks;

Var
  TotalHeight,TotalWidth,
  Cellheight,CellWidth,
  I,J : Integer;
  Block : TBlock;
  Min : Byte;

begin
  FColor:=AllocateColor(0,0,$ffff,DrawingArea);
  Min:=FSpacing div 2;
  If Min<1 then
    Min:=1;
  TotalWidth:=Drawingarea^.Allocation.Width;
  TotalHeight:=DrawingArea^.Allocation.Height;
  Cellheight:=TotalHeight Div TotalRows;
  CellWidth:=TotalWidth div TotalColumns;
  For I:=StartRow to StartRow+BlockRows-1 do
    For J:=0 to TotalColumns-1 do
    begin
    Block:=TBlock.Create(Self);
    With Block do
      begin
      Top:=TotalHeight-(CellHeight*I)+Min;
      Left:=(CellWidth*J)+min;
      Width:=CellWidth-2*min;
      Height:=CellHeight-2*min;
      Color:=Self.FColor;
      FNeedRedraw:=True;
      end;
    add(Block);
    end;
end;

Procedure TBlockList.FreeBlocks;

Var
  I : longint;

begin
  For I:=Count-1 downto 0 do
    begin
    TBlock(Items[i]).Free;
    Delete(i);
    end;
end;

Procedure TBlockList.CheckCollision (Ball: TBall);

var
  brect,ints : tgdkrectangle;
  B : TBlock;
  i : integer;
  flipx,flipy : Boolean;

begin
  For I:=Count-1 downto 0 do
    begin
    B:=TBlock(Items[i]);
    BRect:=B.FRect;
    if gdk_rectangle_intersect(@Ball.Frect,@BRect,@ints)<>0 then
      begin
      FlipY:=((Ball.FpreviousTop>=(B.Top+B.Height)) and (Ball.CurrentSpeedY<0)) or
             ((Ball.FpreviousTop+Ball.Height<=B.Top) and (Ball.CurrentSpeedY>0));
      FlipX:=Not FlipY;
      If FlipX then
        FlipX:=((Ball.FPreviousLeft>=(B.Left+B.Width)) and (Ball.CurrentSpeedX<0)) or
               (((Ball.FPreviousLeft+Ball.Width)<=B.Left) and (Ball.CurrentSpeedX>0));
      Ball.FlipSpeed(FlipX,Flipy);
      if B.Hit and not (Count=0) then
        gtk_widget_draw(DrawingArea,@BRect);
      Break;
      end;
    end;
end;

{ ---------------------------------------------------------------------
    TSprite implementation
  ---------------------------------------------------------------------}

Constructor TSprite.Create(DrawingArea: PGtkWidget);

begin
  Inherited Create;
  FDrawingArea:=DrawingArea;
end;

Procedure TSprite.CreateSpriteFromData(SpriteData : PPGChar);

begin
  FPixMap:=gdk_pixmap_create_from_xpm_d(FDrawingArea^.Window,
                                        @FBitmap,
                                        Nil,
                                        SpriteData);
end;

Procedure TSprite.Draw;

Var
  gc : PGDKGc;

begin
  if FPixMap=Nil then
    CreatePixMap;
  gc:=gtk_widget_get_style(FDrawingArea)^.fg_gc[GTK_STATE_NORMAL];
  gdk_gc_set_clip_origin(gc,Left,Top);
  gdk_gc_set_clip_mask(gc,FBitmap);
  if FDrawPixMap<>Nil then
    gdk_draw_pixmap(FDrawPixMap,gc,FPixMap,0,0,Left,Top,Width,Height)
  else
    gdk_draw_pixmap(FDrawPixMap{FDrawingArea^.window},gc,FPixMap,0,0,Left,Top,Width,Height);
  gdk_gc_set_clip_mask(gc,Nil);
end;

Function TSprite.GetChangeRect (Var Rect : TGDkRectAngle) : Boolean;

begin
  Result:=(FPreviousLeft<>Left) or (FPreviousTop<>Top);
  If Result then
    With Rect do
      begin
      x:=FPreviousLeft;
      y:=FPreviousTop;
      Width:=Abs(Left-FPreviousLeft)+self.Width;
      height:=Abs(Top-FPreviousTop)+self.Height;
      end;
end;

Procedure TSprite.SavePosition;

begin
  FPreviousLeft:=Left;
  FPreviousTop:=Top;
end;


{ ---------------------------------------------------------------------
    TPad implementation
  ---------------------------------------------------------------------}

Const
  PadHeight = 10;
  PadWidth = 40;
  PadBitmap : Array[1..13] of pchar = (
    '40 10 2 1',
    '  c none',
    'x c #ff0000',
    'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx',
    'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx',
    'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx',
    'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx',
    'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx',
    'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx',
    'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx',
    'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx',
    'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx',
    'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
  );

Constructor TPad.Create(DrawingArea: PGtkWidget);

begin
  Inherited Create(DrawingArea);
  FSpeed:=6;
  FSlope:=50;
end;

Procedure TPad.CreatePixMap;

begin
  CreateSpriteFromData(@PadBitmap[1]);
  Width:=PadWidth;
  Height:=PadHeight;
  InitialPosition;
end;

Procedure TPad.InitialPosition;

begin
  Left:=(FDrawingArea^.Allocation.Width-Width) div 2;
  Top:=FDrawingArea^.Allocation.Height-(2*Height);
  FCurrentSpeed:=0;
end;

Procedure TPad.Step;

begin
  SavePosition;
  Left:=Left+FCurrentSpeed;
  if Left<=0 then
    begin
    FCurrentSpeed:=-FCurrentSpeed;
    Left:=0;
    end
  else if Left+Width>=FDrawingArea^.allocation.width then
    begin
    FCurrentSpeed:=-FCurrentSpeed;
    Left:=FDrawingArea^.allocation.width-Width;
    end;
end;

Procedure TPad.GoLeft;

begin
  FCurrentSpeed:=-FSpeed;
end;

Procedure TPad.GoRight;

begin
  FCurrentSpeed:=FSpeed;
end;

Procedure TPad.Stop;

begin
  FCurrentSpeed:=0;
end;

{ ---------------------------------------------------------------------
    TBall implementation
  ---------------------------------------------------------------------}

Const
  BallHeight = 10;
  BallWidth = 10;
  BallBitmap : Array[1..13] of pchar = (
    '10 10 2 1',
    '  c none',
    'x c #ffffff',
    '    xx    ',
    '  xxxxxx  ',
    ' xxxxxxxx ',
    ' xxxxxxxx ',
    'xxxxxxxxxx',
    'xxxxxxxxxx',
    ' xxxxxxxx ',
    ' xxxxxxxx ',
    '  xxxxxx  ',
    '    xx    '
  );


Constructor TBall.Create(BreakOut : TBreakOut);

begin
  Inherited Create(BreakOut.FDrawingArea);
  FBreakOut:=breakout;
  FCurrentSpeedY:=-100;
  FCurrentSpeedX:=0;
  FSpeedFactor:=10;
end;

Procedure TBall.CreatePixMap;

begin
  CreateSpriteFromData(@BallBitmap[1]);
  Width:=BallWidth;
  Height:=BallHeight;
end;


Procedure TBall.Step;

begin
  SavePosition;
  Left :=Left + Round((FCurrentSpeedX*FSpeedFactor/100));
  Top  :=Top  + Round((FCurrentSpeedY*FSpeedFactor/100));
  if Left<=1 then
    begin
    FlipSpeed(True,False);
    Left:=1;
    end
  else if Left+Width>=FDrawingArea^.allocation.width then
    begin
    FlipSpeed(True,False);
    Left:=FDrawingArea^.allocation.width-Width-1;
    end;
  if Top<=1 then
    begin
    FlipSpeed(False,True);
    Top:=1;
    end
  else if Top+Height>=FDrawingArea^.allocation.Height then
    FBreakOut.LostBall
end;

Procedure TBall.SetSpeed(Value : Integer);

begin
  If Value<-FMaxXspeed then
    Value:=-FMaxXSpeed
  else if Value>FMaxXspeed then
    Value:=FMaxXspeed;
  FCurrentSpeedX:=Value;
  If FCurrentSpeedY>0 then
    FCurrentSpeedY:=100-Abs(FCurrentSpeedX)
  else
    FCurrentSpeedY:=-100+Abs(FCurrentSpeedX);
end;

Procedure TBall.IncSpeed (Value: Integer);

begin
  FSpeedFactor:=FSpeedFactor+Value;
  If FSpeedFactor<10 then
    FSpeedFactor:=10;
end;

Procedure TBall.FlipSpeed (FlipX,FlipY : Boolean);

begin
  If FlipX then
    FCurrentSpeedX:=-FCurrentSpeedX;
  If FlipY then
    FCurrentSpeedY:=-FCurrentSpeedY;
end;

{ ---------------------------------------------------------------------
    TBreakout implementation
  ---------------------------------------------------------------------}

Constructor TBreakOut.Create (DrawingArea : PGtkWidget);

begin
  FDrawingArea:=DrawingArea;
  FBlockList:=TBlockList.Create (Self);
  FPad:=TPad.Create(FDrawingArea);
  FBalls:=5;
end;


Procedure TBreakOut.CheckCollision;

Var
  Inters :TGdkrectangle;

begin
  If Assigned(FBall) then
   begin
   if gdk_rectangle_intersect(@FBall.FRect,@FPad.Frect,@inters)<>0 then
     If (FBall.FPreviousTop<FPad.Top) and (FBall.FCurrentSpeedY>0) then
       begin
       FBall.FlipSpeed(False,True);
       If (FPad.CurrentSpeed<>0) then
         if (FBall.FCurrentSpeedX*FPad.CurrentSpeed)>0 then
           FBall.IncSpeed(HitAccelleration)
         else
           FBall.IncSpeed(-HitAccelleration);
       FBall.CurrentSpeedX:=FBall.CurrentSpeedX+(Round(((FBall.Left+(FBall.Width div 2)) - (FPad.left+Fpad.Width div 2)) * (FPad.Slope / 100)));
       end;
   FBlockList.CheckCollision(FBall);
   end;
end;

Procedure TBreakOut.Step;

begin
  FPad.Step;
  If Assigned(FBall) then
    FBall.Step;
  CheckCollision;
  If FBlockList.Count=0 then
    NextLevel;
  if Not Assigned(FBall) and (FBalls=0) then
    GameOver;
end;

Procedure TBreakOut.CreateGC;

begin
  FBGGC:=gdk_gc_new(FDrawingArea^.Window);
  FBackGroundColor:=AllocateColor(0,0,0,FDrawingArea);
  gdk_gc_set_foreground(FBGGC,FBackGroundColor);
  gdk_gc_set_fill(FBGGC,GDK_SOLID);
end;

Procedure TBreakOut.DrawBackGround (Area : TGdkrectAngle);

begin
  With Area do
    begin
    gdk_draw_rectangle(PGDKDrawable(FPixMap),FBGGC,-1,x,y,Width+1,Height+1);
    end;
end;

Procedure TBreakOut.DrawBoard(Exposed : PGdkEventExpose);

begin
  If FBGGC=Nil then
    begin
    CreateGC;
    end;
  DrawBackGround(Exposed^.Area);
end;

Procedure TBreakOut.CreatePixMap;

begin
  If FPixMap<>Nil then
    GDK_pixmap_unref(FPixMap);
  With FDrawingArea^ do
    FPixMap:=gdk_pixmap_new(Window,Allocation.Width,Allocation.Height,-1);
  FBlockList.PixMap:=FPixMap;
  FPad.FDrawPixMap:=FPixMap;
  If Assigned(FBall) then
    FBall.FDrawPixMap:=FPixMap;
end;

Procedure TBreakOut.CopyPixMap(Area : TGdkRectangle);

begin
  gdk_draw_pixmap(FDrawingArea^.Window,
                  gtk_widget_get_style(FDrawingArea)^.fg_gc[GTK_WIDGET_STATE(FDrawingArea)],
                  FPixMap,
                  area.x,area.y,
                  area.x,area.y,
                  area.width,area.height);
end;

Procedure TBreakOut.Draw(Exposed : PGDKEventExpose);

Var
  Rect : TGdkRectangle;

begin
  if FPixMap=Nil then
    CreatePixMap;
  // draw whatever needed on pixmap.
  if Exposed<>Nil then
    begin
    DrawBoard(Exposed);
    FBlockList.DrawBlocks(exposed^.area)
    end
  else
    begin
    If Assigned(FBall) then
      if FBall.GetChangeRect(Rect) then
        begin
        DrawBackground(Rect);
        FBLockList.drawBlocks(Rect);
        end;
    if FPad.GetChangeRect(Rect) then
      DrawBackground(Rect)
    end;

  FPad.Draw;
  if Assigned(FBall) Then
    FBall.draw;

  If Exposed<>Nil then
    CopyPixMap(Exposed^.Area);
  If assigned(FBall) then
    if FBall.GetChangeRect(Rect) then
      CopyPixMap(Rect);
  if FPad.GetChangeRect(Rect) then
    CopyPixMap(Rect);
  IF Assigned(FBall) then
    CopyPixMap(FBall.FRect);
  CopyPixMap(FPad.FRect);
end;


Procedure TBreakout.Redrawgame;

Var
  Rect : TgdkRectangle;

begin
  Rect.X:=FDrawingArea^.allocation.x;
  Rect.Y:=FDrawingArea^.allocation.y;
  Rect.Width:=FDrawingArea^.allocation.Width;
  Rect.Height:=FDrawingArea^.allocation.Height;
  gtk_Widget_draw(FDrawingArea,@rect)
end;


Procedure TBreakOut.FreeBall;

begin
  FBall.Free;
  FBall:=Nil;
end;

Procedure TbreakOut.NextBall;

begin
  If FBall=Nil then
    begin
    FBall:=TBall.Create(Self);
    FBall.Top:=FPad.Top-1;
    FBall.Left:=FPad.Left + (FPad.Width div 2);
    FBall.CurrentSpeedX:=FPad.CurrentSpeed*5;
    FBall.FPreviousTop:=FBall.Top;
    FBall.FPreviousLeft:=FBall.Left;
    FBall.FDrawPixMap:=Self.FPixMap;
    FBall.Draw;
    end;
end;

Procedure TBreakOut.NextLevel;

Var
  Area : TGdkRectangle;

begin
  If Assigned(FBall) then
    FreeBall;
  FPad.FSpeed:=FPad.Speed+LevelAccelleration;
  FPad.InitialPosition;
  RedrawGame;
end;

Procedure TBreakout.LostBall;

begin
  Dec(FBalls);
  If FBalls=0 then
    GameOver;
  FreeBall;
  Fpad.InitialPosition;
  RedrawGame;
end;

Procedure TBreakout.GameOver;

begin
end;

end.

