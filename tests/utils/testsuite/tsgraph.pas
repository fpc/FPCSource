unit tsgraph;

{$mode ObjFPC}

interface

uses
  Classes, SysUtils, tssql, ftFont,fpimage,fpimgcanv,fpWritePng,fpcanvas;

Type

  { TTestSuiteGraph }

  TTestSuiteGraph = class(TObject)
  Private
    FVars : TQueryData;
  Protected
    procedure DoDrawPie(Img: TFPCustomImage; Skipped, Failed, Total: Integer);
  Public
    constructor create(aVars : TQueryData);
    procedure DrawPie(aPNGImage: TStream; Skipped, Failed, Total: Integer);
  end;

implementation


constructor TTestSuiteGraph.create(aVars: TQueryData);
begin
  fVars:=aVars;
  ftFont.InitEngine;
  FontMgr.SearchPath:='/usr/share/fonts/truetype/liberation/';
end;

procedure TTestSuiteGraph.DrawPie(aPNGImage: TStream; Skipped, Failed, Total: Integer);

var
  lImg : TFPMemoryImage;
  lWriter: TFPWriterPNG;

begin
  lWriter:=Nil;
  lImg:=TFPMemoryImage.Create(320,320);
  try
    DoDrawPie(lImg,Skipped,Failed,Total);
    lWriter:=TFPWriterPNG.Create;
    lWriter.UseAlpha:=True;
    lWriter.ImageWrite(aPNGImage,lImg);
    aPNGImage.Position:=0;
  finally
    lWriter.Free;
    lImg.Free;
  end;
end;

procedure TTestSuiteGraph.DoDrawPie(Img: TFPCustomImage; Skipped, Failed, Total: Integer);

Var
  Cnv : TFPImageCanvas;

  Procedure AddPie(X,Y,R : Integer; AStart,AStop : Double; Col : TFPColor);

  Var
    DX,Dy : Integer;

  begin
    DX:=Round(R*Cos(AStart));
    DY:=Round(R*Sin(AStart));
    Cnv.Line(X,Y,X+DX,Y-DY);
    DX:=Round(R*Cos(AStop));
    DY:=Round(R*Sin(AStop));
    Cnv.Line(X,Y,X+DX,Y-Dy);
    DX:=Round(R/2*Cos((AStart+AStop)/2));
    DY:=Round(R/2*Sin((AStart+AStop)/2));
    Cnv.Brush.FpColor:=Col;
    Cnv.FloodFill(X+DX,Y-DY);
  end;

  Function FractionAngle(F,T : Integer): Double;

  begin
    Result:=(2*Pi*(F/T))
  end;

Var
  W,H,FH,CR,RA : Integer;
  A1,A2,FR,SR,PR : Double;
  R : TRect;
  F : TFreeTypeFont;

begin
  F:=TFreeTypeFont.Create;
  With F do
    begin
    Name:='LiberationSans-Regular.ttf';
    FontIndex:=0;
    Size:=12;
    FPColor:=colred;
    AntiAliased:=False;
    Resolution:=96;
    end;
  if FVars.Debug then
    Writeln(stdout,'Creating image');
  Cnv:=TFPImageCanvas.Create(Img);
  if FVars.Debug then
    Writeln(stdout,'CNV=0x',hexstr(ptruint(cnv),16));

  if FVars.Debug then
   Writeln(stdout,'Getting width and height');
  W:=Img.Width;
  H:=Img.Height;
  if FVars.Debug then
    begin
      Writeln(stdout,'width=',W,' height=',H);
      //system.flush(stdout);
    end;
  // Writeln('Transparant');
  cnv.Brush.Style:=bsSolid;
  cnv.Brush.FPColor:=colTransparent;
  cnv.Pen.FPColor:=colWhite;
  Cnv.Rectangle(0,0,W,H);
  if FVars.DEbug then
    Writeln(stdout,'Setting font');
  Cnv.Font:=F;
  if FVars.Debug then
    Writeln(stdout,'Getting textwidth ');
  FH:=CNV.GetTextHeight('A');
  If FH=0 then
    FH:=14; // 3 * 14;
  if FVars.Debug then
    writeln(stdout,'FH=',FH);
  Inc(FH,3);
  R.Top:=FH*4;
  R.Left:=0;
  R.Bottom:=H;
  CR:=H-(FH*4);
  If W>CR then
    R.Right:=CR
  else
    R.Right:=W;
  Ra:=CR div 2;
  if FVars.DEbug then
    begin
      Writeln(stdout,'Setting pen color');
      system.flush(stdout);
    end;
  Cnv.Pen.FPColor:=colBlack;
  if FVars.Debug then
    begin
      Writeln(stdout,'Palette size : ',Img.Palette.Count);
      Writeln(stdout,'Setting brush style');
      system.flush(stdout);
    end;
  cnv.brush.FPColor:=colDkGray;
  SR:=Skipped/Total;
  FR:=Failed/Total;
  PR:=1-SR-FR;
  cnv.font.FPColor:=colDkGray;
  Cnv.Textout(1,FH*2,Format('%d Skipped (%3.1f%%)',[Skipped,SR*100]));
//  cnv.pen.width:=1;
  // Writeln('Drawing ellipse');
  Cnv.Ellipse(R);
  if FVars.Debug then
    begin
      Writeln(stdout,'Setting text');
      system.flush(stdout);
    end;
  A1:=0;
  A2:=A1+FractionAngle(Failed,Total);
  cnv.font.FPColor:=colRed;
  Cnv.Textout(1,FH*3,Format('%d Failed (%3.1f%%)',[Failed,FR*100]));
  AddPie(Ra,R.Top+Ra,Ra,A1,A2,ColRed);
  cnv.font.FPColor:=colGreen;
  Cnv.Textout(1,FH,Format('%d Passed (%3.1f%%)',[Total-Skipped-Failed,PR*100]));
  // Writeln('Palette size : ',Img.Palette.Count);
  A1:=A2;
  A2:=A1+FractionAngle(Total-(Skipped+Failed),Total);
  AddPie(Ra,R.Top+Ra,Ra,A1,A2,ColGreen);
  // Writeln('Palette size : ',Img.Palette.Count);
  // Writeln('All done');
end;

end.

