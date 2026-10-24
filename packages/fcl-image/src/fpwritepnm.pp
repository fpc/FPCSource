{*****************************************************************************}
{
    This file is part of the Free Pascal's "Free Components Library".
    Copyright (c) 2003 by Mazen NEIFER of the Free Pascal development team

    PNM writer implementation.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
{*****************************************************************************}
{Support for writing PNM (Portable aNyMap) formats added :
    * PBM (P1,P4) : Portable BitMap format : 1 bit per pixel
    * PGM (P2,P5) : Portable GrayMap format : 8 bits per pixel for P2 (ASCII), 8 or 16 bit for P5 (binary)
    * PPM (P3,P6) : Portable PixelMap format : 24 bits per pixel for P3 (ASCII), 24 or 48 bit for P6 (binary)
    * PAM (P7) : Portable Arbitrary Map : 8 or 16 bit samples, with an optional alpha channel
    * PFM (PF,Pf) : Portable Float Map : 32-bit float samples}
{$mode objfpc}{$h+}
{$IFNDEF FPC_DOTTEDUNITS}
unit FPWritePNM;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses FpImage, System.Classes, System.SysUtils;
{$ELSE FPC_DOTTEDUNITS}
uses FpImage, classes, sysutils;
{$ENDIF FPC_DOTTEDUNITS}

type
  TPNMColorDepth = (pcdAuto,pcdBlackWhite, pcdGrayscale, pcdRGB);

  { TFPWriterPNM }

  TFPWriterPNM = class(TFPCustomImageWriter)
  private
    FFullWidth: Boolean;
    FColorDepth: TPNMColorDepth;
    FBinaryFormat: boolean;
    procedure SetFullWidth(AValue: Boolean);
  protected
    function SaveHeader(useBitMapType:Integer;Stream:TStream;Img:TFPCustomImage):boolean; virtual;
    procedure InternalWrite(Stream:TStream;Img:TFPCustomImage);override;
  public
    Property FullWidth: Boolean Read FFullWidth Write SetFullWidth; {if true write 16 bits per colour for P5, P6 formats}
    function GuessColorDepthOfImage(Img: TFPCustomImage): TPNMColorDepth;
    function GetColorDepthOfExtension(AExtension: AnsiString): TPNMColorDepth;
    function GetFileExtension(AColorDepth: TPNMColorDepth): AnsiString;
    constructor Create; override;
    Property BinaryFormat : Boolean Read FBinaryFormat Write FBinaryFormat;
    Property ColorDepth: TPNMColorDepth Read FColorDepth Write FColorDepth;
  end;

  { TFPWriterPBM }

  TFPWriterPBM = class(TFPWriterPNM)
      constructor Create; override;
  end;

  { TFPWriterPGM }

  TFPWriterPGM = class(TFPWriterPNM)
      constructor Create; override;
  end;

  { TFPWriterPPM }

  TFPWriterPPM = class(TFPWriterPNM)
      constructor Create; override;
  end;

  { TFPWriterPAM }

  TFPWriterPAM = class(TFPCustomImageWriter)
  private
    FColorDepth: TPNMColorDepth;
    FFullWidth: Boolean;
    FUseAlpha: Boolean;
  protected
    procedure InternalWrite(Stream:TStream;Img:TFPCustomImage);override;
  public
    // Creates a writer that chooses the tuple type from the image.
    constructor Create; override;
    // Tuple type written; pcdAuto chooses it from the image.
    Property ColorDepth: TPNMColorDepth Read FColorDepth Write FColorDepth;
    // Writes 16-bit samples instead of 8-bit ones.
    Property FullWidth: Boolean Read FFullWidth Write FFullWidth;
    // Adds an alpha channel when the image has a pixel that is not opaque.
    Property UseAlpha: Boolean Read FUseAlpha Write FUseAlpha;
  end;

  { TFPWriterPFM }

  TFPWriterPFM = class(TFPCustomImageWriter)
  private
    FColorDepth: TPNMColorDepth;
  protected
    procedure InternalWrite(Stream:TStream;Img:TFPCustomImage);override;
  public
    // Creates a writer that chooses between grey (Pf) and RGB (PF) from the image.
    constructor Create; override;
    // pcdRGB writes PF, the other depths write Pf; pcdAuto chooses from the image.
    Property ColorDepth: TPNMColorDepth Read FColorDepth Write FColorDepth;
  end;

procedure SaveImageToPNMFile(Img: TFPCustomImage; filename: AnsiString; UseBinaryFormat: boolean = true);

implementation

// Returns the smallest PNM colour depth that holds every pixel of Img.
function GuessPNMColorDepth(Img: TFPCustomImage): TPNMColorDepth;

var
  Row, Col: integer;
  aColor: TFPColor;
  Gray: Byte;

begin
  result := pcdBlackWhite;
  for Row:=0 to img.Height-1 do
    for Col:=0 to img.Width-1 do
      begin
      aColor:=img.Colors[Col,Row];
      Gray:=Hi(aColor.Green);
      if (Hi(aColor.Red)<>Gray) or (Hi(aColor.Blue)<>Gray) then
        exit(pcdRGB);
      if (Gray<>0) and (Gray<>$FF) then
        result := pcdGrayscale;
      end;
end;


// Returns True when Img has a pixel that is not opaque.
function HasTranslucentPixel(Img: TFPCustomImage): Boolean;

var
  Row, Col: integer;

begin
  Result:=True;
  for Row:=0 to Img.Height-1 do
    for Col:=0 to Img.Width-1 do
      if Img.Colors[Col,Row].Alpha<>AlphaOpaque then
        Exit;
  Result:=False;
end;



procedure SaveImageToPNMFile(Img: TFPCustomImage; filename: AnsiString; UseBinaryFormat: boolean = true);
var writer: TFPWriterPNM;
    curExt: AnsiString;
begin
  writer := TFPWriterPNM.Create;
  writer.BinaryFormat := UseBinaryFormat;
  curExt := Lowercase(ExtractFileExt(filename));
  if (curExt='.pnm') or (curExt='') then
  begin
    writer.ColorDepth := writer.GuessColorDepthOfImage(Img);
    filename := ChangeFileExt(filename,'.'+writer.GetFileExtension(writer.ColorDepth));
  end else
    writer.ColorDepth := writer.GetColorDepthOfExtension(curExt);
  Img.SaveToFile(filename,writer);
  writer.Free;
end;

{ TFPWriterPPM }

constructor TFPWriterPPM.Create;
begin
  inherited Create;
  ColorDepth := pcdRGB;
end;

{ TFPWriterPGM }

constructor TFPWriterPGM.Create;
begin
  inherited Create;
  ColorDepth := pcdGrayscale;
end;

{ TFPWriterPBM }

constructor TFPWriterPBM.Create;
begin
  inherited Create;
  ColorDepth:= pcdBlackWhite;
end;

{ TFPWriterPNM }

constructor TFPWriterPNM.Create;
begin
  inherited Create;
  ColorDepth := pcdAuto;
  BinaryFormat := True;
end;

procedure TFPWriterPNM.SetFullWidth(AValue: Boolean);
begin
  if FFullWidth=AValue then Exit;
  FFullWidth:=AValue;
  if FFullWidth then
    BinaryFormat:=True;
end;

function TFPWriterPNM.SaveHeader(useBitMapType:Integer;Stream:TStream;Img:TFPCustomImage):boolean;
const
    MagicWords:Array[1..6]OF String[2]=('P1','P2','P3','P4','P5','P6');
var
   PNMInfo:String;
   strWidth,StrHeight:String[15];
begin
    Result:=false;
    with Img do
      begin
        Str(Img.Width,StrWidth);
        Str(Img.Height,StrHeight);
      end;
    PNMInfo:=Concat(MagicWords[useBitMapType],#10,StrWidth,#32,StrHeight,#10);
    if (useBitMapType in [5,6]) and FullWidth then
      PNMInfo:=Concat(PNMInfo,'65535'#10)
    else if (useBitMapType in [2,3,5,6]) then
      PNMInfo:=Concat(PNMInfo,'255'#10);
    Stream.WriteBuffer(PNMInfo[1],Length(PNMInfo));
    Result := true;
end;

// Returns True when the luma of aColor is below half intensity.
function IsDark(const aColor: TFPColor): Boolean;

begin
  Result:=CalculateGray(aColor)<$8000;
end;


procedure TFPWriterPNM.InternalWrite(Stream:TStream;Img:TFPCustomImage);
const
    MaxLineLength = 70;
var
    useBitMapType: integer;
    Row,Coulumn,nBpLine:Integer;
    aColor:TFPColor;
    aLine:PByte;
    dLine : PWord;
    TextLine: AnsiString;
    LineStart: Integer;
    UseColorDepth: TPNMColorDepth;

  // Appends one text sample, starting a new line after MaxLineLength characters.
  procedure AddSample(const aSample: AnsiString);

  begin
    if Length(TextLine)>LineStart then
      if Length(TextLine)-LineStart+1+Length(aSample)>MaxLineLength then
        begin
        TextLine:=TextLine+#10;
        LineStart:=Length(TextLine);
        end
      else
        TextLine:=TextLine+' ';
    TextLine:=TextLine+aSample;
  end;

begin
    //determine color depth
    if ColorDepth = pcdAuto then
      UseColorDepth := GuessColorDepthOfImage(Img) else
      UseColorDepth := ColorDepth;

    //determine file format number (1-6)
    case UseColorDepth of
      pcdBlackWhite: useBitMapType := 1;
      pcdGrayscale: useBitMapType := 2;
      pcdRGB: useBitMapType := 3;
    end;
    if BinaryFormat then inc(useBitMapType,3);
    if FullWidth and Not BinaryFormat then
      Raise FPImageException.Create('Fullwidth can only be used with binary format');
    SaveHeader(useBitMapType, Stream, Img);
    if useBitMapType in [1..3] then
      begin
      for Row:=0 to img.Height-1 do
        begin
        TextLine:='';
        LineStart:=0;
        for Coulumn:=0 to img.Width-1 do
          begin
          aColor:=img.Colors[Coulumn,Row];
          case useBitMapType of
            1: if IsDark(aColor) then
                 AddSample('1')
               else
                 AddSample('0');
            2: AddSample(IntToStr(Hi(CalculateGray(aColor))));
            3: begin
               AddSample(IntToStr(Hi(aColor.Red)));
               AddSample(IntToStr(Hi(aColor.Green)));
               AddSample(IntToStr(Hi(aColor.Blue)));
               end;
          end;
          end;
        TextLine:=TextLine+#10;
        Stream.WriteBuffer(TextLine[1],Length(TextLine));
        end;
      exit;
      end;
    case useBitMapType of
      4:nBpLine:=(Img.Width+7) SHR 3;
      5:nBpLine:=Img.Width*(1+Ord(FullWidth));
    else
      nBpLine:=Img.Width*3*(1+Ord(FullWidth));
    end;

    GetMem(aLine,nBpLine);
    try
    dLine:=PWord(aLine);
    for Row:=0 to img.Height-1 do
      begin
        FillChar(aLine^,nBpLine,0);
        for Coulumn:=0 to img.Width-1 do
          begin
            aColor:=img.Colors[Coulumn,Row];
            with aColor do
              case useBitMapType of
                4:if IsDark(aColor) then
                    aLine[Coulumn shr 3]:=aLine[Coulumn shr 3] or ($80 shr (Coulumn and $07));
                5: if FullWidth then {16 bit per colour}
                     dLine[Coulumn]:=NToBe(CalculateGray(aColor)) {write in big-endian format}
                   else {8 bit per colour}
                     aLine[Coulumn]:=Hi(CalculateGray(aColor));
                6:if FullWidth then
                  begin {16 bit per colour}
                    dLine[3*Coulumn]:=NToBE(Red); {write in big-endian format}
                    dLine[3*Coulumn+1]:=NToBE(Green);
                    dLine[3*Coulumn+2]:=NToBE(Blue);
                  end
                  else
                  begin {8 bit per colour}
                    aLine[3*Coulumn]:=Hi(Red);
                    aLine[3*Coulumn+1]:=Hi(Green);
                    aLine[3*Coulumn+2]:=Hi(Blue);
                  end;
            end;
          end;
        Stream.WriteBuffer(aLine^,nBpLine);
      end;
    finally
      FreeMem(aLine);
    end;
end;

function TFPWriterPNM.GetColorDepthOfExtension(AExtension: AnsiString
  ): TPNMColorDepth;
begin
  if (length(AExtension) > 0) and (AExtension[1]='.') then
    delete(AExtension,1,1);
  AExtension := LowerCase(AExtension);
  if AExtension='pbm' then result := pcdBlackWhite else
  if AExtension='pgm' then result := pcdGrayscale else
  if AExtension='ppm' then result := pcdRGB else
    result := pcdAuto;
end;

function TFPWriterPNM.GuessColorDepthOfImage(Img: TFPCustomImage): TPNMColorDepth;
begin
  Result:=GuessPNMColorDepth(Img);
end;

function TFPWriterPNM.GetFileExtension(AColorDepth: TPNMColorDepth): AnsiString;
begin
  case AColorDepth of
    pcdBlackWhite: result := 'pbm';
    pcdGrayscale: result := 'pgm';
    pcdRGB: result := 'ppm';
  else
    result := 'pnm';
  end;
end;

{ TFPWriterPAM }

constructor TFPWriterPAM.Create;

begin
  inherited Create;
  FColorDepth:=pcdAuto;
  FUseAlpha:=True;
end;


procedure TFPWriterPAM.InternalWrite(Stream:TStream;Img:TFPCustomImage);

const
  TupleTypes: array[TPNMColorDepth] of AnsiString = ('', 'BLACKANDWHITE', 'GRAYSCALE', 'RGB');

var
  lDepth: TPNMColorDepth;
  lAlpha: Boolean;
  lMaxVal, lChannels, lSampleSize, lPos, lRow, lCol: Integer;
  lHeader, lTuple: AnsiString;
  lLine: TBytes;
  lColor: TFPColor;

  // Stores a 16-bit value as one sample of lMaxVal.
  procedure Put(aValue: Word);

  begin
    case lMaxVal of
      1 : lLine[lPos]:=Ord(aValue>=$8000);
      255 : lLine[lPos]:=Hi(aValue);
    else
      lLine[lPos]:=Hi(aValue);
      lLine[lPos+1]:=Lo(aValue);
    end;
    Inc(lPos,lSampleSize);
  end;

begin
  lDepth:=ColorDepth;
  if lDepth=pcdAuto then
    lDepth:=GuessPNMColorDepth(Img);
  lAlpha:=UseAlpha and HasTranslucentPixel(Img);
  if lDepth=pcdRGB then
    lChannels:=3
  else
    lChannels:=1;
  lTuple:=TupleTypes[lDepth];
  if lAlpha then
    begin
    Inc(lChannels);
    lTuple:=lTuple+'_ALPHA';
    end;
  if lDepth=pcdBlackWhite then
    lMaxVal:=1
  else if FullWidth then
    lMaxVal:=65535
  else
    lMaxVal:=255;
  lSampleSize:=1+Ord(lMaxVal=65535);
  lHeader:=Format('P7'#10'WIDTH %d'#10'HEIGHT %d'#10'DEPTH %d'#10'MAXVAL %d'#10'TUPLTYPE %s'#10'ENDHDR'#10,
                  [Img.Width,Img.Height,lChannels,lMaxVal,lTuple]);
  Stream.WriteBuffer(lHeader[1],Length(lHeader));
  SetLength(lLine,Img.Width*lChannels*lSampleSize);
  for lRow:=0 to Img.Height-1 do
    begin
    lPos:=0;
    for lCol:=0 to Img.Width-1 do
      begin
      lColor:=Img.Colors[lCol,lRow];
      case lDepth of
        pcdBlackWhite : Put(65535*Ord(not IsDark(lColor)));
        pcdGrayscale : Put(CalculateGray(lColor));
      else
        Put(lColor.Red);
        Put(lColor.Green);
        Put(lColor.Blue);
      end;
      if lAlpha then
        Put(lColor.Alpha);
      end;
    if Length(lLine)>0 then
      Stream.WriteBuffer(lLine[0],Length(lLine));
    end;
end;


{ TFPWriterPFM }

constructor TFPWriterPFM.Create;

begin
  inherited Create;
  FColorDepth:=pcdAuto;
end;


procedure TFPWriterPFM.InternalWrite(Stream:TStream;Img:TFPCustomImage);

var
  lRGB: Boolean;
  lPos, lRow, lCol: Integer;
  lHeader: AnsiString;
  lLine: array of Cardinal;
  lColor: TFPColor;

  // Stores a 16-bit value as a little-endian float of 0..1.
  procedure Put(aValue: Word);

  var
    lValue: Single;

  begin
    lValue:=aValue/65535;
    lLine[lPos]:=NtoLE(PCardinal(@lValue)^);
    Inc(lPos);
  end;

begin
  if ColorDepth=pcdAuto then
    lRGB:=GuessPNMColorDepth(Img)=pcdRGB
  else
    lRGB:=ColorDepth=pcdRGB;
  if lRGB then
    lHeader:='PF'
  else
    lHeader:='Pf';
  lHeader:=Format('%s'#10'%d %d'#10'-1.0'#10,[lHeader,Img.Width,Img.Height]);
  Stream.WriteBuffer(lHeader[1],Length(lHeader));
  SetLength(lLine,Img.Width*(1+2*Ord(lRGB)));
  for lRow:=Img.Height-1 downto 0 do
    begin
    lPos:=0;
    for lCol:=0 to Img.Width-1 do
      begin
      lColor:=Img.Colors[lCol,lRow];
      if lRGB then
        begin
        Put(lColor.Red);
        Put(lColor.Green);
        Put(lColor.Blue);
        end
      else
        Put(CalculateGray(lColor));
      end;
    if Length(lLine)>0 then
      Stream.WriteBuffer(lLine[0],Length(lLine)*SizeOf(Cardinal));
    end;
end;


initialization
  ImageHandlers.RegisterImageWriter ('Netpbm Portable aNyMap', 'pnm', TFPWriterPNM);
  ImageHandlers.RegisterImageWriter ('Netpbm Portable BitMap', 'pbm', TFPWriterPBM);
  ImageHandlers.RegisterImageWriter ('Netpbm Portable GrayMap', 'pgm', TFPWriterPGM);
  ImageHandlers.RegisterImageWriter ('Netpbm Portable PixelMap', 'ppm', TFPWriterPPM);
  ImageHandlers.RegisterImageWriter ('Netpbm Portable Arbitrary Map', 'pam', TFPWriterPAM);
  ImageHandlers.RegisterImageWriter ('Portable Float Map', 'pfm', TFPWriterPFM);
end.
