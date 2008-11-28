{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 by the Free Pascal development team

    Common stuff for Tiff image format.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
}
unit FPTiffCmn;

{$mode objfpc}{$H+}

interface

uses
  Classes, sysutils, FPimage;

type
  TTiffRational = packed record
    Numerator, Denominator: DWord;
  end;

const
  TiffRational0: TTiffRational = (Numerator: 0; Denominator: 0);
  TiffRational72: TTiffRational = (Numerator: 72; Denominator: 1);

  // TFPCustomImage.Extra properties used by TFPReaderTiff and TFPWriterTiff
  TiffExtraPrefix = 'Tiff';
  TiffPhotoMetric = TiffExtraPrefix+'PhotoMetricInterpretation';
  TiffGrayBits = TiffExtraPrefix+'GrayBits'; // CMYK: key plate
  TiffRedBits = TiffExtraPrefix+'RedBits'; // CMYK: cyan
  TiffGreenBits = TiffExtraPrefix+'GreenBits'; // CMYK: magenta
  TiffBlueBits = TiffExtraPrefix+'BlueBits'; // CMYK: yellow
  TiffAlphaBits = TiffExtraPrefix+'AlphaBits';
  TiffArtist = TiffExtraPrefix+'Artist';
  TiffCopyright = TiffExtraPrefix+'Copyright';
  TiffDocumentName = TiffExtraPrefix+'DocumentName';
  TiffDateTime = TiffExtraPrefix+'DateTime';
  TiffImageDescription = TiffExtraPrefix+'ImageDescription';
  TiffOrientation = TiffExtraPrefix+'Orientation';
  TiffResolutionUnit = TiffExtraPrefix+'ResolutionUnit';
  TiffXResolution = TiffExtraPrefix+'XResolution';
  TiffYResolution = TiffExtraPrefix+'YResolution';

type

  { TTiffIDF }

  TTiffIDF = class
  public
    Artist: String;
    BitsPerSample: DWord; // tiff position of entry
    BitsPerSampleArray: array of Word;
    CellLength: DWord;
    CellWidth: DWord;
    ColorMap: DWord;// tiff position of entry
    Compression: DWord;
    Copyright: string;
    DateAndTime: string;
    DocumentName: string;
    ExtraSamples: DWord;// tiff position of entry
    FillOrder: DWord;
    HostComputer: string;
    ImageDescription: string;
    ImageHeight: DWord;
    ImageIsMask: Boolean;
    ImageIsPage: Boolean;
    ImageIsThumbNail: Boolean;
    ImageWidth: DWord;
    Make_ScannerManufacturer: string;
    Model_Scanner: string;
    Orientation: DWord;
    PhotoMetricInterpretation: DWord;
    PlanarConfiguration: DWord;
    ResolutionUnit: DWord;
    RowsPerStrip: DWord;
    SamplesPerPixel: DWord;
    Software: string;
    StripByteCounts: DWord;// tiff position of entry
    StripOffsets: DWord; // tiff position of entry
    Treshholding: DWord;
    XResolution: TTiffRational;
    YResolution: TTiffRational;
    // image
    Img: TFPCustomImage;
    RedBits: word;
    GreenBits: word;
    BlueBits: word;
    GrayBits: word;
    AlphaBits: word;
    BytesPerPixel: Word;
    procedure Clear;
    procedure Assign(IDF: TTiffIDF);
  end;

function TiffRationalToStr(const r: TTiffRational): string;
function StrToTiffRationalDef(const s: string; const Def: TTiffRational): TTiffRational;
procedure ClearTiffExtras(Img: TFPCustomImage);
procedure CopyTiffExtras(SrcImg, DestImg: TFPCustomImage);
procedure WriteTiffExtras(Msg: string; Img: TFPCustomImage);

implementation

function TiffRationalToStr(const r: TTiffRational): string;
begin
  Result:=IntToStr(r.Numerator)+'/'+IntToStr(r.Denominator);
end;

function StrToTiffRationalDef(const s: string; const Def: TTiffRational
  ): TTiffRational;
var
  p: LongInt;
begin
  Result:=Def;
  p:=System.Pos('/',s);
  if p<1 then exit;
  Result.Numerator:=StrToIntDef(copy(s,1,p-1),TiffRational0.Numerator);
  Result.Denominator:=StrToIntDef(copy(s,p+1,length(s)),TiffRational0.Denominator);
end;

procedure ClearTiffExtras(Img: TFPCustomImage);
var
  i: Integer;
begin
  for i:=Img.ExtraCount-1 downto 0 do
    if SysUtils.CompareText(copy(Img.ExtraKey[i],1,4),'Tiff')=0 then
      Img.RemoveExtra(Img.ExtraKey[i]);
end;

procedure CopyTiffExtras(SrcImg, DestImg: TFPCustomImage);
var
  i: Integer;
begin
  ClearTiffExtras(DestImg);
  for i:=SrcImg.ExtraCount-1 downto 0 do
    if SysUtils.CompareText(copy(SrcImg.ExtraKey[i],1,4),'Tiff')=0 then
      DestImg.Extra[SrcImg.ExtraKey[i]]:=SrcImg.ExtraValue[i];
end;

procedure WriteTiffExtras(Msg: string; Img: TFPCustomImage);
var
  i: Integer;
begin
  writeln('WriteTiffExtras ',Msg);
  for i:=Img.ExtraCount-1 downto 0 do
    //if SysUtils.CompareText(copy(Img.ExtraKey[i],1,4),'Tiff')=0 then
      writeln('  ',i,' ',Img.ExtraKey[i],'=',Img.ExtraValue[i]);
end;

{ TTiffIDF }

procedure TTiffIDF.Clear;
begin
  PhotoMetricInterpretation:=High(PhotoMetricInterpretation);
  PlanarConfiguration:=0;
  Compression:=0;
  ImageHeight:=0;
  ImageWidth:=0;
  ImageIsThumbNail:=false;
  ImageIsPage:=false;
  ImageIsMask:=false;
  BitsPerSample:=0;
  SetLength(BitsPerSampleArray,0);
  ResolutionUnit:=0;
  XResolution:=TiffRational0;
  YResolution:=TiffRational0;
  RowsPerStrip:=0;
  StripOffsets:=0;
  StripByteCounts:=0;
  SamplesPerPixel:=0;
  Artist:='';
  HostComputer:='';
  ImageDescription:='';
  Make_ScannerManufacturer:='';
  Model_Scanner:='';
  Copyright:='';
  DateAndTime:='';
  Software:='';
  CellWidth:=0;
  CellLength:=0;
  FillOrder:=0;
  Orientation:=0;
  Treshholding:=0;

  RedBits:=0;
  GreenBits:=0;
  BlueBits:=0;
  GrayBits:=0;
  AlphaBits:=0;
  BytesPerPixel:=0;
end;

procedure TTiffIDF.Assign(IDF: TTiffIDF);
begin
  PhotoMetricInterpretation:=IDF.PhotoMetricInterpretation;
  PlanarConfiguration:=IDF.PlanarConfiguration;
  Compression:=IDF.Compression;
  ImageHeight:=IDF.ImageHeight;
  ImageWidth:=IDF.ImageWidth;
  ImageIsThumbNail:=IDF.ImageIsThumbNail;
  ImageIsPage:=IDF.ImageIsPage;
  ImageIsMask:=IDF.ImageIsMask;
  BitsPerSample:=IDF.BitsPerSample;
  BitsPerSampleArray:=IDF.BitsPerSampleArray;
  ResolutionUnit:=IDF.ResolutionUnit;
  XResolution:=IDF.XResolution;
  YResolution:=IDF.YResolution;
  RowsPerStrip:=IDF.RowsPerStrip;
  StripOffsets:=IDF.StripOffsets;
  StripByteCounts:=IDF.StripByteCounts;
  SamplesPerPixel:=IDF.SamplesPerPixel;
  Artist:=IDF.Artist;
  HostComputer:=IDF.HostComputer;
  ImageDescription:=IDF.ImageDescription;
  Make_ScannerManufacturer:=IDF.Make_ScannerManufacturer;
  Model_Scanner:=IDF.Model_Scanner;
  Copyright:=IDF.Copyright;
  DateAndTime:=IDF.DateAndTime;
  Software:=IDF.Software;
  CellWidth:=IDF.CellWidth;
  CellLength:=IDF.CellLength;
  FillOrder:=IDF.FillOrder;
  Orientation:=IDF.Orientation;
  Treshholding:=IDF.Treshholding;
  RedBits:=IDF.RedBits;
  GreenBits:=IDF.GreenBits;
  BlueBits:=IDF.BlueBits;
  GrayBits:=IDF.GrayBits;
  AlphaBits:=IDF.AlphaBits;
  if (Img<>nil) and (IDF.Img<>nil) then
    Img.Assign(IDF.Img);
end;

end.

