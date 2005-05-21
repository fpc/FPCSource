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
    * PGM (P2,P5) : Portable GrayMap format : 8 bits per pixel
    * PPM (P5,P6) : Portable PixelMap foramt : 24 bits per pixel}
{$mode objfpc}{$h+}
unit FPWritePNM;
interface

uses FPImage, classes, sysutils;

type

  TFPWriterPNM = class(TFPCustomImageWriter)
    private
      BitMapType:Integer;
    protected
      procedure InternalWrite(Stream:TStream;Img:TFPCustomImage);override;
    public
      constructor Create(aBitMapType:Integer);
  end;

implementation

constructor TFPWriterPNM.Create(aBitMapType:Integer);
  begin
    inherited Create;
    BitMapType:=aBitMapType;
  end;
procedure TFPWriterPNM.InternalWrite(Stream:TStream;Img:TFPCustomImage);
  function SaveHeader(stream:TStream):boolean;
    const
      MagicWords:Array[1..6]OF String[2]=('P1','P2','P3','P4','P5','P6');
    var
      PNMInfo:String;
      strWidth,StrHeight:String[15];
    begin
      SaveHeader:=false;
      with Img do
        begin
          Str(Img.Width,StrWidth);
          Str(Img.Height,StrHeight);
        end;
      PNMInfo:=Concat(MagicWords[BitMapType],#10,StrWidth,#32,StrHeight,#10);
      if BitMapType in [2,3,5,6]
      then
        PNMInfo:=Concat(PNMInfo,'255'#10);
      stream.seek(0,soFromBeginning);
      stream.Write(PNMInfo[1],Length(PNMInfo));
      SaveHeader := true;
    end;
  var
    Row,Coulumn,nBpLine,i:Integer;
    aColor:TFPColor;
    aLine:PByte;
    strCol:String[3];
  begin
    SaveHeader(Stream);
    case BitMapType of
      1:nBpLine:=Img.Width*2;{p p p}
      2:nBpLine:=Img.Width*4;{lll lll lll}
      3:nBpLine:=Img.Width*3*4;{rrr ggg bbb rrr ggg bbb}
      4:begin
          nBpLine:=Img.Width SHR 3;
          if(Img.Width AND $0F)<>0
          then
            Inc(nBpLine);
        end;
      5:nBpLine:=Img.Width;
      6:nBpLine:=Img.Width*3;
    end;
    GetMem(aLine,nBpLine);//3 extra byte for BMP 4Bytes alignement.
    for Row:=0 to img.Height-1 do
      begin
        FillChar(aLine^,nBpLine,0);
        for Coulumn:=0 to img.Width-1 do
          begin
            aColor:=img.Colors[Coulumn,Row];
            with aColor do
              case BitMapType of
                1:begin
                    if(Red<=$2F00)or(Green<=$2F00)or(Blue<=$2F00)
                    then
                      aLine[2*Coulumn]:=Ord('1')
                    else
                      aLine[2*Coulumn]:=Ord('0');
                    aLine[2*Coulumn+1]:=32;
                  end;
                2:begin
                    Str(Hi(Word(Round(Red*0.299+Green*0.587+Blue*0.114))),strCol);
                    for i:=0 to Length(StrCol)-1 do
                      aLine[4*Coulumn+i]:=Ord(StrCol[i+1]);
                    for i:=Length(StrCol) to 4 do
                      aLine[4*Coulumn+i]:=32;
                  end;
                3:begin
                    Str(Hi(Red),strCol);
                    for i:=0 to Length(StrCol)-1 do
                      aLine[4*(3*Coulumn)+i]:=Ord(StrCol[i+1]);
                    for i:=Length(StrCol) to 4 do
                      aLine[4*(3*Coulumn)+i]:=32;
                    Str(Hi(Green),strCol);
                    for i:=0 to Length(StrCol)-1 do
                      aLine[4*(3*Coulumn+1)+i]:=Ord(StrCol[i+1]);
                    for i:=Length(StrCol) to 4 do
                      aLine[4*(3*Coulumn+1)+i]:=32;
                    Str(Hi(Blue),strCol);
                    for i:=0 to Length(StrCol)-1 do
                      aLine[4*(3*Coulumn+2)+i]:=Ord(StrCol[i+1]);
                    for i:=Length(StrCol) to 4 do
                      aLine[4*(3*Coulumn+2)+i]:=32;
                  end;
                4:if(Red<=$2F00)or(Green<=$2F00)or(Blue<=$2F00)
                  then
                    aLine[Coulumn shr 3]:=aLine[Coulumn shr 3] or ($80 shr (Coulumn and $07));
                5:aLine[Coulumn]:=Hi(Word(Round(Red*0.299+Green*0.587+Blue*0.114)));
                6:begin
                    aLine[3*Coulumn]:=Hi(Red);
                    aLine[3*Coulumn+1]:=Hi(Green);
                    aLine[3*Coulumn+2]:=Hi(Blue);
                  end;
            end;
          end;
        Stream.Write(aLine^,nBpLine);
      end;
    FreeMem(aLine,nBpLine);
  end;

initialization
  ImageHandlers.RegisterImageWriter ('PBM Format', 'pbm', TFPWriterPNM);
end.
