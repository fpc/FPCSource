{*****************************************************************************}
{
    $Id$
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

{
The PNM (Portable aNyMaps) is a generic name for :
  PBM : Portable BitMaps,
  PGM : Portable GrayMaps,
  PPM : Portable PixMaps.
There is no file format associated  with PNM itself.}

{$mode objfpc}{$h+}
unit FPReadPNM;

interface

uses FPImage, classes, sysutils;

type
  TFPReaderPNM=class (TFPCustomImageReader)
    private
      FBitMapType : Integer;
      FWidth      : Integer;
      FHeight     : Integer;
    protected
      FMaxVal     : Integer;
      FBPP        : Byte;
      FScanLineSize : Integer;
      FScanLine   : PByte;
      procedure ReadHeader(Stream : TStream);
      function  InternalCheck (Stream:TStream):boolean;override;
      procedure InternalRead(Stream:TStream;Img:TFPCustomImage);override;
      procedure ReadScanLine(Row : Integer; Stream:TStream);
      procedure WriteScanLine(Row : Integer; Img : TFPCustomImage);
  end;

implementation

function TFPReaderPNM.InternalCheck(Stream:TStream):boolean;

begin
  InternalCheck:=True;
end;

const
  WhiteSpaces=[#9,#10,#13,#32]; {Whitespace (TABs, CRs, LFs, blanks) are separators in the PNM Headers}

function DropWhiteSpaces(Stream : TStream) :Char;

begin
  with Stream do
    begin
    repeat
      ReadBuffer(DropWhiteSpaces,1);
    until not(DropWhiteSpaces in WhiteSpaces);
    if DropWhiteSpaces='#' then
      begin
      repeat
        ReadBuffer(DropWhiteSpaces,1);
      until DropWhiteSpaces=#10;
      ReadBuffer(DropWhiteSpaces,1);
      end;
    end;
end;

function ReadInteger(Stream : TStream) :Integer;

var
  s:String[7];
  
begin
  s:='';
  s[1]:=DropWhiteSpaces(Stream);
  with Stream do
    repeat
      Inc(s[0]);
      ReadBuffer(s[Length(s)+1],1)
    until s[Length(s)+1] in WhiteSpaces;
  Result:=StrToInt(s);
end;

procedure TFPReaderPNM.ReadHeader(Stream : TStream);

Var
  C : Char;

begin
  Stream.ReadBuffer(C,1);
  If (C<>'P') then
    Raise Exception.Create('Not a valid PNM image.');
  Stream.ReadBuffer(C,1);
  FBitmapType:=Ord(C)-Ord('0');  
  If Not (FBitmapType in [2,3,5,6]) then
    Raise Exception.CreateFmt('Unknown PNM subtype : %s',[C]);
  FWidth:=ReadInteger(Stream);
  FHeight:=ReadInteger(Stream);
  FMaxVal:=ReadInteger(Stream);
  If (FWidth<=0) or (FHeight<=0) or (FMaxVal<=0) then
    Raise Exception.Create('Invalid PNM header data');
  case FBitMapType of
    2: FBPP:=SizeOf(Word);   // Grayscale (text)
    3: FBPP:=SizeOf(Word)*3; // RGB (text) 
    5: If (FMaxval>255) then   // Grayscale (raw);
         FBPP:=2
       else
         FBPP:=1;
    6: if (FMaxVal>255) then    // RGB (raw)
         FBPP:=6
       else
         FBPP:=3
  end;
//  Writeln(FWidth,'x',Fheight,' Maxval: ',FMaxVal,' BPP: ',FBPP);
end;

procedure TFPReaderPNM.InternalRead(Stream:TStream;Img:TFPCustomImage);

var
  Row,Coulumn,nBpLine,ReadSize:Integer;
  aColor:TFPcolor;
  aLine:PByte;
    
begin
  ReadHeader(Stream);
  Img.SetSize(FWidth,FHeight);
  FScanLineSize:=FBPP*FWidth;
  GetMem(FScanLine,FBPP*FWidth);
  try
    for Row:=0 to img.Height-1 do
      begin
      ReadScanLine(Row,Stream);
      WriteScanLine(Row,Img);
      end;
  finally
    FreeMem(FScanLine);
  end;      
end;

procedure TFPReaderPNM.ReadScanLine(Row : Integer; Stream:TStream);

Var
  P : PWord;
  I : Integer;
  
begin
  Case FBitmapType of
    2 : begin
        P:=PWord(FScanLine);
        For I:=0 to FWidth-1 do
          begin
          P^:=ReadInteger(Stream);
          Inc(P);
          end;
        end;
    3 : begin
        P:=PWord(FScanLine);
        For I:=0 to FWidth-1 do
          begin
          P^:=ReadInteger(Stream); // Red
          Inc(P);
          P^:=ReadInteger(Stream); // Green
          Inc(P);
          P^:=ReadInteger(Stream); // Blue;
          Inc(P)
          end;
        end;
    5,6 : Stream.ReadBuffer(FScanLine^,FScanLineSize);
    end;
end;


procedure TFPReaderPNM.WriteScanLine(Row : Integer; Img : TFPCustomImage);

Var
  C : TFPColor;
  L : Cardinal;
  FHalfMaxVal : Word;

  Procedure WordGrayScanLine;
  
  Var
    P : PWord;
    I : Integer;
    
  begin
    P:=PWord(FScanLine);
    For I:=0 to FWidth-1 do
      begin
      L:=(((P^ shl 16)+FHalfMaxVal) div FMaxVal) and $FFFF;
      C.Red:=L;
      C.Green:=L;
      C.Blue:=L;
      Img.Colors[I,Row]:=C;
      Inc(P);
      end;
  end;
  
  Procedure WordRGBScanLine;

  Var
    P : PWord;
    I : Integer;
    
  begin
    P:=PWord(FScanLine);
    For I:=0 to FWidth-1 do
      begin
      L:=(((P^ shl 16)+FHalfMaxVal) div FMaxVal) and $FFFF;
      C.Red:=L;
      Inc(P);
      L:=(((P^ shl 16)+FHalfMaxVal) div FMaxVal) and $FFFF;
      C.Green:=L;
      Inc(P);
      L:=(((P^ shl 16)+FHalfMaxVal) div FMaxVal) and $FFFF;
      C.Blue:=L;
      Img.Colors[I,Row]:=C;
      Inc(P);
      end;
  end;

  Procedure ByteGrayScanLine;

  Var
    P : PByte;
    I : Integer;
  
  begin
    P:=PByte(FScanLine);
    For I:=0 to FWidth-1 do
      begin
      L:=(((P^ shl 16)+FHalfMaxVal) div FMaxVal) and $FFFF;
      C.Red:=L;
      C.Green:=L;
      C.Blue:=L;
      Img.Colors[I,Row]:=C;
      Inc(P);
      end;
  end;
  
  Procedure ByteRGBScanLine;

  Var
    P : PByte;
    I : Integer;

  begin
    P:=PByte(FScanLine);
    For I:=0 to FWidth-1 do
      begin
      L:=(((P^ shl 16)+FHalfMaxVal) div FMaxVal) and $FFFF;
      C.Red:=L;
      Inc(P);
      L:=(((P^ shl 16)+FHalfMaxVal) div FMaxVal) and $FFFF;
      C.Green:=L;
      Inc(P);
      L:=(((P^ shl 16)+FHalfMaxVal) div FMaxVal) and $FFFF;
      C.Blue:=L;
      Img.Colors[I,Row]:=C;
      Inc(P);
      end;
  end;
    
begin
  C.Alpha:=AlphaOpaque;
  FHalfMaxVal:=(FMaxVal div 2);
  Case FBitmapType of
    2 : WordGrayScanline;
    3 : WordRGBSCanline;
    5 : If FBPP=1 then
          ByteGrayScanLine
        else
          WordGrayScanLine;
    6 : If FBPP=3 then
          ByteRGBScanLine
        else
          WordRGBScanLine;
    end;
end;

initialization
  ImageHandlers.RegisterImageReader ('PNM Format', 'PNM;PGM;PBM', TFPReaderPNM);
end.
{
$Log$
Revision 1.3  2004-03-03 00:03:34  michael
+ Fixed reading of pnm

Revision 1.2  2003/09/30 12:26:33  mazen
+ reading P6 format implemented.

Revision 1.1  2003/09/30 07:15:48  mazen
+ Support for PNM (Portable aNyMap) formats (skeleton only)
   need to complete implementation

}
