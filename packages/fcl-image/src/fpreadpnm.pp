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

{
The PNM (Portable aNyMaps) is a generic name for :
  PBM : Portable BitMaps,
  PGM : Portable GrayMaps,
  PPM : Portable PixMaps,
  PAM : Portable Arbitrary Maps (P7),
  PFM : Portable Float Maps (PF, Pf).
There is normally no file format associated  with PNM itself.}

{$mode objfpc}{$h+}
{$IFNDEF FPC_DOTTEDUNITS}
unit FPReadPNM;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses FpImage, System.Classes, System.SysUtils, System.Math;
{$ELSE FPC_DOTTEDUNITS}
uses FpImage, classes, sysutils, math;
{$ENDIF FPC_DOTTEDUNITS}

Const
  BufSize = 1024;

type

  { TFPReaderPNM }

  TFPReaderPNM=class (TFPCustomImageReader)
    private
      FBitMapType : Integer;
      FWidth      : Integer;
      FHeight     : Integer;
      FBufPos : Integer;
      FBufLen : Integer;
      FBuffer : Array of AnsiChar;
      FDepth : Integer;
      FLittleEndian : Boolean;
      function DropWhiteSpaces(Stream: TStream): AnsiChar;
      function ReadToken(Stream: TStream): AnsiString;
      procedure ReadPAMHeader(Stream: TStream);
      procedure ReadPFMHeader(Stream: TStream);
      function TryReadChar(Stream: TStream; out aChar: AnsiChar): Boolean;
      function ReadChar(Stream: TStream): AnsiChar;
      function ReadInteger(Stream: TStream): Integer;
      function ReadSample(Stream: TStream): Word;
      function ReadBit(Stream: TStream): Byte;
      procedure ReadScanlineBuffer(Stream: TStream;p:Pbyte;Len:Integer);
    protected
      FMaxVal     : Cardinal;
      FBitPP        : Byte;
      FScanLineSize : Integer;
      FScanLine   : PByte;
      procedure ReadHeader(Stream : TStream); virtual;
      function  InternalCheck (Stream:TStream):boolean;override;
      procedure InternalRead(Stream:TStream;Img:TFPCustomImage);override;
      procedure ReadScanLine(Row : Integer; Stream:TStream);
      procedure WriteScanLine(Row : Integer; Img : TFPCustomImage);
  end;

implementation

const
  WhiteSpaces=[#9,#10,#13,#32];
  {Whitespace (TABs, CRs, LFs, blanks) are separators in the PNM Headers}

{ The magic number at the beginning of a pnm file is 'P1', 'P2', ..., 'P7',
  'PF' or 'Pf' followed by a WhiteSpace character }

function TFPReaderPNM.InternalCheck(Stream:TStream):boolean;
var
  hdr: array[0..2] of AnsiChar;
  oldPos: Int64;
  i,n: Integer;
begin
  Result:=False;
  if Stream = nil then
    exit;
  oldPos := Stream.Position;
  try
    n := SizeOf(hdr);
    Result:=(Stream.Size-OldPos>=N);
    if not Result then exit;
    For I:=0 to N-1 do
      hdr[i]:=ReadChar(Stream);
    Result:=(hdr[0] = 'P')
            and (hdr[1] in ['1'..'7','F','f'])
            and (hdr[2] in WhiteSpaces);
  finally
    Stream.Position := oldPos;
    FBufLen:=0;
    FBufPos:=0;
  end;
end;

function TFPReaderPNM.DropWhiteSpaces(Stream : TStream) :AnsiChar;

begin
  with Stream do
    begin
    repeat
      Result:=ReadChar(Stream);
{If we encounter comment then eat line}
      if DropWhiteSpaces='#' then
      repeat
        Result:=ReadChar(Stream);
      until Result=#10;
    until not (Result in WhiteSpaces);
    end;
end;

function TFPReaderPNM.ReadInteger(Stream : TStream) :Integer;

var
  C : AnsiChar;
  Value : Int64;
  HaveChar : Boolean;

begin
  C:=DropWhiteSpaces(Stream);
  if not (C in ['0'..'9']) then
    Raise FPImageException.CreateFmt('Invalid character in PNM data: #%d',[Ord(C)]);
  Value:=0;
  repeat
    Value:=Value*10+Ord(C)-Ord('0');
    if Value>MaxInt then
      Raise FPImageException.Create('Number too large in PNM data');
    HaveChar:=TryReadChar(Stream,C);
  until not (HaveChar and (C in ['0'..'9']));
  if HaveChar and not (C in WhiteSpaces) then
    if C='#' then
      repeat
      until not TryReadChar(Stream,C) or (C=#10)
    else
      Raise FPImageException.CreateFmt('Invalid character in PNM data: #%d',[Ord(C)]);
  Result:=Value;
end;


// Reads one header word; a '#' ends it and starts a comment up to the end of the line.
function TFPReaderPNM.ReadToken(Stream: TStream): AnsiString;

var
  C : AnsiChar;

begin
  C:=DropWhiteSpaces(Stream);
  Result:='';
  repeat
    Result:=Result+C;
    if Length(Result)>70 then
      Raise FPImageException.Create('Header word too long in PNM data');
  until not TryReadChar(Stream,C) or (C in WhiteSpaces) or (C='#');
  if C='#' then
    repeat
    until not TryReadChar(Stream,C) or (C=#10);
end;


// Reads the fields of a PAM header up to ENDHDR.
procedure TFPReaderPNM.ReadPAMHeader(Stream: TStream);

var
  lKey : AnsiString;

begin
  FWidth:=0;
  FHeight:=0;
  FDepth:=0;
  FMaxVal:=0;
  repeat
    lKey:=ReadToken(Stream);
    if lKey='ENDHDR' then
      Break;
    if lKey='WIDTH' then
      FWidth:=ReadInteger(Stream)
    else if lKey='HEIGHT' then
      FHeight:=ReadInteger(Stream)
    else if lKey='DEPTH' then
      FDepth:=ReadInteger(Stream)
    else if lKey='MAXVAL' then
      FMaxVal:=ReadInteger(Stream)
    else if lKey='TUPLTYPE' then
      ReadToken(Stream)
    else
      Raise FPImageException.CreateFmt('Unknown PAM header field: %s',[lKey]);
  until False;
  if (FDepth<1) or (FDepth>4) then
    Raise FPImageException.CreateFmt('Unsupported PAM depth: %d',[FDepth]);
  if FMaxVal>255 then
    FBitPP:=16*FDepth
  else
    FBitPP:=8*FDepth;
end;


// Reads the size and the scale of a PFM header; a negative scale means little-endian samples.
procedure TFPReaderPNM.ReadPFMHeader(Stream: TStream);

var
  lFormat : TFormatSettings;
  lScale : Double;

begin
  FWidth:=ReadInteger(Stream);
  FHeight:=ReadInteger(Stream);
  lFormat:=DefaultFormatSettings;
  lFormat.DecimalSeparator:='.';
  if not TryStrToFloat(ReadToken(Stream),lScale,lFormat) or IsNan(lScale) or IsInfinite(lScale) or (lScale=0) then
    Raise FPImageException.Create('Invalid PFM scale');
  FLittleEndian:=lScale<0;
  FMaxVal:=65535;
  if FBitmapType=8 then
    FDepth:=3
  else
    FDepth:=1;
  FBitPP:=32*FDepth;
end;


// Reads one text sample, limited to the maximum value of the header.
function TFPReaderPNM.ReadSample(Stream: TStream): Word;

var
  Value : Integer;

begin
  Value:=ReadInteger(Stream);
  if Value>FMaxVal then
    Value:=FMaxVal;
  Result:=Value;
end;


// Reads one P1 pixel: a single 0 or 1 digit.
function TFPReaderPNM.ReadBit(Stream: TStream): Byte;

var
  C : AnsiChar;

begin
  C:=DropWhiteSpaces(Stream);
  case C of
    '0' : Result:=0;
    '1' : Result:=1;
  else
    Raise FPImageException.CreateFmt('Invalid character in PBM data: #%d',[Ord(C)]);
  end;
end;

procedure TFPReaderPNM.ReadScanlineBuffer(Stream: TStream;p:Pbyte;Len:Integer);
// after the header read, there are still bytes in the buffer.
// drain the buffer before going for direct stream reads.
var BytesLeft : integer;
begin
  BytesLeft:=FBufLen-FBufPos;
  if BytesLeft>0 then
    begin
      if BytesLeft>Len then
        BytesLeft:=Len;
      Move (FBuffer[FBufPos],p^,BytesLeft);
      Dec(Len,BytesLeft);
      Inc(FBufPos,BytesLeft);
      Inc(p,BytesLeft);
      if Len>0 then
         Stream.ReadBuffer(p^,len);
    end
  else
    Stream.ReadBuffer(p^,len);
end;

// Reads the next character from the buffer; returns False at the end of the stream.
function TFPReaderPNM.TryReadChar(Stream: TStream; out aChar: AnsiChar): Boolean;

begin
  If (FBufPos>=FBufLen) then
    begin
    if Length(FBuffer)=0 then
      SetLength(FBuffer,BufSize);
    FBufLen:=Stream.Read(FBuffer[0],Length(FBuffer));
    FBufPos:=0;
    if FBuflen<=0 then
      begin
      FBufLen:=0;
      aChar:=#0;
      Exit(False);
      end;
    end;
  aChar:=FBuffer[FBufPos];
  Inc(FBufPos);
  Result:=True;
end;


Function TFPReaderPNM.ReadChar(Stream : TStream) : AnsiChar;

begin
  if not TryReadChar(Stream,Result) then
    Raise FPImageException.Create('Unexpected end of PNM data');
end;

procedure TFPReaderPNM.ReadHeader(Stream : TStream);

Var
  C : AnsiChar;

begin
  C:=ReadChar(Stream);
  If (C<>'P') then
    Raise FPImageException.Create('Not a valid PNM image.');
  C:=ReadChar(Stream);
  case C of
    'F' : FBitmapType:=8;
    'f' : FBitmapType:=9;
  else
    FBitmapType:=Ord(C)-Ord('0');
  end;
  If Not (FBitmapType in [1..9]) then
    Raise FPImageException.CreateFmt('Unknown PNM subtype : %s',[C]);
  case FBitmapType of
    7 : ReadPAMHeader(Stream);
    8,9 : ReadPFMHeader(Stream);
  else
    FWidth:=ReadInteger(Stream);
    FHeight:=ReadInteger(Stream);
    if FBitMapType in [1,4]
    then
      FMaxVal:=1
    else
      FMaxVal:=ReadInteger(Stream);
  end;
  If (FWidth<=0) or (FHeight<=0) or (FMaxVal<=0) or (FMaxVal>65535) then
    Raise FPImageException.Create('Invalid PNM header data');
  if (FWidth > 100000) or (FHeight > 100000) then
    Raise FPImageException.Create('PNM dimensions too large');
  case FBitMapType of
    1: FBitPP := 1;                  // 1bit PP (text)
    2: FBitPP := 8 * SizeOf(Word);   // Grayscale (text)
    3: FBitPP := 8 * SizeOf(Word)*3; // RGB (text)
    4: FBitPP := 1;            // 1bit PP (raw)
    5: If (FMaxval>255) then   // Grayscale (raw);
         FBitPP:= 8 * 2
       else
         FBitPP:= 8;
    6: if (FMaxVal>255) then    // RGB (raw)
         FBitPP:= 8 * 6
       else
         FBitPP:= 8 * 3
  else
  end;
//  Writeln(FWidth,'x',Fheight,' Maxval: ',FMaxVal,' BitPP: ',FBitPP);
end;

procedure TFPReaderPNM.InternalRead(Stream:TStream;Img:TFPCustomImage);

var
  Row:Integer;

begin
  FBufPos:=0;
  FBufLen:=0;
  ReadHeader(Stream);
  Img.SetSize(FWidth,FHeight);
  Case FBitmapType of
    5..9 : FScanLineSize:=(FBitPP div 8) * FWidth;
  else
    FScanLineSize:=FBitPP*((FWidth+7) shr 3);
  end;
  GetMem(FScanLine,FScanLineSize);
  try
    for Row:=0 to img.Height-1 do
      begin
      ReadScanLine(Row,Stream);
      WriteScanLine(Row,Img);
      end;
    if FBufPos<FBufLen then
      Stream.Seek(FBufPos-FBufLen,soCurrent);
  finally
    FBufPos:=0;
    FBufLen:=0;
    FreeMem(FScanLine);
  end;
end;

procedure TFPReaderPNM.ReadScanLine(Row : Integer; Stream:TStream);

Var
  P : PWord;
  I,j,bitsLeft : Integer;
  PB: PByte;
  PC: PCardinal;

begin
  Case FBitmapType of
    1 : begin
        PB:=FScanLine;
        For I:=0 to ((FWidth+7)shr 3)-1 do
          begin
            PB^:=0;
            bitsLeft := FWidth-(I shl 3)-1;
            if bitsLeft > 7 then bitsLeft := 7;
            for j:=0 to bitsLeft do
              PB^:=PB^ or (ReadBit(Stream) shl (7-j));
            Inc(PB);
          end;
        end;
    2 : begin
        P:=PWord(FScanLine);
        For I:=0 to FWidth-1 do
          begin
          P^:=ReadSample(Stream);
          Inc(P);
          end;
        end;
    3 : begin
        P:=PWord(FScanLine);
        For I:=0 to FWidth-1 do
          begin
          P^:=ReadSample(Stream); // Red
          Inc(P);
          P^:=ReadSample(Stream); // Green
          Inc(P);
          P^:=ReadSample(Stream); // Blue;
          Inc(P)
          end;
        end;
    8,9 : begin
          ReadScanLineBuffer(Stream,FScanLine,FScanLineSize);
          PC:=PCardinal(FScanLine);
          For I:=0 to (FScanLineSize div 4)-1 do
            begin
            if FLittleEndian then
              PC^:=LEtoN(PC^)
            else
              PC^:=BEtoN(PC^);
            Inc(PC);
            end;
          end;
    4..7 : begin
            ReadScanLineBuffer(Stream,FScanLine,FScanLineSize);
            if FMaxVal>255 then
              begin
              P:=PWord(FScanLine);
              For I:=0 to (FScanLineSize div 2)-1 do
                begin
                P^:=BEtoN(P^);
                Inc(P);
                end;
              end;
            end;
    end;
end;


procedure TFPReaderPNM.WriteScanLine(Row : Integer; Img : TFPCustomImage);

Var
  C : TFPColor;
  L : Cardinal;
  Scale: Int64;

  function ScaleByte(B: Byte):Word;
  begin
    if FMaxVal = 255 then
      Result := (B shl 8) or B { As used for reading .BMP files }
    else { Mimic the above with multiplications }
      begin
      if B>FMaxVal then
        B:=FMaxVal;
      Result := (Int64(B)*(FMaxVal+1) + B) * 65535 div Scale;
      end;
  end;

  function ScaleWord(W: Word):Word;
  begin
    if FMaxVal = 65535 then
      Result := W
    else { Mimic the above with multiplications }
      begin
      if W>FMaxVal then
        W:=FMaxVal;
      Result := (Int64(W)*(FMaxVal+1) + W) * 65535 div Scale;
      end;
  end;

  Procedure ByteBnWScanLine;

  Var
    P : PByte;
    I,j,x,bitsLeft : Integer;

  begin
    P:=PByte(FScanLine);
    For I:=0 to ((FWidth+7)shr 3)-1 do
      begin
      L:=P^;
      x := I shl 3;
      bitsLeft := FWidth-x-1;
      if bitsLeft > 7 then bitsLeft := 7;
      for j:=0 to bitsLeft do
        begin
          if L and $80 <> 0 then
            Img.Colors[x,Row]:=colBlack
          else
            Img.Colors[x,Row]:=colWhite;
          L:=L shl 1;
          inc(x);
        end;
      Inc(P);
      end;
  end;

  Procedure WordGrayScanLine;

  Var
    P : PWord;
    I : Integer;

  begin
    P:=PWord(FScanLine);
    For I:=0 to FWidth-1 do
      begin
      L:=ScaleWord(P^);
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
      C.Red:=ScaleWord(P^);
      Inc(P);
      C.Green:=ScaleWord(P^);
      Inc(P);
      C.Blue:=ScaleWord(P^);
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
      L:=ScaleByte(P^);
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
      C.Red:=ScaleByte(P^);
      Inc(P);
      C.Green:=ScaleByte(P^);
      Inc(P);
      C.Blue:=ScaleByte(P^);
      Img.Colors[I,Row]:=C;
      Inc(P);
      end;
  end;

  // Returns the next PAM sample, of one or two bytes.
  function NextSample(var aP: PByte): Word;

  begin
    if FMaxVal>255 then
      begin
      Result:=ScaleWord(PWord(aP)^);
      Inc(aP,2);
      end
    else
      begin
      Result:=ScaleByte(aP^);
      Inc(aP);
      end;
  end;

  Procedure PAMScanLine;

  Var
    P : PByte;
    I : Integer;

  begin
    P:=PByte(FScanLine);
    For I:=0 to FWidth-1 do
      begin
      C.Red:=NextSample(P);
      if FDepth<3 then
        begin
        C.Green:=C.Red;
        C.Blue:=C.Red;
        end
      else
        begin
        C.Green:=NextSample(P);
        C.Blue:=NextSample(P);
        end;
      if FDepth in [2,4] then
        C.Alpha:=NextSample(P);
      Img.Colors[I,Row]:=C;
      end;
  end;

  // Maps the bits of a linear sample to 0..65535, clamping values outside 0..1; NaN is 0.
  function FloatToWord(aBits: Cardinal): Word;

  var
    lValue: Single absolute aBits;

  begin
    if (aBits and $80000000<>0) or (aBits and $7FFFFFFF>$7F800000) then
      Result:=0
    else if lValue>=1 then
      Result:=65535
    else
      Result:=Round(lValue*65535);
  end;

  Procedure FloatScanLine;

  Var
    P : PCardinal;
    I,Y : Integer;

  begin
    P:=PCardinal(FScanLine);
    Y:=FHeight-1-Row;
    For I:=0 to FWidth-1 do
      begin
      C.Red:=FloatToWord(P^);
      Inc(P);
      if FDepth=1 then
        begin
        C.Green:=C.Red;
        C.Blue:=C.Red;
        end
      else
        begin
        C.Green:=FloatToWord(P^);
        Inc(P);
        C.Blue:=FloatToWord(P^);
        Inc(P);
        end;
      Img.Colors[I,Y]:=C;
      end;
  end;

begin
  C.Alpha:=AlphaOpaque;
  Scale := Int64(FMaxVal)*(FMaxVal+1) + FMaxVal;
  Case FBitmapType of
    1 : ByteBnWScanLine;
    2 : WordGrayScanline;
    3 : WordRGBScanline;
    4 : ByteBnWScanLine;
    5 : If FBitPP=8 then
          ByteGrayScanLine
        else
          WordGrayScanLine;
    6 : If FBitPP=24 then
          ByteRGBScanLine
        else
          WordRGBScanLine;
    7 : PAMScanLine;
    8,9 : FloatScanLine;
    end;
end;

initialization

  ImageHandlers.RegisterImageReader ('Netpbm Portable aNyMap', 'pnm', TFPReaderPNM);
  ImageHandlers.RegisterImageReader ('Netpbm Portable BitMap', 'pbm', TFPReaderPNM);
  ImageHandlers.RegisterImageReader ('Netpbm Portable GrayMap', 'pgm', TFPReaderPNM);
  ImageHandlers.RegisterImageReader ('Netpbm Portable PixelMap', 'ppm', TFPReaderPNM);
  ImageHandlers.RegisterImageReader ('Netpbm Portable Arbitrary Map', 'pam', TFPReaderPNM);
  ImageHandlers.RegisterImageReader ('Portable Float Map', 'pfm', TFPReaderPNM);

end.
