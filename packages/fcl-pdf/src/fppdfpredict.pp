{ **********************************************************************
  This file is part of the Free Component Library

  PDF predictor (de)compression
  Copyright (c) 2022 by Michael Van Canneyt michael@freepascal.org

  See the file COPYING.FPC, included in this distribution,
  for details about the copyright.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  **********************************************************************}

unit fppdfpredict;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpreadpng  ;

Type
  EPDFPredict = Class(Exception);

  { TPDFPredictStream }

  TPDFPredictStream = Class(TOwnerStream)
    FPredictor : Integer;
    Fcolumns : Integer;
    Fcolors : Integer;
    FBitsPerComponent : integer;
    FStride : Integer;
    Fbpp : Integer;
    finbuffer,
    foutbuffer,
    frefbuffer : TBytes;
    FRefPos : PByte;
    freadPos,
    fwritePos : Pbyte;
    Buffer : Array[0..4096] of byte;
  private
    function GetComponent(Line: PByte; x : Integer): Integer;
    Procedure PredictTiff(aOut,aIn : PByte);
    procedure PredictPng(aOut, aIn: PByte; len : Integer; aPredictor : Byte);
    Procedure PutComponent(buf: PByte; x, value: Integer);
    class function Paeth(l,p,lp: integer): integer;
  Public
    Constructor Create(aSource : TStream; aPredictor,aColumns,aColors,aBitsPerComponent : Integer);
    function Read(var aBuffer ; aCount : Integer) : Integer; override;
  end;

implementation


function TPDFPredictStream.GetComponent(Line : PByte; x : Integer) : Integer;
begin
  Case FBitsPerComponent of
     1:  Result :=(line[x shr 3] shr (7-(x and 7))) and 1;
     2:  Result :=(line[x shr 2] shr ((3-(x and 3)) shl 1)) and 3;
     4:  Result :=(line[x shr 1] shr ((1-(x and 1)) shl 2)) and 15;
     8:  Result :=line[x];
     16: Result :=(line[x shl 1] shl 8)+line[(x shl 1)+1];
  else
   Result:=0;
  end;
end;


procedure TPDFPredictStream.PutComponent(buf : PByte; x, value: Integer);
begin
  Case FBitsPerComponent of
  1:
    buf[x shr 3] := buf[x shr 3] or (value shl (7 - (x and 7)));
  2:
    buf[x shr 2] := buf[x shr 2] or (value shl ((3 - (x and 3)) shl 1));
  4:
    buf[x shr 1] := buf[x shr 1] or (value shl ((1 - (x and 1)) shl 2));
  8:
    buf[x]:=value;
  16:
    begin
    buf[x shl 1] := value shr 8;
    buf[(x shl 1)+1]:=value;
    end;
  end;
end;


class function TPDFPredictStream.Paeth(l,p,lp: integer) : integer;

// taken from ReadPNG

Var
  dl,dp,dlp : Integer;
  r : Integer;

begin
  r:=l+p-lp;
  dl:=abs(r-l);
  dp:=abs(r-p);
  dlp:=abs(r-lp);
  if (dl <= dp) and (dl <= dlp) then
    Result:=l
  else if dp <= dlp then
    Result:=p
  else
    Result:=lp;
end;

constructor TPDFPredictStream.Create(aSource: TStream; aPredictor, aColumns, aColors, aBitsPerComponent: Integer);

begin
  Inherited Create(aSource);
  if Not (aPredictor in [1,2,10,11,12,13,14,15]) then
    Raise EPDFPredict.CreateFmt('Invalid predictor value: %d',[aPredictor]);
  if Not (aBitsPerComponent in [1,2,4,8,16]) then
    Raise EPDFPredict.CreateFmt('Invalid bits per component: %d',[aBitsPerComponent]);
  if (aColors > 32) then
    Raise EPDFPredict.CreateFmt('Invalid amount of: %d',[aColors]);
  if (aColumns > (MaxInt div (aBitsPerComponent*aColors))) then
    Raise EPDFPredict.CreateFmt('Too many columns leads to overflow: %d',[aColumns]);
  FPredictor:=aPredictor;
  Fcolumns:=aColumns;
  FBitsPerComponent:=aBitsPerComponent;
  FColors:=aColors;
  FStride:=((FBitsPerComponent * FColors * FColumns) + 7) div 8;
  // Writeln('bpc ',FBitsPerComponent,', colors: ',FColors,' columns: ',FColumns,' Stride ',7);
  FBPP:=((FBitsPerComponent * FColors ) + 7) div 8;
  SetLength(FInbuffer,FStride+1);
  SetLength(FOutbuffer,FStride);
  SetLength(FRefBuffer,FStride);
  FReadPos:=PByte(FOutBuffer);
  FWritePos:=PByte(FOutBuffer);
end;

procedure TPDFPredictStream.PredictTiff(aOut, aIn: PByte);

Var
  Left : array[0..31] of integer;
  Mask,i,j : integer;
  x,a,b,c : Integer;

begin
  Mask:=(1 shl FBitsPerComponent)-1;
  for I:=0 to FColors-1 do
    Left[I]:=0;
  if (FBitsPerComponent=8) then
    begin
    for I:=0 to FColumns-1 do
      for J:=0 to FColors-1 do
        begin
        Left[J]:=(aIn^+left[J]) and $FF;
        aOut^:=Left[J];
        Inc(aIn);
        Inc(aOut);
        end;
    exit;
    end;
  if (FBitsPerComponent<8) then
    FillChar(aOut^,Fstride,0);
  for I:=0 to FColumns-1 do
    for J:=0 to FColors-1 do
      begin
      x:=(i*FColors)+j;
      a:=GetComponent(aIn,x);
      b:=a+left[J];
      c:=b and mask;
      PutComponent(aOut,x,c);
      Left[J]:=c;
      end;
end;

procedure TPDFPredictStream.PredictPng(aOut, aIn: PByte; len: Integer; aPredictor : Byte);

var
  I : integer;
  bpp : Integer;
  ref : PByte;

begin
  Ref:=PByte(FRefbuffer);
  bpp:=FBPP;
  if bpp>len then
    bpp:=len;
  Case aPredictor of
    0 :
      move(aIn^,aOut^,len);
    1 :
      begin
      move(aIn^,aOut^,bpp);
      inc(aIn,bpp);
      Inc(aOut,bpp);
      for I:=len-bpp downto 1 do
        begin
        aout^:=aIn^+aOut[-bpp];
        inc(aOut);
        end;
      end;
    2 :
      begin
      for I:=1 to bpp do
        begin
        aOut^:=aIn^+Ref^;
        inc(aOut);
        inc(aIn);
        inc(Ref);
        end;
      for I:=Len-bpp downto 1 do
        begin
        aOut^:=aIn^+Ref^;
        inc(aout);
        inc(aIn);
        inc(Ref);
        end;
      end;
    3:
      begin
      for I:=1 to bpp do
        begin
        aOut^:=aIn^+(Ref^ div 2);
        inc(aout);
        inc(aIn);
        inc(Ref);
        end;
      for I:=Len-bpp downto 1 do
        begin
        aOut^:=aIn^+((aOut[-bpp] + ref^) div 2);
        inc(aout);
        inc(aIn);
        inc(Ref);
        end;
      end;
    4:
      begin
      for I:=1 to bpp do
        begin
        aOut^:=aIn^+Paeth(0,ref^,0);
        inc(aout);
        inc(aIn);
        inc(Ref);
        end;
      for I:=Len-bpp downto 1 do
        begin
        aOut^:=aIn^+Paeth(aOut[-bpp],Ref^,ref[-bpp]);
        inc(aout);
        inc(aIn);
        inc(Ref);
        end;
      end;
  else
    // Do nothing
  end;
end;

function TPDFPredictStream.Read(var aBuffer ; aCount : Integer) : Integer;

var
  buf,p,ep : PByte;
  n : Integer;
  isPng : Boolean;

begin
  Result:=0;
  buf:=@aBuffer;
  p:=buf;
  isPng:=(FPredictor>=10);
  ep:=buf+aCount;
  // Copy rest of foutbuffer to aBuffer
  while (FReadPos < FWritePos) and (p<ep) do
     begin
     P^:=FReadPos^;
     Inc(p);
     Inc(FreadPos);
     Inc(Result);
     end;
  // Read more data to outbuffer.
  while (p<ep) do
    begin
    n:=Source.read(FInBuffer[0],FStride + Ord(IsPng));
    if (n=0) then
      break;
    if (FPredictor=1) then
      // Just move data
      move(FinBuffer[0], FOutBuffer[0], N)
    else if (FPredictor= 2) then
      // Tiff 2
      PredictTiff(PByte(FOutBuffer),PByte(FInBuffer))
    else
      begin
      // PNG - First byte is the actual predictor
      // System.Write('In[0] : ',FInBuffer[0],' -> ');
      PredictPNG(PByte(FOutBuffer),PByte(@FinBuffer[1]),N-1, FInBuffer[0]);
      // For I:=0 to Length(FoutBuffer)-1 do
      //  System.Write(FoutBuffer[i],' ');
      // writeln;
      // Move out to ref for next round.
      Move(FOutBuffer[0],FRefBuffer[0],FStride);
      end;
    FReadPos:=PByte(FOutBuffer);
    FWritePos:=PByte(@FoutBuffer[N-Ord(ispng)]);
    // Move to output buffer
    while (FReadPos<FWritePos) and (P<EP) do
      begin
      P^:=FReadPos^;
      inc(P);
      inc(FReadPos);
      Inc(Result);
      end;
    end;
end;

end.

