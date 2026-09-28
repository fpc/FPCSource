{
    The RIFF container of WebP files: chunk names, VP8X flags and chunk helpers shared by the
    WebP reader and writer.
    See the file COPYING.FPC, included in this distribution, for details.
}
{$mode objfpc}{$h+}
{$IFNDEF FPC_DOTTEDUNITS}
unit webpcomn;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.SysUtils, System.Classes;
{$ELSE FPC_DOTTEDUNITS}
uses
  SysUtils, Classes;
{$ENDIF FPC_DOTTEDUNITS}

type
  TWebPFourCC = array[0..3] of AnsiChar;

  { A chunk of a WebP file: its name, and the stream position and size of its data. }
  TWebPChunk = record
    FourCC: TWebPFourCC;
    Offset: Int64;
    Size: LongWord;
  end;
  TWebPChunks = array of TWebPChunk;

const
  WebPRIFF: TWebPFourCC = 'RIFF';
  WebPWEBP: TWebPFourCC = 'WEBP';
  WebPVP8: TWebPFourCC = 'VP8 ';
  WebPVP8L: TWebPFourCC = 'VP8L';
  WebPVP8X: TWebPFourCC = 'VP8X';
  WebPALPH: TWebPFourCC = 'ALPH';
  WebPANIM: TWebPFourCC = 'ANIM';
  WebPANMF: TWebPFourCC = 'ANMF';
  WebPICCP: TWebPFourCC = 'ICCP';
  WebPEXIF: TWebPFourCC = 'EXIF';
  WebPXMP: TWebPFourCC = 'XMP ';

  // Flags of the VP8X chunk.
  WebPFlagAnimation = $02;
  WebPFlagXMP = $04;
  WebPFlagExif = $08;
  WebPFlagAlpha = $10;
  WebPFlagICC = $20;

  // Flags of an ANMF chunk.
  WebPFrameDispose = $01;
  WebPFrameNoBlend = $02;

  WebPMax24 = $FFFFFF;
  // Pixels a canvas holds at most: the size of the largest VP8L image.
  WebPMaxPixels = Int64(16384) * 16384;

// Returns the little-endian 24-bit value at aData.
function WebPRead24(aData: PByte): LongWord;
// Stores aValue as a little-endian 24-bit value at aData.
procedure WebPWrite24(aData: PByte; aValue: LongWord);
// Writes a chunk: its name, its size, its data and a pad byte when the size is odd.
procedure WebPWriteChunk(aStream: TStream; const aFourCC: TWebPFourCC; const aData; aSize: LongWord);
// Returns the chunks of aSize bytes of data at aOffset in aStream; raises EReadError when one goes beyond it.
function WebPReadChunks(aStream: TStream; aOffset, aSize: Int64): TWebPChunks;

implementation

function WebPRead24(aData: PByte): LongWord;

begin
  Result := LongWord(aData[0]) or (LongWord(aData[1]) shl 8) or (LongWord(aData[2]) shl 16);
end;


procedure WebPWrite24(aData: PByte; aValue: LongWord);

begin
  aData[0] := aValue and $FF;
  aData[1] := (aValue shr 8) and $FF;
  aData[2] := (aValue shr 16) and $FF;
end;


procedure WebPWriteChunk(aStream: TStream; const aFourCC: TWebPFourCC; const aData; aSize: LongWord);

var
  lSize: LongWord;
  lPad: Byte;

begin
  aStream.WriteBuffer(aFourCC, 4);
  lSize := NtoLE(aSize);
  aStream.WriteBuffer(lSize, 4);
  if aSize > 0 then
    aStream.WriteBuffer(aData, aSize);
  if Odd(aSize) then
    begin
    lPad := 0;
    aStream.WriteBuffer(lPad, 1);
    end;
end;


function WebPReadChunks(aStream: TStream; aOffset, aSize: Int64): TWebPChunks;

var
  lPos, lEnd: Int64;
  lHeader: packed record
    FourCC: TWebPFourCC;
    Size: LongWord;
  end;

begin
  Result := nil;
  lPos := aOffset;
  lEnd := aOffset + aSize;
  while lPos + 8 <= lEnd do
    begin
    aStream.Position := lPos;
    aStream.ReadBuffer(lHeader, 8);
    lHeader.Size := LEtoN(lHeader.Size);
    if lPos + 8 + lHeader.Size > lEnd then
      raise EReadError.Create('WebP chunk beyond the end of the file');
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)].FourCC := lHeader.FourCC;
    Result[High(Result)].Offset := lPos + 8;
    Result[High(Result)].Size := lHeader.Size;
    lPos := lPos + 8 + lHeader.Size + (lHeader.Size and 1);
    end;
end;


end.
