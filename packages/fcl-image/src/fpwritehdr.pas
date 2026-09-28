{
    Writer of Radiance HDR (RGBE) images, run-length encoded.
    This file is part of the Free Pascal run time library.
    See the file COPYING.FPC, included in this distribution, for details.
}
{$mode objfpc}{$h+}
{$IFNDEF FPC_DOTTEDUNITS}
unit fpwritehdr;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils, FpImage, FpImage.Common.HDR;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils, FpImage, hdrcomn;
{$ENDIF FPC_DOTTEDUNITS}

type
  { Writes a Radiance HDR image; colour components of 0..65535 are written as values of 0..1. }
  TFPWriterHDR = class(TFPCustomImageWriter)
  private
    FCompressed: Boolean;
  protected
    procedure InternalWrite(Stream: TStream; Img: TFPCustomImage); override;
  public
    // Creates a writer of run-length encoded scanlines.
    constructor Create; override;
    // Encodes scanlines of 8 to 32767 pixels by component; other scanlines are flat.
    property Compressed: Boolean read FCompressed write FCompressed;
  end;

implementation

constructor TFPWriterHDR.Create;

begin
  inherited Create;
  FCompressed := True;
end;


// Appends the run-length encoding of aCount bytes of aData, aStride bytes apart, to aOut.
procedure EncodeComponent(aData: PByte; aCount, aStride: Integer; var aOut: TBytes; var aLength: Integer);

var
  lPos, lRun, lLiteral: Integer;

  // Returns the number of equal bytes from aFrom on, up to 127.
  function RunAt(aFrom: Integer): Integer;

  begin
    Result := 1;
    while (aFrom + Result < aCount) and (Result < 127)
          and (aData[(aFrom + Result) * aStride] = aData[aFrom * aStride]) do
      Inc(Result);
  end;

  procedure Put(aValue: Byte);

  begin
    aOut[aLength] := aValue;
    Inc(aLength);
  end;

begin
  lPos := 0;
  while lPos < aCount do
    begin
    lRun := RunAt(lPos);
    if lRun >= 3 then
      begin
      Put(128 + lRun);
      Put(aData[lPos * aStride]);
      Inc(lPos, lRun);
      Continue;
      end;
    lLiteral := 0;
    while (lPos + lLiteral < aCount) and (lLiteral < 128) and ((lLiteral = 0) or (RunAt(lPos + lLiteral) < 3)) do
      Inc(lLiteral);
    Put(lLiteral);
    while lLiteral > 0 do
      begin
      Put(aData[lPos * aStride]);
      Inc(lPos);
      Dec(lLiteral);
      end;
    end;
end;


procedure TFPWriterHDR.InternalWrite(Stream: TStream; Img: TFPCustomImage);

var
  lHeader: AnsiString;
  lPixels: array of TRGBE;
  lOut: TBytes;
  lLength, lX, lY, lComponent: Integer;
  lEncode: Boolean;

begin
  lHeader := Format(HDRMagic + #10'FORMAT=%s'#10#10'-Y %d +X %d'#10, [HDRFormatRGBE, Img.Height, Img.Width]);
  Stream.WriteBuffer(lHeader[1], Length(lHeader));
  if Img.Width = 0 then
    Exit;
  SetLength(lPixels, Img.Width);
  lEncode := Compressed and (Img.Width >= 8) and (Img.Width <= $7FFF);
  SetLength(lOut, 4 + Img.Width * 5);
  for lY := 0 to Img.Height - 1 do
    begin
    for lX := 0 to Img.Width - 1 do
      lPixels[lX] := ColorToRGBE(Img.Colors[lX, lY]);
    if not lEncode then
      begin
      Stream.WriteBuffer(lPixels[0], Length(lPixels) * SizeOf(TRGBE));
      Continue;
      end;
    lOut[0] := 2;
    lOut[1] := 2;
    lOut[2] := Img.Width shr 8;
    lOut[3] := Img.Width and $FF;
    lLength := 4;
    for lComponent := 0 to 3 do
      EncodeComponent(PByte(@lPixels[0]) + lComponent, Img.Width, SizeOf(TRGBE), lOut, lLength);
    Stream.WriteBuffer(lOut[0], lLength);
    end;
end;


initialization
  ImageHandlers.RegisterImageWriter('Radiance HDR', 'hdr', TFPWriterHDR);
end.
