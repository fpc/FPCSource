{ %target=darwin }
{ %cpu=x86_64,aarch64 }
{ NSImageResizingMode and NSTextAlignment constants of CocoaAll do in AppKit what their names say }
program tw39386;

{$mode objfpc}{$H+}
{$modeswitch objectivec2}

uses
  SysUtils, CocoaAll;

var
  gErrors: Integer = 0;

// Report aMsg as a failure when aOK is false.
procedure Check(aOK: Boolean; const aMsg: string);

begin
  if aOK then
    Writeln('ok:   ', aMsg)
  else
  begin
    Writeln('FAIL: ', aMsg);
    Inc(gErrors);
  end;
end;


// Return an RGBA bitmap of aWidth by 1 pixels.
function NewBitmap(aWidth: Integer): NSBitmapImageRep;

begin
  Result := NSBitmapImageRep.alloc.initWithBitmapDataPlanes_pixelsWide_pixelsHigh_bitsPerSample_samplesPerPixel_hasAlpha_isPlanar_colorSpaceName_bytesPerRow_bitsPerPixel(
    nil, aWidth, 1, 8, 4, True, False, NSDeviceRGBColorSpace, 0, 0);
end;


// Set the RGBA bytes of the pixel at aX of aRep to aR, aG, aB, 255.
procedure SetPixel(aRep: NSBitmapImageRep; aX: Integer; aR, aG, aB: Byte);

var
  lPixel: PByte;

begin
  lPixel := PByte(aRep.bitmapData) + aX * 4;
  lPixel[0] := aR;
  lPixel[1] := aG;
  lPixel[2] := aB;
  lPixel[3] := 255;
end;


// Return R, B, K (black) or ? for the RGBA bytes of the pixel at aX of aRep.
function PixelChar(aRep: NSBitmapImageRep; aX: Integer): Char;

var
  lPixel: PByte;

begin
  lPixel := PByte(aRep.bitmapData) + aX * 4;
  if lPixel[3] < 200 then
    Result := '?'
  else if (lPixel[0] > 180) and (lPixel[2] < 80) then
    Result := 'R'
  else if (lPixel[2] > 180) and (lPixel[0] < 80) then
    Result := 'B'
  else if (lPixel[0] < 80) and (lPixel[2] < 80) then
    Result := 'K'
  else
    Result := '?';
end;


// Return the RGBA bytes of the first aCount pixels of aRep as text.
function PixelBytes(aRep: NSBitmapImageRep; aCount: Integer): string;

var
  lPixel: PByte;
  i: Integer;

begin
  Result := '';
  lPixel := PByte(aRep.bitmapData);
  for i := 0 to aCount - 1 do
    Result := Result + Format(' %d,%d,%d,%d', [lPixel[i * 4], lPixel[i * 4 + 1], lPixel[i * 4 + 2], lPixel[i * 4 + 3]]);
end;


// Draw a 4-pixel image K R B K with 1-pixel caps into 8 pixels using aMode; return the 6 centre pixels.
function DrawResized(aMode: NSImageResizingMode): string;

var
  lSource, lTarget: NSBitmapImageRep;
  lImage: NSImage;
  lInsets: NSEdgeInsets;
  lContext: NSGraphicsContext;
  i: Integer;

begin
  lSource := NewBitmap(4);
  SetPixel(lSource, 0, 0, 0, 0);
  SetPixel(lSource, 1, 255, 0, 0);
  SetPixel(lSource, 2, 0, 0, 255);
  SetPixel(lSource, 3, 0, 0, 0);
  Check(PixelChar(lSource, 0) + PixelChar(lSource, 1) + PixelChar(lSource, 2) + PixelChar(lSource, 3) = 'KRBK',
    'source image is black, red, blue, black');
  lImage := NSImage.alloc.initWithSize(NSMakeSize(4, 1));
  lImage.addRepresentation(lSource);
  lInsets.top := 0;
  lInsets.bottom := 0;
  lInsets.left := 1;
  lInsets.right := 1;
  lImage.setCapInsets(lInsets);
  lImage.setResizingMode(aMode);

  lTarget := NewBitmap(8);
  lContext := NSGraphicsContext.graphicsContextWithBitmapImageRep(lTarget);
  NSGraphicsContext.classSaveGraphicsState;
  NSGraphicsContext.setCurrentContext(lContext);
  lContext.setImageInterpolation(NSImageInterpolationNone);
  lImage.drawInRect(NSMakeRect(0, 0, 8, 1));
  lContext.flushGraphics;
  NSGraphicsContext.classRestoreGraphicsState;

  Result := '';
  for i := 1 to 6 do
    Result := Result + PixelChar(lTarget, i);
  if (Result <> 'RRRBBB') and (Result <> 'RBRBRB') then
    Writeln('      target RGBA bytes:', PixelBytes(lTarget, 8));
  lImage.release;
  lSource.release;
  lTarget.release;
end;


// Return the RTF AppKit writes for a paragraph with alignment aAlignment.
function AlignmentRTF(aAlignment: NSTextAlignment): AnsiString;

var
  lStyle: NSMutableParagraphStyle;
  lText: NSAttributedString;
  lData: NSData;

begin
  lStyle := NSMutableParagraphStyle.alloc.init;
  lStyle.setAlignment(aAlignment);
  lText := NSAttributedString.alloc.initWithString_attributes(NSSTR('x'),
    NSDictionary.dictionaryWithObject_forKey(lStyle, NSParagraphStyleAttributeName));
  lData := lText.RTFFromRange_documentAttributes(NSMakeRange(0, 1), nil);
  SetLength(Result, lData.length);
  if lData.length > 0 then
    Move(lData.bytes^, Result[1], lData.length);
  lText.release;
  lStyle.release;
end;


var
  lPool: NSAutoreleasePool;
  lResult, lRTF: string;

begin
  lPool := NSAutoreleasePool.alloc.init;
  NSApplication.sharedApplication;
  Writeln('NSImageResizingModeStretch = ', NSImageResizingModeStretch, ', NSImageResizingModeTile = ', NSImageResizingModeTile);
  Writeln('NSLeftTextAlignment = ', NSLeftTextAlignment, ', NSCenterTextAlignment = ', NSCenterTextAlignment,
    ', NSRightTextAlignment = ', NSRightTextAlignment);

  lResult := DrawResized(NSImageResizingModeStretch);
  Check(lResult = 'RRRBBB', 'NSImageResizingModeStretch stretches the centre (expected RRRBBB, got ' + lResult + ')');
  lResult := DrawResized(NSImageResizingModeTile);
  Check(lResult = 'RBRBRB', 'NSImageResizingModeTile tiles the centre (expected RBRBRB, got ' + lResult + ')');

  lRTF := AlignmentRTF(NSCenterTextAlignment);
  Check(Pos('\qc', lRTF) > 0, 'NSCenterTextAlignment is written as \qc (centred) in RTF');
  lRTF := AlignmentRTF(NSRightTextAlignment);
  Check(Pos('\qr', lRTF) > 0, 'NSRightTextAlignment is written as \qr (right aligned) in RTF');
  lRTF := AlignmentRTF(NSLeftTextAlignment);
  Check((Pos('\qc', lRTF) = 0) and (Pos('\qr', lRTF) = 0), 'NSLeftTextAlignment is neither centred nor right aligned in RTF');

  lPool.release;
  if gErrors > 0 then
    Halt(1);
end.
