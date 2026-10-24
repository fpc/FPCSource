{ %GRAPH }
{ %TARGET=win32,win64 }

{ Changes palette entries after drawing and checks the RGB values of existing pixels }
program tw23281;

{$mode objfpc}

uses
  Windows, Graph;

const
  cBackX = 400;
  cBackY = 300;
  cRedX = 200;
  cRedY = 200;

var
  gErrors: Integer = 0;

// Reports a failed check
procedure Fail(const aMsg: string);

begin
  Writeln('FAIL: ', aMsg);
  Inc(gErrors);
end;


// Returns the RGB value of palette entry aColor
function PaletteRGB(aColor: smallint): COLORREF;

var
  lRed, lGreen, lBlue: smallint;

begin
  GetRGBPalette(aColor, lRed, lGreen, lBlue);
  Result := RGB(lRed, lGreen, lBlue);
end;


// Counts the pixels that differ from the background aBack in the text rectangle of the backing bitmap
function CountTextPixels(aBack: COLORREF): longint;

var
  lX, lY: longint;

begin
  Result := 0;
  for lY := 10 to 10 + TextHeight('Hello') - 1 do
    for lX := 10 to 10 + TextWidth('Hello') - 1 do
      if Windows.GetPixel(bitmapdc, lX, lY) <> aBack then
        Inc(Result);
end;


// Checks that the backing bitmap and the window show aColor at (aX,aY)
procedure CheckPixel(const aWhat: string; aX, aY: longint; aColor: COLORREF);

var
  lColor: COLORREF;

begin
  lColor := Windows.GetPixel(bitmapdc, aX, aY);
  if lColor <> aColor then
    Fail(aWhat + ' in bitmap: expected $' + HexStr(aColor, 6) + ', got $' + HexStr(lColor, 6));
  lColor := Windows.GetPixel(windc, aX, aY);
  if lColor <> aColor then
    Fail(aWhat + ' in window: expected $' + HexStr(aColor, 6) + ', got $' + HexStr(lColor, 6));
end;


var
  lDriver, lMode: smallint;
  lTextPixels: longint;

begin
  lDriver := VGA;
  lMode := VGAHi;
  InitGraph(lDriver, lMode, '');
  if GraphResult <> grOk then
  begin
    Writeln('InitGraph failed');
    Halt(1);
  end;
  ClearDevice;
  SetColor(White);
  OutTextXY(10, 10, 'Hello');
  PutPixel(cRedX, cRedY, Red);
  lTextPixels := CountTextPixels(PaletteRGB(Black));
  if lTextPixels = 0 then
    Fail('OutTextXY must draw text pixels');
  CheckPixel('background before SetBkColor', cBackX, cBackY, PaletteRGB(Black));

  SetBkColor(Blue);
  CheckPixel('background after SetBkColor(Blue)', cBackX, cBackY, PaletteRGB(Blue));
  CheckPixel('red pixel after SetBkColor(Blue)', cRedX, cRedY, PaletteRGB(Red));
  if CountTextPixels(PaletteRGB(Blue)) <> lTextPixels then
    Fail('SetBkColor must recolor only the background around the text');
  if Graph.GetPixel(cBackX, cBackY) <> 0 then
    Fail('GetPixel of the background must return color 0');

  SetRGBPalette(Red, 0, 255, 0);
  CheckPixel('red pixel after SetRGBPalette(Red, green)', cRedX, cRedY, RGB(0, 255, 0));
  CheckPixel('background after SetRGBPalette(Red, green)', cBackX, cBackY, PaletteRGB(Blue));

  SetBkColor(Black);
  CheckPixel('background after SetBkColor(Black)', cBackX, cBackY, PaletteRGB(Black));
  CloseGraph;

  if gErrors > 0 then
  begin
    Writeln(gErrors, ' errors');
    Halt(1);
  end;
  Writeln('ok');
end.
