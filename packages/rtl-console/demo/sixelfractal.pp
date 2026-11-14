{
  Draw a fractal in a sixel capable terminal: 
  Mandelbrot, Julia, Sierpinski gasket or Koch snowflake.
  Usage: sixelfractal [-t type] [options]; sixelfractal --help lists the options.
  
  Based on an example by Haitham Shatti
}
program SixelFractal;

{$mode objfpc}
{$h+}

uses
  SysUtils, Math, FPSixel;

type
  TFractalType = (ftMandelbrot, ftJulia, ftSierpinski, ftKoch);

  TOptions = record
    FractalType : TFractalType;
    Width : Integer;
    Height : Integer;
    Iterations : Integer;
    Colors : Integer;
    JuliaRe : Double;
    JuliaIm : Double;
    Force : Boolean;
  end;

const
  DefaultIterations : array[TFractalType] of Integer = (256, 256, 7, 5);
  Background : TSixelColor = (R: 16; G: 16; B: 28);

// Print usage information, preceded by aError if it is not empty, and stop.
procedure Usage(const aError : string);

begin
  if aError <> '' then
    Writeln('Error: ', aError);
  Writeln('Usage: ', ExtractFileName(ParamStr(0)), ' [options]');
  Writeln('  -t, --type TYPE      mandelbrot (default), julia, sierpinski or koch');
  Writeln('  -w, --width N        Width in pixels (default: 800 or the terminal width)');
  Writeln('  -h, --height N       Height in pixels (default: 600 or the terminal height)');
  Writeln('  -i, --iterations N   Iterations (mandelbrot, julia) or recursion depth (sierpinski, koch)');
  Writeln('  -j, --julia RE,IM    Julia set constant (default: -0.8,0.156)');
  Writeln('  -c, --colors N       Number of palette colors (default: 256)');
  Writeln('  -f, --force          Draw even if no sixel support was detected');
  Writeln('  -?, --help           Show this help');
  if aError <> '' then
    Halt(1)
  else
    Halt(0);
end;


// Fractal type for a name given on the command line.
function ParseType(const aName : string) : TFractalType;

var
  lName : string;

begin
  lName := LowerCase(aName);
  if lName = 'mandelbrot' then
    Result := ftMandelbrot
  else if lName = 'julia' then
    Result := ftJulia
  else if (lName = 'sierpinski') or (lName = 'sierpinsky') then
    Result := ftSierpinski
  else if lName = 'koch' then
    Result := ftKoch
  else
    begin
    Usage('Unknown fractal type: ' + aName);
    Result := ftMandelbrot;
    end;
end;


// Read the command-line options into aOptions.
procedure ParseOptions(out aOptions : TOptions);

var
  I, P : Integer;
  S, lValue : string;
  lFormat : TFormatSettings;

  // Value of the option at position I, which moves to the next argument.
  function NextValue : string;

  begin
    if I >= ParamCount then
      Usage('Missing value for option ' + S);
    Inc(I);
    Result := ParamStr(I);
  end;

  // Integer value of the option at position I.
  function NextInt : Integer;

  begin
    lValue := NextValue;
    if not TryStrToInt(lValue, Result) or (Result <= 0) then
      Usage('Invalid number for option ' + S + ': ' + lValue);
  end;

begin
  aOptions := Default(TOptions);
  aOptions.Colors := 256;
  aOptions.JuliaRe := -0.8;
  aOptions.JuliaIm := 0.156;
  lFormat := DefaultFormatSettings;
  lFormat.DecimalSeparator := '.';
  I := 1;
  while I <= ParamCount do
    begin
    S := ParamStr(I);
    if (S = '-t') or (S = '--type') then
      aOptions.FractalType := ParseType(NextValue)
    else if (S = '-w') or (S = '--width') then
      aOptions.Width := NextInt
    else if (S = '-h') or (S = '--height') then
      aOptions.Height := NextInt
    else if (S = '-i') or (S = '--iterations') then
      aOptions.Iterations := NextInt
    else if (S = '-c') or (S = '--colors') then
      aOptions.Colors := NextInt
    else if (S = '-j') or (S = '--julia') then
      begin
      lValue := NextValue;
      P := Pos(',', lValue);
      if (P = 0)
         or not TryStrToFloat(Copy(lValue, 1, P - 1), aOptions.JuliaRe, lFormat)
         or not TryStrToFloat(Copy(lValue, P + 1, Length(lValue)), aOptions.JuliaIm, lFormat) then
        Usage('Invalid Julia constant: ' + lValue);
      end
    else if (S = '-f') or (S = '--force') then
      aOptions.Force := True
    else if (S = '-?') or (S = '--help') then
      Usage('')
    else
      Usage('Unknown option: ' + S);
    Inc(I);
    end;
  if aOptions.Iterations = 0 then
    aOptions.Iterations := DefaultIterations[aOptions.FractalType];
end;


// Color on a smooth cyclic gradient; aT and aT+1 give the same color.
function GradientColor(aT : Double) : TSixelColor;

begin
  Result := SixelColor(Round(127.5 + 127.5 * Cos(2 * Pi * (aT + 0.00))),
                       Round(127.5 + 127.5 * Cos(2 * Pi * (aT + 0.10))),
                       Round(127.5 + 127.5 * Cos(2 * Pi * (aT + 0.20))));
end;


// Draw the Mandelbrot set, or the Julia set for constant (aCRe,aCIm) if aJulia is True.
procedure DrawEscapeTime(aBitmap : TSixelBitmap; aJulia : Boolean; aCRe, aCIm : Double; aMaxIter : Integer);

var
  X, Y, N : Integer;
  lScale, lCenterRe, lZRe, lZIm, lCRe, lCIm, lTemp, lModulus, lSmooth : Double;

begin
  if aJulia then
    begin
    lCenterRe := 0;
    lScale := Max(3.2 / aBitmap.Width, 2.0 / aBitmap.Height);
    end
  else
    begin
    lCenterRe := -0.75;
    lScale := Max(3.5 / aBitmap.Width, 2.5 / aBitmap.Height);
    end;
  for Y := 0 to aBitmap.Height - 1 do
    for X := 0 to aBitmap.Width - 1 do
      begin
      lZRe := lCenterRe + (X - aBitmap.Width / 2) * lScale;
      lZIm := (Y - aBitmap.Height / 2) * lScale;
      if aJulia then
        begin
        lCRe := aCRe;
        lCIm := aCIm;
        end
      else
        begin
        lCRe := lZRe;
        lCIm := lZIm;
        lZRe := 0;
        lZIm := 0;
        end;
      N := 0;
      while (N < aMaxIter) and (lZRe * lZRe + lZIm * lZIm <= 256) do
        begin
        lTemp := lZRe * lZRe - lZIm * lZIm + lCRe;
        lZIm := 2 * lZRe * lZIm + lCIm;
        lZRe := lTemp;
        Inc(N);
        end;
      if N >= aMaxIter then
        aBitmap[X, Y] := SixelColor(0, 0, 0)
      else
        begin
        lModulus := Sqrt(lZRe * lZRe + lZIm * lZIm);
        lSmooth := N + 1 - Ln(Ln(lModulus)) / Ln(2);
        aBitmap[X, Y] := GradientColor(0.6 + lSmooth * 0.025);
        end;
      end;
end;


// Fill the Sierpinski gasket inside the triangle with the given corners.
procedure DrawSierpinski(aBitmap : TSixelBitmap; aX1, aY1, aX2, aY2, aX3, aY3 : Double; aDepth : Integer);

var
  lColor : TSixelColor;

begin
  if aDepth <= 0 then
    begin
    lColor := GradientColor(0.5 * ((aX1 + aX2 + aX3) / (3 * aBitmap.Width)) + 0.5 * ((aY1 + aY2 + aY3) / (3 * aBitmap.Height)));
    aBitmap.FillTriangle(Round(aX1), Round(aY1), Round(aX2), Round(aY2), Round(aX3), Round(aY3), lColor);
    Exit;
    end;
  DrawSierpinski(aBitmap, aX1, aY1, (aX1 + aX2) / 2, (aY1 + aY2) / 2, (aX1 + aX3) / 2, (aY1 + aY3) / 2, aDepth - 1);
  DrawSierpinski(aBitmap, (aX1 + aX2) / 2, (aY1 + aY2) / 2, aX2, aY2, (aX2 + aX3) / 2, (aY2 + aY3) / 2, aDepth - 1);
  DrawSierpinski(aBitmap, (aX1 + aX3) / 2, (aY1 + aY3) / 2, (aX2 + aX3) / 2, (aY2 + aY3) / 2, aX3, aY3, aDepth - 1);
end;


// Draw a Koch curve from (aX1,aY1) to (aX2,aY2); aIndex counts the segments drawn, aTotal is their number.
procedure DrawKochSegment(aBitmap : TSixelBitmap; aX1, aY1, aX2, aY2 : Double; aDepth : Integer;
  var aIndex : Integer; aTotal : Integer);

var
  lDX, lDY, lAX, lAY, lBX, lBY, lPX, lPY : Double;
  lColor : TSixelColor;

begin
  if aDepth <= 0 then
    begin
    lColor := GradientColor(aIndex / aTotal);
    Inc(aIndex);
    aBitmap.Line(Round(aX1), Round(aY1), Round(aX2), Round(aY2), lColor);
    aBitmap.Line(Round(aX1) + 1, Round(aY1), Round(aX2) + 1, Round(aY2), lColor);
    aBitmap.Line(Round(aX1), Round(aY1) + 1, Round(aX2), Round(aY2) + 1, lColor);
    Exit;
    end;
  lDX := (aX2 - aX1) / 3;
  lDY := (aY2 - aY1) / 3;
  lAX := aX1 + lDX;
  lAY := aY1 + lDY;
  lBX := aX1 + 2 * lDX;
  lBY := aY1 + 2 * lDY;
  lPX := lAX + lDX * Cos(-Pi / 3) - lDY * Sin(-Pi / 3);
  lPY := lAY + lDX * Sin(-Pi / 3) + lDY * Cos(-Pi / 3);
  DrawKochSegment(aBitmap, aX1, aY1, lAX, lAY, aDepth - 1, aIndex, aTotal);
  DrawKochSegment(aBitmap, lAX, lAY, lPX, lPY, aDepth - 1, aIndex, aTotal);
  DrawKochSegment(aBitmap, lPX, lPY, lBX, lBY, aDepth - 1, aIndex, aTotal);
  DrawKochSegment(aBitmap, lBX, lBY, aX2, aY2, aDepth - 1, aIndex, aTotal);
end;


// Draw a Koch snowflake of recursion depth aDepth, centered in the bitmap.
procedure DrawKoch(aBitmap : TSixelBitmap; aDepth : Integer);

var
  lCX, lCY, lR : Double;
  lX, lY : array[0..2] of Double;
  I, lIndex, lTotal : Integer;

begin
  lCX := aBitmap.Width / 2;
  lCY := aBitmap.Height / 2;
  lR := Min(aBitmap.Height * 0.45, aBitmap.Width * 0.9 / Sqrt(3));
  for I := 0 to 2 do
    begin
    lX[I] := lCX + lR * Cos(-Pi / 2 + I * 2 * Pi / 3);
    lY[I] := lCY + lR * Sin(-Pi / 2 + I * 2 * Pi / 3);
    end;
  lTotal := 3 * Round(Power(4, aDepth));
  lIndex := 0;
  for I := 0 to 2 do
    DrawKochSegment(aBitmap, lX[I], lY[I], lX[(I + 1) mod 3], lY[(I + 1) mod 3], aDepth, lIndex, lTotal);
end;


var
  lOptions : TOptions;
  lInfo : TSixelTerminalInfo;
  lBitmap : TSixelBitmap;
  lWidth, lHeight : Integer;
  lSide, lMargin : Double;

begin
  ParseOptions(lOptions);
  DetectSixel(lInfo);
  if not (lInfo.Supported or lOptions.Force) then
    begin
    Writeln('This terminal does not seem to support sixel graphics (try e.g. "xterm -ti vt340").');
    Writeln('Use --force to draw anyway.');
    Halt(2);
    end;
  lWidth := lOptions.Width;
  if lWidth = 0 then
    lWidth := Min(800, IfThen(lInfo.PixelWidth > 0, lInfo.PixelWidth, 800));
  lHeight := lOptions.Height;
  if lHeight = 0 then
    lHeight := Min(600, IfThen(lInfo.Rows > 2, (lInfo.Rows - 2) * lInfo.CellHeight, 600));
  if lInfo.MaxWidth > 0 then
    lWidth := Min(lWidth, lInfo.MaxWidth);
  if lInfo.MaxHeight > 0 then
    lHeight := Min(lHeight, lInfo.MaxHeight);
  lBitmap := TSixelBitmap.Create(lWidth, lHeight);
  try
    lBitmap.MaxColors := SixelRequestColors(lInfo, lOptions.Colors);
    lBitmap.Clear(Background);
    case lOptions.FractalType of
      ftMandelbrot :
        DrawEscapeTime(lBitmap, False, 0, 0, lOptions.Iterations);
      ftJulia :
        DrawEscapeTime(lBitmap, True, lOptions.JuliaRe, lOptions.JuliaIm, lOptions.Iterations);
      ftSierpinski :
        begin
        lMargin := 8;
        lSide := Min(lWidth - 2 * lMargin, (lHeight - 2 * lMargin) * 2 / Sqrt(3));
        DrawSierpinski(lBitmap,
          lWidth / 2, (lHeight - lSide * Sqrt(3) / 2) / 2,
          (lWidth - lSide) / 2, (lHeight + lSide * Sqrt(3) / 2) / 2,
          (lWidth + lSide) / 2, (lHeight + lSide * Sqrt(3) / 2) / 2,
          lOptions.Iterations);
        end;
      ftKoch :
        DrawKoch(lBitmap, lOptions.Iterations);
    end;
    lBitmap.Draw;
    Writeln;
  finally
    lBitmap.Free;
  end;
end.
