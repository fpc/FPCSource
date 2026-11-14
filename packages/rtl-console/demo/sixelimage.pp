{
  Display an image file in a sixel capable terminal.
  Usage: sixelimage [options] filename; sixelimage --help lists the options.
}
program SixelImage;

{$mode objfpc}
{$h+}

uses
  SysUtils, Math, FPImage, FPReadPNG, FPReadJPEG, FPReadBMP, FPReadGIF, FPReadPNM,
  FPReadTGA, FPReadTiff, FPReadPCX, FPReadXPM, FPReadQOI, FPReadWebP, FPSixel;

type
  TOptions = record
    FileName : string;
    MaxWidth : Integer;
    MaxHeight : Integer;
    Colors : Integer;
    Dither : Boolean;
    FixedPalette : Boolean;
    Transparent : Boolean;
    Force : Boolean;
    Verbose : Boolean;
    Background : TSixelColor;
  end;

const
  TransparentKey : TSixelColor = (R: 255; G: 0; B: 254);

// Print usage information, preceded by aError if it is not empty, and stop.
procedure Usage(const aError : string);

begin
  if aError <> '' then
    Writeln('Error: ', aError);
  Writeln('Usage: ', ExtractFileName(ParamStr(0)), ' [options] filename');
  Writeln('  -w, --width N        Maximum width in pixels (default: terminal width)');
  Writeln('  -h, --height N       Maximum height in pixels (default: terminal height)');
  Writeln('  -c, --colors N       Number of palette colors (default: 256)');
  Writeln('  -d, --dither         Use Floyd-Steinberg dithering');
  Writeln('  -x, --fixed          Use a fixed palette instead of an adaptive one');
  Writeln('  -t, --transparent    Leave transparent pixels undrawn');
  Writeln('  -b, --background HEX Color behind transparent pixels, as RRGGBB (default: 000000)');
  Writeln('  -f, --force          Draw even if no sixel support was detected');
  Writeln('  -v, --verbose        Show the detected terminal properties');
  Writeln('  -?, --help           Show this help');
  if aError <> '' then
    Halt(1)
  else
    Halt(0);
end;


// Convert an RRGGBB hexadecimal string to a color.
function HexToColor(const aHex : string) : TSixelColor;

var
  lValue : Integer;

begin
  if (Length(aHex) <> 6) or not TryStrToInt('$' + aHex, lValue) then
    Usage('Invalid color: ' + aHex);
  Result := SixelColor((lValue shr 16) and $FF, (lValue shr 8) and $FF, lValue and $FF);
end;


// Read the command-line options into aOptions.
procedure ParseOptions(out aOptions : TOptions);

var
  I : Integer;
  S : string;

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

  var
    lValue : string;

  begin
    lValue := NextValue;
    if not TryStrToInt(lValue, Result) or (Result <= 0) then
      Usage('Invalid number for option ' + S + ': ' + lValue);
  end;

begin
  aOptions := Default(TOptions);
  aOptions.Colors := 256;
  I := 1;
  while I <= ParamCount do
    begin
    S := ParamStr(I);
    if (S = '-w') or (S = '--width') then
      aOptions.MaxWidth := NextInt
    else if (S = '-h') or (S = '--height') then
      aOptions.MaxHeight := NextInt
    else if (S = '-c') or (S = '--colors') then
      aOptions.Colors := NextInt
    else if (S = '-d') or (S = '--dither') then
      aOptions.Dither := True
    else if (S = '-x') or (S = '--fixed') then
      aOptions.FixedPalette := True
    else if (S = '-t') or (S = '--transparent') then
      aOptions.Transparent := True
    else if (S = '-b') or (S = '--background') then
      aOptions.Background := HexToColor(NextValue)
    else if (S = '-f') or (S = '--force') then
      aOptions.Force := True
    else if (S = '-v') or (S = '--verbose') then
      aOptions.Verbose := True
    else if (S = '-?') or (S = '--help') then
      Usage('')
    else if (S <> '') and (S[1] = '-') then
      Usage('Unknown option: ' + S)
    else if aOptions.FileName = '' then
      aOptions.FileName := S
    else
      Usage('Only one file name is allowed');
    Inc(I);
    end;
  if aOptions.FileName = '' then
    Usage('No file name given');
end;


// Print the terminal properties found by DetectSixel.
procedure ShowInfo(const aInfo : TSixelTerminalInfo);

const
  MethodNames : array[TSixelDetectMethod] of string = ('none', 'device attributes', 'environment');

begin
  Writeln('Terminal         : ', aInfo.IsTerminal);
  Writeln('Sixel supported  : ', aInfo.Supported, ' (', MethodNames[aInfo.Method], ')');
  Writeln('Device attributes: ', aInfo.DeviceAttributes);
  Writeln('Color registers  : ', aInfo.ColorRegisters, ' (max ', aInfo.MaxColorRegisters, ')');
  Writeln('Max sixel size   : ', aInfo.MaxWidth, 'x', aInfo.MaxHeight);
  Writeln('Text size        : ', aInfo.Columns, 'x', aInfo.Rows, ' cells, ', aInfo.PixelWidth, 'x', aInfo.PixelHeight, ' pixels');
  Writeln('Cell size        : ', aInfo.CellWidth, 'x', aInfo.CellHeight, ' (known: ', aInfo.CellSizeKnown, ')');
end;


// Size of an aWidth x aHeight image scaled down to fit in aMaxWidth x aMaxHeight.
procedure FitSize(aWidth, aHeight, aMaxWidth, aMaxHeight : Integer; out aNewWidth, aNewHeight : Integer);

var
  lScale : Double;

begin
  lScale := Min(1.0, Min(aMaxWidth / aWidth, aMaxHeight / aHeight));
  aNewWidth := Max(1, Round(aWidth * lScale));
  aNewHeight := Max(1, Round(aHeight * lScale));
end;


// Copy aImage into aBitmap, averaging the source pixels that cover each target pixel.
procedure ScaleImage(aImage : TFPCustomImage; aBitmap : TSixelBitmap; const aOptions : TOptions);

var
  X, Y, SX, SY, lX0, lX1, lY0, lY1, lCount : Integer;
  lR, lG, lB, lA, lAlpha : Double;
  lColor : TFPColor;
  lPixel : TSixelColor;

begin
  for Y := 0 to aBitmap.Height - 1 do
    begin
    lY0 := Y * aImage.Height div aBitmap.Height;
    lY1 := Max(lY0 + 1, (Y + 1) * aImage.Height div aBitmap.Height);
    for X := 0 to aBitmap.Width - 1 do
      begin
      lX0 := X * aImage.Width div aBitmap.Width;
      lX1 := Max(lX0 + 1, (X + 1) * aImage.Width div aBitmap.Width);
      lR := 0;
      lG := 0;
      lB := 0;
      lA := 0;
      lCount := 0;
      for SY := lY0 to lY1 - 1 do
        for SX := lX0 to lX1 - 1 do
          begin
          lColor := aImage.Colors[SX, SY];
          lAlpha := lColor.Alpha / $FFFF;
          lR := lR + lColor.Red * lAlpha;
          lG := lG + lColor.Green * lAlpha;
          lB := lB + lColor.Blue * lAlpha;
          lA := lA + lAlpha;
          Inc(lCount);
          end;
      lAlpha := lA / lCount;
      if aOptions.Transparent and (lAlpha < 0.5) then
        lPixel := TransparentKey
      else
        begin
        if lA > 0 then
          lPixel := SixelColor(Round(lR / lA) shr 8, Round(lG / lA) shr 8, Round(lB / lA) shr 8)
        else
          lPixel := aOptions.Background;
        if not aOptions.Transparent then
          lPixel := SixelBlend(aOptions.Background, lPixel, lAlpha);
        if (lPixel.R = TransparentKey.R) and (lPixel.G = TransparentKey.G) and (lPixel.B = TransparentKey.B) then
          lPixel.B := TransparentKey.B + 1;
        end;
      aBitmap[X, Y] := lPixel;
      end;
    end;
end;


var
  lOptions : TOptions;
  lInfo : TSixelTerminalInfo;
  lImage : TFPMemoryImage;
  lBitmap : TSixelBitmap;
  lMaxWidth, lMaxHeight, lWidth, lHeight : Integer;

begin
  ParseOptions(lOptions);
  DetectSixel(lInfo);
  if lOptions.Verbose then
    ShowInfo(lInfo);
  if not (lInfo.Supported or lOptions.Force) then
    begin
    Writeln('This terminal does not seem to support sixel graphics (try e.g. "xterm -ti vt340").');
    Writeln('Use --force to draw anyway.');
    Halt(2);
    end;
  lMaxWidth := lOptions.MaxWidth;
  if lMaxWidth = 0 then
    lMaxWidth := IfThen(lInfo.PixelWidth > 0, lInfo.PixelWidth, 800);
  lMaxHeight := lOptions.MaxHeight;
  if lMaxHeight = 0 then
    lMaxHeight := IfThen(lInfo.Rows > 2, (lInfo.Rows - 2) * lInfo.CellHeight, 600);
  if lInfo.MaxWidth > 0 then
    lMaxWidth := Min(lMaxWidth, lInfo.MaxWidth);
  if lInfo.MaxHeight > 0 then
    lMaxHeight := Min(lMaxHeight, lInfo.MaxHeight);
  lImage := TFPMemoryImage.Create(0, 0);
  try
    try
      lImage.LoadFromFile(lOptions.FileName);
    except
      on E : Exception do
        begin
        Writeln('Cannot read ', lOptions.FileName, ': ', E.Message);
        Halt(3);
        end;
    end;
    FitSize(lImage.Width, lImage.Height, lMaxWidth, lMaxHeight, lWidth, lHeight);
    lBitmap := TSixelBitmap.Create(lWidth, lHeight);
    try
      lBitmap.MaxColors := SixelRequestColors(lInfo, lOptions.Colors);
      lBitmap.Dither := lOptions.Dither;
      if lOptions.FixedPalette then
        lBitmap.PaletteMode := spmFixed;
      lBitmap.Transparent := lOptions.Transparent;
      lBitmap.TransparentColor := TransparentKey;
      ScaleImage(lImage, lBitmap, lOptions);
      lBitmap.Draw;
      Writeln;
    finally
      lBitmap.Free;
    end;
  finally
    lImage.Free;
  end;
end.
