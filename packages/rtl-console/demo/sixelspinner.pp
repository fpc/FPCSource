{
  Show a waiting animation (a rotating circle of dots) in a sixel capable terminal,
  centered on the screen or at the cursor position. 
  Stops on a key press or after --duration seconds.
}
program SixelSpinner;

{$mode objfpc}
{$h+}

uses
  {$IFDEF UNIX}BaseUnix,{$ENDIF}
  SysUtils, Math, FPSixel;

type
  TPlacement = (plCenter, plCursor);

  TOptions = record
    Placement : TPlacement;
    Size : Integer;
    Dots : Integer;
    Delay : Integer;
    Duration : Integer;
    Foreground : TSixelColor;
    Background : TSixelColor;
    Force : Boolean;
  end;

var
  Terminated : Boolean = False;

// Print usage information, preceded by aError if it is not empty, and stop.
procedure Usage(const aError : string);

begin
  if aError <> '' then
    Writeln('Error: ', aError);
  Writeln('Usage: ', ExtractFileName(ParamStr(0)), ' [options]');
  Writeln('  -p, --position POS   center (default) or cursor');
  Writeln('  -s, --size N         Size of the animation in pixels (default: 96)');
  Writeln('  -n, --dots N         Number of dots (default: 12)');
  Writeln('  -i, --interval N     Milliseconds between frames (default: 80)');
  Writeln('  -d, --duration N     Stop after N seconds; 0 waits for a key (default: 0)');
  Writeln('      --fg RRGGBB      Dot color (default: 3C9CFF)');
  Writeln('      --bg RRGGBB      Background color (default: 000000)');
  Writeln('  -f, --force          Draw even if no sixel support was detected');
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
  S, lValue : string;

  // Value of the option at position I, which moves to the next argument.
  function NextValue : string;

  begin
    if I >= ParamCount then
      Usage('Missing value for option ' + S);
    Inc(I);
    Result := ParamStr(I);
  end;

  // Integer value of the option at position I, at least aMin.
  function NextInt(aMin : Integer) : Integer;

  begin
    lValue := NextValue;
    if not TryStrToInt(lValue, Result) or (Result < aMin) then
      Usage('Invalid number for option ' + S + ': ' + lValue);
  end;

begin
  aOptions := Default(TOptions);
  aOptions.Size := 96;
  aOptions.Dots := 12;
  aOptions.Delay := 80;
  aOptions.Foreground := SixelColor($3C, $9C, $FF);
  I := 1;
  while I <= ParamCount do
    begin
    S := ParamStr(I);
    if (S = '-p') or (S = '--position') then
      begin
      lValue := LowerCase(NextValue);
      if lValue = 'center' then
        aOptions.Placement := plCenter
      else if lValue = 'cursor' then
        aOptions.Placement := plCursor
      else
        Usage('Invalid position: ' + lValue);
      end
    else if (S = '-s') or (S = '--size') then
      aOptions.Size := NextInt(16)
    else if (S = '-n') or (S = '--dots') then
      aOptions.Dots := NextInt(3)
    else if (S = '-i') or (S = '--interval') then
      aOptions.Delay := NextInt(10)
    else if (S = '-d') or (S = '--duration') then
      aOptions.Duration := NextInt(0)
    else if S = '--fg' then
      aOptions.Foreground := HexToColor(NextValue)
    else if S = '--bg' then
      aOptions.Background := HexToColor(NextValue)
    else if (S = '-f') or (S = '--force') then
      aOptions.Force := True
    else if (S = '-?') or (S = '--help') then
      Usage('')
    else
      Usage('Unknown option: ' + S);
    Inc(I);
    end;
end;


{$IFDEF UNIX}
// Signal handler that ends the animation loop.
procedure HandleSignal(aSignal : cint); cdecl;

begin
  Terminated := True;
end;
{$ENDIF}


// Encode the animation frame in which dot aHead is the leading one.
function RenderFrame(aBitmap : TSixelBitmap; const aOptions : TOptions; aHead : Integer) : string;

var
  I, lAge : Integer;
  lCenter, lRing, lMaxDot, lMinDot, lAngle, lStrength : Double;

begin
  aBitmap.Clear(aOptions.Background);
  lCenter := aOptions.Size / 2;
  lMaxDot := aOptions.Size * 0.5 * Sin(Pi / aOptions.Dots) * 0.9;
  lMaxDot := Min(lMaxDot, aOptions.Size * 0.1);
  lMinDot := lMaxDot * 0.35;
  lRing := lCenter - lMaxDot - 1;
  for I := 0 to aOptions.Dots - 1 do
    begin
    lAge := (aHead - I + aOptions.Dots) mod aOptions.Dots;
    lStrength := 1 - lAge / aOptions.Dots;
    lAngle := -Pi / 2 + 2 * Pi * I / aOptions.Dots;
    aBitmap.FillCircle(lCenter + lRing * Cos(lAngle), lCenter + lRing * Sin(lAngle),
                       lMinDot + (lMaxDot - lMinDot) * lStrength,
                       SixelBlend(aOptions.Background, aOptions.Foreground, 0.15 + 0.85 * lStrength));
    end;
  Result := aBitmap.ToSixel;
end;


var
  lOptions : TOptions;
  lInfo : TSixelTerminalInfo;
  lBitmap : TSixelBitmap;
  lFrames : array of string;
  lCols, lRows, lColumn, lRow, I, lFrame : Integer;
  lPrefix : string;
  lStop : QWord;

begin
  ParseOptions(lOptions);
  DetectSixel(lInfo);
  if not (lInfo.Supported or lOptions.Force) then
    begin
    Writeln('This terminal does not seem to support sixel graphics (try e.g. "xterm -ti vt340").');
    Writeln('Use --force to draw anyway.');
    Halt(2);
    end;
  lBitmap := TSixelBitmap.Create(lOptions.Size, lOptions.Size);
  try
    lBitmap.MaxColors := SixelRequestColors(lInfo, 64);
    SetLength(lFrames, lOptions.Dots);
    for I := 0 to lOptions.Dots - 1 do
      lFrames[I] := RenderFrame(lBitmap, lOptions, I);
  finally
    lBitmap.Free;
  end;
  lCols := (lOptions.Size + lInfo.CellWidth - 1) div lInfo.CellWidth;
  lRows := (lOptions.Size + lInfo.CellHeight - 1) div lInfo.CellHeight;
  if lOptions.Placement = plCenter then
    begin
    lColumn := Max(1, (lInfo.Columns - lCols) div 2 + 1);
    lRow := Max(1, (lInfo.Rows - lRows) div 2 + 1);
    lPrefix := #27'[' + IntToStr(lRow) + ';' + IntToStr(lColumn) + 'H';
    TSixelTerminal.Write(#27'[2J');
    end
  else
    begin
    TSixelTerminal.Write(StringOfChar(#10, lRows) + #27'[' + IntToStr(lRows) + 'A'#13);
    lPrefix := '';
    end;
{$IFDEF UNIX}
  fpSignal(SIGINT, @HandleSignal);
  fpSignal(SIGTERM, @HandleSignal);
{$ENDIF}
  TSixelTerminal.SaveCursor;
  TSixelTerminal.ShowCursor(False);
  TSixelTerminal.EnterRawMode;
  try
    if lOptions.Duration > 0 then
      lStop := GetTickCount64 + QWord(lOptions.Duration) * 1000
    else
      lStop := High(QWord);
    lFrame := 0;
    while not Terminated and (GetTickCount64 < lStop) do
      begin
      TSixelTerminal.Write(#27'8' + lPrefix + lFrames[lFrame]);
      lFrame := (lFrame + 1) mod lOptions.Dots;
      if TSixelTerminal.InputAvailable(lOptions.Delay) then
        Terminated := True;
      end;
    TSixelTerminal.FlushInput;
  finally
    TSixelTerminal.LeaveRawMode;
    TSixelTerminal.RestoreCursor;
    if lOptions.Placement = plCursor then
      TSixelTerminal.Write(#27'[' + IntToStr(lRows) + 'B'#13#10)
    else
      TSixelTerminal.Write(#27'[2J'#27'[H');
    TSixelTerminal.ShowCursor(True);
  end;
end.
