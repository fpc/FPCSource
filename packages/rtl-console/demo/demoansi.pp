program demoansi;

{$mode objfpc}
{$h+}

uses sysutils, fpansi;

var
  ANI : Array[0..3] of string = ('|','/','-','\');

var
  i : Integer;
  aText : TAnsi;
  aLine : AnsiString;
  
procedure Pause;
begin
  Writeln;
  Write('Press Enter to continue...');
  Readln;
  Writeln(TAnsi.EraseDisplay(edScreen));
  Writeln(TAnsi.CursorAt(1,1));
end;

begin
  Writeln(TAnsi.EraseDisplay(edScreen));  
  Writeln('--- Basic & Existing Demo ---');
  aText:='Hello world!';
  aLine:=aText.Bold.Fg(TAnsi.Red);
  Writeln(TAnsi.CursorAt(1,1),aLine);
  aText:='Hello world, again!';
  aText.Bold.Fg(TAnsi.BrightGreen).At(2,1).EmitLn;
  
  Write('Running animation... ');
  for I:=1 to 100 do
    begin
    aText:=Ani[i mod 4]+' '+Format('%.2d',[(100-I)]);
    AText.Backward(4).FG(TAnsi.BrightRed).Emit;
    Sleep(20);
    end;
  
  Pause;

  // --- Attributes Demo ---
  Writeln('--- Attributes Demo ---');
  aText := 'This text is BLINKING (may not work in all terminals)';
  aText.Blinking.EmitLn;
  
  aText := 'This text is FAINT';
  aText.Faint.EmitLn;
  
  aText := 'This text is STRIKETHROUGH';
  aText.Strikethrough.EmitLn;
  
  aText := 'This text is BOLD and FAINT combined';
  aText.Bold.Faint.EmitLn;

  Pause;

  // --- Colors Demo ---
  Writeln('--- Colors Demo ---');
  aText := 'Standard Blue Background with White Text';
  aText.Bg(TAnsi.Blue).Fg(TAnsi.White).EmitLn;

  aText := 'Custom RGB Foreground (Orange: 255, 165, 0)';
  aText.FgRGB(255, 165, 0).EmitLn;

  aText := 'Custom RGB Background (Purple: 128, 0, 128)';
  aText.BgRGB(128, 0, 128).Fg(TAnsi.White).EmitLn;

  Writeln('Grayscale Ramp (Bg):');
  for i := 0 to 23 do
  begin
    aText := ' ';
    aText.Bg(aText.GrayScale(i)).Emit;
  end;
  Writeln;
  
  Writeln('RGB Helper Demo (Red Gradient Bg):');
  for i := 0 to 5 do
  begin
    aText := '  ';
    aText.Bg(aText.RGB(i, 0, 0)).Emit;
  end;
  Writeln;

  Pause;

  // --- Cursor Movement Demo ---
  Writeln('--- Cursor Movement Demo ---');
  Writeln('Line 1: Origin');
  Writeln('Line 2: Target for Up/Down');
  Writeln('Line 3: Target for PreviousLine');
  Writeln;
  
  // Go back up to Line 1
  aText := ' <--- Appended on Line 1 via PreviousLine';
  aText.PreviousLine(4).Forward(15).Emit; 
  
  // Go down to Line 2
  aText := ' <--- Appended on Line 2 via NextLine';
  aText.NextLine.Forward(15).Emit;

  // Move absolute
  aText := 'Absolute Position (Row 10, Col 20)';
  aText.At(20, 10).Emit;

  // Column movement
  Writeln;
  Writeln; // Ensure we are below
  Write('Column 1');
  aText := 'Column 30 via AtCol';
  aText.AtCol(30).EmitLn;
  
  // Directional
  Writeln;
  Write('Start');
  aText := 'Up and Right';
  // Move Up 1 and Right 5 from current position
  aText.Up(1).Forward(5).EmitLn;
  Writeln;

  Pause;

  // --- Erase Demo ---
  Writeln('--- Erase Demo ---');
  Writeln('1. This line will be partially erased from the END (Watch this part -> XXXXX)');
  Writeln('2. This line will be partially erased from the BEGINNING');
  Writeln('3. This line will be FULLY erased');
  Writeln('4. This line stays.');
  
  // 1. Erase End
  // Move up 4 lines (to line 1), move to column 55 approx
  Write(TAnsi.CursorPreviousLine(4)); 
  Write(TAnsi.CursorAtCol(55)); 
  Write(TAnsi.EraseLine(elEndOfLine));
  
  // 2. Erase Start
  Write(TAnsi.CursorNextLine(1));
  Write(TAnsi.CursorAtCol(10)); // Move in a bit
  Write(TAnsi.EraseLine(edBeginOfLine));

  // 3. Erase Full
  Write(TAnsi.CursorNextLine(1));
  Write(TAnsi.EraseLine(edLine));

  // Return to bottom
  Write(TAnsi.CursorNextLine(2));
  
  Pause;

  Writeln('Demo Complete.');
end.
