{

    "SHEdit" - Text editor with syntax highlighting
    Copyright (C) 1999-2000 by Sebastian Guenther (sg@freepascal.org)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


// Drawing code of TSHTextEdit (renderer for syntax highlighting engine);
// also handles cursor drawing


procedure TSHTextEdit.DoHighlighting(var flags: Byte; source, dest: PChar);
begin
  StrCopy(dest, source);
end;

function TSHTextEdit.CalcSHFlags(FlagsIn: Byte; source: String): Byte;
var
  s: PChar;
  flags: Byte;
begin
  GetMem(s, Length(source) * 3 + 4);
  flags := FlagsIn;
  DoHighlighting(flags, PChar(source), s);
  FreeMem(s, Length(source) * 3 + 4);
  Result := flags;
end;

procedure TSHTextEdit.HideCursor;
begin
  Dec(CursorVisible);
  if CursorVisible >= 0 then
    FWidget.HideCursor(CursorX, CursorY);
end;

procedure TSHTextEdit.ShowCursor;
begin
  Inc(CursorVisible);
  if CursorVisible = 1 then
    FWidget.ShowCursor(CursorX, CursorY);
end;


procedure TSHTextEdit.ChangeInLine(line: Integer);
var
  CurLine: Integer;
  OldFlags, NewFlags: Byte;
begin
  // Determine how many lines must be redrawn

  CurLine := line;
  if CurLine = 0 then
    NewFlags := 0
  else
    NewFlags := FDoc.LineFlags[CurLine - 1];

  while CurLine < FDoc.LineCount - 1 do begin
    NewFlags := CalcSHFlags(NewFlags, FDoc.LineText[CurLine]);
    OldFlags := FDoc.LineFlags[CurLine + 1] and not LF_SH_Valid;
    FDoc.LineFlags[CurLine + 1] := NewFlags or LF_SH_Valid;
    if OldFlags = (NewFlags and not LF_SH_Valid) then break;
    Inc(CurLine);
  end;

  // Redraw all lines with changed SH flags
  FWidget.InvalidateRect(FWidget.HorzPos, line, FWidget.PageWidth, (CurLine - line) + 1);
end;


procedure TSHTextEdit.DrawContent(x, y, w, h: Integer);
var
  x2, y2: Integer;

  procedure PostprocessOutput(py: Integer);
  begin
    // Erase free space below the text area
    if py < y2 then
      FWidget.ClearRect(x, py, w, y2 - py);

    if (FCursorX >= x) and (FCursorY >= y) and
      (FCursorX <= x2) and (FCursorY <= y2) then
      ShowCursor;
  end;

  // If Lenght(s) < x, add as many spaces to s so that x will be at
  // the end of s.
  procedure ProvideSpace(var s: String; x: Integer);
  begin
    while Length(s) < x do
      s := s + ' ';
  end;

var
  i, cx, LineNumber, CheckLine: Integer;
  OrigStr, s, s2: PChar;
  spos, smem: Integer;
  flags: Byte;
  InSel: Boolean;
  LastCol: Char;
  LineWithSpace: String;        // used for virtual whitespace expanding

begin

  if x < 0 then begin
    Inc(w, x);
    x := 0;
  end;
  if y < 0 then begin
    Inc(h, y);
    y := 0;
  end;
  if (w < 0) or (h < 0) then exit;

  x2 := x + w;
  y2 := y + h;

  if (FCursorX >= x) and (FCursorY >= y) and
    (FCursorX <= x2) and (FCursorY <= y2) then
    //HideCursor;
    Dec(CursorVisible);

  if (FDoc = nil) or (FDoc.LineCount <= y) then begin
    PostprocessOutput(y);
    exit;
  end;

  LineNumber := y;

  // Check if syntax highlighting flags are valid:
  if (FDoc.LineFlags[LineNumber] and LF_SH_Valid) <> 0 then
    flags := FDoc.LineFlags[LineNumber] and not LF_SH_Valid
  else begin
    // search for last valid line before the first line to be drawn
    CheckLine := LineNumber;
    while (CheckLine >= 0) and
      ((FDoc.LineFlags[CheckLine] and LF_SH_Valid) = 0) do Dec(CheckLine);
    if CheckLine >= 0 then begin
      flags := FDoc.LineFlags[CheckLine] and not LF_SH_Valid;
      // Recalc SH flags for all lines between last valid and first visible line
      while (CheckLine < LineNumber) do begin
        flags := CalcSHFlags(flags, FDoc.LineText[CheckLine]);
        FDoc.LineFlags[CheckLine] := flags or LF_SH_Valid;
        Inc(CheckLine);
      end;
    end else
      flags := 0;
  end;


  // if FSel.IsValid then writeln('Selection: ',FSel.OStartX,',',FSel.OStartY,'-',FSel.OEndX,',',FSel.OEndY);

  while (LineNumber < FDoc.LineCount) and (LineNumber <= y2) do begin
    i := 0;

    // Do virtual whitespace expanding
    LineWithSpace := FDoc.LineText[LineNumber];
    if LineNumber = FSel.OStartY then
      ProvideSpace(LineWithSpace, FSel.OStartX);
    if LineNumber = FSel.OEndY then
      ProvideSpace(LineWithSpace, FSel.OEndX);
    if LineNumber = FCursorY then
      ProvideSpace(LineWithSpace, FCursorX);

    // Call syntax highlighter for this line

    smem := Length(LineWithSpace) * 3 + 8;
    GetMem(s, smem);
    FDoc.LineFlags[LineNumber] := flags or LF_SH_Valid;
    OrigStr := PChar(LineWithSpace);
    DoHighlighting(flags, OrigStr, s);

    // Handle current selection
    if FSel.IsValid then
      if (LineNumber > FSel.OStartY) and (LineNumber < FSel.OEndY) then begin
        ASSERT(smem > StrLen(OrigStr) + 2);
        s[0] := LF_Escape;
        s[1] := Chr(shSelected);
        StrCopy(@s[2], OrigStr);
      end else if OrigStr[0] = #0 then begin
        if LineNumber = FSel.OStartY then begin
          ASSERT(smem >= 3);
          s[0] := LF_Escape;
          s[1] := Chr(shSelected);
          s[2] := #0;
        end;
      end else if ((LineNumber = FSel.OStartY) or (LineNumber = FSel.OEndY))
        and not FSel.IsEmpty then begin
        s2 := StrNew(s);
        spos := 0;
        i := 0;
        cx := 0;
        if LineNumber > FSel.OStartY then begin
          ASSERT(smem >= 2);
          s[0] := LF_Escape;
          s[1] := Chr(shSelected);
          InSel := True;
          spos := 2;
        end else
          InSel := False;
        LastCol := Chr(shDefault);
        while True do begin
          ASSERT(i <= StrLen(s2));
          if s2[i] = LF_Escape then begin
            LastCol := s2[i + 1];
            if not InSel then begin
              ASSERT(smem > spos + 1);
              s[spos] := LF_Escape;
              s[spos + 1] := LastCol;
              Inc(spos, 2);
            end;
            Inc(i, 2);
          end else begin
            if InSel then begin
              if (LineNumber = FSel.OEndY) and (cx = FSel.OEndX) then begin
                ASSERT(smem > spos + 1);
                s[spos] := LF_Escape;
                s[spos + 1] := LastCol;
                Inc(spos, 2);
                InSel := False;
              end;
            end else
              if (LineNumber = FSel.OStartY) and (cx = FSel.OStartX) then begin
                ASSERT(smem > spos + 1);
                s[spos] := LF_Escape;
                s[spos + 1] := Chr(shSelected);
                Inc(spos, 2);
                InSel := True;
              end;
            if s2[i] = #0 then break;    // only exit of 'while' loop!
            ASSERT(smem > spos);
            s[spos] := s2[i];
            Inc(spos);
            Inc(i);
            Inc(cx);
          end;
        end;
        ASSERT(smem > spos);
        s[spos] := #0;
        StrDispose(s2);
      end;

    FWidget.DrawTextLine(x, x2, LineNumber, s);

    FreeMem(s);
    Inc(LineNumber);
  end;

  PostprocessOutput(LineNumber);
end;


