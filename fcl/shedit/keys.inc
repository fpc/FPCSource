{
  $Id$

  "shedit" - Text editor with syntax highlighting
  Copyright (C) 1999  Sebastian Guenther (sguenther@gmx.de)

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}


// TSHTextEdit: Implementation of keyboard handling methods


function TSHTextEdit.AddKeyboardAction(AMethod: TKeyboardActionProc;
  ADescr: String): TKeyboardActionDescr;
begin
  Result := TKeyboardActionDescr(KeyboardActions.Add);
  Result.Descr := ADescr;
  Result.Method := AMethod;
end;

function TSHTextEdit.AddKeyboardAssignment(AKeyCode: Integer;
  AShiftState: TShiftState; AAction: TKeyboardActionDescr): TShortcut;
begin
  Result := TShortcut(Shortcuts.Add);
  Result.KeyCode := AKeyCode;
  Result.ShiftState := AShiftState;
  Result.Action := AAction;
end;

procedure TSHTextEdit.AddKeyDef(AMethod: TKeyboardActionProc; ADescr: String;
  AKeyCode: Integer; AShiftState: TShiftState);
begin
  AddKeyboardAssignment(AKeyCode, AShiftState,
    AddKeyboardAction(AMethod, ADescr));
end;

procedure TSHTextEdit.ToggleOverwriteMode;
begin
  OverwriteMode := not OverwriteMode;  // *** specify signal for change
end;

procedure TSHTextEdit.CursorUp;
begin
  if FCursorY = 0 then
    FCursorX := 0
  else
    Dec(FCursorY);
  if FCursorY<Renderer.VertPos then
   Renderer.VertPos := Renderer.VertPos - 1;
end;


procedure TSHTextEdit.CursorDown;
begin
  if FCursorY < FDoc.LineCount - 1 then
   Inc(FCursorY)
  else
   FCursorX := FDoc.LineLen[FCursorY];
  if FCursorY>Renderer.VertPos+Renderer.PageHeight then
   Renderer.VertPos := Renderer.VertPos + 1;
end;


procedure TSHTextEdit.CursorLeft;
begin
  if FCursorX > 0 then
    Dec(FCursorX)
  else
    if FCursorY > 0 then
      begin
        Dec(FCursorY);
        FCursorX := FDoc.LineLen[FCursorY];
        if FCursorY<Renderer.VertPos then
         Renderer.VertPos := Renderer.VertPos - 1;
      end;
end;


procedure TSHTextEdit.CursorRight;
begin
  Inc(FCursorX);
  if FCursorX > FDoc.LineLen[FCursorY] then
    if FCursorY < FDoc.LineCount - 1 then
     begin
       Inc(FCursorY);
       FCursorX := 0;
       if FCursorY>Renderer.VertPos+Renderer.PageHeight then
        Renderer.VertPos := Renderer.VertPos + 1;
     end
    else
     FCursorX := FDoc.LineLen[FCursorY];
end;


procedure TSHTextEdit.CursorHome;
begin
  FCursorX := 0;
end;


procedure TSHTextEdit.CursorEnd;
begin
  FCursorX := FDoc.LineLen[FCursorY];
end;


procedure TSHTextEdit.CursorPageUp;
begin
  if FCursorY = 0 then
    FCursorX := 0
  else
    begin
      Dec(FCursorY, Renderer.PageHeight);
      if FCursorY < 0 then
       begin
         FCursorY := 0;
         Renderer.VertPos := 0;
       end
      else
       Renderer.VertPos := Renderer.VertPos - Renderer.PageHeight;
    end;
end;


procedure TSHTextEdit.CursorPageDown;
begin
  if FCursorY = FDoc.LineCount - 1 then
    FCursorX := FDoc.LineLen[FCursorY]
  else
    begin
      Inc(FCursorY, Renderer.PageHeight);
      if FCursorY >= FDoc.LineCount then
       begin
         FCursorY := FDoc.LineCount - Renderer.PageHeight;
         if FCursorY < 0 then
          FCursorY:=0;
         Renderer.VertPos := FCursorY;
       end
      else
       Renderer.VertPos := Renderer.VertPos + Renderer.PageHeight;
    end;
end;


procedure TSHTextEdit.EditDelLeft;
var
  s: String;
begin
  if FCursorX > 0 then begin
    s := FDoc.LineText[FCursorY];
    Dec(FCursorX);
    AddUndoInfo(TUndoDelLeft.Create(s[FCursorX + 1]), True);
    s := Copy(s, 1, FCursorX) + Copy(s, FCursorX + 2, Length(s));
    FDoc.LineText[FCursorY] := s;
    ChangeInLine(FCursorY);
  end else if FCursorY > 0 then begin
    FCursorX := FDoc.LineLen[FCursorY - 1];
    FDoc.LineText[FCursorY - 1] := FDoc.LineText[FCursorY - 1] +
      FDoc.LineText[FCursorY];
    Dec(FCursorY);
    FDoc.RemoveLine(FCursorY + 1);
    AddUndoInfo(TUndoDelLeft.Create(#13), True);
  end;
end;

procedure TSHTextEdit.EditDelRight;
var
  s: String;
begin
  if FCursorX < FDoc.LineLen[FCursorY] then begin
    s := FDoc.LineText[FCursorY];
    AddUndoInfo(TUndoDelRight.Create(s[FCursorX + 1]), True);
    s := Copy(s, 1, FCursorX) + Copy(s, FCursorX + 2, Length(s));
    FDoc.LineText[FCursorY] := s;
    ChangeInLine(FCursorY);
  end else if FCursorY < FDoc.LineCount - 1 then begin
    FDoc.LineText[FCursorY] := FDoc.LineText[FCursorY] +
      FDoc.LineText[FCursorY + 1];
    FDoc.RemoveLine(FCursorY + 1);
    AddUndoInfo(TUndoDelRight.Create(#13), True);
  end;
end;

procedure TSHTextEdit.EditDelLine;
var
  DeletedText: String;
begin
  DeletedText := FDoc.LineText[FCursorY];
  if FDoc.LineCount = 1 then
    FDoc.LineText[FCursorY] := ''
  else
    FDoc.RemoveLine(FCursorY);

  if FCursorY >= FDoc.LineCount then
    FCursorY := FDoc.LineCount - 1;
  FCursorX := 0;

  AddUndoInfo(TUndoDelRight.Create(DeletedText + #13), True);

  ChangeInLine(FCursorY);
end;

procedure TSHTextEdit.EditUndo;
var
  info: TUndoInfo;
begin
  if LastUndoInfo = nil then exit;

  info := LastUndoInfo;
  LastUndoInfo := LastRedoInfo;
  info.DoUndo(Self);
  LastRedoInfo := LastUndoInfo;
  LastUndoInfo := info;

  // Free undo info
  if info.Prev <> nil then
    info.Prev.Next := info.Next
  else
    FDoc.Modified := False;
  LastUndoInfo := info.Prev;
  info.Free;
end;

procedure TSHTextEdit.EditRedo;
var
  info: TUndoInfo;
begin
  if LastRedoInfo = nil then exit;

  info := LastRedoInfo;
  info.DoUndo(Self);

  // Free redo info
  if info.Prev <> nil then
    info.Prev.Next := info.Next;
  LastRedoInfo := info.Prev;
  info.Free;
end;

procedure TSHTextEdit.ClipboardCut;
begin
  WriteLn('ClipboardCut: Not implemented yet');
  ClipboardCopy;
end;

procedure TSHTextEdit.ClipboardCopy;
var
  cbtext: String;
  y: Integer;
begin
  if FSel.OStartY = FSel.OEndY then
    cbtext := Copy(FDoc.LineText[FSel.OStartY], FSel.OStartX + 1, FSel.OEndX - FSel.OStartX)
  else begin
    cbtext := Copy(FDoc.LineText[FSel.OStartY], FSel.OStartX + 1,
      FDoc.LineLen[FSel.OStartY]) + #10;
    for y := FSel.OStartY + 1 to FSel.OEndY - 1 do
      cbtext := cbtext + FDoc.LineText[y] + #10;
    cbtext := cbtext + Copy(FDoc.LineText[FSel.OEndY], 1, FSel.OEndX);
  end;

  Renderer.SetClipboard(cbtext);
end;

procedure TSHTextEdit.ClipboardPaste;
var
  cbtext: String;
begin
  cbtext := Renderer.GetClipboard;

  ExecKeys(cbtext, True);
end;

procedure TSHTextEdit.KeyReturn; begin end;

function TSHTextEdit.ExecKey(Key: Char; BlockMode: Boolean): Boolean;
var
  s, s2: String;
  i: Integer;
begin
  Result := True;
  case Key of
    #9: begin
        s := FDoc.LineText[FCursorY];
        s2 := ' ';
        i := 1;
        while ((FCursorX + i) mod 4) <> 0 do begin
          s2 := s2 + ' ';
          Inc(i);
        end;
        s := Copy(s, 1, FCursorX) + s2 + Copy(s, FCursorX + 1, Length(s));
        FDoc.LineText[FCursorY] := s;
        Inc(FCursorX, i);
        AddUndoInfo(TUndoEdit.Create(i), True);
        ChangeInLine(FCursorY);
      end;
    #13: begin
        s := FDoc.LineText[FCursorY];
        FDoc.LineText[FCursorY] := Copy(s, 1, FCursorX);
        FDoc.InsertLine(FCursorY + 1, Copy(s, FCursorX + 1, Length(s)));
        CursorX := 0;
        Inc(FCursorY);
        AddUndoInfo(TUndoEdit.Create, True);
        if not BlockMode then KeyReturn;
      end;
    #32..#255: begin
        s := FDoc.LineText[FCursorY];
        if OverwriteMode then
          s := Copy(s, 1, FCursorX) + Key + Copy(s, FCursorX + 2, Length(s))
        else
          s := Copy(s, 1, FCursorX) + Key + Copy(s, FCursorX + 1, Length(s));
        FDoc.LineText[FCursorY] := s;
        Inc(FCursorX);
        AddUndoInfo(TUndoEdit.Create, True);
        ChangeInLine(FCursorY);
      end;
    else Result := False;
  end;
end;

procedure TSHTextEdit.ExecKeys(Keys: String; BlockMode: Boolean);
var
  s, s2: String;
  KeysPos, i: Integer;
  Key: Char;
begin
  if BlockMode then
    AddUndoInfo(TUndoEdit.Create(0), False);  // Initialize new undo block

  KeysPos := 1;
  while KeysPos <= Length(Keys) do begin
    case Keys[KeysPos] of
      #9: begin
          s := FDoc.LineText[FCursorY];
          s2 := ' ';
          i := 1;
          while ((FCursorX + i) mod 4) <> 0 do begin
            s2 := s2 + ' ';
            Inc(i);
          end;
          s := Copy(s, 1, FCursorX) + s2 + Copy(s, FCursorX + 1, Length(s));
          FDoc.LineText[FCursorY] := s;
          Inc(FCursorX, i);
          AddUndoInfo(TUndoEdit.Create(i), True);
          ChangeInLine(FCursorY);
          Inc(KeysPos);
        end;
      #13, #10: begin
          s := FDoc.LineText[FCursorY];
          FDoc.LineText[FCursorY] := Copy(s, 1, FCursorX);
          FDoc.InsertLine(FCursorY + 1, Copy(s, FCursorX + 1, Length(s)));
          CursorX := 0;
          Inc(FCursorY);
          AddUndoInfo(TUndoEdit.Create, True);
          if not BlockMode then KeyReturn;
          Inc(KeysPos);
        end;
      #32..#255: begin
          i := 0;
          while (KeysPos <= Length(Keys)) and (Keys[KeysPos] >= #32) do begin
            Key := Keys[KeysPos];
            s := FDoc.LineText[FCursorY];
            s := Copy(s, 1, FCursorX) + Key +
              Copy(s, FCursorX + 1 + Ord(OverwriteMode), Length(s));
            FDoc.LineText[FCursorY] := s;
            Inc(FCursorX);
            Inc(i);
            Inc(KeysPos);
          end;
          AddUndoInfo(TUndoEdit.Create(i), True);

          ChangeInLine(FCursorY);
        end;
      else Inc(KeysPos);
    end;
  end;
end;

procedure TSHTextEdit.MultiDelLeft(count: Integer);
var
  s: String;
begin
  while count > 0 do begin
    if FCursorX > 0 then begin
      while (FCursorX > 0) and (count > 0) do begin
        s := FDoc.LineText[FCursorY];
        Dec(FCursorX);
        AddUndoInfo(TUndoDelLeft.Create(s[FCursorX + 1]), True);
        s := Copy(s, 1, FCursorX) + Copy(s, FCursorX + 2, Length(s));
        FDoc.LineText[FCursorY] := s;
        Dec(count);
      end;
      ChangeInLine(FCursorY);
    end else if FCursorY > 0 then begin
      FCursorX := FDoc.LineLen[FCursorY - 1];
      FDoc.LineText[FCursorY - 1] := FDoc.LineText[FCursorY - 1] +
        FDoc.LineText[FCursorY];
      Dec(FCursorY);
      FDoc.RemoveLine(FCursorY + 1);
      AddUndoInfo(TUndoDelLeft.Create(#13), True);
      Dec(count);
    end else break;
  end;
end;

procedure TSHTextEdit.KeyPressed(KeyCode: LongWord; ShiftState: TShiftState);
var
  RemoveSel: Boolean;

  function CheckEditingKeys: Boolean;

    procedure CheckSelKeys;
    begin
      if ssShift in ShiftState then begin
        RemoveSel := False;
        if not FSel.IsValid then begin
          FSel.StartX := LastCursorX;
          FSel.StartY := LastCursorY;
        end;
        FSel.EndX := FCursorX;
        FSel.EndY := FCursorY;
      end;
    end;

  begin
    if ShiftState * [ssCtrl, ssAlt] = [] then
      Result := ExecKey(Chr(KeyCode), False)
    else
      Result := False;
  end;

  procedure RedrawArea(x1, y1, x2, y2: Integer);
  var
    r: TRect;
  begin
    // WriteLn('Redraw: ', x1, '/', y1, ' - ', x2, '/', y2);
    {###if y1 = y2 then begin
      r.Left := FLeftIndent + x1 * CharW;
      r.Right := FLeftIndent + x2 * CharW;
      r.Top := y1 * CharH;
      r.Bottom := r.Top + CharH;
      PaintBox.Redraw(r);
    end else begin
      r.Left := FLeftIndent + x1 * CharW;
      r.Right := PaintBox.Width;
      r.Top := y1 * CharH;
      r.Bottom := r.Top + CharH;
      PaintBox.Redraw(r);

      if y1 < y2 - 1 then begin
        r.Left := FLeftIndent;
        r.Top := (y1 + 1) * CharH;
        r.Bottom := y2 * CharH;
        PaintBox.Redraw(r);
      end else
        r.Left := FLeftIndent;

      r.Right := FLeftIndent + x2 * CharW;
      r.Top := y2 * CharH;
      r.Bottom := r.Top + CharH;
      PaintBox.Redraw(r);
    end;}
  end;

var
  i: Integer;
  shortcut: TShortcut;
  AssignmentMatched, OldSelValid: Boolean;
  OldSelStartX, OldSelStartY, OldSelEndX, OldSelEndY: Integer;
begin
//  WriteLn('Text Widget: Key pressed: "', Key, '" ', KeyCode);
  HideCursor;

  LastCursorX := FCursorX;
  LastCursorY := FCursorY;
  OldSelValid := FSel.IsValid;
  if OldSelValid then begin
    OldSelStartX := FSel.OStartX;
    OldSelStartY := FSel.OStartY;
    OldSelEndX := FSel.OEndX;
    OldSelEndY := FSel.OEndY;
  end;

  RemoveSel := True;

  // Check for keyboard shortcuts
  AssignmentMatched := False;
  for i := 0 to Shortcuts.Count - 1 do begin
    shortcut := TShortcut(Shortcuts.Items[i]);
    if (KeyCode = shortcut.KeyCode) and
      (ShiftState * [ssShift, ssCtrl, ssAlt] = shortcut.ShiftState) then begin
      shortcut.Action.Method;
      AssignmentMatched := True;
      break;
    end;
  end;

  if not AssignmentMatched then
    if not CheckEditingKeys then RemoveSel := False;

  // Check selection
  if FSel.IsValid then begin
    if (FSel.StartX = FSel.EndX) and (FSel.StartY = FSel.EndY) then
      FSel.Clear
  end;

//if RemoveSel then FSel.Clear;
//PaintBox.Redraw;

  {Write('Sel = ', FSel.StartX, '/', FSel.StartY, ' - ', FSel.EndX, '/', FSel.EndY);
  if OldSelValid then WriteLn('  Old = ', OldSelStartX, '/', OldSelStartY, ' - ', OldSelEndX, '/', OldSelEndY)
  else WriteLn;}


  if RemoveSel then FSel.Clear;

  if not OldSelValid then begin
    if FSel.IsValid then
      RedrawArea(FSel.OStartX, FSel.OStartY, FSel.OEndX, FSel.OEndY);
  end else begin
    if not FSel.IsValid then
      RedrawArea(OldSelStartX, OldSelStartY, OldSelEndX, OldSelEndY)
    else begin
      // Do OldSel and FSel intersect?
      if (OldSelEndY < FSel.OStartY) or (OldSelStartY > FSel.OEndY) or
         ((OldSelEndY = FSel.OStartY) and (OldSelEndX <= FSel.OStartX)) or
         ((OldSelStartY = FSel.OEndY) and (OldSelStartX >= FSel.OEndX)) then begin
         RedrawArea(OldSelStartX, OldSelStartY, OldSelEndX, OldSelEndY);
         RedrawArea(FSel.OStartX, FSel.OStartY, FSel.OEndX, FSel.OEndY);
      end else begin
        // Intersection => determine smallest possible area(s) to redraw
        // 1. Check if the start position has changed
        if (OldSelStartX <> FSel.OStartX) or (OldSelStartY <> FSel.OStartY) then
          if (OldSelStartY < FSel.OStartY) or ((OldSelStartY = FSel.OStartY) and
             (OldSelStartX < FSel.OStartX)) then
            RedrawArea(OldSelStartX, OldSelStartY, FSel.OStartX, FSel.OStartY)
          else
            RedrawArea(FSel.OStartX, FSel.OStartY, OldSelStartX, OldSelStartY);
        // 2. Check if end position has changed
        if (OldSelEndX <> FSel.OEndX) or (OldSelEndY <> FSel.OEndY) then
          if (OldSelEndY < FSel.OEndY) or ((OldSelEndY = FSel.OEndY) and
             (OldSelEndX < FSel.OEndX)) then
            RedrawArea(OldSelEndX, OldSelEndY, FSel.OEndX, FSel.OEndY)
          else
            RedrawArea(FSel.OEndX, FSel.OEndY, OldSelEndX, OldSelEndY);

        {if OldSelEndY = FSel.OEndY then begin
          if OldSelStartX > FSel.OStartX  then
            RedrawArea(FSel.OStartX, FSel.OEndY, OldSelStartX, FSel.OEndY)
          else if OldSelStartX < FSel.OStartX then
            RedrawArea(OldSelStartX, FSel.OEndY, FSel.OStartX, FSel.OEndY);
          if OldSelEndX < FSel.OEndX then
            RedrawArea(OldSelEndX, FSel.OEndY, FSel.OEndX, FSel.OEndY)
          else if OldSelEndX > FSel.OEndX then
            RedrawArea(FSel.OEndX, FSel.OEndY, OldSelEndX, FSel.OEndY);
        end else begin
          if OldSelStartY > FSel.OStartY then
            RedrawArea(FSel.OStartX, FSel.OStartY, OldSelStartX, OldSelStartY)
          else if OldSelStartY < FSel.OStartY then
            RedrawArea(OldSelStartX, OldSelStartY, FSel.OStartX, FSel.OStartY);
          if OldSelEndY < FSel.OEndY then
            RedrawArea(OldSelEndX, OldSelEndY, FSel.OEndX, FSel.OEndY)
          else if OldSelEndY > FSel.OEndY then
            RedrawArea(FSel.OEndX, FSel.OEndY, OldSelEndX, OldSelEndY);
        end;}
      end;
    end;
  end;
  ShowCursor;
end;


{
  $Log$
  Revision 1.3  1999-12-08 01:03:15  peter
    * changes so redrawing and walking with the cursor finally works
      correct

  Revision 1.2  1999/11/15 21:47:36  peter
    * first working keypress things

  Revision 1.1  1999/10/29 15:59:04  peter
    * inserted in fcl

}
