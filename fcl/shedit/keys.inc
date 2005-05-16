{
    $Id: keys.inc,v 1.4 2005/02/14 17:13:17 peter Exp $

    "SHEdit" - Text editor with syntax highlighting
    Copyright (C) 1999-2000 by Sebastian Guenther (sg@freepascal.org)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


// TSHTextEdit: Implementation of keyboard handling methods


function TSHTextEdit.AddKeyboardAction(AMethod: TKeyboardActionProc;ASelectionAction:TSelectionAction;ADescr: String): TKeyboardActionDescr;
begin
  Result := TKeyboardActionDescr(KeyboardActions.Add);
  Result.Descr := ADescr;
  Result.Method := AMethod;
  Result.SelectionAction := ASelectionAction;
end;


function TSHTextEdit.AddKeyboardAssignment(AKeyCode: Integer; AShiftState: TShiftState; AAction: TKeyboardActionDescr): TShortcut;
begin
  Result := TShortcut(Shortcuts.Add);
  Result.KeyCode := AKeyCode;
  Result.ShiftState := AShiftState;
  Result.Action := AAction;
end;


procedure TSHTextEdit.AddKeyDef(AMethod: TKeyboardActionProc; ASelectionAction:TSelectionAction; ADescr: String; AKeyCode: Integer; AShiftState: TShiftState);
begin
  AddKeyboardAssignment(AKeyCode, AShiftState,AddKeyboardAction(AMethod, ASelectionAction, ADescr));
end;


procedure TSHTextEdit.ToggleOverwriteMode;
begin
  OverwriteMode := not OverwriteMode;  // *** specify signal for change
end;


procedure TSHTextEdit.AdjustCursorToRange;
begin
  if FCursorY < FWidget.VertPos then begin
    HideCursor;
    FCursorY := FWidget.VertPos;
    ShowCursor;
  end else if FCursorY > FWidget.VertPos + FWidget.PageHeight then begin
    HideCursor;
    FCursorY := FWidget.VertPos + FWidget.PageHeight - 1;
    ShowCursor;
  end;

  if FCursorX < FWidget.HorzPos then begin
    HideCursor;
    FCursorX := FWidget.HorzPos;
    ShowCursor;
  end else if FCursorX > FWidget.HorzPos + FWidget.PageWidth then begin
    HideCursor;
    FCursorX := FWidget.HorzPos + FWidget.PageWidth - 1;
    ShowCursor;
  end;
end;


procedure TSHTextEdit.AdjustRangeToCursor;
var
  py : integer;
begin
  if FCursorY < FWidget.VertPos then
    FWidget.VertPos := FCursorY
  else if FCursorY >= FWidget.VertPos + FWidget.PageHeight then begin
    py := FCursorY - FWidget.PageHeight + 1;
    if py < 0 then
      FWidget.VertPos:=0
    else
      FWidget.VertPos:=py;
  end;
  if FCursorX < FWidget.HorzPos then
    FWidget.HorzPos := FCursorX
  else if FCursorX >= FWidget.HorzPos + FWidget.PageWidth then begin
    py := FCursorX - FWidget.PageWidth + 1;
    if py < 0 then
      FWidget.HorzPos := 0
    else
      FWidget.HorzPos := py;
  end;
end;


procedure TSHTextEdit.CursorUp;
begin
  if FCursorY > 0 then
    Dec(FCursorY);
end;


procedure TSHTextEdit.CursorDown;
begin
  if FCursorY < FDoc.LineCount - 1 then
    Inc(FCursorY);
end;


procedure TSHTextEdit.CursorLeft;
begin
  Dec(FCursorX);
  if FCursorX < 0 then
    if FCursorY>0 then begin
      Dec(FCursorY);
      FCursorX := FDoc.LineLen[FCursorY];
    end else
      FCursorX := 0;
end;


procedure TSHTextEdit.CursorRight;
begin
  Inc(FCursorX);
end;


procedure TSHTextEdit.CursorDocBegin;
begin
  FCursorX := 0;
  FCursorY := 0;
end;


procedure TSHTextEdit.CursorDocEnd;
begin
  FCursorY := FDoc.LineCount-1;
  FCursorX := FDoc.LineLen[FCursorY];
end;


procedure TSHTextEdit.CursorHome;
begin
  FCursorX := 0;
  AdjustRangeToCursor;
end;


procedure TSHTextEdit.CursorEnd;
begin
  FCursorX := FDoc.LineLen[FCursorY];
  AdjustRangeToCursor;
end;


procedure TSHTextEdit.CursorPageUp;
begin
  Dec(FCursorY, FWidget.PageHeight);
  if FCursorY < 0 then
    FCursorY := 0;
end;


procedure TSHTextEdit.CursorPageDown;
begin
  Inc(FCursorY, FWidget.PageHeight);
  if FCursorY > FDoc.LineCount - 1 then
    FCursorY := FDoc.LineCount - 1;
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
    cbtext := Copy(FDoc.LineText[FSel.OStartY],
      FSel.OStartX + 1, FSel.OEndX - FSel.OStartX)
  else begin
    cbtext := Copy(FDoc.LineText[FSel.OStartY],
      FSel.OStartX + 1, FDoc.LineLen[FSel.OStartY]) + #10;
    for y := FSel.OStartY + 1 to FSel.OEndY - 1 do
      cbtext := cbtext + FDoc.LineText[y] + #10;
    cbtext := cbtext + Copy(FDoc.LineText[FSel.OEndY], 1, FSel.OEndX);
  end;

  FWidget.SetClipboard(cbtext);
end;

procedure TSHTextEdit.ClipboardPaste;
var
  cbtext: String;
begin
  cbtext := FWidget.GetClipboard;
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
    #32..#255:
      begin
        s := FDoc.LineText[FCursorY];
        if FCursorX>=Length(s) then
          s := s + Space(FCursorX-length(s)) + Key
        else
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
            if FCursorX>=Length(s) then
             s := s + Space(FCursorX-length(s)) + Key
            else
             s := Copy(s, 1, FCursorX) + Key + Copy(s, FCursorX + 1 + Ord(OverwriteMode), Length(s));
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

function TSHTextEdit.KeyPressed(KeyCode: LongWord; ShiftState: TShiftState): Boolean;
var
  i: Integer;
  shortcut: TShortcut;
  ShortcutFound: Boolean;
begin
  //  WriteLn('TSHTextEdit: Key pressed: ', KeyCode);
  LastCursorX := FCursorX;
  LastCursorY := FCursorY;
  StartSelectionChange;

  // Check for keyboard shortcuts
  ShortcutFound := False;
  for i := 0 to Shortcuts.Count - 1 do begin
    shortcut := TShortcut(Shortcuts.Items[i]);
    if (KeyCode = shortcut.KeyCode) and
       (ShiftState * [ssShift, ssCtrl, ssAlt] = shortcut.ShiftState) then begin
      ShortcutFound := True;
      break;
    end;
  end;

  Result := True;

  if ShortcutFound then begin
    // WriteLn(shortcut.Action.Descr);
    shortcut.Action.Method;     // Execute associated action
    // Handle the selection extending
    case shortcut.Action.SelectionAction of
      selNothing: ;
      selExtend: begin
          if not FSel.IsValid then begin
            FSel.StartX:=LastCursorX;
            FSel.StartY:=LastCursorY;
          end;
          FSel.EndX:=FCursorX;
          FSel.EndY:=FCursorY;
        end;
      selClear:
        FSel.Clear;
    end;
  end else
    if (KeyCode <= 255) and (ShiftState * [ssCtrl, ssAlt] = []) then
      ExecKey(Chr(KeyCode), False)
    else
      Result := False;          // Key has not been processed

  EndSelectionChange;
  AdjustRangeToCursor;
end;


{
  $Log: keys.inc,v $
  Revision 1.4  2005/02/14 17:13:17  peter
    * truncate log

}
