{

    "SHEdit" - Text editor with syntax highlighting
    Copyright (C) 1999-2000 by Sebastian Guenther (sg@freepascal.org)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


// TSHTextEdit: Undo/Redo support


function TUndoInfo.Merge(AEdit: TSHTextEdit; AInfo: TUndoInfo): Boolean;
begin
  Result := False;
end;


constructor TUndoEdit.Create;
begin
  inherited Create;
  NumOfChars := 1;
end;

constructor TUndoEdit.Create(ANumOfChars: Integer);
begin
  inherited Create;
  NumOfChars := ANumOfChars;
end;

function TUndoEdit.Merge(AEdit: TSHTextEdit; AInfo: TUndoInfo): Boolean;
begin
//  if (CursorX <> AEdit.CursorX - TUndoEdit(AInfo).NumOfChars) or
//     (CursorY <> AEdit.CursorY) then exit(False);
  Inc(NumOfChars, TUndoEdit(AInfo).NumOfChars);
  if AEdit.CursorY = CursorY + 1 then begin
    CursorX := 0;
    Inc(CursorY);
  end else
    Inc(CursorX, TUndoEdit(AInfo).NumOfChars);
  Result := True;
end;

procedure TUndoEdit.DoUndo(AEdit: TSHTextEdit);
begin
  AEdit.FCursorX := CursorX;
  AEdit.FCursorY := CursorY;
  AEdit.MultiDelLeft(NumOfChars);
end;


constructor TUndoDelLeft.Create(const ADeletedString: String);
begin
  inherited Create;
  DeletedString := ADeletedString;
end;

function TUndoDelLeft.Merge(AEdit: TSHTextEdit; AInfo: TUndoInfo): Boolean;
var
  l: Integer;
begin
  if TUndoDelLeft(AInfo).
    DeletedString[Length(TUndoDelLeft(AInfo).DeletedString)] = #13 then
    exit(False);

  l := Length(TUndoDelLeft(AInfo).DeletedString);
  if CursorY <> AEdit.CursorY then exit(False);
  if CursorX = AEdit.CursorX + l then begin
    DeletedString := TUndoDelLeft(AInfo).DeletedString + DeletedString;
    Dec(CursorX, l);
  end else if CursorX = AEdit.CursorX then
    DeletedString := DeletedString + TUndoDelLeft(AInfo).DeletedString
  else
    exit(False);
  Result := True;
end;

procedure TUndoDelLeft.DoUndo(AEdit: TSHTextEdit);
begin
  AEdit.FCursorX := CursorX;
  AEdit.FCursorY := CursorY;
  AEdit.ExecKeys(DeletedString, False);
end;


procedure TUndoDelRight.DoUndo(AEdit: TSHTextEdit);
var
  OldX, OldY: Integer;
begin
  OldX := CursorX;
  OldY := CursorY;
  AEdit.FCursorX := CursorX;
  AEdit.FCursorY := CursorY;
  AEdit.ExecKeys(DeletedString, False);
  AEdit.FCursorX := OldX;
  AEdit.FCursorY := OldY;
end;



procedure TSHTextEdit.AddUndoInfo(AInfo: TUndoInfo; CanMerge: Boolean);
var
  ok: Boolean;
  info: TUndoInfo;
begin
  ok := False;
  info := LastUndoInfo;
  if CanMerge and Assigned(info) and (info.ClassType = AInfo.ClassType) then begin
    if info.Merge(Self, AInfo) then begin
      AInfo.Free;
      AInfo := info;
      ok := True;
    end;
  end;

  if not ok then begin
    if LastUndoInfo = nil then
      LastUndoInfo := AInfo
    else begin
      AInfo.Prev := LastUndoInfo;
      LastUndoInfo.Next := AInfo;
      LastUndoInfo := AInfo;
    end;

    AInfo.CursorX := FCursorX;
    AInfo.CursorY := FCursorY;
  end;
end;


{
  $Log: undo.inc,v $
  Revision 1.4  2005/02/14 17:13:17  peter
    * truncate log

}
