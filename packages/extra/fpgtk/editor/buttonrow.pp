{$mode objfpc}{$h+}
unit ButtonRow;

interface

uses classes, glib, gtk, gdk, FPgtk;

type

  TRefreshProc = procedure (Selected:TCollectionItem; NeedFocus:boolean) of object;
  TCalcIconFunc = procedure (Item:TCollectionItem; var Pixmap:PGdkPixMap; var mask:PGdkBitmap) of object;

  TButtonRow = class (TFPGtkToolbar)
  private
    FMFirst, FMPrev, FMNext, FMLast,
    FMCopy, FMAdd, FMDelete, FMUp, FMDown : TFPGtkMenuItem;
    FCopy, FAdd, FDelete, FUp, FDown : TFPGtkWidget;
    ICopy, IUp, IDown, IDelete, IAdd : TFPgtkPixmap;
    FCollection : TCollection;
    FList : TFPgtkCList;
    FRefreshProc : TRefreshProc;
    FCalcIconFunc : TCalcIconFunc;
    FSelectIndex : integer;
    FNeedFocus : boolean;
    FTitle : string;
    AccelGroup : PGtkAccelGroup;
    procedure SetTitle (Value : string);
    procedure CreatePixmaps;
    procedure NewSelection (Sender : TFPgtkObject; row,column:integer;
                            event:PGdkEventButton; data : pointer);
    procedure ClickedAdd (Sender : TFPgtkObject; data : pointer);
    procedure ClickedCopy (Sender : TFPgtkObject; data : pointer);
    procedure ClickedDelete (Sender : TFPgtkObject; data : pointer);
    procedure ClickedUp (Sender : TFPgtkObject; data : pointer);
    procedure ClickedDown (Sender : TFPgtkObject; data : pointer);
    procedure ClickedFirst (Sender : TFPgtkObject; data : pointer);
    procedure ClickedPrevious (Sender : TFPgtkObject; data : pointer);
    procedure ClickedNext (Sender : TFPgtkObject; data : pointer);
    procedure ClickedLast (Sender : TFPgtkObject; data : pointer);
    procedure CheckSensitive (index : integer);
    procedure FillList;
  public
    constructor create;
    procedure Configure (TheList : TFPgtkCList;
                         CalcIconFunc : TCalcIconFunc;
                         RefreshProc : TRefreshProc;
                         TheSubMenu : TFPgtkMenuShell;
                         AG : PGtkAccelGroup;
                         Mods : TGdkModifierType);
    procedure ChangeCollection (ACollection : TCollection);
    function CurrentItem : TCollectionItem;
    property SelectedRow : integer read FSelectIndex;
    property Title : string read FTitle write SetTitle;

  end;

implementation

uses XPMs, GtkDefTexts, FPgtkExt;

var
  DefAdd, DefCopy, DefDel, DefUp, DefDown : PGdkPixmap;
  DefAddM, DefCopyM, DefDelM, DefUpM, DefDownM : PGdkBitmap;

{ TButtonRow }

procedure TButtonRow.SetTitle (Value : string);
begin
  FTitle := Value + ': ';
end;

procedure TButtonRow.CreatePixmaps;
begin
  IAdd := TFPgtkPixmap.Create;
  ICopy := TFPgtkPixmap.Create;
  IDelete := TFPgtkPixmap.Create;
  IUp := TFPgtkPixmap.Create;
  IDown := TFPgtkPixmap.Create;
  if assigned (DefAdd) then
    begin
    IAdd.SetPixmap (DefAdd, DefAddM);
    ICopy.SetPixmap (DefCopy, DefCopyM);
    IDelete.SetPixmap (DefDel, DefDelM);
    IUp.SetPixmap (DefUp, DefUpM);
    IDown.SetPixmap (DefDown, DefDownM);
    end
  else
    begin
    IAdd.LoadFromArray (XPMEditAdd);
    ICopy.LoadFromArray (XPMEditCopy);
    IDelete.LoadFromArray (XPMEditDelete);
    IUp.LoadFromArray (XPMEditUp);
    IDown.LoadFromArray (XPMEditDown);
    IAdd.GetPixmap (DefAdd, DefAddM);
    ICopy.GetPixmap (DefCopy, DefCopyM);
    IDelete.GetPixmap (DefDel, DefDelM);
    IUp.GetPixmap (DefUp, DefUpM);
    IDown.GetPixmap (DefDown, DefDownM);
    end;
end;

constructor TButtonRow.create;
begin
  inherited;
  // Create the Pixmaps
  CreatePixMaps;
  // Configure the toolbar
  ButtonRelief := Gtk_Relief_None;
  // Create the buttons with the pixmaps
  FAdd := AppendItem ('',RemoveUnderscore(smAdd),'',IAdd, @ClickedAdd, nil);
  FAdd.Sensitive := False;
  FCopy := AppendItem ('',RemoveUnderscore(smCopy),'',ICopy, @ClickedCopy, nil);
  FCopy.Sensitive := False;
  FDelete := AppendItem ('',RemoveUnderscore(smDelete),'',IDelete, @ClickedDelete, nil);
  FDelete.Sensitive := False;
  AppendSpace;
  FUp := AppendItem ('',RemoveUnderscore(smUp),'',IUp, @ClickedUp, nil);
  FUp.Sensitive := False;
  FDown := AppendItem ('',RemoveUnderscore(smDown),'',IDown, @ClickedDown, nil);
  FDown.Sensitive := False;
end;

procedure TButtonRow.Configure (TheList : TFPgtkCList;
                         CalcIconFunc : TCalcIconFunc;
                         RefreshProc : TRefreshProc;
                         TheSubMenu : TFPgtkMenuShell;
                         AG : PGtkAccelGroup;
                         Mods : TGdkModifierType);

  function MyKeyDef (Key : guint) : PAccelKeyDef;
  begin
    if Mods = 0 then
      result := nil
    else
      result := MakeAccelKeyDef (AG, Key, Mods);
  end;
begin
  FList := TheList;
  FCollection := nil;
  FRefreshProc := RefreshProc;
  FCalcIconFunc := CalcIconFunc;
  with FList do
    begin
    SelectionMode := Gtk_Selection_Browse;
    ConnectSelectRow (@NewSelection, nil);
    SetColumnAutoResize (0, true);
    if assigned (FCalcIconFunc) then
      SetColumnAutoResize (1, true);
    end;
  with TheSubMenu do
    begin
    FMAdd := NewMenuItem (smAdd, '', '', MyKeyDef (gdk_A), @ClickedAdd, nil);
    FMDelete := NewMenuItem (smDelete, '', '', MyKeyDef (gdk_D), @ClickedDelete, nil);
    FMCopy := NewMenuItem (smCopy, '', '', MyKeyDef (gdk_C), @ClickedCopy, nil);
    FMUp := NewMenuItem (smUp, '', '', MyKeyDef (gdk_U), @ClickedUp, nil);
    FMDown := NewMenuItem (smDown, '', '', MyKeyDef (gdk_O), @ClickedDown, nil);
    FMFirst := NewMenuItem (smFirst, '', '', MyKeyDef (gdk_F), @ClickedFirst, nil);
    FMLast := NewMenuItem (smLast, '', '', MyKeyDef (gdk_L), @ClickedLast, nil);
    FMPrev := NewMenuItem (smPrevious, '', '', MyKeyDef (gdk_P), @ClickedPrevious, nil);
    FMNext := NewMenuItem (smNext, '', '', MyKeyDef (gdk_N), @ClickedNext, nil);
    Add (FMAdd);
    Add (FMCopy);
    Add (FMDelete);
    Add (NewLine);
    Add (FMUp);
    Add (FMDown);
    Add (NewLine);
    Add (FMFirst);
    Add (FMPrev);
    Add (FMNext);
    Add (FMLast);
    end;
  CheckSensitive (-1);
end;

procedure TButtonRow.FillList;
var r : integer;
    pm : PGdkPixMap;
    m : PGdkBitmap;
begin
  FList.Freeze;
  try
    FList.Clear;
    if assigned (FCollection) and (FCollection.Count > 0) then
      with FCollection do
        begin
        if assigned (FCalcIconFunc) then
          for r := 0 to count-1 do
            begin
            FCalcIconFunc (Items[r], pm, m);
            FList.Append (['',Items[r].Displayname]);
            FList.SetPixmap (r, 0, pm, m);
            end
        else
          for r := 0 to count-1 do
            begin
            FList.Append (Items[r].Displayname, '~');
            end;
        end
    else
      begin
      FSelectIndex := -1;
      if assigned (FRefreshProc) then
        FRefreshProc (nil, false);
      end;
  finally
    FList.Thaw;
  end;
end;

procedure TButtonRow.ChangeCollection (ACollection : TCollection);
begin
  {$IFDEF debug}
  writeln (FTitle, 'ChangeCollection');
  {$ENDIF}
  FCollection := ACollection;
  FillList;
  if assigned(FCollection) and (FCollection.count > 0) then
    CheckSensitive (0)
  else
    CheckSensitive (-1);
  {$IFDEF debug}
  writeln (FTitle, 'ChangeCollection End');
  {$ENDIF}
end;

procedure TButtonRow.NewSelection (Sender : TFPgtkObject; row,column:integer;
                                   event:PGdkEventButton; data:pointer);
begin
  {$IFDEF debug}
  writeln (FTitle, 'NewSelection');
  {$ENDIF}
  if row >= 0 then
    begin
    FSelectIndex := row;
    CheckSensitive (row);
    if assigned (FRefreshProc) then
      FRefreshProc (FCollection.items[row], FNeedFocus);
    end;
  {$IFDEF debug}
  writeln (FTitle, 'NewSelection End');
  {$ENDIF}
end;

procedure TButtonRow.ClickedAdd (Sender : TFPgtkObject; data : pointer);
var i : TCollectionItem;
    pm : PGdkPixmap;
    m : PGdkBitmap;
begin
  {$IFDEF debug}
  writeln (FTitle, 'ClickedAdd');
  {$ENDIF}
  if assigned(FCollection) then
    begin
    i := FCollection.Add;
    i.displayname := sNew;
    if assigned (FCalcIconFunc) then
      begin
      FCalcIconFunc (I, pm, m);
      FList.Append (['',I.DisplayName]);
      FList.SetPixmap (Flist.count, 0, pm, m);
      end
    else
      FList.Append (i.displayName, '~');
    FNeedFocus := True;
    FList.SelectRow (FList.Count-1, 0);
    end;
  {$IFDEF debug}
  writeln (FTitle, 'ClickedAdd End');
  {$ENDIF}
end;

procedure TButtonRow.ClickedCopy (Sender : TFPgtkObject; data : pointer);
var c, i : TCollectionItem;
    pm : PGdkPixmap;
    m : PGdkBitmap;
begin
  {$IFDEF debug}
  writeln (FTitle, 'ClickedCopy');
  {$ENDIF}
  c := CurrentItem;
  if assigned(FCollection) and assigned (c) then
    begin
    i := FCollection.Add;
    i.assign(c);
    if assigned (FCalcIconFunc) then
      begin
      FCalcIconFunc (I, pm, m);
      FList.Append (['',I.DisplayName]);
      FList.SetPixmap (Flist.count-1, 0, pm, m);
      end
    else
      FList.Append (i.displayName, '~');
    FNeedFocus := True;
    FList.SelectRow (FList.Count-1,0);
    end;
  {$IFDEF debug}
  writeln (FTitle, 'ClickedCopy End');
  {$ENDIF}
end;

procedure TButtonRow.ClickedDelete (Sender : TFPgtkObject; data : pointer);
begin
  {$IFDEF debug}
  writeln (FTitle, 'ClickedDelete');
  {$ENDIF}
  if FSelectIndex >= 0 then
    begin
    FCollection.Items[FSelectIndex].Free;
    FList.Remove (FSelectIndex);
    FNeedFocus := False;
    FList.SelectRow (FSelectIndex, 0);
    end;
  {$IFDEF debug}
  writeln (FTitle, 'ClickedDelete End');
  {$ENDIF}
end;

procedure TButtonRow.ClickedUp (Sender : TFPgtkObject; data : pointer);
begin
  {$IFDEF debug}
  writeln (FTitle, 'ClickedUp');
  {$ENDIF}
  if FSelectIndex > 0 then
    begin
    with FCollection.Items[FSelectIndex] do
      Index := Index - 1;
    with FList do
      begin
      SwapRows (FSelectIndex, FSelectIndex-1);
      FNeedFocus := False;
      SelectRow (FSelectIndex-1, 0);
      end;
    //CheckSensitive (FSelectIndex-1);
    end;
  {$IFDEF debug}
  writeln (FTitle, 'ClickedUp End');
  {$ENDIF}
end;

procedure TButtonRow.ClickedDown (Sender : TFPgtkObject; data : pointer);
begin
  {$IFDEF debug}
  writeln (FTitle, 'ClickedDown');
  {$ENDIF}
  if (FSelectIndex >= 0) and (FSelectIndex < FCollection.count-1) then
    begin
    with FCollection.Items[FSelectIndex] do
      Index := Index + 1;
    with FList do
      begin
      SwapRows (FSelectIndex, FSelectIndex+1);
      FNeedFocus := False;
      SelectRow (FSelectIndex+1, 0);
      end;
    end;
  {$IFDEF debug}
  writeln (FTitle, 'ClickedDown End');
  {$ENDIF}
end;

procedure TButtonRow.ClickedFirst (Sender : TFPgtkObject; data : pointer);
begin
  FNeedFocus := False;
  with FList do
    SelectRow (0, 0);
end;

procedure TButtonRow.ClickedPrevious (Sender : TFPgtkObject; data : pointer);
begin
  FNeedFocus := False;
  if (FSelectIndex > 0) then
    with FList do
      SelectRow (FSelectIndex-1, 0);
end;


procedure TButtonRow.ClickedNext (Sender : TFPgtkObject; data : pointer);
begin
  FNeedFocus := False;
  if (FSelectIndex >= 0) and (FSelectIndex < FCollection.count-1) then
    with FList do
      SelectRow (FSelectIndex+1, 0);
end;

procedure TButtonRow.ClickedLast (Sender : TFPgtkObject; data : pointer);
begin
  FNeedFocus := False;
  with FList do
    SelectRow (FSelectIndex+1, 0);
end;

procedure TButtonRow.CheckSensitive (index : integer);
var b : boolean;
begin
  {$IFDEF debug}
  writeln (FTitle, 'CheckSensitive ', index);
  {$ENDIF}
  b := assigned(FCollection);
  FAdd.Sensitive := b;
  FMAdd.Sensitive := b;
  FMFirst.Sensitive := b;
  FMLast.Sensitive := b;
  b := assigned(FCollection) and (index >= 0) and (index < FCollection.Count);
  FCopy.Sensitive := b;
  FMCopy.Sensitive := b;
  b := assigned(FCollection) and (index >= 0) and (index < FCollection.count);
  FDelete.Sensitive := b;
  FMDelete.Sensitive := b;
  b := assigned(FCollection) and (index >= 0) and (index < FCollection.count-1);
  FDown.Sensitive := b;
  FMDown.Sensitive := b;
  FMNext.Sensitive := b;
  FUp.Sensitive := b;
  b := assigned(FCollection) and (index > 0) and (index < FCollection.count);
  FUp.Sensitive := b;
  FMUp.Sensitive := b;
  FMPrev.Sensitive := b;
  {$IFDEF debug}
  writeln (FTitle, 'CheckSensitive End');
  {$ENDIF}
end;

function TButtonRow.CurrentItem : TCollectionItem;
begin
  {$IFDEF debug}
  writeln (FTitle, 'CurrentItem');
  {$ENDIF}
  if FSelectIndex >= 0 then
    result := FCollection.Items[FSelectIndex]
  else
    result := nil;
  {$IFDEF debug}
  writeln (FTitle, 'CurrentItem End');
  {$ENDIF}
end;

end.
