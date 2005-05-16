{
    $Id: shedit.pp,v 1.4 2005/02/14 17:13:17 peter Exp $

    "SHEdit" - Text editor with syntax highlighting
    Copyright (C) 1999-2000 by Sebastian Guenther (sg@freepascal.org)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

// ===================================================================
//   Generic text editor widget with syntax highlighting capabilities
// ===================================================================

{$MODE objfpc}
{$H+}

unit shedit;

interface

uses
  Classes, doc_text;

type

  TSHTextEdit = class;
  TSHTextEditClass = class of TSHTextEdit;


// -------------------------------------------------------------------
//   Keyboard/action assignment handling
// -------------------------------------------------------------------

  TKeyboardActionProc = procedure of object;

  TSelectionAction = (selNothing,selExtend,selClear);

  TKeyboardActionDescr = class(TCollectionItem)
  public
    Descr: String;                      // Human readable description
    Method: TKeyboardActionProc;
    SelectionAction : TSelectionAction;
  end;

  TShortcut = class(TCollectionItem)
  public
    KeyCode: Integer;
    ShiftState: TShiftState;
    Action: TKeyboardActionDescr;
  end;


// -------------------------------------------------------------------
//   Undo/redo buffer stuff
// -------------------------------------------------------------------

  TUndoInfo = class;
  TUndoInfo = class
    Prev, Next: TUndoInfo;
    CursorX, CursorY: Integer;
    function Merge(AEdit: TSHTextEdit; AInfo: TUndoInfo): Boolean; virtual;
    procedure DoUndo(AEdit: TSHTextEdit); virtual; abstract;
  end;

  TUndoEdit = class(TUndoInfo)
    NumOfChars: Integer;
    constructor Create;
    constructor Create(ANumOfChars: Integer);
    function Merge(AEdit: TSHTextEdit; AInfo: TUndoInfo): Boolean; override;
    procedure DoUndo(AEdit: TSHTextEdit); override;
  end;

  TUndoDelLeft = class(TUndoInfo)
    DeletedString: String;
    constructor Create(const ADeletedString: String);
    function Merge(AEdit: TSHTextEdit; AInfo: TUndoInfo): Boolean; override;
    procedure DoUndo(AEdit: TSHTextEdit); override;
  end;

  TUndoDelRight = class(TUndoDelLeft)
    procedure DoUndo(AEdit: TSHTextEdit); override;
  end;


// -------------------------------------------------------------------
//   Selection support
// -------------------------------------------------------------------

  TSelection = class
  public
    constructor Create;
    procedure Clear;

    StartX, StartY, EndX, EndY: Integer;

    function IsValid: Boolean;
    function IsEmpty: Boolean;
    // Ordered coordinates: swaps start and end if necessary
    function OStartX: Integer;
    function OStartY: Integer;
    function OEndX: Integer;
    function OEndY: Integer;
  end;


// -------------------------------------------------------------------
//   SHWidget interface
// -------------------------------------------------------------------

  ISHWidget = class
    // Drawing
    procedure InvalidateRect(x, y, w, h: Integer); virtual; abstract;
    procedure ClearRect(x, y, w, h: Integer); virtual; abstract;
    procedure DrawTextLine(x1, x2, y: Integer; s: PChar); virtual; abstract;

    // Cursor placement
    procedure ShowCursor(x, y: Integer); virtual; abstract;
    procedure HideCursor(x, y: Integer); virtual; abstract;

    // Scrolling support
    function  GetHorzPos: Integer; virtual; abstract;
    procedure SetHorzPos(x: Integer); virtual; abstract;
    function  GetVertPos: Integer; virtual; abstract;
    procedure SetVertPos(y: Integer); virtual; abstract;
    function  GetPageWidth: Integer; virtual; abstract;
    function  GetPageHeight: Integer; virtual; abstract;
    function  GetLineWidth: Integer; virtual; abstract;
    procedure SetLineWidth(count: Integer); virtual; abstract;
    function  GetLineCount: Integer; virtual; abstract;
    procedure SetLineCount(count: Integer); virtual; abstract;

    // Clipboard support
    function  GetClipboard: String; virtual; abstract;
    procedure SetClipboard(Content: String); virtual; abstract;


    property  HorzPos: Integer read GetHorzPos write SetHorzPos;
    property  VertPos: Integer read GetVertPos write SetVertPos;
    property  PageWidth: Integer read GetPageWidth;
    property  PageHeight: Integer read GetPageHeight;
    property  LineWidth: Integer read GetLineWidth write SetLineWidth;
    property  LineCount: Integer read GetLineCount write SetLineCount;
    property  Clipboard: String read GetClipboard write SetClipboard;
  end;


// -------------------------------------------------------------------
//   SHTextEdit: The main editor class
// -------------------------------------------------------------------

  TShortcutEvent = procedure of object;

  TEditLineEvent = procedure(Sender: TSHTextEdit; Line: Integer) of object;

  TSHTextEdit = class
  protected
    // ===== Internally used stuff
    ViewInfo: TViewInfo;                // Connection to document
    CursorVisible: Integer;
    OverwriteMode: Boolean;
    LastUndoInfo, LastRedoInfo: TUndoInfo;      // tails of double linked lists

    FSel: TSelection;
    OldSelValid: Boolean;
    OldSelStartX, OldSelStartY, OldSelEndX, OldSelEndY: Integer;

    // OnKeyPressed saves the cursor position before calling key handlers
    LastCursorX, LastCursorY: Integer;


    function  CalcSHFlags(FlagsIn: Byte; source: String): Byte;
    procedure ChangeInLine(line: Integer);  // Redraws screen where necessary
    procedure AddUndoInfo(AInfo: TUndoInfo; CanMerge: Boolean);

    // The default implementation does not perform any syntax highlighting:
    procedure DoHighlighting(var flags: Byte; source, dest: PChar); virtual;

    // ===== Properties

    FDoc: TTextDoc;                     // Document object for text
    FCursorX, FCursorY: Integer;        // 0/0 = upper left corner
    FOnModifiedChange: TNotifyEvent;
    FOnLineInsert, FOnLineRemove: TEditLineEvent;
    FWidget: ISHWidget;

    procedure SetCursorX(NewCursorX: Integer);
    procedure SetCursorY(NewCursorY: Integer);

    procedure DocumentCleared(Sender: TObject);
    procedure ModifiedChanged(Sender: TObject);
    procedure LineInserted(Sender: TTextDoc; Line: Integer); virtual;
    procedure LineRemoved(Sender: TTextDoc; Line: Integer); virtual;
    procedure LineChanged(Sender: TTextDoc; Line: Integer); virtual;

    function  ExecKey(Key: Char; BlockMode: Boolean): Boolean;
    procedure ExecKeys(Keys: String; BlockMode: Boolean);
    procedure MultiDelLeft(Count: Integer);

  public
    // Keyboard command handlers
    // Cursor movement
    procedure AdjustCursorToRange;
    procedure AdjustRangeToCursor;
    procedure CursorUp;
    procedure CursorDown;
    procedure CursorLeft;
    procedure CursorRight;
    procedure CursorHome;
    procedure CursorEnd;
    procedure CursorDocBegin;
    procedure CursorDocEnd;
    procedure CursorPageUp;
    procedure CursorPageDown;

    // Misc
    procedure ToggleOverwriteMode;
    procedure EditDelLeft;
    procedure EditDelRight;
    procedure EditDelLine;
    procedure EditUndo;
    procedure EditRedo;
    procedure ClipboardCut;
    procedure ClipboardCopy;
    procedure ClipboardPaste;

    // Customizable keyboard handlers
    procedure KeyReturn; virtual;

  public
    constructor Create(ADoc: TTextDoc; AWidget: ISHWidget); virtual;
    destructor Destroy; override;
    function  AddKeyboardAction(AMethod: TKeyboardActionProc;ASelectionAction:TSelectionAction;ADescr: String): TKeyboardActionDescr;
    function AddKeyboardAssignment(AKeyCode: Integer; AShiftState: TShiftState;
      AAction: TKeyboardActionDescr): TShortcut;
    procedure AddKeyDef(AMethod: TKeyboardActionProc; ASelectionAction:TSelectionAction; ADescr: String; AKeyCode: Integer; AShiftState: TShiftState);

    procedure HideCursor;
    procedure ShowCursor;
    procedure FocusIn;
    procedure FocusOut;
    procedure DrawContent(x, y, w, h: Integer);

    // Return value: True=Key has been pressed, False=Key has not been processed
    function  KeyPressed(KeyCode: LongWord; ShiftState: TShiftState): Boolean; virtual;

    procedure StartSelectionChange;
    procedure EndSelectionChange;

    KeyboardActions: TCollection;
    Shortcuts: TCollection;

    shDefault, shSelected: Integer;

    property Doc: TTextDoc read FDoc;
    property CursorX: Integer read FCursorX write SetCursorX;
    property CursorY: Integer read FCursorY write SetCursorY;
    property Selection: TSelection read FSel write FSel;
    property OnModifiedChange: TNotifyEvent read FOnModifiedChange write FOnModifiedChange;
    property OnLineInsert: TEditLineEvent read FOnLineInsert write FOnLineInsert;
    property OnLineRemove: TEditLineEvent read FOnLineRemove write FOnLineRemove;
    property Widget: ISHWidget read FWidget;
  end;



implementation

uses
  SysUtils;


{$INCLUDE undo.inc}
{$INCLUDE keys.inc}
{$INCLUDE drawing.inc}


constructor TSelection.Create;
begin
  inherited Create;
  Clear;
end;

function TSelection.IsValid: Boolean;
begin
  Result := StartX <> -1;
end;

function TSelection.IsEmpty: Boolean;
begin
  Result := (StartX = EndX) and (StartY = EndY);
end;

function TSelection.OStartX: Integer;
begin
  if (StartY > EndY) or ((StartY = EndY) and (StartX > EndX)) then
    Result := EndX
  else
    Result := StartX;
end;

function TSelection.OStartY: Integer;
begin
  if (StartY > EndY) or ((StartY = EndY) and (StartX > EndX)) then
    Result := EndY
  else
    Result := StartY;
end;

function TSelection.OEndX: Integer;
begin
  if (StartY > EndY) or ((StartY = EndY) and (StartX > EndX)) then
    Result := StartX
  else
    Result := EndX;
end;

function TSelection.OEndY: Integer;
begin
  if (StartY > EndY) or ((StartY = EndY) and (StartX > EndX)) then
    Result := StartY
  else
    Result := EndY;
end;



procedure TSelection.Clear;
begin
  StartX := -1;
  StartY := -1;
  EndX := -1;
  EndY := -1;
end;


constructor TSHTextEdit.Create(ADoc: TTextDoc; AWidget: ISHWidget);
var
  i: Integer;
begin
  ASSERT(Assigned(ADoc) and Assigned(AWidget));

  FDoc := ADoc;

  ViewInfo := TViewInfo(FDoc.ViewInfos.Add);
  ViewInfo.OnLineInsert := @LineInserted;
  ViewInfo.OnLineRemove := @LineRemoved;
  ViewInfo.OnLineChange := @LineChanged;
  ViewInfo.OnModifiedChange := @ModifiedChanged;
  ViewInfo.OnClearDocument := @DocumentCleared;

  FWidget := AWidget;

  FSel := TSelection.Create;

  KeyboardActions := TCollection.Create(TKeyboardActionDescr);
  Shortcuts := TCollection.Create(TShortcut);

  Widget.LineCount := FDoc.LineCount;
  Widget.LineWidth := FDoc.LineWidth;
  CursorX:=0;
  CursorY:=0;
end;

destructor TSHTextEdit.Destroy;
var
  buf, prev: TUndoInfo;
begin
  ViewInfo.Free;
  FDoc.Release;
  KeyboardActions.Free;
  Shortcuts.Free;
  FSel.Free;

  buf := LastUndoInfo;
  while Assigned(buf) do begin
    prev := buf.prev;
    buf.Free;
    buf := prev;
  end;

  buf := LastRedoInfo;
  while Assigned(buf) do begin
    prev := buf.prev;
    buf.Free;
    buf := prev;
  end;

  inherited Destroy;
end;

procedure TSHTextEdit.DocumentCleared(Sender: TObject);
begin
  FCursorX := 0;
  FCursorY := 0;
  FSel.Clear;
  AdjustRangeToCursor;
  Widget.ClearRect(0, 0, Widget.PageWidth, Widget.PageHeight);
end;

procedure TSHTextEdit.ModifiedChanged(Sender: TObject);
begin
  if Assigned(OnModifiedChange) then
    OnModifiedChange(Self);
end;

procedure TSHTextEdit.FocusIn;
begin
  CursorVisible := 0;
  ShowCursor;
end;

procedure TSHTextEdit.FocusOut;
begin
  HideCursor;
end;


procedure TSHTextEdit.SetCursorX(NewCursorX: Integer);
begin
  HideCursor;
  if NewCursorX >= 0 then
    FCursorX := NewCursorX
  else
    FCursorX := 0;
  if FCursorX > FDoc.LineWidth then
    Widget.LineWidth := FCursorX + 4
  else
    Widget.LineWidth := FDoc.LineWidth + 4;
  ShowCursor;
  AdjustRangeToCursor;
end;

procedure TSHTextEdit.SetCursorY(NewCursorY: Integer);
begin
  HideCursor;
  if NewCursorY >= 0 then
    FCursorY := NewCursorY
  else
    FCursorY := 0;
  ShowCursor;
  AdjustRangeToCursor;
end;

procedure TSHTextEdit.LineInserted(Sender: TTextDoc; Line: Integer);
begin
  Widget.LineCount := FDoc.LineCount;
  if FCursorX > FDoc.LineWidth then
    Widget.LineWidth := FCursorX + 4
  else
    Widget.LineWidth := FDoc.LineWidth + 4;
  if Assigned(FOnLineInsert) then
    FOnLineInsert(Self, Line);
  ChangeInLine(Line);
end;

procedure TSHTextEdit.LineRemoved(Sender: TTextDoc; Line: Integer);
begin
  Widget.LineCount := FDoc.LineCount;
  if FCursorX > FDoc.LineWidth then
    Widget.LineWidth := FCursorX + 4
  else
    Widget.LineWidth := FDoc.LineWidth + 4;
  if Assigned(FOnLineRemove) then
    FOnLineRemove(Self, Line);
  ChangeInLine(Line);
end;

procedure TSHTextEdit.LineChanged(Sender: TTextDoc; Line: Integer);
begin
  if FCursorX > FDoc.LineWidth then
    Widget.LineWidth := FCursorX + 4
  else
    Widget.LineWidth := FDoc.LineWidth + 4;
  ChangeInLine(Line);
end;

procedure TSHTextEdit.StartSelectionChange;
begin
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
end;

procedure TSHTextEdit.EndSelectionChange;

  procedure RedrawArea(x1, y1, x2, y2: Integer);
  begin
    //WriteLn('Redraw: ', x1, '/', y1, ' - ', x2, '/', y2);
    if y1 = y2 then
      FWidget.InvalidateRect(x1, y1, (x2 - x1) + 1, (y2 - y1) + 1)
    else begin
      FWidget.InvalidateRect(x1, y1, FWidget.PageWidth + FWidget.HorzPos, 1);
      if y1 < y2 - 1 then
        FWidget.InvalidateRect(0, y1 + 1, FWidget.PageWidth + FWidget.HorzPos, (y2 - y1) - 1);
      FWidget.InvalidateRect(0, y2, x2, 1);
    end;
  end;

begin
//WriteLn('=> TSHTextEdit.EndSelectionChange');
  if not OldSelValid then begin
    if FSel.IsValid then
      RedrawArea(FSel.OStartX, FSel.OStartY, FSel.OEndX, FSel.OEndY);
  end else begin
    //WriteLn('Old selection: ', OldSelStartX, '/', OldSelStartY, ' - ', OldSelEndX, '/', OldSelEndY);
    if not FSel.IsValid then begin
      //WriteLn('No new selection');
      RedrawArea(OldSelStartX, OldSelStartY, OldSelEndX, OldSelEndY);
    end else begin
      //WriteLn('New selection: ', FSel.OStartX, '/', FSel.OStartY, ' - ', FSel.OEndX, '/', FSel.OEndY);
      // Do OldSel and FSel intersect?
      if (OldSelEndY < FSel.OStartY) or (OldSelStartY > FSel.OEndY) or
         ((OldSelEndY = FSel.OStartY) and (OldSelEndX <= FSel.OStartX)) or
         ((OldSelStartY = FSel.OEndY) and (OldSelStartX >= FSel.OEndX)) then
      begin
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
      end;
    end;
  end;
  ShowCursor;
end;


end.


{
  $Log: shedit.pp,v $
  Revision 1.4  2005/02/14 17:13:17  peter
    * truncate log

}
