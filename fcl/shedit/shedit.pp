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
    // Ordered coordinates: swaps start and end if necessary
    function OStartX: Integer;
    function OStartY: Integer;
    function OEndX: Integer;
    function OEndY: Integer;
  end;


// -------------------------------------------------------------------
//   SHRenderer interface
// -------------------------------------------------------------------

  ISHRenderer = class

    procedure InvalidateRect(x1, y1, x2, y2: Integer); virtual; abstract;
    procedure InvalidateLines(y1, y2: Integer); virtual; abstract;

    // Drawing
    procedure ClearRect(x1, y1, x2, y2: Integer); virtual; abstract;
    procedure DrawTextLine(x1, x2, y: Integer; s: PChar); virtual; abstract;

    // Cursor
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
    property  HorzPos: Integer read GetHorzPos write SetHorzPos;
    property  VertPos: Integer read GetVertPos write SetVertPos;
    property  PageWidth: Integer read GetPageWidth;
    property  PageHeight: Integer read GetPageHeight;
    property  LineWidth: Integer read GetLineWidth write SetLineWidth;
    property  LineCount: Integer read GetLineCount write SetLineCount;

    // Clipboard support
    function  GetClipboard: String; virtual; abstract;
    procedure SetClipboard(Content: String); virtual; abstract;
  end;


// -------------------------------------------------------------------
//   SHTextEdit: The main editor class
// -------------------------------------------------------------------

  TShortcutEvent = procedure of object;


  TSHTextEdit = class
  protected
    // ===== Internally used stuff
    ViewInfo: TViewInfo;                // Connection to document
    CursorVisible: Integer;
    OverwriteMode: Boolean;
    LastUndoInfo, LastRedoInfo: TUndoInfo;      // tails of double linked lists

    FSel: TSelection;

    // OnKeyPressed saves the cursor position before calling key handlers
    LastCursorX, LastCursorY: Integer;


    function  CalcSHFlags(FlagsIn: Byte; source: String): Byte;
    procedure HideCursor;
    procedure ShowCursor;
    procedure ChangeInLine(line: Integer);  // Redraws screen where necessary
    procedure AddUndoInfo(AInfo: TUndoInfo; CanMerge: Boolean);

    // The default implementation does not perform any syntax highlighting:
    procedure DoHighlighting(var flags: Byte; source, dest: PChar); virtual;

    // ===== Properties

    FDoc: TTextDoc;                     // Document object for text
    FCursorX, FCursorY: Integer;        // 0/0 = upper left corner
    FOnModifiedChange: TNotifyEvent;
    FRenderer: ISHRenderer;

    procedure SetCursorX(NewCursorX: Integer);
    procedure SetCursorY(NewCursorY: Integer);

    procedure ModifiedChanged(Sender: TObject);
    procedure LineInserted(Sender: TTextDoc; Line: Integer); virtual;
    procedure LineRemoved(Sender: TTextDoc; Line: Integer); virtual;

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
    constructor Create(ADoc: TTextDoc; ARenderer: ISHRenderer); virtual;
    function  AddKeyboardAction(AMethod: TKeyboardActionProc;ASelectionAction:TSelectionAction;ADescr: String): TKeyboardActionDescr;
    function AddKeyboardAssignment(AKeyCode: Integer; AShiftState: TShiftState;
      AAction: TKeyboardActionDescr): TShortcut;
    procedure AddKeyDef(AMethod: TKeyboardActionProc; ASelectionAction:TSelectionAction; ADescr: String; AKeyCode: Integer; AShiftState: TShiftState);

    procedure FocusIn;
    procedure FocusOut;
    procedure DrawContent(x1, y1, x2, y2: Integer);
    procedure KeyPressed(KeyCode: LongWord; ShiftState: TShiftState); virtual;

    KeyboardActions: TCollection;
    Shortcuts: TCollection;

    shDefault, shSelected: Integer;

    property Doc: TTextDoc read FDoc;
    property CursorX: Integer read FCursorX write SetCursorX;
    property CursorY: Integer read FCursorY write SetCursorY;
    property Selection: TSelection read FSel write FSel;
    property OnModifiedChange: TNotifyEvent
      read FOnModifiedChange write FOnModifiedChange;
    property Renderer: ISHRenderer read FRenderer;
  end;



implementation

uses
  Sysutils;


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


constructor TSHTextEdit.Create(ADoc: TTextDoc; ARenderer: ISHRenderer);
var
  i: Integer;
begin
  FDoc := ADoc;
  // The document must not be empty
  if FDoc.LineCount = 0 then
    FDoc.AddLine('');
  ViewInfo := TViewInfo(FDoc.ViewInfos.Add);
  ViewInfo.OnLineInsert := @LineInserted;
  ViewInfo.OnLineRemove := @LineRemoved;
  ViewInfo.OnModifiedChange := @ModifiedChanged;

  FRenderer := ARenderer;

  FSel := TSelection.Create;

  KeyboardActions := TCollection.Create(TKeyboardActionDescr);
  Shortcuts := TCollection.Create(TShortcut);

  Renderer.LineCount := FDoc.LineCount;
  Renderer.LineWidth := FDoc.LineWidth;
  CursorX:=0;
  CursorY:=0;
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
  FCursorX := NewCursorX;
  ShowCursor;
end;

procedure TSHTextEdit.SetCursorY(NewCursorY: Integer);
begin
  HideCursor;
  FCursorY := NewCursorY;
  ShowCursor;
end;

procedure TSHTextEdit.LineInserted(Sender: TTextDoc; Line: Integer);
begin
  Renderer.LineCount := FDoc.LineCount;
  Renderer.LineWidth := FDoc.LineWidth;
  ChangeInLine(Line);
end;

procedure TSHTextEdit.LineRemoved(Sender: TTextDoc; Line: Integer);
begin
  LineInserted(Sender, Line);
end;


end.


{
  $Log$
  Revision 1.6  1999-12-22 22:28:09  peter
    * updates for cursor setting
    * button press event works

  Revision 1.5  1999/12/10 15:01:03  peter
    * first things for selection
    * Better Adjusting of range/cursor

  Revision 1.4  1999/12/09 23:16:41  peter
    * cursor walking is now possible, both horz and vert ranges are now
      adapted
    * filter key modifiers
    * selection move routines added, but still no correct output to the
      screen

  Revision 1.3  1999/12/06 21:27:27  peter
    * gtk updates, redrawing works now much better and clears only between
      x1 and x2

  Revision 1.2  1999/11/15 21:47:36  peter
    * first working keypress things

  Revision 1.1  1999/10/29 15:59:04  peter
    * inserted in fcl

}
