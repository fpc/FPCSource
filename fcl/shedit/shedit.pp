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

  TKeyboardActionDescr = class(TCollectionItem)
  public
    Descr: String;                      // Human readable description
    Method: TKeyboardActionProc;
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

    procedure InvalidateLines(y1, y2: Integer); virtual; abstract;

    // Drawing
    procedure ClearRect(x1, y1, x2, y2: Integer); virtual; abstract;
    procedure DrawTextLine(x1, x2, y: Integer; s: PChar); virtual; abstract;

    // Cursor
    procedure ShowCursor(x, y: Integer); virtual; abstract;
    procedure HideCursor(x, y: Integer); virtual; abstract;

    // Scrolling support
    function  GetVertPos: Integer; virtual; abstract;
    procedure SetVertPos(y: Integer); virtual; abstract;
    function  GetPageHeight: Integer; virtual; abstract;
    procedure SetLineCount(count: Integer); virtual; abstract;
    property  VertPos: Integer read GetVertPos write SetVertPos;
    property  PageHeight: Integer read GetPageHeight;
    property  LineCount: Integer write SetLineCount;

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

    procedure CursorUp;
    procedure CursorDown;
    procedure CursorLeft;
    procedure CursorRight;
    procedure CursorHome;
    procedure CursorEnd;
    procedure CursorPageUp;
    procedure CursorPageDown;

    // Keyboard command handlers
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
    function AddKeyboardAction(AMethod: TKeyboardActionProc;
      ADescr: String): TKeyboardActionDescr;
    function AddKeyboardAssignment(AKeyCode: Integer; AShiftState: TShiftState;
      AAction: TKeyboardActionDescr): TShortcut;
    procedure AddKeyDef(AMethod: TKeyboardActionProc; ADescr: String;
      AKeyCode: Integer; AShiftState: TShiftState);

    procedure DrawContent(x1, y1, x2, y2: Integer);
    procedure KeyPressed(KeyCode: LongWord; ShiftState: TShiftState); virtual;

    KeyboardActions: TCollection;
    Shortcuts: TCollection;

    shDefault, shSelected: Integer;

    property Doc: TTextDoc read FDoc;
    property CursorX: Integer read FCursorX write SetCursorX;
    property CursorY: Integer read FCursorY write SetCursorY;
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

  FRenderer.SetLineCount(FDoc.LineCount);
end;

procedure TSHTextEdit.ModifiedChanged(Sender: TObject);
begin
  if Assigned(OnModifiedChange) then
    OnModifiedChange(Self);
end;

procedure TSHTextEdit.SetCursorX(NewCursorX: Integer);
begin
  FCursorX := NewCursorX;
  HideCursor;
  ShowCursor;
end;

procedure TSHTextEdit.SetCursorY(NewCursorY: Integer);
begin
  FCursorY := NewCursorY;
  HideCursor;
  ShowCursor;
end;

procedure TSHTextEdit.LineInserted(Sender: TTextDoc; Line: Integer);
begin
  Renderer.LineCount := FDoc.LineCount;
  ChangeInLine(Line);
end;

procedure TSHTextEdit.LineRemoved(Sender: TTextDoc; Line: Integer);
begin
  LineInserted(Sender, Line);
end;


end.


{
  $Log$
  Revision 1.1  1999-10-29 15:59:04  peter
    * inserted in fcl

}
