(******************************************************************************
 *
 * Copyright (c) 1994-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: Field.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *   This file defines field structures and routines.
 *
 * History:
 *    August 29, 1994   Created by Art Lamb
 *
 *****************************************************************************)
unit field;

interface

uses palmos, coretraps, rect, font, window, control;

const
  maxFieldTextLen = $7fff;

// default maximun number of line the a dynamicly sizing field will expand to.
// Can be changed with FldSetMaxVisibleLines

  maxFieldLines = 11;

// kind alignment values
type
  justifications = Enum;

const
  leftAlign = 0;
  centerAlign = Succ(leftAlign);
  rightAlign = Succ(centerAlign);

type
  JustificationType = justifications;

const
  undoBufferSize = 100;

type
  UndoMode = enum;

const
  undoNone = 0;
  undoTyping = Succ(undoNone);
  undoBackspace = Succ(undoTyping);
  undoDelete = Succ(undoBackspace);
  undoPaste = Succ(undoDelete);
  undoCut = Succ(undoPaste);
  undoInput = Succ(undoCut);

type
  FieldUndoType = record
  {$ifdef ALLOW_ACCESS_TO_INTERNALS_OF_FIELDS} // These fields will not be available in the next OS release!
    mode: UndoMode;
    reserved: UInt8;
    start: UInt16;
    end_: UInt16;
    bufferLen: UInt16;
    buffer: PChar;
  {$endif}
  end;
  FieldUndoTag = FieldUndoType;

  FieldAttrType = record
    Bits: UInt16;
{
 UInt16 usable   :1; // Set if part of ui
 UInt16 visible   :1; // Set if drawn, used internally
 UInt16 editable  :1; // Set if editable
 UInt16 singleLine  :1; // Set if only a single line is displayed
 UInt16 hasFocus      :1;   // Set if the field has the focus
 UInt16 dynamicSize :1;   // Set if height expands as text is entered
 UInt16 insPtVisible :1; // Set if the ins pt is scolled into view
 UInt16 dirty   :1; // Set if user modified
 UInt16 underlined  :2; // text underlined mode
 UInt16 justification :2; // text alignment
 UInt16 autoShift  :1; // Set if auto case shift
 UInt16 hasScrollBar :1; // Set if the field has a scroll bar
 UInt16 numeric   :1; // Set if numeric, digits and secimal separator only
 UInt16 reserved  :1;   // Reserved for future use
}
  end;
  FieldAttrTag = FieldAttrType;
  FieldAttrPtr = ^FieldAttrType;

  LineInfoType = record
  {$ifdef ALLOW_ACCESS_TO_INTERNALS_OF_FIELDS} // These fields will not be available in the next OS release!
    start: UInt16;  // position in text string of first char.
    length: UInt16; // number of character in the line
  {$endif}
  end;
  LineInfoTag = LineInfoType;
  LineInfoPtr = ^LineInfoType;

  FieldType = record
  {$ifdef ALLOW_ACCESS_TO_INTERNALS_OF_FIELDS} // These fields will not be available in the next OS release!
    id: UInt16;
    rect: RectangleType;
    attr: FieldAttrType;
    text: PChar; // pointer to the start of text string
    textHandle: MemHandle; // block the contains the text string
    lines: LineInfoPtr;
    textLen: UInt16;
    textBlockSize: UInt16;
    maxChars: UInt16;
    selFirstPos: UInt16;
    selLastPos: UInt16;
    insPtXPos: UInt16;
    insPtYPos: UInt16;
    fontID: FontID;
    maxVisibleLines: UInt8; // added in 4.0 to support FldSetMaxVisibleLines
  {$endif}
  end;

  FieldPtr = ^FieldType; // deprecated, use FieldType *

//---------------------------------------------------------------------
// Field Functions
//---------------------------------------------------------------------

procedure FldCopy(const fldP: FieldPtr); syscall sysTrapFldCopy;

procedure FldCut(fldP: FieldPtr); syscall sysTrapFldCut;

procedure FldDrawField(fldP: FieldPtr); syscall sysTrapFldDrawField;

procedure FldEraseField(fldP: FieldPtr); syscall sysTrapFldEraseField;

procedure FldFreeMemory(fldP: FieldPtr); syscall sysTrapFldFreeMemory;

procedure FldGetBounds(const fldP: FieldPtr; rect: RectanglePtr); syscall sysTrapFldGetBounds;

function FldGetFont(const fldP: FieldPtr): FontID; syscall sysTrapFldGetFont;

procedure FldGetSelection(const fldP: FieldPtr; var startPosition, endPosition: UInt16); syscall sysTrapFldGetSelection;

function FldGetTextHandle(const fldP: FieldPtr): MemHandle; syscall sysTrapFldGetTextHandle;

function FldGetTextPtr(const fldP: FieldPtr): PChar; syscall sysTrapFldGetTextPtr;

function FldHandleEvent(fldP: FieldPtr; eventP: EventPtr): Boolean; syscall sysTrapFldHandleEvent;

procedure FldPaste(fldP: FieldPtr); syscall sysTrapFldPaste;

procedure FldRecalculateField(fldP: FieldPtr; redraw: Boolean); syscall sysTrapFldRecalculateField;

procedure FldSetBounds(fldP: FieldPtr; const rP: RectanglePtr); syscall sysTrapFldSetBounds;

procedure FldSetFont(fldP: FieldPtr; fontID: FontID); syscall sysTrapFldSetFont;

procedure FldSetText(fldP: FieldPtr; textHandle: MemHandle; offset, size: UInt16); syscall sysTrapFldSetText;

procedure FldSetTextHandle(fldP: FieldPtr; textHandle: MemHandle); syscall sysTrapFldSetTextHandle;

procedure FldSetTextPtr(fldP: FieldPtr; textP: PChar); syscall sysTrapFldSetTextPtr;

procedure FldSetUsable(fldP: FieldPtr; usable: Boolean); syscall sysTrapFldSetUsable;

procedure FldSetSelection(fldP: FieldPtr; startPosition, endPosition: UInt16); syscall sysTrapFldSetSelection;

procedure FldGrabFocus(fldP: FieldPtr); syscall sysTrapFldGrabFocus;

procedure FldReleaseFocus(fldP: FieldPtr); syscall sysTrapFldReleaseFocus;

function FldGetInsPtPosition(const fldP: FieldPtr): UInt16; syscall sysTrapFldGetInsPtPosition;

procedure FldSetInsPtPosition(fldP: FieldPtr; pos: UInt16); syscall sysTrapFldSetInsPtPosition;

procedure FldSetInsertionPoint(fldP: FieldPtr; pos: UInt16); syscall sysTrapFldSetInsertionPoint;

function FldGetScrollPosition(const fldP: FieldPtr): UInt16; syscall sysTrapFldGetScrollPosition;

procedure FldSetScrollPosition(fldP: FieldPtr; pos: UInt16); syscall sysTrapFldSetScrollPosition;

procedure FldGetScrollValues(const fldP: FieldPtr; var scrollPosP, textHeightP, fieldHeightP: UInt16); syscall sysTrapFldGetScrollValues;

function FldGetTextLength(const fldP: FieldPtr): UInt16; syscall sysTrapFldGetTextLength;

procedure FldScrollField(fldP: FieldPtr; linesToScroll: UInt16; direction: WinDirectionType); syscall sysTrapFldScrollField;

function FldScrollable(const fldP: FieldPtr; direction: WinDirectionType): Boolean; syscall sysTrapFldScrollable;

function FldGetVisibleLines(const fldP: FieldPtr): UInt16; syscall sysTrapFldGetVisibleLines;

function FldGetTextHeight(const fldP: FieldPtr): UInt16; syscall sysTrapFldGetTextHeight;

function FldCalcFieldHeight(const chars: PChar; maxWidth: UInt16): UInt16; syscall sysTrapFldCalcFieldHeight;

function FldWordWrap(const chars: PChar; maxWidth: Int16): UInt16; syscall sysTrapFldWordWrap;

procedure FldCompactText(fldP: FieldPtr); syscall sysTrapFldCompactText;

function FldDirty(const fldP: FieldPtr): Boolean; syscall sysTrapFldDirty;

procedure FldSetDirty(fldP: FieldPtr; dirty: Boolean); syscall sysTrapFldSetDirty;

function FldGetMaxChars(const fldP: FieldPtr): UInt16; syscall sysTrapFldGetMaxChars;

procedure FldSetMaxChars(fldP: FieldPtr; maxChars: UInt16); syscall sysTrapFldSetMaxChars;

function FldInsert(fldP: FieldPtr; const insertChars: PChar; insertLen: UInt16): Boolean; syscall sysTrapFldInsert;

procedure FldDelete(fldP: FieldPtr; start, end_: UInt16); syscall sysTrapFldDelete;

procedure FldUndo(fldP: FieldPtr); syscall sysTrapFldUndo;

function FldGetTextAllocatedSize(const fldP: FieldPtr): UInt16; syscall sysTrapFldGetTextAllocatedSize;

procedure FldSetTextAllocatedSize(fldP: FieldPtr; allocatedSize: UInt16); syscall sysTrapFldSetTextAllocatedSize;

procedure FldGetAttributes(const fldP: FieldPtr; attrP: FieldAttrPtr); syscall sysTrapFldGetAttributes;

procedure FldSetAttributes(fldP: FieldPtr; const attrP: FieldAttrPtr); syscall sysTrapFldSetAttributes;

procedure FldSendChangeNotification(const fldP: FieldPtr); syscall sysTrapFldSendChangeNotification;

procedure FldSendHeightChangeNotification(const fldP: FieldPtr; pos: UInt16; numLines: Int16); syscall sysTrapFldSendHeightChangeNotification;

function FldMakeFullyVisible(fldP: FieldPtr): Boolean; syscall sysTrapFldMakeFullyVisible;

function FldGetNumberOfBlankLines(const fldP: FieldPtr): UInt16; syscall sysTrapFldGetNumberOfBlankLines;

function  FldNewField(formPP: PointerPtr; id: UInt16; x, y, width, height: Coord;
                      font: FontID; maxChars: UInt32; editable, underlined, singleLine, dynamicSize: Boolean;
                      justification: JustificationType; autoShift, hasScrollBar, numeric: Boolean): FieldPtr; syscall sysTrapFldNewField;

// added in 4.0
procedure FldSetMaxVisibleLines(fldP: FieldPtr; maxLines: UInt8); syscall sysTrapFldSetMaxVisibleLines;

implementation

end.
