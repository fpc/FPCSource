{$MACRO ON}

(******************************************************************************
 *
 * Copyright (c) 1994-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: clipBoard.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *   This file defines clipboard structures and routines.
 *
 * History:
 *    September 1, 1994 Created by Art Lamb
 *
 *****************************************************************************)

unit clipboard;

interface

uses  palmos, coretraps;

const
  numClipboardForamts = 3;
  numClipboardFormats = numClipboardForamts;
  cbdMaxTextLength = 1000;

// Clipboard standard formats
type
  clipboardFormats = Enum;

const
  clipboardText = 0;
  clipboardInk = Succ(clipboardText);
  clipboardBitmap = Succ(clipboardInk);

type
  ClipboardFormatType = clipboardFormats;

type
  ClipboardItem = record
{$ifdef ALLOW_ACCESS_TO_INTERNALS_OF_CLIPBOARDS} // These fields will not be available in the next OS release!
    item: MemHandle;
    length: UInt16;
{$endif}
  end;
  ClipboardItemTag = ClipboardItem;

//----------------------------------------------------------
// Clipboard Functions
//----------------------------------------------------------

procedure ClipboardAddItem(const format: ClipboardFormatType; const ptr: Pointer; length: UInt16); syscall sysTrapClipboardAddItem;

function ClipboardAppendItem(const format: ClipboardFormatType; const ptr: Pointer; length: UInt16): Err; syscall sysTrapClipboardAppendItem;

function ClipboardGetItem(const format: ClipboardFormatType; var length: UInt16): MemHandle; syscall sysTrapClipboardGetItem;

implementation

end.
