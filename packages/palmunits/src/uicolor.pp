{$MACRO ON}
(******************************************************************************
 *
 * Copyright (c) 1999-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: UIColor.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *    This file defines structs and functions for setting the "system
 *    colors" that the UI routines use.
 *
 * History:
 *    January 20, 1999  Created by Bob Ebert
 *    08/21/99 kwk   Added UIFieldFepRawText...UIFieldFepConvertedBackground
 *                   to the UIColorTableEntries enum.
 *    10/09/99 kwk   Added UIFieldFepUnderline to UIColorTableEntries enum.
 *
 *****************************************************************************)

unit uicolor;

interface

uses palmos, coretraps, bitmap, window;

type
  UIColorTableEntries = Enum;

const
  UIObjectFrame = 0;
  UIObjectFill = Succ(UIObjectFrame);
  UIObjectForeground = Succ(UIObjectFill);
  UIObjectSelectedFill = Succ(UIObjectForeground);
  UIObjectSelectedForeground = Succ(UIObjectSelectedFill);

  UIMenuFrame = Succ(UIObjectSelectedForeground);
  UIMenuFill = Succ(UIMenuFrame);
  UIMenuForeground = Succ(UIMenuFill);
  UIMenuSelectedFill = Succ(UIMenuForeground);
  UIMenuSelectedForeground = Succ(UIMenuSelectedFill);

  UIFieldBackground = Succ(UIMenuSelectedForeground);
  UIFieldText = Succ(UIFieldBackground);
  UIFieldTextLines = Succ(UIFieldText);
  UIFieldCaret = Succ(UIFieldTextLines);
  UIFieldTextHighlightBackground = Succ(UIFieldCaret);
  UIFieldTextHighlightForeground = Succ(UIFieldTextHighlightBackground);
  UIFieldFepRawText = Succ(UIFieldTextHighlightForeground);
  UIFieldFepRawBackground = Succ(UIFieldFepRawText);
  UIFieldFepConvertedText = Succ(UIFieldFepRawBackground);
  UIFieldFepConvertedBackground = Succ(UIFieldFepConvertedText);
  UIFieldFepUnderline = Succ(UIFieldFepConvertedBackground);

  UIFormFrame = Succ(UIFieldFepUnderline);
  UIFormFill = Succ(UIFormFrame);

  UIDialogFrame = Succ(UIFormFill);
  UIDialogFill = Succ(UIDialogFrame);

  UIAlertFrame = Succ(UIDialogFill);
  UIAlertFill = Succ(UIAlertFrame);

  UIOK = Succ(UIAlertFill);
  UICaution = Succ(UIOK);
  UIWarning = Succ(UICaution);

  UILastColorTableEntry = Succ(UIWarning);

//------------------------------------------------------------
// UI Color Table Manipulation Routines
//------------------------------------------------------------

function UIColorGetTableEntryIndex(which: UIColorTableEntries): IndexedColorType; syscall sysTrapUIColorGetTableEntryIndex;

procedure UIColorGetTableEntryRGB(which: UIColorTableEntries; var rgbP: RGBColorType); syscall sysTrapUIColorGetTableEntryRGB;

function UIColorSetTableEntry(which: UIColorTableEntries; {const} var rgbP: RGBColorType): Err; syscall sysTrapUIColorSetTableEntry;

function UIColorPushTable: Err; syscall sysTrapUIColorPushTable;

function UIColorPopTable: Err; syscall sysTrapUIColorPopTable;

implementation

end.
