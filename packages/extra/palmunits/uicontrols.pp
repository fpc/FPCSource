{$MACRO ON}
(******************************************************************************
 *
 * Copyright (c) 1998-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: UIControls.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *                Contrast & brightness control for devices with
 *                software contrast.
 *
 * History:
 *       Name  Date     Description
 *       ----  ----     -----------
 *       bob   02/12/98 Initial version
 *       bob   03/15/99 Added brightness
 *       bob   08/27/99 Added UIPickColor, renamed UIControls.h
 *
 *****************************************************************************)

unit uicontrols;

interface

uses palmos, coretraps, bitmap, window;

// for UIPickColor
const
  UIPickColorStartPalette = 0;
  UIPickColorStartRGB = 1;

type
  UIPickColorStartType = UInt16;

procedure UIContrastAdjust; syscall sysTrapUIContrastAdjust;

procedure UIBrightnessAdjust; syscall sysTrapUIBrightnessAdjust;

function UIPickColor(var indexP: IndexedColorType; var rgbP: RGBColorType;
                    start: UIPickColorStartType; const titleP, tipP: PChar): Boolean; syscall sysTrapUIPickColor;

implementation

end.
