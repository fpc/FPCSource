(******************************************************************************
 *
 * Copyright (c) 1994-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: Control.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *   This file defines check box structures and routines.
 *
 * History:
 *    August 29, 1994   Created by Art Lamb
 *       Name  Date     Description
 *       ----  ----     -----------
 *       bob   2/9/99   Fix up const stuff
 *       bob   4/16/99  add GraphicControlType
 *
 *****************************************************************************)
{$MACRO ON}

unit control;

interface

uses palmos, coretraps, rect, datamgr, font;

type
  ControlAttrType = record
{$ifdef ALLOW_ACCESS_TO_INTERNALS_OF_CONTROLS} // These fields will not be available in the next OS release!
    Bits: UInt16;
{
    UInt8 usable                :1; // set if part of ui
    UInt8 enabled               :1; // set if interactable (not grayed out)
    UInt8 visible               :1; // set if drawn (set internally)
    UInt8 on                    :1; // set if on (checked)
    UInt8 leftAnchor            :1; // set if bounds expand to the right
                                    // clear if bounds expand to the left
    UInt8 frame                 :3;
    UInt8 drawnAsSelected       :1; // support for old-style graphic controls
                                    // where control overlaps a bitmap
    UInt8 graphical             :1; // set if images are used instead of text
    UInt8 vertical              :1; // true for vertical sliders
    UInt8 reserved              :5;
}
{$endif}
  end;
  ControlAttrTag = ControlAttrType;

type
  controlStyles = Enum;

const
  buttonCtl = 0;
  pushButtonCtl = Succ(buttonCtl);
  checkboxCtl = Succ(pushButtonCtl);
  popupTriggerCtl = Succ(checkboxCtl);
  selectorTriggerCtl = Succ(popupTriggerCtl);
  repeatingButtonCtl = Succ(selectorTriggerCtl);
  sliderCtl = Succ(repeatingButtonCtl);
  feedbackSliderCtl = Succ(sliderCtl);

type
  ControlStyleType = controlStyles;

type
  buttonFrames = Enum;

const
  noButtonFrame = 0;
  standardButtonFrame = Succ(noButtonFrame);
  boldButtonFrame = Succ(standardButtonFrame);
  rectangleButtonFrame = Succ(boldButtonFrame);

type
  ButtonFrameType = buttonFrames;

type
  ControlType = record
{$ifdef ALLOW_ACCESS_TO_INTERNALS_OF_CONTROLS} // These fields will not be available in the next OS release!
    id: UInt16;
    bounds: RectangleType;
    text: PChar;
    attr: ControlAttrType;
    style: ControlStyleType;
    font: FontID;
    group: UInt8;
    reserved: UInt8;
{$endif}
  end;

  ControlPtr = ^ControlType; // deprecated, use ControlType *

// GraphicControlType *'s can be cast to ControlType *'s and passed to all
// Control API functions (as long as the 'graphical' bit in the attrs is set)

  GraphicControlType = record
{$ifdef ALLOW_ACCESS_TO_INTERNALS_OF_CONTROLS} // These fields will not be available in the next OS release!
    id: UInt16;
    bounds: RectangleType;
    bitmapID: DmResID;         // overlays text in ControlType
    selectedBitmapID: DmResID; // overlays text in ControlType
    attr: ControlAttrType;
    style: ControlStyleType;
    unused: FontID;
    group: UInt8;
    reserved: UInt8;
{$endif}
  end;
  GraphicControlPtr = ^GraphicControlType;

// SliderControlType *'s can be cast to ControlType *'s and passed to all
// Control API functions (as long as the control style is a slider)

  SliderControlType = record
{$ifdef ALLOW_ACCESS_TO_INTERNALS_OF_CONTROLS} // These fields will not be available in the next OS release!
    id: UInt16;
    bounds: RectangleType;
    thumbID: DmResID;        // overlays text in ControlType
    backgroundID: DmResID;   // overlays text in ControlType
    attr: ControlAttrType;   // graphical *is* set
    style: ControlStyleType; // must be sliderCtl or repeatingSliderCtl
    reserved: UInt8;
    minValue: Int16;
    maxValue: Int16;
    pageSize: Int16;
    value: Int16;
    activeSliderP: MemPtr;
{$endif}
  end;
  SliderControlPtr = ^SliderControlType;

//----------------------------------------------------------
//  Control Functions
//----------------------------------------------------------

procedure CtlDrawControl(controlP: ControlPtr); syscall sysTrapCtlDrawControl;

procedure CtlEraseControl(controlP: ControlPtr); syscall sysTrapCtlEraseControl;

procedure CtlHideControl(controlP: ControlPtr); syscall sysTrapCtlHideControl;

procedure CtlShowControl(controlP: ControlPtr); syscall sysTrapCtlShowControl;

function CtlEnabled(const controlP: ControlPtr): Boolean; syscall sysTrapCtlEnabled;

procedure CtlSetEnabled(controlP: ControlPtr; usable: Boolean); syscall sysTrapCtlSetEnabled;

procedure CtlSetUsable(controlP: ControlPtr; usable: Boolean); syscall sysTrapCtlSetUsable;

function CtlGetValue(const controlP: ControlPtr): Int16; syscall sysTrapCtlGetValue;

procedure CtlSetValue(controlP: ControlPtr; newValue: Int16); syscall sysTrapCtlSetValue;

function CtlGetLabel(const controlP: ControlPtr): PChar; syscall sysTrapCtlGetLabel;

procedure CtlSetLabel(controlP: ControlPtr; const newLabel: PChar); syscall sysTrapCtlSetLabel;

procedure CtlSetGraphics(ctlP: ControlPtr; newBitmapID, newSelectedBitmapID: DmResID); syscall sysTrapCtlSetGraphics;

procedure CtlSetSliderValues(ctlP: ControlPtr; {const} var minValueP, maxValueP, pageSizeP, valueP: UInt16); syscall sysTrapCtlSetSliderValues;

procedure CtlGetSliderValues(const ctlP: ControlPtr; var minValueP, maxValueP, pageSizeP, valueP: UInt16); syscall sysTrapCtlGetSliderValues;

procedure CtlHitControl(const controlP: ControlPtr); syscall sysTrapCtlHitControl;

type
  EventPtr = Pointer;

function CtlHandleEvent(controlP: ControlPtr; pEvent: EventPtr): Boolean; syscall sysTrapCtlHandleEvent;

function CtlValidatePointer(const controlP: ControlPtr): Boolean; syscall sysTrapCtlValidatePointer;

function CtlNewControl(formPP: PointerPtr; ID: UInt16; style: ControlStyleType; const textP: PChar;
                       x, y, width, height: Coord; font: FontID; group: UInt8; leftAnchor: Boolean): ControlPtr; syscall sysTrapCtlNewControl;

function CtlNewGraphicControl(formPP: PointerPtr; ID: UInt16; style: ControlStyleType; bitmapID, selectedBitmapID: DmResID;
                              x, y, width, height: Coord; group: UInt8; leftAnchor: Boolean): GraphicControlPtr; syscall sysTrapCtlNewGraphicControl;

function CtlNewSliderControl(formPP: PointerPtr; ID: UInt16; style: ControlStyleType; thumbID, backgroundID: DmResID;
                             x, y, width, height: Coord; minValue, maxValue, pageSize, value: UInt16): SliderControlPtr; syscall sysTrapCtlNewSliderControl;

implementation

end.
