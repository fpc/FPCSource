(******************************************************************************
 *
 * Copyright (c) 1994-2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: Window.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 *        This file defines window structures and routines that support color.
 *
 * History:
 *    January 20, 1999  Created by Bob Ebert
 *       Name  Date     Description
 *       ----  ----     -----------
 *       bob   1/20/99  Branch off WindowNew.h
 *       BS    4/20/99  Re-design of the screen driver
 *       bob   5/26/99  Cleanup/reorg
 *       jmp   12/23/99 Fix <> vs. "" problem.
 *
 *****************************************************************************)
{$MACRO ON}
unit window;

interface

uses palmos, coretraps, rect, errorbase, bitmap;

const
  kWinVersion = 3;

// enum for WinScrollRectangle
type
  WinDirectionType = Enum;

const
  winUp = 0;
  winDown = Succ(winUp);
  winLeft = Succ(winDown);
  winRight = Succ(winLeft);

// enum for WinCreateOffscreenWindow
type
  WindowFormatType = Enum;

const
  screenFormat = 0;
  genericFormat = Succ(screenFormat);

// enum for WinLockScreen
type
  WinLockInitType = Enum;

const
  winLockCopy = 0;
  winLockErase = Succ(winLockCopy);
  winLockDontCare = Succ(winLockErase);

// operations for the WinScreenMode function
type
  WinScreenModeOperation = Enum;

const
  winScreenModeGetDefaults = 0;
  winScreenModeGet = Succ(winScreenModeGetDefaults);
  winScreenModeSetToDefaults = Succ(winScreenModeGet);
  winScreenModeSet = Succ(winScreenModeSetToDefaults);
  winScreenModeGetSupportedDepths = Succ(winScreenModeSet);
  winScreenModeGetSupportsColor = Succ(winScreenModeGetSupportedDepths);

// Operations for the WinPalette function
const
  winPaletteGet = 0;
  winPaletteSet = 1;
  winPaletteSetToDefault = 2;
  winPaletteInit = 3; // for internal use only

// transfer modes for color drawing
type
  WinDrawOperation = Enum;

const
  winPaint = 0;
  winErase = Succ(winPaint);
  winMask = Succ(winErase);
  winInvert = Succ(winMask);
  winOverlay = Succ(winInvert);
  winPaintInverse = Succ(winOverlay);
  winSwap = Succ(winPaintInverse);

type
  PatternType = Enum;

const
  blackPattern = 0;
  whitePattern = Succ(blackPattern);
  grayPattern = Succ(whitePattern);
  customPattern = Succ(grayPattern);

const
  noPattern = blackPattern;
  grayHLinePattern = $AA;
  grayHLinePatternOdd = $55;

// grayUnderline means dotted current foreground color
// solidUnderline means solid current foreground color
// colorUnderline redundant, use solidUnderline instead
type
  UnderlineModeType = Enum;

const
  noUnderline = 0;
  grayUnderline = Succ(noUnderline);
  solidUnderline = Succ(grayUnderline);
  colorUnderline = Succ(solidUnderline);

const
  WinMaxSupportedDepth = 8;
  WinNumSupportedColors = 4;

type
  IndexedColorType = UInt8; // 1-, 2-, 4-, or 8-bit index
  CustomPatternType = array [0..7] of UInt8; // 8x8 1-bit deep pattern
  CustomPatternPtr = ^CustomPatternType;

// for WinPalette startIndex value, respect indexes in passed table
const
  WinUseTableIndexes = -1;

//-----------------------------------------------
// Draw state structures.
//-----------------------------------------------

type
  DrawStateType = record
  {$ifdef ALLOW_ACCESS_TO_INTERNALS_OF_WINDOWS} // These fields will not be available in the next OS release!
    transferMode: WinDrawOperation;
    pattern: PatternType;
    underlineMode: UnderlineModeType;
    fontId: FontID;
    font: FontPtr;
    patternData: CustomPatternType;

    // These are only valid for indexed color bitmaps

    foreColor: IndexedColorType;
    backColor: IndexedColorType;
    textColor: IndexedColorType;
    reserved: UInt8;

    // These are only valid for direct color bitmaps
    foreColorRGB: RGBColorType;
    backColorRGB: RGBColorType;
    textColorRGB: RGBColorType;
  {$endif}
  end;

const
  DrawStateStackSize = 5; // enough for a control in a field in a window

//-----------------------------------------------
// The Window Structures.
//-----------------------------------------------

type
  FrameBitsType = record
    case Integer of
      1: (bits: UInt16);
{
       (
          UInt16 cornerDiam  : 8;    // corner diameter, max 38
          UInt16 reserved_3  : 3;
          UInt16 threeD   : 1;    // Draw 3D button
          UInt16 shadowWidth : 2;    // Width of shadow
          UInt16 width   : 2;    // Width frame
       ) bits;
}
      2: (word: UInt16);         // IMPORTANT: INITIALIZE word to zero before setting bits!
  end;

  FrameType = UInt16;

//  Standard Frame Types
const
  noFrame        = 0;
  simpleFrame    = 1;
  rectangleFrame = 1;
  simple3DFrame  = $0012; // 3d, frame = 2
  roundFrame     = $0401; // corner = 7, frame = 1
  boldRoundFrame = $0702; // corner = 7, frame = 2
  popupFrame     = $0205; // corner = 2,  frame = 1, shadow = 1
  dialogFrame    = $0302; // corner = 3,  frame = 2
  menuFrame      = popupFrame;

  winDefaultDepthFlag = $FF;

type
  WindowFlagsType = record
  {$ifdef ALLOW_ACCESS_TO_INTERNALS_OF_WINDOWS} // These fields will not be available in the next OS release!
    Bits: UInt16;
{
 UInt16 format:1;      // window format:  0=screen mode; 1=generic mode
 UInt16 offscreen:1;   // offscreen flag: 0=onscreen ; 1=offscreen
 UInt16 modal:1;       // modal flag:     0=modeless window; 1=modal window
 UInt16 focusable:1;   // focusable flag: 0=non-focusable; 1=focusable
 UInt16 enabled:1;     // enabled flag:   0=disabled; 1=enabled
 UInt16 visible:1;     // visible flag:   0-invisible; 1=visible
 UInt16 dialog:1;      // dialog flag:    0=non-dialog; 1=dialog
 UInt16 freeBitmap:1;  // free bitmap w/window: 0=don't free, 1=free
 UInt16 reserved :8;
}
  {$endif}
  end;

  WindowType = record
  {$ifdef ALLOW_ACCESS_TO_INTERNALS_OF_WINDOWS} // These fields will not be available in the next OS release!
    displayWidthV20: Coord; // use WinGetDisplayExtent instead
    displayHeightV20: Coord; // use WinGetDisplayExtent instead
    displayAddrV20: Pointer; // use the drawing functions instead
    windowFlags: WindowFlagsType;
    windowBounds: RectangleType;
    clippingBounds: AbsRectType;
    bitmapP: BitmapPtr;
    frameType: FrameBitsType;
    drawStateP: ^DrawStateType; // was GraphicStatePtr
    nextWindow: ^WindowType;
  {$endif}
  end;

  WinPtr = ^WindowType;
  WinHandle = ^WindowType;

//-----------------------------------------------
//  More graphics shapes
//-----------------------------------------------

  WinLineType = record
    x1: Coord;
    y1: Coord;
    x2: Coord;
    y2: Coord;
  end;
  WinLinePtr = ^WinLineType;

// Rectangles, Points defined in Rect.h

//-----------------------------------------------
//  Low Memory Globals
//-----------------------------------------------

// This is the structure of a low memory global reserved for the Window Manager
// In GRAPHIC_VERSION_2, it held a single drawing state.  In this version, it
// holds stack information for structures that are allocated from the dynamic heap

  GraphicStateType = record
  {$ifdef ALLOW_ACCESS_TO_INTERNALS_OF_WINDOWS} // These fields will not be available in the next OS release!
    drawStateP: ^DrawStateType;
    drawStateStackP: ^DrawStateType;
    drawStateIndex: Int16;
    unused: UInt16; //was screenLockCount
  {$endif}
  end;

// ----------------------
// Window manager errors
// ----------------------

const
  winErrPalette = winErrorClass or 1;

//-----------------------------------------------
//  Macros
//-----------------------------------------------

// For now, the window handle is a pointer to a window structure,
// this however may change, so use the following macros.

function WinGetWindowPointer(winHandle: WinHandle): WinPtr;

function WinGetWindowHandle(winPtr: WinPtr): WinHandle;

//-----------------------------------------------
// Routines relating to windows management
//-----------------------------------------------

function WinValidateHandle(winHandle: WinHandle): Boolean; syscall sysTrapWinValidateHandle;

function WinCreateWindow({const} var bounds: RectangleType; frame: FrameType; modal, focusable: Boolean;
                         var error: UInt16): WinHandle; syscall sysTrapWinCreateWindow;

function WinCreateOffscreenWindow(width, height: Coord; format: WindowFormatType; var error: UInt16): WinHandle; syscall sysTrapWinCreateOffscreenWindow;

function WinCreateBitmapWindow(bitmapP: BitmapPtr; var error: UInt16): WinHandle; syscall sysTrapWinCreateBitmapWindow;

procedure WinDeleteWindow(winHandle: WinHandle; eraseIt: Boolean); syscall sysTrapWinDeleteWindow;

procedure WinInitializeWindow(winHandle: WinHandle); syscall sysTrapWinInitializeWindow;

procedure WinAddWindow(winHandle: WinHandle); syscall sysTrapWinAddWindow;

procedure WinRemoveWindow(winHandle: WinHandle); syscall sysTrapWinRemoveWindow;

procedure WinMoveWindowAddr(oldLocationP, newLocationP: WinPtr); syscall sysTrapWinMoveWindowAddr;

procedure WinSetActiveWindow(winHandle: WinHandle); syscall sysTrapWinSetActiveWindow;

function WinSetDrawWindow(winHandle: WinHandle): WinHandle; syscall sysTrapWinSetDrawWindow;

function WinGetDrawWindow: WinHandle; syscall sysTrapWinGetDrawWindow;

function WinGetActiveWindow: WinHandle; syscall sysTrapWinGetActiveWindow;

function WinGetDisplayWindow: WinHandle; syscall sysTrapWinGetDisplayWindow;

function WinGetFirstWindow: WinHandle; syscall sysTrapWinGetFirstWindow;

procedure WinEnableWindow(winHandle: WinHandle); syscall sysTrapWinEnableWindow;

procedure WinDisableWindow(winHandle: WinHandle); syscall sysTrapWinDisableWindow;

procedure WinGetWindowFrameRect(winHandle: WinHandle; var r: RectangleType); syscall sysTrapWinGetWindowFrameRect;

procedure WinDrawWindowFrame; syscall sysTrapWinDrawWindowFrame;

procedure WinEraseWindow; syscall sysTrapWinEraseWindow;

function WinSaveBits({const} var source: RectangleType; var error: UInt16): WinHandle; syscall sysTrapWinSaveBits;

procedure WinRestoreBits(winHandle: WinHandle; destX, destY: Coord); syscall sysTrapWinRestoreBits;

procedure WinCopyRectangle(srcWin, dstWin: WinHandle; {const} var srcRect: RectangleType;
                           destX, destY: Coord; mode: WinDrawOperation); syscall sysTrapWinCopyRectangle;

procedure WinScrollRectangle({const} var  rP: RectangleType; direction: WinDirectionType;
                             distance: Coord; var vacatedP: RectangleType); syscall sysTrapWinScrollRectangle;

procedure WinGetDisplayExtent(var extentX, extentY: Coord); syscall sysTrapWinGetDisplayExtent;

procedure WinGetDrawWindowBounds(var rP: RectangleType); syscall sysTrapWinGetDrawWindowBounds;

procedure WinGetBounds(winH: WinHandle; var rP: RectangleType); syscall sysTrapWinGetBounds;

procedure WinSetBounds(winHandle: WinHandle; {const} var rP: RectangleType); syscall sysTrapWinSetBounds;

{$ifdef ALLOW_OLD_API_NAMES}
procedure WinGetWindowBounds(var rP: RectangleType); syscall sysTrapWinGetWindowBounds;

procedure WinSetWindowBounds(winHandle: WinHandle; {const} var rP: RectangleType); syscall WinSetWindowBounds;
{$endif}

procedure WinGetWindowExtent(var extentX, extentY: Coord); syscall sysTrapWinGetWindowExtent;

procedure WinDisplayToWindowPt(var extentX, extentY: Coord); syscall sysTrapWinDisplayToWindowPt;

procedure WinWindowToDisplayPt(var extentX, extentY: Coord); syscall sysTrapWinWindowToDisplayPt;

function WinGetBitmap(winHandle: WinHandle): BitmapPtr; syscall sysTrapWinGetBitmap;

procedure WinGetClip(var rP: RectangleType); syscall sysTrapWinGetClip;

procedure WinSetClip({const} var rP: RectangleType); syscall sysTrapWinSetClip;

procedure WinResetClip; syscall sysTrapWinResetClip;

procedure WinClipRectangle(var rP: RectangleType); syscall sysTrapWinClipRectangle;

function WinModal(winHandle: WinHandle): Boolean; syscall sysTrapWinModal;

//-----------------------------------------------
// Routines to draw shapes or frames shapes
//-----------------------------------------------

// Pixel(s)
function WinGetPixel(x, y: Coord): IndexedColorType; syscall sysTrapWinGetPixel;

procedure WinPaintPixel(x, y: Coord); syscall sysTrapWinPaintPixel; // uses drawing mode

function WinGetPixelRGB (x, y: Coord; var rgbP: RGBColorType): Err; syscall sysTrapWinGetPixelRGB; // Direct color version

procedure WinDrawPixel(x, y: Coord); syscall sysTrapWinDrawPixel;

procedure WinErasePixel(x, y: Coord); syscall sysTrapWinErasePixel;

procedure WinInvertPixel(x, y: Coord); syscall sysTrapWinInvertPixel;

procedure WinPaintPixels(numPoints: UInt16; pts: PointPtr); syscall sysTrapWinPaintPixels;

// Line(s)
procedure WinPaintLines(numLines: UInt16; lines: WinLinePtr); syscall sysTrapWinPaintLines;

procedure WinPaintLine(x1, y1, x2, y2: Coord); syscall sysTrapWinPaintLine;

procedure WinDrawLine(x1, y1, x2, y2: Coord); syscall sysTrapWinDrawLine;

procedure WinDrawGrayLine(x1, y1, x2, y2: Coord); syscall sysTrapWinDrawGrayLine;

procedure WinEraseLine(x1, y1, x2, y2: Coord); syscall sysTrapWinEraseLine;

procedure WinInvertLine(x1, y1, x2, y2: Coord); syscall sysTrapWinInvertLine;

procedure WinFillLine(x1, y1, x2, y2: Coord); syscall sysTrapWinFillLine;

// Rectangle
procedure WinPaintRectangle({const} var rP: RectangleType; cornerDiam: UInt16); syscall sysTrapWinPaintRectangle;

procedure WinDrawRectangle({const} var rP: RectangleType; cornerDiam: UInt16); syscall sysTrapWinDrawRectangle;

procedure WinEraseRectangle({const} var rP: RectangleType; cornerDiam: UInt16); syscall sysTrapWinEraseRectangle;

procedure WinInvertRectangle({const} var rP: RectangleType; cornerDiam: UInt16); syscall sysTrapWinInvertRectangle;

procedure WinFillRectangle({const} var rP: RectangleType; cornerDiam: UInt16); syscall sysTrapWinFillRectangle;

// Rectangle frames
procedure WinPaintRectangleFrame(frame: FrameType; {const} var rP: RectangleType); syscall sysTrapWinPaintRectangleFrame;

procedure WinDrawRectangleFrame(frame: FrameType; {const} var rP: RectangleType); syscall sysTrapWinDrawRectangleFrame;

procedure WinDrawGrayRectangleFrame(frame: FrameType; {const} var rP: RectangleType); syscall sysTrapWinDrawGrayRectangleFrame;

procedure WinEraseRectangleFrame(frame: FrameType; {const} var rP: RectangleType); syscall sysTrapWinEraseRectangleFrame;

procedure WinInvertRectangleFrame(frame: FrameType; {const} var rP: RectangleType); syscall sysTrapWinInvertRectangleFrame;

procedure WinGetFramesRectangle(frame: FrameType; {const} var rP, obscuredRect: RectangleType); syscall sysTrapWinGetFramesRectangle;

// Bitmap
procedure WinDrawBitmap(bitmapP: BitmapPtr; x, y: Coord); syscall sysTrapWinDrawBitmap;

procedure WinPaintBitmap(bitmapP: BitmapPtr; x, y: Coord); syscall sysTrapWinPaintBitmap;

// Characters
procedure WinDrawChar(theChar: WChar; x, y: Coord); syscall sysTrapWinDrawChar;

procedure WinDrawChars(const chars: PChar; len: Int16; x, y: Coord); syscall sysTrapWinDrawChars;

procedure WinPaintChar(theChar: WChar; x, y: Coord); syscall sysTrapWinPaintChar;

procedure WinPaintChars(const chars: PChar; len: Int16; x, y: Coord); syscall sysTrapWinPaintChars;

procedure WinDrawInvertedChars(const chars: PChar; len: Int16; x, y: Coord); syscall sysTrapWinDrawInvertedChars;

procedure WinDrawTruncChars(const chars: PChar; len: Int16; x, y, maxWidth: Coord); syscall sysTrapWinDrawTruncChars;

procedure WinEraseChars(const chars: PChar; len: Int16; x, y: Coord); syscall sysTrapWinEraseChars;

procedure WinInvertChars(const chars: PChar; len: Int16; x, y: Coord); syscall sysTrapWinInvertChars;

function WinSetUnderlineMode(mode: UnderlineModeType): UnderlineModeType; syscall sysTrapWinSetUnderlineMode;

//-----------------------------------------------
// Routines for patterns and colors
//-----------------------------------------------

procedure WinPushDrawState; syscall sysTrapWinPushDrawState; // "save" fore, back, text color, pattern, underline mode, font

procedure WinPopDrawState; syscall sysTrapWinPopDrawState; // "restore" saved drawing variables

function WinSetDrawMode(newMode: WinDrawOperation): WinDrawOperation; syscall sysTrapWinSetDrawMode;

function WinSetForeColor(foreColor: IndexedColorType): IndexedColorType; syscall sysTrapWinSetForeColor;

function WinSetBackColor(backColor: IndexedColorType): IndexedColorType; syscall sysTrapWinSetBackColor;

function WinSetTextColor(textColor: IndexedColorType): IndexedColorType; syscall sysTrapWinSetTextColor;

// Direct color versions
procedure WinSetForeColorRGB(const newRgbP: RGBColorPtr; prevRgbP: RGBColorPtr); syscall sysTrapWinSetForeColorRGB;

procedure WinSetBackColorRGB(const newRgbP: RGBColorPtr; prevRgbP: RGBColorPtr); syscall sysTrapWinSetBackColorRGB;

procedure WinSetTextColorRGB(const newRgbP: RGBColorPtr; prevRgbP: RGBColorPtr); syscall sysTrapWinSetTextColorRGB;

procedure WinGetPattern(patternP: CustomPatternPtr); syscall sysTrapWinGetPattern;

function WinGetPatternType: PatternType; syscall sysTrapWinGetPatternType;

procedure WinSetPattern(const patternP: CustomPatternPtr); syscall sysTrapWinSetPattern;

procedure WinSetPatternType(newPattern: PatternType); syscall sysTrapWinSetPatternType;

function WinPalette(operation: UInt8; startIndex: Int16; paletteEntries: UInt16; tableP: RGBColorPtr): Err; syscall sysTrapWinPalette;

function WinRGBToIndex(const rgbP: RGBColorPtr): IndexedColorType; syscall sysTrapWinRGBToIndex;

procedure WinIndexToRGB(i: IndexedColorType; rgbP: RGBColorPtr); syscall sysTrapWinIndexToRGB;

// "obsolete" color call, supported for backwards compatibility

procedure WinSetColors(const newForeColorP: RGBColorPtr; oldForeColorP: RGBColorPtr;
                       const newBackColorP: RGBColorPtr; oldBackColorP: RGBColorPtr); syscall sysTrapWinSetColors;


//-----------------------------------------------
// WinScreen functions
//-----------------------------------------------

procedure WinScreenInit; syscall sysTrapWinScreenInit;

function WinScreenMode(operation: WinScreenModeOperation; var widthP, heightP, depthP: UInt32;
                       var enableColorP: Boolean): Err; syscall sysTrapWinScreenMode;

//-----------------------------------------------
// Screen tracking (double buffering) support
//-----------------------------------------------

function WinScreenLock(initMode: WinLockInitType): UInt8Ptr; syscall sysTrapWinScreenLock;

procedure WinScreenUnlock; syscall sysTrapWinScreenUnlock;

implementation

function WinGetWindowPointer(winHandle: WinHandle): WinPtr;
begin
  WinGetWindowPointer := winHandle;
end;

function WinGetWindowHandle(winPtr: WinPtr): WinHandle;
begin
  WinGetWindowHandle := winPtr
end;

end.
