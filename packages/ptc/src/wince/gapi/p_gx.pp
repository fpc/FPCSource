unit p_gx;

{$MODE objfpc}

{ convention is cdecl for WinCE API}
{$calling cdecl}

interface

uses
  Windows;

const
  GXDLL = 'gx';

type
  GXDisplayProperties = record
    cxWidth: DWord;
    cyHeight: DWord;            // notice lack of 'th' in the word height.
    cbxPitch: LONG;             // number of bytes to move right one x pixel - can be negative.
    cbyPitch: LONG;             // number of bytes to move down one y pixel - can be negative.
    cBPP: LONG;                 // # of bits in each pixel
    ffFormat: DWord;            // format flags.
  end;

  GXKeyList = record
    vkUp: SHORT;             // key for up
    ptUp: POINT;             // x,y position of key/button.  not on screen but in screen coordinates.
    vkDown: SHORT;
    ptDown: POINT;
    vkLeft: SHORT;
    ptLeft: POINT;
    vkRight: SHORT;
    ptRight: POINT;
    vkA: SHORT;
    ptA: POINT;
    vkB: SHORT;
    ptB: POINT;
    vkC: SHORT;
    ptC: POINT;
    vkStart: SHORT;
    ptStart: POINT;
  end;

function GXOpenDisplay(AhWnd: HWND; dwFlags: DWORD): Integer; external GXDLL Name '?GXOpenDisplay@@YAHPAUHWND__@@K@Z';
function GXCloseDisplay: Integer; external GXDLL Name '?GXCloseDisplay@@YAHXZ';
function GXBeginDraw: Pointer; external GXDLL Name '?GXBeginDraw@@YAPAXXZ';
function GXEndDraw: Integer; external GXDLL Name '?GXEndDraw@@YAHXZ';
function GXOpenInput: Integer; external GXDLL Name '?GXOpenInput@@YAHXZ';
function GXCloseInput: Integer; external GXDLL Name '?GXCloseInput@@YAHXZ';
function GXGetDisplayProperties: GXDisplayProperties; external GXDLL Name '?GXGetDisplayProperties@@YA?AUGXDisplayProperties@@XZ';
function GXGetDefaultKeys(iOptions: Integer): GXKeyList; external GXDLL Name '?GXGetDefaultKeys@@YA?AUGXKeyList@@H@Z';
function GXSuspend: Integer; external GXDLL Name '?GXSuspend@@YAHXZ';
function GXResume: Integer; external GXDLL Name '?GXResume@@YAHXZ';
function GXSetViewport(dwTop, dwHeight, dwReserved1, dwReserved2: DWORD): Integer; external GXDLL Name '?GXSetViewport@@YAHKKKK@Z';
function GXIsDisplayDRAMBuffer: BOOL; external GXDLL Name '?GXIsDisplayDRAMBuffer@@YAHXZ';


// Although these flags can be unrelated they still
// have unique values.

const
  GX_FULLSCREEN    = $01;        // for OpenDisplay()
  GX_NORMALKEYS    = $02;
  GX_LANDSCAPEKEYS = $03;

  kfLandscape      = $8;        // Screen is rotated 270 degrees
  kfPalette        = $10;       // Pixel values are indexes into a palette
  kfDirect         = $20;       // Pixel values contain actual level information
  kfDirect555      = $40;       // 5 bits each for red, green and blue values in a pixel.
  kfDirect565      = $80;       // 5 red bits, 6 green bits and 5 blue bits per pixel
  kfDirect888      = $100;      // 8 bits each for red, green and blue values in a pixel.
  kfDirect444      = $200;      // 4 red, 4 green, 4 blue
  kfDirectInverted = $400;

  GETRAWFRAMEBUFFER = $00020001;

type
  RawFrameBufferInfo = record
    wFormat: WORD;
    wBPP: WORD;
    pFramePointer: Pointer;
    cxStride: Integer;
    cyStride: Integer;
    cxPixels: Integer;
    cyPixels: Integer;
  end;

const
  FORMAT_565   = 1;
  FORMAT_555   = 2;
  FORMAT_OTHER = 3;

implementation

end.
