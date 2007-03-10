Unit gx;

{$MODE objfpc}

{ convention is cdecl for WinCE API}
{$calling cdecl}

Interface

Uses
  Windows;

Const
  GXDLL = 'gx.dll';

Type
  GXDisplayProperties = Record
    cxWidth : DWord;
    cyHeight : DWord;            // notice lack of 'th' in the word height.
    cbxPitch : LONG;             // number of bytes to move right one x pixel - can be negative.
    cbyPitch : LONG;             // number of bytes to move down one y pixel - can be negative.
    cBPP : LONG;                 // # of bits in each pixel
    ffFormat : DWord;            // format flags.
  End;

  GXKeyList = Record
    vkUp : SHORT;             // key for up
    ptUp : POINT;             // x,y position of key/button.  Not on screen but in screen coordinates.
    vkDown : SHORT;
    ptDown : POINT;
    vkLeft : SHORT;
    ptLeft : POINT;
    vkRight : SHORT;
    ptRight : POINT;
    vkA : SHORT;
    ptA : POINT;
    vkB : SHORT;
    ptB : POINT;
    vkC : SHORT;
    ptC : POINT;
    vkStart : SHORT;
    ptStart : POINT;
  End;

Function GXOpenDisplay(AhWnd : HWND; dwFlags : DWORD) : Integer; External GXDLL Name '?GXOpenDisplay@@YAHPAUHWND__@@K@Z';
Function GXCloseDisplay : Integer; External GXDLL Name '?GXCloseDisplay@@YAHXZ';
Function GXBeginDraw : Pointer; External GXDLL Name '?GXBeginDraw@@YAPAXXZ';
Function GXEndDraw : Integer; External GXDLL Name '?GXEndDraw@@YAHXZ';
Function GXOpenInput : Integer; External GXDLL Name '?GXOpenInput@@YAHXZ';
Function GXCloseInput : Integer; External GXDLL Name '?GXCloseInput@@YAHXZ';
Function GXGetDisplayProperties : GXDisplayProperties; External GXDLL Name '?GXGetDisplayProperties@@YA?AUGXDisplayProperties@@XZ';
Function GXGetDefaultKeys(iOptions : Integer) : GXKeyList; External GXDLL Name '?GXGetDefaultKeys@@YA?AUGXKeyList@@H@Z';
Function GXSuspend : Integer; External GXDLL Name '?GXSuspend@@YAHXZ';
Function GXResume : Integer; External GXDLL Name '?GXResume@@YAHXZ';
Function GXSetViewport(dwTop, dwHeight, dwReserved1, dwReserved2 : DWORD) : Integer; External GXDLL Name '?GXSetViewport@@YAHKKKK@Z';
Function GXIsDisplayDRAMBuffer : BOOL; External GXDLL Name '?GXIsDisplayDRAMBuffer@@YAHXZ';


// Although these flags can be unrelated they still
// have unique values.

Const
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

Type
  RawFrameBufferInfo = Record
    wFormat : WORD;
    wBPP : WORD;
    pFramePointer : Pointer;
    cxStride : Integer;
    cyStride : Integer;
    cxPixels : Integer;
    cyPixels : Integer;
  End;

Const
  FORMAT_565   = 1;
  FORMAT_555   = 2;
  FORMAT_OTHER = 3;

Implementation

End.
