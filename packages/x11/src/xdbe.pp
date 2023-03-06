(******************************************************************************
 *
 * Copyright (c) 1994, 1995  Hewlett-Packard Company
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL HEWLETT-PACKARD COMPANY BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
 * OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR
 * THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 * Except as contained in this notice, the name of the Hewlett-Packard
 * Company shall not be used in advertising or otherwise to promote the
 * sale, use or other dealings in this Software without prior written
 * authorization from the Hewlett-Packard Company.
 *
 *     Header file for Xlib-related DBE
 *
 *****************************************************************************)

{$IFNDEF FPC_DOTTEDUNITS}
unit xdbe;
{$ENDIF FPC_DOTTEDUNITS}

{$PACKRECORDS c}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.CTypes, Api.X11.X, Api.X11.Xlib;
{$ELSE FPC_DOTTEDUNITS}
uses
  ctypes, x, xlib;
{$ENDIF FPC_DOTTEDUNITS}

const
  libXext = 'Xext';

{$I dbe.inc}

type
  PXdbeVisualInfo = ^TXdbeVisualInfo;
  TXdbeVisualInfo = record
    visual: TVisualID;  { one visual ID that supports double-buffering }
    depth: cint;        { depth of visual in bits                      }
    perflevel: cint;    { performance level of visual                  }
  end;

  PXdbeScreenVisualInfo = ^TXdbeScreenVisualInfo;
  TXdbeScreenVisualInfo = record
    count: cint;               { number of items in visual_depth   }
    visinfo: PXdbeVisualInfo;  { list of visuals & depths for scrn }
  end;

  PXdbeBackBuffer = ^TXdbeBackBuffer;
  TXdbeBackBuffer = TDrawable;

  PXdbeSwapAction = ^TXdbeSwapAction;
  TXdbeSwapAction = cuchar;

  PXdbeSwapInfo = ^TXdbeSwapInfo;
  TXdbeSwapInfo = record
    swap_window: TWindow;          { window for which to swap buffers   }
    swap_action: TXdbeSwapAction;  { swap action to use for swap_window }
  end;

  PXdbeBackBufferAttributes = ^TXdbeBackBufferAttributes;
  TXdbeBackBufferAttributes = record
    window: TWindow;  { window that buffer belongs to }
  end;

  PXdbeBufferError = ^TXdbeBufferError;
  TXdbeBufferError = record
    _type: cint;
    display: PDisplay;        { display the event was read from }
    buffer: TXdbeBackBuffer;  { resource id                     }
    serial: culong;           { serial number of failed request }
    error_code: cuchar;       { error base + XdbeBadBuffer      }
    request_code: cuchar;     { major opcode of failed request  }
    minor_code: cuchar;       { minor opcode of failed request  }
  end;

function XdbeQueryExtension(
    dpy: PDisplay;
    major_version_return,
    minor_version_return: Pcint
): TStatus; cdecl; external libXext;

function XdbeAllocateBackBufferName(
    dpy: PDisplay;
    window: TWindow;
    swap_action: TXdbeSwapAction
): TXdbeBackBuffer; cdecl; external libXext;

function XdbeDeallocateBackBufferName(
    dpy: PDisplay;
    buffer: TXdbeBackBuffer
): TStatus; cdecl; external libXext;

function XdbeSwapBuffers(
    dpy: PDisplay;
    swap_info: PXdbeSwapInfo;
    num_windows: cint
): TStatus; cdecl; external libXext;

function XdbeBeginIdiom(
    dpy: PDisplay
): TStatus; cdecl; external libXext;

function XdbeEndIdiom(
    dpy: PDisplay
): TStatus; cdecl; external libXext;

function XdbeGetVisualInfo(
    dpy: PDisplay;
    screen_specifiers: PDrawable;
    num_screens: Pcint
): PXdbeScreenVisualInfo; cdecl; external libXext;

procedure XdbeFreeVisualInfo(
    visual_info: PXdbeScreenVisualInfo
); cdecl; external libXext;

function XdbeGetBackBufferAttributes(
    dpy: PDisplay;
    buffer: TXdbeBackBuffer
): PXdbeBackBufferAttributes; cdecl; external libXext;

implementation
end.
