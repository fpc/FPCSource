(*
Copyright 1996, 1998  The Open Group

Permission to use, copy, modify, distribute, and sell this software and its
documentation for any purpose is hereby granted without fee, provided that
the above copyright notice appear in all copies and that both that
copyright notice and this permission notice appear in supporting
documentation.

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE OPEN GROUP BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of The Open Group shall
not be used in advertising or otherwise to promote the sale, use or
other dealings in this Software without prior written authorization
from The Open Group.
*)

{$IFNDEF FPC_DOTTEDUNITS}
unit xag;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$MODE objfpc}

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.CTypes, Api.X11.X, Api.X11.Xlib;
{$ELSE FPC_DOTTEDUNITS}
uses
  ctypes, x, xlib;
{$ENDIF FPC_DOTTEDUNITS}

const
  libXext = 'Xext';

{$I ag.inc}

//#include <stdarg.h>

type
  PXAppGroup = ^TXAppGroup;
  TXAppGroup = TXID;

function XagQueryVersion(
    dpy: PDisplay;
    major_version,
    minor_version: Pcint
): TBoolResult; cdecl; external libXext;

function XagCreateEmbeddedApplicationGroup(
    dpy: PDisplay;
    root_visual: TVisualID;
    default_colormap: TColormap;
    black_pixel,
    white_pixel: culong;
    app_group_return: PXAppGroup
): TStatus; cdecl; external libXext;

function XagCreateNonembeddedApplicationGroup(
    dpy: PDisplay;
    app_group_return: PXAppGroup
): TStatus; cdecl; external libXext;

function XagDestroyApplicationGroup(
    dpy: PDisplay;
    app_group: TXAppGroup
): TStatus; cdecl; external libXext;

function XagGetApplicationGroupAttributes(
    dpy: PDisplay;
    app_group: TXAppGroup;
    dotdotdot: array of const
): TStatus; cdecl; external libXext;

function XagQueryApplicationGroup(
    dpy: PDisplay;
    resource_base: TXID;
    app_group_ret: PXAppGroup
): TStatus; cdecl; external libXext;

function XagCreateAssociation(
    dpy: PDisplay;
    window_ret: PWindow;
    system_window: Pointer
): TStatus; cdecl; external libXext;

function XagDestroyAssociation(
    dpy: PDisplay;
    window: TWindow
): TStatus; cdecl; external libXext;

implementation
end.
