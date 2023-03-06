(*

Copyright 1987, 1988, 1998  The Open Group

Permission to use, copy, modify, distribute, and sell this software and its
documentation for any purpose is hereby granted without fee, provided that
the above copyright notice appear in all copies and that both that
copyright notice and this permission notice appear in supporting
documentation.

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
OPEN GROUP BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of The Open Group shall not be
used in advertising or otherwise to promote the sale, use or other dealings
in this Software without prior written authorization from The Open Group.

*)

{$IFNDEF FPC_DOTTEDUNITS}
unit xcup;
{$ENDIF FPC_DOTTEDUNITS}

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

{$I cup.inc}

function XcupQueryVersion(
    dpy: PDisplay;
    major_version,
    minor_version: Pcint
): TBoolResult; cdecl; external libXext;

function XcupGetReservedColormapEntries(
    dpy: PDisplay;
    screen: cint;
    colors_out: PPXColor;
    ncolors: Pcint
): TStatus; cdecl; external libXext;

function XcupStoreColors(
    dpy: PDisplay;
    colormap: TColormap;
    colors: PXColor;
    ncolors: cint
): TStatus; cdecl; external libXext;

implementation
end.
