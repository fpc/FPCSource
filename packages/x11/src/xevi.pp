(************************************************************
Copyright (c) 1997 by Silicon Graphics Computer Systems, Inc.
Permission to use, copy, modify, and distribute this
software and its documentation for any purpose and without
fee is hereby granted, provided that the above copyright
notice appear in all copies and that both that copyright
notice and this permission notice appear in supporting
documentation, and that the name of Silicon Graphics not be
used in advertising or publicity pertaining to distribution
of the software without specific prior written permission.
Silicon Graphics makes no representation about the suitability
of this software for any purpose. It is provided "as is"
without any express or implied warranty.
SILICON GRAPHICS DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS
SOFTWARE, INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY
AND FITNESS FOR A PARTICULAR PURPOSE. IN NO EVENT SHALL SILICON
GRAPHICS BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL
DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE
OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION  WITH
THE USE OR PERFORMANCE OF THIS SOFTWARE.
********************************************************)

{$IFNDEF FPC_DOTTEDUNITS}
unit xevi;
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

{$I evi.inc}

type
  PPExtendedVisualInfo = ^PExtendedVisualInfo;
  PExtendedVisualInfo = ^TExtendedVisualInfo;
  TExtendedVisualInfo = record
    core_visual_id: TVisualID;
    screen: cint;
    level: cint;
    transparency_type: cuint;
    transparency_value: cuint;
    min_hw_colormaps: cuint;
    max_hw_colormaps: cuint;
    num_colormap_conflicts: cuint;
    colormap_conflicts: PVisualID;
  end;

function XeviQueryExtension(
    dpy: PDisplay
): TBoolResult; cdecl; external libXext;

function XeviQueryVersion(
    dpy: PDisplay;
    majorVersion,
    minorVersion: Pcint
): TStatus; cdecl; external libXext;

function XeviGetVisualInfo(
    dpy: PDisplay;
    visual_query: PVisualID;
    nVisual_query: cint;
    extendedVisualInfo_return: PPExtendedVisualInfo;
    nInfo_return: Pcint
): TStatus; cdecl; external libXext;

implementation
end.
