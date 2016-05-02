(*
 * Copyright (c) 2006, Oracle and/or its affiliates. All rights reserved.
 * Copyright 2011 Red Hat, Inc.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice (including the next
 * paragraph) shall be included in all copies or substantial portions of the
 * Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 *)
(*
 * Copyright © 2002 Keith Packard, member of The XFree86 Project, Inc.
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Keith Packard not be used in
 * advertising or publicity pertaining to distribution of the software without
 * specific, written prior permission.  Keith Packard makes no
 * representations about the suitability of this software for any purpose.  It
 * is provided "as is" without express or implied warranty.
 *
 * KEITH PACKARD DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO
 * EVENT SHALL KEITH PACKARD BE LIABLE FOR ANY SPECIAL, INDIRECT OR
 * CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE,
 * DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER
 * TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR
 * PERFORMANCE OF THIS SOFTWARE.
 *)

unit xfixes;

interface

{$PACKRECORDS C}

uses
  ctypes, x, xlib;

const
  libXfixes = 'Xfixes';

{$I xfixeswire.inc}

//#include <X11/Xfuncproto.h>
//#include <X11/Xlib.h>

const
(*
 * This revision number also appears in configure.ac, they have
 * to be manually synchronized
 *)
  XFIXES_REVISION = 1;
  XFIXES_VERSION = (XFIXES_MAJOR * 10000) + (XFIXES_MINOR * 100) + XFIXES_REVISION;

type
  PXFixesSelectionNotifyEvent = ^TXFixesSelectionNotifyEvent;
  TXFixesSelectionNotifyEvent = record
    _type: cint;                        { event base }
    serial: culong;
    send_event: TBool;
    display: PDisplay;
    window: TWindow;
    subtype: cint;
    owner: TWindow;
    selection: TAtom;
    timestamp: TTime;
    selection_timestamp: TTime;
  end;

  PXFixesCursorNotifyEvent = ^TXFixesCursorNotifyEvent;
  TXFixesCursorNotifyEvent = record
    _type: cint;                        { event base }
    serial: culong;
    send_event: TBool;
    display: PDisplay;
    window: TWindow;
    subtype: cint;
    cursor_serial: culong;
    timestamp: TTime;
    cursor_name: TAtom;
  end;

  PXFixesCursorImage = ^TXFixesCursorImage;
  TXFixesCursorImage = record
    x, y: cshort;
    width, height: cushort;
    xhot, yhot: cushort;
    cursor_serial: culong;
    pixels: Pculong;
//#if XFIXES_MAJOR >= 2
    atom: TAtom;                    { Version >= 2 only }
    name: PChar;                    { Version >= 2 only }
//#endif
  end;

//#if XFIXES_MAJOR >= 2
{ Version 2 types }

  PXserverRegion = ^TXserverRegion;
  TXserverRegion = TXID;

  PXFixesCursorImageAndName = ^TXFixesCursorImageAndName;
  TXFixesCursorImageAndName = record
    x, y: cshort;
    width, height: cushort;
    xhot, yhot: cushort;
    cursor_serial: culong;
    pixels: Pculong;
    atom: TAtom;
    name: PChar;
  end;

//#endif

//_XFUNCPROTOBEGIN

function XFixesQueryExtension(dpy: PDisplay;
                               event_base_return: Pcint;
                               error_base_return: Pcint): TBoolResult; cdecl; external libXfixes;
function XFixesQueryVersion(dpy: PDisplay;
                             major_version_return: Pcint;
                             minor_version_return: Pcint): TStatus; cdecl; external libXfixes;

function XFixesVersion: cint; cdecl; external libXfixes;

procedure
XFixesChangeSaveSet(dpy: PDisplay;
                    win: TWindow;
                    mode: cint;
                    target: cint;
                    map: cint); cdecl; external libXfixes;

procedure
XFixesSelectSelectionInput(dpy: PDisplay;
                           win: TWindow;
                           selection: TAtom;
                           eventMask: culong); cdecl; external libXfixes;

procedure
XFixesSelectCursorInput(dpy: PDisplay;
                        win: TWindow;
                        eventMask: culong); cdecl; external libXfixes;

function XFixesGetCursorImage(dpy: PDisplay)
: PXFixesCursorImage; cdecl; external libXfixes;

//#if XFIXES_MAJOR >= 2
{ Version 2 functions }

function XFixesCreateRegion(dpy: PDisplay; rectangles: PXRectangle; nrectangles: cint)
: TXserverRegion; cdecl; external libXfixes;

function XFixesCreateRegionFromBitmap(dpy: PDisplay; bitmap: TPixmap)
: TXserverRegion; cdecl; external libXfixes;

function XFixesCreateRegionFromWindow(dpy: PDisplay; window: TWindow; kind: cint)
: TXserverRegion; cdecl; external libXfixes;

function XFixesCreateRegionFromGC(dpy: PDisplay; gc: TGC)
: TXserverRegion; cdecl; external libXfixes;

function XFixesCreateRegionFromPicture(dpy: PDisplay; picture: TXID)
: TXserverRegion; cdecl; external libXfixes;

procedure
XFixesDestroyRegion(dpy: PDisplay; region: TXserverRegion); cdecl; external libXfixes;

procedure
XFixesSetRegion(dpy: PDisplay; region: TXserverRegion;
                rectangles: PXRectangle; nrectangles: cint); cdecl; external libXfixes;

procedure
XFixesCopyRegion(dpy: PDisplay; dst: TXserverRegion; src: TXserverRegion); cdecl; external libXfixes;

procedure
XFixesUnionRegion(dpy: PDisplay; dst: TXserverRegion;
                   src1: TXserverRegion; src2: TXserverRegion); cdecl; external libXfixes;

procedure
XFixesIntersectRegion(dpy: PDisplay; dst: TXserverRegion;
                       src1: TXserverRegion; src2: TXserverRegion); cdecl; external libXfixes;

procedure
XFixesSubtractRegion(dpy: PDisplay; dst: TXserverRegion;
                      src1: TXserverRegion; src2: TXserverRegion); cdecl; external libXfixes;

procedure
XFixesInvertRegion(dpy: PDisplay; dst: TXserverRegion;
                    rect: PXRectangle; src: TXserverRegion); cdecl; external libXfixes;

procedure
XFixesTranslateRegion(dpy: PDisplay; region: TXserverRegion; dx, dy: cint); cdecl; external libXfixes;

procedure
XFixesRegionExtents(dpy: PDisplay; dst: TXserverRegion; src: TXserverRegion); cdecl; external libXfixes;

function XFixesFetchRegion(dpy: PDisplay; region: TXserverRegion; nrectanglesRet: Pcint)
: PXRectangle; cdecl; external libXfixes;

function XFixesFetchRegionAndBounds(dpy: PDisplay; region: TXserverRegion;
                                    nrectanglesRet: Pcint;
                                    bounds: PXRectangle)
: PXRectangle; cdecl; external libXfixes;

procedure
XFixesSetGCClipRegion(dpy: PDisplay; gc: TGC;
                      clip_x_origin: cint; clip_y_origin: cint;
                      region: TXserverRegion); cdecl; external libXfixes;

procedure
XFixesSetWindowShapeRegion(dpy: PDisplay; win: TWindow; shape_kind: cint;
                           x_off: cint; y_off: cint; region: TXserverRegion); cdecl; external libXfixes;

procedure
XFixesSetPictureClipRegion(dpy: PDisplay; picture: TXID;
                           clip_x_origin: cint; clip_y_origin: cint;
                           region: TXserverRegion); cdecl; external libXfixes;

procedure
XFixesSetCursorName(dpy: PDisplay; cursor: TCursor; name: PChar); cdecl; external libXfixes;

function XFixesGetCursorName(dpy: PDisplay; cursor: TCursor; atom: PAtom)
: PChar; cdecl; external libXfixes;

procedure
XFixesChangeCursor(dpy: PDisplay; source: TCursor; destination: TCursor); cdecl; external libXfixes;

procedure
XFixesChangeCursorByName(dpy: PDisplay; source: TCursor; name: PChar); cdecl; external libXfixes;

//#endif	/* XFIXES_MAJOR >= 2 */

//#if XFIXES_MAJOR >= 3

procedure
XFixesExpandRegion(dpy: PDisplay; dst: TXserverRegion; src: TXserverRegion;
                   left, right, top, bottom: cunsigned); cdecl; external libXfixes;

//#endif	/* XFIXES_MAJOR >= 3 */

//#if XFIXES_MAJOR >= 4
{ Version 4.0 externs }

procedure
XFixesHideCursor(dpy: PDisplay; win: TWindow); cdecl; external libXfixes;

procedure
XFixesShowCursor(dpy: PDisplay; win: TWindow); cdecl; external libXfixes;

//#endif /* XFIXES_MAJOR >= 4 */

//#if XFIXES_MAJOR >= 5

type
  PPointerBarrier = ^TPointerBarrier;
  TPointerBarrier = TXID;


function
XFixesCreatePointerBarrier(dpy: PDisplay; w: TWindow; x1, y1,
			   x2, y2: cint; directions: cint;
			   num_devices: cint; devices: Pcint): TPointerBarrier; cdecl; external libXfixes;

procedure
XFixesDestroyPointerBarrier(dpy: PDisplay; b: TPointerBarrier); cdecl; external libXfixes;

//#endif /* XFIXES_MAJOR >= 5 */

//_XFUNCPROTOEND

implementation
end.
