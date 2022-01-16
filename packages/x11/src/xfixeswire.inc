(*
 * Copyright (c) 2006, Oracle and/or its affiliates. All rights reserved.
 * Copyright 2010 Red Hat, Inc.
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

const
  XFIXES_NAME  = 'XFIXES';
  XFIXES_MAJOR = 5;
  XFIXES_MINOR = 0;

(*************** Version 1 ******************)
  X_XFixesQueryVersion                    = 0;
  X_XFixesChangeSaveSet                   = 1;
  X_XFixesSelectSelectionInput            = 2;
  X_XFixesSelectCursorInput               = 3;
  X_XFixesGetCursorImage                  = 4;
(*************** Version 2 ******************)
  X_XFixesCreateRegion                    = 5;
  X_XFixesCreateRegionFromBitmap          = 6;
  X_XFixesCreateRegionFromWindow          = 7;
  X_XFixesCreateRegionFromGC              = 8;
  X_XFixesCreateRegionFromPicture         = 9;
  X_XFixesDestroyRegion                   = 10;
  X_XFixesSetRegion                       = 11;
  X_XFixesCopyRegion                      = 12;
  X_XFixesUnionRegion                     = 13;
  X_XFixesIntersectRegion                 = 14;
  X_XFixesSubtractRegion                  = 15;
  X_XFixesInvertRegion                    = 16;
  X_XFixesTranslateRegion                 = 17;
  X_XFixesRegionExtents                   = 18;
  X_XFixesFetchRegion                     = 19;
  X_XFixesSetGCClipRegion                 = 20;
  X_XFixesSetWindowShapeRegion            = 21;
  X_XFixesSetPictureClipRegion            = 22;
  X_XFixesSetCursorName                   = 23;
  X_XFixesGetCursorName                   = 24;
  X_XFixesGetCursorImageAndName           = 25;
  X_XFixesChangeCursor                    = 26;
  X_XFixesChangeCursorByName              = 27;
(*************** Version 3 ******************)
  X_XFixesExpandRegion                    = 28;
(*************** Version 4 ******************)
  X_XFixesHideCursor                      = 29;
  X_XFixesShowCursor                      = 30;
(*************** Version 5 ******************)
  X_XFixesCreatePointerBarrier            = 31;
  X_XFixesDestroyPointerBarrier           = 32;

  XFixesNumberRequests = X_XFixesDestroyPointerBarrier+1;

{ Selection events share one event number }
  XFixesSelectionNotify                   = 0;

{ Within the selection, the 'subtype' field distinguishes }
  XFixesSetSelectionOwnerNotify           = 0;
  XFixesSelectionWindowDestroyNotify      = 1;
  XFixesSelectionClientCloseNotify        = 2;

  XFixesSetSelectionOwnerNotifyMask       = 1 shl 0;
  XFixesSelectionWindowDestroyNotifyMask  = 1 shl 1;
  XFixesSelectionClientCloseNotifyMask    = 1 shl 2;

{ There's only one cursor event so far }
  XFixesCursorNotify                      = 1;

  XFixesDisplayCursorNotify               = 0;

  XFixesDisplayCursorNotifyMask           = 1 shl 0;

  XFixesNumberEvents                      = 2;

{ errors }
  BadRegion                               = 0;
  BadBarrier                              = 1;
  XFixesNumberErrors                      = BadBarrier+1;

  SaveSetNearest                          = 0;
  SaveSetRoot                             = 1;

  SaveSetMap                              = 0;
  SaveSetUnmap                            = 1;

(*************** Version 2 ******************)

  WindowRegionBounding                    = 0;
  WindowRegionClip                        = 1;

(*************** Version 5 ******************)

  BarrierPositiveX                        = 1 shl 0;
  BarrierPositiveY                        = 1 shl 1;
  BarrierNegativeX                        = 1 shl 2;
  BarrierNegativeY                        = 1 shl 3;

