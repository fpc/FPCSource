{*
 * $XFree86: xc/include/extensions/randr.h,v 1.4 2001/11/24 07:24:58 keithp Exp $
 *
 * Copyright (C) 2000, Compaq Computer Corporation, 
 * Copyright (C) 2002, Hewlett Packard, Inc.
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of Compaq or HP not be used in advertising
 * or publicity pertaining to distribution of the software without specific,
 * written prior permission.  HP makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * HP DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL HP
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN 
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 * Author:  Jim Gettys, HP Labs, Hewlett-Packard, Inc.
 *}

type
  PRotation      = ^TRotation;
  TRotation      = cushort;
  PSizeID        = ^TSizeID;
  TSizeID        = cushort;
  PSubpixelOrder = ^TSubpixelOrder;
  TSubpixelOrder = cushort;

const
  RANDR_NAME                     = 'RANDR';
  RANDR_MAJOR                    = 1;
  RANDR_MINOR                    = 1;

  RRNumberErrors                 = 0;
  RRNumberEvents                 = 1;

  X_RRQueryVersion               = 0;
{ we skip 1 to make old clients fail pretty immediately }
  X_RROldGetScreenInfo           = 1;
  X_RR1_0SetScreenConfig         = 2;
{ V1.0 apps share the same set screen config request id }
  X_RRSetScreenConfig            = 2;
  X_RROldScreenChangeSelectInput = 3;
{ 3 used to be ScreenChangeSelectInput; deprecated }
  X_RRSelectInput                = 4;
  X_RRGetScreenInfo              = 5;

{ used in XRRSelectInput }

  RRScreenChangeNotifyMask       = 1 shl 0;

  RRScreenChangeNotify           = 0;

{ used in the rotation field; rotation and reflection in 0.1 proto. }
  RR_Rotate_0                    = 1;
  RR_Rotate_90                   = 2;
  RR_Rotate_180                  = 4;
  RR_Rotate_270                  = 8;

{ new in 1.0 protocol, to allow reflection of screen }

  RR_Reflect_X                   = 16;
  RR_Reflect_Y                   = 32;

  RRSetConfigSuccess             = 0;
  RRSetConfigInvalidConfigTime   = 1;
  RRSetConfigInvalidTime         = 2;
  RRSetConfigFailed              = 3;
