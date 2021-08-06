(*
 * Copyright 1992 Network Computing Devices
 *
 * Permission to use, copy, modify, distribute, and sell this software and its
 * documentation for any purpose is hereby granted without fee, provided that
 * the above copyright notice appear in all copies and that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of NCD. not be used in advertising or
 * publicity pertaining to distribution of the software without specific,
 * written prior permission.  NCD. makes no representations about the
 * suitability of this software for any purpose.  It is provided "as is"
 * without express or implied warranty.
 *
 * NCD. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL NCD.
 * BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION
 * OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 *)

const
  LBXNAME = 'LBX';

  LBX_MAJOR_VERSION = 1;
  LBX_MINOR_VERSION = 0;

  LbxNumberReqs            = 44;
  LbxEvent                 = 0;
  LbxQuickMotionDeltaEvent = 1;
  LbxNumberEvents          = 2;

{ This is always the master client }
  LbxMasterClientIndex     = 0;

{ LbxEvent lbxType sub-fields }
  LbxSwitchEvent           = 0;
  LbxCloseEvent            = 1;
  LbxDeltaEvent            = 2;
  LbxInvalidateTagEvent    = 3;
  LbxSendTagDataEvent      = 4;
  LbxListenToOne           = 5;
  LbxListenToAll           = 6;
  LbxMotionDeltaEvent      = 7;
  LbxReleaseCmapEvent      = 8;
  LbxFreeCellsEvent        = 9;

(*
 * Lbx image compression methods
 *
 * No compression is always assigned the value of 0.
 *
 * The rest of the compression method opcodes are assigned dynamically
 * at option negotiation time.
 *)

  LbxImageCompressNone     = 0;


  BadLbxClient             = 0;
  LbxNumberErrors          = BadLbxClient + 1;

{ tagged data types }
  LbxTagTypeModmap         = 1;
  LbxTagTypeKeymap         = 2;
  LbxTagTypeProperty       = 3;
  LbxTagTypeFont           = 4;
  LbxTagTypeConnInfo       = 5;
