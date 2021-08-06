(*
 * xtestext1.h
 *
 * X11 Input Synthesis Extension include file
 *)

(*


Copyright 1986, 1987, 1988, 1998  The Open Group

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


Copyright 1986, 1987, 1988 by Hewlett-Packard Corporation

Permission to use, copy, modify, and distribute this
software and its documentation for any purpose and without
fee is hereby granted, provided that the above copyright
notice appear in all copies and that both that copyright
notice and this permission notice appear in supporting
documentation, and that the name of Hewlett-Packard not be used in
advertising or publicity pertaining to distribution of the
software without specific, written prior permission.

Hewlett-Packard makes no representations about the
suitability of this software for any purpose.  It is provided
"as is" without express or implied warranty.

This software is not subject to any license of the American
Telephone and Telegraph Company or of the Regents of the
University of California.

*)

const
  XTestMAX_ACTION_LIST_SIZE = 64;
  XTestACTIONS_SIZE         = 28;


(*
 * used in the XTestPressButton and XTestPressKey functions
 *)
  XTestPRESS   = 1 shl 0;
  XTestRELEASE = 1 shl 1;
  XTestSTROKE  = 1 shl 2;

(*
 * When doing a key or button stroke, the number of milliseconds
 * to delay between the press and the release of a key or button
 * in the XTestPressButton and XTestPressKey functions.
 *)

  XTestSTROKE_DELAY_TIME = 10;

(*
 * used in the XTestGetInput function
 *)
  XTestEXCLUSIVE      = 1 shl 0;
  XTestPACKED_ACTIONS = 1 shl 1;
  XTestPACKED_MOTION  = 1 shl 2;

(*
 * used in the XTestFakeInput function
 *)
  XTestFAKE_ACK_NOT_NEEDED = 0;
  XTestFAKE_ACK_REQUEST    = 1;

(*
 * used in the XTest extension initialization routine
 *)
  XTestEXTENSION_NAME = 'XTestExtension1';
  XTestEVENT_COUNT    = 2;

(*
 * This is the definition for the format of the header byte
 * in the input action structures.
 *)
  XTestACTION_TYPE_MASK = $03;  { bits 0 and 1          }
  XTestKEY_STATE_MASK   = $04;  { bit 2 (key action)    }
  XTestX_SIGN_BIT_MASK  = $04;  { bit 2 (motion action) }
  XTestY_SIGN_BIT_MASK  = $08;  { bit 3 (motion action) }
  XTestDEVICE_ID_MASK   = $f0;  { bits 4 through 7      }

  XTestMAX_DEVICE_ID    = $0f;
//#define XTestPackDeviceID(x)	(((x) & XTestMAX_DEVICE_ID) << 4)
//#define XTestUnpackDeviceID(x)	(((x) & XTestDEVICE_ID_MASK) >> 4)

(*
 * These are the possible action types.
 *)
  XTestDELAY_ACTION  = 0;
  XTestKEY_ACTION    = 1;
  XTestMOTION_ACTION = 2;
  XTestJUMP_ACTION   = 3;

(*
 * These are the definitions for key/button motion input actions.
 *)
  XTestKEY_UP   = $04;
  XTestKEY_DOWN = $00;

(*
 * These are the definitions for pointer relative motion input
 * actions.
 *
 * The sign bits for the x and y relative motions are contained
 * in the header byte.  The x and y relative motions are packed
 * into one byte to make things fit in 32 bits.  If the relative
 * motion range is larger than +/-15, use the pointer jump action.
 *)
  XTestMOTION_MAX    = 15;
  XTestMOTION_MIN    = -15;

  XTestX_NEGATIVE    = $04;
  XTestY_NEGATIVE    = $08;

  XTestX_MOTION_MASK = $0f;
  XTestY_MOTION_MASK = $f0;

//#define XTestPackXMotionValue(x)   ((x) & XTestX_MOTION_MASK)
//#define XTestPackYMotionValue(x)   (((x) << 4) & XTestY_MOTION_MASK)

//#define XTestUnpackXMotionValue(x) ((x) & XTestX_MOTION_MASK)
//#define XTestUnpackYMotionValue(x) (((x) & XTestY_MOTION_MASK) >> 4)
(*
 * These are the definitions for a long delay input action.  It is
 * used when more than XTestSHORT_DELAY_TIME milliseconds of delay
 * (approximately one minute) is needed.
 *
 * The device ID for a delay is always set to XTestDELAY_DEVICE_ID.
 * This guarantees that a header byte with a value of 0 is not
 * a valid header, so it can be used as a flag to indicate that
 * there are no more input actions in an XTestInputAction event.
 *)

  XTestSHORT_DELAY_TIME = $ffff;
  XTestDELAY_DEVICE_ID  = $0f;
