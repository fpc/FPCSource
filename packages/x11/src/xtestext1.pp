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

unit xtestext1;

{$PACKRECORDS c}

interface

uses
  ctypes, x, xlib, xmd;

const
  libXext = 'Xext';

(*
 * the typedefs for CARD8, CARD16, and CARD32 are defined in Xmd.h
 *)

{$I xtestext1const.inc}

(*
 * This is the definition for the input action host format event structure.
 * This is the form that a client using this extension will see when
 * it receives an input action event.
 *)
type
  PXTestInputActionEvent = ^TXTestInputActionEvent;
  TXTestInputActionEvent = record
    _type: cint;        { always XTestInputActionType }
    display: PDisplay;
    window: TWindow;
    actions: array [0..XTestACTIONS_SIZE-1] of CARD8;
  end;

(*
 * This is the definition for the xTestFakeAck host format event structure.
 * This is the form that a client using this extension will see when
 * it receives an XTestFakeAck event.
 *)
  PXTestFakeAckEvent = ^TXTestFakeAckEvent;
  TXTestFakeAckEvent = record
    _type: cint;        { always XTestFakeAckType }
    display: PDisplay;
    window: TWindow;
  end;

function XTestFakeInput({register} dpy: PDisplay; action_list_addr: PAnsiChar; action_list_size, ack_flag: cint): cint; cdecl; external libXext;
function XTestGetInput({register} dpy: PDisplay; action_handling: cint): cint; cdecl; external libXext;
function XTestQueryInputSize({register} dpy: PDisplay; size_return: Pculong): cint; cdecl; external libXext;
function XTestPressKey(display: PDisplay; device_id: cint; delay: culong; keycode, key_action: cuint): cint; cdecl; external libXext;
function XTestPressButton(display: PDisplay; device_id: cint; delay: culong; button_number, button_action: cuint): cint; cdecl; external libXext;
function XTestMovePointer(display: PDisplay; device_id: cint; delay: Pculong; x, y: Pcint; count: cuint): cint; cdecl; external libXext;
function XTestFlush(display: PDisplay): cint; cdecl; external libXext;
function XTestStopInput({register} dpy: PDisplay): cint; cdecl; external libXext;
function XTestReset({register} dpy: PDisplay): cint; cdecl; external libXext;

implementation
end.
