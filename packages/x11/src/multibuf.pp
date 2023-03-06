(*
 *
Copyright 1989, 1998  The Open Group

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
unit multibuf;
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

{$I multibufconst.inc}

//#define MbufGetReq(name,req,info) GetReq (name, req); \
//        req->reqType = info->codes->major_opcode; \
//        req->mbufReqType = X_##name;

type
(*
 * Extra definitions that will only be needed in the client
 *)
  PMultibuffer = ^TMultibuffer;
  TMultibuffer = TXID;

  PXmbufClobberNotifyEvent = ^TXmbufClobberNotifyEvent;
  TXmbufClobberNotifyEvent = record
    _type: cint;           { of event }
    serial: culong;        { # of last request processed by server }
    send_event: cint;      { true if this came frome a SendEvent request }
    display: PDisplay;     { Display the event was read from }
    buffer: TMultibuffer;  { buffer of event }
    state: cint;           { see Clobbered constants above }
  end;

  PXmbufUpdateNotifyEvent = ^TXmbufUpdateNotifyEvent;
  TXmbufUpdateNotifyEvent = record
    _type: cint;           { of event }
    serial: culong;        { # of last request processed by server }
    send_event: cint;      { true if this came frome a SendEvent request }
    display: PDisplay;     { Display the event was read from }
    buffer: TMultibuffer;  { buffer of event }
  end;


(*
 * per-window attributes that can be got
 *)
  PXmbufWindowAttributes = ^TXmbufWindowAttributes;
  TXmbufWindowAttributes = record
    displayed_index: cint;  { which buffer is being displayed }
    update_action: cint;    { Undefined, Background, Untouched, Copied }
    update_hint: cint;      { Frequent, Intermittent, Static }
    window_mode: cint;      { Mono, Stereo }
    nbuffers: cint;         { Number of buffers }
    buffers: PMultibuffer;  { Buffers }
  end;

(*
 * per-window attributes that can be set
 *)
  PXmbufSetWindowAttributes = ^TXmbufSetWindowAttributes;
  TXmbufSetWindowAttributes = record
    update_hint: cint;  { Frequent, Intermittent, Static }
  end;


(*
 * per-buffer attributes that can be got
 *)
  PXmbufBufferAttributes = ^TXmbufBufferAttributes;
  TXmbufBufferAttributes = record
    window: TWindow;     { which window this belongs to }
    event_mask: culong;  { events that have been selected }
    buffer_index: cint;  { which buffer is this }
    side: cint;          { Mono, Left, Right }
  end;

(*
 * per-buffer attributes that can be set
 *)
  PXmbufSetBufferAttributes = ^TXmbufSetBufferAttributes;
  TXmbufSetBufferAttributes = record
    event_mask: culong;  { events that have been selected }
  end;


(*
 * per-screen buffer info (there will be lists of them)
 *)
  PPXmbufBufferInfo = ^PXmbufBufferInfo;
  PXmbufBufferInfo = ^TXmbufBufferInfo;
  TXmbufBufferInfo = record
    visualid: TVisualID;  { visual usuable at this depth }
    max_buffers: cint;    { most buffers for this visual }
    depth: cint;          { depth of buffers to be created }
  end;

function XmbufQueryExtension(
    dpy: PDisplay;
    event_base_return,
    error_base_return: Pcint
): TBoolResult; cdecl; external libXext;

function XmbufGetVersion(
    dpy: PDisplay;
    major_version_return,
    minor_version_return: Pcint
): TStatus; cdecl; external libXext;

function XmbufCreateBuffers(
    dpy: PDisplay;
    w: TWindow;
    count,
    update_action,
    update_hint: cint;
    buffers: PMultibuffer
): cint; cdecl; external libXext;

procedure XmbufDestroyBuffers(
    dpy: PDisplay;
    window: TWindow
); cdecl; external libXext;

procedure XmbufDisplayBuffers(
    dpy: PDisplay;
    count: cint;
    buffers: PMultibuffer;
    min_delay,
    max_delay: cint
); cdecl; external libXext;

function XmbufGetWindowAttributes(
    dpy: PDisplay;
    w: TWindow;
    attr: PXmbufWindowAttributes
): TStatus; cdecl; external libXext;

procedure XmbufChangeWindowAttributes(
    dpy: PDisplay;
    w: TWindow;
    valuemask: culong;
    attr: PXmbufSetWindowAttributes
); cdecl; external libXext;

function XmbufGetBufferAttributes(
    dpy: PDisplay;
    b: TMultibuffer;
    attr: PXmbufBufferAttributes
): TStatus; cdecl; external libXext;

procedure XmbufChangeBufferAttributes(
    dpy: PDisplay;
    b: TMultibuffer;
    valuemask: culong;
    attr: PXmbufSetBufferAttributes
); cdecl; external libXext;

function XmbufGetScreenInfo(
    dpy: PDisplay;
    d: TDrawable;
    nmono_return: Pcint;
    mono_info_return: PPXmbufBufferInfo;
    nstereo_return: Pcint;
    stereo_info_return: PPXmbufBufferInfo
): TStatus; cdecl; external libXext;

function XmbufCreateStereoWindow(
    dpy: PDisplay;
    parent: TWindow;
    x,
    y: cint;
    width,
    height,
    border_width: cuint;
    depth: cint;
    class: cuint;
    visual: PVisual;
    valuemask: culong;
    attr: PXSetWindowAttributes;
    leftp,
    rightp: PMultibuffer
): TWindow; cdecl; external libXext;

procedure XmbufClearBufferArea(
    dpy: PDisplay;
    buffer: TMultibuffer;
    x,
    y: cint;
    width,
    height: cuint;
    exposures: TBool
); cdecl; external libXext;

implementation
end.
