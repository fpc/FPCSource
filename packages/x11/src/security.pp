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
unit security;
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

{$define _XAUTH_STRUCT_ONLY}
{$I xauth.inc}

{$I secur.inc}

type
{ type for returned auth ids }
  PXSecurityAuthorization = ^TXSecurityAuthorization;
  TXSecurityAuthorization = culong;

  PXSecurityAuthorizationAttributes = ^TXSecurityAuthorizationAttributes;
  TXSecurityAuthorizationAttributes = record
    timeout: cuint;
    trust_level: cuint;
    group: TXID;
    event_mask: clong;
  end;

  PXSecurityAuthorizationRevokedEvent = ^TXSecurityAuthorizationRevokedEvent;
  TXSecurityAuthorizationRevokedEvent = record
    _type: cint;                       { event base + XSecurityAuthorizationRevoked }
    serial: culong;                    { # of last request processed by server }
    send_event: TBool;                 { true if this came from a SendEvent request }
    display: PDisplay;                 { Display the event was read from }
    auth_id: TXSecurityAuthorization;  { revoked authorization id }
  end;

function XSecurityQueryExtension (
    dpy: PDisplay;
    major_version_return,
    minor_version_return: Pcint): TStatus; cdecl; external libXext;

function XSecurityAllocXauth: PXauth; cdecl; external libXext;

procedure XSecurityFreeXauth(auth: PXauth); cdecl; external libXext;

function XSecurityGenerateAuthorization(
    dpy: PDisplay;
    auth_in: PXauth;
    valuemask: culong;
    attributes: PXSecurityAuthorizationAttributes;
    auth_id_return: PXSecurityAuthorization): PXauth; cdecl; external libXext;

function XSecurityRevokeAuthorization(
    dpy: PDisplay;
    auth_id: TXSecurityAuthorization): TStatus; cdecl; external libXext;

implementation
end.
