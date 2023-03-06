(*****************************************************************

Copyright (c) 1996 Digital Equipment Corporation, Maynard, Massachusetts.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software.

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
DIGITAL EQUIPMENT CORPORATION BE LIABLE FOR ANY CLAIM, DAMAGES, INCLUDING,
BUT NOT LIMITED TO CONSEQUENTIAL OR INCIDENTAL DAMAGES, OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR
IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Except as contained in this notice, the name of Digital Equipment Corporation
shall not be used in advertising or otherwise to promote the sale, use or other
dealings in this Software without prior written authorization from Digital
Equipment Corporation.

******************************************************************)

{$IFNDEF FPC_DOTTEDUNITS}
unit dpms;
{$ENDIF FPC_DOTTEDUNITS}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.CTypes, Api.X11.Xmd, Api.X11.Xlib;
{$ELSE FPC_DOTTEDUNITS}
uses
  ctypes, xmd, xlib;
{$ENDIF FPC_DOTTEDUNITS}

const
  libXext = 'Xext';

{$I dpmsconst.inc}

function DPMSQueryExtension(display: PDisplay; event_base, error_base: Pcint): TBoolResult; cdecl; external libXext;
function DPMSGetVersion(display: PDisplay; major_version, minor_version: Pcint): TStatus; cdecl; external libXext;
function DPMSCapable(display: PDisplay): TBoolResult; cdecl; external libXext;
function DPMSSetTimeouts(display: PDisplay; standby, suspend, off: CARD16): TStatus; cdecl; external libXext;
function DPMSGetTimeouts(display: PDisplay; standby, suspend, off: PCARD16): TBoolResult; cdecl; external libXext;
function DPMSEnable(display: PDisplay): TStatus; cdecl; external libXext;
function DPMSDisable(display: PDisplay): TStatus; cdecl; external libXext;
function DPMSForceLevel(display: PDisplay; level: CARD16): TStatus; cdecl; external libXext;
function DPMSInfo(display: PDisplay; power_level: PCARD16; state: PBoolResult): TStatus; cdecl; external libXext;

implementation
end.
