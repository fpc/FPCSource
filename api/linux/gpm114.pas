{
  $Id$

  Low level unit for GPM v1.14, the mouse server for Linux

  This program is free software; you can redistribute it and/or modify
  it under the terms of the GNU Library General Public License as published
  by the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *************************************************************************}
unit gpm114;
interface

uses
  linux;

{$LINKLIB c}
{$LINKLIB gpm}

const
  { Buttons }
  GPM_B_LEFT         = 4;
  GPM_B_MIDDLE       = 2;
  GPM_B_RIGHT        = 1;

  { Event types }
  GPM_MOVE           = 1;
  GPM_DRAG           = 2;
  GPM_DOWN           = 4;
  GPM_UP             = 8;

  GPM_SINGLE         = 16;            { at most one in three is set }
  GPM_DOUBLE         = 32;
  GPM_TRIPLE         = 64;

  GPM_MFLAG          = 128;
  GPM_HARD           = 256;

  GPM_ENTER          = 512;
  GPM_LEAVE          = 1024;

  GPM_BARE_EVENTS    = $60F;

  { Margins }
  GPM_TOP            = 1;
  GPM_BOT            = 2;
  GPM_LFT            = 4;
  GPM_RGT            = 8;

type
{$PACKRECORDS 4}
  TGPMConnect = record
    EventMask, DefaultMask: Word;
    MinMod, MaxMod: Word;
    Pid: Longint;
    vc: Longint;
  end;

  TGPMEvent = record
    Buttons, Modifiers: Byte;
    vc: Word;
    Dx, Dy, X, Y: Integer;
    EventType: Word;
    Clicks: Longint;
    GPMMargin: Word;
  end;

  TGPMHandler = function (const GPMEvent: TGPMEvent; ClientData: Pointer): Longint;cdecl;

{ Global variables }
var
  gpm_flag           : Longint;cvar;external;
  gpm_consolefd      : Longint;cvar;external name 'gpm_fd';
  gpm_tried          : Longbool;cvar;external;
  gpm_hflag          : Longbool;cvar;external;
  gpm_morekeys       : Longbool;cvar;external;
  gpm_zerobased      : Longbool;cvar;external;
  gpm_visiblepointer : Longbool;cvar;external;
  gpm_mx             : Longint;cvar;external;
  gpm_my             : Longint;cvar;external;
  gpm_timeout        : timeval;cvar;external;
  gpm_handler        : TGPMHandler;cvar;external;
  gpm_data           : Pointer;cvar;external;
  gpm_console_fd     : Longint;cvar;external;

function Gpm_Open(var Connect: TGPMConnect; Flag: Longint): Longint;cdecl;
function Gpm_Close:Longint;cdecl;
function Gpm_GetEvent(var Event: TGpmEvent): Longint;cdecl;

function Gpm_GetLibVersion(var where: Longint): PChar;cdecl;
function Gpm_GetServerVersion(var where: Longint): PChar;cdecl;
function Gpm_GetSnapshot(var Event: TGPMEvent): Longint;cdecl;

implementation

function Gpm_Open(var Connect: TGPMConnect; Flag: Longint): Longint; cdecl;external;
function Gpm_Close: Longint; cdecl;external;
function Gpm_GetEvent(var Event: TGpmEvent): Longint; cdecl;external;

function Gpm_GetLibVersion(var where: Longint): PChar; cdecl;external;
function Gpm_GetServerVersion(var where: Longint): PChar; cdecl;external;
function Gpm_GetSnapshot(var Event: TGPMEvent): Longint; cdecl;external;


end.
{
  $Log$
  Revision 1.1  2000-07-13 06:29:39  michael
  + Initial import

  Revision 1.1  2000/01/06 01:20:31  peter
    * moved out of packages/ back to topdir

  Revision 1.1  1999/11/24 23:36:38  peter
    * moved to packages dir

  Revision 1.1  1999/07/01 19:41:26  peter
    * define OLDGPM to compile with old gpm (for v1.14) else the new
      gpm unit from rtl will be used (v1.17)

  Revision 1.1  1998/12/04 12:48:30  peter
    * moved some dirs

  Revision 1.1  1998/10/29 11:02:51  peter
    + mouse for linux

}