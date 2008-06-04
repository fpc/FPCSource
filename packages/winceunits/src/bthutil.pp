{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2008 Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 ********************************************************************** }
//
//  Microsoft Windows Mobile 6.0 for PocketPC SDK.
//

unit bthutil;

{$CALLING cdecl}

interface

uses Windows;

////////////////////////////////////////////////////////////////////////////////
//
// @enum    BTH_RADIO_MODE |
//          Enumerate all the possible modes of operation of the bluetooth radio
//
////////////////////////////////////////////////////////////////////////////////

type
     BTH_RADIO_MODE = DWORD;

const
      BTH_POWER_OFF    = 0;
      BTH_CONNECTABLE  = 1;
      BTH_DISCOVERABLE = 2;

const
      BthutilDLL = 'bthutil.dll';

////////////////////////////////////////////////////////////////////////////////
//
// @func    int | BthSetMode |
//          This function set the bluetooth Mode of operation and reflects it in
//          the control panel.  It also persist that state across hardware insertion
//          and reboot
//
// @syntax  BthSetMode( DWORD dwMode )
//
// @parm    DWORD | dwMode |
//          BTH_POWER_OFF to turn the bluetooth radio OFF
//          BTH_CONNECTABLE to turn the bluetooth radio ON and CONNECTABLE
//          BTH_DISCOVERABLE to turn the bluetooth radio ON and CONNECTABLE and DISCOVERABLE
//
// @rdesc   int: ERROR_SUCCESS on success.  Error code describing error on failure.
//
////////////////////////////////////////////////////////////////////////////////

function BthSetMode(dwMode:DWORD):longint; external BthutilDLL name 'BthSetMode';

////////////////////////////////////////////////////////////////////////////////
//
// @func    int | BthGetMode |
//          This function retrieves the current mode of operation of the bluetooth radio
//
// @syntax  BthGetMode( DWORD* pdwMode )
//
// @parm    DWORD* | pdwMode |
//          BTH_POWER_OFF to indicate the bluetooth radio is OFF
//          BTH_CONNECTABLE to indicate the bluetooth radio ON and CONNECTABLE
//          BTH_DISCOVERABLE to indicate the bluetooth radio ON and CONNECTABLE and DISCOVERABLE
//
// @rdesc   int: ERROR_SUCCESS on success.  Error code describing error on failure.
//
////////////////////////////////////////////////////////////////////////////////

function BthGetMode(pdwMode:LPDWORD):longint; external BthutilDLL name 'BthGetMode';

implementation

end.