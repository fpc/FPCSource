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
// Module: sipapi.h
//

//
//  Microsoft Windows Mobile 5.0 for PocketPC SDK.
//

unit SIPApi;

{$CALLING cdecl}

interface

uses Windows;

const
      SIP_STATUS_UNAVAILABLE	= 0;
      SIP_STATUS_AVAILABLE	  = 1;

function SipStatus:DWORD; external KernelDLL name 'SipStatus'; // index 659


function SipSetDefaultRect(lprt:LPRECT):BOOL; external KernelDLL name 'SipSetDefaultRect'; // index 661
function SipRegisterNotification(hWin:HWND):BOOL; external KernelDLL name 'SipRegisterNotification'; // index 65A

function SipShowIM(dwFlag:DWORD):BOOL; external KernelDLL name 'SipShowIM'; // index 65B

const
      SIPF_OFF	   = $00000000;
      SIPF_ON 	   = $00000001;
      SIPF_DOCKED	= $00000002;
      SIPF_LOCKED	= $00000004;

      SPI_SETCOMPLETIONINFO	= 223;
      SPI_SETSIPINFO			     = 224;
      SPI_GETSIPINFO			     = 225;
      SPI_SETCURRENTIM      = 226;
      SPI_GETCURRENTIM      = 227;
      SPI_SIPMOVE	          = 250;

type
     tagSIPINFO = record
       cbSize:DWORD;
       fdwFlags:DWORD;
       rcVisibleDesktop:RECT;
       rcSipRect:RECT;
       dwImDataSize:DWORD;
       pvImData:LPVOID;
     end;
     SIPINFO = tagSIPINFO;
     LPSIPINFO = ^tagSIPINFO;

function SipGetInfo(pSipInfo:LPSIPINFO):BOOL; external KernelDLL name 'SipGetInfo'; // index 65C

function SipSetInfo(pSipInfo:LPSIPINFO):BOOL; external KernelDLL name 'SipSetInfo'; // index 65D

type
     tagIMENUMINFO = record
       szName:array[0..MAX_PATH-1] of TCHAR;
       clsid:CLSID;
     end;
     IMENUMINFO = tagIMENUMINFO;
     PIMENUMINFO = ^tagIMENUMINFO;

type
     IMENUMPROC = function(pIMInfo:PIMENUMINFO):longint;

function SipEnumIM(pEnumIMProc:IMENUMPROC):longint; external KernelDLL name 'SipEnumIM'; // index 65E
function SipGetCurrentIM(pClsid:LPCLSID):BOOL; external KernelDLL name 'SipGetCurrentIM'; // index 65F
function SipSetCurrentIM(pClsid:LPCLSID):BOOL; external KernelDLL name 'SipSetCurrentIM'; // index 660

type
     tagIMWINDOWPOS = record
       x:longint;	    // Screen coordinate
       y:longint;	    // Screen coordinate
       cx:longint;     // Screen coordinate
       cy:longint;     // Screen coordinate
     end;
     IMWINDOWPOS = tagIMWINDOWPOS;
     LPIMWINDOWPOS = ^tagIMWINDOWPOS;

// The following defines are for the WPARAM value in WM_IM_INFO.
const
      IM_POSITION			= 0;
      IM_WIDEIMAGE		= 1;
      IM_NARROWIMAGE		= 2;
      IM_HWND_CONTEXT		= 3;
      IM_CONVERSION_MODE	= 4;
      IM_SENTENCE_MODE	= 5;
      IM_KEYBOARD_LAYOUT	= 6;

implementation

end.