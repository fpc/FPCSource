{Set tabsize to 4.}
{****************************************************************************

                           KBDCALLS interface unit
                     FPK-Pascal Runtime Library for OS/2
                   Copyright (c) 1993,94 by Florian Kl„mpfl
                    Copyright (c) 1997 by Dani‰l Mantione
                      Copyright (c) 1998 by Tomas Hajny

 The FPK-Pascal runtime library is distributed under the Library GNU Public
 License v2. So is this unit. The Library GNU Public License requires you to
 distribute the source code of this unit with any product that uses it.
 Because the EMX library isn't under the LGPL, we grant you an exception to
 this, and that is, when you compile a program with the FPK Pascal compiler,
 you do not need to ship source code with that program, AS LONG AS YOU ARE
 USING UNMODIFIED CODE! If you modify this code, you MUST change the next
 line:

 <This is an official, unmodified FPK Pascal source code file.>

 Send us your modified files, we can work together if you want!

 FPK-Pascal is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 Library GNU General Public License for more details.

 You should have received a copy of the Library GNU General Public License
 along with FPK-Pascal; see the file COPYING.LIB.  If not, write to
 the Free Software Foundation, 59 Temple Place - Suite 330,
 Boston, MA 02111-1307, USA.

****************************************************************************}

unit KbdCalls;

{ Interface library to KBDCALLS.DLL (through EMXWRAP.DLL)

Changelog:

    People:

        TH - Tomas Hajny

    Date:           Description of change:              Changed by:

     -              First released version 0.99         TH

Coding style:

    I have tried to use the same coding style as Dani‰l Mantione in unit
    DOSCALLS, although I can't say I would write it the same way otherwise (I
    would write much more spaces myself, at least). Try to use it as well,
    please. Original note by Dani‰l Mantione follows:


    It may be well possible that coding style feels a bit strange to you.
    Nevertheless I friendly ask you to try to make your changes not look all
    to different. To make life easier, set your IDE to use tab characters,
    turn optimal fill, autoindent and backspace unindents on and set a
    tabsize of 4.}

{***************************************************************************}
interface
{***************************************************************************}

uses    strings;

{$ifdef FPK}
    {$packrecords 1}
{$endif FPK}

const
{FnMask}
    KR_KBDCHARIN        =$00000001;
    KR_KBDPEEK          =$00000002;
    KR_KBDFLUSHBUFFER   =$00000004;
    KR_KBDGETSTATUS     =$00000008;
    KR_KBDSETSTATUS     =$00000010;
    KR_KBDSTRINGIN      =$00000020;
    KR_KBDOPEN          =$00000040;
    KR_KBDCLOSE         =$00000080;
    KR_KBDGETFOCUS      =$00000100;
    KR_KBDFREEFOCUS     =$00000200;
    KR_KBDGETCP         =$00000400;
    KR_KBDSETCP         =$00000800;
    KR_KBDXLATE         =$00001000;
    KR_KBDSETCUSTXT     =$00002000;

{WaitFlag}
    IO_WAIT     =0;
        {KbdCharIn: wait for a character if one is not available}
        {KbdGetFocus: wait for the focus}
    IO_NOWAIT   =1;
        {KbdCharIn: immediate return if no character is available}
        {KbdGetFocus: do not wait for the focus}

{TKbdInfo.fsMask}
    KEYBOARD_ECHO_ON            =$0001;
    KEYBOARD_ECHO_OFF           =$0002;
    KEYBOARD_BINARY_MODE        =$0004;
    KEYBOARD_ASCII_MODE         =$0008;
    KEYBOARD_MODIFY_STATE       =$0010;
    KEYBOARD_MODIFY_INTERIM     =$0020;
    KEYBOARD_MODIFY_TURNAROUND  =$0040;
    KEYBOARD_2B_TURNAROUND      =$0080;
    KEYBOARD_SHIFT_REPORT       =$0100;

{TKbdInfo.fsState/TKbdKeyInfo.fsState/TKbdTrans.fsState}
    KBDSTF_RIGHTSHIFT           =$0001;
    KBDSTF_LEFTSHIFT            =$0002;
    KBDSTF_CONTROL              =$0004;
    KBDSTF_ALT                  =$0008;
    KBDSTF_SCROLLLOCK_ON        =$0010;
    KBDSTF_NUMLOCK_ON           =$0020;
    KBDSTF_CAPSLOCK_ON          =$0040;
    KBDSTF_INSERT_ON            =$0080;
    KBDSTF_LEFTCONTROL          =$0100;
    KBDSTF_LEFTALT              =$0200;
    KBDSTF_RIGHTCONTROL         =$0400;
    KBDSTF_RIGHTALT             =$0800;
    KBDSTF_SCROLLLOCK           =$1000;
    KBDSTF_NUMLOCK              =$2000;
    KBDSTF_CAPSLOCK             =$4000;
    KBDSTF_SYSREQ               =$8000;

{TKbdTrans.fbStatus}
    KBDTRF_SHIFT_KEY_IN         =$01;   {shift status returned}
                                        {without character    }
    KBDTRF_EXTENDED_KEY_IN      =$02;   {extended key code }
                                        {from the keyboard,}
                                        {not a character   }
    KBDTRF_CONVERSION_REQUEST   =$20;   {immediate conversion}
                                        {requested           }
    KBDTRF_FINAL_CHAR_IN        =$40;   {either $40 or $80 or both}
    KBDTRF_INTERIM_CHAR_IN      =$80;   {must be present          }


type
{TKbdKeyInfo - character data structure for KbdCharIn and KbdPeek}
(*   #pragma pack(2) ??? *)
    TKbdKeyInfo=record
        chChar:char;    {ASCII character code; the scan code received}
                        {from the keyboard is translated to the ASCII}
                        {character code                              }
        chScan:byte;    {scan Code received from the keyboard}
        fbStatus:byte;  {state of the keystroke event, see KBDTRF_*}
        bNlsShift:byte; {NLS shift status (always 0?)}
        fsState:word;   {shift key status, see KBDSTF_*}
        time:longint;   {time stamp indicating when a key was pressed,}
                        {specified in milliseconds from the time      }
                        {the system was started                       }
    end;
    PKbdKeyInfo=^TKbdKeyInfo;

{structure for KbdStringIn}
    TStringInBuf=record
        cb:word;
        cchIn:word;
    end;
    PStringInBuf=TStringInBuf;

{TKbdInfo structure, for KbdSet/GetStatus}
    TKbdInfo=record
        cb,
        fsMask,
        chTurnAround,
        fsInterim,
        fsState:word;
    end;
    PKbdInfo=^TKbdInfo;

{structure for KbdGetHWID}
    TKbdHWID=record
        cb,
        idKbd,
        usReserved1,
        usReserved2:word;
    end;
    PKbdHWID=^TKbdHWID;

{structure for KbdXlate}
(*   #pragma pack(2) ???*)
    TKbdTrans=record
        chChar:char;
        chScan:byte;
        fbStatus:byte;
        bNlsShift:byte;
        fsState:word;
        time:longint;
        fsDD:word;
        fsXlate:word;
        fsShift:word;
        sZero:word;
    end;
    PKbdTrans=^TKbdTrans;


{See KR_* constants for FnMask}
function KbdRegister(ModuleName,ProcName:PChar;FnMask:longint):word;
function KbdRegister(ModuleName,ProcName:string;FnMask:longint):word;

{Deregister a keyboard subsystem previously registered within a session - only
the process that issued the KbdRegister may issue KbdDeRegister}
{Possible return codes:
    0         NO_ERROR
    411       ERROR_KBD_DEREGISTER
    464       ERROR_KBD_DETACHED
    504       ERROR_KBD_EXTENDED_SG}
function KbdDeRegister:word;

{Return a character data record from the keyboard}
{Key - see TKbdKeyInfo, WaitFlag - see IO_WAIT and IO_NOWAIT constants,
KbdHandle is the default keyboard (0) or a logical keyboard.}
{Possible return codes are:
    0         NO_ERROR
    375       ERROR_KBD_INVALID_IOWAIT
    439       ERROR_KBD_INVALID_HANDLE
    445       ERROR_KBD_FOCUS_REQUIRED
    447       ERROR_KBD_KEYBOARD_BUSY
    464       ERROR_KBD_DETACHED
    504       ERROR_KBD_EXTENDED_SG}
{Remarks:
* On an enhanced keyboard, the secondary enter key returns the normal
  character 0DH and a scan code of E0H.
* Double-byte character codes (DBCS) require two function calls to obtain the
  entire code.
* If shift report is set with KbdSetStatus, the CharData record returned
  reflects changed shift information only.
* Extended ASCII codes are identified with the status byte, bit 1 on and the
  ASCII character code being either 00H or E0H. Both conditions must be
  satisfied for the character to be an extended keystroke.  For extended
  ASCII codes, the scan code byte returned is the second code (extended
  code). Usually the extended ASCII code is the scan code of the primary key
  that was pressed.
* A thread in the foreground session that repeatedly polls the keyboard with
  KbdCharIn (with no wait), can prevent all regular priority class threads
  from executing.  If polling must be used and a minimal amount of other
  processing is being performed, the thread should periodically yield to the
  CPU by issuing a DosSleep call for an interval of at least 5 milliseconds.}
function KbdCharIn(var Key:TKbdKeyInfo;WaitFlag,KbdHandle:word):word;

function KbdPeek(var Key:TKbdKeyInfo;KbdHandle:word):word;

function KbdStringIn(var CharBuf;var pchIn:TStringInBuf;WaitFlag:word;
                                                          KbdHandle:word):word;

{Clear the keystroke buffer}
{KbdHandle is the default keyboard (0) or a logical keyboard.}
{Possible return codes are:
    0         NO_ERROR
    439       ERROR_KBD_INVALID_HANDLE
    445       ERROR_KBD_FOCUS_REQUIRED
    447       ERROR_KBD_KEYBOARD_BUSY
    464       ERROR_KBD_DETACHED
    504       ERROR_KBD_EXTENDED_SG}
{Remarks:
* KbdFlushBuffer completes when the handle has access to the physical
  keyboard (focus), or is equal to zero and no other handle has the focus.}
function KbdFlushBuffer(KbdHandle:word):word;

function KbdSetStatus(var Status:TKbdInfo;KbdHandle:word):word;

function KbdGetStatus(var Status:TKbdInfo;KbdHandle:word):word;

function KbdSetCp(usReserved,CodePage,KbdHandle:word):word;

{Query the code page being used to translate scan codes to ASCII characters.}
{ulReserved must be set to 0. The keyboard support returns the current code
page for a specified keyboard handle in CodePage, it is one of the code page
IDs specified in the CONFIG.SYS CODEPAGE= statement or 0000. KbdHandle is
the default keyboard (0) or a logical keyboard.}
{Possible return codes:
    0         NO_ERROR
    373       ERROR_KBD_PARAMETER
    439       ERROR_KBD_INVALID_HANDLE
    445       ERROR_KBD_FOCUS_REQUIRED
    447       ERROR_KBD_KEYBOARD_BUSY
    464       ERROR_KBD_DETACHED
    504       ERROR_KBD_EXTENDED_SG}
{Remarks:
* CodePage is set to the currently active keyboard code page. A value of 0
  indicates the code page translation table in use is the ROM code page
  translation table provided by the hardware.}
function KbdGetCp(ulReserved:longint;var CodePage:word;KbdHandle:word):word;

function KbdOpen(var KbdHandle:word):word;

{Close the existing logical keyboard identified by the keyboard handle}
{KbdHandle is the default keyboard (0) or a logical keyboard}
{Possible return codes:
    0         NO_ERROR
    439       ERROR_KBD_INVALID_HANDLE
    464       ERROR_KBD_DETACHED
    504       ERROR_KBD_EXTENDED_SG}
{Remarks:
* KbdClose blocks while another thread has the keyboard focus (by way of
  KbdGetFocus) until the thread with the focus issues KbdFreeFocus.
  Therefore, to prevent KbdClose from blocking, it is recommended that
  KbdClose be issued only while the current thread has the focus.  For
  example:
    KbdGetFocus Wait until focus available on handle 0.
    KbdClose    Close a logical keyboard handle.
    KbdFreeFocus    Give up the focus on handle 0.}
function KbdClose(KbdHandle:word):word;

{Bind the logical keyboard to the physical keyboard.}
{KbdHandle is the default keyboard (0) or a logical keyboard}
{Possible return codes:
    0         NO_ERROR
    439       ERROR_KBD_INVALID_HANDLE
    445       ERROR_KBD_FOCUS_REQUIRED
    464       ERROR_KBD_DETACHED
    504       ERROR_KBD_EXTENDED_SG}
function KbdGetFocus(WaitFlag,KbdHandle:word):word;

{Free the logical-to-physical keyboard bond created by KbdGetFocus.}
{KbdHandle is the default keyboard (0) or a logical keyboard}
{Possible return codes:
    0         NO_ERROR
    439       ERROR_KBD_INVALID_HANDLE
    445       ERROR_KBD_FOCUS_REQUIRED
    464       ERROR_KBD_DETACHED
    504       ERROR_KBD_EXTENDED_SG}
{Remarks:
* KbdFreeFocus may be replaced by issuing KbdRegister. Unlike other keyboard
  subsystem functions, the replaced KbdFreeFocus is called only if there is
  an outstanding focus.}
function KbdFreeFocus(KbdHandle:word):word;

function KbdSynch (WaitFlag:word):word;

function KbdSetFgnd:word;

function KbdGetHWID(var HWID:TKbdHWID;KbdHandle:word):word;

function KbdSetHWID(var HWID:TKbdHWID;KbdHandle:word):word;

function KbdXlate(var TransData:TKbdTrans;KbdHandle:word):word;

function KbdSetCustXt(var XLateTbl;KbdHandle:word):word;


(* Following routines are not supported
   (just have a look in some C header
   file - you probably won't find it there either).
KbdInit (index 2)
KbdLoadInstance (index 6)
KbdSwitchFgnd (index 15)
KbdShellInit (index 16)
KbdFree (index 19)
*)


{***************************************************************************}
implementation
{***************************************************************************}


function KbdRegister(ModuleName,ProcName:PChar;FnMask:longint):word;
external 'EMXWRAP' index 208;
{external 'KBDCALLS' index 8;}

function KbdRegister(ModuleName,ProcName:string;FnMask:longint):word;

var A1:array[0..8] of char;
    A2:array[0..32] of char;

begin
    if byte(ModuleName[0])>8 then byte(ModuleName[0]):=8;
    StrPCopy(@A1,ModuleName);
    if byte(ProcName[0])>32 then byte(ProcName[0]):=32;
    StrPCopy(@A2,ProcName);
    KbdRegister:=KbdRegister(@A1,@A2,FnMask);
end;

function KbdDeRegister:word;
external 'EMXWRAP' index 220;
{external 'KBDCALLS' index 20;}

function KbdCharIn(var Key:TKbdKeyInfo;WaitFlag,KbdHandle:word):word;
external 'EMXWRAP' index 204;
{external 'KBDCALLS' index 4;}

function KbdPeek(var Key:TKbdKeyInfo;KbdHandle:word):word;
external 'EMXWRAP' index 222;
{external 'KBDCALLS' index 22;}

function KbdStringIn(var CharBuf;var pchIn:TStringInBuf;WaitFlag:word;
                                                          KbdHandle:word):word;
external 'EMXWRAP' index 209;
{external 'KBDCALLS' index 9;}

function KbdFlushBuffer(KbdHandle:word):word;
external 'EMXWRAP' index 213;
{external 'KBDCALLS' index 13;}

function KbdSetStatus(var Status:TKbdInfo;KbdHandle:word):word;
external 'EMXWRAP' index 211;
{external 'KBDCALLS' index 11;}

function KbdGetStatus(var Status:TKbdInfo;KbdHandle:word):word;
external 'EMXWRAP' index 210;
{external 'KBDCALLS' index 10;}

function KbdSetCp(usReserved,CodePage,KbdHandle:word):word;
external 'EMXWRAP' index 205;
{external 'KBDCALLS' index 5;}

function KbdGetCp(ulReserved:longint;var CodePage:word;KbdHandle:word):word;
external 'EMXWRAP' index 203;
{external 'KBDCALLS' index 3;}

function KbdOpen(var KbdHandle:word):word;
external 'EMXWRAP' index 223;
{external 'KBDCALLS' index 23;}

function KbdClose(KbdHandle:word):word;
external 'EMXWRAP' index 217;
{external 'KBDCALLS' index 17;}

function KbdGetFocus(WaitFlag,KbdHandle:word):word;
external 'EMXWRAP' index 212;
{external 'KBDCALLS' index 12;}

function KbdFreeFocus(KbdHandle:word):word;
external 'EMXWRAP' index 218;
{external 'KBDCALLS' index 18;}

function KbdSynch (WaitFlag:word):word;
external 'EMXWRAP' index 207;
{external 'KBDCALLS' index 7;}

function KbdSetFgnd:word;
external 'EMXWRAP' index 221;
{external 'KBDCALLS' index 21;}

function KbdGetHWID(var HWID:TKbdHWID;KbdHandle:word):word;
external 'EMXWRAP' index 224;
{external 'KBDCALLS' index 24;}

function KbdSetHWID(var HWID:TKbdHWID;KbdHandle:word):word;
external 'EMXWRAP' index 225;
{external 'KBDCALLS' index 25;}

function KbdXlate(var TransData:TKbdTrans;KbdHandle:word):word;
external 'EMXWRAP' index 214;
{external 'KBDCALLS' index 14;}

function KbdSetCustXt(var XLateTbl;KbdHandle:word):word;
external 'EMXWRAP' index 201;
{external 'KBDCALLS' index 1;}


end.
