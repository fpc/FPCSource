{Set tabsize to 4.}
{****************************************************************************

                           MOUCALLS interface unit
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

unit MouCalls;

{ Interface library to MOUCALLS.DLL (through EMXWRAP.DLL)

Changelog:

    People:

        TH - Tomas Hajny

    Date:           Description of change:              Changed by:

     -              First released version 0.50         TH

Coding style:

    I have tried to use the same coding style as Dani‰l Mantione in unit
    DOSCALLS, although I can't say I would write it the same way otherwise
    (I would write much more spaces myself, at least). Try to use it as well,
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
{return codes / error constants}
    ERROR_MOUSE_NO_DEVICE           =385;
    ERROR_MOUSE_INV_HANDLE          =386;
    ERROR_MOUSE_INV_PARMS           =387;
    ERROR_MOUSE_CANT_RESET          =388;
    ERROR_MOUSE_DISPLAY_PARMS       =389;
    ERROR_MOUSE_INV_MODULE          =390;
    ERROR_MOUSE_INV_ENTRY_PT        =391;
    ERROR_MOUSE_INV_MASK            =392;
    NO_ERROR_MOUSE_NO_DATA          =393;
    NO_ERROR_MOUSE_PTR_DRAWN        =394;
    ERROR_MOUSE_SMG_ONLY            =412;
    ERROR_MOUSE_INVALID_ASCIIZ      =413;
    ERROR_MOUSE_INVALID_MASK        =414;
    ERROR_MOUSE_REGISTER            =415;
    ERROR_MOUSE_DEREGISTER          =416;
    ERROR_MOUSE_INVALID_IOWAIT      =435;
    ERROR_MOU_DETACHED              =466;
    ERROR_MOUSE_NO_CONSOLE          =501;
    ERROR_MOUSE_INVALID_HANDLE      =502;
    ERROR_MOU_EXTENDED_SG           =505;
    ERROR_MOU_NOT_INITIALIZED       =530;
    ERROR_MOUINITREAL_DONE          =531;
    ERROR_MOUSE_CALLER_NOT_SUBSYS   =533;

{constants for FnMask in MouRegister}
    MR_MOUGETNUMBUTTONS =$00000001;
    MR_MOUGETNUMMICKEYS =$00000002;
    MR_MOUGETDEVSTATUS  =$00000004;
    MR_MOUGETNUMQUEEL   =$00000008;
    MR_MOUREADEVENTQUE  =$00000010;
    MR_MOUGETSCALEFACT  =$00000020;
    MR_MOUGETEVENTMASK  =$00000040;
    MR_MOUSETSCALEFACT  =$00000080;
    MR_MOUSETEVENTMASK  =$00000100;
    MR_MOUOPEN          =$00000800;
    MR_MOUCLOSE         =$00001000;
    MR_MOUGETPTRSHAPE   =$00002000;
    MR_MOUSETPTRSHAPE   =$00004000;
    MR_MOUDRAWPTR       =$00008000;
    MR_MOUREMOVEPTR     =$00010000;
    MR_MOUGETPTRPOS     =$00020000;
    MR_MOUSETPTRPOS     =$00040000;
    MR_MOUINITREAL      =$00080000;
    MR_MOUSETDEVSTATUS  =$00100000;

{constants for mouse hot key bits in MouGetHotKey/MouSetHotKey}
    MHK_BUTTON1 =1;
    MHK_BUTTON2 =2;
    MHK_BUTTON3 =4;

{MouGetDevStatus/MouSetDevStatus device status constants}
    MOUSE_QUEUEBUSY         =$0001;
    MOUSE_BLOCKREAD         =$0002;
    MOUSE_FLUSH             =$0004;
    MOUSE_UNSUPPORTED_MODE  =$0008;
    MOUSE_DISABLED          =$0100;
    MOUSE_MICKEYS           =$0200;

{constants for WaitFlag in MouReadEventQue}
    MOU_NOWAIT   =$0000;
    MOU_WAIT     =$0001;

{constants for MouGetEventMask/MouSetEventMask events}
    MOUSE_MOTION                =$0001;
    MOUSE_MOTION_WITH_BN1_DOWN  =$0002;
    MOUSE_BN1_DOWN              =$0004;
    MOUSE_MOTION_WITH_BN2_DOWN  =$0008;
    MOUSE_BN2_DOWN              =$0010;
    MOUSE_MOTION_WITH_BN3_DOWN  =$0020;
    MOUSE_BN3_DOWN              =$0040;

{constants for Status in MouSetDevStatus}
    MOU_DRAW    =0;
    MOU_NODRAW  =1;

    MOU_PELS    =0;
    MOU_MICKEYS =2;

type

(*This should be removed as soon as cardinal arithmetic in FPC works OK.*)
    cardinal=longint;

{unnecessary, just FYI}
    THMOU=word;
    PHMOU=^THMOU;

{record type for MouGetPos/SetPtrPos}
    TPtrLoc=record
        Row:word;
        Col:word;
    end;
    PPtrLoc=^TPtrLoc;

{record type for MouGetShape/SetPtrShape}
    TPtrShape=record
        cb:word;        {length of image buffer in bytes}
        Col:word;       {pointer width in characters or pixels}
        Row:word;       {pointer height in characters or pixels}
        ColHot:word;    {hotspot offset from the left side}
        RowHot:word;    {hotspot offset from the top}
     end;
     PPtrShape=^TPtrShape;

{record type for MouReadEventQue}
(*   #pragma pack(2) ??? *)
    TMouEventInfo=record
        fs:word;        {event bits}
        Time:cardinal;  {event timestamp - unique number of milliseconds}
        Row:integer;    {pointer current row position}
        Col:integer;    {pointer current column position}
    end;
    PMouEventInfo=^TMouEventInfo;

{record type for MouGetNumQueEl}
    TMouQueInfo=record
        cEvents:word;       {number of elements in event queue}
        cmaxEvents:word;    {maximum queue size in elements}
    end;
    PMouQueInfo=^TMouQueInfo;

{record type for MouGetScaleFact/MouSetScaleFact}
    TScaleFact=record
        RowScale:word;  {scaling factor of current row}
        ColScale:word;  {scaling factor of current column}
    end;
    PScaleFact=^TScaleFact;

{record type for MouRemovePtr}
    TNoPtrRect=record
        Row:word;   {row of the top of the rectangle}
        Col:word;   {column of the left edge}
        cRow:word;  {row of the bottom}
        cCol:word;  {column of the right edge}
    end;
    PNoPtrRect=^TNoPtrRect;

    TThreshold=record
        Length:word;
        Level1:word;    {first movement level}
        Lev1Mult:word;  {first level multiplier}
        Level2:word;    {second movement level}
        Lev2Mult:word;  {second level multiplier}
    end;
    PThreshold=^TThreshold;


function MouRegister(ModuleName,ProcName:PChar;FnMask:cardinal):word;

function MouRegister(ModuleName,ProcName:string;FnMask:cardinal):word;

function MouDeRegister:word;

function MouFlushQue(MouHandle:word):word;

function MouGetPtrPos(var MouPtr:TPtrLoc;MouHandle:word):word;

function MouSetPtrPos(var MouPtr:TPtrLoc;MouHandle:word):word;

function MouSetPtrShape(ImageBuf:pointer;var ImageInfo:TPtrShape;
                                                          MouHandle:word):word;

function MouGetPtrShape(ImageBuf:pointer;var ImageInfo:TPtrShape;
                                                          MouHandle:word):word;

function MouGetDevStatus(var Status:word;MouHandle:word):word;

function MouGetNumButtons(var ButtonCount:word;MouHandle:word):word;

function MouGetNumMickeys(var MickeyCnt:word;MouHandle:word):word;

function MouReadEventQue(var Event:TMouEventInfo;var WaitFlag:word;
                                                          MouHandle:word):word;

function MouGetNumQueEl(var MouseQInfo:TMouQueInfo;MouHandle:word):word;

function MouGetEventMask(var EventMask:word;MouHandle:word):word;

function MouSetEventMask(var EventMask:word;MouHandle:word):word;

function MouGetScaleFact(var Scale:TScaleFact;MouHandle:word):word;

function MouSetScaleFact(var Scale:TScaleFact;MouHandle:word):word;

function MouOpen(DriverName:PChar;var MouHandle:word):word;

function MouOpen(DriverName:string;var MouHandle:word):word;

function MouClose(MouHandle:word):word;

function MouRemovePtr(var ProtectArea:TNoPtrRect;MouHandle:word):word;

function MouDrawPtr(MouHandle:word):word;

function MouSetDevStatus(var Status:word;MouHandle:word):word;

function MouInitReal(DriverName:PChar):word;

function MouInitReal(DriverName:string):word;

function MouSynch(WaitFlag:word):word;

function MouGetThreshold(var MouThreshold:TThreshold;MouHandle:word):word;

function MouSetThreshold(var MouThreshold:TThreshold;MouHandle:word):word;

(*
following two functions are undocumented and not present within C header files:

function MouGetHotKey(var ButtonBits:word;MouHandle:word):word;

function MouSetHotKey(var ButtonBits:word;MouHandle:word):word;
*)

(* Following routines are not supported
   (just have a look in some C header
   file - you probably won't find it there either).
MouFree (index 4)
MouShellInit (index 12)
*)

{***************************************************************************}
implementation
{***************************************************************************}


function MouRegister(ModuleName,ProcName:PChar;FnMask:cardinal):word;
external 'EMXWRAP' index 324;
{external 'MOUCALLS' index 24;}

function MouRegister(ModuleName,ProcName:string;FnMask:cardinal):word;
begin
    if byte(ModuleName[0])>8 then byte(ModuleName[0]):=8;
    ModuleName[Succ(byte(ModuleName[0]))]:=#0;
    if byte(ProcName[0])>32 then byte(ProcName[0]):=32;
    ProcName[Succ(byte(ProcName[0]))]:=#0;
    MouRegister:=MouRegister(@ModuleName[1],@ProcName[1],FnMask);
end;

function MouDeRegister:word;
external 'EMXWRAP' index 314;
{external 'MOUCALLS' index 14;}

function MouFlushQue(MouHandle:word):word;
external 'EMXWRAP' index 307;
{external 'MOUCALLS' index 7;}

function MouGetPtrPos(var MouPtr:TPtrLoc;MouHandle:word):word;
external 'EMXWRAP' index 319;
{external 'MOUCALLS' index 19;}

function MouSetPtrPos(var MouPtr:TPtrLoc;MouHandle:word):word;
external 'EMXWRAP' index 321;
{external 'MOUCALLS' index 21;}

function MouSetPtrShape(ImageBuf:pointer;var ImageInfo:TPtrShape;
                                                          MouHandle:word):word;
external 'EMXWRAP' index 302;
{external 'MOUCALLS' index 2;}

function MouGetPtrShape(ImageBuf:pointer;var ImageInfo:TPtrShape;
                                                          MouHandle:word):word;
external 'EMXWRAP' index 301;
{external 'MOUCALLS' index 1;}

function MouGetDevStatus(var Status:word;MouHandle:word):word;
external 'EMXWRAP' index 322;
{external 'MOUCALLS' index 22;}

function MouGetNumButtons(var ButtonCount:word;MouHandle:word):word;
external 'EMXWRAP' index 308;
{external 'MOUCALLS' index 8;}

function MouGetNumMickeys(var MickeyCnt:word;MouHandle:word):word;
external 'EMXWRAP' index 303;
{external 'MOUCALLS' index 3;}

function MouReadEventQue(var Event:TMouEventInfo;var WaitFlag:word;
                                                          MouHandle:word):word;
external 'EMXWRAP' index 320;
{external 'MOUCALLS' index 20;}

function MouGetNumQueEl(var MouseQInfo:TMouQueInfo;MouHandle:word):word;
external 'EMXWRAP' index 313;
{external 'MOUCALLS' index 13;}

function MouGetEventMask(var EventMask:word;MouHandle:word):word;
external 'EMXWRAP' index 315;
{external 'MOUCALLS' index 15;}

function MouSetEventMask(var EventMask:word;MouHandle:word):word;
external 'EMXWRAP' index 316;
{external 'MOUCALLS' index 16;}

function MouGetScaleFact(var Scale:TScaleFact;MouHandle:word):word;
external 'EMXWRAP' index 306;
{external 'MOUCALLS' index 6;}

function MouSetScaleFact(var Scale:TScaleFact;MouHandle:word):word;
external 'EMXWRAP' index 311;
{external 'MOUCALLS' index 11;}

function MouOpen(DriverName:PChar;var MouHandle:word):word;
external 'EMXWRAP' index 317;
{external 'MOUCALLS' index 17;}

function MouOpen(DriverName:string;var MouHandle:word):word;

var B:byte;

begin
    B:=byte(DriverName[0]);
    if B=0 then MouOpen:=MouOpen(nil,MouHandle) else
    begin
        if B<>255 then
        begin
            DriverName[Succ(B)]:=#0;
            MouOpen:=MouOpen(@DriverName[1],MouHandle);
        end else
        begin
            Move(DriverName[1],DriverName[0],B);
            DriverName[B]:=#0;
            MouOpen:=MouOpen(@DriverName,MouHandle);
        end;
    end;
end;

function MouClose(MouHandle:word):word;
external 'EMXWRAP' index 309;
{external 'MOUCALLS' index 9;}

function MouRemovePtr(var ProtectArea:TNoPtrRect;MouHandle:word):word;
external 'EMXWRAP' index 318;
{external 'MOUCALLS' index 18;}

function MouDrawPtr(MouHandle:word):word;
external 'EMXWRAP' index 326;
{external 'MOUCALLS' index 26;}

function MouSetDevStatus(var Status:word;MouHandle:word):word;
external 'EMXWRAP' index 326;
{external 'MOUCALLS' index 26;}

function MouInitReal(DriverName:PChar):word;
external 'EMXWRAP' index 327;
{external 'MOUCALLS' index 27;}

function MouInitReal(DriverName:string):word;

var B:byte;

begin
    B:=byte(DriverName[0]);
    if B=0 then MouInitReal:=MouInitReal(nil) else
    begin
        if B<>255 then
        begin
            DriverName[Succ(B)]:=#0;
            MouInitReal:=MouInitReal(@DriverName[1]);
        end else
        begin
            Move(DriverName[1],DriverName[0],B);
            DriverName[B]:=#0;
            MouInitReal:=MouInitReal(@DriverName);
        end;
    end;
end;

function MouSynch(WaitFlag:word):word;
external 'EMXWRAP' index 323;
{external 'MOUCALLS' index 23;}

function MouGetThreshold(var MouThreshold:TThreshold;MouHandle:word):word;
external 'EMXWRAP' index 329;
{external 'MOUCALLS' index 29;}

function MouSetThreshold(var MouThreshold:TThreshold;MouHandle:word):word;
external 'EMXWRAP' index 330;
{external 'MOUCALLS' index 30;}


(*
following two functions are undocumented and not present within C header files:

function MouGetHotKey(var ButtonBits:word;MouHandle:word):word;
external 'MOUCALLS' index 4;

function MouSetHotKey(var ButtonBits:word;MouHandle:word):word;
external 'MOUCALLS' index 10;
*)


end.
