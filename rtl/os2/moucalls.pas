{Set tabsize to 4.}
{****************************************************************************


                           MOUCALLS interface unit
                     Free Pascal Runtime Library for OS/2
                   Copyright (c) 1999-2000 by Florian Kl„mpfl
                    Copyright (c) 1999-2000 by Daniel Mantione
                      Copyright (c) 1999-2000 by Tomas Hajny

 The Free Pascal runtime library is distributed under the Library GNU Public
 License v2. So is this unit. The Library GNU Public License requires you to
 distribute the source code of this unit with any product that uses it.
 Because the EMX library isn't under the LGPL, we grant you an exception to
 this, and that is, when you compile a program with the Free Pascal Compiler,
 you do not need to ship source code with that program, AS LONG AS YOU ARE
 USING UNMODIFIED CODE! If you modify this code, you MUST change the next
 line:

 <This is an official, unmodified Free Pascal source code file.>

 Send us your modified files, we can work together if you want!

 Free Pascal is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 Library GNU General Public License for more details.

 You should have received a copy of the Library GNU General Public License
 along with Free Pascal; see the file COPYING.LIB.  If not, write to
 the Free Software Foundation, 59 Temple Place - Suite 330,
 Boston, MA 02111-1307, USA.

****************************************************************************}

unit MouCalls;

{ Interface library to MOUCALLS.DLL (through EMXWRAP.DLL; C calling convention
  - cdecl - must be used for EMXWRAP, whereas direct MOUCALLS calls would need
  16-bit Pascal calling convention with thunking instead).

Variant records and aliases for some record types created to maintain highest
possible level of compatibility with other existing OS/2 compilers.

Changelog:

    People:

        TH - Tomas Hajny (xhajt03@mbox.vol.cz on Internet)

    Date:           Description of change:              Changed by:

     -              First released version 1.0          TH

Coding style:

    I have tried to use the same coding style as Daniel Mantione in unit
    DOSCALLS, although I can't say I would write it the same way otherwise
    (I would write much more spaces myself, at least). Try to use it as well,
    please. Original note by Daniel Mantione follows:


    It may be well possible that coding style feels a bit strange to you.
    Nevertheless I friendly ask you to try to make your changes not look all
    to different. To make life easier, set your IDE to use tab characters,
    turn optimal fill, autoindent and backspace unindents on and set a
    tabsize of 4.}

{***************************************************************************}
interface
{***************************************************************************}

{$IFDEF FPC}
    {$PACKRECORDS 1}
{$ENDIF FPC}

const
{return codes / error constants (those marked with * shouldn't occur under
normal conditions)}
    NO_ERROR                        =  0;
    Error_Invalid_Parameter         = 87;
    ERROR_SEM_TIMEOUT               =121;
    ERROR_MOUSE_NO_DEVICE           =385;
    ERROR_MOUSE_INV_HANDLE          =386; {*}
    ERROR_MOUSE_INV_PARMS           =387;
    ERROR_MOUSE_CANT_RESET          =388; {*}
    ERROR_MOUSE_DISPLAY_PARMS       =389; {*}
    ERROR_MOUSE_INV_MODULE          =390;
    ERROR_MOUSE_INV_ENTRY_PT        =391; {*}
    ERROR_MOUSE_INV_MASK            =392; {*}
    NO_ERROR_MOUSE_NO_DATA          =393;
    NO_ERROR_MOUSE_PTR_DRAWN        =394; {*}
    ERROR_MOUSE_SMG_ONLY            =412;
    ERROR_MOUSE_INVALID_ASCIIZ      =413;
    ERROR_MOUSE_INVALID_MASK        =414;
    ERROR_MOUSE_REGISTER            =415;
    ERROR_MOUSE_DEREGISTER          =416;
    ERROR_MOUSE_INVALID_IOWAIT      =435; {*}
    ERROR_MOU_DETACHED              =466;
    ERROR_MOUSE_NO_CONSOLE          =501;
    ERROR_MOUSE_INVALID_HANDLE      =502; {*}
    ERROR_MOU_EXTENDED_SG           =505;
    ERROR_MOU_NOT_INITIALIZED       =530; {*}
    ERROR_MOUINITREAL_DONE          =531; {*}
    ERROR_MOUSE_CALLER_NOT_SUBSYS   =533; {*}

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
    MOUSE_QUEUEBUSY         =$0001; {event queue busy with I/O}
    MOUSE_BLOCKREAD         =$0002; {block read in progress}
    MOUSE_FLUSH             =$0004; {flush in progress}
    MOUSE_UNSUPPORTED_MODE  =$0008; {pointer draw routine disabled}
                                    {by unsupported mode          }
    MOUSE_DISABLED          =$0100; {drawing operations for pointer}
                                    {draw routine are disabled     }
    MOUSE_MICKEYS           =$0200; {mouse data returned in mickeys, not pels}

{constants for WaitFlag in MouReadEventQue and MouSynch}
    MOU_NOWAIT   =$0000;    {MouReadEventQue: don't wait for data    }
                            {   on empty queue (return a null record)}
                            {MouSynch: control immediately}
                            {   returned to requestor     }
    MOU_WAIT     =$0001;    {MouReadEventQue: wait for data on empty queue}
                            {MouSynch: requestor waits until}
                            {   mouse device driver is free }

{constants for MouGetEventMask/MouSetEventMask events}
    MOUSE_MOTION                =$0001; {report mouse motion events with}
                                        {no button press/release events }
    MOUSE_MOTION_WITH_BN1_DOWN  =$0002; {report button 1 press/release}
                                        {events, with mouse motion    }
    MOUSE_BN1_DOWN              =$0004; {report button 1 press/release}
                                        {events, without mouse motion }
    MOUSE_MOTION_WITH_BN2_DOWN  =$0008; {report button 2 press/release}
                                        {events, with mouse motion    }
    MOUSE_BN2_DOWN              =$0010; {report button 2 press/release}
                                        {events, without mouse motion }
    MOUSE_MOTION_WITH_BN3_DOWN  =$0020; {report button 3 press/release}
                                        {events, with mouse motion    }
    MOUSE_BN3_DOWN              =$0040; {report button 3 press/release}
                                        {events, without mouse motion }

{constants for Status in MouSetDevStatus}
    MOU_DRAW    =0;
    MOU_NODRAW  =1;

    MOU_PELS    =0;
    MOU_MICKEYS =2;

type
{unnecessary, just FYI}
    THMOU=word;
    PHMOU=^THMOU;

{record type for MouGetPtrPos/MouSetPtrPos}
    TPtrLoc=record
        Row,        {mouse pointer row coordinate (in pels or characters)}
        Col:word;   {mouse pointer column coordinate (in pels or characters)}
    end;
    PPtrLoc=^TPtrLoc;
    PtrLoc=TPtrLoc;

{record type for MouGetPtrShape/MouSetPtrShape}
    TPtrShape=record
        cb,             {length of image buffer in bytes}
        Col,            {pointer width in characters (in text}
                        {modes; always 1) or pixels (>= 1)   }
        Row,            {pointer height in characters (in text}
                        {modes; always 1) or pixels (>=1)     }
        ColHot,         {hotspot offset from the left}
                        {side, in characters or pels }
                        {(must be 0 in text modes)   }
        RowHot:word;    {hotspot offset from the top,}
                        {in characters or pels       }
                        {(must be 0 in text modes)   }
     end;
     PPtrShape=^TPtrShape;
     PtrShape=TPtrShape;

{record type for MouReadEventQue}
(*   #pragma pack(2) ??? *)
    TMouEventInfo=record
        fs:word;        {event bits (state of the mouse at the time     }
                        {of the event) - see MOUSE_MOTION,              }
                        {MOUSE_MOTION_WITH_BN1_DOWN, MOUSE_BN1_DOWN,    }
                        {MOUSE_MOTION_WITH_BN2_DOWN, MOUSE_BN2_DOWN,    }
                        {MOUSE_MOTION_WITH_BN3_DOWN and MOUSE_BN3_DOWN  }
                        {constants (other bits reserved and set to zero)}
        Time:cardinal;  {event timestamp - unique number of milliseconds}
                        {(since the system was started                  }
        Row,            {pointer current row position (absolute or relative)}
        Col:integer;    {pointer current column position}
                        {(absolute or relative)         }
    end;
    PMouEventInfo=^TMouEventInfo;
    MouEventInfo=TMouEventInfo;

{record type for MouGetNumQueEl}
    TMouQueInfo=record
        cEvents,            {current number of elements in event}
                            {queue, between 0 and cmaxEvents    }
        cmaxEvents:word;    {maximum queue size in elements, as specified    }
                            {in the QSIZE=NN parameter in DEVICE=MOUSExxx.SYS}
                            {statement in CONFIG.SYS                         }
    end;
    PMouQueInfo=^TMouQueInfo;
    MouQueInfo=TMouQueInfo;

{record type for MouGetScaleFact/MouSetScaleFact}
    TScaleFact=record
        RowScale,       {current row scaling factor}
        ColScale:word;  {current column scaling factor}
    end;
    PScaleFact=^TScaleFact;
    ScaleFact=TScaleFact;

{record type for MouRemovePtr}
    TNoPtrRect=record
        Row,        {upper row of the rectangle (pels or characters)}
        Col,        {column of the left edge (pels or characters)}
        cRow,       {bottom row of the rectangle (pels or characters)}
        cCol:word;  {column of the right edge (pels or characters)}
    end;
    PNoPtrRect=^TNoPtrRect;
    NoPtrRect=TNoPtrRect;

    TThreshold=record
        case boolean of
        false:(Length:word;     {length of data in bytes}
        Level1,                 {first movement level}
        Lev1Mult,               {first level multiplier}
        Level2,                 {second movement level}
        Lev2Mult:word);         {second level multiplier}
        true:(aLength:word);
    end;
    PThreshold=^TThreshold;
    Threshold=TThreshold;


{Register a mouse subsystem within a session.}
{ModuleName is name of the dynamic link module (the maximum length
is 8 characters - plus the final #0 character in the PChar version of this
call), ProcName is the dynamic link entry point name of a routine that receives
control when any of the registered functions are called (the maximum length
is 32 bytes - plus the final #0 character in the PChar version of this call),
FnMask is a mask of bits, where each bit set to 1 identifies a mouse function
being registered - see MR_MOU* constants.}
{Possible return codes:
    0         NO_ERROR
    385       ERROR_MOUSE_NO_DEVICE
    413       ERROR_MOUSE_INVALID_ASCIIZ
    414       ERROR_MOUSE_INVALID_MASK
    415       ERROR_MOUSE_REGISTER
    466       ERROR_MOU_DETACHED
    505       ERROR_MOU_EXTENDED_SG}
{Remarks:
* The Base Mouse Subsystem is the default mouse subsystem. There can be only
  one MouRegister outstanding for each session without an intervening
  MouDeRegister. MouDeRegister must be issued by the same process that issued
  MouRegister.
* When any registered function is called, control is routed to EntryName.
  When this routine is entered, four additional values are pushed onto the
  stack. The first is the index number (Word) of the function being called.
  The second is a near pointer (Word). The third is the caller's DS register
  (Word). The fourth is the return address (DWord) to the mouse router. For
  example, if MouGetNumMickeys were called and control routed to EntryName,
  the stack would appear as if the following instructions were executed:

  PUSH@ WORD    MickeyCnt
  PUSH  WORD    MouHandle
  CALL  FAR     MouGetNumMickeys
  PUSH  WORD    Function Code
  CALL  NEAR    Entry point in Mouse Router
  PUSH  DS
  CALL  FAR     EntryName


* When a registered function returns to the Mouse Router, AX is interpreted
  as follows:
    AX = 0 - no error, do not invoke the Base Mouse Subsystem routine,
            return AX = 0
    AX = -1 - invoke the BaseMouse Subsystem routine, return AX = return code
            from the Base Mouse Subsystem
    AX = error (if not 0 or -1) - do not invoke the Base Mouse Subsystem
            Routine, return AX = error
* When the mouse router receives a mouse call, it routes it to the Base Mouse
  Subsystem unless an application or other mouse subsystem has previously
  issued MouRegister for that call. If the call was registered, the subsystem
  is entered at the EntryName specified, and provided with the applicable
  function code.
* The registered function mask is used to determine whether a requested
  function is performed by the registered mouse subsystem or default to the
  Base Mouse Subsystem.
* The following list shows the relationship of the mouse API calls and the
  Function Code passed to either the Base Mouse Subsystem or a registered
  mouse subsystem.

  MOU API calls               Function Code
  MouGetNumButtons            00h
  MouGetNumMickeys            01h
  MouGetDevStatus             02h
  MouGetNumQueEl              03h
  MouReadEventQue             04h
  MouGetScaleFact             05h
  MouGetEventMask             06h
  MouSetScaleFact             07h
  MouSetEventMask             08h
  Reserved                    09h
  Reserved                    0Ah
  MouOpen                     0Bh
  MouClose                    0Ch
  MouGetPtrShape              0Dh
  MouSetPtrShape              0Eh
  MouDrawPtr                  0Fh
  MouRemovePtr                10h
  MouGetPtrPos                11h
  MouSetPtrPos                12h
  MouInitReal                 13h
  MouFlushQue                 14h
  MouSetDevStatus             15h
* A registered mouse sybsystem must leave the stack, on exit, in the exact
  state it was received.}
function MouRegister(ModuleName,ProcName:PChar;FnMask:cardinal):word; cdecl;
function MouRegister(ModuleName,ProcName:string;FnMask:cardinal):word;

{Deregister a mouse subsystem previously registered within a session.}
{Possible return codes:
    0         NO_ERROR
    385       ERROR_MOUSE_NO_DEVICE
    416       ERROR_MOUSE_DEREGISTER
    466       ERROR_MOU_DETACHED
    505       ERROR_MOU_EXTENDED_SG}
{Remarks:
* The process that issued the MouRegister must release the session
  (by a MouDeRegister call) from the registered subsystem before another PID
  may issue MouRegister.
* The process that issued the MouRegister is the only process that may
  issue MouDeRegister against the currently registered subsystem.
* After the owning process has released the subsystem with a MouDeRegister
  call, any other process in the session may issue a MouRegister and therefore
  modify the mouse support for the entire session.}
function MouDeRegister:word; cdecl;

{Direct the mouse driver to flush (empty) the mouse event queue and the monitor
chain data for the session.}
{MouHandle is the mouse device handle from a previous MouOpen call.}
{Possible return codes:
    0         NO_ERROR
    385       ERROR_MOUSE_NO_DEVICE
    466       ERROR_MOU_DETACHED
    501       ERROR_MOUSE_NO_CONSOLE
    505       ERROR_MOU_EXTENDED_SG}
function MouFlushQue(MouHandle:word):word; cdecl;

{Query the mouse driver to determine the current row and column coordinate
position of the mouse pointer.}
{Mouse pointer position returned in MouPtr, MouHandle is the mouse device
handle from a previous MouOpen call.}
{Possible return codes:
    0         NO_ERROR
    385       ERROR_MOUSE_NO_DEVICE
    466       ERROR_MOU_DETACHED
    501       ERROR_MOUSE_NO_CONSOLE
    505       ERROR_MOU_EXTENDED_SG}
{Remarks:
* For a text window (VIO) application, the text window is a view on the
  larger logical video buffer (LVB). The mouse pointer can be outside that
  view and still be within the extent of the LVB. MouGetPtrPos then returns
  the coordinates of the cell under the mouse pointer. If the pointer is
  outside the LVB image extent, the coordinates of the nearest LVB cell are
  returned. In either case, the LVB is scrolled until the reported LVB cell
  appears within the view window.}
function MouGetPtrPos(var MouPtr:TPtrLoc;MouHandle:word):word; cdecl;

{Direct the mouse driver to set a new row and column coordinate position for
the mouse pointer.}
{MouPtr contains the new pointer coordinates, MouHandle is the mouse device
handle from a previous MouOpen call.}
{Possible return codes:
    0         NO_ERROR
    385       ERROR_MOUSE_NO_DEVICE
    387       ERROR_MOUSE_INV_PARMS
    466       ERROR_MOU_DETACHED
    501       ERROR_MOUSE_NO_CONSOLE
    505       ERROR_MOU_EXTENDED_SG}
{Remarks:
* The application must ensure that the coordinate position specified conforms
  to the current display mode orientation for the session. Pel values must
  be used for graphics modes and character values for text modes.
* This function has no effect on the display's current collision area
  definition as specified by the MouDrawPtr call. If the mouse pointer image
  is directed into a defined collision area, the pointer image is not drawn
  until either the pointer is moved outside the collision area or the collision
  area is released by the MouDrawPtr call.}
function MouSetPtrPos(const MouPtr:TPtrLoc;MouHandle:word):word; cdecl;

{Set the pointer shape and size to be used as the mouse device pointer image
for all applications in a session.}
{ImageBuf contains the bit image used by the mouse device driver as the pointer
shape for that session. The buffer consists of AND and XOR pointer masks
in a format meaningful to the pointer draw device driver (see remarks bellow),
ImageInfo contains the necessary data for the pointer draw device driver to
build a row-by-column image for each bit plan for the current display mode,
MouHandle is the mouse device handle from a previous MouOpen call.}
{Possible return codes:
    0         NO_ERROR
    385       ERROR_MOUSE_NO_DEVICE
    387       ERROR_MOUSE_INV_PARMS
    466       ERROR_MOU_DETACHED
    501       ERROR_MOUSE_NO_CONSOLE
    505       ERROR_MOU_EXTENDED_SG}
{Remarks:
* An application passes a data image to the mouse device driver that the mouse
  driver applies to the screen whenever the logical pointer position is not
  located in the application-defined collision area. The application
  synchronizes use of the screen with the mouse driver by way of MouRemovePtr
  and MouDrawPtr.
* The pointer shape is dependent on the display device driver used to support
  the display device. OS/2 supports text and graphics modes. These modes are
  restricted to modes 0 through 7, depending on the display device. Character
  modes (modes 0, 1, 2, 3, and 7) support the pointer cursor only as a reverse
  block character. This reverse block character has a character height
  and width equal to 1.
* The pointer shape is mapped by the Pointer Draw Device Driver and determined
  completely by the application. The height and width may vary from 1 through
  the pel size of the display screen. For restrictions concerning the Pointer
  Draw Device Driver, see IBM Operating System/2 Version 1.2 I/O Subsystems And
  Device Support Volume 1.
* For CGA compatible text modes (0, 1, 2, and 3) the following describes
  the AND and XOR pointer mask bit definitions for each character cell
  of the masks. Bit values are:
    Bit       Description
    15        Blinking
    14-12     Background color
    11        Intensity
    10-8      Foreground color
    7-0       Character
* For other custom displays and for the extended modes of the EGA attachment,
  it is possible to set the display to modes that require multiple bit planes.
  In these cases, the area sized by the row and column limits must be repeated
  for each bit plane supported in that mode. Consequently, the calling process
  must supply enough data to allow the mouse device driver to draw the pointer
  shape on all currently supported bit planes in that session. For text modes,
  row and column offset must equal 0.}
function MouSetPtrShape(var ImageBuf;var ImageInfo:TPtrShape;
                                                   MouHandle:word):word; cdecl;
function MouSetPtrShape(ImageBuf:pointer;var ImageInfo:TPtrShape;
                                                   MouHandle:word):word; cdecl;

{Get (copy) the mouse pointer shape for the session.}
{The pointer bit image is returned in ImageBuf (see MouSetPtrShape description
for information about the resulting content of this buffer), the size of the
pointer image buffer must be supplied in ImageInfo.cb (if the value is too
small, the true length is placed in this field and an error is returned),
on return, ImageInfo is filled with mouse pointer information, MouHandle is
the mouse device handle from a previous MouOpen call.}
{Possible return codes:
    0         NO_ERROR
    385       ERROR_MOUSE_NO_DEVICE
    387       ERROR_MOUSE_INV_PARMS
    466       ERROR_MOU_DETACHED
    501       ERROR_MOUSE_NO_CONSOLE
    505       ERROR_MOU_EXTENDED_SG}
{Remarks:
* The application passes a parameter list with the same meaning as defined
  for MouSetPtrShape to the mouse device driver. The mouse device driver
  copies the parameters that describe the pointer shape and attributes into
  the pointer definition control block pointed to by the PtrDefRec parameter.
  The word 0 (buffer length = cb) pointer definition record parameter field
  must contain the size in bytes of the application buffer where the device
  driver is to insert the session's pointer image. All other words in
  the parameter list are returned to the application by MouGetPtrShape.
* For all OS/2 system-supported modes, size of the pointer image buffer
  is specified in bytes and is equal to:
    1) Mono & Text Modes:
        For text mode, height and width must be 1, so length is always 4.

        size = (height in chars) * (width in chars) * 2 * 2 = 1 * 1 * 2 * 2 = 4

    2) Graphics Mode:
        Width-in-pels must be a multiple of 8.

        size = (height in pels) * (width in pels) * (bits per pel) * 2 / 8

        a) Modes 4 and 5 (320 X 200)

            size = (height) * (width) * 2 * 2 / 8

        b) Mode 6 (640 X 200)

            size = (height) * (width) * 1 * 2 / 8
* If the buffer size is insufficient, the cb field contains the actual size
  in bytes of the returned pointer image.
* The pointer shape may be set by the application with MouSetPtrShape or may
  be the default image provided by the installed Pointer Device Driver.}
function MouGetPtrShape(var ImageBuf;var ImageInfo:TPtrShape;
                                                   MouHandle:word):word; cdecl;
function MouGetPtrShape(ImageBuf:pointer;var ImageInfo:TPtrShape;
                                                   MouHandle:word):word; cdecl;

{Return status flags for the installed mouse device driver.}
{The current status flag settings for the installed mouse device driver are
returned in Status - see MOUSE_QUEUEBUSY, MOUSE_BLOCKREAD, MOUSE_FLUSH,
MOUSE_UNSUPPORTED_MODE, MOUSE_DISABLED and MOUSE_MICKEYS constants (other bits
are reserved and set to zero), MouHandle is the mouse device handle from
a previous MouOpen call.}
{Possible return codes:
    0         NO_ERROR
    385       ERROR_MOUSE_NO_DEVICE
    466       ERROR_MOU_DETACHED
    501       ERROR_MOUSE_NO_CONSOLE
    505       ERROR_MOU_EXTENDED_SG}
function MouGetDevStatus(var Status:word;MouHandle:word):word; cdecl;

{Return the number of buttons supported on the installed mouse driver.}
{Number of physical buttons (1..3) returned in ButtonCount, MouHandle is
the mouse device handle from a previous MouOpen call.}
{Possible return codes:
    0         NO_ERROR
    385       ERROR_MOUSE_NO_DEVICE
    466       ERROR_MOU_DETACHED
    501       ERROR_MOUSE_NO_CONSOLE
    505       ERROR_MOU_EXTENDED_SG}
function MouGetNumButtons(var ButtonCount:word;MouHandle:word):word; cdecl;

{Return the number of mickeys in each centimeter for the installed mouse
driver.}
{Number of physical mouse motion units (mickeys) in each centimeter (a constant
based upon the attached mouse device) returned in MickeyCnt, MouHandle is
the mouse device handle from a previous MouOpen call.}
{Possible return codes:
    0         NO_ERROR
    385       ERROR_MOUSE_NO_DEVICE
    466       ERROR_MOU_DETACHED
    501       ERROR_MOUSE_NO_CONSOLE
    505       ERROR_MOU_EXTENDED_SG}
function MouGetNumMickeys(var MickeyCnt:word;MouHandle:word):word; cdecl;

{Read an event from the mouse device FIFO event queue.}
{The mouse event queue is returned in Event, WaitFlag determines the action to
take when MouReadEventQue is issued and no event is available (the mouse event
queue is empty) - see MOU_NOWAIT and MOU_WAIT constants, MouHandle is the mouse
device handle from a previous MouOpen call.}
{Possible return codes:
    0         NO_ERROR
    385       ERROR_MOUSE_NO_DEVICE
    387       ERROR_MOUSE_INV_PARMS
    393       ERROR_MOUSE_NO_DATA
    466       ERROR_MOU_DETACHED
    501       ERROR_MOUSE_NO_CONSOLE
    505       ERROR_MOU_EXTENDED_SG}
{Remarks:
* The types of queued events are directly affected by the current value of
  the Mouse EventMask. MouSetEventMask is used to indicate the types of events
  desired, and MouGetEventMask is used to query the current value of the mask.
  Refer to these functions for further explanation of the masking of events.
  Recognition of the mouse transition depends on the use of MouState returned
  in the event record. The application should focus on bit transitions that
  occur in this word. It is important to properly set the event mask with
  MouSetEventMask for reporting the state transitions.
  Event.fs reports the state of the mouse that resulted from the action that
  caused the event. The action can be pressing or releasing a button, and/or
  moving the mouse. All status is given, regardless of the EventMask that
  was used to determine whether or not to report the event.
  For example, assume the EventMask indicates that the application wishes only
  button 1 events. The EventMask has only bits 1 and 2 set in this case. Also
  assume the current state of the mouse is no buttons down, and mouse is not
  moving. At this point, button 1 is pressed causing an event; the status shows
  button 1 down (bit 2 set). Next the mouse is moved, thereby causing more
  events; status shows bit 1 set. Finally, mouse is stopped and button 1 is
  released. The event shows status with no bits set.
  Next, button 2 is pressed. No event occurs. Mouse is then moved; again,
  no event. Then, while mouse is still in motion, button 1 is pressed; an event
  is generated with bits 1 and 3 set in the state word. While mouse is still
  in motion, both buttons are released. Because button 1 changes states,
  an event occurs. The state word has bit 0 set. Finally, mouse is stopped.
  No event occurs, again because no button 1 transition has taken place.
* The Event.Row and Event.Col fields may contain either absolute display
  coordinates or relative mouse motion in mickeys. See MouSetDevStatus for
  additional information.}
function MouReadEventQue(var Event:TMouEventInfo;var WaitFlag:word;
                                                   MouHandle:word):word; cdecl;

{Return the current status for the mouse device driver event queue.}
{Mouse queue status returned in MouseQInfo, MouHandle is the mouse device
handle from a previous MouOpen call.}
{Possible return codes:
    0         NO_ERROR
    385       ERROR_MOUSE_NO_DEVICE
    466       ERROR_MOU_DETACHED
    501       ERROR_MOUSE_NO_CONSOLE
    505       ERROR_MOU_EXTENDED_SG}
function MouGetNumQueEl(var MouseQInfo:TMouQueInfo;MouHandle:word):word; cdecl;

{Return the current value of the mouse event queue mask.}
{The current mouse device driver's event mask (as previously set by
MouSetEventMask call) is returned in EventMask - see MOUSE_MOTION,
MOUSE_MOTION_WITH_BN1_DOWN, MOUSE_BN1_DOWN, MOUSE_MOTION_WITH_BN2_DOWN,
MOUSE_BN2_DOWN, MOUSE_MOTION_WITH_BN3_DOWN and MOUSE_BN3_DOWN constants (other
bits are reserved and set to zero, MouHandle is the mouse device handle from
a previous MouOpen call.}
{Possible return codes:
    0         NO_ERROR
    385       ERROR_MOUSE_NO_DEVICE
    466       ERROR_MOU_DETACHED
    501       ERROR_MOUSE_NO_CONSOLE
    505       ERROR_MOU_EXTENDED_SG}
{Remarks:
* Buttons are logically numbered from left to right.}
function MouGetEventMask(var EventMask:word;MouHandle:word):word; cdecl;

{Assign a new event mask to the current mouse device driver.}
{EventMask contains the mask indicating what mouse events are to be placed on
the event queue (see MouReadEventQue) and which events are to be ignored - see
MOUSE_MOTION, MOUSE_MOTION_WITH_BN1_DOWN, MOUSE_BN1_DOWN,
MOUSE_MOTION_WITH_BN2_DOWN, MOUSE_BN2_DOWN, MOUSE_MOTION_WITH_BN3_DOWN and
MOUSE_BN3_DOWN constants (other bits reserved and set to zero; a bit set to
zero means that the associated type of event is not reported to the
application, mouse buttons are always numbered from left to right - when the
mouse is properly positioned for use, the left-hand button is button 1),
MouHandle is the mouse device handle from a previous MouOpen call.}
{Possible return codes:
    0         NO_ERROR
    385       ERROR_MOUSE_NO_DEVICE
    466       ERROR_MOU_DETACHED
    501       ERROR_MOUSE_NO_CONSOLE
    505       ERROR_MOU_EXTENDED_SG}
{Remarks:
* Setting a bit in the event mask means that the associated event is reported
  on the mouse FIFO event queue. See MouReadEventQue for examples of event
  mask use.}
function MouSetEventMask(var EventMask:word;MouHandle:word):word; cdecl;

{Return scaling factors for the current mouse device (a pair of 1-word
values).}
{Current row and column coordinate scaling factors (1 <= factor <= 32767)
returned in Scale (see MouSetScaleFact for more information), MouHandle is
the mouse device handle from a previous MouOpen call.}
{Possible return codes:
    0         NO_ERROR
    385       ERROR_MOUSE_NO_DEVICE
    466       ERROR_MOU_DETACHED
    501       ERROR_MOUSE_NO_CONSOLE
    505       ERROR_MOU_EXTENDED_SG}
{Remarks:
* The units of the scale factor depend on the mode of the display screen
  for the session. If the screen is operating in text mode, the scaling units
  are relative to characters. If the screen is operating in graphics mode,
  the scaling units are relative to pels.}
function MouGetScaleFact(var Scale:TScaleFact;MouHandle:word):word; cdecl;

{Assign to the current mouse device driver a new pair of 1-word scaling
factors.}
{Scale contains the new row and column coordinate scaling factors (1 <= factor
<= 32767), MouHandle is the mouse device handle from a previous MouOpen call.}
{Possible return codes:
    0         NO_ERROR
    385       ERROR_MOUSE_NO_DEVICE
    387       ERROR_MOUSE_INV_PARMS
    466       ERROR_MOU_DETACHED
    501       ERROR_MOUSE_NO_CONSOLE
    505       ERROR_MOU_EXTENDED_SG}
{Remarks:
* MouSetScaleFact sets the mickey-to-pixel ratio for mouse motion. The row
  scale and column scale ratios specify a number of mickeys for each 8 pixels.
  The default value for the row scale is 16 mickeys for each 8 pixels. The
  default value for the column scale is 8 mickeys to 8 pixels.
* The number of pixels moved does not have to correspond 1-to-1 with the number
  of mickeys the mouse moves. The scaling factor defines a sensitivity
  for the mouse that is a ratio of the number of mickeys required to move
  the cursor 8 pixels on the screen. The sensitivity determines at what rate
  the cursor moves on the screen.}
function MouSetScaleFact(const Scale:TScaleFact;MouHandle:word):word; cdecl;

{Open the mouse device for the current session.}
{DriverName contains the name of the pointer draw device driver to be used as
the pointer-image drawing routine for this session (such device driver must be
included in the CONFIG.SYS file at system start-up time) or is nil (the default
pointer draw device driver supplied by the system is used then), mouse device
handle is returned in MouHandle.}
{Possible return codes:
    0         NO_ERROR
    385       ERROR_MOUSE_NO_DEVICE
    390       ERROR_MOUSE_INV_MODULE_PT
    466       ERROR_MOU_DETACHED
    501       ERROR_MOUSE_NO_CONSOLE
    505       ERROR_MOU_EXTENDED_SG}
{Remarks:
* MouOpen initializes the mouse functions to a known state. The application
  may have to issue additional mouse functions to establish the environment
  it desires. For example, after the MouOpen, the collision area is defined
  to be the size of the entire display. Therefore, to get the pointer to be
  displayed, the application must issue a MouDrawPtr to remove the collision
  area.
* The state of the mouse after the first MouOpen is:
  - Row/Col scale factors set to 16/8 (see MouSetScaleFact)
  - all events reported (see MouSetEventMask)
  - empty event queue (see MouReadEventQue and MouGetNumQueEl)
  - all user settable Device Status bits reset (set to zero;
    see MouSetDevStatus)
  - pointer set to center of screen if valid display mode is set (see
    MouSetPtrPos)
  - pointer shape set to the default for the pointer device driver currently
    registered in the session (see MouSetPtrShape)
  - collision area equal to full screen (see MouDrawPtr and MouRemovePtr)
* DriverName has a different definition when the caller is the Base Video
  Subsystem (BVS). However, this implies direct calling of the 16-bit routine,
  which is not supported currently. In such case the selector portion
  of the pointer is zero, the offset portion is non-zero and contains a display
  configuration number (sequentially numbered where 1 is the first display
  configuration). The MouOpen call issued by BVS is executed on the VioSetMode
  path. Using the display configuration number passed on the MouOpen call, the
  Base Mouse Subsystem can detect a change in display configurations. This form
  of the MouOpen call is not recommended for applications. Applications should
  either send the name of the pointer draw device driver or nil.}
function MouOpen(DriverName:PChar;var MouHandle:word):word; cdecl;
function MouOpen(DriverName:string;var MouHandle:word):word;

{Close the mouse device for the current session.}
{MouHandle is the mouse device handle from a previous MouOpen call.}
{Possible return codes:
    0         NO_ERROR
    385       ERROR_MOUSE_NO_DEVICE
    466       ERROR_MOU_DETACHED
    501       ERROR_MOUSE_NO_CONSOLE
    505       ERROR_MOU_EXTENDED_SG}
{Remarks:
* MouClose closes the mouse device for the current session and removes the
  mouse device driver handle from the list of valid open mouse device
  handles.}
function MouClose(MouHandle:word):word; cdecl;

{Notify the mouse device driver that the area defined by the passed parameters
if for exclusive use of the application. This area is defined as the
"collision" area and is not available to the mouse device driver when drawing
pointer images.}
{ProtectArea is the pointer shape collision area, MouHandle is the mouse device
handle from a previous MouOpen call.}
{Possible return codes:
    0         NO_ERROR
    385       ERROR_MOUSE_NO_DEVICE
    387       ERROR_MOUSE_INV_PARMS
    466       ERROR_MOU_DETACHED
    501       ERROR_MOUSE_NO_CONSOLE
    505       ERROR_MOU_EXTENDED_SG}
{Remarks:
* MouRemovePtr may be issued by any process in the session. However, only one
  collision area is active at a time. Each MouRemovePtr command has the effect
  of resetting the collision area to the location and area specified
  by the current command.
* If the logical pointer position is outside of the collision area specified
  by the latest MouRemovePtr command, the pointer image is drawn.
* The MouDrawPtr command effectively cancels the MouRemovePtr command
  and allows the pointer to be drawn anywhere on the screen, until a new
  MouRemovePtr command is issued.}
function MouRemovePtr(var ProtectArea:TNoPtrRect;MouHandle:word):word; cdecl;

{Notify the mouse device driver that an area previously restricted
to the pointer image is now available to the mouse device driver.}
{MouHandle is the mouse device handle from a previous MouOpen call.}
{Possible return codes:
    0         NO_ERROR
    385       ERROR_MOUSE_NO_DEVICE
    466       ERROR_MOU_DETACHED
    501       ERROR_MOUSE_NO_CONSOLE
    505       ERROR_MOU_EXTENDED_SG}
{Remarks:
* The collision area (the pointer image restricted area) is established by
  MouOpen and by MouRemovePtr. MouDrawPtr nullifies the effect of the
  MouRemovePtr command. If there was no previous MouDrawPtr command or if a
  previous  MouDrawPtr command has already nullified the collision area, the
  MouRemovePtr command is effectively a null operation.
* This call is required to begin session pointer image drawing. Immediately
  after MouOpen is issued, the collision area is defined as the size of the
  display. A MouDrawPtr is issued to begin pointer drawing after the
  MouOpen.}
function MouDrawPtr(MouHandle:word):word; cdecl;

{Set the mouse device driver status flags for the installed mouse device
driver.}
{Status contains the desired status flag settings (2-byte set, only the
high-order byte has meaning - see MOUSE_DISABLED and MOUSE_MICKEYS constants;
other bits are reserved and set to zero). MouHandle is the mouse device handle
from a previous MouOpen call.}
{Possible return codes:
    0         NO_ERROR
    385       ERROR_MOUSE_NO_DEVICE
    387       ERROR_MOUSE_INV_PARMS
    466       ERROR_MOU_DETACHED
    501       ERROR_MOUSE_NO_CONSOLE
    505       ERROR_MOU_EXTENDED_SG}
{Remarks:
* MouSetDevStatus is the complement to MouGetDevStatus. However, not all status
  flags may be set with MouSetDevStatus. Only the flags corresponding
  to the following functions may be modified:
  - Return data in mickeys
    Normally, mouse data is returned to the application with the absolute
    display mode coordinates of the pointer image position on the display
    screen. By setting this status flag, mouse data is returned in relative
    mickeys, a unit of mouse movement.
  - Don't call pointer draw device
    Normally, the pointer draw device driver is called for all drawing
    operations. By setting this status flag, the mouse device driver does not
    call the pointer draw device driver. The application must draw any required
    pointer image on the screen.}
function MouSetDevStatus(var Status:word;MouHandle:word):word; cdecl;

{Initialize mouse pointer draw support for DOS mode.}
{Name of the Pointer Draw Device Driver used as the pointer-image drawing
routine for the DOS mode session must sent in DriverName; the name of the
device driver must be included in the CONFIG.SYS file at system start-up time.}
{Possible return codes:
    0         NO_ERROR
    385       ERROR_MOUSE_NO_DEVICE
    466       ERROR_MOU_DETACHED
    412       ERROR_MOUSE_SMG_ONLY
    501       ERROR_MOUSE_NO_CONSOLE
    505       ERROR_MOU_EXTENDED_SG}
{Remarks:
* MouInitReal is issued by the Base Video Subsystem at system initialization
  time.
* The DOS mode mouse API (INT 33h), in contrast to the OS/2 mode Mouse API,
  does not contain an OPEN command. In addition, there is only one session
  for DOS mode.
* The default pointer draw routine for DOS mode is located in the same pointer
  draw device driver, POINTER$, that is used for OS/2 mode. Establishing
  addressability to the pointer draw routine must be done during system
  initialization. This requires passing the entry point of the DOS mode pointer
  draw routine to the mouse device driver. This is the purpose
  of the MouInitReal call. It passes the address of the default, power-up
  pointer draw routine for DOS mode to the mouse device driver. This
  initialization is transparent to applications.
* This call is for use only by the Base Video Subsystem when invoked during
  system initialization under the shell/session manager PID.
* The error code ERROR_MOUSE_SMG_ONLY is valid from shell process only.
* When using direct calls to the 16-bit routine, another version of this call
  is supported as well - if the selector part of the far pointer is zero
  and the offset portion is non-zero, the offset portion identifies the
  power-up display configuration. However, this isn't possible in the current
  implementation (using 32-bit wrap-around function supplied in EMXWRAP.DLL).}
function MouInitReal(DriverName:PChar):word; cdecl;
function MouInitReal(DriverName:string):word;

{Synchronize the mouse subsystem with the mouse device driver.}
{WaitFlag specifies whether the routine should wait for the mouse device driver
being free - see MOU_NOWAIT and MOU_WAIT constants.}
{Possible return codes:
    0         NO_ERROR
    121       ERROR_SEM_TIMEOUT}
{Remarks:
* MouSynch blocks all other threads within a session until the semaphore
  clears (returns from the subsystem to the router). To ensure proper
  synchronization, MouSynch should be issued by a mouse subsystem if it intends
  to access dynamically modifiable shared data for each session or if it
  intends to issue a DosDevIOCtl. MouSynch does not protect globally shared
  data from threads in other sessions.}
function MouSynch(WaitFlag:word):word; cdecl;

function MouGetThreshold(var MouThreshold:TThreshold;MouHandle:word):word;
                                                                         cdecl;

function MouSetThreshold(var MouThreshold:TThreshold;MouHandle:word):word;
                                                                         cdecl;

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


function MouRegister(ModuleName,ProcName:PChar;FnMask:cardinal):word; cdecl;
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

function MouDeRegister:word; cdecl;
external 'EMXWRAP' index 314;
{external 'MOUCALLS' index 14;}

function MouFlushQue(MouHandle:word):word; cdecl;
external 'EMXWRAP' index 307;
{external 'MOUCALLS' index 7;}

function MouGetPtrPos(var MouPtr:TPtrLoc;MouHandle:word):word; cdecl;
external 'EMXWRAP' index 319;
{external 'MOUCALLS' index 19;}

function MouSetPtrPos(const MouPtr:TPtrLoc;MouHandle:word):word; cdecl;
external 'EMXWRAP' index 321;
{external 'MOUCALLS' index 21;}

function MouSetPtrShape(ImageBuf:pointer;var ImageInfo:TPtrShape;
                                                   MouHandle:word):word; cdecl;
external 'EMXWRAP' index 302;
{external 'MOUCALLS' index 2;}

function MouSetPtrShape(var ImageBuf;var ImageInfo:TPtrShape;
                                                   MouHandle:word):word; cdecl;
external 'EMXWRAP' index 302;
{external 'MOUCALLS' index 2;}

function MouGetPtrShape(var ImageBuf;var ImageInfo:TPtrShape;
                                                   MouHandle:word):word; cdecl;
external 'EMXWRAP' index 301;
{external 'MOUCALLS' index 1;}

function MouGetPtrShape(ImageBuf:pointer;var ImageInfo:TPtrShape;
                                                   MouHandle:word):word; cdecl;
external 'EMXWRAP' index 301;
{external 'MOUCALLS' index 1;}

function MouGetDevStatus(var Status:word;MouHandle:word):word; cdecl;
external 'EMXWRAP' index 322;
{external 'MOUCALLS' index 22;}

function MouGetNumButtons(var ButtonCount:word;MouHandle:word):word; cdecl;
external 'EMXWRAP' index 308;
{external 'MOUCALLS' index 8;}

function MouGetNumMickeys(var MickeyCnt:word;MouHandle:word):word; cdecl;
external 'EMXWRAP' index 303;
{external 'MOUCALLS' index 3;}

function MouReadEventQue(var Event:TMouEventInfo;var WaitFlag:word;
                                                   MouHandle:word):word; cdecl;
external 'EMXWRAP' index 320;
{external 'MOUCALLS' index 20;}

function MouGetNumQueEl(var MouseQInfo:TMouQueInfo;MouHandle:word):word; cdecl;
external 'EMXWRAP' index 313;
{external 'MOUCALLS' index 13;}

function MouGetEventMask(var EventMask:word;MouHandle:word):word; cdecl;
external 'EMXWRAP' index 315;
{external 'MOUCALLS' index 15;}

function MouSetEventMask(var EventMask:word;MouHandle:word):word; cdecl;
external 'EMXWRAP' index 316;
{external 'MOUCALLS' index 16;}

function MouGetScaleFact(var Scale:TScaleFact;MouHandle:word):word; cdecl;
external 'EMXWRAP' index 306;
{external 'MOUCALLS' index 6;}

function MouSetScaleFact(const Scale:TScaleFact;MouHandle:word):word; cdecl;
external 'EMXWRAP' index 311;
{external 'MOUCALLS' index 11;}

function MouOpen(DriverName:PChar;var MouHandle:word):word; cdecl;
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

function MouClose(MouHandle:word):word; cdecl;
external 'EMXWRAP' index 309;
{external 'MOUCALLS' index 9;}

function MouRemovePtr(var ProtectArea:TNoPtrRect;MouHandle:word):word; cdecl;
external 'EMXWRAP' index 318;
{external 'MOUCALLS' index 18;}

function MouDrawPtr(MouHandle:word):word; cdecl;
external 'EMXWRAP' index 326;
{external 'MOUCALLS' index 26;}

function MouSetDevStatus(var Status:word;MouHandle:word):word; cdecl;
external 'EMXWRAP' index 326;
{external 'MOUCALLS' index 26;}

function MouInitReal(DriverName:PChar):word; cdecl;
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

function MouSynch(WaitFlag:word):word; cdecl;
external 'EMXWRAP' index 323;
{external 'MOUCALLS' index 23;}

function MouGetThreshold(var MouThreshold:TThreshold;MouHandle:word):word;
                                                                         cdecl;
external 'EMXWRAP' index 329;
{external 'MOUCALLS' index 29;}

function MouSetThreshold(var MouThreshold:TThreshold;MouHandle:word):word;
                                                                         cdecl;
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
