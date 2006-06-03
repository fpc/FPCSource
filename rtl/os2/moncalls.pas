{Set tabsize to 4.}
{****************************************************************************


                           MONCALLS interface unit
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

unit MonCalls;

{ Interface library to MONCALLS.DLL (through EMXWRAP.DLL)

Please, note, that monitors are supported for OS/2 v2.1 and above only
(not for v2.0) and that they cannot be used in PM applications.

Changelog:

    People:

        TH - Tomas Hajny

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
{return codes / error constants (those marked with * shouldn't occur)}
    No_Error                        =     0;
    Error_Not_Enough_Memory         =     8;
    Error_Open_Failed               =   110;
    Error_Monitors_Not_Supported    =   165;
    Error_Mon_Invalid_Parms         =   379;
    Error_Mon_Invalid_DevName       =   380;
    Error_Mon_Invalid_Handle        =   381;
    Error_Mon_Buffer_Too_Small      =   382;
    Error_Mon_Buffer_Empty          =   383;
    Error_Mon_Data_Too_Large        =   384;
    Error_Mon_Bad_Buffer            =   730; {*}
    Error_Mon_Chain_Handle          = 32784; {*}
    Error_Mon_Not_Registered        = 32785; {*}

{WaitFlag}
    IO_Wait     =0; {The monitor thread that issues DosMonRead wishes to block}
                    {until a data record is available in its input buffer.}
    IO_NoWait   =1; {The monitor thread that issues DosMonRead does not wish}
                    {to block when its input buffer is empty.}


{Terminate character device monitoring.  All monitor buffers associated with
this process are flushed and closed.}
{MonHandle - device handle returned from a previous DosMonOpen call.}
{Possible return codes:
    0         No_Error
    381       Error_Mon_Invalid_Handle}
{Remarks:
* A single process may register one or more monitors with a character device
  using the same device handle returned from a previous DosMonOpen call.
  When DosMonClose is issued for a specific, opened device handle, all
  monitors for the current process registered with this handle terminate.
* When DosMonClose is issued, the monitor loses access to the device data
  stream. Before issuing DosMonClose, monitor threads calling DosMonRead and
  DosMonWrite should be terminated. After DosMonClose has been called,
  DosMonRead calls return an ERROR_MON_BUFFER_EMPTY return code and
  DosMonWrite calls return an ERROR_NOT_ENOUGH_MEMORY return code.
* Data area containing monitor buffers should not be freed until after
  DosMonClose is called. If data area containing monitor buffers is freed
  before DosMonClose is called, a GP fault occurs when DosMonClose is called
  and the process is terminated.
* For a detailed description of this call see the chapter "Character Device
  Monitors" in the IBM Operating System/2 Version 1.2 I/O Subsystems And
  Device Support Volume 1.}
function DosMonClose(MonHandle:word):word; cdecl;

{Gain access to a character device data stream.}
{DevName - device name, monitor handle returned in MonHandle.}
{Possible return codes:
    0         No_Error
    110       Error_Open_Failed
    379       Error_Mon_Invalid_Parms
    380       Error_Mon_Invalid_DevName}
{Remarks:
* Only one DosMonOpen call is necessary per device per process. That is,
  several DosMonReg calls can be made using the same monitor handle to the
  same device. This allows monitors to be registered using different values
  for Index from the same process and going to the same device. When the
  DosMonClose is issued, all of the monitors registered on the handle are
  closed.
* For a detailed description of this call see the chapter "Character Device
  Monitors" in the IBM Operating System/2 Version 1.2 I/O Subsystems And
  Device Support Volume 1.}
function DosMonOpen(DevName:PChar;var MonHandle:word):word; cdecl;
function DosMonOpen(DevName:string;var MonHandle:word):word;

{Wait for a data record, move it from the input buffer of a registered
character device monitor and place it in a private data area where the monitor
can freely access it.}
{InBuf - monitor input buffer, WaitFlag - see IO_WAIT and IO_NOWAIT constants,
DataBuf - data area in the calling process address space that the data from the
monitor's input buffer is read into, ByteCount - on input size of the DataBuf,
on return number of bytes of data moved.}
{Possible return codes:
    0         No_Error
    379       Error_Mon_Invalid_Parms
    382       Error_Mon_Buffer_Too_Small
    383       Error_Mon_Buffer_Empty}
{Remarks:
* For a detailed description of this call see the chapter "Character Device
  Monitors" in the IBM Operating System/2 Version 1.2 I/O Subsystems And
  Device Support Volume 1.}
function DosMonRead(var InBuf;WaitFlag:word;var DataBuf;
                                               var ByteCount:word):word; cdecl;

{Establish an input and output buffers to monitor an I/O stream for a character
device.}
{MonHandle - device handle returned from a previous DosMonOpen call, InBuf -
monitor input buffer, the monitor dispatcher moves data records into this
buffer from the device driver (if the monitor is the first one in the monitor
chain) or from the previous monitor in the chain, monitor then takes data from
this buffer for filtering by calling DosMonRead, OutBuf - monitor output
buffer, monitor places filtered data into this buffer by calling DosMonWrite,
the monitor dispatcher moves data records from this buffer to the device driver
(if the monitor is the last one in the monitor chain) or to the next monitor in
the chain, PosCode - used to specify placement of a monitor's buffers with the
monitor chain (FIRST, LAST or DEFAULT) and whether one or two threads are
created by the monitor dispatcher to handle data movement (see explanation
bellow), Index - device specific value, for the keyboard it pertains to the
session you wish to register a monitor on, for the printer it pertains to the
data or code page monitor chain.}
{Possible return codes:
    0         No_Error
    8         Error_Not_Enough_Memory
    165       Error_Monitors_Not_Supported
    379       Error_Mon_Invalid_Parms
    381       Error_Mon_Invalid_Handle
    382       Error_Mon_Buffer_Too_Small}
{Remarks:
* PosCode meaning:
    0   DEFAULT (no position preference) and one thread for data movement
    1   FIRST (monitor placed at beginning of monitor chain) and one thread for
        data movement
    2   LAST (monitor placed at the end of monitor chain) and one thread for
        data movement
    3   DEFAULT with two threads for data movement
    4   FIRST with two threads for data movement
    5   LAST with two threads for data movement
  The first monitor in a monitor chain that registers as FIRST is placed at the
  head of the monitor chain. The next monitor that registers as FIRST follows
  the last monitor registered as FIRST, and so on. Similarly, the first monitor
  that registers as LAST is placed at the end of the monitor chain. The next
  monitor that registers as LAST is placed before the last monitor that
  registered as LAST, and so on. The first monitor that registers as DEFAULT is
  placed before the last monitor, if any, that registered as LAST. The next
  monitor that registers as DEFAULT is placed before the last monitor that
  registered as DEFAULT, and so on.
* For a detailed description of this call see the chapter "Character Device
  Monitors" in the IBM Operating System/2 Version 1.2 I/O Subsystems And
  Device Support Volume 1.}
function DosMonReg(MonHandle:word;var InBuf,OutBuf;PosCode,Index:word):word;
                                                                         cdecl;

{Move a filtered data record from the monitor's private data area into the
monitor's output buffer.}
{OutBuf - monitor output buffer, DataBuf - monitor's private data area
containing a filtered data record of length ByteCount, this filtered data
record is moved into the monitor's output buffer by this call, ByteCount - size
of the data record.}
{Possible return codes:
    0         No_Error
    8         Error_Not_Enough_Memory
    379       Error_Mon_Invalid_Parms
    384       Error_Mon_Data_Too_Large}
{Remarks:
* For a detailed description of the use of this call see the chapter
  "Character Device Monitors" in the IBM Operating System/2 Version 1.2 I/O
  Subsystems And Device Support Volume 1.}
function DosMonWrite(var OutBuf,DataBuf;ByteCount:word):word; cdecl;


{***************************************************************************}
implementation
{***************************************************************************}


function DosMonClose(MonHandle:word):word; cdecl;
external 'EMXWRAP' index 403;
{external 'MONCALLS' index 3;}

function DosMonOpen(DevName:PChar;var MonHandle:word):word; cdecl;
external 'EMXWRAP' index 404;
{external 'MONCALLS' index 4;}

function DosMonOpen(DevName:string;var MonHandle:word):word;
begin
    if DevName[0]=#255 then
    begin
        Move(DevName[1],DevName[0],255);
        DevName[255]:=#0;
        DosMonOpen:=DosMonOpen(@DevName,MonHandle);
    end else
    begin
        DevName[Succ(byte(DevName[0]))]:=#0;
        DosMonOpen:=DosMonOpen(@DevName[1],MonHandle);
    end;
end;

function DosMonRead(var InBuf;WaitFlag:word;var DataBuf;
                                               var ByteCount:word):word; cdecl;
external 'EMXWRAP' index 402;
{external 'MONCALLS' index 2;}

function DosMonReg(MonHandle:word;var InBuf,OutBuf;PosCode,Index:word):word;
                                                                         cdecl;
external 'EMXWRAP' index 405;
{external 'MONCALLS' index 5;}

function DosMonWrite(var OutBuf,DataBuf;ByteCount:word):word; cdecl;
external 'EMXWRAP' index 401;
{external 'MONCALLS' index 1;}


end.
