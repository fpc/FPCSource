{Set tabsize to 4.}
{****************************************************************************

                           DOSCALLS interface unit
                     FPK-Pascal Runtime Library for OS/2
                   Copyright (c) 1993,94 by Florian Kl„mpfl
                    Copyright (c) 1997 by Dani‰l Mantione

 The FPK-Pascal runtime library is distributed under the Library GNU Public
 License v2. So is this unit. The Library GNU Public License requires you to
 distribute the source code of this unit with any product that uses it.
 Because the EMX library isn't under the LGPL, we grant you an exception to
 this, and that is, when you compile a program with the FPK Pascal compiler,
 you do not need to ship source code with that program, AS LONG AS YOU ARE
 USING UNMODIFIED CODE! If you modify this code, you MUST change the next
 line:

 <This an official, unmodified FPK Pascal source code file.>

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

unit doscalls;

{This unit was called bsedos in the original OS/2 runtime library.
 Changed, because it's an interface library to DOSCALLS.DLL.

 The goal was to make this unit a real Pascal unit instead of a C unit in
 Pascal clothes. Not that I want to make every translated statement look
 exactly like other Pascal code, but in this case, it was too crazy.
 Therefore typenames, constants etc. have names like the ones in IBM's
 documentation (which is C oriented), but do not match exactly. In general
 constants do now use the common two letter prefix, followed by the constant
 description. Type names use a capital T in front of their name, handles do
 not have separate types, longints are used, and whenever possible pointers
 were replaced by var parameters. All constructions like 'type LONG=longint'
 have been removed. Furthermore, most of the procedures also accept strings
 instead of just a Pchar, thanks to that cool feature of procedure
 overloading.

 Dani‰l Mantione,
 June 1997

Changelog:

    People:

        DM - Dani‰l Mantione

    Date:           Description of change:              Changed by:

     -              First released version 0.1.         DM

Coding style:

    It may be well possible that coding style feels a bit strange to you.
    Nevertheless I friendly ask you to try to make your changes not look all
    too different. To make life easier, set your IDE to use tab characters,
    turn optimal fill, autoindent and backspace unindents on and set a
    tabsize of 4.}

{****************************************************************************

                           Preprocessor definitions

****************************************************************************}


{$IFNDEF FVISION_PSTRING}
{$IFNDEF OWN_PSTRING}
{$DEFINE FVISION_PSTRING}       {Get the Pstring type from Free Vision.}
{$ENDIF}
{$ENDIF}

{***************************************************************************}
interface
{***************************************************************************}

{$IFDEF OWN_PSTRING}
uses    strings;

type    Pstring=^string;
{$ELSE}
 {$IFDEF FVISION_PSTRING}
uses    strings,objects;
 {$ELSE}
    {$ERROR Pstring source unknown.}
 {$ENDIF}
{$ENDIF}

{$ifdef FPC}
    {$packrecords 1}
{$endif FPC}

type    Tbytearray=array[0..$fff0] of byte;
        Pbytearray=^Tbytearray;
        Tchararray=array[0..$fff0] of char;
        Pchararray=^Tchararray;
        Twordarray=array[0..$7ff8] of word;
        Pwordarray=^Twordarray;

{****************************************************************************

                            Thread related routines.

****************************************************************************}

type   Tthreadentry=procedure(param:pointer);


const   dtsuspended         =1; {Thread is started suspended instead of
                                 started at once.}
        dtstack_commited    =2; {Allocate all stack space at once. The
                                 operating system normally allocates more
                                 memory to the stack if the stack grows with
                                 the given stacksize as limit. This has the
                                 restriction that you cannot create a stack
                                 frame > 4 kb. at once. If you want to do
                                 this, or for other reasons you can allocate
                                 the complete stack at once with this flag.}

        dtwait              =0; {Wait until termination.}
        dtnowait            =1; {Do not wait. Return with error if not yet
                                 terminated.}

{Create a new thread.
 tid        = Thread ID of new thread is returned here.
 address    = Thread entry point. The new thread starts executing here.
 Aparam     = This one is passed to the thread entry procedure.
 flags      = Flags. Either dtsuspended or dt_stackcommited.
 stacksize  = Size of the stack of the new thread.}
function doscreatethread(var tid:longint;address:Tthreadentry;
                         Aparam:pointer;flags:longint;stacksize:longint):word;

{Suspend a running thread.}
function dossuspendthread(tid:longint):word;

{Resume a suspended thread.}
function dosresumethread(tid:longint):word;

{Terminate a specific thread.}
function doskillthread(tid:longint):word;

{Wait until a specific thread has ended.
 tid            = Thread to terminate. Can also be zero. In that case we
                  wait until the next thread terminates. It's thread ID is
                  returned back.
 option         = Flags. Either dtwait or dtnowait.}
function doswaitthread(var tid:longint;option:longint):word;

{All other threads in the same process are suspended until a
dosexitcritsec.}
function dosentercritsec:word;

{Resume the other threads again.}
function dosexitcritsec:word;

const   dethread=0;         {Terminate thread only.}
        deprocess=1;        {Terminate the whole process.}

{Terminate the thread or the program. Never returns, so it's defined as
 procedure.}
procedure dosexit(action:longint;result:longint);

type    Pthreadinfoblock=^Tthreadinfoblock;
        Psysthreadib=^Tsysthreadib;
        Pprocessinfoblock=^Tprocessinfoblock;

        Tthreadinfoblock=record
            exh_chain,              {Head of exeption handler chain.}
            stack,                  {Pointer to the thread's stack.}
            stacklimit:pointer;     {Pointer to the thread's stack-end.}
            tib2:Psysthreadib;      {Pointer to system specific thread info.}
            version,                {Version of this datastructure.}
            ordinal:longint;        {Thread ordinal number.}
        end;

        Tsysthreadib=record
            tid,                    {Thread ID.}
            priority,               {Low byte of low word: thread priority.
                                     High byte of high word: thread class
                                        1 = Idle
                                        2 = Regular
                                        3 = Time critical
                                        4 = Server}
            version:longint;        {Version of this datastructure.}
            MCcount,                {Must complete count. ??? Info wanted!}
            MCforceflag:word;       {Must complete force flag. Info wanted!}
        end;

        Tprocessinfoblock=record
            pid,                    {Process ID.}
            parentpid,              {Parent's process ID.}
            hmte:longint;           {Module handle of executable program.
                                     ??? Info wanted!}
            cmd,                    {Command line options.}
            env:Pbytearray;         {Environment strings.}
            flstatus,               {1 means that the process is in exit list
                                     processing.}
            ttype:longint;          {Type of process:
                                        0:  Full screen protected mode.
                                        1:  DOS emulation.
                                        2:  Windowable full screen protected
                                            mode program.
                                        3:  Presentation manager program.
                                        4:  Detached mode process.}
        end;

{OS/2 keeps information about the current process and the current thread
 is the datastructures Tprocessinfoblock and Tthreadinfoblock. All data
 can both be read and be changed. Use dosGetInfoBlocks to get their
 address. The service cannot fail, so it is defined as procedure.}

procedure dosgetinfoblocks(var Atib:Pthreadinfoblock;
                           var Apib:Pprocessinfoblock);

{Wait a number of microseconds. Cannot fail, so it is defined as procedure.}
procedure dossleep(msec:longint);

{Beep the speaker. You do not need to check for an error if you can
 guarantee that the frequency is correct.}
function dosbeep(freq,ms:longint):word;

{****************************************************************************

                        Process handling routines.

****************************************************************************}

{You need a heavy manual if you want to know how this procedure works. Used
for writing debuggers.}
function dosdebug(debugbuf:pointer):word;

const   TC_exit         = 0;
        TC_harderror    = 1;
        TC_trap         = 2;
        TC_killprocess  = 3;
        TC_exception    = 4;

        EXLST_add       = 1;
        EXLST_remove    = 2;
        EXLST_exit      = 3;

type    Texitproc=procedure(reason:longint);

{Add/remove an exitprocedure to the exit list. Also used to terminate an
 exit procedure. An exit procedure will be called on exiting of the program.

    ordercode       = One of the EXLST_XXXX constants.
    proc            = Address of the exit procedure.

An exit procedure is called with one of the TC_XXXX constants. When it is
done it must call dosExitList with EXLST_EXIT.

Exit procedures are called in random order.}
function dosexitlist(ordercode:longint;proc:Texitproc):word;

const   desync          = 0;    {Wait until program terminates.}
        deasync         = 1;    {Do not wait.}
        deasyncresult   = 2;    {Do not wait. DosWaitChild will follow to
                                 check if process has been terminated. If
                                 you use this, you must use DosWaitChild,
                                 because OS/2 will not free memory that is
                                 allocated for the result codes if you don't.}
        detrace         = 3;    {Trace-able. Info Wanted!}
        debackground    = 4;    {Do not run as child. Run in a separate
                                 session.}
        desuspended     = 5;    {Child will be loaded, but not executed.}
        deasyncresultdb = 6;    {?? Info wanted.}

type    Tresultcodes=record
            terminatereason,        {0 = Normal termionation.
                                     1 = Critical error.
                                     2 = Trapped. (GPE, etc.)
                                     3 = Killed by DosKillProcess.}
            exitcode:longint;       {Exit code of child.}
        end;

{Execute a program.

 objnaam        = If a DLL cannot be found, it's name will be returned here.
 objlen         = Size of your objnaam buffer.
 execflag       = One of the dexxxx constants.
 res            = See resultcodes.
 args           = Arguments. ASCIIZ strings. End of ARGS given by an empty
                  string (#0). First arg must be filename without path and
                  extension. NIL is also allowed.
 env            = Environment. ASCIIZ strings. A variable has the format
                  NAME=CONTENTS. End of ENV given by an empty string (#0).
                  NIL is also allowed.
 filenaam       = Filename with full path and extension. Is not sensitive
                  for the PATH environment variabele.}
function dosexecpgm(objnaam:Pchar;cbobjnaam,execflag:longint;
                    args,env:Pbytearray;var res:Tresultcodes;
                    filenaam:Pchar):word;
function dosexecpgm(var objnaam:string;execflag:longint;
                    args,env:Pbytearray;var res:Tresultcodes;
                    const filenaam:string):word;

{Wait until a child process terminated. Sometimes called DosCWait.

action              = 0 = Wait until child terminates.
                      1 = Wait until child and all it's childs terminate.
option              = Flags. Either dtwait or dtnowait.
res                 = See resultcodes.
termpid             = Process ID that has been terminated. Usefull when
                      terminating a random process.
pid                 = Process ID of process to terminate. Use a zero to
                      terminate a random process.}
function doswaitchild(action:longint;option:longint;var res:Tresultcodes;
                      var termpid:longint;pid:longint):word;

const   dpprocess       = 0;
        dpprocesschilds = 1;
        dpthread        = 2;

        dpsameclass     = 0;
        dpidleclass     = 1;
        dpregular       = 2;
        dptimecritical  = 3;

{Set priority of a thread or all threads in another process.

 scope              = 0 = Set for all threads of a process.
                      1 = Set for all threads of a process and it's childs.
                      2 = Set for a thread of the current process.
 trclass            = 0 = Do not change class.
                      1 = Change to idle time class.
                      2 = Change to regular class.
                      3 = Change to time-critical class.
 delta              = Value to add to priority. Resulting priority must be in
                      the range 0..31.
 portid             = Process ID when scope=0 or 1, thread ID when scope=2.}
function dossetpriority(scope,trclass,delta,portid:longint):word;

{Terminate a process. If the process isn't a child process, it can refuse
 to terminate.

 action             = 0 = Terminate process and all it's childs.
                      1 = Terminate process only.
 pid                = Process ID of process to terminate.}
function doskillprocess(action:longint;pid:longint):word;

const   APPTYP_notspec          = $0000; {Apptype is unknown.}
        APPTYP_notwindowcompat  = $0001; {App cannot run in a window.}
        APPTYP_windowcompat     = $0002; {App can run in a window.}
        APPTYP_windowapi        = $0003; {App uses PM}
        APPTYP_bound            = $0008; {App uses family API.}
        APPTYP_DLL              = $0010; {File is a DLL.}
        APPTYP_dos              = $0020; {App is a PC-DOS program.}
        APPTYP_physdrv          = $0040; {App is a physical device driver.}
        APPTYP_virtdrv          = $0080; {App is virtual device driver.}
        APPTYP_protDLL          = $0100; {File is a protected mode DLL.}
        APPTYP_windowsreal      = $0200; {M$ Winslows app, real mode.}
        APPTYP_windowsprot      = $0400; {M$ Winslows app, protected mode.}
        APPTYP_32bit            = $4000; {App is 32 bit.}

{Get the application type of an executable file on disk.
 filenaam           = Name of file to get type from.
 flags              = Receives a bitfield using the APPTYP constants.}
function dosqueryapptype(filenaam:Pchar;var flags:longint):word;

const   diprinter   = 0;    {Get number of printer (parallel) ports.}
        diRS232     = 1;    {Get number of serial ports.}
        difloppy    = 2;    {Get number of floppy drives.}
        dicopro     = 3;    {Get number of FPU's installed (either 0 or 1).}
        disubmodel  = 4;    {??? System submodel type?}
        dimodel     = 5;    {??? System model type?}
        diadapter   = 6;    {0=Monochrome display, 1=other. ??? Does OS/2
                             support monochrome displays?}

{Get information about attached devices.
 devinfo            = Receives requested information.
 item               = One of the dixxxx constants.}
function dosdevconfig(var devinfo:byte;item:longint):word;

{****************************************************************************

                        File handling related routines.

****************************************************************************}

const   maxpathlength=260;
        maxpathcomponent=256;

type    Tfilelock=record
            offset,range:longint;
        end;
        Pfilelock=^Tfilelock;

{Lock or unlock an area of a file. Other processes may not access that part
 of the file.

 unlock         = Area to unlock. (0,0) = Do not unlock.
 lock           = Area to lock.   (0,0) = Do not lock.
 timeout        = Number of miliseconds to wait if another process has locked
                  the file.
 flags          = Bitfield:
                  Bit 0:    0 = Other processes are denied access.
                            1 = Other processes may still read from the area.
                  Bit 1:    0 = Normal locking mode.
                            1 = Atomic mode. Refer IBM's documentation.}
function dossetfilelocks(handle:longint;var unlock,lock:Tfilelock;
                         timeout:longint;flags:longint):word;

{Cancel a filelock area.

handle  = File handle.
lock    = Area that is locked now.}
function doscancellockrequest(handle:longint;var lock:Tfilelock):word;

{Data structures for extended attributes. Reading IBM's documentation is
 highly recommended before experimenting with EAs.}

const   fEA_needEA=$80;

        eabinary        = $fffe;
        eaASCII         = $fffd;
        eabitmap        = $fffb;
        eametafile      = $fffa;
        eaicon          = $fff9;
        eaEA            = $ffee;
        eaMVMT          = $ffdf;
        eaMVST          = $ffde;
        eaASN1          = $ffdd;

type    TgEA=record
            naamlen:byte;
            naam:array[0..9999] of char;
        end;
        PgEA=^TgEA;

        TgEAlist=record
            listlen:longint;
            list:array[0..9999] of TgEA;
        end;
        PgEAlist=^TgEAlist;

        TfEA=record
            EA,
            naamlen:byte;
            value:word;
        end;
        PfEA=^TfEA;

        TfEAlist=record
            size:longint;
            list:array[0..9999] of TfEA;
        end;
        PfEAlist=^TfEAlist;

        TEAop=record
            gEAlist:PgEAlist;
            fEAlist:PfEAlist;
            error:longint;
        end;
        PEAop=^TEAop;

        TfEA2=record
            nextentry:longint;
            flags,
            naamlen:byte;
            value:word;
            szname:array[0..9999] of char;
        end;
        PfEA2=^TfEA2;

        TfEA2list=record
            listlen:longint;
            list:array[0..9999] of TfEA2;
        end;
        PfEA2list=^TfEA2list;

        TgEA2=record
            nextentry:longint;
            naamlen:byte;
            naam:array[0..9999] of char;
        end;
        PgEA2=^TgEA2;

        TgEA2list=record
          listlen:longint;
          list:array[0..9999] of TgEA2;
        end;
        PgEA2list=^TgEA2list;

        TEAop2=record
            gEA2list:PgEA2list;
            fEA2list:PfEA2list;
            error:longint;
        end;
        PEAop2=^TEAop2;

        TEAsizebuf=record
            maxEAsize:word;
            maxEAlistsize:longint;
        end;
        PEAsizebuf=^TEAsizebuf;


{*******************End of extented attribute datastructures.***************}

{Usefull constanst for action parameter.}
const       doopened        =  1;
            docreated       =  2;
            dooverwritten   =  3;

{Usefull constants for openflags parameter.}
const       dofail          =  0;
            doopen          =  1;
            dooverwrite     =  2;
            docreate        = 10;

{Usefull constants for openmode parameter.}

const       doread          =     0;
            dowrite         =     1;
            doreadwrite     =     2;
            dodenyRW        =    16;
            dodenywrite     =    32;
            dodenyread      =    48;
            dodenynone      =    64;
            donoinherit     =   128;
            dosequential    =   256;
            dorandom        =   512;
            donocache       =  4096;
            dofailonerr     =  8192;
            dowritethru     = 16384;
            doDASD          = 32768;

{ Open a file.

 filenaam       = Name of file.
 handle         = Receives filehandle.
 action         = Receives result of opening.
                    1 = Existing file opened.
                    2 = File did not exist. Created.
                    3 = File existed. Overwritten.
 initsize       = Initial size of file when creating or overwriting.
                  Ignored when you do not. Must be zero when the file is
                  created or overwritten in read-only mode.
 attrib         = Attributes when creating or overwriting files.
 openflags      = Bitfield describing what to do when file exists or doesn't
                  exist.
 openmode       = Bitfield describing describing how to open a file.
 ea             = Extended attributes to give file when created. Use a NIL
                  pointer if you don't want to give it extended attributes.
                  Use it only when creating or overwriting file. Use NIL
                  when not. Only the FEA list will be used.

The bits in the openflags parameter have the following meanings:

 Bit 0-3:   Action to take when file exists.    0000 = Return with error.
                                                0001 = Open it.
                                                0010 = Overwrite it.
 Bit 4-7:   Action to take when file does not   0000 = Return with error.
            exist.                              0001 = Create it.

The bits in the filemode parameter have the following meanings:

 Bit 0-2:   Access mode:        000 = Read-only
                                001 = Write-only
                                010 = Read/Write
 Bit 3:     Reserved.
 Bit 4-6:   Sharing mode.       001 = Deny all
                                010 = Deny write
                                011 = Deny read
                                100 = Deny none
 Bit 7:     Inheritance.        0 = Handle will be inherited by childs.
                                1 = Handle will not be inherited.
 Bit 8-11:  Reserved.
 Bit 12:    Cache flag.         0 = Use caching.
                                1 = Disable both read and write caching.
 Bit 13:    Error handling.     0 = Use critical error handler.
                                1 = Return just an error code.
 Bit 14:    Write cache flag.   0 = Write operations may be cached.
                                1 = Write operations must be executed
                                    before write operation functions return.
 Bit 15:    DASD flag.          0 = Open a file or device.
                                1 = Open a drive as file.

When the DASD flag is set, the whole drive is read as a single file. The
file starts with 512 of bootsector, then 512 bytes of the second sector etc.
The filename must consist of the driveletter followed by a semicolon.}
function dosopen(filenaam:Pchar;var handle,action:longint;
                 initsize:longint;attrib,openflags,filemode:longint;
                 ea:PEAop2):word;
{This variant of dosopen always creates or overwrites a file.}
function doscreate(filenaam:Pchar;var handle:longint;
                   attrib,openmode:longint):word;
{This variant of dosOpen always opens an existing file.}
function dosopen(filenaam:Pchar;var handle:longint;
                 attrib,openmode:longint):word;
{There are also string variants.}
function dosopen(const filenaam:string;var handle,action:longint;
                 initsize:longint;attrib,openflags,openmode:longint;
                 ea:PEAop2):word;
function doscreate(const filenaam:string;var handle:longint;
                   attrib,openmode:longint):word;
function dosopen(const filenaam:string;var handle:longint;
                 attrib,openmode:longint):word;


{Close a file.
Cannot fail if handle does exist.}
function dosclose(handle:longint):word;

{Read from a file or other type of handle.

    handle      = File handle.
    buffer      = The read data is stored here.
    count       = Number of bytes to read.
    actcount    = Number of bytes actually read.}
function dosread(handle:longint;var buffer;count:longint;
                 var actcount:longint):word;

{Write to a file or other type of handle.

    handle      = File handle.
    buffer      = The data to be written.
    count       = Number of bytes to write.
    actcount    = Number of bytes actually written.}
function doswrite(handle:longint;var buffer;count:longint;
                  var actcount:longint):word;

const   dszerobased=0;      {Set filepointer from begin of file.}
        dsrelative=1;       {Set filepointer relative to the current one.}
        dsendbased=2;       {Set filepointer from end of file.}

{Change the filepointer of a file.}
function dossetfileptr(handle:word;pos:longint;method:longint;
                       var posactual:longint):word;
{This variant seeks always from begin of file and does not return the
 actual position.}
function dossetfileptr(handle:word;pos:longint):word;
{This variant returns the current filepointer.}
function dosgetfileptr(handle:word;var posactual:longint):word;

{Use dosqueryfileinfo or dosquerypathinfo to get the size of a file.}

{Change the size of a file.}
function dossetfilesize(handle,size:longint):word;

{Flush update the changes to a file to disk.}
function dosresetbuffer(handle:longint):word;

{Duplicate or redirect a handle.
To duplicate a handle: Fill handle with source handle and duplicate with -1.
                       Copy of handle will be returned in duplicate.
To redirect a handle:  Fill handle with handle to which the handle to
                       redirect will be redirected. The handle that will be
                       redirected should be placed in duplicate.}
function dosduphandle(handle:longint;var duplicate:longint):word;

{Return information about a specific handle. See dosopen for a
 description of filemode.}
function dosqueryFHstate(handle:longint;var filemode:longint):word;

{Set information about a specific handle. See dosopen for a description
 of filemode.}
function dossetFHstate(handle,filemode:longint):word;

{Usefull constants for the handle type.}
const   dhfile      =    0;
        dhdevice    =    1;
        dhpipe      =    2;
        dhnetwork   = 8192;

{Determine if a handle belongs to a file, a device or a pipe.
 handle             = Handle tp query info about.
 handtype           = Bits 0-1:   00 = File
                                  01 = Device
                                  02 = Pipe
                      Bit 15:     0 = Local.
                                  1 = On network.}
function dosqueryHtype(handle:longint;var handtype:longint;
                       var attr:longint):word;

{****************************************************************************

                    File management related routines.

****************************************************************************}


{Edit a filename using wildcard.

Example editing CONFIG.SYS with *.BAK becomes CONFIG.BAK.
Usefull when parsing commands like 'copy config.sys *.bak'.
All filename characters are casemapped.'

metalevel       = 0 Use modern semantics
metalevel       = 1 Use OS/2 1.2 semantics
source          = string to edit
edit            = editstring
target          = destination buffer
cbtarget        = size of the destination buffer}
function doseditname(metalevel:longint;source,edit:Pchar;
                     target:Pchar;cbtarget:longint):word;
function doseditname(metalevel:longint;const source,edit:string;
                     var target:string):word;

{Move or rename a file.
 Oud    - Old name of file.
 Nieuw  - New name of file.}
function dosmove(oud,nieuw:Pchar):word;
function dosmove(const oud,nieuw:string):word;


const   dcexisting=1;           {Overwrite existing files.}
        dcappend=2;             {Append to existing file.}
        dcfailas=4;             {?? Info wanted!}

{Copy a file.
 Oud    - Source-file.
 Nieuw  - Destination-file.}
function doscopy(oud,nieuw:Pchar;option:longint):word;
function doscopy(const oud,nieuw:string;option:longint):word;

{Delete a file from disk.}
function dosdelete(filenaam:Pchar):word;
function dosdelete(const filenaam:string):word;

{Destroy a file on disk. dosForceDelete makes sure that the file cannot
 be unerased anymore.}
function dosforcedelete(filenaam:Pchar):word;
function dosforcedelete(const filenaam:string):word;

{Create a new directory.

naam            = Name of directory to create.
ea              = Extented attributes to give the directory. Use NIL if you
                  do not want do give it extented attributes. Only the FEA
                  list is used.}
function doscreatedir(naam:Pchar;ea:PEAOP2):word;
function doscreatedir(const naam:string;ea:PEAOP2):word;
{Variants without the ea parameter (NIL is used).}
function doscreatedir(naam:Pchar):word;
function doscreatedir(const naam:string):word;

{Remove a directory.}
function dosdeletedir(naam:Pchar):word;
function dosdeletedir(const naam:string):word;

{Set the current drive. Cannot fail if the driveletter is correct.}
function dossetdefaultdisk(disknum:longint):word;

{Get the current drive. Because it cannot fail, it is declared as procedure.}
procedure dosquerycurrentdisk(var disknum:longint;var logical:longint);

{Set the current directory.}
function dossetcurrentdir(naam:Pchar):word;
function dossetcurrentdir(const naam:string):word;

{Get the current directory.}
function dosquerycurrentdir(disknum:longint;var buffer;
                            var buflen:longint):word;
function dosquerycurrentdir(disknum:longint;var buffer:string):word;

{Send/receive information to a device.

 handle             = A file handle to a device, instead of a file.
 category           = The category of functions the function is in.
 func               = Function to call.
 params             = Parameters for the function.
 paramlen           = Size of the params buffer.
 paramsize          = Size of the parametrs to send to the device
                      Receives size of the returned parameters.
 data               = Data to send to device.
 datalen            = Size of your data buffer.
 datasize           = Size of the data to send to device.
                      Receives size of the data returned by the device.}
function dosdevIOctl(handle,category,func:longint;var params;
                     paramlen:longint;var paramsize:longint;
                     var data;var datalen:longint;var datasize:
                     longint):word;

{****************************************************************************

                      File searching related routines.

****************************************************************************}

const   fareadonly      =  1;
        fahidden        =  2;
        fasystem        =  4;
        fareserve       =  8;
        fadirectory     = 16;
        faarchive       = 32;

        ilstandard      =  1;
        ilqueryEAsize   =  2;
        ilqueryEAs      =  3;
        ilqueryfullname =  5;

{Format of date records:

 Bit 0..4:      Day.
 Bit 5..8:      Month.
 Bit 9..15:     Year minus 1980.

 Format of time records:

 Bit 0..4:      Seconds divided by 2.
 Bit 5..10      Minutes.
 Bit 11..15:    Hours.}

type    Tfilestatus=object
            datecreation,           {Date of file creation.}
            timecreation,           {Time of file creation.}
            datelastaccess,         {Date of last access to file.}
            timelastaccess,         {Time of last access to file.}
            datelastwrite,          {Date of last modification of file.}
            timelastwrite:word;     {Time of last modification of file.}
            filesize,               {Size of file.}
            filealloc:longint;      {Amount of space the file really
                                     occupies on disk.}
        end;
        Pfilestatus=^Tfilestatus;

        Tfilestatus1=object(Tfilestatus)
            attrfile:word;          {Attributes of file.}
        end;
        Pfilestatus1=^Tfilestatus1;

        Tfilestatus2=object(Tfilestatus)
            attrfile:word;
            cblist:longint;
        end;
        Pfilestatus2=^Tfilestatus2;

        Tfilestatus3=object(Tfilestatus)
            attrfile:longint;       {Attributes of file.}
        end;
        Pfilestatus3=^Tfilestatus3;

        Tfilestatus4=object(Tfilestatus)
            attrfile:longint;
            cblist:longint;
        end;
        Pfilestatus4=^Tfilestatus4;

        Tfilefindbuf1=object(Tfilestatus1)
            name:string;                {Also possible to use as ASCIIZ.
                                         The byte following the last string
                                         character is always zero.}
        end;
        Pfilefindbuf1=^Tfilefindbuf1;


        Tfilefindbuf2=object(Tfilestatus2)
            name:string;                {Also possible to use as ASCIIZ.
                                         The byte following the last string
                                         character is always zero.}
        end;
        Pfilefindbuf2=^Tfilefindbuf2;

        Tfilefindbuf3=object(Tfilestatus3)
            name:string;                {Also possible to use as ASCIIZ.
                                         The byte following the last string
                                         character is always zero.}
        end;
        Pfilefindbuf3=^Tfilefindbuf3;

        Tfilefindbuf4=object(Tfilestatus4)
            name:string;                {Also possible to use as ASCIIZ.
                                         The byte following the last string
                                         character is always zero.}
        end;
        Pfilefindbuf4=^Tfilefindbuf4;

{Find first file matching a filemask. In contradiction to DOS, a search
 handle is returned which should be closed with findclose when done.
 filemask       = Filemask to search.
 handle         = Search handle will be returned here, fill with -1 before
                  call.
 attrib         = Fileattributes to search for.
 Afilestatus    = Return buffer.
 cbfilestatus   = Size of return buffer.
 count          = Fill with maximum number of files to search for, the
                  actual number of matching files found is returned here.
 infolevel      = One of the ilxxxx constants. Consult IBM documentation
                  for exact meaning. For normal use: Use ilstandard and
                  use Pfilefindbuf3 for Afilestatus.}
function dosfindfirst(filemask:Pchar;var handle:longint;attrib:longint;
                      Afilestatus:Pfilestatus;cbfilestatus:longint;
                      var count:longint;infolevel:longint):word;
function dosfindfirst(const filemask:string;var handle:longint;
                      attrib:longint;Afilestatus:Pfilestatus;
                      cbfilestatus:longint;var count:longint;
                      infolevel:longint):word;

{Find next matching file.}
function dosfindnext(handle:longint;Afilestatus:Pfilestatus;
                     cbfilestatus:longint;var count:longint):word;

{Close a search handle. Cannot fail if handle does exist.}
function dosfindclose(handle:longint):word;

{Get info about a file.

 handle         = Handle of file.
 infolevel      = One of the ilxxxx constants. Consult IBM documentation
                  for exect meaning. For normal use: Use ilstandard and
                  Pfilefindbuf3 for Afilestatus.
 Afilestatus    = An info return buffer.
 cbfilestatus   = Size of info buffer.}
function dosqueryfileinfo(handle,infolevel:longint;Afilestatus:Pfilestatus;
                          cbfilestatus:longint):word;

{Set info about a file. File must be opened with write permissions. See
 above fo the parameters.}
function dossetfileinfo(handle,infolevel:longint;Afilestatus:Pfilestatus;
                        cbfilestatus:longint):word;

{Return info about a file. In contradiction to the above functions, the
 file does not have to be openened.}
function dosquerypathinfo(filenaam:Pchar;infolevel:longint;
                          Afilestatus:Pfilestatus;cbfilestatus:longint):word;
function dosquerypathinfo(const filenaam:string;infolevel:longint;
                          Afilestatus:Pfilestatus;cbfilestatus:longint):word;

{Set information about a file.}
function dossetpathinfo(filenaam:Pchar;infolevel:longint;
                        Afilestatus:Pfilestatus;cbfilestatus,
                        options:longint):word;

{Get info about the names and lengths of the EA's for a file or directory.

 reftype            = 0 = Afile is a pointer to a file-handle.
                      1 = Afile is a pointer to an ASCIIZ string.
 AFile              = Pointer file's name or handle.
 entry              = Number of EA to query inof about. (1 = first EA).
 buf                = Buffer where requested info is returned. For infolevel
                      1, the buffer is a TfEA2 datastructure.
 buflen             = Size of buf in bytes.
 count              = Number of EA's to return info for. Number of EA's that
                      actually fitted in buf is returned here.
 infolevel          = Level of information to return. Only level 1 is
                      currently allowed.}

function dosenumattribute(reftype:longint;Afile:pointer;
                          entry:longint;var buf;bufsize:longint;
                          var count:longint;infolevel:longint):word;
function dosenumattribute(handle:longint;
                          entry:longint;var buf;bufsize:longint;
                          var count:longint;infolevel:longint):word;
function dosenumattribute(const filenaam:string;
                          entry:longint;var buf;bufsize:longint;
                          var count:longint;infolevel:longint):word;

{Get an environment variabele.
 naam               = Name of environment variabele to get.
 value              = Receives pointer to environment string.}
function dosscanenv(naam:Pchar;var value:Pchar):word;
{There is, of course a string variant.}
function dosscanenv(const naam:string;var value:string):word;

const   dspathonly      = 0;    {Do not search current dir. (Unless it is
                                 in the directory list.)}
        dscurrentdir    = 1;    {Search in the current direcotry and in the
                                 directory list.}
        dsenvironment   = 2;    {The dirlist parameter is not a directory
                                 list, but an environment variabele
                                 containing one.}
        dsignoreneterrs = 4;    {Ignore network errors when searching.}

{Search for a file in a given number of directories.
 flags          = A combination of the dsxxxx constants.
 dirlist        = Directory list or environmant variabele containing list
                  to search in.
 filenaam       = Filename to search for. May contain wildcards.
 fullname       = Receives filename found, including path.
 fulllen        = Length of your fullname buffer.}
function dossearchpath(flag:longint;dirlist,filenaam:Pchar;
                       fullname:Pchar;fulllen:longint):word;
function dossearchpath(flag:longint;const dirlist,filenaam:string;
                       var fullname:string):word;

{****************************************************************************

                       File system related routines.

****************************************************************************}

type    TFSinfo=record
            case word of
                1:
                    (file_sys_ID,
                     sectors_per_cluster,
                     total_clusters,
                     free_clusters:longint;
                     bytes_per_sector:word);
                2:                          {For date/time description,
                                             see file searching realted
                                             routines.}
                    (label_date,            {Date when volumelabel created.}
                     label_time:word;       {Time when volumelabel created.}
                     volumelabel:string);   {Volume label. Can also be used
                                             as ASCIIZ, because the byte
                                             following the last character of
                                             the string is always zero.}
        end;
        PFSinfo=^TFSinfo;

        Tattachdata=record
            case integer of         {Flag in [0,1,2].}
                0,1:                {Flag = 0.}
                    (count:word;
                     data:Tchararray);
                2:                  {Flag = 2.}
                    (pipehandle:longint;
                                    {Handle of named pipe opened by spooler.}
                     spoolname:string);
                                    {Name of spooler object. Can also be used
                                     as ASCIIZ, because the bute following
                                     the last character is always zero.}
        end;
        Pattachdata=^Tattachdata;

        TFSqbuffer2=record
            _type:word;
            naamlen:word;
            FSDnaamlen:word;
            FSAdatalen:word;
            naam:char;
            nul1:byte;
            FSDnaam:char;
            nul2:byte;
            FSAdata:char;
            nul3:byte;
        end;
        PFSqbuffer2=^TFSQbuffer2;

const   fsattach        = 0;    {Attach a drive.}
        fsdetach        = 1;    {Detach a drive.}
        fsspoolattach   = 2;    {Attach a spool device.}
        fsspooldetach   = 3;    {Detach a spool device.}

{IBM DOCS: "DosFSAttach attaches or detaches a drive to or from a remote file
 system driver (FSD), or a pseudocharacter device name to or from a local or
 remote FSD."

 devnaam            = When flag is 0 or 1, the name of a drive or a pseudo-
                      character device. When using a drivename use the drive-
                      letter followed by a colon.
                      When flag is 2 or 3, the name of a spooled device.
 filesystem         = Name of the driver that should be attached or detached
                      to devnaam. Use nil when flag is 2 or 3.
 data               = Should contain a number of ASCIIZ strings that will
                      be passed to the filesystem driver when flag is 0 or 1.
                      Should contain de pipehandle and spoolname when flag is
                      2. Should be nil when flag is 3.
 datalen            = Number of bytes in data parameter.
 flag               = One of the dsxxxx constants. See above}
function dosFSattach(devnaam,filesystem:Pchar;var data:Tattachdata;
                     datalen,flag:longint):word;
function dosFSattach(const devnaam,filesystem:string;var data:Tattachdata;
                     datalen,flag:longint):word;

{IBMDOCS: "DosQueryFSAttach obtains information about an attached file system
 (local or remote), or about a character device or pseudocharacter device
 attached to the file system."

 devnaam            = Name info drive or pseudo character device to query
                      info about. Ignored for infolevels 2 and 3.
 ordinal            = Index into list of character/pseudo-character
                      devices. Starts at 1. Ignored for infolevel 1.
 infolevel          = 1 = Return information about a drive or device named
                          by devnaam.
                      2 = Return information about a (pseudo) charachter
                          device numbered by ordinal.
                      3 = Return information about a drive numbered by
                          ordinal.
 buffer             = Will be filled with infomation.
 buflen             = Size of your buffer in bytes. Number of bytes filled
                      in your buffer is returned here.}
function dosqueryFSattach(devnaam:Pchar;ordinal,infolevel:longint;
                          var buffer:TFSqbuffer2;var buflen:longint):word;
function dosqueryFSattach(const devnaam:string;ordinal,infolevel:longint;
                          var buffer:TFSqbuffer2;var buflen:longint):word;

const   FSCTL_handle=1;
        FSCTL_pathname=2;
        FSCTL_FSDname=3;
        FSCTL_error_info=1;
        FSCTL_max_EAsize=2;

{IBMDOCS: "DosFSCtl provides an extended standard interface between an
 application and a file-system driver (FSD).

 Consult IBM documentation about this function..}
function dosFSctl(data:pointer;datalen:longint;var resdatalen:longint;
                  parms:pointer;parmslen:longint;var resparmslen:longint;
                  _function:longint;route:Pchar;
                  handle,method:longint):word;
function dosFSctl(data:pointer;datalen:longint;var resdatalen:longint;
                  parms:pointer;parmslen:longint;var resparmslen:longint;
                  _function:longint;const route:string;
                  handle,method:longint):word;

{Get information about a drive.
Infolevels:
 1              Get total/free space etc.
 2              Get volumelabel.}
function dosqueryFSinfo(disknum,infolevel:longint;var buffer:TFSinfo;
                        buflen:longint):word;

{Set information about a drive.}
function dossetFSinfo(disknum,infolevel:longint;var buffer:TFSinfo;
                      buflen:longint):word;

{Check if verify mode is enabled.}
function dosqueryverify(var enabled:longint):word;

{Turn the verify mode on or off.}
function dossetverify(enable:longint):word;

{Change the number of filehandles our program can open. (Default=50). It
 won't hurt if there are files open when you are calling this.}
function dossetmaxFH(count:longint):word;

{Ask for more filehandles (or dump filehandles). It won't hurt if there are
 files open when you are calling this.
 reqcount       = Number of filehandles to ask for. (Negative to dump them.)
 curmaxFH       = Receives the total number of filehandles your program has
                  access to.}
function dossetrelmaxFH(var reqcount,curmaxFH:longint):word;

const   dsfull=0;       {IBM DOCS: "Perform full system shutdown and
                         file-system lock."}
        dsquiescient=1; {IBM DOCS: "Perform buffer and cache flushing to
                         make system quiescent."}

{Prepare the system for shutdown.}
function dosshutdown(flags:longint):word;


{Usefull constants fo dosQuerySysInfo.}
const   svmaxpathlength     = 1;        {Maximum length of a pathname.}
        svmaxtextsessions   = 2;        {Maximum number of text sessions.}
        svmaxPMsessions     = 3;        {Maximum number of PM sessions.}
        svmaxVDMsessions    = 4;        {Maximum number of DOS sessions.}
        svbootdrive         = 5;        {Get the boot drive. (A=1, B=2 etc.)}
        svdynprivariation   = 6;
        svmaxwait           = 7;
        svminslice          = 8;
        svmaxslice          = 9;
        svpagesize          = 10;       {Size of a page. (Always 4096 bytes.)}
        svmajorversion      = 11;       {Major version number of kernel:
                                         10 for OS/2 1.0 and 1.1,
                                         20 for OS/2 2.0 .. OS/2 4.0.}
        svminorversion      = 12;       {Minor version of kernel:
                                         OS/2 2.0: 00, 2.1: 10, 2.11: 11,
                                              3.0: 30, 4.0: 40.}
        svrevision          = 13;       {Revision of kernel. Until now all
                                         OS/2 versions return 0.}
        svmscount           = 14;       {Uptime in milliseconds.}
        svtimelow           = 15;       {System time in seconds since
                                         1 January 1970 0:00:00, low dword.}
        svtimehigh          = 16;       {System time in seconds since
                                         1 January 1970 0:00:00, high dword.}
        svphysmem           = 17;       {Amount in bytes of physical memory
                                         in system.}
        svresmem            = 18;       {Amount in bytes of resident memory
                                         in system.}
        svavailmem          = 19;       {Amount in bytes of available
                                         memory.}
        svprmem             = 20;       {Maximum amount of memory the current
                                         process can request for it's
                                         private use.}
        svshmem             = 21;       {Maximum amount of memory the current
                                         process can request for shared
                                         purposes.}
        svtimerinterval     = 22;       {Timer interval in tenths of a
                                         millisecond.}
        svmaxcomplength     = 23;       {Maxmimum length of a component in a
                                         pathname.}
        svforegroundsession = 24;       {Returns the session ID of the fore-
                                         ground session. The presentation
                                         manager has ID 1.}
        svforegroundprocess = 25;       {Returns the process ID of the
                                         current foreground process.}

{Get one or more system parameters.
 first          = First variabele to get.
 last           = Last variabele to get.
 buf            = Receives variabeles.
 bufsize        - Size of the buffer/}
function dosquerysysinfo(first,last:longint;var buf;bufsize:longint):word;

{Return information about a partitionable disk.}
function dosphysicaldisk(func:longint;buf:pointer;bufsize:longint;
                         params:pointer;paramsize:longint):word;

{****************************************************************************

                       Memory allocation related routines.

****************************************************************************}

const   mfpag_read      = $00001;   {Give read access to memory.}
        mfpag_write     = $00002;   {Give write access to memory.}
        mfpag_execute   = $00004;   {Allow code execution in memory.}
        mfpag_guard     = $00008;   {Used for dynamic memory growing. Create
                                     uncommitted memory and make the first
                                     page guarded. Once it is accessed it
                                     will be made committed, and the next
                                     uncommitted page will be made guarded.}
        mfpag_commit    = $00010;   {Make the memory committed.}
        mfpag_decommit  = $00020;   {Decommit the page.}
        mfobj_tile      = $00040;   {Also allocate 16-bit segments of 64k
                                     which map the memory. (Makes 16<>32 bit
                                     pointer conversion possible.}
        mfobj_protected = $00080;
        mfobj_gettable  = $00100;
        mfobj_giveable  = $00200;
        mfpag_default   = $00400;
        mfpag_shared    = $02000;
        mfpag_free      = $04000;
        mfpag_base      = $10000;

        mfsub_init      = $00001;   {Use base, if not set, choose a base
                                     address yourself.}
        mfsub_grow      = $00002;   {Grow the specified heap, instead of
                                     allocating it. Ignore fmsub_init.}
        mfsub_sparse    = $00004;
        mfsub_serialize = $00008;

{Get some memory.
 p          = Pointer to memory will be returned here.
 size       = Number of bytes to get. The size is rounded up to a multiple
              of 4096. This is propably not the case on non-intel 386
              versions of OS/2.
 flags      = One or more of the mfxxxx constants.}
function dosallocmem(var p:pointer;size:longint;flag:longint):word;

{Free a memory block.}
function dosfreemem(p:pointer):word;

{Set settings for a block of memory.
 p          = Pointer to the memory. Doesn't need to be the start of the
              memory block allocated with dosallocmem, but must be a multiple
              of 4096.
 cb         = Number of bytes to change settings for. Is rounded up to a
              multile of 4096.
 flags      = New flags for the memory.}
function dossetmem(p:pointer;cb,flag:longint):word;

{Give another process access to a shared memory block.

 p          = Pointer to the shared memory object.
 pid        = Process of destination process.
 flag       = Permissions the the destination process gets.}
function dosgivesharedmem(p:pointer;pid,flag:longint):word;

{Get access to a shared memory object.

 p          = Pointer to shared memory object.
 flag       = Permissions to ask.}
function dosgetsharedmem(p:pointer;flag:longint):word;

{Get access to a shared memory object that has a name.

 p          = Pointer to shared memory object.
 naa,       = Name of the memory object. (Starting with '\SHAREMEM\'.
 flag       = Permissions to ask.}
function dosgetnamedsharedmem(var p:pointer;naam:Pchar;flag:longint):word;
function dosgetnamedsharedmem(var p:pointer;const naam:string;
                              flag:longint):word;

{Allocate memory so that it can later be shared with another program.
 p          = Reveives pointer to memory.
 naam       = Optional: name to give memory. Must start with '\SHAREMEM\'.
              Use nil for the Pchar or '' for the string variant for no name.
 cm         = Number of bytes to allocate.}
function dosallocsharedmem(var p:pointer;naam:Pchar;size,flag:longint):word;
function dosallocsharedmem(var p:pointer;const naam:string;size,
                           flag:longint):word;

{Get the size and flags of a block of memory.

 p          = Pointer to the block of memory.
 size       = Receives block size.
 flag       = Receives the flags.}
function dosquerymem(p:pointer;var size,flag:longint):word;

{Allocate a block of memory in a heap.
 base       = Pointer to the start of the heap.
 p          = Receives pointer to the memory bock.
 cb         = Number of bytes to allocate.}
function dossuballocmem(base:pointer;var p:pointer;size:longint):word;

{Free a block of memory in a heap.
 base       = Pointer to the start of the heap.
 p          = Pointer to memory block to free.
 cb         = Number of bytes to free.}
function dossubfreemem(base,p:pointer;size:longint):word;

{Turn a block of memory into a heap.

base        = Pointer to memory block to turn into a heap.
flag        = One or more of the mfsub_xxxx flags.
cb          = Size of the requested heap.}
function dossubsetmem(base:pointer;flag:longint;size:longint):word;

{Destroy a heap. (Memory remains allocated).

base        = Pointer to the heap to destroy.}
function dossubunsetmem(base:pointer):word;

{****************************************************************************

                        Semaphore related routines

****************************************************************************}

const   smshared        = $0001;    {Semafore is shared.}
        smMWwaitany     = $0002;    {Muxwait only: Wait until a semafore
                                     is cleared.}
        smMWwaitall     = $0004;    {Muxwait only: Wait until all semafores
                                     are cleared.}

type   Psemrecord=^Tsemrecord;
       Tsemrecord=record
          semafore,                 {Handle of semafore to link.}
          user:longint;
       end;

       Psemarray=^Tsemarray;
       Tsemarray=array[0..$ffff] of Tsemrecord;

{Create an event semafore.
 naam       = Optional: Name of semafore to create. Must start with '\SEM32\.
              Use nil for Pchar or '' for string variant for noname. A
              unnamed semafore is not shared unless the sm_shared flag is
              set.
 handle     = Receives handle of semafore.
 attr       = One or more of the smxxxx constants.
 state      = Initial state: 0 = Reset (False), 1 = Posted (True).}
function doscreateeventsem(naam:Pchar;var handle:longint;
                           attr,state:longint):word;
function doscreateeventsem(const naam:string;var handle:longint;
                           attr,state:longint):word;

{Open a semafore created by another process or thread.

 naam       = Name of semafore.
 handle     = Receives handle of semafore.}
function dosopeneventsem(naam:Pchar;var handle:longint):word;
function dosopeneventsem(const naam:string;var handle:longint):word;

{Close an event semafore.
 handle     = Handle of semofore to close.}
function doscloseeventsem(handle:longint):word;

{Reset an event semafore: probeer operation.
 handle     = Handle of semafore.
 postcount  = Number of times dosposteventsem has been called since last
              reset.

 Note:      Returns errorcode 300 if semafore is already reset.}
function dosreseteventsem(handle:longint;var postcount:longint):word;

{Post an event semafore: verhoog operation.
 handle     = Handle of semafore.

Note:       Returns errorcode 299 if semafore is already posted.}
function dosposteventsem(handle:longint):word;

{Wait until an event semafore is posted (wait until verhoog operation).
 handle     = Handle of semafore.
 timeout    = Return with errorcode if timeout milliseconds have past and the
              semafore is still reset. To return immideatly use 0,
              to wait forever use -1.}
function doswaiteventsem(handle,timeout:longint):word;

{Check if an event semafore is posted (if a verhoog operation has been done).
 handle     = Handle of semafore.
 posted     = Receives number of times dosposteventsem was called since
              the last reset.}
function dosqueryeventsem(handle:longint;var posted:longint):word;

{Create a Mutual Exclusion semafore (Mutex).
 naam       = Optional: Name to give semafore. Must start with '\SEM32\'.
              Use nil for Pchar or '' for string variant to use no name.
              If a name if used the semafore is shared.
 handle     = Receives handle of semafore.
 attr       = One or more of the smxxxx constants.
 state      = Initial state: (0=Unowned, 1=Owned.)}
function doscreatemutexsem(naam:Pchar;var handle:longint;
                           attr,state:longint):word;
function doscreatemutexsem(const naam:string;var handle:longint;
                           attr,state:longint):word;

{Open a shared mutex semafore.
 naam       = Name of semafore to open, always starts with '\SEM32\'.
 handle     = Receives handle to semafore.}
function dosopenmutexsem(naam:Pchar;var handle:longint):word;
function dosopenmutexsem(const naam:string;var handle:longint):word;

{Close a mutex semafore.
 handle     = Handle of semafore to close.}
function dosclosemutexsem(handle:longint):word;

{Request ownership of a mutex semafore. If the semafore is already owned the
 process is halted until the semafore is released.
 handle     = Handle of semafore.
 timeout    = Return with errorcode if the semafore is still owned after
              timeout milliseconds.}
function dosrequestmutexsem(handle,timeout:longint):word;

{Release the ownership of a mutex semafore.
 handle     = Handle of semafore to release.}
function dosreleasemutexsem(handle:longint):word;

{Query the pid and tib of the owner of a mutex semafore.
 handle     = Handle of semafore.
 pid        = Receives process id of owner.
 tib        = Receives thread if of owner.
 count      = Number of threads (within and outside current process) waiting
              for ownership of semafore.}
function dosquerymutexsem(handle:longint;var pid,tid,count:longint):word;

{Create a Multiple Wait (Muxwait) semafore.
 naam       = Optional: Name to give semafore. Must start with '\SEM32\'.
              Use nil for Pchar or '' for string variant to use no name.
              If a name if used the semafore is shared.
 handle     = Receives handle of semafore.
 csemrec    = Number of semafores to link muxwait semafore with.
 semarray   = Array of semafore records to link with muxwait semafore.
 attr       = One or more of the smxxxx constants.}
function doscreatemuxwaitsem(naam:Pchar;var handle:longint;csemrec:longint;
                             var semarray:Tsemarray;attr:longint):word;
function doscreatemuxwaitsem(const naam:string;var handle:longint;
                             csemrec:longint;var semarray:Tsemarray;
                             attr:longint):word;

{Open a muxwait semafore.
 naam       = Name of semafore to open.
 handle     = Receives handle of semafore.}
function dosopenmuxwaitsem(naam:Pchar;var handle:longint):word;
function dosopenmuxwaitsem(const naam:string;var handle:longint):word;

{Close a mutex semafore.}
function dosclosemuxwaitsem(handle:longint):word;

{Wait for the muxwait semafore to be cleared.
 handle     = Handle of semafore.
 timeout    = Timeout. See above.
 user       = Receives user value of the semafore that caused the muxwait
              semafore to be cleared.}
function doswaitmuxwaitsem(handle,timeout:longint;var user:longint):word;

{Add a semafore to the muxwait semafore.

 handle     = Handle of semafore.
 semrec     = The semafore to add.}
function dosaddmuxwaitsem(handle:longint;var semrec:Tsemrecord):word;

{Remove a semafore from the muxwait semafore.
 handle     = Handle of muxwait semafore.
 sem        = Handle of semafore to remove.}
function dosdeletemuxwaitsem(handle,sem:longint):word;

{Query the semafores from a muxwait semafore.
 handle     = Handle of semafore.
 csemrec    = Input: Size of our array. Output: Number of items in array.
 semrecs    = Array where Tsemrecords are stored.
 attr       = Flags used by creation of semafore.}
function dosquerymuxwaitsem(handle:longint;var csemrec:longint;
                            var semrecs:Tsemarray;var attr:longint):word;

{****************************************************************************

                        Timing related routines.

****************************************************************************}


type    Tdatetime=record
            hour,
            minute,
            second,
            sec100,
            day,
            month:byte;
            year:word;
            timezone:integer;
            weekday:byte;
        end;
        Pdatetime=^Tdatetime;


{Get the date and time.}
function dosgetdatetime(var buf:Tdatetime):word;

{Set the date and time.}
function dossetdatetime(var buf:Tdatetime):word;

{Start a one shot timer.
 msec       = Number of miliseconds the timer will run.
 hsem       = Handle of event semafore that is posted when time has expired.
 TIMhandle  = Receives timer handle.}
function dosasynctimer(msec:longint;hsem:longint;
                       var TIMhandle:longint):word;

{Start a cyclic timer.
 msec       = Number of miliseconds the timer will run.
 hsem       = Handle of event semafore that is posted when time has expired.
 TIMhandle  = Receives timer handle.}
function dosstarttimer(msec:longint;hsem:longint;
                       var TIMhandle:longint):word;

{Stop a timer and destroy it's handle. There is no need to check for an
 error code if you know your timer handle is correct.}
function dosstoptimer(TIMhandle:longint):word;

{Get the frequency of the high resolution timer.}
function dostmrqueryfreq(var freq:longint):word;

{Get the current value of the high resolution timer.}
function dostmrquerytime(var time:comp):word;

{****************************************************************************

                             DLL specific routines.

****************************************************************************}

{Load a DLL in memory if it is not yet loaded.
 objnaam        = When the DLL cannot be found, or one of the DLL's it needs
                  cannot be found, the name of the DLL will be put here.
 objlen         = Size of the objnaam result buffer.
 DLLnaam        = Name of DLL to load. Do not give an extension or a path,
                  just the name. OS/2 will automatically search through the
                  LIBPATH for the DLL.
 handle         = Receives DLL handle.}
function dosloadmodule(objnaam:Pchar;objlen:longint;DLLnaam:Pchar;
                       var handle:longint):word;
function dosloadmodule(var objnaam:string;objlen:longint;
                       const DLLnaam:string;var handle:longint):word;

{Let OS/2 know that we do not need a DLL anymore. If we were the only process
 using the DLL, it is unloaded.}
function dosfreemodule(handle:longint):word;

{Get the address of a procedure.
 handle         = DLL handle,
 ordinal        = Procedure to get address form. 0=Use it's name.
 procnaam       = Name of the procedure to query address for. Must be nil
                  for Pchar or '' for string variant if ordinal is nonzero.
 adres          = Receives address of procedure.}
function dosqueryprocaddr(handle,ordinal:longint;procnaam:Pchar;
                          var adres:pointer):word;
function dosqueryprocaddr(handle,ordinal:longint;
                          const procnaam:string;var adres:pointer):word;

{Get the handle of a loaded DLL or a loaded executable.
 DLLnaam        = Name of DLL.
 handle         = Receives DLL handle if present.}
function dosquerymodulehandle(DLLnaam:Pchar;var handle:longint):word;
function dosquerymodulehandle(const DLLnaam:string;var handle:longint):word;

{Get the pathname of a loaded DLL or a loaded executable.

 handle         = Handle of DLL.
 naamlen        = Maximum length of char array.
 naam           = Chararray (or string) where name is returned.}
function dosquerymodulename(handle,naamlen:longint;naam:Pchar):word;
{function dosquerymodulename(handle:longint;var naam:openstring):word;}

const   pt16bit=0;
        pt32bit=1;

{Return if a procedure is either 16 or 32 bit.
 handle         = Handle of DLL.
 ordinal        = DLL index number. 0 means use naam.
 naam           = Must be nil for Pchar or '' for string variant if ordinal
                  is zero. Otherwise it contains the procname.
 proctype       = One of the ptxxxx constants.}
function dosqueryproctype(handle,ordinal:longint;naam:Pchar;
                          var proctype:longint):word;
function dosqueryproctype(handle,ordinal:longint;const naam:string;
                          var proctype:longint):word;

{****************************************************************************

                           Resource related routines.

****************************************************************************}

{Possible resource types:}

const   rtpointer       =  1;       {Mouse pointer.}
        rtbitmap        =  2;       {Bitmap}
        rtmenu          =  3;       {Menu template.}
        rtdialog        =  4;       {Dialog template.}
        rtstring        =  5;       {A string table.}
        rtfontdir       =  6;       {Font directory.}
        rtfont          =  7;       {A font.}
        rtacceltable    =  8;       {Accelerator table.}
        rtrcdata        =  9;       {Binary data.}
        rtmessage       = 10;       {Error message table.}
        rtdlginclude    = 11;       {Dialog include filename.}
        rtvkeytbl       = 12;       {Key to vkey tables.}
        rtkeytbl        = 13;       {Key to ugl tables.}
        rtchartbl       = 14;       {Glyph to character tables.}
        rtdisplayinfo   = 15;       {Screen display information.}
        rtfkashort      = 16;       {Function key area short form.}
        rtfkalong       = 17;       {Function key area long form.}
        rthelptable     = 18;       {Help table.}
        rthelpsubtable  = 19;       {Sub help table.}
        rtfddir         = 20;       {DBCS unique/font driver directory.}
        rtfd            = 21;       {DBCS unique/font driver.}

{Get the address of a resource object.
 handle         = Handle of DLL (or executable) to get resource from.
 restype        = One of the RT_xxxx constants.
 resnaam        = Number associated to resource object by resource compiler.}
function dosgetresource(handle,restype,resnaam:longint;var p:pointer):word;

{Remove a resource object from memory.
 p              = Pointer to resource.}
function dosfreeresource(p:pointer):word;

{Get the size of a resource object.
 handle         = Handle to DLL (or executable).
 idt            = One of the RT_xxxx constants.
 idn            = Number associated to resource object by resource compiler.
 size           = Receives resource size.}
function dosqueryresourcesize(handle,idt,idn:longint;var size:longint):word;


{****************************************************************************

                   Country and codepage specific routines.

****************************************************************************}

type    Tcountrycode=record
            country,            {Country to query info about (0=current).}
            codepage:longint;   {Code page to query info about (0=current).}
        end;

        Ttimefmt=(clock12,clock24);

        Tcountryinfo=record
            country,codepage,               {Country and codepage requested.}
            dateformat:longint;             {1=ddmmyy 2=yymmdd 3=mmddyy}
            currencyunit:array[0..4] of char;
            thousandseparator:char;         {Thousands separator.}
            zero1:byte;                     {Always zero.}
            decimalseparator:char;          {Decimals separator,}
            zero2:byte;
            dateseparator:char;             {Date separator.}
            zero3:byte;
            timeseparator:char;             {Time separator.}
            zero4:byte;
            currencyformat,                 {Bit field:
                                             Bit 0: 0=indicator before value
                                                    1=indicator after value
                                             Bit 1: 1=insert space after
                                                      indicator.
                                             Bit 2: 1=Ignore bit 0&1, replace
                                                      decimal separator with
                                                      indicator.}
            decimalplace:byte;              {Number of decimal places used in
                                             currency indication.}
            timeformat:Ttimefmt;            {12/24 hour.}
            reserve1:array[0..1] of word;
            dataseparator:char;             {Data list separator.}
            zero5:byte;
            reserve2:array[0..4] of word;
        end;
        Pcountryinfo=^Tcountryinfo;

        TDBCSrange=record
            start,stop:byte;
        end;

        TDBCSarray=array[0..$ffff] of TDBCSrange;
        PDBCSarray=^TDBCSarray;

const   currentcountry:Tcountrycode=(country:0;codepage:0);

{Get country specific information.
    cb          = Size of our datastructure. (sizeof(Tcountryinfo))
    cbactual    = Size of OS/2's datastructure. cbactual bytes of
                  our Tcountryinfo have been filled.}
function dosqueryctryinfo(cb:longint;var country:Tcountrycode;
                          var res:Tcountryinfo;var cbactual:longint):word;

{Get info about a code page with a DBCS character set.}
function dosqueryDBCSenv(cb:longint;var country:Tcountrycode;buf:Pchar):word;

{Convert a string to uppercase.
    cb          = Length of string.
    country     = Country and codepage for converting.
    Astring     = String to convert.}
function dosmapcase(cb:longint;var country:Tcountrycode;
                    Astring:Pchar):word;
function dosmapcase(var country:Tcountrycode;
                    var Astring:string):word;

{Get a collate table. (A table which says which character if a character is
 smaller than another.
    cb          = Length of the databuffer the program has.
    country     = Country to query table for. (0,0) is default country and
                  codepage.
    buf         = Buffer to return table in. It's filled with the sort
                  weights of the ascii code. For example the 128th byte
                  contains the weight for ascii code 128.
    tablelen    = Length of collating table.}
function dosquerycollate(cb:longint;var country:Tcountrycode;
                         buf:Pbytearray;var tablelen:longint):word;

{Get the current codepage. The Pwordarray is filled with the current code
 page followed by alternative codepages.}
function dosquerycp(cb:longint;codepages:Pwordarray;var actcb:longint):word;

{Change the codepage, but only for the current process.}
function dossetprocesscp(cp:longint):word;

{****************************************************************************

                       Exception handling related functions

****************************************************************************}


{Exception constants.}
const       XCPT_continue_search            = $00000000;
            XCPT_continue_execution         = $ffffffff;
            XCPT_continue_stop              = $00716668;

            XCPT_signal_intr                = $1;
            XCPT_signal_killproc            = $3;
            XCPT_signal_break               = $4;

            XCPT_fatal_exception            = $c0000000;
            XCPT_severity_code              = $c0000000;
            XCPT_customer_code              = $20000000;
            XCPT_facility_code              = $1fff0000;
            XCPT_exception_code             = $0000ffff;

            XCPT_unkwown_access             = $00000000;
            XCPT_read_access                = $00000001;
            XCPT_write_access               = $00000002;
            XCPT_execute_access             = $00000004;
            XCPT_space_access               = $00000008;
            XCPT_limit_access               = $00000010;
            XCPT_data_unknown               = $ffffffff;

            XCPT_guard_page_violation       = $80000001;
            XCPT_unable_to_grow_stack       = $80010001;
            XCPT_access_violation           = $c0000005;
            XCPT_in_page_error              = $c0000006;
            XCPT_illegal_instruction        = $c000001c;
            XCPT_invalid_lock_sequence      = $c000001d;
            XCPT_noncontinuable_exception   = $c0000024;
            XCPT_invalid_disposition        = $c0000025;
            XCPT_unwind                     = $c0000026;
            XCPT_bad_stack                  = $c0000027;
            XCPT_invalid_unwind_target      = $c0000028;
            XCPT_array_bounds_exceeded      = $c0000093;
            XCPT_float_denormal_operand     = $c0000094;
            XCPT_float_divide_by_zero       = $c0000095;
            XCPT_float_inexact_result       = $c0000096;
            XCPT_float_invalid_operation    = $c0000097;
            XCPT_float_overflow             = $c0000098;
            XCPT_float_stack_check          = $c0000099;
            XCPT_float_underflow            = $c000009a;
            XCPT_integer_divide_by_zero     = $c000009b;
            XCPT_integer_overflow           = $c000009c;
            XCPT_privileged_instruction     = $c000009d;
            XCPT_datatype_misalignment      = $c000009e;
            XCPT_breakpoint                 = $c000009f;
            XCPT_single_step                = $c00000a0;
            XCPT_process_terminate          = $c0010001;
            XCPT_async_process_terminate    = $c0010002;
            XCPT_signal                     = $c0010003;

type        Pexceptionregistrationrecord=^Texceptionregistrationrecord;
            Pexceptionreportrecord=^Texceptionreportrecord;
            Pcontextrecord=^Tcontextrecord;

            Texceptionhandler=procedure(report:Pexceptionreportrecord;
                                        regrec:Pexceptionregistrationrecord;
                                        context:Pcontextrecord;
                                        dispcontext:pointer);

            Texceptionregistrationrecord=record
                prev_structure:Pexceptionregistrationrecord;
                exceptionhandler:Texceptionhandler;
            end;

            Texceptionreportrecord=record
                exception_num,
                handlerflags:longint;
                nested_reprec:Pexceptionreportrecord;
                address:pointer;
                paramcount:longint;
                parameters:array [0..9999] of longint;
            end;

            Tcontextrecord=record
                contextflags:longint;
                env:array[1..7] of longint;
                fpustack:array[0..7] of extended;
                reg_gs,
                reg_fs,
                reg_es,
                reg_ds,
                reg_edi,
                reg_esi,
                reg_eax,
                reg_ebx,
                reg_ecx,
                reg_edx,
                reg_ebp,
                reg_eip,
                reg_cs,
                flags,
                reg_esp,
                reg_ss:longint;
            end;

{Warning!!! Never use presentation manager functions from exception
 handlers!}

{Install an exceptionhandler. The prev_structure field of regrec should be
nil, it will be filled in be OS/2. Regrec must be on the stack: It must be a
local variabele.}
function dossetexceptionhandler(var regrec:Texceptionregistrationrecord
                                ):word;

{Uninstall an exception handler.}
function dosunsetexceptionhandler(var regrec:Texceptionregistrationrecord
                                  ):word;

{Trigger an exception.}
function dosraiseexception(var excpt:Texceptionreportrecord):word;

{Send a signal to a process.}
function dossendsignalexception(pid,exception:longint):word;

{Call and remove a set of exceptionhandlers}
function dosunwindexception(var handler:Texceptionregistrationrecord;
                            targetIP:pointer;
                            var reprec:Texceptionreportrecord):word;

{Full screen applications can get ctrl-c and ctrl-brk focus. For all
 processed sharing one screen, only only v=can have ctrl-c focus.
 enable     = 0 = Release focus, 1 = Get focus.
 times      = Number of times focus has been get minus number of times it
              has been released.}
function dossetsignalexceptionfocus(enable:longint;var times:longint):word;

{Tell OS/2 that if an exception occurs, it must queue it up, until a
 dosExitMustComplete follows. Urgent exceptions still occur. The only
 possible error is that the nesting becomes too high, so error checking
 is only needed in seldom cases.
 nesting    = Number of dosEnterMustComplete calls minus number of
              dosExitMustComplete calls.}
function dosentermustcomplete(var nesting:longint):word;

{Tell OS/2 that it can send exceptions again. See above}
function dosexitmustcomplete(var nesting:longint):word;

{Tell we want further signal exceptions.
 signalnum  = Signal nummer to acknowlegde.}
function dosacknowledgesignalexception(signalnum:longint):word;


{****************************************************************************

                           Queue related routines.

****************************************************************************}

type    Trequestdata=record
            pid,                {ID iof process that wrote element.}
            data:longint;       {Information from process writing the data.}
        end;
        Prequestdata=^Trequestdata;

{Usefull constants for priority parameters.}
const   quFIFO=0;
        quLIFO=1;
        qupriority=2;
        qunoconvert_address=0;
        quconvert_address=4;

{Close a queue. If the calling process has created the queue, it is
 destroyed. If you can guarantee the handle is correct, there is no need
 to check for error codes.}
function dosclosequeue(handle:longint):word;

{Create a queue. The process that creates a queue, owns that queue, and is
 the only one who can read from that queue. Other processes can only write
 to that queue. The queuename must have the format '\QUEUES\name.ext' .

 handle         = Receives queue handle.
 priority       = 0 = Use FIFO system.
                  1 = Use LIFO system.
                  2 = Use priority system.
                  Add 4 to convert addresses of data inserted by 16-bit
                  processes to 32 bit pointers.
 naam           = Name of queue to create.}
function doscreatequeue(var handle:longint;priority:longint;
                        naam:Pchar):word;
function doscreatequeue(var handle:longint;priority:longint;
                        const naam:string):word;

{Open an existing queue. You cannot read from the queue unless you are the
 process that created it. The name must have the format '\QUEUES\name.ext'}
function dosopenqueue(var parent_pid:longint;var handle:longint;
                      naam:Pchar):word;
function dosopenqueue(var parent_pid:longint;var handle:longint;
                      const naam:string):word;

{Read a record from a queue, but do not remove it from the queue.
 handle         = Handle of queue to read from.
 reqbuffer      = Receives information about read data.
 datalen        = Receives length of data read.
 dataptr        = Receives the address of the data.
 element        = 0 = Return first element in queue.
                  1 = Return next element in queue. Can be repeated.
                      Current element number is returned here, for use
                      with dosreadqueue.
 wait           = 0 = Wait until there is a queue element available.
                  1 = Return with an error when queue is empty.
 priority       = Receives priority of queue record (1..15).
 Asem           = Use NIL if wait=0, give a handle of a semafore when
                  wait=1. The semafore will be cleared when there is an
                  element inserted it the queue.
                  !! event queue}
function dospeekqueue(handle:longint;var reqbuffer:Trequestdata;
                      var datalen:longint;var dataptr:pointer;
                      var element:longint;wait:longint;
                      var priority:byte;Asem:longint):word;

{Empty a queue. You must be the process the created the queue.}
function dospurgequeue(handle:longint):word;

{Return the number of elements in the queue.}
function dosqueryqueue(handle:longint;var count:longint):word;

{Read a record from a queue, but do not remove it from the queue.
 handle         = Handle of queue to read from.
 reqbuffer      = Receives information about read data.
 datalen        = Receives length of data read.
 dataptr        = Receives the address of the data.
 element        = 0 = Return first element in queue.
                  Otherwise: Return the element numbered with this.
 wait           = 0 = Wait until there is a queue element available.
                  1 = Return with an error when queue is empty.
 priority       = Receives priority of queue record (1..15).
 Asem           = Use NIL if wait=0, give a handle of a semafore when
                  wait=1. The semafore will be cleared when there is an
                  element inserted it the queue.
                  !! event queue}
function dosreadqueue(handle:longint;var reqbuffer:Trequestdata;
                      var datalen:longint;var dataptr:pointer;
                      element,wait:longint;var priority:byte;
                      Asem:longint):word;

{Write a data record to a queue.
 handle         = Handle of queue to write to.
 request        = Value that will be inserted in the requestdata field when
                  element is read from queue.
 datalen        = Size of data to write.
 databuf        = Data to write.
 priority       = Priority of data in buffer. On relevant when queue is
                  created with priority support.}
function doswritequeue(handle,request,datalen:longint;var databuf;
                       priority:longint):word;

const   deharderr           = 1;    {Hard errors are enabled, to disable
                                     do not give this switch.}
        dedisableexceptions = 2;    {Exceptions are disabled, to enable
                                     do not give this switch.}

{****************************************************************************

                        Error handling related routines.

****************************************************************************}

{Disable the end user notification of hardware errors and exceptions. Users
 can overide this in config.sys. By default, notification is enabled.
 There is no need for error checking if you can guarantee the parameter is
 correct.}
function doserror(error:longint):word;

{Get information about an error code.
 It cannot fail, so it is written as procedure.

code            = Error code to get info about.
_class          = Receives the error class.
action          = Receives the recommended action you should take.
locus           = Receives what could have caused the error.}
procedure doserrclass(code:longint;var _class,action,locus:longint);


{****************************************************************************

                        Message file specific routines.

****************************************************************************}


type    Pinserttable=^Tinserttable;
        Tinserttable=array[1..9] of Pchar;

{Get a message from a messagefile.
 table          = Table of strings to insert.
 tablesize      = Number of strings in table.
 buf            = Address of buffer to store message in.
 bufsize        = Size of buffer to store message in.
 msgnumber      = Number of message to get.
 filenaam       = Name of file to get message from.
 msgsize        = The size of the message returned.}
function dosgetmessage(table:Pinserttable;tablesize:longint;buf:Pchar;
                       bufsize,msgnumber:longint;filenaam:Pchar;
                       var msgsize:longint):word;
{And a variant using strings and open arrays.
function dosgetmessage(const table:array of Pstring;var buf:string;
                       bufsize,msgnumber:longint;const filenaam:Pchar):word;}

{And a variant using strings, but with a Pchar buffer, because of long
 messages, and open arrays.
function dosgetmessage(const table:array of Pstring;buf:Pchar;
                       bufsize,msgnumber:longint;const filenaam:string;
                       msgsize:longint):word;}

{Insert textstrings into a message. The message must be loaded before with
 dosGetMessage. This function is used when the insert strings are not yet
 known when the message was loaded.
 table          = Table of strings to insert.
 tablesize      = Number of struings to insert.
 message        = Message to insert strings into.
 srcmessagesize = Size of message to insert strings into.
 buf            = Receives adjusted message.
 bufsize        = Size of your buffer.
 dstmessagesize = Receives size of adjusted message.}
function dosinsertmessage(table:Pinserttable;tablesize:longint;
                          message:Pchar;srcmessagesize:longint;
                          buf:Pchar;bufsize:longint;
                          var dstmessagesize:longint):word;
{And a variant using strings and open arrays.
function dosinsertmessage(table:array of Pstring;
                          const message:string;
                          var buf:openstring):word;}

{And a variant using strings, but with a Pchar buffer, because of long
 messages, and open arrays.
function dosinsertmessage(table:array of Pstring;
                          message:Pchar;srcmessagesize:longint;
                          buf:Pchar;bufsize:longint;
                          var dstmessagesize:longint):word;}

{Write a message to a file.
 handle         = Handle of file.
 size           = Size of message.
 buf            = Buffer where message is located.}
function dosputmessage(handle,size:longint;buf:Pchar):word;
function dosputmessage(handle:longint;const buf:string):word;

{Get info about which codepages and languages a messagefile supports.
 buf            = Receives information.
 bufsize        = Size of buffer.
 filenaam       = Filename of message file.
 infosize       = Receives size in bytes of the returned info.}
function dosquerymessageCP(var buf;bufsize:longint;filenaam:Pchar;
                            var infosize:longint):word;
function dosquerymessageCP(var buf;bufsize:longint;const filenaam:string;
                            var infosize:longint):word;

{****************************************************************************

                           Session specific routines.

****************************************************************************}

type    Tstatusdata=record
            length:word;                {Length, in bytes, of datastructure.}
            selectind:word;             {Determines if the session can be
                                         selected: Don't change/selectable/
                                         not selectable (0/1/2).}
            bondind:word;               {Determines which section will come
                                         to the foreground when it is
                                         selected: Don't change/child to
                                         foreground when parent selected/
                                         parent to foreground when parent
                                         selected.}
        end;
        Pstatusdata=^Tstatusdata;

type    Tstartdata=record
        {Note: to omit some fields, use a length smaller than
         sizeof(Tstartdata).}
            length:word;                {Length, in bytes, of datastructure.}
            related:word;               {Independent/child session (0/1).}
            fgbg:word;                  {Foreground/background (0/1).}
            traceopt:word;              {No trace/trace this/trace all
                                         (0/1/2).}
            pgmtitle:Pchar;             {Program title.}
            pgmnaam:Pchar;              {Filename to program.}
            pgminputs:Pchar;            {Command parameters (nil allowed).}
            termq:Pchar;                {System queue. (nil allowed).}
            environment:Pchar;          {Environment to pass (nil allowed).}
            inheritopt:word;            {Inherite enviroment from shell/
                                         inherite environment from parant
                                         (0/1).}
            sessiontype:word;           {Auto/full screen/window/presentation
                                         manager/full screen dos/windowed dos
                                         (0/1/2/3/4/5/6/7).}
            iconfile:Pchar;             {Icon file to use (nil allowed).}
            pgmhandle:longint;          {0 or the program handle.}
            pgmcontrol:word;            {Bitfield describing initial state
                                         of windowed sessions.}
            initXpos,initYpos:word;     {Initial top coordinates.}
            initXsize,initYsize:word;   {Initial size.}
            reserved:word;
            objectbuffer:Pchar;         {If a module cannot be loaded, it's
                                         name will be returned here.}
            objectbufflen:longint;      {Size of your huffer.}
        end;
        Pstartdata=^Tstartdata;

{Start a new session.
 Astartdata         = A startdata record.
 sesid              = Receives session ID of session created.
 pid                = Receives process ID of process created.}
function dosstartsession(const Astartdata:Tstartdata;
                         var sesid,pid:longint):word;

{Set the status of a child session.
 sesid              = ID of session.
 Astatus            = Status to set.}
function dossetsession(sesid:longint;const Astatus:Tstatusdata):word;

{Bring a child session to the foreground.
 sesid              = ID of session.}
function dosselectsession(sesid:longint):word;

{Terminate (a) child session(s).
 scope              = 0 = Terminate specified session.
                      1 = Terminate all child sessions.
 sesid              = ID of session to terminate (ignored when terminating
                      all).}
function dosstopsession(scope,sesid:longint):word;

{****************************************************************************

                     Named/unnamed pipe specific routines.

****************************************************************************}

type    Tavaildata=record
            cbpipe,             {Number of bytes in pipe.}
            cbmessage:word;     {Number of bytes in current message.}
        end;

        Tpipeinfo=record
            cbout:word;         {Size of outbound data.}
            cbin:word;          {Size of inbound data.}
            maxinst:byte;       {Maximum number of instances.}
            curinst:byte;       {Current number of instances.}
            naam:string;        {Name of the pipe. You can use @naam[1] if
                                 you need a Pchar to the name; the string is
                                 always followed byte a zero.}
        end;

        Tpipesemstate=record
            status:byte;
            flag:byte;
            key:word;
            avail:word;
        end;

{Create an unnamed pipe.
 readhandle     = Receives handle for reading from pipe.
 writehanlde    = Receives handle to write to pipe.
 size           = Size of pipe to create. 0 means default size. If data is
                  written into a pipe that is smaller than the sent data, the
                  writing thread is suspended until the data has been read
                  from the pipe, thus making room for more data to send.}
function doscreatepipe(var readhandle,writehandle:longint;
                       size:longint):word;

const   {np_xxxx constants for openmode.}
        np_access_inbound       = $0000;    {Client to server connection.}
        np_access_outbound      = $0001;    {Server to client access.}
        np_access_duplex        = $0002;    {Two way access.}
        np_inherite             = $0080;    {Pipe handle is inherited by
                                             child processes.}
        np_no_write_behind      = $4000;    {Don't allow write behind for
                                             remote pipes.}
        {np_xxxx constants for pipemode.}
        np_unlimited_instances  = $00ff;    {Unlimited instances.}
        np_readmode_mesg        = $0100;    {Read the pipe as a message
                                             stream instead of as a byte
                                             stream.}
        np_writemode_mesg       = $0400;    {Write the pipe as a message
                                             stream instead of as a byte
                                             stream.}
        np_nowait               = $8000;    {Dosread and Doswrite do not
                                             wait is no data can be read or
                                             written; they return with an
                                             error message.}

{Create a named pipe.
 naam           = Name of pipe to create.
 handle         = Receives handle to pipe.
 openmode       = A combinations of the np_xxxx constants for openmode.
 pipemode       = A combination of the np_xxxx constants for pipemode,
                  plus a number within [1..254] which determines the number
                  of instances that can be created to the pipe, or,
                  np_unlimited_instance for an unlimited number of
                  instances.
 outbufsize     = The number of bytes to allocate for the output buffer.
 inbufsize      = The number of bytes to allocate for the input buffer.
 msec           = The maximum time to wait for an available instance.}
function doscreatenpipe(naam:Pchar;var handle:longint;openmode,pipemode,
                        outbufsize,inbufsize,msec:longint):word;
function doscreatenpipe(const naam:string;var handle:longint;openmode,
                        pipemode,outbufsize,inbufsize,msec:longint):word;

{Makes a procedure call to a duplex message pipe.
 naam           = Name of pipe.
 input          = Buffer that contains data to be written to the pipe.
 inputsize      = Size of the inputdata.
 output         = Buffer that contains data to be read from the pipe.
 outputsize     = Size of the outputbuffer.
 readbytes      = Receives number of bytes actually read.
 msec           = The maximum time to wait for an available instance.}
function doscallnpipe(naam:Pchar;var input;inputsize:longint;
                      var output;outputsize:longint;var readbytes:longint;
                      msec:longint):word;
function doscallnpipe(const naam:string;var input;inputsize:longint;
                      var output;outputsize:longint;var readbytes:longint;
                      msec:longint):word;

{Prepare a named pipe for a client process.
 handle         = Handle that was returned when pipe was created.}
function dosconnectnpipe(handle:longint):word;

{Acknowledges that a client process has closed a named pipe.
 handle         = Handle that was returned when pipe was created.}
function dosdisconnectnpipe(handle:longint):word;

const   np_state_disconnected   = 1;    {Pipe is disconnected.}
        np_state_listening      = 2;    {Pipe is listening.}
        np_state_connected      = 3;    {Pipe is connected.}
        np_state_closing        = 4;    {Pipe is closing.}

{Preview data in a pipe: Read data without removing it.
 handle         = Handle to named pipe.
 buffer         = Buffer to receive data in.
 bufsize        = Size of the buffer.
 readbytes      = Receives number of bytes put in buffer.
 avail          = Receives size of available data.
 state          = One of the np_xxxx constants for states.}
function dospeeknpipe(handle:longint;var buffer;bufsize:longint;
                      var readbytes:longint;var avail:Tavaildata;
                      var state:longint):word;

{Get information about a named pipe handle.
 handle         = Handle to pipe.
 state          = A combination of the np_xxxx constants for (!!!) pipemode.}
function dosquerynphstate(handle:longint;var state:longint):word;

{Return information about a named pipe.
 handle         = Handle to pipe.
 infolevel      = Level of information wanted (1 or 2 allowed).
 buffer         = Tpipeinfo datastructure for level 1.
                  Unique 4 byte identifier of the client for level 2. Only
                  used for LAN based pipe servers.}
function dosquerynpipeinfo(handle,infolevel:longint;var buffer;
                           bufsize:longint):word;

{Return information of local named pipes that are attached to a semaphore.
 semhandle      = Handle to a shared event or muxwait semaphore that is
                  attached to a named pipe.
 semarray       = Array in which for each pipe attached to the semaphore.
 bufsize        = Size of semarray, in bytes.}
function dosquerynpipesemstate(semhandle:longint;var semarray;
                               bufsize:longint):word;

{Resets the blocking mode and state of a named pipe.
 handle         = Handle to named pipe.
 state          = One of the np_xxxx constants for pipemode.}
function dossetnphstate(handle,state:longint):word;

{Attach a shared event semaphore to a local named pipe.
 pipehandle     = Handle to named pipe.
 semhandle      = Handle to semaphore.
 key            = A key that must be different for each named pipe that is
                  attached to the semaphore.}
function dossetnpipesem(pipehandle,semhandle,key:longint):word;

{Write to a duplex named pipe; then read from it.
 handle         = Handle to named pipe.
 outbuf         = The data to write.
 outsize        = Size of the data to write.
 inbuf          = Receives the read data.
 insize         = Size of the input buffer.
 readbytes      = Number of bytes read from the pipe.}
function dostransactnpipe(handle:longint;var outbuf;outsize:longint;
                          var inbuf;insize:longint;
                          var readbytes:longint):word;

{Waits until an instance of a named pipe becomes available.
 naam           = Name of named pipe (always starts with '\PIPE\').
 msec           = Return with an error code if this time has elapsed.}
function doswaitnpipe(naam:Pchar;msec:longint):word;
function doswaitnpipe(const naam:string;msec:longint):word;

{****************************************************************************

                    Virtual device driver related routines.

****************************************************************************}

{Open a virtual device driver.
 naam           = Name of virtual device driver.
 handle         = Receives handle to virtual device driver.}
function dosopenVDD(naam:Pchar;var handle:longint):word;

{Request to talk with a virtual device driver.
 handle         = Handle to virtual device driver.
 sgroup         = Handle to the screen group of a DOS session (may be nil).
 cmd            = A number which indicates the service you call.
 insize         = Size of the data to send to the VDD.
 inbuffer       = Buffer which contains the data to send to the VDD.
 outsize        = Size of the buffer in which the VDD will return data.
 outbuffer      = Receives the data that the VDD returns.}
function dosrequestVDD(handle,sgroup,cmd:longint;
                       insize:longint;var inbuffer;
                       outsize:longint;var outbuffer):word;

{Close a virtual device driver.}
function doscloseVDD(handle:longint):word;

{****************************************************************************

                    16 <=> 32 bit support related routines

****************************************************************************}

{Convert a 16 bit far pointer to a 32 bit near pointer.
 This procedure needs to be called from assembler.
 This procedure works by mapping an area in your flat address space onto the
 same physical memory address as the selector of the 16 bit far pointer.

In:
  eax           Pointer to convert in selector:offset format.

Out:
  eax           Returned 32 bit near pointer.}
procedure seltoflat;

{Convert a 32 bit near pointer to a 16 bit far pointer.
 This procedure needs to be called from assembler.
 This procedure works by allocating a selector at the same physical address
 as the pointer you pass points to.

In:
  eax           Pointer to convert in 32 bit near format.

Out:
  eax           Returned 16 bit far pointer in selector:offset format.}
procedure flattosel;

{***************************************************************************}
implementation
{***************************************************************************}

{$l code2.oo2}
{$l code3.oo2}

function doscreatethread(var tid:longint;address:Tthreadentry;
                          Aparam:pointer;flags:longint;
                          stacksize:longint):word;

external 'DOSCALLS' index 311;

function dossuspendthread(tid:longint):word;

external 'DOSCALLS' index 238;

function dosresumethread(tid:longint):word;

external 'DOSCALLS' index 237;

function doskillthread(tid:longint):word;

external 'DOSCALLS' index 111;

function doswaitthread(var tid:longint;option:longint):word;

external 'DOSCALLS' index 349;

function dosentercritsec:word;

external 'DOSCALLS' index 232;

function dosexitcritsec:word;

external 'DOSCALLS' index 233;

procedure dosexit(action:longint;result:longint);

external 'DOSCALLS' index 233;

procedure dosgetinfoblocks(var Atib:Pthreadinfoblock;
                           var Apib:Pprocessinfoblock);

external 'DOSCALLS' index 312;

procedure dossleep(msec:longint);

external 'DOSCALLS' index 229;

function dosbeep(freq,ms:longint):word;

external 'DOSCALLS' index 286;

function dosdebug(debugbuf:pointer):word;

external 'DOSCALLS' index 317;

function dosexitlist(ordercode:longint;proc:Texitproc):word;

external 'DOSCALLS' index 296;

function dosexecpgm(objnaam:Pchar;cbobjnaam,execflag:longint;
                    args,env:Pbytearray;var res:Tresultcodes;
                    filenaam:Pchar):word;

external 'DOSCALLS' index 283;

function dosexecpgm(var objnaam:string;execflag:longint;
                    args,env:Pbytearray;var res:Tresultcodes;
                    const filenaam:string):word;

var t,t2:array[0..255] of char;

begin
    strPcopy(@t,filenaam);
    dosexecpgm:=dosexecpgm(@t2,sizeof(t2),execflag,args,env,res,@t);;
    objnaam:=strpas(@t2);
end;

function doswaitchild(action:longint;option:longint;var res:Tresultcodes;
                      var termpid:longint;pid:longint):word;

external 'DOSCALLS' index 280;

function dossetpriority(scope,trclass,delta,portid:longint):word;

external 'DOSCALLS' index 236;

function doskillprocess(action:longint;pid:longint):word;

external 'DOSCALLS' index 235;

function dosqueryapptype(filenaam:Pchar;var flags:longint):word;

external 'DOSCALLS' index 323;

function dosdevconfig(var devinfo:byte;item:longint):word;

external 'DOSCALLS' index 231;

function dossetfilelocks(handle:longint;var unlock,lock:Tfilelock;
                         timeout:longint;flags:longint):word;

external 'DOSCALLS' index 428;

function doscancellockrequest(handle:longint;var lock:Tfilelock):word;

external 'DOSCALLS' index 429;

function dosopen(filenaam:Pchar;var handle,action:longint;
                 initsize:longint;attrib,openflags,filemode:longint;
                 ea:PEAop2):word;

external 'DOSCALLS' index 273;

function doscreate(filenaam:Pchar;var handle:longint;
                   attrib,openmode:longint):word;

var action:longint;

begin
    doscreate:=dosopen(filenaam,handle,action,0,attrib,18,openmode,nil);
end;

function dosopen(filenaam:Pchar;var handle:longint;
                 attrib,openmode:longint):word;

var action:longint;

begin
    dosopen:=dosopen(filenaam,handle,action,0,attrib,1,openmode,nil);
end;

function dosopen(const filenaam:string;var handle,action:longint;
                 initsize:longint;attrib,openflags,openmode:longint;
                 ea:PEAop2):word;

var t:array[0..255] of char;

begin
    strPcopy(@t,filenaam);
    dosopen:=dosopen(@t,handle,action,initsize,attrib,openflags,openmode,ea);
end;

function doscreate(const filenaam:string;var handle:longint;
                   attrib,openmode:longint):word;

var t:array[0..255] of char;
    action:longint;

begin
    strPcopy(@t,filenaam);
    doscreate:=dosopen(@t,handle,action,0,attrib,18,openmode,nil);
end;

function dosopen(const filenaam:string;var handle:longint;
                 attrib,openmode:longint):word;

var t:array[0..255] of char;
    action:longint;

begin
    strPcopy(@t,filenaam);
    dosopen:=dosopen(@t,handle,action,0,attrib,1,openmode,nil);
end;

function dosclose(handle:longint):word;

external 'DOSCALLS' index 257;

function dosread(handle:longint;var buffer;count:longint;
                 var actcount:longint):word;

external 'DOSCALLS' index 281;

function doswrite(handle:longint;var buffer;count:longint;
                  var actcount:longint):word;

external 'DOSCALLS' index 282;

function dossetfileptr(handle:word;pos:longint;method:longint;
                       var posactual:longint):word;

external 'DOSCALLS' index 256;

function dossetfileptr(handle:word;pos:longint):word;

var posactual:longint;

begin
    dossetfileptr:=dossetfileptr(handle,pos,0,posactual);
end;

function dosgetfileptr(handle:word;var posactual:longint):word;

begin
    dosgetfileptr:=dossetfileptr(handle,0,1,posactual);
end;

function dossetfilesize(handle,size:longint):word;

external 'DOSCALLS' index 272;

function dosresetbuffer(handle:longint):word;

external 'DOSCALLS' index 254;

function dosduphandle(handle:longint;var duplicate:longint):word;

external 'DOSCALLS' index 260;

function dosqueryFHstate(handle:longint;var filemode:longint):word;

external 'DOSCALLS' index 276;

function dossetFHstate(handle,filemode:longint):word;

external 'DOSCALLS' index 221;

function dosqueryHtype(handle:longint;var handtype:longint;
                       var attr:longint):word;

external 'DOSCALLS' index 224;

function doseditname(metalevel:longint;source,edit:Pchar;
                     target:Pchar;cbtarget:longint):word;

external 'DOSCALLS' index 261;

function doseditname(metalevel:longint;const source,edit:string;
                     var target:string):word;

var t,t2,t3:array[0..255] of char;

begin
    strPcopy(@t,source);
    strPcopy(@t2,edit);
    doseditname:=doseditname(metalevel,@t,@t2,@t3,sizeof(t3));
    target:=strpas(@t3);
end;

function dosmove(oud,nieuw:Pchar):word;

external 'DOSCALLS' index 271;

function dosmove(const oud,nieuw:string):word;

var t,t2:array[0..255] of char;

begin
    strPcopy(@t,oud);
    strPcopy(@t2,nieuw);
    dosmove:=dosmove(@t,@t2);
end;

function doscopy(oud,nieuw:Pchar;option:longint):word;

external 'DOSCALLS' index 258;

function doscopy(const oud,nieuw:string;option:longint):word;

var t,t2:array[0..255] of char;

begin
    strPcopy(@t,oud);
    strPcopy(@t2,nieuw);
    doscopy:=doscopy(@t,@t2,option);
end;

function dosdelete(filenaam:Pchar):word;

external 'DOSCALLS' index 259;

function dosdelete(const filenaam:string):word;

var t:array[0..255] of char;

begin
    strPcopy(@t,filenaam);
    dosdelete:=dosdelete(@t);
end;

function dosforcedelete(filenaam:Pchar):word;

external 'DOSCALLS' index 110;

function dosforcedelete(const filenaam:string):word;

var t:array[0..255] of char;

begin
    strPcopy(@t,filenaam);
    dosforcedelete:=dosforcedelete(@t);
end;

function doscreatedir(naam:Pchar;ea:PEAOP2):word;

external 'DOSCALLS' index 270;

function doscreatedir(naam:Pchar):word;

begin
    doscreatedir:=doscreatedir(naam,nil);
end;

function doscreatedir(const naam:string;ea:PEAOP2):word;

var t:array[0..255] of char;

begin
    strPcopy(@t,naam);
    doscreatedir:=doscreatedir(@t,ea);
end;

function doscreatedir(const naam:string):word;

begin
    doscreatedir:=doscreatedir(naam,nil);
end;

function dosdeletedir(naam:Pchar):word;

external 'DOSCALLS' index 226;

function dosdeletedir(const naam:string):word;

var t:array[0..255] of char;

begin
    strPcopy(@t,naam);
    dosdeletedir:=dosdeletedir(@t);
end;

function dossetdefaultdisk(disknum:longint):word;

external 'DOSCALLS' index 220;

procedure dosquerycurrentdisk(var disknum:longint;var logical:longint);

external 'DOSCALLS' index 275;

function dossetcurrentdir(naam:Pchar):word;

external 'DOSCALLS' index 255;

function dossetcurrentdir(const naam:string):word;

var t:array[0..255] of char;

begin
    strPcopy(@t,naam);
    dossetcurrentdir:=dossetcurrentdir(@t);
end;

function dosquerycurrentdir(disknum:longint;var buffer;
                            var buflen:longint):word;

external 'DOSCALLS' index 274;

function dosquerycurrentdir(disknum:longint;var buffer:string):word;

var t:array[0..255] of char;
    l:longint;

begin
    l:=255;
    dosquerycurrentdir:=dosquerycurrentdir(disknum,t,l);
    buffer:=strpas(@t);
end;

function dosdevIOctl(handle,category,func:longint;var params;
                     paramlen:longint;var paramsize:longint;
                     var data;var datalen:longint;var datasize:
                     longint):word;

external 'DOSCALLS' index 284;

function dosfindfirst(filemask:Pchar;var handle:longint;attrib:longint;
                      Afilestatus:Pfilestatus;cbfilestatus:longint;
                      var count:longint;infolevel:longint):word;

external 'DOSCALLS' index 264;

function dosfindfirst(const filemask:string;var handle:longint;
                      attrib:longint;Afilestatus:Pfilestatus;
                      cbfilestatus:longint;var count:longint;
                      infolevel:longint):word;

var t:array[0..255] of char;

begin
    strPcopy(@t,filemask);
    dosfindfirst:=dosfindfirst(@t,handle,attrib,Afilestatus,cbfilestatus,
     count,infolevel);
end;

function dosfindnext(handle:longint;Afilestatus:Pfilestatus;
                     cbfilestatus:longint;var count:longint):word;

external 'DOSCALLS' index 265;

function dosfindclose(handle:longint):word;

external 'DOSCALLS' index 263;

function dosqueryfileinfo(handle,infolevel:longint;Afilestatus:Pfilestatus;
                          cbfilestatus:longint):word;

external 'DOSCALLS' index 279;

function dossetfileinfo(handle,infolevel:longint;Afilestatus:Pfilestatus;
                        cbfilestatus:longint):word;

external 'DOSCALLS' index 218;

function dosquerypathinfo(filenaam:Pchar;infolevel:longint;
                          Afilestatus:Pfilestatus;cbfilestatus:longint):word;

external 'DOSCALLS' index 223;

function dosquerypathinfo(const filenaam:string;infolevel:longint;
                          Afilestatus:Pfilestatus;cbfilestatus:longint):word;

var t:array[0..255] of char;

begin
    strPcopy(@t,filenaam);
    dosquerypathinfo:=dosquerypathinfo(@t,infolevel,Afilestatus,
     cbfilestatus);
end;

function dossetpathinfo(filenaam:Pchar;infolevel:longint;
                        Afilestatus:Pfilestatus;cbfilestatus,
                        options:longint):word;

external 'DOSCALLS' index 219;

function dosenumattribute(reftype:longint;Afile:pointer;
                          entry:longint;var buf;bufsize:longint;
                          var count:longint;infolevel:longint):word;

external 'DOSCALLS' index 372;

function dosenumattribute(handle:longint;
                          entry:longint;var buf;bufsize:longint;
                          var count:longint;infolevel:longint):word;

begin
    dosenumattribute:=dosenumattribute(0,@handle,entry,buf,bufsize,count,
     infolevel);
end;

function dosenumattribute(const filenaam:string;
                          entry:longint;var buf;bufsize:longint;
                          var count:longint;infolevel:longint):word;

var t:array[0..255] of char;

begin
    strPcopy(@t,filenaam);
    dosenumattribute:=dosenumattribute(1,@t,entry,buf,bufsize,count,
     infolevel);
end;

function dosscanenv(naam:Pchar;var value:Pchar):word;

external 'DOSCALLS' index 227;

function dosscanenv(const naam:string;var value:string):word;

var t:array[0..255] of char;
    p:Pchar;

begin
    strPcopy(@t,naam);
    dosscanenv:=dosscanenv(@t,p);
    value:=strpas(p);
end;

function dossearchpath(flag:longint;dirlist,filenaam:Pchar;
                       fullname:Pchar;fulllen:longint):word;

external 'DOSCALLS' index 228;

function dossearchpath(flag:longint;const dirlist,filenaam:string;
                       var fullname:string):word;

var t1,t2,t3:array[0..255] of char;

begin
    strPcopy(@t1,dirlist);
    strPcopy(@t2,filenaam);
    dossearchpath:=dossearchpath(flag,@t1,@t2,@t3,sizeof(t3));
    fullname:=strpas(@t3);
end;

function dosFSattach(devnaam,filesystem:Pchar;var data:Tattachdata;
                     datalen,flag:longint):word;

external 'DOSCALLS' index 269;

function dosFSattach(const devnaam,filesystem:string;var data:Tattachdata;
                     datalen,flag:longint):word;

var t1,t2:array[0..255] of char;

begin
    strPcopy(@t1,devnaam);
    strPcopy(@t2,filesystem);
    dosFSattach:=dosFSattach(@t1,@t2,data,datalen,flag);
end;

function dosqueryFSattach(devnaam:Pchar;ordinal,infolevel:longint;
                          var buffer:TFSqbuffer2;var buflen:longint):word;

external 'DOSCALLS' index 277;

function dosqueryFSattach(const devnaam:string;ordinal,infolevel:longint;
                          var buffer:TFSqbuffer2;var buflen:longint):word;

var t:array[0..255] of char;

begin
    strPcopy(@t,devnaam);
    dosqueryFSattach:=dosqueryFSattach(@t,ordinal,infolevel,buffer,buflen);
end;

function dosFSctl(data:pointer;datalen:longint;var resdatalen:longint;
                  parms:pointer;parmslen:longint;var resparmslen:longint;
                  _function:longint;route:Pchar;
                  handle,method:longint):word;

external 'DOSCALLS' index 285;

function dosFSctl(data:pointer;datalen:longint;var resdatalen:longint;
                  parms:pointer;parmslen:longint;var resparmslen:longint;
                  _function:longint;const route:string;
                  handle,method:longint):word;

var t:array[0..255] of char;

begin
    strPcopy(@t,route);
    dosFSctl:=dosFSctl(data,datalen,resdatalen,parms,parmslen,resparmslen,
     _function,route,handle,method);
end;

function dosqueryFSinfo(disknum,infolevel:longint;var buffer:TFSinfo;
                        buflen:longint):word;

external 'DOSCALLS' index 278;

function dossetFSinfo(disknum,infolevel:longint;var buffer:TFSinfo;
                      buflen:longint):word;

external 'DOSCALLS' index 222;

function dosqueryverify(var enabled:longint):word;

external 'DOSCALLS' index 225;

function dossetverify(enable:longint):word;

external 'DOSCALLS' index 210;

function dossetmaxFH(count:longint):word;

external 'DOSCALLS' index 209;

function dossetrelmaxFH(var reqcount,curmaxFH:longint):word;

external 'DOSCALLS' index 382;

function dosshutdown(flags:longint):word;

external 'DOSCALLS' index 415;

function dosquerysysinfo(first,last:longint;var buf;bufsize:longint):word;

external 'DOSCALLS' index 348;

function dosphysicaldisk(func:longint;buf:pointer;bufsize:longint;
                         params:pointer;paramsize:longint):word;

external 'DOSCALLS' index 287;

function dosallocmem(var p:pointer;size:longint;flag:longint):word;

external 'DOSCALLS' index 299;

function dosfreemem(p:pointer):word;

external 'DOSCALLS' index 304;

function dossetmem(p:pointer;cb,flag:longint):word;

external 'DOSCALLS' index 305;

function dosgivesharedmem(p:pointer;pid,flag:longint):word;

external 'DOSCALLS' index 303;

function dosgetsharedmem(p:pointer;flag:longint):word;

external 'DOSCALLS' index 302;

function dosgetnamedsharedmem(var p:pointer;naam:Pchar;flag:longint):word;

external 'DOSCALLS' index 301;

function dosgetnamedsharedmem(var p:pointer;const naam:string;
                              flag:longint):word;

var t:array[0..255] of char;

begin
    strPcopy(@t,naam);
    dosgetnamedsharedmem:=dosgetnamedsharedmem(p,@t,flag);
end;

function dosallocsharedmem(var p:pointer;naam:Pchar;size,flag:longint):word;

external 'DOSCALLS' index 300;

function dosallocsharedmem(var p:pointer;const naam:string;size,
                           flag:longint):word;

var t:array[0..255] of char;

begin
    if naam<>'' then
        begin
            strPcopy(@t,naam);
            dosallocsharedmem:=dosallocsharedmem(p,naam,size,flag);
        end
    else
        dosallocsharedmem:=dosallocsharedmem(p,nil,size,flag);
end;

function dosquerymem(p:pointer;var size,flag:longint):word;

external 'DOSCALLS' index 306;

function dossuballocmem(base:pointer;var p:pointer;size:longint):word;

external 'DOSCALLS' index 345;

function dossubfreemem(base,p:pointer;size:longint):word;

external 'DOSCALLS' index 346;

function dossubsetmem(base:pointer;flag:longint;size:longint):word;

external 'DOSCALLS' index 344;

function dossubunsetmem(base:pointer):word;

external 'DOSCALLS' index 347;

function doscreateeventsem(naam:Pchar;var handle:longint;
                           attr,state:longint):word;

external 'DOSCALLS' index 324;

function doscreateeventsem(const naam:string;var handle:longint;
                           attr,state:longint):word;

var t:array[0..255] of char;

begin
    if naam<>'' then
        begin
            strPcopy(@t,naam);
            doscreateeventsem:=doscreateeventsem(@t,handle,attr,state);
        end
    else
        doscreateeventsem:=doscreateeventsem(@t,handle,attr,state);
end;

function dosopeneventsem(naam:Pchar;var handle:longint):word;

external 'DOSCALLS' index 325;

function dosopeneventsem(const naam:string;var handle:longint):word;

var t:array[0..255] of char;

begin
    strPcopy(@t,naam);
    dosopeneventsem:=dosopeneventsem(@t,handle);
end;

function doscloseeventsem(handle:longint):word;

external 'DOSCALLS' index 326;

function dosreseteventsem(handle:longint;var postcount:longint):word;

external 'DOSCALLS' index 327;

function dosposteventsem(handle:longint):word;

external 'DOSCALLS' index 328;

function doswaiteventsem(handle,timeout:longint):word;

external 'DOSCALLS' index 329;

function dosqueryeventsem(handle:longint;var posted:longint):word;

external 'DOSCALLS' index 330;

function doscreatemutexsem(naam:Pchar;var handle:longint;
                           attr,state:longint):word;

external 'DOSCALLS' index 331;

function doscreatemutexsem(const naam:string;var handle:longint;
                           attr,state:longint):word;

var t:array[0..255] of char;

begin
    if naam<>'' then
        begin
            strPcopy(@t,naam);
            doscreatemutexsem:=doscreatemutexsem(@t,handle,attr,state);
        end
    else
        doscreatemutexsem:=doscreatemutexsem(nil,handle,attr,state);
end;

function dosopenmutexsem(naam:Pchar;var handle:longint):word;

external 'DOSCALLS' index 332;

function dosopenmutexsem(const naam:string;var handle:longint):word;

var t:array[0..255] of char;

begin
    strPcopy(@t,naam);
    dosopenmutexsem:=dosopenmutexsem(@t,handle);
end;

function dosclosemutexsem(handle:longint):word;

external 'DOSCALLS' index 333;

function dosrequestmutexsem(handle,timeout:longint):word;

external 'DOSCALLS' index 334;

function dosreleasemutexsem(handle:longint):word;

external 'DOSCALLS' index 335;

function dosquerymutexsem(handle:longint;var pid,tid,count:longint):word;

external 'DOSCALLS' index 336;

function doscreatemuxwaitsem(naam:Pchar;var handle:longint;csemrec:longint;
                             var semarray:Tsemarray;attr:longint):word;

external 'DOSCALLS' index 337;

function doscreatemuxwaitsem(const naam:string;var handle:longint;
                             csemrec:longint;var semarray:Tsemarray;
                             attr:longint):word;

var t:array[0..255] of char;

begin
    if naam<>'' then
        begin
            strPcopy(@t,naam);
            doscreatemuxwaitsem:=doscreatemuxwaitsem(@t,handle,csemrec,
             semarray,attr);
        end
    else
        doscreatemuxwaitsem:=doscreatemuxwaitsem(nil,handle,csemrec,semarray,
         attr);
end;

function dosopenmuxwaitsem(naam:Pchar;var handle:longint):word;

external 'DOSCALLS' index 338;

function dosopenmuxwaitsem(const naam:string;var handle:longint):word;

var t:array[0..255] of char;

begin
    strPcopy(@t,naam);
    dosopenmuxwaitsem:=dosopenmuxwaitsem(@t,handle);
end;

function dosclosemuxwaitsem(handle:longint):word;

external 'DOSCALLS' index 339;

function doswaitmuxwaitsem(handle,timeout:longint;var user:longint):word;

external 'DOSCALLS' index 340;

function dosaddmuxwaitsem(handle:longint;var semrec:Tsemrecord):word;

external 'DOSCALLS' index 341;

function dosdeletemuxwaitsem(handle,sem:longint):word;

external 'DOSCALLS' index 342;

function dosquerymuxwaitsem(handle:longint;var csemrec:longint;
                            var semrecs:Tsemarray;var attr:longint):word;

external 'DOSCALLS' index 343;

function dosgetdatetime(var buf:Tdatetime):word;

external 'DOSCALLS' index 230;

function dossetdatetime(var buf:Tdatetime):word;

external 'DOSCALLS' index 292;

function dosasynctimer(msec:longint;hsem:longint;
                       var TIMhandle:longint):word;

external 'DOSCALLS' index 350;

function dosstarttimer(msec:longint;hsem:longint;
                       var TIMhandle:longint):word;

external 'DOSCALLS' index 351;

function dosstoptimer(TIMhandle:longint):word;

external 'DOSCALLS' index 290;

function dostmrqueryfreq(var freq:longint):word;

external 'DOSCALLS' index 362;

function dostmrquerytime(var time:comp):word;

external 'DOSCALLS' index 363;

function dosloadmodule(objnaam:Pchar;objlen:longint;DLLnaam:Pchar;
                       var handle:longint):word;

external 'DOSCALLS' index 318;

function dosloadmodule(var objnaam:string;objlen:longint;
                       const DLLnaam:string;var handle:longint):word;

var t1,t2:array[0..255] of char;

begin
    strPcopy(@t2,DLLnaam);
    dosloadmodule:=dosloadmodule(@t1,objlen,@t2,handle);
    objnaam:=strpas(@t1);
end;

function dosfreemodule(handle:longint):word;

external 'DOSCALLS' index 322;

function dosqueryprocaddr(handle,ordinal:longint;procnaam:Pchar;
                          var adres:pointer):word;

external 'DOSCALLS' index 321;

function dosqueryprocaddr(handle,ordinal:longint;
                          const procnaam:string;var adres:pointer):word;

var t1:array[0..255] of char;

begin
    if procnaam<>'' then
        begin
            strPcopy(@t1,procnaam);
            dosqueryprocaddr:=dosqueryprocaddr(handle,ordinal,@t1,adres);
        end
    else
        dosqueryprocaddr:=dosqueryprocaddr(handle,ordinal,nil,adres);
end;

function dosquerymodulehandle(DLLnaam:Pchar;var handle:longint):word;

external 'DOSCALLS' index 319;

function dosquerymodulehandle(const DLLnaam:string;var handle:longint):word;

var t1:array[0..255] of char;

begin
    strPcopy(@t1,DLLnaam);
    dosquerymodulehandle:=dosquerymodulehandle(@t1,handle);
end;

function dosquerymodulename(handle,naamlen:longint;naam:Pchar):word;

external 'DOSCALLS' index 320;

{function dosquerymodulename(handle:longint;var naam:openstring):word;

var t1:array[0..255] of char;

begin
    dosquerymodulename:=dosquerymodulename(handle,high(naam),@t1);
    naam:=strpas(@t1);
end;}

function dosqueryproctype(handle,ordinal:longint;naam:Pchar;
                          var proctype:longint):word;

external 'DOSCALLS' index 586;

function dosqueryproctype(handle,ordinal:longint;const naam:string;
                          var proctype:longint):word;

var t1:array[0..255] of char;

begin
    if naam<>'' then
        begin
            strPcopy(@t1,naam);
            dosqueryproctype:=dosqueryproctype(handle,ordinal,@t1,proctype);
        end
    else
        dosqueryproctype:=dosqueryproctype(handle,ordinal,nil,proctype);
end;

function dosgetresource(handle,restype,resnaam:longint;var p:pointer):word;

external 'DOSCALLS' index 352;

function dosfreeresource(p:pointer):word;

external 'DOSCALLS' index 353;

function dosqueryresourcesize(handle,idt,idn:longint;var size:longint):word;

external 'DOSCALLS' index 572;

function dosqueryctryinfo(cb:longint;var country:Tcountrycode;
                          var res:Tcountryinfo;var cbactual:longint):word;

external 'NLS' index 5;

function dosqueryDBCSenv(cb:longint;var country:Tcountrycode;buf:Pchar):word;

external 'NLS' index 6;

function dosmapcase(cb:longint;var country:Tcountrycode;
                    Astring:Pchar):word;

external 'NLS' index 7;

function dosmapcase(var country:Tcountrycode;var Astring:string):word;

var t1:string;

begin
    strPcopy(@t1,Astring);
    dosmapcase:=dosmapcase(length(Astring),country,@t1);
    Astring:=strpas(@t1);
end;

function dosquerycollate(cb:longint;var country:Tcountrycode;
                         buf:Pbytearray;var tablelen:longint):word;

external 'NLS' index 8;

function dosquerycp(cb:longint;codepages:Pwordarray;var actcb:longint):word;

external 'DOSCALLS' index 291;

function dossetprocesscp(cp:longint):word;

external 'DOSCALLS' index 289;

function dossetexceptionhandler(var regrec:Texceptionregistrationrecord
                                ):word;

external 'DOSCALLS' index 354;

function dosunsetexceptionhandler(var regrec:Texceptionregistrationrecord
                                  ):word;

external 'DOSCALLS' index 355;

function dosraiseexception(var excpt:Texceptionreportrecord):word;

external 'DOSCALLS' index 356;

function dossendsignalexception(pid,exception:longint):word;

external 'DOSCALLS' index 379;

function dosunwindexception(var handler:Texceptionregistrationrecord;
                            targetIP:pointer;
                            var reprec:Texceptionreportrecord):word;

external 'DOSCALLS' index 357;

function dossetsignalexceptionfocus(enable:longint;var times:longint):word;

external 'DOSCALLS' index 378;

function dosentermustcomplete(var nesting:longint):word;

external 'DOSCALLS' index 380;

function dosexitmustcomplete(var nesting:longint):word;

external 'DOSCALLS' index 381;

function dosacknowledgesignalexception(signalnum:longint):word;

external 'DOSCALLS' index 418;

function dosclosequeue(handle:longint):word;

external 'QUECALLS' index 11;

function doscreatequeue(var handle:longint;priority:longint;
                        naam:Pchar):word;

external 'QUECALLS' index 16;

function doscreatequeue(var handle:longint;priority:longint;
                        const naam:string):word;

var t1:array[0..255] of char;

begin
    strPcopy(@t1,naam);
    doscreatequeue:=doscreatequeue(handle,priority,@t1);
end;

function dosopenqueue(var parent_pid:longint;var handle:longint;
                      naam:Pchar):word;

external 'QUECALLS' index 15;

function dosopenqueue(var parent_pid:longint;var handle:longint;
                      const naam:string):word;

var t1:array[0..255] of char;

begin
    strPcopy(@t1,naam);
    dosopenqueue:=dosopenqueue(parent_pid,handle,@t1);
end;

function dospeekqueue(handle:longint;var reqbuffer:Trequestdata;
                      var datalen:longint;var dataptr:pointer;
                      var element:longint;wait:longint;
                      var priority:byte;Asem:longint):word;

external 'QUECALLS' index 13;

function dospurgequeue(handle:longint):word;

external 'QUECALLS' index 10;

function dosqueryqueue(handle:longint;var count:longint):word;

external 'QUECALLS' index 12;

function dosreadqueue(handle:longint;var reqbuffer:Trequestdata;
                      var datalen:longint;var dataptr:pointer;
                      element,wait:longint;var priority:byte;
                      Asem:longint):word;

external 'QUECALLS' index 9;

function doswritequeue(handle,request,datalen:longint;var databuf;
                       priority:longint):word;

external 'QUECALLS' index 14;

function doserror(error:longint):word;

external 'DOSCALLS' index 212;

procedure doserrclass(code:longint;var _class,action,locus:longint);

external 'DOSCALLS' index 211;

function dostruegetmessage(msgseg:pointer;table:Pinserttable;
                           tablesize:longint;buf:Pchar;bufsize,
                           msgnumber:longint;filenaam:Pchar;
                           var msgsize:longint):word;

external 'MSG' index 6;

function dosgetmessage(table:Pinserttable;tablesize:longint;buf:Pchar;
                       bufsize,msgnumber:longint;filenaam:Pchar;
                       var msgsize:longint):word;

external name 'DosGetMessage';  {Procedure is in code2.so2.}

(*function dosgetmessage(const table:array of Pstring;var buf:openstring;
                        msgnumber:longint;const filenaam:string):word;

{Hmm. This takes too much stackspace. Let's use the
 heap instead.}

type    tablebuffer=record
            it:Tinserttable;
            strings:Tbytearray;
        end;

var buffer:^tablebuffer;
    i,s:word;
    bufptr:pointer;
    t1,t2:array[0..255] of char;

begin
    {Check if there are more than nine items in the table.}
    if high(table)>8 then
        dosgetmessage:=87
    else
        begin
            {Step 1: Calculate the space we need on the heap.}
            s:=sizeof(Tinserttable);
            for i:=low(table) to high(table) do
                s:=s+length(table[i])+1;

            {Step 2: Allocate the buffer.}
            getmem(buffer,s);

            {Step 3: Fill the buffer.}
            bufptr:=@(s^.strings);
            for i:=low(table) to high(table) do
                begin
                    s^.it[i+1]:=bufptr;
                    strPcopy(bufptr,table[i]);
                    inc(longint(bufptr),length(table[i])+1);
                end;

            {Step 4: Convert the filename.}
            strPcopy(@t2,filenaam);

            {Step 5: Get the message.}
            dosgetmessage:=dosgetmessage(@(s^.it),high(table)+1,@t1,
             high(buf),msgnumber,@t2,l);

            {Step 6: Convert the returned message.}
            buf[0]:=char(l);
            move(t1,buf[1],l);

            {Step 7: Free the memory.}
            freemem(buffer,s);
        end;
end;*)

{function dosgetmessage(const table:array of Pstring;buf:Pchar;
                       bufsize,msgnumber:longint;const filenaam:string;
                       msgsize:longint):word;}

function dosinsertmessage(table:Pinserttable;tablesize:longint;
                          message:Pchar;srcmessagesize:longint;
                          buf:Pchar;bufsize:longint;
                          var dstmessagesize:longint):word;

external 'MSG' index 4;

{function dosinsertmessage(table:array of Pstring;
                          const message:string;
                          var buf:openstring):word;

function dosinsertmessage(table:array of Pstring;
                          message:Pchar;srcmessagesize:longint;
                          buf:Pchar;bufsize:longint;
                          var dstmessagesize:longint):word;}

function dosputmessage(handle,size:longint;buf:Pchar):word;

external 'MSG' index 5;

function dosputmessage(handle:longint;const buf:string):word;

begin
    dosputmessage:=dosputmessage(handle,length(buf),@buf[1]);
end;

function dosIquerymessageCP(var buf;bufsize:longint;filenaam:Pchar;
                            var infosize:longint;messeg:pointer):word;

external 'MSG' index 8;

function dosquerymessageCP(var buf;bufsize:longint;filenaam:Pchar;
                           var infosize:longint):word;

external name 'DosQueryMessageCP';

function dosquerymessageCP(var buf;bufsize:longint;const filenaam:string;
                           var infosize:longint):word;

var t:array[0..255] of char;

begin
    strPcopy(@t,filenaam);
    dosquerymessageCP:=dosquerymessageCP(buf,bufsize,@t,infosize);
end;

function dosstartsession(const Astartdata:Tstartdata;
                         var sesid,pid:longint):word;

external 'SESMGR' index 37;

function dossetsession(sesid:longint;const Astatus:Tstatusdata):word;

external 'SESMGR' index 39;

function dosselectsession(sesid:longint):word;

external 'SESMGR' index 38;

function dosstopsession(scope,sesid:longint):word;

external 'SESMGR' index 40;

function doscreatepipe(var readhandle,writehandle:longint;
                       size:longint):word;

external 'DOSCALLS' index 239;

function doscreatenpipe(naam:Pchar;var handle:longint;openmode,pipemode,
                        outbufsize,inbufsize,msec:longint):word;

external 'DOSCALLS' index 243;

function doscreatenpipe(const naam:string;var handle:longint;openmode,
                        pipemode,outbufsize,inbufsize,msec:longint):word;

var t:array[0..255] of char;

begin
    strPcopy(@t,naam);
    doscreatenpipe:=doscreatenpipe(@t,handle,openmode,pipemode,outbufsize,
     inbufsize,msec);
end;

function doscallnpipe(naam:Pchar;var input;inputsize:longint;
                      var output;outputsize:longint;var readbytes:longint;
                      msec:longint):word;

external 'DOSCALLS' index 240;

function doscallnpipe(const naam:string;var input;inputsize:longint;
                      var output;outputsize:longint;var readbytes:longint;
                      msec:longint):word;

var t:array[0..255] of char;

begin
    strPcopy(@t,naam);
    doscallnpipe:=doscallnpipe(@t,input,inputsize,output,outputsize,
     readbytes,msec);
end;

function dosconnectnpipe(handle:longint):word;

external 'DOSCALLS' index 241;

function dosdisconnectnpipe(handle:longint):word;

external 'DOSCALLS' index 242;

function dospeeknpipe(handle:longint;var buffer;bufsize:longint;
                      var readbytes:longint;var avail:Tavaildata;
                      var state:longint):word;

external 'DOSCALLS' index 244;

function dosquerynphstate(handle:longint;var state:longint):word;

external 'DOSCALLS' index 245;

function dosquerynpipeinfo(handle,infolevel:longint;var buffer;
                           bufsize:longint):word;

external 'DOSCALLS' index 248;

function dosquerynpipesemstate(semhandle:longint;var semarray;
                               bufsize:longint):word;

external 'DOSCALLS' index 249;

function dossetnphstate(handle,state:longint):word;

external 'DOSCALLS' index 250;

function dossetnpipesem(pipehandle,semhandle,key:longint):word;

external 'DOSCALLS' index 251;

function dostransactnpipe(handle:longint;var outbuf;outsize:longint;
                          var inbuf;insize:longint;
                          var readbytes:longint):word;

external 'DOSCALLS' index 252;

function doswaitnpipe(naam:Pchar;msec:longint):word;

external 'DOSCALLS' index 253;

function doswaitnpipe(const naam:string;msec:longint):word;

var t:array[0..255] of char;

begin
    strPcopy(@t,naam);
    doswaitnpipe:=doswaitnpipe(@t,msec);
end;

function dosopenVDD(naam:Pchar;var handle:longint):word;

external 'DOSCALLS' index 308;

function dosrequestVDD(handle,sgroup,cmd:longint;
                       insize:longint;var inbuffer;
                       outsize:longint;var outbuffer):word;

external 'DOSCALLS' index 309;

function doscloseVDD(handle:longint):word;

external 'DOSCALLS' index 310;

procedure seltoflat;

external 'DOSCALLS' index 425;

procedure flattosel;

external 'DOSCALLS' index 426;

end.
