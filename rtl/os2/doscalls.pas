{Set tabsize to 4.}
{****************************************************************************

    $Id$

                           DOSCALLS interface unit
                     FPC Pascal Runtime Library for OS/2
                   Copyright (c) 1999-2000 by Florian Klaempfl
                    Copyright (c) 1999-2000 by Daniel Mantione

 The Free Pascal runtime library is distributed under the Library GNU Public
 License v2. So is this unit. The Library GNU Public License requires you to
 distribute the source code of this unit with any product that uses it.
 Because the EMX library isn't under the LGPL, we grant you an exception to
 this, and that is, when you compile a program with the Free Pascal Compiler,
 you do not need to ship source code with that program, AS LONG AS YOU ARE
 USING UNMODIFIED CODE! If you modify this code, you MUST change the next
 line:

 <This an official, unmodified Free Pascal source code file.>

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

unit DosCalls;

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
 instead of just a PChar, thanks to that cool feature of procedure
 overloading.

 Daniel Mantione,
 June 1997

 Please, note, that all calls of the original functions from DOSCALLS.DLL must
 be declared using cdecl (C calling convention)! This doesn't apply to wrapped
 around functions, of course (like equivalents of some functions with string
 instead of PChar).

Changelog:

    People:
        KOMH    - KO Myung-Hun ( komh@chollian.net )

    Date:           Description of change:              Changed by:

                    Type of parameter is corrected,     KOMH
                    'word' to 'longint'.
    People:

        DM - Daniel Mantione
        TH - Tomas Hajny (XHajT03@mbox.vol.cz on Internet)

    Date:           Description of change:              Changed by:

     -              First released version 0.1.         DM
    98/11/21        Mostly cosmetic changes - higher    TH
                    level of compatibility with other
                    OS/2 compilers, some Dutch names of
                    parameters changed to English ones,
                    better readability of identifiers,
                    some mistypings corrected etc.

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
{$DEFINE FVISION_PSTRING}       {Get the PString type from Free Vision.}
{$ENDIF}
{$ENDIF}

{***************************************************************************}
interface
{***************************************************************************}

{$IFDEF OWN_PSTRING}
uses    Strings;

type    PString=^string;
{$ELSE}
 {$IFDEF FVISION_PSTRING}
uses    Strings,Objects;
 {$ELSE}
    {$ERROR PString source unknown.}
 {$ENDIF}
{$ENDIF}

{$IFDEF FPC}
    {$PACKRECORDS 1}
{$ENDIF FPC}

type    TByteArray=array[0..$fff0] of byte;
        PByteArray=^TByteArray;
        TCharArray=array[0..$fff0] of char;
        PCharArray=^TCharArray;
        TWordArray=array[0..$7ff8] of word;
        PWordArray=^TWordArray;

{****************************************************************************

                            Thread related routines.

****************************************************************************}

type    TThreadEntry = function (Param: pointer): longint; cdecl;
        ThreadEntry = TThreadEntry;


const   dtSuspended         =1; {Thread is started suspended instead of
                                 started at once.}
        dtStack_Commited    =2; {Allocate all stack space at once. The
                                 operating system normally allocates more
                                 memory to the stack if the stack grows with
                                 the given stacksize as limit. This has the
                                 restriction that you cannot create a stack
                                 frame > 4 kb. at once. If you want to do
                                 this, or for other reasons you can allocate
                                 the complete stack at once with this flag.}
(* The following for compatibility only *)
        CREATE_READY          =0;                   { defect 65437  }
        CREATE_SUSPENDED      =dtSuspended;
        STACK_SPARSE          =0;
        STACK_COMMITTED       =dtStack_Commited;

   { Wait option values }
        dtWait              =0; {Wait until termination.}
        dtNoWait            =1; {Do not wait. Return with error if not yet
                                 terminated.}
(* The following for compatibility only *)
        DCWW_WAIT           =dtWait;
        DCWW_NOWAIT         =dtNoWait;


{Create a new thread.
 TID        = Thread ID of new thread is returned here.
 Address    = Thread entry point. The new thread starts executing here.
 AParam     = This one is passed to the thread entry procedure.
 Flags      = Flags. Either dtsuspended or dt_stackcommited.
 StackSize  = Size of the stack of the new thread.}
function DosCreateThread (var TID: longint; Address: TThreadEntry;
                   AParam: pointer; Flags, StackSize: longint): longint; cdecl;

(* Overloaded version for compatibility. *)
function DosCreateThread (var TID: longint; Address: pointer;
                   AParam: Pointer; Flags, StackSize: longint): longint; cdecl;
                       

{Suspend a running thread.}
function DosSuspendThread(TID:longint):longint; cdecl;

{Resume a suspended thread.}
function DosResumeThread(TID:longint):longint; cdecl;

{Terminate a specific thread.}
function DosKillThread(TID:longint):longint; cdecl;

{Wait until a specific thread has ended.
 TID            = Thread to terminate. Can also be zero. In that case we
                  wait until the next thread terminates. Its thread ID is
                  returned back.
 Option         = Flags. Either dtWait or dtNoWait.}
function DosWaitThread(var TID:longint;Option:longint):longint; cdecl;

{All other threads in the same process are suspended until a
DosExitCritSec.}
function DosEnterCritSec:longint; cdecl;

{Resume the other threads again.}
function DosExitCritSec:longint; cdecl;

{ DosExit codes }
const   deThread=0;         {Terminate thread only.}
        deProcess=1;        {Terminate the whole process.}
(* The following for compatibility only *)
        Exit_Thread = deThread;
        Exit_Process = deProcess;

{Terminate the thread or the program. Never returns, so it's defined as
 procedure.}
procedure DosExit(Action,Result:longint); cdecl;

type    PThreadInfoBlock=^TThreadInfoBlock;
        PPThreadInfoBlock=^PThreadInfoBlock;
        PSysThreadIB=^TSysThreadIB;
        PProcessInfoBlock=^TProcessInfoBlock;
        PPProcessInfoBlock=^PProcessInfoBlock;

        TThreadInfoBlock=record
            Exh_Chain,              {Head of exeption handler chain.}
            Stack,                  {Pointer to the thread's stack.}
            StackLimit:pointer;     {Pointer to the thread's stack-end.}
            TIB2:PSysThreadIB;      {Pointer to system specific thread info.}
            Version,                {Version of this datastructure.}
            Ordinal:longint;        {Thread ordinal number.}
        end;
        ThreadInfoBlock=TThreadInfoBlock;

        TSysThreadIB=record
            TID,                    {Thread ID.}
            Priority,               {Low byte of low word: thread priority.
                                     High byte of low word: thread class
                                        1 = Idle
                                        2 = Regular
                                        3 = Time critical
                                        4 = Server}
            Version:longint;        {Version of this datastructure.}
            MCCount,                {Must complete count. ??? Info wanted!}
            MCForceFlag:word;       {Must complete force flag. Info wanted!}
        end;
        SysThreadIB=TSysThreadIB;

        TProcessInfoBlock=record
            PID,                    {Process ID.}
            ParentPID,              {Parent's process ID.}
            HMTE:longint;           {Module handle of executable program.
                                     ??? Info wanted!}
            Cmd,                    {Command line options.}
            Env:PByteArray;         {Environment strings.}
            flStatus,               {1 means that the process is in exit list
                                     processing.}
            tType:longint;          {Type of process:
                                        0:  Full screen protected mode.
                                        1:  DOS emulation.
                                        2:  Windowable full screen protected
                                            mode program.
                                        3:  Presentation manager program.
                                        4:  Detached mode process.}
        end;
        ProcessInfoBlock=TProcessInfoBlock;

{OS/2 keeps information about the current process and the current thread
 is the datastructures Tprocessinfoblock and Tthreadinfoblock. All data
 can both be read and be changed. Use DosGetInfoBlocks to get their
 address. The service cannot fail, so it is defined as procedure.
 The second version of the call might be useful if you only want address
 of one of those datastructures, since you can supply nil for the other
 parameter then.}

procedure DosGetInfoBlocks(var ATIB:PThreadInfoBlock;
                           var APIB:PProcessInfoBlock); cdecl;
procedure DosGetInfoBlocks(PATIB:PPThreadInfoBlock;
                           PAPIB:PPProcessInfoBlock); cdecl;

{Wait a number of microseconds. Cannot fail, so it is defined as procedure.}
procedure DosSleep(MSec:longint); cdecl;

{Beep the speaker. You do not need to check for an error if you can
 guarantee that the frequency is correct.}
function DosBeep(Freq,MS:longint):longint; cdecl;

{****************************************************************************

                        Process handling routines.

****************************************************************************}

{ User's Debug Buffer structure }

type    PDbgBuf = ^TDbgBuf;
        TDbgBuf = record
            Pid: longint;             { Debuggee Process id          }
            Tid: longint;             { Debuggee Thread id           }
            Cmd: longint;             { Command or Notification      }
            Value: longint;           { Generic Data Value           }
            Addr: pointer;            { Debuggee Address             }
            Buffer: pointer;          { Debugger Buffer Address      }
            Len: cardinal;            { Length of Range              }
            index: cardinal;          { Generic Identifier Index     }
            MTE: cardinal;            { Module Table Entry Handle    }
            EAX: cardinal;            { Register Set                 }
            ECX: cardinal;
            EDX: cardinal;
            EBX: cardinal;
            ESP: cardinal;
            EBP: cardinal;
            ESI: cardinal;
            EDI: cardinal;
            EFlags: cardinal;
            EIP: cardinal;
            CSLim: cardinal;
            CSBase: cardinal;
            CSAcc: byte;
            CSAtr: byte;
            CS: word;
            DSLim: cardinal;
            DSBase: cardinal;
            DSAcc: byte;
            DSAtr: byte;
            DS: word;
            ESLim: cardinal;
            ESBase: cardinal;
            ESAcc: byte;
            ESAtr: byte;
            ES: word;
            FSLim: cardinal;
            FSBase: cardinal;
            FSAcc: byte;
            FSAtr: byte;
            FS: word;
            GSLim: cardinal;
            GSBase: cardinal;
            GSAcc: byte;
            GSAtr: byte;
            GS: word;
            SSLim: cardinal;
            SSBase: cardinal;
            SSAcc: byte;
            SSAtr: byte;
            SS: word;
        end;


{ DosDebug Command Numbers
 *
 *      These numbers are placed in the Cmd field of the uDB on
 *      entry to DosDebug.
 *
 *      These numbers identify which command DosDebug is requested
 *      to perform.
 *
 }

const   DBG_C_Null              = 0;       { Null                         }
        DBG_C_ReadMem           = 1;       { Read Word                    }
        DBG_C_ReadMem_I         = 1;       { Read Word                    }
        DBG_C_ReadMem_D         = 2;       { Read Word (same as 1)        }
        DBG_C_ReadReg           = 3;       { Read Register Set            }
        DBG_C_WriteMem          = 4;       { Write Word                   }
        DBG_C_WriteMem_I        = 4;       { Write Word                   }
        DBG_C_WriteMem_D        = 5;       { Write Word (same as 4)       }
        DBG_C_WriteReg          = 6;       { Write Register Set           }
        DBG_C_Go                = 7;       { Go                           }
        DBG_C_Term              = 8;       { Terminate                    }
        DBG_C_SStep             = 9;       { Single Step                  }
        DBG_C_Stop              = 10;      { Stop                         }
        DBG_C_Freeze            = 11;      { Freeze Thread                }
        DBG_C_Resume            = 12;      { Resume Thread                }
        DBG_C_NumToAddr         = 13;      { Object Number to Address     }
        DBG_C_ReadCoRegs        = 14;      { Read Coprocessor Registers   }
        DBG_C_WriteCoRegs       = 15;      { Write Coprocessor Registers  }
                                           { 16 is reserved               }
        DBG_C_ThrdStat          = 17;      { Get Thread Status            }
        DBG_C_MapROAlias        = 18;      { Map read-only alias          }
        DBG_C_MapRWAlias        = 19;      { Map read-write alias         }
        DBG_C_UnMapAlias        = 20;      { Unmap Alias                  }
        DBG_C_Connect           = 21;      { Connect to Debuggee          }
        DBG_C_ReadMemBuf        = 22;      { Read Memory Buffer           }
        DBG_C_WriteMemBuf       = 23;      { Write Memory Buffer          }
        DBG_C_SetWatch          = 24;      { Set Watchpoint               }
        DBG_C_ClearWatch        = 25;      { Clear Watchpoint             }
        DBG_C_RangeStep         = 26;      { Range Step                   }
        DBG_C_Continue          = 27;      { Continue after an Exception  }
        DBG_C_AddrToObject      = 28;      { Address to Object            }
        DBG_C_XchgOpcode        = 29;      { Exchange opcode and go       }
        DBG_C_LinToSel          = 30;      { 32 to 16 conversion      A001}
        DBG_C_SelToLin          = 31;      { 16 to 32 conversion      A001}

        {------ Constants -------------------}
        DBG_L_386               = 1;
        DBG_O_OBJMTE            = $10000000;

        {------ Notifications ---------------}
        DBG_N_SUCCESS             =  0;
        DBG_N_ERROR               = -1;
        DBG_N_ProcTerm            = -6;
        DBG_N_Exception           = -7;
        DBG_N_ModuleLoad          = -8;
        DBG_N_CoError             = -9;
        DBG_N_ThreadTerm          = -10;
        DBG_N_AsyncStop           = -11;
        DBG_N_NewProc             = -12;
        DBG_N_AliasFree           = -13;
        DBG_N_Watchpoint          = -14;
        DBG_N_ThreadCreate        = -15;
        DBG_N_ModuleFree          = -16;
        DBG_N_RangeStep           = -17;

        DBG_X_PRE_FIRST_CHANCE    = 0;
        DBG_X_FIRST_CHANCE        = 1;
        DBG_X_LAST_CHANCE         = 2;
        DBG_X_STACK_INVALID       = 3;
     
        DBG_W_Local               = $0000001;
        DBG_W_Global              = $0000002;
        DBG_W_Execute             = $00010000;
        DBG_W_Write               = $00020000;
        DBG_W_ReadWrite           = $00030000;

{You need a heavy manual if you want to know how this procedure works. Used
for writing debuggers.}
function DosDebug (DebugBuf: PDbgBuf):longint; cdecl;

function DosDebug (var APDbgBuf: TDbgBuf): longint; cdecl;
                  
{ codeTerminate values (also passed to ExitList routines) }
const   TC_exit         = 0;
        TC_harderror    = 1;
        TC_trap         = 2;
        TC_killprocess  = 3;
        TC_exception    = 4;

{ DosExitList options }
const   ExLst_Add       = 1;
        ExLst_Remove    = 2;
        ExLst_Exit      = 3;

type    TExitProc=procedure(Reason:longint); cdecl;

{Add/remove an exitprocedure to the exit list. Also used to terminate an
 exit procedure. An exit procedure will be called on exiting of the program.

    OrderCode       = One of the EXLST_XXXX constants.
    Proc            = Address of the exit procedure.

An exit procedure is called with one of the TC_XXXX constants. When it is
done it must call DosExitList with ExLst_Exit.

Exit procedures are called in random order.}
function DosExitList(OrderCode:longint;Proc:TExitProc):longint; cdecl;

{ DosExecPgm options }
const   deSync          = 0;    {Wait until program terminates.}
        deAsync         = 1;    {Do not wait.}
        deAsyncResult   = 2;    {Do not wait. DosWaitChild will follow to
                                 check if process has been terminated. If
                                 you use this, you must use DosWaitChild,
                                 because OS/2 will not free memory that is
                                 allocated for the result codes if you don't.}
        deTrace         = 3;    {Trace-able. Info Wanted!}
        deBackground    = 4;    {Do not run as child. Run in a separate
                                 session.}
        deSuspended     = 5;    {Child will be loaded, but not executed.}
        deAsyncResultDb = 6;    {?? Info wanted.}

(* The following for compatibility only *)
        EXEC_SYNC          =deSync;
        EXEC_ASYNC         =deAsync;
        EXEC_ASYNCRESULT   =deAsyncResult;
        EXEC_TRACE         =deTrace;
        EXEC_BACKGROUND    =deBackground;
        EXEC_LOAD          =deSuspended;
        EXEC_ASYNCRESULTDB =deAsyncResultDb;
        
type    TResultCodes=record
            TerminateReason,        {0 = Normal termionation.
                                     1 = Critical error.
                                     2 = Trapped. (GPE, etc.)
                                     3 = Killed by DosKillProcess.}
            ExitCode:longint;       {Exit code of child.}
        end;

{Execute a program.

 ObjName        = If a DLL cannot be found, its name will be returned here.
 ObjLen         = Size of your ObjName buffer.
 ExecFlag       = One of the deXXXX constants.
 Res            = See TResultcodes.
 Args           = Arguments. ASCIIZ strings. End of Args given by an empty
                  string (#0). First arg must be filename without path and
                  extension. nil is also allowed.
 Env            = Environment. ASCIIZ strings. A variable has the format
                  NAME=CONTENTS. End of Env given by an empty string (#0).
                  nil is also allowed.
 FileName       = Filename with full path and extension. Is not sensitive
                  for the PATH environment variable.}
function DosExecPgm(ObjName:PChar;ObjLen,ExecFlag:longint;
                    Args,Env:PByteArray;var Res:TResultCodes;
                    FileName:PChar):longint; cdecl;
function DosExecPgm(var ObjName:string;ExecFlag:longint;
                    Args,Env:PByteArray;var Res:TResultCodes;
                    const FileName:string):longint;

{Wait until a child process terminated. Sometimes called DosCWait.

Action              = 0 = Wait until child terminates.
                      1 = Wait until child and all its childs terminate.
Option              = Flags. Either dtWait or dtNoWait.
Res                 = See TResultCodes.
TermPID             = Process ID that has been terminated. Usefull when
                      terminating a random process.
PID                 = Process ID of process to terminate. Use a zero to
                      terminate a random process.}
function DosWaitChild(Action,Option:longint;var Res:TResultCodes;
                      var TermPID:longint;PID:longint):longint; cdecl;

const   dpProcess       = 0;
        dpProcessChilds = 1;
        dpThread        = 2;

        dpSameClass     = 0;
        dpIdleClass     = 1;
        dpRegular       = 2;
        dpTimeCritical  = 3;

{Set priority of a thread or all threads in another process.

 Scope              = 0 = Set for all threads of a process.
                      1 = Set for all threads of a process and its childs.
                      2 = Set for a thread of the current process.
 TrClass            = 0 = Do not change class.
                      1 = Change to idle time class.
                      2 = Change to regular class.
                      3 = Change to time-critical class.
 Delta              = Value to add to priority. Resulting priority must be in
                      the range 0..31 (Delta value must be within -31..31).
 PortID             = Process ID when Scope=0 or 1, thread ID when Scope=2.}
function DosSetPriority(Scope,TrClass,Delta,PortID:longint):longint; cdecl;

{Terminate a process. If the process isn't a child process, it can refuse
 to terminate.

 Action             = 0 = Terminate process and all its childs.
                      1 = Terminate process only.
 PID                = Process ID of process to terminate.}
function DosKillProcess(Action,PID:longint):longint; cdecl;

const   AppTyp_NotSpec          = $0000; {Apptype is unknown.}
        AppTyp_NotWindowCompat  = $0001; {App cannot run in a window.}
        AppTyp_WindowCompat     = $0002; {App can run in a window.}
        AppTyp_WindowAPI        = $0003; {App uses PM}
        AppTyp_Bound            = $0008; {App uses Family API.}
        AppTyp_DLL              = $0010; {File is a DLL.}
        AppTyp_DOS              = $0020; {App is a PC-DOS program.}
        AppTyp_PhysDrv          = $0040; {App is a physical device driver.}
        AppTyp_VirtDrv          = $0080; {App is virtual device driver.}
        AppTyp_ProtDLL          = $0100; {File is a protected mode DLL.}
        AppTyp_WindowsReal      = $0200; {M$ Winslows app, real mode.}
        AppTyp_WindowsProt      = $0400; {M$ Winslows app, protected mode.}
        AppTyp_32bit            = $4000; {App is 32 bit.}

{Get the application type of an executable file on disk.
 FileName           = Name of file to get type from.
 Flags              = Receives a bitfield using the AppTyp constants.}
function DosQueryAppType(FileName:PChar;var Flags:longint):longint; cdecl;

const   diPrinter   = 0;    {Get number of printer (parallel) ports.}
        diRS232     = 1;    {Get number of serial ports.}
        diFloppy    = 2;    {Get number of floppy drives.}
        diCopro     = 3;    {Get number of FPU's installed (either 0 or 1).}
        diSubModel  = 4;    {??? System submodel type?}
        diModel     = 5;    {??? System model type?}
        diAdapter   = 6;    {0=Monochrome display, 1=other. ??? Does OS/2
                             support monochrome displays?}

{Get information about attached devices.
 DevInfo            = Receives requested information.
 Item               = One of the dixxxx constants.}
function DosDevConfig(var DevInfo:byte;Item:longint):longint; cdecl;

{****************************************************************************

                        File handling related routines.

****************************************************************************}

const   MaxPathLength=260;
        MaxPathComponent=256;

type    TFileLock=record
            Offset,Range:longint;
        end;
        PFileLock=^TFileLock;
        FileLock=TFileLock;

{Lock or unlock an area of a file. Other processes may not access that part
 of the file.

 Unlock         = Area to unlock. (0,0) = Do not unlock.
 Lock           = Area to lock.   (0,0) = Do not lock.
 Timeout        = Number of miliseconds to wait if another process has locked
                  the file.
 Flags          = Bitfield:
                  Bit 0:    0 = Other processes are denied access.
                            1 = Other processes may still read from the area.
                  Bit 1:    0 = Normal locking mode.
                            1 = Atomic mode. Refer IBM's documentation.}
function DosSetFileLocks(Handle:longint;var Unlock,Lock:TFileLock;
                         Timeout,Flags:longint):longint; cdecl;

{Cancel a filelock area.

Handle  = File handle.
Lock    = Area that is locked now.}
function DosCancelLockRequest(Handle:longint;var Lock:TFileLock):longint;
                                                                         cdecl;

{Data structures for extended attributes. Reading IBM's documentation is
 highly recommended before experimenting with EAs.}

const   fEA_needEA=$80;

        eaBinary        = $fffe;
        eaASCII         = $fffd;
        eaBitmap        = $fffb;
        eaMetaFile      = $fffa;
        eaIcon          = $fff9;
        eaEA            = $ffee;
        eaMVMT          = $ffdf;
        eaMVST          = $ffde;
        eaASN1          = $ffdd;

type    TgEA=record
            NameLen:byte;
            Name:array[0..9999] of char;
        end;
        PgEA=^TgEA;

        TgEAList=record
            ListLen:longint;
            List:array[0..9999] of TgEA;
        end;
        PgEAList=^TgEAList;

        TfEA=record
            EA,
            NameLen:byte;
            Value:word;
        end;
        PfEA=^TfEA;

        TfEAList=record
            Size:longint;
            List:array[0..9999] of TfEA;
        end;
        PfEAList=^TfEAlist;

        TEAOp=record
            gEAList:PgEAList;
            fEAList:PfEAList;
            Error:longint;
        end;
        PEAOp=^TEAOp;

        TfEA2=record
            NextEntry:longint;
            Flags,
            NameLen:byte;
            Value:word;
            szName:array[0..9999] of char;
        end;
        PfEA2=^TfEA2;

        TfEA2List=record
            ListLen:longint;
            List:array[0..9999] of TfEA2;
        end;
        PfEA2List=^TfEA2List;

        TgEA2=record
            NextEntry:longint;
            NameLen:byte;
            Name:array[0..9999] of char;
        end;
        PgEA2=^TgEA2;

        TgEA2list=record
          ListLen:longint;
          List:array[0..9999] of TgEA2;
        end;
        PgEA2List=^TgEA2List;

        TEAOp2=record
            gEA2List:PgEA2List;
            fEA2List:PfEA2List;
            Error:longint;
        end;
        PEAOp2=^TEAOp2;

        TEASizeBuf=record
            MaxEASize:word;
            MaxEAListSize:longint;
        end;
        PEASizeBuf=^TEASizeBuf;


{*******************End of extented attribute datastructures.***************}

{Usefull constanst for Action parameter.}
const       doOpened        =  1;
            doCreated       =  2;
            doOverwritten   =  3;

{Usefull constants for OpenFlags parameter.}
const       doFail          =  0;
            doOpen          =  1;
            doOverwrite     =  2;
            (*
                fixed by KO M.H. on 1999.07.04
                contents : Creation flags is 10 hex not 10 dec.
            *)
            doCreate        = 16;

{Usefull constants for openmode parameter.}

const       doRead          =     0;
            doWrite         =     1;
            doReadWrite     =     2;
            doDenyRW        =    16;
            doDenyWrite     =    32;
            doDenyRead      =    48;
            doDenyNone      =    64;
            doNoInherit     =   128;
            doSequential    =   256;
            doRandom        =   512;
            doNoCache       =  4096;
            doFailOnErr     =  8192;
            doWriteThru     = 16384;
            doDASD          = 32768;

{ Open a file.

 FileName       = Name of file.
 Handle         = Receives filehandle.
 Action         = Receives result of opening.
                    1 = Existing file opened.
                    2 = File did not exist. Created.
                    3 = File existed. Overwritten.
 InitSize       = Initial size of file when creating or overwriting.
                  Ignored when you do not. Must be zero when the file is
                  created or overwritten in read-only mode.
 Attrib         = Attributes when creating or overwriting files.
 OpenFlags      = Bitfield describing what to do when file exists or doesn't
                  exist.
 OpenMode       = Bitfield describing describing how to open a file.
 EA             = Extended attributes to give file when created. Use a nil
                  pointer if you don't want to give it extended attributes.
                  Use it only when creating or overwriting file. Use nil
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
file starts with 512 bytes of bootsector, then 512 bytes of the second sector etc.
The filename must consist of the driveletter followed by a semicolon.}
function DosOpen(FileName:PChar;var Handle,Action:longint;
                 InitSize:longint;Attrib,OpenFlags,FileMode:longint;
                 EA:PEAOp2):longint; cdecl;
{This variant of DosOpen always creates or overwrites a file.}
function DosCreate(FileName:PChar;var Handle:longint;
                   Attrib,OpenMode:longint):longint;
{This variant of DosOpen always opens an existing file.}
function DosOpen(FileName:PChar;var Handle:longint;
                 Attrib,OpenMode:longint):longint;
{There are also string variants.}
function DosOpen(const FileName:string;var Handle,Action:longint;
                 InitSize,Attrib,OpenFlags,OpenMode:longint;
                 ea:PEAOp2):longint;
function DosCreate(const FileName:string;var Handle:longint;
                   Attrib,OpenMode:longint):longint;
function DosOpen(const FileName:string;var Handle:longint;
                 Attrib,OpenMode:longint):longint;


{Close a file.
Cannot fail if handle does exist.}
function DosClose(Handle:longint):longint; cdecl;

{Read from a file or other type of handle.

    Handle      = File handle.
    Buffer      = The read data is stored here.
    Count       = Number of bytes to read.
    ActCount    = Number of bytes actually read.}
function DosRead(Handle:longint;var Buffer;Count:longint;
                 var ActCount:longint):longint; cdecl;

{Write to a file or other type of handle.

    Handle      = File handle.
    Buffer      = The data to be written.
    Count       = Number of bytes to write.
    ActCount    = Number of bytes actually written.}
function DosWrite(Handle:longint;var Buffer;Count:longint;
                  var ActCount:longint):longint; cdecl;

const   dsZeroBased=0;      {Set filepointer from begin of file.}
        dsRelative=1;       {Set filepointer relative to the current one.}
        dsEndBased=2;       {Set filepointer from end of file.}

{Change the filepointer of a file.}
function DosSetFilePtr(Handle:Longint;Pos,Method:longint;
                       var PosActual:longint):Longint; cdecl;
{This variant seeks always from begin of file and does not return the
 actual position.}
function DosSetFilePtr(Handle:longint;Pos:longint):longint;
{This variant returns the current filepointer.}
function DosGetFilePtr(Handle:longint;var PosActual:longint):longint;

{Use DosQueryFileInfo or DosQueryPathInfo to get the size of a file.}

{Change the size of a file.}
function DosSetFileSize(Handle,Size:longint):longint; cdecl;

{Flush update the changes to a file to disk.}
function DosResetBuffer(Handle:longint):longint; cdecl;

{Duplicate or redirect a handle.
To duplicate a handle: Fill handle with source handle and duplicate with -1.
                       Copy of handle will be returned in duplicate.
To redirect a handle:  Fill handle with handle to which the handle to
                       redirect will be redirected. The handle that will be
                       redirected should be placed in duplicate.}
function DosDupHandle(Handle:longint;var Duplicate:longint):longint; cdecl;

{Return information about a specific handle. See DosOpen for a
 description of FileMode.}
function DosQueryFHState(Handle:longint;var FileMode:longint):longint; cdecl;

{Set information about a specific handle. See DosOpen for a description
 of FileMode.}
function DosSetFHState(Handle,FileMode:longint):longint; cdecl;

{Usefull constants for the handle type.}
const   dhFile      =    0;
        dhDevice    =    1;
        dhPipe      =    2;
        dhNetwork   = 8192;

{Determine if a handle belongs to a file, a device or a pipe.
 Handle             = Handle tp query info about.
 HandType           = Bits 0-1:   00 = File
                                  01 = Device
                                  02 = Pipe
                      Bit 15:     0 = Local.
                                  1 = On network.}
function DosQueryHType(Handle:longint;var HandType:longint;
                       var Attr:longint):longint; cdecl;

{****************************************************************************

                    File management related routines.

****************************************************************************}


{Edit a filename using wildcard.

Example editing CONFIG.SYS with *.BAK becomes CONFIG.BAK.
Usefull when parsing commands like 'copy config.sys *.bak'.
All filename characters are casemapped.'

MetaLevel       = 0 Use modern semantics
MetaLevel       = 1 Use OS/2 1.2 semantics
Source          = string to edit
Edit            = editstring
Target          = destination buffer
TargetLen       = size of the destination buffer}
function DosEditName(MetaLevel:longint;Source,Edit:PChar;
                     Target:PChar;TargetLen:longint):longint; cdecl;
function DosEditName(MetaLevel:longint;const Source,Edit:string;
                     var Target:string):longint;

{Move or rename a file.
 OldFile    = old name of file
 NewFile    = new name of file}
function DosMove(OldFile,NewFile:PChar):longint; cdecl;
function DosMove(const OldFile,NewFile:string):longint;


const   dcExisting=1;           {Overwrite existing files.}
        dcAppend=2;             {Append to existing file.}
        dcFailAs=4;             {?? Info wanted!}

{Copy a file.
 OldFile    = source file
 NewFile    = destination file}
function DosCopy(OldFile,NewFile:PChar;Option:longint):longint; cdecl;
function DosCopy(const OldFile,NewFile:string;Option:longint):longint;

{Delete a file from disk.}
function DosDelete(FileName:PChar):longint; cdecl;
function DosDelete(const FileName:string):longint;

{Destroy a file on disk. DosForceDelete makes sure that the file cannot
 be unerased anymore.}
function DosForceDelete(FileName:PChar):longint; cdecl;
function DosForceDelete(const FileName:string):longint;

{Create a new directory.

Name            = Name of directory to create.
EA              = Extented attributes to give the directory. Use nil if you
                  do not want do give it extented attributes. Only the FEA
                  list is used.}
function DosCreateDir(Name:PChar;EA:PEAOp2):longint; cdecl;
function DosCreateDir(const Name:string;EA:PEAOp2):longint;
{Variants without the EA parameter (nil is used).}
function DosCreateDir(Name:PChar):longint;
function DosCreateDir(const Name:string):longint;

{Remove a directory.}
function DosDeleteDir(Name:PChar):longint; cdecl;
function DosDeleteDir(const Name:string):longint;

{Set the current drive. Cannot fail if the driveletter is correct.}
function DosSetDefaultDisk(DiskNum:longint):longint; cdecl;

{Get the current drive. Because it cannot fail, it is declared as procedure.}
procedure DosQueryCurrentDisk(var DiskNum:longint;var Logical:longint); cdecl;

{Set the current directory.}
function DosSetCurrentDir(Name:PChar):longint; cdecl;
function DosSetCurrentDir(const Name:string):longint;

{Get the current directory.}
function DosQueryCurrentDir(DiskNum:longint;var Buffer;
                            var BufLen:longint):longint; cdecl;
function DosQueryCurrentDir(DiskNum:longint;var Buffer:string):longint;

{Send/receive information to a device.

 Handle             = A file handle to a device, instead of a file.
 Category           = The category of functions the function is in.
 Func               = Function to call.
 Params             = Parameters for the function.
 ParamLen           = Size of the params buffer.
 ParamSize          = Size of the parametrs to send to the device
                      Receives size of the returned parameters.
 Data               = Data to send to device.
 DataLen            = Size of your data buffer.
 DataSize           = Size of the data to send to device.
                      Receives size of the data returned by the device.}
function DosDevIOCtl(Handle,Category,Func:longint;var Params;
                     ParamLen:longint;var ParamSize:longint;
                     var Data;DataLen:longint;var DataSize:
                     longint):longint; cdecl;

{****************************************************************************

                      File searching related routines.

****************************************************************************}

const   faReadOnly      =  1;
        faHidden        =  2;
        faSystem        =  4;
        faReserve       =  8;
        faDirectory     = 16;
        faArchive       = 32;

        ilStandard      =  1;
        ilQueryEAsize   =  2;
        ilQueryEAs      =  3;
        ilQueryFullName =  5;

{Format of date records:

 Bit 0..4:      day
 Bit 5..8:      month
 Bit 9..15:     year minus 1980

 Format of time records:

 Bit 0..4:      seconds divided by 2
 Bit 5..10:     minutes
 Bit 11..15:    hours}

type
        (*
            added by KO Myung-Hun on 1999.07.04
            for compatible with previous source code
        *)
        TFileStatus = object
        end;
        PFileStatus = ^TFileStatus;

        (*
            modified TFileStatus to TFileStatus0 by KO M.H. on 1999.07.04
            because TFileStatus3 included a new field, NextEntryOffset and
            new TFileStatus3 needed compatibility with previous source code.
        *)
        TFileStatus0 = object( TFileStatus )
            DateCreation,           {Date of file creation.}
            TimeCreation,           {Time of file creation.}
            DateLastAccess,         {Date of last access to file.}
            TimeLastAccess,         {Time of last access to file.}
            DateLastWrite,          {Date of last modification of file.}
            TimeLastWrite:word;     {Time of last modification of file.}
            FileSize,               {Size of file.}
            FileAlloc:longint;      {Amount of space the file really
                                     occupies on disk.}
        end;
        PFileStatus0=^TFileStatus0;

        TFileStatus1=object(TFileStatus0)
            AttrFile:word;          {Attributes of file.}
        end;
        PFileStatus1=^TFileStatus1;

        TFileStatus2=object(TFileStatus0)
            AttrFile:word;
            cbList:longint;
        end;
        PFileStatus2=^TFileStatus2;

        (*
            fixed by KO Myung-Hun on 1999.07.04

            contents : NextEntryOffset field is not included
        *)
        TFileStatus3 = object( TFileStatus )
            NextEntryOffset : Longint;  {Offset of next entry}
            DateCreation,               {Date of file creation.}
            TimeCreation,               {Time of file creation.}
            DateLastAccess,             {Date of last access to file.}
            TimeLastAccess,             {Time of last access to file.}
            DateLastWrite,              {Date of last modification of file.}
            TimeLastWrite:word;         {Time of last modification of file.}
            FileSize,                   {Size of file.}
            FileAlloc:longint;          {Amount of space the file really
                                         occupies on disk.}
            AttrFile:longint;           {Attributes of file.}
        end;
        PFileStatus3=^TFileStatus3;

        TFileStatus4=object(TFileStatus0)
            AttrFile:longint;
            cbList:longint;
        end;
        PFileStatus4=^TFileStatus4;

        TFileFindBuf1=object(TFileStatus1)
            Name:string;                {Also possible to use as ASCIIZ.
                                         The byte following the last string
                                         character is always zero.}
        end;
        PFileFindBuf1=^TFileFindBuf1;


        TFileFindBuf2=object(TFileStatus2)
            Name:string;                {Also possible to use as ASCIIZ.
                                         The byte following the last string
                                         character is always zero.}
        end;
        PFileFindBuf2=^TFileFindBuf2;

        TFileFindBuf3=object(TFileStatus3)
            Name:string;                {Also possible to use as ASCIIZ.
                                         The byte following the last string
                                         character is always zero.}
        end;
        PFileFindBuf3=^TFileFindBuf3;

        TFileFindBuf4=object(TFileStatus4)
            Name:string;                {Also possible to use as ASCIIZ.
                                         The byte following the last string
                                         character is always zero.}
        end;
        PFileFindBuf4=^TFileFindBuf4;

{Find first file matching a filemask. In contradiction to DOS, a search
 handle is returned which should be closed with FindClose when done.
 FileMask       = Filemask to search.
 Handle         = Search handle will be returned here, fill with -1 before
                  call.
 Attrib         = File attributes to search for.
 AFileStatus    = Return buffer.
 FileStatusLen  = Size of return buffer.
 Count          = Fill with maximum number of files to search for, the
                  actual number of matching files found is returned here.
 InfoLevel      = One of the ilXXXX constants. Consult IBM documentation
                  for exact meaning. For normal use: Use ilStandard and
                  use PFileFindBuf3 for AFileStatus.}
function DosFindFirst(FileMask:PChar;var Handle:longint;Attrib:longint;
                      AFileStatus:PFileStatus;FileStatusLen:longint;
                      var Count:longint;InfoLevel:longint):longint; cdecl;
function DosFindFirst(const FileMask:string;var Handle:longint;
                      Attrib:longint;AFileStatus:PFileStatus;
                      FileStatusLen:longint;var Count:longint;
                      InfoLevel:longint):longint;

{Find next matching file.}
function DosFindNext(Handle:longint;AFileStatus:PFileStatus;
                     FileStatusLen:longint;var Count:longint):longint; cdecl;

{Close a search handle. Cannot fail if handle does exist.}
function DosFindClose(Handle:longint):longint; cdecl;

{Get info about a file.

 Handle         = Handle of file.
 InfoLevel      = One of the ilXXXX constants. Consult IBM documentation
                  for exect meaning. For normal use: Use ilStandard and
                  PFileFindBuf3 for AFileStatus.
 AFileStatus    = An info return buffer.
 FileStatusLen  = Size of info buffer.}
function DosQueryFileInfo(Handle,InfoLevel:longint;AFileStatus:PFileStatus;
                          FileStatusLen:longint):longint; cdecl;

{Set info about a file. File must be opened with write permissions. See
 above fo the parameters.}
function DosSetFileInfo(Handle,InfoLevel:longint;AFileStatus:PFileStatus;
                        FileStatusLen:longint):longint; cdecl;

{Return info about a file. In contradiction to the above functions, the
 file does not have to be open.}
function DosQueryPathInfo(FileName:PChar;InfoLevel:longint;
                 AFileStatus:PFileStatus;FileStatusLen:longint):longint; cdecl;
function DosQueryPathInfo(const FileName:string;InfoLevel:longint;
                        AFileStatus:PFileStatus;FileStatusLen:longint):longint;

{Set information about a file.}
function DosSetPathInfo(FileName:PChar;InfoLevel:longint;
                        AFileStatus:PFileStatus;FileStatusLen,
                        Options:longint):longint; cdecl;

{Get info about the names and lengths of the EA's for a file or directory.

 RefType            = 0 = AFile is a pointer to a file-handle.
                      1 = AFile is a pointer to an ASCIIZ string.
 AFile              = Pointer file's name or handle.
 Entry              = Number of EA to query inof about. (1 = first EA).
 Buf                = Buffer where requested info is returned. For InfoLevel
                      1, the buffer is a TfEA2 datastructure.
 BufLen             = Size of buf in bytes.
 Count              = Number of EA's to return info for. Number of EA's that
                      actually fitted in buf is returned here.
 InfoLevel          = Level of information to return. Only level 1 is
                      currently allowed.}

function DosEnumAttribute(RefType:longint;AFile:pointer;
                          Entry:longint;var Buf;BufSize:longint;
                          var Count:longint;InfoLevel:longint):longint; cdecl;
function DosEnumAttribute(Handle,Entry:longint;var Buf;BufSize:longint;
                          var Count:longint;InfoLevel:longint):longint;
function DosEnumAttribute(const FileName:string;
                          Entry:longint;var Buf;BufSize:longint;
                          var Count:longint;InfoLevel:longint):longint;

{Get an environment variable.
 Name               = Name of environment variable to get.
 Value              = Receives pointer to environment string.}
function DosScanEnv(Name:PChar;var Value:PChar):longint; cdecl;
{There is, of course a string variant.}
function DosScanEnv(const Name:string;var Value:string):longint;

const   dsPathOnly      = 0;    {Do not search current dir. (Unless it is
                                 in the directory list.)}
        dsCurrentDir    = 1;    {Search in the current direcotry and in the
                                 directory list.}
        dsEnvironment   = 2;    {The dirlist parameter is not a directory
                                 list, but an environment variable
                                 containing one.}
        dsIgnoreNetErrs = 4;    {Ignore network errors when searching.}

{Search for a file in a given number of directories.
 Flags          = A combination of the dsXXXX constants.
 DirList        = Directory list or environment variable containing list
                  to search in.
 FileName       = Filename to search for. May contain wildcards.
 FullName       = Receives filename found, including path.
 FullLen        = Length of your fullname buffer.}
function DosSearchPath(Flag:longint;DirList,FileName:PChar;
                       FullName:PChar;FullLen:longint):longint; cdecl;
function DosSearchPath(Flag:longint;const DirList,FileName:string;
                       var FullName:string):longint;

{****************************************************************************

                       File system related routines.

****************************************************************************}

type    TFSInfo=record
            case word of
                1:
                    (File_Sys_ID,
                     Sectors_Per_Cluster,
                     Total_Clusters,
                     Free_Clusters:longint;
                     Bytes_Per_Sector:word);
                2:                          {For date/time description,
                                             see file searching realted
                                             routines.}
                    (Label_Date,            {Date when volumelabel created.}
                     Label_Time:word;       {Time when volumelabel created.}
                     VolumeLabel:string);   {Volume label. Can also be used
                                             as ASCIIZ, because the byte
                                             following the last character of
                                             the string is always zero.}
        end;
        PFSInfo=^TFSInfo;

        TAttachData=record
            case integer of         {Flag in [0,1,2].}
                0,1:                {Flag = 0.}
                    (Count:word;
                     Data:TCharArray);
                2:                  {Flag = 2.}
                    (PipeHandle:longint;
                                    {Handle of named pipe opened by spooler.}
                     SpoolName:string);
                                    {Name of spooler object. Can also be used
                                     as ASCIIZ, because the bute following
                                     the last character is always zero.}
        end;
        PAttachData=^TAttachData;

        TFSQBuffer2=record
            _Type:word;
            NameLen:word;
            FSDNameLen:word;
            FSADataLen:word;
            Name:char;
            Nul1:byte;
            FSDName:char;
            Nul2:byte;
            FSAData:char;
            Nul3:byte;
        end;
        PFSQBuffer2=^TFSQBuffer2;

const   fsAttach        = 0;    {Attach a drive.}
        fsDetach        = 1;    {Detach a drive.}
        fsSpoolAttach   = 2;    {Attach a spool device.}
        fsSpoolDetach   = 3;    {Detach a spool device.}

{IBM DOCS: "DosFSAttach attaches or detaches a drive to or from a remote file
 system driver (FSD), or a pseudocharacter device name to or from a local or
 remote FSD."

 DevName            = When flag is 0 or 1, the name of a drive or a pseudo-
                      character device. When using a drivename use the drive-
                      letter followed by a colon.
                      When flag is 2 or 3, the name of a spooled device.
 FileSystem         = Name of the driver that should be attached or detached
                      to DevName. Use nil when flag is 2 or 3.
 Data               = Should contain a number of ASCIIZ strings that will
                      be passed to the filesystem driver when flag is 0 or 1.
                      Should contain de pipehandle and spoolname when flag is
                      2. Should be nil when flag is 3.
 DataLen            = Number of bytes in data parameter.
 Flag               = One of the dsXXXX constants. See above}
function DosFSAttach(DevName,FileSystem:PChar;var Data:TAttachData;
                     DataLen,Flag:longint):longint; cdecl;
function DosFSAttach(const DevName,FileSystem:string;var Data:TAttachData;
                     DataLen,Flag:longint):longint;

{IBMDOCS: "DosQueryFSAttach obtains information about an attached file system
 (local or remote), or about a character device or pseudocharacter device
 attached to the file system."

 DevName            = Name info drive or pseudo character device to query
                      info about. Ignored for InfoLevels 2 and 3.
 Ordinal            = Index into list of character/pseudo-character
                      devices. Starts at 1. Ignored for infolevel 1.
 InfoLevel          = 1 = Return information about a drive or device named
                          by DevName.
                      2 = Return information about a (pseudo) charachter
                          device numbered by Ordinal.
                      3 = Return information about a drive numbered by
                          Ordinal.
 Buffer             = Will be filled with infomation.
 BufLen             = Size of your buffer in bytes. Number of bytes filled
                      in your buffer is returned here.}
function DosQueryFSAttach(DevName:PChar;Ordinal,InfoLevel:longint;
                     var Buffer:TFSQBuffer2;var BufLen:longint):longint; cdecl;
function DosQueryFSAttach(const DevName:string;Ordinal,InfoLevel:longint;
                          var Buffer:TFSQBuffer2;var BufLen:longint):longint;

const   FSCtl_Handle=1;
        FSCtl_PathName=2;
        FSCtl_FSDName=3;
        FSCtl_Error_Info=1;
        FSCtl_Max_EASize=2;

{IBMDOCS: "DosFSCtl provides an extended standard interface between an
 application and a file-system driver (FSD).

 Consult IBM documentation about this function..}
function DosFSCtl(Data:pointer;DataLen:longint;var ResDataLen:longint;
                  Parms:pointer;ParmsLen:longint;var ResParmsLen:longint;
                  _Function:longint;Route:PChar;
                  Handle,Method:longint):longint; cdecl;
function DosFSCtl(Data:pointer;DataLen:longint;var ResDataLen:longint;
                  Parms:pointer;ParmsLen:longint;var ResParmsLen:longint;
                  _Function:longint;const Route:string;
                  Handle,Method:longint):longint;

{Get information about a drive.
InfoLevels:
 1              Get total/free space etc.
 2              Get volumelabel.}
function DosQueryFSInfo(DiskNum,InfoLevel:longint;var Buffer:TFSInfo;
                        BufLen:longint):longint; cdecl;

{Set information about a drive.}
function DosSetFSInfo(DiskNum,InfoLevel:longint;var Buffer:TFSinfo;
                      BufLen:longint):longint; cdecl;

{Check if verify mode is enabled.}
function DosQueryVerify(var Enabled:longint):longint; cdecl;

{Turn the verify mode on or off.}
function DosSetVerify(Enable:longint):longint; cdecl;

{Change the number of filehandles our program can open. (Default=50). It
 won't hurt if there are files open when you are calling this.}
function DosSetMaxFH(Count:longint):longint; cdecl;

{Ask for more filehandles (or dump filehandles). It won't hurt if there are
 files open when you are calling this.
 ReqCount       = Number of filehandles to ask for. (Negative to dump them.)
 CurMaxFH       = Receives the total number of filehandles your program has
                  access to.}
function DosSetRelMaxFH(var ReqCount,CurMaxFH:longint):longint; cdecl;

const   dsFull=0;       {IBM DOCS: "Perform full system shutdown and
                         file-system lock."}
        dsQuiescient=1; {IBM DOCS: "Perform buffer and cache flushing to
                         make system quiescent."}

{Prepare the system for shutdown.}
function DosShutdown(Flags:longint):longint; cdecl;


{Usefull constants fo DosQuerySysInfo.}
const   svMaxPathLength     = 1;        {Maximum length of a pathname.}
        svMaxTextSessions   = 2;        {Maximum number of text sessions.}
        svMaxPMSessions     = 3;        {Maximum number of PM sessions.}
        svMaxVDMSessions    = 4;        {Maximum number of DOS sessions.}
        svBootDrive         = 5;        {Get the boot drive. (A=1, B=2 etc.)}
        svDynPriVariation   = 6;
        svMaxWait           = 7;
        svMinSlice          = 8;
        svMaxSlice          = 9;
        svPageSize          = 10;       {Size of a page. (Always 4096 bytes.)}
        svMajorVersion      = 11;       {Major version number of kernel:
                                         10 for OS/2 1.0 and 1.1,
                                         20 for OS/2 2.0 .. OS/2 4.0.}
        svMinorVersion      = 12;       {Minor version of kernel:
                                         OS/2 2.0: 00, 2.1: 10, 2.11: 11,
                                              3.0: 30, 4.0: 40.}
        svRevision          = 13;       {Revision of kernel. Until now all
                                         OS/2 versions return 0.}
        svMsCount           = 14;       {Uptime in milliseconds.}
        svTimeLow           = 15;       {System time in seconds since
                                         1 January 1970 0:00:00, low dword.}
        svTimeHigh          = 16;       {System time in seconds since
                                         1 January 1970 0:00:00, high dword.}
        svPhysMem           = 17;       {Amount in bytes of physical memory
                                         in system.}
        svResMem            = 18;       {Amount in bytes of resident memory
                                         in system.}
        svAvailMem          = 19;       {Amount in bytes of available
                                         memory.}
        svPrMem             = 20;       {Maximum amount of memory the current
                                         process can request for its
                                         private use.}
        svShMem             = 21;       {Maximum amount of memory the current
                                         process can request for shared
                                         purposes.}
        svTimerInterval     = 22;       {Timer interval in tenths of a
                                         millisecond.}
        svMaxCompLength     = 23;       {Maxmimum length of a component in a
                                         pathname.}
        svForegroundSession = 24;       {Returns the session ID of the fore-
                                         ground session. The presentation
                                         manager has ID 1.}
        svForegroundProcess = 25;       {Returns the process ID of the
                                         current foreground process.}

{Get one or more system parameters.
 First          = First variable to get.
 Last           = Last variable to get.
 Buf            = Receives variables.
 BufSize        - Size of the buffer/}
function DosQuerySysInfo(First,Last:longint;var Buf;BufSize:longint):longint;
                                                                         cdecl;

{Return information about a partitionable disk.}
function DosPhysicalDisk(Func:longint;Buf:pointer;BufSize:longint;
                         Params:pointer;ParamSize:longint):longint; cdecl;

{****************************************************************************

                       Memory allocation related routines.

****************************************************************************}

const   mfPag_Read      = $00001;   {Give read access to memory.}
        mfPag_Write     = $00002;   {Give write access to memory.}
        mfPag_Execute   = $00004;   {Allow code execution in memory.}
        mfPag_Guard     = $00008;   {Used for dynamic memory growing. Create
                                     uncommitted memory and make the first
                                     page guarded. Once it is accessed it
                                     will be made committed, and the next
                                     uncommitted page will be made guarded.}
        mfPag_Commit    = $00010;   {Make the memory committed.}
        mfPag_Decommit  = $00020;   {Decommit the page.}
        mfObj_Tile      = $00040;   {Also allocate 16-bit segments of 64k
                                     which map the memory. (Makes 16<>32 bit
                                     pointer conversion possible.}
        mfObj_Protected = $00080;
        mfObj_Gettable  = $00100;
        mfObj_Giveable  = $00200;
        mfPag_Default   = $00400;
        mfPag_Shared    = $02000;
        mfPag_Free      = $04000;
        mfPag_Base      = $10000;

        mfSub_Init      = $00001;   {Use base, if not set, choose a base
                                     address yourself.}
        mfSub_Grow      = $00002;   {Grow the specified heap, instead of
                                     allocating it. Ignore mfSub_Init.}
        mfSub_Sparse    = $00004;
        mfSub_Serialize = $00008;

(* Plus a little bit compatibility... *)
        pag_Read = mfPag_Read;
        pag_Write = mfPag_Write;
        pag_Execute = mfPag_Execute;
        pag_Guard = mfPag_Guard;
        pag_Commit = mfPag_Commit;
        pag_Decommit = mfPag_Decommit;
        obj_Tile = mfObj_Tile;
        obj_Protected = mfObj_Protected;
        obj_Gettable = mfObj_Gettable;
        obj_Giveable = mfObj_Giveable;
        pag_Default = mfPag_Default;
        pag_Shared = mfPag_Shared;
        pag_Free = mfPag_Free;
        pag_Base = mfPag_Base;
        sub_Init = mfSub_Init;
        sub_Grow = mfSub_Grow;
        sub_Sparse = mfSub_Sparse;
        sub_Serialize = mfSub_Serialize;

{Get some memory.
 P          = Pointer to memory will be returned here.
 Size       = Number of bytes to get. The size is rounded up to a multiple
              of 4096. This is probably not the case on non-intel 386
              versions of OS/2.
 Flags      = One or more of the mfXXXX constants.}
function DosAllocMem(var P:pointer;Size,Flag:longint):longint; cdecl;

{Free a memory block.}
function DosFreeMem(P:pointer):longint; cdecl;

{Set settings for a block of memory.
 P          = Pointer to the memory. Doesn't need to be the start of the
              memory block allocated with DosAllocMem, but must be a multiple
              of 4096.
 Size       = Number of bytes to change settings for. Is rounded up to a
              multile of 4096.
 Flags      = New flags for the memory.}
function DosSetMem(P:pointer;Size,Flag:longint):longint; cdecl;

{Give another process access to a shared memory block.

 P          = Pointer to the shared memory object.
 PID        = Process of destination process.
 Flag       = Permissions the the destination process gets.}
function DosGiveSharedMem(P:pointer;PID,Flag:longint):longint; cdecl;

{Get access to a shared memory object.

 P          = Pointer to shared memory object.
 Flag       = Permissions to ask.}
function DosGetSharedMem(P:pointer;Flag:longint):longint; cdecl;

{Get access to a shared memory object that has a name.

 P          = Pointer to shared memory object.
 Name       = Name of the memory object. (Starting with '\SHAREMEM\'.
 Flag       = Permissions to ask.}
function DosGetNamedSharedMem(var P:pointer;Name:PChar;Flag:longint):longint;
                                                                         cdecl;
function DosGetNamedSharedMem(var P:pointer;const Name:string;
                              Flag:longint):longint;

{Allocate memory so that it can later be shared with another program.
 P          = Reveives pointer to memory.
 Name       = Optional: name to give memory. Must start with '\SHAREMEM\'.
              Use nil for the PChar or '' for the string variant for no name.
 Size       = Number of bytes to allocate.}
function DosAllocSharedMem(var P:pointer;Name:PChar;Size,Flag:longint):longint;
                                                                         cdecl;
function DosAllocSharedMem(var P:pointer;const Name:string;Size,
                                                            Flag:longint):longint;

{Get the size and flags of a block of memory.

 P          = Pointer to the block of memory.
 Size       = Receives block size.
 Flag       = Receives the flags.}
function DosQueryMem(P:pointer;var Size,Flag:longint):longint; cdecl;

{Allocate a block of memory in a heap.
 Base       = Pointer to the start of the heap.
 P          = Receives pointer to the memory bock.
 Size       = Number of bytes to allocate.}
function DosSubAllocMem(Base:pointer;var P:pointer;Size:longint):longint;
                                                                         cdecl;

{Free a block of memory in a heap.
 Base       = Pointer to the start of the heap.
 P          = Pointer to memory block to free.
 Size       = Number of bytes to free.}
function DosSubFreeMem(Base,P:pointer;Size:longint):longint; cdecl;

{Turn a block of memory into a heap.

Base        = Pointer to memory block to turn into a heap.
Flag        = One or more of the mfSub_XXXX flags.
Size        = Size of the requested heap.}
function DosSubSetMem(Base:pointer;Flag,Size:longint):longint; cdecl;

{Destroy a heap. (Memory remains allocated).

Base        = Pointer to the heap to destroy.}
function DosSubUnsetMem(Base:pointer):longint; cdecl;

{****************************************************************************

                        Semaphore related routines

****************************************************************************}

const   smShared        = $0001;    {Semaphore is shared.}
        smMWWaitAny     = $0002;    {MuxWait only: Wait until a semaphore
                                     is cleared.}
        smMWWaitAll     = $0004;    {MuxWait only: Wait until all semaphores
                                     are cleared.}
        Sem_Indefinite_Wait = -1;   {DosRequestMutExSem blocks the calling
                                     thread indefinitely.}
        Sem_Immediate_Return = 0;   {DosRequestMutExSem returns immediately
                                     without blocking the calling thread.}
(* The following just for compatibility. *)
        dcMW_Wait_Any = smMWWaitAny;
        dcMW_Wait_All = smMWWaitAll;

type   PSemRecord=^TSemRecord;
       TSemRecord=record
          Semaphore,                {Handle of semaphore to link.}
          User:longint;
       end;

       PSemArray=^TSemArray;
       TSemArray=array[0..$ffff] of TSemRecord;

{Create an event semaphore.
 Name       = Optional: Name of semaphore to create. Must start with '\SEM32\.
              Use nil for PChar or '' for string variant for noname. An
              unnamed semaphore is not shared unless the sm_Shared flag is
              set.
 Handle     = Receives handle of semaphore.
 Attr       = One or more of the smXXXX constants.
 State      = Initial state: 0 = Reset (false), 1 = Posted (true).}
function DosCreateEventSem(Name:PChar;var Handle:longint;
                           Attr:longint;State:boolean):longint; cdecl;
function DosCreateEventSem(const Name:string;var Handle:longint;
                           Attr:longint;State:boolean):longint;
function DosCreateEventSem(Name:PChar;var Handle:longint;
                           Attr,State:longint):longint; cdecl;
function DosCreateEventSem(const Name:string;var Handle:longint;
                           Attr,State:longint):longint;

{Open a semaphore created by another process or thread.

 Name       = Name of semaphore.
 Handle     = Receives handle of semaphore.}
function DosOpenEventSem(Name:PChar;var Handle:longint):longint; cdecl;
function DosOpenEventSem(const Name:string;var Handle:longint):longint;

{Close an event semaphore.
 Handle     = Handle of a semaphore to close.}
function DosCloseEventSem(Handle:longint):longint; cdecl;

{Reset an event semaphore: *** probeer *** operation.
 Handle     = Handle of semaphore.
 PostCount  = Number of times DosPostEventSem has been called since last
              reset.

 Note:      Returns errorcode 300 if semaphore is already reset.}
function DosResetEventSem(Handle:longint;var PostCount:longint):longint; cdecl;

{Post an event semaphore: *** verhoog *** operation.
 Handle     = Handle of semaphore.

Note:       Returns errorcode 299 if semaphore is already posted.}
function DosPostEventSem(Handle:longint):longint; cdecl;

{Wait until an event semaphore is posted (wait until *** verhoog *** operation).
 Handle     = Handle of semaphore.
 Timeout    = Return with errorcode if timeout milliseconds have past and the
              semaphore is still reset. To return immediately use 0,
              to wait forever use -1.}
function DosWaitEventSem(Handle,Timeout:longint):longint; cdecl;

{Check if an event semaphore is posted (if a *** verhoog *** operation has been done).
 Handle     = Handle of semaphore.
 Posted     = Receives number of times DosPostEventSem was called since
              the last reset.}
function DosQueryEventSem(Handle:longint;var Posted:longint):longint; cdecl;

{Create a Mutual Exclusion semaphore (mutex).
 Name       = Optional: Name to give to semaphore. Must start with '\SEM32\'.
              Use nil for PChar or '' for string variant to use no name.
              If a name if used the semaphore is shared.
 Handle     = Receives handle of semaphore.
 Attr       = One or more of the smXXXX constants.
 State      = Initial state: (0/false=Not owned, 1/true=Owned.)}
function DosCreateMutExSem(Name:PChar;var Handle:longint;
                           Attr:longint;State:boolean):longint; cdecl;
function DosCreateMutExSem(const Name:string;var Handle:longint;
                           Attr:longint;State:boolean):longint;
function DosCreateMutExSem(Name:PChar;var Handle:longint;
                           Attr,State:longint):longint; cdecl;
function DosCreateMutExSem(const Name:string;var Handle:longint;
                           Attr,State:longint):longint;

{Open a shared mutex semaphore.
 Name       = Name of semaphore to open, always starts with '\SEM32\'.
 Handle     = Receives handle to semaphore.}
function DosOpenMutExSem(Name:PChar;var Handle:longint):longint; cdecl;
function DosOpenMutExSem(const Name:string;var Handle:longint):longint;

{Close a mutex semaphore.
 handle     = Handle of semaphore to close.}
function DosCloseMutExSem(Handle:longint):longint; cdecl;

{Request ownership of a mutex semaphore. If the semaphore is already owned the
 process is halted until the semaphore is released.
 Handle     = Handle of semaphore.
 Timeout    = Return with errorcode if the semaphore is still owned after
              timeout milliseconds; special values are Sem_Indefinite_Wait
              and Sem_Immediate_Return.}
function DosRequestMutExSem(Handle,Timeout:longint):longint; cdecl;

{Release the ownership of a mutex semaphore.
 Handle     = Handle of semaphore to release.}
function DosReleaseMutExSem(Handle:longint):longint; cdecl;

{Query the PID and TIB of the owner of a mutex semaphore.
 Handle     = Handle of semaphore.
 PID        = Receives process ID of owner.
 TID        = Receives thread ID of owner.
 Count      = Number of threads (within and outside current process) waiting
              for ownership of semaphore.}
function DosQueryMutExSem(Handle:longint;var PID,TID,Count:longint):longint;
                                                                         cdecl;

{Create a Multiple Wait (MuxWait) semaphore.
 Name       = Optional: Name to give semaphore. Must start with '\SEM32\'.
              Use nil for PChar or '' for string variant to use no name.
              If a name if used the semaphore is shared.
 Handle     = Receives handle of semaphore.
 CSemRec    = Number of semaphores to link muxwait semaphore with.
 SemArray   = Array of semaphore records to link with muxwait semaphore.
 Attr       = One or more of the smXXXX constants.}
function DosCreateMuxWaitSem(Name:PChar;var Handle:longint;CSemRec:longint;
                           var SemArray:TSemArray;Attr:longint):longint; cdecl;
function DosCreateMuxWaitSem(const Name:string;var Handle:longint;
                             CSemRec:longint;var SemArray:TSemArray;
                             Attr:longint):longint;

{Open a MuxWait semaphore.
 Name       = Name of semaphore to open.
 Handle     = Receives handle of semaphore.}
function DosOpenMuxWaitSem(Name:PChar;var Handle:longint):longint; cdecl;
function DosOpenMuxWaitSem(const Name:string;var Handle:longint):longint;

{Close a MuxWait semaphore.}
function DosCloseMuxWaitSem(Handle:longint):longint; cdecl;

{Wait for the MuxWait semaphore to be cleared.
 Handle     = Handle of semaphore.
 Timeout    = Timeout. See above.
 User       = Receives user value of the semaphore that caused the muxwait
              semaphore to be cleared.}
function DosWaitMuxWaitSem(Handle,Timeout:longint;var User:longint):longint;
                                                                         cdecl;

{Add a semaphore to the MuxWait semaphore.

 Handle     = Handle of semaphore.
 SemRec     = The semaphore to add.}
function DosAddMuxWaitSem(Handle:longint;var SemRec:TSemRecord):longint; cdecl;

{Remove a semaphore from the MuxWait semaphore.
 Handle     = Handle of muxwait semaphore.
 Sem        = Handle of semaphore to remove.}
function DosDeleteMuxWaitSem(Handle,Sem:longint):longint; cdecl;

{Query the semaphores from a MuxWait semaphore.
 Handle     = Handle of semaphore.
 CSemRec    = Input: Size of our array. Output: Number of items in array.
 SemRecs    = Array where TSemRecords are stored.
 Attr       = Flags used by creation of semaphore.}
function DosQueryMuxWaitSem(Handle:longint;var CSemRec:longint;
                        var SemRecs:TSemArray;var Attr:longint):longint; cdecl;

{****************************************************************************

                        Timing related routines.

****************************************************************************}


type    TDateTime=packed record
            case byte of
                1:
                    (Hour,
                     Minute,
                     Second,
                     Sec100,
                     Day,
                     Month: byte;
                     Year: word;
                     TimeZone: smallint;
                     WeekDay: byte);
                2:                        (* For compatibility *)
                    (Hours,
                     Minutes,
                     Seconds,
                     Hundredths,
                     _Day,
                     _Month: byte;
                     _Year: word;
                     _TimeZone: smallint;
                     _WeekDay: byte);
        end;
        PDateTime=^TDateTime;


{Get the date and time.}
function DosGetDateTime(var Buf:TDateTime):longint; cdecl;

{Set the date and time.}
function DosSetDateTime(var Buf:TDateTime):longint; cdecl;

{Start a one shot timer.
 MSec       = Number of miliseconds the timer will run.
 HSem       = Handle of event semaphore that is posted when time has expired.
 TimHandle  = Receives timer handle.}
function DosAsyncTimer(MSec,HSem:longint;
                       var TimHandle:longint):longint; cdecl;

{Start a cyclic timer.
 MSec       = Number of miliseconds the timer will run.
 HSem       = Handle of event semaphore that is posted when time has expired.
 TimHandle  = Receives timer handle.}
function DosStartTimer(MSec,HSem:longint;
                       var TimHandle:longint):longint; cdecl;

{Stop a timer and destroy its handle. There is no need to check for an
 error code if you know your timer handle is correct.}
function DosStopTimer(TimHandle:longint):longint; cdecl;

{Get the frequency of the high resolution timer.}
function DosTmrQueryFreq(var Freq:longint):longint; cdecl;

{Get the current value of the high resolution timer.}
function DosTmrQueryTime(var Time:comp):longint; cdecl;

{****************************************************************************

                             DLL specific routines.

****************************************************************************}

{Load a DLL in memory if it is not yet loaded.
 ObjName        = When the DLL cannot be found, or one of the DLL's it needs
                  cannot be found, the name of the DLL will be put here.
 ObjLen         = Size of the ObjName result buffer.
 DLLName        = Name of DLL to load. Do not give an extension or a path,
                  just the name. OS/2 will automatically search through the
                  LIBPATH for the DLL.
 Handle         = Receives DLL handle.}
function DosLoadModule(ObjName:PChar;ObjLen:longint;DLLName:PChar;
                       var Handle:longint):longint; cdecl;
function DosLoadModule(var ObjName:string;ObjLen:longint;
                       const DLLName:string;var Handle:longint):longint;

{Let OS/2 know that we do not need a DLL anymore. If we were the only process
 using the DLL, it is unloaded.}
function DosFreeModule(Handle:longint):longint; cdecl;

{Get the address of a procedure.
 Handle         = DLL handle,
 Ordinal        = Procedure to get address for. 0=Use its name.
 ProcName       = Name of the procedure to query address for. Must be nil
                  for PChar or '' for string variant if Ordinal is nonzero.
 Address        = Receives address of procedure.}
function DosQueryProcAddr(Handle,Ordinal:longint;ProcName:PChar;
                                           var Address:pointer):longint; cdecl;
function DosQueryProcAddr(Handle,Ordinal:longint;const ProcName:string;
                                                     var Address:pointer):longint;

{Get the handle of a loaded DLL or a loaded executable.
 DLLName        = Name of DLL.
 Handle         = Receives DLL handle if present.}
function DosQueryModuleHandle(DLLName:PChar;var Handle:longint):longint; cdecl;
function DosQueryModuleHandle(const DLLName:string;var Handle:longint):longint;

{Get the pathname of a loaded DLL or a loaded executable.

 Handle         = Handle of DLL.
 NameLen        = Maximum length of char array.
 Name           = PChar (or string) where name is returned.}
function DosQueryModuleName(Handle,NameLen:longint;Name:Pchar):longint; cdecl;
{function DosQueryModuleName(Handle:longint;var Name:OpenString):longint;}

const   pt16bit=0;
        pt32bit=1;

{Return if a procedure is either 16 or 32 bit.
 Handle         = Handle of DLL.
 Ordinal        = DLL index number. 0 means use Name.
 Name           = Must be nil for PChar or '' for string variant if Ordinal
                  is zero. Otherwise it contains the procedure name.
 ProcType       = One of the ptXXXX constants.}
function DosQueryProcType(Handle,Ordinal:longint;Name:PChar;
                          var ProcType:longint):longint; cdecl;
function DosQueryProcType(Handle,Ordinal:longint;const Name:string;
                          var ProcType:longint):longint;

{****************************************************************************

                           Resource related routines.

****************************************************************************}

{Possible resource types:}

const   rtPointer       =  1;       {Mouse pointer.}
        rtBitmap        =  2;       {Bitmap}
        rtMenu          =  3;       {Menu template.}
        rtDialog        =  4;       {Dialog template.}
        rtString        =  5;       {A string table.}
        rtFontDir       =  6;       {Font directory.}
        rtFont          =  7;       {A font.}
        rtAccelTable    =  8;       {Accelerator table.}
        rtRcData        =  9;       {Binary data.}
        rtMessage       = 10;       {Error message table.}
        rtDlgInclude    = 11;       {Dialog include filename.}
        rtVKeyTbl       = 12;       {Key to vkey tables.}
        rtKeyTbl        = 13;       {Key to ugl tables.}
        rtCharTbl       = 14;       {Glyph to character tables.}
        rtDisplayInfo   = 15;       {Screen display information.}
        rtFKAShort      = 16;       {Function key area short form.}
        rtFKALong       = 17;       {Function key area long form.}
        rtHelpTable     = 18;       {Help table.}
        rtHelpSubTable  = 19;       {Sub help table.}
        rtFDDir         = 20;       {DBCS unique/font driver directory.}
        rtFD            = 21;       {DBCS unique/font driver.}

{Get the address of a resource object.
 Handle         = Handle of DLL (or executable) to get resource from.
 ResType        = One of the rtXXXX constants.
 ResName        = Number associated to resource object by resource compiler.}
function DosGetResource(Handle,ResType,ResName:longint;var P:pointer):longint;
                                                                         cdecl;

{Remove a resource object from memory.
 P              = Pointer to resource.}
function DosFreeResource(P:pointer):longint; cdecl;

{Get the size of a resource object.
 Handle         = Handle to DLL (or executable).
 IDT            = One of the rtXXXX constants.
 IDN            = Number associated to resource object by resource compiler.
 Size           = Receives resource size.}
function DosQueryResourceSize(Handle,IDT,IDN:longint;var Size:longint):longint;
                                                                         cdecl;


{****************************************************************************

                   Country and codepage specific routines.

****************************************************************************}

type    TCountryCode=record
            Country,            {Country to query info about (0=current).}
            CodePage:longint;   {Code page to query info about (0=current).}
        end;
        PCountryCode=^TCountryCode;
        CountryCode=TCountryCode;

        TTimeFmt=(Clock12,Clock24);

        TCountryInfo=record
            Country,CodePage:longint;       {Country and codepage requested.}
            case byte of
            0:(
            DateFormat:longint;             {1=ddmmyy 2=yymmdd 3=mmddyy}
            CurrencyUnit:array[0..4] of char;
            ThousandSeparator:char;         {Thousands separator.}
            Zero1:byte;                     {Always zero.}
            DecimalSeparator:char;          {Decimals separator,}
            Zero2:byte;
            DateSeparator:char;             {Date separator.}
            Zero3:byte;
            TimeSeparator:char;             {Time separator.}
            Zero4:byte;
            CurrencyFormat,                 {Bit field:
                                             Bit 0: 0=indicator before value
                                                    1=indicator after value
                                             Bit 1: 1=insert space after
                                                      indicator.
                                             Bit 2: 1=Ignore bit 0&1, replace
                                                      decimal separator with
                                                      indicator.}
            DecimalPlace:byte;              {Number of decimal places used in
                                             currency indication.}
            TimeFormat:TTimeFmt;            {12/24 hour.}
            Reserve1:array[0..1] of word;
            DataSeparator:char;             {Data list separator}
            Zero5:byte;
            Reserve2:array[0..4] of word);
            1:(
            fsDateFmt:longint;              {1=ddmmyy 2=yymmdd 3=mmddyy}
            szCurrency:array[0..4] of char; {null terminated currency symbol}
            szThousandsSeparator:array[0..1] of char;
                                            {Thousands separator + #0}
            szDecimal:array[0..1] of char;  {Decimals separator + #0}
            szDateSeparator:array[0..1] of char;
                                            {Date separator + #0}
            szTimeSeparator:array[0..1] of char;
                                            {Time separator + #0}
            fsCurrencyFmt,                  {Bit field:
                                             Bit 0: 0=indicator before value
                                                    1=indicator after value
                                             Bit 1: 1=insert space after
                                                      indicator.
                                             Bit 2: 1=Ignore bit 0&1, replace
                                                      decimal separator with
                                                      indicator}
            cDecimalPlace:byte;             {Number of decimal places used in
                                             currency indication}
            fsTimeFmt:byte;                 {0=12,1=24 hours}
            abReserved1:array[0..1] of word;
            szDataSeparator:array[0..1] of char;
                                            {Data list separator + #0}
            abReserved2:array[0..4] of word);
        end;
        PCountryInfo=^TCountryInfo;
        CountryInfo=TCountryInfo;

        TDBCSRange=record
            Start,Stop:byte;
        end;

        TDBCSArray=array[0..$ffff] of TDBCSRange;
        PDBCSArray=^TDBCSArray;

const   CurrentCountry:TCountryCode=(Country:0;CodePage:0);

{Get country specific information.
    Size        = Size of our datastructure. (SizeOf(TCountryInfo))
    ActualSize  = Size of OS/2's datastructure. ActualSize bytes of
                  our TCountryInfo have been filled.}
function DosQueryCtryInfo(Size:longint;var Country:TCountryCode;
                   var Res:TCountryInfo;var ActualSize:longint):longint; cdecl;

{Get info about a code page with a DBCS character set.}
function DosQueryDBCSEnv(Size:longint;var Country:TCountryCode;
                                                     Buf:PChar):longint; cdecl;

{Convert a string to uppercase.
    Size        = Length of string.
    Country     = Country and codepage for converting.
    AString     = String to convert.}
function DosMapCase(Size:longint;var Country:TCountryCode;
                    AString:PChar):longint; cdecl;
function DosMapCase(var Country:TCountryCode;
                    var AString:string):longint;

{Get a collate table (a table for comparing which character is smaller and
 which one is greater).
    Size        = Length of the databuffer the program has.
    Country     = Country to query table for. (0,0) is default country and
                  codepage.
    Buf         = Buffer to return table in. It's filled with the sort
                  weights of the ascii code. For example the 128th byte
                  contains the weight for ascii code 128.
    TableLen    = Length of collating table.}
function DosQueryCollate(Size:longint;var Country:TCountryCode;
                           Buf:PByteArray;var TableLen:longint):longint; cdecl;

{Get the current codepage. The PWordArray is filled with the current code
 page followed by alternative codepages.}
function DosQueryCP(Size:longint;CodePages:PWordArray;
                                           var ActSize:longint):longint; cdecl;

{Change the codepage, but only for the current process.}
function DosSetProcessCP(CP:longint):longint; cdecl;

{****************************************************************************

                       Exception handling related functions

****************************************************************************}


{Exception constants.}
const       XCPT_Continue_Search            = $00000000;
            XCPT_Continue_Execution         = $ffffffff;
            XCPT_Continue_Stop              = $00716668;

            XCPT_Signal_Intr                = $1;
            XCPT_Signal_KillProc            = $3;
            XCPT_Signal_Break               = $4;

            XCPT_Fatal_Exception            = $c0000000;
            XCPT_Severity_Code              = $c0000000;
            XCPT_Customer_Code              = $20000000;
            XCPT_Facility_Code              = $1fff0000;
            XCPT_Exception_Code             = $0000ffff;

            XCPT_Unknown_Access             = $00000000;
            XCPT_Read_Access                = $00000001;
            XCPT_Write_Access               = $00000002;
            XCPT_Execute_Access             = $00000004;
            XCPT_Space_Access               = $00000008;
            XCPT_Limit_Access               = $00000010;
            XCPT_Data_Unknown               = $ffffffff;

            XCPT_Guard_Page_Violation       = $80000001;
            XCPT_Unable_To_Grow_Stack       = $80010001;
            XCPT_Access_Violation           = $c0000005;
            XCPT_In_Page_Error              = $c0000006;
            XCPT_Illegal_Instruction        = $c000001c;
            XCPT_Invalid_Lock_Sequence      = $c000001d;
            XCPT_Noncontinuable_Exception   = $c0000024;
            XCPT_Invalid_Disposition        = $c0000025;
            XCPT_Unwind                     = $c0000026;
            XCPT_Bad_Stack                  = $c0000027;
            XCPT_Invalid_Unwind_Target      = $c0000028;
            XCPT_Array_Bounds_Exceeded      = $c0000093;
            XCPT_Float_Denormal_Operand     = $c0000094;
            XCPT_Float_Divide_By_Zero       = $c0000095;
            XCPT_Float_Inexact_Result       = $c0000096;
            XCPT_Float_Invalid_Operation    = $c0000097;
            XCPT_Float_Overflow             = $c0000098;
            XCPT_Float_Stack_Check          = $c0000099;
            XCPT_Float_Underflow            = $c000009a;
            XCPT_Integer_Divide_By_Zero     = $c000009b;
            XCPT_Integer_Overflow           = $c000009c;
            XCPT_Privileged_Instruction     = $c000009d;
            XCPT_Datatype_Misalignment      = $c000009e;
            XCPT_Breakpoint                 = $c000009f;
            XCPT_Single_Step                = $c00000a0;
            XCPT_Process_Terminate          = $c0010001;
            XCPT_Async_Process_Terminate    = $c0010002;
            XCPT_Signal                     = $c0010003;

type        PExceptionRegistrationRecord=^TExceptionRegistrationRecord;
            PExceptionReportRecord=^TExceptionReportRecord;
            PContextRecord=^TContextRecord;

            TExceptionHandler=procedure(Report:PExceptionReportRecord;
                                        RegRec:PExceptionRegistrationRecord;
                                        Context:PContextRecord;
                                        DispContext:pointer); cdecl;

            TExceptionRegistrationRecord=record
                Prev_Structure:PExceptionRegistrationRecord;
                ExceptionHandler:TExceptionHandler;
            end;

            TExceptionReportRecord=record
                Exception_Num,
                HandlerFlags:longint;
                Nested_RepRec:PExceptionReportRecord;
                Address:pointer;
                ParamCount:longint;
                Parameters:array [0..9999] of longint;
            end;

            TContextRecord=record
                ContextFlags:longint;
                Env:array[1..7] of longint;
                FPUStack:array[0..7] of extended;
                Reg_GS,
                Reg_FS,
                Reg_ES,
                Reg_DS,
                Reg_EDI,
                Reg_ESI,
                Reg_EAX,
                Reg_EBX,
                Reg_ECX,
                Reg_EDX,
                Reg_EBP,
                Reg_EIP,
                Reg_CS,
                Flags,
                Reg_ESP,
                Reg_SS:longint;
            end;

{Warning!!! Never use Presentation Manager functions from exception
 handlers!}

{Install an exceptionhandler. The Prev_Structure field of RegRec should be
nil, it will be filled in be OS/2. RegRec must be on the stack: It must be a
local variable.}
function DosSetExceptionHandler(var RegRec:TExceptionRegistrationRecord):longint;
                                                                         cdecl;

{Uninstall an exception handler.}
function DosUnSetExceptionHandler(var RegRec:TExceptionRegistrationRecord
                                  ):longint; cdecl;

{Trigger an exception.}
function DosRaiseException(var Excpt:TExceptionReportRecord):longint; cdecl;

{Send a signal to a process.}
function DosSendSignalException(PID,Exception:longint):longint; cdecl;

{Call and remove a set of exceptionhandlers}
function DosUnwindException(var Handler:TExceptionRegistrationRecord;
                            TargetIP:pointer;
                            var RepRec:TExceptionReportRecord):longint; cdecl;

{Full screen applications can get Ctrl-C and Ctrl-Break focus. For all
 processes sharing one screen, only one can have Ctrl-C focus.
 Enable     = 0 = Release focus, 1 = Get focus.
 Times      = Number of times focus has been get minus number of times it
              has been released.}
function DosSetSignalExceptionFocus(Enable:longint;var Times:longint):longint;
                                                                         cdecl;

{Tell OS/2 that if an exception occurs, it must queue it up, until a
 DosExitMustComplete follows. Urgent exceptions still occur. The only
 possible error is that the nesting becomes too high, so error checking
 is only needed in seldom cases.
 Nesting    = Number of DosEnterMustComplete calls minus number of
              DosExitMustComplete calls.}
function DosEnterMustComplete(var Nesting:longint):longint; cdecl;

{Tell OS/2 that it can send exceptions again. See above}
function DosExitMustComplete(var Nesting:longint):longint; cdecl;

{Tell we want further signal exceptions.
 SignalNum  = Signal nummer to acknowlegde.}
function DosAcknowledgeSignalException(SignalNum:longint):longint; cdecl;


{****************************************************************************

                           Queue related routines.

****************************************************************************}

type    TRequestData=record
            PID,                {ID of process that wrote element.}
            Data:longint;       {Information from process writing the data.}
        end;
        PRequestData=^TRequestData;

{Useful constants for priority parameters.}
const   quFIFO=0;
        quLIFO=1;
        quPriority=2;
        quNoConvert_Address=0;
        quConvert_Address=4;

{Close a queue. If the calling process has created the queue, it is
 destroyed. If you can guarantee the handle is correct, there is no need
 to check for error codes.}
function DosCloseQueue(Handle:longint):longint; cdecl;

{Create a queue. The process that creates a queue, owns that queue, and is
 the only one who can read from that queue. Other processes can only write
 to that queue. The queuename must have the format '\QUEUES\name.ext' .

 Handle         = Receives queue handle.
 Priority       = 0 = Use FIFO system.
                  1 = Use LIFO system.
                  2 = Use priority system.
                  Add 4 to convert addresses of data inserted by 16-bit
                  processes to 32 bit pointers.
 Name           = Name of queue to create.}
function DosCreateQueue(var Handle:longint;Priority:longint;
                        Name:PChar):longint; cdecl;
function DosCreateQueue(var Handle:longint;Priority:longint;
                        const Name:string):longint;

{Open an existing queue. You cannot read from the queue unless you are the
 process that created it. The name must have the format '\QUEUES\name.ext'}
function DosOpenQueue(var Parent_PID:longint;var Handle:longint;
                      Name:PChar):longint; cdecl;
function DosOpenQueue(var Parent_PID:longint;var Handle:longint;
                      const Name:string):longint;

{Read a record from a queue, but do not remove it from the queue.
 Handle         = Handle of queue to read from.
 ReqBuffer      = Receives information about read data.
 DataLen        = Receives length of data read.
 DataPtr        = Receives the address of the data.
 Element        = 0 = Return first element in queue.
                  1 = Return next element in queue. Can be repeated.
                      Current element number is returned here, for use
                      with DosReadQueue.
 Wait           = 0 = Wait until there is a queue element available.
                  1 = Return with an error when queue is empty.
 Priority       = Receives priority of queue record (1..15).
 ASem           = Use NIL if Wait=0, give a handle of a semaphore when
                  Wait=1. The semaphore will be cleared when there is an
                  element inserted it the queue.
                  !! event queue}
function DosPeekQueue(Handle:longint;var ReqBuffer:TRequestData;
                      var DataLen:longint;var DataPtr:pointer;
                      var Element:longint;Wait:longint;
                      var Priority:byte;ASem:longint):longint; cdecl;

{Empty a queue. You must be the process the created the queue.}
function DosPurgeQueue(Handle:longint):longint; cdecl;

{Return the number of elements in the queue.}
function DosQueryQueue(Handle:longint;var Count:longint):longint; cdecl;

{Read a record from a queue, but do not remove it from the queue.
 Handle         = Handle of queue to read from.
 ReqBuffer      = Receives information about read data.
 DataLen        = Receives length of data read.
 DataPtr        = Receives the address of the data.
 Element        = 0 = Return first element in queue.
                  Otherwise: Return the element numbered with this.
 Wait           = 0 = Wait until there is a queue element available.
                  1 = Return with an error when queue is empty.
 Priority       = Receives priority of queue record (1..15).
 ASem           = Use NIL if Wait=0, give a handle of a semaphore when
                  Wait=1. The semaphore will be cleared when there is an
                  element inserted it the queue.
                  !! event queue}
function DosReadQueue(Handle:longint;var ReqBuffer:TRequestData;
                      var DataLen:longint;var DataPtr:pointer;
                      Element,Wait:longint;var Priority:byte;
                      ASem:longint):longint; cdecl;

{Write a data record to a queue.
 Handle         = Handle of queue to write to.
 Request        = Value that will be inserted in the RequestData field when
                  element is read from queue.
 DataLen        = Size of data to write.
 DataBuf        = Data to write.
 Priority       = Priority of data in buffer. Only relevant when queue is
                  created with priority support.}
function DosWriteQueue(Handle,Request,Datalen:longint;var DataBuf;
                       Priority:longint):longint; cdecl;

const   deHardErr           = 1;    {Hard errors are enabled, to disable
                                     do not give this switch.}
        deDisableExceptions = 2;    {Exceptions are disabled, to enable
                                     do not give this switch.}

{****************************************************************************

                        Error handling related routines.

****************************************************************************}

{Disable the end user notification of hardware errors and exceptions. Users
 can overide this in config.sys. By default, notification is enabled.
 There is no need for error checking if you can guarantee the parameter is
 correct.}
function DosError(Error:longint):longint; cdecl;

{Get information about an error code.
 It cannot fail, so it is written as procedure.

Code            = Error code to get info about.
_Class          = Receives the error class.
Action          = Receives the recommended action you should take.
Locus           = Receives what could have caused the error.}
procedure DosErrClass(Code:longint;var _Class,Action,Locus:longint); cdecl;


{****************************************************************************

                        Message file specific routines.

****************************************************************************}


type    PInsertTable=^TInsertTable;
        TInsertTable=array[1..9] of PChar;

{Get a message from a messagefile.
 Table          = Table of strings to insert.
 TableSize      = Number of strings in table.
 Buf            = Address of buffer to store message in.
 BufSize        = Size of buffer to store message in.
 MsgNumber      = Number of message to get.
 FileName       = Name of file to get message from.
 MsgSize        = The size of the message returned.}
function DosGetMessage(Table:PInsertTable;TableSize:longint;Buf:PChar;
                       BufSize,MsgNumber:longint;FileName:PChar;
                       var MsgSize:longint):longint;
{And a variant using strings and open arrays.
function DosGetMessage(const Table:array of PString;var Buf:string;
                       BufSize,MsgNumber:longint;const FileName:PChar):longint;}

{And a variant using strings, but with a PChar buffer, because of long
 messages, and open arrays.
function DosGetMessage(const Table:array of PString;Buf:PChar;
                       BufSize,MsgNumber:longint;const FileName:string;
                       MsgSize:longint):longint;}

{Insert textstrings into a message. The message must be loaded before with
 DosGetMessage. This function is used when the insert strings are not yet
 known when the message was loaded.
 Table          = Table of strings to insert.
 TableSize      = Number of struings to insert.
 Message        = Message to insert strings into.
 SrcMessageSize = Size of message to insert strings into.
 Buf            = Receives adjusted message.
 BufSize        = Size of your buffer.
 DstMessageSize = Receives size of adjusted message.}
function DosInsertMessage(Table:PInsertTable;TableSize:longint;
                          Message:PChar;SrcMessageSize:longint;
                          Buf:PChar;BufSize:longint;
                          var DstMessageSize:longint):longint; cdecl;
{And a variant using strings and open arrays.
function DosInsertMessage(Table:array of PString;
                          const Message:string;
                          var Buf:openstring):longint;}

{And a variant using strings, but with a PChar buffer, because of long
 messages, and open arrays.
function DosInsertMessage(Table:array of PString;
                          Message:PChar;SrcMessageSize:longint;
                          Buf:PChar;BufSize:longint;
                          var DstMessageSize:longint):longint;}

{Write a message to a file.
 Handle         = Handle of file.
 Size           = Size of message.
 Buf            = Buffer where message is located.}
function DosPutMessage(Handle,Size:longint;Buf:PChar):longint; cdecl;
function DosPutMessage(Handle:longint;const Buf:string):longint;

{Get info about which codepages and languages a messagefile supports.
 Buf            = Receives information.
 BufSize        = Size of buffer.
 FileName       = Filename of message file.
 InfoSize       = Receives size in bytes of the returned info.}
function DosQueryMessageCP(var Buf;BufSize:longint;FileName:PChar;
                            var InfoSize:longint):longint;
function DosQueryMessageCP(var Buf;BufSize:longint;const FileName:string;
                            var InfoSize:longint):longint;

{****************************************************************************

                           Session specific routines.

****************************************************************************}

type    TStatusData=record
            Length:word;                {Length, in bytes, of datastructure.}
            SelectIND:word;             {Determines if the session can be
                                         selected: Don't change/selectable/
                                         not selectable (0/1/2).}
            BondIND:word;               {Determines which section will come
                                         to the foreground when it is
                                         selected: Don't change/child to
                                         foreground when parent selected/
                                         parent to foreground when parent
                                         selected.}
        end;
        PStatusData=^TStatusData;

type    TStartData=record
        {Note: to omit some fields, use a length smaller than
         SizeOf(TStartData).}
            Length:word;                {Length, in bytes, of datastructure.}
            Related:word;               {Independent/child session (0/1).}
            FgBg:word;                  {Foreground/background (0/1).}
            TraceOpt:word;              {No trace/trace this/trace all
                                         (0/1/2).}
            PgmTitle:PChar;             {Program title.}
            PgmName:PChar;              {Filename to program.}
            PgmInputs:PChar;            {Command parameters (nil allowed).}
            TermQ:PChar;                {System queue. (nil allowed).}
            Environment:PChar;          {Environment to pass (nil allowed).}
            InheritOpt:word;            {Inherit enviroment from shell/
                                         inherit environment from parent
                                         (0/1).}
            SessionType:word;           {Auto/full screen/window/presentation
                                         manager/full screen Dos/windowed Dos
                                         (0/1/2/3/4/5/6/7).}
            Iconfile:PChar;             {Icon file to use (nil allowed).}
            PgmHandle:longint;          {0 or the program handle.}
            PgmControl:word;            {Bitfield describing initial state
                                         of windowed sessions.}
            InitXPos,InitYPos:word;     {Initial top coordinates.}
            InitXSize,InitYSize:word;   {Initial size.}
            Reserved:word;
            ObjectBuffer:PChar;         {If a module cannot be loaded, its
                                         name will be returned here.}
            ObjectBuffLen:longint;      {Size of your buffer.}
        end;
        PStartData=^TStartData;
        StartData=TStartData;

{Start a new session.
 AStartData         = A startdata record.
 SesID              = Receives session ID of session created.
 PID                = Receives process ID of process created.}
function DosStartSession(const AStartData:TStartData;
                         var SesID,PID:longint):longint; cdecl;

{Set the status of a child session.
 SesID              = ID of session.
 AStatus            = Status to set.}
function DosSetSession(SesID:longint;const AStatus:TStatusData):longint; cdecl;

{Bring a child session to the foreground.
 SesID              = ID of session.}
function DosSelectSession(SesID:longint):longint; cdecl;

{Terminate (a) child session(s).
 Scope              = 0 = Terminate specified session.
                      1 = Terminate all child sessions.
 SesID              = ID of session to terminate (ignored when terminating
                      all).}
function DosStopSession(Scope,SesID:longint):longint; cdecl;

{****************************************************************************

                     Named/unnamed pipe specific routines.

****************************************************************************}

type    TAvailData=record
            cbPipe,             {Number of bytes in pipe.}
            cbMessage:word;     {Number of bytes in current message.}
        end;

        TPipeInfo=record
            cbOut:word;         {Size of outbound data.}
            cbIn:word;          {Size of inbound data.}
            MaxInst:byte;       {Maximum number of instances.}
            CurInst:byte;       {Current number of instances.}
            Name:string;        {Name of the pipe. You can use @Name[1] if
                                 you need a PChar to the name; the string is
                                 always followed by a zero.}
        end;

        TPipeSemState=record
            Status:byte;
            Flag:byte;
            Key:word;
            Avail:word;
        end;

{Create an unnamed pipe.
 ReadHandle     = Receives handle for reading from pipe.
 WriteHandle    = Receives handle to write to pipe.
 Size           = Size of pipe to create. 0 means default size. If data is
                  written into a pipe that is smaller than the sent data, the
                  writing thread is suspended until the data has been read
                  from the pipe, thus making room for more data to send.}
function DosCreatePipe(var ReadHandle,WriteHandle:longint;
                       Size:longint):longint; cdecl;

const   {np_XXXX constants for openmode.}
        np_Access_Inbound       = $0000;    {Client to server connection.}
        np_Access_Outbound      = $0001;    {Server to client access.}
        np_Access_Duplex        = $0002;    {Two way access.}
        np_Inherit              = $0080;    {Pipe handle is inherited by
                                             child processes.}
        np_No_Write_Behind      = $4000;    {Don't allow write behind for
                                             remote pipes.}
        {np_XXXX constants for pipemode.}
        np_Unlimited_Instances  = $00ff;    {Unlimited instances.}
        np_ReadMode_Mesg        = $0100;    {Read the pipe as a message
                                             stream instead of as a byte
                                             stream.}
        np_ReadMode_Message     = np_ReadMode_Mesg;
        np_WriteMode_Mesg       = $0400;    {Write the pipe as a message
                                             stream instead of as a byte
                                             stream.}
        np_WriteMode_Message    = np_WriteMode_Mesg;
        np_Type_Message         = np_WriteMode_Mesg;
        np_NoWait               = $8000;    {Dosread and Doswrite do not
                                             wait is no data can be read or
                                             written; they return with an
                                             error message.}

{Create a named pipe.
 Name           = Name of pipe to create.
 Handle         = Receives handle to pipe.
 OpenMode       = A combination of np_XXXX constants for openmode.
 PipeMode       = A combination of np_XXXX constants for pipemode,
                  plus a number within [1..254] which determines the number
                  of instances that can be created to the pipe, or,
                  np_Unlimited_Instance for an unlimited number of
                  instances.
 OutBufSize     = The number of bytes to allocate for the output buffer.
 InBufSize      = The number of bytes to allocate for the input buffer.
 MSec           = The maximum time to wait for an available instance.}
function DosCreateNPipe(Name:PChar;var Handle:longint;OpenMode,PipeMode,
                        OutBufSize,InBufSize,MSec:longint):longint; cdecl;
function DosCreateNPipe(const Name:string;var Handle:longint;OpenMode,
                        PipeMode,OutBufSize,InBufSize,MSec:longint):longint;

{Makes a procedure call to a duplex message pipe.
 Name           = Name of pipe.
 Input          = Buffer that contains data to be written to the pipe.
 InputSize      = Size of the inputdata.
 Output         = Buffer that contains data to be read from the pipe.
 OutputSize     = Size of the outputbuffer.
 ReadBytes      = Receives number of bytes actually read.
 MSec           = The maximum time to wait for an available instance.}
function DosCallNPipe(Name:PChar;var Input;InputSize:longint;
                      var Output;OutputSize:longint;var ReadBytes:longint;
                      MSec:longint):longint; cdecl;
function DosCallNPipe(const Name:string;var Input;InputSize:longint;
                      var Output;OutputSize:longint;var ReadBytes:longint;
                      MSec:longint):longint;

{Prepare a named pipe for a client process.
 Handle         = Handle that was returned when pipe was created.}
function DosConnectNPipe(Handle:longint):longint; cdecl;

{Acknowledges that a client process has closed a named pipe.
 Handle         = Handle that was returned when pipe was created.}
function DosDisconnectNPipe(Handle:longint):longint; cdecl;

const   np_State_Disconnected   = 1;    {Pipe is disconnected.}
        np_State_Listening      = 2;    {Pipe is listening.}
        np_State_Connected      = 3;    {Pipe is connected.}
        np_State_Closing        = 4;    {Pipe is closing.}

{Preview data in a pipe: Read data without removing it.
 Handle         = Handle to named pipe.
 Buffer         = Buffer to receive data in.
 BufSize        = Size of the buffer.
 ReadBytes      = Receives number of bytes put in buffer.
 Avail          = Receives size of available data.
 State          = One of the np_xxxx constants for states.}
function DosPeekNPipe(Handle:longint;var Buffer;BufSize:longint;
                      var ReadBytes:longint;var Avail:TAvailData;
                      var State:longint):longint; cdecl;

{Get information about a named pipe handle.
 Handle         = Handle to pipe.
 State          = A combination of np_XXXX constants for (!!!) pipemode.}
function DosQueryNPHState(Handle:longint;var State:longint):longint; cdecl;

{Return information about a named pipe.
 Handle         = Handle to pipe.
 InfoLevel      = Level of information wanted (1 or 2 allowed).
 Buffer         = TPipeInfo datastructure for level 1.
                  Unique 4 byte identifier of the client for level 2. Only
                  used for LAN based pipe servers.}
function DosQueryNPipeInfo(Handle,InfoLevel:longint;var Buffer;
                           BufSize:longint):longint; cdecl;

{Return information of local named pipes that are attached to a semaphore.
 SemHandle      = Handle to a shared event or MuxWait semaphore that is
                  attached to a named pipe.
 SemArray       = Array in which for each pipe attached to the semaphore.
 BufSize        = Size of SemArray, in bytes.}
function DosQueryNPipeSemState(Semhandle:longint;var SemArray;
                               BufSize:longint):longint; cdecl;

{Resets the blocking mode and state of a named pipe.
 Handle         = Handle to named pipe.
 State          = One of the np_XXXX constants for pipemode.}
function DosSetNPHState(Handle,State:longint):longint; cdecl;

{Attach a shared event semaphore to a local named pipe.
 PipeHandle     = Handle to named pipe.
 SemHandle      = Handle to semaphore.
 Key            = A key that must be different for each named pipe that is
                  attached to the semaphore.}
function DosSetNPipeSem(PipeHandle,SemHandle,Key:longint):longint; cdecl;

{Write to a duplex named pipe; then read from it.
 Handle         = Handle to named pipe.
 OutBuf         = The data to write.
 OutSize        = Size of the data to write.
 InBuf          = Receives the read data.
 InSize         = Size of the input buffer.
 ReadBytes      = Number of bytes read from the pipe.}
function DosTransactNPipe(Handle:longint;var OutBuf;OutSize:longint;
                          var InBuf;InSize:longint;
                          var ReadBytes:longint):longint; cdecl;

{Waits until an instance of a named pipe becomes available.
 Name           = Name of named pipe (always starts with '\PIPE\').
 MSec           = Return with an error code if this time has elapsed.}
function DosWaitNPipe(Name:PChar;MSec:longint):longint; cdecl;
function DosWaitNPipe(const Name:string;MSec:longint):longint;

{****************************************************************************

                    Virtual device driver related routines.

****************************************************************************}

{Open a virtual device driver.
 Name           = Name of virtual device driver.
 Handle         = Receives handle to virtual device driver.}
function DosOpenVDD(Name:PChar;var Handle:longint):longint; cdecl;

{Request to talk with a virtual device driver.
 Handle         = Handle to virtual device driver.
 SGroup         = Handle to the screen group of a DOS session (may be nil).
 Cmd            = A number which indicates the service you call.
 InSize         = Size of the data to send to the VDD.
 InBuffer       = Buffer which contains the data to send to the VDD.
 OutSize        = Size of the buffer in which the VDD will return data.
 OutBuffer      = Receives the data that the VDD returns.}
function DosRequestVDD(Handle,SGroup,Cmd:longint;
                       InSize:longint;var InBuffer;
                       OutSize:longint;var OutBuffer):longint; cdecl;

{Close a virtual device driver.}
function DosCloseVDD(Handle:longint):longint; cdecl;

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
procedure DosSelToFlat; cdecl;

type
 TFarPtr = record
  Sel, Offset: word;
 end;

function SelToFlat (AFarPtr: TFarPtr): pointer;

{Convert a 32 bit near pointer to a 16 bit far pointer.
 This procedure needs to be called from assembler.
 This procedure works by allocating a selector at the same physical address
 as the pointer you pass points to.

In:
  eax           Pointer to convert in 32 bit near format.

Out:
  eax           Returned 16 bit far pointer in selector:offset format.}
procedure DosFlatToSel; cdecl;

{typecast result to TFarPtr}
function FlatToSel (APtr: pointer): cardinal;

{Allocate Count dwords in a memory block unique in each thread. A maximum
 of 8 dwords can be allocated at a time, the total size of the thread local
 memory area is 128 bytes; FPC 1.1+ uses one dword from this for internal
 multi-threading support, leaving 124 bytes to programmers.}
function DosAllocThreadLocalMemory (Count: cardinal; var P: pointer): longint;
                                                                         cdecl;

{Deallocate a previously allocated space in the thread local memory area.}
function DosFreeThreadLocalMemory (P: pointer): longint; cdecl;

{***************************************************************************}
implementation
{***************************************************************************}

function DosCreateThread(var TID:longint;Address:TThreadEntry;
                          aParam:pointer;Flags:longint;
                          StackSize:longint):longint; cdecl;

external 'DOSCALLS' index 311;

function DosCreateThread (var TID: longint; Address: pointer;
                   AParam: Pointer; Flags, StackSize: longint): longint; cdecl;

external 'DOSCALLS' index 311;

function DosSuspendThread(TID:longint):longint; cdecl;

external 'DOSCALLS' index 238;

function DosResumeThread(TID:longint):longint; cdecl;

external 'DOSCALLS' index 237;

function DosKillThread(TID:longint):longint; cdecl;

external 'DOSCALLS' index 111;

function DosWaitThread(var TID:longint;Option:longint):longint; cdecl;

external 'DOSCALLS' index 349;

function DosEnterCritSec:longint; cdecl;

external 'DOSCALLS' index 232;

function DosExitCritSec:longint; cdecl;

external 'DOSCALLS' index 233;

procedure DosExit(Action,Result:longint); cdecl;

external 'DOSCALLS' index 234;

procedure DosGetInfoBlocks(var ATIB:PThreadInfoBlock;
                           var APIB:PProcessInfoBlock); cdecl;

external 'DOSCALLS' index 312;

procedure DosGetInfoBlocks(PATIB:PPThreadInfoBlock;
                           PAPIB:PPProcessInfoBlock); cdecl;

external 'DOSCALLS' index 312;

procedure DosSleep (MSec:longint); cdecl;

external 'DOSCALLS' index 229;

function DosBeep(Freq,MS:longint):longint; cdecl;

external 'DOSCALLS' index 286;

function DosDebug (DebugBuf: PDbgBuf):longint; cdecl;

external 'DOSCALLS' index 317;

function DosDebug (var APDbgBuf: TDbgBuf): longint; cdecl;

external 'DOSCALLS' index 317;

function DosExitList(OrderCode:longint;Proc:TExitProc):longint; cdecl;

external 'DOSCALLS' index 296;

function DosExecPgm(ObjName:PChar;ObjLen,ExecFlag:longint;
                    Args,Env:PByteArray;var Res:TResultCodes;
                    FileName:PChar):longint; cdecl;

external 'DOSCALLS' index 283;

function DosExecPgm(var ObjName:string;Execflag:longint;
                    Args,Env:PByteArray;var Res:TResultCodes;
                    const FileName:string):longint;

var T,T2:array[0..255] of char;

begin
    StrPCopy(@T,FileName);
    DosExecPgm:=DosExecPgm(@T2,SizeOf(T2),ExecFlag,Args,Env,Res,@T);;
    ObjName:=StrPas(@T2);
end;

function DosWaitChild(Action,Option:longint;var Res:TResultCodes;
                      var TermPID:longint;PID:longint):longint; cdecl;

external 'DOSCALLS' index 280;

function DosSetPriority(Scope,TrClass,Delta,PortID:longint):longint; cdecl;

external 'DOSCALLS' index 236;

function DosKillProcess(Action,PID:longint):longint; cdecl;

external 'DOSCALLS' index 235;

function DosQueryAppType(FileName:PChar;var Flags:longint):longint; cdecl;

external 'DOSCALLS' index 323;

function DosDevConfig(var DevInfo:byte;Item:longint):longint; cdecl;

external 'DOSCALLS' index 231;

function DosSetFileLocks(Handle:longint;var Unlock,Lock:TFileLock;
                         Timeout,Flags:longint):longint; cdecl;

external 'DOSCALLS' index 428;

function DosCancelLockRequest(Handle:longint;var Lock:TFileLock):longint;
                                                                         cdecl;

external 'DOSCALLS' index 429;

function DosOpen(FileName:PChar;var Handle,Action:longint;
                 InitSize,Attrib,OpenFlags,FileMode:longint;
                 EA:PEAOp2):longint; cdecl;

external 'DOSCALLS' index 273;

function DosCreate(FileName:PChar;var Handle:longint;
                   Attrib,OpenMode:longint):longint;

var Action:longint;

begin
    DosCreate:=DosOpen(FileName,Handle,Action,0,Attrib,18,OpenMode,nil);
end;

function DosOpen(FileName:PChar;var Handle:longint;
                 Attrib,OpenMode:longint):longint;

var Action:longint;

begin
    DosOpen:=DosOpen(FileName,Handle,Action,0,Attrib,1,OpenMode,nil);
end;

function DosOpen(const FileName:string;var Handle,Action:longint;
                 InitSize,Attrib,OpenFlags,OpenMode:longint;
                 EA:PEAOp2):longint;

var T:array[0..255] of char;

begin
    StrPCopy(@T,FileName);
    DosOpen:=DosOpen(@T,Handle,Action,InitSize,Attrib,OpenFlags,OpenMode,EA);
end;

function DosCreate(const FileName:string;var Handle:longint;
                   Attrib,OpenMode:longint):longint;

var T:array[0..255] of char;
    Action:longint;

begin
    StrPCopy(@T,FileName);
    DosCreate:=DosOpen(@T,Handle,Action,0,Attrib,18,OpenMode,nil);
end;

function DosOpen(const FileName:string;var Handle:longint;
                 Attrib,OpenMode:longint):longint;

var T:array[0..255] of char;
    Action:longint;

begin
    StrPCopy(@T,FileName);
    DosOpen:=DosOpen(@T,Handle,Action,0,Attrib,1,OpenMode,nil);
end;

function DosClose(Handle:longint):longint; cdecl;

external 'DOSCALLS' index 257;

function DosRead(Handle:longint;var Buffer;Count:longint;
                 var ActCount:longint):longint; cdecl;

external 'DOSCALLS' index 281;

function DosWrite(Handle:longint;var Buffer;Count:longint;
                  var ActCount:longint):longint; cdecl;

external 'DOSCALLS' index 282;

function DosSetFilePtr(Handle:longint;Pos,Method:longint;
                       var PosActual:longint):longint; cdecl;

external 'DOSCALLS' index 256;

function DosSetFilePtr(Handle:longint;Pos:longint):longint;

var PosActual:longint;

begin
    DosSetFilePtr:=DosSetFilePtr(Handle,Pos,0,PosActual);
end;

function DosGetFilePtr(Handle:longint;var PosActual:longint):longint;

begin
    DosGetFilePtr:=DosSetFilePtr(Handle,0,1,PosActual);
end;

function DosSetFileSize(Handle,Size:longint):longint; cdecl;

external 'DOSCALLS' index 272;

function DosResetBuffer(Handle:longint):longint; cdecl;

external 'DOSCALLS' index 254;

function DosDupHandle(Handle:longint;var Duplicate:longint):longint; cdecl;

external 'DOSCALLS' index 260;

function DosQueryFHState(Handle:longint;var FileMode:longint):longint; cdecl;

external 'DOSCALLS' index 276;

function DosSetFHState(Handle,FileMode:longint):longint; cdecl;

external 'DOSCALLS' index 221;

function DosQueryHType(Handle:longint;var HandType:longint;
                       var Attr:longint):longint; cdecl;

external 'DOSCALLS' index 224;

function DosEditName(MetaLevel:longint;Source,Edit:PChar;
                     Target:PChar;TargetLen:longint):longint; cdecl;

external 'DOSCALLS' index 261;

function DosEditName(MetaLevel:longint;const Source,Edit:string;
                     var Target:string):longint;

var T,T2,T3:array[0..255] of char;

begin
    StrPCopy(@T,Source);
    StrPCopy(@T2,Edit);
    DosEditName:=DosEditName(MetaLevel,@T,@T2,@T3,SizeOf(T3));
    Target:=StrPas(@T3);
end;

function DosMove(OldFile,NewFile:PChar):longint; cdecl;

external 'DOSCALLS' index 271;

function DosMove(const OldFile,NewFile:string):longint;

var T,T2:array[0..255] of char;

begin
    StrPCopy(@T,OldFile);
    StrPCopy(@T2,NewFile);
    DosMove:=DosMove(@T,@T2);
end;

function DosCopy(OldFile,NewFile:PChar;Option:longint):longint; cdecl;

external 'DOSCALLS' index 258;

function DosCopy(const OldFile,NewFile:string;Option:longint):longint;

var T,T2:array[0..255] of char;

begin
    StrPCopy(@T,OldFile);
    StrPCopy(@T2,NewFile);
    DosCopy:=DosCopy(@T,@T2,Option);
end;

function DosDelete(FileName:PChar):longint; cdecl;

external 'DOSCALLS' index 259;

function DosDelete(const FileName:string):longint;

var T:array[0..255] of char;

begin
    StrPCopy(@T,FileName);
    DosDelete:=DosDelete(@T);
end;

function DosForceDelete(FileName:PChar):longint; cdecl;

external 'DOSCALLS' index 110;

function DosForceDelete(const FileName:string):longint;

var T:array[0..255] of char;

begin
    StrPCopy(@T,FileName);
    DosForceDelete:=DosForceDelete(@T);
end;

function DosCreateDir(Name:PChar;EA:PEAOp2):longint; cdecl;

external 'DOSCALLS' index 270;

function DosCreateDir(Name:PChar):longint;

begin
    DosCreateDir:=DosCreateDir(Name,nil);
end;

function DosCreateDir(const Name:string;EA:PEAOp2):longint;

var T:array[0..255] of char;

begin
    StrPCopy(@T,Name);
    DosCreateDir:=DosCreateDir(@T,EA);
end;

function DosCreateDir(const Name:string):longint;

var T:array[0..255] of char;

begin
    StrPCopy(@T,Name);
    DosCreateDir:=DosCreateDir(@T,nil);
end;

function DosDeleteDir(Name:PChar):longint; cdecl;

external 'DOSCALLS' index 226;

function DosDeleteDir(const Name:string):longint;

var T:array[0..255] of char;

begin
    StrPCopy(@T,Name);
    DosDeleteDir:=DosDeleteDir(@T);
end;

function DosSetDefaultDisk(DiskNum:longint):longint; cdecl;

external 'DOSCALLS' index 220;

procedure DosQueryCurrentDisk(var DiskNum:longint;var Logical:longint); cdecl;

external 'DOSCALLS' index 275;

function DosSetCurrentDir(Name:PChar):longint; cdecl;

external 'DOSCALLS' index 255;

function DosSetCurrentDir(const Name:string):longint;

var T:array[0..255] of char;

begin
    StrPCopy(@T,Name);
    DosSetCurrentDir:=DosSetCurrentDir(@T);
end;

function DosQueryCurrentDir(DiskNum:longint;var Buffer;
                            var BufLen:longint):longint; cdecl;

external 'DOSCALLS' index 274;

function DosQueryCurrentDir(DiskNum:longint;var Buffer:string):longint;

var T:array[0..255] of char;
    L:longint;

begin
    L:=255;
    DosQueryCurrentDir:=DosQueryCurrentDir(DiskNum,T,L);
    Buffer:=StrPas(@T);
end;

function DosDevIOCtl(Handle,Category,Func:longint;var Params;
                     ParamLen:longint;var ParamSize:longint;
                 var Data;DataLen:longint;var DataSize:longint):longint; cdecl;

external 'DOSCALLS' index 284;

function DosFindFirst(FileMask:PChar;var Handle:longint;Attrib:longint;
                      AFileStatus:PFileStatus;FileStatusLen:longint;
                      var Count:longint;InfoLevel:longint):longint; cdecl;

external 'DOSCALLS' index 264;

function DosFindFirst(const FileMask:string;var Handle:longint;
                      Attrib:longint;AFileStatus:PFileStatus;
                      FileStatusLen:longint;var Count:longint;
                      InfoLevel:longint):longint;

var T:array[0..255] of char;

begin
    StrPCopy(@T,FileMask);
    DosFindFirst:=DosFindFirst(@T,Handle,Attrib,AFileStatus,FileStatusLen,
     Count,InfoLevel);
end;

function DosFindNext(Handle:longint;AFileStatus:PFileStatus;
                     FileStatusLen:longint;var Count:longint):longint; cdecl;

external 'DOSCALLS' index 265;

function DosFindClose(Handle:longint):longint; cdecl;

external 'DOSCALLS' index 263;

function DosQueryFileInfo(Handle,InfoLevel:longint;AFileStatus:PFileStatus;
                          FileStatusLen:longint):longint; cdecl;

external 'DOSCALLS' index 279;

function DosSetFileInfo(Handle,InfoLevel:longint;AFileStatus:PFileStatus;
                        FileStatusLen:longint):longint; cdecl;

external 'DOSCALLS' index 218;

function DosQueryPathInfo(FileName:PChar;InfoLevel:longint;
                 AFileStatus:PFileStatus;FileStatusLen:longint):longint; cdecl;

external 'DOSCALLS' index 223;

function DosQueryPathInfo(const FileName:string;InfoLevel:longint;
                 AFileStatus:PFileStatus;FileStatusLen:longint):longint;

var T:array[0..255] of char;

begin
    StrPCopy(@T,FileName);
    DosQueryPathInfo:=DosQueryPathInfo(@T,InfoLevel,AFileStatus,
     FileStatusLen);
end;

function DosSetPathInfo(FileName:PChar;InfoLevel:longint;
                        AFileStatus:PFileStatus;FileStatusLen,
                        Options:longint):longint; cdecl;

external 'DOSCALLS' index 219;

function DosEnumAttribute(RefType:longint;AFile:pointer;
                          Entry:longint;var Buf;BufSize:longint;
                          var Count:longint;InfoLevel:longint):longint; cdecl;

external 'DOSCALLS' index 372;

function DosEnumAttribute(Handle,Entry:longint;var Buf;BufSize:longint;
                          var Count:longint;InfoLevel:longint):longint;

begin
    DosEnumAttribute:=DosEnumAttribute(0,@Handle,Entry,Buf,BufSize,Count,
     InfoLevel);
end;

function DosEnumAttribute(const FileName:string;
                          Entry:longint;var Buf;BufSize:longint;
                          var Count:longint;InfoLevel:longint):longint;

var T:array[0..255] of char;

begin
    StrPCopy(@T,FileName);
    DosEnumAttribute:=DosEnumAttribute(1,@T,Entry,Buf,BufSize,Count,
     InfoLevel);
end;

function DosScanEnv(Name:PChar;var Value:PChar):longint; cdecl;

external 'DOSCALLS' index 227;

function DosScanEnv(const Name:string;var Value:string):longint;

var T:array[0..255] of char;
    P:PChar;

begin
    StrPCopy(@T,Name);
    DosScanEnv:=DosScanEnv(@T,P);
    Value:=StrPas(P);
end;

function DosSearchPath(Flag:longint;DirList,FileName:PChar;
                       FullName:PChar;FullLen:longint):longint; cdecl;

external 'DOSCALLS' index 228;

function DosSearchPath(Flag:longint;const DirList,FileName:string;
                       var FullName:string):longint;

var T1,T2,T3:array[0..255] of char;

begin
    StrPCopy(@T1,DirList);
    StrPCopy(@T2,FileName);
    DosSearchPath:=DosSearchPath(Flag,@T1,@T2,@T3,SizeOf(T3));
    FullName:=StrPas(@T3);
end;

function DosFSAttach(DevName,FileSystem:PChar;var Data:TAttachData;
                     DataLen,Flag:longint):longint; cdecl;

external 'DOSCALLS' index 269;

function DosFSAttach(const DevName,FileSystem:string;var Data:TAttachData;
                     DataLen,Flag:longint):longint;

var T1,T2:array[0..255] of char;

begin
    StrPCopy(@T1,DevName);
    StrPCopy(@T2,FileSystem);
    DosFSAttach:=DosFSAttach(@T1,@T2,Data,DataLen,Flag);
end;

function DosQueryFSAttach(DevName:PChar;Ordinal,InfoLevel:longint;
                     var Buffer:TFSQBuffer2;var BufLen:longint):longint; cdecl;

external 'DOSCALLS' index 277;

function DosQueryFSAttach(const DevName:string;Ordinal,InfoLevel:longint;
                          var Buffer:TFSQBuffer2;var BufLen:longint):longint;

var T:array[0..255] of char;

begin
    StrPCopy(@T,DevName);
    DosQueryFSAttach:=DosQueryFSAttach(@T,Ordinal,InfoLevel,Buffer,BufLen);
end;

function DosFSCtl(Data:pointer;DataLen:longint;var ResDataLen:longint;
                  Parms:pointer;ParmsLen:longint;var ResParmsLen:longint;
                  _Function:longint;Route:PChar;
                  Handle,Method:longint):longint; cdecl;

external 'DOSCALLS' index 285;

function DosFSCtl(Data:pointer;DataLen:longint;var ResDataLen:longint;
                  Parms:pointer;ParmsLen:longint;var ResParmsLen:longint;
                  _Function:longint;const Route:string;
                  Handle,Method:longint):longint;

var T:array[0..255] of char;

begin
    StrPCopy(@T,Route);
    DosFSCtl:=DosFSCtl(Data,Datalen,ResDataLen,Parms,ParmsLen,ResParmsLen,
     _Function,Route,Handle,Method);
end;

function DosQueryFSInfo(DiskNum,InfoLevel:longint;var Buffer:TFSInfo;
                        BufLen:longint):longint; cdecl;

external 'DOSCALLS' index 278;

function DosSetFSInfo(DiskNum,InfoLevel:longint;var Buffer:TFSInfo;
                      BufLen:longint):longint; cdecl;

external 'DOSCALLS' index 222;

function DosQueryVerify(var Enabled:longint):longint; cdecl;

external 'DOSCALLS' index 225;

function DosSetVerify(Enable:longint):longint; cdecl;

external 'DOSCALLS' index 210;

function DosSetMaxFH(Count:longint):longint; cdecl;

external 'DOSCALLS' index 209;

function DosSetRelMaxFH(var ReqCount,CurMaxFH:longint):longint; cdecl;

external 'DOSCALLS' index 382;

function DosShutDown(Flags:longint):longint; cdecl;

external 'DOSCALLS' index 415;

function DosQuerySysInfo(First,Last:longint;var Buf;BufSize:longint):longint;
                                                                         cdecl;

external 'DOSCALLS' index 348;

function DosPhysicalDisk(Func:longint;Buf:pointer;BufSize:longint;
                         Params:pointer;ParamSize:longint):longint; cdecl;

external 'DOSCALLS' index 287;

function DosAllocMem(var P:pointer;Size,Flag:longint):longint; cdecl;

external 'DOSCALLS' index 299;

function DosFreeMem(P:pointer):longint; cdecl;

external 'DOSCALLS' index 304;

function DosSetMem(P:pointer;Size,Flag:longint):longint; cdecl;

external 'DOSCALLS' index 305;

function DosGiveSharedMem(P:pointer;PID,Flag:longint):longint; cdecl;

external 'DOSCALLS' index 303;

function DosGetSharedMem(P:pointer;Flag:longint):longint; cdecl;

external 'DOSCALLS' index 302;

function DosGetNamedSharedMem(var P:pointer;Name:PChar;Flag:longint):longint;
                                                                         cdecl;

external 'DOSCALLS' index 301;

function DosGetNamedSharedMem(var P:pointer;const Name:string;
                              Flag:longint):longint;

var T:array[0..255] of char;

begin
    StrPCopy(@T,Name);
    DosGetNamedSharedMem:=DosGetNamedSharedMem(P,@T,Flag);
end;

function DosAllocSharedMem(var P:pointer;Name:PChar;Size,Flag:longint):longint;
                                                                         cdecl;

external 'DOSCALLS' index 300;

function DosAllocSharedMem(var P:pointer;const Name:string;
                                                    Size,Flag:longint):longint;

var T:array[0..255] of char;

begin
    if Name<>'' then
        begin
            StrPCopy(@T,Name);
            DosAllocSharedMem:=DosAllocSharedMem(P,@T,Size,Flag);
        end
    else
        DosAllocSharedMem:=DosAllocSharedMem(P,nil,Size,Flag);
end;

function DosQueryMem(P:pointer;var Size,Flag:longint):longint; cdecl;

external 'DOSCALLS' index 306;

function DosSubAllocMem(Base:pointer;var P:pointer;Size:longint):longint;
                                                                         cdecl;

external 'DOSCALLS' index 345;

function DosSubFreeMem(Base,P:pointer;Size:longint):longint; cdecl;

external 'DOSCALLS' index 346;

function DosSubSetMem(Base:pointer;Flag,Size:longint):longint; cdecl;

external 'DOSCALLS' index 344;

function DosSubUnSetMem(Base:pointer):longint; cdecl;

external 'DOSCALLS' index 347;

function DosCreateEventSem(Name:PChar;var Handle:longint;
                           Attr,State:longint):longint; cdecl;

external 'DOSCALLS' index 324;

function DosCreateEventSem(Name:PChar;var Handle:longint;
                           Attr:longint;State:boolean):longint; cdecl;

external 'DOSCALLS' index 324;

function DosCreateEventSem(const Name:string;var Handle:longint;
                           Attr:longint;State:boolean):longint;

var T:array[0..255] of char;

begin
    if Name<>'' then
        begin
            StrPCopy(@T,Name);
            DosCreateEventSem:=DosCreateEventSem(@T,Handle,Attr,State);
        end
    else
        DosCreateEventSem:=DosCreateEventSem(nil,Handle,Attr,State);
end;

function DosCreateEventSem(const Name:string;var Handle:longint;
                           Attr,State:longint):longint;

begin
    DosCreateEventSem:=DosCreateEventSem(Name,Handle,Attr,boolean(State));
end;

function DosOpenEventSem(Name:PChar;var Handle:longint):longint; cdecl;

external 'DOSCALLS' index 325;

function DosOpenEventSem(const Name:string;var Handle:longint):longint;

var T:array[0..255] of char;

begin
    StrPCopy(@T,Name);
    DosOpenEventSem:=DosOpenEventSem(@T,Handle);
end;

function DosCloseEventSem(Handle:longint):longint; cdecl;

external 'DOSCALLS' index 326;

function DosResetEventSem(Handle:longint;var PostCount:longint):longint; cdecl;

external 'DOSCALLS' index 327;

function DosPostEventSem(Handle:longint):longint; cdecl;

external 'DOSCALLS' index 328;

function DosWaitEventSem(Handle,Timeout:longint):longint; cdecl;

external 'DOSCALLS' index 329;

function DosQueryEventSem(Handle:longint;var Posted:longint):longint; cdecl;

external 'DOSCALLS' index 330;

function DosCreateMutExSem(Name:PChar;var Handle:longint;
                           Attr:longint;State:boolean):longint; cdecl;

external 'DOSCALLS' index 331;

function DosCreateMutExSem(Name:PChar;var Handle:longint;
                           Attr,State:longint):longint; cdecl;

external 'DOSCALLS' index 331;

function DosCreateMutExSem(const Name:string;var Handle:longint;
                           Attr:longint;State:boolean):longint;

var T:array[0..255] of char;

begin
    if Name<>'' then
        begin
            StrPCopy(@T,Name);
            DosCreateMutExSem:=DosCreateMutExSem(@T,Handle,Attr,State);
        end
    else
        DosCreateMutExSem:=DosCreateMutExSem(nil,Handle,Attr,State);
end;

function DosCreateMutExSem(const Name:string;var Handle:longint;
                           Attr,State:longint):longint;

begin
    DosCreateMutExSem:=DosCreateMutExSem(Name,Handle,Attr,boolean(State));
end;

function DosOpenMutExSem(Name:PChar;var Handle:longint):longint; cdecl;

external 'DOSCALLS' index 332;

function DosOpenMutExSem(const Name:string;var Handle:longint):longint;

var T:array[0..255] of char;

begin
    StrPCopy(@T,Name);
    DosOpenMutExSem:=DosOpenMutExSem(@T,Handle);
end;

function DosCloseMutExSem(handle:longint):longint; cdecl;

external 'DOSCALLS' index 333;

function DosRequestMutExSem(Handle,Timeout:longint):longint; cdecl;

external 'DOSCALLS' index 334;

function DosReleaseMutExSem(Handle:longint):longint; cdecl;

external 'DOSCALLS' index 335;

function DosQueryMutExSem(Handle:longint;var PID,TID,Count:longint):longint;
                                                                         cdecl;

external 'DOSCALLS' index 336;

function DosCreateMuxWaitSem(Name:PChar;var Handle:longint;CSemRec:longint;
                           var SemArray:TSemArray;Attr:longint):longint; cdecl;

external 'DOSCALLS' index 337;

function DosCreateMuxWaitSem(const Name:string;var Handle:longint;
                             CSemRec:longint;var SemArray:TSemArray;
                             Attr:longint):longint;

var T:array[0..255] of char;

begin
    if Name<>'' then
        begin
            StrPCopy(@T,Name);
            DosCreateMuxWaitSem:=DosCreateMuxWaitSem(@T,Handle,CSemRec,
             SemArray,Attr);
        end
    else
        DosCreateMuxWaitSem:=DosCreateMuxWaitSem(nil,Handle,CSemRec,SemArray,
         Attr);
end;

function DosOpenMuxWaitSem(Name:PChar;var Handle:longint):longint; cdecl;

external 'DOSCALLS' index 338;

function DosOpenMuxWaitSem(const Name:string;var Handle:longint):longint;

var T:array[0..255] of char;

begin
    StrPCopy(@T,Name);
    DosOpenMuxWaitSem:=DosOpenMuxWaitSem(@T,Handle);
end;

function DosCloseMuxWaitSem(Handle:longint):longint; cdecl;

external 'DOSCALLS' index 339;

function DosWaitMuxWaitSem(Handle,Timeout:longint;var User:longint):longint;
                                                                         cdecl;

external 'DOSCALLS' index 340;

function DosAddMuxWaitSem(Handle:longint;var SemRec:TSemRecord):longint; cdecl;

external 'DOSCALLS' index 341;

function DosDeleteMuxWaitSem(Handle,Sem:longint):longint; cdecl;

external 'DOSCALLS' index 342;

function DosQueryMuxWaitSem(Handle:longint;var CSemRec:longint;
                        var SemRecs:TSemArray;var Attr:longint):longint; cdecl;

external 'DOSCALLS' index 343;

function DosGetDateTime(var Buf:TDateTime):longint; cdecl;

external 'DOSCALLS' index 230;

function DosSetDateTime(var Buf:TDateTime):longint; cdecl;

external 'DOSCALLS' index 292;

function DosAsyncTimer(MSec,HSem:longint;
                       var TimHandle:longint):longint; cdecl;

external 'DOSCALLS' index 350;

function DosStartTimer(MSec,HSem:longint;
                       var TimHandle:longint):longint; cdecl;

external 'DOSCALLS' index 351;

function DosStopTimer(TimHandle:longint):longint; cdecl;

external 'DOSCALLS' index 290;

function DosTmrQueryFreq(var Freq:longint):longint; cdecl;

external 'DOSCALLS' index 362;

function DosTmrQueryTime(var Time:comp):longint; cdecl;

external 'DOSCALLS' index 363;

function DosLoadModule(ObjName:PChar;ObjLen:longint;DLLName:PChar;
                       var Handle:longint):longint; cdecl;

external 'DOSCALLS' index 318;

function DosLoadModule(var ObjName:string;ObjLen:longint;
                       const DLLName:string;var Handle:longint):longint;

var T1,T2:array[0..255] of char;

begin
    StrPCopy(@T2,DLLName);
    DosLoadModule:=DosLoadModule(@T1,ObjLen,@T2,Handle);
    ObjName:=StrPas(@T1);
end;

function DosFreeModule(Handle:longint):longint; cdecl;

external 'DOSCALLS' index 322;

function DosQueryProcAddr(Handle,Ordinal:longint;ProcName:PChar;
                          var Address:pointer):longint; cdecl;

external 'DOSCALLS' index 321;

function DosQueryProcAddr(Handle,Ordinal:longint;
                      const ProcName:string;var Address:pointer):longint;

var T1:array[0..255] of char;

begin
    if ProcName<>'' then
        begin
            StrPCopy(@T1,ProcName);
            DosQueryProcAddr:=DosQueryProcAddr(Handle,Ordinal,@T1,Address);
        end
    else
        DosQueryProcAddr:=DosQueryProcAddr(Handle,Ordinal,nil,Address);
end;

function DosQueryModuleHandle(DLLName:PChar;var Handle:longint):longint; cdecl;

external 'DOSCALLS' index 319;

function DosQueryModuleHandle(const DLLName:string;var Handle:longint):longint;

var T1:array[0..255] of char;

begin
    StrPCopy(@T1,DLLName);
    DosQueryModuleHandle:=DosQueryModuleHandle(@T1,Handle);
end;

function DosQueryModuleName(Handle,NameLen:longint;Name:PChar):longint; cdecl;

external 'DOSCALLS' index 320;

{function DosQueryModuleName(Handle:longint;var Name:openstring):longint;

var T1:array[0..255] of char;

begin
    DosQueryModuleName:=DosQueryModuleName(Handle,High(Name),@T1);
    Name:=StrPas(@T1);
end;}

function DosQueryProcType(Handle,Ordinal:longint;Name:PChar;
                          var ProcType:longint):longint; cdecl;

external 'DOSCALLS' index 586;

function DosQueryProcType(Handle,Ordinal:longint;const Name:string;
                          var ProcType:longint):longint;

var T1:array[0..255] of char;

begin
    if Name<>'' then
        begin
            StrPCopy(@T1,Name);
            DosQueryProcType:=DosQueryProcType(Handle,Ordinal,@T1,ProcType);
        end
    else
        DosQueryProcType:=DosQueryProcType(Handle,Ordinal,nil,ProcType);
end;

function DosGetResource(Handle,ResType,ResName:longint;var P:pointer):longint;
                                                                         cdecl;

external 'DOSCALLS' index 352;

function DosFreeResource(P:pointer):longint; cdecl;

external 'DOSCALLS' index 353;

function DosQueryResourceSize(Handle,IDT,IDN:longint;var Size:longint):longint;
                                                                         cdecl;

external 'DOSCALLS' index 572;

function DosQueryCtryInfo(Size:longint;var Country:TCountryCode;
                   var Res:TCountryInfo;var ActualSize:longint):longint; cdecl;

external 'NLS' index 5;

function DosQueryDBCSEnv(Size:longint;var Country:TCountryCode;
                                                     Buf:PChar):longint; cdecl;

external 'NLS' index 6;

function DosMapCase(Size:longint;var Country:TCountryCode;
                    AString:PChar):longint; cdecl;

external 'NLS' index 7;

function DosMapCase(var Country:TCountryCode;var AString:string):longint;

var T1:string;

begin
    StrPCopy(@T1,AString);
    DosMapCase:=DosMapCase(length(AString),Country,@T1);
    AString:=StrPas(@T1);
end;

function DosQueryCollate(Size:longint;var Country:TCountryCode;
                         buf:PByteArray;var TableLen:longint):longint; cdecl;

external 'NLS' index 8;

function DosQueryCP(Size:longint;CodePages:PWordArray;
                                           var ActSize:longint):longint; cdecl;

external 'DOSCALLS' index 291;

function DosSetProcessCP(CP:longint):longint; cdecl;

external 'DOSCALLS' index 289;

function DosSetExceptionHandler(var RegRec:TExceptionRegistrationRecord
                                ):longint; cdecl;

external 'DOSCALLS' index 354;

function DosUnsetExceptionHandler(var RegRec:TExceptionRegistrationRecord
                                  ):longint; cdecl;

external 'DOSCALLS' index 355;

function DosRaiseException(var Excpt:TExceptionReportRecord):longint; cdecl;

external 'DOSCALLS' index 356;

function DosSendSignalException(PID,Exception:longint):longint; cdecl;

external 'DOSCALLS' index 379;

function DosUnwindException(var Handler:TExceptionRegistrationRecord;
                            TargetIP:pointer;
                            var RepRec:TExceptionReportRecord):longint; cdecl;

external 'DOSCALLS' index 357;

function DosSetSignalExceptionFocus(Enable:longint;var Times:longint):longint;
                                                                         cdecl;

external 'DOSCALLS' index 378;

function DosEnterMustComplete(var Nesting:longint):longint; cdecl;

external 'DOSCALLS' index 380;

function DosExitMustComplete(var Nesting:longint):longint; cdecl;

external 'DOSCALLS' index 381;

function DosAcknowledgeSignalException(SignalNum:longint):longint; cdecl;

external 'DOSCALLS' index 418;

function DosCloseQueue(Handle:longint):longint; cdecl;

external 'QUECALLS' index 11;

function DosCreateQueue(var Handle:longint;Priority:longint;
                        Name:PChar):longint; cdecl;

external 'QUECALLS' index 16;

function DosCreateQueue(var Handle:longint;Priority:longint;
                        const Name:string):longint;

var T1:array[0..255] of char;

begin
    StrPCopy(@T1,Name);
    DosCreateQueue:=DosCreateQueue(Handle,Priority,@T1);
end;

function DosOpenQueue(var Parent_PID:longint;var Handle:longint;
                      Name:PChar):longint; cdecl;

external 'QUECALLS' index 15;

function DosOpenQueue(var Parent_PID:longint;var Handle:longint;
                      const Name:string):longint;

var T1:array[0..255] of char;

begin
    StrPCopy(@T1,Name);
    DosOpenQueue:=DosOpenQueue(Parent_PID,Handle,@T1);
end;

function DosPeekQueue(Handle:longint;var ReqBuffer:TRequestData;
                      var DataLen:longint;var DataPtr:pointer;
                      var Element:longint;Wait:longint;
                      var Priority:byte;ASem:longint):longint; cdecl;

external 'QUECALLS' index 13;

function DosPurgeQueue(Handle:longint):longint; cdecl;

external 'QUECALLS' index 10;

function DosQueryQueue(Handle:longint;var Count:longint):longint; cdecl;

external 'QUECALLS' index 12;

function DosReadQueue(Handle:longint;var ReqBuffer:TRequestData;
                      var DataLen:longint;var DataPtr:pointer;
                      Element,Wait:longint;var Priority:byte;
                      ASem:longint):longint; cdecl;

external 'QUECALLS' index 9;

function DosWriteQueue(Handle,Request,DataLen:longint;var DataBuf;
                       Priority:longint):longint; cdecl;

external 'QUECALLS' index 14;

function DosError(Error:longint):longint; cdecl;

external 'DOSCALLS' index 212;

procedure DosErrClass(Code:longint;var _Class,Action,Locus:longint); cdecl;

external 'DOSCALLS' index 211;

function DosTrueGetMessage(MsgSeg:pointer;Table:PInsertTable;
                           TableSize:longint;Buf:PChar;BufSize,
                           MsgNumber:longint;FileName:PChar;
                           var MsgSize:longint):longint; cdecl;

external 'MSG' index 6;

function DosIQueryMessageCP(var Buf;BufSize:longint;FileName:PChar;
                           var InfoSize:longint;MesSeg:pointer):longint; cdecl;

external 'MSG' index 8;

procedure MagicHeaderEnd; assembler; forward;

{$ASMMODE INTEL}

{start of _MSGSEG32 segment}
procedure MagicHeaderStart; assembler;               
asm
  db $0FF
  db $4D,$53,$47,$53,$45,$47,$33,$32, 0       //'MSGSEG32'
  dd $8001
  dd MAGICHEADEREND
end;
    
function DosGetMessage(Table:PInsertTable;TableSize:longint;Buf:PChar;
                       BufSize,MsgNumber:longint;FileName:PChar;
                       var MsgSize:longint):longint;
begin
    DosGetMessage := DosTrueGetMessage(@MagicHeaderStart,Table,TableSize,
                                 Buf,BufSize,msgnumber,filename,msgsize);
end;

function DosQueryMessageCP(var Buf;BufSize:longint;FileName:PChar;
                            var InfoSize:longint):longint;
begin
    DosQueryMessageCP := DosIQueryMessageCP(Buf, BufSize, FileName, InfoSize,
                                                            @MagicHeaderStart);
end;

procedure MagicHeaderEnd; assembler;
asm
  dd $0FFFF0000
end;
{$ASMMODE DEFAULT}

(*function DosGetMessage(const Table:array of PString;var Buf:openstring;
                        MsgNumber:longint;const FileName:string):longint;

{Hmm. This takes too much stackspace. Let's use the
 heap instead.}

type    TTableBuffer=record
            IT:TinsertTable;
            Strings:TByteArray;
        end;
        PTableBuffer=^TTableBuffer;

var Buffer:PTableBuffer;
    I,S:word;
    BufPtr:pointer;
    T1,T2:array[0..255] of char;

begin
    {Check if there are more than nine items in the table.}
    if High(Table)>8 then
        DosGetMessage:=87
    else
        begin
            {Step 1: Calculate the space we need on the heap.}
            S:=SizeOf(TInsertTable);
            for I:=Low(Table) to High(Table) do
                S:=S+Length(Table[I])+1;

            {Step 2: Allocate the buffer.}
            GetMem(Buffer,S);

            {Step 3: Fill the buffer.}
            BufPtr:=@(S^.Strings);
            for I:=Low(Table) to High(Table) do
                begin
                    S^.IT[I+1]:=bufptr;
                    StrPCopy(BufPtr,Table[I]);
                    Inc(longint(BufPtr),Length(Table[I])+1);
                end;

            {Step 4: Convert the filename.}
            StrPCopy(@T2,FileName);

            {Step 5: Get the message.}
            DosGetMessage:=DosGetMessage(@(S^.IT),High(Table)+1,@T1,
             High(Buf),MsgNumber,@T2,I);

            {Step 6: Convert the returned message.}
            Buf[0]:=Char(I);
            Move(T1,Buf[1],I);

            {Step 7: Free the memory.}
            FreeMem(Buffer,S);
        end;
end;*)

{function DosGetMessage(const Table:array of PString;Buf:PChar;
                       BufSize,MsgNumber:longint;const FileName:string;
                       MsgSize:longint):longint;}

function DosQueryMessageCP(var Buf;BufSize:longint;const FileName:string;
                           var InfoSize:longint):longint;

var T:array[0..255] of char;

begin
    StrPCopy(@T,FileName);
    DosQueryMessageCP:=DosQueryMessageCP(Buf,BufSize,@T,InfoSize);
end;

function DosInsertMessage(Table:PInsertTable;TableSize:longint;
                          Message:PChar;SrcMessageSize:longint;
                          Buf:PChar;BufSize:longint;
                          var DstMessageSize:longint):longint; cdecl;

external 'MSG' index 4;

{function DosInsertMessage(Table:array of PString;
                          const Message:string;
                          var Buf:openstring):longint;

function DosInsertMessage(Table:array of PString;
                          Message:PChar;SrcMessageSize:longint;
                          Buf:PChar;BufSize:longint;
                          var DstMessageSize:longint):longint;}

function DosPutMessage(Handle,Size:longint;Buf:PChar):longint; cdecl;

external 'MSG' index 5;

function DosPutMessage(Handle:longint;const Buf:string):longint;

begin
    DosPutMessage:=DosPutMessage(Handle,Length(Buf),@Buf[1]);
end;

function DosStartSession(const AStartData:TStartData;
                         var SesID,PID:longint):longint; cdecl;

external 'SESMGR' index 37;

function DosSetSession(SesID:longint;const AStatus:TStatusData):longint; cdecl;

external 'SESMGR' index 39;

function DosSelectSession(SesID:longint):longint; cdecl;

external 'SESMGR' index 38;

function DosStopSession(Scope,SesID:longint):longint; cdecl;

external 'SESMGR' index 40;

function DosCreatePipe(var ReadHandle,WriteHandle:longint;
                       Size:longint):longint; cdecl;

external 'DOSCALLS' index 239;

function DosCreateNPipe(Name:PChar;var Handle:longint;OpenMode,PipeMode,
                        OutBufSize,InBufSize,MSec:longint):longint; cdecl;

external 'DOSCALLS' index 243;

function DosCreateNPipe(const Name:string;var Handle:longint;OpenMode,
                        PipeMode,OutBufSize,InBufSize,MSec:longint):longint;

var T:array[0..255] of char;

begin
    StrPCopy(@T,Name);
    DosCreateNPipe:=DosCreateNPipe(@T,Handle,OpenMode,PipeMode,OutBufSize,
     InBufSize,MSec);
end;

function DosCallNPipe(Name:PChar;var Input;InputSize:longint;
                      var Output;OutputSize:longint;var ReadBytes:longint;
                      MSec:longint):longint; cdecl;

external 'DOSCALLS' index 240;

function DosCallNPipe(const Name:string;var Input;InputSize:longint;
                      var Output;OutputSize:longint;var ReadBytes:longint;
                      MSec:longint):longint;

var T:array[0..255] of char;

begin
    StrPCopy(@T,Name);
    DosCallNPipe:=DosCallNPipe(@T,Input,InputSize,Output,OutputSize,
     ReadBytes,MSec);
end;

function DosConnectNPipe(Handle:longint):longint; cdecl;

external 'DOSCALLS' index 241;

function DosDisconnectNPipe(Handle:longint):longint; cdecl;

external 'DOSCALLS' index 242;

function DosPeekNPipe(Handle:longint;var Buffer;BufSize:longint;
                      var ReadBytes:longint;var Avail:TAvailData;
                      var State:longint):longint; cdecl;

external 'DOSCALLS' index 244;

function DosQueryNPHState(Handle:longint;var State:longint):longint; cdecl;

external 'DOSCALLS' index 245;

function DosQueryNPipeInfo(Handle,InfoLevel:longint;var Buffer;
                           BufSize:longint):longint; cdecl;

external 'DOSCALLS' index 248;

function DosQueryNPipeSemState(SemHandle:longint;var SemArray;
                               BufSize:longint):longint; cdecl;

external 'DOSCALLS' index 249;

function DosSetNPHState(Handle,State:longint):longint; cdecl;

external 'DOSCALLS' index 250;

function DosSetNPipeSem(PipeHandle,SemHandle,Key:longint):longint; cdecl;

external 'DOSCALLS' index 251;

function DosTransactNPipe(Handle:longint;var OutBuf;OutSize:longint;
                          var InBuf;InSize:longint;
                          var ReadBytes:longint):longint; cdecl;

external 'DOSCALLS' index 252;

function DosWaitNPipe(Name:PChar;MSec:longint):longint; cdecl;

external 'DOSCALLS' index 253;

function DosWaitNPipe(const Name:string;MSec:longint):longint;

var T:array[0..255] of char;

begin
    StrPCopy(@T,Name);
    DosWaitNPipe:=DosWaitNPipe(@T,MSec);
end;

function DosOpenVDD(Name:PChar;var Handle:longint):longint; cdecl;

external 'DOSCALLS' index 308;

function DosRequestVDD(Handle,SGroup,Cmd:longint;
                       InSize:longint;var InBuffer;
                       OutSize:longint;var OutBuffer):longint; cdecl;

external 'DOSCALLS' index 309;

function DosCloseVDD(Handle:longint):longint; cdecl;

external 'DOSCALLS' index 310;

procedure DosSelToFlat; cdecl;

external 'DOSCALLS' index 426;

procedure DosFlatToSel; cdecl;

external 'DOSCALLS' index 425;

{$ASMMODE INTEL}
function SelToFlat (AFarPtr: TFarPtr): pointer; assembler;
 asm
  mov eax, AFarPtr
  call DosSelToFlat
 end;

function FlatToSel (APtr: pointer): cardinal; assembler;
 asm
  mov eax, APtr
  call DosFlatToSel
 end;

function DosAllocThreadLocalMemory (Count: cardinal; var P: pointer): longint;
                                                                         cdecl;

external 'DOSCALLS' index 454;

function DosFreeThreadLocalMemory (P: pointer): longint; cdecl;

external 'DOSCALLS' index 455;

(* Todo:

function DosRawReadNPipe ...; cdecl;

external 'DOSCALLS' index 246;

function DosRawWriteNPipe ...; cdecl;

external 'DOSCALLS' index 247;

function DosSetCP ...; cdecl;

external 'DOSCALLS' index 288;

function DosDynamicTrace ...; cdecl;

external 'DOSCALLS' index 316;

function DosRegisterPerfCtrs ...; cdecl;

external 'DOSCALLS' index 367;

function DosQueryDOSProperty ...; cdecl;

external 'DOSCALLS' index 373;

function DosSetDOSProperty ...; cdecl;

external 'DOSCALLS' index 374;

function DosProfile ...; cdecl;

external 'DOSCALLS' index 377;

function DosReplaceModule ...; cdecl;

external 'DOSCALLS' index 417;

function DosTIB ...; cdecl;

external 'DOSCALLS' index 419;

function DosOpenChangeNotify ...; cdecl;

external 'DOSCALLS' index 440;

function DosResetChangeNotify ...; cdecl;

external 'DOSCALLS' index 441;

function DosCloseChangeNotify ...; cdecl;

external 'DOSCALLS' index 442;

function DosInitializePorthole ...; cdecl;

external 'DOSCALLS' index 580;

function DosQueryHeaderInfo ...; cdecl;

external 'DOSCALLS' index 582;

*)


end.
{
  $Log$
  Revision 1.14  2002-10-05 19:09:57  hajny
    * code2.as + code3.as not needed any more

  Revision 1.13  2002/09/22 18:44:13  hajny
    * Compatibilty mode for DateTime fields

  Revision 1.12  2002/09/07 16:01:24  peter
    * old logs removed and tabs fixed

  Revision 1.11  2002/07/07 18:03:22  hajny
    * 1st part of corrections/additions by Yuri Prokushev

}
