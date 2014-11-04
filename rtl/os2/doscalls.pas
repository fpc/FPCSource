{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2014 by the Free Pascal development team.

    Basic OS/2 constants, types and functions implemented (mostly)
    in DOSCALL1.DLL. Only functions available in all 32-bit OS/2
    versions (i.e. starting with OS/2 2.0) are included here
    to make sure that programs using this unit could still run on
    these old versions. Certain functions supported in later versions
    which could be emulated on older versions are provided in unit
    DosCall2 (using dynamic loading of the respective entry points).

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit DosCalls;

{***************************************************************************}
interface
{***************************************************************************}

uses    Strings;

type    PString = PShortString;

{$PACKRECORDS 1}

type    TByteArray=array[0..$fff0] of byte;
        PByteArray=^TByteArray;
        TCharArray=array[0..$fff0] of char;
        PCharArray=^TCharArray;
        TWordArray=array[0..$7ff8] of word;
        PWordArray=^TWordArray;

{****************************************************************************
                            Thread related routines.
****************************************************************************}

type    TThreadEntry = function (Param: pointer): cardinal; cdecl;
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
function DosCreateThread(var TID:longint;Address:TThreadEntry;
                        AParam:pointer;Flags,StackSize:longint):cardinal;cdecl;
function DosCreateThread (var TID: cardinal; Address: TThreadEntry;
                 AParam: pointer; Flags, StackSize: cardinal): cardinal; cdecl;

(* Overloaded version for compatibility. *)
function DosCreateThread(var TID:longint;Address:pointer;
                        AParam:Pointer;Flags,StackSize:longint):cardinal;cdecl;
function DosCreateThread (var TID: cardinal; Address: pointer;
                 AParam: Pointer; Flags, StackSize: cardinal): cardinal; cdecl;


{Suspend a running thread.}
function DosSuspendThread(TID:cardinal):cardinal; cdecl;

{Resume a suspended thread.}
function DosResumeThread(TID:cardinal):cardinal; cdecl;

{Terminate a specific thread.}
function DosKillThread(TID:cardinal):cardinal; cdecl;

{Wait until a specific thread has ended.
 TID            = Thread to terminate. Can also be zero. In that case we
                  wait until the next thread terminates. Its thread ID is
                  returned back.
 Option         = Flags. Either dtWait or dtNoWait.}
function DosWaitThread(var TID:longint;Option:longint):cardinal; cdecl;
function DosWaitThread(var TID:cardinal;Option:cardinal):cardinal; cdecl;

{All other threads in the same process are suspended until a
DosExitCritSec.}
function DosEnterCritSec: cardinal; cdecl;

{Resume the other threads again.}
function DosExitCritSec: cardinal; cdecl;

{ DosExit codes }
const   deThread=0;         {Terminate thread only.}
        deProcess=1;        {Terminate the whole process.}
(* The following for compatibility only *)
        Exit_Thread = deThread;
        Exit_Process = deProcess;

{Terminate the thread or the program. Never returns, so it's defined as
 procedure.}
procedure DosExit(Action,Result:cardinal); cdecl;

type    PThreadInfoBlock=^TThreadInfoBlock;
        PPThreadInfoBlock=^PThreadInfoBlock;
        PSysThreadIB=^TSysThreadIB;
        PProcessInfoBlock=^TProcessInfoBlock;
        PPProcessInfoBlock=^PProcessInfoBlock;

        TThreadInfoBlock=record
            Exh_Chain,              {Head of exception handler chain.}
            Stack,                  {Pointer to the thread's stack.}
            StackLimit:pointer;     {Pointer to the thread's stack-end.}
            TIB2:PSysThreadIB;      {Pointer to system specific thread info.}
            Version,                {Version of this datastructure.}
            Ordinal:cardinal;       {Thread ordinal number.}
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
            Version:cardinal;       {Version of this datastructure.}
            MCCount,                {Must complete count. ??? Info wanted!}
            MCForceFlag:word;       {Must complete force flag. Info wanted!}
        end;
        SysThreadIB=TSysThreadIB;

        TProcessInfoBlock=record
            PID,                    {Process ID.}
            ParentPID,              {Parent's process ID.}
            HMTE:cardinal;          {Module handle of executable program.
                                     ??? Info wanted!}
            Cmd,                    {Command line options.}
            Env:PByteArray;         {Environment strings.}
            flStatus,               {1 means that the process is in exit list
                                     processing.}
            tType:cardinal;         {Type of process:
                                        0:  Full screen protected mode.
                                        1:  DOS emulation.
                                        2:  Windowable full screen protected
                                            mode program.
                                        3:  Presentation manager program.
                                        4:  Detached mode process.}
        end;
        ProcessInfoBlock=TProcessInfoBlock;

{OS/2 keeps information about the current process and the current thread
 is the datastructures TProcessInfoBlock and TThreadInfoBlock. All data
 can both be read and be changed. Use DosGetInfoBlocks to get their
 address. The service cannot fail, so it is defined as procedure. The
 second version of the call might be useful if you only want address of one
 of those datastructures, since you can supply nil for the other parameter
 then - beware, omitting one of these parameters (passing nil) is only
 supported on newer OS/2 versions, and causes SIGSEGV on e.g. OS/2 v2.1!!!}

procedure DosGetInfoBlocks(var ATIB:PThreadInfoBlock;
                           var APIB:PProcessInfoBlock); cdecl;
procedure DosGetInfoBlocks(PATIB:PPThreadInfoBlock;
                           PAPIB:PPProcessInfoBlock); cdecl;

{Wait a number of milliseconds. Cannot fail, so it is defined as procedure.}
procedure DosSleep (MSec:cardinal); cdecl;

{Beep the speaker. You do not need to check for an error if you can
 guarantee that the frequency is correct.}
function DosBeep(Freq,MS:cardinal):cardinal; cdecl;

{****************************************************************************

                        Process handling routines.

****************************************************************************}

{ User's Debug Buffer structure }

type    PDbgBuf = ^TDbgBuf;
        TDbgBuf = record
            Pid: cardinal;             { Debuggee Process id          }
            Tid: cardinal;             { Debuggee Thread id           }
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
function DosDebug (DebugBuf: PDbgBuf):cardinal; cdecl;

function DosDebug (var APDbgBuf: TDbgBuf): cardinal; cdecl;

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

type    TExitProc=procedure(Reason:cardinal); cdecl;

{Add/remove an exitprocedure to the exit list. Also used to terminate an
 exit procedure. An exit procedure will be called on exiting of the program.

    OrderCode       = One of the EXLST_XXXX constants.
    Proc            = Address of the exit procedure.

An exit procedure is called with one of the TC_XXXX constants. When it is
done it must call DosExitList with ExLst_Exit.

Exit procedures are called in random order.}
function DosExitList(OrderCode:cardinal;Proc:TExitProc):cardinal; cdecl;

{ DosExecPgm options }
const   deSync          = 0;    {Wait until program terminates.}
        deAsync         = 1;    {Do not wait.}
        deAsyncResult   = 2;    {Do not wait. DosWaitChild will follow to
                                 check if process has been terminated. If
                                 you use this, you must use DosWaitChild,
                                 because OS/2 will not free memory that is
                                 allocated for the result codes if you don't.}
        deTrace         = 3;    {For debugging.}
        deBackground    = 4;    {Do not run as child. Run in a separate
                                 session.}
        deSuspended     = 5;    {Child will be loaded, but not executed.}
        deAsyncResultDb = 6;    {Similar to deAsyncResult, but for debugging.}

(* The following for compatibility only *)
        EXEC_SYNC          =deSync;
        EXEC_ASYNC         =deAsync;
        EXEC_ASYNCRESULT   =deAsyncResult;
        EXEC_TRACE         =deTrace;
        EXEC_BACKGROUND    =deBackground;
        EXEC_LOAD          =deSuspended;
        EXEC_ASYNCRESULTDB =deAsyncResultDb;

type    TResultCodes=record
         case byte of
          0: (
            TerminateReason,        {0 = Normal termination.
                                     1 = Critical error.
                                     2 = Trapped. (GPE, etc.)
                                     3 = Killed by DosKillProcess.}
            ExitCode:cardinal);     {Exit code of child.}
          1: (CodeTerminate: cardinal); {For compatibility only}
          2: (PID: cardinal);  {Process ID returned for asynchronous execution}
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
                  nil is also allowed meaning inherit parent's environment.
 FileName       = Filename with full path and extension. Is not sensitive
                  for the PATH environment variable.}
function DosExecPgm(ObjName:PChar;ObjLen:longint;ExecFlag:cardinal;
                    Args,Env:PByteArray;var Res:TResultCodes;
                    FileName:PChar):cardinal; cdecl;
function DosExecPgm(var ObjName:string;ExecFlag:cardinal;
                    Args,Env:PByteArray;var Res:TResultCodes;
                    const FileName:string):cardinal;

const
  DCWA_PROCESS     = 0;
  DCWA_PROCESSTREE = 1;

{Wait until a child process terminated. Sometimes called DosCWait.

Action              = 0 = Wait until child terminates (DCWA_PROCESS).
                      1 = Wait until child and all its childs terminate
                      (DCWA_PROCESSTREE).
Option              = Flags. Either dtWait or dtNoWait.
Res                 = See TResultCodes.
TermPID             = Process ID that has been terminated. Usefull when
                      terminating a random process.
PID                 = Process ID of process to terminate. Use a zero to
                      terminate a random process.}
function DosWaitChild(Action,Option:longint;var Res:TResultCodes;
                      var TermPID:longint;PID:longint):cardinal; cdecl;
function DosWaitChild(Action,Option:cardinal;var Res:TResultCodes;
                      var TermPID:cardinal;PID:cardinal):cardinal; cdecl;

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
                      the range 0..31 (Delta itself must be within -31..31).
 PortID             = Process ID when Scope=0 or 1, thread ID when Scope=2.}
function DosSetPriority(Scope,TrClass: cardinal;Delta:longint;PortID:cardinal):cardinal; cdecl;

{Terminate a process. If the process isn't a child process, it can refuse
 to terminate.

 Action             = 0 = Terminate process and all its childs.
                      1 = Terminate process only.
 PID                = Process ID of process to terminate.}
function DosKillProcess(Action,PID:cardinal):cardinal; cdecl;

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
function DosQueryAppType(FileName:PChar;var Flags:longint):cardinal; cdecl;
function DosQueryAppType (FileName: PChar; var Flags: cardinal): cardinal;
                                                                         cdecl;

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
function DosDevConfig(var DevInfo:byte;Item:cardinal):cardinal; cdecl;

{****************************************************************************

                        File handling related routines.

****************************************************************************}

const   MaxPathLength=260;
        MaxPathComponent=256;

type    TFileLock=record
         case boolean of
          false: (Offset, Range: longint);
          true:  (lOffset, lRange: longint);
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
function DosSetFileLocks(Handle:THandle;var Unlock,Lock:TFileLock;
                         Timeout,Flags: cardinal):cardinal; cdecl;

{Cancel a filelock area.

Handle  = File handle.
Lock    = Area that is locked now.}
function DosCancelLockRequest(Handle:THandle;var Lock:TFileLock):cardinal;
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

type    TgEA = record
            case byte of
                1: (NameLen: byte;
                    Name: array [0..0] of char);
                2: (cbName: byte;         { name length not including NULL }
                    szName: char);        { attribute name }
        end;
        PgEA = ^TgEA;
        GEA = TgEA;

        TgEAList = record
            ListLen: cardinal; { total bytes of structure including full list }
            List: array [0..0] of TgEA;      { variable length GEA structures }
        end;
        PgEAList = ^TgEAList;
        GEAList = TgEAList;

        TfEA = record
            case byte of
                1: (EA,
                    NameLen: byte;
                    Value: word);
                2: (fEA: byte;          { flags                          }
                    cbName: byte;       { name length not including NULL }
                    cbValue: word);     { value length }
        end;
        PfEA = ^TfEA;
        FEA=TfEA;

        TfEAList = record
            Size: cardinal; { total bytes of structure including full list }
            List: array [0..0] of TfEA;   { variable length FEA structures }
        end;
        PfEAList = ^TfEAlist;
        FEAList = TfEAList;

        TEAOp = record
            case byte of
                1: (gEAList: PgEAList;
                    fEAList: PfEAList;
                    Error: cardinal);
                2: (fpGEAList: PGEAList;    { general EA list }
                    fpFEAList: PFEAList;    { full EA list }
                    oError: cardinal);
        end;
        PEAOp = ^TEAOp;
        EAOp = TEAOp;

        TfEA2 = record
            NextEntry: cardinal;
            Flags,
            NameLen: byte;
            Value: word;
            szName: array [0..0] of char;
        end;
        PfEA2 = ^TfEA2;
        FEA2 = TfEA2;

        TfEA2List = record
            ListLen: cardinal;
            List: array [0..0] of TfEA2;
        end;
        PfEA2List = ^TfEA2List;
        FEA2List = TfEA2List;

        TgEA2 = record
            case byte of
                1: (NextEntry: cardinal;
                    NameLen: byte;
                    Name: array [0..0] of char);
                2: (oNextEntryOffset: cardinal;     { new field }
                    cbName: byte;
                    szName: array [0..0] of byte);  { new field }
        end;
        PgEA2 = ^TgEA2;
        GEA2 = TgEA2;

        TgEA2list = record
          ListLen: cardinal;
          List: array [0..0] of TgEA2;
        end;
        PgEA2List = ^TgEA2List;
        GEA2List = TgEAList;

        TEAOp2 = record
            case byte of
                1: (gEA2List: PgEA2List;
                    fEA2List: PfEA2List;
                    Error: cardinal);
                2: (fpGEA2List: PGEA2List;      { GEA set }
                    fpFEA2List: PFEA2List;      { FEA set }
                    oError: cardinal);          { offset of FEA error }
        end;
        PEAOp2 = ^TEAOp2;
        EAOp2 = TEAOp2;

        TEASizeBuf = record     { struct for FSCTL fn 2 - max ea size }
            case byte of
                1: (MaxEASize: word;
                    MaxEAListSize: cardinal);
                2: (cbMaxEASize: word;         { max size of one EA }
                    cbMaxEAListSize: cardinal);{ max size of the full EA list }
        end;
        PEASizeBuf = ^TEASizeBuf;
        EASizeBuf = TEASizeBuf;


{*******************End of extented attribute datastructures.***************}

{Usefull constanst for Action parameter.}
   { DosOpen() actions }
const       doOpened        =  1;
            doCreated       =  2;
            doOverwritten   =  3;

            FILE_EXISTED    =  doOpened;
            FILE_CREATED    =  doCreated;
            FILE_TRUNCATED  =  doOverwritten;

{Usefull constants for OpenFlags parameter.}
   { DosOpen() open flags }
const       doFail          =  0;
            doOpen          =  1;
            doOverwrite     =  2;
            (*
                fixed by KO M.H. on 1999.07.04
                contents : Creation flags is 10 hex not 10 dec.
            *)
            doCreate        = 16;

            FILE_OPEN       = doOpen;
            FILE_TRUNCATE   = doOverwrite;
            FILE_CREATE     = doCreate;

{    this nibble applies if file already exists                      xxxx }
      OPEN_ACTION_FAIL_IF_EXISTS     =doFail;       { ---- ---- ---- 0000 }
      OPEN_ACTION_OPEN_IF_EXISTS     =doOpen;       { ---- ---- ---- 0001 }
      OPEN_ACTION_REPLACE_IF_EXISTS  =doOverwrite;  { ---- ---- ---- 0010 }

{    this nibble applies if file does not exist              xxxx      }
      OPEN_ACTION_FAIL_IF_NEW        =doFail;    { ---- ---- 0000 ---- }
      OPEN_ACTION_CREATE_IF_NEW      =DoCreate;  { ---- ---- 0001 ---- }

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


   { DosOpen/DosSetFHandState flags }
      OPEN_ACCESS_READONLY           =doRead;       { ---- ---- ---- -000 }
      OPEN_ACCESS_WRITEONLY          =doWrite;      { ---- ---- ---- -001 }
      OPEN_ACCESS_READWRITE          =doReadWrite;  { ---- ---- ---- -010 }
      OPEN_SHARE_DENYREADWRITE       =doDenyRW;     { ---- ---- -001 ---- }
      OPEN_SHARE_DENYWRITE           =doDenyWrite;  { ---- ---- -010 ---- }
      OPEN_SHARE_DENYREAD            =doDenyRead;   { ---- ---- -011 ---- }
      OPEN_SHARE_DENYNONE            =doDenyNone;   { ---- ---- -100 ---- }
      OPEN_FLAGS_NOINHERIT           =doNoInherit;  { ---- ---- 1--- ---- }
      OPEN_FLAGS_NO_LOCALITY         =$0000;        { ---- -000 ---- ---- }
      OPEN_FLAGS_SEQUENTIAL          =doSequential; { ---- -001 ---- ---- }
      OPEN_FLAGS_RANDOM              =doRandom;     { ---- -010 ---- ---- }
      OPEN_FLAGS_RANDOMSEQUENTIAL    =doSequential
                                   or doRandom;     { ---- -011 ---- ---- }
      OPEN_FLAGS_NO_CACHE            =doNoCache;    { ---1 ---- ---- ---- }
      OPEN_FLAGS_FAIL_ON_ERROR       =doFailOnErr;  { --1- ---- ---- ---- }
      OPEN_FLAGS_WRITE_THROUGH       =doWriteThru;  { -1-- ---- ---- ---- }
      OPEN_FLAGS_DASD                =doDASD;       { 1--- ---- ---- ---- }

      OPEN_FLAGS_NONSPOOLED          =$00040000;
      OPEN_FLAGS_PROTECTED_HANDLE    =$40000000;


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
function DosOpen(FileName:PChar;var Handle: longint; var Action:longint;
                 InitSize:longint;Attrib,OpenFlags,FileMode:longint;
                 EA:PEAOp2):cardinal; cdecl;
function DosOpen(FileName:PChar;var Handle: THandle;var Action:cardinal;
                 InitSize,Attrib,OpenFlags,FileMode:cardinal;
                 EA:PEAOp2):cardinal; cdecl;
{This variant of DosOpen always creates or overwrites a file.}
function DosCreate(FileName:PChar;var Handle: THandle;
                   Attrib,OpenMode:cardinal):cardinal;
{This variant of DosOpen always opens an existing file.}
function DosOpen(FileName:PChar;var Handle: THandle;
                 Attrib,OpenMode:cardinal):cardinal;
{There are also string variants.}
function DosOpen(const FileName:string;var Handle: longint; var Action:longint;
                 InitSize,Attrib,OpenFlags,OpenMode:longint;
                 ea:PEAOp2):cardinal;
function DosOpen(const FileName:string;var Handle: THandle;var Action:cardinal;
                 InitSize,Attrib,OpenFlags,OpenMode:cardinal;
                 ea:PEAOp2):cardinal;
function DosCreate(const FileName:string;var Handle: THandle;
                   Attrib,OpenMode:cardinal):cardinal;
function DosOpen(const FileName:string;var Handle: THandle;
                 Attrib,OpenMode:cardinal):cardinal;

{Close a file.
Cannot fail if handle does exist.}
function DosClose (Handle: THandle): cardinal; cdecl;

{Read from a file or other type of handle.

    Handle      = File handle.
    Buffer      = The read data is stored here.
    Count       = Number of bytes to read.
    ActCount    = Number of bytes actually read.}
function DosRead (Handle: longint; var Buffer; Count: longint;
                 var ActCount:longint):cardinal; cdecl;

function DosRead (Handle: THandle; var Buffer; Count: cardinal;
                 var ActCount:cardinal):cardinal; cdecl;

{Write to a file or other type of handle.

    Handle      = File handle.
    Buffer      = The data to be written.
    Count       = Number of bytes to write.
    ActCount    = Number of bytes actually written.}
function DosWrite (Handle: longint; const Buffer; Count: longint;
                  var ActCount:longint):cardinal; cdecl;

function DosWrite (Handle: THandle; const Buffer; Count: cardinal;
                  var ActCount:cardinal):cardinal; cdecl;

const   dsZeroBased=0;      {Set filepointer from begin of file.}
        dsRelative=1;       {Set filepointer relative to the current one.}
        dsEndBased=2;       {Set filepointer from end of file.}
(* The following for compatibility only *)
        FILE_BEGIN   = dsZeroBased; { Move relative to beginning of file }
        FILE_CURRENT = dsRelative;  { Move relative to current fptr position }
        FILE_END     = dsEndBased;  { Move relative to end of file }

{Change the filepointer of a file.}
function DosSetFilePtr (Handle: longint; Pos: longint; Method: cardinal;
                       var PosActual: longint): cardinal; cdecl;
function DosSetFilePtr (Handle: THandle; Pos: longint; Method: cardinal;
                       var PosActual: cardinal): cardinal; cdecl;
{This variant seeks always from begin of file and does not return the
 actual position.}
function DosSetFilePtr (Handle: THandle; Pos: longint): cardinal;

{This variant returns the current filepointer.}
function DosGetFilePtr (Handle: longint; var PosActual: longint): cardinal;

function DosGetFilePtr (Handle: THandle; var PosActual: cardinal): cardinal;

{Use DosQueryFileInfo or DosQueryPathInfo to get the size of a file.}

{Change the size of a file.}
function DosSetFileSize (Handle: THandle; Size: cardinal): cardinal; cdecl;

{Flush update the changes to a file to disk.}
function DosResetBuffer (Handle: THandle): cardinal; cdecl;

{Duplicate or redirect a handle.
To duplicate a handle: Fill handle with source handle and duplicate with -1.
                       Copy of handle will be returned in duplicate.
To redirect a handle:  Fill handle with handle to which the handle to
                       redirect will be redirected. The handle that will be
                       redirected should be placed in duplicate.}
function DosDupHandle (Handle: THandle; var Duplicate: THandle): cardinal;
                                                                         cdecl;

{Return information about a specific handle. See DosOpen for a
 description of FileMode.}
function DosQueryFHState (Handle: longint; var FileMode: longint): cardinal;
                                                                         cdecl;
function DosQueryFHState (Handle: THandle; var FileMode: cardinal): cardinal;
                                                                         cdecl;

{Set information about a specific handle. See DosOpen for a description
 of FileMode.}
function DosSetFHState (Handle: THandle; FileMode: cardinal): cardinal; cdecl;

{Useful constants for the handle type.}
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
function DosQueryHType (Handle: longint; var HandType: longint;
                       var Attr: longint): cardinal; cdecl;
function DosQueryHType (Handle: THandle; var HandType:cardinal;
                       var Attr:cardinal):cardinal; cdecl;

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
function DosEditName(MetaLevel:cardinal;Source,Edit:PChar;
                     Target:PChar;TargetLen:cardinal):cardinal; cdecl;
function DosEditName(MetaLevel:cardinal;const Source,Edit:string;
                     var Target:string):cardinal;

{Move or rename a file.
 OldFile    = old name of file
 NewFile    = new name of file}
function DosMove(OldFile,NewFile:PChar):cardinal; cdecl;
function DosMove(const OldFile,NewFile:string):cardinal;


const   dcExisting=1;           {Overwrite existing files.}
        dcAppend=2;             {Append to existing file.}
        dcFailEAs=4;            {Discard EAs if not supported by target FS}

{Copy a file.
 OldFile    = source file
 NewFile    = destination file}
function DosCopy(OldFile,NewFile:PChar;Option:cardinal):cardinal; cdecl;
function DosCopy(const OldFile,NewFile:string;Option:cardinal):cardinal;

{Delete a file from disk.}
function DosDelete(FileName:PChar):cardinal; cdecl;
function DosDelete(const FileName:string):cardinal;

{Destroy a file on disk. DosForceDelete makes sure that the file cannot
 be unerased anymore.}
function DosForceDelete(FileName:PChar):cardinal; cdecl;
function DosForceDelete(const FileName:string):cardinal;

{Create a new directory.

Name            = Name of directory to create.
EA              = Extented attributes to give the directory. Use nil if you
                  do not want do give it extented attributes. Only the FEA
                  list is used.}
function DosCreateDir(Name:PChar;EA:PEAOp2):cardinal; cdecl;
function DosCreateDir(const Name:string;EA:PEAOp2):cardinal;
{Variants without the EA parameter (nil is used).}
function DosCreateDir(Name:PChar):cardinal;
function DosCreateDir(const Name:string):cardinal;

{Remove a directory.}
function DosDeleteDir(Name:PChar):cardinal; cdecl;
function DosDeleteDir(const Name:string):cardinal;

{Set the current drive. Cannot fail if the driveletter is correct.}
function DosSetDefaultDisk(DiskNum:cardinal):cardinal; cdecl;

{Get the current drive. Because it cannot fail, it is declared as procedure.}
procedure DosQueryCurrentDisk(var DiskNum:longint;var Logical:longint); cdecl;
procedure DosQueryCurrentDisk(var DiskNum:cardinal;var Logical:cardinal); cdecl;

{Set the current directory.}
function DosSetCurrentDir(Name:PChar):cardinal; cdecl;
function DosSetCurrentDir(const Name:string):cardinal;

{Get the current directory.}
function DosQueryCurrentDir(DiskNum:longint;var Buffer;
                            var BufLen:longint):cardinal; cdecl;
function DosQueryCurrentDir(DiskNum:cardinal;var Buffer:string):cardinal;
function DosQueryCurrentDir(DiskNum:cardinal;var Buffer;
                            var BufLen:cardinal):cardinal; cdecl;

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
                     longint):cardinal; cdecl;
function DosDevIOCtl (Handle: THandle; Category,Func:cardinal;var Params;
                     ParamLen:cardinal;var ParamSize:cardinal;
              var Data;DataLen:cardinal;var DataSize:cardinal):cardinal; cdecl;

{****************************************************************************

                      File searching related routines.

****************************************************************************}

const   faReadOnly      =  1;
        faHidden        =  2;
        faSystem        =  4;
        faReserve       =  8;
        faDirectory     = 16;
        faArchive       = 32;

        ilStandard      =  1; (* Use TFileStatus3/TFindFileBuf3 *)
        ilQueryEASize   =  2; (* Use TFileStatus4/TFindFileBuf4 *)
        ilQueryEAs      =  3;
        ilQueryFullName =  5;
        ilStandardL     = 11; (* Use TFileStatus3L/TFindFileBuf3L *)
        ilQueryEASizeL  = 12; (* Use TFileStatus4L/TFindFileBuf4L *)
        ilQueryEAsL     = 13;

        FIL_Standard    = ilStandard;
        FIL_QueryEASize = ilQueryEASize;
        FIL_QueryEAsFromList = ilQueryEAs;
        FIL_StandardL   = ilStandardL;
        FIL_QueryEASizeL = ilQueryEASizeL;
        FIL_QueryEAsFromListL = ilQueryEAsL;

{Format of date records:

 Bit 0..4:      day
 Bit 5..8:      month
 Bit 9..15:     year minus 1980

 Format of time records:

 Bit 0..4:      seconds divided by 2
 Bit 5..10:     minutes
 Bit 11..15:    hours}

type
        TFileStatus = object
        end;
        PFileStatus = ^TFileStatus;

        TFileStatus3 = object (TFileStatus)
            DateCreation,               {Date of file creation.}
            TimeCreation,               {Time of file creation.}
            DateLastAccess,             {Date of last access to file.}
            TimeLastAccess,             {Time of last access to file.}
            DateLastWrite,              {Date of last modification of file.}
            TimeLastWrite:word;         {Time of last modification of file.}
            FileSize,                   {Size of file.}
            FileAlloc:cardinal;         {Amount of space the file really
                                         occupies on disk.}
            AttrFile:cardinal;          {Attributes of file.}
        end;
        PFileStatus3=^TFileStatus3;

        TFileStatus4=object(TFileStatus3)
            cbList:cardinal;            {Length of entire EA set.}
        end;
        PFileStatus4=^TFileStatus4;

        TFileStatus3L = object (TFileStatus)
            DateCreation,               {Date of file creation.}
            TimeCreation,               {Time of file creation.}
            DateLastAccess,             {Date of last access to file.}
            TimeLastAccess,             {Time of last access to file.}
            DateLastWrite,              {Date of last modification of file.}
            TimeLastWrite:word;         {Time of last modification of file.}
            FileSize,                   {Size of file.}
            FileAlloc:int64;         {Amount of space the file really
                                         occupies on disk.}
            AttrFile:cardinal;          {Attributes of file.}
        end;
        PFileStatus3L=^TFileStatus3L;

        TFileStatus4L=object(TFileStatus3L)
            cbList:cardinal;            {Length of entire EA set.}
        end;
        PFileStatus4L=^TFileStatus4L;

        TFileFindBuf3=object(TFileStatus)
            NextEntryOffset: cardinal;  {Offset of next entry}
            DateCreation,               {Date of file creation.}
            TimeCreation,               {Time of file creation.}
            DateLastAccess,             {Date of last access to file.}
            TimeLastAccess,             {Time of last access to file.}
            DateLastWrite,              {Date of last modification of file.}
            TimeLastWrite:word;         {Time of last modification of file.}
            FileSize,                   {Size of file.}
            FileAlloc:cardinal;         {Amount of space the file really
                                         occupies on disk.}
            AttrFile:cardinal;          {Attributes of file.}
            Name:string;                {Also possible to use as ASCIIZ.
                                         The byte following the last string
                                         character is always zero.}
        end;
        PFileFindBuf3=^TFileFindBuf3;

        TFileFindBuf4=object(TFileStatus)
            NextEntryOffset: cardinal;  {Offset of next entry}
            DateCreation,               {Date of file creation.}
            TimeCreation,               {Time of file creation.}
            DateLastAccess,             {Date of last access to file.}
            TimeLastAccess,             {Time of last access to file.}
            DateLastWrite,              {Date of last modification of file.}
            TimeLastWrite:word;         {Time of last modification of file.}
            FileSize,                   {Size of file.}
            FileAlloc:cardinal;         {Amount of space the file really
                                         occupies on disk.}
            AttrFile:cardinal;          {Attributes of file.}
            cbList:cardinal;            {Size of the file's extended attributes.}
            Name:string;                {Also possible to use as ASCIIZ.
                                         The byte following the last string
                                         character is always zero.}
        end;
        PFileFindBuf4=^TFileFindBuf4;

        TFileFindBuf3L=object(TFileStatus)
            NextEntryOffset: cardinal;  {Offset of next entry}
            DateCreation,               {Date of file creation.}
            TimeCreation,               {Time of file creation.}
            DateLastAccess,             {Date of last access to file.}
            TimeLastAccess,             {Time of last access to file.}
            DateLastWrite,              {Date of last modification of file.}
            TimeLastWrite:word;         {Time of last modification of file.}
            FileSize,                   {Size of file.}
            FileAlloc:int64;            {Amount of space the file really
                                         occupies on disk.}
            AttrFile:cardinal;          {Attributes of file.}
            Name:string;                {Also possible to use as ASCIIZ.
                                         The byte following the last string
                                         character is always zero.}
        end;
        PFileFindBuf3L=^TFileFindBuf3L;

        TFileFindBuf4L=object(TFileStatus)
            NextEntryOffset: cardinal;  {Offset of next entry}
            DateCreation,               {Date of file creation.}
            TimeCreation,               {Time of file creation.}
            DateLastAccess,             {Date of last access to file.}
            TimeLastAccess,             {Time of last access to file.}
            DateLastWrite,              {Date of last modification of file.}
            TimeLastWrite:word;         {Time of last modification of file.}
            FileSize,                   {Size of file.}
            FileAlloc:int64;            {Amount of space the file really
                                         occupies on disk.}
            AttrFile:cardinal;          {Attributes of file.}
            cbList:cardinal;            {Size of the file's extended attributes.}
            Name:string;                {Also possible to use as ASCIIZ.
                                         The byte following the last string
                                         character is always zero.}
        end;
        PFileFindBuf4L=^TFileFindBuf4L;


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
function DosFindFirst (FileMask: PChar; var Handle: THandle; Attrib: cardinal;
                      AFileStatus: PFileStatus; FileStatusLen: cardinal;
                      var Count: cardinal; InfoLevel: cardinal): cardinal;
                                                                         cdecl;
function DosFindFirst (const FileMask: string; var Handle: THandle;
                      Attrib: cardinal; AFileStatus: PFileStatus;
                      FileStatusLen: cardinal; var Count: cardinal;
                      InfoLevel: cardinal): cardinal;

{Find next matching file.}
function DosFindNext (Handle: THandle; AFileStatus: PFileStatus;
                     FileStatusLen: cardinal; var Count: cardinal): cardinal;
                                                                         cdecl;

{Close a search handle. Cannot fail if handle does exist.}
function DosFindClose (Handle: THandle): cardinal; cdecl;

{Get info about a file.

 Handle         = Handle of file.
 InfoLevel      = One of the ilXXXX constants. Consult IBM documentation
                  for exect meaning. For normal use: Use ilStandard and
                  PFileFindBuf3 for AFileStatus.
 AFileStatus    = An info return buffer.
 FileStatusLen  = Size of info buffer.}
function DosQueryFileInfo (Handle: THandle; InfoLevel: cardinal;
           AFileStatus: PFileStatus; FileStatusLen: cardinal): cardinal; cdecl;

{Set info about a file. File must be opened with write permissions. See
 above fo the parameters.}
function DosSetFileInfo (Handle: THandle; InfoLevel: cardinal;
           AFileStatus: PFileStatus; FileStatusLen: cardinal): cardinal; cdecl;

{Return info about a file. In contradiction to the above functions, the
 file does not have to be open.}
function DosQueryPathInfo(FileName:PChar;InfoLevel:cardinal;
                AFileStatus:PFileStatus;FileStatusLen:cardinal):cardinal; cdecl;
function DosQueryPathInfo(const FileName:string;InfoLevel:cardinal;
                       AFileStatus:PFileStatus;FileStatusLen:cardinal):cardinal;

{Set information about a file.}
function DosSetPathInfo(FileName:PChar;InfoLevel:cardinal;
                        AFileStatus:PFileStatus;FileStatusLen,
                        Options:cardinal):cardinal; cdecl;

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
                        var Count:longint;InfoLevel:longint):cardinal; cdecl;

function DosEnumAttribute(RefType:cardinal;AFile:pointer;
                          Entry:cardinal;var Buf;BufSize:cardinal;
                        var Count:cardinal;InfoLevel:cardinal):cardinal; cdecl;

function DosEnumAttribute (RefType: cardinal; AFile: PChar;
                          Entry: cardinal; var Buf; BufSize:cardinal;
                    var Count: cardinal; InfoLevel: cardinal): cardinal; cdecl;

function DosEnumAttribute(RefType: cardinal; const AFile: THandle;
                          Entry: cardinal; var Buf; BufSize: cardinal;
                    var Count: cardinal; InfoLevel: cardinal): cardinal; cdecl;

function DosEnumAttribute (Handle: longint; Entry: longint; var Buf;
           BufSize: longint; var Count: longint; InfoLevel: longint): cardinal;

function DosEnumAttribute (Handle: THandle; Entry: cardinal; var Buf;
        BufSize: cardinal; var Count: cardinal; InfoLevel: cardinal): cardinal;

function DosEnumAttribute (const FileName: string;
                           Entry: cardinal; var Buf; BufSize: cardinal;
                           var Count: cardinal; InfoLevel: cardinal): cardinal;

{Get an environment variable.
 Name               = Name of environment variable to get.
 Value              = Receives pointer to environment string.}
function DosScanEnv(Name:PChar;var Value:PChar):cardinal; cdecl;
{There is, of course a string variant.}
function DosScanEnv(const Name:string;var Value:string):cardinal;

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
function DosSearchPath(Flag:cardinal;DirList,FileName:PChar;
                       FullName:PChar;FullLen:cardinal):cardinal; cdecl;
function DosSearchPath(Flag:cardinal;const DirList,FileName:string;
                       var FullName:string):cardinal;

{****************************************************************************

                       File system related routines.

****************************************************************************}

type    TFSInfo=record
            case word of
                1:
                    (File_Sys_ID,
                     Sectors_Per_Cluster,
                     Total_Clusters,
                     Free_Clusters:cardinal;
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
                    (PipeHandle: THandle;
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
                     DataLen,Flag:cardinal):cardinal; cdecl;
function DosFSAttach(const DevName,FileSystem:string;var Data:TAttachData;
                     DataLen,Flag:cardinal):cardinal;

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
                     var Buffer:TFSQBuffer2;var BufLen:longint):cardinal; cdecl;
function DosQueryFSAttach(const DevName:string;Ordinal,InfoLevel:longint;
                          var Buffer:TFSQBuffer2;var BufLen:longint):cardinal;
function DosQueryFSAttach(DevName:PChar;Ordinal,InfoLevel:cardinal;
                     var Buffer:TFSQBuffer2;var BufLen:cardinal):cardinal; cdecl;
function DosQueryFSAttach(const DevName:string;Ordinal,InfoLevel:cardinal;
                          var Buffer:TFSQBuffer2;var BufLen:cardinal):cardinal;

const   FSCtl_Handle=1;
        FSCtl_PathName=2;
        FSCtl_FSDName=3;
        FSCtl_Error_Info=1;
        FSCtl_Max_EASize=2;

{IBMDOCS: "DosFSCtl provides an extended standard interface between an
 application and a file-system driver (FSD).

 Consult IBM documentation about this function..}
function DosFSCtl (Data: pointer; DataLen: longint; var ResDataLen: longint;
                  Parms: pointer; ParmsLen: longint; var ResParmsLen: longint;
                  _Function: longint; Route: PChar;
                  Handle, Method: longint): cardinal; cdecl;
function DosFSCtl (Data: pointer; DataLen: longint; var ResDataLen: longint;
                  Parms: pointer;ParmsLen: longint; var ResParmsLen: longint;
                  _Function: longint; const Route: string;
                  Handle, Method: longint): cardinal;
function DosFSCtl (Data: pointer; DataLen: cardinal; var ResDataLen: cardinal;
                 Parms: pointer; ParmsLen: cardinal; var ResParmsLen: cardinal;
                  _Function: cardinal; Route: PChar;
                  Handle: THandle;Method: cardinal): cardinal; cdecl;
function DosFSCtl (Data: pointer; DataLen: cardinal; var ResDataLen: cardinal;
                 Parms: pointer; ParmsLen: cardinal; var ResParmsLen: cardinal;
                  _Function: cardinal; const Route: string;
                  Handle: THandle; Method: cardinal): cardinal;

{Get information about a drive.
InfoLevels:
 1              Get total/free space etc.
 2              Get volumelabel.}
function DosQueryFSInfo (DiskNum, InfoLevel: cardinal; var Buffer: TFSInfo;
                         BufLen: cardinal): cardinal; cdecl;

{Set information about a drive.}
function DosSetFSInfo (DiskNum, InfoLevel: cardinal; var Buffer: TFSinfo;
                       BufLen: cardinal): cardinal; cdecl;

{Check if verify mode is enabled.}
function DosQueryVerify (var Enabled: longint): cardinal; cdecl;
function DosQueryVerify (var Enabled: cardinal): cardinal; cdecl;
function DosQueryVerify (var Enabled: boolean): cardinal;

{Turn the verify mode on or off.}
function DosSetVerify (Enable: cardinal): cardinal; cdecl;
function DosSetVerify (Enable: boolean): cardinal;

{Change the number of filehandles our program can open. (Default=50). It
 won't hurt if there are files open when you are calling this.}
function DosSetMaxFH (Count: cardinal): cardinal; cdecl;

{Ask for more filehandles (or dump filehandles). It won't hurt if there are
 files open when you are calling this.
 ReqCount       = Number of filehandles to ask for. (Negative to dump them.)
 CurMaxFH       = Receives the total number of filehandles your program has
                  access to.}
function DosSetRelMaxFH (var ReqCount: longint; var CurMaxFH: longint):
                                                               cardinal; cdecl;
function DosSetRelMaxFH (var ReqCount: longint; var CurMaxFH: cardinal):
                                                               cardinal; cdecl;

const   dsFull=0;       {IBM DOCS: "Perform full system shutdown and
                         file-system lock."}
        dsQuiescient=1; {IBM DOCS: "Perform buffer and cache flushing to
                         make system quiescent."}

{Prepare the system for shutdown.}
function DosShutdown (Flags: cardinal): cardinal; cdecl;


{Parameters (system variables) for DosQuerySysInfo.}
const   svMaxPathLength     = 1;        {Maximum length of a pathname.}
        svMaxTextSessions   = 2;        {Maximum number of text sessions.}
        svMaxPMSessions     = 3;        {Maximum number of PM sessions.}
        svMaxVDMSessions    = 4;        {Maximum number of DOS sessions.}
        svBootDrive         = 5;        {Get the boot drive. (A=1, B=2 etc.)}
        svDynPriVariation   = 6;        {Dynamic priority variation flag
                                         (0 = absolute priority, 1 means
                                         dynamic priority).}
        svMaxWait           = 7;        {Maximum wait time in seconds.}
        svMinSlice          = 8;        {Minimum time slice in milliseconds.}
        svMaxSlice          = 9;        {Maximum time slice in milliseconds.}
        svPageSize          = 10;       {Size of a page (always 4096 bytes for
                                         x86).}
        svVersionMajor      = 11;       {Major version number of kernel:
                                         10 for OS/2 1.0 and 1.1,
                                         20 for OS/2 2.0 .. OS/2 4.0.}
        svVersionMinor      = 12;       {Minor version of kernel:
                                         OS/2 2.0: 00, 2.1: 10, 2.11: 11,
                                              3.0: 30, 4.0: 40.}
        svVersionRevision   = 13;       {Revision of kernel. Until now all
                                         OS/2 versions return 0.}
        svMsCount           = 14;       {Uptime in milliseconds.}
        svTimeLow           = 15;       {System time in seconds since
                                         1 January 1970 0:00:00, low dword.}
        svTimeHigh          = 16;       {System time in seconds since
                                         1 January 1970 0:00:00, high dword.}
        svTotPhysMem        = 17;       {Amount in bytes of physical memory
                                         in system.}
        svTotResMem         = 18;       {Amount in bytes of resident memory
                                         in system.}
        svTotAvailMem       = 19;       {Amount in bytes of available
                                         memory.}
        svMaxPrMem          = 20;       {Maximum amount of memory the current
                                         process can request for its
                                         private use.}
        svMaxShMem          = 21;       {Maximum amount of shared memory
                                         the current process can request.}
        svTimerInterval     = 22;       {Timer interval in tenths of a
                                         millisecond.}
        svMaxCompLength     = 23;       {Maximum length of a component in a
                                         pathname.}
        svForegroundFSSession = 24;     {Session ID of the foreground
                                         full-screen session. Presentation
                                         Manager and all sessions running under
                                         PM (including PM, windowed VIO, VDM
                                         and seamless Win 3.x) return ID 1.}
        svForegroundProcess = 25;       {Process ID of the current foreground
                                         process.}
        svNumProcessors     = 26;       {Number of CPUs in machine - supported
                                         since WarpServer Advanced SMP.}
{The following parameters are only supported in WSeB/MCP/eCS
 or OS/2 Warp 4.0 with FP14 and above.}
        svMaxHPrMem         = 27;       {Maximum amount of high memory the
                                         process can request for its private
                                         use in total (not necessarily at once
                                         because of potential fragmentation).}
        svMaxHShMem         = 28;       {Maximum amount of high shared memory
                                         the process can request.}
        svMaxProcesses      = 29;       {Maximum number of concurrent processes
                                         supported.}
        svVirtualAddressLimit = 30;     {Size of the user address space in MB
                                         (i.e. the value of the rounded
                                         VIRTUALADDRESSLIMIT as specified in
                                         CONFIG.SYS, or the default value of
                                         512).}
        svMax               = 30;       {The maximum parameter number
                                         for WSeB/MCP/eCS.}

{Aliases for compatibility...}
        QSV_MAX_PATH_LENGTH = svMaxPathLength;
        QSV_MAX_TEXT_SESSIONS = svMaxTextSessions;
        QSV_MAX_PM_SESSIONS = svMaxPMSessions;
        QSV_MAX_VDM_SESSIONS = svMaxVDMSessions;
        QSV_BOOT_DRIVE = svBootDrive;
        QSV_DYN_PRI_VARIATION = svDynPriVariation;
        QSV_MAX_WAIT = svMaxWait;
        QSV_MIN_SLICE = svMinSlice;
        QSV_MAX_SLICE = svMaxSlice;
        QSV_PAGE_SIZE = svPageSize;
        svMajorVersion = svVersionMajor;
        QSV_VERSION_MAJOR = svVersionMajor;
        svMinorVersion = svVersionMinor;
        QSV_VERSION_MINOR = svVersionMinor;
        svRevision = svVersionRevision;
        QSV_VERSION_REVISION = svVersionRevision;
        QSV_MS_COUNT = svMsCount;
        QSV_TIME_LOW = svTimeLow;
        QSV_TIME_HIGH = svTimeHigh;
        svPhysMem = svTotPhysMem;
        QSV_TOTPHYSMEM = svTotPhysMem;
        svResMem = svTotResMem;
        QSV_TOTRESMEM = svTotResMem;
        svAvailMem = svTotAvailMem;
        QSV_TOTAVAILMEM = svTotAvailMem;
        svPrMem = svMaxPrMem;
        svShMem = svMaxShMem;
        QSV_MAXPRMEM = svMaxPrMem;
        QSV_MAXSHMEM = svMaxShMem;
        QSV_TIMER_INTERVAL = svTimerInterval;
        QSV_MAX_COMP_LENGTH = svMaxCompLength;
        QSV_FOREGROUND_FS_SESSION = svForegroundFSSession;
        svForegroundSession = svForegroundFSSession;
        QSV_FOREGROUND_PROCESS = svForegroundProcess;
        QSV_NUMPROCESSORS = svNumProcessors;
        QSV_MAXHPRMEM = svMaxHPrMem;
        QSV_MAXHSHMEM = svMaxHShMem;
        QSV_MAXPROCESSES = svMaxProcesses;
        QSV_VIRTUALADDRESSLIMIT = svVirtualAddressLimit;
        QSV_MAX = svMax;

{Get one or more system variables.
 First          = First variable to get.
 Last           = Last variable to get.
 Buf            = Receives variables.
 BufSize        = Size of the buffer (every system variable is a cardinal).}
function DosQuerySysInfo(First,Last:cardinal;var Buf;BufSize:cardinal):cardinal;
                                                                         cdecl;
type
    TQSVValues = array [1..svMax] of cardinal;
    PQSVValues = ^TQSVValues;

function DosQuerySysInfo(First,Last:cardinal;Buf:PQSVValues;
                                              BufSize:cardinal):cardinal;cdecl;

{Return information about a partitionable disk.}
function DosPhysicalDisk(Func:cardinal;Buf:pointer;BufSize:cardinal;
                            Params:pointer;ParamSize:cardinal):cardinal; cdecl;

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
        mfObj_Any       = $00400;   {Allow using high memory (> 512 MB).}
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
        obj_Any = mfObj_Any;
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
function DosAllocMem(var P:pointer;Size,Flag:cardinal):cardinal; cdecl;

{Free a memory block.}
function DosFreeMem(P:pointer):cardinal; cdecl;

{Set settings for a block of memory.
 P          = Pointer to the memory. Doesn't need to be the start of the
              memory block allocated with DosAllocMem, but must be a multiple
              of 4096.
 Size       = Number of bytes to change settings for. Is rounded up to a
              multile of 4096.
 Flags      = New flags for the memory.}
function DosSetMem(P:pointer;Size,Flag:cardinal):cardinal; cdecl;

{Give another process access to a shared memory block.

 P          = Pointer to the shared memory object.
 PID        = Process of destination process.
 Flag       = Permissions the the destination process gets.}
function DosGiveSharedMem(P:pointer;PID,Flag:cardinal):cardinal; cdecl;

{Get access to a shared memory object.

 P          = Pointer to shared memory object.
 Flag       = Permissions to ask.}
function DosGetSharedMem(P:pointer;Flag:cardinal):cardinal; cdecl;

{Get access to a shared memory object that has a name.

 P          = Pointer to shared memory object.
 Name       = Name of the memory object. (Starting with '\SHAREMEM\'.
 Flag       = Permissions to ask.}
function DosGetNamedSharedMem(var P:pointer;Name:PChar;Flag:cardinal):cardinal;
                                                                         cdecl;
function DosGetNamedSharedMem(var P:pointer;const Name:string;
                              Flag:cardinal):cardinal;

{Allocate memory so that it can later be shared with another program.
 P          = Reveives pointer to memory.
 Name       = Optional: name to give memory. Must start with '\SHAREMEM\'.
              Use nil for the PChar or '' for the string variant for no name.
 Size       = Number of bytes to allocate.}
function DosAllocSharedMem(var P:pointer;Name:PChar;
                                            Size,Flag:cardinal):cardinal; cdecl;
function DosAllocSharedMem(var P:pointer;const Name:string;Size,
                                                        Flag:cardinal):cardinal;

{Get the size and flags of a block of memory.

 P          = Pointer to the block of memory.
 Size       = Receives block size.
 Flag       = Receives the flags.}
function DosQueryMem(P:pointer;var Size,Flag:longint):cardinal; cdecl;
function DosQueryMem (P: pointer; var Size, Flag: cardinal): cardinal; cdecl;

{Allocate a block of memory in a heap.
 Base       = Pointer to the start of the heap.
 P          = Receives pointer to the memory bock.
 Size       = Number of bytes to allocate.}
function DosSubAllocMem(Base:pointer;var P:pointer;Size:cardinal):cardinal;
                                                                         cdecl;

{Free a block of memory in a heap.
 Base       = Pointer to the start of the heap.
 P          = Pointer to memory block to free.
 Size       = Number of bytes to free.}
function DosSubFreeMem(Base,P:pointer;Size:cardinal):cardinal; cdecl;

{Turn a block of memory into a heap.

Base        = Pointer to memory block to turn into a heap.
Flag        = One or more of the mfSub_XXXX flags.
Size        = Size of the requested heap.}
function DosSubSetMem(Base:pointer;Flag,Size:cardinal):cardinal; cdecl;

{Destroy a heap. (Memory remains allocated).

Base        = Pointer to the heap to destroy.}
function DosSubUnsetMem(Base:pointer):cardinal; cdecl;

{****************************************************************************

                        Semaphore related routines

****************************************************************************}

const   smShared        = $0001;    {Semaphore is shared.}
        smMWWaitAny     = $0002;    {MuxWait only: Wait until a semaphore
                                     is cleared.}
        smMWWaitAll     = $0004;    {MuxWait only: Wait until all semaphores
                                     are cleared.}
        Sem_Indefinite_Wait = cardinal (-1);
                                    {DosRequestMutExSem blocks the calling
                                     thread indefinitely.}
        Sem_Immediate_Return = 0;   {DosRequestMutExSem returns immediately
                                     without blocking the calling thread.}
(* The following two flag values are only available
   on Warp 3 FP 29, Warp 4 FP 5 and above. *)
        dce_AutoReset   = $1000;    {This will cause the (event) semaphore
                                     to be reset automatically at the time
                                     it is posted.}
        dce_PostOne     = $0800;    {This will cause only one thread to be
                                     posted where multiple threads are
                                     waiting on an event semaphore created
                                     with this attribute. dce_PostOne also
                                     causes the semaphore to be reset
                                     automatically when it is posted.}
(* The following just for compatibility. *)
        dcMW_Wait_Any = smMWWaitAny;
        dcMW_Wait_All = smMWWaitAll;

type   PSemRecord=^TSemRecord;
       TSemRecord=record
          Semaphore: THandle;               {Handle of semaphore to link.}
          User: cardinal;
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
function DosCreateEventSem (Name: PChar;var Handle: THandle;
                            Attr, State: cardinal): cardinal; cdecl;
function DosCreateEventSem (const Name: string; var Handle: THandle;
                            Attr, State: cardinal): cardinal;
function DosCreateEventSem (Name: PChar;var Handle: THandle;
                            Attr: cardinal; State: boolean): cardinal;
function DosCreateEventSem (const Name: string; var Handle: THandle;
                            Attr: cardinal; State: boolean): cardinal;

{Open a semaphore created by another process or thread.

 Name       = Name of semaphore.
 Handle     = Receives handle of semaphore.}
function DosOpenEventSem (Name: PChar; var Handle: THandle): cardinal; cdecl;
function DosOpenEventSem (const Name: string; var Handle: THandle): cardinal;

{Close an event semaphore.
 Handle     = Handle of a semaphore to close.}
function DosCloseEventSem (Handle: THandle): cardinal; cdecl;

{Reset an event semaphore: *** probeer *** operation.
 Handle     = Handle of semaphore.
 PostCount  = Number of times DosPostEventSem has been called since last
              reset.

 Note:      Returns errorcode 300 if semaphore is already reset.}
function DosResetEventSem(Handle:longint;var PostCount:longint):cardinal;
                                                                         cdecl;
function DosResetEventSem (Handle: THandle; var PostCount: cardinal): cardinal;
                                                                         cdecl;

{Post an event semaphore: *** verhoog *** operation.
 Handle     = Handle of semaphore.

Note:       Returns errorcode 299 if semaphore is already posted.}
function DosPostEventSem (Handle: THandle): cardinal; cdecl;

{Wait until an event semaphore is posted (wait until *** verhoog *** operation).
 Handle     = Handle of semaphore.
 Timeout    = Return with errorcode if timeout milliseconds have past and the
              semaphore is still reset. To return immediately use 0,
              to wait forever use -1.}
function DosWaitEventSem (Handle: THandle; Timeout: cardinal): cardinal; cdecl;

{Check if an event semaphore is posted (if a *** verhoog *** operation has been done).
 Handle     = Handle of semaphore.
 Posted     = Receives number of times DosPostEventSem was called since
              the last reset.}
function DosQueryEventSem (Handle: longint; var Posted: longint): cardinal;
                                                                         cdecl;
function DosQueryEventSem (Handle: THandle; var Posted: cardinal): cardinal;
                                                                         cdecl;

{Create a Mutual Exclusion semaphore (mutex).
 Name       = Optional: Name to give to semaphore. Must start with '\SEM32\'.
              Use nil for PChar or '' for string variant to use no name.
              If a name if used the semaphore is shared.
 Handle     = Receives handle of semaphore.
 Attr       = One or more of the smXXXX constants.
 State      = Initial state: (0/false=Not owned, 1/true=Owned.)}
function DosCreateMutExSem (Name: PChar; var Handle: THandle;
                            Attr: cardinal; State: boolean): cardinal;
function DosCreateMutExSem (const Name: string; var Handle: THandle;
                            Attr: cardinal; State: boolean): cardinal;
function DosCreateMutExSem (Name: PChar; var Handle: THandle;
                            Attr, State: cardinal): cardinal; cdecl;
function DosCreateMutExSem (const Name: string; var Handle: THandle;
                            Attr, State: cardinal): cardinal;

{Open a shared mutex semaphore.
 Name       = Name of semaphore to open, always starts with '\SEM32\'.
 Handle     = Receives handle to semaphore.}
function DosOpenMutExSem (Name: PChar; var Handle: THandle): cardinal; cdecl;
function DosOpenMutExSem (const Name: string; var Handle: THandle): cardinal;

{Close a mutex semaphore.
 handle     = Handle of semaphore to close.}
function DosCloseMutExSem (Handle: THandle): cardinal; cdecl;

{Request ownership of a mutex semaphore. If the semaphore is already owned the
 process is halted until the semaphore is released.
 Handle     = Handle of semaphore.
 Timeout    = Return with errorcode if the semaphore is still owned after
              timeout milliseconds; special values are Sem_Indefinite_Wait
              and Sem_Immediate_Return.}
function DosRequestMutExSem (Handle: THandle; Timeout: cardinal): cardinal;
                                                                         cdecl;

{Release the ownership of a mutex semaphore.
 Handle     = Handle of semaphore to release.}
function DosReleaseMutExSem (Handle: THandle): cardinal; cdecl;

{Query the PID and TIB of the owner of a mutex semaphore.
 Handle     = Handle of semaphore.
 PID        = Receives process ID of owner.
 TID        = Receives thread ID of owner.
 Count      = Number of threads (within and outside current process) waiting
              for ownership of semaphore.}
function DosQueryMutExSem(Handle:longint;var PID,TID,Count:longint):
                                                               cardinal; cdecl;
function DosQueryMutExSem (Handle: THandle; var PID, TID, Count: cardinal):
                                                               cardinal; cdecl;

{Create a Multiple Wait (MuxWait) semaphore.
 Name       = Optional: Name to give semaphore. Must start with '\SEM32\'.
              Use nil for PChar or '' for string variant to use no name.
              If a name if used the semaphore is shared.
 Handle     = Receives handle of semaphore.
 CSemRec    = Number of semaphores to link muxwait semaphore with.
 SemArray   = Array of semaphore records to link with muxwait semaphore.
 Attr       = One or more of the smXXXX constants.}
function DosCreateMuxWaitSem (Name: PChar; var Handle: THandle;
  CSemRec: cardinal; var SemArray: TSemArray; Attr: cardinal): cardinal; cdecl;
function DosCreateMuxWaitSem (Name: PChar; var Handle: THandle;
      CSemRec: cardinal; SemArray: PSemArray; Attr: cardinal): cardinal; cdecl;
function DosCreateMuxWaitSem (const Name: string; var Handle: THandle;
                             CSemRec: cardinal; var SemArray: TSemArray;
                             Attr: cardinal): cardinal;

{Open a MuxWait semaphore.
 Name       = Name of semaphore to open.
 Handle     = Receives handle of semaphore.}
function DosOpenMuxWaitSem (Name: PChar; var Handle: THandle): cardinal; cdecl;
function DosOpenMuxWaitSem (const Name: string; var Handle: THandle): cardinal;

{Close a MuxWait semaphore.}
function DosCloseMuxWaitSem (Handle: THandle): cardinal; cdecl;

{Wait for the MuxWait semaphore to be cleared.
 Handle     = Handle of semaphore.
 Timeout    = Timeout. See above.
 User       = Receives user value of the semaphore that caused the muxwait
              semaphore to be cleared.}
function DosWaitMuxWaitSem(Handle:longint;Timeout:longint;
                                              var User:longint):cardinal;cdecl;
function DosWaitMuxWaitSem (Handle: THandle; Timeout: cardinal;
                                          var User: cardinal): cardinal; cdecl;

{Add a semaphore to the MuxWait semaphore.

 Handle     = Handle of semaphore.
 SemRec     = The semaphore to add.}
function DosAddMuxWaitSem (Handle: THandle; var SemRec: TSemRecord): cardinal;
                                                                         cdecl;

{Remove a semaphore from the MuxWait semaphore.
 Handle     = Handle of muxwait semaphore.
 Sem        = Handle of semaphore to remove.}
function DosDeleteMuxWaitSem (Handle, Sem: THandle): cardinal; cdecl;

{Query the semaphores from a MuxWait semaphore.
 Handle     = Handle of semaphore.
 CSemRec    = Input: Size of our array. Output: Number of items in array.
 SemRecs    = Array where TSemRecords are stored.
 Attr       = Flags used by creation of semaphore.}
function DosQueryMuxWaitSem (Handle: longint; var CSemRec: longint;
                   var SemRecs: TSemArray; var Attr: longint): cardinal; cdecl;
function DosQueryMuxWaitSem (Handle: THandle; var CSemRec: cardinal;
                  var SemRecs: TSemArray; var Attr: cardinal): cardinal; cdecl;

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
function DosGetDateTime(var Buf:TDateTime):cardinal; cdecl;

{Set the date and time.}
function DosSetDateTime(var Buf:TDateTime):cardinal; cdecl;

{Start a one shot timer.
 MSec       = Number of miliseconds the timer will run.
 HSem       = Handle of event semaphore that is posted when time has expired.
 TimHandle  = Receives timer handle.}
function DosAsyncTimer (MSec: cardinal; HSem: THandle;
                                      var TimHandle: THandle): cardinal; cdecl;

{Start a cyclic timer.
 MSec       = Number of miliseconds the timer will run.
 HSem       = Handle of event semaphore that is posted when time has expired.
 TimHandle  = Receives timer handle.}
function DosStartTimer (MSec: cardinal; HSem: THandle;
                                      var TimHandle: THandle): cardinal; cdecl;

{Stop a timer and destroy its handle. There is no need to check for an
 error code if you know your timer handle is correct.}
function DosStopTimer (TimHandle: THandle): cardinal; cdecl;

{Get the frequency of the high resolution timer.}
function DosTmrQueryFreq(var Freq:longint):cardinal; cdecl;
function DosTmrQueryFreq(var Freq:cardinal):cardinal; cdecl;

{Get the current value of the high resolution timer.}
function DosTmrQueryTime(var Time:comp):cardinal; cdecl;
function DosTmrQueryTime (var Time: QWord): cardinal; cdecl;

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
function DosLoadModule (ObjName: PChar; ObjLen: cardinal; DLLName: PChar;
                                         var Handle: THandle): cardinal; cdecl;
function DosLoadModule (var ObjName: string; ObjLen: cardinal;
                         const DLLName: string; var Handle: THandle): cardinal;

{Let OS/2 know that we do not need a DLL anymore. If we were the only process
 using the DLL, it is unloaded.}
function DosFreeModule (Handle: THandle): cardinal; cdecl;

{Get the address of a procedure.
 Handle         = DLL handle,
 Ordinal        = Procedure to get address for. 0=Use its name.
 ProcName       = Name of the procedure to query address for. Must be nil
                  for PChar or '' for string variant if Ordinal is nonzero.
 Address        = Receives address of procedure.}
function DosQueryProcAddr (Handle: THandle; Ordinal: cardinal; ProcName: PChar;
                                        var Address: pointer): cardinal; cdecl;
function DosQueryProcAddr (Handle: THandle; Ordinal: cardinal;
                       const ProcName: string; var Address: pointer): cardinal;

{Get the handle of a loaded DLL or a loaded executable.
 DLLName        = Name of DLL.
 Handle         = Receives DLL handle if present.}
function DosQueryModuleHandle (DLLName: PChar; var Handle: THandle): cardinal;
                                                                         cdecl;
function DosQueryModuleHandle (const DLLName: string;
                                                var Handle: THandle): cardinal;

{Get the pathname of a loaded DLL or a loaded executable.

 Handle         = Handle of DLL.
 NameLen        = Maximum length of char array.
 Name           = PChar (or string) where name is returned.}
function DosQueryModuleName (Handle: THandle; NameLen: cardinal;
                                                 Name: Pchar): cardinal; cdecl;
{function DosQueryModuleName(Handle:THandle;var Name:OpenString):cardinal;}

const   pt16bit=0;
        pt32bit=1;

{Return if a procedure is either 16 or 32 bit.
 Handle         = Handle of DLL.
 Ordinal        = DLL index number. 0 means use Name.
 Name           = Must be nil for PChar or '' for string variant if Ordinal
                  is zero. Otherwise it contains the procedure name.
 ProcType       = One of the ptXXXX constants.}
function DosQueryProcType (Handle,Ordinal:longint;Name:PChar;
                          var ProcType:longint):cardinal; cdecl;
function DosQueryProcType(Handle,Ordinal:longint;const Name:string;
                          var ProcType:longint):cardinal;
function DosQueryProcType (Handle: THandle; Ordinal: cardinal; Name:PChar;
                          var ProcType: cardinal): cardinal; cdecl;
function DosQueryProcType (Handle: THandle; Ordinal: cardinal; const Name: string;
                          var ProcType: cardinal): cardinal;

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
function DosGetResource (Handle: THandle; ResType, ResName: cardinal;
                                              var P: pointer): cardinal; cdecl;

{Remove a resource object from memory.
 P              = Pointer to resource.}
function DosFreeResource (P: pointer): cardinal; cdecl;

{Get the size of a resource object.
 Handle         = Handle to DLL (or executable).
 IDT            = One of the rtXXXX constants.
 IDN            = Number associated to resource object by resource compiler.
 Size           = Receives resource size.}
function DosQueryResourceSize (Handle,IDT,IDN:longint;var Size:longint):cardinal;
                                                                         cdecl;
function DosQueryResourceSize (Handle: THandle; IDT, IDN: cardinal;
                                          var Size: cardinal): cardinal; cdecl;


{****************************************************************************

                   Country and codepage specific routines.

****************************************************************************}

type    TCountryCode=record
            Country,            {Country to query info about (0=current).}
            CodePage:cardinal;  {Code page to query info about (0=current).}
        end;
        PCountryCode=^TCountryCode;
        CountryCode=TCountryCode;

        TTimeFmt=(Clock12,Clock24);

        TCountryInfo=record
            Country,CodePage:cardinal;     {Country and codepage requested.}
            case byte of
            0:(
            DateFormat:cardinal;            {1=ddmmyy 2=yymmdd 3=mmddyy}
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
            fsDateFmt:cardinal;             {1=ddmmyy 2=yymmdd 3=mmddyy}
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
                  var Res:TCountryInfo;var ActualSize:longint):cardinal; cdecl;
function DosQueryCtryInfo(Size:cardinal;var Country:TCountryCode;
                 var Res:TCountryInfo;var ActualSize:cardinal):cardinal; cdecl;

{Get info about a code page with a DBCS character set.}
function DosQueryDBCSEnv(Size:cardinal;var Country:TCountryCode;
                                                    Buf:PChar):cardinal; cdecl;

{Convert a string to uppercase.
    Size        = Length of string.
    Country     = Country and codepage for converting.
    AString     = String to convert.}
function DosMapCase(Size:cardinal;var Country:TCountryCode;
                                                AString:PChar):cardinal; cdecl;
function DosMapCase(var Country:TCountryCode;
                                                  var AString:string):cardinal;

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
                          Buf:PByteArray;var TableLen:longint):cardinal; cdecl;
function DosQueryCollate (Size: cardinal; var Country: TCountryCode;
                      Buf:PByteArray; var TableLen: cardinal): cardinal; cdecl;

{Get the current codepage. The return buffer (CodePages) is filled with the
 current code page followed by the list of prepared codepages as specified in
 CONFIG.SYS - all of them returned as DWORDs, typically not more than 3 should
 be necessary. Return value of 473 indicates that not all values fit into the
 provided space.}
function DosQueryCP(Size:longint; PCodePages: PWordArray;
                                          var ActSize:longint):cardinal; cdecl;
function DosQueryCP(Size: cardinal; PCodePages: PWordArray;
                                         var ActSize:cardinal):cardinal; cdecl;
function DosQueryCP (Size: cardinal; var CodePages;
                                       var ActSize: cardinal): cardinal; cdecl;

{Change the codepage, but only for the current process.}
function DosSetProcessCP (CP: cardinal): cardinal; cdecl;

{****************************************************************************

                       Exception handling related functions

****************************************************************************}


{Exception constants.}
const
  Xcpt_Continue_Search            = $00000000;
  Xcpt_Continue_Execution         = $ffffffff;
  Xcpt_Continue_Stop              = $00716668;

  Xcpt_Signal_Intr                = $1;
  Xcpt_Signal_KillProc            = $3;
  Xcpt_Signal_Break               = $4;

  Xcpt_Fatal_Exception            = $c0000000;
  Xcpt_Severity_Code              = $c0000000;
  Xcpt_Customer_Code              = $20000000;
  Xcpt_Facility_Code              = $1fff0000;
  Xcpt_Exception_Code             = $0000ffff;

  Xcpt_Unknown_Access             = $00000000;
  Xcpt_Read_Access                = $00000001;
  Xcpt_Write_Access               = $00000002;
  Xcpt_Execute_Access             = $00000004;
  Xcpt_Space_Access               = $00000008;
  Xcpt_Limit_Access               = $00000010;
  Xcpt_Data_Unknown               = $ffffffff;

  Xcpt_Guard_Page_Violation       = $80000001;
  Xcpt_Unable_To_Grow_Stack       = $80010001;
  Xcpt_Access_Violation           = $c0000005;
  Xcpt_In_Page_Error              = $c0000006;
  Xcpt_Illegal_Instruction        = $c000001c;
  Xcpt_Invalid_Lock_Sequence      = $c000001d;
  Xcpt_Noncontinuable_Exception   = $c0000024;
  Xcpt_Invalid_Disposition        = $c0000025;
  Xcpt_Unwind                     = $c0000026;
  Xcpt_Bad_Stack                  = $c0000027;
  Xcpt_Invalid_Unwind_Target      = $c0000028;
  Xcpt_Array_Bounds_Exceeded      = $c0000093;
  Xcpt_Float_Denormal_Operand     = $c0000094;
  Xcpt_Float_Divide_By_Zero       = $c0000095;
  Xcpt_Float_Inexact_Result       = $c0000096;
  Xcpt_Float_Invalid_Operation    = $c0000097;
  Xcpt_Float_Overflow             = $c0000098;
  Xcpt_Float_Stack_Check          = $c0000099;
  Xcpt_Float_Underflow            = $c000009a;
  Xcpt_Integer_Divide_By_Zero     = $c000009b;
  Xcpt_Integer_Overflow           = $c000009c;
  Xcpt_Privileged_Instruction     = $c000009d;
  Xcpt_Datatype_Misalignment      = $c000009e;
  Xcpt_Breakpoint                 = $c000009f;
  Xcpt_Single_Step                = $c00000a0;
  Xcpt_Process_Terminate          = $c0010001;
  Xcpt_Async_Process_Terminate    = $c0010002;
  Xcpt_Signal                     = $c0010003;

  Context_Control        = $00000001; { SS:ESP, CS:EIP, EFLAGS and EBP set }
  Context_Integer        = $00000002; { EAX, EBX, ECX, EDX, ESI and EDI set }
  Context_Segments       = $00000004; { DS, ES, FS, and GS set }
  Context_Floating_Point = $00000008; { numeric coprocessor state set }
  Context_Full           = Context_Control or
                           Context_Integer or
                           Context_Segments or
                           Context_Floating_Point;

const
    MaxExceptionParameters = 4;  { Enough for all system exceptions. }

type        PExceptionRegistrationRecord=^TExceptionRegistrationRecord;
            PExceptionReportRecord=^TExceptionReportRecord;
            PContextRecord=^TContextRecord;

            TExceptionHandler = function (Report: PExceptionReportRecord;
                                          RegRec: PExceptionRegistrationRecord;
                                          Context: PContextRecord;
                                          DispContext: pointer): cardinal;
                                                                         cdecl;

            TExceptionRegistrationRecord=record
                Prev_Structure:PExceptionRegistrationRecord;
                ExceptionHandler:TExceptionHandler;
            end;

            TExceptionReportRecord=record
                Exception_Num,
                HandlerFlags:cardinal;
                Nested_RepRec:PExceptionReportRecord;
                Address:pointer;
                ParamCount:cardinal;
                Parameters:array [0..MaxExceptionParameters] of cardinal;
            end;

            TContextRecord=record
                ContextFlags:cardinal;
                Env:array[1..7] of cardinal;
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
                Reg_SS:cardinal;
            end;

{Warning!!! Never use Presentation Manager functions from exception
 handlers!}

{Install an exceptionhandler. The Prev_Structure field of RegRec should be
nil, it will be filled in be OS/2. RegRec must be on the stack: It must be a
local variable.}
function DosSetExceptionHandler(var RegRec:TExceptionRegistrationRecord):cardinal;
                                                                         cdecl;

{Uninstall an exception handler.}
function DosUnSetExceptionHandler(var RegRec:TExceptionRegistrationRecord
                                  ):cardinal; cdecl;

{Trigger an exception.}
function DosRaiseException(var Excpt:TExceptionReportRecord):cardinal; cdecl;

{Send a signal to a process.}
function DosSendSignalException(PID,Exception:cardinal):cardinal; cdecl;

{Call and remove a set of exceptionhandlers}
function DosUnwindException(var Handler:TExceptionRegistrationRecord;
                            TargetIP:pointer;
                            var RepRec:TExceptionReportRecord):cardinal; cdecl;

{Full screen applications can get Ctrl-C and Ctrl-Break focus. For all
 processes sharing one screen, only one can have Ctrl-C focus.
 Enable     = 0 = Release focus, 1 = Get focus.
 Times      = Number of times focus has been get minus number of times it
              has been released.}
function DosSetSignalExceptionFocus(Enable:longint;var Times:longint):cardinal;
                                                                         cdecl;
function DosSetSignalExceptionFocus (Enable: cardinal;
                                         var Times: cardinal): cardinal; cdecl;
function DosSetSignalExceptionFocus (Enable: boolean;
                                                var Times: cardinal): cardinal;

{Tell OS/2 that if an exception occurs, it must queue it up, until a
 DosExitMustComplete follows. Urgent exceptions still occur. The only
 possible error is that the nesting becomes too high, so error checking
 is only needed in seldom cases.
 Nesting    = Number of DosEnterMustComplete calls minus number of
              DosExitMustComplete calls.}
function DosEnterMustComplete(var Nesting:longint):cardinal; cdecl;
function DosEnterMustComplete(var Nesting:cardinal):cardinal; cdecl;

{Tell OS/2 that it can send exceptions again. See above}
function DosExitMustComplete(var Nesting:longint):cardinal; cdecl;
function DosExitMustComplete(var Nesting:cardinal):cardinal; cdecl;

{Tell we want further signal exceptions.
 SignalNum  = Signal nummer to acknowlegde.}
function DosAcknowledgeSignalException (SignalNum: cardinal): cardinal; cdecl;


{****************************************************************************

                           Queue related routines.

****************************************************************************}

type
 TRequestData = record
                 case boolean of
                  false: (PID0:longint;   {ID of process that wrote element.}
                          Data0:longint); {Information from process
                                          writing the data.}
                  true: (PID, Data: cardinal);
                end;
 PRequestData = ^TRequestData;

{Useful constants for priority parameters.}
const   quFIFO=0;
        quLIFO=1;
        quPriority=2;
        quNoConvert_Address=0;
        quConvert_Address=4;

{Close a queue. If the calling process has created the queue, it is
 destroyed. If you can guarantee the handle is correct, there is no need
 to check for error codes.}
function DosCloseQueue (Handle: THandle): cardinal; cdecl;

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
function DosCreateQueue (var Handle: THandle; Priority: cardinal;
                         Name: PChar): cardinal; cdecl;
function DosCreateQueue (var Handle: THandle; Priority: cardinal;
                         const Name: string): cardinal;

{Open an existing queue. You cannot read from the queue unless you are the
 process that created it. The name must have the format '\QUEUES\name.ext'}
function DosOpenQueue(var Parent_PID:longint;var Handle:longint;
                      Name:PChar):cardinal; cdecl;
function DosOpenQueue(var Parent_PID:longint;var Handle:longint;
                      const Name:string):cardinal;

function DosOpenQueue (var Parent_PID: cardinal; var Handle: THandle;
                       Name: PChar): cardinal; cdecl;
function DosOpenQueue (var Parent_PID: cardinal; var Handle: THandle;
                       const Name: string): cardinal;

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
                      var Priority:byte;ASem:longint):cardinal; cdecl;
function DosPeekQueue (Handle: THandle; var ReqBuffer: TRequestData;
                       var DataLen: cardinal; var DataPtr: pointer;
                       var Element: cardinal; Wait: boolean;
                       var Priority: byte; ASem: THandle): cardinal;
function DosPeekQueue (Handle: THandle; var ReqBuffer: TRequestData;
                       var DataLen: cardinal; var DataPtr: pointer;
                       var Element: cardinal; Wait: cardinal;
                       var Priority: byte; ASem: THandle): cardinal; cdecl;

{Empty a queue. You must be the process the created the queue.}
function DosPurgeQueue (Handle: THandle): cardinal; cdecl;

{Return the number of elements in the queue.}
function DosQueryQueue(Handle:longint;var Count:longint):cardinal; cdecl;
function DosQueryQueue (Handle: THandle; var Count: cardinal): cardinal; cdecl;

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
                      ASem:longint):cardinal; cdecl;
function DosReadQueue (Handle: THandle; var ReqBuffer: TRequestData;
                      var DataLen: cardinal; var DataPtr: pointer;
                      Element, Wait: cardinal; var Priority: byte;
                      ASem: THandle): cardinal; cdecl;
function DosReadQueue (Handle: THandle; var ReqBuffer: TRequestData;
                      var DataLen: cardinal; var DataPtr: pointer;
                      Element: cardinal; Wait: boolean; var Priority: byte;
                      ASem: THandle): cardinal;

{Write a data record to a queue.
 Handle         = Handle of queue to write to.
 Request        = Value that will be inserted in the RequestData field when
                  element is read from queue.
 DataLen        = Size of data to write.
 DataBuf        = Data to write.
 Priority       = Priority of data in buffer. Only relevant when queue is
                  created with priority support.}
function DosWriteQueue (Handle: THandle; Request, Datalen: cardinal;
                        var DataBuf; Priority: cardinal): cardinal; cdecl;

{****************************************************************************

                        Error handling related routines.

****************************************************************************}

const   deHardErr           = 1;    {Hard errors are enabled, to disable
                                     do not give this switch.}
        deDisableExceptions = 2;    {Exceptions are disabled, to enable
                                     do not give this switch.}
{For compatibility with VP:}
        ferr_DisableHardErr = 0;    {Disable hard error popups.}
        ferr_EnableHardErr  = deHardErr;
        ferr_EnableException = 0;   {Enable exception popups.}
        ferr_DisableException = deDisableExceptions;

{Disable the end user notification of hardware errors and exceptions. Users
 can overide this in config.sys. By default, notification is enabled.
 There is no need for error checking if you can guarantee the parameter is
 correct.}
function DosError (Error: cardinal): cardinal; cdecl;

{Get information about an error code.
 It cannot fail, so it is written as procedure.

Code            = Error code to get info about.
_Class          = Receives the error class.
Action          = Receives the recommended action you should take.
Locus           = Receives what could have caused the error.}
procedure DosErrClass(Code:longint;var _Class,Action,Locus:longint); cdecl;
procedure DosErrClass (Code: cardinal; var _Class, Action, Locus: cardinal);
                                                                         cdecl;


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
                       var MsgSize:longint):cardinal;
function DosGetMessage (Table: PInsertTable; TableSize: cardinal; Buf: PChar;
                        BufSize, MsgNumber: cardinal; FileName: PChar;
                        var MsgSize: cardinal): cardinal; cdecl;
{And a variant using strings and open arrays.
function DosGetMessage(const Table:array of PString;var Buf:string;
                       BufSize,MsgNumber:longint;const FileName:PChar):cardinal;}

{And a variant using strings, but with a PChar buffer, because of long
 messages, and open arrays.
function DosGetMessage(const Table:array of PString;Buf:PChar;
                       BufSize,MsgNumber:longint;const FileName:string;
                       MsgSize:longint):cardinal;}

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
                          var DstMessageSize:longint):cardinal; cdecl;
function DosInsertMessage (Table: PInsertTable; TableSize: cardinal;
                           Message: PChar; SrcMessageSize: cardinal;
                           Buf: PChar; BufSize: cardinal;
                           var DstMessageSize: cardinal): cardinal; cdecl;
{And a variant using strings and open arrays.
function DosInsertMessage(Table:array of PString;
                          const Message:string;
                          var Buf:openstring):cardinal;}

{And a variant using strings, but with a PChar buffer, because of long
 messages, and open arrays.
function DosInsertMessage(Table:array of PString;
                          Message:PChar;SrcMessageSize:longint;
                          Buf:PChar;BufSize:longint;
                          var DstMessageSize:longint):cardinal;}

{Write a message to a file.
 Handle         = Handle of file.
 Size           = Size of message.
 Buf            = Buffer where message is located.}
function DosPutMessage (Handle: THandle; Size: cardinal; Buf: PChar): cardinal;
                                                                         cdecl;
function DosPutMessage (Handle: THandle; const Buf: string): cardinal;

{Get info about which codepages and languages a messagefile supports.
 Buf            = Receives information.
 BufSize        = Size of buffer.
 FileName       = Filename of message file.
 InfoSize       = Receives size in bytes of the returned info.}
function DosQueryMessageCP(var Buf;BufSize:longint;FileName:PChar;
                            var InfoSize:longint):cardinal;
function DosQueryMessageCP(var Buf;BufSize:longint;const FileName:string;
                            var InfoSize:longint):cardinal;
function DosQueryMessageCP (var Buf; BufSize: cardinal; FileName: PChar;
                            var InfoSize: cardinal): cardinal;
function DosQueryMessageCP (var Buf; BufSize: cardinal; const FileName: string;
                            var InfoSize: cardinal): cardinal;

{****************************************************************************

                           Session specific routines.

****************************************************************************}

const
{Start the new session independent or as a child.}
    ssf_Related_Independent = 0;    {Start new session independent
                                     of the calling session.}
    ssf_Related_Child       = 1;    {Start new session as a child
                                     session to the calling session.}

{Start the new session in the foreground or in the background.}
    ssf_FgBg_Fore           = 0;    {Start new session in foreground.}
    ssf_FgBg_Back           = 1;    {Start new session in background.}

{Should the program started in the new session
 be executed under conditions for tracing?}
    ssf_TraceOpt_None       = 0;    {No trace.}
    ssf_TraceOpt_Trace      = 1;    {Trace with no notification
                                     of descendants.}
    ssf_TraceOpt_TraceAll   = 2;    {Trace all descendant sessions.
                                     A termination queue must be
                                     supplied and Related must be
                                     ssf_Related_Child (=1).}

{Will the new session inherit open file handles
 and environment from the calling process.}
    ssf_InhertOpt_Shell     = 0;    {Inherit from the shell.}
    ssf_InhertOpt_Parent    = 1;    {Inherit from the calling process.}

{Specifies the type of session to start.}
    ssf_Type_Default        = 0;    {Use program's type.}
    ssf_Type_FullScreen     = 1;    {OS/2 full screen.}
    ssf_Type_WindowableVIO  = 2;    {OS/2 window.}
    ssf_Type_PM             = 3;    {Presentation Manager.}
    ssf_Type_VDM            = 4;    {DOS full screen.}
    ssf_Type_WindowedVDM    = 7;    {DOS window.}
{Additional values for Windows programs}
    Prog_31_StdSeamlessVDM    = 15; {Windows 3.1 program in its
                                     own windowed session.}
    Prog_31_StdSeamlessCommon = 16; {Windows 3.1 program in a
                                     common windowed session.}
    Prog_31_EnhSeamlessVDM    = 17; {Windows 3.1 program in enhanced
                                     compatibility mode in its own
                                     windowed session.}
    Prog_31_EnhSeamlessCommon = 18; {Windows 3.1 program in enhanced
                                     compatibility mode in a common
                                     windowed session.}
    Prog_31_Enh               = 19; {Windows 3.1 program in enhanced
                                     compatibility mode in a full
                                     screen session.}
    Prog_31_Std               = 20; {Windows 3.1 program in a full
                                     screen session.}

{Specifies the initial attributes for a OS/2 window or DOS window session.}
    ssf_Control_Visible      = 0;   {Window is visible.}
    ssf_Control_Invisible    = 1;   {Window is invisible.}
    ssf_Control_Maximize     = 2;   {Window is maximized.}
    ssf_Control_Minimize     = 4;   {Window is minimized.}
    ssf_Control_NoAutoClose  = 8;   {Window will not close after
                                     the program has ended.}
    ssf_Control_SetPos   = 32768;   {Use InitXPos, InitYPos,
                                     InitXSize, and InitYSize for
                                     the size and placement.}

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

{Queue data structure for synchronously started sessions.}
        TChildInfo = record
            case boolean of
             false:
               (SessionID,
                Return: word);  {Return code from the child process.}
             true:
               (usSessionID,
                usReturn: word);     {Return code from the child process.}
        end;
        PChildInfo = ^TChildInfo;


        TStartData=record
        {Note: to omit some fields, use a length smaller than
         SizeOf(TStartData).}
            Length:word;                {Length, in bytes, of datastructure
                                         (24/30/32/50/60).}
            Related:word;               {Independent/child session (0/1).}
            FgBg:word;                  {Foreground/background (0/1).}
            TraceOpt:word;              {No trace/trace this/trace all
                                         (0/1/2).}
            PgmTitle:PChar;             {Program title.}
            PgmName:PChar;              {Filename to program.}
            PgmInputs:PChar;            {Command parameters (nil allowed).}
            TermQ:PChar;                {System queue. (nil allowed).}
            Environment:PChar;          {Environment to pass (nil allowed).}
            InheritOpt:word;            {Inherit environment from shell/
                                         inherit environment from parent
                                         (0/1).}
            SessionType:word;           {Auto/full screen/window/presentation
                                         manager/full screen Dos/windowed Dos
                                         (0/1/2/3/4/5/6/7).}
            Iconfile:PChar;             {Icon file to use (nil allowed).}
            PgmHandle:cardinal;         {0 or the program handle.}
            PgmControl:word;            {Bitfield describing initial state
                                         of windowed sessions.}
            InitXPos,InitYPos:word;     {Initial top coordinates.}
            InitXSize,InitYSize:word;   {Initial size.}
            Reserved:word;
            ObjectBuffer:PChar;         {If a module cannot be loaded, its
                                         name will be returned here.}
            ObjectBuffLen:cardinal;     {Size of your buffer.}
        end;
        PStartData=^TStartData;
        StartData=TStartData;

{Start a new session.
 AStartData         = A startdata record.
 SesID              = Receives session ID of session created.
 PID                = Receives process ID of process created.}
function DosStartSession(var AStartData:TStartData;
                                         var SesID,PID:longint):cardinal;cdecl;
function DosStartSession (var AStartData: TStartData;
                          var SesID, PID: cardinal): cardinal; cdecl;

{Set the status of a child session.
 SesID              = ID of session.
 AStatus            = Status to set.}
function DosSetSession (SesID: cardinal; var AStatus: TStatusData): cardinal;
                                                                         cdecl;

{Bring a child session to the foreground.
 SesID              = ID of session.}
function DosSelectSession (SesID: cardinal): cardinal; cdecl;

{Terminate (a) child session(s).
 Scope              = 0 = Terminate specified session.
                      1 = Terminate all child sessions.
 SesID              = ID of session to terminate (ignored when terminating
                      all).}
function DosStopSession (Scope, SesID: cardinal): cardinal; cdecl;

{****************************************************************************

                     Named/unnamed pipe specific routines.

****************************************************************************}

type
  TAvailData = record
    cbPipe,              {Number of bytes in pipe.}
    cbMessage: word;     {Number of bytes in current message.}
  end;

  TPipeInfo = record
    cbOut: word;         {Size of outbound data.}
    cbIn: word;          {Size of inbound data.}
    MaxInst: byte;       {Maximum number of instances.}
    CurInst: byte;       {Current number of instances.}
    Name: string;        {Name of the pipe. You can use @Name[1] if
                          you need a PChar to the name; the string is
                          always followed by a zero.}
  end;

  TPipeSemState = record
    case boolean of
      false: (Status: byte;
              Flag: byte;
              Key: word;
              Avail: word);
      true:  (fStatus: byte;
              fFlag: byte;
              usKey: word;
              usAvail: word);
  end;
  PPipeSemState = ^TPipeSemState;
  TPipeSemStates = array [0..$FFFF] of TPipeSemState;

{Create an unnamed pipe.
 ReadHandle     = Receives handle for reading from pipe.
 WriteHandle    = Receives handle to write to pipe.
 Size           = Size of pipe to create. 0 means default size. If data is
                  written into a pipe that is smaller than the sent data, the
                  writing thread is suspended until the data has been read
                  from the pipe, thus making room for more data to send.}
function DosCreatePipe (var ReadHandle, WriteHandle: THandle;
                        Size: cardinal): cardinal; cdecl;

const   {np_XXXX constants for openmode.}
        np_Access_Inbound       = $0000;    {Client to server connection.}
        np_Access_Outbound      = $0001;    {Server to client access.}
        np_Access_Duplex        = $0002;    {Two way access.}
        np_Inherit              = $0000;    {Pipe handle is inherited by
                                             child processes.}
        np_NoInherit            = $0080;    {Pipe handle is _not_ inherited by
                                             child processes.}
        np_No_Write_Behind      = $4000;    {Don't allow write behind for
                                             remote pipes.}
        {np_XXXX constants for pipemode.}
        np_Unlimited_Instances  = $00ff;    {Unlimited instances.}
        np_ReadMode_Mesg        = $0100;    {Read the pipe as a message
                                             stream instead of as a byte
                                             stream.}
        np_ReadMode_Message     = np_ReadMode_Mesg;
        np_RMesg                = np_ReadMode_Message;
        np_WriteMode_Mesg       = $0400;    {Write the pipe as a message
                                             stream instead of as a byte
                                             stream.}
        np_WriteMode_Message    = np_WriteMode_Mesg;
        np_Type_Message         = np_WriteMode_Mesg;
        np_WMesg                = np_WriteMode_Mesg;
        np_Wait                 = 0;        { For compatibility only }
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
function DosCreateNPipe (Name: PChar; var Handle: THandle; OpenMode, PipeMode,
                         OutBufSize, InBufSize, MSec: cardinal): cardinal;
                                                                         cdecl;
function DosCreateNPipe (const Name: string; var Handle: THandle; OpenMode,
                         PipeMode, OutBufSize, InBufSize, MSec: cardinal):
                                                                      cardinal;

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
                      MSec:longint):cardinal; cdecl;
function DosCallNPipe(const Name:string;var Input;InputSize:longint;
                      var Output;OutputSize:longint;var ReadBytes:longint;
                      MSec:longint):cardinal;
function DosCallNPipe (Name: PChar; var Input; InputSize: cardinal;
                       var Output; OutputSize: cardinal;
                       var ReadBytes: cardinal; MSec: cardinal): cardinal;
                                                                         cdecl;
function DosCallNPipe (const Name: string; var Input; InputSize: cardinal;
                       var Output; OutputSize: cardinal;
                       var ReadBytes: cardinal; MSec: cardinal): cardinal;

{Prepare a named pipe for a client process.
 Handle         = Handle that was returned when pipe was created.}
function DosConnectNPipe (Handle: THandle): cardinal; cdecl;

{Acknowledges that a client process has closed a named pipe.
 Handle         = Handle that was returned when pipe was created.}
function DosDisconnectNPipe (Handle: THandle): cardinal; cdecl;

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
                      var State:longint):cardinal; cdecl;
function DosPeekNPipe (Handle: THandle; var Buffer; BufSize: cardinal;
                       var ReadBytes: cardinal; var Avail: TAvailData;
                       var State: cardinal): cardinal; cdecl;

{Get information about a named pipe handle.
 Handle         = Handle to pipe.
 State          = A combination of np_XXXX constants for (!!!) pipemode.}
function DosQueryNPHState(Handle:longint;var State:longint):cardinal; cdecl;
function DosQueryNPHState (Handle: THandle; var State: cardinal): cardinal;
                                                                         cdecl;

{Return information about a named pipe.
 Handle         = Handle to pipe.
 InfoLevel      = Level of information wanted (1 or 2 allowed).
 Buffer         = TPipeInfo datastructure for level 1.
                  Unique 4 byte identifier of the client for level 2. Only
                  used for LAN based pipe servers.}
function DosQueryNPipeInfo (Handle: THandle; InfoLevel: cardinal; var Buffer;
                            BufSize: cardinal): cardinal; cdecl;

{Return information of local named pipes that are attached to a semaphore.
 SemHandle      = Handle to a shared event or MuxWait semaphore that is
                  attached to a named pipe.
 SemArray       = Array in which for each pipe attached to the semaphore.
 BufSize        = Size of SemArray, in bytes.}
function DosQueryNPipeSemState (SemHandle: THandle; var SemArray;
                                BufSize: cardinal): cardinal; cdecl;
function DosQueryNPipeSemState (SemHandle: THandle; SemArray: PPipeSemState;
                                           BufSize: cardinal): cardinal; cdecl;

{Resets the blocking mode and state of a named pipe.
 Handle         = Handle to named pipe.
 State          = One of the np_XXXX constants for pipemode.}
function DosSetNPHState (Handle: THandle; State: cardinal): cardinal; cdecl;

{Attach a shared event semaphore to a local named pipe.
 PipeHandle     = Handle to named pipe.
 SemHandle      = Handle to semaphore.
 Key            = A key that must be different for each named pipe that is
                  attached to the semaphore.}
function DosSetNPipeSem (PipeHandle, SemHandle: THandle; Key: cardinal):
                                                               cardinal; cdecl;

{Write to a duplex named pipe; then read from it.
 Handle         = Handle to named pipe.
 OutBuf         = The data to write.
 OutSize        = Size of the data to write.
 InBuf          = Receives the read data.
 InSize         = Size of the input buffer.
 ReadBytes      = Number of bytes read from the pipe.}
function DosTransactNPipe(Handle:longint;var OutBuf;OutSize:longint;
                          var InBuf;InSize:longint;
                          var ReadBytes:longint):cardinal; cdecl;
function DosTransactNPipe (Handle: THandle; var OutBuf; OutSize: cardinal;
                           var InBuf; InSize: cardinal;
                           var ReadBytes: cardinal): cardinal; cdecl;

{Waits until an instance of a named pipe becomes available.
 Name           = Name of named pipe (always starts with '\PIPE\').
 MSec           = Return with an error code if this time has elapsed.}
function DosWaitNPipe (Name: PChar; MSec: cardinal): cardinal; cdecl;
function DosWaitNPipe (const Name: string; MSec: cardinal): cardinal;

{****************************************************************************

                    Virtual device driver related routines.

****************************************************************************}

{Open a virtual device driver.
 Name           = Name of virtual device driver.
 Handle         = Receives handle to virtual device driver.}
function DosOpenVDD (Name: PChar; var Handle: THandle): cardinal; cdecl;

{Request to talk with a virtual device driver.
 Handle         = Handle to virtual device driver.
 SGroup         = Handle to the screen group of a DOS session (may be nil).
 Cmd            = A number which indicates the service you call.
 InSize         = Size of the data to send to the VDD.
 InBuffer       = Buffer which contains the data to send to the VDD.
 OutSize        = Size of the buffer in which the VDD will return data.
 OutBuffer      = Receives the data that the VDD returns.}
function DosRequestVDD (Handle: THandle; SGroup, Cmd: cardinal;
                        InSize: cardinal; var InBuffer;
                        OutSize: cardinal; var OutBuffer): cardinal; cdecl;

{Close a virtual device driver.}
function DosCloseVDD (Handle: THandle): cardinal; cdecl;

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
function SelToFlat (AFarPtr: cardinal): pointer;
{The second variant can make use of the register calling convention.}

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
function DosAllocThreadLocalMemory (Count: cardinal; var P: pointer): cardinal;
                                                                         cdecl;

{Deallocate a previously allocated space in the thread local memory area.}
function DosFreeThreadLocalMemory (P: pointer): cardinal; cdecl;

const
{ Values for DosQueryRASInfo Index parameter }
  sis_MMIOAddr = 0;
  sis_MEC_Table = 1;
  sis_Sys_Log = 2;
{ The following one for compatibility only }
  SPU_SIS_MEC_TABLE = sis_MEC_Table;

{ Bit flags for the SYSLOG status word. }
  lf_LogEnable = 1;     { Logging enabled }
  lf_LogAvailable = 2;  { Logging available }

{ DosQueryRASInfo returns information about active trace event recording
  and System Logging facility from the Global Information Segment (InfoSegGDT)
  dump.}
{ Parameters
  * Index - one of the sis_* values:
    sis_MEC_Table - return the address of the table of actively traced major
      event codes in the InfoSegGDT. The table is 32 bytes long, each bit
      represents each major event code from 0 to 255.
    sis_Sys_Log - return the address of the SYSLOG status word from
      the InfoSegGDT. The status may contain a combination of lf_Log* flags
      defined above.}
{ Possible return codes:
   0 - No_Error
   5 - Error_Access_Denied
  87 - Error_Invalid_Parameter}
function DosQueryRASInfo (Index: cardinal; var PBuffer: pointer): cardinal;
                                                                         cdecl;

const
{ Logging constants }
  ErrLog_Service = 1;
  ErrLog_Version = 1;

{ LogRecord status bits }
  lf_Bit_ProcName = 1;    {used to indicate whether the current error log}
                          {entry packet contains space in which the error}
                          {logging facility can place a long process name}
                          {("on" indicates YES, "off" indicates NO)      }
  lf_Bit_Origin_256 = 2;  {used to indicate whether the current error log }
                          {entry packet contains an 8 byte originator name}
                          {or a 256 byte originator name ("on" indicates  }
                          {a 256 byte originator name, "off" indicates an }
                          {8 byte originator name)                        }
  lf_Bit_DateTime = 4;    {used to indicate that the caller has placed time}
                          {and date values in the Error Log entry packet   }
                          {and does not wish to have those values modified }
                          {during the logging process ("on" indicates that }
                          {the error log entry packet already contains time}
                          {and date values, "off" indicates the packet does}
                          {not already contain time and date values)       }
  lf_Bit_Suspend = 8;
  lf_Bit_Resume = 16;
  lf_Bit_Redirect = 32;
  lf_Bit_GetStatus = 64;
  lf_Bit_Register = 128;
  lf_Bit_Remote_Fail = 256;

type
{ Log entry record header for OS/2 2.x and above used    }
{ by 32-bit device drivers and callers of LogAddEntries. }
  TLogRecord = record
    Len: word;          { length of this record (including the Len field) }
    Rec_ID: word;       { record ID }
    Status: cardinal;   { record status bits (see lf_Bit_* constants) }
    Qualifier: array [1..4] of char;    { qualifier tag }
    Reserved: cardinal;
    Time: cardinal;     { hours, minutes, seconds, hundreds }
    Date: cardinal;     { day, month, year (stored as word) }
    case byte of
     0: (Data: array [1..3400] of char); { variable data (up to 3400 bytes); }
                                         { beginning of this area must match }
                                         { one of the following patterns     }
     1: (Originator256: array [0..255] of char;     {Originator - if the flag }
                                                    {lf_Bit_Origin_256 is set }
         ProcessName_O256: array [1..260] of char;  {if lf_Bit_ProcName is set}
         FormatDLLName_O256_ProcName: array [1..12] of char;  {ASCIIZ DLL name}
         Data_O256_ProcName: array [1..3400] of char);        {Variable data  }
     2: (Originator256b: array [0..255] of char;
         FormatDLLName_O256: array [1..12] of char;
         Data_O256: array [1..3400] of char);
     3: (Originator8: array [0..7] of char;      {Originator - if flag     }
                                                 {lf_Bit_Origin_256 clear  }
         ProcessName_O8: array [1..260] of char; {if lf_Bit_ProcName is set}
         FormatDLLName_O8_ProcName: array [1..12] of char;
         Data_O8_ProcName: array [1..3400] of char);
     4: (Originator8b: array [0..7] of char;
         FormatDLLName_O8: array [1..12] of char;
         Data_O8: array [1..3400] of char);
  end;
  LogRecord = TLogRecord;
  PLogRecord = ^TLogRecord;

{ Format of buffer sent to LogAddEntries }
  TLogEntryRec = record
    Version: word;                      {this version is 1}
    Count: word;                        {number of log records in this buffer}
    LogRec: array [0..0] of TLogRecord; {repeated count times}
  end;
  LogEntryRec = TLogEntryRec;
  PLogEntryRec = ^TLogEntryRec;

{ Logging facility functions }
{ Open a connection to the system error logging facility (through the system
  logging service device driver). }
{ Possible return codes:
  0 .......... success
  non-zero ... facility not available }
function LogOpen (var Handle: cardinal): cardinal; cdecl;

{ Close the connection to the to the system error logging facility. }
{ Possible return codes:
  0 .......... success
  non-zero ... failure (possible reason - facility not open)}
function LogClose (Handle: cardinal): cardinal; cdecl;

{ Add error log entries to the internal error log buffer maintained by
  the system logging service device driver.}
{ Parameters:
  Handle - handle returned by previous LogOpen
  Service - specifies the class of logging facility:
    0 ........... reserved
    1 ........... error logging
    2 - $FFFF ... reserved
  LogEntries - buffer containing a variable length error log entry. The first
    word of the buffer contains the number of packets in the error log entry.
    Multiple error log packets (LogRec structure) can be included within
    a single error log entry buffer. If multiple packets are included within
    a buffer, each individual packet should be aligned on a double word
    boundary.

    Version - packet revision number. Can be used to distinguish error logging
              packets that are intended to be handled by different revisions
              of the LogAddEntries API. For the initial version of the API
              (all OS/2 versions from OS/2 v2.0 up to WSeB and eComStation),
              this field should be set to a value of 1. This field is included
              in the packet to support future backward compatibility.
    Count - number of separate error log entry packets contained within
            the user's buffer
    Len - length of this error log entry packet (LogRec) within the user's
          error log entry buffer in bytes (this length includes the length
          of all the error log entry packet control fields and the size
          of the error log entry text). To support efficient logging execution,
          this length should be a multiple of 4 bytes (i.e. if necessary
          the user should pad the error log entry packet).
    Rec_ID - error log record ID for the current error log entry
             (ID registration will be statically registered by the OS/2
             development organization)
    Status - status flags (two byte flag holder containing three single bit
             flags lf_Bit_* - all the other 29 bits in status flags
             are considered reserved at this time and will be zeroed by
             the LogAddEntries API)
    Qualifier - qualifier name is a secondary name field that is provided
                by the caller
    Reserved - four byte reserved field
    Time - time of logging, filled in by the system error logging facility
           (unless lf_Bit_DateTime status flag is set to "on", indicating
           that the caller has preset a time value)
    Date - date of logging, filled in by the system error logging facility
           (unless lf_Bit_DateTime status flag is set to "on", indicating
           that the caller has preset a date value)
    Originator* - originator name (8 or 256 characters depending on Status),
                  a primary name field provided by the caller
    ProcName* - process name (0 or 260 characters), an optional long process
                name field that will be filled in by the error logging facility
                if the field is provided by the caller in the error log entry
                packet
    FormatDLLName* - formatting DLL module name (optional); the optional name
                     of a DLL module that houses a formatting routine that
                     recognizes this type of error log entry and can format it
                     for display by the SYSLOG utility. The name is specified
                     as an ASCIIZ string that can be up to eight characters
                     in length. If no module name is specified in this field,
                     then SYSLOG will display the data portion of the error log
                     entry as a hexadecimal dump.
    Data* - error log entry data (up to 3400 characters / bytes); an optional
            variable length set of data that can be supplied by the caller
            (the format of the string is under the control of the caller)
}
{ Possible return codes:
    0        - success
    non-zero - failure (invalid log type, facility unavailable, facility
               suspended, facility not open, error log buffer temporarily full)
}
function LogAddEntries (Handle: cardinal; Service: cardinal;
                                     LogEntries: PLogEntryRec): cardinal; cdecl;

function LogAddEntries (Handle: cardinal; Service: cardinal;
                                var LogEntries: TLogEntryRec): cardinal; cdecl;


function DosReplaceModule (OldModule, NewModule, BackupModule: PChar):
                                                               cardinal; cdecl;


const
{ Flags allowed for DosQuerySysState parameter EntityList
  (multiple flags may be combined together) and also values
  used as markers of records returned in the provided buffer:
}
  qs_End = 0;           { Marker for the last TFileSys record }
  qs_Process = 1;       { Requests process information }
  qs_Semaphore = 2;     { Requests semaphore information }
  qs_MTE = 4;           { Requests module information }
  qs_FileSys = 8;       { Requests file system information }
  qs_ShMemory = 16;     { Requests shared memory information }
  qs_Disk = 32;         { Not supported apparently? }
  qs_HwConfig = 64;     { Not supported apparently? }
  qs_NamedPipe = 128;   { Not supported apparently? }
  qs_Thread = 256;      { Not supported apparently? }
  qs_ModVer = 512;      { Requests module version information }

  qs_Supported = qs_Process or qs_Semaphore or qs_MTE or QS_FileSys or
                                                      qs_ShMemory or qs_ModVer;

type
{ Global record
  Holds all global system information. Placed first in user buffer
}
  PQSGRec = ^TQSGRec;
  TQSGRec = record
    cThrds,
    c32SSem,
    cMFTNodes: cardinal;
  end;

{ Thread record
  Holds all per thread information.
}
  PQSTRec = ^TQSTRec;
  TQSTRec = record
    RecType: cardinal;              { Record type }
    TID: word;                      { Thread ID }
    Slot: word;                     { "Unique" thread slot number }
    SleepID: cardinal;              { Sleep ID thread is sleeping on }
    case boolean of
     false: (
      Priority: cardinal;           { Thread priority (class + level) }
      SysTime: cardinal;            { Thread system time }
      UserTime: cardinal;           { Thread user time }
      State: byte;                  { Thread state }
      Pad: array [1..3] of byte);   { Padding for 32-bit alignment }
     true: (
      PrioLevel: byte;              { Thread priority level only }
      PrioClass: byte;              { Thread priority class only }
      Pad2: array [1..14] of byte);
  end;

{ Process record
  Holds all per process information.
      ________________________________
      |       RecType                 |
      |-------------------------------|
      |       pThrdRec                |----|
      |-------------------------------|    |
      |       pid                     |    |
      |-------------------------------|    |
      |       ppid                    |    |
      |-------------------------------|    |
      |       type                    |    |
      |-------------------------------|    |
      |       stat                    |    |
      |-------------------------------|    |
      |       sgid                    |    |
      |-------------------------------|    |
      |       hMte                    |    |
      |-------------------------------|    |
      |       cTCB                    |    |
      |-------------------------------|    |
      |       c32PSem                 |    |
      |-------------------------------|    |
      |       p32SemRec               |----|---|
      |-------------------------------|    |   |
      |       c16Sem                  |    |   |
      |-------------------------------|    |   |
      |       cLib                    |    |   |
      |-------------------------------|    |   |
      |       cShrMem                 |    |   |
      |-------------------------------|    |   |
      |       cFS                     |    |   |
      |-------------------------------|    |   |
      |       p16SemRec               |----|---|----|
      |-------------------------------|    |   |    |
      |       pLibRec                 |----|---|----|------|
      |-------------------------------|    |   |    |      |
      |       pShrMemRec              |----|---|----|------|----|
      |-------------------------------|    |   |    |      |    |
      |       pFSRec                  |----|---|----|------|----|-----|
      |-------------------------------|    |   |    |      |    |     |
      |       32SemPPRUN[0]           |<---|---|    |      |    |     |
      |          .                    |    |        |      |    |     |
      |          .                    |    |        |      |    |     |
      |          .                    |    |        |      |    |     |
      |       32SemPPRUN[c32PSem-1]   |    |        |      |    |     |
      |-------------------------------|    |        |      |    |     |
      |       16SemIndx[0]            |<---|--------|      |    |     |
      |          .                    |    |               |    |     |
      |          .                    |    |               |    |     |
      |          .                    |    |               |    |     |
      |       16SemIndx[c16Sem-1]     |    |               |    |     |
      |-------------------------------|    |               |    |     |
      |       hmte[0] (or "name str") |<---|---------------|    |     |
      |          .                    |    |                    |     |
      |          .                    |    |                    |     |
      |          .                    |    |                    |     |
      |       hmte[cLib-1]            |    |                    |     |
      |-------------------------------|    |                    |     |
      |       hshmem[0]               |<---|--------------------|     |
      |          .                    |    |                          |
      |          .                    |    |                          |
      |          .                    |    |                          |
      |       hshmem[cShrMem-1]       |    |                          |
      |-------------------------------|    |                          |
      |       fsinfo[0]               |<---|--------------------------|
      |          .                    |    |
      |          .                    |    |
      |          .                    |    |
      |       fsinfo[cFS-1]           |    |
      |-------------------------------|    |
                                      <-----
      NOTE that the process name string will be stored in place of hmtes
              if MTE information is NOT being requested.
      NOTE that following this structure in the user buffer is
              an array c32Sems long of PRUN structures for 32 bit sems
              an array c16Sems long of indices for 16 bit sems
              the process name string
}
  PQSPRec = ^TQSPRec;
  TQSPrec = record
    RecType: cardinal;     { Type of record being processed }
    PThrdRec: PQSTRec;     { (Far?) pointer to thread records for this process }
    PID: word;             { Process ID }
    PPID: word;            { Parent process ID }
    ProcType: cardinal;    { Process type }
    Stat: cardinal;        { Process status }
    SGID: cardinal;        { Process screen group }
    hMte: word;            { Program module handle for process }
    cTCB: word;            { Number of TCBs (Thread Control Blocks) in use }
    c32PSem: cardinal;     { Number of private 32-bit semaphores in use }
    p32SemRec: pointer;    { (Far?) pointer to head of 32-bit semaphores info }
    c16Sem: word;          { Number of 16 bit system semaphores in use }
    cLib: word;            { Number of runtime linked libraries }
    cShrMem: word;         { Number of shared memory handles }
    cFH: word;             { Number of open files }
                           { NOTE: cFH is size of active part of   }
                           { the handle table if QS_FILE specified }
    p16SemRec: word;       { Far pointer? to head of 16-bit semaphores info }
    pLibRec: word;         { Far pointer? to list of runtime libraries }
    pShrMemRec: word;      { Far pointer? to list of shared memory handles }
    pFSRec: word;          { Far pointer to list of file handles; }
                           { 0xFFFF means it's closed, otherwise }
                           { it's an SFN if non-zero }
  end;

{     16-bit system semaphore record
      ________________________________
      |       pNextRec                |----|
      |-------------------------------|    |
      |SysSemData     :               |    |
      |       SysSemOwner             |    |
      |       SysSemFlag              |    |
      |       SysSemRecCnt            |    |
      |       SysSemProcCnt           |    |
      |-------------------------------|    |
      |-------------------------------|    |
      |-------------------------------|    |
      |       SysSemPtr               |    |
      |-------------------------------|    |
      |SysSemName:                    |    |
      |       "pathname"              |    |
      |-------------------------------|    |
                                      <-----
}

const
{ Values for SysSemFlag: }
  qs_SysSem_Waiting = 1;            { A thread is waiting on the semaphore }
  qs_SysSem_MuxWaiting = 2;         { A thread is muxwaiting on the semaphore }
  qs_SysSem_Owner_Died = 4;         { The process/thread owning the sem died }
  qs_SysSem_Exclusive = 8;          { Indicates an exclusive system semaphore }
  qs_SysSem_Name_Cleanup = 16;      { Name table entry needs to be removed }
  qs_SysSem_Thread_Owner_Died = 32; { The thread owning the semaphore died }
  qs_SysSem_ExitList_Owner = 64;    { The exitlist thread owns the semaphore }

type
  PQSS16Rec = ^TQSS16Rec;
  TQSS16Rec = record
    NextRec: cardinal;              { Offset to next record in buffer }
                                    { System Semaphore Table Structure }
    SysSemOwner: word;              { Thread owning this semaphore }
    SysSemFlag: byte;               { System semaphore flag bit field }
    SysSemRefCnt: byte;             { Number of references to this system semaphore }
    SysSemProcCnt: byte;            { Number of requests for this owner }
    SysSemPad: byte;                { Pad byte to round structure up to word }
    Pad_sh: word;                   { Padding for 32-bit alignment }
    SemPtr: word;                   { RMP SysSemPtr field }
    SemName: array [0..0] of char;  { Start of semaphore name string }
  end;

  PQSS16HeadRec = ^TQSS16HeadRec;
  TQSS16HeadRec = record
    SRecType: cardinal; { Offset of SysSemDataTable }
    SpNextRec:cardinal; { Overlays NextRec of 1st PQSS16Rec }
    S32SemRec: cardinal;
    S16TblOff: cardinal;
    pSem16Rec: cardinal;
  end;

{     System-wide shared memory information
      ________________________________
      |       NextRec                 |
      |-------------------------------|
      |       hMem                    |
      |-------------------------------|
      |       Sel                     |
      |-------------------------------|
      |       RefCnt                  |
      |-------------------------------|
      |       Name                    |
      |_______________________________|
}
  PQSMRec = ^TQSMRec;
  TQSMRec = record
    MemNextRec: cardinal;           { Offset to next record in buffer }
    hMem: word;                     { Handle for shared memory }
    Sel: word;                      { Selector }
    RefCnt: word;                   { Reference count }
    MemName: array [0..0] of char;  { Start of shared memory name string }
  end;

{     32-bit system semaphore record
      ________________________________
      |       pNextRec                |----|
      |-------------------------------|    |
      |       QSHUN[0]                |    |
      |-------------------------------|    |
      |         MuxQ                  |    |
      |-------------------------------|    |
      |         OpenQ                 |    |
      |-------------------------------|    |
      |         SemName               |    |
      |-------------------------------|<---|
      |          .                    |
      |          .                    |
      |-------------------------------|<---|
      |       pNextRec                |----|
      |-------------------------------|    |
      |       QSHUN[c32SSem-1]        |    |
      |-------------------------------|    |
      |         MuxQ                  |    |
      |-------------------------------|    |
      |         OpenQ                 |    |
      |-------------------------------|    |
      |         SemName               |    |
      |-------------------------------|<---|
}
const
{ 32-bit semaphore flags }
  qs_dc_Sem_Shared = 1;     { Shared Mutex, Event or MUX semaphore }
  qs_dcmw_Wait_Any = 2;     { Wait on any event/mutex to occur }
  qs_dcmw_Wait_All = 4;     { Wait on all events/mutexs to occur }
  qs_dcm_Mutex_Sem = 8;     { Mutex semaphore }
  qs_dce_Event_Sem = 16;    { Event semaphore }
  qs_dcmw_Mux_Sem  = 32;    { Muxwait semaphore }
  qs_dc_Sem_PM     = 64;    { PM Shared Event Semphore }
  qs_de_Posted     = 64;    { Event semaphore is in the posted state }
  qs_dm_Owner_Died = 128;   { The owning process died }
  qs_dmw_Mtx_Mux   = 256;   { MUX contains mutex semaphores }
  qs_dho_Sem_Open  = 512;   { Device drivers have opened this semaphore }
  qs_de_16Bit_MW   = 1024;  { Part of a 16-bit MuxWait }
  qs_dce_PostOne   = 2048;  { Post one flag event semaphore }
  qs_dce_AutoReset = 4096;  { Auto-reset event semaphore }

type
  PQSOpenQ = ^TQSOpenQ;
  TQSOpenQ = record
    PidOpener: word;        { Process ID of the opening process }
    OpenCt: word;           { Number of opens for this process }
  end;

  PQSEvent = ^TQSEvent;
  TQSEvent = record
    pOpenQ: PQSOpenQ;       { Pointer to open Queue entries }
    pName: PChar;           { Pointer to semaphore name }
    pMuxQ: PCardinal;       { Pointer to the mux queue }
    Flags: word;
    PostCt: word;           { Number of posts }
  end;

  PQSMutex = ^TQSMutex;
  TQSMutex = record
    pOpenQ: PQSOpenQ;       { Pointer to open queue entries }
    pName: PChar;           { Pointer to semaphore name }
    pMuxQ: PCardinal;       { Pointer to the mux queue }
    Flags: word;
    ReqCt: word;            { Number of requests }
    SlotNum: word;          { Slot number of the owning thread }
    Padding: word;
  end;

  PQSMux = ^TQSMux;
  TQSMux = record
    pOpenQ: PQSOpenQ;       { Pointer to open queue entries }
    pName: PChar;           { Pointer to semaphore name }
    pSemRec: PSemArray;     { Array of semaphore record entries }
    Flags: word;
    cSemRec: word;          { Count of semaphore records }
    WaitCt: word;           { Number of threads waiting on the mux }
    Padding: word;
  end;

  PQSSHUN = ^TQSSHUN;
  TQSSHUN = record
    qsSEvt: PQSEvent;       { Shared event semaphore }
    qsSMtx: PQSMutex;       { Shared mutex semaphore }
    qsSMux: PQSMux;         { Shared mux semaphore }
  end;

  PQSS32Rec = ^TQSS32Rec;
  TQSS32Rec = record
    pNextRec: PQSS32Rec;    { Pointer to the next record in buffer }
    qsh: PQSSHUN;           { QState version of SHUN record }
  end;

{     System wide MTE information
      ________________________________
      |       pNextRec                |----|
      |-------------------------------|    |
      |       hmte                    |    |
      |-------------------------------|    |
      |       ctImpMod                |    |
      |-------------------------------|    |
      |       ctObj                   |    |
      |-------------------------------|    |
      |       pObjInfo                |----|----------|
      |-------------------------------|    |          |
      |       pName                   |----|----|     |
      |-------------------------------|    |    |     |
      |       imported module handles |    |    |     |
      |          .                    |    |    |     |
      |          .                    |    |    |     |
      |          .                    |    |    |     |
      |-------------------------------| <--|----|     |
      |       "pathname"              |    |          |
      |-------------------------------| <--|----------|
      |       Object records          |    |
      |       (if requested)          |    |
      |_______________________________|    |
                                      <-----
  NOTE that if the level bit is set to qs_MTE, the base Lib record
  will be followed by a series of object records (TQSLObjRec); one for each
  object of the module.
}
  PQSLObjRec = ^TQSLObjRec;
  TQSLObjRec = record
    OAddr: cardinal;        { Object address }
    OSize: cardinal;        { Object size }
    OFlags: cardinal;       { Object flags }
  end;

  PQSLRec = ^TQSLRec;
  TQSLRec = record
    pNextRec: PQSLRec;      { (Far?) pointer to the next record in buffer }
    hMTE: word;             { Handle for this MTE }
    fFlat: word;            { True if 32 bit module }
    ctImpMod: cardinal;     { Number of imported modules in table }
    ctObj: cardinal;        { Number of objects in module (MTE_ObjCnt) }
    pObjInfo: PQSLObjRec;   { (Far?) pointer to per object information if any }
    pName: PChar;           { (Far?) pointer to name string following record }
  end;

  PQSExLRec = ^TQSExLRec;
  TQSExLRec = record        { Used for 9th bit (Extended Module Data Summary) }
    Next: PQSExLRec;        { Pointer to next Extended Module Data }
    hndMod: word;           { Module Handle }
    PID: word;              { Process ID }
    ModType: word;          { Type of Module }
    RefCnt: cardinal;       { Size of reference array }
    SegCnt: cardinal;       { Number of segments in module }
    _reserved_: pointer;
    Name: PChar;            { (Far?) pointer to Module Name }
    ModuleVersion: cardinal;{ Module version value }
    ShortModName: PChar;    { (Far?) new pointer to module short name }
    ModRef: cardinal;       { Start of array of handles of module }
  end;

{     System wide FILE information
      ________________________________
      |       RecType                 |
      |-------------------------------|
      |       pNextRec                |-------|
      |-------------------------------|       |
      |       ctSft                   |       |
      |-------------------------------|       |
      |       pSft                    |---|   |
      |-------------------------------|   |   |
      |       name                    |   |   |
      |-------------------------------|<--|   |
      |       qsSft[0]                |       |
      |-------------------------------|       |
      |       ...                     |       |
      |-------------------------------|       |
      |       qsSft[ctSft -1]         |       |
      |_______________________________|       |
      |       name                    |       |
      |_______________________________|       |
                                      <-------|
}
  PQSSft = ^TQSSft;
  TQSSft = record
    SFN: word;              { SFN sf_fsi.sfi_selfSFN }
    RefCnt: word;           { sf_ref_count }
    Flags: word;            { sf_flags }
    Flags2: word;           { sf_flags2 }
    Mode: word;             { sf_fsi.sfi_mode - mode of access }
    Mode2: word;            { sf_fsi.sfi_mode2 - mode of access }
    Size: cardinal;         { sf_fsi.sfi_size }
    hVPB: word;             { sf_fsi.sfi_hVPB handle of volume }
    Attr: word;             { sf_attr }
    Padding: word;
  end;

  PQSFRec = ^TQSFRec;
  TQSFRec = record
    RecType: cardinal;      { Record Type }
    pNextRec: PQSFRec;      { Pointer to the next record in buffer }
    ctSft: cardinal;        { Number of SFT entries for this MFT entry }
    pSft: PQSSft;           { Pointer to start of SFT entries in buffer }
  end;

{ Pointer record
  This structure is the first in the user buffer.
  It contains pointers to heads of record types that are loaded
  into the buffer.
}
  PQSPtrRec = ^TQSPtrRec;
  TQSPtrRec = record
    PGlobalRec: PQSGRec;
    PProcRec: PQSPRec;          { Pointer to head of process records }
    P16SemRec: PQSS16HeadRec;   { Pointer to head of 16-bit semaphore records }
    P32SemRec: PQSS32Rec;       { Pointer to head of 32-bit semaphore records }
    PMemRec: PQSMRec;           { ???Pointer to head of shared memory records? }
    PLibRec: PQSLRec;           { Pointer to head of MTE records }
    PShrMemRec: PQSMRec;        { ???Pointer to head of shared memory records? }
    PFSRec: PQSFRec;            { Pointer to head of file system records }
  end;

{DosQuerySysState returns information about various resources in use
 by the system. The EntityList parameter determines which information
 is returned according to the bits set in this parameter.
 Note that this API is fairly low-level and it hasn't been part
 of the official IBM documentation until WSeB (and even then the documentation
 was highly incomplete, so}
{Parameters:
 EntityList    Determines what information is returned. May be a combination
               one or more qs_* flags.
 EntityLevel   Determines the extent of information returned for a given
               entity. This applies to qs_MTE entities only. If EntityLevel is
               also set to qs_MTE, then module object information is returned.
 PID           Restricts information to a particular process ID. If 0 is
               specified, then entities for all processes are returned.
 TID           Restricts information to a particular thread ID. A value of zero
               only is supported, requesting all threads of a process.
 Buffer        Buffer allocated by the user into which entity structures are
               returned. If the buffer is of insufficient size, then an
               Error_Buffer_Overflow is returned.
 BufLen        Size of the buffer pointed to by Buffer in bytes.
}
function DosQuerySysState (EntityList, EntityLevel, PID, TID: cardinal;
                                var Buffer; BufLen: cardinal): cardinal; cdecl;

function DosQuerySysState (EntityList, EntityLevel, PID, TID: cardinal;
                          PDataBuf: pointer; cbBuf: cardinal): cardinal; cdecl;


{
 Creates a private Read/Write alias or an LDT code segment alias to part
 of an existing memory object. The alias object is accessible only to the
 process that created it. The original object must be accessible to the caller
 of DosAliasMem.

 An alias is removed by calling DosFreeMem with the alias address.

 Although it is possible to create a Read/Write alias to a code segment
 to allow code modification, this is not recommended. On Pentium processors,
 and later, pipe-lining techniques used by the processor might allow
 the processor not to be aware of the modified code, if appropriate
 pipe-line serialization is not performed by the programmer. For further
 information see the processor documentation.

 Possible return values:
     0 No_Error
     8 Error_Not_Enough_Memory
    87 Error_Invalid_Parameter
    95 Error_Interrupt
 32798 Error_Crosses_Object_Boundary

pMem   = Pointer to the memory to be aliased. It must be on a page boundary
         (i.e. aligned to 4 kB), but may specify an address within a memory
         object.
Size   = Specifies size in bytes for the memory to alias. The entire range
         must lie within a single memory object and must be committed
         if OBJ_SELMAPALL is specified.
Alias  = Pointer where the address of the aliased memory is returned.
         The corresponding LDT selector is not explicitly returned but may be
         calculated by using the Compatibility Mapping Algorithm
         ((Alias shr 13) or 7).
Flags  = Combination of the following values:
            obj_SelMapAll = $800 (Create a Read/Write 32 bit alias
                                  to the address specified. The entire range
                                  must be committed, start on page boundary
                                  and be within the extent of a single memory
                                  object. An LDT selector is created to map
                                  the entire range specified. If obj_SelMapAll
                                  is not specified, then size is rounded up
                                  to a 4K multiple and the alias created
                                  inherits the permissions from the pages
                                  of the original object.)
            obj_Tile      =  $40  (Obj_Tile may be specified, but currently
                                   this is enforced whether or not specified.
                                   This forces LDT selectors to be based
                                   on 64K boundaries.)
            sel_Code      =    1  (Marks the LDT alias selector(s)
                                   Read-Executable code selectors.)
            sel_Use32     =    2  (Used with obj_SelMapAll, otherwise ignored.
                                   Marks the first alias LDT selector
                                   as a 32 bit selector by setting the BIG/C32
                                   bit.)
}
function DosAliasMem (pMem: pointer; Size: cardinal; var Alias: pointer;
                                             Flags: cardinal): cardinal; cdecl;

const
  PAG_INVALID     =  0; (* Page is invalid. *)
  PAG_NPOUT       =  0; (* Page is not present, not in core. *)
  PAG_PRESENT     =  1; (* Page is present. *)
  PAG_NPIN        =  2; (* Page is not present, in core. *)
  PAG_PRESMASK    =  3; (* Present state mask. *)
  PAG_RESIDENT    = 16; (* Page is resident. *)
  PAG_SWAPPABLE   = 32; (* Page is swappable. *)
  PAG_DISCARDABLE = 48; (* Page is discardable. *)
  PAG_TYPEMASK    = 48; (* Typemask *)

{
DosQueryMemState gets the status of a range of pages in memory. Its input
parameters are an address and size. The address is rounded down to page
boundary and size is rounded up to a whole number of pages. The status
of the pages in the range is returned in the state parameter, and the size
of the range queried is returned in the size parameter. If the pages
in the range have conflicting states, then the state of the first page
is returned.

Parameters:
PMem - Pointer to memory to be queried
Size - Size of memory to be queried
State - Flags (PAG_*) indicating state of the memory (or the first memory page
        if the states for the following pages differ):

Possible return codes:
  0 - NO_ERROR 
 87 - ERROR_INVALID_PARAMETER 
487 - ERROR_INVALID_ADDRESS 
}
function DosQueryMemState (PMem: pointer; var Size: cardinal;
                                         var State: cardinal): cardinal; cdecl;

{***************************************************************************}
implementation
{***************************************************************************}

function DosCreateThread(var TID:longint;Address:TThreadEntry;
                          aParam:pointer;Flags:longint;
                          StackSize:longint):cardinal; cdecl;
external 'DOSCALLS' index 311;

function DosCreateThread (var TID: cardinal; Address: TThreadEntry;
                          aParam: pointer; Flags: cardinal;
                          StackSize: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 311;

function DosCreateThread(var TID:longint;Address:pointer;
                        AParam:Pointer;Flags,StackSize:longint):cardinal;cdecl;
external 'DOSCALLS' index 311;

function DosCreateThread (var TID: cardinal; Address: pointer;
                 AParam: Pointer; Flags, StackSize: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 311;

function DosSuspendThread (TID:cardinal): cardinal; cdecl;
external 'DOSCALLS' index 238;

function DosResumeThread (TID: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 237;

function DosKillThread (TID: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 111;

function DosWaitThread(var TID:longint;Option:longint):cardinal; cdecl;
external 'DOSCALLS' index 349;

function DosWaitThread (var TID: cardinal; Option: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 349;

function DosEnterCritSec: cardinal; cdecl;
external 'DOSCALLS' index 232;

function DosExitCritSec: cardinal; cdecl;
external 'DOSCALLS' index 233;

procedure DosExit (Action, Result: cardinal); cdecl;
external 'DOSCALLS' index 234;

procedure DosGetInfoBlocks(var ATIB:PThreadInfoBlock;
                           var APIB:PProcessInfoBlock); cdecl;
external 'DOSCALLS' index 312;

procedure DosGetInfoBlocks(PATIB:PPThreadInfoBlock;
                           PAPIB:PPProcessInfoBlock); cdecl;
external 'DOSCALLS' index 312;

procedure DosSleep (MSec: cardinal); cdecl;
external 'DOSCALLS' index 229;

function DosBeep (Freq, MS: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 286;

function DosDebug (DebugBuf: PDbgBuf): cardinal; cdecl;
external 'DOSCALLS' index 317;

function DosDebug (var APDbgBuf: TDbgBuf): cardinal; cdecl;
external 'DOSCALLS' index 317;

function DosExitList (OrderCode: cardinal; Proc: TExitProc): cardinal; cdecl;
external 'DOSCALLS' index 296;

function DosExecPgm (ObjName: PChar; ObjLen: longint; ExecFlag: cardinal;
                     Args, Env: PByteArray; var Res: TResultCodes;
                     FileName:PChar): cardinal; cdecl;
external 'DOSCALLS' index 283;

function DosExecPgm (var ObjName: string; Execflag: cardinal;
                     Args, Env: PByteArray; var Res: TResultCodes;
                     const FileName: string): cardinal;
var T,T2:array[0..255] of char;

begin
    StrPCopy(@T,FileName);
    DosExecPgm:=DosExecPgm(@T2,SizeOf(T2),ExecFlag,Args,Env,Res,@T);;
    ObjName:=StrPas(@T2);
end;

function DosWaitChild(Action,Option:longint;var Res:TResultCodes;
                      var TermPID:longint;PID:longint):cardinal; cdecl;
external 'DOSCALLS' index 280;

function DosWaitChild (Action, Option: cardinal; var Res: TResultCodes;
                       var TermPID: cardinal; PID: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 280;

function DosSetPriority (Scope, TrClass: cardinal; Delta: longint;
                                            PortID: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 236;

function DosKillProcess (Action, PID: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 235;

function DosQueryAppType(FileName:PChar;var Flags:longint):cardinal; cdecl;
external 'DOSCALLS' index 323;

function DosQueryAppType (FileName: PChar; var Flags: cardinal): cardinal;
                                                                         cdecl;
external 'DOSCALLS' index 323;

function DosDevConfig (var DevInfo: byte; Item: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 231;

function DosSetFileLocks (Handle: THandle; var Unlock, Lock: TFileLock;
                          Timeout, Flags: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 428;

function DosCancelLockRequest (Handle: THandle; var Lock: TFileLock): cardinal;
                                                                         cdecl;
external 'DOSCALLS' index 429;

function DosOpen(FileName:PChar;var Handle,Action:longint;
                 InitSize,Attrib,OpenFlags,FileMode:longint;
                 EA:PEAOp2):cardinal; cdecl;
external 'DOSCALLS' index 273;

function DosOpen (FileName: PChar; var Handle: THandle; var Action: cardinal;
                  InitSize, Attrib, OpenFlags, FileMode: cardinal;
                  EA: PEAOp2): cardinal; cdecl;
external 'DOSCALLS' index 273;

function DosCreate (FileName: PChar; var Handle: THandle;
                    Attrib, OpenMode: cardinal): cardinal;

var Action: cardinal;

begin
    DosCreate:=DosOpen(FileName,Handle,Action,0,Attrib,18,OpenMode,nil);
end;

function DosOpen (FileName: PChar; var Handle: THandle;
                  Attrib, OpenMode: cardinal): cardinal;

var Action: cardinal;

begin
    DosOpen:=DosOpen(FileName,Handle,Action,0,Attrib,1,OpenMode,nil);
end;

function DosOpen(const FileName:string;var Handle,Action:longint;
                 InitSize,Attrib,OpenFlags,OpenMode:longint;
                 EA:PEAOp2):cardinal;

var T:array[0..255] of char;

begin
    StrPCopy(@T,FileName);
    DosOpen:=DosOpen(@T,Handle,Action,InitSize,Attrib,OpenFlags,OpenMode,EA);
end;

function DosOpen (const FileName: string; var Handle: THandle;
                  var Action: cardinal; InitSize, Attrib, OpenFlags: cardinal;
                  OpenMode: cardinal; EA: PEAOp2): cardinal;

var T:array[0..255] of char;

begin
    StrPCopy(@T,FileName);
    DosOpen:=DosOpen(@T,Handle,Action,InitSize,Attrib,OpenFlags,OpenMode,EA);
end;

function DosCreate (const FileName: string; var Handle: THandle;
                    Attrib, OpenMode: cardinal): cardinal;

var T:array[0..255] of char;
    Action:cardinal;

begin
    StrPCopy(@T,FileName);
    DosCreate:=DosOpen(@T,Handle,Action,0,Attrib,18,OpenMode,nil);
end;

function DosOpen (const FileName: string; var Handle: THandle;
                  Attrib, OpenMode: cardinal): cardinal;

var T:array[0..255] of char;
    Action:cardinal;

begin
    StrPCopy(@T,FileName);
    DosOpen:=DosOpen(@T,Handle,Action,0,Attrib,1,OpenMode,nil);
end;

function DosClose (Handle: THandle): cardinal; cdecl;
external 'DOSCALLS' index 257;

function DosRead(Handle:longint;var Buffer;Count:longint;
                 var ActCount:longint):cardinal; cdecl;
external 'DOSCALLS' index 281;

function DosRead (Handle: THandle; var Buffer; Count: cardinal;
                  var ActCount: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 281;

function DosWrite(Handle:longint;const Buffer;Count:longint;
                  var ActCount:longint):cardinal; cdecl;
external 'DOSCALLS' index 282;

function DosWrite (Handle: THandle; const Buffer; Count: cardinal;
                   var ActCount: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 282;

function DosSetFilePtr(Handle:longint;Pos:longint;Method:cardinal;
                       var PosActual:longint):cardinal; cdecl;
external 'DOSCALLS' index 256;

function DosSetFilePtr (Handle: THandle; Pos: longint; Method: cardinal;
                        var PosActual: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 256;

function DosSetFilePtr (Handle: THandle; Pos: longint): cardinal;

var PosActual: cardinal;

begin
    DosSetFilePtr:=DosSetFilePtr(Handle,Pos,0,PosActual);
end;

function DosGetFilePtr(Handle:longint;var PosActual:longint):cardinal;

begin
    DosGetFilePtr:=DosSetFilePtr(Handle,0,1,PosActual);
end;

function DosGetFilePtr (Handle: THandle; var PosActual: cardinal): cardinal;

begin
    DosGetFilePtr:=DosSetFilePtr(Handle,0,1,PosActual);
end;

function DosSetFileSize (Handle: THandle; Size: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 272;

function DosResetBuffer (Handle: THandle): cardinal; cdecl;
external 'DOSCALLS' index 254;

function DosDupHandle (Handle: THandle; var Duplicate: THandle): cardinal;
                                                                         cdecl;
external 'DOSCALLS' index 260;

function DosQueryFHState(Handle:longint;var FileMode:longint):cardinal; cdecl;
external 'DOSCALLS' index 276;

function DosQueryFHState (Handle: THandle; var FileMode: cardinal): cardinal;
                                                                         cdecl;
external 'DOSCALLS' index 276;

function DosSetFHState (Handle: THandle; FileMode: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 221;

function DosQueryHType(Handle:longint;var HandType:longint;
                       var Attr:longint):cardinal; cdecl;
external 'DOSCALLS' index 224;

function DosQueryHType (Handle: THandle; var HandType: cardinal;
                        var Attr: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 224;

function DosEditName (MetaLevel: cardinal; Source, Edit: PChar;
                      Target: PChar; TargetLen: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 261;

function DosEditName (MetaLevel: cardinal; const Source, Edit: string;
                      var Target: string): cardinal;

var T,T2,T3:array[0..255] of char;

begin
    StrPCopy(@T,Source);
    StrPCopy(@T2,Edit);
    DosEditName:=DosEditName(MetaLevel,@T,@T2,@T3,SizeOf(T3));
    Target:=StrPas(@T3);
end;

function DosMove(OldFile,NewFile:PChar):cardinal; cdecl;
external 'DOSCALLS' index 271;

function DosMove(const OldFile,NewFile:string):cardinal;

var T,T2:array[0..255] of char;

begin
    StrPCopy(@T,OldFile);
    StrPCopy(@T2,NewFile);
    DosMove:=DosMove(@T,@T2);
end;

function DosCopy (OldFile, NewFile: PChar; Option: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 258;

function DosCopy (const OldFile, NewFile: string; Option: cardinal): cardinal;

var T,T2:array[0..255] of char;

begin
    StrPCopy(@T,OldFile);
    StrPCopy(@T2,NewFile);
    DosCopy:=DosCopy(@T,@T2,Option);
end;

function DosDelete(FileName:PChar):cardinal; cdecl;
external 'DOSCALLS' index 259;

function DosDelete(const FileName:string):cardinal;

var T:array[0..255] of char;

begin
    StrPCopy(@T,FileName);
    DosDelete:=DosDelete(@T);
end;

function DosForceDelete(FileName:PChar):cardinal; cdecl;
external 'DOSCALLS' index 110;

function DosForceDelete(const FileName:string):cardinal;

var T:array[0..255] of char;

begin
    StrPCopy(@T,FileName);
    DosForceDelete:=DosForceDelete(@T);
end;

function DosCreateDir(Name:PChar;EA:PEAOp2):cardinal; cdecl;
external 'DOSCALLS' index 270;

function DosCreateDir(Name:PChar):cardinal;

begin
    DosCreateDir:=DosCreateDir(Name,nil);
end;

function DosCreateDir(const Name:string;EA:PEAOp2):cardinal;

var T:array[0..255] of char;

begin
    StrPCopy(@T,Name);
    DosCreateDir:=DosCreateDir(@T,EA);
end;

function DosCreateDir(const Name:string):cardinal;

var T:array[0..255] of char;

begin
    StrPCopy(@T,Name);
    DosCreateDir:=DosCreateDir(@T,nil);
end;

function DosDeleteDir(Name:PChar):cardinal; cdecl;
external 'DOSCALLS' index 226;

function DosDeleteDir(const Name:string):cardinal;

var T:array[0..255] of char;

begin
    StrPCopy(@T,Name);
    DosDeleteDir:=DosDeleteDir(@T);
end;

function DosSetDefaultDisk(DiskNum:cardinal):cardinal; cdecl;
external 'DOSCALLS' index 220;

procedure DosQueryCurrentDisk(var DiskNum:longint;var Logical:longint); cdecl;
external 'DOSCALLS' index 275;

procedure DosQueryCurrentDisk (var DiskNum: cardinal; var Logical: cardinal);
                                                                         cdecl;
external 'DOSCALLS' index 275;

function DosSetCurrentDir(Name:PChar):cardinal; cdecl;
external 'DOSCALLS' index 255;

function DosSetCurrentDir(const Name:string):cardinal;

var T:array[0..255] of char;

begin
    StrPCopy(@T,Name);
    DosSetCurrentDir:=DosSetCurrentDir(@T);
end;

function DosQueryCurrentDir(DiskNum:longint;var Buffer;
                            var BufLen:longint):cardinal; cdecl;
external 'DOSCALLS' index 274;

function DosQueryCurrentDir (DiskNum: cardinal; var Buffer;
                             var BufLen: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 274;

function DosQueryCurrentDir (DiskNum: cardinal; var Buffer: string): cardinal;

var T:array[0..255] of char;
    L: cardinal;

begin
    L:=255;
    DosQueryCurrentDir:=DosQueryCurrentDir(DiskNum,T,L);
    Buffer:=StrPas(@T);
end;

function DosDevIOCtl(Handle,Category,Func:longint;var Params;
                     ParamLen:longint;var ParamSize:longint;
                var Data;DataLen:longint;var DataSize:longint):cardinal; cdecl;
external 'DOSCALLS' index 284;

function DosDevIOCtl (Handle: THandle; Category, Func: cardinal; var Params;
                      ParamLen: cardinal; var ParamSize: cardinal;
                      var Data; DataLen: cardinal; var DataSize: cardinal):
                                                               cardinal; cdecl;
external 'DOSCALLS' index 284;

function DosFindFirst (FileMask: PChar; var Handle: THandle; Attrib: cardinal;
                       AFileStatus: PFileStatus; FileStatusLen: cardinal;
                       var Count: cardinal; InfoLevel: cardinal): cardinal;
                                                                         cdecl;
external 'DOSCALLS' index 264;

function DosFindFirst (const FileMask: string; var Handle: THandle;
                       Attrib: cardinal; AFileStatus: PFileStatus;
                       FileStatusLen: cardinal; var Count: cardinal;
                       InfoLevel: cardinal): cardinal;

var T:array[0..255] of char;

begin
    StrPCopy(@T,FileMask);
    DosFindFirst:=DosFindFirst(@T,Handle,Attrib,AFileStatus,FileStatusLen,
     Count,InfoLevel);
end;

function DosFindNext (Handle: THandle; AFileStatus: PFileStatus;
                      FileStatusLen: cardinal; var Count: cardinal): cardinal;
                                                                         cdecl;
external 'DOSCALLS' index 265;

function DosFindClose (Handle: THandle): cardinal; cdecl;
external 'DOSCALLS' index 263;

function DosQueryFileInfo (Handle: THandle; InfoLevel: cardinal;
                           AFileStatus: PFileStatus;
                                     FileStatusLen: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 279;

function DosSetFileInfo (Handle: THandle; InfoLevel: cardinal;
                         AFileStatus: PFileStatus;
                         FileStatusLen: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 218;

function DosQueryPathInfo (FileName: PChar; InfoLevel: cardinal;
           AFileStatus: PFileStatus; FileStatusLen: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 223;

function DosQueryPathInfo (const FileName: string; InfoLevel: cardinal;
                  AFileStatus: PFileStatus; FileStatusLen: cardinal): cardinal;

var T:array[0..255] of char;

begin
    StrPCopy(@T,FileName);
    DosQueryPathInfo:=DosQueryPathInfo(@T,InfoLevel,AFileStatus,
     FileStatusLen);
end;

function DosSetPathInfo (FileName: PChar; InfoLevel: cardinal;
                         AFileStatus: PFileStatus; FileStatusLen,
                         Options: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 219;

function DosEnumAttribute(RefType:longint;AFile:pointer;
                          Entry:longint;var Buf;BufSize:longint;
                          var Count:longint;InfoLevel:longint):cardinal; cdecl;
external 'DOSCALLS' index 372;

function DosEnumAttribute (RefType: cardinal; AFile: pointer;
                           Entry: cardinal; var Buf; BufSize: cardinal;
                           var Count: cardinal; InfoLevel: cardinal): cardinal;
                                                                         cdecl;
external 'DOSCALLS' index 372;

function DosEnumAttribute (RefType: cardinal; AFile: PChar;
                           Entry: cardinal; var Buf; BufSize: cardinal;
                           var Count: cardinal; InfoLevel: cardinal): cardinal;
                                                                         cdecl;
external 'DOSCALLS' index 372;

function DosEnumAttribute (RefType: cardinal; const AFile: THandle;
                           Entry: cardinal; var Buf; BufSize: cardinal;
                           var Count: cardinal; InfoLevel: cardinal): cardinal;
                                                                         cdecl;
external 'DOSCALLS' index 372;

function DosEnumAttribute (Handle: longint; Entry: longint; var Buf;
                           BufSize: longint;
                           var Count: longint; InfoLevel: longint): cardinal;

begin
    DosEnumAttribute:=DosEnumAttribute(0,@Handle,Entry,Buf,BufSize,Count,
     InfoLevel);
end;

function DosEnumAttribute (Handle: THandle; Entry: cardinal; var Buf;
                           BufSize: cardinal;
                           var Count: cardinal; InfoLevel: cardinal): cardinal;

begin
    DosEnumAttribute:=DosEnumAttribute(0,@Handle,Entry,Buf,BufSize,Count,
     InfoLevel);
end;

function DosEnumAttribute (const FileName: string;
                           Entry: cardinal;var Buf;BufSize: cardinal;
                           var Count: cardinal; InfoLevel: cardinal): cardinal;

var T:array[0..255] of char;

begin
    StrPCopy(@T,FileName);
    DosEnumAttribute:=DosEnumAttribute(1,@T,Entry,Buf,BufSize,Count,
     InfoLevel);
end;

function DosScanEnv(Name:PChar;var Value:PChar):cardinal; cdecl;
external 'DOSCALLS' index 227;

function DosScanEnv(const Name:string;var Value:string):cardinal;

var T:array[0..255] of char;
    P:PChar;

begin
    StrPCopy(@T,Name);
    DosScanEnv:=DosScanEnv(@T,P);
    Value:=StrPas(P);
end;

function DosSearchPath (Flag: cardinal; DirList, FileName: PChar;
                        FullName: PChar; FullLen: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 228;

function DosSearchPath (Flag: cardinal; const DirList, FileName: string;
                        var FullName: string): cardinal;

var T1,T2,T3:array[0..255] of char;

begin
    StrPCopy(@T1,DirList);
    StrPCopy(@T2,FileName);
    DosSearchPath:=DosSearchPath(Flag,@T1,@T2,@T3,SizeOf(T3));
    FullName:=StrPas(@T3);
end;

function DosFSAttach (DevName, FileSystem: PChar; var Data: TAttachData;
                      DataLen, Flag: cardinal):cardinal; cdecl;
external 'DOSCALLS' index 269;

function DosFSAttach (const DevName, FileSystem: string; var Data: TAttachData;
                      DataLen, Flag: cardinal): cardinal;

var T1,T2:array[0..255] of char;

begin
    StrPCopy(@T1,DevName);
    StrPCopy(@T2,FileSystem);
    DosFSAttach:=DosFSAttach(@T1,@T2,Data,DataLen,Flag);
end;

function DosQueryFSAttach(DevName:PChar;Ordinal,InfoLevel:longint;
                     var Buffer:TFSQBuffer2;var BufLen:longint):cardinal; cdecl;
external 'DOSCALLS' index 277;

function DosQueryFSAttach (DevName: PChar; Ordinal, InfoLevel: cardinal;
               var Buffer: TFSQBuffer2; var BufLen: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 277;

function DosQueryFSAttach(const DevName:string;Ordinal,InfoLevel:longint;
                          var Buffer:TFSQBuffer2;var BufLen:longint):cardinal;

var T:array[0..255] of char;

begin
    StrPCopy(@T,DevName);
    DosQueryFSAttach:=DosQueryFSAttach(@T,Ordinal,InfoLevel,Buffer,BufLen);
end;

function DosQueryFSAttach (const DevName: string; Ordinal, InfoLevel: cardinal;
                      var Buffer: TFSQBuffer2; var BufLen: cardinal): cardinal;

var T:array[0..255] of char;

begin
    StrPCopy(@T,DevName);
    DosQueryFSAttach:=DosQueryFSAttach(@T,Ordinal,InfoLevel,Buffer,BufLen);
end;

function DosFSCtl(Data:pointer;DataLen:longint;var ResDataLen:longint;
                  Parms:pointer;ParmsLen:longint;var ResParmsLen:longint;
                  _Function:longint;Route:PChar;
                  Handle,Method:longint):cardinal; cdecl;
external 'DOSCALLS' index 285;

function DosFSCtl (Data: pointer; DataLen: cardinal; var ResDataLen: cardinal;
                   Parms: pointer; ParmsLen: cardinal;
                   var ResParmsLen: cardinal; _Function: cardinal;
                   Route: PChar; Handle: THandle; Method: cardinal): cardinal;
                                                                         cdecl;
external 'DOSCALLS' index 285;

function DosFSCtl(Data:pointer;DataLen:longint;var ResDataLen:longint;
                  Parms:pointer;ParmsLen:longint;var ResParmsLen:longint;
                  _Function:longint;const Route:string;
                  Handle,Method:longint):cardinal;

var T:array[0..255] of char;

begin
    StrPCopy(@T,Route);
    DosFSCtl:=DosFSCtl(Data,Datalen,ResDataLen,Parms,ParmsLen,ResParmsLen,
     _Function,Route,Handle,Method);
end;

function DosFSCtl (Data: pointer; DataLen: cardinal; var ResDataLen: cardinal;
                   Parms: pointer; ParmsLen: cardinal;
                   var ResParmsLen: cardinal; _Function: cardinal;
                   const Route: string; Handle: THandle; Method: cardinal):
                                                                      cardinal;

var T:array[0..255] of char;

begin
    StrPCopy(@T,Route);
    DosFSCtl:=DosFSCtl(Data,Datalen,ResDataLen,Parms,ParmsLen,ResParmsLen,
     _Function,Route,Handle,Method);
end;

function DosQueryFSInfo (DiskNum, InfoLevel: cardinal; var Buffer: TFSInfo;
                         BufLen: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 278;

function DosSetFSInfo (DiskNum, InfoLevel: cardinal; var Buffer: TFSInfo;
                       BufLen: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 222;

function DosQueryVerify(var Enabled:longint):cardinal; cdecl;
external 'DOSCALLS' index 225;

function DosQueryVerify (var Enabled: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 225;

function DosQueryVerify (var Enabled: boolean): cardinal;
var
  En: cardinal;
  RC: cardinal;
begin
  RC := DosQueryVerify (En);
  Enabled := boolean (En);
  DosQueryVerify := RC;
end;

function DosSetVerify (Enable: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 210;

function DosSetVerify (Enable: boolean): cardinal;
begin
  DosSetVerify := DosSetVerify (cardinal (Enable));
end;

function DosSetMaxFH (Count: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 209;

function DosSetRelMaxFH(var ReqCount,CurMaxFH:longint):cardinal; cdecl;
external 'DOSCALLS' index 382;

function DosSetRelMaxFH (var ReqCount: longint; var CurMaxFH: cardinal):
                                                               cardinal; cdecl;
external 'DOSCALLS' index 382;

function DosShutDown (Flags: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 415;

function DosQuerySysInfo (First, Last: cardinal; var Buf; BufSize: cardinal):
                                                               cardinal; cdecl;
external 'DOSCALLS' index 348;

function DosQuerySysInfo(First,Last:cardinal;Buf:PQSVValues;
                                              BufSize:cardinal):cardinal;cdecl;
external 'DOSCALLS' index 348;

function DosPhysicalDisk (Func: cardinal; Buf: pointer; BufSize: cardinal;
                          Params: pointer; ParamSize: cardinal): cardinal;
                                                                         cdecl;
external 'DOSCALLS' index 287;

function DosAllocMem (var P: pointer; Size, Flag: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 299;

function DosFreeMem (P: pointer): cardinal; cdecl;
external 'DOSCALLS' index 304;

function DosSetMem (P: pointer; Size, Flag: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 305;

function DosGiveSharedMem (P: pointer; PID, Flag: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 303;

function DosGetSharedMem (P: pointer; Flag: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 302;

function DosGetNamedSharedMem (var P: pointer; Name: PChar; Flag: cardinal):
                                                               cardinal; cdecl;
external 'DOSCALLS' index 301;

function DosGetNamedSharedMem (var P: pointer; const Name: string;
                               Flag: cardinal): cardinal;

var T:array[0..255] of char;

begin
    StrPCopy(@T,Name);
    DosGetNamedSharedMem:=DosGetNamedSharedMem(P,@T,Flag);
end;

function DosAllocSharedMem (var P: pointer; Name: PChar;
                                        Size, Flag: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 300;

function DosAllocSharedMem (var P: pointer; const Name: string;
                                               Size, Flag: cardinal): cardinal;

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

function DosQueryMem(P:pointer;var Size,Flag:longint):cardinal; cdecl;
external 'DOSCALLS' index 306;

function DosQueryMem (P: pointer; var Size, Flag: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 306;

function DosSubAllocMem (Base: pointer; var P: pointer; Size: cardinal):
                                                               cardinal; cdecl;
external 'DOSCALLS' index 345;

function DosSubFreeMem (Base, P: pointer; Size: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 346;

function DosSubSetMem (Base: pointer; Flag, Size: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 344;

function DosSubUnSetMem (Base: pointer): cardinal; cdecl;
external 'DOSCALLS' index 347;

function DosCreateEventSem (Name: PChar; var Handle: THandle;
                            Attr, State: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 324;

function DosCreateEventSem (Name: PChar; var Handle: THandle;
                            Attr: cardinal; State: boolean): cardinal;
begin
  DosCreateEventSem :=
                      DosCreateEventSem (Name, Handle, Attr, cardinal (State));
end;

function DosCreateEventSem (const Name: string; var Handle: THandle;
                            Attr: cardinal; State: boolean): cardinal;

var T:array[0..255] of char;

begin
  if Name<>'' then
   begin
    StrPCopy(@T,Name);
    DosCreateEventSem :=
                        DosCreateEventSem (@T, Handle, Attr, cardinal (State));
   end
  else
   DosCreateEventSem :=
                       DosCreateEventSem (nil, Handle, Attr, cardinal (State));
end;

function DosCreateEventSem (const Name: string; var Handle: THandle;
                            Attr, State: cardinal): cardinal;

begin
  DosCreateEventSem := DosCreateEventSem (Name, Handle, Attr, boolean (State));
end;

function DosOpenEventSem (Name: PChar; var Handle: THandle): cardinal; cdecl;
external 'DOSCALLS' index 325;

function DosOpenEventSem (const Name: string; var Handle: THandle): cardinal;

var T:array[0..255] of char;

begin
    StrPCopy(@T,Name);
    DosOpenEventSem:=DosOpenEventSem(@T,Handle);
end;

function DosCloseEventSem (Handle: THandle): cardinal; cdecl;
external 'DOSCALLS' index 326;

function DosResetEventSem(Handle:longint;var PostCount:longint):cardinal; cdecl;
external 'DOSCALLS' index 327;

function DosResetEventSem (Handle: THandle; var PostCount: cardinal): cardinal;
                                                                         cdecl;
external 'DOSCALLS' index 327;

function DosPostEventSem (Handle: THandle): cardinal; cdecl;
external 'DOSCALLS' index 328;

function DosWaitEventSem (Handle: THandle; Timeout: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 329;

function DosQueryEventSem(Handle:longint;var Posted:longint):cardinal; cdecl;
external 'DOSCALLS' index 330;

function DosQueryEventSem (Handle: THandle; var Posted: cardinal): cardinal;
                                                                         cdecl;
external 'DOSCALLS' index 330;

function DosCreateMutExSem (Name: PChar; var Handle: THandle;
                            Attr, State: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 331;

function DosCreateMutExSem (Name: PChar; var Handle: THandle;
                            Attr: cardinal; State: boolean): cardinal;
begin
  DosCreateMutExSem :=
                      DosCreateMutExSem (Name, Handle, Attr, cardinal (State));
end;

function DosCreateMutExSem (const Name: string; var Handle: THandle;
                            Attr: cardinal; State: boolean): cardinal;

var T:array[0..255] of char;

begin
  if Name<>'' then
   begin
    StrPCopy(@T,Name);
    DosCreateMutExSem :=
                        DosCreateMutExSem (@T, Handle, Attr, cardinal (State));
   end
  else
   DosCreateMutExSem :=
                       DosCreateMutExSem (nil, Handle, Attr, cardinal (State));
end;

function DosCreateMutExSem (const Name: string; var Handle: THandle;
                            Attr, State: cardinal): cardinal;

begin
    DosCreateMutExSem :=
                       DosCreateMutExSem (Name, Handle, Attr, boolean (State));
end;

function DosOpenMutExSem (Name: PChar; var Handle: THandle): cardinal; cdecl;
external 'DOSCALLS' index 332;

function DosOpenMutExSem (const Name: string; var Handle: THandle): cardinal;

var T:array[0..255] of char;

begin
    StrPCopy(@T,Name);
    DosOpenMutExSem:=DosOpenMutExSem(@T,Handle);
end;

function DosCloseMutExSem (Handle: THandle): cardinal; cdecl;
external 'DOSCALLS' index 333;

function DosRequestMutExSem (Handle: THandle; Timeout: cardinal): cardinal;
                                                                         cdecl;
external 'DOSCALLS' index 334;

function DosReleaseMutExSem (Handle: THandle): cardinal; cdecl;
external 'DOSCALLS' index 335;

function DosQueryMutExSem(Handle:longint;var PID,TID,Count:longint):cardinal;
                                                                         cdecl;
external 'DOSCALLS' index 336;

function DosQueryMutExSem (Handle: THandle; var PID, TID, Count: cardinal):
                                                               cardinal; cdecl;
external 'DOSCALLS' index 336;

function DosCreateMuxWaitSem (Name: PChar; var Handle: THandle;
                              CSemRec: cardinal; var SemArray: TSemArray;
                              Attr: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 337;

function DosCreateMuxWaitSem (Name: PChar; var Handle: THandle;
                              CSemRec: cardinal; SemArray: PSemArray;
                              Attr: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 337;

function DosCreateMuxWaitSem (const Name: string; var Handle: THandle;
                              CSemRec: cardinal; var SemArray: TSemArray;
                              Attr: cardinal): cardinal;

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

function DosOpenMuxWaitSem (Name: PChar; var Handle: THandle): cardinal; cdecl;
external 'DOSCALLS' index 338;

function DosOpenMuxWaitSem (const Name: string; var Handle: THandle): cardinal;

var T:array[0..255] of char;

begin
    StrPCopy(@T,Name);
    DosOpenMuxWaitSem:=DosOpenMuxWaitSem(@T,Handle);
end;

function DosCloseMuxWaitSem (Handle: THandle): cardinal; cdecl;
external 'DOSCALLS' index 339;

function DosWaitMuxWaitSem(Handle,Timeout:longint;var User:longint):cardinal;
                                                                         cdecl;
external 'DOSCALLS' index 340;

function DosWaitMuxWaitSem (Handle: THandle; Timeout: cardinal;
                                          var User: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 340;

function DosAddMuxWaitSem (Handle: THandle; var SemRec: TSemRecord): cardinal;
                                                                         cdecl;
external 'DOSCALLS' index 341;

function DosDeleteMuxWaitSem (Handle, Sem: THandle): cardinal; cdecl;
external 'DOSCALLS' index 342;

function DosQueryMuxWaitSem(Handle:longint;var CSemRec:longint;
                        var SemRecs:TSemArray;var Attr:longint):cardinal; cdecl;
external 'DOSCALLS' index 343;

function DosQueryMuxWaitSem (Handle: THandle; var CSemRec: cardinal;
                  var SemRecs: TSemArray; var Attr: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 343;

function DosGetDateTime (var Buf: TDateTime): cardinal; cdecl;
external 'DOSCALLS' index 230;

function DosSetDateTime (var Buf: TDateTime): cardinal; cdecl;
external 'DOSCALLS' index 292;

function DosAsyncTimer (MSec: cardinal; HSem: THandle;
                        var TimHandle: THandle): cardinal; cdecl;
external 'DOSCALLS' index 350;

function DosStartTimer (MSec: cardinal; HSem: THandle;
                        var TimHandle: THandle): cardinal; cdecl;
external 'DOSCALLS' index 351;

function DosStopTimer (TimHandle: THandle): cardinal; cdecl;
external 'DOSCALLS' index 290;

function DosTmrQueryFreq(var Freq:longint):cardinal; cdecl;
external 'DOSCALLS' index 362;

function DosTmrQueryFreq (var Freq: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 362;

function DosTmrQueryTime (var Time: comp): cardinal; cdecl;
external 'DOSCALLS' index 363;

function DosTmrQueryTime (var Time: qword): cardinal; cdecl;
external 'DOSCALLS' index 363;

function DosLoadModule (ObjName: PChar; ObjLen: cardinal; DLLName: PChar;
                        var Handle: THandle): cardinal; cdecl;
external 'DOSCALLS' index 318;

function DosLoadModule (var ObjName: string; ObjLen: cardinal;
                        const DLLName: string; var Handle: THandle): cardinal;

var T1,T2:array[0..255] of char;

begin
    StrPCopy(@T2,DLLName);
    DosLoadModule:=DosLoadModule(@T1,ObjLen,@T2,Handle);
    ObjName:=StrPas(@T1);
end;

function DosFreeModule (Handle: THandle): cardinal; cdecl;
external 'DOSCALLS' index 322;

function DosQueryProcAddr (Handle: THandle; Ordinal: cardinal; ProcName: PChar;
                           var Address: pointer): cardinal; cdecl;
external 'DOSCALLS' index 321;

function DosQueryProcAddr (Handle: THandle; Ordinal: cardinal;
                       const ProcName: string; var Address: pointer): cardinal;

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

function DosQueryModuleHandle (DLLName: PChar; var Handle: THandle): cardinal;
                                                                         cdecl;
external 'DOSCALLS' index 319;

function DosQueryModuleHandle (const DLLName: string; var Handle: THandle):
                                                                      cardinal;

var T1:array[0..255] of char;

begin
    StrPCopy(@T1,DLLName);
    DosQueryModuleHandle:=DosQueryModuleHandle(@T1,Handle);
end;

function DosQueryModuleName (Handle: THandle; NameLen: cardinal; Name: PChar):
                                                               cardinal; cdecl;
external 'DOSCALLS' index 320;

{function DosQueryModuleName(Handle:longint;var Name:openstring):cardinal;

var T1:array[0..255] of char;

begin
    DosQueryModuleName:=DosQueryModuleName(Handle,High(Name),@T1);
    Name:=StrPas(@T1);
end;}

function DosQueryProcType(Handle,Ordinal:longint;Name:PChar;
                          var ProcType:longint):cardinal; cdecl;
external 'DOSCALLS' index 586;

function DosQueryProcType (Handle: THandle; Ordinal: cardinal; Name: PChar;
                           var ProcType: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 586;

function DosQueryProcType(Handle,Ordinal:longint;const Name:string;
                          var ProcType:longint):cardinal;

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

function DosQueryProcType (Handle: THandle; Ordinal: cardinal;
                         const Name: string; var ProcType: cardinal): cardinal;

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

function DosGetResource (Handle: THandle; ResType, ResName: cardinal;
                                              var P: pointer): cardinal; cdecl;
external 'DOSCALLS' index 352;

function DosFreeResource (P: pointer): cardinal; cdecl;
external 'DOSCALLS' index 353;

function DosQueryResourceSize(Handle,IDT,IDN:longint;var Size:longint):cardinal;
                                                                         cdecl;
external 'DOSCALLS' index 572;

function DosQueryResourceSize (Handle: THandle; IDT, IDN: cardinal;
                                          var Size: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 572;

function DosQueryCtryInfo(Size:longint;var Country:TCountryCode;
                  var Res:TCountryInfo;var ActualSize:longint):cardinal; cdecl;
external 'NLS' index 5;

function DosQueryCtryInfo (Size: cardinal; var Country: TCountryCode;
             var Res: TCountryInfo; var ActualSize: cardinal): cardinal; cdecl;
external 'NLS' index 5;

function DosQueryDBCSEnv (Size: cardinal; var Country: TCountryCode;
                                                  Buf: PChar): cardinal; cdecl;
external 'NLS' index 6;

function DosMapCase (Size: cardinal; var Country: TCountryCode;
                     AString: PChar): cardinal; cdecl;
external 'NLS' index 7;

function DosMapCase (var Country: TCountryCode; var AString: string): cardinal;

var T1:string;

begin
    StrPCopy(@T1,AString);
    DosMapCase:=DosMapCase(length(AString),Country,@T1);
    AString:=StrPas(@T1);
end;

function DosQueryCollate(Size:longint;var Country:TCountryCode;
                         buf:PByteArray;var TableLen:longint):cardinal; cdecl;
external 'NLS' index 8;

function DosQueryCollate (Size: cardinal; var Country: TCountryCode;
                     Buf: PByteArray; var TableLen: cardinal): cardinal; cdecl;
external 'NLS' index 8;

function DosQueryCP(Size:longint; PCodePages:PWordArray;
                                          var ActSize:longint):cardinal; cdecl;
external 'DOSCALLS' index 291;

function DosQueryCP (Size: cardinal; PCodePages: PWordArray;
                                       var ActSize: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 291;

function DosQueryCP (Size: cardinal; var CodePages;
                                       var ActSize: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 291;

function DosSetProcessCP (CP: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 289;

function DosSetExceptionHandler (var RegRec: TExceptionRegistrationRecord):
                                                               cardinal; cdecl;
external 'DOSCALLS' index 354;

function DosUnsetExceptionHandler (var RegRec: TExceptionRegistrationRecord):
                                                               cardinal; cdecl;
external 'DOSCALLS' index 355;

function DosRaiseException (var Excpt: TExceptionReportRecord): cardinal;
                                                                         cdecl;
external 'DOSCALLS' index 356;

function DosSendSignalException (PID, Exception: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 379;

function DosUnwindException (var Handler: TExceptionRegistrationRecord;
                             TargetIP: pointer;
                          var RepRec: TExceptionReportRecord): cardinal; cdecl;
external 'DOSCALLS' index 357;

function DosSetSignalExceptionFocus(Enable:longint;var Times:longint):cardinal;
                                                                         cdecl;
external 'DOSCALLS' index 378;

function DosSetSignalExceptionFocus (Enable: cardinal;
                                         var Times: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 378;

function DosSetSignalExceptionFocus (Enable: boolean;
                                                var Times: cardinal): cardinal;
begin
  DosSetSignalExceptionFocus :=
                         DosSetSignalExceptionFocus (cardinal (Enable), Times);
end;

function DosEnterMustComplete(var Nesting:longint):cardinal; cdecl;
external 'DOSCALLS' index 380;

function DosEnterMustComplete (var Nesting: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 380;

function DosExitMustComplete(var Nesting:longint):cardinal; cdecl;
external 'DOSCALLS' index 381;

function DosExitMustComplete (var Nesting: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 381;

function DosAcknowledgeSignalException (SignalNum: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 418;

function DosCloseQueue (Handle: THandle): cardinal; cdecl;
external 'QUECALLS' index 11;

function DosCreateQueue (var Handle: THandle; Priority: cardinal;
                         Name: PChar): cardinal; cdecl;
external 'QUECALLS' index 16;

function DosCreateQueue (var Handle: THandle; Priority: cardinal;
                         const Name: string): cardinal;

var T1:array[0..255] of char;

begin
    StrPCopy(@T1,Name);
    DosCreateQueue:=DosCreateQueue(Handle,Priority,@T1);
end;

function DosOpenQueue(var Parent_PID:longint;var Handle:longint;
                      Name:PChar):cardinal; cdecl;
external 'QUECALLS' index 15;

function DosOpenQueue (var Parent_PID: cardinal; var Handle: THandle;
                       Name: PChar): cardinal; cdecl;
external 'QUECALLS' index 15;

function DosOpenQueue(var Parent_PID:longint;var Handle:longint;
                      const Name:string):cardinal;

var T1:array[0..255] of char;

begin
    StrPCopy(@T1,Name);
    DosOpenQueue:=DosOpenQueue(Parent_PID,Handle,@T1);
end;

function DosOpenQueue (var Parent_PID: cardinal; var Handle: THandle;
                       const Name: string): cardinal;

var T1:array[0..255] of char;

begin
    StrPCopy(@T1,Name);
    DosOpenQueue:=DosOpenQueue(Parent_PID,Handle,@T1);
end;

function DosPeekQueue(Handle:longint;var ReqBuffer:TRequestData;
                      var DataLen:longint;var DataPtr:pointer;
                      var Element:longint;Wait:longint;
                      var Priority:byte;ASem:longint):cardinal; cdecl;
external 'QUECALLS' index 13;

function DosPeekQueue (Handle: THandle; var ReqBuffer:TRequestData;
                       var DataLen: cardinal; var DataPtr: pointer;
                       var Element: cardinal; Wait: cardinal;
                       var Priority: byte; ASem: THandle): cardinal; cdecl;
external 'QUECALLS' index 13;

function DosPeekQueue (Handle: THandle; var ReqBuffer: TRequestData;
                       var DataLen: cardinal; var DataPtr: pointer;
                       var Element: cardinal; Wait: boolean;
                       var Priority: byte; ASem: THandle): cardinal;
begin
  DosPeekQueue := DosPeekQueue (Handle, ReqBuffer, DataLen, DataPtr, Element,
                                              cardinal (Wait), Priority, ASem);
end;

function DosPurgeQueue (Handle: THandle): cardinal; cdecl;
external 'QUECALLS' index 10;

function DosQueryQueue(Handle:longint;var Count:longint):cardinal; cdecl;
external 'QUECALLS' index 12;

function DosQueryQueue (Handle: THandle; var Count: cardinal): cardinal; cdecl;
external 'QUECALLS' index 12;

function DosReadQueue(Handle:longint;var ReqBuffer:TRequestData;
                      var DataLen:longint;var DataPtr:pointer;
                      Element,Wait:longint;var Priority:byte;
                      ASem:longint):cardinal; cdecl;
external 'QUECALLS' index 9;

function DosReadQueue (Handle: THandle; var ReqBuffer: TRequestData;
                       var DataLen: cardinal; var DataPtr: pointer;
                       Element, Wait: cardinal; var Priority: byte;
                       ASem: THandle): cardinal; cdecl;
external 'QUECALLS' index 9;

function DosReadQueue (Handle: THandle; var ReqBuffer: TRequestData;
                       var DataLen: cardinal; var DataPtr: pointer;
                       Element: cardinal; Wait: boolean; var Priority: byte;
                       ASem: THandle): cardinal;
begin
  DosReadQueue := DosReadQueue (Handle, ReqBuffer, DataLen, DataPtr, Element,
                                              cardinal (Wait), Priority, ASem);
end;

function DosWriteQueue (Handle: THandle; Request, DataLen: cardinal;
                        var DataBuf; Priority: cardinal): cardinal; cdecl;
external 'QUECALLS' index 14;

function DosError (Error: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 212;

procedure DosErrClass(Code:longint;var _Class,Action,Locus:longint); cdecl;
external 'DOSCALLS' index 211;

procedure DosErrClass (Code: cardinal; var _Class, Action, Locus: cardinal);
                                                                         cdecl;
external 'DOSCALLS' index 211;

function DosTrueGetMessage (MsgSeg: pointer; Table: PInsertTable;
                            TableSize: cardinal; Buf: PChar;
                            BufSize, MsgNumber: cardinal; FileName: PChar;
                            var MsgSize: cardinal): cardinal; cdecl;
external 'MSG' index 6;

function DosIQueryMessageCP (var Buf; BufSize: cardinal; FileName: PChar;
                     var InfoSize: cardinal; MesSeg: pointer): cardinal; cdecl;
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

function DosGetMessage (Table: PInsertTable; TableSize: cardinal; Buf: PChar;
                        BufSize, MsgNumber: cardinal; FileName: PChar;
                        var MsgSize: cardinal): cardinal; cdecl; assembler;
                                                                  nostackframe;
asm
  pop eax
  push offset MagicHeaderStart
  push eax
  jmp DosTrueGetMessage
end;

function DosGetMessage (Table: PInsertTable; TableSize:longint;Buf:PChar;
                        BufSize,MsgNumber:longint;FileName:PChar;
                        var MsgSize:longint):cardinal;
begin
  DosGetMessage := DosGetMessage (Table, cardinal (TableSize), Buf,
       cardinal (BufSize), cardinal (MsgNumber), FileName, cardinal (MsgSize));
end;

function DosQueryMessageCP (var Buf; BufSize: cardinal; FileName: PChar;
                            var InfoSize: cardinal): cardinal;
begin
    DosQueryMessageCP := DosIQueryMessageCP(Buf, BufSize, FileName, InfoSize,
                                                            @MagicHeaderStart);
end;

function DosQueryMessageCP(var Buf;BufSize:longint;FileName:PChar;
                            var InfoSize:longint):cardinal;
begin
    DosQueryMessageCP := DosIQueryMessageCP(Buf, cardinal (BufSize), FileName,
                                       cardinal (InfoSize), @MagicHeaderStart);
end;

procedure MagicHeaderEnd; assembler;
asm
  dd $0FFFF0000
end;
{$ASMMODE DEFAULT}

(*function DosGetMessage(const Table:array of PString;var Buf:openstring;
                        MsgNumber:longint;const FileName:string):cardinal;

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
                       MsgSize:longint):cardinal;}

function DosQueryMessageCP(var Buf;BufSize:longint;const FileName:string;
                           var InfoSize:longint):cardinal;

var T:array[0..255] of char;

begin
    StrPCopy(@T,FileName);
    DosQueryMessageCP:=DosQueryMessageCP(Buf,BufSize,@T,InfoSize);
end;

function DosQueryMessageCP (var Buf; BufSize: cardinal; const FileName: string;
                            var InfoSize: cardinal): cardinal;

var T:array[0..255] of char;

begin
    StrPCopy(@T,FileName);
    DosQueryMessageCP:=DosQueryMessageCP(Buf,BufSize,@T,InfoSize);
end;

function DosInsertMessage(Table:PInsertTable;TableSize:longint;
                          Message:PChar;SrcMessageSize:longint;
                          Buf:PChar;BufSize:longint;
                          var DstMessageSize:longint):cardinal; cdecl;
external 'MSG' index 4;

function DosInsertMessage (Table: PInsertTable; TableSize: cardinal;
                           Message: PChar; SrcMessageSize: cardinal;
                           Buf: PChar; BufSize: cardinal;
                           var DstMessageSize: cardinal): cardinal; cdecl;
external 'MSG' index 4;

{function DosInsertMessage(Table:array of PString;
                          const Message:string;
                          var Buf:openstring):cardinal;

function DosInsertMessage(Table:array of PString;
                          Message:PChar;SrcMessageSize:longint;
                          Buf:PChar;BufSize:longint;
                          var DstMessageSize:longint):cardinal;}

function DosPutMessage (Handle: THandle; Size: cardinal; Buf: PChar): cardinal;
                                                                         cdecl;
external 'MSG' index 5;

function DosPutMessage (Handle: THandle; const Buf: string): cardinal;

begin
    DosPutMessage:=DosPutMessage(Handle,Length(Buf),@Buf[1]);
end;

function DosStartSession (var AStartData:TStartData;
                          var SesID,PID:longint):cardinal; cdecl;
external 'SESMGR' index 37;

function DosStartSession (var AStartData: TStartData;
                          var SesID, PID: cardinal): cardinal; cdecl;
external 'SESMGR' index 37;

function DosSetSession (SesID: cardinal; var AStatus: TStatusData): cardinal;
                                                                         cdecl;
external 'SESMGR' index 39;

function DosSelectSession (SesID: cardinal): cardinal; cdecl;
external 'SESMGR' index 38;

function DosStopSession (Scope, SesID: cardinal): cardinal; cdecl;
external 'SESMGR' index 40;

function DosCreatePipe (var ReadHandle, WriteHandle: THandle;
                        Size: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 239;

function DosCreateNPipe (Name: PChar; var Handle: THandle; OpenMode, PipeMode,
                       OutBufSize, InBufSize, MSec: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 243;

function DosCreateNPipe (const Name: string; var Handle: THandle; OpenMode,
                    PipeMode, OutBufSize, InBufSize, MSec: cardinal): cardinal;

var T:array[0..255] of char;

begin
    StrPCopy(@T,Name);
    DosCreateNPipe:=DosCreateNPipe(@T,Handle,OpenMode,PipeMode,OutBufSize,
     InBufSize,MSec);
end;

function DosCallNPipe(Name:PChar;var Input;InputSize:longint;
                      var Output;OutputSize:longint;var ReadBytes:longint;
                      MSec:longint):cardinal; cdecl;
external 'DOSCALLS' index 240;

function DosCallNPipe (Name: PChar; var Input; InputSize: cardinal;
                       var Output; OutputSize: cardinal;
                       var ReadBytes: cardinal; MSec: cardinal): cardinal;
                                                                         cdecl;
external 'DOSCALLS' index 240;

function DosCallNPipe(const Name:string;var Input;InputSize:longint;
                      var Output;OutputSize:longint;var ReadBytes:longint;
                      MSec:longint):cardinal;

var T:array[0..255] of char;

begin
    StrPCopy(@T,Name);
    DosCallNPipe:=DosCallNPipe(@T,Input,InputSize,Output,OutputSize,
     ReadBytes,MSec);
end;

function DosCallNPipe (const Name: string; var Input; InputSize: cardinal;
                       var Output; OutputSize: cardinal;
                       var ReadBytes: cardinal; MSec: cardinal): cardinal;

var T:array[0..255] of char;

begin
    StrPCopy(@T,Name);
    DosCallNPipe:=DosCallNPipe(@T,Input,InputSize,Output,OutputSize,
     ReadBytes,MSec);
end;

function DosConnectNPipe (Handle: THandle): cardinal; cdecl;
external 'DOSCALLS' index 241;

function DosDisconnectNPipe (Handle: THandle): cardinal; cdecl;
external 'DOSCALLS' index 242;

function DosPeekNPipe(Handle:longint;var Buffer;BufSize:longint;
                      var ReadBytes:longint;var Avail:TAvailData;
                      var State:longint):cardinal; cdecl;
external 'DOSCALLS' index 244;

function DosPeekNPipe (Handle: THandle; var Buffer; BufSize: cardinal;
                       var ReadBytes: cardinal; var Avail: TAvailData;
                       var State: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 244;

function DosQueryNPHState(Handle:longint;var State:longint):cardinal; cdecl;
external 'DOSCALLS' index 245;

function DosQueryNPHState (Handle: THandle; var State: cardinal): cardinal;
                                                                         cdecl;
external 'DOSCALLS' index 245;

function DosQueryNPipeInfo (Handle: THandle; InfoLevel: cardinal; var Buffer;
                            BufSize: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 248;

function DosQueryNPipeSemState (SemHandle: THandle; var SemArray;
                                           BufSize: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 249;

function DosQueryNPipeSemState (SemHandle: THandle; SemArray: PPipeSemState;
                                           BufSize: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 249;

function DosSetNPHState (Handle: THandle; State: cardinal):cardinal; cdecl;
external 'DOSCALLS' index 250;

function DosSetNPipeSem(PipeHandle,SemHandle: THandle; Key: cardinal):
                                                               cardinal; cdecl;
external 'DOSCALLS' index 251;

function DosTransactNPipe(Handle:longint;var OutBuf;OutSize:longint;
                          var InBuf;InSize:longint;
                          var ReadBytes:longint):cardinal; cdecl;
external 'DOSCALLS' index 252;

function DosTransactNPipe (Handle: THandle; var OutBuf; OutSize: cardinal;
                          var InBuf; InSize: cardinal;
                          var ReadBytes: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 252;

function DosWaitNPipe (Name: PChar; MSec: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 253;

function DosWaitNPipe (const Name: string; MSec: cardinal): cardinal;

var T:array[0..255] of char;

begin
    StrPCopy(@T,Name);
    DosWaitNPipe:=DosWaitNPipe(@T,MSec);
end;

function DosOpenVDD (Name: PChar; var Handle: THandle): cardinal; cdecl;
external 'DOSCALLS' index 308;

function DosRequestVDD (Handle: THandle; SGroup, Cmd: cardinal;
                        InSize: cardinal; var InBuffer;
                        OutSize: cardinal; var OutBuffer): cardinal; cdecl;
external 'DOSCALLS' index 309;

function DosCloseVDD (Handle: THandle): cardinal; cdecl;
external 'DOSCALLS' index 310;

procedure DosSelToFlat; cdecl;
external 'DOSCALLS' index 426;

procedure DosFlatToSel; cdecl;
external 'DOSCALLS' index 425;

{$ASMMODE INTEL}
function SelToFlat (AFarPtr: cardinal): pointer; assembler;
 asm
  push ebx
  push esi
  push edi
{$IFNDEF REGCALL}
  mov eax, AFarPtr
{$ENDIF REGCALL}
  call DosSelToFlat
  pop edi
  pop esi
  pop ebx
 end;

function SelToFlat (AFarPtr: TFarPtr): pointer; assembler;
 asm
  push ebx
  push esi
  push edi
  mov eax, AFarPtr
  call DosSelToFlat
  pop edi
  pop esi
  pop ebx
 end;

function FlatToSel (APtr: pointer): cardinal; assembler;
 asm
  push ebx
  push esi
  push edi
{$IFNDEF REGCALL}
  mov eax, APtr
{$ENDIF REGCALL}
  call DosFlatToSel
  pop edi
  pop esi
  pop ebx
 end;

function DosAllocThreadLocalMemory (Count: cardinal; var P: pointer): cardinal;
                                                                         cdecl;
external 'DOSCALLS' index 454;

function DosFreeThreadLocalMemory (P: pointer): cardinal; cdecl;
external 'DOSCALLS' index 455;

function DosQueryRASInfo (Index: cardinal; var PBuffer: pointer): cardinal;
                                          cdecl; external 'DOSCALLS' index 112;

function LogOpen (var Handle: cardinal): cardinal; cdecl;
                                                 external 'DOSCALLS' index 430;

function LogClose (Handle: cardinal): cardinal; cdecl;
                                                 external 'DOSCALLS' index 431;

function LogAddEntries (Handle: cardinal; Service: cardinal;
      LogEntries: PLogEntryRec): cardinal; cdecl; external 'DOSCALLS' index 432;

function LogAddEntries (Handle: cardinal; Service: cardinal;
  var LogEntries: TLogEntryRec): cardinal; cdecl; external 'DOSCALLS' index 432;

function DosQueryMemState (PMem: pointer; var Size: cardinal;
                                         var State: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 307;

(* Todo:

function DosRawReadNPipe (HPIPE hPipe,
                                    PVOID pBuffer,
                                    ULONG cbRead,
                                    PULONG pcbActual); cdecl;
external 'DOSCALLS' index 246; - no documentation???

function DosRawWriteNPipe (HPIPE hPipe,
                                    PVOID pBuffer,
                                    ULONG cbWrite,
                                    PULONG pcbActual); cdecl;
external 'DOSCALLS' index 247; - no documentation???

function DosSetCP ...; cdecl;
external 'DOSCALLS' index 288; - no documentation???

function DosDynamicTrace ...; cdecl;
external 'DOSCALLS' index 316; - no documentation???

function DosRegisterPerfCtrs (PBYTE pbDataBlk,
                                       PBYTE pbTextBlk,
                                       ULONG flFlags); cdecl;
external 'DOSCALLS' index 367; - no documentation???

function DosQueryDOSProperty (SGID sgid,
                                        PSZ pszName,
                                        ULONG cb,
                                        PSZ pch); cdecl;
external 'DOSCALLS' index 373; - no documentation???

function DosSetDOSProperty (SGID sgid,
                                      PSZ pszName,
                                      ULONG cb,
                                      PSZ pch); cdecl;
external 'DOSCALLS' index 374; - no documentation???

function DosProfile ...; cdecl;
external 'DOSCALLS' index 377; - no documentation???
*)

function DosReplaceModule (OldModule, NewModule, BackupModule: PChar):
                                                               cardinal; cdecl;
external 'DOSCALLS' index 417;

(*
??? function DosTIB ...; cdecl;
external 'DOSCALLS' index 419;

??? function DosOpenChangeNotify ...; cdecl;
external 'DOSCALLS' index 440;

??? function DosResetChangeNotify ...; cdecl;
external 'DOSCALLS' index 441;

??? function DosCloseChangeNotify ...; cdecl;
external 'DOSCALLS' index 442;

??? function DosInitializePorthole ...; cdecl;
external 'DOSCALLS' index 580;

??? function DosQueryHeaderInfo ...; cdecl;
external 'DOSCALLS' index 582;

DosCreateThread2 2.45
DosDumpProcess 2.10
DosForceSystemDump - ???

??? function DosQueryPageUsage (...): cardinal; cdecl;
external 'DOSCALLS' index 358;

 DosPerfSystemCall 2.40


functionDosGetProcessorStatus (...): cardinal; cdecl;
external 'DOSCALLS' index 447;
 DosSetProcessorStatus = DOSCALLS.448
 DosCreateSpinLock     = DOSCALLS.449
 DosAcquireSpinLock    = DOSCALLS.450
 DosReleaseSpinLock    = DOSCALLS.451
 DosFreeSpinLock       = DOSCALLS.452
 DosListIO 2.45
 DosListIOL 2.45
 DosQueryABIOSSuport 2.10

___ functionDos16QueryModFromCS (...): ...
external 'DOSCALLS' index 359;

 DosQueryModFromEIP 2.10
*)
function DosQuerySysState (EntityList, EntityLevel, PID, TID: cardinal;
                                var Buffer; BufLen: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 368;

function DosQuerySysState (EntityList, EntityLevel, PID, TID: cardinal;
                          PDataBuf: pointer; cbBuf: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 368;

function DosAliasMem (pMem: pointer; Size: cardinal; var Alias: pointer;
                                             Flags: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 298;

(*
 DosQueryThreadAffinity
 DosSetThreadAffinity
 Dos16SysTrace
 DosVerifyPidTid 2.30
*)


end.
