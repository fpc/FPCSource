{
    $Id$
    This file is part of the Free Pascal run time library.
    Amiga exec.library include file
    Copyright (c) 1997 by Nils Sjoholm
    member of the Amiga RTL development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit Exec;

INTERFACE

{$I exec.inc}

procedure AbortIO(io : pIORequest);
procedure AddDevice(device : pDevice);
procedure AddHead(list : pList;
                  node : pNode);
procedure AddIntServer(intNum : ULONG;
                       Int : pInterrupt);
procedure AddLibrary(lib : pLibrary);
procedure AddMemHandler(memhand : pInterrupt);
procedure AddMemList(size, attr : ULONG;
                     pri : Longint;
                     base : Pointer;
                     name : STRPTR);
procedure AddPort(port : pMsgPort);
procedure AddResource(resource : Pointer);
procedure AddSemaphore(sigsem : pSignalSemaphore);
procedure AddTail(list : pList;
                  node : pNode);
procedure AddTask(task : pTask;
                  initialPC, finalPC : Pointer);
procedure Alert(alertNum : ULONG;
                parameters : Pointer);
function AllocAbs(bytesize : ULONG;
                  location : Pointer) : Pointer;
function Allocate(mem : pMemHeader;
                  bytesize : ULONG) : Pointer;
function AllocEntry(mem : pMemList) : pMemList;
function AllocMem(bytesize : ULONG;
                  reqs : ULONG) : Pointer;
function AllocPooled( pooleheader : Pointer;
                      memsize : ULONG ): Pointer;
function AllocSignal(signalNum : Longint) : Shortint;
function AllocTrap(trapNum : Longint) : Longint;
function AllocVec( size, reqm : ULONG ): Pointer;
function AttemptSemaphore(sigsem : pSignalSemaphore) : Boolean;
function AttemptSemaphoreShared(sigsem : pSignalSemaphore): ULONG;
function AvailMem(attr : ULONG) : ULONG;
procedure CacheClearE( cxa : Pointer;
                       lenght, caches : ULONG);
procedure CacheClearU;
function CacheControl( cachebits, cachemask: ULONG ): ULONG;
procedure CachePostDMA(vaddress, length_IntPtr : Pointer;
                        flags : ULONG );
function CachePreDMA(vaddress, length_intPtr : Pointer;
                     flags : ULONG): Pointer;
procedure Cause(Int : pInterrupt);
function CheckIO(io : pIORequest) : pIORequest;
procedure ChildFree( tid : Pointer);
procedure ChildOrphan( tid : Pointer);
procedure ChildStatus( tid : Pointer);
procedure ChildWait( tid : Pointer);
procedure CloseDevice(io : pIORequest);
procedure CloseLibrary(lib : pLibrary);
procedure ColdReboot;
procedure CopyMem(source, dest : Pointer;
                  size : ULONG);
procedure CopyMemQuick(source, dest : Pointer;
                       size : ULONG);
function CreateIORequest( mp : pMsgPort;
                          size : ULONG ): pIORequest;
function CreateMsgPort: pMsgPort;
function CreatePool( requrements,puddlesize,
                     puddletresh : ULONG ): Pointer;
procedure Deallocate(header : pMemHeader;
                     block : Pointer;
                     size : ULONG);
procedure Debug(Param : ULONG);
procedure DeleteIORequest( iorq : Pointer );
procedure DeleteMsgPort( mp : pMsgPort );
procedure DeletePool( poolheader : Pointer );
procedure Disable;
function DoIO(io : pIORequest) : Shortint;
procedure Enable;
procedure Enqueue(list : pList;
                  node : pNode);
function FindName(start : pList;
                  name : STRPTR) : pNode;
function FindPort(name : STRPTR): pMsgPort;
function FindResident(name : STRPTR) : pResident;
function FindSemaphore(name : STRPTR) : pSignalSemaphore;
function FindTask(name : STRPTR) : pTask;
procedure Forbid;
procedure FreeEntry(memList : pMemList);
procedure ExecFreeMem(memBlock : Pointer;
                  size : ULONG);
procedure FreePooled( poolheader, memory: Pointer;
                      memsize: ULONG);
procedure FreeSignal(signalNum : Longint);
procedure FreeTrap(signalNum : ULONG);
procedure FreeVec( memory : Pointer );
function GetCC : ULONG;
function GetMsg(port : pMsgPort): pMessage;
procedure InitCode(startClass, version : ULONG);
procedure InitResident(resident : pResident;
                       segList : ULONG);
procedure InitSemaphore(sigsem : pSignalSemaphore);
procedure InitStruct(table, memory : Pointer;
                     size : ULONG);
procedure Insert(list : pList;
                 node, listNode : pNode);
procedure MakeFunctions(target, functionarray : Pointer ;
                       dispbase : ULONG);
function MakeLibrary(vec, struct, init : Pointer;
                     dSize : ULONG ;
                     segList : Pointer) : pLibrary;
function ObtainQuickVector(interruptCode : Pointer) : ULONG;
procedure ObtainSemaphore(sigsem : pSignalSemaphore);
procedure ObtainSemaphoreList(semlist : pList);
procedure ObtainSemaphoreShared(sigsem : pSignalSemaphore);
function OldOpenLibrary(lib : STRPTR): pLibrary;
function OpenDevice(devName : STRPTR;
                    unitNumber : ULONG;
                    io : pIORequest; flags : ULONG) : Shortint;
function OpenLibrary(libName : STRPTR;
                     version : Integer) : pLibrary;
function OpenResource(resname : STRPTR): Pointer;
procedure Permit;
function Procure(sem : pSemaphore;
                 bid : pMessage) : Boolean;
procedure PutMsg(port : pMsgPort;
                 mess : pMessage);
procedure RawDoFmt(Form : STRPTR;
                   data, putChProc, putChData : Pointer);
procedure ReleaseSemaphore(sigsem : pSignalSemaphore);
procedure ReleaseSemaphoreList(siglist : pList);
procedure RemDevice(device : pDevice);
function RemHead(list : pList) : pNode;
procedure RemIntServer(intNum : Longint;
                       Int : pInterrupt);
procedure RemLibrary(lib : pLibrary);
procedure RemMemHandler(memhand : pInterrupt);
procedure Remove(node : pNode);
procedure RemPort(port : pMsgPort);
procedure RemResource(resname : Pointer);
procedure RemSemaphore(sigsem : pSignalSemaphore);
function RemTail(list : pList) : pNode;
procedure RemTask(task : pTask);
procedure ReplyMsg(mess : pMessage);
procedure SendIO(io : pIORequest);
function SetExcept(newSignals, signalMask : ULONG) : ULONG;
function SetFunction(lib : pLibrary;
                     funcOff : LONG;
                     funcEntry : Pointer) : Pointer;
function SetIntVector(intNum : Longint;
                      Int : pInterrupt) : pInterrupt;
function SetSignal(newSignals, signalMask : ULONG) : ULONG;
function SetSR(newSR, mask : ULONG) : ULONG;
function SetTaskPri(task : pTask;
                    priority : Longint) : Shortint;
procedure Signal(task : pTask; signals : ULONG);
procedure StackSwap( StackSwapRecord : Pointer );
procedure SumKickData;
procedure SumLibrary(lib : pLibrary);
function SuperState : Pointer;
function Supervisor(thefunc : Pointer): ULONG;
function TypeOfMem(mem : Pointer) : ULONG;
procedure UserState(s : Pointer);
procedure Vacate(sigsem : pSignalSemaphore;
                 bidMsg : pSemaphoreMessage);
function Wait(signals : ULONG) : ULONG;
function WaitIO(io : pIORequest) : Shortint;
function WaitPort(port : pMsgPort): pMessage;

{*  Exec support functions from amiga.lib  *}

procedure BeginIO (ioRequest: pIORequest);
function CreateExtIO (port: pMsgPort; size: Longint): pIORequest;
procedure DeleteExtIO (ioReq: pIORequest);
function CreateStdIO (port: pMsgPort): pIOStdReq;
procedure DeleteStdIO (ioReq: pIOStdReq);
function CreatePort (name: STRPTR; pri: integer): pMsgPort;
procedure DeletePort (port: pMsgPort);
function CreateTask (name: STRPTR; pri: longint; 
                     initPC : Pointer;
             stackSize : ULONG): pTask; 
procedure DeleteTask (task: pTask);
procedure NewList (list: pList);

IMPLEMENTATION

{*  Exec support functions from amiga.lib  *}

procedure BeginIO (ioRequest: pIORequest);
begin
   asm
      move.l  a6,-(a7)
      move.l  ioRequest,a1    ; get IO Request
      move.l  20(a1),a6      ; extract Device ptr
      jsr     -30(a6)        ; call BEGINIO directly
      move.l  (a7)+,a6
   end;
end;

function CreateExtIO (port: pMsgPort; size: Longint): pIORequest;
var
   IOReq: pIORequest;
begin
    IOReq := NIL;
    if port <> NIL then
    begin
        IOReq := AllocMem(size, MEMF_CLEAR or MEMF_PUBLIC);
        if IOReq <> NIL then
        begin
            IOReq^.io_Message.mn_Node.ln_Type   := NT_REPLYMSG;
            IOReq^.io_Message.mn_Length    := size;
            IOReq^.io_Message.mn_ReplyPort := port;
        end;
    end;
    CreateExtIO := IOReq;
end;


procedure DeleteExtIO (ioReq: pIORequest);
begin
    if ioReq <> NIL then
    begin
        ioReq^.io_Message.mn_Node.ln_Type := $FF;
        ioReq^.io_Message.mn_ReplyPort    := pMsgPort(-1);
        ioReq^.io_Device                  := pDevice(-1);
        ExecFreeMem(ioReq, ioReq^.io_Message.mn_Length);
    end
end;


function CreateStdIO (port: pMsgPort): pIOStdReq;
begin
    CreateStdIO := pIOStdReq(CreateExtIO(port, sizeof(tIOStdReq)))
end;


procedure DeleteStdIO (ioReq: pIOStdReq);
begin
    DeleteExtIO(pIORequest(ioReq))
end;


function CreatePort (name: STRPTR; pri: integer): pMsgPort;
var
   port   : pMsgPort;
   sigbit : shortint;
begin
    port  := NIL;
    sigbit := AllocSignal(-1);
    if sigbit <> -1 then
    begin
        port := AllocMem(sizeof(tMsgPort), MEMF_CLEAR or MEMF_PUBLIC);
        if port = NIL then
            FreeSignal(sigbit)
        else
            begin
                port^.mp_Node.ln_Name  := name;
                port^.mp_Node.ln_Pri   := pri;
                port^.mp_Node.ln_Type  := NT_MSGPORT;

                port^.mp_Flags    := PA_SIGNAL;
                port^.mp_SigBit   := sigbit;
                port^.mp_SigTask  := FindTask(NIL);

                if name <> NIL then
                    AddPort(port)
                else
                    NewList(@port^.mp_MsgList);
            end;
    end;
    CreatePort := port;
end;


procedure DeletePort (port: pMsgPort);
begin
    if port <> NIL then
    begin
        if port^.mp_Node.ln_Name <> NIL then
            RemPort(port);

        port^.mp_SigTask       := pTask(-1);
        port^.mp_MsgList.lh_Head  := pNode(-1);
        FreeSignal(port^.mp_SigBit);
        ExecFreeMem(port, sizeof(tMsgPort));
    end;
end;


function CreateTask (name: STRPTR; pri: longint;
        initPC: pointer; stackSize: ULONG): pTask;
var
   memlist : pMemList;
   task    : pTask;
   totalsize : Longint;
begin
    task  := NIL;
    stackSize   := (stackSize + 3) and not 3;
    totalsize := sizeof(tMemList) + sizeof(tTask) + stackSize;

    memlist := AllocMem(totalsize, MEMF_PUBLIC + MEMF_CLEAR);
    if memlist <> NIL then begin
       memlist^.ml_NumEntries := 1;
       memlist^.ml_ME[0].me_Un.meu_Addr := Pointer(memlist + 1);
       memlist^.ml_ME[0].me_Length := totalsize - sizeof(tMemList);

       task := pTask(memlist + sizeof(tMemList) + stackSize);
       task^.tc_Node.ln_Pri := pri;
       task^.tc_Node.ln_Type := NT_TASK;
       task^.tc_Node.ln_Name := name;
       task^.tc_SPLower := Pointer(memlist + sizeof(tMemList));
       task^.tc_SPUpper := Pointer(task^.tc_SPLower + stackSize);
       task^.tc_SPReg := task^.tc_SPUpper;

       NewList(@task^.tc_MemEntry);
       AddTail(@task^.tc_MemEntry,@memlist^.ml_Node);

       AddTask(task,initPC,NIL) 
    end;
    CreateTask := task;
end;


procedure DeleteTask (task: pTask);
begin
    RemTask(task)
end;


procedure NewList (list: pList);
begin
    with list^ do
    begin
        lh_Head     := pNode(@lh_Tail);
        lh_Tail     := NIL;
        lh_TailPred := pNode(@lh_Head)
    end
end;



procedure AbortIO(io : pIORequest);
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  io,a1
       MOVE.L  _ExecBase,A6
       JSR -480(A6)
       MOVE.L  (A7)+,A6
   end;
end;

procedure AddDevice(device : pDevice);
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  device,a1
       MOVE.L  _ExecBase,A6
       JSR -432(A6)
       MOVE.L  (A7)+,A6
   end;
end;

procedure AddHead(list : pList;
                  node : pNode);
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  list,a0
       MOVE.L  node,a1
       MOVE.L  _ExecBase,A6
       JSR -240(A6)
       MOVE.L  (A7)+,A6
   end;
end;

procedure AddIntServer(intNum : ULONG;
                       Int : pInterrupt);
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  intNum,d0
       MOVE.L  Int,a1
       MOVE.L  _ExecBase,A6
       JSR -168(A6)
       MOVE.L  (A7)+,A6
   end;
end;

procedure AddLibrary(lib : pLibrary);
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  lib,a1
       MOVE.L  _ExecBase,A6
       JSR -396(A6)
       MOVE.L  (A7)+,A6
   end;
end;

procedure AddMemHandler(memhand : pInterrupt);
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  memhand,a1
       MOVE.L  _ExecBase,A6
       JSR -774(A6)
       MOVE.L  (A7)+,A6
   end;
end;

procedure AddMemList(size, attr : ULONG;
                     pri : Longint;
                     base : Pointer;
                     name : STRPTR);
begin
   asm
       MOVEM.L d2/a6,-(A7)
       MOVE.L  size,d0
       MOVE.L  attr,d1
       MOVE.L  pri,d2
       MOVE.L  base,a0
       MOVE.L  name,a1
       MOVE.L  _ExecBase,A6
       JSR -618(A6)
       MOVEM.L (A7)+,d2/a6
   end;
end;

procedure AddPort(port : pMsgPort);
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  port,a1
       MOVE.L  _ExecBase,A6
       JSR -354(A6)
       MOVE.L  (A7)+,A6
   end;
end;

procedure AddResource(resource : Pointer);
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  resource,a1
       MOVE.L  _ExecBase,A6
       JSR -486(A6)
       MOVE.L  (A7)+,A6
   end;
end;

procedure AddSemaphore(sigsem : pSignalSemaphore);
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  sigsem,a1
       MOVE.L  _ExecBase,A6
       JSR -600(A6)
       MOVE.L  (A7)+,A6
   end;
end;

procedure AddTail(list : pList;
                  node : pNode);
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  list,a0
       MOVE.L  node,a1
       MOVE.L  _ExecBase,A6
       JSR -246(A6)
       MOVE.L  (A7)+,A6
   end;
end;

procedure AddTask(task : pTask;
                  initialPC, finalPC : Pointer);
begin
   asm
       MOVEM.L a2/a3/a6,-(A7)
       MOVE.L  task,a1
       MOVE.L  initialPC,a2
       MOVE.L  finalPC,a3
       MOVE.L  _ExecBase,A6
       JSR -282(A6)
       MOVEM.L (A7)+,a2/a3/a6
   end;
end;

procedure Alert(alertNum : ULONG;
                parameters : Pointer);
begin
   asm
       MOVEM.L d7/a6,-(A7)
       MOVE.L  alertNum,d7
       MOVE.L  _ExecBase,A6
       JSR -108(A6)
       MOVEM.L (A7)+,d7/a6
   end;
end;

function AllocAbs(bytesize : ULONG;
                  location : Pointer) : Pointer;
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  bytesize,d0
       MOVE.L  location,a1
       MOVE.L  _ExecBase,A6
       JSR -204(A6)
       MOVE.L  (A7)+,A6
       MOVE.L  d0,@RESULT
   end;
end;

function Allocate(mem : pMemHeader;
                  bytesize : ULONG) : Pointer;
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  mem,a0
       MOVE.L  bytesize,d0
       MOVE.L  _ExecBase,A6
       JSR -186(A6)
       MOVE.L  (A7)+,A6
       MOVE.L  d0,@RESULT
   end;
end;

function AllocEntry(mem : pMemList) : pMemList;
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  mem,a0
       MOVE.L  _ExecBase,A6
       JSR -222(A6)
       MOVE.L  (A7)+,A6
       MOVE.L  d0,@RESULT
   end;
end;

function AllocMem(bytesize : ULONG;
                  reqs : ULONG) : Pointer;
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  bytesize,d0
       MOVE.L  reqs,d1
       MOVE.L  _ExecBase,A6
       JSR -198(A6)
       MOVE.L  (A7)+,A6
       MOVE.L  d0,@RESULT
   end;
end;

function AllocPooled( pooleheader : Pointer;
                      memsize : ULONG ): Pointer;
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  pooleheader,a0
       MOVE.L  memsize,d0
       MOVE.L  _ExecBase,A6
       JSR -708(A6)
       MOVE.L  (A7)+,A6
       MOVE.L  d0,@RESULT
   end;
end;

function AllocSignal(signalNum : Longint) : Shortint;
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  signalNum,d0
       MOVE.L  _ExecBase,A6
       JSR -330(A6)
       MOVE.L  (A7)+,A6
       MOVE.B  d0,@RESULT
   end;
end;

function AllocTrap(trapNum : Longint) : Longint;
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  trapNum,d0
       MOVE.L  _ExecBase,A6
       JSR -342(A6)
       MOVE.L  (A7)+,A6
       MOVE.L  d0,@RESULT
   end;
end;

function AllocVec( size, reqm : ULONG ): Pointer;
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  size,d0
       MOVE.L  reqm,d1
       MOVE.L  _ExecBase,A6
       JSR -684(A6)
       MOVE.L  (A7)+,A6
       MOVE.L  d0,@RESULT
   end;
end;

function AttemptSemaphore(sigsem : pSignalSemaphore) : Boolean;
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  sigsem,a0
       MOVE.L  _ExecBase,A6
       JSR -576(A6)
       MOVE.L  (A7)+,A6
       TST.W   D0
       BEQ.B   @end
       MOVEQ   #1,D0
    @end: MOVE.B  D0,@RESULT
   end;
end;

function AttemptSemaphoreShared(sigsem : pSignalSemaphore): ULONG;
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  sigsem,a0
       MOVE.L  _ExecBase,A6
       JSR -720(A6)
       MOVE.L  (A7)+,A6
       MOVE.L  d0,@RESULT
   end;
end;

function AvailMem(attr : ULONG) : ULONG;
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  attr,d1
       MOVE.L  _ExecBase,A6
       JSR -216(A6)
       MOVE.L  (A7)+,A6
       MOVE.L  d0,@RESULT
   end;
end;

procedure CacheClearE( cxa : Pointer;
                       lenght, caches : ULONG);
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  cxa,a0
       MOVE.L  lenght,d0
       MOVE.L  caches,d1
       MOVE.L  _ExecBase,A6
       JSR -642(A6)
       MOVE.L  (A7)+,A6
   end;
end;

procedure CacheClearU;
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  _ExecBase,A6
       JSR -636(A6)
       MOVE.L  (A7)+,A6
   end;
end;

function CacheControl( cachebits, cachemask: ULONG ): ULONG;
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  cachebits,d0
       MOVE.L  cachemask,d1
       MOVE.L  _ExecBase,A6
       JSR -648(A6)
       MOVE.L  (A7)+,A6
       MOVE.L  d0,@RESULT
   end;
end;

procedure CachePostDMA(vaddress, length_IntPtr : Pointer;
                        flags : ULONG );
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  vaddress,a0
       MOVE.L  length_IntPtr,a1
       MOVE.L  flags,d0
       MOVE.L  _ExecBase,A6
       JSR -768(A6)
       MOVE.L  (A7)+,A6
   end;
end;

function CachePreDMA(vaddress, length_intPtr : Pointer;
                     flags : ULONG): Pointer;
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  vaddress,a0
       MOVE.L  length_intPtr,a1
       MOVE.L  flags,d0
       MOVE.L  _ExecBase,A6
       JSR -762(A6)
       MOVE.L  (A7)+,A6
       MOVE.L  d0,@RESULT
   end;
end;

procedure Cause(Int : pInterrupt);
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  Int,a1
       MOVE.L  _ExecBase,A6
       JSR -180(A6)
       MOVE.L  (A7)+,A6
   end;
end;

function CheckIO(io : pIORequest) : pIORequest;
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  io,a1
       MOVE.L  _ExecBase,A6
       JSR -468(A6)
       MOVE.L  (A7)+,A6
       MOVE.L  d0,@RESULT
   end;
end;

procedure ChildFree( tid : Pointer);
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  tid,d0
       MOVE.L  _ExecBase,A6
       JSR -738(A6)
       MOVE.L  (A7)+,A6
   end;
end;

procedure ChildOrphan( tid : Pointer);
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  tid,d0
       MOVE.L  _ExecBase,A6
       JSR -744(A6)
       MOVE.L  (A7)+,A6
   end;
end;

procedure ChildStatus( tid : Pointer);
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  tid,d0
       MOVE.L  _ExecBase,A6
       JSR -750(A6)
       MOVE.L  (A7)+,A6
   end;
end;

procedure ChildWait( tid : Pointer);
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  tid,d0
       MOVE.L  _ExecBase,A6
       JSR -756(A6)
       MOVE.L  (A7)+,A6
   end;
end;

procedure CloseDevice(io : pIORequest);
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  io,a1
       MOVE.L  _ExecBase,A6
       JSR -450(A6)
       MOVE.L  (A7)+,A6
   end;
end;

procedure CloseLibrary(lib : pLibrary);
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  lib,a1
       MOVE.L  _ExecBase,A6
       JSR -414(A6)
       MOVE.L  (A7)+,A6
   end;
end;

procedure ColdReboot;
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  _ExecBase,A6
       JSR -726(A6)
       MOVE.L  (A7)+,A6
   end;
end;

procedure CopyMem(source, dest : Pointer;
                  size : ULONG);
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  source,a0
       MOVE.L  dest,a1
       MOVE.L  size,d0
       MOVE.L  _ExecBase,A6
       JSR -624(A6)
       MOVE.L  (A7)+,A6
   end;
end;

procedure CopyMemQuick(source, dest : Pointer;
                       size : ULONG);
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  source,a0
       MOVE.L  dest,a1
       MOVE.L  size,d0
       MOVE.L  _ExecBase,A6
       JSR -630(A6)
       MOVE.L  (A7)+,A6
   end;
end;

function CreateIORequest( mp : pMsgPort;
                          size : ULONG ): pIORequest;
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  mp,a0
       MOVE.L  size,d0
       MOVE.L  _ExecBase,A6
       JSR -654(A6)
       MOVE.L  (A7)+,A6
       MOVE.L  d0,@RESULT
   end;
end;

function CreateMsgPort: pMsgPort;
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  _ExecBase,A6
       JSR -666(A6)
       MOVE.L  (A7)+,A6
       MOVE.L  d0,@RESULT
   end;
end;

function CreatePool( requrements,puddlesize,
                     puddletresh : ULONG ): Pointer;
begin
   asm
       MOVEM.L d2/a6,-(A7)
       MOVE.L  requrements,d0
       MOVE.L  puddlesize,d1
       MOVE.L  puddletresh,d2
       MOVE.L  _ExecBase,A6
       JSR -696(A6)
       MOVEM.L (A7)+,d2/a6
       MOVE.L  d0,@RESULT
   end;
end;

procedure Deallocate(header : pMemHeader;
                     block : Pointer;
                     size : ULONG);
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  header,a0
       MOVE.L  block,a1
       MOVE.L  size,d0
       MOVE.L  _ExecBase,A6
       JSR -192(A6)
       MOVE.L  (A7)+,A6
   end;
end;

procedure Debug(Param : ULONG);
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  Param,d0
       MOVE.L  _ExecBase,A6
       JSR -114(A6)
       MOVE.L  (A7)+,A6
   end;
end;

procedure DeleteIORequest( iorq : Pointer );
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  iorq,a0
       MOVE.L  _ExecBase,A6
       JSR -660(A6)
       MOVE.L  (A7)+,A6
   end;
end;

procedure DeleteMsgPort( mp : pMsgPort );
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  mp,a0
       MOVE.L  _ExecBase,A6
       JSR -672(A6)
       MOVE.L  (A7)+,A6
   end;
end;

procedure DeletePool( poolheader : Pointer );
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  poolheader,a0
       MOVE.L  _ExecBase,A6
       JSR -702(A6)
       MOVE.L  (A7)+,A6
   end;
end;

procedure Disable;
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  _ExecBase,A6
       JSR -120(A6)
       MOVE.L  (A7)+,A6
   end;
end;

function DoIO(io : pIORequest) : Shortint;
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  io,a1
       MOVE.L  _ExecBase,A6
       JSR -456(A6)
       MOVE.L  (A7)+,A6
       MOVE.L  d0,@RESULT
   end;
end;

procedure Enable;
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  _ExecBase,A6
       JSR -126(A6)
       MOVE.L  (A7)+,A6
   end;
end;

procedure Enqueue(list : pList;
                  node : pNode);
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  list,a0
       MOVE.L  node,a1
       MOVE.L  _ExecBase,A6
       JSR -270(A6)
       MOVE.L  (A7)+,A6
   end;
end;

function FindName(start : pList;
                  name : STRPTR) : pNode;
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  start,a0
       MOVE.L  name,a1
       MOVE.L  _ExecBase,A6
       JSR -276(A6)
       MOVE.L  (A7)+,A6
       MOVE.L  d0,@RESULT
   end;
end;

function FindPort(name : STRPTR): pMsgPort;
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  name,a1
       MOVE.L  _ExecBase,A6
       JSR -390(A6)
       MOVE.L  (A7)+,A6
       MOVE.L  d0,@RESULT
   end;
end;

function FindResident(name : STRPTR) : pResident;
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  name,a1
       MOVE.L  _ExecBase,A6
       JSR -96(A6)
       MOVE.L  (A7)+,A6
       MOVE.L  d0,@RESULT
   end;
end;

function FindSemaphore(name : STRPTR) : pSignalSemaphore;
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  name,a1
       MOVE.L  _ExecBase,A6
       JSR -594(A6)
       MOVE.L  (A7)+,A6
       MOVE.L  d0,@RESULT
   end;
end;

function FindTask(name : STRPTR) : pTask;
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  name,a1
       MOVE.L  _ExecBase,A6
       JSR -294(A6)
       MOVE.L  (A7)+,A6
       MOVE.L  d0,@RESULT
   end;
end;

procedure Forbid;
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  _ExecBase,A6
       JSR -132(A6)
       MOVE.L  (A7)+,A6
   end;
end;

procedure FreeEntry(memList : pMemList);
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  memlist,a0
       MOVE.L  _ExecBase,A6
       JSR -228(A6)
       MOVE.L  (A7)+,A6
   end;
end;

procedure ExecFreeMem(memBlock : Pointer;
                  size : ULONG);
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  memBlock,a1
       MOVE.L  size,d0
       MOVE.L  _ExecBase,A6
       JSR -210(A6)
       MOVE.L  (A7)+,A6
   end;
end;

procedure FreePooled( poolheader, memory: Pointer;
                      memsize: ULONG);
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  poolheader,a0
       MOVE.L  memory,a1
       MOVE.L  memsize,d0
       MOVE.L  _ExecBase,A6
       JSR -714(A6)
       MOVE.L  (A7)+,A6
   end;
end;

procedure FreeSignal(signalNum : Longint);
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  signalNum,d0
       MOVE.L  _ExecBase,A6
       JSR -336(A6)
       MOVE.L  (A7)+,A6
   end;
end;

procedure FreeTrap(signalNum : ULONG);
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  signalNum,d0
       MOVE.L  _ExecBase,A6
       JSR -348(A6)
       MOVE.L  (A7)+,A6
   end;
end;

procedure FreeVec( memory : Pointer );
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  memory,a1
       MOVE.L  _ExecBase,A6
       JSR -690(A6)
       MOVE.L  (A7)+,A6
   end;
end;

function GetCC : ULONG;
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  _ExecBase,A6
       JSR -528(A6)
       MOVE.L  (A7)+,A6
       MOVE.L  d0,@RESULT
   end;
end;

function GetMsg(port : pMsgPort): pMessage;
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  port,a0
       MOVE.L  _ExecBase,A6
       JSR -372(A6)
       MOVE.L  (A7)+,A6
       MOVE.L  d0,@RESULT
   end;
end;

procedure InitCode(startClass, version : ULONG);
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  startClass,d0
       MOVE.L  version,d1
       MOVE.L  _ExecBase,A6
       JSR -72(A6)
       MOVE.L  (A7)+,A6
   end;
end;

procedure InitResident(resident : pResident;
                       segList : ULONG);
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  resident,a1
       MOVE.L  seglist,d1
       MOVE.L  _ExecBase,A6
       JSR -102(A6)
       MOVE.L  (A7)+,A6
   end;
end;

procedure InitSemaphore(sigsem : pSignalSemaphore);
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  sigsem,a0
       MOVE.L  _ExecBase,A6
       JSR -558(A6)
       MOVE.L  (A7)+,A6
   end;
end;

procedure InitStruct(table, memory : Pointer;
                     size : ULONG);
begin
   asm
       MOVEM.L a2/a6,-(A7)
       MOVE.L  table,a1
       MOVE.L  memory,a2
       MOVE.L  size,d0
       MOVE.L  _ExecBase,A6
       JSR -78(A6)
       MOVEM.L (A7)+,a2/a6
   end;
end;

procedure Insert(list : pList;
                 node, listNode : pNode);
begin
   asm
       MOVEM.L a2/a6,-(A7)
       MOVE.L  list,a0
       MOVE.L  node,a1
       MOVE.L  listNode,a2
       MOVE.L  _ExecBase,A6
       JSR -234(A6)
       MOVEM.L (A7)+,a2/a6
   end;
end;

procedure MakeFunctions(target, functionarray : Pointer ;
                       dispbase : ULONG);
begin
   asm
       MOVEM.L a2/a6,-(A7)
       MOVE.L  target,a0
       MOVE.L  functionarray,a1
       MOVE.L  dispbase,a2
       MOVE.L  _ExecBase,A6
       JSR -90(A6)
       MOVEM.L (A7)+,a2/a6
   end;
end;

function MakeLibrary(vec, struct, init : Pointer;
                     dSize : ULONG ;
                     segList : Pointer) : pLibrary;
begin
   asm
       MOVEM.L a2/a6,-(A7)
       MOVE.L  vec,a0
       MOVE.L  struct,a1
       MOVE.L  init,a2
       MOVE.L  dSize,d0
       MOVE.L  seglist,d1
       MOVE.L  _ExecBase,A6
       JSR -84(A6)
       MOVEM.L (A7)+,a2/a6
       MOVE.L  d0,@RESULT
   end;
end;

function ObtainQuickVector(interruptCode : Pointer) : ULONG;
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  interruptCode,a0
       MOVE.L  _ExecBase,A6
       JSR -786(A6)
       MOVE.L  (A7)+,A6
       MOVE.L  d0,@RESULT
   end;
end;

procedure ObtainSemaphore(sigsem : pSignalSemaphore);
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  sigsem,a0
       MOVE.L  _ExecBase,A6
       JSR -564(A6)
       MOVE.L  (A7)+,A6
   end;
end;

procedure ObtainSemaphoreList(semlist : pList);
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  semlist,a0
       MOVE.L  _ExecBase,A6
       JSR -582(A6)
       MOVE.L  (A7)+,A6
   end;
end;

procedure ObtainSemaphoreShared(sigsem : pSignalSemaphore);
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  sigsem,a0
       MOVE.L  _ExecBase,A6
       JSR -678(A6)
       MOVE.L  (A7)+,A6
   end;
end;

function OldOpenLibrary(lib : STRPTR): pLibrary;
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  lib,a1
       MOVE.L  _ExecBase,A6
       JSR -408(A6)
       MOVE.L  (A7)+,A6
       MOVE.L  d0,@RESULT
   end;
end;

function OpenDevice(devName : STRPTR;
                    unitNumber : ULONG;
                    io : pIORequest; flags : ULONG) : Shortint;
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  devName,a0
       MOVE.L  unitNumber,d0
       MOVE.L  io,a1
       MOVE.L  flags,d1
       MOVE.L  _ExecBase,A6
       JSR -444(A6)
       MOVE.L  (A7)+,A6
       MOVE.B  d0,@RESULT
   end;
end;

function OpenLibrary(libName : STRPTR;
                     version : Integer) : pLibrary;
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  libName,a1
       MOVE.L  version,d0
       MOVE.L  _ExecBase,A6
       JSR -552(A6)
       MOVE.L  (A7)+,A6
       MOVE.L  d0,@RESULT
   end;
end;

function OpenResource(resname : STRPTR): Pointer;
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  resname,a1
       MOVE.L  _ExecBase,A6
       JSR -498(A6)
       MOVE.L  (A7)+,A6
       MOVE.L  d0,@RESULT
   end;
end;

procedure Permit;
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  _ExecBase,A6
       JSR -138(A6)
       MOVE.L  (A7)+,A6
   end;
end;

function Procure(sem : pSemaphore;
                 bid : pMessage) : Boolean;
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  sem,a0
       MOVE.L  bid,a1
       MOVE.L  _ExecBase,A6
       JSR -540(A6)
       MOVE.L  (A7)+,A6
       TST.W   D0
       BEQ.B   @end
       MOVEQ   #1,D0
   @end: MOVE.B  D0,@RESULT
   end;
end;

procedure PutMsg(port : pMsgPort;
                 mess : pMessage);
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  port,a0
       MOVE.L  mess,a1
       MOVE.L  _ExecBase,A6
       JSR -366(A6)
       MOVE.L  (A7)+,A6
   end;
end;

procedure RawDoFmt(Form : STRPTR;
                   data, putChProc, putChData : Pointer);
begin
   asm
       MOVEM.L a2/a3/a6,-(A7)
       MOVE.L  Form,a0
       MOVE.L  data,a1
       MOVE.L  putChProc,a2
       MOVE.L  putChData,a3
       MOVE.L  _ExecBase,A6
       JSR -522(A6)
       MOVEM.L (A7)+,a2/a3/a6
   end;
end;

procedure ReleaseSemaphore(sigsem : pSignalSemaphore);
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  sigsem,a0
       MOVE.L  _ExecBase,A6
       JSR -570(A6)
       MOVE.L  (A7)+,A6
   end;
end;

procedure ReleaseSemaphoreList(siglist : pList);
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  siglist,a0
       MOVE.L  _ExecBase,A6
       JSR -588(A6)
       MOVE.L  (A7)+,A6
   end;
end;

procedure RemDevice(device : pDevice);
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  device,a1
       MOVE.L  _ExecBase,A6
       JSR -438(A6)
       MOVE.L  (A7)+,A6
   end;
end;

function RemHead(list : pList) : pNode;
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  list,a0
       MOVE.L  _ExecBase,A6
       JSR -258(A6)
       MOVE.L  (A7)+,A6
       MOVE.L  d0,@RESULT
   end;
end;

procedure RemIntServer(intNum : Longint;
                       Int : pInterrupt);
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  intNum,d0
       MOVE.L  Int,a1
       MOVE.L  _ExecBase,A6
       JSR -174(A6)
       MOVE.L  (A7)+,A6
   end;
end;

procedure RemLibrary(lib : pLibrary);
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  lib,a1
       MOVE.L  _ExecBase,A6
       JSR -402(A6)
       MOVE.L  (A7)+,A6
   end;
end;

procedure RemMemHandler(memhand : pInterrupt);
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  memhand,a1
       MOVE.L  _ExecBase,A6
       JSR -780(A6)
       MOVE.L  (A7)+,A6
   end;
end;

procedure Remove(node : pNode);
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  node,a1
       MOVE.L  _ExecBase,A6
       JSR -252(A6)
       MOVE.L  (A7)+,A6
   end;
end;

procedure RemPort(port : pMsgPort);
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  port,a1
       MOVE.L  _ExecBase,A6
       JSR -360(A6)
       MOVE.L  (A7)+,A6
   end;
end;

procedure RemResource(resname : Pointer);
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  resname,a1
       MOVE.L  _ExecBase,A6
       JSR -492(A6)
       MOVE.L  (A7)+,A6
   end;
end;

procedure RemSemaphore(sigsem : pSignalSemaphore);
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  sigsem,a1
       MOVE.L  _ExecBase,A6
       JSR -606(A6)
       MOVE.L  (A7)+,A6
   end;
end;

function RemTail(list : pList) : pNode;
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  list,a0
       MOVE.L  _ExecBase,A6
       JSR -264(A6)
       MOVE.L  (A7)+,A6
       MOVE.L  d0,@RESULT
   end;
end;

procedure RemTask(task : pTask);
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  task,a1
       MOVE.L  _ExecBase,A6
       JSR -288(A6)
       MOVE.L  (A7)+,A6
   end;
end;

procedure ReplyMsg(mess : pMessage);
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  mess,a1
       MOVE.L  _ExecBase,A6
       JSR -378(A6)
       MOVE.L  (A7)+,A6
   end;
end;

procedure SendIO(io : pIORequest);
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  io,a1
       MOVE.L  _ExecBase,A6
       JSR -462(A6)
       MOVE.L  (A7)+,A6
   end;
end;

function SetExcept(newSignals, signalMask : ULONG) : ULONG;
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  newSignals,d0
       MOVE.L  signalMask,d1
       MOVE.L  _ExecBase,A6
       JSR -312(A6)
       MOVE.L  (A7)+,A6
       MOVE.L  d0,@RESULT
   end;
end;

function SetFunction(lib : pLibrary;
                     funcOff : LONG;
                     funcEntry : Pointer) : Pointer;
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  lib,a1
       MOVE.L  funcOff,a0
       MOVE.L  funcEntry,d0
       MOVE.L  _ExecBase,A6
       JSR -420(A6)
       MOVE.L  (A7)+,A6
       MOVE.L  d0,@RESULT
   end;
end;

function SetIntVector(intNum : Longint;
                      Int : pInterrupt) : pInterrupt;
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  intNum,d0
       MOVE.L  Int,a1
       MOVE.L  _ExecBase,A6
       JSR -162(A6)
       MOVE.L  (A7)+,A6
       MOVE.L  d0,@RESULT
   end;
end;

function SetSignal(newSignals, signalMask : ULONG) : ULONG;
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  newSignals,d0
       MOVE.L  signalMask,d1
       MOVE.L  _ExecBase,A6
       JSR -306(A6)
       MOVE.L  (A7)+,A6
       MOVE.L  d0,@RESULT
   end;
end;

function SetSR(newSR, mask : ULONG) : ULONG;
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  newSR,d0
       MOVE.L  mask,d1
       MOVE.L  _ExecBase,A6
       JSR -144(A6)
       MOVE.L  (A7)+,A6
       MOVE.L  d0,@RESULT
   end;
end;

function SetTaskPri(task : pTask;
                    priority : Longint) : Shortint;
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  task,a1
       MOVE.L  priority,d0
       MOVE.L  _ExecBase,A6
       JSR -300(A6)
       MOVE.L  (A7)+,A6
       MOVE.B  d0,@RESULT
   end;
end;

procedure Signal(task : pTask; signals : ULONG);
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  task,a1
       MOVE.L  signals,d0
       MOVE.L  _ExecBase,A6
       JSR -324(A6)
       MOVE.L  (A7)+,A6
   end;
end;

procedure StackSwap( StackSwapRecord : Pointer );
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  StackSwapRecord,a0
       MOVE.L  _ExecBase,A6
       JSR -732(A6)
       MOVE.L  (A7)+,A6
   end;
end;

procedure SumKickData;
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  _ExecBase,A6
       JSR -612(A6)
       MOVE.L  (A7)+,A6
   end;
end;

procedure SumLibrary(lib : pLibrary);
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  lib,a1
       MOVE.L  _ExecBase,A6
       JSR -426(A6)
       MOVE.L  (A7)+,A6
   end;
end;

function SuperState : Pointer;
begin
   asm
       MOVE.L  A6,-(A7)
       JSR -150(A6)
       MOVE.L  (A7)+,A6
       MOVE.L  d0,@RESULT
   end;
end;

function Supervisor(thefunc : Pointer): ULONG;
begin
   asm
       MOVEM.L a5/a6,-(A7)
       MOVE.L  _ExecBase,A6
       MOVE.L  thefunc,a5
       MOVE.L  _ExecBase,A6
       JSR -30(A6)
       MOVEM.L (A7)+,a5/a6
       MOVE.L  d0,@RESULT
   end;
end;

function TypeOfMem(mem : Pointer) : ULONG;
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  mem,a1
       MOVE.L  _ExecBase,A6
       JSR -534(A6)
       MOVE.L  (A7)+,A6
       MOVE.L  d0,@RESULT
   end;
end;

procedure UserState(s : Pointer);
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  s,d0
       MOVE.L  _ExecBase,A6
       JSR -156(A6)
       MOVE.L  (A7)+,A6
   end;
end;

procedure Vacate(sigsem : pSignalSemaphore;
                 bidMsg : pSemaphoreMessage);
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  sigsem,a0
       MOVE.L  bidMsg,a1
       MOVE.L  _ExecBase,A6
       JSR -546(A6)
       MOVE.L  (A7)+,A6
   end;
end;

function Wait(signals : ULONG) : ULONG;
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  signals,d0
       MOVE.L  _ExecBase,A6
       JSR -318(A6)
       MOVE.L  (A7)+,A6
       MOVE.L  d0,@RESULT
   end;
end;

function WaitIO(io : pIORequest) : Shortint;
begin
   asm
       MOVE.L  A6,-(A7)
       MOVE.L  io,a1
       MOVE.L  _ExecBase,A6
       JSR -474(A6)
       MOVE.L  (A7)+,A6
       MOVE.B  d0,@RESULT
   end;
end;

function WaitPort(port : pMsgPort): pMessage;
begin
  asm
      MOVE.L  A6,-(A7)
      MOVE.L  port,a0
      MOVE.L  _ExecBase,A6
      JSR -384(A6)
      MOVE.L  (A7)+,A6
      MOVE.L  d0,@RESULT
  end;
end;


end.



{
  $Log$
  Revision 1.2  1998-07-09 17:39:40  carl
    * several bugfixes by Nils Sjoholm

  Revision 1.1.1.1  1998/03/25 11:18:47  root
  * Restored version

  Revision 1.3  1998/01/26 12:02:42  michael
  + Added log at the end


  
  Working file: rtl/amiga/exec.pp
  description:
  ----------------------------
  revision 1.2
  date: 1997/12/14 19:02:47;  author: carl;  state: Exp;  lines: +11 -10
  * small bugfixes
  ----------------------------
  revision 1.1
  date: 1997/12/10 13:48:45;  author: carl;  state: Exp;
  + exec dynamic library definitions and calls.
  =============================================================================
}
