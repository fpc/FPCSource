{
    This file is part of the Free Pascal run time library.

    A file in Amiga system run time library.
    Copyright (c) 1999-2000 by Nils Sjoholm
    member of the Amiga RTL development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


UNIT EXEC;

INTERFACE

{$I exec.inc}


PROCEDURE AbortIO(ioRequest : pIORequest);
PROCEDURE AddDevice(device : pDevice);
PROCEDURE AddHead(list : pList; node : pNode);
PROCEDURE AddIntServer(intNumber : LONGINT; interrupt : pInterrupt);
PROCEDURE AddLibrary(lib : pLibrary);
PROCEDURE AddMemHandler(memhand : pInterrupt);
PROCEDURE AddMemList(size : ULONG; attributes : ULONG; pri : LONGINT; base : POINTER; name : pCHAR);
PROCEDURE AddPort(port : pMsgPort);
PROCEDURE AddResource(resource : POINTER);
PROCEDURE AddSemaphore(sigSem : pSignalSemaphore);
PROCEDURE AddTail(list : pList; node : pNode);
FUNCTION AddTask(task : pTask; initPC : POINTER; finalPC : POINTER) : POINTER;
PROCEDURE Alert(alertNum : ULONG);
FUNCTION AllocAbs(byteSize : ULONG; location : POINTER) : POINTER;
FUNCTION Allocate(freeList : pMemHeader; byteSize : ULONG) : POINTER;
FUNCTION AllocEntry(entry : pMemList) : pMemList;
FUNCTION AllocMem(byteSize : ULONG; requirements : ULONG) : POINTER;
FUNCTION AllocPooled(poolHeader : POINTER; memSize : ULONG) : POINTER;
FUNCTION AllocSignal(signalNum : LONGINT) : SHORTINT;
FUNCTION AllocTrap(trapNum : LONGINT) : LONGINT;
FUNCTION AllocVec(byteSize : ULONG; requirements : ULONG) : POINTER;
FUNCTION AttemptSemaphore(sigSem : pSignalSemaphore) : BOOLEAN;
FUNCTION AttemptSemaphoreShared(sigSem : pSignalSemaphore) : ULONG;
FUNCTION AvailMem(requirements : ULONG) : ULONG;
PROCEDURE CacheClearE(address : POINTER; length : ULONG; caches : ULONG);
PROCEDURE CacheClearU;
FUNCTION CacheControl(cacheBits : ULONG; cacheMask : ULONG) : ULONG;
PROCEDURE CachePostDMA(address : POINTER; VAR length : ULONG; flags : ULONG);
FUNCTION CachePreDMA(address : POINTER; VAR length : ULONG; flags : ULONG) : POINTER;
PROCEDURE Cause(interrupt : pInterrupt);
FUNCTION CheckIO(ioRequest : pIORequest) : pIORequest;
PROCEDURE ChildFree(tid : POINTER);
PROCEDURE ChildOrphan(tid : POINTER);
PROCEDURE ChildStatus(tid : POINTER);
PROCEDURE ChildWait(tid : POINTER);
PROCEDURE CloseDevice(ioRequest : pIORequest);
PROCEDURE CloseLibrary(lib : pLibrary);
PROCEDURE ColdReboot;
PROCEDURE CopyMem(source : POINTER; dest : POINTER; size : ULONG);
PROCEDURE CopyMemQuick(source : POINTER; dest : POINTER; size : ULONG);
FUNCTION CreateIORequest(port : pMsgPort; size : ULONG) : POINTER;
FUNCTION CreateMsgPort : pMsgPort;
FUNCTION CreatePool(requirements : ULONG; puddleSize : ULONG; threshSize : ULONG) : POINTER;
PROCEDURE Deallocate(freeList : pMemHeader; memoryBlock : POINTER; byteSize : ULONG);
PROCEDURE Debug(flags : ULONG);
PROCEDURE DeleteIORequest(iorequest : POINTER);
PROCEDURE DeleteMsgPort(port : pMsgPort);
PROCEDURE DeletePool(poolHeader : POINTER);
PROCEDURE Disable;
FUNCTION DoIO(ioRequest : pIORequest) : SHORTINT;
PROCEDURE Enable;
PROCEDURE Enqueue(list : pList; node : pNode);
PROCEDURE ExecFreeMem(memoryBlock : POINTER; byteSize : ULONG);
PROCEDURE ExecInsert(list : pList; node : pNode; pred : pNode);
FUNCTION FindName(list : pList; name : pCHAR) : pNode;
FUNCTION FindPort(name : pCHAR) : pMsgPort;
FUNCTION FindResident(name : pCHAR) : pResident;
FUNCTION FindSemaphore(sigSem : pCHAR) : pSignalSemaphore;
FUNCTION FindTask(name : pCHAR) : pTask;
PROCEDURE Forbid;
PROCEDURE FreeEntry(entry : pMemList);
PROCEDURE FreePooled(poolHeader : POINTER; memory : POINTER; memSize : ULONG);
PROCEDURE FreeSignal(signalNum : LONGINT);
PROCEDURE FreeTrap(trapNum : LONGINT);
PROCEDURE FreeVec(memoryBlock : POINTER);
FUNCTION GetCC : ULONG;
FUNCTION GetMsg(port : pMsgPort) : pMessage;
PROCEDURE InitCode(startClass : ULONG; version : ULONG);
FUNCTION InitResident(resident : pResident; segList : ULONG) : POINTER;
PROCEDURE InitSemaphore(sigSem : pSignalSemaphore);
PROCEDURE InitStruct(initTable : POINTER; memory : POINTER; size : ULONG);
PROCEDURE MakeFunctions(target : POINTER; functionArray : POINTER; funcDispBase : ULONG);
FUNCTION MakeLibrary(funcInit : POINTER; structInit : POINTER; libInit : tPROCEDURE; dataSize : ULONG; segList : ULONG) : pLibrary;
FUNCTION ObtainQuickVector(interruptCode : POINTER) : ULONG;
PROCEDURE ObtainSemaphore(sigSem : pSignalSemaphore);
PROCEDURE ObtainSemaphoreList(sigSem : pList);
PROCEDURE ObtainSemaphoreShared(sigSem : pSignalSemaphore);
FUNCTION OldOpenLibrary(libName : pCHAR) : pLibrary;
FUNCTION OpenDevice(devName : pCHAR; unite : ULONG; ioRequest : pIORequest; flags : ULONG) : SHORTINT;
FUNCTION OpenLibrary(libName : pCHAR; version : ULONG) : pLibrary;
FUNCTION OpenResource(resName : pCHAR) : POINTER;
PROCEDURE Permit;
FUNCTION Procure(sigSem : pSignalSemaphore; bidMsg : pSemaphoreMessage) : BOOLEAN;
PROCEDURE PutMsg(port : pMsgPort; message : pMessage);
PROCEDURE RawDoFmt(formatString : pCHAR; dataStream : POINTER; putChProc : tPROCEDURE; putChData : POINTER);
PROCEDURE ReleaseSemaphore(sigSem : pSignalSemaphore);
PROCEDURE ReleaseSemaphoreList(sigSem : pList);
PROCEDURE RemDevice(device : pDevice);
FUNCTION RemHead(list : pList) : pNode;
PROCEDURE RemIntServer(intNumber : LONGINT; interrupt : pInterrupt);
PROCEDURE RemLibrary(lib : pLibrary);
PROCEDURE RemMemHandler(memhand : pInterrupt);
PROCEDURE Remove(node : pNode);
PROCEDURE RemPort(port : pMsgPort);
PROCEDURE RemResource(resource : POINTER);
PROCEDURE RemSemaphore(sigSem : pSignalSemaphore);
FUNCTION RemTail(list : pList) : pNode;
PROCEDURE RemTask(task : pTask);
PROCEDURE ReplyMsg(message : pMessage);
PROCEDURE SendIO(ioRequest : pIORequest);
FUNCTION SetExcept(newSignals : ULONG; signalSet : ULONG) : ULONG;
FUNCTION SetFunction(lib : pLibrary; funcOffset : LONGINT; newFunction : tPROCEDURE) : POINTER;
FUNCTION SetIntVector(intNumber : LONGINT; interrupt : pInterrupt) : pInterrupt;
FUNCTION SetSignal(newSignals : ULONG; signalSet : ULONG) : ULONG;
FUNCTION SetSR(newSR : ULONG; mask : ULONG) : ULONG;
FUNCTION SetTaskPri(task : pTask; priority : LONGINT) : SHORTINT;
PROCEDURE Signal(task : pTask; signalSet : ULONG);
PROCEDURE StackSwap(newStack : pStackSwapStruct);
PROCEDURE SumKickData;
PROCEDURE SumLibrary(lib : pLibrary);
FUNCTION SuperState : POINTER;
FUNCTION Supervisor(userFunction : tPROCEDURE) : ULONG;
FUNCTION TypeOfMem(address : POINTER) : ULONG;
PROCEDURE UserState(sysStack : POINTER);
PROCEDURE Vacate(sigSem : pSignalSemaphore; bidMsg : pSemaphoreMessage);
FUNCTION Wait(signalSet : ULONG) : ULONG;
FUNCTION WaitIO(ioRequest : pIORequest) : SHORTINT;
FUNCTION WaitPort(port : pMsgPort) : pMessage;

IMPLEMENTATION

PROCEDURE AbortIO(ioRequest : pIORequest);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L ioRequest,A1
    MOVEA.L _ExecBase,A6
    JSR -480(A6)
    MOVEA.L (A7)+,A6
  END;
END;

PROCEDURE AddDevice(device : pDevice);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L device,A1
    MOVEA.L _ExecBase,A6
    JSR -432(A6)
    MOVEA.L (A7)+,A6
  END;
END;

PROCEDURE AddHead(list : pList; node : pNode);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L list,A0
    MOVEA.L node,A1
    MOVEA.L _ExecBase,A6
    JSR -240(A6)
    MOVEA.L (A7)+,A6
  END;
END;

PROCEDURE AddIntServer(intNumber : LONGINT; interrupt : pInterrupt);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVE.L  intNumber,D0
    MOVEA.L interrupt,A1
    MOVEA.L _ExecBase,A6
    JSR -168(A6)
    MOVEA.L (A7)+,A6
  END;
END;

PROCEDURE AddLibrary(lib : pLibrary);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L lib,A1
    MOVEA.L _ExecBase,A6
    JSR -396(A6)
    MOVEA.L (A7)+,A6
  END;
END;

PROCEDURE AddMemHandler(memhand : pInterrupt);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L memhand,A1
    MOVEA.L _ExecBase,A6
    JSR -774(A6)
    MOVEA.L (A7)+,A6
  END;
END;

PROCEDURE AddMemList(size : ULONG; attributes : ULONG; pri : LONGINT; base : POINTER; name : pCHAR);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVE.L  size,D0
    MOVE.L  attributes,D1
    MOVE.L  pri,D2
    MOVEA.L base,A0
    MOVEA.L name,A1
    MOVEA.L _ExecBase,A6
    JSR -618(A6)
    MOVEA.L (A7)+,A6
  END;
END;

PROCEDURE AddPort(port : pMsgPort);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L port,A1
    MOVEA.L _ExecBase,A6
    JSR -354(A6)
    MOVEA.L (A7)+,A6
  END;
END;

PROCEDURE AddResource(resource : POINTER);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L resource,A1
    MOVEA.L _ExecBase,A6
    JSR -486(A6)
    MOVEA.L (A7)+,A6
  END;
END;

PROCEDURE AddSemaphore(sigSem : pSignalSemaphore);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L sigSem,A1
    MOVEA.L _ExecBase,A6
    JSR -600(A6)
    MOVEA.L (A7)+,A6
  END;
END;

PROCEDURE AddTail(list : pList; node : pNode);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L list,A0
    MOVEA.L node,A1
    MOVEA.L _ExecBase,A6
    JSR -246(A6)
    MOVEA.L (A7)+,A6
  END;
END;

FUNCTION AddTask(task : pTask; initPC : POINTER; finalPC : POINTER) : POINTER;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L task,A1
    MOVEA.L initPC,A2
    MOVEA.L finalPC,A3
    MOVEA.L _ExecBase,A6
    JSR -282(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

PROCEDURE Alert(alertNum : ULONG);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVE.L  alertNum,D7
    MOVEA.L _ExecBase,A6
    JSR -108(A6)
    MOVEA.L (A7)+,A6
  END;
END;

FUNCTION AllocAbs(byteSize : ULONG; location : POINTER) : POINTER;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVE.L  byteSize,D0
    MOVEA.L location,A1
    MOVEA.L _ExecBase,A6
    JSR -204(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION Allocate(freeList : pMemHeader; byteSize : ULONG) : POINTER;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L freeList,A0
    MOVE.L  byteSize,D0
    MOVEA.L _ExecBase,A6
    JSR -186(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION AllocEntry(entry : pMemList) : pMemList;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L entry,A0
    MOVEA.L _ExecBase,A6
    JSR -222(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION AllocMem(byteSize : ULONG; requirements : ULONG) : POINTER;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVE.L  byteSize,D0
    MOVE.L  requirements,D1
    MOVEA.L _ExecBase,A6
    JSR -198(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION AllocPooled(poolHeader : POINTER; memSize : ULONG) : POINTER;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L poolHeader,A0
    MOVE.L  memSize,D0
    MOVEA.L _ExecBase,A6
    JSR -708(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION AllocSignal(signalNum : LONGINT) : SHORTINT;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVE.L  signalNum,D0
    MOVEA.L _ExecBase,A6
    JSR -330(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION AllocTrap(trapNum : LONGINT) : LONGINT;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVE.L  trapNum,D0
    MOVEA.L _ExecBase,A6
    JSR -342(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION AllocVec(byteSize : ULONG; requirements : ULONG) : POINTER;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVE.L  byteSize,D0
    MOVE.L  requirements,D1
    MOVEA.L _ExecBase,A6
    JSR -684(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION AttemptSemaphore(sigSem : pSignalSemaphore) : BOOLEAN;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L sigSem,A0
    MOVEA.L _ExecBase,A6
    JSR -576(A6)
    MOVEA.L (A7)+,A6
    TST.L   D0
    BEQ.B   @end
    MOVEQ   #1,D0
    @end: MOVE.B  D0,@RESULT
  END;
END;

FUNCTION AttemptSemaphoreShared(sigSem : pSignalSemaphore) : ULONG;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L sigSem,A0
    MOVEA.L _ExecBase,A6
    JSR -720(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION AvailMem(requirements : ULONG) : ULONG;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVE.L  requirements,D1
    MOVEA.L _ExecBase,A6
    JSR -216(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

PROCEDURE CacheClearE(address : POINTER; length : ULONG; caches : ULONG);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L address,A0
    MOVE.L  length,D0
    MOVE.L  caches,D1
    MOVEA.L _ExecBase,A6
    JSR -642(A6)
    MOVEA.L (A7)+,A6
  END;
END;

PROCEDURE CacheClearU;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L _ExecBase,A6
    JSR -636(A6)
    MOVEA.L (A7)+,A6
  END;
END;

FUNCTION CacheControl(cacheBits : ULONG; cacheMask : ULONG) : ULONG;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVE.L  cacheBits,D0
    MOVE.L  cacheMask,D1
    MOVEA.L _ExecBase,A6
    JSR -648(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

PROCEDURE CachePostDMA(address : POINTER; VAR length : ULONG; flags : ULONG);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L address,A0
    MOVEA.L length,A1
    MOVE.L  flags,D0
    MOVEA.L _ExecBase,A6
    JSR -768(A6)
    MOVEA.L (A7)+,A6
  END;
END;

FUNCTION CachePreDMA(address : POINTER; VAR length : ULONG; flags : ULONG) : POINTER;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L address,A0
    MOVEA.L length,A1
    MOVE.L  flags,D0
    MOVEA.L _ExecBase,A6
    JSR -762(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

PROCEDURE Cause(interrupt : pInterrupt);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L interrupt,A1
    MOVEA.L _ExecBase,A6
    JSR -180(A6)
    MOVEA.L (A7)+,A6
  END;
END;

FUNCTION CheckIO(ioRequest : pIORequest) : pIORequest;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L ioRequest,A1
    MOVEA.L _ExecBase,A6
    JSR -468(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

PROCEDURE ChildFree(tid : POINTER);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVE.L  tid,D0
    MOVEA.L _ExecBase,A6
    JSR -738(A6)
    MOVEA.L (A7)+,A6
  END;
END;

PROCEDURE ChildOrphan(tid : POINTER);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVE.L  tid,D0
    MOVEA.L _ExecBase,A6
    JSR -744(A6)
    MOVEA.L (A7)+,A6
  END;
END;

PROCEDURE ChildStatus(tid : POINTER);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVE.L  tid,D0
    MOVEA.L _ExecBase,A6
    JSR -750(A6)
    MOVEA.L (A7)+,A6
  END;
END;

PROCEDURE ChildWait(tid : POINTER);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVE.L  tid,D0
    MOVEA.L _ExecBase,A6
    JSR -756(A6)
    MOVEA.L (A7)+,A6
  END;
END;

PROCEDURE CloseDevice(ioRequest : pIORequest);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L ioRequest,A1
    MOVEA.L _ExecBase,A6
    JSR -450(A6)
    MOVEA.L (A7)+,A6
  END;
END;

PROCEDURE CloseLibrary(lib : pLibrary);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L lib,A1
    MOVEA.L _ExecBase,A6
    JSR -414(A6)
    MOVEA.L (A7)+,A6
  END;
END;

PROCEDURE ColdReboot;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L _ExecBase,A6
    JSR -726(A6)
    MOVEA.L (A7)+,A6
  END;
END;

PROCEDURE CopyMem(source : POINTER; dest : POINTER; size : ULONG);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L source,A0
    MOVEA.L dest,A1
    MOVE.L  size,D0
    MOVEA.L _ExecBase,A6
    JSR -624(A6)
    MOVEA.L (A7)+,A6
  END;
END;

PROCEDURE CopyMemQuick(source : POINTER; dest : POINTER; size : ULONG);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L source,A0
    MOVEA.L dest,A1
    MOVE.L  size,D0
    MOVEA.L _ExecBase,A6
    JSR -630(A6)
    MOVEA.L (A7)+,A6
  END;
END;

FUNCTION CreateIORequest(port : pMsgPort; size : ULONG) : POINTER;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L port,A0
    MOVE.L  size,D0
    MOVEA.L _ExecBase,A6
    JSR -654(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION CreateMsgPort : pMsgPort;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L _ExecBase,A6
    JSR -666(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION CreatePool(requirements : ULONG; puddleSize : ULONG; threshSize : ULONG) : POINTER;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVE.L  requirements,D0
    MOVE.L  puddleSize,D1
    MOVE.L  threshSize,D2
    MOVEA.L _ExecBase,A6
    JSR -696(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

PROCEDURE Deallocate(freeList : pMemHeader; memoryBlock : POINTER; byteSize : ULONG);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L freeList,A0
    MOVEA.L memoryBlock,A1
    MOVE.L  byteSize,D0
    MOVEA.L _ExecBase,A6
    JSR -192(A6)
    MOVEA.L (A7)+,A6
  END;
END;

PROCEDURE Debug(flags : ULONG);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVE.L  flags,D0
    MOVEA.L _ExecBase,A6
    JSR -114(A6)
    MOVEA.L (A7)+,A6
  END;
END;

PROCEDURE DeleteIORequest(iorequest : POINTER);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L iorequest,A0
    MOVEA.L _ExecBase,A6
    JSR -660(A6)
    MOVEA.L (A7)+,A6
  END;
END;

PROCEDURE DeleteMsgPort(port : pMsgPort);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L port,A0
    MOVEA.L _ExecBase,A6
    JSR -672(A6)
    MOVEA.L (A7)+,A6
  END;
END;

PROCEDURE DeletePool(poolHeader : POINTER);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L poolHeader,A0
    MOVEA.L _ExecBase,A6
    JSR -702(A6)
    MOVEA.L (A7)+,A6
  END;
END;

PROCEDURE Disable;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L _ExecBase,A6
    JSR -120(A6)
    MOVEA.L (A7)+,A6
  END;
END;

FUNCTION DoIO(ioRequest : pIORequest) : SHORTINT;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L ioRequest,A1
    MOVEA.L _ExecBase,A6
    JSR -456(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

PROCEDURE Enable;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L _ExecBase,A6
    JSR -126(A6)
    MOVEA.L (A7)+,A6
  END;
END;

PROCEDURE Enqueue(list : pList; node : pNode);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L list,A0
    MOVEA.L node,A1
    MOVEA.L _ExecBase,A6
    JSR -270(A6)
    MOVEA.L (A7)+,A6
  END;
END;

PROCEDURE ExecFreeMem(memoryBlock : POINTER; byteSize : ULONG);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L memoryBlock,A1
    MOVE.L  byteSize,D0
    MOVEA.L _ExecBase,A6
    JSR -210(A6)
    MOVEA.L (A7)+,A6
  END;
END;

PROCEDURE ExecInsert(list : pList; node : pNode; pred : pNode);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L list,A0
    MOVEA.L node,A1
    MOVEA.L pred,A2
    MOVEA.L _ExecBase,A6
    JSR -234(A6)
    MOVEA.L (A7)+,A6
  END;
END;

FUNCTION FindName(list : pList; name : pCHAR) : pNode;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L list,A0
    MOVEA.L name,A1
    MOVEA.L _ExecBase,A6
    JSR -276(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION FindPort(name : pCHAR) : pMsgPort;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L name,A1
    MOVEA.L _ExecBase,A6
    JSR -390(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION FindResident(name : pCHAR) : pResident;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L name,A1
    MOVEA.L _ExecBase,A6
    JSR -096(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION FindSemaphore(sigSem : pCHAR) : pSignalSemaphore;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L sigSem,A1
    MOVEA.L _ExecBase,A6
    JSR -594(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION FindTask(name : pCHAR) : pTask;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L name,A1
    MOVEA.L _ExecBase,A6
    JSR -294(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

PROCEDURE Forbid;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L _ExecBase,A6
    JSR -132(A6)
    MOVEA.L (A7)+,A6
  END;
END;

PROCEDURE FreeEntry(entry : pMemList);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L entry,A0
    MOVEA.L _ExecBase,A6
    JSR -228(A6)
    MOVEA.L (A7)+,A6
  END;
END;

PROCEDURE FreePooled(poolHeader : POINTER; memory : POINTER; memSize : ULONG);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L poolHeader,A0
    MOVEA.L memory,A1
    MOVE.L  memSize,D0
    MOVEA.L _ExecBase,A6
    JSR -714(A6)
    MOVEA.L (A7)+,A6
  END;
END;

PROCEDURE FreeSignal(signalNum : LONGINT);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVE.L  signalNum,D0
    MOVEA.L _ExecBase,A6
    JSR -336(A6)
    MOVEA.L (A7)+,A6
  END;
END;

PROCEDURE FreeTrap(trapNum : LONGINT);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVE.L  trapNum,D0
    MOVEA.L _ExecBase,A6
    JSR -348(A6)
    MOVEA.L (A7)+,A6
  END;
END;

PROCEDURE FreeVec(memoryBlock : POINTER);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L memoryBlock,A1
    MOVEA.L _ExecBase,A6
    JSR -690(A6)
    MOVEA.L (A7)+,A6
  END;
END;

FUNCTION GetCC : ULONG;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L _ExecBase,A6
    JSR -528(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION GetMsg(port : pMsgPort) : pMessage;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L port,A0
    MOVEA.L _ExecBase,A6
    JSR -372(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

PROCEDURE InitCode(startClass : ULONG; version : ULONG);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVE.L  startClass,D0
    MOVE.L  version,D1
    MOVEA.L _ExecBase,A6
    JSR -072(A6)
    MOVEA.L (A7)+,A6
  END;
END;

FUNCTION InitResident(resident : pResident; segList : ULONG) : POINTER;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L resident,A1
    MOVE.L  segList,D1
    MOVEA.L _ExecBase,A6
    JSR -102(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

PROCEDURE InitSemaphore(sigSem : pSignalSemaphore);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L sigSem,A0
    MOVEA.L _ExecBase,A6
    JSR -558(A6)
    MOVEA.L (A7)+,A6
  END;
END;

PROCEDURE InitStruct(initTable : POINTER; memory : POINTER; size : ULONG);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L initTable,A1
    MOVEA.L memory,A2
    MOVE.L  size,D0
    MOVEA.L _ExecBase,A6
    JSR -078(A6)
    MOVEA.L (A7)+,A6
  END;
END;

PROCEDURE MakeFunctions(target : POINTER; functionArray : POINTER; funcDispBase : ULONG);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L target,A0
    MOVEA.L functionArray,A1
    MOVEA.L funcDispBase,A2
    MOVEA.L _ExecBase,A6
    JSR -090(A6)
    MOVEA.L (A7)+,A6
  END;
END;

FUNCTION MakeLibrary(funcInit : POINTER; structInit : POINTER; libInit : tPROCEDURE; dataSize : ULONG; segList : ULONG) : pLibrary;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L funcInit,A0
    MOVEA.L structInit,A1
    MOVEA.L libInit,A2
    MOVE.L  dataSize,D0
    MOVE.L  segList,D1
    MOVEA.L _ExecBase,A6
    JSR -084(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION ObtainQuickVector(interruptCode : POINTER) : ULONG;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L interruptCode,A0
    MOVEA.L _ExecBase,A6
    JSR -786(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

PROCEDURE ObtainSemaphore(sigSem : pSignalSemaphore);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L sigSem,A0
    MOVEA.L _ExecBase,A6
    JSR -564(A6)
    MOVEA.L (A7)+,A6
  END;
END;

PROCEDURE ObtainSemaphoreList(sigSem : pList);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L sigSem,A0
    MOVEA.L _ExecBase,A6
    JSR -582(A6)
    MOVEA.L (A7)+,A6
  END;
END;

PROCEDURE ObtainSemaphoreShared(sigSem : pSignalSemaphore);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L sigSem,A0
    MOVEA.L _ExecBase,A6
    JSR -678(A6)
    MOVEA.L (A7)+,A6
  END;
END;

FUNCTION OldOpenLibrary(libName : pCHAR) : pLibrary;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L libName,A1
    MOVEA.L _ExecBase,A6
    JSR -408(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION OpenDevice(devName : pCHAR; unite : ULONG; ioRequest : pIORequest; flags : ULONG) : SHORTINT;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L devName,A0
    MOVE.L  unite,D0
    MOVEA.L ioRequest,A1
    MOVE.L  flags,D1
    MOVEA.L _ExecBase,A6
    JSR -444(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION OpenLibrary(libName : pCHAR; version : ULONG) : pLibrary;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L libName,A1
    MOVE.L  version,D0
    MOVEA.L _ExecBase,A6
    JSR -552(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION OpenResource(resName : pCHAR) : POINTER;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L resName,A1
    MOVEA.L _ExecBase,A6
    JSR -498(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

PROCEDURE Permit;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L _ExecBase,A6
    JSR -138(A6)
    MOVEA.L (A7)+,A6
  END;
END;

FUNCTION Procure(sigSem : pSignalSemaphore; bidMsg : pSemaphoreMessage) : BOOLEAN;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L sigSem,A0
    MOVEA.L bidMsg,A1
    MOVEA.L _ExecBase,A6
    JSR -540(A6)
    MOVEA.L (A7)+,A6
    TST.L   D0
    BEQ.B   @end
    MOVEQ   #1,D0
    @end: MOVE.B  D0,@RESULT
  END;
END;

PROCEDURE PutMsg(port : pMsgPort; message : pMessage);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L port,A0
    MOVEA.L message,A1
    MOVEA.L _ExecBase,A6
    JSR -366(A6)
    MOVEA.L (A7)+,A6
  END;
END;

PROCEDURE RawDoFmt(formatString : pCHAR; dataStream : POINTER; putChProc : tPROCEDURE; putChData : POINTER);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L formatString,A0
    MOVEA.L dataStream,A1
    MOVEA.L putChProc,A2
    MOVEA.L putChData,A3
    MOVEA.L _ExecBase,A6
    JSR -522(A6)
    MOVEA.L (A7)+,A6
  END;
END;

PROCEDURE ReleaseSemaphore(sigSem : pSignalSemaphore);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L sigSem,A0
    MOVEA.L _ExecBase,A6
    JSR -570(A6)
    MOVEA.L (A7)+,A6
  END;
END;

PROCEDURE ReleaseSemaphoreList(sigSem : pList);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L sigSem,A0
    MOVEA.L _ExecBase,A6
    JSR -588(A6)
    MOVEA.L (A7)+,A6
  END;
END;

PROCEDURE RemDevice(device : pDevice);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L device,A1
    MOVEA.L _ExecBase,A6
    JSR -438(A6)
    MOVEA.L (A7)+,A6
  END;
END;

FUNCTION RemHead(list : pList) : pNode;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L list,A0
    MOVEA.L _ExecBase,A6
    JSR -258(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

PROCEDURE RemIntServer(intNumber : LONGINT; interrupt : pInterrupt);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVE.L  intNumber,D0
    MOVEA.L interrupt,A1
    MOVEA.L _ExecBase,A6
    JSR -174(A6)
    MOVEA.L (A7)+,A6
  END;
END;

PROCEDURE RemLibrary(lib : pLibrary);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L lib,A1
    MOVEA.L _ExecBase,A6
    JSR -402(A6)
    MOVEA.L (A7)+,A6
  END;
END;

PROCEDURE RemMemHandler(memhand : pInterrupt);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L memhand,A1
    MOVEA.L _ExecBase,A6
    JSR -780(A6)
    MOVEA.L (A7)+,A6
  END;
END;

PROCEDURE Remove(node : pNode);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L node,A1
    MOVEA.L _ExecBase,A6
    JSR -252(A6)
    MOVEA.L (A7)+,A6
  END;
END;

PROCEDURE RemPort(port : pMsgPort);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L port,A1
    MOVEA.L _ExecBase,A6
    JSR -360(A6)
    MOVEA.L (A7)+,A6
  END;
END;

PROCEDURE RemResource(resource : POINTER);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L resource,A1
    MOVEA.L _ExecBase,A6
    JSR -492(A6)
    MOVEA.L (A7)+,A6
  END;
END;

PROCEDURE RemSemaphore(sigSem : pSignalSemaphore);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L sigSem,A1
    MOVEA.L _ExecBase,A6
    JSR -606(A6)
    MOVEA.L (A7)+,A6
  END;
END;

FUNCTION RemTail(list : pList) : pNode;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L list,A0
    MOVEA.L _ExecBase,A6
    JSR -264(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

PROCEDURE RemTask(task : pTask);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L task,A1
    MOVEA.L _ExecBase,A6
    JSR -288(A6)
    MOVEA.L (A7)+,A6
  END;
END;

PROCEDURE ReplyMsg(message : pMessage);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L message,A1
    MOVEA.L _ExecBase,A6
    JSR -378(A6)
    MOVEA.L (A7)+,A6
  END;
END;

PROCEDURE SendIO(ioRequest : pIORequest);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L ioRequest,A1
    MOVEA.L _ExecBase,A6
    JSR -462(A6)
    MOVEA.L (A7)+,A6
  END;
END;

FUNCTION SetExcept(newSignals : ULONG; signalSet : ULONG) : ULONG;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVE.L  newSignals,D0
    MOVE.L  signalSet,D1
    MOVEA.L _ExecBase,A6
    JSR -312(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION SetFunction(lib : pLibrary; funcOffset : LONGINT; newFunction : tPROCEDURE) : POINTER;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L lib,A1
    MOVEA.L funcOffset,A0
    MOVE.L  newFunction,D0
    MOVEA.L _ExecBase,A6
    JSR -420(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION SetIntVector(intNumber : LONGINT; interrupt : pInterrupt) : pInterrupt;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVE.L  intNumber,D0
    MOVEA.L interrupt,A1
    MOVEA.L _ExecBase,A6
    JSR -162(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION SetSignal(newSignals : ULONG; signalSet : ULONG) : ULONG;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVE.L  newSignals,D0
    MOVE.L  signalSet,D1
    MOVEA.L _ExecBase,A6
    JSR -306(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION SetSR(newSR : ULONG; mask : ULONG) : ULONG;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVE.L  newSR,D0
    MOVE.L  mask,D1
    MOVEA.L _ExecBase,A6
    JSR -144(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION SetTaskPri(task : pTask; priority : LONGINT) : SHORTINT;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L task,A1
    MOVE.L  priority,D0
    MOVEA.L _ExecBase,A6
    JSR -300(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

PROCEDURE Signal(task : pTask; signalSet : ULONG);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L task,A1
    MOVE.L  signalSet,D0
    MOVEA.L _ExecBase,A6
    JSR -324(A6)
    MOVEA.L (A7)+,A6
  END;
END;

PROCEDURE StackSwap(newStack : pStackSwapStruct);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L newStack,A0
    MOVEA.L _ExecBase,A6
    JSR -732(A6)
    MOVEA.L (A7)+,A6
  END;
END;

PROCEDURE SumKickData;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L _ExecBase,A6
    JSR -612(A6)
    MOVEA.L (A7)+,A6
  END;
END;

PROCEDURE SumLibrary(lib : pLibrary);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L lib,A1
    MOVEA.L _ExecBase,A6
    JSR -426(A6)
    MOVEA.L (A7)+,A6
  END;
END;

FUNCTION SuperState : POINTER;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L _ExecBase,A6
    JSR -150(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION Supervisor(userFunction : tPROCEDURE) : ULONG;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L userFunction,A5
    MOVEA.L _ExecBase,A6
    JSR -030(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION TypeOfMem(address : POINTER) : ULONG;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L address,A1
    MOVEA.L _ExecBase,A6
    JSR -534(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

PROCEDURE UserState(sysStack : POINTER);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVE.L  sysStack,D0
    MOVEA.L _ExecBase,A6
    JSR -156(A6)
    MOVEA.L (A7)+,A6
  END;
END;

PROCEDURE Vacate(sigSem : pSignalSemaphore; bidMsg : pSemaphoreMessage);
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L sigSem,A0
    MOVEA.L bidMsg,A1
    MOVEA.L _ExecBase,A6
    JSR -546(A6)
    MOVEA.L (A7)+,A6
  END;
END;

FUNCTION Wait(signalSet : ULONG) : ULONG;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVE.L  signalSet,D0
    MOVEA.L _ExecBase,A6
    JSR -318(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION WaitIO(ioRequest : pIORequest) : SHORTINT;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L ioRequest,A1
    MOVEA.L _ExecBase,A6
    JSR -474(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

FUNCTION WaitPort(port : pMsgPort) : pMessage;
BEGIN
  ASM
    MOVE.L  A6,-(A7)
    MOVEA.L port,A0
    MOVEA.L _ExecBase,A6
    JSR -384(A6)
    MOVEA.L (A7)+,A6
    MOVE.L  D0,@RESULT
  END;
END;

END. (* UNIT EXEC *)

{
  $Log$
  Revision 1.3  2002-09-07 16:01:16  peter
    * old logs removed and tabs fixed

}
