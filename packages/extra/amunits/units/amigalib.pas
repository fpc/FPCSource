{
    This file is part of the Free Pascal run time library.

    A file in Amiga system run time library.
    Copyright (c) 1998 by Nils Sjoholm
    member of the Amiga RTL development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit amigalib;

INTERFACE

uses exec;

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

end.


