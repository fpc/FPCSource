{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by the Free Pascal development team

    Scalable Asynchronous I/O Event Loop - Linux io_uring Backend

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.
}
{$mode objfpc}{$H+}
{$modeswitch nestedprocvars}

unit eventloop.backend.uring;

interface

{$if defined(LINUX) or defined(ANDROID)}

uses
  SysUtils, Classes, BaseUnix, Unix, Linux, Syscall, eventloop.types, eventloop.core;

const
  { io_uring opcodes }
  IORING_OP_NOP         = 0;
  IORING_OP_READV       = 1;
  IORING_OP_WRITEV      = 2;
  IORING_OP_FSYNC       = 3;
  IORING_OP_POLL_ADD    = 6;
  IORING_OP_POLL_REMOVE = 7;
  IORING_OP_TIMEOUT     = 11;
  IORING_OP_ACCEPT      = 13;
  IORING_OP_CONNECT     = 16;
  IORING_OP_READ        = 22;
  IORING_OP_WRITE       = 23;

  { Magic offsets for mmap }
  IORING_OFF_SQ_RING    = 0;
  IORING_OFF_CQ_RING    = $8000000;
  IORING_OFF_SQES       = $10000000;

  { io_uring_enter flags }
  IORING_ENTER_GETEVENTS = 1;

type
  { io_uring Submission Queue Entry (64 bytes) }
  Pio_uring_sqe = ^Tio_uring_sqe;
  Tio_uring_sqe = record
    opcode: cuint8;
    flags: cuint8;
    ioprio: cuint16;
    fd: cint32;
    off: cuint64;
    addr: cuint64;
    len: cuint32;
    rw_flags: cuint32;
    user_data: cuint64;
    buf_index: cuint16;
    personality: cuint16;
    splice_fd_in: cint32;
    addr3: cuint64;
    pad2: cuint64;
  end;

  { io_uring Completion Queue Entry (16 bytes) }
  Pio_uring_cqe = ^Tio_uring_cqe;
  Tio_uring_cqe = record
    user_data: cuint64;
    res: cint32;
    flags: cuint32;
  end;

  { Layout offsets inside mmapped ring buffers }
  Tio_sqring_offsets = record
    head: cuint32;
    tail: cuint32;
    ring_mask: cuint32;
    ring_entries: cuint32;
    flags: cuint32;
    dropped: cuint32;
    array_offset: cuint32;
    resv1: cuint32;
    user_addr: cuint64;
  end;

  Tio_cqring_offsets = record
    head: cuint32;
    tail: cuint32;
    ring_mask: cuint32;
    ring_entries: cuint32;
    overflow: cuint32;
    cqes: cuint32;
    flags: cuint32;
    resv1: cuint32;
    user_addr: cuint64;
  end;

  { io_uring setup parameters }
  Pio_uring_params = ^Tio_uring_params;
  Tio_uring_params = record
    sq_entries: cuint32;
    cq_entries: cuint32;
    flags: cuint32;
    sq_thread_cpu: cuint32;
    sq_thread_idle: cuint32;
    features: cuint32;
    wq_fd: cuint32;
    resv: array[0..2] of cuint32;
    sq_off: Tio_sqring_offsets;
    cq_off: Tio_cqring_offsets;
  end;

  { High-performance io_uring event loop engine }
  TIOUringEventLoop = class(TEventLoopBase)
  private
    FRingFd: cint;
    FParams: Tio_uring_params;

    { Mmapped SQ ring pointers }
    FSQRingPtr: Pointer;
    FSQRingSize: size_t;
    FSQHead: PCuint32;
    FSQTail: PCuint32;
    FSQRingMask: cuint32;
    FSQArray: PCuint32;
    FSQEs: Pio_uring_sqe;
    FSQEsSize: size_t;

    { Mmapped CQ ring pointers }
    FCQRingPtr: Pointer;
    FCQRingSize: size_t;
    FCQHead: PCuint32;
    FCQTail: PCuint32;
    FCQRingMask: cuint32;
    FCQEs: Pio_uring_cqe;

    FWakeupFd: cint;

    function EventsToPollMask(AEvents: TEventLoopEvents): cuint16;
    function PollMaskToEvents(AMask: cint32): TEventLoopEvents;
    procedure SubmitSQE(constref ASQE: Tio_uring_sqe);
    procedure FlushSubmissions(AWaitEvents: cuint32 = 0);
  protected
    procedure DoRegisterHandle(AHandle: TLoopHandle; AEvents: TEventLoopEvents; const ACallback: TAsyncIOCompletion; AUserData: Pointer); override;
    procedure DoModifyHandle(AHandle: TLoopHandle; AEvents: TEventLoopEvents); override;
    procedure DoUnregisterHandle(AHandle: TLoopHandle); override;
    procedure DoPoll(ATimeoutMs: Integer); override;
    procedure DoWakeup; override;
  public
    class function IsSupported: Boolean; static;
    constructor Create(AQueueDepth: Integer = 256);
    destructor Destroy; override;
    property RingFd: cint read FRingFd;
  end;

{$endif}

implementation

{$if defined(LINUX) or defined(ANDROID)}

const
  MAP_POPULATE_CONST = $8000;
  POLLIN_CONST  = $0001;
  POLLPRI_CONST = $0002;
  POLLOUT_CONST = $0004;
  POLLERR_CONST = $0008;
  POLLHUP_CONST = $0010;

{ TIOUringEventLoop }

class function TIOUringEventLoop.IsSupported: Boolean;
var
  LParams: Tio_uring_params;
  LFd: cint;
begin
  FillChar(LParams, SizeOf(LParams), 0);
  LFd := cint(Do_SysCall(syscall_nr_io_uring_setup, 16, TSysParam(@LParams)));
  if LFd >= 0 then
  begin
    fpclose(LFd);
    Result := True;
  end
  else
    Result := False;
end;

constructor TIOUringEventLoop.Create(AQueueDepth: Integer);
var
  LEfd: cint;
  LSqe: Tio_uring_sqe;
begin
  inherited Create(bkIOUring);
  FillChar(FParams, SizeOf(FParams), 0);
  if AQueueDepth < 16 then
    AQueueDepth := 16;

  FRingFd := cint(Do_SysCall(syscall_nr_io_uring_setup, AQueueDepth, TSysParam(@FParams)));
  if FRingFd < 0 then
    raise EEventLoopException.CreateFmt('io_uring_setup failed: %s (errno=%d)', [SysErrorMessage(fpgeterrno), fpgeterrno]);

  { 1. Map Submission Queue ring }
  FSQRingSize := FParams.sq_off.array_offset + FParams.sq_entries * SizeOf(cuint32);
  FSQRingPtr := fpmmap(nil, FSQRingSize, PROT_READ or PROT_WRITE, MAP_SHARED or MAP_POPULATE_CONST, FRingFd, IORING_OFF_SQ_RING);
  if FSQRingPtr = MAP_FAILED then
    raise EEventLoopException.Create('Failed to mmap io_uring SQ ring');

  FSQHead := PCuint32(PByte(FSQRingPtr) + FParams.sq_off.head);
  FSQTail := PCuint32(PByte(FSQRingPtr) + FParams.sq_off.tail);
  FSQRingMask := PCuint32(PByte(FSQRingPtr) + FParams.sq_off.ring_mask)^;
  FSQArray := PCuint32(PByte(FSQRingPtr) + FParams.sq_off.array_offset);

  { 2. Map Submission Queue Entries array }
  FSQEsSize := FParams.sq_entries * SizeOf(Tio_uring_sqe);
  FSQEs := Pio_uring_sqe(fpmmap(nil, FSQEsSize, PROT_READ or PROT_WRITE, MAP_SHARED or MAP_POPULATE_CONST, FRingFd, IORING_OFF_SQES));
  if Pointer(FSQEs) = MAP_FAILED then
    raise EEventLoopException.Create('Failed to mmap io_uring SQEs');

  { 3. Map Completion Queue ring }
  FCQRingSize := FParams.cq_off.cqes + FParams.cq_entries * SizeOf(Tio_uring_cqe);
  FCQRingPtr := fpmmap(nil, FCQRingSize, PROT_READ or PROT_WRITE, MAP_SHARED or MAP_POPULATE_CONST, FRingFd, IORING_OFF_CQ_RING);
  if FCQRingPtr = MAP_FAILED then
    raise EEventLoopException.Create('Failed to mmap io_uring CQ ring');

  FCQHead := PCuint32(PByte(FCQRingPtr) + FParams.cq_off.head);
  FCQTail := PCuint32(PByte(FCQRingPtr) + FParams.cq_off.tail);
  FCQRingMask := PCuint32(PByte(FCQRingPtr) + FParams.cq_off.ring_mask)^;
  FCQEs := Pio_uring_cqe(PByte(FCQRingPtr) + FParams.cq_off.cqes);

  { Setup wakeup eventfd }
  LEfd := cint(Do_SysCall(syscall_nr_eventfd2, 0, $80000 or $800));
  FWakeupFd := LEfd;
  if FWakeupFd >= 0 then
  begin
    FillChar(LSqe, SizeOf(LSqe), 0);
    LSqe.opcode := IORING_OP_POLL_ADD;
    LSqe.fd := FWakeupFd;
    LSqe.rw_flags := POLLIN_CONST;
    LSqe.user_data := QWord(FWakeupFd);
    SubmitSQE(LSqe);
    FlushSubmissions(0);
  end;
end;

destructor TIOUringEventLoop.Destroy;
begin
  if FWakeupFd >= 0 then
  begin
    fpclose(FWakeupFd);
    FWakeupFd := -1;
  end;
  if FSQRingPtr <> nil then
    fpmunmap(FSQRingPtr, FSQRingSize);
  if Pointer(FSQEs) <> nil then
    fpmunmap(Pointer(FSQEs), FSQEsSize);
  if FCQRingPtr <> nil then
    fpmunmap(FCQRingPtr, FCQRingSize);
  if FRingFd >= 0 then
  begin
    fpclose(FRingFd);
    FRingFd := -1;
  end;
  inherited Destroy;
end;

function TIOUringEventLoop.EventsToPollMask(AEvents: TEventLoopEvents): cuint16;
begin
  Result := 0;
  if eleRead in AEvents then
    Result := Result or POLLIN_CONST or POLLPRI_CONST;
  if eleWrite in AEvents then
    Result := Result or POLLOUT_CONST;
end;

function TIOUringEventLoop.PollMaskToEvents(AMask: cint32): TEventLoopEvents;
begin
  Result := [];
  if (AMask and (POLLIN_CONST or POLLPRI_CONST)) <> 0 then
    Include(Result, eleRead);
  if (AMask and POLLOUT_CONST) <> 0 then
    Include(Result, eleWrite);
  if (AMask and POLLERR_CONST) <> 0 then
    Include(Result, eleError);
  if (AMask and POLLHUP_CONST) <> 0 then
    Include(Result, eleHangup);
end;

procedure TIOUringEventLoop.SubmitSQE(constref ASQE: Tio_uring_sqe);
var
  LTail, LNextTail, LIndex: cuint32;
begin
  LTail := FSQTail^;
  LNextTail := LTail + 1;
  LIndex := LTail and FSQRingMask;
  FSQEs[LIndex] := ASQE;
  (FSQArray + LIndex)^ := LIndex;
  FSQTail^ := LNextTail;
end;

procedure TIOUringEventLoop.FlushSubmissions(AWaitEvents: cuint32);
var
  LToSubmit: cuint32;
  LFlags: cuint32;
begin
  LToSubmit := FSQTail^ - FSQHead^;
  LFlags := 0;
  if AWaitEvents > 0 then
    LFlags := IORING_ENTER_GETEVENTS;
  Do_SysCall(syscall_nr_io_uring_enter, FRingFd, LToSubmit, AWaitEvents, LFlags, 0);
end;

procedure TIOUringEventLoop.DoRegisterHandle(AHandle: TLoopHandle; AEvents: TEventLoopEvents; const ACallback: TAsyncIOCompletion; AUserData: Pointer);
var
  LSqe: Tio_uring_sqe;
begin
  FillChar(LSqe, SizeOf(LSqe), 0);
  LSqe.opcode := IORING_OP_POLL_ADD;
  LSqe.fd := AHandle;
  LSqe.rw_flags := EventsToPollMask(AEvents);
  LSqe.user_data := QWord(AHandle);
  SubmitSQE(LSqe);
  FlushSubmissions(0);
end;

procedure TIOUringEventLoop.DoModifyHandle(AHandle: TLoopHandle; AEvents: TEventLoopEvents);
begin
  DoUnregisterHandle(AHandle);
  DoRegisterHandle(AHandle, AEvents, nil, nil);
end;

procedure TIOUringEventLoop.DoUnregisterHandle(AHandle: TLoopHandle);
var
  LSqe: Tio_uring_sqe;
begin
  FillChar(LSqe, SizeOf(LSqe), 0);
  LSqe.opcode := IORING_OP_POLL_REMOVE;
  LSqe.addr := QWord(AHandle);
  LSqe.user_data := 0;
  SubmitSQE(LSqe);
  FlushSubmissions(0);
end;

procedure TIOUringEventLoop.DoWakeup;
var
  LVal: QWord;
begin
  if FWakeupFd >= 0 then
  begin
    LVal := 1;
    fpwrite(FWakeupFd, LVal, SizeOf(LVal));
  end;
end;

procedure TIOUringEventLoop.DoPoll(ATimeoutMs: Integer);
var
  LHead, LTail, LIndex: cuint32;
  LCqe: Pio_uring_cqe;
  LFd: cint;
  LEvents: TEventLoopEvents;
  LVal: QWord;
  LSqe: Tio_uring_sqe;
begin
  { Flush pending submissions and wait if needed }
  FlushSubmissions(0);

  LHead := FCQHead^;
  LTail := FCQTail^;

  while LHead <> LTail do
  begin
    LIndex := LHead and FCQRingMask;
    LCqe := FCQEs + LIndex;

    LFd := cint(LCqe^.user_data);
    if LFd = FWakeupFd then
    begin
      fpread(FWakeupFd, LVal, SizeOf(LVal));
      { Re-arm wakeup poll }
      FillChar(LSqe, SizeOf(LSqe), 0);
      LSqe.opcode := IORING_OP_POLL_ADD;
      LSqe.fd := FWakeupFd;
      LSqe.rw_flags := POLLIN_CONST;
      LSqe.user_data := QWord(FWakeupFd);
      SubmitSQE(LSqe);
    end
    else if LFd > 0 then
    begin
      LEvents := PollMaskToEvents(LCqe^.res);
      TriggerHandle(LFd, LEvents);

      { Re-arm poll for persistent edge/level monitoring }
      FillChar(LSqe, SizeOf(LSqe), 0);
      LSqe.opcode := IORING_OP_POLL_ADD;
      LSqe.fd := LFd;
      LSqe.rw_flags := POLLIN_CONST or POLLOUT_CONST;
      LSqe.user_data := QWord(LFd);
      SubmitSQE(LSqe);
    end;

    Inc(LHead);
    FCQHead^ := LHead;
  end;
end;

{$endif}

end.
