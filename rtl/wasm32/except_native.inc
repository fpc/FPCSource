{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{****************************************************************************
                                Exception support
****************************************************************************}

{$ifdef FPC_HAS_FEATURE_THREADING}
ThreadVar
{$else FPC_HAS_FEATURE_THREADING}
Var
{$endif FPC_HAS_FEATURE_THREADING}
  WASM_ExceptAddrStack   : PExceptAddr;
  WASM_ExceptObjectStack : PExceptObject;
  WASM_ExceptTryLevel    : ObjpasInt;

{ This routine is called only from fpc_raiseexception, which uses ExceptTryLevel
  flag to guard against repeated exceptions which can occur due to corrupted stack
  or heap. }
function WASM_PushExceptObject(Obj : TObject; AnAddr : CodePointer; AFrame : Pointer): PExceptObject;
var
  Newobj : PExceptObject;
  _ExceptObjectStack : ^PExceptObject;
  framebufsize,
  framecount  : PtrInt;
  frames      : PCodePointer;
  prev_frame,
  curr_frame  : Pointer;
  curr_addr   : CodePointer;
begin
{$ifdef excdebug}
  writeln ('In PushExceptObject');
{$endif}
  _ExceptObjectStack:=@WASM_ExceptObjectStack;
  NewObj:=AllocMem(sizeof(TExceptObject));
  NewObj^.Next:=_ExceptObjectStack^;
  _ExceptObjectStack^:=NewObj;

  NewObj^.FObject:=Obj;
  NewObj^.Addr:=AnAddr;
  if assigned(get_frame) then
    begin
      NewObj^.refcount:=0;

      { Backtrace }
      curr_frame:=AFrame;
      curr_addr:=AnAddr;
      frames:=nil;
      framecount:=0;
      framebufsize:=0;
      { The frame pointer of this procedure is used as initial stack bottom value. }
      prev_frame:=get_frame;
      while (framecount<RaiseMaxFrameCount) and (curr_frame > prev_frame) and
            (curr_frame<StackTop) do
        Begin
          prev_frame:=curr_frame;
          get_caller_stackinfo(curr_frame,curr_addr);
          if (curr_addr=nil) or
             (curr_frame=nil) then
            break;
          if (framecount>=framebufsize) then
            begin
              inc(framebufsize,16);
              reallocmem(frames,framebufsize*sizeof(codepointer));
            end;
          frames[framecount]:=curr_addr;
          inc(framecount);
        End;
      NewObj^.framecount:=framecount;
      NewObj^.frames:=frames;
    end;
  Result:=NewObj;
end;

{$define FPC_SYSTEM_HAS_RAISEEXCEPTION}
procedure fpc_RaiseException (Obj : TObject; AnAddr : CodePointer; AFrame : Pointer);[Public, Alias : 'FPC_RAISEEXCEPTION']; compilerproc;
var
  _ExceptObjectStack : PExceptObject;
  _ExceptAddrstack : PExceptAddr;
begin
{$ifdef excdebug}
  writeln ('In RaiseException');
{$endif}
  if WASM_ExceptTryLevel<>0 then
    Halt(217);
  WASM_ExceptTryLevel:=1;
  WASM_PushExceptObject(Obj,AnAddr,AFrame);
  { if PushExceptObject causes another exception, the following won't be executed,
    causing halt upon entering this routine recursively. }
  WASM_ExceptTryLevel:=0;
  //_ExceptAddrstack:=ExceptAddrStack;
  //If _ExceptAddrStack=Nil then
//    DoUnhandledException;
  _ExceptObjectStack:=WASM_ExceptObjectStack;
  if (RaiseProc <> nil) and (_ExceptObjectStack <> nil) then
    with _ExceptObjectStack^ do
      RaiseProc(FObject,Addr,FrameCount,Frames);
//  longjmp(_ExceptAddrStack^.Buf^,FPC_Exception);
  fpc_wasm32_throw_fpcexception;
end;
