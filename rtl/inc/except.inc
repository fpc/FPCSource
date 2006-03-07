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


Const
  { Type of exception. Currently only one. }
  FPC_EXCEPTION   = 1;

  { types of frames for the exception address stack }
  cExceptionFrame = 1;
  cFinalizeFrame  = 2;

Type
  PExceptAddr = ^TExceptAddr;
  TExceptAddr = record
    buf       : pjmp_buf;
    next      : PExceptAddr;
    frametype : Longint;
  end;

  TExceptObjectClass = Class of TObject;

Const
  CatchAllExceptions : PtrInt = -1;

ThreadVar
  ExceptAddrStack   : PExceptAddr;
  ExceptObjectStack : PExceptObject;

Function RaiseList : PExceptObject;
begin
  RaiseList:=ExceptObjectStack;
end;


function AcquireExceptionObject: Pointer;
begin
  If ExceptObjectStack<>nil then
    begin
      Inc(ExceptObjectStack^.refcount);
      AcquireExceptionObject := ExceptObjectStack^.FObject;
    end
  else
    RunError(231);
end;


procedure ReleaseExceptionObject;
begin
  If ExceptObjectStack <> nil then
    begin
      if ExceptObjectStack^.refcount > 0 then
        Dec(ExceptObjectStack^.refcount);
    end
  else
    RunError(231);
end;


Function fpc_PushExceptAddr (Ft: Longint;_buf,_newaddr : pointer): PJmp_buf ;
  [Public, Alias : 'FPC_PUSHEXCEPTADDR'];compilerproc;
begin
{$ifdef excdebug}
  writeln ('In PushExceptAddr');
{$endif}
  PExceptAddr(_newaddr)^.Next:=ExceptAddrstack;
  ExceptAddrStack:=PExceptAddr(_newaddr);
  PExceptAddr(_newaddr)^.Buf:=PJmp_Buf(_buf);
  PExceptAddr(_newaddr)^.FrameType:=ft;
  result:=PJmp_Buf(_buf);
end;


Procedure fpc_PushExceptObj (Obj : TObject; AnAddr,AFrame : Pointer);
  [Public, Alias : 'FPC_PUSHEXCEPTOBJECT']; compilerproc;
var
  Newobj : PExceptObject;
  framebufsize,
  framecount  : longint;
  frames      : PPointer;
  prev_frame,
  curr_frame,
  caller_frame,
  caller_addr : Pointer;
begin
{$ifdef excdebug}
  writeln ('In PushExceptObject');
{$endif}
  If ExceptObjectStack=Nil then
    begin
      New(ExceptObjectStack);
      ExceptObjectStack^.Next:=Nil;
    end
  else
    begin
      New(NewObj);
      NewObj^.Next:=ExceptObjectStack;
      ExceptObjectStack:=NewObj;
    end;
  ExceptObjectStack^.FObject:=Obj;
  ExceptObjectStack^.Addr:=AnAddr;
  ExceptObjectStack^.refcount:=0;
  { Backtrace }
  curr_frame:=AFrame;
  prev_frame:=get_frame;
  frames:=nil;
  framebufsize:=0;
  framecount:=0;
  while (framecount<RaiseMaxFrameCount) and (curr_frame > prev_frame) and
        (curr_frame<(StackBottom + StackLength)) do
   Begin
     caller_addr := get_caller_addr(curr_frame);
     caller_frame := get_caller_frame(curr_frame);
     if (caller_addr=nil) or
        (caller_frame=nil) then
       break;
     if (framecount>=framebufsize) then
       begin
         inc(framebufsize,16);
         reallocmem(frames,framebufsize*sizeof(pointer));
       end;
     frames[framecount]:=caller_addr;
     inc(framecount);
     prev_frame:=curr_frame;
     curr_frame:=caller_frame;
   End;
  ExceptObjectStack^.framecount:=framecount;
  ExceptObjectStack^.frames:=frames;
end;

{ make it avalable for local use }
Procedure fpc_PushExceptObj (Obj : TObject; AnAddr,AFrame : Pointer); [external name 'FPC_PUSHEXCEPTOBJECT'];


Procedure DoUnHandledException;
begin
  If (ExceptProc<>Nil) and (ExceptObjectStack<>Nil) then
    with ExceptObjectStack^ do
      TExceptProc(ExceptProc)(FObject,Addr,FrameCount,Frames);
  if erroraddr = nil then
    RunError(217)
  else
    if errorcode <= maxExitCode then
      halt(errorcode)
    else
      halt(255)
end;


Function fpc_Raiseexception (Obj : TObject; AnAddr,AFrame : Pointer) : TObject;[Public, Alias : 'FPC_RAISEEXCEPTION']; compilerproc;
begin
{$ifdef excdebug}
  writeln ('In RaiseException');
{$endif}
  fpc_Raiseexception:=nil;
  fpc_PushExceptObj(Obj,AnAddr,AFrame);
  If ExceptAddrStack=Nil then
    DoUnhandledException;
  if (RaiseProc <> nil) and (ExceptObjectStack <> nil) then
    with ExceptObjectStack^ do
      RaiseProc(FObject,Addr,FrameCount,Frames);
  longjmp(ExceptAddrStack^.Buf^,FPC_Exception);
end;


Procedure fpc_PopAddrStack;[Public, Alias : 'FPC_POPADDRSTACK']; compilerproc;
var
  hp : ^PExceptAddr;
begin
{$ifdef excdebug}
  writeln ('In Popaddrstack');
{$endif}
  hp:=@ExceptAddrStack;
  If hp^=nil then
    begin
      writeln ('At end of ExceptionAddresStack');
      halt (255);
    end
  else
    begin
      hp^:=hp^^.Next;
    end;
end;


function fpc_PopObjectStack : TObject;[Public, Alias : 'FPC_POPOBJECTSTACK']; compilerproc;
var
  hp : PExceptObject;
begin
{$ifdef excdebug}
  writeln ('In PopObjectstack');
{$endif}
  If ExceptObjectStack=nil then
    begin
    writeln ('At end of ExceptionObjectStack');
    halt (1);
    end
  else
    begin
       { we need to return the exception object to dispose it }
       if ExceptObjectStack^.refcount = 0 then begin
         fpc_PopObjectStack:=ExceptObjectStack^.FObject;
       end else begin
         fpc_PopObjectStack:=nil;
       end;
       hp:=ExceptObjectStack;
       ExceptObjectStack:=ExceptObjectStack^.next;
       if assigned(hp^.frames) then
         freemem(hp^.frames);
       dispose(hp);
       erroraddr:=nil;
    end;
end;

{ this is for popping exception objects when a second exception is risen }
{ in an except/on                                                        }
function fpc_PopSecondObjectStack : TObject;[Public, Alias : 'FPC_POPSECONDOBJECTSTACK']; compilerproc;
var
  hp : PExceptObject;
begin
{$ifdef excdebug}
  writeln ('In PopObjectstack');
{$endif}
  If not(assigned(ExceptObjectStack)) or
     not(assigned(ExceptObjectStack^.next)) then
    begin
      writeln ('At end of ExceptionObjectStack');
      halt (1);
    end
  else
    begin
      if ExceptObjectStack^.next^.refcount=0 then
        { we need to return the exception object to dispose it if refcount=0 }
        fpc_PopSecondObjectStack:=ExceptObjectStack^.next^.FObject
      else
        fpc_PopSecondObjectStack:=nil;
      hp:=ExceptObjectStack^.next;
      ExceptObjectStack^.next:=hp^.next;
      if assigned(hp^.frames) then
        freemem(hp^.frames);
      dispose(hp);
    end;
end;

Procedure fpc_ReRaise;[Public, Alias : 'FPC_RERAISE']; compilerproc;
begin
{$ifdef excdebug}
  writeln ('In reraise');
{$endif}
  If ExceptAddrStack=Nil then
    DoUnHandledException;
  ExceptObjectStack^.refcount := 0;
  longjmp(ExceptAddrStack^.Buf^,FPC_Exception);
end;


Function fpc_Catches(Objtype : TClass) : TObject;[Public, Alias : 'FPC_CATCHES']; compilerproc;
var
  _Objtype : TExceptObjectClass;
begin
  If ExceptObjectStack=Nil then
   begin
     Writeln ('Internal error.');
     halt (255);
   end;
  _Objtype := TExceptObjectClass(Objtype);
  if Not ((_Objtype = TExceptObjectClass(CatchAllExceptions)) or
         (ExceptObjectStack^.FObject is _ObjType)) then
    fpc_Catches:=Nil
  else
    begin
      // catch !
      fpc_Catches:=ExceptObjectStack^.FObject;
      { this can't be done, because there could be a reraise (PFV)
       PopObjectStack;

       Also the PopAddrStack shouldn't be done, we do it now
       immediatly in the exception handler (FK)
      PopAddrStack; }
    end;
end;

Procedure fpc_DestroyException(o : TObject);[Public, Alias : 'FPC_DESTROYEXCEPTION']; compilerproc;
begin
  { with free we're on the really save side }
  o.Free;
end;


Procedure SysInitExceptions;
{
  Initialize exceptionsupport
}
begin
  ExceptObjectstack:=Nil;
  ExceptAddrStack:=Nil;
end;
