{
    $Id$
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
    frametype : Longint;
    next      : PExceptAddr;
  end;


  TExceptObjectClass = Class of TObject;

Const
  CatchAllExceptions = SizeInt(-1);
{$ifdef SUPPORT_THREADVAR}
ThreadVar
{$else SUPPORT_THREADVAR}
Var
{$endif SUPPORT_THREADVAR}
  ExceptAddrStack   : PExceptAddr;
  ExceptObjectStack : PExceptObject;

{$IFNDEF VIRTUALPASCAL}
Function RaiseList : PExceptObject;

begin
  RaiseList:=ExceptObjectStack;
end;
{$ENDIF}

{$ifndef HAS_ADDR_STACK_ON_STACK}
Function fpc_PushExceptAddr (Ft: Longint): PJmp_buf ;
  [Public, Alias : 'FPC_PUSHEXCEPTADDR'];saveregisters;
{$else HAS_ADDR_STACK_ON_HEAP}
Function fpc_PushExceptAddr (Ft: Longint;_buf,_newaddr : pointer): PJmp_buf ;
  [Public, Alias : 'FPC_PUSHEXCEPTADDR'];saveregisters; {$ifdef hascompilerproc} compilerproc; {$endif}
{$endif HAS_ADDR_STACK_ON_STACK}

var
  Buf : PJmp_buf;
  NewAddr : PExceptAddr;
begin
{$ifdef excdebug}
  writeln ('In PushExceptAddr');
{$endif}
  If ExceptAddrstack=Nil then
    begin
{$ifndef HAS_ADDR_STACK_ON_STACK}
      New(ExceptAddrStack);
{$else HAS_ADDR_STACK_ON_STACK}
      ExceptAddrStack:=PExceptAddr(_newaddr);
{$endif HAS_ADDR_STACK_ON_STACK}
      ExceptAddrStack^.Next:=Nil;
    end
  else
    begin
{$ifndef HAS_ADDR_STACK_ON_STACK}
      New(NewAddr);
{$else HAS_ADDR_STACK_ON_STACK}
      NewAddr:=PExceptAddr(_newaddr);
{$endif HAS_ADDR_STACK_ON_STACK}
      NewAddr^.Next:=ExceptAddrStack;
      ExceptAddrStack:=NewAddr;
    end;
{$ifndef HAS_ADDR_STACK_ON_STACK}
  new(buf);
{$else HAS_ADDR_STACK_ON_STACK}
  buf:=PJmp_Buf(_buf);
{$endif HAS_ADDR_STACK_ON_STACK}
  ExceptAddrStack^.Buf:=Buf;
  ExceptAddrStack^.FrameType:=ft;
  fpc_PushExceptAddr:=Buf;
end;


Procedure fpc_PushExceptObj (Obj : TObject; AnAddr,AFrame : Pointer);
  [Public, Alias : 'FPC_PUSHEXCEPTOBJECT'];saveregisters; {$ifdef hascompilerproc} compilerproc; {$endif}
var
  Newobj : PExceptObject;
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
  ExceptObjectStack^.Frame:=AFrame;
end;

{$ifdef hascompilerproc}
{ make it avalable for local use }
Procedure fpc_PushExceptObj (Obj : TObject; AnAddr,AFrame : Pointer); [external name 'FPC_PUSHEXCEPTOBJECT'];
{$endif}


Procedure DoUnHandledException;
begin
  If ExceptProc<>Nil then
    If ExceptObjectStack<>Nil then
      TExceptPRoc(ExceptProc)(ExceptObjectStack^.FObject,ExceptObjectStack^.Addr,ExceptObjectStack^.Frame);
  RunError(217);
end;


Function fpc_Raiseexception (Obj : TObject; AnAddr,AFrame : Pointer) : TObject;[Public, Alias : 'FPC_RAISEEXCEPTION']; {$ifdef hascompilerproc} compilerproc; {$endif}
begin
{$ifdef excdebug}
  writeln ('In RaiseException');
{$endif}
  fpc_Raiseexception:=nil;
  fpc_PushExceptObj(Obj,AnAddr,AFrame);
  If ExceptAddrStack=Nil then
    DoUnhandledException;
  if (RaiseProc <> nil) and (ExceptObjectStack <> nil) then
    RaiseProc(Obj, AnAddr, AFrame);
  longjmp(ExceptAddrStack^.Buf^,FPC_Exception);
end;


Procedure fpc_PopAddrStack;[Public, Alias : 'FPC_POPADDRSTACK']; {$ifdef hascompilerproc} compilerproc; {$endif}
{$ifndef HAS_ADDR_STACK_ON_STACK}
var
  hp : PExceptAddr;
{$endif HAS_ADDR_STACK_ON_STACK}
begin
{$ifdef excdebug}
  writeln ('In Popaddrstack');
{$endif}
  If ExceptAddrStack=nil then
    begin
      writeln ('At end of ExceptionAddresStack');
      halt (255);
    end
  else
    begin
{$ifndef HAS_ADDR_STACK_ON_STACK}
      hp:=ExceptAddrStack;
      ExceptAddrStack:=ExceptAddrStack^.Next;
      dispose(hp^.buf);
      dispose(hp);
{$else HAS_ADDR_STACK_ON_STACK}
      ExceptAddrStack:=ExceptAddrStack^.Next;
{$endif HAS_ADDR_STACK_ON_STACK}
    end;
end;


function fpc_PopObjectStack : TObject;[Public, Alias : 'FPC_POPOBJECTSTACK']; {$ifdef hascompilerproc} compilerproc; {$endif}
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
       fpc_PopObjectStack:=ExceptObjectStack^.FObject;
       hp:=ExceptObjectStack;
       ExceptObjectStack:=ExceptObjectStack^.next;
       dispose(hp);
    end;
end;

{ this is for popping exception objects when a second exception is risen }
{ in an except/on                                                        }
function fpc_PopSecondObjectStack : TObject;[Public, Alias : 'FPC_POPSECONDOBJECTSTACK']; {$ifdef hascompilerproc} compilerproc; {$endif}
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
       { we need to return the exception object to dispose it }
       fpc_PopSecondObjectStack:=ExceptObjectStack^.next^.FObject;
       hp:=ExceptObjectStack^.next;
       ExceptObjectStack^.next:=hp^.next;
       dispose(hp);
    end;
end;

Procedure fpc_ReRaise;[Public, Alias : 'FPC_RERAISE']; {$ifdef hascompilerproc} compilerproc; {$endif}
begin
{$ifdef excdebug}
  writeln ('In reraise');
{$endif}
  If ExceptAddrStack=Nil then
    DoUnHandledException;
  longjmp(ExceptAddrStack^.Buf^,FPC_Exception);
end;


Function fpc_Catches(Objtype : TClass) : TObject;[Public, Alias : 'FPC_CATCHES']; {$ifdef hascompilerproc} compilerproc; {$endif}
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

Procedure fpc_DestroyException(o : TObject);[Public, Alias : 'FPC_DESTROYEXCEPTION']; {$ifdef hascompilerproc} compilerproc; {$endif}
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
{
  $Log$
  Revision 1.11  2003-09-06 21:56:29  marco
   * one VIRTUALPASCAL

  Revision 1.10  2003/05/01 08:05:23  florian
    * started to make the rtl 64 bit save by introducing SizeInt and SizeUInt (similar to size_t of C)

  Revision 1.9  2002/10/14 19:39:17  peter
    * threads unit added for thread support

  Revision 1.8  2002/09/07 15:07:45  peter
    * old logs removed and tabs fixed

}
