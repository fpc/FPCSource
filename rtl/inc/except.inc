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

  PExceptObject = ^TExceptObject;
  TExceptObject = record
    FObject : TObject;
    Addr    : pointer;
    Next    : PExceptObject;
  end;

  TExceptObjectClass = Class of TObject;

Const
  CatchAllExceptions = -1;

Var
  ExceptAddrStack   : PExceptAddr;
  ExceptObjectStack : PExceptObject;


Function PushExceptAddr (Ft: Longint): PJmp_buf ;[Public, Alias : 'FPC_PUSHEXCEPTADDR'];
var
  Buf : PJmp_buf;
  NewAddr : PExceptAddr;
begin
{$ifdef excdebug}
  writeln ('In PushExceptAddr');
{$endif}
  If ExceptAddrstack=Nil then
    begin
      New(ExceptAddrStack);
      ExceptAddrStack^.Next:=Nil;
    end
  else
    begin
      New(NewAddr);
      NewAddr^.Next:=ExceptAddrStack;
      ExceptAddrStack:=NewAddr;
    end;
  new(buf);
  ExceptAddrStack^.Buf:=Buf;
  ExceptAddrStack^.FrameType:=ft;
  PushExceptAddr:=Buf;
end;


Procedure PushExceptObj (Obj : TObject; AnAddr : Pointer); [Public, Alias : 'FPC_PUSHEXCEPTOBJECT'];
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
end;


Procedure DoUnHandledException;
begin
  If ExceptProc<>Nil then
    If ExceptObjectStack<>Nil then
      TExceptPRoc(ExceptProc)(ExceptObjectStack^.FObject,ExceptObjectStack^.Addr);
  RunError(217);
end;


Function Raiseexcept (Obj : TObject; AnAddr : Pointer) : TObject;[Public, Alias : 'FPC_RAISEEXCEPTION'];
begin
{$ifdef excdebug}
  writeln ('In RaiseException');
{$endif}
  Raiseexcept:=nil;
  PushExceptObj(Obj,AnAddr);
  If ExceptAddrStack=Nil then
    DoUnhandledException;
  longjmp(ExceptAddrStack^.Buf^,FPC_Exception);
end;


Procedure PopAddrStack;[Public, Alias : 'FPC_POPADDRSTACK'];
var
  hp : PExceptAddr;
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
      hp:=ExceptAddrStack;
      ExceptAddrStack:=ExceptAddrStack^.Next;
      dispose(hp^.buf);
      dispose(hp);
    end;
end;


function PopObjectStack : TObject;[Public, Alias : 'FPC_POPOBJECTSTACK'];
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
       PopObjectStack:=ExceptObjectStack^.FObject;
       hp:=ExceptObjectStack;
       ExceptObjectStack:=ExceptObjectStack^.next;
       dispose(hp);
    end;
end;

{ this is for popping exception objects when a second exception is risen }
{ in an except/on                                                        }
function PopSecondObjectStack : TObject;[Public, Alias : 'FPC_POPSECONDOBJECTSTACK'];
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
       PopSecondObjectStack:=ExceptObjectStack^.next^.FObject;
       hp:=ExceptObjectStack^.next;
       ExceptObjectStack^.next:=hp^.next;
       dispose(hp);
    end;
end;

Procedure ReRaise;[Public, Alias : 'FPC_RERAISE'];
begin
{$ifdef excdebug}
  writeln ('In reraise');
{$endif}
  If ExceptAddrStack=Nil then
    DoUnHandledException;
  longjmp(ExceptAddrStack^.Buf^,FPC_Exception);
end;


Function Catches(Objtype : TExceptObjectClass) : TObject;[Public, Alias : 'FPC_CATCHES'];
begin
  If ExceptObjectStack=Nil then
   begin
     Writeln ('Internal error.');
     halt (255);
   end;
  if Not ((Objtype = TExceptObjectClass(CatchAllExceptions)) or
         (ExceptObjectStack^.FObject is ObjType)) then
    Catches:=Nil
  else
    begin
      // catch !
      Catches:=ExceptObjectStack^.FObject;
      { this can't be done, because there could be a reraise (PFV)
       PopObjectStack;

       Also the PopAddrStack shouldn't be done, we do it now
       immediatly in the exception handler (FK)
      PopAddrStack; }
    end;
end;

Procedure DestroyException(o : TObject);[Public, Alias : 'FPC_DESTROYEXCEPTION'];
begin
  { with free we're on the really save side }
  o.Free;
end;


Procedure InitExceptions;
{
  Initialize exceptionsupport
}
begin
  ExceptObjectstack:=Nil;
  ExceptAddrStack:=Nil;
end;
{
  $Log$
  Revision 1.17  2000-02-09 22:16:50  florian
    + popsecondobjectstack added

  Revision 1.16  2000/02/09 16:59:29  peter
    * truncated log

  Revision 1.15  2000/02/06 17:17:57  florian
    * popobjectstack is now a function

  Revision 1.14  2000/01/07 16:41:33  daniel
    * copyright 2000

  Revision 1.13  1999/07/27 08:14:15  florian
    * catch doesn't call popaddrstack anymore, this is done now by the compiler

  Revision 1.12  1999/07/26 12:11:28  florian
    * reraise doesn't call popaddrstack anymode
}
