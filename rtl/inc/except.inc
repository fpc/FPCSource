{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993,97 by xxxx
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
  { types of frames for the exception address stack}
  cExceptionFrame = 1;
  cFinalizeFrame  = 2;

Type
  PExceptAddr = ^TExceptAddr;
  TExceptAddr = record
    buf : pjmp_buf;
    frametype : Longint;
    next : PExceptAddr;
    end;

  PExceptObject = ^TExceptObject;
  TExceptObject = record
    FObject : TObject;
    addr : pointer;
    Next : PExceptObject;
    end;

  TExceptObjectClass = Class of TObject;

Const 
  CatchAllExceptions = -1;
      
Var ExceptAddrStack : PExceptAddr;
    ExceptObjectStack : PExceptObject;


Function PushExceptAddr (Ft: Longint): PJmp_buf ;[Public, Alias : 'FPC_PUSHEXCEPTADDR'];
    
var Buf : PJmp_buf; 
    NewAddr : PExceptAddr;
    
begin
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


Procedure PushExceptObj (Obj : TObject; AnAddr : Pointer);

var 
    Newobj : PExceptObject;
    
begin
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

Function Raiseexcept (Obj : TObject; AnAddr : Pointer) : TObject;[Public, Alias : 'FPC_RAISEEXCEPTION'];

begin
  PushExceptObj(Obj,AnAddr);
  longjmp(ExceptAddrStack^.Buf^,FPC_Exception);
end;

Procedure PopAddrStack ;[Public, Alias : 'FPC_POPADDRSTACK'];

begin
  If ExceptAddrStack=nil then
    begin
    writeln ('At end of ExceptionAddresStack');
    halt (1);
    end
  else
    ExceptAddrStack:=ExceptAddrStack^.Next;
end;

Procedure PopObjectStack ;

begin
  If ExceptObjectStack=nil then
    begin
    writeln ('At end of ExceptionObjectStack');
    halt (1);
    end
  else
    ExceptObjectStack:=ExceptObjectStack^.Next;
end;

Procedure ReRaise;[Public, Alias : 'FPC_RERAISE'];

begin
  PopAddrStack;
  If ExceptAddrStack=Nil then
    begin
    writeln ('Re-Raise : At end of address chain.');
    halt (1);
    end;
  longjmp(ExceptAddrStack^.Buf^,FPC_Exception);
end;

Function Catches (Objtype : TExceptObjectClass) : TObject; [Public, Alias : 'FPC_CATCHES'];

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
    PopObjectStack;
    PopAddrStack;
    end;
end;

Procedure InitExceptions;
{
  Initialize exceptionsupport
}
begin
  ExceptObjectstack:=Nil;
  ExceptAddrStack:=Nil;
end;