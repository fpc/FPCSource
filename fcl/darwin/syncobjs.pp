{
    $Id: syncobjs.pp,v 1.2 2005/02/14 17:13:12 peter Exp $
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1998 by Florian Klaempfl
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$h+}
unit syncobjs;

interface

uses
  pthreads,
{$ifdef ver1_0}
  Linux,
{$else}
  unix,
{$endif}
  sysutils;

type
  PSecurityAttributes = Pointer;
  TEventHandle = THandle;
  TRTLCriticalSection = TPthreadMutex;

{$I syncobh.inc}

implementation

{ ---------------------------------------------------------------------
    Some wrappers around PThreads.
  ---------------------------------------------------------------------}

function InitializeCriticalSection(var lpCriticalSection: TRTLCriticalSection): Integer;

var
  MAttr : TMutexAttribute;

begin
  Result:=pthread_mutexattr_init(@MAttr);
  if Result=0 then
    try
    Result:=pthread_mutexattr_settype(@MAttr,longint(PTHREAD_MUTEX_RECURSIVE));
    if Result=0 then
       Result:=pthread_mutex_init(@lpCriticalSection,@MAttr);
    finally
      pthread_mutexattr_destroy(@MAttr);
    end;
end;


function EnterCriticalSection(var lpCriticalSection: TRTLCriticalSection) : Integer;

begin
  Result:=pthread_mutex_lock(@lpCriticalSection);
end;

function LeaveCriticalSection (var lpCriticalSection: TRTLCriticalSection) : Integer;
begin
  Result:=pthread_mutex_unlock(@lpCriticalSection);
end;

function DeleteCriticalSection(var lpCriticalSection: TRTLCriticalSection) : Integer;
begin
  Result:=pthread_mutex_destroy(@lpCriticalSection);
end;

{ ---------------------------------------------------------------------
    Real syncobjs implementation
  ---------------------------------------------------------------------}

{$I syncob.inc}


procedure TCriticalSection.Acquire;

begin
  EnterCriticalSection(CriticalSection);
end;

procedure TCriticalSection.Release;

begin
  LeaveCriticalSection(CriticalSection);
end;

constructor TCriticalSection.Create;

begin
  Inherited Create;
  InitializeCriticalSection(CriticalSection);
end;

destructor TCriticalSection.Destroy;

begin
  DeleteCriticalSection(CriticalSection);
end;

destructor THandleObject.destroy;

begin
end;

constructor TEventObject.Create(EventAttributes : PSecurityAttributes;
  AManualReset,InitialState : Boolean;const Name : string);

begin
  FManualReset:=AManualReset;
  FSem:=New(PSemaphore);
  FEventSection:=TCriticalSection.Create;
  sem_init(psem_t(FSem),ord(False),Ord(InitialState));
end;

destructor TEventObject.destroy;

begin
  sem_destroy(psem_t(FSem));
end;

procedure TEventObject.ResetEvent;

begin
  While sem_trywait(psem_t(FSem))=0 do
    ;
end;

procedure TEventObject.SetEvent;

Var
  Value : Longint;

begin
  FEventSection.Enter;
  Try
    sem_getvalue(FSem,@Value);
    if Value=0 then
      sem_post(psem_t(FSem));
  finally
    FEventSection.Leave;
  end;
end;


function TEventObject.WaitFor(Timeout : Cardinal) : TWaitResult;

begin
  If TimeOut<>Cardinal($FFFFFFFF) then
    result:=wrError
  else
    begin
    sem_wait(psem_t(FSem));
    result:=wrSignaled;
    if FManualReset then
      begin
      FEventSection.Enter;
      Try
        resetevent;
        sem_post(psem_t(FSem));
      Finally
        FEventSection.Leave;
      end;
      end;
    end;
end;

constructor TSimpleEvent.Create;

begin
  inherited Create(nil, True, False, '');
end;

end.

{
  $Log: syncobjs.pp,v $
  Revision 1.2  2005/02/14 17:13:12  peter
    * truncate log

}
