{
    $Id$
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1998 by Florian Klaempfl
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit syncobjs;

  interface

    uses
       windows,sysutils;

    type
      PSecurityAttributes = Windows.PSecurityAttributes;
      TSecurityAttributes = Windows.TSecurityAttributes;
      TEventHandle = THandle;

    {$I syncobh.inc}

  implementation

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
         inherited Create;
         InitializeCriticalSection(CriticalSection);
      end;

    destructor TCriticalSection.Destroy;

      begin
         DeleteCriticalSection(CriticalSection);
         inherited Destroy;
      end;

    destructor THandleObject.destroy;

      begin
         CloseHandle(FHandle);
         inherited Destroy;
      end;

    constructor TEvent.Create(EventAttributes : PSecurityAttributes;
      ManualReset,InitialState : Boolean;const Name : string);

      begin
      end;

    procedure TEvent.ResetEvent;

      begin
      end;

    procedure TEvent.SetEvent;

      begin
      end;

    function TEvent.WaitFor(Timeout : Cardinal) : TWaitResult;

      begin
      end;

    constructor TSimpleEvent.Create;

      begin
      end;

end.

{
  $Log$
  Revision 1.3  2002-09-07 15:15:29  peter
    * old logs removed and tabs fixed

}
