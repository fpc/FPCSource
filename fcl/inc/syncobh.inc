{
    $Id$
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by Florian Klaempfl
    member of the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

type
   TWaitResult = (wrSignaled, wrTimeout, wrAbandoned, wrError);

   TSyncroObject = class(TObject)
      procedure Acquire;virtual;abstract;
      procedure Release;virtual;abstract;
   end;

   TCriticalSection = class(TSyncroObject)
{$ifdef win32}
   private
      CriticalSection : TRTLCriticalSection;
{$endif win32}
   public
      procedure Acquire;override;
      procedure Release;override;
      procedure Enter;
      procedure Leave;
      constructor Create;
      destructor Destroy;override;
   end;

   THandleObject = class(TSyncroObject)
   protected
      FHandle : TEventHandle;
      FLastError : Integer;
   public
      destructor destroy;override;
      property Handle : TEventHandle read FHandle;
      property LastError : Integer read FLastError;
   end;

   TEventObject = class(THandleObject)
   public
      constructor Create(EventAttributes : PSecurityAttributes;
        ManualReset,InitialState : Boolean;const Name : string);
      procedure ResetEvent;
      procedure SetEvent;
      function WaitFor(Timeout : Cardinal) : TWaitResult;
   end;

   TSimpleEvent = class(TEventObject)
      constructor Create;
   end;

{
  $Log$
  Revision 1.4  2002-09-07 15:15:26  peter
    * old logs removed and tabs fixed

}
