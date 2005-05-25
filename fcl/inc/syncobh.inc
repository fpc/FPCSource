{
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

   TSynchroObject = class(TObject)
      procedure Acquire;virtual;
      procedure Release;virtual;
   end;

   TCriticalSection = class(TSynchroObject)
   private
      CriticalSection : TRTLCriticalSection;
   public
      procedure Acquire;override;
      procedure Release;override;
      procedure Enter;
      procedure Leave;
      constructor Create;
      destructor Destroy;override;
   end;

   THandleObject = class(TSynchroObject)
   protected
      FHandle : TEventHandle;
      FLastError : Integer;
   public
      destructor destroy;override;
      property Handle : TEventHandle read FHandle;
      property LastError : Integer read FLastError;
   end;

   TEventObject = class(THandleObject)
   private
      FSem: Pointer;
      FManualReset: Boolean;
      FEventSection: TCriticalSection;
   public
      constructor Create(EventAttributes : PSecurityAttributes;
        AManualReset,InitialState : Boolean;const Name : string);
      destructor destroy; override;
      procedure ResetEvent;
      procedure SetEvent;
      function WaitFor(Timeout : Cardinal) : TWaitResult;
      Property ManualReset : Boolean read FManualReset;
   end;

   TEvent = TEventObject;

   TSimpleEvent = class(TEventObject)
      constructor Create;
   end;
