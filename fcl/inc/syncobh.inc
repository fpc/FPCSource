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

{
  $Log$
  Revision 1.6  2003-06-14 19:14:58  michael
  + Some improvements for the Linux version

  Revision 1.5  2003/06/11 12:00:09  michael
  + Implemented Win32 of syncobjs

  Revision 1.4  2002/09/07 15:15:26  peter
    * old logs removed and tabs fixed

}
