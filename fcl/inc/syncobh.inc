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
  Revision 1.1  2000-07-13 06:31:31  michael
  + Initial import

  Revision 1.4  2000/01/07 01:24:33  peter
    * updated copyright to 2000

  Revision 1.3  2000/01/06 01:20:33  peter
    * moved out of packages/ back to topdir

  Revision 1.1  2000/01/03 19:33:08  peter
    * moved to packages dir

  Revision 1.1  1999/06/07 15:52:46  michael
  + Renamed to syncobh

  Revision 1.2  1998/09/30 13:41:05  florian
    * fixes to make it compilable

  Revision 1.1  1998/09/29 11:14:25  florian
    + initial revision

}
