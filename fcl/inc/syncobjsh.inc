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

type
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
      destructor Destroy;
   end;

{
  $Log$
  Revision 1.1  1998-09-29 11:14:25  florian
    + initial revision

}
