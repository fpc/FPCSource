{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2002 by Peter Vreman,
    member of the Free Pascal development team.

    Dummy implementation for platforms not having real one

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit systhrds;
{$mode objfpc}

interface

  type
     PRTLCriticalSection = ^TRTLCriticalSection;
     TRTLCriticalSection = record
       Locked: boolean
     end;

{ Include generic thread interface }
{$i threadh.inc}

implementation

{ Include generic overloaded routines }
{$i thread.inc}

{ Include OS independent threadvar initialization }
{$ifdef HASTHREADVAR}
{$i threadvr.inc}
{$endif HASTHREADVAR}

initialization
  SetNoThreadManager;
end.
{
  $Log$
  Revision 1.1  2005-01-21 21:43:12  armin
  * applied patch to compile go32v2 from Tomas (tested by John)


}
