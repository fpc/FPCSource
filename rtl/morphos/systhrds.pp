{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2002 by the Free Pascal development team.

    MorphOS threading support implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$mode objfpc}
{$define dynpthreads}

unit systhrds;

interface

{
  uses
    unixtype;
}
{ Posix compliant definition }
  
{$WARNING FIX ME!!! Dummy type definition}
  type
     PRTLCriticalSection = ^TRTLCriticalSection;
     TRTLCriticalSection = LongInt; 

{ Include generic thread interface }
{$i threadh.inc}

implementation

{*****************************************************************************
                             Generic overloaded
*****************************************************************************}

{ Include generic overloaded routines }
{$i thread.inc}

{ Include OS independent Threadvar initialization }
{$ifdef HASTHREADVAR}
{$i threadvr.inc}
{$endif HASTHREADVAR}

Procedure InitSystemThreads;

begin
  SetNoThreadManager;
end;

initialization
  InitSystemThreads;
end.
{
  $Log$
  Revision 1.1  2005-01-30 02:25:27  karoly
    + initial dummy implementation

}
