{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2004 Karoly Balogh for Genesi S.a.r.l. <www.genesi.lu>

    timer.device interface unit for MorphOS/PowerPC

    MorphOS port was done on a free Pegasos II/G4 machine 
    provided by Genesi S.a.r.l. <www.genesi.lu>

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit timer;

interface

uses
  exec;

var 
  TimerBase : Pointer;

{$include timerd.inc}
{$include timerf.inc}

implementation

begin
end.

{
  $Log$
  Revision 1.2  2004-08-03 19:45:18  karoly
    * fixed missing uses exec

  Revision 1.1  2004/06/26 20:46:17  karoly
    * initial revision

}
