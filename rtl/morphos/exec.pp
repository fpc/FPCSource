{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2004 Karoly Balogh for Genesi S.a.r.l. <www.genesi.lu>

    exec.library interface unit for MorphOS/PowerPC

    MorphOS port was done on a free Pegasos II/G4 machine 
    provided by Genesi S.a.r.l. <www.genesi.lu>

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit exec;

interface

var
  ExecBase: Pointer;

{$include execd.inc}
{$include execf.inc}

{ * Define real AllocMem() and FreeMem() * }
function AllocMem(byteSize     : LongInt location 'd0';
                   requirements: LongInt location 'd1'): Pointer; 
SysCall MOS_ExecBase 198;

procedure FreeMem(memoryBlock: Pointer location 'a1';
                   byteSize  : LongInt location 'd0'); 
SysCall MOS_ExecBase 210;

implementation

begin
  ExecBase:=MOS_ExecBase;
end.

{
  $Log$
  Revision 1.1  2004-06-13 22:46:36  karoly
    * initial revision

}
