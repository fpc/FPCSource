{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2004 Karoly Balogh for Genesi S.a.r.l. <www.genesi.lu>

    dos.library interface unit for MorphOS/PowerPC

    MorphOS port was done on a free Pegasos II/G4 machine 
    provided by Genesi S.a.r.l. <www.genesi.lu>

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit doslib;

interface

uses Exec, Timer;

var
  DosBase: Pointer;

{$include doslibd.inc}
{$include doslibf.inc}


{ * Define real functions * }

function Close(fileh: LongInt location 'd1'): Boolean; 
SysCall MOS_DOSBase 36;

function Read(fileh : LongInt location 'd1'; 
              buffer: Pointer location 'd2'; 
              length: LongInt location 'd3'): LongInt; 
SysCall MOS_DOSBase 42;

function Write(fileh : LongInt location 'd1'; 
               buffer: Pointer location 'd2'; 
               length: LongInt location 'd3'): LongInt; 
SysCall MOS_DOSBase 48;

function Input: LongInt; 
SysCall MOS_DOSBase 54;

function Output: LongInt; 
SysCall MOS_DOSBase 60;

function Seek(fileh   : LongInt location 'd1';
              position: LongInt location 'd2';
              posmode : LongInt location 'd3'): LongInt; 
SysCall MOS_DOSBase 66;

function Rename(oldName: PChar location 'd1';
                newName: PChar location 'd2'): Boolean; 
SysCall MOS_DOSBase 78;


implementation

begin
  DosBase:=MOS_DOSBase;
end.

{
  $Log$
  Revision 1.1  2004-06-26 20:46:17  karoly
    * initial revision

}
