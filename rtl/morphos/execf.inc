{
    $Id$
    This file is part of the Free Pascal run time library.

    exec functions (V50) for MorphOS/PowerPC
    Copyright (c) 2002 The MorphOS Development Team, All Rights Reserved.
    
    Free Pascal conversion 
    Copyright (c) 2004 Karoly Balogh for Genesi S.a.r.l. <www.genesi.lu>

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


function OpenLibrary(libname: PChar   location 'a1'; 
                     libver : LongInt location 'd0'): Pointer; 
SysCall MOS_ExecBase 552;

procedure CloseLibrary(libhandle: Pointer location 'a1'); 
SysCall MOS_ExecBase 414;

function CreatePool(memflags  : LongInt location 'd0'; 
                    puddleSize: LongInt location 'd1'; 
                    threshSize: LongInt location 'd2'): Pointer; 
SysCall MOS_ExecBase 696;

procedure DeletePool(poolHeader: Pointer location 'a0'); 
SysCall MOS_ExecBase 702;

function AllocPooled(poolHeader: Pointer location 'a0';
                     memSize   : LongInt location 'd0'): Pointer; 
SysCall MOS_ExecBase 708;

function SetSignal(newSignals: LongInt location 'd0';
                   signalMask: LongInt location 'd1'): LongInt; 
SysCall MOS_ExecBase 306;

function FindTask(tname: PChar location 'a1'): PTask; 
SysCall MOS_ExecBase 294;

function GetMsg(port: PMsgPort location 'a0'): PMessage; 
SysCall MOS_ExecBase 372;

function WaitPort(port: PMsgPort location 'a0'): PMessage; 
SysCall MOS_ExecBase 384;

{ * To avoid conflict with heap manager... * }
function AllocMem2(byteSize    : LongInt location 'd0';
                   requirements: LongInt location 'd1'): Pointer; 
SysCall MOS_ExecBase 198;

{ * To avoid conflict with heap manager... * }
procedure FreeMem2(memoryBlock: Pointer location 'a1';
                   byteSize   : LongInt location 'd0'); 
SysCall MOS_ExecBase 210;

function AllocSignal(signalNum: LongInt location 'd0'): ShortInt; 
SysCall MOS_ExecBase 330;

procedure FreeSignal(signalNum: LongInt location 'd0'); 
SysCall MOS_ExecBase 336;

procedure AddPort(port: PMsgPort location 'a1'); 
SysCall MOS_ExecBase 354;

procedure RemPort(port: PMsgPort location 'a1'); 
SysCall MOS_ExecBase 360;

function DoIO(ioRequest: PIORequest location 'a1'): ShortInt; 
SysCall MOS_ExecBase 456;

function OpenDevice(const devName: PChar      location 'a0';
                    unite        : LongInt    location 'd0';
                    ioRequest    : PIORequest location 'a1';
                    flags        : LongInt    location 'd1'): ShortInt; 
SysCall MOS_ExecBase 444;

procedure CloseDevice(ioRequest: PIORequest location 'a1'); 
SysCall MOS_ExecBase 450;


{
  $Log$
  Revision 1.1  2004-06-13 22:38:08  karoly
    * initial revision

}
