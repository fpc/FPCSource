{
    $Id$
    This file is part of the Free Pascal run time library.

    dos functions (V50) for MorphOS/PowerPC
    Copyright (c) 2002 The MorphOS Development Team, All Rights Reserved.
    
    Free Pascal conversion 
    Copyright (c) 2004 Karoly Balogh for Genesi S.a.r.l. <www.genesi.lu>

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{ dos.library functions }

function Open(fname     : PChar   location 'd1';
              accessMode: LongInt location 'd2'): LongInt; 
SysCall MOS_DOSBase 30;

function Close2(fileh: LongInt location 'd1'): Boolean; 
SysCall MOS_DOSBase 36;

function Read2(fileh : LongInt location 'd1'; 
               buffer: Pointer location 'd2'; 
               length: LongInt location 'd3'): LongInt; 
SysCall MOS_DOSBase 42;

function Write2(fileh : LongInt location 'd1'; 
                buffer: Pointer location 'd2'; 
                length: LongInt location 'd3'): LongInt; 
SysCall MOS_DOSBase 48;

function Input2: LongInt; 
SysCall MOS_DOSBase 54;

function Output2: LongInt; 
SysCall MOS_DOSBase 60;

function Seek2(fileh   : LongInt location 'd1';
               position: LongInt location 'd2';
               posmode : LongInt location 'd3'): LongInt; 
SysCall MOS_DOSBase 66;

function DeleteFile(fname: PChar location 'd1'): Boolean; 
SysCall MOS_DOSBase 72;

function Rename2(oldName: PChar location 'd1';
                 newName: PChar location 'd2'): Boolean; 
SysCall MOS_DOSBase 78;

function Lock(lname     : PChar   location 'd1';
              accessMode: LongInt location 'd2'): LongInt; 
SysCall MOS_DOSBase 84;

procedure Unlock(lock: LongInt location 'd1'); 
SysCall MOS_DOSBase 90;

function Examine(lock         : LongInt location 'd1';
                 FileInfoBlock: Pointer location 'd2'): Boolean; 
SysCall MOS_DOSBase 102;

function Info(lock          : LongInt   location 'd1';
              parameterBlock: PInfoData location 'd2'): Boolean; 
SysCall MOS_DOSBase 114;

function CreateDir(dname: PChar location 'd1'): LongInt; 
SysCall MOS_DOSBase 120;

function CurrentDir(lock: LongInt location 'd1'): LongInt; 
SysCall MOS_DOSBase 126;

function IoErr: LongInt; 
SysCall MOS_DOSBase 132;

function SetProtection(name: PChar   location 'd1';
                       mask: LongInt location 'd2'): Boolean; 
SysCall MOS_DOSBase 186;

function DateStamp(var ds: TDateStamp location 'd1'): LongInt; 
SysCall MOS_DOSBase 192;

function SetFileDate(name: PChar      location 'd1';
                     date: PDateStamp location 'd2'): Boolean; 
SysCall MOS_DOSBase 396;

function NameFromLock(lock  : LongInt location 'd1';
                      buffer: PChar   location 'd2';
                      len   : LongInt location 'd3'): Boolean; 
SysCall MOS_DOSBase 402;

function SetFileSize(fileh   : LongInt location 'd1';
                     position: LongInt location 'd2';
                     posmode : LongInt location 'd3'): LongInt; 
SysCall MOS_DOSBase 456;

function GetArgStr: PChar; 
SysCall MOS_DOSBase 534;

function GetCurrentDirName(buf: PChar   location 'd1';
                           len: LongInt location 'd2'): Boolean; 
SysCall MOS_DOSBase 564;

function GetProgramName(buf: PChar   location 'd1';
                        len: LongInt location 'd2'): Boolean; 
SysCall MOS_DOSBase 576;

function GetProgramDir: LongInt; 
SysCall MOS_DOSBase 600;

function SystemTagList(command: PChar   location 'd1';
                       tags   : Pointer location 'd2'): LongInt; 
SysCall MOS_DOSBase 606;

function LockDosList(flags: LongInt location 'd1'): PDOSList; 
SysCall MOS_DOSBase 654;

procedure UnLockDosList(flags: LongInt location 'd1'); 
SysCall MOS_DOSBase 660;

function NextDosEntry(dlist: PDOSList location 'd1';
                      flags: LongInt  location 'd2'): PDOSList; 
SysCall MOS_DOSBase 690;

function MatchFirst(pat   : PChar       location 'd1';
                    anchor: PAnchorPath location 'd2'): LongInt; 
SysCall MOS_DOSBase 822;

function MatchNext(anchor: PAnchorPath location 'd1'): LongInt; 
SysCall MOS_DOSBase 828;

procedure MatchEnd(anchor: PAnchorPath location 'd1'); 
SysCall MOS_DOSBase 834;

function GetVar(vname : PChar   location 'd1';
                buffer: PChar   location 'd2';
                size  : LongInt location 'd3';
                flags : LongInt location 'd4'): LongInt; 
SysCall MOS_DOSBase 906;

function WriteChars(buf   : PChar   location 'd1'; 
                    buflen: LongInt location 'd2'): LongInt; 
SysCall MOS_DOSBase 942;


{
  $Log$
  Revision 1.1  2004-06-26 20:46:17  karoly
    * initial revision

}
