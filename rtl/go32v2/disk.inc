{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    Disk functions from Delphi's sysutils.pas

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

TYPE  ExtendedFat32FreeSpaceRec=packed Record
         RetSize           : WORD; { (ret) size of returned structure}
         Strucversion      : WORD; {(call) structure version (0000h)
                                    (ret) actual structure version (0000h)}
         SecPerClus,               {number of sectors per cluster}
         BytePerSec,               {number of bytes per sector}
         AvailClusters,            {number of available clusters}
         TotalClusters,            {total number of clusters on the drive}
         AvailPhysSect,            {physical sectors available on the drive}
         TotalPhysSect,            {total physical sectors on the drive}
         AvailAllocUnits,          {Available allocation units}
         TotalAllocUnits : DWORD;  {Total allocation units}
         Dummy,Dummy2    : DWORD;  {8 bytes reserved}
         END;

function do_diskdata(drive : byte; Free : BOOLEAN) : Int64;

VAR S    : String;
    Rec  : ExtendedFat32FreeSpaceRec;
    regs : registers;
BEGIN
 if (swap(dosversion)>=$070A) AND LFNSupport then
  begin
   DosError:=0;
   S:='C:\'#0;
   if Drive=0 then
    begin
     GetDir(Drive,S);
     Setlength(S,4);
     S[4]:=#0;
    end
   else
    S[1]:=chr(Drive+64);
   Rec.Strucversion:=0;
   dosmemput(tb_segment,tb_offset,Rec,SIZEOF(ExtendedFat32FreeSpaceRec));
   dosmemput(tb_segment,tb_offset+Sizeof(ExtendedFat32FreeSpaceRec)+1,S[1],4);
   regs.dx:=tb_offset+Sizeof(ExtendedFat32FreeSpaceRec)+1;
   regs.ds:=tb_segment;
   regs.di:=tb_offset;
   regs.es:=tb_segment;
   regs.cx:=Sizeof(ExtendedFat32FreeSpaceRec);
   regs.ax:=$7303;
   msdos(regs);
   if regs.ax<>$ffff then
    begin
      copyfromdos(rec,Sizeof(ExtendedFat32FreeSpaceRec));
      if Free then
       Do_DiskData:=int64(rec.AvailAllocUnits)*rec.SecPerClus*rec.BytePerSec
      else
       Do_DiskData:=int64(rec.TotalAllocUnits)*rec.SecPerClus*rec.BytePerSec;
    end
   else
    Do_DiskData:=-1;
  end
 else
  begin
   DosError:=0;
   regs.dl:=drive;
   regs.ah:=$36;
   msdos(regs);
   if regs.ax<>$FFFF then
    begin
     if Free then
      Do_DiskData:=int64(regs.ax)*regs.bx*regs.cx
     else
      Do_DiskData:=int64(regs.ax)*regs.cx*regs.dx;
    end
   else
    do_diskdata:=-1;
  end;
end;

function diskfree(drive : byte) : int64;

begin
   diskfree:=Do_DiskData(drive,TRUE);
end;


function disksize(drive : byte) : int64;
begin
  disksize:=Do_DiskData(drive,false);
end;


Function GetCurrentDir : String;
begin
  GetDir(0, result);
end;


Function SetCurrentDir (Const NewDir : String) : Boolean;
begin
  {$I-}
  ChDir(NewDir);
  result := (IOResult = 0);
  {$I+}
end;


Function CreateDir (Const NewDir : String) : Boolean;
begin
  {$I-}
  MkDir(NewDir);
  result := (IOResult = 0);
  {$I+}
end;


Function RemoveDir (Const Dir : String) : Boolean;
begin
  {$I-}
  RmDir(Dir);
  result := (IOResult = 0);
  {$I+}
end;


{
  $Log$
  Revision 1.3  2000-07-14 10:33:09  michael
  + Conditionals fixed

  Revision 1.2  2000/07/13 11:33:39  michael
  + removed logs
 
}
