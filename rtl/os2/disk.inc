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

{$ASMMODE ATT}

{$IFDEF INT64}

function DiskFree (Drive: byte): int64;

var FI: TFSinfo;
    RC: longint;

begin
    if (os_mode = osDOS) or (os_mode = osDPMI) then
    {Function 36 is not supported in OS/2.}
        asm
            movb 8(%ebp),%dl
            movb $0x36,%ah
            call syscall
            cmpw $-1,%ax
            je .LDISKFREE1
            mulw %cx
            mulw %bx
            shll $16,%edx
            movw %ax,%dx
            xchgl %edx,%eax
            leave
            ret
         .LDISKFREE1:
            cltd
            leave
            ret
        end
    else
        {In OS/2, we use the filesystem information.}
        begin
            RC := DosQueryFSInfo (Drive, 1, FI, SizeOf (FI));
            if RC = 0 then
                DiskFree := int64 (FI.Free_Clusters) *
                   int64 (FI.Sectors_Per_Cluster) * int64 (FI.Bytes_Per_Sector)
            else
                DiskFree := -1;
        end;
end;

function DiskSize (Drive: byte): int64;

var FI: TFSinfo;
    RC: longint;

begin
    if (os_mode = osDOS) or (os_mode = osDPMI) then
        {Function 36 is not supported in OS/2.}
        asm
            movb 8(%ebp),%dl
            movb $0x36,%ah
            call syscall
            movw %dx,%bx
            cmpw $-1,%ax
            je .LDISKSIZE1
            mulw %cx
            mulw %bx
            shll $16,%edx
            movw %ax,%dx
            xchgl %edx,%eax
            leave
            ret
        .LDISKSIZE1:
            cltd
            leave
            ret
        end
    else
        {In OS/2, we use the filesystem information.}
        begin
            RC := DosQueryFSinfo (Drive, 1, FI, SizeOf (FI));
            if RC = 0 then
                DiskSize := int64 (FI.Total_Clusters) *
                   int64 (FI.Sectors_Per_Cluster) * int64 (FI.Bytes_Per_Sector)
            else
                DiskSize := -1;
        end;
end;

{$ELSE}

function DiskFree (Drive: byte): longint;

var FI: TFSinfo;
    RC: longint;

begin
    if (os_mode = osDOS) or (os_mode = osDPMI) then
    {Function 36 is not supported in OS/2.}
        asm
            movb 8(%ebp),%dl
            movb $0x36,%ah
            call syscall
            cmpw $-1,%ax
            je .LDISKFREE1
            mulw %cx
            mulw %bx
            shll $16,%edx
            movw %ax,%dx
            xchgl %edx,%eax
            leave
            ret
         .LDISKFREE1:
            cltd
            leave
            ret
        end
    else
        {In OS/2, we use the filesystem information.}
        begin
            RC := DosQueryFSInfo (Drive, 1, FI, SizeOf (FI));
            if RC = 0 then
                DiskFree := FI.Free_Clusters *
                                   FI.Sectors_Per_Cluster * FI.Bytes_Per_Sector
            else
                DiskFree := -1;
        end;
end;

function DiskSize (Drive: byte): longint;

var FI: TFSinfo;
    RC: longint;

begin
    if (os_mode = osDOS) or (os_mode = osDPMI) then
        {Function 36 is not supported in OS/2.}
        asm
            movb 8(%ebp),%dl
            movb $0x36,%ah
            call syscall
            movw %dx,%bx
            cmpw $-1,%ax
            je .LDISKSIZE1
            mulw %cx
            mulw %bx
            shll $16,%edx
            movw %ax,%dx
            xchgl %edx,%eax
            leave
            ret
        .LDISKSIZE1:
            cltd
            leave
            ret
        end
    else
        {In OS/2, we use the filesystem information.}
        begin
            RC := DosQueryFSinfo (Drive, 1, FI, SizeOf (FI));
            if RC = 0 then
                DiskSize := FI.Total_Clusters *
                                   FI.Sectors_Per_Cluster * FI.Bytes_Per_Sector
            else
                DiskSize := -1;
        end;
end;

{$ENDIF}

function GetCurrentDir: string;
begin
 GetDir (0, Result);
end;


function SetCurrentDir (const NewDir: string): boolean;
begin
{$I-}
 ChDir (NewDir);
 Result := (IOResult = 0);
{$I+}
end;


function CreateDir (const NewDir: string): boolean;
begin
{$I-}
 MkDir (NewDir);
 Result := (IOResult = 0);
{$I+}
end;


function RemoveDir (const Dir: string): boolean;
begin
{$I-}
 RmDir (Dir);
 Result := (IOResult = 0);
 {$I+}
end;



{
 $Log$
 Revision 1.1  2000-07-13 06:31:03  michael
 + Initial import

 Revision 1.6  2000/06/04 14:23:18  hajny
   * asmmode fixed

 Revision 1.5  2000/05/28 18:21:51  hajny
   + initial implementation

 Revision 1.4  2000/05/21 15:55:11  hajny
   * int64 result for Disk* functions

 Revision 1.3  2000/02/09 16:59:33  peter
   * truncated log

 Revision 1.2  2000/01/07 16:41:45  daniel
   * copyright 2000

}
