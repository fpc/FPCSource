{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2001 by Free Pascal development team

    Low leve file functions

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{****************************************************************************

                          Low Level File Routines

****************************************************************************}

procedure allowslash(p:Pchar);
{Allow slash as backslash.}
var i:longint;
begin
    for i:=0 to strlen(p) do
        if p[i]='/' then p[i]:='\';
end;

procedure do_close(h:thandle);
begin
{ Only three standard handles under real OS/2 }
  if h>2 then
  begin
    InOutRes:=DosClose(h);
  end;
{$ifdef IODEBUG}
  writeln('do_close: handle=', H, ', InOutRes=', InOutRes);
{$endif}
end;

procedure do_erase(p:Pchar);
begin
  allowslash(p);
  inoutres:=DosDelete(p);
end;

procedure do_rename(p1,p2:Pchar);
begin
  allowslash(p1);
  allowslash(p2);
  inoutres:=DosMove(p1, p2);
end;

function do_read(h:thandle;addr:pointer;len:longint):longint;
Var
  T: cardinal;
begin
{$ifdef IODEBUG}
  write('do_read: handle=', h, ', addr=', ptrint(addr), ', length=', len);
{$endif}
  InOutRes:=DosRead(H, Addr, Len, T);
  do_read:= longint (T);
{$ifdef IODEBUG}
  writeln(', actual_len=', t, ', InOutRes=', InOutRes);
{$endif}
end;

function do_write(h:thandle;addr:pointer;len:longint) : longint;
Var
  T: cardinal;
begin
{$ifdef IODEBUG}
  write('do_write: handle=', h, ', addr=', ptrint(addr), ', length=', len);
{$endif}
  InOutRes:=DosWrite(H, Addr, Len, T);
  do_write:= longint (T);
{$ifdef IODEBUG}
  writeln(', actual_len=', t, ', InOutRes=', InOutRes);
{$endif}
end;

function Do_FilePos (Handle: THandle): int64;
var
  PosActual: int64;
begin
  InOutRes := Sys_DosSetFilePtrL (Handle, 0, 1, PosActual);
  Do_FilePos := PosActual;
{$ifdef IODEBUG}
  writeln('do_filepos: handle=', Handle, ', actual_pos=', PosActual, ', InOutRes=', InOutRes);
{$endif}
end;

procedure Do_Seek (Handle: THandle; Pos: int64);
var
  PosActual: int64;
begin
  InOutRes:=Sys_DosSetFilePtrL(Handle, Pos, 0 {ZeroBased}, PosActual);
{$ifdef IODEBUG}
  writeln('do_seek: handle=', Handle, ', pos=', pos, ', actual_pos=', PosActual, ', InOutRes=', InOutRes);
{$endif}
end;

function Do_SeekEnd (Handle: THandle): int64;
var
  PosActual: int64;
begin
  InOutRes := Sys_DosSetFilePtrL (Handle, 0, 2 {EndBased}, PosActual);
  Do_SeekEnd := PosActual;
{$ifdef IODEBUG}
  writeln('do_seekend: handle=', Handle, ', actual_pos=', PosActual, ', InOutRes=', InOutRes);
{$endif}
end;

function Do_FileSize (Handle: THandle): int64;
var
  AktFilePos: int64;
begin
  AktFilePos := Do_FilePos (Handle);
  Do_FileSize := Do_SeekEnd (Handle);
  Do_Seek (Handle, AktFilePos);
end;

procedure Do_Truncate (Handle: THandle; Pos: int64);
begin
  InOutRes := Sys_DosSetFileSizeL (Handle, Pos);
  Do_SeekEnd (Handle);
end;


const
    FileHandleCount: cardinal = 20;

function Increase_File_Handle_Count: boolean;
var Err: word;
    L1: longint;
    L2: cardinal;
begin
  L1 := 10;
  if DosSetRelMaxFH (L1, L2) <> 0 then
    Increase_File_Handle_Count := false
  else
    if L2 > FileHandleCount then
    begin
      FileHandleCount := L2;
      Increase_File_Handle_Count := true;
    end
    else
      Increase_File_Handle_Count := false;
end;

procedure do_open(var f;p:pchar;flags:longint);
{
  filerec and textrec have both handle and mode as the first items so
  they could use the same routine for opening/creating.

  when (flags and $100)   the file will be append
  when (flags and $1000)  the file will be truncate/rewritten
  when (flags and $10000) there is no check for close (needed for textfiles)
}
var
  Action, Attrib, OpenFlags, FM: Cardinal;
begin
  // convert unix slashes to normal slashes
  allowslash(p);

  // close first if opened
  if ((flags and $10000)=0) then
  begin
    case filerec(f).mode of
      fminput,fmoutput,fminout : Do_Close(filerec(f).handle);
      fmclosed:;
    else
      begin
        inoutres:=102; {not assigned}
        exit;
      end;
    end;
  end;

  // reset file handle
  filerec(f).handle := UnusedHandle;

  Attrib:=0;
  OpenFlags:=0;

  // convert filesharing
  FM := Flags and $FF and not (8);
(* DenyNone if sharing not specified. *)
  if FM and 112 = 0 then
    FM := FM or 64;
  // convert filemode to filerec modes and access mode
  case (FM and 3) of
    0: filerec(f).mode:=fminput;
    1: filerec(f).mode:=fmoutput;
    2: filerec(f).mode:=fminout;
  end;

  if (flags and $1000)<>0 then
    OpenFlags:=OpenFlags or 2 {doOverwrite} or 16 {doCreate} // Create/overwrite
  else
    OpenFlags:=OpenFlags or 1 {doOpen}; // Open existing

  // Handle Std I/O
  if p[0]=#0 then
  begin
    case FileRec(f).mode of
      fminput :
        FileRec(f).Handle:=StdInputHandle;
      fminout, // this is set by rewrite
      fmoutput :
        FileRec(f).Handle:=StdOutputHandle;
      fmappend :
        begin
          FileRec(f).Handle:=StdOutputHandle;
          FileRec(f).mode:=fmoutput; // fool fmappend
        end;
    end;
    exit;
  end;

  Attrib:=32 {faArchive};

  InOutRes:=Sys_DosOpenL(p, FileRec(F).Handle, Action, 0, Attrib, OpenFlags, FM, nil);

  // If too many open files try to set more file handles and open again
  if (InOutRes = 4) then
    if Increase_File_Handle_Count then
      InOutRes:=Sys_DosOpenL(p, FileRec(F).Handle, Action, 0, Attrib, OpenFlags, FM, nil);

  If InOutRes<>0 then FileRec(F).Handle:=UnusedHandle;

  // If Handle created -> make some things
  if (FileRec(F).Handle <> UnusedHandle) then
  begin

    // Move to end of file for Append command
    if ((Flags and $100) <> 0) then
    begin
      do_seekend(FileRec(F).Handle);
      FileRec(F).Mode := fmOutput;
    end;

  end;

{$ifdef IODEBUG}
  writeln('do_open,', filerec(f).handle, ',', filerec(f).name, ',', filerec(f).mode, ', InOutRes=', InOutRes);
{$endif}
end;

function do_isdevice (Handle: THandle): boolean;
var
  HT, Attr: cardinal;
begin
  do_isdevice:=false;
  If DosQueryHType(Handle, HT, Attr)<>0 then exit;
  if ht=1 then do_isdevice:=true;
end;
{$ASMMODE ATT}
