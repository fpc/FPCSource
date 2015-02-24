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
                        Low level File Routines
       All these functions can set InOutRes on errors
 ****************************************************************************}


PROCEDURE NW2PASErr (Err : LONGINT);
BEGIN
  if Err = 0 then { Else it will go through all the cases }
   exit;
  case Err of
   Sys_ENFILE,
   Sys_EMFILE : Inoutres:=4;
   Sys_ENOENT : Inoutres:=2;
    Sys_EBADF : Inoutres:=6;
   Sys_ENOMEM,
   Sys_EFAULT : Inoutres:=217;
   Sys_EINVAL : Inoutres:=218;
    Sys_EPIPE,
    Sys_EINTR,
      Sys_EIO,
   Sys_EAGAIN,
   Sys_ENOSPC : Inoutres:=101;
 Sys_ENAMETOOLONG,
    Sys_ELOOP,
  Sys_ENOTDIR : Inoutres:=3;
    Sys_EROFS,
   Sys_EEXIST,
   Sys_EACCES : Inoutres:=5;
  Sys_EBUSY   : Inoutres:=162;
  end;
END;

FUNCTION errno : LONGINT;
BEGIN
  errno := __get_errno_ptr^;
END;

PROCEDURE Errno2Inoutres;
BEGIN
  NW2PASErr (errno);
END;

PROCEDURE SetFileError (VAR Err : LONGINT);
BEGIN
  IF Err >= 0 THEN
    InOutRes := 0
  ELSE
  BEGIN
    Err := errno;
    NW2PASErr (Err);
    Err := 0;
  END;
END;

{ close a file from the handle value }
procedure do_close(handle : thandle);
VAR res : LONGINT;
begin
  res := _close (handle);
  IF res <> 0 THEN
    SetFileError (res)
  ELSE
    InOutRes := 0;
end;

procedure do_erase(p : pchar; pchangeable: boolean);
VAR res : LONGINT;
begin
  res := _unlink (p);
  IF Res < 0 THEN
    SetFileError (res)
  ELSE
    InOutRes := 0;
end;

procedure do_rename(p1,p2 : pchar; p1changeable, p2changeable: boolean);
VAR res : LONGINT;
begin
  res := _rename (p1,p2);
  IF Res < 0 THEN
    SetFileError (res)
  ELSE
    InOutRes := 0
end;

function do_write(h:thandle;addr:pointer;len : longint) : longint;
VAR res : LONGINT;
begin
  res := _write (h,addr,len);
  IF res > 0 THEN
    InOutRes := 0
  ELSE
    SetFileError (res);
  do_write := res;
end;

function do_read(h:thandle;addr:pointer;len : longint) : longint;
VAR res : LONGINT;
begin
  res := _read (h,addr,len);
  IF res > 0 THEN
    InOutRes := 0
  ELSE
    SetFileError (res);
  do_read := res;
end;


function do_filepos(handle : thandle) : longint;
VAR res : LONGINT;
begin
  InOutRes:=1;
  res := _tell (handle);
  IF res < 0 THEN
    SetFileError (res)
  ELSE
    InOutRes := 0;
  do_filepos := res;
end;

CONST SEEK_SET = 0; // Seek from beginning of file.
      SEEK_CUR = 1; // Seek from current position.
      SEEK_END = 2; // Seek from end of file.


procedure do_seek(handle:thandle;pos : longint);
VAR res : LONGINT;
begin
  res := _lseek (handle,pos, SEEK_SET);
  IF res >= 0 THEN
    InOutRes := 0
  ELSE
    SetFileError (res);
end;

function do_seekend(handle:thandle):longint;
VAR res : LONGINT;
begin
  res := _lseek (handle,0, SEEK_END);
  IF res >= 0 THEN
    InOutRes := 0
  ELSE
    SetFileError (res);
  do_seekend := res;
end;


function do_filesize(handle : thandle) : longint;
VAR res     : LONGINT;
begin
  res := _filelength (handle);
  IF res < 0 THEN
  BEGIN
    SetFileError (Res);
    do_filesize := -1;
  END ELSE
  BEGIN
    InOutRes := 0;
    do_filesize := res;
  END;
end;

{ truncate at a given position }
procedure do_truncate (handle:thandle;pos:longint);
VAR res : LONGINT;
begin
  res := _chsize (handle,pos);
  IF res <> 0 THEN
    SetFileError (res)
  ELSE
    InOutRes := 0;
end;

// mostly stolen from syslinux
procedure do_open(var f;p:pchar;flags:longint; pchangeable: boolean);
{
  filerec and textrec have both handle and mode as the first items so
  they could use the same routine for opening/creating.
  when (flags and $10)   the file will be append
  when (flags and $100)  the file will be truncate/rewritten
  when (flags and $1000) there is no check for close (needed for textfiles)
}
var
  oflags : longint;
Begin
{ close first if opened }
  if ((flags and $10000)=0) then
   begin
     case FileRec(f).mode of
      fminput,fmoutput,fminout : Do_Close(FileRec(f).Handle);
      fmclosed : ;
     else
      begin
        inoutres:=102; {not assigned}
        exit;
      end;
     end;
   end;
{ reset file Handle }
  FileRec(f).Handle:=UnusedHandle;

{ We do the conversion of filemodes here, concentrated on 1 place }
  case (flags and 3) of
   0 : begin
         oflags := O_RDONLY;
         filerec(f).mode := fminput;
       end;
   1 : begin
         oflags := O_WRONLY;
         filerec(f).mode := fmoutput;
       end;
   2 : begin
         oflags := O_RDWR;
         filerec(f).mode := fminout;
       end;
  end;
  if (flags and $1000)=$1000 then
   oflags:=oflags or (O_CREAT or O_TRUNC)
  else
   if (flags and $100)=$100 then
    oflags:=oflags or (O_APPEND);
{ empty name is special }
  if p[0]=#0 then
   begin
     case FileRec(f).mode of
       fminput :
         FileRec(f).Handle:=StdInputHandle;
       fminout, { this is set by rewrite }
       fmoutput :
         FileRec(f).Handle:=StdOutputHandle;
       fmappend :
         begin
           FileRec(f).Handle:=StdOutputHandle;
           FileRec(f).mode:=fmoutput; {fool fmappend}
         end;
     end;
     exit;
   end;
{ real open call }
  FileRec(f).Handle := _open(p,oflags,438);
  //WriteLn ('_open (',p,') returned ',ErrNo, 'Handle: ',FileRec(f).Handle);
  // errno does not seem to be set on succsess ??
  IF FileRec(f).Handle < 0 THEN
    if (ErrNo=Sys_EROFS) and ((OFlags and O_RDWR)<>0) then
    begin  // i.e. for cd-rom
      Oflags:=Oflags and not(O_RDWR);
      FileRec(f).Handle := _open(p,oflags,438);
    end;
  IF FileRec(f).Handle < 0 THEN
    begin
      Errno2Inoutres;
      FileRec(f).mode:=fmclosed;
    end
  ELSE
    InOutRes := 0;
End;

function do_isdevice(handle:THandle):boolean;
begin
  do_isdevice := (_isatty (handle) > 0);
end;




