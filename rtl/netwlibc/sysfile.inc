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
  Sys_EBUSY   : Inoutres:=162
  else begin
    Writeln (stderr,'NW2PASErr: unknown error ',err);
    libc_perror('NW2PASErr');
    Inoutres := Err;
  end;
  end;
END;


procedure Errno2Inoutres;
begin
  NW2PASErr (___errno^);
end;

procedure SetFileError (VAR Err : LONGINT);
begin
  if Err >= 0 then
    InOutRes := 0
  else begin
    // libc_perror ('SetFileError');
    Err := ___errno^;
    NW2PASErr (Err);
    Err := 0;
  end;
end;

{ close a file from the handle value }
procedure do_close(handle : thandle);
VAR res : LONGINT;
begin
  {$ifdef IOpossix}
  res := FpClose (handle);
  {$else}
  res := _fclose (_TFILE(handle));
  {$endif}
  IF res <> 0 THEN
    SetFileError (res)
  ELSE
    InOutRes := 0;
end;

procedure do_erase(p : pchar);
VAR res : LONGINT;
begin
  res := unlink (p);
  IF Res < 0 THEN
    SetFileError (res)
  ELSE
    InOutRes := 0;
end;

procedure do_rename(p1,p2 : pchar);
VAR res : LONGINT;
begin
  res := rename (p1,p2);
  IF Res < 0 THEN
    SetFileError (res)
  ELSE
    InOutRes := 0
end;

function do_write(h:thandle;addr:pointer;len : longint) : longint;
var res : LONGINT;
begin
  {$ifdef IOpossix}
  res := Fpwrite (h,addr,len);
  {$else}
  res := _fwrite (addr,1,len,_TFILE(h));
  {$endif}
  if res > 0 then
    InOutRes := 0
  else
    SetFileError (res);
  do_write := res;
  NXThreadYield;
end;

function do_read(h:thandle;addr:pointer;len : longint) : longint;
VAR res : LONGINT;
begin
  {$ifdef IOpossix}
  res := Fpread (h,addr,len);
  {$else}
  res := _fread (addr,1,len,_TFILE(h));
  {$endif}
  IF res > 0 THEN
    InOutRes := 0
  ELSE
    SetFileError (res);
  do_read := res;
  NXThreadYield;
end;


function do_filepos(handle : thandle) : longint;
var res : LONGINT;
begin
  InOutRes:=1;
  {$ifdef IOpossix}
  res := Fptell (handle);
  {$else}
  res := _ftell (_TFILE(handle));
  {$endif}
  if res < 0 THEN
    SetFileError (res)
  else
    InOutRes := 0;
  do_filepos := res;
end;


procedure do_seek(handle:thandle;pos : longint);
VAR res : LONGINT;
begin
  {$ifdef IOpossix}
  res := Fplseek (handle,pos, SEEK_SET);
  {$else}
  res := _fseek (_TFILE(handle),pos, SEEK_SET);
  {$endif}
  IF res >= 0 THEN
    InOutRes := 0
  ELSE
    SetFileError (res);
end;

function do_seekend(handle:thandle):longint;
VAR res : LONGINT;
begin
  {$ifdef IOpossix}
  res := Fplseek (handle,0, SEEK_END);
  {$else}
  res := _fseek (_TFILE(handle),0, SEEK_END);
  {$endif}
  IF res >= 0 THEN
    InOutRes := 0
  ELSE
    SetFileError (res);
  do_seekend := res;
end;


function do_filesize(handle : thandle) : longint;
VAR res     : LONGINT;
    statbuf : TStat;
begin
  {$ifdef IOpossix}
  res := Fpfstat (handle, statbuf);
  {$else}
  res := _fstat (_fileno (_TFILE(handle)), statbuf);  // was _filelength for clib
  {$endif}
  if res <> 0 then
  begin
    SetFileError (Res);
    do_filesize := -1;
  end else
  begin
    InOutRes := 0;
    do_filesize := statbuf.st_size;
  end;
end;

{ truncate at a given position }
procedure do_truncate (handle:thandle;pos:longint);
VAR res : LONGINT;
begin
  {$ifdef IOpossix}
  res := ftruncate (handle,pos);
  {$else}
  res := _ftruncate (_fileno (_TFILE(handle)),pos);
  {$endif}
  IF res <> 0 THEN
    SetFileError (res)
  ELSE
    InOutRes := 0;
end;

{$ifdef IOpossix}
// mostly stolen from syslinux
procedure do_open(var f;p:pchar;flags:longint);
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
  ___errno^ := 0;
  FileRec(f).Handle := open(p,oflags,438);
  { open somtimes returns > -1 but errno was set }
  if (___errno^ <> 0) or (longint(FileRec(f).Handle) < 0) then
    if (___errno^=Sys_EROFS) and ((OFlags and O_RDWR)<>0) then
    begin  // i.e. for cd-rom
      Oflags:=Oflags and not(O_RDWR);
      FileRec(f).Handle := open(p,oflags,438);
    end;
  if (___errno^ <> 0) or (longint(FileRec(f).Handle) < 0) then
    Errno2Inoutres
  else
    InOutRes := 0;
end;


{$else}
procedure do_open(var f;p:pchar;flags:longint);
{
  filerec and textrec have both handle and mode as the first items so
  they could use the same routine for opening/creating.
  when (flags and $10)   the file will be append
  when (flags and $100)  the file will be truncate/rewritten
  when (flags and $1000) there is no check for close (needed for textfiles)
}
var
  oflags : string[10];
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
         oflags := 'rb'#0;
         filerec(f).mode := fminput;
       end;
   1 : begin
         if (flags and $1000)=$1000 then
           oflags := 'w+b' else
           oflags := 'wb';
         filerec(f).mode := fmoutput;
       end;
   2 : begin
         if (flags and $1000)=$1000 then
           oflags := 'w+' else
           oflags := 'r+';
         filerec(f).mode := fminout;
       end;
  end;
  {if (flags and $1000)=$1000 then
   oflags:=oflags or (O_CREAT or O_TRUNC)
  else
   if (flags and $100)=$100 then
    oflags:=oflags or (O_APPEND);}
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
  FileRec(f).Handle := THandle (_fopen (p,@oflags[1]));//_open(p,oflags,438);
  //WriteLn ('_open (',p,') returned ',ErrNo, 'Handle: ',FileRec(f).Handle);
  // errno does not seem to be set on succsess ??
  {IF FileRec(f).Handle < 0 THEN
    if (ErrNo=Sys_EROFS) and ((OFlags and O_RDWR)<>0) then
    begin  // i.e. for cd-rom
      Oflags:=Oflags and not(O_RDWR);
      FileRec(f).Handle := _open(p,oflags,438);
    end;}
  if FileRec(f).Handle = 0 then
    Errno2Inoutres
  else
    InOutRes := 0;
End;
{$endif}

function do_isdevice(handle:THandle):boolean;
begin
  {$ifdef IOpossix}
  do_isdevice := (Fpisatty (handle) > 0);
  {$else}
  do_isdevice := (isatty (_fileno(_TFILE(handle))) > 0);
  {$endif}
end;





