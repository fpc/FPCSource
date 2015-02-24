{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2005 by Free Pascal development team

    Low level file functions
    Nintendo DS does not have any drive, so no file handling is needed.
    Copyright (c) 2006 by Francesco Lombardi

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


procedure NDS2PASErr(Err: longint);
begin
  if Err = 0 then { Else it will go through all the cases }
   exit;
  case Err of
   Sys_ENFILE,
   Sys_EMFILE : Inoutres := 4;
   Sys_ENOENT : Inoutres := 2;
    Sys_EBADF : Inoutres := 6;
   Sys_ENOMEM,
   Sys_EFAULT : Inoutres := 217;
   Sys_EINVAL : Inoutres := 218;
    Sys_EPIPE,
    Sys_EINTR,
      Sys_EIO,
   Sys_EAGAIN,
   Sys_ENOSPC : Inoutres := 101;
 Sys_ENAMETOOLONG,
    Sys_ELOOP,
  Sys_ENOTDIR : Inoutres := 3;
    Sys_EROFS,
   Sys_EEXIST,
   Sys_EACCES : Inoutres := 5;
  Sys_EBUSY   : Inoutres := 162
  else begin
    Writeln(stderr, 'NDS2PASErr: unknown error ', err);
    flush(stderr);
    Inoutres := Err;
  end;
  end;
END;


procedure Errno2Inoutres;
begin
  NDS2PASErr(errno^);
end;

procedure SetFileError(var Err: longint);
begin
  if Err >= 0 then
    InOutRes := 0
  else begin
    Err := errno^;
    NDS2PASErr(Err);
    Err := 0;
  end;
end;




{ close a file from the handle value }
procedure do_close(handle: THandle);
var
  res: longint;
begin
  //fclose(P_FILE(Handle));
  res := _close(handle);
  if res <> 0 then
    SetFileError(res)
  else
    InOutRes := 0;
end;

procedure do_erase(p: pchar; pchangeable: boolean);
var
  res: longint;
begin
  //unlink(p);
  res := _unlink(p);
  if res <> 0 then
    SetFileError(res)
  else
    InOutRes := 0;
end;

procedure do_rename(p1, p2: pchar; p1changeable, p2changeable: boolean);
var
  res: longint;
begin
  //rename(p1, p2);
  res := _rename(p1, p2);
  if res <> 0 then
    SetFileError(res)
  else
    InOutRes := 0;
end;

function do_write(h: THandle; addr: pointer; len: longint) : longint;
var
  res: longint;
begin
  //result := fwrite(addr, 1, len, P_FILE(h));
  res := _write(h, addr, len);
  if res > 0 then
    InOutRes := 0
  else
    SetFileError(res);
  do_write := res;
end;

function do_read(h: THandle; addr: pointer; len: longint) : longint;
var
  res: longint;
begin
  //result := fread(addr, 1, len, P_FILE(h));
  res := _read(h, addr, len);
  if res > 0 then
    InOutRes := 0
  else
    SetFileError(res);
  do_read := res;
end;

function do_filepos(handle: THandle): longint;
var
  res: longint;
begin
  InOutRes := 0;

  //result := ftell(P_FILE(handle));
  res := _tell(handle);
  if res < 0 then
    SetFileError(res)
  else
    InOutRes := 0;
  do_filepos := res;
end;

procedure do_seek(handle: THandle; pos: longint);
var
  res: longint;
begin
  //fseek(P_FILE(handle), pos, SEEK_SET);
  _lseek(handle, pos, SEEK_SET);
  if res < 0 then
    SetFileError(res)
  else
    InOutRes := 0;
end;

function do_seekend(handle: THandle): longint;
var
  res: longint;
begin
  //result := fseek(P_FILE(handle), 0, SEEK_END);
  res := _lseek(handle, 0, SEEK_END);
  if res < 0 then
    SetFileError(res)
  else
    InOutRes := 0;
  do_seekend := res;
end;

function do_filesize(handle: THandle): longint;
var
  res     : longint;
  statbuf : TStat;
begin
  //res := fstat(fileno(P_FILE(handle)), statbuf);
  res := fstat(handle, statbuf);
  if res = 0 then
  begin
    InOutRes := 0;
    result := statbuf.st_size
  end else
  begin
    SetFileError(Res);
    do_filesize := -1;
  end;
end;

{ truncate at a given position }
procedure do_truncate(handle: THandle; pos: longint);
var
  res     : longint;
begin
  //ftruncate(fileno(P_FILE(handle)), pos);
  res := _truncate(handle, pos);
  if res <> 0 then
    SetFileError(res)
  else
    InOutRes := 0;
end;

procedure do_open(var f;p:pchar;flags:longint; pchangeable: boolean);
{
  filerec and textrec have both handle and mode as the first items so
  they could use the same routine for opening/creating.
  when (flags and $10)   the file will be append
  when (flags and $100)  the file will be truncate/rewritten
  when (flags and $1000) there is no check for close (needed for textfiles)
}
var
  oflags: longint;
begin
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
  if (flags and $1000) = $1000 then
   oflags := oflags or (O_CREAT or O_TRUNC)
  else
   if (flags and $100) = $100 then
    oflags := oflags or (O_APPEND);
{ empty name is special }
  if p[0] = #0 then
   begin
     case FileRec(f).mode of
       fminput:
         FileRec(f).Handle := StdInputHandle;
       fminout, { this is set by rewrite }
       fmoutput :
         FileRec(f).Handle := StdOutputHandle;
       fmappend :
         begin
           FileRec(f).Handle := StdOutputHandle;
           FileRec(f).mode := fmoutput; {fool fmappend}
         end;
     end;
     exit;
   end;
{ real open call }
  errno^ := 0;
  FileRec(f).Handle := _open(p, oflags, 438);
  { open somtimes returns > -1 but errno was set }
  if (errno^ <> 0) or (longint(FileRec(f).Handle) < 0) then
    if (errno^ = Sys_EROFS) and ((OFlags and O_RDWR) <> 0) then
    begin  // i.e. for cd-rom
      Oflags := Oflags and not(O_RDWR);
      FileRec(f).Handle := _open(p,oflags,438);
    end;
  if (errno^ <> 0) or (longint(FileRec(f).Handle) < 0) then
    begin
      Errno2Inoutres;
      FileRec(f).mode:=fmclosed;
    end
  else
    InOutRes := 0;
end;




function do_isdevice(handle: THandle): boolean;
begin
  //result :=  (isatty(fileno(P_FILE(handle))) > 0);
  do_isdevice := (_isatty(handle) > 0);
end;


