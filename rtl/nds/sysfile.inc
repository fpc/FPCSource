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

{ close a file from the handle value }
procedure do_close(handle: THandle);
begin
  fclose(P_FILE(Handle));
end;

procedure do_erase(p: pchar);
begin
  unlink(p);
end;

procedure do_rename(p1, p2: pchar);
begin
  rename(p1, p2);
end;

function do_write(h: THandle; addr: pointer; len: longint) : longint;
begin
  result := fwrite(addr, 1, len, P_FILE(h));
end;

function do_read(h: THandle; addr: pointer; len: longint) : longint;
begin
  result := fread(addr, 1, len, P_FILE(h));
end;

function do_filepos(handle: THandle): longint;
begin
  result := ftell(P_FILE(handle));
end;

procedure do_seek(handle: THandle; pos: longint);
begin
  fseek(P_FILE(handle), pos, SEEK_SET);
end;

function do_seekend(handle: THandle): longint;
begin
  result := fseek(P_FILE(handle), 0, SEEK_END);
end;

function do_filesize(handle: THandle): longint;
var
  res     : LONGINT;
  statbuf : TStat;
begin
  res := fstat(fileno(P_FILE(handle)), statbuf);
  if res = 0 then
    result := statbuf.st_size
  else
    result := -1;
end;

{ truncate at a given position }
procedure do_truncate(handle: THandle; pos: longint);
begin
  ftruncate(fileno(P_FILE(handle)), pos);
end;

procedure do_open(var f; p: pchar; flags: longint);
var
  oflags : string[10];
begin
{ close first if opened }
  if ((flags and $10000) = 0) then
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
  FileRec(f).Handle := THandle(fopen(p, @oflags[1]));//_open(p,oflags,438);
  //WriteLn ('_open (',p,') returned ',ErrNo, 'Handle: ',FileRec(f).Handle);
  // errno does not seem to be set on succsess ??
  {IF FileRec(f).Handle < 0 THEN
    if (ErrNo=Sys_EROFS) and ((OFlags and O_RDWR)<>0) then
    begin  // i.e. for cd-rom
      Oflags:=Oflags and not(O_RDWR);
      FileRec(f).Handle := _open(p,oflags,438);
    end;}
{
  if FileRec(f).Handle = 0 then
    Errno2Inoutres
  else
    InOutRes := 0;
}
end;

function do_isdevice(handle: THandle): boolean;
begin
  result :=  (isatty(fileno(P_FILE(handle))) > 0);
end;


