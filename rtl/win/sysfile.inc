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


{*****************************************************************************
                          Low Level File Routines
*****************************************************************************}

function do_isdevice(handle:thandle):boolean;
begin
{$ifndef WINCE}
  do_isdevice:=(getfiletype(handle)=2);
{$else WINCE}
  do_isdevice:=(handle = StdInputHandle) or (handle = StdOutputHandle) or (handle = StdErrorHandle);
{$endif WINCE}
end;


procedure do_close(h : thandle);
begin
  if do_isdevice(h) then
   exit;
  CloseHandle(h);
end;


procedure do_erase(p : pchar);
begin
   DoDirSeparators(p);
   if DeleteFile(p)=0 then
    Begin
      errno:=GetLastError;
      if errno=5 then
       begin
         if ((GetFileAttributes(p) and FILE_ATTRIBUTE_DIRECTORY)=FILE_ATTRIBUTE_DIRECTORY) then
          errno:=2;
       end;
      Errno2InoutRes;
    end;
end;


procedure do_rename(p1,p2 : pchar);
begin
  DoDirSeparators(p1);
  DoDirSeparators(p2);
  if MoveFile(p1,p2)=0 then
   Begin
      errno:=GetLastError;
      Errno2InoutRes;
   end;
end;


function do_write(h:thandle;addr:pointer;len : longint) : longint;
var
   size:longint;
begin
   if writefile(h,addr,len,size,nil)=0 then
    Begin
      errno:=GetLastError;
      Errno2InoutRes;
    end;
   do_write:=size;
end;


function do_read(h:thandle;addr:pointer;len : longint) : longint;
var
  _result:longint;
begin
  if readfile(h,addr,len,_result,nil)=0 then
    Begin
      errno:=GetLastError;
      if errno=ERROR_BROKEN_PIPE then
        errno:=0
      else
        Errno2InoutRes;
    end;
  do_read:=_result;
end;


function do_filepos(handle : thandle) : Int64;
var
  l:longint;
begin
{$ifndef wince}
  if assigned(SetFilePointerEx) then
    begin
      if not(SetFilePointerEx(handle,0,@result,FILE_CURRENT)) then
        begin
          errno:=GetLastError;
          Errno2InoutRes;
        end;
    end
  else
{$endif wince}
    begin
      l:=SetFilePointer(handle,0,nil,FILE_CURRENT);
      if l=-1 then
       begin
        l:=0;
        errno:=GetLastError;
        Errno2InoutRes;
       end;
      do_filepos:=l;
    end;
end;


procedure do_seek(handle:thandle;pos : Int64);
begin
{$ifndef wince}
  if assigned(SetFilePointerEx) then
    begin
      if not(SetFilePointerEx(handle,pos,nil,FILE_BEGIN)) then
        begin
          errno:=GetLastError;
          Errno2InoutRes;
        end;
    end
  else
{$endif wince}
    begin
      if SetFilePointer(handle,pos,nil,FILE_BEGIN)=-1 then
       Begin
        errno:=GetLastError;
        Errno2InoutRes;
       end;
    end;
end;


function do_seekend(handle:thandle):Int64;
begin
{$ifndef wince}
  if assigned(SetFilePointerEx) then
    begin
      if not(SetFilePointerEx(handle,0,@result,FILE_END)) then
        begin
          errno:=GetLastError;
          Errno2InoutRes;
        end;
    end
  else
{$endif wince}
    begin
      do_seekend:=SetFilePointer(handle,0,nil,FILE_END);
      if do_seekend=-1 then
        begin
          errno:=GetLastError;
          Errno2InoutRes;
        end;
    end;
end;


function do_filesize(handle : thandle) : Int64;
var
  aktfilepos : Int64;
begin
  aktfilepos:=do_filepos(handle);
  do_filesize:=do_seekend(handle);
  do_seek(handle,aktfilepos);
end;


procedure do_truncate (handle:thandle;pos:Int64);
begin
   do_seek(handle,pos);
   if not(SetEndOfFile(handle)) then
    begin
      errno:=GetLastError;
      Errno2InoutRes;
    end;
end;


procedure do_open(var f;p:pchar;flags:longint);
{
  filerec and textrec have both handle and mode as the first items so
  they could use the same routine for opening/creating.
  when (flags and $100)   the file will be append
  when (flags and $1000)  the file will be truncate/rewritten
  when (flags and $10000) there is no check for close (needed for textfiles)
}
Const
  file_Share_Read   = $00000001;
  file_Share_Write  = $00000002;
  file_Share_Delete = $00000004;
Var
  shflags,
  oflags,cd : longint;
  security : TSecurityAttributes;
begin
  DoDirSeparators(p);
  { close first if opened }
  if ((flags and $10000)=0) then
   begin
     case filerec(f).mode of
       fminput,fmoutput,fminout : Do_Close(filerec(f).handle);
       fmclosed : ;
     else
      begin
        {not assigned}
        inoutres:=102;
        exit;
      end;
     end;
   end;
  { reset file handle }
  filerec(f).handle:=UnusedHandle;
  { convert filesharing }
  shflags:=0;
  if ((filemode and fmshareExclusive) = fmshareExclusive) then
    { no sharing }
  else
    if (filemode = fmShareCompat) or ((filemode and fmshareDenyWrite) = fmshareDenyWrite) then
      shflags := file_Share_Read
  else
    if ((filemode and fmshareDenyRead) = fmshareDenyRead) then
      shflags := file_Share_Write
  else
    if ((filemode and fmshareDenyNone) = fmshareDenyNone) then
      shflags := file_Share_Read + file_Share_Write;
  { convert filemode to filerec modes }
  case (flags and 3) of
   0 : begin
         filerec(f).mode:=fminput;
         oflags:=longint(GENERIC_READ);
       end;
   1 : begin
         filerec(f).mode:=fmoutput;
         oflags:=longint(GENERIC_WRITE);
       end;
   2 : begin
         filerec(f).mode:=fminout;
         oflags:=longint(GENERIC_WRITE or GENERIC_READ);
       end;
  end;
  { create it ? }
  if (flags and $1000)<>0 then
    cd:=CREATE_ALWAYS
  { or Append/Open ? }
  else
    cd:=OPEN_EXISTING;
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
  security.nLength := Sizeof(TSecurityAttributes);
  security.bInheritHandle:=true;
  security.lpSecurityDescriptor:=nil;
  filerec(f).handle:=CreateFile(p,oflags,shflags,@security,cd,FILE_ATTRIBUTE_NORMAL,0);

  { append mode }
  if ((flags and $100)<>0) and
     (filerec(f).handle<>0) and
     (filerec(f).handle<>UnusedHandle) then
   begin
     do_seekend(filerec(f).handle);
     filerec(f).mode:=fmoutput; {fool fmappend}
   end;

  { get errors }
  { handle -1 is returned sometimes !! (PM) }
  if (filerec(f).handle=0) or (filerec(f).handle=UnusedHandle) then
    begin
      errno:=GetLastError;
      Errno2InoutRes;
    end;
end;
