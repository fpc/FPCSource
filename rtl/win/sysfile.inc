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
  do_isdevice:=(handle = StdInputHandle) or (handle = StdOutputHandle) or (handle = StdErrorHandle);
end;


procedure do_close(h : thandle);
begin
  if do_isdevice(h) then
   exit;
  CloseHandle(h);
end;

{$ifdef FPC_UNICODE_RTL}
procedure do_erase(const p : unicodestring);
var lp:unicodestring;
begin
   lp:=p;
   DoDirSeparators(lp);
   if DeleteFile(punicodechar(lp))=0 then
    Begin
      errno:=GetLastError;
      if errno=5 then
       begin
         if ((GetFileAttributes(punicodechar(lp)) and FILE_ATTRIBUTE_DIRECTORY)=FILE_ATTRIBUTE_DIRECTORY) then
          errno:=2;
       end;
      Errno2InoutRes;
    end;
end;

procedure do_erase(const p : rawbytestring);
begin
   Do_Erase(Unicodestring(p));
end;

procedure do_rename(const p1:unicodestring;const p2 : unicodestring);
var lp1,lp2:unicodestring;
begin
  lp1:=p1; lp2:=p2;
  DoDirSeparators(lp1);
  DoDirSeparators(lp2);
  if MoveFile(punicodechar(lp1),punicodechar(lp2))=0 then
   Begin
      errno:=GetLastError;
      Errno2InoutRes;
   end;
end;

procedure do_rename(const p1:unicodestring;const p2 : rawbytestring);
begin
  do_rename(p1,unicodestring(p2));
end;
{$else}
procedure do_erase(p : rawbytestring);
begin
   DoDirSeparators(p);
   if DeleteFile(pchar(p))=0 then
    Begin
      errno:=GetLastError;
      if errno=5 then
       begin
         if ((GetFileAttributes(pchar(p)) and FILE_ATTRIBUTE_DIRECTORY)=FILE_ATTRIBUTE_DIRECTORY) then
          errno:=2;
       end;
      Errno2InoutRes;
    end;
end;


procedure do_rename(p1,p2 : rawbytestring);
begin
  DoDirSeparators(p1);
  DoDirSeparators(p2);
  if MoveFile(pchar(p1),pchar(p2))=0 then
   Begin
      errno:=GetLastError;
      Errno2InoutRes;
   end;
end;

{$endif}
function do_write(h:thandle;addr:pointer;len : longint) : longint;
var
   size:longint;
{$ifndef WINCE}
   ConsoleMode : dword;
   CodePage : UInt;
   accept_smaller_size : boolean;
{$endif ndef WINCE}
begin
   if writefile(h,addr,len,size,nil)=0 then
    Begin
      errno:=GetLastError;
      Errno2InoutRes;
{$ifndef WINCE}
    end
   else if (size<len) then
    Begin
      if GetConsoleMode (h, @ConsoleMode) then
       Begin
         accept_smaller_size:=false;
         { GetConsoleMode success means that we do have a
           console handle that might return less than
           LEN because a UTF-8 with length LEN input was
           transformed into a shorter string of size SIZE }
         CodePage:=GetConsoleOutputCP;
         Case CodePage of
           1200, {utf-16}
           1201, {unicodeFFFE}
           12000, {utf-32}
           12001, {utf-32BE}
           65000, {utf-7}
           65001: {utf-8}
             accept_smaller_size:=true;
         end;
         if accept_smaller_size then
           size:=len;
       end;

{$endif ndef WINCE}
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


type
  tint64rec = record
    low, high: dword;
  end;

function do_filepos(handle : thandle) : Int64;
var
  rslt: tint64rec;
begin
  rslt.high := 0;
  rslt.low := SetFilePointer(handle, 0, @rslt.high, FILE_CURRENT);
  if (rslt.low = $FFFFFFFF) and (GetLastError <> 0) then
  begin
    errno := GetLastError;
    Errno2InoutRes;
  end;
  do_filepos := int64(rslt);
end;


procedure do_seek(handle: thandle; pos: Int64);
var
  posHigh: LongInt;
begin
  posHigh := tint64rec(pos).high;
  if (SetFilePointer(handle, pos, @posHigh, FILE_BEGIN)=-1) and
  { return value of -1 is valid unless GetLastError is non-zero }
    (GetLastError <> 0) then
  begin
    errno := GetLastError;
    Errno2InoutRes;
  end;
end;


function do_seekend(handle:thandle):Int64;
var
  rslt: tint64rec;
begin
  rslt.high := 0;
  rslt.low := SetFilePointer(handle, 0, @rslt.high, FILE_END);
  if (rslt.low = $FFFFFFFF) and (GetLastError <> 0) then
  begin
    errno := GetLastError;
    Errno2InoutRes;
  end;
  do_seekend := int64(rslt);
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


{$ifdef FPC_UNICODE_RTL}
procedure do_open(var f;const op:unicodestring;flags:longint);
{$else}
procedure do_open(var f;p:RawByteString;flags:longint);
{$endif}
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
{$ifdef FPC_UNICODE_RTL}
  p : unicodestring;
{$endif}
begin
{$ifdef FPC_UNICODE_RTL}
  p:=op;
{$endif}
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
    if ((filemode and $f0) = fmShareCompat) or ((filemode and fmshareDenyWrite) = fmshareDenyWrite) then
      shflags := file_Share_Read
  else
    if ((filemode and fmshareDenyRead) = fmshareDenyRead) then
      shflags := file_Share_Write
  else
    if ((filemode and fmshareDenyNone) = fmshareDenyNone) then
      shflags :=
{$ifdef WINCE}
        { WinCE does not know file_share_delete }
        file_Share_Read or file_Share_Write;
{$else WINCE}
        fmShareDenyNoneFlags;
{$endif WINCE}
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
  if p='' then
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
  {$ifdef FPC_UNICODE_RTL}
  filerec(f).handle:=CreateFile(punicodechar(p),oflags,shflags,@security,cd,FILE_ATTRIBUTE_NORMAL,0);
  {$else}
  filerec(f).handle:=CreateFile(pchar(p),oflags,shflags,@security,cd,FILE_ATTRIBUTE_NORMAL,0);
  {$endif}
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

{$ifdef FPC_UNICODE_RTL}
procedure do_open(var f;const p:rawbytestring;flags:longint);
begin
 do_open(f,UnicodeString(p),flags);  
end;
{$endif}
