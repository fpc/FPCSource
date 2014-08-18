{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2005 by Free Pascal development team

    Low level file functions

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ Enable this for file handling debug }
{DEFINE MOSFPC_FILEDEBUG}

{*****************************************************************************
                  MorphOS File-handling Support Functions
*****************************************************************************}
type
  { AmigaOS does not automatically close opened files on exit back to  }
  { the operating system, therefore as a precuation we close all files }
  { manually on exit.                                                  }
  PFileList = ^TFileList;
  TFileList = record { no packed, must be correctly aligned }
    handle   : LongInt;      { Handle to file     }
    next     : PFileList;    { Next file in list  }
    buffered : boolean;      { used buffered I/O? }
  end;

var
  AOS_fileList: PFileList; public name 'AOS_FILELIST'; { List pointer to opened files }

{ Function to be called at program shutdown, to close all opened files }
procedure CloseList(l: PFileList);
var
  tmpNext   : PFileList;
  tmpHandle : LongInt;
begin
  if l=nil then exit;

  { First, close all tracked files }
  tmpNext:=l^.next;
  while tmpNext<>nil do begin
    tmpHandle:=tmpNext^.handle;
    if (tmpHandle<>StdInputHandle) and (tmpHandle<>StdOutputHandle)
       and (tmpHandle<>StdErrorHandle) then begin
      dosClose(tmpHandle);
    end;
    tmpNext:=tmpNext^.next;
  end;

  { Next, erase the linked list }
  while l<>nil do begin
    tmpNext:=l;
    l:=l^.next;
    dispose(tmpNext);
  end;
end;

{ Function to be called to add a file to the opened file list }
procedure AddToList(var l: PFileList; h: LongInt); alias: 'ADDTOLIST'; [public];
var
  p     : PFileList;
  inList: Boolean;
begin
  inList:=False;
  if l<>nil then begin
    { if there is a valid filelist, search for the value }
    { in the list to avoid double additions }
    p:=l;
    while (p^.next<>nil) and (not inList) do
      if p^.next^.handle=h then inList:=True
                           else p:=p^.next;
    p:=nil;
  end else begin
    { if the list is not yet allocated, allocate it. }
    New(l);
    l^.next:=nil;
  end;

  if not inList then begin
    New(p);
    p^.handle:=h;
    p^.buffered:=False;
    p^.next:=l^.next;
    l^.next:=p;
  end
{$IFDEF MOSFPC_FILEDEBUG}
  else 
    RawDoFmt('FPC_FILE_DEBUG: Error! Trying add filehandle a filehandle twice: $%lx !'+#10,@h,pointer(1),nil);
{$ENDIF}
  ;
end;

{ Function to be called to remove a file from the list }
function RemoveFromList(var l: PFileList; h: LongInt): boolean; alias: 'REMOVEFROMLIST'; [public];
var
  p      : PFileList;
  inList : Boolean;
  tmpList: PFileList;
begin
  inList:=False;
  if l=nil then begin
    RemoveFromList:=inList;
    exit;
  end;

  p:=l;
  while (p^.next<>nil) and (not inList) do
    if p^.next^.handle=h then inList:=True
                         else p:=p^.next;
  
  if inList then begin
    tmpList:=p^.next^.next;
    dispose(p^.next);
    p^.next:=tmpList;
  end
{$IFDEF MOSFPC_FILEDEBUG}
  else 
    RawDoFmt('FPC_FILE_DEBUG: Error! Trying to remove not existing filehandle: $%lx !'+#10,@h,pointer(1),nil);
{$ENDIF}
  ;

  RemoveFromList:=inList;
end;

{ Function to check if file is in the list }
function CheckInList(var l: PFileList; h: LongInt): pointer; alias: 'CHECKINLIST'; [public];
var
  p      : PFileList;
  inList : Pointer;
  
begin
  inList:=nil;
  if l=nil then begin
    CheckInList:=inList;
    exit;
  end;

  p:=l;
  while (p^.next<>nil) and (inList=nil) do
    if p^.next^.handle=h then inList:=p^.next
                         else p:=p^.next;

{$IFDEF MOSFPC_FILEDEBUG}
  if inList=nil then
    RawDoFmt('FPC_FILE_DEBUG: Warning! Check for not existing filehandle: $%lx !'+#10,@h,pointer(1),nil);
{$ENDIF}

  CheckInList:=inList;
end;


{****************************************************************************
                        Low level File Routines
               All these functions can set InOutRes on errors
****************************************************************************}

{ close a file from the handle value }
procedure do_close(handle : longint);
begin
  if RemoveFromList(AOS_fileList,handle) then begin
    { Do _NOT_ check CTRL_C on Close, because it will conflict
      with System_Exit! }
    if not dosClose(handle) then
      dosError2InOut(IoErr);
  end;
end;

procedure do_erase(p : pchar; pchangeable: boolean);
var
  tmpStr: array[0..255] of Char;
begin
  tmpStr:=PathConv(strpas(p))+#0;
  checkCTRLC;
  if not dosDeleteFile(@tmpStr) then
    dosError2InOut(IoErr);
end;

procedure do_rename(p1,p2 : pchar; p1changeable, p2changeable: boolean);
{ quite stack-effective code, huh? :) damn path conversions... (KB) }
var
  tmpStr1: array[0..255] of Char;
  tmpStr2: array[0..255] of Char;
begin
  tmpStr1:=PathConv(strpas(p1))+#0;
  tmpStr2:=PathConv(strpas(p2))+#0;
  checkCTRLC;
  if not (dosRename(@tmpStr1,@tmpStr2) <> 0) then
    dosError2InOut(IoErr);
end;

function do_write(h: longint; addr: pointer; len: longint) : longint;
var dosResult: LongInt;
begin
  checkCTRLC;
  do_write:=0;
  if (len<=0) or (h<=0) then exit;

{$IFDEF MOSFPC_FILEDEBUG}
  if not ((h=StdOutputHandle) or (h=StdInputHandle) or
     (h=StdErrorHandle)) then CheckInList(AOS_fileList,h);
{$ENDIF}

  dosResult:=dosWrite(h,addr,len);
  if dosResult<0 then begin
    dosError2InOut(IoErr);
  end else begin
    do_write:=dosResult;
  end;
end;

function do_read(h: longint; addr: pointer; len: longint) : longint;
var dosResult: LongInt;
begin
  checkCTRLC;
  do_read:=0;
  if (len<=0) or (h<=0) then exit;

{$IFDEF MOSFPC_FILEDEBUG}
  if not ((h=StdOutputHandle) or (h=StdInputHandle) or
     (h=StdErrorHandle)) then CheckInList(AOS_fileList,h);
{$ENDIF}

  dosResult:=dosRead(h,addr,len);
  if dosResult<0 then begin
    dosError2InOut(IoErr);
  end else begin
    do_read:=dosResult;
  end
end;

function do_filepos(handle: longint) : longint;
var dosResult: LongInt;
begin
  checkCTRLC;
  do_filepos:=-1;
  if CheckInList(AOS_fileList,handle)<>nil then begin

    { Seeking zero from OFFSET_CURRENT to find out where we are }    
    dosResult:=dosSeek(handle,0,OFFSET_CURRENT);
    if dosResult<0 then begin
      dosError2InOut(IoErr);
    end else begin
      do_filepos:=dosResult;
    end;
  end;
end;

procedure do_seek(handle, pos: longint);
begin
  checkCTRLC;
  if CheckInList(AOS_fileList,handle)<>nil then begin

    { Seeking from OFFSET_BEGINNING }  
    if dosSeek(handle,pos,OFFSET_BEGINNING)<0 then
      dosError2InOut(IoErr);
  end;
end;

function do_seekend(handle: longint):longint;
var dosResult: LongInt;
begin
  checkCTRLC;
  do_seekend:=-1;
  if CheckInList(AOS_fileList,handle)<>nil then begin

    { Seeking to OFFSET_END }
    dosResult:=dosSeek(handle,0,OFFSET_END);
    if dosResult<0 then begin
      dosError2InOut(IoErr);
    end else begin
      do_seekend:=dosResult;
    end;
  end;
end;

function do_filesize(handle : longint) : longint;
var currfilepos: longint;
begin
  checkCTRLC;
  do_filesize:=-1;
  if CheckInList(AOS_fileList,handle)<>nil then begin

    currfilepos:=do_filepos(handle);
    { We have to do this twice, because seek returns the OLD position }
    do_filesize:=do_seekend(handle);
    do_filesize:=do_seekend(handle);
    do_seek(handle,currfilepos);

  end;
end;

{ truncate at a given position }
procedure do_truncate(handle, pos: longint);
begin
  checkCTRLC;
  if CheckInList(AOS_fileList,handle)<>nil then begin

    { Seeking from OFFSET_BEGINNING }
    if SetFileSize(handle,pos,OFFSET_BEGINNING)<0 then
      dosError2InOut(IoErr);
  end;
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
  handle   : LongInt;
  openflags: LongInt;
  tmpStr   : array[0..255] of Char;
begin
  tmpStr:=PathConv(strpas(p))+#0;

  { close first if opened }
  if ((flags and $10000)=0) then begin
    case filerec(f).mode of
      fminput,fmoutput,fminout : Do_Close(filerec(f).handle);
      fmclosed : ;
      else begin
        inoutres:=102; {not assigned}
        exit;
      end;
    end;
  end;

  { reset file handle }
  filerec(f).handle:=UnusedHandle;

  { convert filemode to filerec modes }
  { READ/WRITE on existing file }
  { RESET/APPEND                }
  openflags:=MODE_OLDFILE;
  case (flags and 3) of
    0 : filerec(f).mode:=fminput;
    1 : filerec(f).mode:=fmoutput;
    2 : filerec(f).mode:=fminout;
  end;

  { rewrite (create a new file) }
  if (flags and $1000)<>0 then openflags:=MODE_NEWFILE;

  { empty name is special }
  if p[0]=#0 then begin
    case filerec(f).mode of
      fminput :
        filerec(f).handle:=StdInputHandle;
      fmappend,
      fmoutput : begin
        filerec(f).handle:=StdOutputHandle;
        filerec(f).mode:=fmoutput; {fool fmappend}
      end;
    end;
    exit;
  end;

  handle:=Open(@tmpStr,openflags);
  if handle=0 then begin
    dosError2InOut(IoErr);
  end else begin
    AddToList(AOS_fileList,handle);
    filerec(f).handle:=handle;
  end;

  { append mode }
  if ((Flags and $100)<>0) and
      (FileRec(F).Handle<>UnusedHandle) then begin
    do_seekend(filerec(f).handle);
    filerec(f).mode:=fmoutput; {fool fmappend}
  end;
end;

function do_isdevice(handle: longint): boolean;
begin
  if (handle=StdOutputHandle) or (handle=StdInputHandle) or
     (handle=StdErrorHandle) then
    do_isdevice:=True
  else
    do_isdevice:=False;
end;
