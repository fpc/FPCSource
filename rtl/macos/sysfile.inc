{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2001-2005 by Free Pascal development team

    Low level file functions for MacOS

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{*****************************************************************************
                          Low Level File Routines
 ****************************************************************************}

function do_isdevice(handle:longint):boolean;
begin
  do_isdevice:= (handle=StdInputHandle) or
                (handle=StdOutputHandle) or
                (handle=StdErrorHandle);
end;

{ close a file from the handle value }
procedure do_close(h : longint);
var
  err: OSErr;
{Ignore error handling, according to the other targets, which seems reasonable,
because close might be used to clean up after an error.}
begin
  {$ifdef MACOS_USE_STDCLIB}
  c_close(h);
  errno:= 0;
  {$else}
  err:= FSClose(h);
  // OSErr2InOutRes(err);
  {$endif}
end;

procedure do_erase(p : pchar);

var
  spec: FSSpec;
  err: OSErr;
  res: Integer;

begin
  res:= PathArgToFSSpec(p, spec);
  if (res = 0) then
    begin
      if not IsDirectory(spec) then
        begin
          err:= FSpDelete(spec);
          OSErr2InOutRes(err);
        end
      else
        InOutRes:= 2;
    end
  else
    InOutRes:=res;
end;

procedure do_rename(p1,p2 : pchar);
var
  s1,s2: AnsiString;
begin
  {$ifdef MACOS_USE_STDCLIB}
  InOutRes:= PathArgToFullPath(p1, s1);
  if InOutRes <> 0 then
    exit;
  InOutRes:= PathArgToFullPath(p2, s2);
  if InOutRes <> 0 then
    exit;
  c_rename(PChar(s1),PChar(s2));
  Errno2InoutRes;
  {$else}
  InOutRes:=1;
  {$endif}
end;

function do_write(h:longint;addr:pointer;len : longint) : longint;
begin
  {$ifdef MACOS_USE_STDCLIB}
  do_write:= c_write(h, addr, len);
  Errno2InoutRes;
  {$else}
  InOutRes:=1;
  if FSWrite(h, len, Mac_Ptr(addr)) = noErr then
    InOutRes:=0;
  do_write:= len;
  {$endif}
end;

function do_read(h:longint;addr:pointer;len : longint) : longint;

var
  i: Longint;

begin
  {$ifdef MACOS_USE_STDCLIB}
  len:= c_read(h, addr, len);
  Errno2InoutRes;

  do_read:= len;

  {$else}
  InOutRes:=1;
  if FSread(h, len, Mac_Ptr(addr)) = noErr then
    InOutRes:=0;
  do_read:= len;
  {$endif}
end;

function do_filepos(handle : longint) : longint;

var
  pos: Longint;

begin
  {$ifdef MACOS_USE_STDCLIB}
  {This returns the filepos without moving it.}
  do_filepos := lseek(handle, 0, SEEK_CUR);
  Errno2InoutRes;
  {$else}
  InOutRes:=1;
  if GetFPos(handle, pos) = noErr then
    InOutRes:=0;
  do_filepos:= pos;
  {$endif}
end;

procedure do_seek(handle,pos : longint);
begin
  {$ifdef MACOS_USE_STDCLIB}
  lseek(handle, pos, SEEK_SET);
  Errno2InoutRes;
  {$else}
  InOutRes:=1;
  if SetFPos(handle, fsFromStart, pos) = noErr then
    InOutRes:=0;
  {$endif}
end;

function do_seekend(handle:longint):longint;
begin
  {$ifdef MACOS_USE_STDCLIB}
  do_seekend:= lseek(handle, 0, SEEK_END);
  Errno2InoutRes;
  {$else}
  InOutRes:=1;
  if SetFPos(handle, fsFromLEOF, 0) = noErr then
    InOutRes:=0;
  {TODO Resulting file position is to be returned.}
  {$endif}
end;

function do_filesize(handle : longint) : longint;

var
  aktfilepos: Longint;

begin
  {$ifdef MACOS_USE_STDCLIB}
  aktfilepos:= lseek(handle, 0, SEEK_CUR);
  if errno = 0 then
    begin
      do_filesize := lseek(handle, 0, SEEK_END);
      Errno2InOutRes; {Report the error from this operation.}
      lseek(handle, aktfilepos, SEEK_SET);   {Always try to move back,
         even in presence of error.}
    end
  else
    Errno2InOutRes;
  {$else}
  InOutRes:=1;
  if GetEOF(handle, pos) = noErr then
    InOutRes:=0;
  do_filesize:= pos;
  {$endif}
end;

{ truncate at a given position }
procedure do_truncate (handle,pos:longint);
begin
  {$ifdef MACOS_USE_STDCLIB}
  ioctl(handle, FIOSETEOF, pointer(pos));
  Errno2InoutRes;
  {$else}
  InOutRes:=1;
  do_seek(handle,pos);  //TODO: Is this needed (Does the user anticipate the filemarker is at the end?)
  if SetEOF(handle, pos) = noErr then
    InOutRes:=0;
  {$endif}
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
  scriptTag: ScriptCode;
  refNum: Integer;

  err: OSErr;
  res: Integer;
  spec: FSSpec;

  fh: Longint;

  oflags : longint;
  fullPath: AnsiString;

  finderInfo: FInfo;

begin

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

  {$ifdef MACOS_USE_STDCLIB}

{ We do the conversion of filemodes here, concentrated on 1 place }
  case (flags and 3) of
   0 : begin
         oflags :=O_RDONLY;
         filerec(f).mode:=fminput;
       end;
   1 : begin
         oflags :=O_WRONLY;
         filerec(f).mode:=fmoutput;
       end;
   2 : begin
         oflags :=O_RDWR;
         filerec(f).mode:=fminout;
       end;
  end;

  if (flags and $1000)=$1000 then
    oflags:=oflags or (O_CREAT or O_TRUNC)
  else if (flags and $100)=$100 then
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
   end
  else
    begin
      InOutRes:= PathArgToFSSpec(p, spec);
      if (InOutRes = 0) or (InOutRes = 2) then
        begin
          err:= FSpGetFullPath(spec, fullPath, false);
          InOutRes:= MacOSErr2RTEerr(err);
        end;
      if InOutRes <> 0 then
        exit;

      p:= PChar(fullPath);
    end;


  fh:= c_open(p, oflags);
  if (fh = -1) and (errno = Sys_EROFS) and ((oflags and O_RDWR)<>0) then
    begin
      oflags:=oflags and not(O_RDWR);
      fh:= c_open(p, oflags);
    end;
  Errno2InOutRes;
  if fh <> -1 then
    begin
      if FileRec(f).mode in [fmoutput, fminout, fmappend] then
        begin
          {Change of filetype and creator is always done when a file is opened
          for some kind of writing. This ensures overwritten Darwin files will
          get apropriate filetype. It must be done after file is opened,
          in the case the file did not previously exist.}

          FSpGetFInfo(spec, finderInfo);
          finderInfo.fdType:= defaultFileType;
          finderInfo.fdCreator:= defaultCreator;
          FSpSetFInfo(spec, finderInfo);
        end;
      filerec(f).handle:= fh;
    end
  else
    filerec(f).handle:= UnusedHandle;

  {$else}

  InOutRes:=1;

  { reset file handle }
  filerec(f).handle:=UnusedHandle;

  res:= FSpLocationFromFullPath(StrLen(p), p, spec);
  if (res = noErr) or (res = fnfErr) then
    begin
      if FSpCreate(spec, defaultCreator, defaultFileType, smSystemScript) = noErr then
        ;

      if FSpOpenDF(spec, fsCurPerm, refNum) = noErr then
        begin
          filerec(f).handle:= refNum;
          InOutRes:=0;
        end;
    end;

  if (filerec(f).handle=UnusedHandle) then
    begin
      //errno:=GetLastError;
      //Errno2InoutRes;
    end;
  {$endif}
end;



