{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2023 by Free Pascal development team

    Low level file functions for Human 68k (Sharp X68000)

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
procedure do_close(handle : longint);
var
  dosResult: longint;
begin
  dosResult:=h68kdos_close(handle);
  if dosResult < 0 then
    Error2InOutRes(dosResult);
end;


procedure do_erase(p : pchar; pchangeable: boolean);
var
  oldp: PAnsiChar;
  dosResult: longint;
begin
  oldp:=p;
  DoDirSeparators(p,pchangeable);
  dosResult:=h68kdos_delete(p);
  if dosResult <0 then
    Error2InOutRes(dosResult);
  if oldp<>p then
    FreeMem(p);
end;


procedure do_rename(p1,p2 : pchar; p1changeable, p2changeable: boolean);
var
  oldp1, oldp2 : PAnsiChar;
  dosResult: longint;
begin
  oldp1:=p1;
  oldp2:=p2;
  DoDirSeparators(p1,p1changeable);
  DoDirSeparators(p2,p2changeable);

  if hi(human68k_vernum) <= 2 then
    dosResult:=h68kdos_rename_v2(p1,p2)
  else
    dosResult:=h68kdos_rename_v3(p1,p2);

  if dosResult < 0 then
    Error2InOutRes(dosResult);

  if oldp1<>p1 then
    FreeMem(p1);
  if oldp2<>p2 then
    FreeMem(p2);
end;


function do_write(h: longint; addr: pointer; len: longint) : longint;
var
  dosResult: longint;
begin
  do_write:=0;
  if (len<=0) or (h=-1) then
    exit;

  dosResult:=h68kdos_write(h, addr, len);
  if dosResult < 0 then
    begin
      Error2InOutRes(dosResult);
    end
  else
    do_write:=dosResult;
end;


function do_read(h: longint; addr: pointer; len: longint) : longint;
var
  dosResult: longint;
begin
  do_read:=0;
  if (len<=0) or (h=-1) then exit;

  dosResult:=h68kdos_read(h, addr, len);
  if dosResult<0 then
    begin
      Error2InOutRes(dosResult);
    end
  else
    do_read:=dosResult;
end;


function do_filepos(handle: longint) : longint;
var
  dosResult: longint;
begin
  do_filepos:=-1;
  dosResult:=h68kdos_seek(handle, 0, SEEK_FROM_CURRENT);
  if dosResult < 0 then
    begin
      Error2InOutRes(dosResult);
    end
  else
    do_filepos:=dosResult;
end;


procedure do_seek(handle, pos: longint);
var
  dosResult: longint;
begin
  dosResult:=h68kdos_seek(handle, pos, SEEK_FROM_START);
  if dosResult < 0 then
    Error2InOutRes(dosResult);
end;


function do_seekend(handle: longint):longint;
var
  dosResult: longint;
begin
  do_seekend:=-1;

  dosResult:=h68kdos_seek(handle, 0, SEEK_FROM_END);
  if dosResult < 0 then
    begin
      Error2InOutRes(dosResult);
    end
  else
    do_seekend:=dosResult;
end;


function do_filesize(handle : THandle) : longint;
var
  currfilepos: longint;
begin
  do_filesize:=-1;
  currfilepos:=do_filepos(handle);
  if currfilepos >= 0 then
    begin
      do_filesize:=do_seekend(handle);
    end;
  do_seek(handle,currfilepos);
end;


{ truncate at a given position }
procedure do_truncate(handle, pos: longint);
begin
end;


procedure do_open(var f;p:pchar;flags:longint; pchangeable: boolean);
{
  filerec and textrec have both handle and mode as the first items so
  they could use the same routine for opening/creating.
  when (flags and $100)   the file will be append
  when (flags and $1000)  the file will be truncate/rewritten
  when (flags and $10000) there is no check for close (needed for textfiles)
}
var
  oldp     : PAnsiChar;
  dosResult: longint;
begin
{ close first if opened }
  if ((flags and $10000)=0) then
   begin
     case filerec(f).mode of
       fmInput, fmOutput, fmInout:
         do_close(filerec(f).handle);
       fmClosed: ;
     else
       begin
         InOutRes:=102; {not assigned}
         exit;
       end;
     end;
   end;

{ reset file handle }
  filerec(f).handle:=UnusedHandle;

  { convert filemode to filerec modes }
  case (flags and 3) of
    0 : filerec(f).mode:=fmInput;
    1 : filerec(f).mode:=fmOutput;
    2 : filerec(f).mode:=fmInout;
  end;

  { empty name is special }
  if p[0]=#0 then begin
    case filerec(f).mode of
      fminput :
        filerec(f).handle:=StdInputHandle;
      fmappend,
      fmoutput : begin
        filerec(f).handle:=StdOutputHandle;
        filerec(f).mode:=fmOutput; {fool fmappend}
      end;
    end;
    exit;
  end;

  oldp:=p;
  DoDirSeparators(p);

  { rewrite (create a new file) }
  if (flags and $1000)<>0 then
    dosResult:=h68kdos_create(p,0)
  else
    dosResult:=h68kdos_open(p,flags and 3);

  if oldp<>p then
    freemem(p);

  if dosResult < 0 then
    begin
      Error2InOutRes(dosResult);
      filerec(f).mode:=fmClosed;
      exit;
    end
  else
    filerec(f).handle:=word(dosResult);

  { append mode }
  if ((Flags and $100)<>0) and
      (FileRec(F).Handle<>UnusedHandle) then begin
    do_seekend(filerec(f).handle);
    filerec(f).mode:=fmOutput; {fool fmappend}
  end;
end;


function do_isdevice(handle: thandle): boolean;
begin
  { FIX ME! }

  { Prefer to return true here as a default answer, as it is less harmful
    than false. This basically determines if the file handle is a "device",
    for example the console. Returning true here causes a flush before a
    read on the file handle which is preferred for consoleio, and a few
    other minor behavioral changes, for example shorter stacktraces.
    Returning false will cause weird behavior and unprinted lines when
    read() and write() is mixed during consoleio. }

  do_isdevice:=true;
end;
