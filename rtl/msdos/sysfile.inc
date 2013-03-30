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

   { Keep Track of open files }
   const
      max_files = 50;
   var
      openfiles : array [0..max_files-1] of boolean;
{$ifdef SYSTEMDEBUG}
      opennames : array [0..max_files-1] of pchar;
   const
      free_closed_names : boolean = true;
{$endif SYSTEMDEBUG}


{****************************************************************************
                        Low level File Routines
 ****************************************************************************}

procedure do_close(handle : thandle);
var
  regs : Registers;
begin
  if Handle<=4 then
   exit;
  regs.BX:=handle;
  if handle<max_files then
    begin
       openfiles[handle]:=false;
{$ifdef SYSTEMDEBUG}
       if assigned(opennames[handle]) and free_closed_names then
         begin
            sysfreememsize(opennames[handle],strlen(opennames[handle])+1);
            opennames[handle]:=nil;
         end;
{$endif SYSTEMDEBUG}
    end;
  regs.AX:=$3e00;
  MsDos(regs);
  if (regs.Flags and fCarry) <> 0 then
   GetInOutRes(regs.AX);
end;


procedure do_erase(p : pchar);
begin
end;


procedure do_rename(p1,p2 : pchar);
begin
end;


function do_write(h:thandle;addr:pointer;len : longint) : longint;
var
  regs: Registers;
begin
  regs.AH := $40;
  regs.BX := h;
  regs.CX := len;
  regs.DS := Seg(addr^);
  regs.DX := Ofs(addr^);
  MsDos(regs);
  if (regs.Flags and fCarry) <> 0 then
  begin
    GetInOutRes(regs.AX);
    exit(0);
  end;
  do_write := regs.AX;
end;


function do_read(h:thandle;addr:pointer;len : longint) : longint;
var
  regs: Registers;
begin
  regs.AH := $3F;
  regs.BX := h;
  regs.CX := len;
  regs.DS := Seg(addr^);
  regs.DX := Ofs(addr^);
  MsDos(regs);
  if (regs.Flags and FCarry) <> 0 then
  begin
    GetInOutRes(regs.AX);
    exit(0);
  end;
  do_read := regs.AX;
end;


function do_filepos(handle : thandle) : longint;
begin
end;


procedure do_seek(handle:thandle;pos : longint);
begin
end;



function do_seekend(handle:thandle):longint;
begin
end;


function do_filesize(handle : thandle) : longint;
begin
end;


{ truncate at a given position }
procedure do_truncate (handle:thandle;pos:longint);
begin
end;

const
  FileHandleCount : word = 20;

function Increase_file_handle_count : boolean;
var
  regs : Registers;
begin
  Inc(FileHandleCount,10);
  regs.BX:=FileHandleCount;
  regs.AX:=$6700;
  MsDos(regs);
  if (regs.Flags and fCarry) <> 0 then
   begin
    Increase_file_handle_count:=false;
    Dec (FileHandleCount, 10);
   end
  else
    Increase_file_handle_count:=true;
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
  regs   : Registers;
  action : longint;
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
        inoutres:=102; {not assigned}
        exit;
      end;
     end;
   end;
{ reset file handle }
  filerec(f).handle:=UnusedHandle;
  action:=$1;
{ convert filemode to filerec modes }
  case (flags and 3) of
   0 : filerec(f).mode:=fminput;
   1 : filerec(f).mode:=fmoutput;
   2 : filerec(f).mode:=fminout;
  end;
  if (flags and $1000)<>0 then
   action:=$12; {create file function}
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
{$ifndef RTLLITE}
  if LFNSupport then
   begin
     regs.AX := $716c;                        { Use LFN Open/Create API }
     regs.DX := action;             { action if file does/doesn't exist }
     regs.SI := Ofs(p^);
     regs.BX := $2000 + (flags and $ff);               { file open mode }
   end
  else
{$endif RTLLITE}
   begin
     if (action and $00f0) <> 0 then
       regs.AX := $3c00                     { Map to Create/Replace API }
     else
       regs.AX := $3d00 + (flags and $ff);   { Map to Open_Existing API }
     regs.DX := Ofs(p^);
   end;
  regs.DS := Seg(p^);
  regs.CX := $20;                                     { file attributes }
  MsDos(regs);
{$ifndef RTLLITE}
  if (regs.Flags and fCarry) <> 0 then
    if regs.AX=4 then
      if Increase_file_handle_count then
        begin
          { Try again }
          if LFNSupport then
            begin
              regs.AX := $716c;                 {Use LFN Open/Create API}
              regs.DX := action;      {action if file does/doesn't exist}
              regs.SI := Ofs(p^);
              regs.BX := $2000 + (flags and $ff);        {file open mode}
            end
          else
            begin
              if (action and $00f0) <> 0 then
                regs.AX := $3c00              {Map to Create/Replace API}
              else
                regs.AX := $3d00 + (flags and $ff);     {Map to Open API}
              regs.DX := Ofs(p^);
            end;
          regs.DS := Seg(p^);
          regs.CX := $20;                               {file attributes}
          MsDos(regs);
        end;
{$endif RTLLITE}
  if (regs.Flags and fCarry) <> 0 then
    begin
      GetInOutRes(regs.AX);
      exit;
    end
  else
    begin
      filerec(f).handle:=regs.AX;
{$ifndef RTLLITE}
      { for systems that have more then 20 by default ! }
      if regs.AX>FileHandleCount then
        FileHandleCount:=regs.AX;
{$endif RTLLITE}
    end;
  if regs.AX<max_files then
    begin
{$ifdef SYSTEMDEBUG}
       if openfiles[regs.AX] and
          assigned(opennames[regs.AX]) then
         begin
            Writeln(stderr,'file ',opennames[regs.AX],'(',regs.AX,') not closed but handle reused!');
            sysfreememsize(opennames[regs.AX],strlen(opennames[regs.AX])+1);
         end;
{$endif SYSTEMDEBUG}
       openfiles[regs.AX]:=true;
{$ifdef SYSTEMDEBUG}
       opennames[regs.AX] := sysgetmem(strlen(p)+1);
       move(p^,opennames[regs.AX]^,strlen(p)+1);
{$endif SYSTEMDEBUG}
    end;
{ append mode }
  if ((flags and $100) <> 0) and
   (FileRec (F).Handle <> UnusedHandle) then
   begin
     do_seekend(filerec(f).handle);
     filerec(f).mode:=fmoutput; {fool fmappend}
   end;
end;


function do_isdevice(handle:THandle):boolean;
var
  regs: Registers;
begin
  regs.AX := $4400;
  regs.BX := handle;
  MsDos(regs);
  do_isdevice := (regs.DL and $80) <> 0;
  if (regs.Flags and fCarry) <> 0 then
   GetInOutRes(regs.AX);
end;
