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
      verbose_files : boolean = true;

{$endif SYSTEMDEBUG}


{****************************************************************************
                        Low level File Routines
 ****************************************************************************}

procedure do_close(handle : thandle);
var
  regs: Registers;
begin
  if Handle <= 4 then
   exit;
  regs.A := 0;
  regs.B := Byte(handle);
  if handle < max_files then
    begin
{$ifdef SYSTEMDEBUG}
       if not openfiles[handle] then
          Writeln(stderr,'Trying to close file h=',handle,' marked as closed');
       if assigned(opennames[handle]) and free_closed_names then
         begin
            if verbose_files then
              Writeln(stderr,'file ',opennames[handle],' closed');

            sysfreememsize(opennames[handle],strlen(opennames[handle])+1);
            opennames[handle]:=nil;
         end;
{$endif SYSTEMDEBUG}
       openfiles[handle]:=false;
    end;
  regs.C := $45;
  MsxDos(regs);
  if regs.A <> 0 then
   begin
     GetInOutRes(regs.A);
{$ifdef SYSTEMDEBUG}
     if verbose_files then
       Writeln(stderr,'file close failed A = ',regs.A);
{$endif SYSTEMDEBUG}
   end;
end;


procedure do_erase(p : pchar; pchangeable: boolean);
var
  regs: Registers;
  oldp: pchar;
begin
  oldp := p;
  DoDirSeparators(p, pchangeable);
  regs.A := 0;
  regs.C := $4D;
  regs.DE := PtrUInt(p);
  MsxDos(regs);
  if regs.A <> 0 then
   GetInOutRes(regs.A);
  if p <> oldp then
    freemem(p);
end;


procedure do_rename(p1,p2 : pchar; p1changeable, p2changeable: boolean);
var
  regs: Registers;
  oldp1, oldp2: pchar;
begin
  oldp1 := p1;
  oldp2 := p2;
  DoDirSeparators(p1, p1changeable);
  DoDirSeparators(p2, p2changeable);
  regs.A := 0;
  regs.C := $4E;
  { ToDo: check for same directory? }
  regs.DE := PtrUInt(p1);
  regs.HL := PtrUInt(p2);
  MsxDos(regs);
  if regs.A <> 0 then
   GetInOutRes(regs.A);
  if p1 <> oldp1 then
    freemem(p1);
  if p2 <> oldp2 then
    freemem(p2);
end;


function do_write(h:thandle;addr:pointer;len : longint) : longint;
var
  regs: Registers;
begin
  regs.C := $49;
  regs.A := 0;
  regs.B := h;
  regs.DE := PtrUInt(addr);
  regs.HL := len;
  MsxDos(regs);
  if regs.A <> 0 then
  begin
    GetInOutRes(regs.A);
    exit(0);
  end;
  do_write := regs.HL;
end;


function do_read(h:thandle;addr:pointer;len : longint) : longint;
var
  regs: Registers;
begin
  regs.C := $48;
  regs.A := 0;
  regs.B := h;
  regs.DE := PtrUInt(addr);
  regs.HL := len;
  MsxDos(regs);
  if regs.A <> 0 then
  begin
    GetInOutRes(regs.A);
    exit(0);
  end;
  do_read := regs.HL;
end;


function do_filepos(handle : thandle) : longint;
var
  regs : Registers;
begin
  regs.C := $4A;
  regs.B := handle;
  regs.A := 1;
  regs.DE := 0;
  regs.HL := 0;
  MsxDos(regs);
  if regs.A <> 0 then
    begin
      GetInOutRes(regs.A);
      do_filepos := 0;
    end
  else
    do_filepos := (longint(regs.DE) shl 16) + regs.HL;
end;


procedure do_seek(handle:thandle;pos : longint);
var
  regs: Registers;
begin
  regs.C := $4A;
  regs.B := handle;
  regs.A := 0;
  regs.DE := pos shr 16;
  regs.HL := pos and $ffff;
  MsxDos(regs);
  if regs.A <> 0 then
    GetInOutRes(regs.A);
end;



function do_seekend(handle:thandle):longint;
var
  regs : Registers;
begin
  regs.C := $4A;
  regs.B := handle;
  regs.A := 2;
  regs.DE := 0;
  regs.HL := 0;
  MsxDos(regs);
  if regs.A <> 0 then
    begin
      GetInOutRes(regs.A);
      do_seekend := 0;
    end
  else
    do_seekend := (longint(regs.DE) shl 16) + regs.HL;
end;


function do_filesize(handle : thandle) : longint;
var
  aktfilepos : longint;
begin
  aktfilepos:=do_filepos(handle);
  do_filesize:=do_seekend(handle);
  do_seek(handle,aktfilepos);
end;


{ truncate at a given position }
procedure do_truncate (handle:thandle;pos:longint);
{var
  regs : Registers;}
begin
  GetInOutRes(153);
  {do_seek(handle,pos);
  regs.C:=??;
  regs.B:=handle;
  MsxDos(regs);
  if regs.A <> 0 then
   GetInOutRes(regs.A);}
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
  regs   : Registers;
  action : word;
  oldp : pchar;
begin
{$ifdef SYSTEMDEBUG}
  if verbose_files then
     Writeln(stderr,'do_open for file "',p,'" called');
{$endif SYSTEMDEBUG}
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
  regs.A:=0;
  case (flags and 3) of
   0 : begin
     filerec(f).mode:=fminput;
     { b1 -> no write }
     regs.A := 1;
   end;
   1 : begin
     filerec(f).mode:=fmoutput;
     { b2 -> no read }
     regs.A := 2;
   end;
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
  oldp:=p;
  DoDirSeparators(p,pchangeable);

  if (action and $00f0) <> 0 then
    regs.C := $44                     { Map to Create/Replace API }
  else
    regs.C := $43;                    { Map to Open_Existing API }

  regs.B := 0;

  MsxDos(regs);

  if regs.A <> 0 then
    begin
      FileRec(f).mode:=fmclosed;
      GetInOutRes(regs.A);
      if oldp<>p then
        freemem(p);
{$ifdef SYSTEMDEBUG}
      if verbose_files then
        Writeln(stderr,'MSXDOS INT open for file "',p,'" failed err=',regs.A);
{$endif SYSTEMDEBUG}
      exit;
    end
  else
    begin
      filerec(f).handle:=regs.B;
    end;
{$ifdef SYSTEMDEBUG}
  if verbose_files then
     Writeln(stderr,'MSXDOS INT open for file "',p,'" returned ',regs.B);
{$endif SYSTEMDEBUG}
  if regs.B<max_files then
    begin
{$ifdef SYSTEMDEBUG}
       if openfiles[regs.B] and
          assigned(opennames[regs.B]) then
         begin
            Writeln(stderr,'file ',opennames[regs.B],'(',regs.B,') not closed but handle reused!');
            sysfreememsize(opennames[regs.B],strlen(opennames[regs.B])+1);
         end;
{$endif SYSTEMDEBUG}
       openfiles[regs.B]:=true;
{$ifdef SYSTEMDEBUG}
       opennames[regs.B] := sysgetmem(strlen(p)+1);
       move(p^,opennames[regs.B]^,strlen(p)+1);
       if verbose_files then
         Writeln(stderr,'file ',opennames[regs.B],' opened');
{$endif SYSTEMDEBUG}
    end;
{ append mode }
  if ((flags and $100) <> 0) and
   (FileRec (F).Handle <> UnusedHandle) then
   begin
     do_seekend(filerec(f).handle);
     filerec(f).mode:=fmoutput; {fool fmappend}
   end;

  if oldp<>p then
    freemem(p);
end;


function do_isdevice(handle:THandle):boolean;
var
  regs: Registers;
begin
  regs.C := $4B;
  regs.B := handle;
  regs.A := $00;
  MsxDos(regs);
  do_isdevice := (regs.D and $80) <> 0;
  if regs.A <> 0 then
   GetInOutRes(regs.A);
end;

