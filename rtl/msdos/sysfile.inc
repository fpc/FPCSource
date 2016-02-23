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

      { we cannot allocate memory using SysGetMem, as we use tinyheap code,
        but tinyheap.inc source in inserted later, so we need a forward declaration }
   function SysTinyFreeMemSize(Addr: Pointer; Size: Ptruint): ptruint; forward;
   function SysTinyGetMem(Size: ptruint): pointer; forward;
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
{$ifdef SYSTEMDEBUG}
       if not openfiles[handle] then
          Writeln(stderr,'Trying to close file h=',handle,' marked as closed');
       if assigned(opennames[handle]) and free_closed_names then
         begin
            if verbose_files then
              Writeln(stderr,'file ',opennames[handle],' closed');

            systinyfreememsize(opennames[handle],strlen(opennames[handle])+1);
            opennames[handle]:=nil;
         end;
{$endif SYSTEMDEBUG}
       openfiles[handle]:=false;
    end;
  regs.AX:=$3e00;
  MsDos(regs);
  if (regs.Flags and fCarry) <> 0 then
   begin
     GetInOutRes(regs.AX);
{$ifdef SYSTEMDEBUG}
     if verbose_files then
       Writeln(stderr,'file close failed AX= ',regs.AX);
{$endif SYSTEMDEBUG}
   end;
end;


procedure do_erase(p : pchar; pchangeable: boolean);
var
  regs : Registers;
  oldp : pchar;
begin
  oldp:=p;
  DoDirSeparators(p,pchangeable);
  regs.DX:=Ofs(p^);
  regs.DS:=Seg(p^);
{$ifndef RTLLITE}
  if LFNSupport then
   regs.AX:=$7141
  else
{$endif ndef RTLLITE}
   regs.AX:=$4100;
  regs.SI:=0;
  regs.CX:=0;
  MsDos(regs);
  if (regs.Flags and fCarry) <> 0 then
   GetInOutRes(regs.AX);
  if p<>oldp then
    freemem(p);
end;


procedure do_rename(p1,p2 : pchar; p1changeable, p2changeable: boolean);
var
  regs : Registers;
  oldp1, oldp2 : pchar;
begin
  oldp1:=p1;
  oldp2:=p2;
  DoDirSeparators(p1,p1changeable);
  DoDirSeparators(p2,p2changeable);
  regs.DS:=Seg(p1^);
  regs.DX:=Ofs(p1^);
  regs.ES:=Seg(p2^);
  regs.DI:=Ofs(p2^);
{$ifndef RTLLITE}
  if LFNSupport then
   regs.AX:=$7156
  else
{$endif ndef RTLLITE}
   regs.AX:=$5600;
  regs.CX:=$ff;            { attribute problem here ! }
  MsDos(regs);
  if (regs.Flags and fCarry) <> 0 then
   GetInOutRes(regs.AX);
  if p1<>oldp1 then
    freemem(p1);
  if p2<>oldp2 then
    freemem(p2);
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
  if (regs.Flags and fCarry) <> 0 then
  begin
    GetInOutRes(regs.AX);
    exit(0);
  end;
  do_read := regs.AX;
end;


function do_filepos(handle : thandle) : longint;
var
  regs : Registers;
begin
  regs.BX:=handle;
  regs.CX:=0;
  regs.DX:=0;
  regs.AX:=$4201;
  MsDos(regs);
  if (regs.Flags and fCarry) <> 0 then
   Begin
     GetInOutRes(regs.AX);
     do_filepos:=0;
   end
  else
   do_filepos:=(longint(regs.DX) shl 16) + regs.AX;
end;


procedure do_seek(handle:thandle;pos : longint);
var
  regs : Registers;
begin
  regs.BX:=handle;
  regs.CX:=pos shr 16;
  regs.DX:=pos and $ffff;
  regs.AX:=$4200;
  MsDos(regs);
  if (regs.Flags and fCarry) <> 0 then
   GetInOutRes(regs.AX);
end;



function do_seekend(handle:thandle):longint;
var
  regs : Registers;
begin
  regs.BX:=handle;
  regs.CX:=0;
  regs.DX:=0;
  regs.AX:=$4202;
  MsDos(regs);
  if (regs.Flags and fCarry) <> 0 then
   Begin
     GetInOutRes(regs.AX);
     do_seekend:=0;
   end
  else
   do_seekend:=(longint(regs.DX) shl 16) + regs.AX;
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
var
  regs : Registers;
begin
  do_seek(handle,pos);
  regs.CX:=0;
  regs.BX:=handle;
  regs.AX:=$4000;
  MsDos(regs);
  if (regs.Flags and fCarry) <> 0 then
   GetInOutRes(regs.AX);
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
  oldp:=p;
  DoDirSeparators(p,pchangeable);
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
      FileRec(f).mode:=fmclosed;
      GetInOutRes(regs.AX);
      if oldp<>p then
        freemem(p);
{$ifdef SYSTEMDEBUG}
      if verbose_files then
        Writeln(stderr,'MSDOS INT open for file "',p,'" failed err=',regs.AX);
{$endif SYSTEMDEBUG}
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
{$ifdef SYSTEMDEBUG}
  if verbose_files then
     Writeln(stderr,'MSDOS INT open for file "',p,'" returned ',regs.AX);
{$endif SYSTEMDEBUG}
  if regs.AX<max_files then
    begin
{$ifdef SYSTEMDEBUG}
       if openfiles[regs.AX] and
          assigned(opennames[regs.AX]) then
         begin
            Writeln(stderr,'file ',opennames[regs.AX],'(',regs.AX,') not closed but handle reused!');
            systinyfreememsize(opennames[regs.AX],strlen(opennames[regs.AX])+1);
         end;
{$endif SYSTEMDEBUG}
       openfiles[regs.AX]:=true;
{$ifdef SYSTEMDEBUG}
       opennames[regs.AX] := systinygetmem(strlen(p)+1);
       move(p^,opennames[regs.AX]^,strlen(p)+1);
       if verbose_files then
         Writeln(stderr,'file ',opennames[regs.AX],' opened');
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
  regs.AX := $4400;
  regs.BX := handle;
  MsDos(regs);
  do_isdevice := (regs.DL and $80) <> 0;
  if (regs.Flags and fCarry) <> 0 then
   GetInOutRes(regs.AX);
end;
