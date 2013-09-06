{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Florian Klaempfl and Pavel Ozerski
    member of the Free Pascal development team.

    FPC Pascal system unit for go32v2.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{*****************************************************************************
                           Directory Handling
*****************************************************************************}

procedure DosDir(func:byte;s:rawbytestring);
var
  regs   : trealregs;
  len    : longint;
begin
  DoDirSeparators(s);
  { True DOS does not like backslashes at end
    Win95 DOS accepts this !!
    but "\" and "c:\" should still be kept and accepted hopefully PM }
  len:=length(s);
  if (len>0) and (s[len]='\') and
     Not ((len=1) or ((len=3) and (s[2]=':'))) then
    s[len]:=#0;
  syscopytodos(longint(pointer(s)),len+1);
  regs.realedx:=tb_offset;
  regs.realds:=tb_segment;
  if LFNSupport then
   regs.realeax:=$7100+func
  else
   regs.realeax:=func shl 8;
  sysrealintr($21,regs);
  if (regs.realflags and carryflag) <> 0 then
   GetInOutRes(lo(regs.realeax));
end;

Procedure do_MkDir(const s: rawbytestring);
begin
  DosDir($39,s);
end;

Procedure do_RmDir(const s: rawbytestring);
begin
  if s='.' then
    begin
      InOutRes := 16;
      exit;
    end;
  DosDir($3a,s);
end;

Procedure do_ChDir(const s: rawbytestring);
var
  regs : trealregs;
  len  : longint;
begin
  len:=length(s);
{ First handle Drive changes }
  if (len>=2) and (s[2]=':') then
   begin
     regs.realedx:=(ord(s[1]) and (not 32))-ord('A');
     regs.realeax:=$0e00;
     sysrealintr($21,regs);
     regs.realeax:=$1900;
     sysrealintr($21,regs);
     if byte(regs.realeax)<>byte(regs.realedx) then
      begin
        Inoutres:=15;
        exit;
      end;
     { DosDir($3b,'c:') give Path not found error on
       pure DOS PM }
     if len=2 then
       exit;
   end;
{ do the normal dos chdir }
  DosDir($3b,s);
end;

procedure do_GetDir (DriveNr: byte; var Dir: RawByteString);
var
  temp : array[0..255] of char;
  i    : longint;
  regs : trealregs;
begin
  regs.realedx:=drivenr;
  regs.realesi:=tb_offset;
  regs.realds:=tb_segment;
  if LFNSupport then
   regs.realeax:=$7147
  else
   regs.realeax:=$4700;
  sysrealintr($21,regs);
  if (regs.realflags and carryflag) <> 0 then
   Begin
     GetInOutRes (lo(regs.realeax));
     Dir := char (DriveNr + 64) + ':\';
     SetCodePage (Dir,DefaultFileSystemCodePage,false);
     exit;
   end
  else
   syscopyfromdos(longint(@temp),251);
{ conversion to Pascal string including slash conversion }
  i:=0;
  SetLength(dir,260);
  while (temp[i]<>#0) do
   begin
     if temp[i] in AllowDirectorySeparators then
       temp[i]:=DirectorySeparator;
     dir[i+4]:=temp[i];
     inc(i);
   end;
  dir[2]:=':';
  dir[3]:='\';
  SetLength(dir,i+3);
  SetCodePage(dir,DefaultFileSystemCodePage,false);
{ upcase the string }
  if not FileNameCasePreserving then
   dir:=upcase(dir);
  if drivenr<>0 then   { Drive was supplied. We know it }
   dir[1]:=char(65+drivenr-1)
  else
   begin
   { We need to get the current drive from DOS function 19H  }
   { because the drive was the default, which can be unknown }
     regs.realeax:=$1900;
     sysrealintr($21,regs);
     i:= (regs.realeax and $ff) + ord('A');
     dir[1]:=chr(i);
   end;
end;



