{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Florian Klaempfl and Pavel Ozerski
    member of the Free Pascal development team.

    FPC Pascal system unit for the Win32 API.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{*****************************************************************************
                           Directory Handling
*****************************************************************************}

procedure DosDir(func:byte;s:pchar;len:integer);
var
  regs   : Registers;
begin
  DoDirSeparators(s);
  { True DOS does not like backslashes at end
    Win95 DOS accepts this !!
    but "\" and "c:\" should still be kept and accepted hopefully PM }
  if (len>0) and (s[len-1]='\') and
     Not ((len=1) or ((len=3) and (s[1]=':'))) then
    s[len-1]:=#0;
  regs.DX:=Ofs(s^);
  regs.DS:=Seg(s^);
  if LFNSupport then
   regs.AX:=$7100+func
  else
   regs.AX:=func shl 8;
  MsDos(regs);
  if (regs.Flags and fCarry) <> 0 then
   GetInOutRes(regs.AX);
end;

Procedure MkDir(s: pchar;len:sizeuint);[IOCheck, public, alias : 'FPC_SYS_MKDIR'];
begin
  If not assigned(s) or (len=0) or (InOutRes <> 0) then
    exit;
   DosDir($39,s,len);
end;

Procedure RmDir(s: pchar;len:sizeuint);[IOCheck, public, alias : 'FPC_SYS_RMDIR'];
begin
  if (len=1) and (s[0] = '.' ) then
    InOutRes := 16;
  If not assigned(s) or (len=0) or (InOutRes <> 0) then
   exit;
  DosDir($3a,s,len);
end;

Procedure ChDir(s: pchar;len:sizeuint);[IOCheck, public, alias : 'FPC_SYS_CHDIR'];
var
  regs : Registers;
begin
  If not assigned(s) or (len=0) or (InOutRes <> 0) then
   exit;
{ First handle Drive changes }
  if (len>=2) and (s[1]=':') then
   begin
     regs.DX:=(ord(s[0]) and (not 32))-ord('A');
     regs.AX:=$0e00;
     MsDos(regs);
     regs.AX:=$1900;
     MsDos(regs);
     if regs.AL<>regs.DL then
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
  DosDir($3b,s,len);
end;

procedure GetDir (DriveNr: byte; var Dir: ShortString);
var
  temp : array[0..260] of char;
  i    : longint;
  regs : Registers;
begin
  regs.DX:=drivenr;
  regs.SI:=Ofs(temp);
  regs.DS:=Seg(temp);
  if LFNSupport then
   regs.AX:=$7147
  else
   regs.AX:=$4700;
  MsDos(regs);
  if (regs.Flags and fCarry) <> 0 then
   Begin
     GetInOutRes (regs.AX);
     Dir := char (DriveNr + 64) + ':\';
     exit;
   end;
{ conversion to Pascal string including slash conversion }
  i:=0;
  while (temp[i]<>#0) do
   begin
     if temp[i] in AllowDirectorySeparators then
       temp[i]:=DirectorySeparator;
     dir[i+4]:=temp[i];
     inc(i);
   end;
  dir[2]:=':';
  dir[3]:='\';
  dir[0]:=char(i+3);
{ upcase the string }
  if not FileNameCasePreserving then
   dir:=upcase(dir);
  if drivenr<>0 then   { Drive was supplied. We know it }
   dir[1]:=char(65+drivenr-1)
  else
   begin
   { We need to get the current drive from DOS function 19H  }
   { because the drive was the default, which can be unknown }
     regs.AX:=$1900;
     MsDos(regs);
     i:= (regs.AX and $ff) + ord('A');
     dir[1]:=chr(i);
   end;
end;
