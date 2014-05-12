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

procedure DosDir(func:byte;s: rawbytestring);
var
  regs   : Registers;
  len    : Integer;
begin
  DoDirSeparators(s);
  { True DOS does not like backslashes at end
    Win95 DOS accepts this !!
    but "\" and "c:\" should still be kept and accepted hopefully PM }
  len:=length(s);
  if (len>0) and (s[len]='\') and
     Not ((len=1) or ((len=3) and (s[2]=':'))) then
    s[len]:=#0;
  regs.DX:=Ofs(s[1]);
  regs.DS:=Seg(s[1]);
  if LFNSupport then
   regs.AX:=$7100+func
  else
   regs.AX:=func shl 8;
  MsDos(regs);
  if (regs.Flags and fCarry) <> 0 then
   GetInOutRes(regs.AX);
end;

Procedure do_MkDir(const s: rawbytestring);
begin
   DosDir($39,s);
end;

Procedure do_RmDir(const s: rawbytestring);
begin
  if s='.' then
    begin
      InOutRes:=16;
      exit;
    end;
  DosDir($3a,s);
end;

Procedure do_ChDir(const s: rawbytestring);
var
  regs : Registers;
  len  : Integer;
begin
  len:=Length(s);
{ First handle Drive changes }
  if (len>=2) and (s[2]=':') then
   begin
     regs.DX:=(ord(s[1]) and (not 32))-ord('A');
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
  DosDir($3b,s);
end;

procedure do_GetDir (DriveNr: byte; var Dir: RawByteString);
var
  temp : array[0..260] of char;
  i    : integer;
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
     SetCodePage (Dir,DefaultFileSystemCodePage,false);
     exit;
   end
  else
    temp[252] := #0;  { to avoid shortstring buffer overflow }
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
  SetCodePage (dir,DefaultFileSystemCodePage,false);
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
