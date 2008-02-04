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

procedure MkDir (const S: string);[IOCHECK];
var buffer:array[0..255] of char;
    Rc : word;
begin
  If (s='') or (InOutRes <> 0) then
   exit;
      move(s[1],buffer,length(s));
      buffer[length(s)]:=#0;
      DoDirSeparators(Pchar(@buffer));
      Rc := DosCreateDir(buffer,nil);
      if Rc <> 0 then
       begin
         InOutRes := Rc;
         Errno2Inoutres;
       end;
end;


procedure rmdir(const s : string);[IOCHECK];
var buffer:array[0..255] of char;
    Rc : word;
begin
  if (s = '.' ) then
    InOutRes := 16;
  If (s='') or (InOutRes <> 0) then
   exit;
      move(s[1],buffer,length(s));
      buffer[length(s)]:=#0;
      DoDirSeparators(Pchar(@buffer));
      Rc := DosDeleteDir(buffer);
      if Rc <> 0 then
       begin
         InOutRes := Rc;
         Errno2Inoutres;
       end;
end;

{$ASMMODE INTEL}

procedure ChDir (const S: string);[IOCheck];

var RC: cardinal;
    Buffer: array [0..255] of char;

begin
  If (s='') or (InOutRes <> 0) then exit;
  if (Length (S) >= 2) and (S [2] = ':') then
  begin
    RC := DosSetDefaultDisk ((Ord (S [1]) and not ($20)) - $40);
    if RC <> 0 then
      InOutRes := RC
    else
      if Length (S) > 2 then
      begin
        Move (S [1], Buffer, Length (S));
        Buffer [Length (S)] := #0;
        DoDirSeparators (PChar (@Buffer));
        RC := DosSetCurrentDir (@Buffer);
        if RC <> 0 then
        begin
          InOutRes := RC;
          Errno2InOutRes;
        end;
      end;
  end else begin
    Move (S [1], Buffer, Length (S));
    Buffer [Length (S)] := #0;
    DoDirSeparators (PChar (@Buffer));
    RC := DosSetCurrentDir (@Buffer);
    if RC <> 0 then
    begin
      InOutRes:= RC;
      Errno2InOutRes;
    end;
  end;
end;

{$ASMMODE ATT}

procedure GetDir (DriveNr: byte; var Dir: ShortString);
{Written by Michael Van Canneyt.}
var sof: Pchar;
    i:byte;
    l,l2:cardinal;
begin
    Dir [4] := #0;
    { Used in case the specified drive isn't available }
    sof:=pchar(@dir[4]);
    { dir[1..3] will contain '[drivenr]:\', but is not }
    { supplied by DOS, so we let dos string start at   }
    { dir[4]                                           }
    { Get dir from drivenr : 0=default, 1=A etc... }
    l:=255-3;
    InOutRes:=longint (DosQueryCurrentDir(DriveNr, sof^, l));
{$WARNING Result code should be translated in some cases!}
    { Now Dir should be filled with directory in ASCIIZ, }
    { starting from dir[4]                               }
    dir[0]:=#3;
    dir[2]:=':';
    dir[3]:='\';
    i:=4;
    {Conversion to Pascal string }
    while (dir[i]<>#0) do
        begin
            { convert path name to DOS }
            if dir[i] in AllowDirectorySeparators then
              dir[i]:=DirectorySeparator;
            dir[0]:=char(i);
            inc(i);
        end;
    { upcase the string (FPC function) }
    if drivenr<>0 then   { Drive was supplied. We know it }
        dir[1]:=chr(64+drivenr)
    else
        begin
            { We need to get the current drive from DOS function 19H  }
            { because the drive was the default, which can be unknown }
            DosQueryCurrentDisk(l, l2);
            dir[1]:=chr(64+l);
        end;
    if not (FileNameCaseSensitive) then dir:=upcase(dir);
end;



