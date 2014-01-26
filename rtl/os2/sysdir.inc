{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2014 by Florian Klaempfl
    and other members of the Free Pascal development team.

    FPC Pascal system unit for OS/2.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{*****************************************************************************
                           Directory Handling
*****************************************************************************}

Procedure do_MkDir(s: rawbytestring);
var 
    Rc : word;
begin
  DoDirSeparators(s);
  Rc := DosCreateDir(pchar(s),nil);
  if Rc <> 0 then
    begin
      InOutRes := Rc;
      Errno2Inoutres;
    end;
end;

Procedure do_RmDir(s: rawbytestring);
var 
    Rc : word;
begin
  if s = '.' then
    begin
      InOutRes := 16;
      exit;
    end;
  DoDirSeparators(s);
  Rc := DosDeleteDir(pchar(s));
  if Rc <> 0 then
    begin
      InOutRes := Rc;
      Errno2Inoutres;
    end;
end;

{$ASMMODE INTEL}

Procedure do_ChDir(s: rawbytestring);

var RC: cardinal;
    Len: Longint;

begin
  Len := Length (S);
  if (Len >= 2) and (S[2] = ':') then
  begin
    RC := DosSetDefaultDisk ((Ord (S [1]) and not ($20)) - $40);
    if RC <> 0 then
      InOutRes := RC
    else
      if Len > 2 then
      begin
        DoDirSeparators (s);
        if (S [Len] = DirectorySeparator) and (Len <> 3) then
         S [Len] := #0;
        RC := DosSetCurrentDir (pchar (s));
        if RC <> 0 then
        begin
          InOutRes := RC;
          Errno2InOutRes;
        end;
      end;
  end else begin
    DoDirSeparators (s);
    if (Len > 1) and (S [Len] = DirectorySeparator) then
     S [Len] := #0;
    RC := DosSetCurrentDir (pchar (s));
    if RC <> 0 then
    begin
      InOutRes:= RC;
      Errno2InOutRes;
    end;
  end;
end;

{$ASMMODE ATT}

procedure do_GetDir (DriveNr: byte; var Dir: RawByteString);
{Written by Michael Van Canneyt.}
var sof: Pchar;
    i:byte;
    l,l2:cardinal;
begin
    setlength(Dir,255);
    Dir [4] := #0;
    { Used in case the specified drive isn't available }
    sof:=pchar(@dir[4]);
    { dir[1..3] will contain '[drivenr]:\', but is not }
    { supplied by DOS, so we let dos string start at   }
    { dir[4]                                           }
    { Get dir from drivenr : 0=default, 1=A etc... }
    { TODO: if max path length is > 255, increase the setlength parameter above and
      the 255 below }
    l:=255-3;
    InOutRes:=longint (DosQueryCurrentDir(DriveNr, sof^, l));
{$WARNING Result code should be translated in some cases!}
    { Now Dir should be filled with directory in ASCIIZ, }
    { starting from dir[4]                               }
    dir[2]:=':';
    dir[3]:='\';
    i:=4;
    {Conversion to Pascal string }
    while (dir[i]<>#0) do
        begin
            { convert path name to DOS }
            if dir[i] in AllowDirectorySeparators then
              dir[i]:=DirectorySeparator;
            inc(i);
        end;
    setlength(dir,i-1);
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
    SetCodePage(dir,DefaultFileSystemCodePage,false);
    if not (FileNameCasePreserving) then dir:=upcase(dir);
end;



