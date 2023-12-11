{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2023 by Free Pascal development team

    DOS API unit for Human 68k (Sharp X68000)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit h68kdos;

interface

{$i h68kdos.inc}

function h68kdos_rename(oldname: PChar; newname: PChar): longint;
function h68kdos_exfiles(filbuf: Ph68kdos_exfilbuf; name: pchar; atr: word): longint;
function h68kdos_exnfiles(filbuf: Ph68kdos_exfilbuf): longint;

implementation

function h68kdos_rename(oldname: PChar; newname: PChar): longint;
begin
  if hi(human68k_vernum) <= 2 then
    h68kdos_rename:=h68kdos_rename_v2(oldname,newname)
  else
    h68kdos_rename:=h68kdos_rename_v3(oldname,newname);
end;

function h68kdos_exfiles(filbuf: Ph68kdos_exfilbuf; name: pchar; atr: word): longint;
begin
  h68kdos_exfiles:=h68kdos_files(Ph68kdos_filbuf(ptruint(filbuf) or $80000000),name,atr);
end;

function h68kdos_exnfiles(filbuf: Ph68kdos_exfilbuf): longint;
begin
  h68kdos_exnfiles:=h68kdos_nfiles(Ph68kdos_filbuf(ptruint(filbuf) or $80000000));
end;

end.
