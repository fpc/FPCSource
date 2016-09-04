{
    Copyright (c) 2016 by Karoly Balogh

    Contains information on syscalls

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}
unit syscinfo;

{$i fpcdefs.inc}

interface

uses
  globtype, systems, tokens, symconst;

type
  tsyscallinfo = record
    token: ttoken;
    procoption: tprocoption;
    validon: set of tsystem;
  end;
  psyscallinfo = ^tsyscallinfo;

const
  syscall_conventions: array[1..7] of tsyscallinfo = (
      ( token: _LEGACY;    procoption: po_syscall_legacy;    validon: [system_powerpc_morphos,system_m68k_amiga] ),
      ( token: _SYSV;      procoption: po_syscall_sysv;      validon: [system_powerpc_morphos] ),
      ( token: _SYSVBASE;  procoption: po_syscall_sysvbase;  validon: [system_powerpc_morphos] ),
      ( token: _BASESYSV;  procoption: po_syscall_basesysv;  validon: [system_powerpc_morphos,system_powerpc_amiga] ),
      ( token: _R12BASE;   procoption: po_syscall_r12base;   validon: [system_powerpc_morphos] ),
      ( token: _STACKBASE; procoption: po_syscall_stackbase; validon: [system_i386_aros,system_x86_64_aros] ),
      ( token: _EAXBASE;   procoption: po_syscall_eaxbase;   validon: [system_i386_aros,system_x86_64_aros] ));


function get_syscall_by_token(const token: ttoken): psyscallinfo;
function get_syscall_by_name(const name: string): psyscallinfo;
function get_default_syscall: tprocoption;
procedure set_default_syscall(sc: tprocoption);

implementation

uses
  verbose;

const
  syscall_conventions_po = [ po_syscall_legacy, po_syscall_sysv, po_syscall_sysvbase, po_syscall_basesysv,
                             po_syscall_r12base, po_syscall_stackbase, po_syscall_eaxbase ];

type
  tsyscalldefaultinfo = record
    system: tsystem;
    procoption: tprocoption;
  end;

const
  default_syscall_conventions: array[0..4] of tsyscalldefaultinfo = (
      ( system: system_m68k_amiga;      procoption: po_syscall_legacy ),
      ( system: system_powerpc_amiga;   procoption: po_syscall_basesysv ),
      ( system: system_powerpc_morphos; procoption: po_syscall_legacy ),
      ( system: system_i386_aros;       procoption: po_syscall_stackbase ),
      ( system: system_x86_64_aros;     procoption: po_syscall_stackbase ));

var
  default_syscall_convention: tprocoption = po_none;

function get_syscall_by_token(const token: ttoken): psyscallinfo;
var
  i: longint;
begin
  result:=nil;
  for i:=low(syscall_conventions) to high(syscall_conventions) do
    if syscall_conventions[i].token = token then
      begin
        result:=@syscall_conventions[i];
        break;
      end;
end;

function get_syscall_by_name(const name: string): psyscallinfo;
var
  i: longint;
begin
  result:=nil;
  for i:=low(syscall_conventions) to high(syscall_conventions) do
    if arraytokeninfo[syscall_conventions[i].token].str = name then
      begin
        result:=@syscall_conventions[i];
        break;
      end;
end;

function get_default_syscall: tprocoption;
var
  i: longint;
begin
  if not (default_syscall_convention in syscall_conventions_po) then
    begin
      for i:=low(default_syscall_conventions) to high(default_syscall_conventions) do
        if default_syscall_conventions[i].system = target_info.system then
          default_syscall_convention:=default_syscall_conventions[i].procoption;
      if not (default_syscall_convention in syscall_conventions_po) then
        internalerror(2016090302);
    end;

  result:=default_syscall_convention;
end;

procedure set_default_syscall(sc: tprocoption);
begin
  if not (sc in syscall_conventions_po) then
    internalerror(2016090301);

  default_syscall_convention:=sc;
end;

end.
