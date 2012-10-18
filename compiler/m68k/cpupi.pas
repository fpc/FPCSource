{
    Copyright (c) 2002 by Florian Klaempfl

    This unit contains the CPU specific part of tprocinfo

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

{ This unit contains the CPU specific part of tprocinfo. }
unit cpupi;

{$i fpcdefs.inc}

  interface

    uses
      psub;

    type
      tm68kprocinfo = class(tcgprocinfo)
        procedure init_framepointer;override;
      end;

  implementation

    uses
      procinfo,
      cpubase,
      systems;

  { tm68kprocinfo }

    procedure tm68kprocinfo.init_framepointer;
      begin
        { ToDo : what about system_m68k_embedded? }
        if target_info.system in [system_m68k_linux,system_m68k_netbsd,system_m68k_openbsd] then
          begin
            RS_FRAME_POINTER_REG:=RS_A6;
            NR_FRAME_POINTER_REG:=NR_A6;
          end
        else
          begin
            NR_FRAME_POINTER_REG:=NR_A5;
            RS_FRAME_POINTER_REG:=RS_A5;
          end;
      end;

begin
   cprocinfo:=tm68kprocinfo;
end.
