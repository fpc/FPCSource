{
    Copyright (c) 2002 by Florian Klaempfl

    RiscV specific calling conventions

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
unit pararv;

{$I fpcdefs.inc}

  interface

    uses
      globtype,
      cgutils,
      paramgr;

    type
      trvparamanager = class(tparamanager)
        function get_volatile_registers_int(calloption: tproccalloption): tcpuregisterset; override;
        function get_volatile_registers_fpu(calloption: tproccalloption): tcpuregisterset; override;

       function get_saved_registers_fpu(calloption: tproccalloption): tcpuregisterarray;override;
       function get_saved_registers_int(calloption: tproccalloption): tcpuregisterarray;override;
      end;

implementation

    uses
      cpubase;

    function trvparamanager.get_volatile_registers_int(calloption: tproccalloption): tcpuregisterset;
      begin
        result:=[RS_X0..RS_X31]-[RS_X2,RS_X8..RS_X9,RS_X18..RS_X27];
      end;


    function trvparamanager.get_volatile_registers_fpu(calloption: tproccalloption): tcpuregisterset;
      begin
        result:=[RS_F0..RS_F31]-[RS_F8..RS_F9,RS_F18..RS_F27];
      end;


    function trvparamanager.get_saved_registers_int(calloption : tproccalloption):tcpuregisterarray;
      const
        saved_regs: tcpuregisterarray = (RS_X2,RS_X8,RS_X9,RS_X18,RS_X19,RS_X20,RS_X21,RS_X22,RS_X23,RS_X24,RS_X26,RS_X26,RS_X27);
      begin
        result:=saved_regs;
      end;


    function trvparamanager.get_saved_registers_fpu(calloption : tproccalloption):tcpuregisterarray;
      const
        saved_regs: tcpuregisterarray = (RS_F8,RS_F9,RS_F18,RS_F19,RS_F20,RS_F21,RS_F22,RS_F23,RS_F24,RS_F25,RS_F26,RS_F27);
      begin
        result:=saved_regs;
      end;

end.


