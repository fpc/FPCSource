{
    Copyright (c) 1998-2008 by Florian Klaempfl

    This unit implements the MOS Technology 6502 specific class for the register
    allocator

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

unit rgcpu;

{$i fpcdefs.inc}

  interface

     uses
       aasmbase,aasmtai,aasmdata,aasmcpu,aasmsym,
       cgbase,cgutils,
       cpubase,
       rgobj;

     type
       trgcpu = class(trgobj)
       end;

       trgintcpu = class(trgcpu)
         procedure add_cpu_interferences(p : tai);override;
       end;

  implementation

    uses
      verbose, cutils,
      cgobj,
      procinfo;


    procedure trgintcpu.add_cpu_interferences(p : tai);
      var
        r : tsuperregister;
      begin
        inherited;
        //if p.typ=ait_instruction then
        //  begin
        //    case taicpu(p).opcode of
        //      A_CPI,
        //      A_ANDI,
        //      A_ORI,
        //      A_SUBI,
        //      A_SBCI,
        //      A_LDI:
        //        for r:=RS_R0 to RS_R15 do
        //          add_edge(r,GetSupReg(taicpu(p).oper[0]^.reg));
        //      A_MULS:
        //        begin
        //          for r:=RS_R0 to RS_R15 do
        //            add_edge(r,GetSupReg(taicpu(p).oper[0]^.reg));
        //          for r:=RS_R0 to RS_R15 do
        //            add_edge(r,GetSupReg(taicpu(p).oper[1]^.reg));
        //        end;
        //    end;
        //  end;
      end;

end.
