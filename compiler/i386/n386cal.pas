{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate i386 assembler for in call nodes

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
unit n386cal;

{$i fpcdefs.inc}

interface

{ $define AnsiStrRef}

    uses
      ncgcal;

    type
       ti386callnode = class(tcgcallnode)
       protected
          procedure pop_parasize(pop_size:longint);override;
          procedure extra_interrupt_code;override;
       end;


implementation

    uses
      globtype,systems,
      cutils,verbose,globals,
{$ifdef GDB}
      gdb,
{$endif GDB}
      cgbase,
      cpubase,paramgr,
      aasmtai,aasmcpu,
      ncal,nbas,nmem,nld,ncnv,
      cga,cgobj,cpuinfo;


{*****************************************************************************
                             TI386CALLNODE
*****************************************************************************}


    procedure ti386callnode.extra_interrupt_code;
      begin
        emit_none(A_PUSHF,S_L);
        emit_reg(A_PUSH,S_L,NR_CS);
      end;


    procedure ti386callnode.pop_parasize(pop_size:longint);
      var
        hreg : tregister;
      begin
        { better than an add on all processors }
        if pop_size=4 then
          begin
            hreg:=cg.getintregister(exprasmlist,OS_INT);
            exprasmlist.concat(taicpu.op_reg(A_POP,S_L,hreg));
          end
        { the pentium has two pipes and pop reg is pairable }
        { but the registers must be different!        }
        else
          if (pop_size=8) and
             not(cs_littlesize in aktglobalswitches) and
             (aktoptprocessor=ClassPentium) then
            begin
               hreg:=cg.getintregister(exprasmlist,OS_INT);
               exprasmlist.concat(taicpu.op_reg(A_POP,S_L,hreg));
               hreg:=cg.getintregister(exprasmlist,OS_INT);
               exprasmlist.concat(taicpu.op_reg(A_POP,S_L,hreg));
            end
        else
          if pop_size<>0 then
            exprasmlist.concat(taicpu.op_const_reg(A_ADD,S_L,pop_size,NR_ESP));
      end;


begin
   ccallnode:=ti386callnode;
end.
{
  $Log$
  Revision 1.102  2004-09-25 14:23:54  peter
    * ungetregister is now only used for cpuregisters, renamed to
      ungetcpuregister
    * renamed (get|unget)explicitregister(s) to ..cpuregister
    * removed location-release/reference_release

  Revision 1.101  2004/06/20 08:55:31  florian
    * logs truncated

  Revision 1.100  2004/06/16 20:07:10  florian
    * dwarf branch merged

  Revision 1.99.2.1  2004/05/02 12:45:32  peter
    * enabled cpuhasfixedstack for x86-64 again
    * fixed size of temp allocation for parameters

}
