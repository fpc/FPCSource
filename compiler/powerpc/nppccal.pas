{
    $Id$
    Copyright (c) 2002 by Florian Klaempfl

    Implements the PowerPC specific part of call nodes

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published bymethodpointer
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
unit nppccal;

{$i fpcdefs.inc}

interface

    uses
      symdef,node,ncal,ncgcal;

    type
       tppccallnode = class(tcgcallnode)
          function pass_1 : tnode;override;
          procedure push_framepointer;override;
       end;

implementation

    uses
      globtype,systems,
      cutils,verbose,globals,
      symconst,symbase,symsym,symtable,defutil,paramgr,
{$ifdef GDB}
  {$ifdef delphi}
      sysutils,
  {$else}
      strings,
  {$endif}
      gdb,
{$endif GDB}
      cginfo,cgbase,pass_2,
      cpuinfo,cpubase,aasmbase,aasmtai,aasmcpu,
      nmem,nld,ncnv,
      ncgutil,cgobj,tgobj,regvars,rgobj,rgcpu,cg64f32,cgcpu,cpupi;

  function tppccallnode.pass_1 : tnode;

    begin
       result:=inherited pass_1;
       if assigned(result) then
         exit;
       if procdefinition is tprocdef then
         begin
            if tprocdef(procdefinition).parast.datasize>tppcprocinfo(current_procinfo).maxpushedparasize then
              tppcprocinfo(current_procinfo).maxpushedparasize:=tprocdef(procdefinition).parast.datasize;
         end
       else
         begin
            {!!!!}
         end;
    end;

  procedure tppccallnode.push_framepointer;
    var
       href : treference;
       hregister1,hregister2 : tregister;
       i : longint;
    begin
       if current_procdef.parast.symtablelevel=(tprocdef(procdefinition).parast.symtablelevel) then
         begin
            { pass the same framepointer as the current procedure got }
            hregister2.enum:=R_INTREGISTER;
            hregister2.number:=NR_R11;
            reference_reset_base(href,current_procinfo.framepointer,current_procinfo.framepointer_offset);
            cg.a_load_ref_reg(exprasmlist,OS_ADDR,href,hregister2);
            { it must be adjusted! }
         end
         { this is only true if the difference is one !!
           but it cannot be more !! }
       else if (current_procdef.parast.symtablelevel=(tprocdef(procdefinition).parast.symtablelevel)-1) then
         begin
            { pass the same framepointer as the current procedure got }
            hregister1.enum:=R_INTREGISTER;
            hregister1.number:=NR_R1;
            hregister2.enum:=R_INTREGISTER;
            hregister2.number:=NR_R11;
            if assigned(current_procinfo.procdef.localst) then
              exprasmlist.concat(taicpu.op_reg_reg_const(A_ADDI,hregister2,hregister1,current_procinfo.procdef.localst.address_fixup));
         end
       else if (current_procdef.parast.symtablelevel>(tprocdef(procdefinition).parast.symtablelevel)) then
         begin
            hregister1:=rg.getregisterint(exprasmlist,OS_ADDR);
            reference_reset_base(href,current_procinfo.framepointer,current_procinfo.framepointer_offset);
            cg.a_load_ref_reg(exprasmlist,OS_ADDR,href,hregister1);
            { !!!!!!!!!! not true anymore!!!!!!! (JM)       }
            { the previous frame pointer is always saved at }
            { previous_framepointer-sizeof(pointer)         }
            reference_reset_base(href,hregister1,-POINTER_SIZE);
            i:=current_procdef.parast.symtablelevel-1;
            while (i>tprocdef(procdefinition).parast.symtablelevel) do
              begin
                 {we should get the correct frame_pointer_offset at each level
                 how can we do this !!! }
                 cg.a_load_ref_reg(exprasmlist,OS_ADDR,href,hregister1);
                 dec(i);
              end;
            hregister2.enum:=R_11;
            cg.a_load_reg_reg(exprasmlist,OS_ADDR,OS_ADDR,hregister1,hregister2);
            rg.ungetregisterint(exprasmlist,hregister1);
         end
       else
         internalerror(2002081303);
    end;

begin
   ccallnode:=tppccallnode;
end.
{
  $Log$
  Revision 1.13  2003-05-18 15:15:59  florian
    + added abi field to tsysteminfo

  Revision 1.12  2003/05/16 23:15:51  jonas
    * workaround for nested procedures until Peter fixes it properly :)

  Revision 1.11  2003/05/16 20:00:39  jonas
    * powerpc nested procedure fixes, should work completely now if all
      local variables of the parent procedure are declared before the
      nested procedures are declared

  Revision 1.10  2003/04/27 11:21:36  peter
    * aktprocdef renamed to current_procdef
    * procinfo renamed to current_procinfo
    * procinfo will now be stored in current_module so it can be
      cleaned up properly
    * gen_main_procsym changed to create_main_proc and release_main_proc
      to also generate a tprocinfo structure
    * fixed unit implicit initfinal

  Revision 1.9  2003/04/27 10:41:47  florian
    * fixed nested procedures to get them working as before

  Revision 1.8  2003/04/27 07:48:05  peter
    * updated for removed lexlevel

  Revision 1.7  2003/04/24 11:24:00  florian
    * fixed several issues with nested procedures

  Revision 1.6  2003/04/23 12:35:35  florian
    * fixed several issues with powerpc
    + applied a patch from Jonas for nested function calls (PowerPC only)
    * ...

  Revision 1.5  2003/04/04 15:38:56  peter
    * moved generic code from n386cal to ncgcal, i386 now also
      uses the generic ncgcal

  Revision 1.4  2002/12/05 14:28:12  florian
    * some variant <-> dyn. array stuff

  Revision 1.3  2002/11/25 17:43:28  peter
    * splitted defbase in defutil,symutil,defcmp
    * merged isconvertable and is_equal into compare_defs(_ext)
    * made operator search faster by walking the list only once

  Revision 1.2  2002/08/17 09:23:49  florian
    * first part of procinfo rewrite

  Revision 1.1  2002/08/13 21:40:59  florian
    * more fixes for ppc calling conventions
}

