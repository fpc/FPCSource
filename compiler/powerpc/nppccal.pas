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
          procedure load_framepointer;override;
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
            if tprocdef(procdefinition).parast.datasize>tppcprocinfo(procinfo).maxpushedparasize then
              tppcprocinfo(procinfo).maxpushedparasize:=tprocdef(procdefinition).parast.datasize
         end
       else
         begin
            {!!!!}
         end;
    end;

  procedure tppccallnode.load_framepointer;

    begin
       { if we call a nested function in a method, we must      }
       { push also SELF!                                        }
       { THAT'S NOT TRUE, we have to load ESI via frame pointer }
       { access                                                 }
       {
         begin
            loadesi:=false;
            emit_reg(A_PUSH,S_L,R_ESI);
         end;
       }
       {
       if lexlevel=(tprocdef(procdefinition).parast.symtablelevel) then
         begin
            reference_reset_base(href,procinfo^.framepointer,procinfo^.framepointer_offset);
            cg.a_param_ref(exprasmlist,OS_ADDR,href,-1);
         end
         { this is only true if the difference is one !!
           but it cannot be more !! }
       else if (lexlevel=(tprocdef(procdefinition).parast.symtablelevel)-1) then
         begin
            cg.a_param_reg(exprasmlist,OS_ADDR,procinfo^.framepointer,-1);
         end
       else if (lexlevel>(tprocdef(procdefinition).parast.symtablelevel)) then
         begin
            hregister:=rg.getregisterint(exprasmlist);
            reference_reset_base(href,procinfo^.framepointer,procinfo^.framepointer_offset);
            cg.a_load_ref_reg(exprasmlist,OS_ADDR,href,hregister);
            for i:=(tprocdef(procdefinition).parast.symtablelevel) to lexlevel-1 do
              begin
                 {we should get the correct frame_pointer_offset at each level
                 how can we do this !!! }
                 reference_reset_base(href,hregister,procinfo^.framepointer_offset);
                 cg.a_load_ref_reg(exprasmlist,OS_ADDR,href,hregister);
              end;
            cg.a_param_reg(exprasmlist,OS_ADDR,hregister,-1);
            rg.ungetregisterint(exprasmlist,hregister);
         end
       else
         internalerror(2002081303);
       }
    end;

begin
   ccallnode:=tppccallnode;
end.
{
  $Log$
  Revision 1.3  2002-11-25 17:43:28  peter
    * splitted defbase in defutil,symutil,defcmp
    * merged isconvertable and is_equal into compare_defs(_ext)
    * made operator search faster by walking the list only once

  Revision 1.2  2002/08/17 09:23:49  florian
    * first part of procinfo rewrite

  Revision 1.1  2002/08/13 21:40:59  florian
    * more fixes for ppc calling conventions
}

