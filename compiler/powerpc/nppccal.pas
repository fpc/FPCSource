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
         procedure extra_call_code;override;
         procedure do_syscall;override;
       end;


implementation

    uses
      globtype,systems,
      cutils,verbose,globals,
      symconst,symbase,symsym,symtable,defutil,paramgr,parabase,
{$ifdef GDB}
      strings,
      gdb,
{$endif GDB}
      cgbase,pass_2,
      cpuinfo,cpubase,aasmbase,aasmtai,aasmcpu,
      nmem,nld,ncnv,
      ncgutil,cgutils,cgobj,tgobj,regvars,rgobj,rgcpu,
      cg64f32,cgcpu,cpupi,procinfo;


    procedure tppccallnode.extra_call_code;
      begin
        if assigned(varargsparas) then
          begin
            if (target_info.abi = abi_powerpc_sysv) then
              begin
                if va_uses_float_reg in varargsparas.varargsinfo then
                  exprasmlist.concat(taicpu.op_const_const_const(A_CREQV,6,6,6))
                else
                  exprasmlist.concat(taicpu.op_const_const_const(A_CRXOR,6,6,6));
              end;
          end;
      end;

    procedure tppccallnode.do_syscall;
      var
        tmpref: treference;
      begin
        case target_info.system of
          system_powerpc_morphos:
            begin
              cg.getcpuregister(exprasmlist,NR_R0);
              cg.getcpuregister(exprasmlist,NR_R3);

              { store call offset into R3 }
              exprasmlist.concat(taicpu.op_reg_const(A_LI,NR_R3,-tprocdef(procdefinition).extnumber));

              { prepare LR, and call function }
              reference_reset_base(tmpref,NR_R2,100); { 100 ($64) is EmulDirectCallOS offset }
              exprasmlist.concat(taicpu.op_reg_ref(A_LWZ,NR_R0,tmpref));
              exprasmlist.concat(taicpu.op_reg(A_MTLR,NR_R0));
              exprasmlist.concat(taicpu.op_none(A_BLRL));

              cg.ungetcpuregister(exprasmlist,NR_R0);
              cg.ungetcpuregister(exprasmlist,NR_R3);
            end;
          else
            internalerror(2004042901);
        end;
      end;

begin
   ccallnode:=tppccallnode;
end.
{
  $Log$
  Revision 1.31  2004-12-06 18:06:37  jonas
    * only set/clear bit 6 of cr in case of varargs for the sysv abi

  Revision 1.30  2004/10/15 09:30:13  mazen
  - remove $IFDEF DELPHI and related code
  - remove $IFDEF FPCPROCVAR and related code

  Revision 1.29  2004/09/25 14:23:54  peter
    * ungetregister is now only used for cpuregisters, renamed to
      ungetcpuregister
    * renamed (get|unget)explicitregister(s) to ..cpuregister
    * removed location-release/reference_release

  Revision 1.28  2004/09/21 17:25:13  peter
    * paraloc branch merged

  Revision 1.27.4.1  2004/09/18 20:21:08  jonas
    * fixed ppc, but still needs fix in tgobj

  Revision 1.27  2004/06/20 08:55:32  florian
    * logs truncated

  Revision 1.26  2004/04/29 22:18:37  karoly
    * removed some unneeded parts of do_syscall

  Revision 1.25  2004/04/29 14:36:42  karoly
    * little cleanup of the previous commit

  Revision 1.24  2004/04/29 14:01:23  karoly
    + first implementation of PowerPC/MorphOS do_syscall

}
