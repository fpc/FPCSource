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
      aasmdata,
      symtype,symdef,
      cgbase,cgutils,
      parabase,paramgr;

    type
      trvparamanager = class(tparamanager)
        function get_volatile_registers_int(calloption: tproccalloption): tcpuregisterset; override;
        function get_volatile_registers_fpu(calloption: tproccalloption): tcpuregisterset; override;

        function get_saved_registers_fpu(calloption: tproccalloption): tcpuregisterarray;override;
        function get_saved_registers_int(calloption: tproccalloption): tcpuregisterarray;override;

        procedure getcgtempparaloc(list: TAsmList; pd: tabstractprocdef; nr: longint; var cgpara: tcgpara);override;

      protected
        procedure init_values(var curintreg, curfloatreg, curmmreg: tsuperregister; var cur_stack_offset: aword);
      end;

    function getparaloc(p : tdef) : tcgloc;

implementation

    uses
      verbose,
      globals,
      cpuinfo,
      symconst,symsym,
      defutil,
      cpubase;

    function getparaloc(p : tdef) : tcgloc;
      begin
         { Later, the LOC_REFERENCE is in most cases changed into LOC_REGISTER
           if push_addr_param for the def is true
         }
         case p.typ of
            orddef:
              result:=LOC_REGISTER;
            floatdef:
              if (cs_fp_emulation in current_settings.moduleswitches) or
                 (current_settings.fputype in [fpu_soft]) then
                result := LOC_REGISTER
              else
                result := LOC_FPUREGISTER;
            enumdef:
              result:=LOC_REGISTER;
            pointerdef:
              result:=LOC_REGISTER;
            formaldef:
              result:=LOC_REGISTER;
            classrefdef:
              result:=LOC_REGISTER;
            procvardef:
            { this must be fixed for RISC64, the ifdef is not correct }
{$ifdef RISCV32}
              if (p.size = sizeof(pint)) then
                result:=LOC_REGISTER
              else
                result:=LOC_REFERENCE;
{$else RISCV32}
              result:=LOC_REGISTER;
{$endif RISCV32}
            recorddef:
            { this must be fixed for RISC64, the ifdef is not correct }
{$ifdef RISCV32}
              if (p.size > sizeof(pint)) then
                result:=LOC_REFERENCE
              else
{$endif RISCV32}
                result:=LOC_REGISTER;
            objectdef:
              if is_object(p) then
                result:=LOC_REFERENCE
              else
                result:=LOC_REGISTER;
            stringdef:
              if is_shortstring(p) or is_longstring(p) then
                result:=LOC_REFERENCE
              else
                result:=LOC_REGISTER;
            filedef:
              result:=LOC_REGISTER;
            arraydef:
              if is_dynamic_array(p) then
                getparaloc:=LOC_REGISTER
              else
                result:=LOC_REFERENCE;
            setdef:
              if is_smallset(p) then
                result:=LOC_REGISTER
              else
                result:=LOC_REFERENCE;
            variantdef:
              result:=LOC_REFERENCE;
            { avoid problems with errornous definitions }
            errordef:
              result:=LOC_REGISTER;
            else
              internalerror(2002071001);
         end;
      end;


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


    procedure trvparamanager.getcgtempparaloc(list: TAsmList; pd : tabstractprocdef; nr : longint; var cgpara : tcgpara);
      var
        paraloc : pcgparalocation;
        psym : tparavarsym;
        pdef : tdef;
      begin
        psym:=tparavarsym(pd.paras[nr-1]);
        pdef:=psym.vardef;
        if push_addr_param(psym.varspez,pdef,pd.proccalloption) then
          pdef:=cpointerdef.getreusable_no_free(pdef);
        cgpara.reset;
        cgpara.size:=def_cgsize(pdef);
        cgpara.intsize:=tcgsize2size[cgpara.size];
        cgpara.alignment:=get_para_align(pd.proccalloption);
        cgpara.def:=pdef;
        paraloc:=cgpara.add_location;
        with paraloc^ do
         begin
           size:=def_cgsize(pdef);
           def:=pdef;
           if (nr<=8) then
             begin
               if nr=0 then
                 internalerror(2024121501);
               loc:=LOC_REGISTER;
               register:=newreg(R_INTREGISTER,RS_X10+nr-1,R_SUBWHOLE);
             end
           else
             begin
               loc:=LOC_REFERENCE;
               paraloc^.reference.index:=NR_STACK_POINTER_REG;
               reference.offset:=sizeof(pint)*nr;
             end;
          end;
      end;


    procedure trvparamanager.init_values(var curintreg, curfloatreg, curmmreg: tsuperregister; var cur_stack_offset: aword);
      begin
        { register parameter save area begins at 48(r2) }
        cur_stack_offset := 0;
        curintreg := RS_X10;
        curfloatreg := RS_F10;
        curmmreg := RS_NO;
      end;

end.


