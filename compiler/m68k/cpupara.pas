{
    Copyright (c) 2002 by Florian Klaempfl

    Generates the argument location information for 680x0

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
{ Generates the argument location information for 680x0.
}
unit cpupara;

{$i fpcdefs.inc}

  interface

    uses
      globtype,
      cpubase,
      aasmdata,
      symconst,symtype,symdef,symsym,
      parabase,paramgr,cgbase,cgutils;

    type
       { Returns the location for the nr-st 32 Bit int parameter
         if every parameter before is an 32 Bit int parameter as well
         and if the calling conventions for the helper routines of the
         rtl are used.
       }
       tm68kparamanager = class(tparamanager)
          procedure getintparaloc(pd : tabstractprocdef; nr : longint; var cgpara : tcgpara);override;
          function create_paraloc_info(p : tabstractprocdef; side: tcallercallee):longint;override;
          function push_addr_param(varspez:tvarspez;def : tdef;calloption : tproccalloption) : boolean;override;
          function get_funcretloc(p : tabstractprocdef; side: tcallercallee; forcetempdef: tdef): tcgpara;override;
          procedure createtempparaloc(list: TAsmList;calloption : tproccalloption;parasym : tparavarsym;can_use_final_stack_loc : boolean;var cgpara:TCGPara);override;
          function create_varargs_paraloc_info(p : tabstractprocdef; varargspara:tvarargsparalist):longint;override;
          function parseparaloc(p : tparavarsym;const s : string) : boolean;override;
          function parsefuncretloc(p : tabstractprocdef; const s : string) : boolean;override;
          function get_volatile_registers_int(calloption:tproccalloption):tcpuregisterset;override;
          function get_volatile_registers_address(calloption:tproccalloption):tcpuregisterset;override;
         private
          function parse_loc_string_to_register(var locreg: tregister; const s : string): boolean;
          procedure init_values(var curintreg, curfloatreg: tsuperregister; var cur_stack_offset: aword);
          function create_paraloc_info_intern(p : tabstractprocdef; side: tcallercallee; paras: tparalist;
                                               var curintreg, curfloatreg: tsuperregister; var cur_stack_offset: aword):longint;
       end;

  implementation

    uses
       verbose,
       globals,
       systems,
       cpuinfo,
       defutil;


    function tm68kparamanager.get_volatile_registers_int(calloption:tproccalloption):tcpuregisterset;
      begin
        { d0 and d1 are considered volatile }
        Result:=VOLATILE_INTREGISTERS;
      end;


    function tm68kparamanager.get_volatile_registers_address(calloption:tproccalloption):tcpuregisterset;
      begin
        { a0 and a1 are considered volatile }
        Result:=VOLATILE_ADDRESSREGISTERS;
      end;


    procedure tm68kparamanager.getintparaloc(pd : tabstractprocdef; nr : longint; var cgpara : tcgpara);
      var
        paraloc : pcgparalocation;
        psym: tparavarsym;
        pdef: tdef;
      begin
         if nr<1 then
           internalerror(2002070801);
         psym:=tparavarsym(pd.paras[nr-1]);
         pdef:=psym.vardef;
         if push_addr_param(psym.varspez,pdef,pd.proccalloption) then
           pdef:=getpointerdef(pdef);
         cgpara.reset;
         cgpara.size:=def_cgsize(pdef);
         cgpara.intsize:=tcgsize2size[cgpara.size];
         cgpara.alignment:=std_param_align;
         cgpara.def:=pdef;
         paraloc:=cgpara.add_location;
         with paraloc^ do
           begin
              { warning : THIS ONLY WORKS WITH INTERNAL ROUTINES,
                WHICH MUST ALWAYS PASS 4-BYTE PARAMETERS!!
              }
              loc:=LOC_REFERENCE;
              reference.index:=NR_STACK_POINTER_REG;
              reference.offset:=target_info.first_parm_offset+nr*4;
              size:=def_cgsize(pdef);
              def:=pdef;
           end;
      end;

    function getparaloc(p : tdef) : tcgloc;

      begin
         result:=LOC_REFERENCE;
         (* Later, the LOC_REFERENCE is in most cases changed into LOC_REGISTER
           if push_addr_param for the def is true
         case p.typ of
            orddef:
              result:=LOC_REGISTER;
            floatdef:
              result:=LOC_FPUREGISTER;
            enumdef:
              result:=LOC_REGISTER;
            pointerdef:
              result:=LOC_REGISTER;
            formaldef:
              result:=LOC_REGISTER;
            classrefdef:
              result:=LOC_REGISTER;
            recorddef:
              if (target_info.abi<>abi_powerpc_aix) then
                result:=LOC_REFERENCE
              else
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
            procvardef:
              if (po_methodpointer in tprocvardef(p).procoptions) then
                result:=LOC_REFERENCE
              else
                result:=LOC_REGISTER;
            filedef:
              result:=LOC_REGISTER;
            arraydef:
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
         *)
      end;


{ TODO: copied from ppc cg, needs work}
    function tm68kparamanager.push_addr_param(varspez:tvarspez;def : tdef;calloption : tproccalloption) : boolean;
      begin
        result:=false;
        { var,out,constref always require address }
        if varspez in [vs_var,vs_out,vs_constref] then
          begin
            result:=true;
            exit;
          end;
        case def.typ of
          variantdef,
          formaldef :
            result:=true;
          recorddef:
            result:=true;
          arraydef:
            result:=(tarraydef(def).highrange>=tarraydef(def).lowrange) or
                             is_open_array(def) or
                             is_array_of_const(def) or
                             is_array_constructor(def);
          objectdef :
            result:=is_object(def);
          setdef :
            result:=not is_smallset(def);
          stringdef :
            result:=tstringdef(def).stringtype in [st_shortstring,st_longstring];
          procvardef :
            result:=po_methodpointer in tprocvardef(def).procoptions;
        end;
      end;

    procedure tm68kparamanager.init_values(var curintreg, curfloatreg: tsuperregister; var cur_stack_offset: aword);
      begin
        cur_stack_offset:=8;
        curintreg:=RS_D0;
        curfloatreg:=RS_FP0;
      end;

    function tm68kparamanager.get_funcretloc(p : tabstractprocdef; side: tcallercallee; forcetempdef: tdef): tcgpara;
      var
        paraloc : pcgparalocation;
        retcgsize  : tcgsize;
      begin
        if set_common_funcretloc_info(p,forcetempdef,retcgsize,result) then
          exit;

        paraloc:=result.add_location;
        { Return in FPU register? }
        if not (cs_fp_emulation in current_settings.moduleswitches) and
           not (current_settings.fputype=fpu_soft) and (result.def.typ=floatdef) then
          begin
            paraloc^.loc:=LOC_FPUREGISTER;
            paraloc^.register:=NR_FPU_RESULT_REG;
            paraloc^.size:=retcgsize;
            paraloc^.def:=result.def;
          end
        else
         { Return in register }
          begin
            if retcgsize in [OS_64,OS_S64] then
             begin
               { low 32bits }
               paraloc^.loc:=LOC_REGISTER;
               paraloc^.size:=OS_32;
               paraloc^.def:=u32inttype;
               if side=callerside then
                 paraloc^.register:=NR_FUNCTION_RESULT64_LOW_REG
               else
                 paraloc^.register:=NR_FUNCTION_RETURN64_LOW_REG;
               { high 32bits }
               paraloc:=result.add_location;
               paraloc^.loc:=LOC_REGISTER;
               paraloc^.size:=OS_32;
               paraloc^.def:=u32inttype;
               if side=calleeside then
                 paraloc^.register:=NR_FUNCTION_RESULT64_HIGH_REG
               else
                 paraloc^.register:=NR_FUNCTION_RETURN64_HIGH_REG;
             end
            else
             begin
               paraloc^.loc:=LOC_REGISTER;
               paraloc^.size:=retcgsize;
               paraloc^.def:=result.def;
               if side=callerside then
                 paraloc^.register:=newreg(R_INTREGISTER,RS_FUNCTION_RESULT_REG,cgsize2subreg(R_INTREGISTER,retcgsize))
               else
                 paraloc^.register:=newreg(R_INTREGISTER,RS_FUNCTION_RETURN_REG,cgsize2subreg(R_INTREGISTER,retcgsize));
             end;
          end;
      end;

    function tm68kparamanager.create_paraloc_info(p : tabstractprocdef; side: tcallercallee):longint;
      var
        cur_stack_offset: aword;
        curintreg, curfloatreg: tsuperregister;
      begin
        init_values(curintreg,curfloatreg,cur_stack_offset);

        result:=create_paraloc_info_intern(p,side,p.paras,curintreg,curfloatreg,cur_stack_offset);

        create_funcretloc_info(p,side);
      end;

    function tm68kparamanager.create_paraloc_info_intern(p : tabstractprocdef; side: tcallercallee; paras: tparalist;
                               var curintreg, curfloatreg: tsuperregister; var cur_stack_offset: aword):longint;
      var
        paraloc      : pcgparalocation;
        hp           : tparavarsym;
        paracgsize   : tcgsize;
        paralen      : aint;
        parasize     : longint;
	paradef      : tdef;
        i            : longint;
	loc          : tcgloc;
	nextintreg,
	nextfloatreg : tsuperregister;
	stack_offset : longint;
        firstparaloc : boolean;

      begin
        result:=0;
	nextintreg:=curintreg;
	nextfloatreg:=curfloatreg;
	stack_offset:=cur_stack_offset;

        parasize:=0;

        for i:=0 to paras.count-1 do
          begin
            hp:=tparavarsym(paras[i]);
	    paradef:=hp.vardef;

	    { syscall for AmigaOS can have already a paraloc set }
            if (vo_has_explicit_paraloc in hp.varoptions) then
              begin
                if not(vo_is_syscall_lib in hp.varoptions) then
                  internalerror(200506051);
                continue;
              end;
            hp.paraloc[side].reset;

            { currently only support C-style array of const }
            if (p.proccalloption in cstylearrayofconst) and
               is_array_of_const(paradef) then
              begin
{$ifdef DEBUG_CHARLIE}
                writeln('loc register');
{$endif DEBUG_CHARLIE}
                paraloc:=hp.paraloc[side].add_location;
                { hack: the paraloc must be valid, but is not actually used }
                paraloc^.loc:=LOC_REGISTER;
		paraloc^.register:=NR_D0;
                paraloc^.size:=OS_ADDR;
                paraloc^.def:=voidpointertype;
                break;
              end;

            if push_addr_param(hp.varspez,paradef,p.proccalloption) then
              begin
{$ifdef DEBUG_CHARLIE}
                writeln('loc register');
{$endif DEBUG_CHARLIE}
                paradef:=getpointerdef(paradef);
                loc:=LOC_REGISTER;
                paracgsize := OS_ADDR;
                paralen := tcgsize2size[OS_ADDR];
              end
            else
              begin
                if not is_special_array(paradef) then
                  paralen:=paradef.size
                else
                  paralen:=tcgsize2size[def_cgsize(paradef)];

                loc:=getparaloc(paradef);
                paracgsize:=def_cgsize(paradef);
                { for things like formaldef }
                if (paracgsize=OS_NO) then
                  begin
                    paracgsize:=OS_ADDR;
                    paralen := tcgsize2size[OS_ADDR];
                  end;
              end;

            hp.paraloc[side].alignment:=std_param_align;
            hp.paraloc[side].size:=paracgsize;
            hp.paraloc[side].intsize:=paralen;
            hp.paraloc[side].def:=paradef;

            if (paralen = 0) then
              if (paradef.typ = recorddef) then
                begin
                  paraloc:=hp.paraloc[side].add_location;
                  paraloc^.loc := LOC_VOID;
                end
              else
                internalerror(200506052);
            firstparaloc:=true;
            { can become < 0 for e.g. 3-byte records }
            while (paralen > 0) do
              begin
                paraloc:=hp.paraloc[side].add_location;
                (*
                  by default, the m68k doesn't know any register parameters  (FK)
                if (loc = LOC_REGISTER) and
                   (nextintreg <= RS_D2) then
                  begin
		    //writeln('loc register');
                    paraloc^.loc := loc;
                    { make sure we don't lose whether or not the type is signed }
                    if (paradef.typ <> orddef) then
                      paracgsize := int_cgsize(paralen);
                    if (paracgsize in [OS_NO,OS_64,OS_S64]) then
                      paraloc^.size := OS_INT
                    else
                      paraloc^.size := paracgsize;
                    paraloc^.register:=newreg(R_INTREGISTER,nextintreg,R_SUBNONE);
                    inc(nextintreg);
                    dec(paralen,tcgsize2size[paraloc^.size]);
                  end
                else if (loc = LOC_FPUREGISTER) and
                        (nextfloatreg <= RS_FP2) then
                  begin
//		    writeln('loc fpuregister');
                    paraloc^.loc:=loc;
                    paraloc^.size := paracgsize;
                    paraloc^.register:=newreg(R_FPUREGISTER,nextfloatreg,R_SUBWHOLE);
                    inc(nextfloatreg);
                    dec(paralen,tcgsize2size[paraloc^.size]);
                  end
                else { LOC_REFERENCE }
                *)
                  begin
{$ifdef DEBUG_CHARLIE}
		    writeln('loc reference');
{$endif DEBUG_CHARLIE}
                    paraloc^.loc:=LOC_REFERENCE;
                    paraloc^.def:=get_paraloc_def(paradef,paralen,firstparaloc);
                    if paradef.typ<>orddef then
                      paracgsize:=int_cgsize(paralen);
                    if paracgsize=OS_NO then
                      paraloc^.size:=OS_INT
                    else
                      paraloc^.size:=paracgsize;
                    if (side = callerside) then
                      paraloc^.reference.index:=NR_STACK_POINTER_REG
                    else
                      paraloc^.reference.index:=NR_FRAME_POINTER_REG;
                    paraloc^.reference.offset:=stack_offset;
                    inc(stack_offset,align(paralen,4));
                    paralen := 0;
                  end;
                firstparaloc:=false;
              end;
          end;
         result:=stack_offset;
//	 writeln('stack offset:',stack_offset);
      end;


{

            if push_addr_param(hp.varspez,paradef,p.proccalloption) then
              paracgsize:=OS_ADDR
            else
              begin
                paracgsize:=def_cgsize(paradef);
                if paracgsize=OS_NO then
                  paracgsize:=OS_ADDR;
              end;
            hp.paraloc[side].size:=paracgsize;
            hp.paraloc[side].Alignment:=std_param_align;
            paraloc:=hp.paraloc[side].add_location;
            paraloc^.size:=paracgsize;
            paraloc^.loc:=LOC_REFERENCE;
            if side=callerside then
              paraloc^.reference.index:=NR_STACK_POINTER_REG
            else
              paraloc^.reference.index:=NR_FRAME_POINTER_REG;
            paraloc^.reference.offset:=target_info.first_parm_offset+parasize;
          end;
	create_funcretloc_info(p,side);
        result:=parasize;
      end;
}

    function tm68kparamanager.parse_loc_string_to_register(var locreg: tregister; const s : string): boolean;
      begin
        locreg:=std_regnum_search(lowercase(s));
        result:=(locreg <> NR_NO) and (locreg <> NR_SP);
      end;

    function tm68kparamanager.parsefuncretloc(p : tabstractprocdef; const s : string) : boolean;
      begin
        case target_info.system of
          system_m68k_amiga:
            result:=parse_loc_string_to_register(p.exp_funcretloc, s);
          else
            internalerror(2005121801);
        end;
      end;

    function tm68kparamanager.parseparaloc(p : tparavarsym;const s : string) : boolean;
      var
        paraloc : pcgparalocation;
      begin
        result:=false;
        case target_info.system of
          system_m68k_amiga:
            begin
              p.paraloc[callerside].alignment:=4;
              paraloc:=p.paraloc[callerside].add_location;
              paraloc^.loc:=LOC_REGISTER;
              paraloc^.size:=def_cgsize(p.vardef);
              paraloc^.def:=p.vardef;

              if not parse_loc_string_to_register(paraloc^.register, s) then
                exit;

              { copy to callee side }
              p.paraloc[calleeside].add_location^:=paraloc^;
            end;
          else
            internalerror(200405092);
        end;
        result:=true;
      end;


    procedure tm68kparamanager.createtempparaloc(list: TAsmList;calloption : tproccalloption;parasym : tparavarsym;can_use_final_stack_loc : boolean;var cgpara:TCGPara);
      var
        paraloc : pcgparalocation;
      begin
        paraloc:=parasym.paraloc[callerside].location;
        { Never a need for temps when value is pushed (calls inside parameters
          will simply allocate even more stack space for their parameters) }
        if not(use_fixed_stack) then
          can_use_final_stack_loc:=true;
        inherited createtempparaloc(list,calloption,parasym,can_use_final_stack_loc,cgpara);
      end;

    function tm68kparamanager.create_varargs_paraloc_info(p : tabstractprocdef; varargspara:tvarargsparalist):longint;
      var
        cur_stack_offset: aword;
        curintreg, curfloatreg: tsuperregister;
      begin
        init_values(curintreg,curfloatreg,cur_stack_offset);

        result:=create_paraloc_info_intern(p,callerside,p.paras,curintreg,curfloatreg,cur_stack_offset);
        if (p.proccalloption in cstylearrayofconst) then
          { just continue loading the parameters in the registers }
          result:=create_paraloc_info_intern(p,callerside,varargspara,curintreg,curfloatreg,cur_stack_offset)
        else
          internalerror(200410231);
      end;


begin
  paramanager:=tm68kparamanager.create;
end.
