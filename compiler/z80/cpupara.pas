{
    Copyright (c) 2008 by Florian Klaempfl

    Z80 specific calling conventions

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
{ ARM specific calling conventions are handled by this unit
}
unit cpupara;

{$i fpcdefs.inc}

  interface

    uses
       globtype,globals,
       aasmtai,aasmdata,
       cpuinfo,cpubase,cgbase,cgutils,
       symconst,symbase,symtype,symdef,parabase,paramgr;

    type
       tcpuparamanager = class(tparamanager)
          function get_volatile_registers_int(calloption : tproccalloption):tcpuregisterset;override;
          function get_volatile_registers_fpu(calloption : tproccalloption):tcpuregisterset;override;
          function push_addr_param(varspez:tvarspez;def : tdef;calloption : tproccalloption) : boolean;override;
          function ret_in_param(def:tdef;pd:tabstractprocdef):boolean;override;
          function create_paraloc_info(p : tabstractprocdef; side: tcallercallee):longint;override;
          function create_varargs_paraloc_info(p : tabstractprocdef; side: tcallercallee; varargspara:tvarargsparalist):longint;override;
          function  get_funcretloc(p : tabstractprocdef; side: tcallercallee; forcetempdef: tdef): tcgpara;override;
         private
          procedure init_values(var curintreg, curfloatreg, curmmreg: tsuperregister; var cur_stack_offset: longint);
          function create_paraloc_info_intern(p : tabstractprocdef; side: tcallercallee; paras: tparalist;
            var curintreg, curfloatreg, curmmreg: tsuperregister; var cur_stack_offset: longint):longint;
       end;

  implementation

    uses
       verbose,systems,
       rgobj,
       defutil,symsym;


    function tcpuparamanager.get_volatile_registers_int(calloption : tproccalloption):tcpuregisterset;
      begin
        result:=VOLATILE_INTREGISTERS;
      end;


    function tcpuparamanager.get_volatile_registers_fpu(calloption : tproccalloption):tcpuregisterset;
      begin
        result:=VOLATILE_FPUREGISTERS;
      end;


    function getparaloc_fastcall(calloption : tproccalloption; p : tdef) : tcgloc;
      begin
         { Later, the LOC_REFERENCE is in most cases changed into LOC_REGISTER
           if push_addr_param for the def is true
         }
         case p.typ of
            orddef:
              result:=LOC_REGISTER;
            floatdef:
              result:=LOC_REGISTER;
            enumdef:
              result:=LOC_REGISTER;
            pointerdef:
              result:=LOC_REGISTER;
            formaldef:
              result:=LOC_REGISTER;
            classrefdef:
              result:=LOC_REGISTER;
            recorddef:
              result:=LOC_REGISTER;
            objectdef:
              result:=LOC_REGISTER;
            stringdef:
              if is_shortstring(p) or is_longstring(p) then
                result:=LOC_REFERENCE
              else
                result:=LOC_REGISTER;
            procvardef:
              result:=LOC_REGISTER;
            filedef:
              result:=LOC_REGISTER;
            arraydef:
              if is_dynamic_array(p) then
                result:=LOC_REGISTER
              else
                result:=LOC_REFERENCE;
            setdef:
              if is_smallset(p) then
                result:=LOC_REGISTER
              else
                result:=LOC_REFERENCE;
            variantdef:
              result:=LOC_REGISTER;
            { avoid problems with errornous definitions }
            errordef:
              result:=LOC_REGISTER;
            else
              internalerror(2017032603);
         end;
      end;


    function tcpuparamanager.push_addr_param(varspez:tvarspez;def : tdef;calloption : tproccalloption) : boolean;
      begin
        result:=false;
        if varspez in [vs_var,vs_out,vs_constref] then
          begin
            result:=true;
            exit;
          end;
        case def.typ of
          objectdef:
            result:=is_object(def) and ((varspez=vs_const) or (def.size=0));
          recorddef:
            result:=(varspez=vs_const) or (def.size=0);
          variantdef,
          formaldef:
            result:=true;
          arraydef:
            result:=(tarraydef(def).highrange>=tarraydef(def).lowrange) or
                             is_open_array(def) or
                             is_array_of_const(def) or
                             is_array_constructor(def);
          setdef :
            result:=not is_smallset(def);
          stringdef :
            result:=tstringdef(def).stringtype in [st_shortstring,st_longstring];
        else
          result:=def.size>8;
        end;
      end;


    function tcpuparamanager.ret_in_param(def:tdef;pd:tabstractprocdef):boolean;
      begin
        if handle_common_ret_in_param(def,pd,result) then
          exit;
        case def.typ of
          recorddef:
            { this is how gcc 4.0.4 on linux seems to do it, it doesn't look like being
              ARM ABI standard compliant
            }
            result:=not((trecorddef(def).symtable.SymList.count=1) and
              not(ret_in_param(tabstractvarsym(trecorddef(def).symtable.SymList[0]).vardef,pd)));
          {
          objectdef
          arraydef:
            result:=not(def.size in [1,2,4]);
          }
          else
            if def.size>4 then
              result:=true
            else
              result:=inherited ret_in_param(def,pd);
        end;
      end;


    procedure tcpuparamanager.init_values(var curintreg, curfloatreg, curmmreg: tsuperregister; var cur_stack_offset: longint);
      begin
        curintreg:=RS_HL;
        curfloatreg:=RS_INVALID;
        curmmreg:=RS_INVALID;
        cur_stack_offset:=0;
      end;


    { TODO : fix tcpuparamanager.create_paraloc_info_intern }
    function tcpuparamanager.create_paraloc_info_intern(p : tabstractprocdef; side: tcallercallee; paras: tparalist;
        var curintreg, curfloatreg, curmmreg: tsuperregister; var cur_stack_offset: longint):longint;

      var
        nextintreg,nextfloatreg,nextmmreg : tsuperregister;
        paradef : tdef;
        paraloc : pcgparalocation;
        stack_offset : longint;
        hp : tparavarsym;
        loc : tcgloc;
        paracgsize   : tcgsize;
        paralen : longint;
        i : integer;
        firstparaloc: boolean;

      procedure assignintreg;
        begin
          { In case of po_delphi_nested_cc, the parent frame pointer
            is always passed on the stack. }
           //if false and { no fastcall yet }
           //   (nextintreg=RS_HL) and
           //   (not(vo_is_parentfp in hp.varoptions) or
           //    not(po_delphi_nested_cc in p.procoptions)) then
           //  begin
           //    paraloc^.loc:=LOC_REGISTER;
           //    paraloc^.register:=newreg(R_INTREGISTER,nextintreg-1,R_SUBWHOLE);
           //    paraloc:=hp.paraloc[side].add_location;
           //    paraloc^.loc:=LOC_REGISTER;
           //    paraloc^.register:=newreg(R_INTREGISTER,nextintreg,R_SUBWHOLE);
           //    dec(nextintreg,2);
           //  end
           //else
             begin
               paraloc^.loc:=LOC_REFERENCE;
               paraloc^.reference.index:=NR_STACK_POINTER_REG;
               paraloc^.reference.offset:=stack_offset;
               inc(stack_offset,2);
            end;
        end;


      begin
        result:=0;
        nextintreg:=curintreg;
        nextfloatreg:=curfloatreg;
        nextmmreg:=curmmreg;
        stack_offset:=cur_stack_offset;

        for i:=0 to paras.count-1 do
          begin
            hp:=tparavarsym(paras[i]);
            paradef:=hp.vardef;

            hp.paraloc[side].reset;

            { currently only support C-style array of const,
              there should be no location assigned to the vararg array itself }
            if (p.proccalloption in cstylearrayofconst) and
               is_array_of_const(paradef) then
              begin
                paraloc:=hp.paraloc[side].add_location;
                { hack: the paraloc must be valid, but is not actually used }
                paraloc^.loc:=LOC_REGISTER;
                paraloc^.register:=NR_HL;
                paraloc^.size:=OS_ADDR;
                paraloc^.def:=voidpointertype;
                break;
              end;

            if push_addr_param(hp.varspez,paradef,p.proccalloption) then
              begin
                paradef:=cpointerdef.getreusable_no_free(paradef);
                loc:=LOC_REGISTER;
                paracgsize:=OS_ADDR;
                paralen:=tcgsize2size[OS_ADDR];
              end
            else
              begin
                if not is_special_array(paradef) then
                  paralen := paradef.size
                else
                  paralen := tcgsize2size[def_cgsize(paradef)];
                // loc := getparaloc(p.proccalloption,paradef);
                loc:=LOC_REFERENCE;
                if (paradef.typ in [objectdef,arraydef,recorddef]) and
                  not is_special_array(paradef) and
                  (hp.varspez in [vs_value,vs_const]) then
                  paracgsize := int_cgsize(paralen)
                else
                  begin
                    paracgsize:=def_cgsize(paradef);
                    { for things like formaldef }
                    if (paracgsize=OS_NO) then
                      begin
                        paracgsize:=OS_ADDR;
                        paralen:=tcgsize2size[OS_ADDR];
                        paradef:=voidpointertype;
                      end;
                  end
              end;

             hp.paraloc[side].size:=paracgsize;
             hp.paraloc[side].Alignment:=std_param_align;
             hp.paraloc[side].intsize:=paralen;
             hp.paraloc[side].def:=paradef;

{$ifdef EXTDEBUG}
             if paralen=0 then
               internalerror(200410311);
{$endif EXTDEBUG}
             firstparaloc:=true;
             if loc=LOC_REGISTER then
               begin
                 { the lsb is located in the register with the lowest number,
                   by adding paralen mod 2, make the size even
                 }
                 nextintreg:=curintreg-(paralen+(paralen mod 2))+1;
                 if nextintreg>=RS_HL then
                   curintreg:=nextintreg-1
                 else
                   begin
                     curintreg:=RS_HL;
                     loc:=LOC_REFERENCE;
                   end;
               end;
             while paralen>0 do
               begin
                 paraloc:=hp.paraloc[side].add_location;

                 if (loc=LOC_REGISTER) and (paracgsize in [OS_F32,OS_F64,OS_F80]) then
                   case paracgsize of
                     OS_F32:
                       begin
                         paraloc^.size:=OS_32;
                         paraloc^.def:=u32inttype;
                       end;
                     OS_F64:
                       begin
                         paraloc^.size:=OS_32;
                         paraloc^.def:=u32inttype;
                       end;
                     else
                       internalerror(2005082901);
                   end
                 else
                   begin
                     paraloc^.size:=paracgsize;
                     paraloc^.def:=paradef;
                   end;
                 case loc of
                    LOC_REGISTER:
                      begin
                        if nextintreg>=RS_HL then
                          begin
                            paraloc^.loc:=LOC_REGISTER;
                            paraloc^.size:=OS_8;
                            paraloc^.def:=u8inttype;
                            paraloc^.register:=newreg(R_INTREGISTER,nextintreg,R_SUBWHOLE);
                            inc(nextintreg);
                          end
                        else
                          { parameters are always passed completely in registers or in memory on Z80 }
                          internalerror(2015041002);
                        dec(paralen,tcgsize2size[paraloc^.size]);
                      end;
                    LOC_REFERENCE:
                      begin
                        if push_addr_param(hp.varspez,paradef,p.proccalloption) then
                          begin
                            paraloc^.size:=OS_ADDR;
                            paraloc^.def:=cpointerdef.getreusable_no_free(paradef);
                            assignintreg
                          end
                        else
                          begin
                             paraloc^.def:=paradef;
                             paraloc^.loc:=LOC_REFERENCE;
                             paraloc^.reference.index:=NR_STACK_POINTER_REG;
                             paraloc^.reference.offset:=stack_offset;
                             inc(stack_offset,paradef.size);
                          end;
                        paralen:=0;
                      end;
                    else
                      internalerror(2002071002);
                 end;
                 if side=calleeside then
                   begin
                     if paraloc^.loc=LOC_REFERENCE then
                       begin
                         paraloc^.reference.index:=NR_FRAME_POINTER_REG;
                         inc(paraloc^.reference.offset,target_info.first_parm_offset);
                       end;
                   end;
                 firstparaloc:=false;
               end;
          end;
        curfloatreg:=nextfloatreg;
        curmmreg:=nextmmreg;
        cur_stack_offset:=stack_offset;
        result:=cur_stack_offset;
      end;


    function tcpuparamanager.create_paraloc_info(p : tabstractprocdef; side: tcallercallee):longint;
      var
        cur_stack_offset: longint;
        curintreg, curfloatreg, curmmreg: tsuperregister;
        retcgsize  : tcgsize;
      begin
        init_values(curintreg,curfloatreg,curmmreg,cur_stack_offset);

        result:=create_paraloc_info_intern(p,side,p.paras,curintreg,curfloatreg,curmmreg,cur_stack_offset);

        create_funcretloc_info(p,side);
     end;


    function  tcpuparamanager.get_funcretloc(p : tabstractprocdef; side: tcallercallee; forcetempdef: tdef): tcgpara;
      var
        retcgsize : tcgsize;
        paraloc : pcgparalocation;
        reg : TRegister;
      begin
         if set_common_funcretloc_info(p,forcetempdef,retcgsize,result) then
           exit;

        paraloc:=result.add_location;
        { Return in FPU register? }
        if result.def.typ=floatdef then
          begin
            if (p.proccalloption in [pocall_softfloat]) or (cs_fp_emulation in current_settings.moduleswitches) then
              begin
                case retcgsize of
                  OS_32,
                  OS_F32:
                    begin
                      paraloc^.loc:=LOC_REGISTER;
                      paraloc^.register:=NR_L;
                      paraloc^.size:=OS_8;
                      paraloc^.def:=u8inttype;

                      paraloc:=result.add_location;
                      paraloc^.loc:=LOC_REGISTER;
                      paraloc^.register:=NR_H;
                      paraloc^.size:=OS_8;
                      paraloc^.def:=u8inttype;

                      paraloc:=result.add_location;
                      paraloc^.loc:=LOC_REGISTER;
                      paraloc^.register:=NR_E;
                      paraloc^.size:=OS_8;
                      paraloc^.def:=u8inttype;

                      paraloc:=result.add_location;
                      paraloc^.loc:=LOC_REGISTER;
                      paraloc^.register:=NR_D;
                      paraloc^.size:=OS_8;
                      paraloc^.def:=u8inttype;
                    end;
                  else
                    internalerror(2005082603);
                end;
              end
            else
              begin
                paraloc^.loc:=LOC_FPUREGISTER;
                paraloc^.register:=NR_FPU_RESULT_REG;
                paraloc^.size:=retcgsize;
                paraloc^.def:=result.def;
              end;
          end
          { Return in register }
        else
          begin
            case retcgsize of
              OS_32,OS_S32:
                begin
                  paraloc^.loc:=LOC_REGISTER;
                  paraloc^.register:=NR_L;
                  paraloc^.size:=OS_8;
                  paraloc^.def:=u8inttype;

                  paraloc:=result.add_location;
                  paraloc^.loc:=LOC_REGISTER;
                  paraloc^.register:=NR_H;
                  paraloc^.size:=OS_8;
                  paraloc^.def:=u8inttype;

                  paraloc:=result.add_location;
                  paraloc^.loc:=LOC_REGISTER;
                  paraloc^.register:=NR_E;
                  paraloc^.size:=OS_8;
                  paraloc^.def:=u8inttype;

                  paraloc:=result.add_location;
                  paraloc^.loc:=LOC_REGISTER;
                  paraloc^.register:=NR_D;
                  paraloc^.size:=OS_8;
                  paraloc^.def:=u8inttype;
                end;
              OS_16,OS_S16:
                begin
                  paraloc^.loc:=LOC_REGISTER;
                  paraloc^.register:=NR_L;
                  paraloc^.size:=OS_8;
                  paraloc^.def:=u8inttype;

                  paraloc:=result.add_location;
                  paraloc^.loc:=LOC_REGISTER;
                  paraloc^.register:=NR_H;
                  paraloc^.size:=OS_8;
                  paraloc^.def:=u8inttype;
                end;
              OS_8,OS_S8:
                begin
                  paraloc^.loc:=LOC_REGISTER;
                  paraloc^.register:=NR_L;
                  paraloc^.size:=OS_8;
                  paraloc^.def:=u8inttype;
                end;
              else
                internalerror(2014030101);
            end;
          end;
      end;


    function tcpuparamanager.create_varargs_paraloc_info(p : tabstractprocdef; side: tcallercallee; varargspara:tvarargsparalist):longint;
      var
        cur_stack_offset: longint;
        curintreg, curfloatreg, curmmreg: tsuperregister;
      begin
        init_values(curintreg,curfloatreg,curmmreg,cur_stack_offset);

        result:=create_paraloc_info_intern(p,side,p.paras,curintreg,curfloatreg,curmmreg,cur_stack_offset);
        if (p.proccalloption in cstylearrayofconst) then
          { just continue loading the parameters in the registers }
          result:=create_paraloc_info_intern(p,side,varargspara,curintreg,curfloatreg,curmmreg,cur_stack_offset)
        else
          internalerror(2004102302);
      end;

begin
   paramanager:=tcpuparamanager.create;
end.
