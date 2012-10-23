{
    Copyright (c) 2003 by Florian Klaempfl

    avr32 specific calling conventions
     - (R12,R10,R9,R8,FP[4],FP[8]...)

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
{ avr32 specific calling conventions are handled by this unit
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
       tavr32paramanager = class(tparamanager)
          function get_volatile_registers_int(calloption : tproccalloption):tcpuregisterset;override;
          function push_addr_param(varspez:tvarspez;def : tdef;calloption : tproccalloption) : boolean;override;
          function ret_in_param(def : tdef;calloption : tproccalloption) : boolean;override;
          procedure getintparaloc(calloption : tproccalloption; nr : longint;var cgpara:TCGPara);override;
          function create_paraloc_info(p : tabstractprocdef; side: tcallercallee):longint;override;
          function create_varargs_paraloc_info(p : tabstractprocdef; varargspara:tvarargsparalist):longint;override;
          function get_funcretloc(p : tabstractprocdef; side: tcallercallee; def: tdef): tcgpara;override;
         private
          procedure init_values(var curintreg: tsuperregister; var cur_stack_offset: aword);
          function create_paraloc_info_intern(p : tabstractprocdef; side: tcallercallee; paras: tparalist; var curintreg: tsuperregister; var cur_stack_offset: aword):longint;
          procedure create_funcretloc_info(p : tabstractprocdef; side: tcallercallee);
       end;

  implementation

    uses
       verbose,systems,cutils,
       rgobj,
       defutil,symsym;


    function tavr32paramanager.get_volatile_registers_int(calloption : tproccalloption):tcpuregisterset;
      begin
        result:=VOLATILE_INTREGISTERS;
      end;


    procedure tavr32paramanager.getintparaloc(calloption : tproccalloption; nr : longint;var cgpara:TCGPara);
      var
        paraloc : pcgparalocation;
      begin
        if nr<1 then
          internalerror(2002070801);
        cgpara.reset;
        cgpara.size:=OS_ADDR;
        cgpara.intsize:=sizeof(pint);
        cgpara.alignment:=std_param_align;
        paraloc:=cgpara.add_location;
        with paraloc^ do
          begin
            size:=OS_INT;
            { the four first parameters are passed into registers }
            if nr<=4 then
              begin
                loc:=LOC_REGISTER;
                register:=newreg(R_INTREGISTER,RS_R12-nr+1,R_SUBWHOLE);
              end
            else
              begin
                { the other parameters are passed on the stack }
                loc:=LOC_REFERENCE;
                reference.index:=NR_STACK_POINTER_REG;
                reference.offset:=(nr-6)*4;
              end;
          end;
      end;


    function getparaloc(calloption : tproccalloption; p : tdef) : tcgloc;
      begin
         { Later, the LOC_REFERENCE is in most cases changed into LOC_REGISTER
           if push_addr_param for the def is true
         }
         case p.typ of
            orddef:
              getparaloc:=LOC_REGISTER;
            floatdef:
              getparaloc:=LOC_REGISTER;
            enumdef:
              getparaloc:=LOC_REGISTER;
            pointerdef:
              getparaloc:=LOC_REGISTER;
            formaldef:
              getparaloc:=LOC_REGISTER;
            classrefdef:
              getparaloc:=LOC_REGISTER;
            recorddef:
              getparaloc:=LOC_REGISTER;
            objectdef:
              getparaloc:=LOC_REGISTER;
            stringdef:
              if is_shortstring(p) or is_longstring(p) then
                getparaloc:=LOC_REFERENCE
              else
                getparaloc:=LOC_REGISTER;
            procvardef:
              getparaloc:=LOC_REGISTER;
            filedef:
              getparaloc:=LOC_REGISTER;
            arraydef:
              getparaloc:=LOC_REFERENCE;
            setdef:
              if is_smallset(p) then
                getparaloc:=LOC_REGISTER
              else
                getparaloc:=LOC_REFERENCE;
            variantdef:
              getparaloc:=LOC_REGISTER;
            { avoid problems with errornous definitions }
            errordef:
              getparaloc:=LOC_REGISTER;
            else
              internalerror(2002071001);
         end;
      end;


    function tavr32paramanager.push_addr_param(varspez:tvarspez;def : tdef;calloption : tproccalloption) : boolean;
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
            { note: should this ever be changed, make sure that const records
                are always passed by reference for calloption=pocall_mwpascal }
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
        end;
      end;


    function tavr32paramanager.ret_in_param(def : tdef;calloption : tproccalloption) : boolean;
      begin
        case def.typ of
          recorddef:
            result:=def.size>4;
          procvardef:
            if not tprocvardef(def).is_addressonly then
              result:=true
            else
              result:=false
          else
            result:=inherited ret_in_param(def,calloption);
        end;
      end;


    procedure tavr32paramanager.init_values(var curintreg: tsuperregister; var cur_stack_offset: aword);
      begin
        curintreg:=RS_R12;
        cur_stack_offset:=0;
      end;


    function tavr32paramanager.create_paraloc_info_intern(p : tabstractprocdef; side: tcallercallee; paras: tparalist;
        var curintreg: tsuperregister; var cur_stack_offset: aword):longint;

      var
        nextintreg : tsuperregister;
        paradef : tdef;
        paraloc : pcgparalocation;
        stack_offset : aword;
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
           if (nextintreg>=RS_R8) and
              (not(vo_is_parentfp in hp.varoptions) or
               not(po_delphi_nested_cc in p.procoptions)) then
             begin
               paraloc^.loc:=LOC_REGISTER;
               paraloc^.register:=newreg(R_INTREGISTER,nextintreg,R_SUBWHOLE);
               dec(nextintreg);
             end
           else
             begin
               paraloc^.loc:=LOC_REFERENCE;
               paraloc^.reference.index:=NR_STACK_POINTER_REG;
               paraloc^.reference.offset:=stack_offset;
               inc(stack_offset,4);
            end;
        end;


      begin
        result:=0;
        nextintreg:=curintreg;
        stack_offset:=cur_stack_offset;

        for i:=0 to paras.count-1 do
          begin
            hp:=tparavarsym(paras[i]);
            paradef:=hp.vardef;

            hp.paraloc[side].reset;

            { currently only support C-style array of const,
              there should be no location assigned to the vararg array itself }
            if (p.proccalloption in [pocall_cdecl,pocall_cppdecl]) and
               is_array_of_const(paradef) then
              begin
                paraloc:=hp.paraloc[side].add_location;
                { hack: the paraloc must be valid, but is not actually used }
                paraloc^.loc:=LOC_REGISTER;
                paraloc^.register:=NR_R0;
                paraloc^.size:=OS_ADDR;
                break;
              end;

            if (hp.varspez in [vs_var,vs_out]) or
               push_addr_param(hp.varspez,paradef,p.proccalloption) or
               is_open_array(paradef) or
               is_array_of_const(paradef) then
              begin
                paradef:=voidpointertype;
                loc:=LOC_REGISTER;
                paracgsize := OS_ADDR;
                paralen := tcgsize2size[OS_ADDR];
              end
            else
              begin
                if not is_special_array(paradef) then
                  paralen := paradef.size
                else
                  paralen := tcgsize2size[def_cgsize(paradef)];
                loc := getparaloc(p.proccalloption,paradef);
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
                        paralen := tcgsize2size[OS_ADDR];
                      end;
                  end
              end;

             hp.paraloc[side].size:=paracgsize;
             hp.paraloc[side].Alignment:=std_param_align;
             hp.paraloc[side].intsize:=paralen;
             firstparaloc:=true;

{$ifdef EXTDEBUG}
             if paralen=0 then
               internalerror(200410311);
{$endif EXTDEBUG}
             while paralen>0 do
               begin
                 paraloc:=hp.paraloc[side].add_location;

                 if (loc=LOC_REGISTER) and (paracgsize in [OS_F32,OS_F64,OS_F80]) then
                   case paracgsize of
                     OS_F32:
                       paraloc^.size:=OS_32;
                     OS_F64:
                       paraloc^.size:=OS_32;
                     else
                       internalerror(2005082901);
                   end
                 else if (paracgsize in [OS_NO,OS_64,OS_S64]) then
                   paraloc^.size := OS_32
                 else
                   paraloc^.size:=paracgsize;
                 case loc of
                    LOC_REGISTER:
                      begin
                        if nextintreg>=RS_R8 then
                          begin
                            paraloc^.loc:=LOC_REGISTER;
                            paraloc^.register:=newreg(R_INTREGISTER,nextintreg,R_SUBWHOLE);
                            dec(nextintreg);
                          end
                        else
                          begin
                            { LOC_REFERENCE always contains everything that's left }
                            paraloc^.loc:=LOC_REFERENCE;
                            paraloc^.size:=int_cgsize(paralen);
                            if (side=callerside) then
                              paraloc^.reference.index:=NR_STACK_POINTER_REG;
                            paraloc^.reference.offset:=stack_offset;
                            inc(stack_offset,align(paralen,4));
                            paralen:=0;
                         end;
                      end;
                    LOC_FPUREGISTER:
                      begin
                        paraloc^.loc:=LOC_REFERENCE;
                        paraloc^.reference.index:=NR_STACK_POINTER_REG;
                        paraloc^.reference.offset:=stack_offset;
                        case paraloc^.size of
                          OS_F32:
                            inc(stack_offset,4);
                          OS_F64:
                            inc(stack_offset,8);
                          OS_F80:
                            inc(stack_offset,10);
                          OS_F128:
                            inc(stack_offset,16);
                          else
                            internalerror(200403201);
                          end;
                      end;
                    LOC_REFERENCE:
                      begin
                        if push_addr_param(hp.varspez,paradef,p.proccalloption) then
                          begin
                            paraloc^.size:=OS_ADDR;
                            assignintreg
                          end
                        else
                          begin
                             paraloc^.size:=paracgsize;
                             paraloc^.loc:=LOC_REFERENCE;
                             paraloc^.reference.index:=NR_STACK_POINTER_REG;
                             paraloc^.reference.offset:=stack_offset;
                             inc(stack_offset,align(paralen,4));
                             paralen:=0
                          end;
                      end;
                    else
                      internalerror(2002071002);
                 end;
                 if side=calleeside then
                   begin
                     if paraloc^.loc=LOC_REFERENCE then
                       begin
                         paraloc^.reference.index:=NR_FRAME_POINTER_REG;
                         //inc(paraloc^.reference.offset,4);
                       end;
                   end;
                 dec(paralen,tcgsize2size[paraloc^.size]);
                 firstparaloc:=false
               end;
          end;
        curintreg:=nextintreg;
        cur_stack_offset:=stack_offset;
        result:=cur_stack_offset;
      end;


    procedure tavr32paramanager.create_funcretloc_info(p : tabstractprocdef; side: tcallercallee);
      begin
        p.funcretloc[side]:=get_funcretloc(p,side,p.returndef);
      end;


    function  tavr32paramanager.get_funcretloc(p : tabstractprocdef; side: tcallercallee; def: tdef): tcgpara;
      var
        paraloc : pcgparalocation;
        retcgsize  : tcgsize;
      begin
        result.init;
        result.alignment:=get_para_align(p.proccalloption);
        { void has no location }
        if is_void(def) then
          begin
            paraloc:=result.add_location;
            result.size:=OS_NO;
            result.intsize:=0;
            paraloc^.size:=OS_NO;
            paraloc^.loc:=LOC_VOID;
            exit;
          end;
        { Constructors return self instead of a boolean }
        if (p.proctypeoption=potype_constructor) then
          begin
            retcgsize:=OS_ADDR;
            result.intsize:=sizeof(pint);
          end
        else
          begin
            retcgsize:=def_cgsize(def);
            result.intsize:=def.size;
          end;
        result.size:=retcgsize;
        { Return is passed as var parameter }
        if ret_in_param(def,p.proccalloption) then
          begin
            paraloc:=result.add_location;
            paraloc^.loc:=LOC_REFERENCE;
            paraloc^.size:=retcgsize;
            exit;
          end;

        paraloc:=result.add_location;
        { Return in FPU register? }
        if def.typ=floatdef then
          begin
            case retcgsize of
              OS_64,
              OS_F64:
                begin
                  paraloc^.loc:=LOC_REGISTER;
                  paraloc^.register:=NR_FUNCTION_RESULT64_LOW_REG;
                  paraloc^.size:=OS_32;
                  paraloc:=result.add_location;
                  paraloc^.loc:=LOC_REGISTER;
                  paraloc^.register:=NR_FUNCTION_RESULT64_HIGH_REG;
                  paraloc^.size:=OS_32;
                end;
              OS_32,
              OS_F32:
                begin
                  paraloc^.loc:=LOC_REGISTER;
                  paraloc^.register:=NR_FUNCTION_RETURN_REG;
                  paraloc^.size:=OS_32;
                end;
              else
                internalerror(2005082603);
            end;
          end
          { Return in register }
        else
          begin
            if retcgsize in [OS_64,OS_S64] then
              begin
                paraloc^.loc:=LOC_REGISTER;
                paraloc^.register:=NR_FUNCTION_RESULT64_LOW_REG;
                paraloc^.size:=OS_32;
                paraloc:=result.add_location;
                paraloc^.loc:=LOC_REGISTER;
                paraloc^.register:=NR_FUNCTION_RESULT64_HIGH_REG;
                paraloc^.size:=OS_32;
              end
            else
              begin
                paraloc^.loc:=LOC_REGISTER;
                paraloc^.register:=NR_FUNCTION_RETURN_REG;
                if (result.intsize<>3) then
                  paraloc^.size:=retcgsize
                else
                  paraloc^.size:=OS_32;
              end;
          end;
      end;


    function tavr32paramanager.create_paraloc_info(p : tabstractprocdef; side: tcallercallee):longint;
      var
        cur_stack_offset: aword;
        curintreg: tsuperregister;
      begin
        init_values(curintreg,cur_stack_offset);

        result:=create_paraloc_info_intern(p,side,p.paras,curintreg,cur_stack_offset);

        create_funcretloc_info(p,side);
     end;


    function tavr32paramanager.create_varargs_paraloc_info(p : tabstractprocdef; varargspara:tvarargsparalist):longint;
      var
        cur_stack_offset: aword;
        curintreg: tsuperregister;
      begin
        init_values(curintreg,cur_stack_offset);

        result:=create_paraloc_info_intern(p,callerside,p.paras,curintreg,cur_stack_offset);
        if (p.proccalloption in [pocall_cdecl,pocall_cppdecl]) then
          { just continue loading the parameters in the registers }
          result:=create_paraloc_info_intern(p,callerside,varargspara,curintreg,cur_stack_offset)
        else
          internalerror(200410231);
      end;

begin
   paramanager:=tavr32paramanager.create;
end.
