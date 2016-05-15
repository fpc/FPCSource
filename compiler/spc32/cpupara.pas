{
    Copyright (c) 2008 by Florian Klaempfl

    SPC32 specific calling conventions

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
unit cpupara;

{$i fpcdefs.inc}

  interface

    uses
       globtype,globals,
       aasmtai,aasmdata,
       cpuinfo,cpubase,cgbase,cgutils,
       symconst,symbase,symtype,symdef,symtable,
       parabase,paramgr;

    type
       tspc32paramanager = class(tparamanager)
          function get_volatile_registers_int(calloption : tproccalloption):tcpuregisterset;override;
          function get_volatile_registers_fpu(calloption : tproccalloption):tcpuregisterset;override;
          function push_addr_param(varspez:tvarspez;def : tdef;calloption : tproccalloption) : boolean;override;
          function ret_in_param(def:tdef;pd:tabstractprocdef):boolean;override;
          function create_paraloc_info(p : tabstractprocdef; side: tcallercallee):longint;override;
          function create_varargs_paraloc_info(p : tabstractprocdef; varargspara:tvarargsparalist):longint;override;
          function  get_funcretloc(p : tabstractprocdef; side: tcallercallee; forcetempdef: tdef): tcgpara;override;
         private
          procedure init_values(var curintreg, curfloatreg, curmmreg: tsuperregister; var cur_stack_offset: aword);
          function create_paraloc_info_intern(p : tabstractprocdef; side: tcallercallee; paras: tparalist;
            var curintreg, curfloatreg, curmmreg: tsuperregister; var cur_stack_offset: aword):longint;
       end;

  implementation

    uses
       verbose,systems,
       rgobj,
       defutil,symsym;


    function tspc32paramanager.get_volatile_registers_int(calloption : tproccalloption):tcpuregisterset;
      begin
        result:=VOLATILE_INTREGISTERS;
      end;


    function tspc32paramanager.get_volatile_registers_fpu(calloption : tproccalloption):tcpuregisterset;
      begin
        result:=VOLATILE_FPUREGISTERS;
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


    function tspc32paramanager.push_addr_param(varspez:tvarspez;def : tdef;calloption : tproccalloption) : boolean;
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


    function tspc32paramanager.ret_in_param(def:tdef;pd:tabstractprocdef):boolean;
      var
        i: longint;
        sym: tsym;
        fpufield: boolean;
      begin
        if handle_common_ret_in_param(def,pd,result) then
          exit;
        case def.typ of
          recorddef:
            begin
              result:=def.size>4;
              if not result and
                 (target_info.abi in [abi_default,abi_armeb]) then
                begin
                  { in case of the old ARM abi (APCS), a struct is returned in
                    a register only if it is simple. And what is a (non-)simple
                    struct:

                    "A non-simple type is any non-floating-point type of size
                     greater than one word (including structures containing only
                     floating-point fields), and certain single-word structured
                     types."
                       (-- ARM APCS documentation)

                    So only floating point types or more than one word ->
                    definitely non-simple (more than one word is already
                    checked above). This includes unions/variant records with
                    overlaid floating point and integer fields.

                    Smaller than one word struct types are simple if they are
                    "integer-like", and:

                    "A structure is termed integer-like if its size is less than
                    or equal to one word, and the offset of each of its
                    addressable subfields is zero."
                      (-- ARM APCS documentation)

                    An "addressable subfield" is a field of which you can take
                    the address, which in practive means any non-bitfield.
                    In Pascal, there is no way to express the difference that
                    you can have in C between "char" and "int :8". In this
                    context, we use the fake distinction that a type defined
                    inside the record itself (such as "a: 0..255;") indicates
                    a bitpacked field while a field using a different type
                    (such as "a: byte;") is not.
                  }
                  for i:=0 to trecorddef(def).symtable.SymList.count-1 do
                    begin
                      sym:=tsym(trecorddef(def).symtable.SymList[i]);
                      if sym.typ<>fieldvarsym then
                        continue;
                      { bitfield -> ignore }
                      if (trecordsymtable(trecorddef(def).symtable).usefieldalignment=bit_alignment) and
                         (tfieldvarsym(sym).vardef.typ in [orddef,enumdef]) and
                         (tfieldvarsym(sym).vardef.owner.defowner=def) then
                        continue;
                      { all other fields must be at offset zero }
                      if tfieldvarsym(sym).fieldoffset<>0 then
                        begin
                          result:=true;
                          exit;
                        end;
                      { floating point field -> also by reference }
                      if tfieldvarsym(sym).vardef.typ=floatdef then
                        begin
                          result:=true;
                          exit;
                        end;
                    end;
                end;
            end;
          procvardef:
            if not tprocvardef(def).is_addressonly then
              result:=true
            else
              result:=false
          else
            result:=inherited ret_in_param(def,pd);
        end;
      end;


    procedure tspc32paramanager.init_values(var curintreg, curfloatreg, curmmreg: tsuperregister; var cur_stack_offset: aword);
      begin
        curintreg:=RS_R0;
        curfloatreg:=RS_INVALID;
        curmmreg:=RS_INVALID;
        cur_stack_offset:=0;
      end;


    { TODO : fix tspc32paramanager.create_paraloc_info_intern }
    function tspc32paramanager.create_paraloc_info_intern(p : tabstractprocdef; side: tcallercallee; paras: tparalist;
        var curintreg, curfloatreg, curmmreg: tsuperregister; var cur_stack_offset: aword):longint;

      var
        nextintreg,nextfloatreg,nextmmreg : tsuperregister;
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
           if (nextintreg<=RS_R2) and
              (not(vo_is_parentfp in hp.varoptions) or
               not(po_delphi_nested_cc in p.procoptions)) then
             begin
               paraloc^.loc:=LOC_REGISTER;
               paraloc^.register:=newreg(R_INTREGISTER,nextintreg,R_SUBWHOLE);
               inc(nextintreg);
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
                paraloc^.register:=NR_R0;
                paraloc^.size:=OS_ADDR;
                paraloc^.def:=voidpointertype;
                break;
              end;

            if push_addr_param(hp.varspez,paradef,p.proccalloption) then
              begin
                paradef:=cpointerdef.getreusable_no_free(paradef);
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
                        paralen:=tcgsize2size[OS_ADDR];
                        paradef:=voidpointertype;
                      end;
                  end
              end;

             hp.paraloc[side].size:=paracgsize;
             hp.paraloc[side].Alignment:=std_param_align;
             hp.paraloc[side].intsize:=paralen;
             hp.paraloc[side].def:=paradef;
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
                 else if (paracgsize in [OS_NO]) then
                   begin
                     paraloc^.size:=OS_32;
                     paraloc^.def:=voidpointertype;
                   end
                 else if (paracgsize in [OS_64,OS_S64]) then
                   begin
                     paraloc^.size:=OS_32;
                     paraloc^.def:=u32inttype;
                   end
                 else
                   begin
                     paraloc^.size:=paracgsize;
                     paraloc^.def:=get_paraloc_def(paradef,paralen,firstparaloc);
                   end;
                 case loc of
                    LOC_REGISTER:
                      begin
                        if nextintreg<=RS_R2 then
                          begin
                            paraloc^.loc:=LOC_REGISTER;
                            paraloc^.register:=newreg(R_INTREGISTER,nextintreg,R_SUBWHOLE);
                            inc(nextintreg);
                          end
                        else
                          begin
                            { LOC_REFERENCE always contains everything that's left }
                            paraloc^.loc:=LOC_REFERENCE;
                            paraloc^.size:=int_cgsize(paralen);
                            paraloc^.def:=carraydef.getreusable_no_free(u8inttype,paralen);
                            if (side=callerside) then
                              paraloc^.reference.index:=NR_STACK_POINTER_REG;
                            paraloc^.reference.offset:=stack_offset;
                            inc(stack_offset,align(paralen,4));
                            paralen:=0;
                         end;
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
                             paraloc^.size:=paracgsize;
                             paraloc^.def:=paradef;
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
                         inc(paraloc^.reference.offset,4);
                       end;
                   end;
                 dec(paralen,tcgsize2size[paraloc^.size]);
                 firstparaloc:=false
               end;
          end;
        curintreg:=nextintreg;
        curfloatreg:=nextfloatreg;
        curmmreg:=nextmmreg;
        cur_stack_offset:=stack_offset;
        result:=cur_stack_offset;
      end;


    function tspc32paramanager.create_paraloc_info(p : tabstractprocdef; side: tcallercallee):longint;
      var
        cur_stack_offset: aword;
        curintreg, curfloatreg, curmmreg: tsuperregister;
        retcgsize  : tcgsize;
      begin
        init_values(curintreg,curfloatreg,curmmreg,cur_stack_offset);

        result:=create_paraloc_info_intern(p,side,p.paras,curintreg,curfloatreg,curmmreg,cur_stack_offset);

        create_funcretloc_info(p,side);
     end;


    { TODO : fix tspc32paramanager.get_funcretloc }
    function  tspc32paramanager.get_funcretloc(p : tabstractprocdef; side: tcallercallee; forcetempdef: tdef): tcgpara;
      var
        paraloc : pcgparalocation;
        retcgsize  : tcgsize;
      begin
         if set_common_funcretloc_info(p,forcetempdef,retcgsize,result) then
           exit;

        paraloc:=result.add_location;
        { Return in FPU register? }
        if result.def.typ=floatdef then
          begin
            //if (p.proccalloption in [pocall_softfloat]) or
            //   (cs_fp_emulation in current_settings.moduleswitches) then
              begin
                case retcgsize of
                  OS_64,
                  OS_F64:
                    begin
                      paraloc^.loc:=LOC_REGISTER;
                      if target_info.endian = endian_big then
                        paraloc^.register:=NR_FUNCTION_RESULT64_HIGH_REG
                      else
                        paraloc^.register:=NR_FUNCTION_RESULT64_LOW_REG;
                      paraloc^.size:=OS_32;
                      paraloc^.def:=u32inttype;
                      paraloc:=result.add_location;
                      paraloc^.loc:=LOC_REGISTER;
                      if target_info.endian = endian_big then
                        paraloc^.register:=NR_FUNCTION_RESULT64_LOW_REG
                      else
                        paraloc^.register:=NR_FUNCTION_RESULT64_HIGH_REG;
                      paraloc^.size:=OS_32;
                      paraloc^.def:=u32inttype;
                    end;
                  OS_32,
                  OS_F32:
                    begin
                      paraloc^.loc:=LOC_REGISTER;
                      paraloc^.register:=NR_FUNCTION_RETURN_REG;
                      paraloc^.size:=OS_32;
                      paraloc^.def:=u32inttype;
                    end;
                  else
                    internalerror(2005082603);
                end;
              end
            {else
              begin
                paraloc^.loc:=LOC_FPUREGISTER;
                paraloc^.register:=NR_FPU_RESULT_REG;
                paraloc^.size:=retcgsize;
                paraloc^.def:=result.def;
              end; }
          end
          { Return in register }
        else
          begin
            if retcgsize in [OS_64,OS_S64] then
              begin
                paraloc^.loc:=LOC_REGISTER;
                if target_info.endian = endian_big then
                  paraloc^.register:=NR_FUNCTION_RESULT64_HIGH_REG
                else
                  paraloc^.register:=NR_FUNCTION_RESULT64_LOW_REG;
                paraloc^.size:=OS_32;
                paraloc^.def:=u32inttype;
                paraloc:=result.add_location;
                paraloc^.loc:=LOC_REGISTER;
                if target_info.endian = endian_big then
                  paraloc^.register:=NR_FUNCTION_RESULT64_LOW_REG
                else
                  paraloc^.register:=NR_FUNCTION_RESULT64_HIGH_REG;
                paraloc^.size:=OS_32;
                paraloc^.def:=u32inttype;
              end
            else
              begin
                paraloc^.loc:=LOC_REGISTER;
                paraloc^.register:=NR_FUNCTION_RETURN_REG;
                case result.IntSize of
                  0:
                    begin
                      paraloc^.loc:=LOC_VOID;
                      paraloc^.register:=NR_NO;
                      paraloc^.size:=OS_NO;
                      paraloc^.def:=voidpointertype;
                    end;
                  3:
                    begin
                      paraloc^.size:=OS_32;
                      paraloc^.def:=u32inttype;
                    end;
                  else
                    begin
                      paraloc^.size:=retcgsize;
                      paraloc^.def:=result.def;
                    end;
                end;
              end;
          end;
      end;


    function tspc32paramanager.create_varargs_paraloc_info(p : tabstractprocdef; varargspara:tvarargsparalist):longint;
      var
        cur_stack_offset: aword;
        curintreg, curfloatreg, curmmreg: tsuperregister;
      begin
        init_values(curintreg,curfloatreg,curmmreg,cur_stack_offset);

        result:=create_paraloc_info_intern(p,callerside,p.paras,curintreg,curfloatreg,curmmreg,cur_stack_offset);
        if (p.proccalloption in cstylearrayofconst) then
          { just continue loading the parameters in the registers }
          result:=create_paraloc_info_intern(p,callerside,varargspara,curintreg,curfloatreg,curmmreg,cur_stack_offset)
        else
          internalerror(200410231);
      end;

begin
   paramanager:=tspc32paramanager.create;
end.
