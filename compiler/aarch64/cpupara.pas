{
    Copyright (c) 2013-2014 by Jonas Maebe, Florian Klaempfl and others

    AArch64 specific calling conventions

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
{ AArch64 specific calling conventions are handled by this unit
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
       taarch64paramanager = class(tparamanager)
          function get_volatile_registers_int(calloption: tproccalloption): tcpuregisterset; override;
          function get_volatile_registers_fpu(calloption: tproccalloption): tcpuregisterset; override;
          function get_volatile_registers_mm(calloption: tproccalloption): tcpuregisterset; override;
          function push_addr_param(varspez: tvarspez; def: tdef; calloption: tproccalloption): boolean; override;
          function ret_in_param(def: tdef; pd: tabstractprocdef):boolean;override;
          function create_paraloc_info(p: tabstractprocdef; side: tcallercallee):longint;override;
          function create_varargs_paraloc_info(p: tabstractprocdef; varargspara: tvarargsparalist):longint;override;
          function get_funcretloc(p: tabstractprocdef; side: tcallercallee; forcetempdef: tdef): tcgpara;override;
          function param_use_paraloc(const cgpara: tcgpara): boolean; override;
         private
          curintreg,
          curmmreg: tsuperregister;
          curstackoffset: aword;

          procedure init_para_alloc_values;
          procedure alloc_para(out result: tcgpara; p: tabstractprocdef; varspez: tvarspez; side: tcallercallee; paradef: tdef; isvariadic, isdelphinestedcc: boolean);

          procedure create_paraloc_info_intern(p: tabstractprocdef; side: tcallercallee; paras: tparalist; isvariadic: boolean);
       end;

  implementation

    uses
       verbose,systems,cutils,
       rgobj,
       defutil,symsym,symtable;

    const
      RS_FIRST_INT_PARAM_SUPREG = RS_X0;
      RS_LAST_INT_PARAM_SUPREG = RS_X7;
      { Q0/D0/S0/H0/B0 all have the same superregister number }
      RS_FIRST_MM_PARAM_SUPREG = RS_D0;
      RS_LAST_MM_PARAM_SUPREG = RS_D7;


    function taarch64paramanager.get_volatile_registers_int(calloption : tproccalloption):tcpuregisterset;
      begin
        result:=VOLATILE_INTREGISTERS
      end;


    function taarch64paramanager.get_volatile_registers_fpu(calloption : tproccalloption):tcpuregisterset;
      begin
        result:=[];
      end;


    function taarch64paramanager.get_volatile_registers_mm(calloption: tproccalloption): tcpuregisterset;
      begin
        result:=VOLATILE_MMREGISTERS;
      end;


    function is_hfa_internal(p: tdef; var basedef: tdef; var elecount: longint): boolean;
      var
        i: longint;
        sym: tsym;
        tmpelecount: longint;
      begin
        result:=false;
        case p.typ of
          arraydef:
            begin
              if is_special_array(p) then
                exit;
              case tarraydef(p).elementdef.typ of
                floatdef:
                  begin
                    { an array of empty records has no influence }
                    if tarraydef(p).elementdef.size=0 then
                      begin
                        result:=true;
                        exit
                      end;
                    tmpelecount:=0;
                    if not is_hfa_internal(tarraydef(p).elementdef,basedef,tmpelecount) then
                      exit;
                    { tmpelecount now contains the number of hfa elements in a
                      single array element (e.g. 2 if it's an array of a record
                      containing two singles) -> multiply by number of elements
                      in the array }
                    inc(elecount,tarraydef(p).elecount*tmpelecount);
                    if elecount>4 then
                      exit;
                  end;
                else
                  result:=is_hfa_internal(tarraydef(p).elementdef,basedef,elecount);
                end;
            end;
          floatdef:
            begin
              if not assigned(basedef) then
                basedef:=p
              else if basedef<>p then
                exit;
              inc(elecount);
              result:=true;
            end;
          recorddef:
            begin
              for i:=0 to tabstractrecorddef(p).symtable.symlist.count-1 do
                begin
                  sym:=tsym(tabstractrecorddef(p).symtable.symlist[i]);
                  if sym.typ<>fieldvarsym then
                    continue;
                  if not is_hfa_internal(tfieldvarsym(sym).vardef,basedef,elecount) then
                    exit
                end;
              result:=true;
            end;
          else
            exit
        end;
      end;


    { Returns whether a def is a "homogeneous float array" at the machine level.
      This means that in the memory layout, the def only consists of maximally
      4 floating point values that appear consecutively in memory }
    function is_hfa(p: tdef; out basedef: tdef) : boolean;
      var
        elecount: longint;
      begin
        result:=false;
        basedef:=nil;
        elecount:=0;
        result:=is_hfa_internal(p,basedef,elecount);
        result:=
          result and
          (elecount>0) and
          (p.size=basedef.size*elecount)
      end;


    function getparaloc(calloption: tproccalloption; p: tdef): tcgloc;
      var
        hfabasedef: tdef;
      begin
         { Later, the LOC_REFERENCE is in most cases changed into LOC_REGISTER
           if push_addr_param for the def is true
         }
         case p.typ of
            orddef:
              getparaloc:=LOC_REGISTER;
            floatdef:
              getparaloc:=LOC_MMREGISTER;
            enumdef:
              getparaloc:=LOC_REGISTER;
            pointerdef:
              getparaloc:=LOC_REGISTER;
            formaldef:
              getparaloc:=LOC_REGISTER;
            classrefdef:
              getparaloc:=LOC_REGISTER;
            recorddef:
              if not is_hfa(p,hfabasedef) then
                getparaloc:=LOC_REGISTER
              else
                getparaloc:=LOC_MMREGISTER;
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
              if not is_hfa(p,hfabasedef) then
                getparaloc:=LOC_REGISTER
              else
                getparaloc:=LOC_MMREGISTER;
            setdef:
              getparaloc:=LOC_REGISTER;
            variantdef:
              getparaloc:=LOC_REGISTER;
            { avoid problems with errornous definitions }
            errordef:
              getparaloc:=LOC_REGISTER;
            else
              internalerror(2002071001);
         end;
      end;


    function taarch64paramanager.push_addr_param(varspez: tvarspez; def :tdef; calloption: tproccalloption): boolean;
      var
        hfabasedef: tdef;
      begin
        result:=false;
        if varspez in [vs_var,vs_out,vs_constref] then
          begin
            result:=true;
            exit;
          end;
        case def.typ of
          objectdef:
            result:=is_object(def);
          recorddef:
            { ABI: any composite > 16 bytes that not a hfa/hva
              Special case: MWPascal, which passes all const parameters by
                reference for compatibility reasons
            }
            result:=
              ((varspez=vs_const) and
               (calloption=pocall_mwpascal)) or
              (not is_hfa(def,hfabasedef) and
               (def.size>16));
          variantdef,
          formaldef:
            result:=true;
          { arrays are composites and hence treated the same as records by the
            ABI (watch out for C, where an array is a pointer)
            Also: all other platforms pass const arrays by reference. Do the
              same here, because there is too much hacky code out there that
              relies on this ("array[0..0] of x" passed as const parameter and
              then indexed beyond its bounds) }
          arraydef:
            result:=
              (calloption in cdecl_pocalls) or
              is_open_array(def) or
              is_array_of_const(def) or
              is_array_constructor(def) or
              ((tarraydef(def).highrange>=tarraydef(def).lowrange) and
               ((varspez=vs_const) or
                (not is_hfa(def,hfabasedef) and
                 (def.size>16))));
          setdef :
            result:=def.size>16;
          stringdef :
            result:=tstringdef(def).stringtype in [st_shortstring,st_longstring];
        end;
      end;


    function taarch64paramanager.ret_in_param(def: tdef; pd: tabstractprocdef): boolean;
      begin
        if handle_common_ret_in_param(def,pd,result) then
          exit;
        { ABI: if the parameter would be passed in registers, it is returned
            in those registers; otherwise, it's returned by reference }
        result:=push_addr_param(vs_value,def,pd.proccalloption);
      end;


    procedure taarch64paramanager.create_paraloc_info_intern(p : tabstractprocdef; side: tcallercallee; paras: tparalist; isvariadic: boolean);
      var
        hp: tparavarsym;
        i: longint;
      begin
        for i:=0 to paras.count-1 do
          begin
            hp:=tparavarsym(paras[i]);
            { hidden function result parameter is passed in X8 (doesn't have to
              be valid on return) according to the ABI

              -- don't follow the ABI for managed types, because
               a) they are passed in registers as parameters, so we should also
                  return them in a register to be ABI-compliant (which we can't
                  because the entire compiler is built around the idea that
                  they are returned by reference, for ref-counting performance
                  and Delphi-compatibility reasons)
               b) there are hacks in the system unit that expect that you can
                  call
                    function f: com_interface;
                  as
                    procedure p(out o: obj);
                  That can only work in case we do not use x8 to return them
                  from the function, but the regular first parameter register.

              As the ABI says this behaviour is ok for C++ classes with a
              non-trivial copy constructor or destructor, it seems reasonable
              for us to do this for managed types as well.}
            if (vo_is_funcret in hp.varoptions) and
               not is_managed_type(hp.vardef) then
              begin
                hp.paraloc[side].reset;
                hp.paraloc[side].size:=OS_ADDR;
                hp.paraloc[side].alignment:=voidpointertype.alignment;
                hp.paraloc[side].intsize:=voidpointertype.size;
                hp.paraloc[side].def:=getpointerdef(hp.vardef);
                with hp.paraloc[side].add_location^ do
                  begin
                    size:=OS_ADDR;
                    def:=hp.paraloc[side].def;
                    loc:=LOC_REGISTER;
                    register:=NR_X8;
                  end
              end
            else
              alloc_para(hp.paraloc[side],p,hp.varspez,side,hp.vardef,isvariadic,
                (vo_is_parentfp in hp.varoptions) and
                (po_delphi_nested_cc in p.procoptions));
          end;
      end;


    function  taarch64paramanager.get_funcretloc(p : tabstractprocdef; side: tcallercallee; forcetempdef: tdef): tcgpara;
      var
        retcgsize: tcgsize;
      begin
         if set_common_funcretloc_info(p,forcetempdef,retcgsize,result) then
           exit;

         { in this case, it must be returned in registers as if it were passed
           as the first parameter }
         init_para_alloc_values;
         alloc_para(result,p,vs_value,side,result.def,false,false);
         { sanity check (LOC_VOID for empty records) }
         if not assigned(result.location) or
            not(result.location^.loc in [LOC_REGISTER,LOC_MMREGISTER,LOC_VOID]) then
           internalerror(2014113001);
      end;

    function taarch64paramanager.param_use_paraloc(const cgpara: tcgpara): boolean;
      begin
        { we always set up a stack frame -> we can always access the parameters
          this way }
        result:=
          (cgpara.location^.loc=LOC_REFERENCE) and
          not assigned(cgpara.location^.next);
      end;


    procedure taarch64paramanager.init_para_alloc_values;
      begin
        curintreg:=RS_FIRST_INT_PARAM_SUPREG;
        curmmreg:=RS_FIRST_MM_PARAM_SUPREG;
        curstackoffset:=0;
      end;


    procedure taarch64paramanager.alloc_para(out result: tcgpara; p: tabstractprocdef; varspez: tvarspez; side: tcallercallee; paradef: tdef; isvariadic, isdelphinestedcc: boolean);
      var
        hfabasedef, locdef: tdef;
        paraloc: pcgparalocation;
        paralen, stackslotlen: asizeint;
        loc: tcgloc;
        paracgsize, locsize: tcgsize;
        firstparaloc: boolean;
      begin
        result.reset;

        { currently only support C-style array of const,
          there should be no location assigned to the vararg array itself }
        if (p.proccalloption in cstylearrayofconst) and
           is_array_of_const(paradef) then
          begin
            paraloc:=result.add_location;
            { hack: the paraloc must be valid, but is not actually used }
            paraloc^.loc:=LOC_REGISTER;
            paraloc^.register:=NR_X0;
            paraloc^.size:=OS_ADDR;
            exit;
          end;

        if push_addr_param(varspez,paradef,p.proccalloption) then
          begin
            paradef:=getpointerdef(paradef);
            loc:=LOC_REGISTER;
            paracgsize:=OS_ADDR;
            paralen:=tcgsize2size[OS_ADDR];
          end
        else
          begin
            if not is_special_array(paradef) then
              paralen:=paradef.size
            else
              paralen:=tcgsize2size[def_cgsize(paradef)];
            loc:=getparaloc(p.proccalloption,paradef);
            if (paradef.typ in [objectdef,arraydef,recorddef]) and
               not is_special_array(paradef) and
               (varspez in [vs_value,vs_const]) then
              paracgsize:=int_cgsize(paralen)
            else
              begin
                paracgsize:=def_cgsize(paradef);
                { for things like formaldef }
                if paracgsize=OS_NO then
                  begin
                    paracgsize:=OS_ADDR;
                    paralen:=tcgsize2size[OS_ADDR];
                    paradef:=voidpointertype;
                  end;
              end
          end;

          { get hfa basedef if applicable }
          if not is_hfa(paradef,hfabasedef) then
            hfabasedef:=nil;

         result.size:=paracgsize;
         result.alignment:=std_param_align;
         result.intsize:=paralen;
         result.def:=paradef;

         { empty record: skipped (explicitly defined by Apple ABI, undefined
           by general ABI; libffi also skips them in all cases) }
         if not is_special_array(paradef) and
            (paradef.size=0) then
           begin
             paraloc:=result.add_location;
             paraloc^.loc:=LOC_VOID;
             paraloc^.def:=paradef;
             paraloc^.size:=OS_NO;
             exit;
           end;

         { sufficient registers left? }
         case loc of
           LOC_REGISTER:
             begin
               { In case of po_delphi_nested_cc, the parent frame pointer
                 is always passed on the stack. }
               if isdelphinestedcc then
                 loc:=LOC_REFERENCE
               else if curintreg+((paralen-1) shr 3)>RS_LAST_INT_PARAM_SUPREG then
                 begin
                   { not enough integer registers left -> no more register
                     parameters, copy all to stack
                   }
                   curintreg:=succ(RS_LAST_INT_PARAM_SUPREG);
                   loc:=LOC_REFERENCE;
                 end;
             end;
           LOC_MMREGISTER:
             begin;
               { every hfa element must be passed in a separate register }
               if (assigned(hfabasedef) and
                   (curmmreg+(paralen div hfabasedef.size)>RS_LAST_MM_PARAM_SUPREG)) or
                  (curmmreg+((paralen-1) shr 3)>RS_LAST_MM_PARAM_SUPREG) then
                 begin
                   { not enough mm registers left -> no more register
                     parameters, copy all to stack
                   }
                   curmmreg:=succ(RS_LAST_MM_PARAM_SUPREG);
                   loc:=LOC_REFERENCE;
                 end;
             end;
         end;

         { allocate registers/stack locations }
         firstparaloc:=true;
         repeat
           paraloc:=result.add_location;

           { set paraloc size/def }
           if assigned(hfabasedef) then
             begin
               locsize:=def_cgsize(hfabasedef);
               locdef:=hfabasedef;
             end
           { make sure we don't lose whether or not the type is signed }
           else if (loc=LOC_REGISTER) and
                   (paradef.typ<>orddef) then
             begin
               locsize:=int_cgsize(paralen);
               locdef:=get_paraloc_def(paradef,paralen,firstparaloc);
             end
           else
             begin
               locsize:=paracgsize;
               locdef:=paradef;
             end;
           if locsize in [OS_NO,OS_128,OS_S128] then
             begin
               if paralen>4 then
                 begin
                   paraloc^.size:=OS_INT;
                   paraloc^.def:=u64inttype;
                 end
               else
                 begin
                   { for 3-byte records }
                   paraloc^.size:=OS_32;
                   paraloc^.def:=u32inttype;
                 end;
             end
           else
             begin
               paraloc^.size:=locsize;
               paraloc^.def:=locdef;
             end;

           { paraloc loc }
           paraloc^.loc:=loc;

           { assign register/stack address }
           case loc of
             LOC_REGISTER:
               begin
                 paraloc^.register:=newreg(R_INTREGISTER,curintreg,cgsize2subreg(R_INTREGISTER,paraloc^.size));
                 inc(curintreg);
                 dec(paralen,tcgsize2size[paraloc^.size]);

                 { "The general ABI specifies that it is the callee's
                    responsibility to sign or zero-extend arguments having fewer
                    than 32 bits, and that unused bits in a register are
                    unspecified. In iOS, however, the caller must perform such
                    extensions, up to 32 bits." }
                 if (target_info.abi=abi_aarch64_darwin) and
                    (side=callerside) and
                    is_ordinal(paradef) and
                    (paradef.size<4) then
                   paraloc^.size:=OS_32;

                 { in case it's a composite, "The argument is passed as though
                   it had been loaded into the registers from a double-word-
                   aligned address with an appropriate sequence of LDR
                   instructions loading consecutive registers from memory" ->
                   in case of big endian, values in not completely filled
                   registers must be shifted to the top bits }
                 if (target_info.endian=endian_big) and
                    not(paraloc^.size in [OS_64,OS_S64]) and
                    (paradef.typ in [setdef,recorddef,arraydef,objectdef]) then
                   paraloc^.shiftval:=-(8-tcgsize2size[paraloc^.size]);
               end;
             LOC_MMREGISTER:
               begin
                 paraloc^.register:=newreg(R_MMREGISTER,curmmreg,cgsize2subreg(R_MMREGISTER,paraloc^.size));
                 inc(curmmreg);
                 dec(paralen,tcgsize2size[paraloc^.size]);
               end;
             LOC_REFERENCE:
               begin
                  paraloc^.size:=paracgsize;
                  paraloc^.loc:=LOC_REFERENCE;

                  { the current stack offset may not be properly aligned in
                    case we're on Darwin have allocated a non-variadic argument
                    < 8 bytes previously }
                  if target_info.abi=abi_aarch64_darwin then
                    curstackoffset:=align(curstackoffset,paraloc^.def.alignment);

                  { on Darwin, non-variadic arguments take up their actual size
                    on the stack; on other platforms, they take up a multiple of
                    8 bytes }
                  if (target_info.abi=abi_aarch64_darwin) and
                     not isvariadic then
                    stackslotlen:=paralen
                  else
                    stackslotlen:=align(paralen,8);

                  { from the ABI: if arguments occupy partial stack space, they
                    have to occupy the lowest significant bits of a register
                    containing that value which is then stored to memory ->
                    in case of big endian, skip the alignment bytes (if any) }
                  if target_info.endian=endian_little then
                    paraloc^.reference.offset:=curstackoffset
                  else
                    paraloc^.reference.offset:=curstackoffset+stackslotlen-paralen;
                  if side=callerside then
                    paraloc^.reference.index:=NR_STACK_POINTER_REG
                  else
                    begin
                      paraloc^.reference.index:=NR_FRAME_POINTER_REG;
                      inc(paraloc^.reference.offset,16);
                    end;
                  inc(curstackoffset,stackslotlen);
                  paralen:=0
               end;
             else
               internalerror(2002071002);
           end;
         firstparaloc:=false;
         { <=0 for sign/zero-extended locations }
         until paralen<=0;
      end;


    function taarch64paramanager.create_paraloc_info(p: tabstractprocdef; side: tcallercallee):longint;
      begin
        init_para_alloc_values;

        create_paraloc_info_intern(p,side,p.paras,false);
        result:=curstackoffset;

        create_funcretloc_info(p,side);
     end;


    function taarch64paramanager.create_varargs_paraloc_info(p: tabstractprocdef; varargspara: tvarargsparalist):longint;
      begin
        init_para_alloc_values;

        { non-variadic parameters }
        create_paraloc_info_intern(p,callerside,p.paras,false);
        if p.proccalloption in cstylearrayofconst then
          begin
            { on Darwin, we cannot use any registers for variadic parameters }
            if target_info.abi=abi_aarch64_darwin then
              begin
                curintreg:=succ(RS_LAST_INT_PARAM_SUPREG);
                curmmreg:=succ(RS_LAST_MM_PARAM_SUPREG);
              end;
            { continue loading the parameters  }
            create_paraloc_info_intern(p,callerside,varargspara,true);
            result:=curstackoffset;
          end
        else
          internalerror(200410231);
      end;

begin
   paramanager:=taarch64paramanager.create;
end.
