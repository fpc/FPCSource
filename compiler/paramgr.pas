{
    $Id$
    Copyright (c) 2002 by Florian Klaempfl

    Generic calling convention handling

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
{# Parameter passing manager. Used to manage how
   parameters are passed to routines.
}
unit paramgr;

{$i fpcdefs.inc}

  interface

    uses
       cpubase,
       symtype,symdef;

    type
       {# This class defines some methods to take care of routine
          parameters. It should be overriden for each new processor
       }
       tparamanager = class
          {# Returns true if the return value can be put in accumulator }
          function ret_in_acc(def : tdef) : boolean;virtual;
          {# Returns true if the return value is put in a register

             Either a floating point register, or a general purpose
             register.
          }
          function ret_in_reg(def : tdef) : boolean;virtual;

          {# Returns true if the return value is actually a parameter
             pointer.
          }
          function ret_in_param(def : tdef) : boolean;virtual;

          function push_high_param(def : tdef) : boolean;virtual;

          {# Returns true if a parameter is too large to copy and only
            the address is pushed
          }
          function push_addr_param(def : tdef;is_cdecl:boolean) : boolean;virtual;
          {# Returns a structure giving the information on
             the storage of the parameter (which must be
             an integer parameter)

             @param(nr Parameter number of routine, starting from 1)
          }
          function getintparaloc(nr : longint) : tparalocation;virtual;abstract;
          procedure create_param_loc_info(p : tabstractprocdef);virtual;abstract;

          {
            Returns the location where the invisible parameter for structured
            function results will be passed.
          }
          function getfuncretparaloc(p : tabstractprocdef) : tparalocation;virtual;

          {
            Returns the location where the invisible parameter for nested
            subroutines is passed.
          }
          function getframepointerloc(p : tabstractprocdef) : tparalocation;virtual;

          { Returns the self pointer location for the given tabstractprocdef,
            when the stack frame is already created. This is used by the code
            generating the wrappers for implemented interfaces.
          }
          function getselflocation(p : tabstractprocdef) : tparalocation;virtual;abstract;

          {
            Returns the location of the result if the result is in
            a register, the register(s) return depend on the type of
            the result.

            @param(def The definition of the result type of the function)
          }
          function getfuncresultloc(def : tdef): tparalocation; virtual;
       end;

    procedure setparalocs(p : tprocdef);
    function getfuncretusedregisters(def : tdef): tregisterset;

    var
       paralocdummy : tparalocation;
       paramanager : tparamanager;

  implementation

    uses
       cpuinfo,globals,globtype,
       symconst,symbase,symsym,
       rgobj,
       defbase,cgbase,cginfo,verbose;

    { true if the return value is in accumulator (EAX for i386), D0 for 68k }
    function tparamanager.ret_in_acc(def : tdef) : boolean;
      begin
         ret_in_acc:=(def.deftype in [orddef,pointerdef,enumdef,classrefdef]) or
                     ((def.deftype=stringdef) and (tstringdef(def).string_typ in [st_ansistring,st_widestring])) or
                     ((def.deftype=procvardef) and not(po_methodpointer in tprocvardef(def).procoptions)) or
                     ((def.deftype=objectdef) and not is_object(def)) or
                     ((def.deftype=setdef) and (tsetdef(def).settype=smallset));
      end;

    function tparamanager.ret_in_reg(def : tdef) : boolean;
      begin
        ret_in_reg:=ret_in_acc(def) or (def.deftype=floatdef);
      end;

    { true if uses a parameter as return value }
    function tparamanager.ret_in_param(def : tdef) : boolean;
      begin
         ret_in_param:=(def.deftype in [arraydef,recorddef]) or
           ((def.deftype=stringdef) and (tstringdef(def).string_typ in [st_shortstring,st_longstring])) or
           ((def.deftype=procvardef) and (po_methodpointer in tprocvardef(def).procoptions)) or
           ((def.deftype=objectdef) and is_object(def)) or
           (def.deftype=variantdef) or
           ((def.deftype=setdef) and (tsetdef(def).settype<>smallset));
      end;


    function tparamanager.push_high_param(def : tdef) : boolean;
      begin
         push_high_param:=is_open_array(def) or
                          is_open_string(def) or
                          is_array_of_const(def);
      end;


    { true if a parameter is too large to copy and only the address is pushed }
    function tparamanager.push_addr_param(def : tdef;is_cdecl:boolean) : boolean;
      begin
        push_addr_param:=false;
        if never_copy_const_param then
         push_addr_param:=true
        else
         begin
           case def.deftype of
             variantdef,
             formaldef :
               push_addr_param:=true;
             recorddef :
               push_addr_param:=(not is_cdecl) and (def.size>pointer_size);
             arraydef :
               push_addr_param:=(
                                 (tarraydef(def).highrange>=tarraydef(def).lowrange) and
                                 (def.size>pointer_size) and
                                 (not is_cdecl)
                                ) or
                                is_open_array(def) or
                                is_array_of_const(def) or
                                is_array_constructor(def);
             objectdef :
               push_addr_param:=is_object(def);
             stringdef :
               push_addr_param:=tstringdef(def).string_typ in [st_shortstring,st_longstring];
             procvardef :
               push_addr_param:=(not is_cdecl) and (po_methodpointer in tprocvardef(def).procoptions);
             setdef :
               push_addr_param:=(not is_cdecl) and (tsetdef(def).settype<>smallset);
           end;
         end;
      end;


    function tparamanager.getfuncretparaloc(p : tabstractprocdef) : tparalocation;
      begin
         result.loc:=LOC_REFERENCE;
         result.size:=OS_ADDR;
         result.sp_fixup:=pointer_size;
         result.reference.index:=stack_pointer_reg;
         result.reference.offset:=0;
      end;


    function tparamanager.getframepointerloc(p : tabstractprocdef) : tparalocation;
      begin
         result.loc:=LOC_REFERENCE;
         result.size:=OS_ADDR;
         result.sp_fixup:=pointer_size;
         result.reference.index:=stack_pointer_reg;
         result.reference.offset:=0;
      end;


    function tparamanager.getfuncresultloc(def : tdef): tparalocation;
      begin
         fillchar(result,sizeof(tparalocation),0);
         if is_void(def) then exit;

         result.size := def_cgsize(def);
         case def.deftype of
           orddef,
           enumdef :
             begin
               result.loc := LOC_REGISTER;
               if result.size in [OS_64,OS_S64] then
                begin
                  result.register64.reghi:=accumulatorhigh;
                  result.register64.reglo:=accumulator;
                end
               else
                  result.register:=accumulator;
             end;
           floatdef :
             begin
               result.loc := LOC_FPUREGISTER;
               if cs_fp_emulation in aktmoduleswitches then
                  result.register := accumulator
               else
                  result.register := FPU_RESULT_REG;
             end;
          else
             begin
                if ret_in_reg(def) then
                  begin
                    result.loc := LOC_REGISTER;
                    result.register := accumulator;
                  end
                else
                   begin
                     result.loc := LOC_REFERENCE;
                     internalerror(2002081602);
(*
{$ifdef EXTDEBUG}
                     { it is impossible to have the
                       return value with an index register
                       and a symbol!
                     }
                     if (ref.index <> R_NO) or (assigned(ref.symbol)) then
                        internalerror(2002081602);
{$endif}
                     result.reference.index := ref.base;
                     result.reference.offset := ref.offset;
*)
                   end;
             end;
          end;
      end;


    function getfuncretusedregisters(def : tdef): tregisterset;
      var
        paramloc : tparalocation;
        regset : tregisterset;
      begin
        regset:=[];
        getfuncretusedregisters:=[];
        { if nothing is returned in registers,
          its useless to continue on in this
          routine
        }
        if not paramanager.ret_in_reg(def) then
          exit;
        paramloc := paramanager.getfuncresultloc(def);
        case paramloc.loc of
          LOC_FPUREGISTER,
          LOC_CFPUREGISTER,
          LOC_MMREGISTER,
          LOC_CMMREGISTER,
          LOC_REGISTER,LOC_CREGISTER :
              begin
                regset := regset + [paramloc.register];
                if ((paramloc.size in [OS_S64,OS_64]) and
                   (sizeof(aword) < 8))
                then
                  begin
                     regset := regset + [paramloc.registerhigh];
                  end;
              end;
          else
            internalerror(20020816);
        end;
        getfuncretusedregisters:=regset;
      end;

    procedure setparalocs(p : tprocdef);

      var
         hp : tparaitem;

      begin
         hp:=tparaitem(p.para.first);
         while assigned(hp) do
           begin
              if (hp.paraloc.loc in [LOC_REGISTER,LOC_FPUREGISTER,
                 LOC_MMREGISTER]) and
                 (
                  (vo_regable in tvarsym(hp.parasym).varoptions) or
                  (vo_fpuregable in tvarsym(hp.parasym).varoptions) or
                   paramanager.push_addr_param(hp.paratype.def,p.proccalloption in [pocall_cdecl,pocall_cppdecl]) or
                   (hp.paratyp in [vs_var,vs_out])
                 ) then
                begin
                   case hp.paraloc.loc of
                     LOC_REGISTER:
                       hp.paraloc.loc := LOC_CREGISTER;
                     LOC_FPUREGISTER:
                       hp.paraloc.loc := LOC_CFPUREGISTER;
{$ifdef SUPPORT_MMX}
                     LOC_MMREGISTER:
                       hp.paraloc.loc := LOC_CMMREGISTER;
{$endif}
                   end;
                   tvarsym(hp.parasym).paraitem:=hp;
                end;
              hp:=tparaitem(hp.next);
           end;
      end;

finalization
  paramanager.free;
end.

{
   $Log$
   Revision 1.18  2002-09-09 09:10:51  florian
     + added generic tparamanager.getframepointerloc

   Revision 1.17  2002/09/07 19:40:39  florian
     * tvarsym.paraitem is set now

   Revision 1.16  2002/09/01 21:04:48  florian
     * several powerpc related stuff fixed

   Revision 1.15  2002/08/25 19:25:19  peter
     * sym.insert_in_data removed
     * symtable.insertvardata/insertconstdata added
     * removed insert_in_data call from symtable.insert, it needs to be
       called separatly. This allows to deref the address calculation
     * procedures now calculate the parast addresses after the procedure
       directives are parsed. This fixes the cdecl parast problem
     * push_addr_param has an extra argument that specifies if cdecl is used
       or not

   Revision 1.14  2002/08/17 22:09:47  florian
     * result type handling in tcgcal.pass_2 overhauled
     * better tnode.dowrite
     * some ppc stuff fixed

   Revision 1.13  2002/08/17 09:23:38  florian
     * first part of procinfo rewrite

   Revision 1.12  2002/08/16 14:24:58  carl
     * issameref() to test if two references are the same (then emit no opcodes)
     + ret_in_reg to replace ret_in_acc
       (fix some register allocation bugs at the same time)
     + save_std_register now has an extra parameter which is the
       usedinproc registers

   Revision 1.11  2002/08/12 15:08:40  carl
     + stab register indexes for powerpc (moved from gdb to cpubase)
     + tprocessor enumeration moved to cpuinfo
     + linker in target_info is now a class
     * many many updates for m68k (will soon start to compile)
     - removed some ifdef or correct them for correct cpu

   Revision 1.10  2002/08/10 17:15:20  jonas
     * register parameters are now LOC_CREGISTER instead of LOC_REGISTER

   Revision 1.9  2002/08/09 07:33:02  florian
     * a couple of interface related fixes

   Revision 1.8  2002/08/06 20:55:21  florian
     * first part of ppc calling conventions fix

   Revision 1.7  2002/08/05 18:27:48  carl
     + more more more documentation
     + first version include/exclude (can't test though, not enough scratch for i386 :()...

   Revision 1.6  2002/07/30 20:50:43  florian
     * the code generator knows now if parameters are in registers

   Revision 1.5  2002/07/26 21:15:39  florian
     * rewrote the system handling

   Revision 1.4  2002/07/20 11:57:55  florian
     * types.pas renamed to defbase.pas because D6 contains a types
       unit so this would conflicts if D6 programms are compiled
     + Willamette/SSE2 instructions to assembler added

   Revision 1.3  2002/07/13 19:38:43  florian
     * some more generic calling stuff fixed

   Revision 1.2  2002/07/13 07:17:15  jonas
     * fixed memory leak reported by  Sergey Korshunoff

   Revision 1.1  2002/07/11 14:41:28  florian
     * start of the new generic parameter handling
}

