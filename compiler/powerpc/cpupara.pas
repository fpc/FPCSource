{
    $Id$
    Copyright (c) 2002 by Florian Klaempfl

    PowerPC specific calling conventions

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
{ PowerPC specific calling conventions are handled by this unit
}
unit cpupara;

{$i fpcdefs.inc}

  interface

    uses
       globtype,
       aasmtai,
       cpubase,
       symconst,symbase,symtype,symdef,paramgr;

    type
       tppcparamanager = class(tparamanager)
          function push_addr_param(def : tdef;calloption : tproccalloption) : boolean;override;
          function getintparaloc(list: taasmoutput; nr : longint) : tparalocation;override;
          procedure freeintparaloc(list: taasmoutput; nr : longint); override;
          procedure create_paraloc_info(p : tabstractprocdef; side: tcallercallee);override;
       end;

  implementation

    uses
       verbose,systems,
       cpuinfo,cginfo,cgbase,
       rgobj,
       defutil,symsym;

    function tppcparamanager.getintparaloc(list: taasmoutput; nr : longint) : tparalocation;

      begin
         fillchar(result,sizeof(tparalocation),0);
         if nr<1 then
           internalerror(2002070801)
         else if nr<=8 then
           begin
              result.loc:=LOC_REGISTER;
              result.register:=newreg(R_INTREGISTER,RS_R2+nr,R_SUBWHOLE);
              rg.getexplicitregisterint(list,result.register);
           end
         else
           begin
              result.loc:=LOC_REFERENCE;
              result.reference.index:=NR_STACK_POINTER_REG;
              result.reference.offset:=(nr-8)*4;
           end;
         result.size := OS_INT;
      end;


    procedure tppcparamanager.freeintparaloc(list: taasmoutput; nr : longint);

      var
        r: tregister;

      begin
         if nr<1 then
           internalerror(2003060401)
         else if nr<=8 then
           begin
             r:=newreg(R_INTREGISTER,RS_R2+nr,R_SUBWHOLE);
             rg.ungetregisterint(list,r);
           end;
      end;


    function getparaloc(p : tdef) : tcgloc;

      begin
         { Later, the LOC_REFERENCE is in most cases changed into LOC_REGISTER
           if push_addr_param for the def is true
         }
         case p.deftype of
            orddef:
              getparaloc:=LOC_REGISTER;
            floatdef:
              getparaloc:=LOC_FPUREGISTER;
            enumdef:
              getparaloc:=LOC_REGISTER;
            pointerdef:
              getparaloc:=LOC_REGISTER;
            formaldef:
              getparaloc:=LOC_REGISTER;
            classrefdef:
              getparaloc:=LOC_REGISTER;
            recorddef:
              getparaloc:=LOC_REFERENCE;
            objectdef:
              if is_object(p) then
                getparaloc:=LOC_REFERENCE
              else
                getparaloc:=LOC_REGISTER;
            stringdef:
              if is_shortstring(p) or is_longstring(p) then
                getparaloc:=LOC_REFERENCE
              else
                getparaloc:=LOC_REGISTER;
            procvardef:
              if (po_methodpointer in tprocvardef(p).procoptions) then
                getparaloc:=LOC_REFERENCE
              else
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
              getparaloc:=LOC_REFERENCE;
            { avoid problems with errornous definitions }
            errordef:
              getparaloc:=LOC_REGISTER;
            else
              internalerror(2002071001);
         end;
      end;

    function tppcparamanager.push_addr_param(def : tdef;calloption : tproccalloption) : boolean;
      begin
        case def.deftype of
          recorddef:
            push_addr_param:=true;
          arraydef:
            push_addr_param:=(tarraydef(def).highrange>=tarraydef(def).lowrange) or
                             is_open_array(def) or
                             is_array_of_const(def) or
                             is_array_constructor(def);
          setdef :
            push_addr_param:=(tsetdef(def).settype<>smallset);
          stringdef :
            push_addr_param:=tstringdef(def).string_typ in [st_shortstring,st_longstring];
          procvardef :
            push_addr_param:=po_methodpointer in tprocvardef(def).procoptions;
          else
            push_addr_param:=inherited push_addr_param(def,calloption);
        end;
      end;

    procedure tppcparamanager.create_paraloc_info(p : tabstractprocdef; side: tcallercallee);

      var
         nextintreg,nextfloatreg,nextmmreg : tsuperregister;
         paradef : tdef;
         paraloc : tparalocation;
         stack_offset : aword;
         hp : tparaitem;
         loc : tcgloc;
         is_64bit: boolean;

      procedure assignintreg;

        begin
           if nextintreg<=NR_R10 then
             begin
                paraloc.loc:=LOC_REGISTER;
                paraloc.register:=newreg(R_INTREGISTER,nextintreg,R_SUBNONE);
                inc(nextintreg);
                if target_info.abi=abi_powerpc_aix then
                  inc(stack_offset,4);
             end
           else
              begin
                 paraloc.loc:=LOC_REFERENCE;
                 paraloc.reference.index:=NR_STACK_POINTER_REG;
                 paraloc.reference.offset:=stack_offset;
                 inc(stack_offset,4);
             end;
        end;

      begin
         nextintreg:=RS_R3;
         nextfloatreg:=RS_F1;
         nextmmreg:=RS_M1;
         case target_info.abi of
           abi_powerpc_aix:
             stack_offset:=24;
           abi_powerpc_sysv:
             stack_offset:=8;
           else
             internalerror(2003051901);
         end;

         { frame pointer for nested procedures? }
         { inc(nextintreg);                     }
         { constructor? }
         { destructor? }
         hp:=tparaitem(p.para.first);
         while assigned(hp) do
           begin
              if (hp.paratyp in [vs_var,vs_out]) then
                begin
                  paradef := voidpointertype.def;
                  loc := LOC_REGISTER;
                end
              else
                begin
                  paradef := hp.paratype.def;
                  loc:=getparaloc(paradef);
                end;
              { make sure all alignment bytes are 0 as well }
              fillchar(paraloc,sizeof(paraloc),0);
              case loc of
                 LOC_REGISTER:
                   begin
                      paraloc.size := def_cgsize(paradef);
                      { for things like formaldef }
                      if paraloc.size = OS_NO then
                        paraloc.size := OS_ADDR;
                      is_64bit := paraloc.size in [OS_64,OS_S64];
                      if nextintreg<=(RS_R10-ord(is_64bit))  then
                        begin
                           paraloc.loc:=LOC_REGISTER;
                           if is_64bit then
                             begin
                               if odd(nextintreg-RS_R3) and (target_info.abi=abi_powerpc_sysv) Then
                                 inc(nextintreg);
                               paraloc.registerhigh:=newreg(R_INTREGISTER,nextintreg,R_SUBNONE);
                               inc(nextintreg);
                               if target_info.abi=abi_powerpc_aix then
                                 inc(stack_offset,4);
                             end;
                           paraloc.registerlow:=newreg(R_INTREGISTER,nextintreg,R_SUBNONE);
                           inc(nextintreg);
                           if target_info.abi=abi_powerpc_aix then
                             inc(stack_offset,4);

                        end
                      else
                         begin
                            nextintreg:=RS_R11;
                            paraloc.loc:=LOC_REFERENCE;
                            paraloc.reference.index:=NR_STACK_POINTER_REG;
                            paraloc.reference.offset:=stack_offset;
                            if not is_64bit then
                              inc(stack_offset,4)
                            else
                              inc(stack_offset,8);
                        end;
                   end;
                 LOC_FPUREGISTER:
                   begin
                      paraloc.size:=def_cgsize(paradef);
                      if nextfloatreg<=RS_F10 then
                        begin
                           paraloc.loc:=LOC_FPUREGISTER;
                           paraloc.register:=newreg(R_FPUREGISTER,nextfloatreg,R_SUBWHOLE);
                           inc(nextfloatreg);
                           if target_info.abi=abi_powerpc_aix then
                             inc(stack_offset,8);
                        end
                      else
                         begin
                            {!!!!!!!}
                            paraloc.size:=def_cgsize(paradef);
                            internalerror(2002071004);
                        end;
                   end;
                 LOC_REFERENCE:
                   begin
                      paraloc.size:=OS_ADDR;
                      if push_addr_param(paradef,p.proccalloption) or
                        is_open_array(paradef) or
                        is_array_of_const(paradef) then
                        assignintreg
                      else
                        begin
                           paraloc.loc:=LOC_REFERENCE;
                           paraloc.reference.index:=NR_STACK_POINTER_REG;
                           paraloc.reference.offset:=stack_offset;
                           inc(stack_offset,hp.paratype.def.size);
                        end;
                   end;
                 else
                   internalerror(2002071002);
              end;
              if side = calleeside then
                begin
                  if (paraloc.loc = LOC_REFERENCE) then
                    paraloc.reference.offset := tvarsym(hp.parasym).adjusted_address;
                end;
              hp.paraloc[side]:=paraloc;
              hp:=tparaitem(hp.next);
           end;

        { Function return }
        fillchar(paraloc,sizeof(tparalocation),0);
        paraloc.size:=def_cgsize(p.rettype.def);
        { Return in FPU register? }
        if p.rettype.def.deftype=floatdef then
          begin
            paraloc.loc:=LOC_FPUREGISTER;
            paraloc.register:=NR_FPU_RESULT_REG;
          end
        else
         { Return in register? }
         if not ret_in_param(p.rettype.def,p.proccalloption) then
          begin
            paraloc.loc:=LOC_REGISTER;
{$ifndef cpu64bit}
            if paraloc.size in [OS_64,OS_S64] then
             begin
               paraloc.register64.reglo:=NR_FUNCTION_RETURN64_LOW_REG;
               paraloc.register64.reghi:=NR_FUNCTION_RETURN64_HIGH_REG;
             end
            else
{$endif cpu64bit}
             paraloc.register:=NR_FUNCTION_RETURN_REG;
          end
        else
          begin
            paraloc.loc:=LOC_REFERENCE;
          end;
        p.funcret_paraloc[side]:=paraloc;
      end;

begin
   paramanager:=tppcparamanager.create;
end.
{
  $Log$
  Revision 1.44  2003-09-03 21:04:14  peter
    * some fixes for ppc

  Revision 1.43  2003/09/03 19:35:24  peter
    * powerpc compiles again

  Revision 1.42  2003/08/11 21:18:20  peter
    * start of sparc support for newra

  Revision 1.41  2003/07/05 20:11:41  jonas
    * create_paraloc_info() is now called separately for the caller and
      callee info
    * fixed ppc cycle

  Revision 1.40  2003/07/02 22:18:04  peter
    * paraloc splitted in callerparaloc,calleeparaloc
    * sparc calling convention updates

  Revision 1.39  2003/06/17 17:27:08  jonas
    - removed allocparaloc/freeparaloc, generic ones are ok now

  Revision 1.38  2003/06/17 16:34:44  jonas
    * lots of newra fixes (need getfuncretparaloc implementation for i386)!
    * renamed all_intregisters to volatile_intregisters and made it
      processor dependent

  Revision 1.37  2003/06/09 14:54:26  jonas
    * (de)allocation of registers for parameters is now performed properly
      (and checked on the ppc)
    - removed obsolete allocation of all parameter registers at the start
      of a procedure (and deallocation at the end)

  Revision 1.36  2003/06/08 10:52:01  jonas
    * zero paraloc tregisters, so that the alignment bytes are 0 (otherwise
      the crc of the ppu files can change between interface and
      implementation)

  Revision 1.35  2003/06/07 18:57:04  jonas
    + added freeintparaloc
    * ppc get/freeintparaloc now check whether the parameter regs are
      properly allocated/deallocated (and get an extra list para)
    * ppc a_call_* now internalerrors if pi_do_call is not yet set
    * fixed lot of missing pi_do_call's

  Revision 1.34  2003/05/30 23:45:49  marco
   * register skipping (aligning) for int64 parameters, sys V abi only.

  Revision 1.33  2003/05/30 22:54:19  marco
   * getfuncretparaloc now uses r3 for highdword and r4 for lo. Doesn't work tho

  Revision 1.32  2003/05/30 22:35:03  marco
   * committed fix that swaps int64 parameters hi and lo.

  Revision 1.31  2003/05/24 11:48:40  jonas
    * added some missing paralocation size settings

  Revision 1.30  2003/05/19 12:15:28  florian
    * fixed calling sequence for subroutines using the aix abi

  Revision 1.29  2003/05/12 20:14:47  florian
    * fixed parameter passing by value of large sets, strings and method pointers

  Revision 1.28  2003/05/11 23:19:32  florian
    * fixed passing of small const arrays and const records, they are always passed by reference

  Revision 1.27  2003/04/26 11:30:59  florian
    * fixed the powerpc to work with the new function result handling

  Revision 1.26  2003/04/23 12:35:35  florian
    * fixed several issues with powerpc
    + applied a patch from Jonas for nested function calls (PowerPC only)
    * ...

  Revision 1.25  2003/04/17 18:52:35  jonas
    * process para's from first to last instead of the other way round

  Revision 1.24  2003/04/16 07:55:07  jonas
    * fixed paralocation for integer var/out parameters

  Revision 1.23  2003/03/11 21:46:24  jonas
    * lots of new regallocator fixes, both in generic and ppc-specific code
      (ppc compiler still can't compile the linux system unit though)

  Revision 1.22  2003/01/09 22:00:53  florian
    * fixed some PowerPC issues

  Revision 1.21  2003/01/09 20:41:10  florian
    * fixed broken PowerPC compiler

  Revision 1.20  2003/01/09 11:22:14  olle
    * made powerpc compiler compile after Daniels Tregister modification

  Revision 1.19  2003/01/08 18:43:58  daniel
   * Tregister changed into a record

  Revision 1.18  2002/12/15 19:22:01  florian
    * fixed some crashes and a rte 201

  Revision 1.17  2002/11/25 17:43:27  peter
    * splitted defbase in defutil,symutil,defcmp
    * merged isconvertable and is_equal into compare_defs(_ext)
    * made operator search faster by walking the list only once

  Revision 1.16  2002/11/18 17:32:01  peter
    * pass proccalloption to ret_in_xxx and push_xxx functions

  Revision 1.15  2002/10/02 13:33:36  jonas
    + set, variant support in getfuncretparaloc

  Revision 1.14  2002/09/28 21:27:16  florian
    + getparaloc supports now sets and variants

  Revision 1.13  2002/09/10 21:28:05  jonas
    * int64 paras are now handled correctly (until the registers are used up
      anyway :)
    * the return location is now initialized correctly
    * fixed bug where ret_in_reg() was called for the procdefinition instead
      of for the result of the procedure

  Revision 1.12  2002/09/09 09:11:37  florian
    - removed passes_parameters_in_reg

  Revision 1.11  2002/09/07 17:54:59  florian
    * first part of PowerPC fixes

  Revision 1.10  2002/09/01 21:04:49  florian
    * several powerpc related stuff fixed

  Revision 1.9  2002/08/31 12:43:31  florian
    * ppc compilation fixed

  Revision 1.8  2002/08/18 10:42:38  florian
    * remaining assembler writer bugs fixed, the errors in the
      system unit are inline assembler problems

  Revision 1.7  2002/08/17 22:09:47  florian
    * result type handling in tcgcal.pass_2 overhauled
    * better tnode.dowrite
    * some ppc stuff fixed

  Revision 1.6  2002/08/13 21:40:58  florian
    * more fixes for ppc calling conventions

  Revision 1.5  2002/07/30 20:50:44  florian
    * the code generator knows now if parameters are in registers

  Revision 1.4  2002/07/28 20:45:22  florian
    + added direct assembler reader for PowerPC

  Revision 1.3  2002/07/26 22:22:10  florian
    * several PowerPC related fixes to get forward with system unit compilation

  Revision 1.2  2002/07/11 14:41:34  florian
    * start of the new generic parameter handling

  Revision 1.1  2002/07/07 09:44:32  florian
    * powerpc target fixed, very simple units can be compiled
}
