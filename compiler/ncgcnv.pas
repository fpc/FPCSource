{
    $Id$
    Copyright (c) 2000-2002 by Florian Klaempfl

    Generate assembler for nodes that handle type conversions which are
    the same for all (most) processors

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
unit ncgcnv;

{$i fpcdefs.inc}

interface

    uses
       node,ncnv,types;

    type
       tcgtypeconvnode = class(ttypeconvnode)
         procedure second_int_to_int;override;
         procedure second_cstring_to_pchar;override;
         procedure second_string_to_chararray;override;
         procedure second_array_to_pointer;override;
         procedure second_pointer_to_array;override;
         procedure second_char_to_string;override;
         procedure second_real_to_real;override;
         procedure second_cord_to_pointer;override;
         procedure second_proc_to_procvar;override;
         procedure second_bool_to_int;override;
         procedure second_bool_to_bool;override;
         procedure second_ansistring_to_pchar;override;
         procedure second_class_to_intf;override;
         procedure second_char_to_char;override;
         procedure second_nothing;override;
{$ifdef TESTOBJEXT2}
         procedure checkobject;virtual;
{$endif TESTOBJEXT2}
         procedure second_call_helper(c : tconverttype);virtual;abstract;
         procedure pass_2;override;
       end;

       tcgasnode = class(tasnode)
         procedure pass_2;override;
       end;

  implementation

    uses
      cutils,verbose,
      aasm,symconst,symdef,
      ncon,ncal,
      cpubase,cpuinfo,
      pass_2,
      cginfo,cgbase,
      cgobj,cgcpu,
      ncgutil,
      tgobj,rgobj
      ;


    procedure tcgtypeconvnode.second_int_to_int;
      var
        newsize : tcgsize;
      begin
        newsize:=def_cgsize(resulttype.def);

        { insert range check if not explicit conversion }
        if not(nf_explizit in flags) then
          cg.g_rangecheck(exprasmlist,left,resulttype.def);

        { is the result size smaller ? }
        if resulttype.def.size<>left.resulttype.def.size then
          begin
            { reuse the left location by default }
            location_copy(location,left.location);
            location_force_reg(exprasmlist,location,newsize,false);
          end
        else
          begin
            { no special loading is required, reuse current location }
            location_copy(location,left.location);
            location.size:=newsize;
          end;
      end;


    procedure tcgtypeconvnode.second_cstring_to_pchar;

      var
        hr : treference;

      begin
         location_release(exprasmlist,left.location);
         location_reset(location,LOC_REGISTER,OS_ADDR);
         case tstringdef(left.resulttype.def).string_typ of
           st_shortstring :
             begin
               inc(left.location.reference.offset);
               location.register:=rg.getregisterint(exprasmlist);
               cg.a_loadaddr_ref_reg(exprasmlist,left.location.reference,location.register);
             end;
           st_ansistring :
             begin
               if (left.nodetype=stringconstn) and
                  (str_length(left)=0) then
                begin
                  reference_reset(hr);
                  hr.symbol:=newasmsymbol('FPC_EMPTYCHAR');
                  location.register:=rg.getregisterint(exprasmlist);
                  cg.a_loadaddr_ref_reg(exprasmlist,hr,location.register);
                end
               else
                begin
                  location.register:=rg.getregisterint(exprasmlist);
                  cg.a_load_ref_reg(exprasmlist,OS_ADDR,left.location.reference,location.register);
                end;
             end;
           st_longstring:
             begin
               {!!!!!!!}
               internalerror(8888);
             end;
           st_widestring:
             begin
               if (left.nodetype=stringconstn) and
                  (str_length(left)=0) then
                begin
                  reference_reset(hr);
                  hr.symbol:=newasmsymbol('FPC_EMPTYCHAR');
                  location.register:=rg.getregisterint(exprasmlist);
                  cg.a_loadaddr_ref_reg(exprasmlist,hr,location.register);
                end
               else
                begin
                  location.register:=rg.getregisterint(exprasmlist);
{$warning Todo: convert widestrings to ascii when typecasting them to pchars}
                  cg.a_load_ref_reg(exprasmlist,OS_ADDR,left.location.reference,
                    location.register);
                end;
             end;
         end;
      end;


    procedure tcgtypeconvnode.second_string_to_chararray;

      var
        arrsize: longint;

      begin
         with tarraydef(resulttype.def) do
           arrsize := highrange-lowrange+1;
         if (left.nodetype = stringconstn) and
            { left.length+1 since there's always a terminating #0 character (JM) }
            (tstringconstnode(left).len+1 >= arrsize) and
            (tstringdef(left.resulttype.def).string_typ=st_shortstring) then
           begin
             location_copy(location,left.location);
             inc(location.reference.offset);
             exit;
           end
         else
           { should be handled already in resulttype pass (JM) }
           internalerror(200108292);
      end;


    procedure tcgtypeconvnode.second_array_to_pointer;

      begin
         location_release(exprasmlist,left.location);
         location_reset(location,LOC_REGISTER,OS_ADDR);
         location.register:=rg.getregisterint(exprasmlist);
         cg.a_loadaddr_ref_reg(exprasmlist,left.location.reference,location.register);
      end;


    procedure tcgtypeconvnode.second_pointer_to_array;

      begin
        location_reset(location,LOC_REFERENCE,OS_NO);
        case left.location.loc of
          LOC_REGISTER :
            location.reference.base:=left.location.register;
          LOC_CREGISTER :
            begin
              location.reference.base:=rg.getregisterint(exprasmlist);
              cg.a_load_reg_reg(exprasmlist,OS_ADDR,left.location.register,
                location.reference.base);
            end;
          LOC_REFERENCE,
          LOC_CREFERENCE :
            begin
              location_release(exprasmlist,left.location);
              location.reference.base:=rg.getregisterint(exprasmlist);
              cg.a_load_ref_reg(exprasmlist,OS_ADDR,left.location.reference,
                location.reference.base);
            end;
          else
            internalerror(2002032216);
        end;
      end;


    procedure tcgtypeconvnode.second_char_to_string;
      begin
         location_reset(location,LOC_REFERENCE,OS_NO);
         case tstringdef(resulttype.def).string_typ of
           st_shortstring :
             begin
               tg.gettempofsizereference(exprasmlist,256,location.reference);
               cg.a_load_loc_ref(exprasmlist,left.location,
                 location.reference);
               location_release(exprasmlist,left.location);
             end;
           { the rest is removed in the resulttype pass and converted to compilerprocs }
           else
            internalerror(4179);
        end;
      end;


    procedure tcgtypeconvnode.second_real_to_real;
      begin
         location_reset(location,LOC_FPUREGISTER,def_cgsize(resulttype.def));
         case left.location.loc of
            LOC_FPUREGISTER,
            LOC_CFPUREGISTER:
              begin
                location_copy(location,left.location);
                location.size:=def_cgsize(resulttype.def);
                exit;
              end;
            LOC_CREFERENCE,
            LOC_REFERENCE:
              begin
                 location_release(exprasmlist,left.location);
                 location.register:=rg.getregisterfpu(exprasmlist);
                 cg.a_loadfpu_loc_reg(exprasmlist,left.location,location.register);
              end;
            else
              internalerror(2002032215);
         end;
      end;


    procedure tcgtypeconvnode.second_cord_to_pointer;
      begin
        { this can't happen because constants are already processed in
          pass 1 }
        internalerror(47423985);
      end;


    procedure tcgtypeconvnode.second_proc_to_procvar;

      begin
        { method pointer ? }
        if assigned(tunarynode(left).left) then
          begin
             location_copy(location,left.location);
          end
        else
          begin
             location_release(exprasmlist,left.location);
             location_reset(location,LOC_REGISTER,OS_ADDR);
             location.register:=rg.getregisterint(exprasmlist);
             cg.a_loadaddr_ref_reg(exprasmlist,left.location.reference,location.register);
          end;
      end;


    procedure tcgtypeconvnode.second_bool_to_int;
      var
         oldtruelabel,oldfalselabel : tasmlabel;
      begin
         oldtruelabel:=truelabel;
         oldfalselabel:=falselabel;
         getlabel(truelabel);
         getlabel(falselabel);
         secondpass(left);
         location_copy(location,left.location);
         { byte(boolean) or word(wordbool) or longint(longbool) must }
         { be accepted for var parameters                            }
         if not((nf_explizit in flags) and
                (left.resulttype.def.size=resulttype.def.size) and
                (left.location.loc in [LOC_REFERENCE,LOC_CREFERENCE,LOC_CREGISTER])) then
           location_force_reg(exprasmlist,location,def_cgsize(resulttype.def),false);
         truelabel:=oldtruelabel;
         falselabel:=oldfalselabel;
      end;


    procedure tcgtypeconvnode.second_bool_to_bool;
      begin
        { we can reuse the conversion already available
          in bool_to_int to resize the value. But when the
          size of the new boolean is smaller we need to calculate
          the value as is done in int_to_bool. This is needed because
          the bits that define the true status can be outside the limits
          of the new size and truncating the register can result in a 0
          value }
        if resulttype.def.size<left.resulttype.def.size then
          second_int_to_bool
        else
          second_bool_to_int;
      end;


    procedure tcgtypeconvnode.second_ansistring_to_pchar;
      var
         l1 : tasmlabel;
         hr : treference;
      begin
         location_reset(location,LOC_REGISTER,OS_ADDR);
         getlabel(l1);
         case left.location.loc of
            LOC_CREGISTER,LOC_REGISTER:
              location.register:=left.location.register;
            LOC_CREFERENCE,LOC_REFERENCE:
              begin
                location_release(exprasmlist,left.location);
                location.register:=rg.getregisterint(exprasmlist);
                cg.a_load_ref_reg(exprasmlist,OS_32,left.location.reference,location.register);
              end;
            else
              internalerror(2002032214);
         end;
         cg.a_cmp_const_reg_label(exprasmlist,OS_32,OC_NE,0,location.register,l1);
         reference_reset(hr);
         hr.symbol:=newasmsymbol('FPC_EMPTYCHAR');
         cg.a_loadaddr_ref_reg(exprasmlist,hr,location.register);
         cg.a_label(exprasmlist,l1);
      end;


    procedure tcgtypeconvnode.second_class_to_intf;
      var
         l1 : tasmlabel;
      begin
         location_reset(location,LOC_REGISTER,OS_ADDR);
         case left.location.loc of
            LOC_CREFERENCE,
            LOC_REFERENCE:
              begin
                 location_release(exprasmlist,left.location);
                 location.register:=rg.getregisterint(exprasmlist);
                 cg.a_load_ref_reg(exprasmlist,OS_ADDR,left.location.reference,location.register);
              end;
            LOC_CREGISTER:
              begin
                 location.register:=rg.getregisterint(exprasmlist);
                 cg.a_load_reg_reg(exprasmlist,OS_ADDR,left.location.register,location.register);
              end;
            LOC_REGISTER:
              location.register:=left.location.register;
            else
              internalerror(121120001);
         end;
         getlabel(l1);
         cg.a_cmp_const_reg_label(exprasmlist,OS_ADDR,OC_EQ,0,location.register,l1);
         cg.a_op_const_reg(exprasmlist,OP_ADD,aword(
           tobjectdef(left.resulttype.def).implementedinterfaces.ioffsets(
           tobjectdef(left.resulttype.def).implementedinterfaces.searchintf(
           resulttype.def))^),location.register);
         cg.a_label(exprasmlist,l1);
      end;


    procedure tcgtypeconvnode.second_char_to_char;
      begin
        {$warning todo: add RTL routine for widechar-char conversion }
        { Quick hack to atleast generate 'working' code (PFV) }
        second_int_to_int;
      end;


    procedure tcgtypeconvnode.second_nothing;
      begin
        { we reuse the old value }
        location_copy(location,left.location);

        { Floats should never be returned as LOC_CONSTANT, do the
          moving to memory before the new size is set }
        if (resulttype.def.deftype=floatdef) and
           (location.loc=LOC_CONSTANT) then
         location_force_mem(exprasmlist,location);

        { but use the new size, but we don't know the size of all arrays }
        location.size:=def_cgsize(resulttype.def);
      end;


{$ifdef TESTOBJEXT2}
    procedure tcgtypeconvnode.checkobject;
      begin
        { no checking by default }
      end;
{$endif TESTOBJEXT2}


    procedure tcgtypeconvnode.pass_2;
      begin
        { the boolean routines can be called with LOC_JUMP and
          call secondpass themselves in the helper }
        if not(convtype in [tc_bool_2_int,tc_bool_2_bool,tc_int_2_bool]) then
         begin
           secondpass(left);
           if codegenerror then
            exit;
         end;

        second_call_helper(convtype);

{$ifdef TESTOBJEXT2}
         { Check explicit conversions to objects pointers !! }
         if p^.explizit and
            (p^.resulttype.def.deftype=pointerdef) and
            (tpointerdef(p^.resulttype.def).definition.deftype=objectdef) and not
            (tobjectdef(tpointerdef(p^.resulttype.def).definition).isclass) and
            ((tobjectdef(tpointerdef(p^.resulttype.def).definition).options and oo_hasvmt)<>0) and
            (cs_check_range in aktlocalswitches) then
           checkobject;
{$endif TESTOBJEXT2}
      end;


    procedure tcgasnode.pass_2;
      var
        pushed : tpushedsaved;
      begin
        if (right.nodetype=guidconstn) then
         begin
{$warning need to push a third parameter}
           { instance to check }
           secondpass(left);
           rg.saveusedregisters(exprasmlist,pushed,all_registers);
           cg.a_param_loc(exprasmlist,left.location,2);
           { type information }
           secondpass(right);
           cg.a_paramaddr_ref(exprasmlist,right.location.reference,1);
           location_release(exprasmlist,right.location);
           { call helper }
           if is_class(left.resulttype.def) then
             cg.a_call_name(exprasmlist,'FPC_CLASS_AS_INTF')
           else
             cg.a_call_name(exprasmlist,'FPC_INTF_AS');
           cg.g_maybe_loadself(exprasmlist);
           rg.restoreusedregisters(exprasmlist,pushed);
         end
        else
         begin
           { instance to check }
           secondpass(left);
           rg.saveusedregisters(exprasmlist,pushed,all_registers);
           cg.a_param_loc(exprasmlist,left.location,2);
           { type information }
           secondpass(right);
           cg.a_param_loc(exprasmlist,right.location,1);
           location_release(exprasmlist,right.location);
           { call helper }
           cg.a_call_name(exprasmlist,'FPC_DO_AS');
           cg.g_maybe_loadself(exprasmlist);
           rg.restoreusedregisters(exprasmlist,pushed);
         end;

        location_copy(location,left.location);
      end;


begin
  ctypeconvnode := tcgtypeconvnode;
  casnode := tcgasnode;
end.

{
  $Log$
  Revision 1.16  2002-07-01 16:23:53  peter
    * cg64 patch
    * basics for currency
    * asnode updates for class and interface (not finished)

  Revision 1.15  2002/05/18 13:34:09  peter
    * readded missing revisions

  Revision 1.14  2002/05/16 19:46:37  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.12  2002/05/12 16:53:07  peter
    * moved entry and exitcode to ncgutil and cgobj
    * foreach gets extra argument for passing local data to the
      iterator function
    * -CR checks also class typecasts at runtime by changing them
      into as
    * fixed compiler to cycle with the -CR option
    * fixed stabs with elf writer, finally the global variables can
      be watched
    * removed a lot of routines from cga unit and replaced them by
      calls to cgobj
    * u32bit-s32bit updates for and,or,xor nodes. When one element is
      u32bit then the other is typecasted also to u32bit without giving
      a rangecheck warning/error.
    * fixed pascal calling method with reversing also the high tree in
      the parast, detected by tcalcst3 test

  Revision 1.11  2002/04/21 19:02:03  peter
    * removed newn and disposen nodes, the code is now directly
      inlined from pexpr
    * -an option that will write the secondpass nodes to the .s file, this
      requires EXTDEBUG define to actually write the info
    * fixed various internal errors and crashes due recent code changes

  Revision 1.10  2002/04/19 15:39:34  peter
    * removed some more routines from cga
    * moved location_force_reg/mem to ncgutil
    * moved arrayconstructnode secondpass to ncgld

  Revision 1.9  2002/04/15 19:44:19  peter
    * fixed stackcheck that would be called recursively when a stack
      error was found
    * generic changeregsize(reg,size) for i386 register resizing
    * removed some more routines from cga unit
    * fixed returnvalue handling
    * fixed default stacksize of linux and go32v2, 8kb was a bit small :-)

  Revision 1.8  2002/04/06 18:10:42  jonas
    * several powerpc-related additions and fixes

  Revision 1.7  2002/04/04 19:05:57  peter
    * removed unused units
    * use tlocation.size in cg.a_*loc*() routines

  Revision 1.6  2002/04/02 17:11:28  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods

  Revision 1.5  2002/03/31 20:26:34  jonas
    + a_loadfpu_* and a_loadmm_* methods in tcg
    * register allocation is now handled by a class and is mostly processor
      independent (+rgobj.pas and i386/rgcpu.pas)
    * temp allocation is now handled by a class (+tgobj.pas, -i386\tgcpu.pas)
    * some small improvements and fixes to the optimizer
    * some register allocation fixes
    * some fpuvaroffset fixes in the unary minus node
    * push/popusedregisters is now called rg.save/restoreusedregisters and
      (for i386) uses temps instead of push/pop's when using -Op3 (that code is
      also better optimizable)
    * fixed and optimized register saving/restoring for new/dispose nodes
    * LOC_FPU locations now also require their "register" field to be set to
      R_ST, not R_ST0 (the latter is used for LOC_CFPUREGISTER locations only)
    - list field removed of the tnode class because it's not used currently
      and can cause hard-to-find bugs

}
