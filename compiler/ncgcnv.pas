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
       node,ncnv,defutil,defcmp;

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
      cutils,verbose,globtype,
      aasmbase,aasmtai,aasmcpu,symconst,symdef,paramgr,
      ncon,ncal,
      cpubase,cpuinfo,systems,
      pass_2,
      cginfo,cgbase,
      cgobj,
      ncgutil,
      tgobj,rgobj
      ;


    procedure tcgtypeconvnode.second_int_to_int;
      var
        newsize : tcgsize;
        ressize,
        leftsize : longint;
      begin
        newsize:=def_cgsize(resulttype.def);

        { insert range check if not explicit conversion }
        if not(nf_explicit in flags) then
          cg.g_rangecheck(exprasmlist,left,resulttype.def);

        { is the result size smaller? when typecasting from void
          we always reuse the current location, because there is
          nothing that we can load in a register }
        ressize := resulttype.def.size;
        leftsize := left.resulttype.def.size;
        if (ressize<>leftsize) and
           not is_void(left.resulttype.def) then
          begin
            location_copy(location,left.location);
            { reuse a loc_reference when the newsize is smaller than
              than the original, else load it to a register }
            if (location.loc in [LOC_REFERENCE,LOC_CREFERENCE]) and
               (ressize<leftsize) then
             begin
               location.size:=newsize;
               if (target_info.endian = ENDIAN_BIG) then
                 inc(location.reference.offset,leftsize-ressize);
             end
            else
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
               location.register:=rg.getaddressregister(exprasmlist);
               cg.a_loadaddr_ref_reg(exprasmlist,left.location.reference,location.register);
             end;
           st_ansistring :
             begin
               if (left.nodetype=stringconstn) and
                  (str_length(left)=0) then
                begin
                  reference_reset(hr);
                  hr.symbol:=objectlibrary.newasmsymboldata('FPC_EMPTYCHAR');
                  location.register:=rg.getaddressregister(exprasmlist);
                  cg.a_loadaddr_ref_reg(exprasmlist,hr,location.register);
                end
               else
                begin
                  location.register:=rg.getaddressregister(exprasmlist);
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
                  hr.symbol:=objectlibrary.newasmsymboldata('FPC_EMPTYCHAR');
                  location.register:=rg.getaddressregister(exprasmlist);
                  cg.a_loadaddr_ref_reg(exprasmlist,hr,location.register);
                end
               else
                begin
                  location.register:=rg.getregisterint(exprasmlist,OS_INT);
{$ifdef fpc}
{$warning Todo: convert widestrings to ascii when typecasting them to pchars}
{$endif}
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
         location.register:=rg.getaddressregister(exprasmlist);
         cg.a_loadaddr_ref_reg(exprasmlist,left.location.reference,location.register);
      end;


    procedure tcgtypeconvnode.second_pointer_to_array;

      begin
        location_reset(location,LOC_REFERENCE,OS_NO);
        case left.location.loc of
          LOC_REGISTER :
            begin
              if not rg.isaddressregister(left.location.register) then
                begin
                  location_release(exprasmlist,left.location);
                  location.reference.base:=rg.getaddressregister(exprasmlist);
                  cg.a_load_reg_reg(exprasmlist,OS_ADDR,OS_ADDR,
                          left.location.register,location.reference.base);
                end
              else
                location.reference.base := left.location.register;
            end;
          LOC_CREGISTER :
            begin
              location.reference.base:=rg.getaddressregister(exprasmlist);
              cg.a_load_reg_reg(exprasmlist,OS_ADDR,OS_ADDR,left.location.register,
                location.reference.base);
            end;
          LOC_REFERENCE,
          LOC_CREFERENCE :
            begin
              location_release(exprasmlist,left.location);
              location.reference.base:=rg.getaddressregister(exprasmlist);
              cg.a_load_ref_reg(exprasmlist,OS_ADDR,left.location.reference,
                location.reference.base);
              location_freetemp(exprasmlist,left.location);
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
               tg.GetTemp(exprasmlist,256,tt_normal,location.reference);
               cg.a_load_loc_ref(exprasmlist,left.location,
                 location.reference);
               location_release(exprasmlist,left.location);
               location_freetemp(exprasmlist,left.location);
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
                 location_freetemp(exprasmlist,left.location);
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
             location.register:=rg.getaddressregister(exprasmlist);
             cg.a_loadaddr_ref_reg(exprasmlist,left.location.reference,location.register);
          end;
      end;


    procedure tcgtypeconvnode.second_bool_to_int;
      var
         oldtruelabel,oldfalselabel : tasmlabel;
      begin
         oldtruelabel:=truelabel;
         oldfalselabel:=falselabel;
         objectlibrary.getlabel(truelabel);
         objectlibrary.getlabel(falselabel);
         secondpass(left);
         location_copy(location,left.location);
         { byte(boolean) or word(wordbool) or longint(longbool) must }
         { be accepted for var parameters                            }
         if not((nf_explicit in flags) and
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
         objectlibrary.getlabel(l1);
         case left.location.loc of
            LOC_CREGISTER,LOC_REGISTER:
              begin
                 if not rg.isaddressregister(left.location.register) then
                   begin
                     location_release(exprasmlist,left.location);
                     location.register:=rg.getaddressregister(exprasmlist);
                     cg.a_load_reg_reg(exprasmlist,OS_ADDR,OS_ADDR,
                              left.location.register,location.register);
                   end
                 else
                    location.register := left.location.register;
              end;
            LOC_CREFERENCE,LOC_REFERENCE:
              begin
                location_release(exprasmlist,left.location);
                location.register:=rg.getaddressregister(exprasmlist);
                cg.a_load_ref_reg(exprasmlist,OS_32,left.location.reference,location.register);
                location_freetemp(exprasmlist,left.location);
              end;
            else
              internalerror(2002032214);
         end;
         cg.a_cmp_const_reg_label(exprasmlist,OS_32,OC_NE,0,location.register,l1);
         reference_reset(hr);
         hr.symbol:=objectlibrary.newasmsymboldata('FPC_EMPTYCHAR');
         cg.a_loadaddr_ref_reg(exprasmlist,hr,location.register);
         cg.a_label(exprasmlist,l1);
      end;


    procedure tcgtypeconvnode.second_class_to_intf;
      var
         l1 : tasmlabel;
         hd : tobjectdef;
      begin
         location_reset(location,LOC_REGISTER,OS_ADDR);
         case left.location.loc of
            LOC_CREFERENCE,
            LOC_REFERENCE:
              begin
                 location_release(exprasmlist,left.location);
                 location.register:=rg.getaddressregister(exprasmlist);
                 cg.a_load_ref_reg(exprasmlist,OS_ADDR,left.location.reference,location.register);
                 location_freetemp(exprasmlist,left.location);
              end;
            LOC_CREGISTER:
              begin
                 location.register:=rg.getaddressregister(exprasmlist);
                 cg.a_load_reg_reg(exprasmlist,OS_ADDR,OS_ADDR,left.location.register,location.register);
              end;
            LOC_REGISTER:
              location.register:=left.location.register;
            else
              internalerror(121120001);
         end;
         objectlibrary.getlabel(l1);
         cg.a_cmp_const_reg_label(exprasmlist,OS_ADDR,OC_EQ,0,location.register,l1);
         hd:=tobjectdef(left.resulttype.def);
         while assigned(hd) do
           begin
              if hd.implementedinterfaces.searchintf(resulttype.def)<>-1 then
                begin
                   cg.a_op_const_reg(exprasmlist,OP_ADD,aword(
                     hd.implementedinterfaces.ioffsets(
                     hd.implementedinterfaces.searchintf(
                     resulttype.def))^),location.register);
                   break;
                end;
              hd:=hd.childof;
           end;
         if hd=nil then
           internalerror(2002081301);
         cg.a_label(exprasmlist,l1);
      end;


    procedure tcgtypeconvnode.second_char_to_char;
      begin
{$ifdef fpc}
        {$warning todo: add RTL routine for widechar-char conversion }
{$endif}
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
      begin
        secondpass(call);
        location_copy(location,call.location);
      end;


begin
  ctypeconvnode := tcgtypeconvnode;
  casnode := tcgasnode;
end.

{
  $Log$
  Revision 1.40  2003-05-23 14:27:35  peter
    * remove some unit dependencies
    * current_procinfo changes to store more info

  Revision 1.39  2003/04/22 23:50:22  peter
    * firstpass uses expectloc
    * checks if there are differences between the expectloc and
      location.loc from secondpass in EXTDEBUG

  Revision 1.38  2003/04/06 21:11:23  olle
    * changed newasmsymbol to newasmsymboldata for data symbols

  Revision 1.37  2003/03/28 19:16:56  peter
    * generic constructor working for i386
    * remove fixed self register
    * esi added as address register for i386

  Revision 1.36  2003/02/19 22:00:14  daniel
    * Code generator converted to new register notation
    - Horribily outdated todo.txt removed

  Revision 1.35  2003/01/02 22:20:51  peter
    * fix typecasts from void to int

  Revision 1.34  2002/11/25 17:43:17  peter
    * splitted defbase in defutil,symutil,defcmp
    * merged isconvertable and is_equal into compare_defs(_ext)
    * made operator search faster by walking the list only once

  Revision 1.33  2002/10/05 12:43:25  carl
    * fixes for Delphi 6 compilation
     (warning : Some features do not work under Delphi)

  Revision 1.32  2002/09/17 18:54:02  jonas
    * a_load_reg_reg() now has two size parameters: source and dest. This
      allows some optimizations on architectures that don't encode the
      register size in the register name.

  Revision 1.31  2002/09/16 13:08:44  jonas
    * big endian fix for second_int_to_int

  Revision 1.30  2002/09/07 15:25:02  peter
    * old logs removed and tabs fixed

  Revision 1.29  2002/09/02 18:46:00  peter
    * reuse a reference when resizing ordinal values to smaller sizes,
      this is required for constructions like byte(w):=1 that are
      allowed in tp mode only

  Revision 1.28  2002/08/25 09:06:58  peter
    * add calls to release temps

  Revision 1.27  2002/08/23 16:14:48  peter
    * tempgen cleanup
    * tt_noreuse temp type added that will be used in genentrycode

  Revision 1.26  2002/08/20 18:23:32  jonas
    * the as node again uses a compilerproc
    + (untested) support for interface "as" statements

  Revision 1.25  2002/08/13 18:01:52  carl
    * rename swatoperands to swapoperands
    + m68k first compilable version (still needs a lot of testing):
        assembler generator, system information , inline
        assembler reader.

  Revision 1.24  2002/08/12 20:39:17  florian
    * casting of classes to interface fixed when the interface was
      implemented by a parent class

  Revision 1.23  2002/08/11 14:32:26  peter
    * renamed current_library to objectlibrary

  Revision 1.22  2002/08/11 13:24:11  peter
    * saving of asmsymbols in ppu supported
    * asmsymbollist global is removed and moved into a new class
      tasmlibrarydata that will hold the info of a .a file which
      corresponds with a single module. Added librarydata to tmodule
      to keep the library info stored for the module. In the future the
      objectfiles will also be stored to the tasmlibrarydata class
    * all getlabel/newasmsymbol and friends are moved to the new class

  Revision 1.21  2002/07/20 11:57:53  florian
    * types.pas renamed to defbase.pas because D6 contains a types
      unit so this would conflicts if D6 programms are compiled
    + Willamette/SSE2 instructions to assembler added

  Revision 1.20  2002/07/11 14:41:28  florian
    * start of the new generic parameter handling

  Revision 1.19  2002/07/07 09:52:32  florian
    * powerpc target fixed, very simple units can be compiled
    * some basic stuff for better callparanode handling, far from being finished

  Revision 1.18  2002/07/04 20:43:01  florian
    * first x86-64 patches

  Revision 1.17  2002/07/01 18:46:22  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.16  2002/07/01 16:23:53  peter
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

}
