{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate i386 assembler for type converting nodes

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
unit n386cnv;

{$i fpcdefs.inc}

interface

    uses
      node,ncgcnv,defutil,defcmp,nx86cnv;

    type
       ti386typeconvnode = class(tx86typeconvnode)
         protected
         { procedure second_int_to_int;override; }
         { procedure second_string_to_string;override; }
         { procedure second_cstring_to_pchar;override; }
         { procedure second_string_to_chararray;override; }
         { procedure second_array_to_pointer;override; }
         { procedure second_pointer_to_array;override; }
         { procedure second_chararray_to_string;override; }
         { procedure second_char_to_string;override; }
          function first_int_to_real: tnode; override;
          procedure second_int_to_real;override;
         { procedure second_real_to_real;override; }
         { procedure second_cord_to_pointer;override; }
         { procedure second_proc_to_procvar;override; }
         { procedure second_bool_to_int;override; }
         { procedure second_int_to_bool;override; }
         { procedure second_load_smallset;override;  }
         { procedure second_ansistring_to_pchar;override; }
         { procedure second_pchar_to_string;override; }
         { procedure second_class_to_intf;override;  }
         { procedure second_char_to_char;override; }
{$ifdef TESTOBJEXT2}
          procedure checkobject;override;
{$endif TESTOBJEXT2}
       end;


implementation

   uses
      verbose,systems,
      symconst,symdef,aasmbase,aasmtai,aasmcpu,
      cgbase,
      ncon,ncal,ncnv,
      cpubase,tgobj,
      cgobj,cga,cgx86,ncgutil;


    function ti386typeconvnode.first_int_to_real : tnode;

      begin
        first_int_to_real:=nil;
         if registersfpu<1 then
          registersfpu:=1;
        expectloc:=LOC_FPUREGISTER;
      end;


    procedure ti386typeconvnode.second_int_to_real;

      var
         href : treference;
         hregister : tregister;
         l1,l2 : tasmlabel;
      begin
         location_reset(location,LOC_FPUREGISTER,def_cgsize(resulttype.def));

         { We need to load from a reference }
         location_force_mem(exprasmlist,left.location);

         { For u32bit we need to load it as comp and need to
           make it 64bits }
         if (torddef(left.resulttype.def).typ=u32bit) then
           begin
             tg.GetTemp(exprasmlist,8,tt_normal,href);
             location_release(exprasmlist,left.location);
             location_freetemp(exprasmlist,left.location);
             cg.a_load_ref_ref(exprasmlist,left.location.size,OS_32,left.location.reference,href);
             inc(href.offset,4);
             cg.a_load_const_ref(exprasmlist,OS_32,0,href);
             dec(href.offset,4);
             left.location.reference:=href;
           end;

         { Load from reference to fpu reg }
         location_release(exprasmlist,left.location);
         case torddef(left.resulttype.def).typ of
           u32bit,
           scurrency,
           s64bit:
             exprasmlist.concat(taicpu.op_ref(A_FILD,S_IQ,left.location.reference));
           u64bit:
             begin
                { unsigned 64 bit ints are harder to handle: }
                { we load bits 0..62 and then check bit 63:  }
                { if it is 1 then we add $80000000 000000000 }
                { as double                                  }
                inc(left.location.reference.offset,4);
                hregister:=cg.getintregister(exprasmlist,OS_32);
                cg.a_load_ref_reg(exprasmlist,OS_INT,OS_INT,left.location.reference,hregister);
                emit_const_ref(A_AND,S_L,$7fffffff,left.location.reference);
                emit_const_reg(A_TEST,S_L,longint($80000000),hregister);
                cg.ungetregister(exprasmlist,hregister);
                dec(left.location.reference.offset,4);
                exprasmlist.concat(taicpu.op_ref(A_FILD,S_IQ,left.location.reference));
                objectlibrary.getdatalabel(l1);
                objectlibrary.getlabel(l2);
                cg.a_jmp_flags(exprasmlist,F_E,l2);
                Consts.concat(Tai_label.Create(l1));
                { I got this constant from a test progtram (FK) }
                Consts.concat(Tai_const.Create_32bit(0));
                Consts.concat(Tai_const.Create_32bit(1138753536));
                reference_reset_symbol(href,l1,0);
                emit_ref(A_FADD,S_FL,href);
                cg.a_label(exprasmlist,l2);
             end
           else
             begin
               if left.resulttype.def.size<4 then
                 begin
                   tg.GetTemp(exprasmlist,4,tt_normal,href);
                   location_freetemp(exprasmlist,left.location);
                   cg.a_load_ref_ref(exprasmlist,left.location.size,OS_32,left.location.reference,href);
                   left.location.reference:=href;
                 end;
              exprasmlist.concat(taicpu.op_ref(A_FILD,S_IL,left.location.reference));
             end;
         end;
         location_freetemp(exprasmlist,left.location);
         tcgx86(cg).inc_fpu_stack;
         location.register:=NR_ST;
      end;


{$ifdef TESTOBJEXT2}
    procedure ti386typeconvnode.checkobject;
      var
         r : preference;
         nillabel : plabel;
       begin
         new(r);
         reset_reference(r^);
         if p^.location.loc in [LOC_REGISTER,LOC_CREGISTER] then
          r^.base:=p^.location.register
         else
           begin
              cg.getexplicitregister(exprasmlist,R_EDI);
              emit_mov_loc_reg(p^.location,R_EDI);
              r^.base:=R_EDI;
           end;
         { NIL must be accepted !! }
         emit_reg_reg(A_OR,S_L,r^.base,r^.base);
         rg.ungetregisterint(exprasmlist,R_EDI);
         objectlibrary.getlabel(nillabel);
         cg.a_jmp_flags(exprasmlist,F_E,nillabel);
         { this is one point where we need vmt_offset (PM) }
         r^.offset:= tobjectdef(tpointerdef(p^.resulttype.def).definition).vmt_offset;
         rg.getexplicitregisterint(exprasmlist,R_EDI);
         emit_ref_reg(A_MOV,S_L,r,R_EDI);
         emit_sym(A_PUSH,S_L,
           objectlibrary.newasmsymbol(tobjectdef(tpointerdef(p^.resulttype.def).definition).vmt_mangledname));
         emit_reg(A_PUSH,S_L,R_EDI);
         rg.ungetregister32(exprasmlist,R_EDI);
         emitcall('FPC_CHECK_OBJECT_EXT');
         emitlab(nillabel);
       end;
{$endif TESTOBJEXT2}


begin
   ctypeconvnode:=ti386typeconvnode;
end.
{
  $Log$
  Revision 1.70  2003-12-08 15:35:00  peter
    * fix loading of word/byte to real

  Revision 1.69  2003/12/03 23:13:20  peter
    * delayed paraloc allocation, a_param_*() gets extra parameter
      if it needs to allocate temp or real paralocation
    * optimized/simplified int-real loading

  Revision 1.68  2003/11/04 22:30:15  florian
    + type cast variant<->enum
    * cnv. node second pass uses now as well helper wrappers

  Revision 1.67  2003/10/10 17:48:14  peter
    * old trgobj moved to x86/rgcpu and renamed to trgx86fpu
    * tregisteralloctor renamed to trgobj
    * removed rgobj from a lot of units
    * moved location_* and reference_* to cgobj
    * first things for mmx register allocation

  Revision 1.66  2003/10/09 21:31:37  daniel
    * Register allocator splitted, ans abstract now

  Revision 1.65  2003/10/01 20:34:49  peter
    * procinfo unit contains tprocinfo
    * cginfo renamed to cgbase
    * moved cgmessage to verbose
    * fixed ppc and sparc compiles

  Revision 1.64  2003/09/28 21:48:20  peter
    * fix register leaks

  Revision 1.63  2003/09/03 15:55:01  peter
    * NEWRA branch merged

  Revision 1.62.2.2  2003/08/31 15:46:26  peter
    * more updates for tregister

  Revision 1.62.2.1  2003/08/31 13:58:46  daniel
    * Some more work to make things compile

  Revision 1.62  2003/06/03 21:11:09  peter
    * cg.a_load_* get a from and to size specifier
    * makeregsize only accepts newregister
    * i386 uses generic tcgnotnode,tcgunaryminus

  Revision 1.61  2003/04/30 20:53:32  florian
    * error when address of an abstract method is taken
    * fixed some x86-64 problems
    * merged some more x86-64 and i386 code

  Revision 1.60  2003/04/23 20:16:04  peter
    + added currency support based on int64
    + is_64bit for use in cg units instead of is_64bitint
    * removed cgmessage from n386add, replace with internalerrors

  Revision 1.59  2003/04/22 23:50:23  peter
    * firstpass uses expectloc
    * checks if there are differences between the expectloc and
      location.loc from secondpass in EXTDEBUG

  Revision 1.58  2003/04/22 10:09:35  daniel
    + Implemented the actual register allocator
    + Scratch registers unavailable when new register allocator used
    + maybe_save/maybe_restore unavailable when new register allocator used

  Revision 1.57  2003/03/13 19:52:23  jonas
    * and more new register allocator fixes (in the i386 code generator this
      time). At least now the ppc cross compiler can compile the linux
      system unit again, but I haven't tested it.

  Revision 1.56  2003/02/19 22:00:15  daniel
    * Code generator converted to new register notation
    - Horribily outdated todo.txt removed

  Revision 1.55  2003/01/13 18:37:44  daniel
    * Work on register conversion

  Revision 1.54  2003/01/08 18:43:57  daniel
   * Tregister changed into a record

  Revision 1.53  2002/12/05 14:27:42  florian
    * some variant <-> dyn. array stuff

  Revision 1.52  2002/11/25 17:43:26  peter
    * splitted defbase in defutil,symutil,defcmp
    * merged isconvertable and is_equal into compare_defs(_ext)
    * made operator search faster by walking the list only once

  Revision 1.51  2002/10/10 16:14:54  florian
    * fixed to reflect last tconvtype change

  Revision 1.50  2002/10/05 12:43:29  carl
    * fixes for Delphi 6 compilation
     (warning : Some features do not work under Delphi)

  Revision 1.49  2002/09/17 18:54:03  jonas
    * a_load_reg_reg() now has two size parameters: source and dest. This
      allows some optimizations on architectures that don't encode the
      register size in the register name.

  Revision 1.48  2002/08/14 19:19:14  carl
    * first_int_to_real moved to i386 (other one is generic)

  Revision 1.47  2002/08/11 14:32:30  peter
    * renamed current_library to objectlibrary

  Revision 1.46  2002/08/11 13:24:16  peter
    * saving of asmsymbols in ppu supported
    * asmsymbollist global is removed and moved into a new class
      tasmlibrarydata that will hold the info of a .a file which
      corresponds with a single module. Added librarydata to tmodule
      to keep the library info stored for the module. In the future the
      objectfiles will also be stored to the tasmlibrarydata class
    * all getlabel/newasmsymbol and friends are moved to the new class

  Revision 1.45  2002/07/27 19:53:51  jonas
    + generic implementation of tcg.g_flags2ref()
    * tcg.flags2xxx() now also needs a size parameter

  Revision 1.44  2002/07/20 11:58:01  florian
    * types.pas renamed to defbase.pas because D6 contains a types
      unit so this would conflicts if D6 programms are compiled
    + Willamette/SSE2 instructions to assembler added

  Revision 1.43  2002/07/01 18:46:31  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.42  2002/05/20 13:30:40  carl
  * bugfix of hdisponen (base must be set, not index)
  * more portability fixes

  Revision 1.41  2002/05/18 13:34:24  peter
    * readded missing revisions

  Revision 1.40  2002/05/16 19:46:51  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.38  2002/05/12 16:53:17  peter
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

  Revision 1.37  2002/04/21 19:02:07  peter
    * removed newn and disposen nodes, the code is now directly
      inlined from pexpr
    * -an option that will write the secondpass nodes to the .s file, this
      requires EXTDEBUG define to actually write the info
    * fixed various internal errors and crashes due recent code changes

  Revision 1.36  2002/04/21 15:35:23  carl
  * changeregsize -> rg.makeregsize

  Revision 1.35  2002/04/19 15:39:35  peter
    * removed some more routines from cga
    * moved location_force_reg/mem to ncgutil
    * moved arrayconstructnode secondpass to ncgld

  Revision 1.34  2002/04/15 19:44:21  peter
    * fixed stackcheck that would be called recursively when a stack
      error was found
    * generic changeregsize(reg,size) for i386 register resizing
    * removed some more routines from cga unit
    * fixed returnvalue handling
    * fixed default stacksize of linux and go32v2, 8kb was a bit small :-)

  Revision 1.33  2002/04/04 19:06:10  peter
    * removed unused units
    * use tlocation.size in cg.a_*loc*() routines

  Revision 1.32  2002/04/02 17:11:36  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods

  Revision 1.31  2002/03/31 20:26:38  jonas
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

  Revision 1.30  2002/03/04 19:10:13  peter
    * removed compiler warnings

}
