{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate i386 inline nodes

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
unit n386inl;

{$i fpcdefs.inc}

interface

    uses
       node,ninl,ncginl;

    type
       ti386inlinenode = class(tcginlinenode)
          { first pass override
            so that the code generator will actually generate
            these nodes.
          }
          function first_pi: tnode ; override;
          function first_arctan_real: tnode; override;
          function first_abs_real: tnode; override;
          function first_sqr_real: tnode; override;
          function first_sqrt_real: tnode; override;
          function first_ln_real: tnode; override;
          function first_cos_real: tnode; override;
          function first_sin_real: tnode; override;
          { second pass override to generate these nodes }
          procedure second_IncludeExclude;override;
          procedure second_pi; override;
          procedure second_arctan_real; override;
          procedure second_abs_real; override;
          procedure second_sqr_real; override;
          procedure second_sqrt_real; override;
          procedure second_ln_real; override;
          procedure second_cos_real; override;
          procedure second_sin_real; override;
       private
          procedure load_fpu_location;
       end;

implementation

    uses
      globtype,systems,
      cutils,verbose,globals,fmodule,
      symconst,symdef,defutil,
      aasmbase,aasmtai,aasmcpu,
      cginfo,cgbase,pass_1,pass_2,
      cpubase,paramgr,
      nbas,ncon,ncal,ncnv,nld,
      cga,tgobj,ncgutil,cgobj,cg64f32,rgobj,rgcpu;


{*****************************************************************************
                              TI386INLINENODE
*****************************************************************************}

     function ti386inlinenode.first_pi : tnode;
      begin
        location.loc:=LOC_FPUREGISTER;
        registersfpu:=1;
        first_pi := nil;
      end;


     function ti386inlinenode.first_arctan_real : tnode;
      begin
        location.loc:=LOC_FPUREGISTER;
        registers32:=left.registers32;
        registersfpu:=max(left.registersfpu,2);
{$ifdef SUPPORT_MMX}
        registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
        first_arctan_real := nil;
      end;

     function ti386inlinenode.first_abs_real : tnode;
      begin
        location.loc:=LOC_FPUREGISTER;
        registers32:=left.registers32;
        registersfpu:=max(left.registersfpu,1);
{$ifdef SUPPORT_MMX}
        registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
        first_abs_real := nil;
      end;

     function ti386inlinenode.first_sqr_real : tnode;
      begin
        location.loc:=LOC_FPUREGISTER;
        registers32:=left.registers32;
        registersfpu:=max(left.registersfpu,1);
{$ifdef SUPPORT_MMX}
        registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
        first_sqr_real := nil;
      end;

     function ti386inlinenode.first_sqrt_real : tnode;
      begin
        location.loc:=LOC_FPUREGISTER;
        registers32:=left.registers32;
        registersfpu:=max(left.registersfpu,1);
{$ifdef SUPPORT_MMX}
        registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
        first_sqrt_real := nil;
      end;

     function ti386inlinenode.first_ln_real : tnode;
      begin
        location.loc:=LOC_FPUREGISTER;
        registers32:=left.registers32;
        registersfpu:=max(left.registersfpu,2);
{$ifdef SUPPORT_MMX}
        registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
        first_ln_real := nil;
      end;

     function ti386inlinenode.first_cos_real : tnode;
      begin
        location.loc:=LOC_FPUREGISTER;
        registers32:=left.registers32;
        registersfpu:=max(left.registersfpu,1);
{$ifdef SUPPORT_MMX}
        registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
        first_cos_real := nil;
      end;

     function ti386inlinenode.first_sin_real : tnode;
      begin
        location.loc:=LOC_FPUREGISTER;
        registers32:=left.registers32;
        registersfpu:=max(left.registersfpu,1);
{$ifdef SUPPORT_MMX}
        registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
        first_sin_real := nil;
      end;


     procedure ti386inlinenode.second_Pi;
       begin
         location_reset(location,LOC_FPUREGISTER,def_cgsize(resulttype.def));
         emit_none(A_FLDPI,S_NO);
         inc(trgcpu(rg).fpuvaroffset);
         location.register.enum:=FPU_RESULT_REG;

       end;

       { load the FPU into the an fpu register }
       procedure ti386inlinenode.load_fpu_location;
         begin
           location_reset(location,LOC_FPUREGISTER,def_cgsize(resulttype.def));
           location.register.enum:=FPU_RESULT_REG;
           secondpass(left);
           case left.location.loc of
             LOC_FPUREGISTER:
                      ;
             LOC_CFPUREGISTER:
               begin
                 cg.a_loadfpu_reg_reg(exprasmlist,
                   left.location.register,location.register);
               end;
             LOC_REFERENCE,LOC_CREFERENCE:
               begin
                 cg.a_loadfpu_ref_reg(exprasmlist,
                    def_cgsize(left.resulttype.def),
                    left.location.reference,location.register);
                 location_release(exprasmlist,left.location);
               end
           else
              internalerror(309991);
           end;
         end;

     procedure ti386inlinenode.second_arctan_real;
       begin
         load_fpu_location;
         emit_none(A_FLD1,S_NO);
         emit_none(A_FPATAN,S_NO);
       end;

     procedure ti386inlinenode.second_abs_real;
       begin
         load_fpu_location;
         emit_none(A_FABS,S_NO);
       end;

     procedure ti386inlinenode.second_sqr_real;
     
     var r:Tregister;
     
       begin
         load_fpu_location;
         r.enum:=R_ST0;
         emit_reg_reg(A_FMUL,S_NO,r,r);
       end;

     procedure ti386inlinenode.second_sqrt_real;
       begin
         load_fpu_location;
         emit_none(A_FSQRT,S_NO);
       end;

     procedure ti386inlinenode.second_ln_real;
       begin
         load_fpu_location;
         emit_none(A_FLDLN2,S_NO);
         emit_none(A_FXCH,S_NO);
         emit_none(A_FYL2X,S_NO);
       end;

     procedure ti386inlinenode.second_cos_real;
       begin
         load_fpu_location;
         emit_none(A_FCOS,S_NO);
       end;

     procedure ti386inlinenode.second_sin_real;
       begin
         load_fpu_location;
         emit_none(A_FSIN,S_NO)
       end;

{*****************************************************************************
                     INCLUDE/EXCLUDE GENERIC HANDLING
*****************************************************************************}

      procedure ti386inlinenode.second_IncludeExclude;
        var
         scratch_reg : boolean;
         hregister : tregister;
         asmop : tasmop;
         L : cardinal;
         pushedregs : TMaybesave;
         cgop : topcg;
        begin
          location_copy(location,left.location);
          secondpass(tcallparanode(left).left);
          if tcallparanode(tcallparanode(left).right).left.nodetype=ordconstn then
            begin
              { calculate bit position }
              l:=cardinal(1 shl (tordconstnode(tcallparanode(tcallparanode(left).right).left).value mod 32));

              { determine operator }
              if inlinenumber=in_include_x_y then
                cgop:=OP_OR
              else
                begin
                  cgop:=OP_AND;
                  l:=not(l);
                end;
              if (tcallparanode(left).left.location.loc=LOC_REFERENCE) then
                begin
                  inc(tcallparanode(left).left.location.reference.offset,
                    (tordconstnode(tcallparanode(tcallparanode(left).right).left).value div 32)*4);
                  cg.a_op_const_ref(exprasmlist,cgop,OS_INT,l,tcallparanode(left).left.location.reference);
                  location_release(exprasmlist,tcallparanode(left).left.location);
                end
              else
                { LOC_CREGISTER }
                begin
                  cg.a_op_const_reg(exprasmlist,cgop,l,tcallparanode(left).left.location.register);
                end;
            end
          else
            begin
              { generate code for the element to set }
            {$ifndef newra}
              maybe_save(exprasmlist,tcallparanode(tcallparanode(left).right).left.registers32,
                        tcallparanode(left).left.location,pushedregs);
            {$endif}
              secondpass(tcallparanode(tcallparanode(left).right).left);
            {$ifndef newra}
              maybe_restore(exprasmlist,tcallparanode(left).left.location,pushedregs);
            {$endif}
              { determine asm operator }
              if inlinenumber=in_include_x_y then
                 asmop:=A_BTS
              else
                 asmop:=A_BTR;

              if tcallparanode(tcallparanode(left).right).left.location.loc in [LOC_CREGISTER,LOC_REGISTER] then
                { we don't need a mod 32 because this is done automatically  }
                { by the bts instruction. For proper checking we would       }

                { note: bts doesn't do any mod'ing, that's why we can also use }
                { it for normalsets! (JM)                                      }

                { need a cmp and jmp, but this should be done by the         }
                { type cast code which does range checking if necessary (FK) }
                begin
                  scratch_reg := FALSE;
                  hregister.enum:=R_INTREGISTER;
                  hregister.number:=(Tcallparanode(Tcallparanode(left).right).left.location.register.number and not $ff)
                    or R_SUBWHOLE;
                end
              else
                begin
                  scratch_reg := TRUE;
                {$ifdef newra}
                  hregister:=rg.getregisterint(exprasmlist,OS_INT);
                {$else}
                  hregister:=cg.get_scratch_reg_int(exprasmlist,OS_INT);
                {$endif newra}
                end;
              cg.a_load_loc_reg(exprasmlist,tcallparanode(tcallparanode(left).right).left.location,hregister);
              if (tcallparanode(left).left.location.loc=LOC_REFERENCE) then
                emit_reg_ref(asmop,S_L,hregister,tcallparanode(left).left.location.reference)
              else
                emit_reg_reg(asmop,S_L,hregister,tcallparanode(left).left.location.register);
            {$ifdef newra}
              if scratch_reg then
                rg.ungetregisterint(exprasmlist,hregister);
            {$else}
              if scratch_reg then
                cg.free_scratch_reg(exprasmlist,hregister);
            {$endif newra}
            end;
        end;


begin
   cinlinenode:=ti386inlinenode;
end.
{
  $Log$
  Revision 1.57  2003-04-22 10:09:35  daniel
    + Implemented the actual register allocator
    + Scratch registers unavailable when new register allocator used
    + maybe_save/maybe_restore unavailable when new register allocator used

  Revision 1.56  2003/02/19 22:00:15  daniel
    * Code generator converted to new register notation
    - Horribily outdated todo.txt removed

  Revision 1.55  2003/01/08 18:43:57  daniel
   * Tregister changed into a record

  Revision 1.54  2002/11/25 17:43:26  peter
    * splitted defbase in defutil,symutil,defcmp
    * merged isconvertable and is_equal into compare_defs(_ext)
    * made operator search faster by walking the list only once

  Revision 1.53  2002/09/07 15:25:10  peter
    * old logs removed and tabs fixed

  Revision 1.52  2002/08/02 07:44:31  jonas
    * made assigned() handling generic
    * add nodes now can also evaluate constant expressions at compile time
      that contain nil nodes

  Revision 1.51  2002/07/26 11:16:35  jonas
    * fixed (actual and potential) range errors

  Revision 1.50  2002/07/25 18:02:33  carl
   + added generic inline nodes

  Revision 1.49  2002/07/20 11:58:02  florian
    * types.pas renamed to defbase.pas because D6 contains a types
      unit so this would conflicts if D6 programms are compiled
    + Willamette/SSE2 instructions to assembler added

  Revision 1.48  2002/07/11 14:41:33  florian
    * start of the new generic parameter handling

  Revision 1.47  2002/07/07 09:52:34  florian
    * powerpc target fixed, very simple units can be compiled
    * some basic stuff for better callparanode handling, far from being finished

  Revision 1.46  2002/07/01 18:46:33  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.45  2002/07/01 16:23:56  peter
    * cg64 patch
    * basics for currency
    * asnode updates for class and interface (not finished)

  Revision 1.44  2002/05/18 13:34:25  peter
    * readded missing revisions

  Revision 1.43  2002/05/16 19:46:51  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.41  2002/05/13 19:54:38  peter
    * removed n386ld and n386util units
    * maybe_save/maybe_restore added instead of the old maybe_push

  Revision 1.40  2002/05/12 16:53:17  peter
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

  Revision 1.39  2002/04/23 19:16:35  peter
    * add pinline unit that inserts compiler supported functions using
      one or more statements
    * moved finalize and setlength from ninl to pinline

  Revision 1.38  2002/04/21 15:35:54  carl
  * changeregsize -> rg.makeregsize

  Revision 1.37  2002/04/19 15:39:35  peter
    * removed some more routines from cga
    * moved location_force_reg/mem to ncgutil
    * moved arrayconstructnode secondpass to ncgld

  Revision 1.36  2002/04/15 19:44:21  peter
    * fixed stackcheck that would be called recursively when a stack
      error was found
    * generic changeregsize(reg,size) for i386 register resizing
    * removed some more routines from cga unit
    * fixed returnvalue handling
    * fixed default stacksize of linux and go32v2, 8kb was a bit small :-)

  Revision 1.35  2002/04/04 19:06:11  peter
    * removed unused units
    * use tlocation.size in cg.a_*loc*() routines

  Revision 1.34  2002/04/02 17:11:36  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods

  Revision 1.33  2002/03/31 20:26:39  jonas
    + a_loadfpu_* and a_loadmm_* methods in tcg
    * register allocation is now second_d by a class and is mostly processor
      independent (+rgobj.pas and i386/rgcpu.pas)
    * temp allocation is now second_d by a class (+tgobj.pas, -i386\tgcpu.pas)
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

  Revision 1.32  2002/03/04 19:10:14  peter
    * removed compiler warnings

}
