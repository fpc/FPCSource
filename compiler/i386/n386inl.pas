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
       nx86inl;

    type
       ti386inlinenode = class(tx86inlinenode)
       end;

implementation

  uses
    ninl;

begin
   cinlinenode:=ti386inlinenode;
end.
{
  $Log$
  Revision 1.73  2004-02-05 01:24:08  florian
    * several fixes to compile x86-64 system

  Revision 1.72  2004/02/03 22:32:54  peter
    * renamed xNNbittype to xNNinttype
    * renamed registers32 to registersint
    * replace some s32bit,u32bit with torddef([su]inttype).def.typ

  Revision 1.71  2004/02/02 20:41:59  florian
    + added prefetch(const mem) support

  Revision 1.70  2003/10/10 17:48:14  peter
    * old trgobj moved to x86/rgcpu and renamed to trgx86fpu
    * tregisteralloctor renamed to trgobj
    * removed rgobj from a lot of units
    * moved location_* and reference_* to cgobj
    * first things for mmx register allocation

  Revision 1.69  2003/10/09 21:31:37  daniel
    * Register allocator splitted, ans abstract now

  Revision 1.68  2003/10/01 20:34:49  peter
    * procinfo unit contains tprocinfo
    * cginfo renamed to cgbase
    * moved cgmessage to verbose
    * fixed ppc and sparc compiles

  Revision 1.67  2003/09/28 21:48:20  peter
    * fix register leaks

  Revision 1.66  2003/09/07 22:09:35  peter
    * preparations for different default calling conventions
    * various RA fixes

  Revision 1.65  2003/09/03 15:55:01  peter
    * NEWRA branch merged

  Revision 1.64.2.2  2003/08/31 15:46:26  peter
    * more updates for tregister

  Revision 1.64.2.1  2003/08/29 17:29:00  peter
    * next batch of updates

  Revision 1.64  2003/07/02 22:18:04  peter
    * paraloc splitted in callerparaloc,calleeparaloc
    * sparc calling convention updates

  Revision 1.63  2003/06/03 13:01:59  daniel
    * Register allocator finished

  Revision 1.62  2003/06/01 21:38:06  peter
    * getregisterfpu size parameter added
    * op_const_reg size parameter added
    * sparc updates

  Revision 1.61  2003/05/30 23:49:18  jonas
    * a_load_loc_reg now has an extra size parameter for the destination
      register (properly fixes what I worked around in revision 1.106 of
      ncgutil.pas)

  Revision 1.60  2003/04/23 09:50:31  peter
    * wrong location_copy for include/exclude

  Revision 1.59  2003/04/22 23:50:23  peter
    * firstpass uses expectloc
    * checks if there are differences between the expectloc and
      location.loc from secondpass in EXTDEBUG

  Revision 1.58  2003/04/22 14:33:38  peter
    * removed some notes/hints

  Revision 1.57  2003/04/22 10:09:35  daniel
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
