{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate i386 assembler for in memory related nodes

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
unit n386mem;

{$i fpcdefs.inc}

interface

    uses
      cgbase,cpuinfo,cpubase,
      node,nmem,ncgmem;

    type
       ti386addrnode = class(tcgaddrnode)
          procedure pass_2;override;
       end;

       ti386derefnode = class(tcgderefnode)
          procedure pass_2;override;
       end;

       ti386vecnode = class(tcgvecnode)
          procedure update_reference_reg_mul(reg:tregister;l:aword);override;
          procedure pass_2;override;
       end;

implementation

    uses
{$ifdef delphi}
      sysutils,
{$endif}
      systems,
      cutils,verbose,
      symdef,paramgr,
      aasmtai,
      nld,ncon,nadd,
      cgobj,tgobj,rgobj;

{*****************************************************************************
                             TI386ADDRNODE
*****************************************************************************}

    procedure ti386addrnode.pass_2;

      begin
        inherited pass_2;
        { for use of other segments }
        if left.location.reference.segment<>NR_NO then
          location.segment:=left.location.reference.segment;
      end;


{*****************************************************************************
                           TI386DEREFNODE
*****************************************************************************}

    procedure ti386derefnode.pass_2;
      begin
         inherited pass_2;
         if tpointerdef(left.resulttype.def).is_far then
           location.reference.segment:=NR_FS;
      end;


{*****************************************************************************
                             TI386VECNODE
*****************************************************************************}

     procedure ti386vecnode.update_reference_reg_mul(reg:tregister;l:aword);
       var
         l2 : integer;
       begin
         { Optimized for x86 to use the index register and scalefactor }
         if location.reference.index=NR_NO then
          begin
            { no preparations needed }
          end
         else if location.reference.base=NR_NO then
          begin
            case location.reference.scalefactor of
             2 : cg.a_op_const_reg(exprasmlist,OP_SHL,OS_ADDR,1,location.reference.index);
             4 : cg.a_op_const_reg(exprasmlist,OP_SHL,OS_ADDR,2,location.reference.index);
             8 : cg.a_op_const_reg(exprasmlist,OP_SHL,OS_ADDR,3,location.reference.index);
            end;
            location.reference.base:=location.reference.index;
          end
         else
          begin
            cg.ungetregister(exprasmlist,location.reference.base);
            cg.a_loadaddr_ref_reg(exprasmlist,location.reference,location.reference.index);
            reference_reset_base(location.reference,location.reference.index,0);
          end;
         { insert the new index register and scalefactor or
           do the multiplication manual }
         case l of
          1,2,4,8 : location.reference.scalefactor:=l;
         else
           begin
              if ispowerof2(l,l2) then
                cg.a_op_const_reg(exprasmlist,OP_SHL,OS_ADDR,l2,reg)
              else
                cg.a_op_const_reg(exprasmlist,OP_IMUL,OS_ADDR,l,reg);
           end;
         end;
         location.reference.index:=reg;
       end;


    procedure ti386vecnode.pass_2;
      begin
        inherited pass_2;
        if nf_memseg in flags then
          location.reference.segment:=NR_FS;
      end;


begin
   caddrnode:=ti386addrnode;
   cderefnode:=ti386derefnode;
   cvecnode:=ti386vecnode;
end.
{
  $Log$
  Revision 1.57  2003-10-09 21:31:37  daniel
    * Register allocator splitted, ans abstract now

  Revision 1.56  2003/10/01 20:34:49  peter
    * procinfo unit contains tprocinfo
    * cginfo renamed to cgbase
    * moved cgmessage to verbose
    * fixed ppc and sparc compiles

  Revision 1.55  2003/09/29 20:58:56  peter
    * optimized releasing of registers

  Revision 1.54  2003/09/03 15:55:01  peter
    * NEWRA branch merged

  Revision 1.53.2.2  2003/08/31 15:46:26  peter
    * more updates for tregister

  Revision 1.53.2.1  2003/08/29 17:29:00  peter
    * next batch of updates

  Revision 1.53  2003/06/01 21:38:06  peter
    * getregisterfpu size parameter added
    * op_const_reg size parameter added
    * sparc updates

  Revision 1.52  2003/04/22 14:33:38  peter
    * removed some notes/hints

  Revision 1.51  2003/03/28 19:16:57  peter
    * generic constructor working for i386
    * remove fixed self register
    * esi added as address register for i386

  Revision 1.50  2003/02/19 22:00:15  daniel
    * Code generator converted to new register notation
    - Horribily outdated todo.txt removed

  Revision 1.49  2003/01/13 18:37:44  daniel
    * Work on register conversion

  Revision 1.48  2003/01/08 18:43:57  daniel
   * Tregister changed into a record

  Revision 1.47  2002/12/03 22:14:12  carl
     + use FPC_CHECKPOINTER once again

  Revision 1.46  2002/11/25 17:43:27  peter
    * splitted defbase in defutil,symutil,defcmp
    * merged isconvertable and is_equal into compare_defs(_ext)
    * made operator search faster by walking the list only once

  Revision 1.45  2002/11/23 22:50:09  carl
    * some small speed optimizations
    + added several new warnings/hints

  Revision 1.44  2002/09/07 15:25:11  peter
    * old logs removed and tabs fixed

  Revision 1.43  2002/09/01 19:27:35  peter
    * use index register when available for generating a reference with
      only a signle register. Using the base register could possibly
      destroy the framepointer

  Revision 1.42  2002/09/01 18:46:01  peter
    * fixed generic tcgvecnode
    * move code that updates a reference with index register and multiplier
      to separate method so it can be overriden for scaled indexing
    * i386 uses generic tcgvecnode

  Revision 1.41  2002/08/11 14:32:30  peter
    * renamed current_library to objectlibrary

  Revision 1.40  2002/08/11 13:24:17  peter
    * saving of asmsymbols in ppu supported
    * asmsymbollist global is removed and moved into a new class
      tasmlibrarydata that will hold the info of a .a file which
      corresponds with a single module. Added librarydata to tmodule
      to keep the library info stored for the module. In the future the
      objectfiles will also be stored to the tasmlibrarydata class
    * all getlabel/newasmsymbol and friends are moved to the new class

  Revision 1.39  2002/07/28 21:34:31  florian
    * more powerpc fixes
    + dummy tcgvecnode

  Revision 1.38  2002/07/20 11:58:04  florian
    * types.pas renamed to defbase.pas because D6 contains a types
      unit so this would conflicts if D6 programms are compiled
    + Willamette/SSE2 instructions to assembler added

  Revision 1.37  2002/07/11 14:41:33  florian
    * start of the new generic parameter handling

  Revision 1.36  2002/07/07 09:52:34  florian
    * powerpc target fixed, very simple units can be compiled
    * some basic stuff for better callparanode handling, far from being finished

  Revision 1.35  2002/07/01 18:46:33  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.34  2002/06/24 12:43:01  jonas
    * fixed errors found with new -CR code from Peter when cycling with -O2p3r

  Revision 1.33  2002/05/18 13:34:25  peter
    * readded missing revisions

  Revision 1.32  2002/05/16 19:46:51  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.30  2002/05/13 19:54:38  peter
    * removed n386ld and n386util units
    * maybe_save/maybe_restore added instead of the old maybe_push

  Revision 1.29  2002/05/12 16:53:17  peter
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

  Revision 1.28  2002/04/21 19:02:07  peter
    * removed newn and disposen nodes, the code is now directly
      inlined from pexpr
    * -an option that will write the secondpass nodes to the .s file, this
      requires EXTDEBUG define to actually write the info
    * fixed various internal errors and crashes due recent code changes

  Revision 1.27  2002/04/20 21:37:07  carl
  + generic FPC_CHECKPOINTER
  + first parameter offset in stack now portable
  * rename some constants
  + move some cpu stuff to other units
  - remove unused constents
  * fix stacksize for some targets
  * fix generic size problems which depend now on EXTEND_SIZE constant
  * removing frame pointer in routines is only available for : i386,m68k and vis targets

  Revision 1.26  2002/04/19 15:39:35  peter
    * removed some more routines from cga
    * moved location_force_reg/mem to ncgutil
    * moved arrayconstructnode secondpass to ncgld

  Revision 1.25  2002/04/15 19:12:09  carl
  + target_info.size_of_pointer -> pointer_size
  + some cleanup of unused types/variables
  * move several constants from cpubase to their specific units
    (where they are used)
  + att_Reg2str -> gas_reg2str
  + int_reg2str -> std_reg2str

  Revision 1.24  2002/04/04 19:06:12  peter
    * removed unused units
    * use tlocation.size in cg.a_*loc*() routines

  Revision 1.23  2002/04/02 17:11:36  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods

}
