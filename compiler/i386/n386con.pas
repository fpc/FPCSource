{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate i386 assembler for constants

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
unit n386con;

{$i fpcdefs.inc}

interface

    uses
       node,ncon,ncgcon;

    type
       ti386realconstnode = class(tcgrealconstnode)
          function pass_1 : tnode;override;
          procedure pass_2;override;
       end;

implementation

    uses
      systems,globtype,globals,
      defutil,
      cpubase,
      cga,cgx86,cgobj,cgbase,rgobj,rgcpu;

{*****************************************************************************
                           TI386REALCONSTNODE
*****************************************************************************}

    function ti386realconstnode.pass_1 : tnode;
      begin
         result:=nil;
         if is_number_float(value_real) and (value_real=1.0) or (value_real=0.0) then
           begin
              expectloc:=LOC_FPUREGISTER;
              registersfpu:=1;
           end
         else
           expectloc:=LOC_CREFERENCE;
      end;

    procedure ti386realconstnode.pass_2;

      begin
         if is_number_float(value_real) then
           begin
             if (value_real=1.0) then
               begin
                  emit_none(A_FLD1,S_NO);
                  location_reset(location,LOC_FPUREGISTER,def_cgsize(resulttype.def));
                  location.register:=NR_ST;
                  tcgx86(cg).inc_fpu_stack;
               end
             else if (value_real=0.0) then
               begin
                  emit_none(A_FLDZ,S_NO);
                  location_reset(location,LOC_FPUREGISTER,def_cgsize(resulttype.def));
                  location.register:=NR_ST;
                  tcgx86(cg).inc_fpu_stack;
               end
            else
              inherited pass_2;
           end
         else
           inherited pass_2;
      end;


begin
   crealconstnode:=ti386realconstnode;
end.
{
  $Log$
  Revision 1.23  2003-10-09 21:31:37  daniel
    * Register allocator splitted, ans abstract now

  Revision 1.22  2003/10/01 20:34:49  peter
    * procinfo unit contains tprocinfo
    * cginfo renamed to cgbase
    * moved cgmessage to verbose
    * fixed ppc and sparc compiles

  Revision 1.21  2003/09/06 16:47:24  florian
    + support of NaN and Inf in the compiler as values of real constants

  Revision 1.20  2003/09/03 15:55:01  peter
    * NEWRA branch merged

  Revision 1.19.2.1  2003/08/29 17:29:00  peter
    * next batch of updates

  Revision 1.19  2003/04/22 23:50:23  peter
    * firstpass uses expectloc
    * checks if there are differences between the expectloc and
      location.loc from secondpass in EXTDEBUG

  Revision 1.18  2003/04/22 09:54:18  peter
    * use location_reset

  Revision 1.17  2003/01/08 18:43:57  daniel
   * Tregister changed into a record

  Revision 1.16  2002/05/18 13:34:25  peter
    * readded missing revisions

  Revision 1.15  2002/05/16 19:46:51  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.13  2002/04/02 17:11:36  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods

  Revision 1.12  2002/03/31 20:26:38  jonas
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
