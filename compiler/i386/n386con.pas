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
      systems,
      cpubase,
      cga,rgobj,rgcpu;

{*****************************************************************************
                           TI386REALCONSTNODE
*****************************************************************************}

    function ti386realconstnode.pass_1 : tnode;
      begin
         result:=nil;
         if (value_real=1.0) or (value_real=0.0) then
           begin
              location.loc:=LOC_FPUREGISTER;
              registersfpu:=1;
           end
         else
           location.loc:=LOC_CREFERENCE;
      end;

    procedure ti386realconstnode.pass_2;

      begin
         if (value_real=1.0) then
           begin
              emit_none(A_FLD1,S_NO);
              location.loc:=LOC_FPUREGISTER;
              location.register.enum:=R_ST;
              inc(trgcpu(rg).fpuvaroffset);
           end
         else if (value_real=0.0) then
           begin
              emit_none(A_FLDZ,S_NO);
              location.loc:=LOC_FPUREGISTER;
              location.register.enum:=R_ST;
              inc(trgcpu(rg).fpuvaroffset);
           end
         else
           inherited pass_2;
      end;


begin
   crealconstnode:=ti386realconstnode;
end.
{
  $Log$
  Revision 1.17  2003-01-08 18:43:57  daniel
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
