{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    Helper routines for the i386 code generator

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

unit cga;

{$i fpcdefs.inc}

interface

    uses
       cpuinfo,cpubase,cgbase,
       symconst,symtype,symdef,aasmbase,aasmtai,aasmcpu;

{$define TESTGETTEMP to store const that
 are written into temps for later release PM }

    function def_opsize(p1:tdef):topsize;

    procedure emit_none(i : tasmop;s : topsize);

    procedure emit_const(i : tasmop;s : topsize;c : longint);
    procedure emit_reg(i : tasmop;s : topsize;reg : tregister);
    procedure emit_ref(i : tasmop;s : topsize;const ref : treference);

    procedure emit_const_reg(i : tasmop;s : topsize;c : longint;reg : tregister);
    procedure emit_const_ref(i : tasmop;s : topsize;c : longint;const ref : treference);
    procedure emit_ref_reg(i : tasmop;s : topsize;const ref : treference;reg : tregister);
    procedure emit_reg_ref(i : tasmop;s : topsize;reg : tregister;const ref : treference);
    procedure emit_reg_reg(i : tasmop;s : topsize;reg1,reg2 : tregister);

    procedure emit_const_reg_reg(i : tasmop;s : topsize;c : longint;reg1,reg2 : tregister);
    procedure emit_reg_reg_reg(i : tasmop;s : topsize;reg1,reg2,reg3 : tregister);


    procedure emit_sym(i : tasmop;s : topsize;op : tasmsymbol);


implementation

    uses
       cutils,
       systems,globals,verbose,
       cgobj,tgobj,rgobj;


{*****************************************************************************
                                Helpers
*****************************************************************************}

    function def_opsize(p1:tdef):topsize;
      begin
        case p1.size of
         1 : def_opsize:=S_B;
         2 : def_opsize:=S_W;
         4 : def_opsize:=S_L;
{$ifdef x86_64}
         8 : def_opsize:=S_Q;
{$else x86_64}
         { I don't know if we need it (FK) }
         8 : def_opsize:=S_L;
{$endif x86_64}
        else
         internalerror(130820001);
        end;
      end;


{*****************************************************************************
                              Emit Assembler
*****************************************************************************}

    procedure emit_none(i : tasmop;s : topsize);
      begin
         exprasmList.concat(Taicpu.Op_none(i,s));
      end;

    procedure emit_reg(i : tasmop;s : topsize;reg : tregister);
      begin
         exprasmList.concat(Taicpu.Op_reg(i,s,reg));
      end;

    procedure emit_ref(i : tasmop;s : topsize;const ref : treference);
      begin
         exprasmList.concat(Taicpu.Op_ref(i,s,ref));
      end;

    procedure emit_const(i : tasmop;s : topsize;c : longint);
      begin
         exprasmList.concat(Taicpu.Op_const(i,s,aword(c)));
      end;

    procedure emit_const_reg(i : tasmop;s : topsize;c : longint;reg : tregister);
      begin
         exprasmList.concat(Taicpu.Op_const_reg(i,s,aword(c),reg));
      end;

    procedure emit_const_ref(i : tasmop;s : topsize;c : longint;const ref : treference);
      begin
         exprasmList.concat(Taicpu.Op_const_ref(i,s,aword(c),ref));
      end;

    procedure emit_ref_reg(i : tasmop;s : topsize;const ref : treference;reg : tregister);
      begin
         exprasmList.concat(Taicpu.Op_ref_reg(i,s,ref,reg));
      end;

    procedure emit_reg_ref(i : tasmop;s : topsize;reg : tregister;const ref : treference);
      begin
         exprasmList.concat(Taicpu.Op_reg_ref(i,s,reg,ref));
      end;

    procedure emit_reg_reg(i : tasmop;s : topsize;reg1,reg2 : tregister);

    var instr:Taicpu;

    begin
      if not ((reg1=reg2) and (i=A_MOV)) then
        begin
          instr:=Taicpu.op_reg_reg(i,s,reg1,reg2);
          exprasmlist.concat(instr);
          if i=A_MOV then
            cg.add_move_instruction(instr);
        end;
    end;

    procedure emit_const_reg_reg(i : tasmop;s : topsize;c : longint;reg1,reg2 : tregister);
      begin
         exprasmList.concat(Taicpu.Op_const_reg_reg(i,s,c,reg1,reg2));
      end;

    procedure emit_reg_reg_reg(i : tasmop;s : topsize;reg1,reg2,reg3 : tregister);
      begin
         exprasmList.concat(Taicpu.Op_reg_reg_reg(i,s,reg1,reg2,reg3));
      end;

    procedure emit_sym(i : tasmop;s : topsize;op : tasmsymbol);
      begin
        exprasmList.concat(Taicpu.Op_sym(i,s,op));
      end;

end.
{
  $Log$
  Revision 1.6  2003-10-09 21:31:37  daniel
    * Register allocator splitted, ans abstract now

  Revision 1.5  2003/10/01 20:34:50  peter
    * procinfo unit contains tprocinfo
    * cginfo renamed to cgbase
    * moved cgmessage to verbose
    * fixed ppc and sparc compiles

  Revision 1.4  2003/09/28 21:49:39  peter
    * removed emitjmp

  Revision 1.3  2003/09/03 15:55:02  peter
    * NEWRA branch merged

  Revision 1.2.2.1  2003/08/29 17:29:00  peter
    * next batch of updates

  Revision 1.2  2003/05/22 21:33:31  peter
    * removed some unit dependencies

  Revision 1.1  2003/04/30 20:53:32  florian
    * error when address of an abstract method is taken
    * fixed some x86-64 problems
    * merged some more x86-64 and i386 code

  Revision 1.38  2003/04/17 16:48:21  daniel
    * Added some code to keep track of move instructions in register
      allocator

  Revision 1.37  2003/03/08 08:59:07  daniel
    + $define newra will enable new register allocator
    + getregisterint will return imaginary registers with $newra
    + -sr switch added, will skip register allocation so you can see
      the direct output of the code generator before register allocation

  Revision 1.36  2003/02/19 22:00:15  daniel
    * Code generator converted to new register notation
    - Horribily outdated todo.txt removed

  Revision 1.35  2003/01/13 14:54:34  daniel
    * Further work to convert codegenerator register convention;
      internalerror bug fixed.

  Revision 1.34  2003/01/08 18:43:57  daniel
   * Tregister changed into a record

  Revision 1.33  2002/07/01 18:46:29  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.32  2002/05/18 13:34:21  peter
    * readded missing revisions

  Revision 1.31  2002/05/16 19:46:50  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.29  2002/05/13 19:54:37  peter
    * removed n386ld and n386util units
    * maybe_save/maybe_restore added instead of the old maybe_push

  Revision 1.28  2002/05/12 16:53:16  peter
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

  Revision 1.27  2002/04/25 20:16:39  peter
    * moved more routines from cga/n386util

  Revision 1.26  2002/04/21 15:29:53  carl
  * changeregsize -> rg.makeregsize

  Revision 1.25  2002/04/20 21:37:07  carl
  + generic FPC_CHECKPOINTER
  + first parameter offset in stack now portable
  * rename some constants
  + move some cpu stuff to other units
  - remove unused constents
  * fix stacksize for some targets
  * fix generic size problems which depend now on EXTEND_SIZE constant
  * removing frame pointer in routines is only available for : i386,m68k and vis targets

  Revision 1.24  2002/04/19 15:39:34  peter
    * removed some more routines from cga
    * moved location_force_reg/mem to ncgutil
    * moved arrayconstructnode secondpass to ncgld

  Revision 1.23  2002/04/15 19:44:20  peter
    * fixed stackcheck that would be called recursively when a stack
      error was found
    * generic changeregsize(reg,size) for i386 register resizing
    * removed some more routines from cga unit
    * fixed returnvalue handling
    * fixed default stacksize of linux and go32v2, 8kb was a bit small :-)

  Revision 1.22  2002/04/14 20:54:17  carl
  + stack checking enabled for all targets (it is simulated now)

  Revision 1.21  2002/04/04 19:06:08  peter
    * removed unused units
    * use tlocation.size in cg.a_*loc*() routines

  Revision 1.20  2002/04/04 18:30:22  carl
  + added wdosx support (patch from Pavel)

  Revision 1.19  2002/04/02 17:11:33  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods

  Revision 1.18  2002/03/31 20:26:37  jonas
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

  Revision 1.17  2002/03/28 16:07:52  armin
  + initialize threadvars defined local in units

  Revision 1.16  2002/03/04 19:10:12  peter
    * removed compiler warnings

  Revision 1.15  2002/01/24 18:25:53  peter
   * implicit result variable generation for assembler routines
   * removed m_tp modeswitch, use m_tp7 or not(m_fpc) instead

  Revision 1.14  2002/01/19 14:21:17  peter
    * fixed init/final for value parameters

}
