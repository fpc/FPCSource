{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

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

{$i defines.inc}

interface

    uses
       cpuinfo,cpubase,cpuasm,cginfo,
       symconst,symtype,symdef,aasm;

{$define TESTGETTEMP to store const that
 are written into temps for later release PM }

    function def_opsize(p1:tdef):topsize;
    function def_getreg(p1:tdef):tregister;

    procedure emitjmp(c : tasmcond;var l : tasmlabel);

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
       cgbase,cgobj,tgobj,rgobj,rgcpu;


{*****************************************************************************
                                Helpers
*****************************************************************************}

    function def_opsize(p1:tdef):topsize;
      begin
        case p1.size of
         1 : def_opsize:=S_B;
         2 : def_opsize:=S_W;
         4 : def_opsize:=S_L;
         { I don't know if we need it (FK) }
         8 : def_opsize:=S_L;
        else
         internalerror(130820001);
        end;
      end;


    function def_getreg(p1:tdef):tregister;
      begin
        def_getreg:=rg.makeregsize(rg.getregisterint(exprasmlist),int_cgsize(p1.size));
      end;


{*****************************************************************************
                              Emit Assembler
*****************************************************************************}

    procedure emitjmp(c : tasmcond;var l : tasmlabel);
      var
        ai : taicpu;
      begin
        if c=C_None then
          ai := Taicpu.Op_sym(A_JMP,S_NO,l)
        else
          begin
            ai:=Taicpu.Op_sym(A_Jcc,S_NO,l);
            ai.SetCondition(c);
          end;
        ai.is_jmp:=true;
        exprasmList.concat(ai);
      end;


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
      begin
         if (reg1<>reg2) or (i<>A_MOV) then
           exprasmList.concat(Taicpu.Op_reg_reg(i,s,reg1,reg2));
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
  Revision 1.29  2002-05-13 19:54:37  peter
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

  Revision 1.13  2001/12/30 17:24:45  jonas
    * range checking is now processor independent (part in cgobj,
      part in cg64f32) and should work correctly again (it needed
      some changes after the changes of the low and high of
      tordef's to int64)
    * maketojumpbool() is now processor independent (in ncgutil)
    * getregister32 is now called getregisterint

  Revision 1.12  2001/12/29 15:28:58  jonas
    * powerpc/cgcpu.pas compiles :)
    * several powerpc-related fixes
    * cpuasm unit is now based on common tainst unit
    + nppcmat unit for powerpc (almost complete)

  Revision 1.11  2001/11/18 18:59:59  peter
    * changed aktprocsym to aktprocdef for stabs generation

  Revision 1.10  2001/11/06 16:39:02  jonas
    * moved call to "cleanup_regvars" to cga.pas for i386 because it has
      to insert "fstp %st0" instructions after the exit label

  Revision 1.9  2001/11/02 22:58:09  peter
    * procsym definition rewrite

  Revision 1.8  2001/10/25 21:22:41  peter
    * calling convention rewrite

  Revision 1.7  2001/10/20 17:22:57  peter
    * concatcopy could release a wrong reference because the offset was
      increased without restoring the original before the release of
      a temp

  Revision 1.6  2001/10/14 11:49:51  jonas
    * finetuned register allocation info for assignments

  Revision 1.5  2001/09/30 21:28:34  peter
    * int64->boolean fixed

  Revision 1.4  2001/08/30 20:13:57  peter
    * rtti/init table updates
    * rttisym for reusable global rtti/init info
    * support published for interfaces

  Revision 1.3  2001/08/29 12:01:47  jonas
    + support for int64 LOC_REGISTERS in remove_non_regvars_from_loc

  Revision 1.2  2001/08/26 13:36:52  florian
    * some cg reorganisation
    * some PPC updates

  Revision 1.29  2001/08/12 20:23:02  peter
    * netbsd doesn't use stackchecking

  Revision 1.28  2001/08/07 18:47:13  peter
    * merged netbsd start
    * profile for win32

  Revision 1.27  2001/08/06 21:40:49  peter
    * funcret moved from tprocinfo to tprocdef

  Revision 1.26  2001/07/30 20:59:28  peter
    * m68k updates from v10 merged

  Revision 1.25  2001/07/01 20:16:18  peter
    * alignmentinfo record added
    * -Oa argument supports more alignment settings that can be specified
      per type: PROC,LOOP,VARMIN,VARMAX,CONSTMIN,CONSTMAX,RECORDMIN
      RECORDMAX,LOCALMIN,LOCALMAX. It is possible to set the mimimum
      required alignment and the maximum usefull alignment. The final
      alignment will be choosen per variable size dependent on these
      settings

  Revision 1.24  2001/05/27 14:30:55  florian
    + some widestring stuff added

  Revision 1.23  2001/04/21 13:33:16  peter
    * move winstackpagesize const to cgai386 to remove uses t_win32

  Revision 1.22  2001/04/21 12:05:32  peter
    * add nop after popa (merged)

  Revision 1.21  2001/04/18 22:02:00  peter
    * registration of targets and assemblers

  Revision 1.20  2001/04/13 01:22:17  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.19  2001/04/05 21:33:07  peter
    * fast exit fix merged

  Revision 1.18  2001/04/02 21:20:35  peter
    * resulttype rewrite

  Revision 1.17  2001/01/05 17:36:58  florian
  * the info about exception frames is stored now on the stack
  instead on the heap

  Revision 1.16  2000/12/25 00:07:31  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.15  2000/12/05 11:44:32  jonas
    + new integer regvar handling, should be much more efficient

  Revision 1.14  2000/11/29 00:30:43  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.13  2000/11/28 00:28:07  pierre
   * stabs fixing

  Revision 1.12  2000/11/22 15:12:06  jonas
    * fixed inline-related problems (partially "merges")

  Revision 1.11  2000/11/17 10:30:24  florian
    * passing interfaces as parameters fixed

  Revision 1.10  2000/11/07 23:40:48  florian
    + AfterConstruction and BeforeDestruction impemented

  Revision 1.9  2000/11/06 23:49:20  florian
    * fixed init_paras call

  Revision 1.8  2000/11/06 23:15:01  peter
    * added copyvaluepara call again

  Revision 1.7  2000/11/04 14:25:23  florian
    + merged Attila's changes for interfaces, not tested yet

  Revision 1.6  2000/10/31 22:02:55  peter
    * symtable splitted, no real code changes

  Revision 1.5  2000/10/24 22:23:04  peter
    * emitcall -> emitinsertcall for profiling (merged)

  Revision 1.4  2000/10/24 12:47:45  jonas
    * allocate registers which hold function result

  Revision 1.3  2000/10/24 08:54:25  michael
  + Extra patch from peter

  Revision 1.2  2000/10/24 07:20:03  pierre
   * fix for bug 1193 (merged)

  Revision 1.1  2000/10/15 09:47:42  peter
    * moved to i386/

  Revision 1.19  2000/10/14 10:14:46  peter
    * moehrendorf oct 2000 rewrite

  Revision 1.18  2000/10/10 14:55:28  jonas
    * added missing regallocs for edi in emit_mov_ref_reg64 (merged)

  Revision 1.17  2000/10/01 19:48:23  peter
    * lot of compile updates for cg11

  Revision 1.16  2000/09/30 16:08:45  peter
    * more cg11 updates

  Revision 1.15  2000/09/24 15:06:12  peter
    * use defines.inc

  Revision 1.14  2000/09/16 12:22:52  peter
    * freebsd support merged

  Revision 1.13  2000/08/27 16:11:49  peter
    * moved some util functions from globals,cobjects to cutils
    * splitted files into finput,fmodule

  Revision 1.12  2000/08/24 19:07:54  peter
    * don't initialize if localvarsym is set because that varsym will
      already be initialized
    * first initialize local data before copy of value para's (merged)

  Revision 1.11  2000/08/19 20:09:33  peter
    * check size after checking openarray in push_value_para (merged)

  Revision 1.10  2000/08/16 13:06:06  florian
    + support of 64 bit integer constants

  Revision 1.9  2000/08/10 18:42:03  peter
    * fixed for constants in emit_push_mem_size for go32v2 (merged)

  Revision 1.8  2000/08/07 11:29:40  jonas
    + emit_push_mem_size() which pushes a value in memory of a certain size
    * pushsetelement() and pushvaluepara() use this new procedure, because
      otherwise they could sometimes try to push data past the end of the
      heap, causing a crash
     (merged from fixes branch)

  Revision 1.7  2000/08/03 13:17:25  jonas
    + allow regvars to be used inside inlined procs, which required  the
      following changes:
        + load regvars in genentrycode/free them in genexitcode (cgai386)
        * moved all regvar related code to new regvars unit
        + added pregvarinfo type to hcodegen
        + added regvarinfo field to tprocinfo (symdef/symdefh)
        * deallocate the regvars of the caller in secondprocinline before
          inlining the called procedure and reallocate them afterwards

  Revision 1.6  2000/08/02 08:05:04  jonas
    * fixed web bug1087
    * allocate R_ECX explicitely if it's used
    (merged from fixes branch)

  Revision 1.5  2000/07/27 09:25:05  jonas
    * moved locflags2reg() procedure from cg386add to cgai386
    + added locjump2reg() procedure to cgai386
    * fixed internalerror(2002) when the result of a case expression has
      LOC_JUMP
    (all merged from fixes branch)

  Revision 1.4  2000/07/21 15:14:02  jonas
    + added is_addr field for labels, if they are only used for getting the address
       (e.g. for io checks) and corresponding getaddrlabel() procedure

  Revision 1.3  2000/07/13 12:08:25  michael
  + patched to 1.1.0 with former 1.09patch from peter

  Revision 1.2  2000/07/13 11:32:37  michael
  + removed logs

}
