{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    This unit implements the i386 specific class for the register
    allocator

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

unit rgcpu;

{$i defines.inc}

  interface

    uses
      cpubase,
      cpuinfo,
      cpuasm,
      tainst,
      cclasses,globtype,cgbase,aasm,rgobj;

    type
       trgcpu = class(trgobj)

          { to keep the same allocation order as with the old routines }
          function getregisterint(list: taasmoutput): tregister; override;
          procedure ungetregisterint(list: taasmoutput; r : tregister); override;
          function getexplicitregisterint(list: taasmoutput; r : tregister) : tregister; override;

          function getregisterfpu(list: taasmoutput) : tregister; override;
          procedure ungetregisterfpu(list: taasmoutput; r : tregister); override;

          procedure ungetreference(list: taasmoutput; const ref : treference); override;

          { pushes and restores registers }
          procedure pushusedregisters(list: taasmoutput;
            var pushed : tpushedsaved;const s: tregisterset);
          procedure popusedregisters(list: taasmoutput;
            const pushed : tpushedsaved);

          procedure saveusedregisters(list: taasmoutput;
            var saved : tpushedsaved;const s: tregisterset);override;
          procedure restoreusedregisters(list: taasmoutput;
            const saved : tpushedsaved);override;

          procedure resetusableregisters;override;

         { corrects the fpu stack register by ofs }
         function correct_fpuregister(r : tregister;ofs : byte) : tregister;

         fpuvaroffset : byte;
       end;


  implementation

    uses
       systems,
       globals,verbose,
       cginfo,tgobj,cga;


    function trgcpu.getregisterint(list: taasmoutput): tregister;
      begin
         if countunusedregsint=0 then
           internalerror(10);
{$ifdef TEMPREGDEBUG}
         if curptree^.usableregsint-countunusedregsint>curptree^.registers32 then
           internalerror(10);
{$endif TEMPREGDEBUG}
{$ifdef EXTTEMPREGDEBUG}
         if curptree^.usableregs-countunusedregistersint>curptree^^.reallyusedregs then
           curptree^.reallyusedregs:=curptree^^.usableregs-countunusedregistersint;
{$endif EXTTEMPREGDEBUG}
         dec(countunusedregsint);
         if R_EAX in unusedregsint then
           begin
              exclude(unusedregsint,R_EAX);
              include(usedinproc,R_EAX);
              getregisterint:=R_EAX;
{$ifdef TEMPREGDEBUG}
              reg_user[R_EAX]:=curptree^;
{$endif TEMPREGDEBUG}
              exprasmlist.concat(tairegalloc.alloc(R_EAX));
           end
         else if R_EDX in unusedregsint then
           begin
              exclude(unusedregsint,R_EDX);
              include(usedinproc,R_EDX);
              getregisterint:=R_EDX;
{$ifdef TEMPREGDEBUG}
              reg_user[R_EDX]:=curptree^;
{$endif TEMPREGDEBUG}
              exprasmlist.concat(tairegalloc.alloc(R_EDX));
           end
         else if R_EBX in unusedregsint then
           begin
              exclude(unusedregsint,R_EBX);
              include(usedinproc,R_EBX);
              getregisterint:=R_EBX;
{$ifdef TEMPREGDEBUG}
              reg_user[R_EBX]:=curptree^;
{$endif TEMPREGDEBUG}
              exprasmlist.concat(tairegalloc.alloc(R_EBX));
           end
         else if R_ECX in unusedregsint then
           begin
              exclude(unusedregsint,R_ECX);
              include(usedinproc,R_ECX);
              getregisterint:=R_ECX;
{$ifdef TEMPREGDEBUG}
              reg_user[R_ECX]:=curptree^;
{$endif TEMPREGDEBUG}
              exprasmlist.concat(tairegalloc.alloc(R_ECX));
           end
         else internalerror(10);
{$ifdef TEMPREGDEBUG}
         testregisters;
{$endif TEMPREGDEBUG}
      end;

    procedure trgcpu.ungetregisterint(list: taasmoutput; r : tregister);
      begin
         if r=R_NO then
          exit;
         r := changeregsize(r,S_L);
         if (r = R_EDI) or
            ((not assigned(procinfo^._class)) and (r = R_ESI)) then
           begin
             list.concat(Tairegalloc.DeAlloc(r));
             exit;
           end;
         if not(r in [R_EAX,R_EBX,R_ECX,R_EDX]) then
           exit;
         inherited ungetregisterint(list,r);
      end;


   function trgcpu.getexplicitregisterint(list: taasmoutput; r : tregister) : tregister;
     begin
       if r in [R_ESI,R_EDI] then
         begin
           list.concat(Tairegalloc.Alloc(r));
           getexplicitregisterint := r;
           exit;
         end;
       result := inherited getexplicitregisterint(list,r);
    end;


    function trgcpu.getregisterfpu(list: taasmoutput) : tregister;

      begin
        { note: don't return R_ST0, see comments above implementation of }
        { a_loadfpu_* methods in cgcpu (JM)                              }
        result := R_ST;
      end;


    procedure trgcpu.ungetregisterfpu(list : taasmoutput; r : tregister);

      begin
        { nothing to do, fpu stack management is handled by the load/ }
        { store operations in cgcpu (JM)                              }
      end;


    procedure trgcpu.ungetreference(list: taasmoutput; const ref : treference);

      begin
         ungetregisterint(list,ref.base);
         ungetregisterint(list,ref.index);
      end;


    procedure trgcpu.pushusedregisters(list: taasmoutput;
        var pushed : tpushedsaved; const s: tregisterset);

      var
        r: tregister;
{$ifdef SUPPORT_MMX}
        hr : treference;
{$endif SUPPORT_MMX}
      begin
        usedinproc:=usedinproc + s;
        for r:=R_EAX to R_EBX do
          begin
            pushed[r].pushed:=false;
            { if the register is used by the calling subroutine    }
            if not is_reg_var[r] and
               (r in s) and
               { and is present in use }
               not(r in unusedregsint) then
              begin
                { then save it }
                list.concat(Taicpu.Op_reg(A_PUSH,S_L,r));
                include(unusedregsint,r);
                inc(countunusedregsint);
                pushed[r].pushed:=true;
              end;
          end;
{$ifdef SUPPORT_MMX}
        for r:=R_MM0 to R_MM6 do
          begin
            pushed[r].pushed:=false;
            { if the register is used by the calling subroutine    }
            if not is_reg_var[r] and
               (r in s) and
               { and is present in use }
               not(r in unusedregsmm) then
              begin
                list.concat(Taicpu.Op_const_reg(A_SUB,S_L,8,R_ESP));
                reference_reset_base(hr,R_ESP,0);
                list.concat(Taicpu.Op_reg_ref(A_MOVQ,S_NO,r,hr));
                include(unusedregsmm,r);
                inc(countunusedregsmm);
                pushed[r].pushed:=true;
              end;
          end;
{$endif SUPPORT_MMX}
{$ifdef TEMPREGDEBUG}
        testregisters;
{$endif TEMPREGDEBUG}
      end;


    procedure trgcpu.popusedregisters(list: taasmoutput;
        const pushed : tpushedsaved);

      var
        r : tregister;
{$ifdef SUPPORT_MMX}
        hr : treference;
{$endif SUPPORT_MMX}
      begin
        { restore in reverse order: }
{$ifdef SUPPORT_MMX}
        for r:=R_MM6 downto R_MM0 do
          if pushed[r].pushed then
            begin
              reference_reset_base(hr,R_ESP,0);
              list.concat(Taicpu.Op_ref_reg(
                A_MOVQ,S_NO,hr,r));
              list.concat(Taicpu.Op_const_reg(
                A_ADD,S_L,8,R_ESP));
              if not (r in unusedregsmm) then
                { internalerror(10)
                  in cg386cal we always restore regs
                  that appear as used
                  due to a unused tmep storage PM }
              else
                dec(countunusedregsmm);
              exclude(unusedregsmm,r);
            end;
{$endif SUPPORT_MMX}
        for r:=R_EBX downto R_EAX do
          if pushed[r].pushed then
            begin
              list.concat(Taicpu.Op_reg(A_POP,S_L,r));
              if not (r in unusedregsint) then
                { internalerror(10)
                  in cg386cal we always restore regs
                  that appear as used
                  due to a unused tmep storage PM }
              else
                dec(countunusedregsint);
              exclude(unusedregsint,r);
            end;
{$ifdef TEMPREGDEBUG}
        testregisters;
{$endif TEMPREGDEBUG}
      end;

    procedure trgcpu.saveusedregisters(list: taasmoutput;var saved : tpushedsaved;
      const s: tregisterset);

      begin
        if (aktoptprocessor in [class386,classP5]) or
           (CS_LittleSize in aktglobalswitches) then
          pushusedregisters(list,saved,s)
        else
          inherited saveusedregisters(list,saved,s);
      end;


    procedure trgcpu.restoreusedregisters(list: taasmoutput;
      const saved : tpushedsaved);

      begin
        if (aktoptprocessor in [class386,classP5]) or
           (CS_LittleSize in aktglobalswitches) then
          popusedregisters(list,saved)
        else
          inherited restoreusedregisters(list,saved);
      end;


   procedure trgcpu.resetusableregisters;

     begin
       inherited resetusableregisters;
       fpuvaroffset := 0;
     end;


   function trgcpu.correct_fpuregister(r : tregister;ofs : byte) : tregister;

     begin
        correct_fpuregister:=tregister(longint(r)+ofs);
     end;


initialization
  rg := trgcpu.create;
end.

{
  $Log$
  Revision 1.4  2002-04-15 19:44:22  peter
    * fixed stackcheck that would be called recursively when a stack
      error was found
    * generic changeregsize(reg,size) for i386 register resizing
    * removed some more routines from cga unit
    * fixed returnvalue handling
    * fixed default stacksize of linux and go32v2, 8kb was a bit small :-)

  Revision 1.3  2002/04/04 19:06:13  peter
    * removed unused units
    * use tlocation.size in cg.a_*loc*() routines

  Revision 1.2  2002/04/02 17:11:39  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods

  Revision 1.1  2002/03/31 20:26:40  jonas
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
