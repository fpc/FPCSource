{
    $Id$
    Copyright (C) 1998-2000 by Florian Klaempfl

    This unit handles the temporary variables stuff for i386

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
unit tgeni386;

  interface

    uses
       cobjects,globals,tree,hcodegen,verbose,files,aasm,
       cpubase,cpuasm
       ;

    type
       tregisterset = set of tregister;

       tpushed = array[R_EAX..R_MM6] of boolean;
       tsaved = array[R_EAX..R_MM6] of longint;

    const
       usablereg32 : byte = 4;

       { this value is used in tsaved, if the register isn't saved }
       reg_not_saved = $7fffffff;
{$ifdef SUPPORT_MMX}
       usableregmmx : byte = 8;
{$endif SUPPORT_MMX}

{$ifdef TEMPREGDEBUG}
    procedure testregisters32;
{$endif TEMPREGDEBUG}
    function getregister32 : tregister;
    procedure ungetregister32(r : tregister);
    { tries to allocate the passed register, if possible }
    function getexplicitregister32(r : tregister) : tregister;
{$ifdef SUPPORT_MMX}
    function getregistermmx : tregister;
    procedure ungetregistermmx(r : tregister);
{$endif SUPPORT_MMX}

    procedure ungetregister(r : tregister);

    procedure cleartempgen;
    procedure del_reference(const ref : treference);
    procedure del_locref(const location : tlocation);
    procedure del_location(const l : tlocation);

    { pushs and restores registers }
    procedure pushusedregisters(var pushed : tpushed;b : byte);
    procedure popusedregisters(const pushed : tpushed);

    { saves and restores used registers to temp. values }
    procedure saveusedregisters(var saved : tsaved;b : byte);
    procedure restoreusedregisters(const saved : tsaved);

    procedure clearregistercount;
    procedure resetusableregisters;

    { corrects the fpu stack register by ofs }
    function correct_fpuregister(r : tregister;ofs : byte) : tregister;

    var
       unused,usableregs : tregisterset;
       c_usableregs : longint;

       { uses only 1 byte while a set uses in FPC 32 bytes }
       usedinproc : byte;

       fpuvaroffset : byte;

       { count, how much a register must be pushed if it is used as register }
       { variable                                                           }
{$ifdef SUPPORT_MMX}
       reg_pushes : array[R_EAX..R_MM6] of longint;
       is_reg_var : array[R_EAX..R_MM6] of boolean;
{$ifdef TEMPREGDEBUG}
       reg_user   : array[R_EAX..R_MM6] of ptree;
       reg_releaser : array[R_EAX..R_MM6] of ptree;
{$endif TEMPREGDEBUG}
{$else SUPPORT_MMX}
       reg_pushes : array[R_EAX..R_EDI] of longint;
       is_reg_var : array[R_EAX..R_EDI] of boolean;
{$ifdef TEMPREGDEBUG}
       reg_user   : array[R_EAX..R_EDI] of ptree;
       reg_releaser : array[R_EAX..R_EDI] of ptree;
{$endif TEMPREGDEBUG}
{$endif SUPPORT_MMX}


implementation

    uses
      globtype,temp_gen;

    procedure pushusedregisters(var pushed : tpushed;b : byte);

      var
         r : tregister;
{$ifdef SUPPORT_MMX}
         hr : preference;
{$endif}
      begin
         usedinproc:=usedinproc or b;
         for r:=R_EAX to R_EBX do
           begin
              pushed[r]:=false;
              { if the register is used by the calling subroutine    }
              if ((b and ($80 shr byte(r)))<>0) then
                begin
                   { and is present in use }
                   if not(r in unused) then
                     begin
                        { then save it }
                        exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,S_L,r)));

                        { here was a big problem  !!!!!}
                        { you cannot do that for a register that is
                        globally assigned to a var
                        this also means that you must push it much more
                        often, but there must be a better way
                        maybe by putting the value back to the stack !! }
                        if not(is_reg_var[r]) then
                          begin
                            unused:=unused+[r];
{$ifdef TEMPREGDEBUG}
                            inc(usablereg32);
{$endif TEMPREGDEBUG}
                          end;
                        pushed[r]:=true;
                     end;
                end;
           end;
{$ifdef SUPPORT_MMX}
         for r:=R_MM0 to R_MM6 do
           begin
              pushed[r]:=false;
              { if the mmx register is in use, save it }
              if not(r in unused) then
                begin
                   exprasmlist^.concat(new(paicpu,op_const_reg(
                     A_SUB,S_L,8,R_ESP)));
                   new(hr);
                   reset_reference(hr^);
                   hr^.base:=R_ESP;
                   exprasmlist^.concat(new(paicpu,op_reg_ref(
                     A_MOVQ,S_NO,r,hr)));
                   if not(is_reg_var[r]) then
                     begin
                       unused:=unused+[r];
{$ifdef TEMPREGDEBUG}
                       inc(usableregmmx);
{$endif TEMPREGDEBUG}
                     end;
                   pushed[r]:=true;
                end;
           end;
{$endif SUPPORT_MMX}
{$ifdef TEMPREGDEBUG}
        testregisters32;
{$endif TEMPREGDEBUG}
      end;

    procedure saveusedregisters(var saved : tsaved;b : byte);

      var
         r : tregister;
         hr : treference;

      begin
         usedinproc:=usedinproc or b;
         for r:=R_EAX to R_EBX do
           begin
              saved[r]:=reg_not_saved;
              { if the register is used by the calling subroutine    }
              if ((b and ($80 shr byte(r)))<>0) then
                begin
                   { and is present in use }
                   if not(r in unused) then
                     begin
                        { then save it }
                        gettempofsizereference(4,hr);
                        saved[r]:=hr.offset;
                        exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_L,r,newreference(hr))));
                        { here was a big problem  !!!!!}
                        { you cannot do that for a register that is
                        globally assigned to a var
                        this also means that you must push it much more
                        often, but there must be a better way
                        maybe by putting the value back to the stack !! }
                        if not(is_reg_var[r]) then
                          begin
                            unused:=unused+[r];
{$ifdef TEMPREGDEBUG}
                            inc(usablereg32);
{$endif TEMPREGDEBUG}
                          end;
                     end;
                end;
           end;
{$ifdef SUPPORT_MMX}
         for r:=R_MM0 to R_MM6 do
           begin
              saved[r]:=reg_not_saved;
              { if the mmx register is in use, save it }
              if not(r in unused) then
                begin
                   gettempofsizereference(8,hr);
                   exprasmlist^.concat(new(paicpu,op_reg_ref(
                     A_MOVQ,S_NO,r,newreference(hr))));
                   if not(is_reg_var[r]) then
                     begin
                       unused:=unused+[r];
{$ifdef TEMPREGDEBUG}
                       inc(usableregmmx);
{$endif TEMPREGDEBUG}
                     end;
                   saved[r]:=hr.offset;
                end;
           end;
{$endif SUPPORT_MMX}
{$ifdef TEMPREGDEBUG}
        testregisters32;
{$endif TEMPREGDEBUG}
      end;

    procedure popusedregisters(const pushed : tpushed);

      var
         r : tregister;
{$ifdef SUPPORT_MMX}
         hr : preference;
{$endif SUPPORT_MMX}
      begin
         { restore in reverse order: }
{$ifdef SUPPORT_MMX}
         for r:=R_MM6 downto R_MM0 do
           begin
              if pushed[r] then
                begin
                   new(hr);
                   reset_reference(hr^);
                   hr^.base:=R_ESP;
                   exprasmlist^.concat(new(paicpu,op_ref_reg(
                     A_MOVQ,S_NO,hr,r)));
                   exprasmlist^.concat(new(paicpu,op_const_reg(
                     A_ADD,S_L,8,R_ESP)));
                   unused:=unused-[r];
{$ifdef TEMPREGDEBUG}
                   dec(usableregmmx);
{$endif TEMPREGDEBUG}
                end;
           end;
{$endif SUPPORT_MMX}
         for r:=R_EBX downto R_EAX do
           if pushed[r] then
             begin
                exprasmlist^.concat(new(paicpu,op_reg(A_POP,S_L,r)));
{$ifdef TEMPREGDEBUG}
                if not (r in unused) then
                  { internalerror(10)
                    in cg386cal we always restore regs
                    that appear as used
                    due to a unused tmep storage PM }
                else
                  dec(usablereg32);
{$endif TEMPREGDEBUG}
                unused:=unused-[r];
             end;
{$ifdef TEMPREGDEBUG}
        testregisters32;
{$endif TEMPREGDEBUG}
      end;

    procedure restoreusedregisters(const saved : tsaved);
      var
         r : tregister;
         hr : treference;

      begin
         { restore in reverse order: }
{$ifdef SUPPORT_MMX}
         for r:=R_MM6 downto R_MM0 do
           begin
              if saved[r]<>reg_not_saved then
                begin
                   reset_reference(hr);
                   hr.base:=frame_pointer;
                   hr.offset:=saved[r];
                   exprasmlist^.concat(new(paicpu,op_ref_reg(
                     A_MOVQ,S_NO,newreference(hr),r)));
                   unused:=unused-[r];
{$ifdef TEMPREGDEBUG}
                   dec(usableregmmx);
{$endif TEMPREGDEBUG}
                   ungetiftemp(hr);
                end;
           end;
{$endif SUPPORT_MMX}
         for r:=R_EBX downto R_EAX do
           if saved[r]<>reg_not_saved then
             begin
                reset_reference(hr);
                hr.base:=frame_pointer;
                hr.offset:=saved[r];
                exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOV,S_L,newreference(hr),r)));
{$ifdef TEMPREGDEBUG}
                if not (r in unused) then
                  internalerror(10)
                else
                  dec(usablereg32);
{$endif TEMPREGDEBUG}
                unused:=unused-[r];
                ungetiftemp(hr);
             end;
{$ifdef TEMPREGDEBUG}
        testregisters32;
{$endif TEMPREGDEBUG}
      end;

    procedure ungetregister(r : tregister);

      begin
         if r in [R_EAX,R_ECX,R_EDX,R_EBX,R_ESP,R_EBP,R_ESI,R_EDI] then
           ungetregister32(r)
          else if r in [R_AX,R_CX,R_DX,R_BX,R_SP,R_BP,R_SI,R_DI] then
           ungetregister32(reg16toreg32(r))
         else if r in [R_AL,R_BL,R_CL,R_DL] then
           ungetregister32(reg8toreg32(r))
{$ifdef SUPPORT_MMX}
         else if r in [R_MM0..R_MM6] then
           ungetregistermmx(r)
{$endif SUPPORT_MMX}
         else internalerror(18);
      end;

    procedure ungetregister32(r : tregister);

      begin
         if cs_regalloc in aktglobalswitches then
           begin
              { takes much time }
              if not(r in usableregs) then
                exit;
              unused:=unused+[r];
              inc(usablereg32);
           end
         else
           begin
              if not(r in [R_EAX,R_EBX,R_ECX,R_EDX]) then
                exit;
{$ifdef TEMPREGDEBUG}
                if (r in unused) then
{$ifdef EXTTEMPREGDEBUG}
                  begin
                    Comment(V_Debug,'register freed twice '+reg2str(r));
                    testregisters32;
                    exit;
                  end
{$else EXTTEMPREGDEBUG}
                  exit
{$endif EXTTEMPREGDEBUG}
                else
{$endif TEMPREGDEBUG}
                  inc(usablereg32);
              unused:=unused+[r];
{$ifdef TEMPREGDEBUG}
              reg_releaser[r]:=curptree^;
{$endif TEMPREGDEBUG}
           end;
         exprasmlist^.concat(new(pairegalloc,dealloc(r)));
{$ifdef TEMPREGDEBUG}
        testregisters32;
{$endif TEMPREGDEBUG}
      end;

{$ifdef SUPPORT_MMX}
    function getregistermmx : tregister;

      var
         r : tregister;

      begin
         dec(usableregmmx);
         for r:=R_MM0 to R_MM6 do
           if r in unused then
             begin
                unused:=unused-[r];
                usedinproc:=usedinproc or ($80 shr byte(R_EAX));
                getregistermmx:=r;
                exit;
             end;
         internalerror(10);
      end;

    procedure ungetregistermmx(r : tregister);

      begin
         if cs_regalloc in aktglobalswitches then
           begin
              { takes much time }
              if not(r in usableregs) then
                exit;
              unused:=unused+[r];
              inc(usableregmmx);
           end
         else
           begin
              unused:=unused+[r];
              inc(usableregmmx);
           end;
      end;
{$endif SUPPORT_MMX}

    procedure del_reference(const ref : treference);

      begin
         if ref.is_immediate then
           exit;
         ungetregister32(ref.base);
         ungetregister32(ref.index);
      end;


    procedure del_locref(const location : tlocation);
      begin
         if (location.loc<>loc_mem) and (location.loc<>loc_reference) then
           exit;
         if location.reference.is_immediate then
           exit;
         ungetregister32(location.reference.base);
         ungetregister32(location.reference.index);
      end;


    procedure del_location(const l : tlocation);
      begin
        case l.loc of
          LOC_REGISTER :
            ungetregister(l.register);
          LOC_MEM,LOC_REFERENCE :
            del_reference(l.reference);
        end;
      end;


{$ifdef TEMPREGDEBUG}
    procedure testregisters32;
     var test : byte;
       begin
         test:=0;
         if R_EAX in unused then
           inc(test);
         if R_EBX in unused then
           inc(test);
         if R_ECX in unused then
           inc(test);
         if R_EDX in unused then
           inc(test);
         if test<>usablereg32 then
           internalerror(10);
       end;
{$endif TEMPREGDEBUG}

    function getregister32 : tregister;
      begin
         if usablereg32=0 then
           internalerror(10);
         dec(usablereg32);
{$ifdef TEMPREGDEBUG}
         if curptree^^.usableregs-usablereg32>curptree^^.registers32 then
           internalerror(10);
{$endif TEMPREGDEBUG}
{$ifdef EXTTEMPREGDEBUG}
         if curptree^^.usableregs-usablereg32>curptree^^.reallyusedregs then
           curptree^^.reallyusedregs:=curptree^^.usableregs-usablereg32;
{$endif EXTTEMPREGDEBUG}
         if R_EAX in unused then
           begin
              unused:=unused-[R_EAX];
              usedinproc:=usedinproc or ($80 shr byte(R_EAX));
              getregister32:=R_EAX;
{$ifdef TEMPREGDEBUG}
              reg_user[R_EAX]:=curptree^;
{$endif TEMPREGDEBUG}
              exprasmlist^.concat(new(pairegalloc,alloc(R_EAX)));
           end
         else if R_EDX in unused then
           begin
              unused:=unused-[R_EDX];
              usedinproc:=usedinproc or ($80 shr byte(R_EDX));
              getregister32:=R_EDX;
{$ifdef TEMPREGDEBUG}
              reg_user[R_EDX]:=curptree^;
{$endif TEMPREGDEBUG}
              exprasmlist^.concat(new(pairegalloc,alloc(R_EDX)));
           end
         else if R_EBX in unused then
           begin
              unused:=unused-[R_EBX];
              usedinproc:=usedinproc or ($80 shr byte(R_EBX));
              getregister32:=R_EBX;
{$ifdef TEMPREGDEBUG}
              reg_user[R_EBX]:=curptree^;
{$endif TEMPREGDEBUG}
              exprasmlist^.concat(new(pairegalloc,alloc(R_EBX)));
           end
         else if R_ECX in unused then
           begin
              unused:=unused-[R_ECX];
              usedinproc:=usedinproc or ($80 shr byte(R_ECX));
              getregister32:=R_ECX;
{$ifdef TEMPREGDEBUG}
              reg_user[R_ECX]:=curptree^;
{$endif TEMPREGDEBUG}
              exprasmlist^.concat(new(pairegalloc,alloc(R_ECX)));
           end
         else internalerror(10);
{$ifdef TEMPREGDEBUG}
         testregisters32;
{$endif TEMPREGDEBUG}
      end;

    function getexplicitregister32(r : tregister) : tregister;

      begin
         if r in unused then
           begin
              dec(usablereg32);
{$ifdef TEMPREGDEBUG}
              if curptree^^.usableregs-usablereg32>curptree^^.registers32 then
                internalerror(10);
              reg_user[r]:=curptree^;
{$endif TEMPREGDEBUG}
              unused:=unused-[r];
              usedinproc:=usedinproc or ($80 shr byte(r));
              exprasmlist^.concat(new(pairegalloc,alloc(r)));
              getexplicitregister32:=r;
{$ifdef TEMPREGDEBUG}
         testregisters32;
{$endif TEMPREGDEBUG}
           end
         else
           getexplicitregister32:=getregister32;
      end;

    procedure cleartempgen;

      begin
         unused:=usableregs;
         usablereg32:=c_usableregs;
         {fpuvaroffset:=0;
          this must only be resetted at each procedure
          compilation start PM }
      end;


   procedure clearregistercount;
      var
        regi : tregister;
      begin
{$ifdef SUPPORT_MMX}
         for regi:=R_EAX to R_MM6 do
           begin
              reg_pushes[regi]:=0;
              is_reg_var[regi]:=false;
           end;
{$else SUPPORT_MMX}
         for regi:=R_EAX to R_EDI do
           begin
              reg_pushes[regi]:=0;
              is_reg_var[regi]:=false;
           end;
{$endif SUPPORT_MMX}
      end;

   function correct_fpuregister(r : tregister;ofs : byte) : tregister;

     begin
        correct_fpuregister:=tregister(longint(r)+ofs);
     end;

   procedure resetusableregisters;
      begin
{$ifdef SUPPORT_MMX}
        usableregs:=[R_EAX,R_EBX,R_ECX,R_EDX,R_MM0..R_MM6];
        c_usableregs:=4;
        usableregmmx:=8;
{$else}
        usableregs:=[R_EAX,R_EBX,R_ECX,R_EDX];
        c_usableregs:=4;
{$endif SUPPORT_MMX}
        fpuvaroffset:=0;
      end;

begin
  resetusableregisters;
end.
{
  $Log$
  Revision 1.37  2000-01-07 01:14:47  peter
    * updated copyright to 2000

  Revision 1.36  1999/11/06 14:34:31  peter
    * truncated log to 20 revs

  Revision 1.35  1999/09/27 23:45:02  peter
    * procinfo is now a pointer
    * support for result setting in sub procedure

  Revision 1.34  1999/08/27 10:38:32  pierre
   + EXTTEMPREGDEBUG code added

  Revision 1.33  1999/08/25 12:00:06  jonas
    * changed pai386, paippc and paiapha (same for tai*) to paicpu (taicpu)

  Revision 1.32  1999/08/23 23:25:58  pierre
    + TEMPREGDEBUG code, test of register allocation
      if a tree uses more than registers32 regs then
      internalerror(10) is issued
    + EXTTEMPREGDEBUG will also give internalerror(10) if
      a same register is freed twice (happens in several part
      of current compiler like addn for strings and sets)

  Revision 1.31  1999/08/10 12:47:55  pierre
   * fpuvaroffset problems solved

  Revision 1.30  1999/08/04 13:45:32  florian
    + floating point register variables !!
    * pairegalloc is now generated for register variables

  Revision 1.29  1999/08/04 00:23:48  florian
    * renamed i386asm and i386base to cpuasm and cpubase

  Revision 1.28  1999/08/02 17:17:11  florian
    * small changes for the new code generator

  Revision 1.27  1999/06/09 23:22:39  peter
    + del_location

  Revision 1.26  1999/05/27 19:45:27  peter
    * removed oldasm
    * plabel -> pasmlabel
    * -a switches to source writing automaticly
    * assembler readers OOPed
    * asmsymbol automaticly external
    * jumptables and other label fixes for asm readers

  Revision 1.25  1999/05/19 22:00:48  florian
    * some new routines for register management:
       maybe_savetotemp,restorefromtemp, saveusedregisters,
       restoreusedregisters

  Revision 1.24  1999/05/18 21:58:34  florian
    * fixed some bugs related to temp. ansistrings and functions results
      which return records/objects/arrays which need init/final.

  Revision 1.23  1999/05/01 13:25:01  peter
    * merged nasm compiler
    * old asm moved to oldasm/

  Revision 1.22  1999/04/21 16:31:48  pierre
  ra386att.pas

  Revision 1.21  1999/04/16 11:49:47  peter
    + tempalloc
    + -at to show temp alloc info in .s file

  Revision 1.20  1999/02/25 21:02:55  peter
    * ag386bin updates
    + coff writer

  Revision 1.19  1999/02/22 02:15:58  peter
    * updates for ag386bin

  Revision 1.18  1999/01/18 16:02:20  pierre
   * better error info with -Co

  Revision 1.17  1998/12/11 23:36:09  florian
    + again more stuff for int64/qword:
         - comparision operators
         - code generation for: str, read(ln), write(ln)

  Revision 1.16  1998/12/11 17:22:40  florian
    * fixed previous commit bug fix of getexplicitregister32
      (usableregs32 was decremented twice, thnaks Pierre for that hint)

}

