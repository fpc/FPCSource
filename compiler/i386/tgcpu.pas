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
unit tgcpu;

{$i defines.inc}

interface

    uses
       globals,
       cgbase,verbose,aasm,
       node,
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

    var
       { tries to hold the amount of times which the current tree is processed  }
       t_times : longint;

{$ifdef TEMPREGDEBUG}
    procedure testregisters32;
{$endif TEMPREGDEBUG}
    function getregister32 : tregister;
    function getaddressregister: tregister;
    procedure ungetregister32(r : tregister);
    { tries to allocate the passed register, if possible }
    function getexplicitregister32(r : tregister) : tregister;
{$ifdef SUPPORT_MMX}
    function getregistermmx : tregister;
    procedure ungetregistermmx(r : tregister);
{$endif SUPPORT_MMX}

    function isaddressregister(reg: tregister): boolean;

    procedure ungetregister(r : tregister);

    procedure cleartempgen;
    procedure del_reference(const ref : treference);
    procedure del_locref(const location : tlocation);
    procedure del_location(const l : tlocation);

    { pushs and restores registers }
    procedure pushusedregisters(var pushed : tpushed;b : byte);
    procedure popusedregisters(const pushed : tpushed);

    { saves register variables (restoring happens automatically (JM) }
    procedure saveregvars(b: byte);

    { saves and restores used registers to temp. values }
    procedure saveusedregisters(var saved : tsaved;b : byte);
    procedure restoreusedregisters(const saved : tsaved);

    { increments the push count of all registers in b}
    procedure incrementregisterpushed(b : byte);

    procedure clearregistercount;
    procedure resetusableregisters;

    { corrects the fpu stack register by ofs }
    function correct_fpuregister(r : tregister;ofs : byte) : tregister;

    type
{$ifdef SUPPORT_MMX}
       regvar_longintarray = array[R_EAX..R_MM6] of longint;
       regvar_booleanarray = array[R_EAX..R_MM6] of boolean;
       regvar_ptreearray = array[R_EAX..R_MM6] of tnode;
{$else SUPPORT_MMX}
       regvar_longintarray = array[R_EAX..R_EDI] of longint;
       regvar_booleanarray = array[R_EAX..R_EDI] of boolean;
       regvar_ptreearray = array[R_EAX..R_EDI] of tnode;
{$endif SUPPORT_MMX}

    var
       unused,usableregs : tregisterset;
       c_usableregs : longint;

       { uses only 1 byte while a set uses in FPC 32 bytes }
       usedinproc : byte;

       fpuvaroffset : byte;

       { count, how much a register must be pushed if it is used as register }
       { variable                                                           }
       reg_pushes : regvar_longintarray;
       is_reg_var : regvar_booleanarray;
       regvar_loaded: regvar_booleanarray;

{$ifdef TEMPREGDEBUG}
       reg_user   : regvar_ptreearray;
       reg_releaser : regvar_ptreearray;
{$endif TEMPREGDEBUG}


implementation

    uses
      globtype,temp_gen,tainst,regvars;

    procedure incrementregisterpushed(b : byte);

      var
         regi : tregister;

      begin
         for regi:=R_EAX to R_EDI do
           begin
              if (b and ($80 shr word(regi)))<>0 then
                inc(reg_pushes[regi],t_times*2);
           end;
      end;

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
                  if not is_reg_var[r] then
                    if not(r in unused) then
                     begin
                        { then save it }
                        exprasmlist.concat(Taicpu.Op_reg(A_PUSH,S_L,r));

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
                   exprasmList.concat(Taicpu.Op_const_reg(A_SUB,S_L,8,R_ESP));
                   new(hr);
                   reset_reference(hr^);
                   hr^.base:=R_ESP;
                   exprasmList.concat(Taicpu.Op_reg_ref(A_MOVQ,S_NO,r,hr));
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


    procedure saveregvars(b: byte);

      var
         r : tregister;

      begin
         if not(cs_regalloc in aktglobalswitches) then
           exit;
         for r:=R_EAX to R_EBX do
           { if the register is used by the calling subroutine    }
           if ((b and ($80 shr byte(r)))<>0) and is_reg_var[r] then
             store_regvar(exprasmlist,r)
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
                        exprasmList.concat(Taicpu.Op_reg_ref(A_MOV,S_L,r,newreference(hr)));
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
                   exprasmList.concat(Taicpu.Op_reg_ref(A_MOVQ,S_NO,r,newreference(hr)));
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
                   exprasmList.concat(Taicpu.Op_ref_reg(
                     A_MOVQ,S_NO,hr,r));
                   exprasmList.concat(Taicpu.Op_const_reg(
                     A_ADD,S_L,8,R_ESP));
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
                exprasmList.concat(Taicpu.Op_reg(A_POP,S_L,r));
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
                   exprasmList.concat(Taicpu.Op_ref_reg(
                     A_MOVQ,S_NO,newreference(hr),r));
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
                exprasmList.concat(Taicpu.Op_ref_reg(A_MOV,S_L,newreference(hr),r));
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
         if (r = R_EDI) or
            ((not assigned(procinfo^._class)) and (r = R_ESI)) then
           begin
             exprasmList.concat(Tairegalloc.DeAlloc(r));
             exit;
           end;
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
         exprasmList.concat(Tairegalloc.DeAlloc(r));
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

    function isaddressregister(reg: tregister): boolean;

      begin
        isaddressregister := true;
      end;

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
{$ifdef TEMPREGDEBUG}
         if curptree^^.usableregs-usablereg32>curptree^^.registers32 then
           internalerror(10);
{$endif TEMPREGDEBUG}
{$ifdef EXTTEMPREGDEBUG}
         if curptree^^.usableregs-usablereg32>curptree^^.reallyusedregs then
           curptree^^.reallyusedregs:=curptree^^.usableregs-usablereg32;
{$endif EXTTEMPREGDEBUG}
         dec(usablereg32);
         if R_EAX in unused then
           begin
              unused:=unused-[R_EAX];
              usedinproc:=usedinproc or ($80 shr byte(R_EAX));
              getregister32:=R_EAX;
{$ifdef TEMPREGDEBUG}
              reg_user[R_EAX]:=curptree^;
{$endif TEMPREGDEBUG}
              exprasmList.concat(Tairegalloc.Alloc(R_EAX));
           end
         else if R_EDX in unused then
           begin
              unused:=unused-[R_EDX];
              usedinproc:=usedinproc or ($80 shr byte(R_EDX));
              getregister32:=R_EDX;
{$ifdef TEMPREGDEBUG}
              reg_user[R_EDX]:=curptree^;
{$endif TEMPREGDEBUG}
              exprasmList.concat(Tairegalloc.Alloc(R_EDX));
           end
         else if R_EBX in unused then
           begin
              unused:=unused-[R_EBX];
              usedinproc:=usedinproc or ($80 shr byte(R_EBX));
              getregister32:=R_EBX;
{$ifdef TEMPREGDEBUG}
              reg_user[R_EBX]:=curptree^;
{$endif TEMPREGDEBUG}
              exprasmList.concat(Tairegalloc.Alloc(R_EBX));
           end
         else if R_ECX in unused then
           begin
              unused:=unused-[R_ECX];
              usedinproc:=usedinproc or ($80 shr byte(R_ECX));
              getregister32:=R_ECX;
{$ifdef TEMPREGDEBUG}
              reg_user[R_ECX]:=curptree^;
{$endif TEMPREGDEBUG}
              exprasmList.concat(Tairegalloc.Alloc(R_ECX));
           end
         else internalerror(10);
{$ifdef TEMPREGDEBUG}
         testregisters32;
{$endif TEMPREGDEBUG}
      end;


    function getaddressregister: tregister;

      begin
        getaddressregister := getregister32;
      end;

    function getexplicitregister32(r : tregister) : tregister;

      begin
         if r in [R_ESI,R_EDI] then
           begin
             exprasmList.concat(Tairegalloc.Alloc(r));
             getexplicitregister32 := r;
             exit;
           end;
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
              exprasmList.concat(Tairegalloc.Alloc(r));
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
        fillchar(regvar_loaded,sizeof(regvar_loaded),false);
        fillchar(is_reg_var,sizeof(is_reg_var),false);
        fpuvaroffset:=0;
      end;

begin
  resetusableregisters;
end.
{
  $Log$
  Revision 1.7  2001-12-29 15:29:59  jonas
    * powerpc/cgcpu.pas compiles :)
    * several powerpc-related fixes
    * cpuasm unit is now based on common tainst unit
    + nppcmat unit for powerpc (almost complete)

  Revision 1.5  2001/08/26 13:37:03  florian
    * some cg reorganisation
    * some PPC updates

  Revision 1.4  2001/04/13 01:22:21  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.3  2000/12/25 00:07:34  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.2  2000/12/05 11:44:34  jonas
    + new integer regvar handling, should be much more efficient

  Revision 1.1  2000/11/29 00:30:51  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.9  2000/10/31 22:30:13  peter
    * merged asm result patch part 2

  Revision 1.8  2000/10/14 10:14:56  peter
    * moehrendorf oct 2000 rewrite

  Revision 1.7  2000/09/30 16:08:46  peter
    * more cg11 updates

  Revision 1.6  2000/09/24 15:06:32  peter
    * use defines.inc

  Revision 1.5  2000/08/27 16:11:55  peter
    * moved some util functions from globals,cobjects to cutils
    * splitted files into finput,fmodule

  Revision 1.4  2000/08/05 13:32:39  peter
    * fixed build prob without support_mmx

  Revision 1.3  2000/08/04 05:09:49  jonas
    * forgot to commit :( (part of regvar changes)

  Revision 1.2  2000/07/13 11:32:52  michael
  + removed logs
}
