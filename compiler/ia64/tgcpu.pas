{
    $Id$
    Copyright (C) 2000 by Florian Klaempfl

    This unit handles the temporary variables stuff for iA64
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

{$i fpcdefs.inc}

interface

    uses
       cobjects,globals,
       hcodegen,verbose,aasm,
       node,
       cpuinfo,cpubase,cpuasm;

    const
       { this is iA64 specific }
       countusableregqp : byte = c_countusableregsqp;

       { this value is used in tsaved, if the register isn't saved }
       reg_not_saved = $7fffffff;

    type
       tpushed = array[R_NO..R_NO] of boolean;
       tsaved = array[R_NO..R_NO] of longint;

    var
       { tries to hold the amount of times which the current tree is processed  }
       t_times : longint;

    function getregisterint : tregister;
    procedure ungetregisterint(r : tregister);
    { tries to allocate the passed register, if possible }
    function getexplicitregisterint(r : tregister) : tregister;

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

    { increments the push count of all registers in b}
    procedure incrementregisterpushed(b : byte);

    procedure clearregistercount;
    procedure resetusableregisters;

    type
       regvar_longintarray = array[0..128+128+64-1] of longint;
       regvar_booleanarray = array[0..128+128+64-1] of boolean;
       regvar_ptreearray = array[0..128+128+64-1] of tnode;

    var
       unused,usableregs : tregisterset;

       { uses only 1 byte while a set uses in FPC 32 bytes }
       usedinproc : byte;

       { count, how much a register must be pushed if it is used as register }
       { variable                                                           }
       reg_pushes : regvar_longintarray;
       is_reg_var : regvar_booleanarray;


implementation

    uses
      globtype,temp_gen;

    procedure incrementregisterpushed(b : byte);

      var
         regi : tregister;

      begin
{!!!!!!!!
         for regi:=R_EAX to R_EDI do
           begin
              if (b and ($80 shr word(regi)))<>0 then
                inc(reg_pushes[regi],t_times*2);
           end;
}
      end;

    procedure pushusedregisters(var pushed : tpushed;b : byte);

      var
         r : tregister;

      begin
{!!!!!!!!
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
                          end;
                        pushed[r]:=true;
                     end;
                end;
           end;
}
      end;

    procedure saveusedregisters(var saved : tsaved;b : byte);

      var
         r : tregister;
         hr : treference;

      begin
{!!!!!!!
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
                          end;
                     end;
                end;
           end;
}
      end;

    procedure popusedregisters(const pushed : tpushed);

      var
         r : tregister;

      begin
{!!!!!!!
         { restore in reverse order: }
         for r:=R_EBX downto R_EAX do
           if pushed[r] then
             begin
                exprasmlist^.concat(new(paicpu,op_reg(A_POP,S_L,r)));
                unused:=unused-[r];
             end;
}
      end;

    procedure restoreusedregisters(const saved : tsaved);
      var
         r : tregister;
         hr : treference;

      begin
{
         { restore in reverse order: }
         for r:=R_EBX downto R_EAX do
           if saved[r]<>reg_not_saved then
             begin
                reset_reference(hr);
                hr.base:=frame_pointer_reg;
                hr.offset:=saved[r];
                exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOV,S_L,newreference(hr),r)));
                unused:=unused-[r];
                ungetiftemp(hr);
             end;
}
      end;

    procedure ungetregister(r : tregister);

      begin
{
         if r in [R_EAX,R_ECX,R_EDX,R_EBX,R_ESP,R_EBP,R_ESI,R_EDI] then
           ungetregister32(r)
          else if r in [R_AX,R_CX,R_DX,R_BX,R_SP,R_BP,R_SI,R_DI] then
           ungetregister32(reg16toreg32(r))
         else if r in [R_AL,R_BL,R_CL,R_DL] then
           ungetregister32(reg8toreg32(r))
         else internalerror(18);
}
      end;

    procedure ungetregisterint(r : tregister);

      begin
{
         if (r = R_EDI) or
            ((not assigned(procinfo^._class)) and (r = R_ESI)) then
           begin
             exprasmlist^.concat(new(pairegalloc,dealloc(r)));
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
                  inc(usablereg32);
              unused:=unused+[r];
           end;
         exprasmlist^.concat(new(pairegalloc,dealloc(r)));
}
      end;


    procedure del_reference(const ref : treference);

      begin
         if ref.is_immediate then
           exit;
         ungetregisterint(ref.base);
      end;


    procedure del_locref(const location : tlocation);
      begin
         if (location.loc<>LOC_MEM) and (location.loc<>LOC_REFERENCE) then
           exit;
         if location.reference.is_immediate then
           exit;
         ungetregisterint(location.reference.base);
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


    function getregisterint : tregister;
      begin
{
         if usableregint=0 then
           internalerror(10);
         dec(usableregint);
         if R_EAX in unused then
           begin
              unused:=unused-[R_EAX];
              usedinproc:=usedinproc or ($80 shr byte(R_EAX));
              getregister32:=R_EAX;
              exprasmlist^.concat(new(pairegalloc,alloc(R_EAX)));
           end
         else if R_EDX in unused then
           begin
              unused:=unused-[R_EDX];
              usedinproc:=usedinproc or ($80 shr byte(R_EDX));
              getregister32:=R_EDX;
              exprasmlist^.concat(new(pairegalloc,alloc(R_EDX)));
           end
         else if R_EBX in unused then
           begin
              unused:=unused-[R_EBX];
              usedinproc:=usedinproc or ($80 shr byte(R_EBX));
              getregister32:=R_EBX;
              exprasmlist^.concat(new(pairegalloc,alloc(R_EBX)));
           end
         else if R_ECX in unused then
           begin
              unused:=unused-[R_ECX];
              usedinproc:=usedinproc or ($80 shr byte(R_ECX));
              getregister32:=R_ECX;
              exprasmlist^.concat(new(pairegalloc,alloc(R_ECX)));
           end
         else internalerror(10);
}
      end;

    function getexplicitregisterint(r : tregister) : tregister;

      begin
{
         if r in [R_ESI,R_EDI] then
           begin
             exprasmlist^.concat(new(pairegalloc,alloc(r)));
             getexplicitregister32 := r;
             exit;
           end;
         if r in unused then
           begin
              dec(usablereg32);
              unused:=unused-[r];
              usedinproc:=usedinproc or ($80 shr byte(r));
              exprasmlist^.concat(new(pairegalloc,alloc(r)));
              getexplicitregister32:=r;
           end
         else
           getexplicitregister32:=getregister32;
}
      end;

    procedure cleartempgen;

      begin
         unused:=usableregs;
         countusableregint:=c_countusableregsint;
         countusableregfpu:=c_countusableregsfpu;
         countusableregqp:=c_countusableregsqp;
      end;


   procedure clearregistercount;
      var
        regi : tregister;
      begin
{
         for regi:=R_EAX to R_EDI do
           begin
              reg_pushes[regi]:=0;
              is_reg_var[regi]:=false;
           end;
}
      end;

   procedure resetusableregisters;
      begin
{
        usableregs:=[R_EAX,R_EBX,R_ECX,R_EDX];
        c_usableregs:=4;
}
      end;

begin
  resetusableregisters;
end.
{
  $Log$
  Revision 1.3  2002-05-16 19:46:52  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.2  2002/04/20 21:38:45  carl
  * renamed some constants

  Revision 1.1  2000/12/31 16:54:19  florian
    + initial revision

}
