{
    Copyright (c) 2015 by Jonas Maebe, member of the Free Pascal
    Development Team

    This unit implements the JVM optimizer object

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


Unit aoptcpu;

{$i fpcdefs.inc}

Interface

uses cpubase, aasmtai, aopt, aoptcpub;

Type
  TCpuAsmOptimizer = class(TAsmOptimizer)
   protected
    function RemoveDoubleSwap(var p: tai): boolean;
    function RemoveCommutativeSwap(var p: tai): boolean;
    function RemoveLoadLoadSwap(var p: tai): boolean;
   public
    { uses the same constructor as TAopObj }
    function PeepHoleOptPass1Cpu(var p: tai): boolean; override;
    procedure PeepHoleOptPass2;override;
    function PostPeepHoleOptsCpu(var p: tai): boolean; override;
  End;

Implementation

  uses
    aasmbase,aasmcpu,cgbase;


  function TCpuAsmOptimizer.RemoveDoubleSwap(var p: tai): boolean;
    var
      next, next2: tai;
    begin
      result:=false;
      { remove two successive "swap" instructions }
      if (taicpu(p).opcode=a_swap) and
         GetNextInstruction(p,next) and
         (next.typ=ait_instruction) and
         (taicpu(next).opcode=a_swap) then
        begin
          { can't be the instruction, must end in a return or so }
          next2:=tai(next.next);
          asml.remove(p);
          asml.remove(next);
          p.free;
          next.free;
          p:=next2;
          result:=true;
        end;
    end;


  { returns whether p is an instruction that does not consume any stack slots,
    and adds a new item on the stack that is one stack slot wide }
  function OpCreatesSingleStackSlot(p: tai): boolean;
    begin
      result:=
        (p.typ=ait_instruction) and
        (taicpu(p).opcode in
          [a_aload, a_aload_0, a_aload_1, a_aload_2, a_aload_3,
           a_bipush,
           a_fconst_0, a_fconst_1, a_fconst_2,
           a_fload, a_fload_0, a_fload_1, a_fload_2, a_fload_3,
           a_getstatic,
           a_iconst_m1, a_iconst_0, a_iconst_1, a_iconst_2, a_iconst_3,
           a_iconst_4, a_iconst_5,
           a_iload, a_iload_0, a_iload_1, a_iload_2, a_iload_3,
           a_new,
           a_sipush]);
    end;


  function OpIsCommutativeSingleSlots(p: tai): boolean;
    begin
      result:=
        (p.typ=ait_instruction) and
        (taicpu(p).opcode in
          [a_fadd, a_fmul,
           a_iadd, a_iand, a_imul, a_ior, a_ixor,
           a_pop2])
    end;


  function TCpuAsmOptimizer.RemoveCommutativeSwap(var p: tai): boolean;
    var
      next: tai;
    begin
      result:=false;
      if (taicpu(p).opcode<>a_swap) then
        exit;
      { if the next opcode is commutative operation, we can remove the swap }
      if GetNextInstruction(p,next) and
         OpIsCommutativeSingleSlots(next) then
        begin
          asml.remove(p);
          p.free;
          p:=next;
          result:=true;
          exit;
        end;
    end;


  function TCpuAsmOptimizer.PeepHoleOptPass1Cpu(var p: tai): boolean;
    var
      next, next2: tai;
    begin
      result:=false;
      case p.typ of
        ait_instruction:
          begin
            if RemoveDoubleSwap(p) or
               RemoveCommutativeSwap(p) then
              exit(true)
          end;
      end;
    end;


  procedure TCpuAsmOptimizer.PeepHoleOptPass2;
    begin
    end;


  function TCpuAsmOptimizer.RemoveLoadLoadSwap(var p: tai): boolean;
    var
      next, prev1, prev2: tai;
    begin
      result:=false;
      if (taicpu(p).opcode<>a_swap) then
        exit;
      { if we can swap the previous two instructions that put the items on the
        stack, we can remove the swap -- only do this in PostPeepholeOpts,
        because this may make the temp alloc information invalid. Ideally, we
        should move the tempallocs around too }
      if GetLastInstruction(p,prev1) and
         OpCreatesSingleStackSlot(prev1) and
         GetLastInstruction(prev1,prev2) and
         OpCreatesSingleStackSlot(prev2) then
        begin
          next:=tai(p.next);
          asml.remove(prev2);
          asml.InsertAfter(prev2,prev1);
          asml.remove(p);
          p.free;
          p:=next;
          result:=true;
        end;
    end;


  function TCpuAsmOptimizer.PostPeepHoleOptsCpu(var p: tai): boolean;
    begin
      result:=
        RemoveLoadLoadSwap(p);
    end;

begin
  casmoptimizer:=TCpuAsmOptimizer;
End.

