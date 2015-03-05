{
    Copyright (c) 2007-2008, 2013 by Jonas Maebe

    Contains the base types for LLVM

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
{ This Unit contains the base types for LLVM
}
unit llvmbase;

{$i fpcdefs.inc}

interface

  uses
    strings,globtype,
    cutils,cclasses,aasmbase,cpubase,cpuinfo,cgbase;


{*****************************************************************************
                                Assembler Opcodes
*****************************************************************************}

  type
    tllvmop = (la_none,
      { terminator instructions }
      la_ret, la_br, la_switch, la_indirectbr,
      la_invoke, la_resume,
      la_unreachable,
      { binary operations }
      la_add, la_fadd, la_sub, la_fsub, la_mul, la_fmul,
      la_udiv,la_sdiv, la_fdiv, la_urem, la_srem, la_frem,
      { bitwise binary operations }
      la_shl, la_lshr, la_ashr, la_and, la_or, la_xor,
      { vector operations }
      la_extractelement, la_insertelement, la_shufflevector,
      { aggregate operations }
      la_extractvalue, la_insertvalue,
      { memory access and memory addressing operations }
      la_alloca,
      la_load, la_store,
      la_fence, la_cmpxchg, la_atomicrmw,
      la_getelementptr,
      { conversion operations }
      la_trunc, la_zext, la_sext, la_fptrunc, la_fpext,
      la_fptoui, la_fptosi, la_uitofp, la_sitofp,
      la_ptrtoint, la_inttoptr,
      la_bitcast,
      { other operations }
      la_icmp, la_fcmp,
      la_phi, la_select, la_call,
      la_va_arg, la_landingpad,
      { fpc pseudo opcodes }
      la_type, { type definition }
      la_x_to_inttoptr, { have to convert something first to int before it can be converted to a pointer }
      la_ptrtoint_to_x { have to convert a pointer first to int before it can be converted to something else }
    );

    tllvmvalueextension = (lve_none, lve_zeroext, lve_signext);

  const
    llvmterminatoropcodes = [la_ret, la_br, la_switch, la_indirectbr,
      la_invoke, la_resume,
      la_unreachable];

    llvmvalueextension2str: array[tllvmvalueextension] of TSymStr = ('',
      ' zeroext',' signext');


  type
    tllvmfpcmp = (
      lfc_false,
      lfc_oeq, lfc_ogt, lfc_oge, lfc_olt, lfc_ole, lfc_one, lfc_ord,
      lfc_ueq, lfc_ugt, lfc_uge, lfc_ult, lfc_ule, lfc_une, lfc_uno,
      lfc_true);

    {# This should define the array of instructions as string }
    llvmop2strtable=array[tllvmop] of string[14];

  const
    { = max(cpubase.max_operands,7) }
    max_operands = ((-ord(cpubase.max_operands<=7)) and 7) or ((-ord(cpubase.max_operands>7)) and cpubase.max_operands);

  function llvm_target_name: ansistring;

implementation

  uses
    globals,
    systems;

{$j-}
  const
    llvmsystemcpu: array[tsystemcpu] of ansistring =
      ('unknown',
       'i386',
       'm68k',
       'alpha',
       'powerpc',
       'sparc',
       'unknown',
       'ia64',
       'x86_64',
       'mips',
       'arm',
       'powerpc64',
       'avr',
       'mipsel',
       'unknown',
       'unknown',
       'aarch64'
      );

  function llvm_target_name: ansistring;
    begin
      { architecture }
{$ifdef arm}
      llvm_target_name:=lower(cputypestr[current_settings.cputype]);
{$else arm}
      llvm_target_name:=llvmsystemcpu[target_info.cpu];
{$endif}
      { vendor and/or OS }
      if target_info.system in systems_darwin then
        begin
          llvm_target_name:=llvm_target_name+'-apple';
          if not(target_info.system in [system_arm_darwin,system_i386_iphonesim]) then
            llvm_target_name:=llvm_target_name+'-macosx'+MacOSXVersionMin
          else
            llvm_target_name:=llvm_target_name+'-ios'+iPhoneOSVersionMin;
        end
      else if target_info.system in (systems_linux+systems_android) then
        llvm_target_name:=llvm_target_name+'-linux'
      else if target_info.system in systems_windows then
        begin
          { WinCE isn't supported (yet) by llvm, but if/when added this is
            presumably how they will differentiate it }
          if not(target_info.system in [system_i386_wince,system_arm_wince]) then
            llvm_target_name:=llvm_target_name+'-pc';
          llvm_target_name:=llvm_target_name+'-win32'
        end
      else if target_info.system in systems_freebsd then
        llvm_target_name:=llvm_target_name+'-freebsd'
      else if target_info.system in systems_openbsd then
        llvm_target_name:=llvm_target_name+'-openbsd'
      else if target_info.system in systems_netbsd then
        llvm_target_name:=llvm_target_name+'-netbsd'
      else if target_info.system in systems_aix then
        llvm_target_name:=llvm_target_name+'-ibm-aix'
      else if target_info.system in [system_i386_haiku] then
        llvm_target_name:=llvm_target_name+'-haiku'
      else if target_info.system in systems_embedded then
        llvm_target_name:=llvm_target_name+'-none'
      else
        llvm_target_name:=llvm_target_name+'-unknown';

      { environment/ABI }
      if target_info.system in systems_android then
        llvm_target_name:=llvm_target_name+'-android';
{$if defined(FPC_ARMHF)}
      llvm_target_name:=llvm_target_name+'-gnueabihf';
{$elseif defined(FPC_ARMEL)}
      if target_info.system in systems_embedded then
        llvm_target_name:=llvm_target_name+'-eabi'
      else if target_info.system=system_arm_android then
        { handled above already
        llvm_target_name:=llvm_target_name+'-android' }
      else
        llvm_target_name:=llvm_target_name+'-gnueabi';
{$endif FPC_ARM_HF}
    end;

end.
