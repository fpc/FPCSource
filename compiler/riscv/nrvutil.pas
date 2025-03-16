{
    Copyright (c) 2024

    RISCV version of some node tree helper routines

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
unit nrvutil;

{$i fpcdefs.inc}

interface

  uses
    ngenutil;


  type
    trvnodeutils = class(tnodeutils)
      class procedure InsertObjectInfo; override;
    end;

implementation

  uses
    globtype,globals,
    systems,
    cpuinfo,
    aasmdata,aasmtai;

  const
    tag_stack_align = 4;
    tag_arch = 5;
    tag_unaligned_access = 6;
    tag_priv_spec = 8;
    tag_priv_spec_minor = 10;
    tag_priv_spec_revision = 12;

  class procedure trvnodeutils.InsertObjectInfo;
    var
     attr_arch: String;
    begin
      inherited InsertObjectInfo;
      if (target_info.system in systems_linux) then
        begin
          if (cs_create_pic in current_settings.moduleswitches) then
            current_asmdata.asmlists[al_start].Concat(tai_directive.create(asd_option,'pic'))
          else
            current_asmdata.asmlists[al_start].Concat(tai_directive.create(asd_option,'nopic'));

          current_asmdata.asmlists[al_start].Concat(tai_attribute.create(ait_attribute,tag_stack_align,target_info.stackalign));
          current_asmdata.asmlists[al_start].Concat(tai_attribute.create(ait_attribute,tag_unaligned_access,0));
{$if defined(RISCV32)}
          if CPURV_HAS_16REGISTERS in cpu_capabilities[current_settings.cputype] then
            attr_arch:='rv32e2p0'
          else
            attr_arch:='rv32i2p1';
{$elseif defined(RISCV64)}
          if CPURV_HAS_16REGISTERS in cpu_capabilities[current_settings.cputype] then
            attr_arch:='rv64e2p0'
          else
            attr_arch:='rv64i2p1';
{$elseif defined(RISCV128)}
          attr_arch:='rv128i2p1';
{$endif defined(RISCV32)}
          if CPURV_HAS_MUL in cpu_capabilities[current_settings.cputype] then
            attr_arch:=attr_arch+'_m2p0';
          if CPURV_HAS_ATOMIC in cpu_capabilities[current_settings.cputype] then
            attr_arch:=attr_arch+'_a2p1';
          if CPURV_HAS_F in cpu_capabilities[current_settings.cputype] then
            attr_arch:=attr_arch+'_f2p2';
          if CPURV_HAS_D in cpu_capabilities[current_settings.cputype] then
            attr_arch:=attr_arch+'_d2p2';
          if CPURV_HAS_COMPACT in cpu_capabilities[current_settings.cputype] then
            attr_arch:=attr_arch+'_c2p0';
          if CPURV_HAS_ZICOND in cpu_capabilities[current_settings.cputype] then
            attr_arch:=attr_arch+'_zicond1p0';
          if CPURV_HAS_CSR_INSTRUCTIONS in cpu_capabilities[current_settings.cputype] then
            attr_arch:=attr_arch+'_zicsr2p0';
          if CPURV_HAS_FETCH_FENCE in cpu_capabilities[current_settings.cputype] then
            attr_arch:=attr_arch+'_zifencei2p0';
          if CPURV_HAS_ZMMUL in cpu_capabilities[current_settings.cputype] then
            attr_arch:=attr_arch+'_zmmul1p0';
          if CPURV_HAS_ZFA in cpu_capabilities[current_settings.cputype] then
            attr_arch:=attr_arch+'_zfa1p0';
          if CPURV_HAS_ZBA in cpu_capabilities[current_settings.cputype] then
            attr_arch:=attr_arch+'_zba1p0';
          if CPURV_HAS_ZBB in cpu_capabilities[current_settings.cputype] then
            attr_arch:=attr_arch+'_zbb1p0';
          if CPURV_HAS_ZBC in cpu_capabilities[current_settings.cputype] then
            attr_arch:=attr_arch+'_zbc1p0';
          if CPURV_HAS_ZBS in cpu_capabilities[current_settings.cputype] then
            attr_arch:=attr_arch+'_zbs1p0';
          current_asmdata.asmlists[al_start].Concat(tai_attribute.create(ait_attribute,tag_arch,attr_arch));
        end;
    end;


begin
  cnodeutils:=trvnodeutils;
end.

