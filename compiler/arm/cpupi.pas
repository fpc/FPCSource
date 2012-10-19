{
    Copyright (c) 2002 by Florian Klaempfl

    This unit contains the CPU specific part of tprocinfo

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

{ This unit contains the CPU specific part of tprocinfo. }
unit cpupi;

{$i fpcdefs.inc}

  interface

    uses
       globtype,cutils,
       procinfo,cpuinfo,psub;

    type
       tarmprocinfo = class(tcgprocinfo)
          floatregstart : aint;
          // procedure handle_body_start;override;
          // procedure after_pass1;override;
          procedure set_first_temp_offset;override;
          function calc_stackframe_size:longint;override;
          procedure init_framepointer; override;
       end;


  implementation

    uses
       globals,systems,
       cpubase,
       aasmtai,aasmdata,
       tgobj,
       symconst,symsym,paramgr,
       cgbase,cgutils,
       cgobj;

    procedure tarmprocinfo.set_first_temp_offset;
      begin
        { We allocate enough space to save all registers because we can't determine
          the necessary space because the used registers aren't known before
          secondpass is run. Even worse, patching
          the local offsets after generating the code could cause trouble because
          "shifter" constants could change to non-"shifter" constants. This
          is especially a problem when taking the address of a local. For now,
          this extra memory should hurt less than generating all local contants with offsets
          >256 as non shifter constants }
        if (po_nostackframe in procdef.procoptions) then
          begin
             { maxpushedparasize sghould be zero,
               if not we will get an error later. }
             tg.setfirsttemp(maxpushedparasize);
             exit;
          end;
        if tg.direction = -1 then
          begin
            if (target_info.system<>system_arm_darwin) then
              { Non-Darwin, worst case: r4-r10,r11,r13,r14,r15 is saved -> -28-16, but we
                always adjust the frame pointer to point to the first stored
                register (= last register in list above) -> + 4 }
              tg.setfirsttemp(-28-16)
            else
              { on Darwin first r4-r7,r14 are saved, then r7 is adjusted to
                point to the saved r7, and next r8,r10,r11 gets saved -> -24
                (r4-r6 and r8,r10,r11) }
              tg.setfirsttemp(-24)
          end
        else
          tg.setfirsttemp(maxpushedparasize);
      end;


    function tarmprocinfo.calc_stackframe_size:longint;
      var
         firstfloatreg,lastfloatreg,
         r : byte;
         floatsavesize : aword;
         regs: tcpuregisterset;
      begin
        maxpushedparasize:=align(maxpushedparasize,max(current_settings.alignment.localalignmin,4));
        floatsavesize:=0;
        case current_settings.fputype of
          fpu_fpa,
          fpu_fpa10,
          fpu_fpa11:
            begin
              { save floating point registers? }
              firstfloatreg:=RS_NO;
              regs:=cg.rg[R_FPUREGISTER].used_in_proc-paramanager.get_volatile_registers_fpu(pocall_stdcall);
              for r:=RS_F0 to RS_F7 do
                if r in regs then
                  begin
                    if firstfloatreg=RS_NO then
                      firstfloatreg:=r;
                    lastfloatreg:=r;
                  end;
              if firstfloatreg<>RS_NO then
                floatsavesize:=(lastfloatreg-firstfloatreg+1)*12;
            end;
          fpu_vfpv2,
          fpu_vfpv3,
          fpu_vfpv3_d16:
            begin
              floatsavesize:=0;
              regs:=cg.rg[R_MMREGISTER].used_in_proc-paramanager.get_volatile_registers_mm(pocall_stdcall);
              for r:=RS_D0 to RS_D31 do
                if r in regs then
                  inc(floatsavesize,8);
            end;
        end;
        floatsavesize:=align(floatsavesize,max(current_settings.alignment.localalignmin,4));
        result:=Align(tg.direction*tg.lasttemp,max(current_settings.alignment.localalignmin,4))+maxpushedparasize+aint(floatsavesize);
        floatregstart:=tg.direction*result+maxpushedparasize;
        if tg.direction=1 then
          dec(floatregstart,floatsavesize);
      end;


    procedure tarmprocinfo.init_framepointer;
      begin
        if not(target_info.system in systems_darwin) then
          begin
            RS_FRAME_POINTER_REG:=RS_R11;
            NR_FRAME_POINTER_REG:=NR_R11;
          end
        else
          begin
            RS_FRAME_POINTER_REG:=RS_R7;
            NR_FRAME_POINTER_REG:=NR_R7;
          end;
      end;


begin
   cprocinfo:=tarmprocinfo;
end.
