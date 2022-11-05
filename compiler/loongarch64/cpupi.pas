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

{$I fpcdefs.inc}

  interface

    uses
      cutils,aasmdata,
      globtype, cgutils, cgbase,
      procinfo, cpuinfo, psub;

    type
      tloongarch64procinfo = class(tcgprocinfo)
        stackframesize,
        floatregstart : aint;
        stackpaddingreg: TSuperRegister;

        needs_frame_pointer: boolean;

        constructor create(aparent: tprocinfo); override;
        procedure set_first_temp_offset; override;
        function calc_stackframe_size: longint; override;
      end;

implementation

    uses
      globals, systems,
      cpubase,
      aasmtai,
      tgobj,cgobj,
      symconst, symsym, paramgr, symutil, symtable,
      verbose,
      aasmcpu;


    constructor tloongarch64procinfo.create(aparent: tprocinfo);
      begin
        inherited create(aparent);
        maxpushedparasize := 0;
        { GCC option -fomit-frame-pointer is default. }
        framepointer:=NR_STACK_POINTER_REG;
      end;


    procedure tloongarch64procinfo.set_first_temp_offset;
      begin
        { TODO framepointer:=NR_STACK_POINTER_REG; Need get backtrace updated. }
        { TODO Profile }

        if (po_nostackframe in procdef.procoptions) then
          begin
            tg.setfirsttemp(align(maxpushedparasize,
              max(current_settings.alignment.localalignmin,8)));
          end
        else
          begin
            tg.setfirsttemp(align(maxpushedparasize,
              max(current_settings.alignment.localalignmin,8)));
          end;
      end;


    function tloongarch64procinfo.calc_stackframe_size: longint;
      var
         firstfloatreg,lastfloatreg,
         r : byte;
         floatsavesize : aword;
         regs: tcpuregisterset;
      begin
        maxpushedparasize:=align(maxpushedparasize,
                             max(current_settings.alignment.localalignmin,8));
        floatsavesize:=0;
        case current_settings.fputype of
          fpu_fd:
            begin
              floatsavesize:=0;
              regs:=cg.rg[R_FPUREGISTER].used_in_proc-paramanager.get_volatile_registers_fpu(pocall_stdcall);
              for r:=RS_F0 to RS_F31 do
                if r in regs then
                  inc(floatsavesize,8);
            end;
          else
            ;
        end;
        result:=align(tg.direction*tg.lasttemp,
                  max(current_settings.alignment.localalignmin,8))
                  +maxpushedparasize+aint(floatsavesize);
        if tg.direction=1 then
          floatregstart:=result-aint(floatsavesize)
        else
          floatregstart:=-result+maxpushedparasize;
      end;


begin
  cprocinfo := tloongarch64procinfo;
end.

