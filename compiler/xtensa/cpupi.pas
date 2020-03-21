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
       cutils,globtype,
       cpubase,
       cgbase,aasmdata,
       procinfo,cpuinfo,psub;

    type
      txtensaprocinfo = class(tcgprocinfo)
          callins,callxins : TAsmOp;
          stackframesize,
          floatregstart : aint;
          stackpaddingreg: TSuperRegister;

          needs_frame_pointer: boolean;
          // procedure handle_body_start;override;
          // procedure after_pass1;override;            
          constructor create(aparent: tprocinfo); override;
          procedure set_first_temp_offset;override;
          function calc_stackframe_size:longint;override;
      end;


  implementation

    uses
       globals,systems,
       tgobj,
       symconst,symtype,symsym,symcpu,paramgr,
       cgutils,
       cgobj,
       defutil,
       aasmcpu;     

    constructor txtensaprocinfo.create(aparent: tprocinfo);
      begin
        inherited create(aparent);
        maxpushedparasize := 0;
        framepointer:=NR_FRAME_POINTER_REG;
        if target_info.abi=abi_xtensa_windowed then
          begin
            callins:=A_CALL8;
            callxins:=A_CALLX8;
          end
        else
          begin
            callins:=A_CALL0;
            callxins:=A_CALLX0;
          end
      end;


    procedure txtensaprocinfo.set_first_temp_offset;
      begin
        if (po_nostackframe in procdef.procoptions) then
          begin
             { maxpushedparasize sghould be zero,
               if not we will get an error later. }
             tg.setfirsttemp(maxpushedparasize);
             exit;
          end;

        if tg.direction = -1 then
          tg.setfirsttemp(-(1+12)*4)
        else
          tg.setfirsttemp(maxpushedparasize);
      end;


    function txtensaprocinfo.calc_stackframe_size:longint;
      var
         firstfloatreg,lastfloatreg,
         r : byte;
         floatsavesize : aword;
         regs: tcpuregisterset;
      begin
        maxpushedparasize:=align(maxpushedparasize,max(current_settings.alignment.localalignmin,4));
        floatsavesize:=0;
        //case current_settings.fputype of
        //  fpu_fd:
        //    begin
        //      floatsavesize:=0;
        //      regs:=cg.rg[R_FPUREGISTER].used_in_proc-paramanager.get_volatile_registers_fpu(pocall_stdcall);
        //      for r:=RS_F0 to RS_F31 do
        //        if r in regs then
        //          inc(floatsavesize,8);
        //    end;
        //  else
        //    ;
        //end;
        floatsavesize:=align(floatsavesize,max(current_settings.alignment.localalignmin,4));
        result:=Align(tg.direction*tg.lasttemp,max(current_settings.alignment.localalignmin,4))+maxpushedparasize+aint(floatsavesize);

        if tg.direction=1 then
          floatregstart:=result-aint(floatsavesize)
        else
          floatregstart:=-result+maxpushedparasize;
      end;


begin
   cprocinfo:=txtensaprocinfo;
end.

