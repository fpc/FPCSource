{
    Copyright (c) 2002-2009 by Florian Klaempfl and David Zhang

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
unit cpupi;

{$i fpcdefs.inc}

interface

  uses
    cutils,
    globtype,symdef,
    procinfo,cpuinfo,cpupara,
    psub;

  type

    { TMIPSProcInfo }

    TMIPSProcInfo=class(tcgprocinfo)
      intregstart,
      floatregstart : aint;
      intregssave,
      floatregssave : byte;
      needs_frame_pointer: boolean;
      register_used : tparasupregsused;
      register_size : tparasupregsize;
      register_name : tparasuprename;
      register_offset : tparasupregsoffset;
      computed_local_size : longint;
      //intparareg,
      //parasize : longint;
      constructor create(aparent:tprocinfo);override;
      function calc_stackframe_size:longint;override;
      procedure set_first_temp_offset;override;
    end;

   { Used by Stabs debug info generator }

   function mips_extra_offset(procdef : tprocdef) : longint;

implementation

    uses
      systems,globals,verbose,
      cpubase,cgbase,cgutils,cgobj,
      tgobj,paramgr,symconst;

    constructor TMIPSProcInfo.create(aparent: tprocinfo);
      var
        i : longint;
      begin
        inherited create(aparent);
        { will call _mcount if profiling }
        if cs_profile in current_settings.moduleswitches then
          include(flags,pi_do_call);
        for i:=low(tparasupregs)  to high(tparasupregs) do
          begin
            register_used[i]:=false;
            register_size[i]:=OS_NO;
            register_name[i]:='invalid';
            register_offset[i]:=-1;
          end;
        floatregssave:=12; { f20-f31 }
        intregssave:=12;   { r16-r23,r28-r31 }
        { for testing }
        needs_frame_pointer := true;//false;
        computed_local_size:=-1;
        { pi_needs_got is not yet set correctly
          so include it always if creating PIC code }
        if (cs_create_pic in current_settings.moduleswitches) then
          begin
            include(flags, pi_needs_got);
            got:=NR_GP;
          end
        else
          got:=NR_NO;
      end;


    procedure TMIPSProcInfo.set_first_temp_offset;
      begin
        { We allocate enough space to save all registers because we can't determine
          the necessary space because the used registers aren't known before
          secondpass is run. }
        if tg.direction = -1 then
          tg.setfirsttemp(0)
        else
          begin
            { Fixes the case when there are calls done by low-level means
              (cg.a_call_name) but no child callnode }
            if (pi_do_call in flags) then
              allocate_push_parasize(mips_nb_used_registers*sizeof(aint));

            if not (po_nostackframe in procdef.procoptions) then
              tg.setfirsttemp(Align(maxpushedparasize+
                floatregssave*sizeof(aint)+intregssave*sizeof(aint)
                ,max(current_settings.alignment.localalignmin,8)))
            else
              tg.setfirsttemp(align(maxpushedparasize,max(current_settings.alignment.localalignmin,8)));
          end;
      end;


    function TMIPSProcInfo.calc_stackframe_size:longint;
      begin
        result:=maxpushedparasize;
        floatregstart:=result;
        inc(result,floatregssave*4);
        intregstart:=result;
        //inc(result,intregssave*4);
        result:=Align(tg.lasttemp,max(current_settings.alignment.localalignmin,8));
        if computed_local_size=-1 then
          begin
            computed_local_size:=result;
            procdef.total_local_size:=result;
          end
        else if computed_local_size <> result then
          Comment(V_Error,'TMIPSProcInfo.calc_stackframe_size result changed');
      end;

    function mips_extra_offset(procdef : tprocdef) : longint;
      begin
        if procdef=nil then
          mips_extra_offset:=0
        else
          mips_extra_offset:=procdef.total_local_size;
      end;

begin
  cprocinfo:=TMIPSProcInfo;
end.
