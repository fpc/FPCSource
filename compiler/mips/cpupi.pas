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
      register_offset : tparasupregsoffset;
      computed_local_size : longint;
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
        for i:=low(tparasupregs)  to high(tparasupregs) do
          begin
            register_used[i]:=false;
            register_offset[i]:=-1;
          end;
        floatregssave:=12; { f20-f31 }
        intregssave:=12;   { r16-r23,r28-r31 }
        { for testing }
        needs_frame_pointer := true;//false;
        computed_local_size:=-1;
      end;


    procedure TMIPSProcInfo.set_first_temp_offset;
      begin
        { We allocate enough space to save all registers because we can't determine
          the necessary space because the used registers aren't known before
          secondpass is run. }
        if tg.direction = -1 then
          tg.setfirsttemp(0)
        else
          tg.setfirsttemp(maxpushedparasize+
           floatregssave*sizeof(aint)+intregssave*sizeof(aint));
      end;


    function TMIPSProcInfo.calc_stackframe_size:longint;
      var
         r : byte;
         regs: tcpuregisterset;
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
          Comment(V_Warning,'TMIPSProcInfo.calc_stackframe_size result changed');
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
