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
    globtype,
    procinfo,cpuinfo,
    psub;

  type

    { TMIPSProcInfo }

    TMIPSProcInfo=class(tcgprocinfo)
      intregstart,
      floatregstart : aint;
      intregssave,
      floatregssave : byte;
      constructor create(aparent:tprocinfo);override;
      function calc_stackframe_size:longint;override;
      procedure set_first_temp_offset;override;
    end;

implementation

    uses
      systems,globals,
      cpubase,cgbase,cgobj,
      tgobj,paramgr,symconst;

    constructor TMIPSProcInfo.create(aparent: tprocinfo);
      begin
        inherited create(aparent);
        floatregssave:=11;
        intregssave:=10;
      end;


    procedure TMIPSProcInfo.set_first_temp_offset;
      begin
        { We allocate enough space to save all registers because we can't determine
          the necessary space because the used registers aren't known before
          secondpass is run. }
        if tg.direction = -1 then
          tg.setfirsttemp(0)
        else
          tg.setfirsttemp(maxpushedparasize+floatregssave*sizeof(aint)+intregssave*sizeof(aint));
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
        result:=Align(tg.lasttemp,max(current_settings.alignment.localalignmin,8));
      end;


begin
  cprocinfo:=TMIPSProcInfo;
end.
