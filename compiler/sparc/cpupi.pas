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
unit cpupi;

{$i fpcdefs.inc}

interface

  uses
    cutils,
    procinfo,cpuinfo,
    psub;

  type
    TSparcProcInfo=class(tcgprocinfo)
    public
      constructor create(aparent:tprocinfo);override;
      function calc_stackframe_size:longint;override;
    end;

implementation

    uses
      systems,globals,
      tgobj,paramgr,symconst;

    constructor tsparcprocinfo.create(aparent:tprocinfo);
      begin
        inherited create(aparent);
        maxpushedparasize:=0;
      end;


    function TSparcProcInfo.calc_stackframe_size:longint;
      begin
        {
          Stackframe layout:
          %fp
            <locals>
            <temp>
            <arguments 6-n for calling>
          %sp+92
            <space for arguments 0-5>                \
            <return pointer for calling>              | included in first_parm_offset
            <register window save area for calling>  /
          %sp

          Alignment must be the max available, as doubles require
          8 byte alignment
        }
        result:=Align(tg.direction*tg.lasttemp+maxpushedparasize+target_info.first_parm_offset,current_settings.alignment.localalignmax);
      end;


begin
  cprocinfo:=TSparcProcInfo;
end.
