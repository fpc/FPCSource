{
    Copyright (c) 2008 by Florian Klaempfl

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
       tavrprocinfo = class(tcgprocinfo)
          // procedure handle_body_start;override;
          // procedure after_pass1;override;
          procedure set_first_temp_offset;override;
          function calc_stackframe_size:longint;override;
          procedure postprocess_code;override;
       end;


  implementation

    uses
       globals,systems,
       cpubase,
       aasmtai,aasmdata,
       tgobj,
       symconst,symsym,paramgr,
       cgbase,
       cgobj,
       aasmcpu;

    procedure tavrprocinfo.set_first_temp_offset;
      begin
        if tg.direction = -1 then
          tg.setfirsttemp(-1)
        else if not (po_nostackframe in procdef.procoptions) then
          tg.setfirsttemp(maxpushedparasize+1)
        else
          tg.setfirsttemp(maxpushedparasize);
      end;


    function tavrprocinfo.calc_stackframe_size:longint;
      begin
        maxpushedparasize:=align(maxpushedparasize,max(current_settings.alignment.localalignmin,2));
        result:=Align(tg.direction*tg.lasttemp,max(current_settings.alignment.localalignmin,2))+maxpushedparasize;
      end;


    procedure tavrprocinfo.postprocess_code;
      begin
        { because of the limited branch distance of cond. branches, they must be replaced
          sometimes by normal jmps and an inverse branch }
        finalizeavrcode(aktproccode);
      end;

begin
   cprocinfo:=tavrprocinfo;
end.

