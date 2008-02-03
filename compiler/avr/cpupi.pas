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
          floatregstart : aint;
          // procedure handle_body_start;override;
          // procedure after_pass1;override;
          procedure set_first_temp_offset;override;
          function calc_stackframe_size:longint;override;
       end;


  implementation

    uses
       globals,systems,
       cpubase,
       aasmtai,aasmdata,
       tgobj,
       symconst,symsym,paramgr,
       cgbase,
       cgobj;

    procedure tavrprocinfo.set_first_temp_offset;
      begin
        { We allocate enough space to save all registers because we can't determine
          the necessary space because the used registers aren't known before
          secondpass is run. Even worse, patching
          the local offsets after generating the code could cause trouble because
          "shifter" constants could change to non-"shifter" constants. This
          is especially a problem when taking the address of a local. For now,
          this extra memory should hurt less than generating all local contants with offsets
          >256 as non shifter constants }
        if tg.direction = -1 then
          tg.setfirsttemp(-12-28)
        else
          tg.setfirsttemp(maxpushedparasize);
      end;


    function tavrprocinfo.calc_stackframe_size:longint;
      var
         firstfloatreg,lastfloatreg,
         r : byte;
         floatsavesize : aword;
      begin
        maxpushedparasize:=align(maxpushedparasize,max(current_settings.alignment.localalignmin,4));
      end;


begin
   cprocinfo:=tavrprocinfo;
end.
