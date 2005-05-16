{
    $Id: cpupi.pas,v 1.18 2005/02/14 17:13:09 peter Exp $
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
       psub,procinfo;

    type
       ti386procinfo = class(tcgprocinfo)
         constructor create(aparent:tprocinfo);override;
         function calc_stackframe_size:longint;override;
       end;


  implementation

    uses
      cutils,
      globals,
      tgobj,
      cpubase;

    constructor ti386procinfo.create(aparent:tprocinfo);
      begin
        inherited create(aparent);
        got:=NR_EBX;
      end;


    function ti386procinfo.calc_stackframe_size:longint;
      begin
        { align to 4 bytes at least
          otherwise all those subl $2,%esp are meaningless PM }
        result:=Align(tg.direction*tg.lasttemp,min(aktalignment.localalignmin,4));
      end;



begin
   cprocinfo:=ti386procinfo;
end.
{
  $Log: cpupi.pas,v $
  Revision 1.18  2005/02/14 17:13:09  peter
    * truncate log

}


