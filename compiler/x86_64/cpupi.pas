{
    $Id: cpupi.pas,v 1.6 2005/02/14 17:13:10 peter Exp $
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
       tx86_64procinfo = class(tcgprocinfo)
         function calc_stackframe_size:longint;override;
         procedure allocate_push_parasize(size:longint);override;
       end;


implementation

    uses
      globals,
      cutils,
      tgobj;


    procedure tx86_64procinfo.allocate_push_parasize(size:longint);
      begin
        if size>maxpushedparasize then
          maxpushedparasize:=size;
      end;


    function tx86_64procinfo.calc_stackframe_size:longint;
      begin
        maxpushedparasize:=align(maxpushedparasize,max(aktalignment.localalignmin,16));
        { RSP should be aligned on 16 bytes }
        result:=Align(tg.direction*tg.lasttemp,16)+maxpushedparasize;
      end;

begin
   cprocinfo:=tx86_64procinfo;
end.
{
  $Log: cpupi.pas,v $
  Revision 1.6  2005/02/14 17:13:10  peter
    * truncate log

}


