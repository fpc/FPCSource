{
    $Id$
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
       psub;

    type
       ti386procinfo = class(tcgprocinfo)
         function calc_stackframe_size:longint;override;
       end;


  implementation

    uses
      cutils,
      globals,
      tgobj,
      procinfo;

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
  $Log$
  Revision 1.15  2003-10-03 22:00:33  peter
    * parameter alignment fixes

  Revision 1.14  2003/10/01 20:34:49  peter
    * procinfo unit contains tprocinfo
    * cginfo renamed to cgbase
    * moved cgmessage to verbose
    * fixed ppc and sparc compiles

  Revision 1.13  2003/09/25 21:30:11  peter
    * parameter fixes

  Revision 1.12  2003/09/23 17:56:06  peter
    * locals and paras are allocated in the code generation
    * tvarsym.localloc contains the location of para/local when
      generating code for the current procedure

  Revision 1.11  2003/09/09 21:03:17  peter
    * basics for x86 register calling

  Revision 1.10  2003/09/03 15:55:01  peter
    * NEWRA branch merged

  Revision 1.9.2.1  2003/08/31 15:46:26  peter
    * more updates for tregister

  Revision 1.9  2003/07/06 17:58:22  peter
    * framepointer fixes for sparc
    * parent framepointer code more generic

  Revision 1.8  2003/06/13 21:19:31  peter
    * current_procdef removed, use current_procinfo.procdef instead

  Revision 1.7  2003/06/12 18:12:49  jonas
    * fixed compilation problems

  Revision 1.6  2003/06/12 16:43:07  peter
    * newra compiles for sparc

  Revision 1.5  2003/05/25 10:26:15  peter
    * fix interrupt stack allocation

  Revision 1.4  2003/05/22 21:32:29  peter
    * removed some unit dependencies

  Revision 1.3  2003/04/27 11:21:35  peter
    * aktprocdef renamed to current_procinfo.procdef
    * procinfo renamed to current_procinfo
    * procinfo will now be stored in current_module so it can be
      cleaned up properly
    * gen_main_procsym changed to create_main_proc and release_main_proc
      to also generate a tprocinfo structure
    * fixed unit implicit initfinal

  Revision 1.2  2003/04/27 07:48:05  peter
    * updated for removed lexlevel

  Revision 1.1  2002/08/17 09:23:44  florian
    * first part of procinfo rewrite
}


