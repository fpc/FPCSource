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
          procedure allocate_interrupt_parameter;override;
          procedure allocate_framepointer;override;
       end;


  implementation

    uses
      cgbase, cpubase, rgobj;

    procedure ti386procinfo.allocate_interrupt_parameter;
      begin
         { we push Flags and CS as long
           to cope with the IRETD
           and we save 6 register + 4 selectors }
         inc(procdef.parast.address_fixup,8+6*4+4*2);
      end;


    procedure ti386procinfo.allocate_framepointer;
      begin
        if framepointer.number=NR_EBP then
          begin
            { Make sure the register allocator won't allocate registers
              into ebp }
            include(rg.used_in_proc_int,RS_EBP);
            exclude(rg.unusedregsint,RS_EBP);
          end;
      end;


begin
   cprocinfo:=ti386procinfo;
end.
{
  $Log$
  Revision 1.8  2003-06-13 21:19:31  peter
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


