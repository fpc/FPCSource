{
    $Id$
    Copyright (c) 2002 by Florian Klaempfl

    Generates the argument location information for i386

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published bymethodpointer
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
{ Generates the argument location information for i386.
}
unit cpupara;

{$i fpcdefs.inc}

  interface

    uses
       cpubase,
       symtype,symdef,paramgr;

    type
       { Returns the location for the nr-st 32 Bit int parameter
         if every parameter before is an 32 Bit int parameter as well
         and if the calling conventions for the helper routines of the
         rtl are used.
       }
       ti386paramanager = class(tparamanager)
          function ret_in_acc(def : tdef) : boolean;override;
          function ret_in_param(def : tdef) : boolean;override;
          function getintparaloc(nr : longint) : tparalocation;override;
          procedure create_param_loc_info(p : tabstractprocdef);override;
          function getselflocation(p : tabstractprocdef) : tparalocation;override;
       end;

  implementation

    uses
       verbose;

    function ti386paramanager.ret_in_acc(def : tdef) : boolean;
      begin
{$ifdef TEST_WIN32_RECORDS}
        { Win32 returns small records in the accumulator }
        if ((target_info.system=system_i386_win32) and
            (def.deftype=recorddef) and (def.size<=8)) then
          result:=true
        else
{$endif TEST_WIN32_RECORDS}
          result:=inherited ret_in_acc(def);
      end;

    function ti386paramanager.ret_in_param(def : tdef) : boolean;
      begin
{$ifdef TEST_WIN32_RECORDS}
        { Win32 returns small records in the accumulator }
        if ((target_info.system=system_i386_win32) and
            (def.deftype=recorddef) and (def.size<=8)) then
          result:=false
        else
{$endif TEST_WIN32_RECORDS}
          result:=inherited ret_in_param(def);
      end;

    function ti386paramanager.getintparaloc(nr : longint) : tparalocation;
      begin
      end;

    procedure ti386paramanager.create_param_loc_info(p : tabstractprocdef);
      begin
         { set default para_alignment to target_info.stackalignment }
         { if para_alignment=0 then
           para_alignment:=aktalignment.paraalign;
         }
      end;

    function ti386paramanager.getselflocation(p : tabstractprocdef) : tparalocation;
      begin
         getselflocation.loc:=LOC_REFERENCE;
         getselflocation.reference.index:=R_ESP;
         getselflocation.reference.offset:=4;
      end;

begin
   paramanager:=ti386paramanager.create;
end.
{
  $Log$
  Revision 1.4  2002-11-15 01:58:56  peter
    * merged changes from 1.0.7 up to 04-11
      - -V option for generating bug report tracing
      - more tracing for option parsing
      - errors for cdecl and high()
      - win32 import stabs
      - win32 records<=8 are returned in eax:edx (turned off by default)
      - heaptrc update
      - more info for temp management in .s file with EXTDEBUG

  Revision 1.3  2002/08/09 07:33:04  florian
    * a couple of interface related fixes

  Revision 1.2  2002/07/11 14:41:32  florian
    * start of the new generic parameter handling

  Revision 1.1  2002/07/07 09:52:33  florian
    * powerpc target fixed, very simple units can be compiled
    * some basic stuff for better callparanode handling, far from being finished

}
