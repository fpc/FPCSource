{
    $Id$
    Copyright (c) 2002 by Florian Klaempfl

    Generates the argument location information for 680x0

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
{ Generates the argument location information for 680x0.
}
unit cpupara;

{$i fpcdefs.inc}

  interface

    uses
      globtype,
      cpubase,
      symconst,symdef,
      paramgr;

    type
       { Returns the location for the nr-st 32 Bit int parameter
         if every parameter before is an 32 Bit int parameter as well
         and if the calling conventions for the helper routines of the
         rtl are used.
       }
       tm68kparamanager = class(tparamanager)
          function getintparaloc(calloption : tproccalloption;nr : longint) : tparalocation;override;
          function create_paraloc_info(p : tabstractprocdef; side: tcallercallee):longint;override;
         private
           function parseparaloc(p : tparaitem;const s : string) : boolean;override;
       end;

  implementation

    uses
       verbose,
       globals,
       systems,
       cpuinfo,cgbase,
       defutil;

    function tm68kparamanager.getintparaloc(calloption : tproccalloption;nr : longint) : tparalocation;
      begin
         fillchar(result,sizeof(tparalocation),0);
         if nr<1 then
           internalerror(2002070801)
         else
           begin
              { warning : THIS ONLY WORKS WITH INTERNAL ROUTINES,
                WHICH MUST ALWAYS PASS 4-BYTE PARAMETERS!!
              }
              result.loc:=LOC_REFERENCE;
              result.reference.index:=NR_STACK_POINTER_REG;
              result.reference.offset:=target_info.first_parm_offset+nr*4;
           end;
      end;


    function tm68kparamanager.create_paraloc_info(p : tabstractprocdef; side: tcallercallee):longint;
      var
        param_offset : integer;
        hp : tparaitem;
        paraloc: tparalocation;
        l : longint;
        parasize : longint;
      begin
         { frame pointer for nested procedures? }
         { inc(nextintreg);                     }
         { constructor? }
         { destructor? }
         param_offset := target_info.first_parm_offset;
         hp:=tparaitem(p.para.last);
         while assigned(hp) do
           begin
             paraloc.size:=def_cgsize(hp.paratype.def);
             paraloc.loc:=LOC_REFERENCE;
             paraloc.alignment:=4;
             paraloc.reference.index:=NR_FRAME_POINTER_REG;
             l:=push_size(hp.paratyp,hp.paratype.def,p.proccalloption);
             paraloc.reference.offset:=parasize;
             parasize:=parasize+l;
             hp.paraloc[callerside]:=paraloc;
             hp:=tparaitem(hp.next);
           end;
      end;


    function tm68kparamanager.parseparaloc(p : tparaitem;const s : string) : boolean;
      begin
        result:=false;
        case target_info.system of
          system_m68k_amiga:
            begin
              p.paraloc[callerside].loc:=LOC_REGISTER;
              p.paraloc[callerside].lochigh:=LOC_INVALID;
              p.paraloc[callerside].size:=def_cgsize(p.paratype.def);
              p.paraloc[callerside].alignment:=4;
              { pattern is always uppercase'd }
              if s='D0' then
                p.paraloc[callerside].register:=NR_D0
              else if s='D1' then
                p.paraloc[callerside].register:=NR_D1
              else if s='D2' then
                p.paraloc[callerside].register:=NR_D2
              else if s='D3' then
                p.paraloc[callerside].register:=NR_D3
              else if s='D4' then
                p.paraloc[callerside].register:=NR_D4
              else if s='D5' then
                p.paraloc[callerside].register:=NR_D5
              else if s='D6' then
                p.paraloc[callerside].register:=NR_D6
              else if s='D7' then
                p.paraloc[callerside].register:=NR_D7
              else if s='A0' then
                p.paraloc[callerside].register:=NR_A0
              else if s='A1' then
                p.paraloc[callerside].register:=NR_A1
              else if s='A2' then
                p.paraloc[callerside].register:=NR_A2
              else if s='A3' then
                p.paraloc[callerside].register:=NR_A3
              else if s='A4' then
                p.paraloc[callerside].register:=NR_A4
              else if s='A5' then
                p.paraloc[callerside].register:=NR_A5
              { 'A6' is problematic, since it's the frame pointer in fpc,
                so it should be saved before a call! }
              else if s='A6' then
                p.paraloc[callerside].register:=NR_A6
              { 'A7' is the stack pointer on 68k, can't be overwritten by API calls }
              else
                exit;
              p.paraloc[calleeside]:=p.paraloc[callerside];
            end;
          else
            internalerror(200405092);
        end;
        result:=true;
      end;

begin
  paramanager:=tm68kparamanager.create;
end.

{
  $Log$
  Revision 1.7  2004-06-20 08:55:31  florian
    * logs truncated

  Revision 1.6  2004/05/12 13:28:01  karoly
   * added some basic code for later syscall support on M68k/Amiga

  Revision 1.5  2004/01/30 12:17:18  florian
    * fixed some m68k compilation problems

}
