{
    $Id: cpupara.pas,v 1.10 2005/02/14 17:13:10 peter Exp $
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
      symconst,symdef,symsym,
      parabase,paramgr;

    type
       { Returns the location for the nr-st 32 Bit int parameter
         if every parameter before is an 32 Bit int parameter as well
         and if the calling conventions for the helper routines of the
         rtl are used.
       }
       tm68kparamanager = class(tparamanager)
          procedure getintparaloc(calloption : tproccalloption; nr : longint;var cgpara : TCGPara);override;
          function create_paraloc_info(p : tabstractprocdef; side: tcallercallee):longint;override;
         private
           function parseparaloc(p : tparavarsym;const s : string) : boolean;override;
       end;

  implementation

    uses
       verbose,
       globals,
       systems,
       cpuinfo,cgbase,
       defutil;

    procedure tm68kparamanager.getintparaloc(calloption : tproccalloption; nr : longint;var cgpara : TCGPara);
      var
        paraloc : pcgparalocation;
      begin
         if nr<1 then
           internalerror(2002070801);
         cgpara.reset;
         cgpara.size:=OS_INT;
         cgpara.alignment:=std_param_align;
         paraloc:=cgpara.add_location;
         with paraloc^ do
           begin
              { warning : THIS ONLY WORKS WITH INTERNAL ROUTINES,
                WHICH MUST ALWAYS PASS 4-BYTE PARAMETERS!!
              }
              loc:=LOC_REFERENCE;
              reference.index:=NR_STACK_POINTER_REG;
              reference.offset:=target_info.first_parm_offset+nr*4;
           end;
      end;


    function tm68kparamanager.create_paraloc_info(p : tabstractprocdef; side: tcallercallee):longint;
      var
        paraloc      : pcgparalocation;
        hp           : tparavarsym;
        paracgsize   : tcgsize;
        paralen      : longint;
        parasize     : longint;
        i            : longint;
      begin
        parasize:=0;
        for i:=0 to p.paras.count-1 do
          begin
            hp:=tparavarsym(p.paras[i]);

            hp.paraloc[side].reset;
            { currently only support C-style array of const }
            if (p.proccalloption in [pocall_cdecl,pocall_cppdecl]) and
               is_array_of_const(hp.vartype.def) then
              begin
                paraloc:=hp.paraloc[side].add_location;
                { hack: the paraloc must be valid, but is not actually used }
                paraloc^.loc:=LOC_REFERENCE;
                if side=callerside then
                  paraloc^.reference.index:=NR_STACK_POINTER_REG
                else
                  paraloc^.reference.index:=NR_FRAME_POINTER_REG;
                paraloc^.size:=OS_ADDR;
                paraloc^.reference.offset:=0;
                break;
              end;

            if push_addr_param(hp.varspez,hp.vartype.def,p.proccalloption) then
              paracgsize:=OS_ADDR
            else
              begin
                paracgsize:=def_cgsize(hp.vartype.def);
                if paracgsize=OS_NO then
                  paracgsize:=OS_ADDR;
              end;
            hp.paraloc[side].size:=paracgsize;
            hp.paraloc[side].Alignment:=std_param_align;
            paraloc:=hp.paraloc[side].add_location;
            paraloc^.size:=paracgsize;
            paraloc^.loc:=LOC_REFERENCE;
            if side=callerside then
              paraloc^.reference.index:=NR_STACK_POINTER_REG
            else
              paraloc^.reference.index:=NR_FRAME_POINTER_REG;
            paraloc^.reference.offset:=target_info.first_parm_offset+parasize;
          end;
        result:=parasize;
      end;


    function tm68kparamanager.parseparaloc(p : tparavarsym;const s : string) : boolean;
      var
        paraloc : pcgparalocation;
      begin
        result:=false;
        case target_info.system of
          system_m68k_amiga:
            begin
              p.paraloc[callerside].alignment:=4;
              paraloc:=p.paraloc[callerside].add_location;
              paraloc^.loc:=LOC_REGISTER;
              paraloc^.size:=def_cgsize(p.vartype.def);
              { pattern is always uppercase'd }
              if s='D0' then
                paraloc^.register:=NR_D0
              else if s='D1' then
                paraloc^.register:=NR_D1
              else if s='D2' then
                paraloc^.register:=NR_D2
              else if s='D3' then
                paraloc^.register:=NR_D3
              else if s='D4' then
                paraloc^.register:=NR_D4
              else if s='D5' then
                paraloc^.register:=NR_D5
              else if s='D6' then
                paraloc^.register:=NR_D6
              else if s='D7' then
                paraloc^.register:=NR_D7
              else if s='A0' then
                paraloc^.register:=NR_A0
              else if s='A1' then
                paraloc^.register:=NR_A1
              else if s='A2' then
                paraloc^.register:=NR_A2
              else if s='A3' then
                paraloc^.register:=NR_A3
              else if s='A4' then
                paraloc^.register:=NR_A4
              else if s='A5' then
                paraloc^.register:=NR_A5
              { 'A6' is problematic, since it's the frame pointer in fpc,
                so it should be saved before a call! }
              else if s='A6' then
                paraloc^.register:=NR_A6
              { 'A7' is the stack pointer on 68k, can't be overwritten by API calls }
              else
                exit;

              { copy to callee side }
              p.paraloc[calleeside].add_location^:=paraloc^;
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
  $Log: cpupara.pas,v $
  Revision 1.10  2005/02/14 17:13:10  peter
    * truncate log

}
