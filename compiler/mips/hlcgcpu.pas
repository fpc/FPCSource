{
    Copyright (c) 1998-2010 by Florian Klaempfl and Jonas Maebe
    Member of the Free Pascal development team

    This unit contains routines to create a pass-through high-level code
    generator. This is used by most regular code generators.

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

unit hlcgcpu;

{$i fpcdefs.inc}

interface

uses
  globtype,
  aasmbase, aasmdata,
  symdef,
  hlcgobj, hlcg2ll;
  
  type
    thlcg2mips = class(thlcg2ll)
      procedure a_call_name(list: TAsmList; pd: tprocdef; const s: TSymStr; weak: boolean);override;
	end;

  procedure create_hlcodegen;

implementation

  uses
	cgbase,
	cgutils,
	cgobj,
	cpubase,
	cgcpu;

  procedure thlcg2mips.a_call_name(list: TAsmList; pd: tprocdef; const s: TSymStr; weak: boolean);
    var
      ref : treference;
    begin
      if pd.proccalloption =pocall_cdecl then
        begin
          { Use $gp/$t9 registers as the code might be in a shared library }
		  reference_reset(ref,sizeof(aint));
		  ref.symbol:=current_asmdata.RefAsmSymbol('_gp');
		  cg.a_loadaddr_ref_reg(list,ref,NR_GP);
		  reference_reset(ref,sizeof(aint));
		  ref.symbol:=current_asmdata.RefAsmSymbol(s);
		  ref.base:=NR_GP;
		  ref.refaddr:=addr_pic;
		  cg.a_loadaddr_ref_reg(list,ref,NR_PIC_FUNC);
		  cg.a_call_reg(list,NR_PIC_FUNC);
        end
      else
        cg.a_call_name(list,s,weak);
    end;

  procedure create_hlcodegen;
    begin
      hlcg:=thlcg2mips.create;
      create_codegen;
    end;

end.
