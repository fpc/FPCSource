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
  cgbase, cgutils,
  symtype,symdef,
  parabase, hlcgobj, hlcg2ll;

  type
    thlcgmips = class(thlcg2ll)
      function a_call_name(list: TAsmList; pd: tprocdef; const s: TSymStr; forceresdef: tdef; weak: boolean): tcgpara; override;
      procedure a_call_reg(list : TAsmList;pd : tabstractprocdef;reg : tregister);override;
      procedure a_call_ref(list : TAsmList;pd : tabstractprocdef;const ref : treference);override;
  end;

  procedure create_hlcodegen;

implementation

  uses
    aasmtai,
    cutils,
    globals,
    cgobj,
    cpubase,
    cgcpu;

  function thlcgmips.a_call_name(list: TAsmList; pd: tprocdef; const s: TSymStr; forceresdef: tdef; weak: boolean): tcgpara;
    var
      ref : treference;
    begin
      if pd.proccalloption=pocall_cdecl then
        begin
          if (cs_create_pic in current_settings.moduleswitches) then
            begin
              reference_reset(ref,sizeof(aint));
              ref.symbol:=current_asmdata.RefAsmSymbol(s);
              cg.a_loadaddr_ref_reg(list,ref,NR_PIC_FUNC);
            end
          else
            begin
              { Use $gp/$t9 registers as the code might be in a shared library }
              reference_reset(ref,sizeof(aint));
              ref.symbol:=current_asmdata.RefAsmSymbol('_gp');
              list.concat(tai_comment.create(strpnew('Using PIC code for a_call_name')));
              cg.a_loadaddr_ref_reg(list,ref,NR_GP);
              reference_reset(ref,sizeof(aint));
              ref.symbol:=current_asmdata.RefAsmSymbol(s);
              ref.base:=NR_GP;
              ref.refaddr:=addr_pic_call16;
              cg.a_loadaddr_ref_reg(list,ref,NR_PIC_FUNC);
            end;
          cg.a_call_reg(list,NR_PIC_FUNC);
        end
      else
        cg.a_call_name(list,s,weak);
      { set the result location }
      result:=get_call_result_cgpara(pd,forceresdef);
    end;

  procedure thlcgmips.a_call_reg(list: TAsmList; pd: tabstractprocdef; reg: tregister);
    begin
      if (pd.proccalloption=pocall_cdecl) and (reg<>NR_PIC_FUNC) then
        begin
          list.concat(tai_comment.create(strpnew('Using PIC code for a_call_reg')));
          { Use $t9 register as the code might be in a shared library }
          cg.a_load_reg_reg(list,OS_32,OS_32,reg,NR_PIC_FUNC);
          cg.a_call_reg(list,NR_PIC_FUNC);
        end
      else
        cg.a_call_reg(list,reg);
    end;

  procedure thlcgmips.a_call_ref(list: TAsmList; pd: tabstractprocdef; const ref: treference);
    begin
      if pd.proccalloption =pocall_cdecl then
        begin
          { Use $t9 register as the code might be in a shared library }
          list.concat(tai_comment.create(strpnew('Using PIC code for a_call_ref')));
          cg.a_loadaddr_ref_reg(list,ref,NR_PIC_FUNC);
          cg.a_call_reg(list,NR_PIC_FUNC);
        end
      else
        cg.a_call_ref(list,ref);
    end;

  procedure create_hlcodegen;
    begin
      hlcg:=thlcgmips.create;
      create_codegen;
    end;

end.
