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
  symconst,symtype,symdef,
  parabase, hlcgobj, hlcg2ll;

  type
    thlcgmips = class(thlcg2ll)
      function a_call_name(list: TAsmList; pd: tprocdef; const s: TSymStr; forceresdef: tdef; weak: boolean): tcgpara; override;
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
      ref: treference;
      sym: tasmsymbol;
    begin
      if weak then
        sym:=current_asmdata.WeakRefAsmSymbol(s)
      else
        sym:=current_asmdata.RefAsmSymbol(s);

      if (po_external in pd.procoptions) then
        begin
          if not (cs_create_pic in current_settings.moduleswitches) then
            begin
              reference_reset_symbol(ref,current_asmdata.RefAsmSymbol('_gp'),0,sizeof(aint));
              list.concat(tai_comment.create(strpnew('Using PIC code for a_call_name')));
              cg.a_loadaddr_ref_reg(list,ref,NR_GP);
            end;
          TCGMIPS(cg).a_call_sym_pic(list,sym);
        end
      else
        cg.a_call_name(list,s,weak);
      { set the result location }
      result:=get_call_result_cgpara(pd,forceresdef);
    end;


  procedure create_hlcodegen;
    begin
      hlcg:=thlcgmips.create;
      create_codegen;
    end;

end.
