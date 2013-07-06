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
      procedure a_load_subsetreg_reg(list: TAsmList; subsetsize, tosize: tdef; const sreg: tsubsetregister; destreg: tregister);override;
    protected
      procedure a_load_regconst_subsetreg_intern(list: TAsmList; fromsize, subsetsize: tdef; fromreg: tregister; const sreg: tsubsetregister; slopt: tsubsetloadopt); override;
  end;

  procedure create_hlcodegen;

implementation

  uses
    verbose,
    aasmtai,
    aasmcpu,
    cutils,
    globals,
    defutil,
    cgobj,
    cpubase,
    cpuinfo,
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


  procedure thlcgmips.a_load_subsetreg_reg(list: TAsmList; subsetsize, tosize: tdef; const sreg: tsubsetregister; destreg: tregister);
    var
      cgsubsetsize,
      cgtosize: tcgsize;
    begin
      cgsubsetsize:=def_cgsize(subsetsize);
      cgtosize:=def_cgsize(tosize);
      if (current_settings.cputype<>cpu_mips32r2) and (current_settings.cputype<>cpu_pic32mx) then
        inherited a_load_subsetreg_reg(list,subsetsize,tosize,sreg,destreg)
      else if (sreg.bitlen>32) then
        InternalError(2013070201)
      else if (sreg.bitlen<>32) then
        begin
          list.concat(taicpu.op_reg_reg_const_const(A_EXT,destreg,sreg.subsetreg,
            sreg.startbit,sreg.bitlen));
          { types with a negative lower bound are always a base type (8, 16, 32 bits) }
          if (cgsubsetsize in [OS_S8..OS_S128]) then
            if ((sreg.bitlen mod 8) = 0) then
              begin
                cg.a_load_reg_reg(list,tcgsize2unsigned[cgsubsetsize],cgsubsetsize,destreg,destreg);
                cg.a_load_reg_reg(list,cgsubsetsize,cgtosize,destreg,destreg);
              end
            else
              begin
                cg.a_op_const_reg(list,OP_SHL,OS_INT,32-sreg.bitlen,destreg);
                cg.a_op_const_reg(list,OP_SAR,OS_INT,32-sreg.bitlen,destreg);
              end;
        end
      else
        cg.a_load_reg_reg(list,cgsubsetsize,cgtosize,sreg.subsetreg,destreg);
    end;


  procedure thlcgmips.a_load_regconst_subsetreg_intern(list: TAsmList; fromsize, subsetsize: tdef; fromreg: tregister; const sreg: tsubsetregister; slopt: tsubsetloadopt);
    begin
      if (current_settings.cputype<>cpu_mips32r2)  and (current_settings.cputype<>cpu_pic32mx) then
        inherited a_load_regconst_subsetreg_intern(list,fromsize,subsetsize,fromreg,sreg,slopt)
      else if (sreg.bitlen>32) then
        InternalError(2013070202)
      else if (sreg.bitlen<>32) then
        begin
          case slopt of
            SL_SETZERO:
              fromreg:=NR_R0;
            SL_SETMAX:
              begin
                fromreg:=cg.getintregister(list,OS_INT);
                cg.a_load_const_reg(list,OS_INT,-1,fromreg);
              end;
          end;
          list.concat(taicpu.op_reg_reg_const_const(A_INS,sreg.subsetreg,fromreg,
            sreg.startbit,sreg.bitlen));
        end
      else if not (slopt in [SL_SETZERO,SL_SETMAX]) then
        cg.a_load_reg_reg(list,def_cgsize(fromsize),def_cgsize(subsetsize),fromreg,sreg.subsetreg)
      else
        inherited a_load_regconst_subsetreg_intern(list,fromsize,subsetsize,fromreg,sreg,slopt);
    end;


  procedure create_hlcodegen;
    begin
      hlcg:=thlcgmips.create;
      create_codegen;
    end;

end.
