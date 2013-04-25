{
    Copyright (c) 1998-2010 by Florian Klaempfl and Jonas Maebe
    Member of the Free Pascal development team

    This unit contains high-level code generator support for ppc64

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
  aasmdata,
  symtype,
  cgbase,cgutils,hlcgobj,hlcgppc;

type
  thlcgcpu = class(thlcgppcgen)
    procedure a_load_subsetreg_reg(list : TAsmList; subsetsize, tosize: tdef; const sreg: tsubsetregister; destreg: tregister); override;
    procedure a_load_const_subsetreg(list: TAsmlist; tosubsetsize: tdef; a: tcgint; const sreg: tsubsetregister); override;
   protected
    procedure a_load_regconst_subsetreg_intern(list : TAsmList; fromsize, subsetsize: tdef; fromreg: tregister; const sreg: tsubsetregister; slopt: tsubsetloadopt); override;
  end;

   procedure create_hlcodegen;

implementation

  uses
    cpubase,aasmcpu,
    defutil,
    cgobj,cgcpu;

  { thlcgcpu }

  procedure thlcgcpu.a_load_subsetreg_reg(list: TAsmList; subsetsize, tosize: tdef; const sreg: tsubsetregister; destreg: tregister);
    var
      subsetcgsize: tcgsize;
    begin
      subsetcgsize:=def_cgsize(subsetsize);
{$ifdef extdebug}
      list.concat(tai_comment.create(strpnew('a_load_subsetreg_reg subsetregsize = ' + cgsize2string(sreg.subsetregsize) + ' subsetsize = ' + cgsize2string(subsetcgsize) + ' startbit = ' + intToStr(sreg.startbit) + ' tosize = ' + cgsize2string(def_cgsize(tosize)))));
{$endif}
      { do the extraction if required and then extend the sign correctly. (The latter is actually required only for signed subsets
      and if that subset is not >= the tosize). }
      if (sreg.startbit<>0) or
         (sreg.bitlen<>tcgsize2size[subsetcgsize]*8) then
        begin
          list.concat(taicpu.op_reg_reg_const_const(A_RLDICL,destreg,sreg.subsetreg,(64-sreg.startbit) and 63,64-sreg.bitlen));
          if subsetcgsize in [OS_S8..OS_S128] then
            if (sreg.bitlen mod 8)=0 then
              begin
                cg.a_load_reg_reg(list, tcgsize2unsigned[subsetcgsize],subsetcgsize,destreg,destreg);
                cg.a_load_reg_reg(list,subsetcgsize,def_cgsize(tosize),destreg,destreg);
              end
            else
              begin
                cg.a_op_const_reg(list,OP_SHL,OS_INT,64-sreg.bitlen,destreg);
                cg.a_op_const_reg(list,OP_SAR,OS_INT,64-sreg.bitlen,destreg);
             end;
        end
      else
        begin
          cg.a_load_reg_reg(list,tcgsize2unsigned[sreg.subsetregsize],subsetcgsize,sreg.subsetreg,destreg);
          cg.a_load_reg_reg(list,subsetcgsize,def_cgsize(tosize),destreg,destreg);
        end;
    end;


  procedure thlcgcpu.a_load_const_subsetreg(list: TAsmlist; tosubsetsize: tdef; a: tcgint; const sreg: tsubsetregister);
    var
      tmpreg : TRegister;
    begin
{$ifdef extdebug}
      list.concat(tai_comment.create(strpnew('a_load_const_subsetreg subsetregsize = ' + cgsize2string(sreg.subsetregsize) + ' subsetsize = ' + cgsize2string(def_cgsize(subsetsize)) + ' startbit = ' + intToStr(sreg.startbit) + ' a = ' + intToStr(a))));
{$endif}
      { loading the constant into the lowest bits of a temp register and then inserting is
        better than loading some usually large constants and do some masking and shifting on ppc64 }
      tmpreg:=getintregister(list,tosubsetsize);
      a_load_const_reg(list,tosubsetsize,a,tmpreg);
      a_load_reg_subsetreg(list,tosubsetsize,tosubsetsize,tmpreg,sreg);
    end;


  procedure thlcgcpu.a_load_regconst_subsetreg_intern(list: TAsmList; fromsize, subsetsize: tdef; fromreg: tregister; const sreg: tsubsetregister; slopt: tsubsetloadopt);
    begin
{$ifdef extdebug}
      list.concat(tai_comment.create(strpnew('a_load_reg_subsetreg fromsize = ' + cgsize2string(def_cgsize(fromsize)) + ' subsetregsize = ' + cgsize2string(sreg.subsetregsize) + ' subsetsize = ' + cgsize2string(def_cgsize(subsetsize)) + ' startbit = ' + IntToStr(sreg.startbit))));
{$endif}
      if slopt in [SL_SETZERO,SL_SETMAX] then
        inherited a_load_regconst_subsetreg_intern(list,fromsize,subsetsize,fromreg,sreg,slopt)
      else if sreg.bitlen<>sizeof(aint)*8 then
        { simply use the INSRDI instruction }
        list.concat(taicpu.op_reg_reg_const_const(A_INSRDI,sreg.subsetreg,fromreg,sreg.bitlen,(64-(sreg.startbit+sreg.bitlen)) and 63))
      else
        a_load_reg_reg(list,fromsize,subsetsize,fromreg,sreg.subsetreg);
    end;


  procedure create_hlcodegen;
    begin
      hlcg:=thlcgcpu.create;
      create_codegen;
    end;



end.
