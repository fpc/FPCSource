{
    Copyright (c) 1998-2010 by Florian Klaempfl and Jonas Maebe
    Member of the Free Pascal development team

    This unit contains high-level code generator support for riscv64

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
  cgbase,cgutils,hlcgobj,hlcgrv;

type
  thlcgcpu = class(thlcgriscv)
    procedure a_load_const_subsetreg(list: TAsmlist; tosubsetsize: tdef; a: tcgint; const sreg: tsubsetregister); override;
  end;

   procedure create_hlcodegen;

implementation

  uses
    cpubase,aasmcpu,
    defutil,
{$ifdef extdebug}
    aasmtai,cutils,cgrv,
{$endif}
    cgobj,cgcpu;

  { thlcgcpu }


  procedure thlcgcpu.a_load_const_subsetreg(list: TAsmlist; tosubsetsize: tdef; a: tcgint; const sreg: tsubsetregister);
    var
      tmpreg : TRegister;
    begin
{$ifdef extdebug}
      list.concat(tai_comment.create(strpnew('a_load_const_subsetreg subsetregsize = ' + tcgsize2str(sreg.subsetregsize) + ' subsetsize = ' + tcgsize2str(def_cgsize(tosubsetsize)) + ' startbit = ' + ToStr(sreg.startbit) + ' a = ' + ToStr(a))));
{$endif}
      { loading the constant into the lowest bits of a temp register and then inserting is
        better than loading some usually large constants and do some masking and shifting on riscv64 }
      tmpreg:=getintregister(list,tosubsetsize);
      a_load_const_reg(list,tosubsetsize,a,tmpreg);
      a_load_reg_subsetreg(list,tosubsetsize,tosubsetsize,tmpreg,sreg);
    end;


  procedure create_hlcodegen;
    begin
      hlcg:=thlcgcpu.create;
      create_codegen;
    end;



begin
  chlcgobj:=thlcgcpu;
end.
