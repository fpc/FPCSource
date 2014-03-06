{
    Copyright (c) 2010-2013 by Jonas Maebe

    This unit implements the code generator for LLVM

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
unit cgllvm;

{$i fpcdefs.inc}

interface

    uses
       globtype,parabase,
       cgbase,cgutils,cgobj,cghlcpu,
       llvmbase,llvminfo,aasmbase,aasmtai,aasmdata,aasmllvm;

    type
      tcgllvm=class(thlbasecgcpu)
     public
        procedure a_label(list : TAsmList;l : tasmlabel);override;
        procedure a_jmp_always(list: TAsmList; l: tasmlabel); override;
        procedure init_register_allocators;override;
        procedure done_register_allocators;override;
        function  getintregister(list:TAsmList;size:Tcgsize):Tregister;override;
        function  getfpuregister(list:TAsmList;size:Tcgsize):Tregister;override;
      end;

    procedure create_codegen;

implementation

  uses
    globals,verbose,systems,cutils,
    paramgr,fmodule,
    tgobj,rgllvm,cpubase,
    procinfo,cpupi;


{****************************************************************************
                              Assembler code
****************************************************************************}

    procedure tcgllvm.a_label(list: TAsmList; l: tasmlabel);
      begin
        { in llvm, every block must end with a terminator instruction, such as
          a branch -> if the previous instruction is not a terminator instruction,
          add an unconditional branch to the next block (= the one starting with
          this label) }
        if not assigned(list.last) or
           (tai(list.Last).typ<>ait_llvmins) or
           not(taillvm(list.Last).llvmopcode in llvmterminatoropcodes) then
          a_jmp_always(list,l);
        inherited;
      end;


    procedure tcgllvm.a_jmp_always(list: TAsmList; l: tasmlabel);
      begin
        list.concat(taillvm.op_lab(la_br,l));
      end;


    procedure tcgllvm.init_register_allocators;
      begin
        inherited init_register_allocators;
        rg[R_INTREGISTER]:=Trgllvm.create(R_INTREGISTER,R_SUBWHOLE,
          [0],first_int_imreg,[]);
        rg[R_FPUREGISTER]:=Trgllvm.create(R_FPUREGISTER,R_SUBWHOLE,
          [0],first_fpu_imreg,[]);
        rg[R_MMREGISTER]:=Trgllvm.create(R_MMREGISTER,R_SUBWHOLE,
          [0],first_mm_imreg,[]);
        { every temp gets its own "base register" to uniquely identify it }
        rg[R_TEMPREGISTER]:=trgllvm.Create(R_TEMPREGISTER,R_SUBWHOLE,
          [0],1,[]);
      end;


    procedure tcgllvm.done_register_allocators;
      begin
        rg[R_INTREGISTER].free;
        rg[R_FPUREGISTER].free;
        rg[R_TEMPREGISTER].free;
        inherited done_register_allocators;
      end;


    function tcgllvm.getintregister(list:TAsmList;size:Tcgsize):Tregister;
      begin
        { all size determinations are based on tdef, subregisters are
          irrelevant }
        result:=rg[R_INTREGISTER].getregister(list,R_SUBWHOLE)
      end;


    function tcgllvm.getfpuregister(list:TAsmList;size:Tcgsize):Tregister;
      begin
        { all size determinations are based on tdef, subregisters are
          irrelevant }
        result:=rg[R_FPUREGISTER].getregister(list,R_SUBWHOLE);
      end;


    procedure create_codegen;
      begin
        cg:=tcgllvm.Create;
      end;

end.
