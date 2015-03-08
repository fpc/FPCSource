{
    Copyright (c) 2012 by Jonas Maebe
    Member of the Free Pascal development team

    This unit replaces all abstract methods of cgobj that are unused for
    targets that are based on the high level code generator with stubs that
    result in an internalerror

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

unit cghlcpu;

{$i fpcdefs.inc}

interface

uses
  globtype,verbose,
  aasmbase,aasmdata,
  symtype,symdef,
  cpubase,cgbase,cgutils,cgobj;

  type
    thlbasecgcpu = class(tcg)
     public
      procedure g_save_registers(list:TAsmList);override;
      procedure g_restore_registers(list:TAsmList);override;
      procedure g_stackpointer_alloc(list: TAsmList; size: longint); override;
      procedure g_proc_exit(list: TAsmList; parasize: longint; nostackframe: boolean); override;
      procedure g_proc_entry(list: TAsmList; localsize: longint; nostackframe: boolean); override;
      procedure g_overflowcheck(list: TAsmList; const Loc: tlocation; def: tdef); override;
      procedure g_intf_wrapper(list: TAsmList; procdef: tprocdef; const labelname: string; ioffset: longint); override;
{$ifdef cpuflags}
      procedure g_flags2reg(list: TAsmList; size: TCgSize; const f: tresflags; reg: TRegister); override;
      procedure a_jmp_flags(list: TAsmList; const f: TResFlags; l: tasmlabel); override;
{$endif}
      procedure g_concatcopy(list: TAsmList; const source, dest: treference; len: tcgint); override;
      procedure a_op_reg_reg(list: TAsmList; Op: TOpCG; size: TCGSize; reg1, reg2: TRegister); override;
      procedure a_op_const_reg(list: TAsmList; Op: TOpCG; size: TCGSize; a: tcgint; reg: TRegister); override;
      procedure a_load_reg_reg(list: TAsmList; fromsize, tosize: tcgsize; reg1, reg2: tregister); override;
      procedure a_load_reg_ref(list: TAsmList; fromsize, tosize: tcgsize; register: tregister; const ref: treference); override;
      procedure a_load_ref_reg(list: TAsmList; fromsize, tosize: tcgsize; const ref: treference; register: tregister); override;
      procedure a_load_const_reg(list: TAsmList; size: tcgsize; a: tcgint; register: tregister); override;
      procedure a_loadfpu_reg_reg(list: TAsmList; fromsize, tosize: tcgsize; reg1, reg2: tregister); override;
      procedure a_loadfpu_reg_ref(list: TAsmList; fromsize, tosize: tcgsize; reg: tregister; const ref: treference); override;
      procedure a_loadfpu_ref_reg(list: TAsmList; fromsize, tosize: tcgsize; const ref: treference; reg: tregister); override;
      procedure a_loadaddr_ref_reg(list: TAsmList; const ref: treference; r: tregister); override;
      procedure a_jmp_name(list: TAsmList; const s: string); override;
      procedure a_jmp_always(list: TAsmList; l: tasmlabel); override;
      procedure a_cmp_reg_reg_label(list: TAsmList; size: tcgsize; cmp_op: topcmp; reg1, reg2: tregister; l: tasmlabel); override;
      procedure a_call_reg(list: TAsmList; reg: tregister); override;
      procedure a_call_name(list: TAsmList; const s: string; weak: boolean); override;
      procedure a_bit_scan_reg_reg(list: TAsmList; reverse: boolean; srcsize, dstsize: tcgsize; src, dst: TRegister); override;
    end;

implementation

   { thlbasecgcpu }

    procedure thlbasecgcpu.a_bit_scan_reg_reg(list: TAsmList; reverse: boolean; srcsize, dstsize: tcgsize; src, dst: TRegister);
      begin
        internalerror(2012042801);
      end;


    procedure thlbasecgcpu.a_call_name(list: TAsmList; const s: string; weak: boolean);
      begin
        internalerror(2012042802);
      end;


    procedure thlbasecgcpu.a_call_reg(list: TAsmList; reg: tregister);
      begin
        internalerror(2012042803);
      end;


    procedure thlbasecgcpu.a_cmp_reg_reg_label(list: TAsmList; size: tcgsize; cmp_op: topcmp; reg1, reg2: tregister; l: tasmlabel);
      begin
        internalerror(2012042804);
      end;


    procedure thlbasecgcpu.a_jmp_always(list: TAsmList; l: tasmlabel);
      begin
        internalerror(2012042805);
      end;

{$ifdef cpuflags}
    procedure thlbasecgcpu.a_jmp_flags(list: TAsmList; const f: TResFlags; l: tasmlabel);
      begin
        internalerror(2012042806);
      end;
{$endif}

    procedure thlbasecgcpu.a_jmp_name(list: TAsmList; const s: string);
      begin
        internalerror(2012042807);
      end;


    procedure thlbasecgcpu.a_loadaddr_ref_reg(list: TAsmList; const ref: treference; r: tregister);
      begin
        internalerror(2012042808);
      end;

    procedure thlbasecgcpu.a_loadfpu_ref_reg(list: TAsmList; fromsize, tosize: tcgsize; const ref: treference; reg: tregister);
      begin
        internalerror(2012042809);
      end;


    procedure thlbasecgcpu.a_loadfpu_reg_ref(list: TAsmList; fromsize, tosize: tcgsize; reg: tregister; const ref: treference);
      begin
        internalerror(2012042810);
      end;


    procedure thlbasecgcpu.a_loadfpu_reg_reg(list: TAsmList; fromsize, tosize: tcgsize; reg1, reg2: tregister);
      begin
        internalerror(2012042811);
      end;


    procedure thlbasecgcpu.a_load_const_reg(list: TAsmList; size: tcgsize; a: tcgint; register: tregister);
      begin
        internalerror(2012042812);
      end;


    procedure thlbasecgcpu.a_load_ref_reg(list: TAsmList; fromsize, tosize: tcgsize; const ref: treference; register: tregister);
      begin
        internalerror(2012042813);
      end;


    procedure thlbasecgcpu.a_load_reg_ref(list: TAsmList; fromsize, tosize: tcgsize; register: tregister; const ref: treference);
      begin
        internalerror(2012042814);
      end;


    procedure thlbasecgcpu.a_load_reg_reg(list: TAsmList; fromsize, tosize: tcgsize; reg1, reg2: tregister);
      begin
        internalerror(2012042815);
      end;


    procedure thlbasecgcpu.a_op_const_reg(list: TAsmList; Op: TOpCG; size: TCGSize; a: tcgint; reg: TRegister);
      begin
        internalerror(2012042816);
      end;


    procedure thlbasecgcpu.a_op_reg_reg(list: TAsmList; Op: TOpCG; size: TCGSize; reg1, reg2: TRegister);
      begin
        internalerror(2012042817);
      end;


    procedure thlbasecgcpu.g_concatcopy(list: TAsmList; const source, dest: treference; len: tcgint);
      begin
        internalerror(2012042818);
      end;

{$ifdef cpuflags}
    procedure thlbasecgcpu.g_flags2reg(list: TAsmList; size: TCgSize; const f: tresflags; reg: TRegister);
      begin
        internalerror(2012042819);
      end;
{$endif}

    procedure thlbasecgcpu.g_intf_wrapper(list: TAsmList; procdef: tprocdef; const labelname: string; ioffset: longint);
      begin
        internalerror(2012042820);
      end;


    procedure thlbasecgcpu.g_overflowcheck(list: TAsmList; const Loc: tlocation; def: tdef);
      begin
        internalerror(2012042820);
      end;


    procedure thlbasecgcpu.g_proc_entry(list: TAsmList; localsize: longint; nostackframe: boolean);
      begin
        internalerror(2012042821);
      end;


    procedure thlbasecgcpu.g_proc_exit(list: TAsmList; parasize: longint; nostackframe: boolean);
      begin
        internalerror(2012042822);
      end;

    procedure thlbasecgcpu.g_save_registers(list: TAsmList);
      begin
        { do nothing }
      end;

    procedure thlbasecgcpu.g_restore_registers(list: TAsmList);
      begin
        { do nothing }
      end;

    procedure thlbasecgcpu.g_stackpointer_alloc(list: TAsmList; size: longint);
      begin
        internalerror(2012042823);
      end;


end.

