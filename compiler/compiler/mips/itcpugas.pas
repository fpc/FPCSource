{
    Copyright (c) 1998-2005 by Florian Klaempfl

    This unit contains the MIPS GAS instruction tables

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
unit itcpugas;

{$i fpcdefs.inc}

interface

  uses
    cpubase,cgbase;


  const
    { Standard opcode string table (for each tasmop enumeration). The
      opcode strings should conform to the names as defined by the
      processor manufacturer.
    }
    gas_op2str : op2strtable = (
              'abs_d','abs_s','add','add_d','add_s','addi','addiu','addu',
              'and','andi','bc1f','bc1fl','bc1t','bc1tl','bc2f','bc2fl',
              'bc2t','bc2tl','beq','beql','bgez','bgezal','bgezall','bgezl',
              'bgtz','bgtzl','blez','blezl','bltz','bltzal','bltzall','bltzl',
              'bne','bnel','break','c_cond_d','c_cond_s','cache','ceil_w_d','ceil_w_s',
              'cfc1','cfc2','clo','clz','cop2','ctc1','ctc2','cvt_d_s',
              'cvt_d_w','cvt_s_d','cvt_s_w','cvt_w_d','cvt_w_s','div','div_d','div_s',
              'divu','eret','floor_w_d','floor_w_s','j','jal','jalr','jr',
              'lb','lbu','ldc1','ldc2','lh','lhu','ll','lui',
              'lw','lwc1','lwc2','lwl','lwr','madd','maddu','mfc0',
              'mfc1','mfc2','mfhi','mflo','mov_d','mov_s','movf','movf_d',
              'movf_s','movn','movn_d','movn_s','movt','movt_d','movt_s','movz',
              'movz_d','movz_s','msub','msubu','mtc0','mtc1','mtc2','mthi',
              'mtlo','mul','mul_d','mul_s','mult','multu','neg_d','neg_s',
              'nor','or','ori','pref','round_w_d','round_w_s','sb','sc',
              'sdc1','sdc2','sh','sll','sllv','slt','slti','sltiu',
              'sltu','sqrt_d','sqrt_s','sra','srav','srl','srlv','ssnop',
              'sub','sub_d','sub_s','subu','sw','swc1','swc2','swl',
              'swr','sync','syscall','teq','teqi','tge','tgei','tgeiu',
              'tgeu','tlbp','tlbr','tlbwi','tlbwr','tlt','tlti','tltiu',
              'tltu','tne','tnei','trunc_w_d','trunc_w_s','wait','xor','xori'
             );


    function gas_regnum_search(const s:string):Tregister;
    function gas_regname(r:Tregister):string;


implementation

    uses
      cutils,verbose;

    const
      gas_regname_table : array[tregisterindex] of string[7] = (
        {$i rmipsgas.inc}
      );

      gas_regname_index : array[tregisterindex] of tregisterindex = (
        {$i rmipssri.inc}
      );

    function findreg_by_gasname(const s:string):tregisterindex;
      var
        i,p : tregisterindex;
      begin
        {Binary search.}
        p:=0;
        i:=regnumber_count_bsstart;
        repeat
          if (p+i<=high(tregisterindex)) and (gas_regname_table[gas_regname_index[p+i]]<=s) then
            p:=p+i;
          i:=i shr 1;
        until i=0;
        if gas_regname_table[gas_regname_index[p]]=s then
          findreg_by_gasname:=gas_regname_index[p]
        else
          findreg_by_gasname:=0;
      end;


    function gas_regnum_search(const s:string):Tregister;
      begin
        result:=regnumber_table[findreg_by_gasname(s)];
      end;


    function gas_regname(r:Tregister):string;
      var
        p : tregisterindex;
      begin
        p:=findreg_by_number(r);
        if p<>0 then
          result:=gas_regname_table[p]
        else
          result:=generic_regname(r);
      end;

end.
