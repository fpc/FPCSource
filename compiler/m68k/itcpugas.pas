{
    Copyright (c) 1998-2002 by Florian Klaempfl

    This unit contains the m68k GAS instruction tables

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
      gas_op2str : op2strtable=
       {  warning: CPU32 opcodes are not fully compatible with the MC68020. }
       { 68000 only opcodes }
       ( '',
         'abcd','add','adda','addi','addq','addx','and','andi',
         'asl','asr','bcc','bcs','beq','bge','bgt','bhi',
         'ble','bls','blt','bmi','bne','bpl','bvc','bvs',
         'bchg','bclr','bra','bset','bsr','btst','chk',
         'clr','cmp','cmpa','cmpi','cmpm','dbcc','dbcs','dbeq','dbge',
         'dbgt','dbhi','dble','dbls','dblt','dbmi','dbne','dbra',
         'dbpl','dbt','dbvc','dbvs','dbf','divs','divu',
         'eor','eori','exg','illegal','ext','jmp','jsr',
         'lea','link','lsl','lsr','move','movea','movei','moveq',
         'movem','movep','muls','mulu','nbcd','neg','negx',
         'nop','not','or','ori','pea','rol','ror','roxl',
         'roxr','rtr','rts','sbcd','scc','scs','seq','sge',
         'sgt','shi','sle','sls','slt','smi','sne',
         'spl','st','svc','svs','sf','sub','suba','subi','subq',
         'subx','swap','tas','trap','trapv','tst','unlk',
         'rte','reset','stop',
         { mc68010 instructions }
         'bkpt','movec','moves','rtd',
         { mc68020 instructions }
         'bfchg','bfclr','bfexts','bfextu','bfffo',
         'bfins','bfset','bftst','callm','cas','cas2',
         'chk2','cmp2','divsl','divul','extb','pack','rtm',
         'trapcc','tracs','trapeq','trapf','trapge','trapgt',
         'traphi','traple','trapls','traplt','trapmi','trapne',
         'trappl','trapt','trapvc','trapvs','unpk',
         { mc64040 instructions }
         'move16',
         { coldfire v4 instructions }
         'mov3q','mvz','mvs','sats','byterev','ff1',
         { fpu processor instructions - directly supported only. }
         { ieee aware and misc. condition codes not supported   }
         'fabs','fadd',
         'fbeq','fbne','fbngt','fbgt','fbge','fbnge',
         'fblt','fbnlt','fble','fbgl','fbngl','fbgle','fbngle',
         'fdbeq','fdbne','fdbgt','fdbngt','fdbge','fdbnge',
         'fdblt','fdbnlt','fdble','fdbgl','fdbngl','fdbgle','fdbngle',
         'fseq','fsne','fsgt','fsngt','fsge','fsnge',
         'fslt','fsnlt','fsle','fsgl','fsngl','fsgle','fsngle',
         'fcmp','fdiv','fmove','fmovem',
         'fmul','fneg','fnop','fsqrt','fsub','fsgldiv',
         'fsflmul','ftst',
         'ftrapeq','ftrapne','ftrapgt','ftrapngt','ftrapge','ftrapnge',
         'ftraplt','ftrapnlt','ftraple','ftrapgl','ftrapngl','ftrapgle','ftrapngle',
         { protected instructions }
         'cprestore','cpsave',
         { fpu unit protected instructions                    }
         { and 68030/68851 common mmu instructions            }
         { (this may include 68040 mmu instructions)          }
         'frestore','fsave','pflush','pflusha','pload','pmove','ptest',
         { useful for assembly language output }
         'label','db','s','b','fs','fb');

    function gas_regnum_search(const s:string):Tregister;
    function gas_regname(r:Tregister):string;

  implementation

    const
      gas_regname_table : array[tregisterindex] of string[7] = (
        {r386att.inc contains the AT&T name of each register.}
        {$i r68kgas.inc}
      );

      gas_regname_index : array[tregisterindex] of tregisterindex = (
        {r386ari.inc contains an index which sorts att_regname_table by
         ATT name.}
        {$i r68kgri.inc}
      );


    function findreg_by_gasname(const s:string):byte;
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
          result:='%'+generic_regname(r);
      end;

end.
