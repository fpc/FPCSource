{
    Copyright (c) 1998-2002 by Florian Klaempfl

    This unit contains the PowerPC GAS instruction tables

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

{$I fpcdefs.inc}

interface

uses
  cpubase, cgbase;

const
  gas_op2str: array[tasmop] of string[14] = ('<none>',
    'add', 'add.', 'addo', 'addo.', 'addc', 'addc.', 'addco', 'addco.',
    'adde', 'adde.', 'addeo', 'addeo.', 'addi', 'addic', 'addic.', 'addis',
    'addme', 'addme.', 'addmeo', 'addmeo.', 'addze', 'addze.', 'addzeo',
    'addzeo.', 'and', 'and.', 'andc', 'andc.', 'andi.', 'andis.', 'b',
    'ba', 'bl', 'bla', 'bc', 'bca', 'bcl', 'bcla', 'bcctr', 'bcctrl', 'bclr',
    'bclrl', 'cmp', 'cmpi', 'cmpl', 'cmpli', 'cntlzw', 'cntlzw.', 'crand',
    'crandc', 'creqv', 'crnand', 'crnor', 'cror', 'crorc', 'crxor', 'dcba',
    'dcbf', 'dcbi', 'dcbst', 'dcbt', 'dcbtst', 'dcbz', 'divw', 'divw.', 'divwo',
      'divwo.',
    'divwu', 'divwu.', 'divwuo', 'divwuo.', 'eciwx', 'ecowx', 'eieio', 'eqv',
    'eqv.', 'extsb', 'extsb.', 'extsh', 'extsh.', 'fabs', 'fabs.', 'fadd',
    'fadd.', 'fadds', 'fadds.', 'fcmpo', 'fcmpu', 'fctiw', 'fctiw.', 'fctiwz',
    'fctiwz.', 'fdiv', 'fdiv.', 'fdivs', 'fdivs.', 'fmadd', 'fmadd.', 'fmadds',
    'fmadds.', 'fmr', 'fmsub', 'fmsub.', 'fmsubs', 'fmsubs.', 'fmul', 'fmul.',
    'fmuls', 'fmuls.', 'fnabs', 'fnabs.', 'fneg', 'fneg.', 'fnmadd',
    'fnmadd.', 'fnmadds', 'fnmadds.', 'fnmsub', 'fnmsub.', 'fnmsubs',
    'fnmsubs.', 'fres', 'fres.', 'frsp', 'frsp.', 'frsqrte', 'frsqrte.',
    'fsel', 'fsel.', 'fsqrt', 'fsqrt.', 'fsqrts', 'fsqrts.', 'fsub', 'fsub.',
    'fsubs', 'fsubs.', 'icbi', 'isync', 'lbz', 'lbzu', 'lbzux', 'lbzx',
    'lfd', 'lfdu', 'lfdux', 'lfdx', 'lfs', 'lfsu', 'lfsux', 'lfsx', 'lha',
    'lhau', 'lhaux', 'lhax', 'lhbrx', 'lhz', 'lhzu', 'lhzux', 'lhzx', 'lmw',
    'lswi', 'lswx', 'lwarx', 'lwbrx', 'lwz', 'lwzu', 'lwzux', 'lwzx', 'mcrf',
    'mcrfs', 'mcrxr', 'mfcr', 'mffs', 'mffs.', 'mfmsr', 'mfspr', 'mfsr',
    'mfsrin', 'mftb', 'mtcrf', 'mtfsb0', 'mtfsb1', 'mtfsf', 'mtfsf.',
    'mtfsfi', 'mtfsfi.', 'mtmsr', 'mtspr', 'mtsr', 'mtsrin', 'mulhw',
    'mulhw.', 'mulhwu', 'mulhwu.', 'mulli', 'mullw', 'mullw.', 'mullwo',
    'mullwo.', 'nand', 'nand.', 'neg', 'neg.', 'nego', 'nego.', 'nor', 'nor.',
    'or', 'or.', 'orc', 'orc.', 'ori', 'oris', 'rfi', 'rlwimi', 'rlwimi.',
    'rlwinm', 'rlwinm.', 'rlwnm', 'sc', 'slw', 'slw.', 'sraw', 'sraw.',
    'srawi', 'srawi.', 'srw', 'srw.', 'stb', 'stbu', 'stbux', 'stbx', 'stfd',
    'stfdu', 'stfdux', 'stfdx', 'stfiwx', 'stfs', 'stfsu', 'stfsux', 'stfsx',
    'sth', 'sthbrx', 'sthu', 'sthux', 'sthx', 'stmw', 'stswi', 'stswx', 'stw',
    'stwbrx', 'stwcx.', 'stwu', 'stwux', 'stwx', 'subf', 'subf.', 'subfo',
    'subfo.', 'subfc', 'subc.', 'subfco', 'subfco.', 'subfe', 'subfe.',
    'subfeo', 'subfeo.', 'subfic', 'subfme', 'subfme.', 'subfmeo', 'subfmeo.',
    'subfze', 'subfze.', 'subfzeo', 'subfzeo.', 'sync', 'tlbia', 'tlbie',
    'tlbsync', 'tw', 'twi', 'xor', 'xor.', 'xori', 'xoris',
    { some simplified mnemonics }
    'subi', 'subis', 'subic', 'subic.', 'sub', 'sub.', 'subo', 'subo.',
    'subc', 'subc.', 'subco', 'subco.', 'cmpwi', 'cmpw', 'cmplwi', 'cmplw',
    'extlwi', 'extlwi.', 'extrwi', 'extrwi.', 'inslwi', 'inslwi.', 'insrwi',
    'insrwi.', 'rotlwi', 'rotlwi.', 'rotlw', 'rotlw.', 'slwi', 'slwi.',
    'srwi', 'srwi.', 'clrlwi', 'clrlwi.', 'clrrwi', 'clrrwi.', 'clrslwi',
    'clrslwi.', 'bf', 'bt', 'blr', 'bctr', 'blrl', 'bctrl', 'crset', 'crclr', 'crmove',
    'crnot', 'mt', 'mf', 'nop', 'li', 'lis', 'la', 'mr', 'mr.', 'not', 'mtcr',
      'mtlr', 'mflr',
    'mtctr', 'mfctr',
    'extsw', 'rldimi',
    'std', 'stdu', 'stdx', 'stdux',
    'ld', 'ldu', 'ldx', 'ldux',
    'cmpd', 'cmpdi', 'cmpld', 'cmpldi',
    'srdi', 'sradi',
    'sldi',
    'rldcl', 'rldcl_', 'rldicl', 'rldicl_', 'rldcr', 'rldcr_', 'rldicr', 'rldicr_',
    'divdu', 'divdu.', 'divd', 'divd.', 'mulld', 'mulld.', 'mulhd', 'mulhd.', 'srad', 'sld', 'srd',
    'divduo.', 'divdo.',
    'lwa', 'lwax', 'lwaux',
    'fcfid',
    'ldarx', 'stdcx.', 'cntlzd',
    'lvx', 'stvx',
    'mulldo', 'mulldo.', 'mulhdu', 'mulhdu.',
    'mfxer',
    'fctid', 'fctid.', 'fctidz', 'fctidz.',
    'extrdi', 'extrdi.', 'insrdi', 'insrdi.',
    'lwsync');

function gas_regnum_search(const s: string): Tregister;
function gas_regname(r: Tregister): string;

implementation

uses
  globtype, globals,aasmbase,
  cutils, verbose, systems;

const
  gas_regname_table: array[tregisterindex] of string[7] = (
{$I rppcgas.inc}
    );

  gas_regname_short_table: array[tregisterindex] of string[7] = (
{$I rppcgss.inc}
    );

  gas_regname_index: array[tregisterindex] of tregisterindex = (
{$I rppcgri.inc}
    );

function findreg_by_gasname(const s: string): tregisterindex;
var
  i, p: tregisterindex;
begin
  {Binary search.}
  p := 0;
  i := regnumber_count_bsstart;
  repeat
    if (p + i <= high(tregisterindex)) and (gas_regname_table[gas_regname_index[p + i]] <= s) then
      p := p + i;
    i := i shr 1;
  until i = 0;
  if gas_regname_table[gas_regname_index[p]] = s then
    findreg_by_gasname := gas_regname_index[p]
  else
    findreg_by_gasname := 0;
end;

function gas_regnum_search(const s: string): Tregister;
begin
  result := regnumber_table[findreg_by_gasname(s)];
end;

function gas_regname(r: Tregister): string;
var
  p: longint;
begin
  p := findreg_by_number(r);
  if p <> 0 then
    if create_smartlink_library and
       not(target_info.system = system_powerpc64_darwin) then
      result := gas_regname_short_table[p]
    else
      result := gas_regname_table[p]
  else
    result := generic_regname(r);
end;

end.

