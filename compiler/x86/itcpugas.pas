{
    Copyright (c) 1998-2002 by Florian Klaempfl

    This unit contains the i386 AT&T instruction tables

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
      cgbase,cpubase;

    type
      TAttSuffix = (AttSufNONE,AttSufINT,AttSufFPU,AttSufFPUint,AttSufINTdual,AttSufMM);

    const
      { include mnemonic strings }
{$if defined(x86_64)}
      gas_op2str:op2strtable={$i x8664att.inc}
      gas_needsuffix:array[tasmop] of TAttSuffix={$i x8664ats.inc}
{$elseif defined(i386)}
      gas_op2str:op2strtable={$i i386att.inc}
      gas_needsuffix:array[tasmop] of TAttSuffix={$i i386atts.inc}
{$elseif defined(i8086)}
      gas_op2str:op2strtable={$i i8086att.inc}
      gas_needsuffix:array[tasmop] of TAttSuffix={$i i8086atts.inc}
{$endif}

{$ifdef x86_64}
     gas_opsize2str : array[topsize] of string[2] = ('',
       'b','w','l','q','bw','bl','wl','bq','wq','lq',
       's','l','q',
       's','l','t','v','x',
       'd',
       '','','',
       't',
        'x',
        'y'
     );
     { suffix-to-opsize conversion tables, used in asmreadrer }
     { !! S_LQ excluded: movzlq does not exist, movslq is processed
       as a separate instruction w/o suffix (aka movsxd), and there are
       no more instructions needing it. }
     att_sizesuffixstr : array[0..13] of string[2] = (
       '','BW','BL','WL','BQ','WQ',{'LQ',}'B','W','L','S','Q','T','X','Y'
     );
     att_sizesuffix : array[0..13] of topsize = (
       S_NO,S_BW,S_BL,S_WL,S_BQ,S_WQ,{S_LQ,}S_B,S_W,S_L,S_NO,S_Q,S_NO,S_XMM,S_YMM
     );
     att_sizefpusuffix : array[0..13] of topsize = (
       S_NO,S_NO,S_NO,S_NO,S_NO,S_NO,{S_NO,}S_NO,S_NO,S_FL,S_FS,S_NO,S_FX,S_NO,S_NO
     );
     att_sizefpuintsuffix : array[0..13] of topsize = (
       S_NO,S_NO,S_NO,S_NO,S_NO,S_NO,{S_NO,}S_NO,S_NO,S_IL,S_IS,S_IQ,S_NO,S_NO,S_NO
     );
{$else x86_64}
     gas_opsize2str : array[topsize] of string[2] = ('',
       'b','w','l','q','bw','bl','wl',
       's','l','q',
       's','l','t','v','',
       'd',
       '','','',
       't',
        'x',
        'y'
     );
     { suffix-to-opsize conversion tables, used in asmreadrer }
     att_sizesuffixstr : array[0..11] of string[2] = (
       '','BW','BL','WL','B','W','L','S','Q','T','X','Y'
     );
     att_sizesuffix : array[0..11] of topsize = (
       S_NO,S_BW,S_BL,S_WL,S_B,S_W,S_L,S_NO,S_NO,S_NO,S_XMM,S_YMM
     );
     att_sizefpusuffix : array[0..11] of topsize = (
       S_NO,S_NO,S_NO,S_NO,S_NO,S_NO,S_FL,S_FS,S_NO,S_FX,S_NO,S_NO
     );
     att_sizefpuintsuffix : array[0..11] of topsize = (
       S_NO,S_NO,S_NO,S_NO,S_NO,S_NO,S_IL,S_IS,S_IQ,S_NO,S_NO,S_NO
     );
{$endif x86_64}


    function gas_regnum_search(const s:string):Tregister;
    function gas_regname(r:Tregister):string;


implementation

    uses
      cutils,verbose,rgbase;

    const
    {$if defined(x86_64)}
      att_regname_table : TRegNameTable = (
        {r8664att.inc contains the AT&T name of each register.}
        {$i r8664att.inc}
      );

      att_regname_index : array[tregisterindex] of tregisterindex = (
        {r8664ari.inc contains an index which sorts att_regname_table by
         ATT name.}
        {$i r8664ari.inc}
      );
    {$elseif defined(i386)}
      att_regname_table : TRegNameTable = (
        {r386att.inc contains the AT&T name of each register.}
        {$i r386att.inc}
      );

      att_regname_index : array[tregisterindex] of tregisterindex = (
        {r386ari.inc contains an index which sorts att_regname_table by
         ATT name.}
        {$i r386ari.inc}
      );
    {$elseif defined(i8086)}
      att_regname_table : TRegNameTable = (
        {r8086att.inc contains the AT&T name of each register.}
        {$i r8086att.inc}
      );

      att_regname_index : array[tregisterindex] of tregisterindex = (
        {r8086ari.inc contains an index which sorts att_regname_table by
         ATT name.}
        {$i r8086ari.inc}
      );
    {$endif}



    function gas_regnum_search(const s:string):Tregister;
      begin
        result:=regnumber_table[findreg_by_name_table(s,att_regname_table,att_regname_index)];
      end;


    function gas_regname(r:Tregister):string;
      var
        p : tregisterindex;
      begin
        p:=findreg_by_number(r);
        if p<>0 then
          result:=att_regname_table[p]
        else
          result:='%'+generic_regname(r);
      end;

end.
