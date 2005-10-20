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
      TAttSuffix = (AttSufNONE,AttSufINT,AttSufFPU,AttSufFPUint);

    const
{$ifdef x86_64}
      {x86att.inc contains the name for each x86-64 mnemonic}
      gas_op2str:op2strtable={$i x8664att.inc}
      gas_needsuffix:array[tasmop] of TAttSuffix={$i x8664ats.inc}
{$else x86_64}
      {x86att.inc contains the name for each i386 mnemonic}
      gas_op2str:op2strtable={$i i386att.inc}
      gas_needsuffix:array[tasmop] of TAttSuffix={$i i386atts.inc}
{$endif x86_64}

{$ifdef x86_64}
     gas_opsize2str : array[topsize] of string[2] = ('',
       'b','w','l','q','bw','bl','wl','bq','wq','lq',
       's','l','q',
       's','l','t','v','x',
       'd',
       '','','',
       't',
       ''
     );
{$else x86_64}
     gas_opsize2str : array[topsize] of string[2] = ('',
       'b','w','l','q','bw','bl','wl',
       's','l','q',
       's','l','t','v','',
       'd',
       '','','',
       't',
       ''
     );
{$endif x86_64}


    function gas_regnum_search(const s:string):Tregister;
    function gas_regname(r:Tregister):string;


implementation

    uses
      cutils,verbose;

    const
    {$ifdef x86_64}
      att_regname_table : array[tregisterindex] of string[7] = (
        {r8664att.inc contains the AT&T name of each register.}
        {$i r8664att.inc}
      );

      att_regname_index : array[tregisterindex] of tregisterindex = (
        {r8664ari.inc contains an index which sorts att_regname_table by
         ATT name.}
        {$i r8664ari.inc}
      );
    {$else x86_64}
      att_regname_table : array[tregisterindex] of string[7] = (
        {r386att.inc contains the AT&T name of each register.}
        {$i r386att.inc}
      );

      att_regname_index : array[tregisterindex] of tregisterindex = (
        {r386ari.inc contains an index which sorts att_regname_table by
         ATT name.}
        {$i r386ari.inc}
      );
    {$endif x86_64}

    function findreg_by_attname(const s:string):byte;
      var
        i,p : tregisterindex;
      begin
        {Binary search.}
        p:=0;
        i:=regnumber_count_bsstart;
        repeat
          if (p+i<=high(tregisterindex)) and (att_regname_table[att_regname_index[p+i]]<=s) then
            p:=p+i;
          i:=i shr 1;
        until i=0;
        if att_regname_table[att_regname_index[p]]=s then
          findreg_by_attname:=att_regname_index[p]
        else
          findreg_by_attname:=0;
      end;


    function gas_regnum_search(const s:string):Tregister;
      begin
        result:=regnumber_table[findreg_by_attname(s)];
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
