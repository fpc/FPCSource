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
unit itx86int;

{$i fpcdefs.inc}

interface

    uses
      cgbase;

    function masm_regnum_search(const s:string):Tregister;
    function masm_regname(r:Tregister):string;


implementation

    uses
      cutils,verbose,
      cpubase;

    const
    {$if defined(x86_64)}
      int_regname_table : array[tregisterindex] of string[7] = (
        {$i r8664int.inc}
      );

      int_regname_index : array[tregisterindex] of tregisterindex = (
        {$i r8664iri.inc}
      );
    {$elseif defined(i386)}
      int_regname_table : array[tregisterindex] of string[7] = (
        {$i r386int.inc}
      );

      int_regname_index : array[tregisterindex] of tregisterindex = (
        {$i r386iri.inc}
      );
    {$elseif defined(i8086)}
      int_regname_table : array[tregisterindex] of string[7] = (
        {$i r8086int.inc}
      );

      int_regname_index : array[tregisterindex] of tregisterindex = (
        {$i r8086iri.inc}
      );
    {$endif}


    function findreg_by_intname(const s:string):integer;
      var
        l,r,m: integer;
      begin
        {Binary search.}
        l := 0;
        r := high(tregisterindex) + 1;
        while l < r do
          begin
            m := (l + r) div 2;
            if int_regname_table[int_regname_index[m]] < s then l := m + 1
            else r := m;
          end;

        if (r<=high(tregisterindex)) and (int_regname_table[int_regname_index[r]]=s) then
          findreg_by_intname:=int_regname_index[r]
        else
          findreg_by_intname:=0;
      end;


    function masm_regnum_search(const s:string):Tregister;
      begin
        result:=regnumber_table[findreg_by_intname(s)];
      end;


    function masm_regname(r:Tregister):string;
      var
        p : tregisterindex;
      begin
        p:=findreg_by_number(r);
        if p<>0 then
          result:=int_regname_table[p]
        else
          result:=generic_regname(r);
      end;

end.
