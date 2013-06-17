{
    Copyright (c) 1998-2006 by Florian Klaempfl

    This unit contains utility functions for assembler output

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
unit asmutils;

interface

{$i fpcdefs.inc}

uses
  globtype,
  aasmbase,
  aasmdata,
  symconst;

    type
      tasmlabofs = record
        lab: tasmlabel;
        ofs: pint;
      end;

    function emit_ansistring_const(list:TAsmList;data:PChar;len:LongInt;encoding:tstringencoding;NewSection:Boolean=True):tasmlabofs;
    function emit_unicodestring_const(list:TAsmList;data:Pointer;encoding:tstringencoding;Winlike:Boolean):tasmlabofs;

    function get_string_symofs(typ: tstringtype; winlikewidestring: boolean): pint;


implementation

uses
  globals,
  systems,
  verbose,
  aasmtai,
  widestr,
  symdef;

    function emit_ansistring_const(list:TAsmList;data:PChar;len:LongInt;encoding:tstringencoding;NewSection:Boolean): tasmlabofs;
      var
        s: PChar;
      begin
        current_asmdata.getdatalabel(result.lab);
        result.ofs:=0;
        if NewSection then
          begin
            maybe_new_object_file(list);
            new_section(list,sec_rodata_norel,result.lab.name,const_align(sizeof(pint)));
          end;
        { put label before header on Darwin, because there the linker considers
          a global symbol to be the start of a new subsection }
        if target_info.system in systems_darwin then
          list.concat(tai_label.create(result.lab));
        list.concat(tai_const.create_16bit(encoding));
        inc(result.ofs,2);
        list.concat(tai_const.create_16bit(1));
        inc(result.ofs,2);
{$ifdef cpu64bitaddr}
        { dummy for alignment }
        list.concat(tai_const.create_32bit(0));
        inc(result.ofs,4);
{$endif cpu64bitaddr}
        list.concat(tai_const.create_pint(-1));
        inc(result.ofs,sizeof(pint));
        list.concat(tai_const.create_pint(len));
        inc(result.ofs,sizeof(pint));
        if not(target_info.system in systems_darwin) then
          begin
            { results in slightly more efficient code }
            list.concat(tai_label.create(result.lab));
            result.ofs:=0;
          end;
        { sanity check }
        if result.ofs<>get_string_symofs(st_ansistring,false) then
          internalerror(2012051701);

        getmem(s,len+1);
        move(data^,s^,len);
        s[len]:=#0;
        list.concat(tai_string.create_pchar(s,len+1)); { terminating zero included }
      end;


    function emit_unicodestring_const(list:TAsmList;data:Pointer;encoding:tstringencoding;Winlike:Boolean):tasmlabofs;
      var
        i, strlength: SizeInt;
      begin
        current_asmdata.getdatalabel(result.lab);
        result.ofs:=0;
        maybe_new_object_file(list);
        new_section(list,sec_rodata_norel,result.lab.name,const_align(sizeof(pint)));
        strlength := getlengthwidestring(pcompilerwidestring(data));
        if Winlike then
          begin
            list.concat(Tai_const.Create_32bit(strlength*cwidechartype.size));
            { don't increase result.ofs, this is how Windows widestrings are
              defined by the OS: a pointer 4 bytes past the length of the
              string }
            list.concat(Tai_label.Create(result.lab));
          end
        else
          begin
            { put label before header on Darwin, because there the linker considers
              a global symbol to be the start of a new subsection }
            if target_info.system in systems_darwin then
              list.concat(Tai_label.Create(result.lab));
            list.concat(tai_const.create_16bit(encoding));
            inc(result.ofs,2);
            list.concat(tai_const.create_16bit(2));
            inc(result.ofs,2);
    {$ifdef cpu64bitaddr}
            { dummy for alignment }
            list.concat(Tai_const.Create_32bit(0));
            inc(result.ofs,4);
    {$endif cpu64bitaddr}
            list.concat(Tai_const.Create_pint(-1));
            inc(result.ofs,sizeof(pint));
            list.concat(Tai_const.Create_pint(strlength));
            inc(result.ofs,sizeof(pint));
            if not(target_info.system in systems_darwin) then
              begin
                { results in slightly more efficient code }
                list.concat(tai_label.create(result.lab));
                result.ofs:=0;
              end;
            { sanity check }
            if result.ofs<>get_string_symofs(st_unicodestring,false) then
              internalerror(2012051702);
          end;
        if cwidechartype.size = 2 then
          begin
            for i:=0 to strlength-1 do
              list.concat(Tai_const.Create_16bit(pcompilerwidestring(data)^.data[i]));
            { ending #0 }
            list.concat(Tai_const.Create_16bit(0));
          end
        else
          InternalError(200904271); { codegeneration for other sizes must be written }
      end;


    function get_string_symofs(typ: tstringtype; winlikewidestring: boolean): pint;
      const
        ansistring_header_size =
          { encoding }
          2 +
          { elesize }
          2 +
{$ifdef cpu64bitaddr}
          { alignment }
          4 +
{$endif cpu64bitaddr}
          { reference count }
          sizeof(pint) +
          { length }
          sizeof(pint);
        unicodestring_header_size = ansistring_header_size;
      begin
        if not(target_info.system in systems_darwin) then
          result:=0
        else case typ of
          st_ansistring:
            result:=ansistring_header_size;
          st_unicodestring:
            result:=unicodestring_header_size;
          st_widestring:
            if winlikewidestring then
              result:=0
            else
              result:=unicodestring_header_size;
          else
            result:=0;
        end;
      end;


end.
