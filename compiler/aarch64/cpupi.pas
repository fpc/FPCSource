{
    Copyright (c) 2002 by Florian Klaempfl

    This unit contains the CPU specific part of tprocinfo

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
unit cpupi;

{$i fpcdefs.inc}

interface

  uses
    procinfo,
    psub,
    aasmdata,aasmbase;

  type
    tcpuprocinfo=class(tcgprocinfo)
    private
      scopes: TAsmList;
      scopecount: longint;
      unwindflags: byte;
    public
      constructor create(aparent: tprocinfo); override;
      destructor destroy; override;
      procedure set_first_temp_offset; override;
      procedure add_finally_scope(startlabel,endlabel,handler:TAsmSymbol;implicit:Boolean);
      procedure add_except_scope(trylabel,exceptlabel,endlabel,filter:TAsmSymbol);
      procedure dump_scopes(list:tasmlist);
    end;

implementation

  uses
    cutils,
    fmodule,
    symtable,
    tgobj,
    cpubase,
    aasmtai;

  const
    SCOPE_FINALLY=0;
    SCOPE_CATCHALL=1;
    SCOPE_IMPLICIT=2;

  constructor tcpuprocinfo.create(aparent: tprocinfo);
    begin
      inherited;
      { use the stack pointer as framepointer, because
         1) we exactly know the offsets of the temps from the stack pointer
            after pass 1 (based on the require parameter stack size for called
            routines), while we don't know it for the frame pointer (it depends
            on the number of saved registers)
         2) temp offsets from the stack pointer are positive while those from
            the frame pointer are negative, and we can directly encode much
            bigger positive offsets in the instructions
      }
      framepointer:=NR_STACK_POINTER_REG;
    end;

  destructor tcpuprocinfo.destroy;
    begin
      scopes.free;
      inherited destroy;
    end;

  procedure tcpuprocinfo.set_first_temp_offset;
    begin
     { leave room for allocated parameters }
     tg.setfirsttemp(align(maxpushedparasize,16));
    end;

  procedure tcpuprocinfo.add_finally_scope(startlabel,endlabel,handler:TAsmSymbol;implicit:Boolean);
    begin
      unwindflags:=unwindflags or 2;
      if implicit then  { also needs catch functionality }
        unwindflags:=unwindflags or 1;
      inc(scopecount);
      if scopes=nil then
        scopes:=TAsmList.Create;

      if implicit then
        scopes.concat(tai_const.create_32bit(SCOPE_IMPLICIT))
      else
        scopes.concat(tai_const.create_32bit(SCOPE_FINALLY));
      scopes.concat(tai_const.create_rva_sym(startlabel));
      scopes.concat(tai_const.create_rva_sym(endlabel));
      scopes.concat(tai_const.create_rva_sym(handler));
    end;

  procedure tcpuprocinfo.add_except_scope(trylabel,exceptlabel,endlabel,filter:TAsmSymbol);
    begin
      unwindflags:=unwindflags or 3;
      inc(scopecount);
      if scopes=nil then
        scopes:=TAsmList.Create;

      if Assigned(filter) then
        scopes.concat(tai_const.create_rva_sym(filter))
      else
        scopes.concat(tai_const.create_32bit(SCOPE_CATCHALL));
      scopes.concat(tai_const.create_rva_sym(trylabel));
      scopes.concat(tai_const.create_rva_sym(exceptlabel));
      scopes.concat(tai_const.create_rva_sym(endlabel));
    end;

  procedure tcpuprocinfo.dump_scopes(list: tasmlist);
    var
      hdir: tai_seh_directive;
    begin
      if (scopecount=0) then
        exit;
      hdir:=cai_seh_directive.create_name(ash_handler,'__FPC_specific_handler');
      if not systemunit.iscurrentunit then
        current_module.add_extern_asmsym('__FPC_specific_handler',AB_EXTERNAL,AT_FUNCTION);
      hdir.data.flags:=unwindflags;
      list.concat(hdir);
      list.concat(cai_seh_directive.create(ash_handlerdata));
      inc(list.section_count);
      list.concat(tai_const.create_32bit(scopecount));
      list.concatlist(scopes);
      { return to text, required for GAS compatibility }
      { This creates a tai_align which is redundant here (although harmless) }
      new_section(list,sec_code,lower(procdef.mangledname),0);
    end;


begin
  cprocinfo:=tcpuprocinfo;
end.
