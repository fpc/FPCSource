{
    Copyright (c) 1998-2022 by Florian Klaempfl and Nikolay Nikolov

    Generate WebAssembly code for load nodes

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

 ****************************************************************************}
unit nwasmld;

{$i fpcdefs.inc}

interface

    uses
      nld,ncgld,
      symsym;

    type

      { twasmloadnode }

      twasmloadnode = class(tcgloadnode)
      protected
        procedure generate_threadvar_access(gvs: tstaticvarsym); override;
      end;

implementation

    uses
      globtype,globals,
      aasmbase,aasmdata,
      cgbase,cgutils,
      symconst;

    { twasmloadnode }

    procedure twasmloadnode.generate_threadvar_access(gvs: tstaticvarsym);
      begin
        if ts_wasm_threads in current_settings.targetswitches then
          begin
            if not(vo_is_weak_external in gvs.varoptions) then
              reference_reset_symbol(location.reference,current_asmdata.RefAsmSymbol(gvs.mangledname,AT_TLS,use_indirect_symbol(gvs)),0,location.reference.alignment,[])
            else
              reference_reset_symbol(location.reference,current_asmdata.WeakRefAsmSymbol(gvs.mangledname,AT_TLS),0,location.reference.alignment,[]);
            location.reference.refaddr:=addr_got_tls;
          end
        else
          inherited generate_threadvar_access(gvs);
      end;

begin
  cloadnode:=twasmloadnode;
end.
