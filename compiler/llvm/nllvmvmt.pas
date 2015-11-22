{
    Copyright (c) 2015 by Jonas Maebe

    Generate LLVM IR for VMT generation

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
unit nllvmvmt;

{$i fpcdefs.inc}

interface

    uses
      aasmdata,
      ncgvmt,
      symdef;

    type
      tllvmvmtwriter = class(TVMTWriter)
      protected
        procedure generate_abstract_stub(list:TAsmList;pd:tprocdef); override;
      end;


implementation

  uses
    globtype,globals,
    aasmbase,
    symconst,
    hlcgobj;


{*****************************************************************************
                             TLLVMVMTWRITER
*****************************************************************************}


  procedure tllvmvmtwriter.generate_abstract_stub(list: TAsmList; pd: tprocdef);
    var
      sym: TAsmSymbol;
    begin
      if (po_global in pd.procoptions) and
         (pd.owner.defowner<>self._class) then
        exit;
      sym:=current_asmdata.GetAsmSymbol(pd.mangledname);
      if assigned(sym) and (sym.bind<>AB_EXTERNAL) then
        exit;
      hlcg.g_external_wrapper(list,pd,'FPC_ABSTRACTERROR');
    end;

begin
  CVMTWriter:=tllvmvmtwriter;
end.
