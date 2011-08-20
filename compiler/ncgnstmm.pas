{
    Copyright (c) 1998-2002 by Florian Klaempfl


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
unit ncgnstmm;

{$i fpcdefs.inc}

interface

    uses
      globtype,cgbase,cpuinfo,cpubase,
      node,ncgmem;

    type
       tcgnestloadparentfpnode = class(tcgloadparentfpnode)
          function pass_typecheck: tnode; override;
          function pass_1: tnode; override;
          procedure pass_generate_code;override;
       end;

implementation

    uses
      systems,
      cutils,cclasses,verbose,globals,constexp,
      symconst,symdef,symsym,symtable,symcreat,defutil,paramgr,
      aasmbase,aasmtai,aasmdata,
      procinfo,pass_2,parabase,
      pass_1,ncnv,nmem,nld,ncon,nadd,nutils,
      cgutils,cgobj,hlcgobj,
      tgobj,ncgutil,objcgutl
      ;


{*****************************************************************************
                        TCGLOADPARENTFPNODE
*****************************************************************************}

    function tcgnestloadparentfpnode.pass_typecheck: tnode;
      var
        hsym        : tparavarsym;
        currpi,
        nextpi      : tprocinfo;
      begin
        result:=inherited;
        if assigned(result) then
          exit;
        currpi:=current_procinfo.parent;
        while (currpi.procdef.parast.symtablelevel>=parentpd.parast.symtablelevel) do
          begin
            if not assigned(currpi.procdef.parentfpstruct) then
              build_parentfpstruct(currpi.procdef);
            currpi:=currpi.parent;
          end;
        { mark all parent parentfp parameters for inclusion in the struct that
          holds all locals accessed from nested routines }
        currpi:=current_procinfo.parent;
        nextpi:=currpi.parent;
        while (currpi.procdef.parast.symtablelevel>parentpd.parast.symtablelevel) do
          begin
            hsym:=tparavarsym(currpi.procdef.parast.Find('parentfp'));
            maybe_add_sym_to_parentfpstruct(currpi.procdef,hsym,nextpi.procdef.parentfpstructptrtype,false);
            currpi:=nextpi;
            nextpi:=nextpi.parent;
          end;
      end;


    function tcgnestloadparentfpnode.pass_1: tnode;
      var
        fsym        : tfieldvarsym;
        hsym        : tparavarsym;
        currpi,
        nextpi      : tprocinfo;
      begin
        result:=nil;
        if not assigned(current_procinfo.procdef.parentfpstruct) then
          begin
            hsym:=tparavarsym(current_procinfo.procdef.parast.Find('parentfp'));
            result:=cloadnode.create(hsym,hsym.owner);
          end
        else
          begin
            result:=caddrnode.create_internal(cloadnode.create(current_procinfo.procdef.parentfpstruct,current_procinfo.procdef.parentfpstruct.owner));
            include(result.flags,nf_typedaddr);
          end;
        { mark all parent parentfp parameters for inclusion in the struct that
          holds all locals accessed from nested routines }
        currpi:=current_procinfo.parent;
        nextpi:=currpi.parent;
        while (currpi.procdef.parast.symtablelevel>parentpd.parast.symtablelevel) do
          begin
            hsym:=tparavarsym(currpi.procdef.parast.Find('parentfp'));
            fsym:=tfieldvarsym(find_sym_in_parentfpstruct(currpi.procdef,hsym));
            if not assigned(fsym) then
              internalerror(2011060405);
            result:=csubscriptnode.create(fsym,cderefnode.create(result));
            currpi:=nextpi;
            nextpi:=nextpi.parent;
          end;
      end;


    procedure tcgnestloadparentfpnode.pass_generate_code;
      begin
        { should be handled in pass 1 }
        internalerror(2011060202);
      end;


begin
   cloadparentfpnode:=tcgnestloadparentfpnode;
end.
