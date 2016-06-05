{
    Copyright (c) 2011 by Jonas Maebe

    Support for load nodes on targets that have to group all local variables
    and parameters accessed by nested routines into structs (and then pass the
    address of these structs to nested routines rather than the frame pointer,
    and access the local variables as fields thereof)

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
unit ncgnstld;

{$i fpcdefs.inc}

interface

    uses
       node,
       symtype,
       nld,
       ncgld;

    type
       tcgnestloadnode = class(tcgloadnode)
        protected
         nestsym: tsym;
         nestsymderef: tderef;
         procedure generate_nested_access(vs: tsym);override;
         function  keep_param_address_in_nested_struct: boolean; virtual;
        public
         function  pass_typecheck: tnode; override;
         function  pass_1:tnode;override;
         function  dogetcopy: tnode; override;
         function  docompare(p: tnode): boolean; override;
         constructor ppuload(t: tnodetype; ppufile: tcompilerppufile); override;
         procedure ppuwrite(ppufile: tcompilerppufile); override;
         procedure buildderefimpl; override;
         procedure derefimpl; override;
       end;

implementation

    uses
      cutils,verbose,globtype,globals,systems,constexp,
      defutil,defcmp,
      htypechk,pass_1,procinfo,paramgr,
      cpuinfo,
      symconst,symbase,symsym,symdef,symtable,symcreat,
      ncon,ninl,ncnv,nmem,ncal,nutils,nbas,
      pass_2,cgbase
      ;

{*****************************************************************************
                          TCGNESTLOADNODE
*****************************************************************************}

    procedure tcgnestloadnode.generate_nested_access(vs: tsym);
      begin
        { left has been transformed into a string of accesses that result in
          the location of the original variable's copy in the appropriate
          parentfpstruct (via tcgnestloadparentfpnode.pass_1). In case it is a
          var/out/constref parameter, that "copy" will have been a copy of the
          address so the normal handling of such parameters in ncgld is ok) }
        secondpass(left);
        location:=left.location;
      end;


    function tcgnestloadnode.keep_param_address_in_nested_struct: boolean;
      begin
        result:=is_addr_param_load;
      end;


    function tcgnestloadnode.pass_typecheck: tnode;
      var
        nestedvars: tsym;
      begin
        result:=inherited pass_typecheck;
        if assigned(result) then
          exit;
        case symtableentry.typ of
          paravarsym,
          localvarsym :
            begin
              { Nested variable? Then we have to move it to a structure that
                can be passed by reference to nested routines }
              if assigned(current_procinfo) and
                 (symtable.symtabletype in [localsymtable,parasymtable]) and
                 ((symtable.symtablelevel<>current_procinfo.procdef.parast.symtablelevel) or
                  { also redirect loads of locals/paras that have been moved to
                     the parentfpstruct inside the routine in which they were
                     originally declared, except in the initialisation code for
                     the parentfpstruct (nf_internal flag) }
                  (tabstractnormalvarsym(symtableentry).inparentfpstruct and
                   not(nf_internal in flags))) then
                begin
                  { get struct holding all locals accessed by nested routines }
                  nestedvars:=tprocdef(symtable.defowner).parentfpstruct;
                  { don't add the parentfpstruct to itself! }
                  if nestedvars=symtableentry then
                    exit;
                  if not assigned(nestedvars) then
                    begin
                      { create this struct }
                      build_parentfpstruct(tprocdef(symtable.defowner));
                      nestedvars:=tprocdef(symtable.defowner).parentfpstruct;
                    end;
                  {  store result for use in pass_1 }
                  nestsym:=maybe_add_sym_to_parentfpstruct(tprocdef(symtableentry.owner.defowner),symtableentry,resultdef,keep_param_address_in_nested_struct);
                  { left normally holds the parentfp node. If it's not assigned,
                    this is an access to a local variable/para from the routine
                    in which it was actually declared -> redirect to its
                    equivalent in the parentfp struct }
                  if not assigned(left) then
                    begin
                      left:=caddrnode.create_internal(cloadnode.create(tprocdef(symtableentry.owner.defowner).parentfpstruct,tprocdef(symtableentry.owner.defowner).parentfpstruct.owner));
                      include(left.flags,nf_typedaddr);
                    end;
                  typecheckpass(left);
                end;
            end;
        end;
      end;


    function tcgnestloadnode.pass_1:tnode;
      var
        thissym,
        nestedvars: tsym;
        nestedvarsdef: tdef;
      begin
        result:=inherited;
        if assigned(result) then
          exit;
        case symtableentry.typ of
          paravarsym,
          localvarsym :
            begin
              { Nested variable? Then we have to move it to a structure that
                can be passed by reference to nested routines }
              if assigned(current_procinfo) and
                 (symtable.symtabletype in [localsymtable,parasymtable]) and
                 ((symtable.symtablelevel<>current_procinfo.procdef.parast.symtablelevel) or
                  (tabstractnormalvarsym(symtableentry).inparentfpstruct and
                   not(nf_internal in flags))) then
                begin
                  { get struct holding all locals accessed by nested routines }
                  nestedvars:=tprocdef(symtable.defowner).parentfpstruct;
                  if not assigned(nestedvars) then
                    begin
                      { create this struct }
                      build_parentfpstruct(tprocdef(symtable.defowner));
                      nestedvars:=tprocdef(symtable.defowner).parentfpstruct;
                    end;
                  nestedvarsdef:=tlocalvarsym(nestedvars).vardef;
                  if nestedvars<>symtableentry then
                    thissym:=nestsym
                  else
                    thissym:=find_sym_in_parentfpstruct(tprocdef(symtableentry.owner.defowner),symtableentry);
                  if not assigned(thissym) then
                    internalerror(2011060406);
                  { firstpass the parentfpnode. This will transform it into
                    a load of the appropriate parentfpstruct }
                  if not assigned(left) then
                    internalerror(2011060104);
                  firstpass(left);
                  { subscript it to get the variable }
                  left:=csubscriptnode.create(thissym,cderefnode.create(left));
                  firstpass(left);
                 end;
            end;
        end;
      end;


    function tcgnestloadnode.dogetcopy: tnode;
      begin
        result:=inherited dogetcopy;
        tcgnestloadnode(result).nestsym:=nestsym;
      end;


    function tcgnestloadnode.docompare(p: tnode): boolean;
      begin
        result:=
          inherited docompare(p) and
          (tcgnestloadnode(p).nestsym=nestsym);
      end;


    constructor tcgnestloadnode.ppuload(t: tnodetype; ppufile: tcompilerppufile);
      begin
        inherited ppuload(t, ppufile);
        ppufile.getderef(nestsymderef);
      end;


    procedure tcgnestloadnode.ppuwrite(ppufile: tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.putderef(nestsymderef);
      end;


    procedure tcgnestloadnode.buildderefimpl;
      begin
        inherited buildderefimpl;
        nestsymderef.build(nestsym);
      end;


    procedure tcgnestloadnode.derefimpl;
      begin
        inherited derefimpl;
        nestsym:=tsym(nestsymderef.resolve);
      end;


begin
  cloadnode:=tcgnestloadnode;
end.
