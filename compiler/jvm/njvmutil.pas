{
    Copyright (c) 20011 by Jonas Maebe

    JVM version of some node tree helper routines

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
unit njvmutil;

{$i fpcdefs.inc}

interface

  uses
    node,
    ngenutil,
    symsym;


  type
    tjvmnodeutils = class(tnodeutils)
      class function initialize_data_node(p:tnode):tnode; override;
      class function finalize_data_node(p:tnode):tnode; override;
      class function force_init: boolean; override;
      class procedure insertbssdata(sym: tstaticvarsym); override;
    end;


implementation

    uses
      verbose,constexp,
      aasmdata,aasmtai,
      symconst,symtype,symdef,symbase,symtable,defutil,jvmdef,
      nbas,ncnv,ncon,ninl,ncal,
      pass_1;

  class function tjvmnodeutils.initialize_data_node(p:tnode):tnode;
    begin
      if not assigned(p.resultdef) then
        typecheckpass(p);
      if ((p.resultdef.typ=stringdef) and
          not is_shortstring(p.resultdef) and
          not is_longstring(p.resultdef)) or
         is_dynamic_array(p.resultdef) then
        begin
          { Always initialise with empty string/array rather than nil. Java
            makes a distinction between an empty string/array and a null
            string/array,  but we don't. We therefore have to pick which one we
            use to represent empty strings/arrays. I've chosen empty rather than
            null structures, because otherwise it becomes impossible to return
            an empty string to Java code (it would return null).

            On the consumer side, we do interpret both null and empty as the same
            thing, so Java code can pass in null strings/arrays and we'll
            interpret them correctly.
          }
          result:=cinlinenode.create(in_setlength_x,false,
            ccallparanode.create(genintconstnode(0),
              ccallparanode.create(p,nil)));
        end
      else
        { records/arrays/... are automatically initialised }
        result:=cnothingnode.create;
    end;


  class function tjvmnodeutils.finalize_data_node(p:tnode):tnode;
    begin
      // do nothing
      result:=cnothingnode.create;
    end;


  class function tjvmnodeutils.force_init: boolean;
    begin
      { we need an initialisation in case the al_globals list is not empty
        (that's where the initialisation for global records is added) }
      result:=not current_asmdata.asmlists[al_globals].empty;
    end;

  class procedure tjvmnodeutils.insertbssdata(sym: tstaticvarsym);
    begin
      { handled while generating the unit/program init code, or class
        constructor; add something to al_globals to indicate that we need to
        insert an init section though }
      if current_asmdata.asmlists[al_globals].empty and
         jvmimplicitpointertype(sym.vardef) then
        current_asmdata.asmlists[al_globals].concat(cai_align.Create(1));
    end;

begin
  cnodeutils:=tjvmnodeutils;
end.

