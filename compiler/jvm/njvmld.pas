{
    Copyright (c) 2011 by Jonas Maebe

    Generate JVM assembler for nodes that handle loads and assignments

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
unit njvmld;

{$I fpcdefs.inc}

interface

uses
  globtype,
  symtype,
  cgutils,
  node, ncgld;

type
  tjvmloadnode = class(tcgloadnode)
    function is_addr_param_load: boolean; override;
  end;

  tjvmarrayconstructornode = class(tcgarrayconstructornode)
   protected
    procedure makearrayref(var ref: treference; eledef: tdef); override;
    procedure advancearrayoffset(var ref: treference; elesize: asizeint); override;
  end;

implementation

uses
  verbose,
  aasmdata,
  nld,
  symsym,symdef,jvmdef,
  cgbase,hlcgobj;

function tjvmloadnode.is_addr_param_load: boolean;
  begin
    result:=
      inherited and
      not jvmimplicitpointertype(tparavarsym(symtableentry).vardef);
  end;


{ tjvmarrayconstructornode }

procedure tjvmarrayconstructornode.makearrayref(var ref: treference; eledef: tdef);
  var
    basereg: tregister;
  begin
    { arrays are implicitly dereferenced }
    basereg:=hlcg.getaddressregister(current_asmdata.CurrAsmList,java_jlobject);
    hlcg.a_load_ref_reg(current_asmdata.CurrAsmList,java_jlobject,java_jlobject,ref,basereg);
    reference_reset_base(ref,basereg,0,1);
    ref.arrayreftype:=art_indexconst;
    ref.indexoffset:=0;
  end;


procedure tjvmarrayconstructornode.advancearrayoffset(var ref: treference; elesize: asizeint);
  begin
    inc(ref.indexoffset);
  end;


begin
  cloadnode:=tjvmloadnode;
  carrayconstructornode:=tjvmarrayconstructornode;
end.

