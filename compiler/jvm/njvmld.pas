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
    procedure pass_generate_code; override;
  end;

  tjvmassignmentnode  = class(tcgassignmentnode)
    function pass_1: tnode; override;
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
  nbas,nld,ncal,nmem,ncnv,
  symconst,symsym,symdef,symtable,defutil,jvmdef,
  paramgr,
  cgbase,hlcgobj;

{ tjvmassignmentnode }

function tjvmassignmentnode.pass_1: tnode;
  var
    target: tnode;
    psym: tsym;
  begin
    { intercept writes to string elements, because Java strings are immutable
      -> detour via StringBuilder
    }
    target:=left.actualtargetnode;
    if (target.nodetype=vecn) and
       (is_wide_or_unicode_string(tvecnode(target).left.resultdef) or
        is_ansistring(tvecnode(target).left.resultdef)) then
      begin
        { prevent errors in case of an expression such as
            word(str[x]):=1234;
        }
        inserttypeconv_explicit(right,cwidechartype);
        result:=ccallnode.createintern('fpc_'+tstringdef(tvecnode(target).left.resultdef).stringtypname+'_setchar',
          ccallparanode.create(right,
            ccallparanode.create(tvecnode(target).right,
              ccallparanode.create(tvecnode(target).left.getcopy,nil))));
        result:=cassignmentnode.create(tvecnode(target).left,result);
        right:=nil;
        tvecnode(target).left:=nil;
        tvecnode(target).right:=nil;
        exit;
      end
    else if (target.nodetype=vecn) and
       is_shortstring(tvecnode(target).left.resultdef) then
      begin
        { prevent errors in case of an expression such as
            byte(str[x]):=12;
        }
        inserttypeconv_explicit(right,cchartype);
        { call ShortstringClass(shortstring).setChar(index,char) }
        inserttypeconv_explicit(tvecnode(target).left,java_shortstring);
        psym:=search_struct_member(tabstractrecorddef(java_shortstring),'SETCHAR');
        if not assigned(psym) or
           (psym.typ<>procsym) then
          internalerror(2011052408);
        result:=
          ccallnode.create(
            ccallparanode.create(right,
              ccallparanode.create(tvecnode(target).right,nil)),
            tprocsym(psym),psym.owner,tvecnode(target).left,[]);
        right:=nil;
        tvecnode(target).left:=nil;
        tvecnode(target).right:=nil;
        exit;
      end
    else
      result:=inherited;
  end;


function tjvmloadnode.is_addr_param_load: boolean;
  begin
    result:=
      inherited and
      not jvmimplicitpointertype(tparavarsym(symtableentry).vardef);
  end;


procedure tjvmloadnode.pass_generate_code;
  begin
    if (symtable.symtabletype=parasymtable) and
       (symtableentry.typ=paravarsym) and
       paramanager.push_copyout_param(tparavarsym(symtableentry).varspez,resultdef,tprocdef(symtable.defowner).proccalloption) then
      begin
        { the parameter is passed as an array of one element containing the
          parameter value }
        location_reset_ref(location,LOC_REFERENCE,def_cgsize(resultdef),4);
        location.reference.arrayreftype:=art_indexconst;
        location.reference.base:=hlcg.getaddressregister(current_asmdata.CurrAsmList,java_jlobject);
        hlcg.a_load_loc_reg(current_asmdata.CurrAsmList,java_jlobject,java_jlobject,tparavarsym(symtableentry).localloc,location.reference.base);
        location.reference.indexoffset:=0;
      end
    else
      inherited pass_generate_code;
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
  cassignmentnode:=tjvmassignmentnode;
  carrayconstructornode:=tjvmarrayconstructornode;
end.

