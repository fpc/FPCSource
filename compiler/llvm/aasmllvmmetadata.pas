{
    Copyright (c) 2019 by Jonas Maebe,
    member of the Free Pascal Compiler development team

    Support for LLVM metadata

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
unit aasmllvmmetadata;

{$i fpcdefs.inc}

interface

  uses
    aasmtai, aasmcnst,
    symtype;

  type
    tspecialisedmetadatanodekind = (
      smeta_DIFile,
      smeta_DIBasicType,
      smeta_DISubroutineType,
      smeta_DIDerivedType,
      smeta_DICompositeType,
      smeta_DISubrange,
      smeta_DIEnumerator,
      smeta_DITemplateTypeParameter,
      smeta_DITemplateValueParameter,
      smeta_DINamespace,
      smeta_DIGlobalVariable,
      smeta_DISubprogram,
      smeta_DILexicalBlock,
      smeta_DILexicalBlockFile,
      smeta_DILocation,
      smeta_DILocalVariable,
      smeta_DIExpression,
      smeta_DIObjCProperty,
      smeta_DIImportedEntity,
      smeta_DIMacro,
      smeta_DIMacroFile
    );

    tai_llvmbasemetadatanode = class abstract(tai_aggregatetypedconst)
     strict protected
      function getname: ansistring; virtual; abstract;
     public
      procedure addvalue(val: tai_abstracttypedconst); override;
      property name: ansistring read getname;
      constructor create; reintroduce;
    end;

    (* !0 = !{ type1 value1, ... } *)
    tai_llvmunnamedmetadatanode = class(tai_llvmbasemetadatanode)
     strict private class var
      snextid: cardinal;
      class function getnextid: cardinal;
     strict protected
      fnameval: cardinal;
     public
      constructor create; reintroduce;
      function getname: ansistring; override;
    end;

    (* !name = !{ type1 value1, ... } *)
    tai_llvmnamedmetadatanode = class(tai_llvmbasemetadatanode)
     strict protected
      fname: ansistring;
      function getname: ansistring; override;
     public
      constructor create(const aName: ansistring);
    end;

    tai_llvmmetadatareftypedconst = class(tai_simple)
     strict private
      fval: tai_llvmbasemetadatanode;
     public
      constructor create(_val: tai_llvmbasemetadatanode);
      property val: tai_llvmbasemetadatanode read fval;
    end;

    { @g1 = global i32 0, *!id !value.name* }
    tai_llvmmetadatareferenceoperand = class(tai_simple)
     strict private
      fid: ansistring;
      fvalue: tai_llvmbasemetadatanode;
     public
      constructor create(const anID: ansistring; aValue: tai_llvmbasemetadatanode);
      property id: ansistring read fid;
      property value: tai_llvmbasemetadatanode read fvalue;
    end;

      { !name = !kindname(field1: value1, ...) }
    tai_llvmspecialisedmetadatanode = class(tai_llvmunnamedmetadatanode)
      { identifies name and fieldnames }
      kind: tspecialisedmetadatanodekind;
    end;

    function llvm_getmetadatareftypedconst(metadata: tai_llvmbasemetadatanode): tai_simpletypedconst;

implementation

  uses
    symdef;

  function llvm_getmetadatareftypedconst(metadata: tai_llvmbasemetadatanode): tai_simpletypedconst;
    begin
      result:=tai_simpletypedconst.create(llvm_metadatatype, tai_llvmmetadatareftypedconst.create(metadata));
    end;

  procedure tai_llvmbasemetadatanode.addvalue(val: tai_abstracttypedconst);
    begin
      { bypass string merging attempts, as we add tai_strings directly here }
      fvalues.add(val);
    end;

  constructor tai_llvmbasemetadatanode.create;
    begin
      inherited create(tck_array, llvm_metadatatype);
      typ:=ait_llvmmetadatanode;
    end;


  class function tai_llvmunnamedmetadatanode.getnextid: cardinal;
    begin
      result:=snextid;
      inc(snextid);
    end;


  function tai_llvmunnamedmetadatanode.getname: ansistring;
    begin
      str(fnameval,result);
    end;


  constructor tai_llvmunnamedmetadatanode.create;
    begin
      inherited;
      fnameval:=getnextid;
    end;


  function tai_llvmnamedmetadatanode.getname: ansistring;
    begin
      result:=fname;
    end;


  constructor tai_llvmnamedmetadatanode.create(const aName: ansistring);
    begin
      inherited create;
      fname:=aName;
    end;


  constructor tai_llvmmetadatareftypedconst.create(_val: tai_llvmbasemetadatanode);
    begin
      inherited create(ait_llvmmetadatareftypedconst);
      fval:=_val;
    end;


  constructor tai_llvmmetadatareferenceoperand.create(const anID: ansistring; aValue: tai_llvmbasemetadatanode);
    begin
      inherited create(ait_llvmmetadatarefoperand);
      fid:=anID;
      fvalue:=aValue;
    end;


end.

