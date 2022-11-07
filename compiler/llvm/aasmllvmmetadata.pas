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
    globtype,cclasses,
    cgbase,
    aasmtai,aasmcnst,
    symtype;

{$push}{$ScopedEnums on}
  type
    tspecialisedmetadatanodekind = (
      DICompileUnit,
      DIFile,
      DIBasicType,
      DISubroutineType,
      DIDerivedType,
      DICompositeType,
      DISubrange,
      DIEnumerator,
      DITemplateTypeParameter,
      DITemplateValueParameter,
      DINamespace,
      DIGlobalVariable,
      DIGlobalVariableExpression,
      DISubprogram,
      DILexicalBlock,
      DILexicalBlockFile,
      DILocation,
      DILocalVariable,
      DIExpression,
      DIObjCProperty,
      DIImportedEntity,
      DIMacro,
      DIMacroFile
    );
{$pop}

//   represented by a tai_simpletypedconst() with inside a metadata struct,
//   or as a metadata register (for parameters)
//    tai_llvmmetadatastring = class

    tai_llvmbasemetadatanode = class abstract(tai_aggregatetypedconst)
     strict protected
      function getname: ansistring; virtual; abstract;
     public
      procedure addvalue(val: tai_abstracttypedconst); override;
      property name: ansistring read getname;
      constructor create; reintroduce;
      class function isspecialised: boolean; virtual;
    end;

    (* !0 = !{ type1 value1, ... } *)
    tai_llvmunnamedmetadatanode = class(tai_llvmbasemetadatanode)
     strict private
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

    { reference to a metadata node inside an expression, i.e., !X }
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
      fvalue: tai_llvmmetadatareftypedconst;
     public
      constructor createreferenceto(const anID: ansistring; aValue: tai_llvmbasemetadatanode);
      destructor destroy; override;
      property id: ansistring read fid;
      property value: tai_llvmmetadatareftypedconst read fvalue;
    end;


    tllvmspecialmetaitemkind = (
      lsmik_boolean,
      lsmik_int64,
      lsmik_qword,
      lsmik_metadataref,
      lsmik_string,
      { difference with string: not quoted }
      lsmik_enum
    );
    tllvmspecialisedmetaitem = class(tai_simpletypedconst)
     private
      fitemkind: tllvmspecialmetaitemkind;
      fitemname: TSymStr;
     public
      constructor createboolean(const aitemname: TSymStr; boolval: boolean);
      constructor createint64(const aitemname: TSymStr; intval: int64);
      constructor createqword(const aitemname: TSymStr; qwval: qword);
      constructor createmetadataref(const aitemname: TSymStr; aival: tai_llvmmetadatareftypedconst);
      constructor createstring(const aitemname: TSymStr; const stringval: TSymStr);
      constructor createenum(const aitemname: TSymStr; const enumval: TSymStr);

      property itemname: TSymStr read fitemname;
      property itemkind: tllvmspecialmetaitemkind read fitemkind;
    end;

     { !name = !kindname(field1: value1, ...) }
    tai_llvmspecialisedmetadatanode = class(tai_llvmunnamedmetadatanode)
     strict private
      { identifies name and fieldnames }
      fkind: tspecialisedmetadatanodekind;
      { adds the item, appropriating its contents (so don't destroy the original
        afterwards) }
      procedure addemplaceitem(const item: tllvmspecialisedmetaitem);
     public
      constructor create(aKind: tspecialisedmetadatanodekind);
      procedure addvalue(val: tai_abstracttypedconst); override; deprecated 'use addboolean/addinteger/addmetadataref/addstring/addenum';
      procedure addboolean(const aitemname: TSymStr; boolval: boolean);
      procedure addint64(const aitemname: TSymStr; intval: int64);
      procedure addqword(const aitemname: TSymStr; qwval: qword);
      procedure addmetadatarefto(const aitemname: TSymStr; aival: tai_llvmbasemetadatanode);
      procedure addstring(const aitemname: TSymStr; const stringval: TSymStr);
      procedure addenum(const aitemname: TSymStr; const enumval: TSymStr);
      { only allowed before adding any items; needed because when generating
        debug info, we first generat everything as DIDerivedType (works for
        typedefs and most other types, and switch it later when necessary).
        Make it an explicit proc rather than a setter for kind to avoid
        accidental usage }
      procedure switchkind(newKind: tspecialisedmetadatanodekind);
      property kind: tspecialisedmetadatanodekind read fkind;

      function IsDistinct: boolean;
      class function isspecialised: boolean; override;
    end;

    {$push}
    {$scopedenums on}
    { not clear what the difference is between LineTablesOnly and DebugDirectivesOnly }
    tllvmdebugemissionkind = (NoDebug, FullDebug, LineTablesOnly, DebugDirectivesOnly);
    {$pop}

    tllvmmetadata = class
     strict private
       class function addstring(const s: TSymstr): TSuperRegister;
       class function regtostring(reg: TRegister): TSymStr;
     public

      class function getstringreg(const s: TSymstr): TRegister;
      class function getpcharreg(p: pchar; len: longint): TRegister;
      class function getregstring(reg: TRegister): TSymStr;
    end;

    function llvm_getmetadatareftypedconst(metadata: tai_llvmbasemetadatanode): tai_simpletypedconst;

    function llvm_constrainedexceptmodestring: ansistring;

implementation

  uses
    verbose,globals,cutils,
    fmodule,
    symdef,
    dbgdwarfconst,
    aasmdata,aasmllvm;

  function llvm_getmetadatareftypedconst(metadata: tai_llvmbasemetadatanode): tai_simpletypedconst;
    begin
      if assigned(metadata) then
        result:=tai_simpletypedconst.create(llvm_metadatatype, tai_llvmmetadatareftypedconst.create(metadata))
      else
        result:=nil
    end;

  function llvm_constrainedexceptmodestring: ansistring;
    begin
      if not(cs_opt_fastmath in current_settings.optimizerswitches) then
        result:='fpexcept.maytrap'
      else
        result:='fpexcept.ignore'
    end;


  /////////////////////////////////////////////////

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

  class function tai_llvmbasemetadatanode.isspecialised: boolean;
    begin
      result:=false;
    end;


  class function tai_llvmunnamedmetadatanode.getnextid: cardinal;
    begin
      result:=tllvmasmdata(current_asmdata).fnextmetaid;
      inc(tllvmasmdata(current_asmdata).fnextmetaid);
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


  constructor tai_llvmmetadatareferenceoperand.createreferenceto(const anID: ansistring; aValue: tai_llvmbasemetadatanode);
    begin
      inherited create(ait_llvmmetadatarefoperand);
      fid:=anID;
      fvalue:=tai_llvmmetadatareftypedconst.create(aValue);
    end;

  destructor tai_llvmmetadatareferenceoperand.destroy;
    begin
      fvalue.free;
      inherited;
    end;

  /////////////////////////////////////////////////

  constructor tllvmspecialisedmetaitem.createboolean(const aitemname: TSymStr; boolval: boolean);
    begin
      fitemname:=aitemname;
      fitemkind:=lsmik_boolean;
      inherited create(llvmbool1type,tai_const.Create_8bit(ord(boolval)));
    end;

  constructor tllvmspecialisedmetaitem.createint64(const aitemname: TSymStr; intval: int64);
    begin
      fitemname:=aitemname;
      fitemkind:=lsmik_int64;
      inherited create(llvmbool1type,tai_const.Create_64bit(intval));
    end;

  constructor tllvmspecialisedmetaitem.createqword(const aitemname: TSymStr; qwval: qword);
    begin
      fitemname:=aitemname;
      fitemkind:=lsmik_qword;
      inherited create(llvmbool1type,tai_const.Create_64bit(int64(qwval)));
    end;

  constructor tllvmspecialisedmetaitem.createmetadataref(const aitemname: TSymStr; aival: tai_llvmmetadatareftypedconst);
    begin
      fitemname:=aitemname;
      fitemkind:=lsmik_metadataref;
      inherited create(llvm_metadatatype,aival);
    end;

  constructor tllvmspecialisedmetaitem.createstring(const aitemname: TSymStr; const stringval: TSymStr);
    begin
      fitemname:=aitemname;
      fitemkind:=lsmik_string;
      inherited create(charpointertype,tai_string.Create(stringval));
    end;

  constructor tllvmspecialisedmetaitem.createenum(const aitemname: TSymStr; const enumval: TSymStr);
    begin
      fitemname:=aitemname;
      fitemkind:=lsmik_enum;
      inherited create(charpointertype,tai_string.Create(enumval));
    end;

  constructor tai_llvmspecialisedmetadatanode.create(aKind: tspecialisedmetadatanodekind);
    begin
      inherited create;
      fkind:=aKind;
    end;

  procedure tai_llvmspecialisedmetadatanode.addemplaceitem(const item: tllvmspecialisedmetaitem);
    begin
      inherited addvalue(item);
    end;

  procedure tai_llvmspecialisedmetadatanode.switchkind(newKind: tspecialisedmetadatanodekind);
    begin
      if fvalues.Count<>0 then
        internalerror(2022110611);
      fkind:=newKind;
    end;

  procedure tai_llvmspecialisedmetadatanode.addvalue(val: tai_abstracttypedconst);
    begin
      internalerror(2021121601);
    end;

  procedure tai_llvmspecialisedmetadatanode.addboolean(const aitemname: TSymStr; boolval: boolean);
    var
      item: tllvmspecialisedmetaitem;
    begin
      item:=tllvmspecialisedmetaitem.createboolean(aitemname, boolval);
      addemplaceitem(item);
    end;

  procedure tai_llvmspecialisedmetadatanode.addint64(const aitemname: TSymStr; intval: int64);
    var
      item: tllvmspecialisedmetaitem;
    begin
      item:=tllvmspecialisedmetaitem.createint64(aitemname, intval);
      addemplaceitem(item);
    end;

  procedure tai_llvmspecialisedmetadatanode.addqword(const aitemname: TSymStr; qwval: qword);
    var
      item: tllvmspecialisedmetaitem;
    begin
      item:=tllvmspecialisedmetaitem.createqword(aitemname, qwval);
      addemplaceitem(item);
    end;

  procedure tai_llvmspecialisedmetadatanode.addmetadatarefto(const aitemname: TSymStr; aival: tai_llvmbasemetadatanode);
    var
      item: tllvmspecialisedmetaitem;
    begin
      if assigned(aival) then
        item:=tllvmspecialisedmetaitem.createmetadataref(aitemname, tai_llvmmetadatareftypedconst.create(aival))
      else
        item:=tllvmspecialisedmetaitem.createmetadataref(aitemname, nil);
      addemplaceitem(item);
    end;

  procedure tai_llvmspecialisedmetadatanode.addstring(const aitemname: TSymStr; const stringval: TSymStr);
    var
      item: tllvmspecialisedmetaitem;
    begin
      item:=tllvmspecialisedmetaitem.createstring(aitemname, stringval);
      addemplaceitem(item);
    end;

  procedure tai_llvmspecialisedmetadatanode.addenum(const aitemname: TSymStr; const enumval: TSymStr);
    var
      item: tllvmspecialisedmetaitem;
    begin
      item:=tllvmspecialisedmetaitem.createenum(aitemname, enumval);
      addemplaceitem(item);
    end;

  function tai_llvmspecialisedmetadatanode.IsDistinct: boolean;
    begin
      case fkind of
        tspecialisedmetadatanodekind.DICompileUnit,
        tspecialisedmetadatanodekind.DISubprogram,
        tspecialisedmetadatanodekind.DIGlobalVariable,
        tspecialisedmetadatanodekind.DICompositeType,
        tspecialisedmetadatanodekind.DILexicalBlock,
        tspecialisedmetadatanodekind.DIMacro:
          result:=true;
        tspecialisedmetadatanodekind.DIFile,
        tspecialisedmetadatanodekind.DIBasicType,
        tspecialisedmetadatanodekind.DIDerivedType,
        tspecialisedmetadatanodekind.DISubrange,
        tspecialisedmetadatanodekind.DIEnumerator,
        tspecialisedmetadatanodekind.DITemplateTypeParameter,
        tspecialisedmetadatanodekind.DITemplateValueParameter,
        tspecialisedmetadatanodekind.DINamespace,
        tspecialisedmetadatanodekind.DIGlobalVariableExpression,
        tspecialisedmetadatanodekind.DILexicalBlockFile,
        tspecialisedmetadatanodekind.DILocation,
        tspecialisedmetadatanodekind.DILocalVariable,
        tspecialisedmetadatanodekind.DIExpression,
        tspecialisedmetadatanodekind.DIObjCProperty,
        tspecialisedmetadatanodekind.DIImportedEntity,
        tspecialisedmetadatanodekind.DISubroutineType,
        tspecialisedmetadatanodekind.DIMacroFile:
          result:=false;
      end;
    end;

  class function tai_llvmspecialisedmetadatanode.isspecialised: boolean;
    begin
      result:=true;
    end;


/////////////////////////////////////////////////

  class function tllvmmetadata.addstring(const s: TSymStr): TSuperRegister;
    var
      index: longint;
    begin
      index:=current_module.llvmmetadatastrings.Add(s,nil);
      if index>high(result) then
        internalerror(2019122806);
      result:=index;
    end;


  class function tllvmmetadata.regtostring(reg: TRegister): TSymStr;
    begin
      if getregtype(reg)<>R_METADATAREGISTER then
        internalerror(2019122807);
      if getsubreg(reg)<>R_SUBMETASTRING then
        internalerror(2019122808);
      result:=current_module.llvmmetadatastrings.NameOfIndex(getsupreg(reg));
    end;


  class function tllvmmetadata.getstringreg(const s: TSymstr): TRegister;
    var
      supreg: TSuperRegister;
      index: longint;
    begin
      index:=current_module.llvmmetadatastrings.FindIndexOf(s);
      if index<>-1 then
        supreg:=index
      else
        supreg:=addstring(s);
      result:=newreg(R_METADATAREGISTER,supreg,R_SUBMETASTRING);
    end;


  class function tllvmmetadata.getpcharreg(p: pchar; len: longint): TRegister;
    var
      str: TSymStr;
    begin
      if len>0 then
        begin
          setlength(str,len);
          move(p[0],str[1],len);
          result:=getstringreg(str);
        end
      else
        result:=getstringreg('');
    end;


  class function tllvmmetadata.getregstring(reg: TRegister): TSymStr;
    begin
      result:=regtostring(reg);
    end;


end.

