{
    Copyright (c) 2014 by Florian Klaempfl

    Symbol table overrides for JVM

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
unit symcpu;

{$i fpcdefs.inc}

interface

uses
  globtype,
  aasmdata,
  symtype,
  symdef,symsym;

type
  { defs }
  tcpufiledef = class(tfiledef)
  end;

  tcpuvariantdef = class(tvariantdef)
  end;

  tcpuformaldef = class(tformaldef)
  end;

  tcpuforwarddef = class(tforwarddef)
  end;

  tcpuundefineddef = class(tundefineddef)
  end;

  tcpuerrordef = class(terrordef)
  end;

  tcpupointerdef = class(tpointerdef)
  end;

  tcpurecorddef = class(trecorddef)
  end;

  tcpuimplementedinterface = class(timplementedinterface)
  end;

  tcpuobjectdef = class(tobjectdef)
  end;

  tcpuclassrefdef = class(tclassrefdef)
  end;

  tcpuarraydef = class(tarraydef)
  end;

  tcpuorddef = class(torddef)
  end;

  tcpufloatdef = class(tfloatdef)
  end;

  tcpuprocvardef = class(tprocvardef)
   protected
    procedure ppuwrite_platform(ppufile: tcompilerppufile); override;
    procedure ppuload_platform(ppufile: tcompilerppufile); override;
   public
    { class representing this procvar on the Java side }
    classdef  : tobjectdef;
    classdefderef : tderef;
    procedure buildderef;override;
    procedure deref;override;
    function getcopy: tstoreddef; override;
  end;

  tcpuprocdef = class(tprocdef)
    { generated assembler code; used by JVM backend so it can afterwards
      easily write out all methods grouped per class }
    exprasmlist      : TAsmList;
    function  jvmmangledbasename(signature: boolean): TSymStr;
    function mangledname: TSymStr; override;
    destructor destroy; override;
  end;

  tcpustringdef = class(tstringdef)
  end;

  tcpuenumdef = class(tenumdef)
  end;

  tcpusetdef = class(tsetdef)
  end;

  { syms }
  tcpulabelsym = class(tlabelsym)
  end;

  tcpuunitsym = class(tunitsym)
  end;

  tcpunamespacesym = class(tnamespacesym)
  end;

  tcpuprocsym = class(tprocsym)
  end;

  tcpuypesym = class(ttypesym)
  end;

  tcpufieldvarsym = class(tfieldvarsym)
    procedure set_externalname(const s: string); override;
    function mangledname: TSymStr; override;
  end;

  tcpulocalvarsym = class(tlocalvarsym)
  end;

  tcpuparavarsym = class(tparavarsym)
  end;

  tcpustaticvarsym = class(tstaticvarsym)
    procedure set_mangledname(const s: TSymStr); override;
    function mangledname: TSymStr; override;
  end;

  tcpuabsolutevarsym = class(tabsolutevarsym)
  end;

  tcpupropertysym = class(tpropertysym)
  end;

  tcpuconstsym = class(tconstsym)
  end;

  tcpuenumsym = class(tenumsym)
  end;

  tcpusyssym = class(tsyssym)
  end;


implementation

  uses
    verbose,cutils,
    symconst,symbase,jvmdef,
    paramgr;

{****************************************************************************
                             tcpuprocdef
****************************************************************************}

  function tcpuprocdef.jvmmangledbasename(signature: boolean): TSymStr;
  var
    vs: tparavarsym;
    i: longint;
    founderror: tdef;
    tmpresult: TSymStr;
    container: tsymtable;
  begin
    { format:
        * method definition (in Jasmin):
            (private|protected|public) [static] method(parametertypes)returntype
        * method invocation
            package/class/method(parametertypes)returntype
      -> store common part: method(parametertypes)returntype and
         adorn as required when using it.
    }
    if not signature then
      begin
        { method name }
        { special names for constructors and class constructors }
        if proctypeoption=potype_constructor then
          tmpresult:='<init>'
        else if proctypeoption in [potype_class_constructor,potype_unitinit] then
          tmpresult:='<clinit>'
        else if po_has_importname in procoptions then
          begin
            if assigned(import_name) then
              tmpresult:=import_name^
            else
              internalerror(2010122608);
          end
        else
          begin
            tmpresult:=procsym.realname;
            if tmpresult[1]='$' then
              tmpresult:=copy(tmpresult,2,length(tmpresult)-1);
            { nested functions }
            container:=owner;
            while container.symtabletype=localsymtable do
              begin
                tmpresult:='$'+tprocdef(owner.defowner).procsym.realname+'$'+tostr(tprocdef(owner.defowner).procsym.symid)+'$'+tmpresult;
                container:=container.defowner.owner;
              end;
          end;
      end
    else
      tmpresult:='';
    { parameter types }
    tmpresult:=tmpresult+'(';
    { not the case for the main program (not required for defaultmangledname
      because setmangledname() is called for the main program; in case of
      the JVM, this only sets the importname, however) }
    if assigned(paras) then
      begin
        init_paraloc_info(callerside);
        for i:=0 to paras.count-1 do
          begin
            vs:=tparavarsym(paras[i]);
            { function result is not part of the mangled name }
            if vo_is_funcret in vs.varoptions then
              continue;
            { self pointer neither, except for class methods (the JVM only
              supports static class methods natively, so the self pointer
              here is a regular parameter as far as the JVM is concerned }
            if not(po_classmethod in procoptions) and
               (vo_is_self in vs.varoptions) then
              continue;
            { passing by reference is emulated by passing an array of one
              element containing the value; for types that aren't pointers
              in regular Pascal, simply passing the underlying pointer type
              does achieve regular call-by-reference semantics though;
              formaldefs always have to be passed like that because their
              contents can be replaced }
            if paramanager.push_copyout_param(vs.varspez,vs.vardef,proccalloption) then
              tmpresult:=tmpresult+'[';
            { Add the parameter type.  }
            if not jvmaddencodedtype(vs.vardef,false,tmpresult,signature,founderror) then
              { an internalerror here is also triggered in case of errors in the source code }
              tmpresult:='<error>';
          end;
      end;
    tmpresult:=tmpresult+')';
    { And the type of the function result (void in case of a procedure and
      constructor). }
    if (proctypeoption in [potype_constructor,potype_class_constructor]) then
      jvmaddencodedtype(voidtype,false,tmpresult,signature,founderror)
    else if not jvmaddencodedtype(returndef,false,tmpresult,signature,founderror) then
      { an internalerror here is also triggered in case of errors in the source code }
      tmpresult:='<error>';
    result:=tmpresult;
  end;


  function tcpuprocdef.mangledname: TSymStr;
    begin
      if _mangledname='' then
        begin
          result:=jvmmangledbasename(false);
          if (po_has_importdll in procoptions) then
            begin
              { import_dll comes from "external 'import_dll_name' name 'external_name'" }
              if assigned(import_dll) then
                result:=import_dll^+'/'+result
              else
                internalerror(2010122607);
            end
          else
            jvmaddtypeownerprefix(owner,mangledname);
          _mangledname:=result;
        end
      else
        result:=_mangledname;
    end;


  destructor tcpuprocdef.destroy;
    begin
      exprasmlist.free;
      inherited destroy;
    end;

{****************************************************************************
                             tcpuprocvardef
****************************************************************************}

  procedure tcpuprocvardef.ppuwrite_platform(ppufile: tcompilerppufile);
    begin
      inherited;
      ppufile.putderef(classdefderef);
    end;


  procedure tcpuprocvardef.ppuload_platform(ppufile: tcompilerppufile);
    begin
      inherited;
      ppufile.getderef(classdefderef);
    end;


  procedure tcpuprocvardef.buildderef;
    begin
      inherited buildderef;
      classdefderef.build(classdef);
    end;


  procedure tcpuprocvardef.deref;
    begin
      inherited deref;
      classdef:=tobjectdef(classdefderef.resolve);
    end;

  function tcpuprocvardef.getcopy: tstoreddef;
    begin
      result:=inherited;
      tcpuprocvardef(result).classdef:=classdef;
    end;

{****************************************************************************
                             tcpustaticvarsym
****************************************************************************}

  procedure tcpustaticvarsym.set_mangledname(const s: TSymStr);
    begin
      inherited;
      _mangledname:=jvmmangledbasename(self,s,false);
      jvmaddtypeownerprefix(owner,_mangledname);
    end;


  function tcpustaticvarsym.mangledname: TSymStr;
    begin
      if _mangledname='' then
        begin
          if _mangledbasename='' then
            _mangledname:=jvmmangledbasename(self,false)
          else
            _mangledname:=jvmmangledbasename(self,_mangledbasename,false);
          jvmaddtypeownerprefix(owner,_mangledname);
        end;
      result:=_mangledname;
    end;


{****************************************************************************
                             tcpufieldvarsym
****************************************************************************}

  procedure tcpufieldvarsym.set_externalname(const s: string);
    begin
      { make sure it is recalculated }
      cachedmangledname:='';
      if is_java_class_or_interface(tdef(owner.defowner)) then
        begin
          externalname:=stringdup(s);
          include(varoptions,vo_has_mangledname);
        end
      else
        internalerror(2011031201);
    end;


  function tcpufieldvarsym.mangledname: TSymStr;
    begin
      if is_java_class_or_interface(tdef(owner.defowner)) or
         (tdef(owner.defowner).typ=recorddef) then
        begin
          if cachedmangledname<>'' then
            result:=cachedmangledname
          else
            begin
              result:=jvmmangledbasename(self,false);
              jvmaddtypeownerprefix(owner,result);
              cachedmangledname:=result;
            end;
        end
      else
        result:=inherited;
    end;

begin
  { used tdef classes }
  cfiledef:=tcpufiledef;
  cvariantdef:=tcpuvariantdef;
  cformaldef:=tcpuformaldef;
  cforwarddef:=tcpuforwarddef;
  cundefineddef:=tcpuundefineddef;
  cerrordef:=tcpuerrordef;
  cpointerdef:=tcpupointerdef;
  crecorddef:=tcpurecorddef;
  cimplementedinterface:=tcpuimplementedinterface;
  cobjectdef:=tcpuobjectdef;
  cclassrefdef:=tcpuclassrefdef;
  carraydef:=tcpuarraydef;
  corddef:=tcpuorddef;
  cfloatdef:=tcpufloatdef;
  cprocvardef:=tcpuprocvardef;
  cprocdef:=tcpuprocdef;
  cstringdef:=tcpustringdef;
  cenumdef:=tcpuenumdef;
  csetdef:=tcpusetdef;

  { used tsym classes }
  clabelsym:=tcpulabelsym;
  cunitsym:=tcpuunitsym;
  cnamespacesym:=tcpunamespacesym;
  cprocsym:=tcpuprocsym;
  ctypesym:=tcpuypesym;
  cfieldvarsym:=tcpufieldvarsym;
  clocalvarsym:=tcpulocalvarsym;
  cparavarsym:=tcpuparavarsym;
  cstaticvarsym:=tcpustaticvarsym;
  cabsolutevarsym:=tcpuabsolutevarsym;
  cpropertysym:=tcpupropertysym;
  cconstsym:=tcpuconstsym;
  cenumsym:=tcpuenumsym;
  csyssym:=tcpusyssym;
end.

