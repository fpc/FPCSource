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
    symconst,
    jvmdef;

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

