{
    Copyright (c) 2014 by Florian Klaempfl

    Symbol table overrides for i8086

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
  symconst,symtype,symdef,symsym,symx86,symi86;

type
  { defs }
  tcpufiledef = class(tfiledef)
  end;
  tcpufiledefclass = class of tcpufiledef;

  tcpuvariantdef = class(tvariantdef)
  end;
  tcpuvariantdefclass = class of tcpuvariantdef;

  tcpuformaldef = class(tformaldef)
  end;
  tcpuformaldefclass = class of tcpuformaldef;

  tcpuforwarddef = class(tforwarddef)
  end;
  tcpuforwarddefclass = class of tcpuforwarddef;

  tcpuundefineddef = class(tundefineddef)
  end;
  tcpuundefineddefclass = class of tcpuundefineddef;

  tcpuerrordef = class(terrordef)
  end;
  tcpuerrordefclass = class of tcpuerrordef;

  tcpupointerdef = class(tx86pointerdef)
    class function default_x86_data_pointer_type: tx86pointertyp; override;
  end;
  tcpupointerdefclass = class of tcpupointerdef;

  tcpurecorddef = class(trecorddef)
  end;
  tcpurecorddefclass = class of tcpurecorddef;

  tcpuimplementedinterface = class(timplementedinterface)
  end;
  tcpuimplementedinterfaceclass = class of tcpuimplementedinterface;

  tcpuobjectdef = class(tobjectdef)
  end;
  tcpuobjectdefclass = class of tcpuobjectdef;

  tcpuclassrefdef = class(tclassrefdef)
  end;
  tcpuclassrefdefclass = class of tcpuclassrefdef;

  tcpuarraydef = class(tarraydef)
  end;
  tcpuarraydefclass = class of tcpuarraydef;

  tcpuorddef = class(torddef)
  end;
  tcpuorddefclass = class of tcpuorddef;

  tcpufloatdef = class(tfloatdef)
  end;
  tcpufloatdefclass = class of tcpufloatdef;

  { tcpuprocvardef }

  tcpuprocvardef = class(ti86procvardef)
    constructor create(level:byte);override;
    function is_far:boolean;
  end;
  tcpuprocvardefclass = class of tcpuprocvardef;

  { tcpuprocdef }

  tcpuprocdef = class(ti86procdef)
   private
    { returns whether the function is far by default, i.e. whether it would be
      far if _all_ of the following conditions are true:
      - we're in a far code memory model
      - it has no 'near' or 'far' specifiers
      - it is compiled in a $F- state }
    function default_far:boolean;
   public
    constructor create(level:byte);override;
    function address_type:tdef;override;
    procedure declared_far;override;
    procedure declared_near;override;
    function is_far:boolean;
  end;
  tcpuprocdefclass = class of tcpuprocdef;

  tcpustringdef = class(tstringdef)
  end;
  tcpustringdefclass = class of tcpustringdef;

  tcpuenumdef = class(tenumdef)
  end;
  tcpuenumdefclass = class of tcpuenumdef;

  tcpusetdef = class(tsetdef)
  end;
  tcpusetdefclass = class of tcpusetdef;

  { syms }
  tcpulabelsym = class(tlabelsym)
  end;
  tcpulabelsymclass = class of tcpulabelsym;

  tcpuunitsym = class(tunitsym)
  end;
  tcpuunitsymclass = class of tcpuunitsym;

  tcpunamespacesym = class(tnamespacesym)
  end;
  tcpunamespacesymclass = class of tcpunamespacesym;

  tcpuprocsym = class(tprocsym)
  end;
  tcpuprocsymclass = class of tcpuprocsym;

  tcputypesym = class(ttypesym)
  end;
  tcpuypesymclass = class of tcputypesym;

  tcpufieldvarsym = class(tfieldvarsym)
  end;
  tcpufieldvarsymclass = class of tcpufieldvarsym;

  tcpulocalvarsym = class(tlocalvarsym)
  end;
  tcpulocalvarsymclass = class of tcpulocalvarsym;

  tcpuparavarsym = class(tparavarsym)
  end;
  tcpuparavarsymclass = class of tcpuparavarsym;

  tcpustaticvarsym = class(tstaticvarsym)
  end;
  tcpustaticvarsymclass = class of tcpustaticvarsym;

  tcpuabsolutevarsym = class(ti86absolutevarsym)
   protected
    procedure ppuload_platform(ppufile: tcompilerppufile); override;
    procedure ppuwrite_platform(ppufile: tcompilerppufile); override;
   public
    addrsegment : aword;
  end;
  tcpuabsolutevarsymclass = class of tcpuabsolutevarsym;

  tcpupropertysym = class(tpropertysym)
  end;
  tcpupropertysymclass = class of tcpupropertysym;

  tcpuconstsym = class(tconstsym)
  end;
  tcpuconstsymclass = class of tcpuconstsym;

  tcpuenumsym = class(tenumsym)
  end;
  tcpuenumsymclass = class of tcpuenumsym;

  tcpusyssym = class(tsyssym)
  end;
  tcpusyssymclass = class of tcpusyssym;


const
   pbestrealtype : ^tdef = @s80floattype;


  function is_proc_far(p: tabstractprocdef): boolean;


implementation

  uses
    globals, cpuinfo, verbose;


  function is_proc_far(p: tabstractprocdef): boolean;
  begin
    if p is tcpuprocdef then
      result:=tcpuprocdef(p).is_far
    else if p is tcpuprocvardef then
      result:=tcpuprocvardef(p).is_far
    else
      internalerror(2014041301);
  end;


{****************************************************************************
                             tcpuprocdef
****************************************************************************}

  constructor tcpuprocdef.create(level: byte);
    begin
      inherited create(level);
      if (current_settings.x86memorymodel in x86_far_code_models) and
         (cs_force_far_calls in current_settings.localswitches) then
        procoptions:=procoptions+[po_far];
    end;


  function tcpuprocdef.address_type: tdef;
    begin
      if is_far then
        result:=voidfarpointertype
      else
        result:=voidnearpointertype;
    end;


  procedure tcpuprocdef.declared_far;
    begin
      if current_settings.x86memorymodel in x86_far_code_models then
        include(procoptions,po_far)
      else
        inherited declared_far;
    end;


  procedure tcpuprocdef.declared_near;
    begin
      if current_settings.x86memorymodel in x86_far_code_models then
        exclude(procoptions,po_far)
      else
        inherited declared_near;
    end;


  function tcpuprocdef.default_far: boolean;
    begin
      if proctypeoption in [potype_proginit,potype_unitinit,potype_unitfinalize,
                            potype_constructor,potype_destructor,
                            potype_class_constructor,potype_class_destructor,
                            potype_propgetter,potype_propsetter] then
        exit(true);
      if (procoptions*[po_classmethod,po_virtualmethod,po_abstractmethod,
                       po_finalmethod,po_staticmethod,po_overridingmethod,
                       po_external,po_public,po_interrupt])<>[] then
        exit(true);
      if is_methodpointer then
        exit(true);
      result:=not (visibility in [vis_private,vis_hidden]);
    end;


  function tcpuprocdef.is_far: boolean;
    begin
      result:=(current_settings.x86memorymodel in x86_far_code_models) and
        ((po_far in procoptions) or default_far);
    end;

{****************************************************************************
                             tcpuprocvardef
****************************************************************************}

  constructor tcpuprocvardef.create(level: byte);
    begin
      inherited create(level);
      { procvars are always far in the far code memory models }
      if current_settings.x86memorymodel in x86_far_code_models then
        procoptions:=procoptions+[po_far];
    end;


  function tcpuprocvardef.is_far: boolean;
    begin
      { procvars are always far in the far code memory models }
      result:=current_settings.x86memorymodel in x86_far_code_models;
    end;

{****************************************************************************
                             tcpupointerdef
****************************************************************************}

    class function tcpupointerdef.default_x86_data_pointer_type: tx86pointertyp;
      begin
        if current_settings.x86memorymodel in x86_far_data_models then
          result:=x86pt_far
        else
          result:=inherited;
      end;


{****************************************************************************
                             tcpuabsolutevarsym
****************************************************************************}

  procedure tcpuabsolutevarsym.ppuload_platform(ppufile: tcompilerppufile);
    begin
      inherited;
      if absseg then
        addrsegment:=ppufile.getaword;
    end;


  procedure tcpuabsolutevarsym.ppuwrite_platform(ppufile: tcompilerppufile);
    begin
      inherited;
      if absseg then
        ppufile.putaword(addrsegment);
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
  ctypesym:=tcputypesym;
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

