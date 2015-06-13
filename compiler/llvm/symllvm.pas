{
    Copyright (c) 2014 by Florian Klaempfl

    Symbol table overrides for LLVM

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
unit symllvm;

{$i fpcdefs.inc}

interface

uses
  globtype,
  symcpu;

type
  { defs }
  tllvmfiledef = class(tcpufiledef)
  end;

  tllvmvariantdef = class(tcpuvariantdef)
  end;

  tllvmformaldef = class(tcpuformaldef)
  end;

  tllvmforwarddef = class(tcpuforwarddef)
  end;

  tllvmundefineddef = class(tcpuundefineddef)
  end;

  tllvmerrordef = class(tcpuerrordef)
  end;

  tllvmpointerdef = class(tcpupointerdef)
  end;

  tllvmrecorddef = class(tcpurecorddef)
  end;

  tllvmimplementedinterface = class(tcpuimplementedinterface)
  end;

  tllvmobjectdef = class(tcpuobjectdef)
  end;

  tllvmclassrefdef = class(tcpuclassrefdef)
  end;

  tllvmarraydef = class(tcpuarraydef)
  end;

  tllvmorddef = class(tcpuorddef)
  end;

  tllvmfloatdef = class(tcpufloatdef)
  end;

  tllvmprocvardef = class(tcpuprocvardef)
  end;

  { tllvmprocdef }

  tllvmprocdef = class(tcpuprocdef)
   protected
    external_decl_mangled_name: TSymStr;
   public
    { overried the mangled name of external functions }
    function mangledname: TSymStr; override;
    { provide access to the original mangled name of external functions }
    function external_mangledname: TSymStr;
  end;

  tllvmstringdef = class(tcpustringdef)
  end;

  tllvmenumdef = class(tcpuenumdef)
  end;

  tllvmsetdef = class(tcpusetdef)
  end;

  { syms }
  tllvmlabelsym = class(tcpulabelsym)
  end;

  tllvmunitsym = class(tcpuunitsym)
  end;

  tllvmprogramparasym = class(tcpuprogramparasym)
  end;

  tllvmnamespacesym = class(tcpunamespacesym)
  end;

  tllvmprocsym = class(tcpuprocsym)
  end;

  tllvmtypesym = class(tcputypesym)
  end;

  tllvmfieldvarsym = class(tcpufieldvarsym)
  end;

  tllvmlocalvarsym = class(tcpulocalvarsym)
  end;

  tllvmparavarsym = class(tcpuparavarsym)
  end;

  tllvmstaticvarsym = class(tcpustaticvarsym)
  end;

  tllvmabsolutevarsym = class(tcpuabsolutevarsym)
  end;

  tllvmpropertysym = class(tcpupropertysym)
  end;

  tllvmconstsym = class(tcpuconstsym)
  end;

  tllvmenumsym = class(tcpuenumsym)
  end;

  tllvmsyssym = class(tcpusyssym)
  end;


implementation

uses
  symconst,symdef,symsym;

{ tllvmprocdef }

function tllvmprocdef.mangledname: TSymStr;
  begin
    { External declarations are handled separately for the LLVM target and need
      a different mangled name than on other targets, because we have to
      create a declaration based on the declared name that redirects to the
      external name in order to deal with potential different signatures between
      the external declaration the symbol that's referred.

      There's no need to store the external_decl_mangled_name in the ppu file,
      since it can always be regenerated on the fly. }
    if not(po_external in procoptions) then
      result:=inherited
    else
      begin
        if external_decl_mangled_name='' then
          begin
            result:=defaultmangledname;
            external_decl_mangled_name:=result;
          end
        else
          result:=external_decl_mangled_name;
      end;
  end;


function tllvmprocdef.external_mangledname: TSymStr;
  begin
    result:=inherited mangledname;
  end;

begin
  { used tdef classes }
  cfiledef:=tllvmfiledef;
  cvariantdef:=tllvmvariantdef;
  cformaldef:=tllvmformaldef;
  cforwarddef:=tllvmforwarddef;
  cundefineddef:=tllvmundefineddef;
  cerrordef:=tllvmerrordef;
  cpointerdef:=tllvmpointerdef;
  crecorddef:=tllvmrecorddef;
  cimplementedinterface:=tllvmimplementedinterface;
  cobjectdef:=tllvmobjectdef;
  cclassrefdef:=tllvmclassrefdef;
  carraydef:=tllvmarraydef;
  corddef:=tllvmorddef;
  cfloatdef:=tllvmfloatdef;
  cprocvardef:=tllvmprocvardef;
  cprocdef:=tllvmprocdef;
  cstringdef:=tllvmstringdef;
  cenumdef:=tllvmenumdef;
  csetdef:=tllvmsetdef;

  { used tsym classes }
  clabelsym:=tllvmlabelsym;
  cunitsym:=tllvmunitsym;
  cprogramparasym:=tllvmprogramparasym;
  cnamespacesym:=tllvmnamespacesym;
  cprocsym:=tllvmprocsym;
  ctypesym:=tllvmtypesym;
  cfieldvarsym:=tllvmfieldvarsym;
  clocalvarsym:=tllvmlocalvarsym;
  cparavarsym:=tllvmparavarsym;
  cstaticvarsym:=tllvmstaticvarsym;
  cabsolutevarsym:=tllvmabsolutevarsym;
  cpropertysym:=tllvmpropertysym;
  cconstsym:=tllvmconstsym;
  cenumsym:=tllvmenumsym;
  csyssym:=tllvmsyssym;
end.

