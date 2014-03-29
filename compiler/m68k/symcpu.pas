{
    Copyright (c) 2014 by Florian Klaempfl

    Symbol table overrides for m68k

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
  end;

  tcpulocalvarsym = class(tlocalvarsym)
  end;

  tcpuparavarsym = class(tparavarsym)
  end;

  tcpustaticvarsym = class(tstaticvarsym)
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

