{
    Copyright (c) 2014 Jonas Maebe

    Generate high level target code for math nodes

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
unit ncghlmat;

{$i fpcdefs.inc}

interface

uses
  node,
  ncgmat;

type

  tcghlnotnode = class(tcgnotnode)
    function pass_1: tnode; override;
   protected
    procedure second_boolean; override;
  end;

implementation

uses
  aasmbase,aasmdata,
  defutil,
  procinfo,
  cgbase,pass_2,hlcgobj;

{*****************************************************************************
                               tcghlnotnode
*****************************************************************************}

function tcghlnotnode.pass_1: tnode;
  begin
    result:=inherited;
    if not assigned(result) and
       is_boolean(resultdef) then
      expectloc:=LOC_JUMP;
  end;


procedure tcghlnotnode.second_boolean;
  var
    hl : tasmlabel;
  begin
    hl:=current_procinfo.CurrTrueLabel;
    current_procinfo.CurrTrueLabel:=current_procinfo.CurrFalseLabel;
    current_procinfo.CurrFalseLabel:=hl;
    secondpass(left);
    hlcg.maketojumpbool(current_asmdata.CurrAsmList,left);
    hl:=current_procinfo.CurrTrueLabel;
    current_procinfo.CurrTrueLabel:=current_procinfo.CurrFalseLabel;
    current_procinfo.CurrFalseLabel:=hl;
    location.loc:=LOC_JUMP;
  end;

end.
