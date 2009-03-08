{
    Copyright (c) 2009 by Jonas Maebe

    This unit implements code generator support for Objective-C nodes

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

unit ncgobjc;

{$i fpcdefs.inc}

interface

uses
  nobjc;

type
  tcgobjcselectornode = class(tobjcselectornode)
    procedure pass_generate_code; override;
  end;

implementation

uses
    globtype,cclasses,
    aasmbase,aasmdata,aasmtai,
    cgbase,cgutils,defutil,
    symsym,
    node,nld,ncon,
    verbose;

{*****************************************************************************
                           TCGOBJCSELECTORNODE
*****************************************************************************}

procedure tcgobjcselectornode.pass_generate_code;
  var
    reflab,
    strlab : tasmlabel;
    pool   : THashSet;
    entry  : PHashSetItem;
    name   : string;
    pc     : pchar;
  begin
    if current_asmdata.ConstPools[sp_objcselector] = nil then
      current_asmdata.ConstPools[sp_objcselector] := THashSet.Create(64, True, False);
    pool := current_asmdata.ConstPools[sp_objcselector];

    case left.nodetype of
      loadn:
        begin
          name:=tprocsym(tloadnode(left).symtableentry).mangledname;
          entry := pool.FindOrAdd(@name[1],length(name))
        end;
      stringconstn:
        begin
          entry := pool.FindOrAdd(tstringconstnode(left).value_str, tstringconstnode(left).len);
        end;
      else
        internalerror(2009030701);
    end;

    { have we already generated this selector? }
    if not assigned(entry^.Data) then
      begin
        { create new one
          (no getdatalabel, because these labels have to be local)
        }
        current_asmdata.getlabel(reflab,alt_data);
        current_asmdata.getlabel(strlab,alt_data);
        entry^.Data := reflab;
        getmem(pc,entry^.keylength+1);
        move(entry^.key^,pc^,entry^.keylength);
        pc[entry^.keylength]:=#0;
        { add a pointer to the message name in the objc_message_refs section }
        new_section(current_asmdata.asmlists[al_objc_data],sec_objc_message_refs,reflab.name,sizeof(pint));
        current_asmdata.asmlists[al_objc_data].concat(Tai_label.Create(reflab));
        current_asmdata.asmlists[al_objc_data].concat(Tai_const.Create_sym(strlab));

        { and now add the message name to the meth_var_names }
        new_section(current_asmdata.asmlists[al_objc_data],sec_objc_meth_var_names,strlab.name,1);
        current_asmdata.asmlists[al_objc_data].concat(Tai_label.Create(strlab));
        current_asmdata.asmlists[al_objc_data].concat(Tai_string.Create_pchar(pc,entry^.keylength+1));
    end;
    location_reset_ref(location, LOC_CREFERENCE, def_cgsize(resultdef), sizeof(pint));
    location.reference.symbol:=tasmlabel(entry^.Data);
  end;


begin
  cobjcselectornode:=tcgobjcselectornode;
end.
