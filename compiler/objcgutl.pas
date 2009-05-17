{
    Copyright (c) 2009 by Jonas Maebe

    This unit implements some Objective-C helper routines at the code generator
    level.

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

{$i fpcdefs.inc}

unit objcgutl;

interface

uses
  cclasses,
  aasmbase;

procedure objcfinishstringrefpoolentry(entry: phashsetitem; refsec, stringsec: tasmsectiontype);


implementation

uses
  globtype,
  aasmdata,aasmtai,
  cgbase,cgutils,
  symsym,
  verbose;

procedure objcfinishstringrefpoolentry(entry: phashsetitem; refsec, stringsec: tasmsectiontype);
  var
    reflab,
    strlab : tasmlabel;
    pc     : pchar;
  begin
    { have we already generated this selector? }
    if not assigned(entry^.Data) then
      begin
        { create new one
          (no getdatalabel, because these labels have to be local)
        }
        current_asmdata.getlabel(reflab,alt_data);
        current_asmdata.getlabel(strlab,alt_data);
        entry^.Data:=reflab;
        getmem(pc,entry^.keylength+1);
        move(entry^.key^,pc^,entry^.keylength);
        pc[entry^.keylength]:=#0;
        { add a pointer to the message name in the string references section }
        new_section(current_asmdata.asmlists[al_objc_data],refsec,reflab.name,sizeof(pint));
        current_asmdata.asmlists[al_objc_data].concat(Tai_label.Create(reflab));
        current_asmdata.asmlists[al_objc_data].concat(Tai_const.Create_sym(strlab));

        { and now add the message name to the associated strings section }
        new_section(current_asmdata.asmlists[al_objc_data],stringsec,strlab.name,1);
        current_asmdata.asmlists[al_objc_data].concat(Tai_label.Create(strlab));
        current_asmdata.asmlists[al_objc_data].concat(Tai_string.Create_pchar(pc,entry^.keylength+1));
    end;
  end;

end.
