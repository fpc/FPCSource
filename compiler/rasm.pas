{
    Copyright (c) 1998-2003 by Peter Vreman, Florian Klaempfl and Carl Eric Codere

    Basic stuff for assembler readers

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
unit rasm;

{$i fpcdefs.inc}

  interface

    uses
      cclasses,
      rabase,
      aasmtai,aasmdata,
      systems,
      cpubase,
      cgbase;

    type
       tasmreader = class(tbaseasmreader)
         firsttoken     : boolean;
         _asmsorted     : boolean;
         curlist        : TAsmList;
         c              : char;
         actasmpattern  : string;
         actopcode      : tasmop;
         actasmregister : tregister;
         actcondition   : tasmcond;
         iasmops        : TFPHashList;
         constructor create;override;
         destructor destroy;override;
       end;

  implementation

    constructor tasmreader.create;
      begin
        inherited create;
        firsttoken:=true;
      end;


    destructor tasmreader.destroy;
      begin
        if assigned(iasmops) then
          iasmops.Free;
        inherited destroy;
      end;


end.
