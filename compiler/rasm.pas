{
    $Id: rasm.pas,v 1.3 2005/02/14 17:13:07 peter Exp $
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
      aasmtai,
      systems,
      cpubase,
      cgbase;

    type
       tasmreader = class(tbaseasmreader)
         firsttoken     : boolean;
         _asmsorted     : boolean;
         curlist        : TAAsmoutput;
         c              : char;
         actasmpattern  : string;
         actopcode      : tasmop;
         actasmregister : tregister;
         actcondition   : tasmcond;
         iasmops        : tdictionary;
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
{
  $Log: rasm.pas,v $
  Revision 1.3  2005/02/14 17:13:07  peter
    * truncate log

}
