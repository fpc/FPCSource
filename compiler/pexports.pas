{
    $Id$
    Copyright (c) 1998 by Florian Klaempfl

    This unit handles the exports parsing

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
unit pexports;

  interface

    { reads an exports statement in a library }
    procedure read_exports;

  implementation

    uses
       cobjects,globals,scanner,symtable,pbase,verbose,export;

    const
       { export options }
       eo_resident = $1;

    procedure read_exports;

      var
         hp : pexported_procedure;
         code : word;

      begin
         consume(_EXPORTS);
         while true do
           begin
              hp:=new(pexported_procedure,init);
              if token=ID then
                begin
                   getsym(pattern,true);
                   if srsym^.typ=unitsym then
                     begin
                        consume(ID);
                        consume(POINT);
                        getsymonlyin(punitsym(srsym)^.unitsymtable,pattern);
                     end;
                   consume(ID);
                   if assigned(srsym) then
                     begin
                        hp^.sym:=srsym;
                        if (srsym^.typ<>procsym) or
                          ((pprocdef(pprocsym(srsym)^.definition)^.options and poexports)=0) then
                          Message(parser_e_illegal_symbol_exported);
                        if (idtoken=_INDEX) then
                          begin
                             consume(_INDEX);
                             val(pattern,hp^.index,code);
                             consume(INTCONST);
                          end;
                        if (idtoken=_NAME) then
                          begin
                             consume(_NAME);
                             hp^.name:=stringdup(pattern);
                             consume(ID);
                          end;
                        if (idtoken=_RESIDENT) then
                          begin
                             consume(_RESIDENT);
                             hp^.options:=hp^.options or eo_resident;
                          end;
                     end;
                   exportlib^.exportprocedure(hp);
                end
              else
                consume(ID);
              if token=COMMA then
                consume(COMMA)
              else
                break;
           end;
         consume(SEMICOLON);
      end;

end.

{
  $Log$
  Revision 1.3  1998-10-29 11:35:51  florian
    * some dll support for win32
    * fixed assembler writing for PalmOS

  Revision 1.2  1998/09/26 17:45:35  peter
    + idtoken and only one token table

}
