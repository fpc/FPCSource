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
       cobjects,globals,scanner,symtable,pbase,verbose;

    const
       { export options }
       eo_resident = $1;

    type
       pexportsitem = ^texportsitem;

       texportsitem = object(tlinkedlist_item)
          sym : psym;
          index : longint;
          name : pstring;
          options : word;
          constructor init;
       end;

    var
       exportslist : tlinkedlist;

    constructor texportsitem.init;

      begin
         sym:=nil;
         index:=-1;
         name:=nil;
         options:=0;
      end;

    procedure read_exports;

      var
         hp : pexportsitem;
         code : word;

      begin
         hp:=new(pexportsitem,init);
         consume(_EXPORTS);
         while true do
           begin
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
                        if (token=ID) and (pattern='INDEX') then
                          begin
                             consume(ID);
                             val(pattern,hp^.index,code);
                             consume(INTCONST);
                          end;
                        if (token=ID) and (pattern='NAME') then
                          begin
                             consume(ID);
                             hp^.name:=stringdup(pattern);
                             consume(ID);
                          end;
                        if (token=ID) and (pattern='RESIDENT') then
                          begin
                             consume(ID);
                             hp^.options:=hp^.options or eo_resident;
                          end;
                     end;
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

begin
   { a library is a root of sources, e.g. it can't be used
     twice in one compiler run }
   exportslist.init;
end.

{
  $Log$
  Revision 1.1  1998-03-25 11:18:15  root
  Initial revision

  Revision 1.7  1998/03/10 01:17:24  peter
    * all files have the same header
    * messages are fully implemented, EXTDEBUG uses Comment()
    + AG... files for the Assembler generation

  Revision 1.6  1998/03/06 00:52:42  peter
    * replaced all old messages from errore.msg, only ExtDebug and some
      Comment() calls are left
    * fixed options.pas

  Revision 1.5  1998/03/02 01:49:01  peter
    * renamed target_DOS to target_GO32V1
    + new verbose system, merged old errors and verbose units into one new
      verbose.pas, so errors.pas is obsolete

  Revision 1.4  1998/02/13 10:35:24  daniel
  * Made Motorola version compilable.
  * Fixed optimizer

  Revision 1.3  1998/01/12 13:02:41  florian
    + items of exports are now seperated by ,

  Revision 1.2  1998/01/12 12:11:35  florian
    + unit qualifier is now allowed to specify exported symbols
    + exports starts now a list of symbols to export

  Revision 1.1  1998/01/11 10:58:07  florian
    + pexports in lowercase commited

  Revision 1.1  1998/01/11 10:54:19  florian
    + generic library support

}
