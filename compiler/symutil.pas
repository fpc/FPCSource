{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    This unit provides some help routines for symbol handling

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
unit symutil;

{$i fpcdefs.inc}

interface

    uses
       cclasses,
       cpuinfo,
       globals,
       node,
       symconst,symbase,symtype,symdef,symsym;


    function equal_constsym(sym1,sym2:tconstsym):boolean;

    { returns true, if sym needs an entry in the proplist of a class rtti }
    function needs_prop_entry(sym : tsym) : boolean;


implementation

    uses
       globtype,tokens,systems,verbose,
       symtable;


    function needs_prop_entry(sym : tsym) : boolean;

      begin
         needs_prop_entry:=(sp_published in tsym(sym).symoptions) and
         (sym.typ in [propertysym,varsym]);
      end;


    function equal_constsym(sym1,sym2:tconstsym):boolean;
      var
        p1,p2,pend : pchar;
      begin
        equal_constsym:=false;
        if sym1.consttyp<>sym2.consttyp then
         exit;
        case sym1.consttyp of
           constint,
           constbool,
           constchar,
           constord :
             equal_constsym:=(sym1.value.valueord=sym2.value.valueord);
           constpointer :
             equal_constsym:=(sym1.value.valueordptr=sym2.value.valueordptr);
           conststring,constresourcestring :
             begin
               if sym1.value.len=sym2.value.len then
                begin
                  p1:=pchar(sym1.value.valueptr);
                  p2:=pchar(sym2.value.valueptr);
                  pend:=p1+sym1.value.len;
                  while (p1<pend) do
                   begin
                     if p1^<>p2^ then
                      break;
                     inc(p1);
                     inc(p2);
                   end;
                  if (p1=pend) then
                   equal_constsym:=true;
                end;
             end;
           constreal :
             equal_constsym:=(pbestreal(sym1.value.valueptr)^=pbestreal(sym2.value.valueptr)^);
           constset :
             equal_constsym:=(pnormalset(sym1.value.valueptr)^=pnormalset(sym2.value.valueptr)^);
           constnil :
             equal_constsym:=true;
        end;
      end;

end.
{
  $Log$
  Revision 1.1  2002-11-25 17:43:26  peter
    * splitted defbase in defutil,symutil,defcmp
    * merged isconvertable and is_equal into compare_defs(_ext)
    * made operator search faster by walking the list only once

}
