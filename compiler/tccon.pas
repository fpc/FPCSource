{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

    Type checking and register allocation for constants

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
unit tccon;
interface

    uses
      tree;

    procedure firstrealconst(var p : ptree);
    procedure firstfixconst(var p : ptree);
    procedure firstordconst(var p : ptree);
    procedure firstpointerconst(var p : ptree);
    procedure firststringconst(var p : ptree);
    procedure firstsetconst(var p : ptree);
    procedure firstniln(var p : ptree);


implementation

    uses
      cobjects,verbose,globals,systems,
      symconst,symtable,aasm,types,
      hcodegen,pass_1,cpubase;

{*****************************************************************************
                             FirstRealConst
*****************************************************************************}

    procedure firstrealconst(var p : ptree);
      begin
         if (p^.value_real=1.0) or (p^.value_real=0.0) then
           begin
              p^.location.loc:=LOC_FPU;
              p^.registersfpu:=1;
           end
         else
           p^.location.loc:=LOC_MEM;
      end;


{*****************************************************************************
                             FirstFixConst
*****************************************************************************}

    procedure firstfixconst(var p : ptree);
      begin
         p^.location.loc:=LOC_MEM;
      end;


{*****************************************************************************
                             FirstOrdConst
*****************************************************************************}

    procedure firstordconst(var p : ptree);
      begin
         p^.location.loc:=LOC_MEM;
      end;


{*****************************************************************************
                             FirstPointerConst
*****************************************************************************}

    procedure firstpointerconst(var p : ptree);
      begin
         p^.location.loc:=LOC_MEM;
      end;


{*****************************************************************************
                            FirstStringConst
*****************************************************************************}

    procedure firststringconst(var p : ptree);
      begin
{        if cs_ansistrings in aktlocalswitches then
          p^.resulttype:=cansistringdef
         else
          p^.resulttype:=cshortstringdef; }
        case p^.stringtype of
          st_shortstring :
            p^.resulttype:=cshortstringdef;
          st_ansistring :
            p^.resulttype:=cansistringdef;
          st_widestring :
            p^.resulttype:=cwidestringdef;
          st_longstring :
            p^.resulttype:=clongstringdef;
        end;
        p^.location.loc:=LOC_MEM;
      end;


{*****************************************************************************
                           FirstSetConst
*****************************************************************************}

    procedure firstsetconst(var p : ptree);
      begin
         p^.location.loc:=LOC_MEM;
      end;


{*****************************************************************************
                              FirstNilN
*****************************************************************************}

    procedure firstniln(var p : ptree);
      begin
        p^.resulttype:=voidpointerdef;
        p^.location.loc:=LOC_MEM;
      end;


end.
{
  $Log$
  Revision 1.12  2000-02-09 13:23:07  peter
    * log truncated

  Revision 1.11  2000/01/07 01:14:45  peter
    * updated copyright to 2000

  Revision 1.10  1999/09/26 21:30:22  peter
    + constant pointer support which can happend with typecasting like
      const p=pointer(1)
    * better procvar parsing in typed consts

  Revision 1.9  1999/09/04 20:52:07  florian
    * bug 580 fixed

  Revision 1.8  1999/08/04 00:23:38  florian
    * renamed i386asm and i386base to cpuasm and cpubase

  Revision 1.7  1999/08/03 22:03:29  peter
    * moved bitmask constants to sets
    * some other type/const renamings

}
