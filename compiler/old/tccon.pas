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

{$i defines.inc}

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
  Revision 1.1  2000-10-14 10:14:58  peter
    * moehrendorf oct 2000 rewrite

  Revision 1.3  2000/09/24 21:19:53  peter
    * delphi compile fixes

  Revision 1.2  2000/07/13 11:32:51  michael
  + removed logs

}
