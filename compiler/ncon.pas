{
    $Id$
    Copyright (c) 2000 by Florian Klaempfl

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
unit ncon;

{$i defines.inc}

interface

    uses
      globtype,widestr,
      node,
      aasm,cpuinfo,
      symconst,symtype,symdef,symsym;

    type
       trealconstnode = class(tnode)
          value_real : bestreal;
          lab_real : pasmlabel;
          constructor create(v : bestreal;def : pdef);virtual;
          function getcopy : tnode;override;
          function pass_1 : tnode;override;
       end;

       tfixconstnode = class(tnode)
          value_fix: longint;
          constructor create(v : longint;def : pdef);virtual;
          function getcopy : tnode;override;
          function pass_1 : tnode;override;
       end;

       tordconstnode = class(tnode)
          value : TConstExprInt;
          constructor create(v : tconstexprint;def : pdef);virtual;
          function getcopy : tnode;override;
          function pass_1 : tnode;override;
       end;

       tpointerconstnode = class(tnode)
          value : TPointerOrd;
          constructor create(v : tpointerord;def : pdef);virtual;
          function getcopy : tnode;override;
          function pass_1 : tnode;override;
       end;

       tstringconstnode = class(tnode)
          value_str : pchar;
          len : longint;
          lab_str : pasmlabel;
          stringtype : tstringtype;
          constructor createstr(const s : string;st:tstringtype);virtual;
          constructor createpchar(s : pchar;l : longint);virtual;
          constructor createwstr(const w : tcompilerwidestring);virtual;
          destructor destroy;override;
          function getcopy : tnode;override;
          function pass_1 : tnode;override;
          function getpcharcopy : pchar;
       end;

       tsetconstnode = class(tunarynode)
          value_set : pconstset;
          lab_set : pasmlabel;
          constructor create(s : pconstset;settype : psetdef);virtual;
          destructor destroy;override;
          function getcopy : tnode;override;
          function pass_1 : tnode;override;
       end;

       tnilnode = class(tnode)
          constructor create;virtual;
          function pass_1 : tnode;override;
       end;

    var
       crealconstnode : class of trealconstnode;
       cfixconstnode : class of tfixconstnode;
       cordconstnode : class of tordconstnode;
       cpointerconstnode : class of tpointerconstnode;
       cstringconstnode : class of tstringconstnode;
       csetconstnode : class of tsetconstnode;
       cnilnode : class of tnilnode;

    function genordinalconstnode(v : TConstExprInt;def : pdef) : tordconstnode;
    { same as genordinalconstnode, but the resulttype }
    { is determines automatically                     }
    function genintconstnode(v : TConstExprInt) : tordconstnode;
    function genpointerconstnode(v : tpointerord;def : pdef) : tpointerconstnode;
    function genenumnode(v : penumsym) : tordconstnode;
    function genfixconstnode(v : longint;def : pdef) : tfixconstnode;
    function genrealconstnode(v : bestreal;def : pdef) : trealconstnode;
    { allow pchar or string for defining a pchar node }
    function genstringconstnode(const s : string;st:tstringtype) : tstringconstnode;
    { length is required for ansistrings }
    function genpcharconstnode(s : pchar;length : longint) : tstringconstnode;
    function genwstringconstnode(const w : tcompilerwidestring) : tnode;
    function gensetconstnode(s : pconstset;settype : psetdef) : tsetconstnode;

    { some helper routines }

{$ifdef INT64FUNCRESOK}
    function get_ordinal_value(p : tnode) : TConstExprInt;
{$else INT64FUNCRESOK}
    function get_ordinal_value(p : tnode) : longint;
{$endif INT64FUNCRESOK}
    function is_constnode(p : tnode) : boolean;
    function is_constintnode(p : tnode) : boolean;
    function is_constcharnode(p : tnode) : boolean;
    function is_constrealnode(p : tnode) : boolean;
    function is_constboolnode(p : tnode) : boolean;
    function is_constresourcestringnode(p : tnode) : boolean;
    function str_length(p : tnode) : longint;
    function is_emptyset(p : tnode):boolean;
    function genconstsymtree(p : pconstsym) : tnode;

implementation

    uses
      cutils,verbose,globals,systems,
      types,cpubase,nld;

    function genordinalconstnode(v : tconstexprint;def : pdef) : tordconstnode;
      begin
         genordinalconstnode:=cordconstnode.create(v,def);
      end;


    function genintconstnode(v : TConstExprInt) : tordconstnode;

      var
         i : TConstExprInt;

      begin
         { we need to bootstrap this code, so it's a little bit messy }
         i:=2147483647;
         if (v<=i) and (v>=-i-1) then
           genintconstnode:=genordinalconstnode(v,s32bitdef)
         else
           genintconstnode:=genordinalconstnode(v,cs64bitdef);
      end;


    function genpointerconstnode(v : tpointerord;def : pdef) : tpointerconstnode;
      begin
         genpointerconstnode:=cpointerconstnode.create(v,def);
      end;


    function genenumnode(v : penumsym) : tordconstnode;
      begin
         genenumnode:=cordconstnode.create(v^.value,v^.definition);
      end;


    function gensetconstnode(s : pconstset;settype : psetdef) : tsetconstnode;
      begin
         gensetconstnode:=csetconstnode.create(s,settype);
      end;


    function genrealconstnode(v : bestreal;def : pdef) : trealconstnode;
      begin
         genrealconstnode:=crealconstnode.create(v,def);
      end;


    function genfixconstnode(v : longint;def : pdef) : tfixconstnode;
      begin
         genfixconstnode:=cfixconstnode.create(v,def);
      end;


    function genstringconstnode(const s : string;st:tstringtype) : tstringconstnode;
      begin
         genstringconstnode:=cstringconstnode.createstr(s,st);
      end;

    function genwstringconstnode(const w : tcompilerwidestring) : tnode;

      begin
         genwstringconstnode:=cstringconstnode.createwstr(w);
      end;


    function genpcharconstnode(s : pchar;length : longint) : tstringconstnode;
      begin
         genpcharconstnode:=cstringconstnode.createpchar(s,length);
      end;

{$ifdef INT64FUNCRESOK}
    function get_ordinal_value(p : tnode) : TConstExprInt;
{$else INT64FUNCRESOK}
    function get_ordinal_value(p : tnode) : longint;
{$endif INT64FUNCRESOK}
      begin
         if p.nodetype=ordconstn then
           get_ordinal_value:=tordconstnode(p).value
         else
           begin
             Message(type_e_ordinal_expr_expected);
             get_ordinal_value:=0;
           end;
      end;


    function is_constnode(p : tnode) : boolean;
      begin
        is_constnode:=(p.nodetype in [ordconstn,realconstn,stringconstn,fixconstn,setconstn]);
      end;


    function is_constintnode(p : tnode) : boolean;
      begin
         is_constintnode:=(p.nodetype=ordconstn) and is_integer(p.resulttype);
      end;


    function is_constcharnode(p : tnode) : boolean;

      begin
         is_constcharnode:=(p.nodetype=ordconstn) and is_char(p.resulttype);
      end;


    function is_constrealnode(p : tnode) : boolean;

      begin
         is_constrealnode:=(p.nodetype=realconstn);
      end;


    function is_constboolnode(p : tnode) : boolean;

      begin
         is_constboolnode:=(p.nodetype=ordconstn) and is_boolean(p.resulttype);
      end;


    function is_constresourcestringnode(p : tnode) : boolean;
      begin
        is_constresourcestringnode:=(p.nodetype=loadn) and
          (tloadnode(p).symtableentry^.typ=constsym) and
          (pconstsym(tloadnode(p).symtableentry)^.consttyp=constresourcestring);
      end;


    function str_length(p : tnode) : longint;

      begin
         str_length:=tstringconstnode(p).len;
      end;


    function is_emptyset(p : tnode):boolean;

      var
        i : longint;
      begin
        i:=0;
        if p.nodetype=setconstn then
         begin
           while (i<32) and (tsetconstnode(p).value_set^[i]=0) do
            inc(i);
         end;
        is_emptyset:=(i=32);
      end;


    function genconstsymtree(p : pconstsym) : tnode;
      var
        p1  : tnode;
        len : longint;
        pc  : pchar;
      begin
        p1:=nil;
        case p^.consttyp of
          constint :
            p1:=genordinalconstnode(p^.value,s32bitdef);
          conststring :
            begin
              len:=p^.len;
              if not(cs_ansistrings in aktlocalswitches) and (len>255) then
               len:=255;
              getmem(pc,len+1);
              move(pchar(tpointerord(p^.value))^,pc^,len);
              pc[len]:=#0;
              p1:=genpcharconstnode(pc,len);
            end;
          constchar :
            p1:=genordinalconstnode(p^.value,cchardef);
          constreal :
            p1:=genrealconstnode(pbestreal(tpointerord(p^.value))^,bestrealdef^);
          constbool :
            p1:=genordinalconstnode(p^.value,booldef);
          constset :
            p1:=gensetconstnode(pconstset(tpointerord(p^.value)),psetdef(p^.consttype.def));
          constord :
            p1:=genordinalconstnode(p^.value,p^.consttype.def);
          constpointer :
            p1:=genpointerconstnode(p^.value,p^.consttype.def);
          constnil :
            p1:=cnilnode.create;
          constresourcestring:
            begin
              p1:=genloadnode(pvarsym(p),pvarsym(p)^.owner);
              p1.resulttype:=cansistringdef;
            end;
        end;
        genconstsymtree:=p1;
      end;

{*****************************************************************************
                             TREALCONSTNODE
*****************************************************************************}

    constructor trealconstnode.create(v : bestreal;def : pdef);

      begin
         inherited create(realconstn);
         resulttype:=def;
         value_real:=v;
         lab_real:=nil;
      end;

    function trealconstnode.getcopy : tnode;

      var
         n : trealconstnode;

      begin
         n:=trealconstnode(inherited getcopy);
         n.value_real:=value_real;
         n.lab_real:=lab_real;
         getcopy:=n;
      end;

    function trealconstnode.pass_1 : tnode;
      begin
         pass_1:=nil;
         if (value_real=1.0) or (value_real=0.0) then
           begin
              location.loc:=LOC_FPU;
              registersfpu:=1;
           end
         else
           location.loc:=LOC_MEM;
      end;


{*****************************************************************************
                             TFIXCONSTNODE
*****************************************************************************}

    constructor tfixconstnode.create(v : longint;def : pdef);

      begin
         inherited create(fixconstn);
         resulttype:=def;
         value_fix:=v;
      end;

    function tfixconstnode.getcopy : tnode;

      var
         n : tfixconstnode;

      begin
         n:=tfixconstnode(inherited getcopy);
         n.value_fix:=value_fix;
         getcopy:=n;
      end;

    function tfixconstnode.pass_1 : tnode;

      begin
         pass_1:=nil;
         location.loc:=LOC_MEM;
      end;


{*****************************************************************************
                              TORDCONSTNODE
*****************************************************************************}

    constructor tordconstnode.create(v : tconstexprint;def : pdef);

      begin
         inherited create(ordconstn);
         value:=v;
         resulttype:=def;
         if resulttype^.deftype=orddef then
          testrange(resulttype,value);
      end;

    function tordconstnode.getcopy : tnode;

      var
         n : tordconstnode;

      begin
         n:=tordconstnode(inherited getcopy);
         n.value:=value;
         getcopy:=n;
      end;

    function tordconstnode.pass_1 : tnode;
      begin
         pass_1:=nil;
         location.loc:=LOC_MEM;
      end;


{*****************************************************************************
                            TPOINTERCONSTNODE
*****************************************************************************}

    constructor tpointerconstnode.create(v : tpointerord;def : pdef);

      begin
         inherited create(pointerconstn);
         value:=v;
         resulttype:=def;
      end;

    function tpointerconstnode.getcopy : tnode;

      var
         n : tpointerconstnode;

      begin
         n:=tpointerconstnode(inherited getcopy);
         n.value:=value;
         getcopy:=n;
      end;

    function tpointerconstnode.pass_1 : tnode;
      begin
         pass_1:=nil;
         location.loc:=LOC_MEM;
      end;


{*****************************************************************************
                             TSTRINGCONSTNODE
*****************************************************************************}

    constructor tstringconstnode.createstr(const s : string;st:tstringtype);

      var
         l : longint;

      begin
         inherited create(stringconstn);
         l:=length(s);
         len:=l;
         { stringdup write even past a #0 }
         getmem(value_str,l+1);
         move(s[1],value_str^,l);
         value_str[l]:=#0;
         lab_str:=nil;
         if st=st_default then
          begin
            if cs_ansistrings in aktlocalswitches then
              stringtype:=st_ansistring
            else
              stringtype:=st_shortstring;
          end
         else
          stringtype:=st;
         case stringtype of
           st_shortstring :
             resulttype:=cshortstringdef;
           st_ansistring :
             resulttype:=cansistringdef;
           else
             internalerror(44990099);
         end;
      end;

    constructor tstringconstnode.createwstr(const w : tcompilerwidestring);

      begin
         inherited create(stringconstn);
         len:=getlengthwidestring(w);
         new(pcompilerwidestring(value_str));
         initwidestring(pcompilerwidestring(value_str)^);
         copywidestring(w,pcompilerwidestring(value_str)^);
         lab_str:=nil;
         stringtype:=st_widestring;
         resulttype:=cwidestringdef;
      end;

    constructor tstringconstnode.createpchar(s : pchar;l : longint);

      begin
         inherited create(stringconstn);
         len:=l;
         if (cs_ansistrings in aktlocalswitches) or
            (len>255) then
          begin
             stringtype:=st_ansistring;
             resulttype:=cansistringdef;
          end
         else
          begin
             stringtype:=st_shortstring;
             resulttype:=cshortstringdef;
          end;
         value_str:=s;
         lab_str:=nil;
      end;

    destructor tstringconstnode.destroy;
      begin
        ansistringdispose(value_str,len);
        inherited destroy;
      end;

    function tstringconstnode.getcopy : tnode;

      var
         n : tstringconstnode;

      begin
         n:=tstringconstnode(inherited getcopy);
         n.stringtype:=stringtype;
         n.len:=len;
         n.lab_str:=lab_str;
         if stringtype=st_widestring then
           copywidestring(pcompilerwidestring(value_str)^,
             pcompilerwidestring(n.value_str)^)
         else
           n.value_str:=getpcharcopy;
         getcopy:=n;
      end;

    function tstringconstnode.pass_1 : tnode;
      begin
         pass_1:=nil;
        case stringtype of
          st_shortstring :
            resulttype:=cshortstringdef;
          st_ansistring :
            resulttype:=cansistringdef;
          st_widestring :
            resulttype:=cwidestringdef;
          st_longstring :
            resulttype:=clongstringdef;
        end;
        location.loc:=LOC_MEM;
      end;

    function tstringconstnode.getpcharcopy : pchar;
      var
         pc : pchar;
      begin
         pc:=nil;
         getmem(pc,len+1);
         if pc=nil then
           Message(general_f_no_memory_left);
         move(value_str^,pc^,len+1);
         getpcharcopy:=pc;
      end;


{*****************************************************************************
                             TSETCONSTNODE
*****************************************************************************}

    constructor tsetconstnode.create(s : pconstset;settype : psetdef);

      begin
         inherited create(setconstn,nil);
         resulttype:=settype;
         if assigned(s) then
           begin
              new(value_set);
              value_set^:=s^;
           end
         else
           value_set:=nil;
      end;

    destructor tsetconstnode.destroy;
      begin
        if assigned(value_set) then
         dispose(value_set);
        inherited destroy;
      end;

    function tsetconstnode.getcopy : tnode;

      var
         n : tsetconstnode;

      begin
         n:=tsetconstnode(inherited getcopy);
         if assigned(value_set) then
           begin
              new(n.value_set);
              n.value_set^:=value_set^
           end
         else
           n.value_set:=nil;
         n.lab_set:=lab_set;
         getcopy:=n;
      end;

    function tsetconstnode.pass_1 : tnode;
      begin
         pass_1:=nil;
         location.loc:=LOC_MEM;
      end;

{*****************************************************************************
                               TNILNODE
*****************************************************************************}

    constructor tnilnode.create;

      begin
         inherited create(niln);
      end;

    function tnilnode.pass_1 : tnode;
      begin
        pass_1:=nil;
        resulttype:=voidpointerdef;
        location.loc:=LOC_MEM;
      end;

begin
   crealconstnode:=trealconstnode;
   cfixconstnode:=tfixconstnode;
   cordconstnode:=tordconstnode;
   cpointerconstnode:=tpointerconstnode;
   cstringconstnode:=tstringconstnode;
   csetconstnode:=tsetconstnode;
   cnilnode:=tnilnode;
end.
{
  $Log$
  Revision 1.13  2000-12-15 13:26:01  jonas
    * only return int64's from functions if it int64funcresok is defined
    + added int64funcresok define to options.pas

  Revision 1.12  2000/12/07 17:19:42  jonas
    * new constant handling: from now on, hex constants >$7fffffff are
      parsed as unsigned constants (otherwise, $80000000 got sign extended
      and became $ffffffff80000000), all constants in the longint range
      become longints, all constants >$7fffffff and <=cardinal($ffffffff)
      are cardinals and the rest are int64's.
    * added lots of longint typecast to prevent range check errors in the
      compiler and rtl
    * type casts of symbolic ordinal constants are now preserved
    * fixed bug where the original resulttype wasn't restored correctly
      after doing a 64bit rangecheck

  Revision 1.11  2000/11/29 00:30:32  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.10  2000/10/31 22:02:48  peter
    * symtable splitted, no real code changes

  Revision 1.9  2000/10/14 21:52:55  peter
    * fixed memory leaks

  Revision 1.8  2000/10/14 10:14:50  peter
    * moehrendorf oct 2000 rewrite

  Revision 1.7  2000/09/28 19:49:52  florian
  *** empty log message ***

  Revision 1.6  2000/09/27 20:25:44  florian
    * more stuff fixed

  Revision 1.5  2000/09/27 18:14:31  florian
    * fixed a lot of syntax errors in the n*.pas stuff

  Revision 1.4  2000/09/26 14:59:34  florian
    * more conversion work done

  Revision 1.3  2000/09/24 21:15:34  florian
    * some errors fix to get more stuff compilable

  Revision 1.2  2000/09/24 15:06:19  peter
    * use defines.inc

  Revision 1.1  2000/09/22 21:44:48  florian
    + initial revision

}
