{
    $Id$
    Copyright (c) 2000-2002 by Florian Klaempfl

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

{$i fpcdefs.inc}

interface

    uses
      globtype,widestr,
      node,
      aasmbase,aasmtai,cpuinfo,globals,
      symconst,symtype,symdef,symsym;

    type
       trealconstnode = class(tnode)
          restype : ttype;
          value_real : bestreal;
          lab_real : tasmlabel;
          constructor create(v : bestreal;const t:ttype);virtual;
          function getcopy : tnode;override;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
          function docompare(p: tnode) : boolean; override;
       end;
       trealconstnodeclass = class of trealconstnode;

       tordconstnode = class(tnode)
          restype : ttype;
          value : TConstExprInt;
          constructor create(v : tconstexprint;const t:ttype);virtual;
          function getcopy : tnode;override;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
          function docompare(p: tnode) : boolean; override;
       end;
       tordconstnodeclass = class of tordconstnode;

       tpointerconstnode = class(tnode)
          restype : ttype;
          value   : TConstPtrUInt;
          constructor create(v : TConstPtrUInt;const t:ttype);virtual;
          function getcopy : tnode;override;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
          function docompare(p: tnode) : boolean; override;
       end;
       tpointerconstnodeclass = class of tpointerconstnode;

       tstringconstnode = class(tnode)
          value_str : pchar;
          len : longint;
          lab_str : tasmlabel;
          st_type : tstringtype;
          constructor createstr(const s : string;st:tstringtype);virtual;
          constructor createpchar(s : pchar;l : longint);virtual;
          constructor createwstr(w : pcompilerwidestring);virtual;
          destructor destroy;override;
          function getcopy : tnode;override;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
          function getpcharcopy : pchar;
          function docompare(p: tnode) : boolean; override;
       end;
       tstringconstnodeclass = class of tstringconstnode;

       tsetconstnode = class(tunarynode)
          restype : ttype;
          value_set : pconstset;
          lab_set : tasmlabel;
          constructor create(s : pconstset;const t:ttype);virtual;
          destructor destroy;override;
          function getcopy : tnode;override;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
          function docompare(p: tnode) : boolean; override;
       end;
       tsetconstnodeclass = class of tsetconstnode;

       tnilnode = class(tnode)
          constructor create;virtual;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
       end;
       tnilnodeclass = class of tnilnode;

       tguidconstnode = class(tnode)
          value : tguid;
          constructor create(const g:tguid);virtual;
          function getcopy : tnode;override;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
          function docompare(p: tnode) : boolean; override;
       end;
       tguidconstnodeclass = class of tguidconstnode;

    var
       crealconstnode : trealconstnodeclass;
       cordconstnode : tordconstnodeclass;
       cpointerconstnode : tpointerconstnodeclass;
       cstringconstnode : tstringconstnodeclass;
       csetconstnode : tsetconstnodeclass;
       cguidconstnode : tguidconstnodeclass;
       cnilnode : tnilnodeclass;

    function genintconstnode(v : TConstExprInt) : tordconstnode;
    function genenumnode(v : tenumsym) : tordconstnode;

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
    function is_constwidecharnode(p : tnode) : boolean;
    function str_length(p : tnode) : longint;
    function is_emptyset(p : tnode):boolean;
    function genconstsymtree(p : tconstsym) : tnode;

implementation

    uses
      cutils,verbose,systems,
      types,cpubase,nld;

    function genintconstnode(v : TConstExprInt) : tordconstnode;

      var
         i,i2 : TConstExprInt;

      begin
         { we need to bootstrap this code, so it's a little bit messy }
         i:=2147483647;
         { maxcardinal }
         i2 := i+i+1;
         if (v<=i) and (v>=-i-1) then
           genintconstnode:=cordconstnode.create(v,s32bittype)
         else if (v > i) and (v <= i2) then
           genintconstnode:=cordconstnode.create(v,u32bittype)
         else
           genintconstnode:=cordconstnode.create(v,cs64bittype);
      end;


    function genenumnode(v : tenumsym) : tordconstnode;
      var
        htype : ttype;
      begin
         htype.setdef(v.definition);
         genenumnode:=cordconstnode.create(v.value,htype);
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
        is_constnode:=(p.nodetype in [ordconstn,realconstn,stringconstn,setconstn,guidconstn]);
      end;


    function is_constintnode(p : tnode) : boolean;
      begin
         is_constintnode:=(p.nodetype=ordconstn) and is_integer(p.resulttype.def);
      end;


    function is_constcharnode(p : tnode) : boolean;

      begin
         is_constcharnode:=(p.nodetype=ordconstn) and is_char(p.resulttype.def);
      end;


    function is_constwidecharnode(p : tnode) : boolean;

      begin
         is_constwidecharnode:=(p.nodetype=ordconstn) and is_widechar(p.resulttype.def);
      end;


    function is_constrealnode(p : tnode) : boolean;

      begin
         is_constrealnode:=(p.nodetype=realconstn);
      end;


    function is_constboolnode(p : tnode) : boolean;

      begin
         is_constboolnode:=(p.nodetype=ordconstn) and is_boolean(p.resulttype.def);
      end;


    function is_constresourcestringnode(p : tnode) : boolean;
      begin
        is_constresourcestringnode:=(p.nodetype=loadn) and
          (tloadnode(p).symtableentry.typ=constsym) and
          (tconstsym(tloadnode(p).symtableentry).consttyp=constresourcestring);
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


    function genconstsymtree(p : tconstsym) : tnode;
      var
        p1  : tnode;
        len : longint;
        pc  : pchar;
      begin
        p1:=nil;
        case p.consttyp of
          constint :
            p1:=genintconstnode(p.valueord);
          conststring :
            begin
              len:=p.len;
              if not(cs_ansistrings in aktlocalswitches) and (len>255) then
               len:=255;
              getmem(pc,len+1);
              move(pchar(p.valueptr)^,pc^,len);
              pc[len]:=#0;
              p1:=cstringconstnode.createpchar(pc,len);
            end;
          constchar :
            p1:=cordconstnode.create(p.valueord,cchartype);
          constreal :
            p1:=crealconstnode.create(pbestreal(p.valueptr)^,pbestrealtype^);
          constbool :
            p1:=cordconstnode.create(p.valueord,booltype);
          constset :
            p1:=csetconstnode.create(pconstset(p.valueptr),p.consttype);
          constord :
            p1:=cordconstnode.create(p.valueord,p.consttype);
          constpointer :
            p1:=cpointerconstnode.create(p.valueordptr,p.consttype);
          constnil :
            p1:=cnilnode.create;
          else
            internalerror(200205103);
        end;
        genconstsymtree:=p1;
      end;

{*****************************************************************************
                             TREALCONSTNODE
*****************************************************************************}

    { generic code     }
    { overridden by:   }
    {   i386           }
    constructor trealconstnode.create(v : bestreal;const t:ttype);
      begin
         inherited create(realconstn);
         restype:=t;
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

    function trealconstnode.det_resulttype:tnode;
      begin
        result:=nil;
        resulttype:=restype;
      end;

    function trealconstnode.pass_1 : tnode;
      begin
         result:=nil;
         location.loc:=LOC_CREFERENCE;
         { needs to be loaded into an FPU register }
         registersfpu:=1;
      end;

    function trealconstnode.docompare(p: tnode): boolean;
      begin
        docompare :=
          inherited docompare(p) and
          (value_real = trealconstnode(p).value_real);
      end;

{*****************************************************************************
                              TORDCONSTNODE
*****************************************************************************}

    constructor tordconstnode.create(v : tconstexprint;const t:ttype);

      begin
         inherited create(ordconstn);
         value:=v;
         restype:=t;
      end;

    function tordconstnode.getcopy : tnode;

      var
         n : tordconstnode;

      begin
         n:=tordconstnode(inherited getcopy);
         n.value:=value;
         n.restype := restype;
         getcopy:=n;
      end;

    function tordconstnode.det_resulttype:tnode;
      begin
        result:=nil;
        resulttype:=restype;
        testrange(resulttype.def,value,false);
      end;

    function tordconstnode.pass_1 : tnode;
      begin
         result:=nil;
         if is_64bitint(resulttype.def) then
          location.loc:=LOC_CREFERENCE
         else
          location.loc:=LOC_CONSTANT;
      end;

    function tordconstnode.docompare(p: tnode): boolean;
      begin
        docompare :=
          inherited docompare(p) and
          (value = tordconstnode(p).value);
      end;

{*****************************************************************************
                            TPOINTERCONSTNODE
*****************************************************************************}

    constructor tpointerconstnode.create(v : TConstPtrUInt;const t:ttype);

      begin
         inherited create(pointerconstn);
         value:=v;
         restype:=t;
      end;

    function tpointerconstnode.getcopy : tnode;

      var
         n : tpointerconstnode;

      begin
         n:=tpointerconstnode(inherited getcopy);
         n.value:=value;
         n.restype := restype;
         getcopy:=n;
      end;

    function tpointerconstnode.det_resulttype:tnode;
      begin
        result:=nil;
        resulttype:=restype;
      end;

    function tpointerconstnode.pass_1 : tnode;
      begin
         result:=nil;
         location.loc:=LOC_CONSTANT;
      end;

    function tpointerconstnode.docompare(p: tnode): boolean;
      begin
        docompare :=
          inherited docompare(p) and
          (value = tpointerconstnode(p).value);
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
              st_type:=st_ansistring
            else
              st_type:=st_shortstring;
          end
         else
          st_type:=st;
      end;

    constructor tstringconstnode.createwstr(w : pcompilerwidestring);

      begin
         inherited create(stringconstn);
         len:=getlengthwidestring(w);
         initwidestring(pcompilerwidestring(value_str));
         copywidestring(w,pcompilerwidestring(value_str));
         lab_str:=nil;
         st_type:=st_widestring;
      end;

    constructor tstringconstnode.createpchar(s : pchar;l : longint);

      begin
         inherited create(stringconstn);
         len:=l;
         value_str:=s;
         if (cs_ansistrings in aktlocalswitches) or
            (len>255) then
          st_type:=st_ansistring
         else
          st_type:=st_shortstring;
         lab_str:=nil;
      end;

    destructor tstringconstnode.destroy;
      begin
        if st_type=st_widestring then
         donewidestring(pcompilerwidestring(value_str))
        else
         ansistringdispose(value_str,len);
        inherited destroy;
      end;

    function tstringconstnode.getcopy : tnode;

      var
         n : tstringconstnode;

      begin
         n:=tstringconstnode(inherited getcopy);
         n.st_type:=st_type;
         n.len:=len;
         n.lab_str:=lab_str;
         if st_type=st_widestring then
           begin
             initwidestring(pcompilerwidestring(n.value_str));
             copywidestring(pcompilerwidestring(value_str),pcompilerwidestring(n.value_str));
           end
         else
           n.value_str:=getpcharcopy;
         getcopy:=n;
      end;

    function tstringconstnode.det_resulttype:tnode;
      begin
        result:=nil;
        case st_type of
          st_shortstring :
            resulttype:=cshortstringtype;
          st_ansistring :
            resulttype:=cansistringtype;
          st_widestring :
            resulttype:=cwidestringtype;
          st_longstring :
            resulttype:=clongstringtype;
        end;
      end;

    function tstringconstnode.pass_1 : tnode;
      begin
        result:=nil;
        location.loc:=LOC_CREFERENCE;
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

    function tstringconstnode.docompare(p: tnode): boolean;
      begin
        docompare :=
          inherited docompare(p) and
          (len = tstringconstnode(p).len) and
          { Don't compare the pchars, since they may contain null chars }
          { Since all equal constant strings are replaced by the same   }
          { label, the following compare should be enough (JM)          }
          (lab_str = tstringconstnode(p).lab_str);
      end;

{*****************************************************************************
                             TSETCONSTNODE
*****************************************************************************}

    constructor tsetconstnode.create(s : pconstset;const t:ttype);

      begin
         inherited create(setconstn,nil);
         restype:=t;
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
         n.restype := restype;
         n.lab_set:=lab_set;
         getcopy:=n;
      end;

    function tsetconstnode.det_resulttype:tnode;
      begin
        result:=nil;
        resulttype:=restype;
      end;

    function tsetconstnode.pass_1 : tnode;
      begin
         result:=nil;
         if tsetdef(resulttype.def).settype=smallset then
          location.loc:=LOC_CONSTANT
         else
          location.loc:=LOC_CREFERENCE;
      end;

    function tsetconstnode.docompare(p: tnode): boolean;
      var
        i: 0..31;
      begin
        if inherited docompare(p) then
          begin
            for i := 0 to 31 do
              if (value_set^[i] <> tsetconstnode(p).value_set^[i]) then
                begin
                  docompare := false;
                  exit
                end;
            docompare := true;
          end
        else
          docompare := false;
      end;

{*****************************************************************************
                               TNILNODE
*****************************************************************************}

    constructor tnilnode.create;

      begin
        inherited create(niln);
      end;

    function tnilnode.det_resulttype:tnode;
      begin
        result:=nil;
        resulttype:=voidpointertype;
      end;

    function tnilnode.pass_1 : tnode;
      begin
        result:=nil;
        location.loc:=LOC_CONSTANT;
      end;

{*****************************************************************************
                            TGUIDCONSTNODE
*****************************************************************************}

    constructor tguidconstnode.create(const g:tguid);

      begin
         inherited create(guidconstn);
         value:=g;
      end;

    function tguidconstnode.getcopy : tnode;

      var
         n : tguidconstnode;

      begin
         n:=tguidconstnode(inherited getcopy);
         n.value:=value;
         getcopy:=n;
      end;

    function tguidconstnode.det_resulttype:tnode;
      begin
        result:=nil;
        resulttype.setdef(rec_tguid);
      end;

    function tguidconstnode.pass_1 : tnode;
      begin
         result:=nil;
         location.loc:=LOC_CREFERENCE;
      end;

    function tguidconstnode.docompare(p: tnode): boolean;
      begin
        docompare :=
          inherited docompare(p) and
          (guid2string(value) = guid2string(tguidconstnode(p).value));
      end;


begin
   crealconstnode:=trealconstnode;
   cordconstnode:=tordconstnode;
   cpointerconstnode:=tpointerconstnode;
   cstringconstnode:=tstringconstnode;
   csetconstnode:=tsetconstnode;
   cnilnode:=tnilnode;
   cguidconstnode:=tguidconstnode;
end.
{
  $Log$
  Revision 1.33  2002-07-01 18:46:23  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.32  2002/05/18 13:34:09  peter
    * readded missing revisions

  Revision 1.31  2002/05/16 19:46:37  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.29  2002/05/12 16:53:07  peter
    * moved entry and exitcode to ncgutil and cgobj
    * foreach gets extra argument for passing local data to the
      iterator function
    * -CR checks also class typecasts at runtime by changing them
      into as
    * fixed compiler to cycle with the -CR option
    * fixed stabs with elf writer, finally the global variables can
      be watched
    * removed a lot of routines from cga unit and replaced them by
      calls to cgobj
    * u32bit-s32bit updates for and,or,xor nodes. When one element is
      u32bit then the other is typecasted also to u32bit without giving
      a rangecheck warning/error.
    * fixed pascal calling method with reversing also the high tree in
      the parast, detected by tcalcst3 test

  Revision 1.28  2002/04/07 13:25:20  carl
  + change unit use

  Revision 1.27  2002/04/04 19:05:58  peter
    * removed unused units
    * use tlocation.size in cg.a_*loc*() routines

  Revision 1.26  2002/04/02 17:11:29  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods

  Revision 1.25  2002/03/04 19:10:11  peter
    * removed compiler warnings

}
