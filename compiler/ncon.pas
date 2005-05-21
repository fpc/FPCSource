{
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
          constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure buildderefimpl;override;
          procedure derefimpl;override;
          function getcopy : tnode;override;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
          function docompare(p: tnode) : boolean; override;
          procedure printnodedata(var t:text);override;
       end;
       trealconstnodeclass = class of trealconstnode;

       tordconstnode = class(tnode)
          restype : ttype;
          value : TConstExprInt;
          rangecheck : boolean;
          { create an ordinal constant node of the specified type and value.
            _rangecheck determines if the value of the ordinal should be checked
            against the ranges of the type definition.
          }
          constructor create(v : tconstexprint;const t:ttype; _rangecheck : boolean);virtual;
          constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure buildderefimpl;override;
          procedure derefimpl;override;
          function getcopy : tnode;override;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
          function docompare(p: tnode) : boolean; override;
          procedure printnodedata(var t:text);override;
       end;
       tordconstnodeclass = class of tordconstnode;

       tpointerconstnode = class(tnode)
          restype : ttype;
          value   : TConstPtrUInt;
          constructor create(v : TConstPtrUInt;const t:ttype);virtual;
          constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure buildderefimpl;override;
          procedure derefimpl;override;
          function getcopy : tnode;override;
          function pass_1 : tnode;override;
          function det_resulttype:tnode;override;
          function docompare(p: tnode) : boolean; override;
       end;
       tpointerconstnodeclass = class of tpointerconstnode;

       tstringconstnode = class(tnode)
          value_str : pchar;
          len     : longint;
          lab_str : tasmlabel;
          st_type : tstringtype;
          constructor createstr(const s : string;st:tstringtype);virtual;
          constructor createpchar(s : pchar;l : longint);virtual;
          constructor createwstr(w : pcompilerwidestring);virtual;
          constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure buildderefimpl;override;
          procedure derefimpl;override;
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
          constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
          procedure buildderefimpl;override;
          procedure derefimpl;override;
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
          constructor ppuload(t:tnodetype;ppufile:tcompilerppufile);override;
          procedure ppuwrite(ppufile:tcompilerppufile);override;
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
    function is_constresourcestringnode(p : tnode) : boolean;
    function str_length(p : tnode) : longint;
    function is_emptyset(p : tnode):boolean;
    function genconstsymtree(p : tconstsym) : tnode;

implementation

    uses
      cutils,
      verbose,systems,
      defutil,
      cpubase,cgbase,
      nld;

    function genintconstnode(v : TConstExprInt) : tordconstnode;
      var
        htype : ttype;
      begin
         int_to_type(v,htype);
         genintconstnode:=cordconstnode.create(v,htype,true);
      end;


    function genenumnode(v : tenumsym) : tordconstnode;
      var
        htype : ttype;
      begin
         htype.setdef(v.definition);
         genenumnode:=cordconstnode.create(v.value,htype,true);
      end;


{$ifdef INT64FUNCRESOK}
    function get_ordinal_value(p : tnode) : TConstExprInt;
{$else INT64FUNCRESOK}
    function get_ordinal_value(p : tnode) : longint;
{$endif INT64FUNCRESOK}
      begin
        get_ordinal_value:=0;
        if is_constnode(p) then
          begin
            if p.nodetype=ordconstn then
              get_ordinal_value:=tordconstnode(p).value
            else
              Message(type_e_ordinal_expr_expected);
          end
        else
          Message(type_e_constant_expr_expected);
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
      begin
        is_emptyset:=(p.nodetype=setconstn) and
                     (Tsetconstnode(p).value_set^=[]);
      end;


    function genconstsymtree(p : tconstsym) : tnode;
      var
        p1  : tnode;
        len : longint;
        pc  : pchar;
      begin
        p1:=nil;
        case p.consttyp of
          constord :
            p1:=cordconstnode.create(p.value.valueord,p.consttype,true);
          conststring :
            begin
              len:=p.value.len;
              if not(cs_ansistrings in aktlocalswitches) and (len>255) then
               len:=255;
              getmem(pc,len+1);
              move(pchar(p.value.valueptr)^,pc^,len);
              pc[len]:=#0;
              p1:=cstringconstnode.createpchar(pc,len);
            end;
          constreal :
            p1:=crealconstnode.create(pbestreal(p.value.valueptr)^,pbestrealtype^);
          constset :
            p1:=csetconstnode.create(pconstset(p.value.valueptr),p.consttype);
          constpointer :
            p1:=cpointerconstnode.create(p.value.valueordptr,p.consttype);
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

    constructor trealconstnode.ppuload(t:tnodetype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        ppufile.gettype(restype);
        value_real:=ppufile.getreal;
        lab_real:=tasmlabel(ppufile.getasmsymbol);
      end;


    procedure trealconstnode.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.puttype(restype);
        ppufile.putreal(value_real);
        ppufile.putasmsymbol(lab_real);
      end;


    procedure trealconstnode.buildderefimpl;
      begin
        inherited buildderefimpl;
        restype.buildderef;
      end;


    procedure trealconstnode.derefimpl;
      begin
        inherited derefimpl;
        restype.resolve;
        objectlibrary.derefasmsymbol(tasmsymbol(lab_real));
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
         expectloc:=LOC_CREFERENCE;
         { needs to be loaded into an FPU register }
         registersfpu:=1;
      end;

    function trealconstnode.docompare(p: tnode): boolean;
      begin
        docompare :=
          inherited docompare(p) and
          (value_real = trealconstnode(p).value_real) and
          { floating point compares for non-numbers give strange results usually }
          is_number_float(value_real) and
          is_number_float(trealconstnode(p).value_real);
      end;


    procedure Trealconstnode.printnodedata(var t:text);
      begin
        inherited printnodedata(t);
        writeln(t,printnodeindention,'value = ',value_real);
      end;


{*****************************************************************************
                              TORDCONSTNODE
*****************************************************************************}

    constructor tordconstnode.create(v : tconstexprint;const t:ttype;_rangecheck : boolean);

      begin
         inherited create(ordconstn);
         value:=v;
         restype:=t;
         rangecheck := _rangecheck;
      end;


    constructor tordconstnode.ppuload(t:tnodetype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        ppufile.gettype(restype);
        value:=ppufile.getexprint;
        { normally, the value is already compiled, so we don't need
          to do once again a range check
        }
        rangecheck := false;
      end;


    procedure tordconstnode.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.puttype(restype);
        ppufile.putexprint(value);
      end;


    procedure tordconstnode.buildderefimpl;
      begin
        inherited buildderefimpl;
        restype.buildderef;
      end;


    procedure tordconstnode.derefimpl;
      begin
        inherited derefimpl;
        restype.resolve;
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
        { only do range checking when explicitly asked for it }
        if rangecheck then
           testrange(resulttype.def,value,false);
      end;

    function tordconstnode.pass_1 : tnode;
      begin
         result:=nil;
         expectloc:=LOC_CONSTANT;
      end;

    function tordconstnode.docompare(p: tnode): boolean;
      begin
        docompare :=
          inherited docompare(p) and
          (value = tordconstnode(p).value);
      end;


    procedure Tordconstnode.printnodedata(var t:text);
      begin
        inherited printnodedata(t);
        writeln(t,printnodeindention,'value = ',value);
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


    constructor tpointerconstnode.ppuload(t:tnodetype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        ppufile.gettype(restype);
        value:=ppufile.getptruint;
      end;


    procedure tpointerconstnode.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.puttype(restype);
        ppufile.putptruint(value);
      end;


    procedure tpointerconstnode.buildderefimpl;
      begin
        inherited buildderefimpl;
        restype.buildderef;
      end;


    procedure tpointerconstnode.derefimpl;
      begin
        inherited derefimpl;
        restype.resolve;
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
         expectloc:=LOC_CONSTANT;
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
            {$ifdef ansistring_bits}
              case aktansistring_bits of
                sb_16:
                  st_type:=st_ansistring16;
                sb_32:
                  st_type:=st_ansistring32;
                sb_64:
                  st_type:=st_ansistring64;
              end
            {$else}
              st_type:=st_ansistring
            {$endif}
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
          {$ifdef ansistring_bits}
            case aktansistring_bits of
              sb_16:
                st_type:=st_ansistring16;
              sb_32:
                st_type:=st_ansistring32;
              sb_64:
                st_type:=st_ansistring64;
            end
          {$else}
            st_type:=st_ansistring
          {$endif}
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


    constructor tstringconstnode.ppuload(t:tnodetype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        st_type:=tstringtype(ppufile.getbyte);
        len:=ppufile.getlongint;
        getmem(value_str,len+1);
        ppufile.getdata(value_str^,len);
        value_str[len]:=#0;
        lab_str:=tasmlabel(ppufile.getasmsymbol);
      end;


    procedure tstringconstnode.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.putbyte(byte(st_type));
        ppufile.putlongint(len);
        ppufile.putdata(value_str^,len);
        ppufile.putasmsymbol(lab_str);
      end;


    procedure tstringconstnode.buildderefimpl;
      begin
        inherited buildderefimpl;
      end;


    procedure tstringconstnode.derefimpl;
      begin
        inherited derefimpl;
        objectlibrary.derefasmsymbol(tasmsymbol(lab_str));
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
        {$ifdef ansistring_bits}
          st_ansistring16:
            resulttype:=cansistringtype16;
          st_ansistring32:
            resulttype:=cansistringtype32;
          st_ansistring64:
            resulttype:=cansistringtype64;
        {$else}
          st_ansistring :
            resulttype:=cansistringtype;
        {$endif}
          st_widestring :
            resulttype:=cwidestringtype;
          st_longstring :
            resulttype:=clongstringtype;
        end;
      end;

    function tstringconstnode.pass_1 : tnode;
      begin
        result:=nil;
      {$ifdef ansistring_bits}
        if (st_type in [st_ansistring16,st_ansistring32,st_ansistring64,st_widestring]) and
      {$else}
        if (st_type in [st_ansistring,st_widestring]) and
      {$endif}
           (len=0) then
         expectloc:=LOC_CONSTANT
        else
         expectloc:=LOC_CREFERENCE;
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


    constructor tsetconstnode.ppuload(t:tnodetype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        ppufile.gettype(restype);
        new(value_set);
        ppufile.getdata(value_set^,sizeof(tconstset));
      end;


    procedure tsetconstnode.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.puttype(restype);
        ppufile.putdata(value_set^,sizeof(tconstset));
      end;


    procedure tsetconstnode.buildderefimpl;
      begin
        inherited buildderefimpl;
        restype.buildderef;
      end;


    procedure tsetconstnode.derefimpl;
      begin
        inherited derefimpl;
        restype.resolve;
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
          expectloc:=LOC_CONSTANT
         else
          expectloc:=LOC_CREFERENCE;
      end;


    function tsetconstnode.docompare(p: tnode): boolean;
      begin
        docompare:=(inherited docompare(p)) and
                   (value_set^=Tsetconstnode(p).value_set^);
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
        expectloc:=LOC_CONSTANT;
      end;

{*****************************************************************************
                            TGUIDCONSTNODE
*****************************************************************************}

    constructor tguidconstnode.create(const g:tguid);

      begin
         inherited create(guidconstn);
         value:=g;
      end;

    constructor tguidconstnode.ppuload(t:tnodetype;ppufile:tcompilerppufile);
      begin
        inherited ppuload(t,ppufile);
        ppufile.getguid(value);
      end;


    procedure tguidconstnode.ppuwrite(ppufile:tcompilerppufile);
      begin
        inherited ppuwrite(ppufile);
        ppufile.putguid(value);
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
         expectloc:=LOC_CREFERENCE;
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
