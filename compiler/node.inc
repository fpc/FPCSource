7{
    $Id$
    Copyright (c) 1999-2000 by Florian Klaempfl

    The implementation of the abstract nodes

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

{****************************************************************************
                                 TNODE
 ****************************************************************************}

    constructor tnode.create(tt : tnodetype);

      begin
         inherited create;
         nodetype:=tt;
         { this allows easier error tracing }
         location.loc:=LOC_INVALID;
         { save local info }
         fileinfo:=aktfilepos;
         localswitches:=aktlocalswitches;
         resulttype:=nil;
         registersint:=0;
         registersfpu:=0;
{$ifdef SUPPORT_MMX}
         registersmmx:=0;
{$endif SUPPORT_MMX}
         flags:=[];
      end;

    constructor tnode.createforcopy;

      begin
      end;

    destructor tnode.destroy;

      begin
         { reference info }
         if (location.loc in [LOC_MEM,LOC_REFERENCE]) and
            assigned(location.reference.symbol) then
           dispose(location.reference.symbol,done);
{$ifdef EXTDEBUG}
         if firstpasscount>maxfirstpasscount then
            maxfirstpasscount:=firstpasscount;
{$endif EXTDEBUG}
      end;

    function tnode.pass_1 : tnode;

      begin
         if not(assigned(resulttype)) then
           det_resulttype;

         det_temp;
      end;

    procedure tnode.concattolist(l : plinkedlist);

      begin
{$ifdef newcg}
         l^.concat(self);
{$endif newcg}
      end;

    function tnode.ischild(p : tnode) : boolean;

      begin
         ischild:=false;
      end;
{$ifdef EXTDEBUG}
    procedure tnode.dowrite;

      const treetype2str : array[tnodetype] of string[20] = (
          'addn',
          'muln',
          'subn',
          'divn',
          'symdifn',
          'modn',
          'assignn',
          'loadn',
          'rangen',
          'ltn',
          'lten',
          'gtn',
          'gten',
          'equaln',
          'unequaln',
          'inn',
          'orn',
          'xorn',
          'shrn',
          'shln',
          'slashn',
          'andn',
          'subscriptn',
          'derefn',
          'addrn',
          'doubleaddrn',
          'ordconstn',
          'typeconvn',
          'calln',
          'callparan',
          'realconstn',
          'fixconstn',
          'umminusn',
          'asmn',
          'vecn',
          'stringconstn',
          'funcretn',
          'selfn',
          'notn',
          'inlinen',
          'niln',
          'errorn',
          'typen',
          'hnewn',
          'hdisposen',
          'newn',
          'simpledisposen',
          'setelementn',
          'setconstn',
          'blockn',
          'statementn',
          'loopn',
          'ifn',
          'breakn',
          'continuen',
          'repeatn',
          'whilen',
          'forn',
          'exitn',
          'withn',
          'casen',
          'labeln',
          'goton',
          'simplenewn',
          'tryexceptn',
          'raisen',
          'switchesn',
          'tryfinallyn',
          'onn',
          'isn',
          'asn',
          'caretn',
          'failn',
          'starstarn',
          'procinlinen',
          'arrayconstructn',
          'arrayconstructrangen',
          'nothingn',
          'loadvmtn',
          'pointerconstn');

      begin
         write(indention,'(',treetype2str[nodetype]);
      end;
{$endif EXTDEBUG}

    function tnode.isequal(p : tnode) : boolean;

      begin
         isequal:=assigned(p) and (p.nodetype=nodetype) and
           (flags*flagsequal=p.flags*flagsequal) and
           docompare(p);
      end;

    function tnode.docompare(p : tnode) : boolean;

      begin
         docompare:=true;
      end;

    function tnode.getcopy : tnode;

      var
         p : tnode;

      begin
         { this is quite tricky because we need a node of the current }
         { node type and not one of tnode!                            }
         p:=classtype.createforcopy;
         p.nodetype:=nodetype;
         p.location:=location;
         p.varstateset:=varstateset;
         p.parent:=parent;
         p.flags:=flags;
         p.registers32:=registers32
         p.registersfpu:=registersfpu;
{$ifdef SUPPORT_MMX}
         p.registersmmx:=registersmmx;
         p.registerskni:=registerskni
{$endif SUPPORT_MMX}
         p.resulttype:=resulttype;
         p.fileinfo:=fileinfo;
         p.localswitches:=localswitches;
{$ifdef extdebug}
         p.firstpasscount:=firstpasscount;
{$endif extdebug}
         p.list:=list;
         getcopy:=p;
      end;

    procedure tnode.set_file_line(from : tnode);

      begin
         if assigned(from) then
           fileinfo:=from.fileinfo;
      end;

    procedure tnode.set_tree_filepos(const filepos : tfileposinfo);

      begin
         fileinfo:=filepos;
      end;

    procedure tnode.unset_varstate;

      begin
         internalerror(220920002);
      end;

    procedure tnode.set_varstate(must_be_valid : boolean);

      begin
         internalerror(220920001);
      end;

{$warning FIX ME !!!!!}
{$ifdef dummy}
    procedure unset_varstate(p : ptree);
      begin
        while assigned(p) do
         begin
           p^.varstateset:=false;
           case p^.treetype of
             typeconvn,
             subscriptn,
             vecn :
               p:=p^.left;
             else
               break;
           end;
         end;
      end;


    procedure set_varstate(p : ptree;must_be_valid : boolean);

      begin
         if not assigned(p) then
           exit
         else
           begin
             if p^.varstateset then
               exit;
              case p^.treetype of
           typeconvn :
             if p^.convtyp in
               [
                tc_cchar_2_pchar,
                tc_cstring_2_pchar,
                tc_array_2_pointer
               ] then
               set_varstate(p^.left,false)
             else if p^.convtyp in
               [
                tc_pchar_2_string,
                tc_pointer_2_array
               ] then
               set_varstate(p^.left,true)
             else
               set_varstate(p^.left,must_be_valid);
           subscriptn :
             set_varstate(p^.left,must_be_valid);
           vecn:
             begin
               if (p^.left^.resulttype^.deftype in [stringdef,arraydef]) then
                 set_varstate(p^.left,must_be_valid)
               else
                 set_varstate(p^.left,true);
               set_varstate(p^.right,true);
             end;
           { do not parse calln }
           calln : ;
           callparan:
             begin
               set_varstate(p^.left,must_be_valid);
               set_varstate(p^.right,must_be_valid);
             end;
           loadn :
         if (p^.symtableentry^.typ=varsym) then
          begin
            if must_be_valid and p^.is_first then
              begin
                if (pvarsym(p^.symtableentry)^.varstate=vs_declared_and_first_found) or
                   (pvarsym(p^.symtableentry)^.varstate=vs_set_but_first_not_passed) then
                 if (assigned(pvarsym(p^.symtableentry)^.owner) and
                    assigned(aktprocsym) and
                    (pvarsym(p^.symtableentry)^.owner = aktprocsym^.definition^.localst)) then
                  begin
                    if p^.symtable^.symtabletype=localsymtable then
                     CGMessage1(sym_n_uninitialized_local_variable,pvarsym(p^.symtableentry)^.name)
                    else
                     CGMessage1(sym_n_uninitialized_variable,pvarsym(p^.symtableentry)^.name);
                  end;
              end;
          if (p^.is_first) then
           begin
             if pvarsym(p^.symtableentry)^.varstate=vs_declared_and_first_found then
             { this can only happen at left of an assignment, no ? PM }
              if (parsing_para_level=0) and not must_be_valid then
               pvarsym(p^.symtableentry)^.varstate:=vs_assigned
              else
               pvarsym(p^.symtableentry)^.varstate:=vs_used;
             if pvarsym(p^.symtableentry)^.varstate=vs_set_but_first_not_passed then
               pvarsym(p^.symtableentry)^.varstate:=vs_used;
             p^.is_first:=false;
           end
         else
           begin
             if (pvarsym(p^.symtableentry)^.varstate=vs_assigned) and
                (must_be_valid or (parsing_para_level>0) or
                 (p^.resulttype^.deftype=procvardef)) then
               pvarsym(p^.symtableentry)^.varstate:=vs_used;
             if (pvarsym(p^.symtableentry)^.varstate=vs_declared_and_first_found) and
                (must_be_valid or (parsing_para_level>0) or
                (p^.resulttype^.deftype=procvardef)) then
               pvarsym(p^.symtableentry)^.varstate:=vs_set_but_first_not_passed;
           end;
         end;
         funcretn:
         begin
         { no claim if setting higher return value_str }
         if must_be_valid and
            (procinfo=pprocinfo(p^.funcretprocinfo)) and
            ((procinfo^.funcret_state=vs_declared) or
            ((p^.is_first_funcret) and
             (procinfo^.funcret_state=vs_declared_and_first_found))) then
           begin
             CGMessage(sym_w_function_result_not_set);
             { avoid multiple warnings }
             procinfo^.funcret_state:=vs_assigned;
           end;
         if p^.is_first_funcret and not must_be_valid then
           pprocinfo(p^.funcretprocinfo)^.funcret_state:=vs_assigned;
         end;
         else
           begin
             {internalerror(565656);}
           end;
         end;{case }
         p^.varstateset:=true;
      end;
    end;

{$endif}

{****************************************************************************
                                 TUNARYNODE
 ****************************************************************************}

    constructor tunarynode.create(tt : tnodetype;l : tnode);

      begin
         inherited create(tt);
         left:=l;
      end;

    function tunarynode.docompare(p : tnode) : boolean;

      begin
         docompare:=(inherited docompare(p)) and
           left.isequal(tunarynode(p).left);
      end;

    function.tunarynode.getcopy : tnode;

      var
         p : tunarynode;

      begin
         p:=tunarynode(inherited getcopy);
         p.left:=left.getcopy;
      end;

{$ifdef extdebug}
    procedure tunarynode.dowrite;

      begin
         inherited dowrite;
         writeln(',');
         writenode(left);
         writeln(')');
         dec(byte(indention[0]),2);
      end;
{$endif}

    procedure tunarynode.concattolist(l : plinkedlist);

      begin
         left.parent:=self;
         left.concattolist(l);
         inherited concattolist(l);
      end;

    function tunarynode.ischild(p : tnode) : boolean;

      begin
         ischild:=p=left;
      end;

    procedure tunarynode.det_resulttype;

      begin
         left.det_resulttype;
      end;

    procedure tunarynode.det_temp;

      begin
         left.det_temp;
      end;

{****************************************************************************
                            TBINARYNODE
 ****************************************************************************}

    constructor tbinarynode.create(tt : tnodetype;l,r : tnode);

      begin
         inherited create(tt,l);
         right:=r
      end;

    procedure tbinarynode.concattolist(l : plinkedlist);

      begin
         { we could change that depending on the number of }
         { required registers                              }
         left.parent:=self;
         left.concattolist(l);
         left.parent:=self;
         left.concattolist(l);
         inherited concattolist(l);
      end;

    function tbinarynode.ischild(p : tnode) : boolean;

      begin
         ischild:=(p=right) or (p=right);
      end;

    procedure tbinarynode.det_resulttype;

      begin
         left.det_resulttype;
         right.det_resulttype;
      end;

    procedure tbinarynode.det_temp;

      begin
         left.det_temp;
         right.det_temp;
      end;

    function tbinarynode.docompare(p : tnode) : boolean;

      begin
         docompare:=left.isequal(tbinarynode(p).left) and
           right.isequal(tbinarynode(p).right);
      end;

    function.tbinarynode.getcopy : tnode;

      var
         p : tbinarynode;

      begin
         p:=tbinarynode(inherited getcopy);
         p.right:=right.getcopy;
      end;

    function tbinarynode.isbinaryoverloaded(var t : tnode) : boolean;

     var
         rd,ld   : pdef;
         optoken : ttoken;

      begin
        t:=nil;
        isbinaryoverloaded:=false;
        { overloaded operator ? }
        { load easier access variables }
        rd:=right.resulttype;
        ld:=left.resulttype;
        if isbinaryoperatoroverloadable(ld,rd,voiddef,nodetype) then
          begin
             isbinaryoverloaded:=true;
             {!!!!!!!!! handle paras }
             case nodetype of
                { the nil as symtable signs firstcalln that this is
                  an overloaded operator }
                addn:
                  optoken:=_PLUS;
                subn:
                  optoken:=_MINUS;
                muln:
                  optoken:=_STAR;
                starstarn:
                  optoken:=_STARSTAR;
                slashn:
                  optoken:=_SLASH;
                ltn:
                  optoken:=tokens._lt;
                gtn:
                  optoken:=tokens._gt;
                lten:
                  optoken:=_lte;
                gten:
                  optoken:=_gte;
                equaln,unequaln :
                  optoken:=_EQUAL;
                symdifn :
                  optoken:=_SYMDIF;
                modn :
                  optoken:=_OP_MOD;
                orn :
                  optoken:=_OP_OR;
                xorn :
                  optoken:=_OP_XOR;
                andn :
                  optoken:=_OP_AND;
                divn :
                  optoken:=_OP_DIV;
                shln :
                  optoken:=_OP_SHL;
                shrn :
                  optoken:=_OP_SHR;
                else
                  exit;
             end;
             t:=gencallnode(overloaded_operators[optoken],nil);
             { we have to convert p^.left and p^.right into
              callparanodes }
             if tcallnode(t).symtableprocentry=nil then
               begin
                  CGMessage(parser_e_operator_not_overloaded);
                  t.free;
               end
             else
               begin
                  inc(tcallnode(t).symtableprocentry^.refs);
                  tcallnode(t).left:=gencallparanode(left,nil);
                  tcallnode(t).left:=gencallparanode(right,tcallnode(t).left);
                  if nodetype=unequaln then
                    t:=cnotnode.create(t);

                  firstpass(t);

                  putnode(p);
                  p:=t;
               end;
          end;
      end;

    procedure tbinarynode.swapleftright;

      var
         swapp : tnode;

      begin
         swapp:=right;
         right:=left;
         left:=swapp;
         if nf_swaped in flags then
           exclude(flags,nf_swaped)
         else
           include(flags,nf_swaped);
      end;

{****************************************************************************
                            TBINOPYNODE
 ****************************************************************************}

    constructor tbinopnode.create(tt : tnodetype;l,r : tnode);

      begin
         inherited create(tt,l,r);
      end;

    function tbinopnode.docompare(p : tnode) : boolean;

      begin
         docompare:=(inherited docompare(p)) or
           ((nf_swapable in flags) and
            left.isequal(tbinopnode(p).right) and
            right.isequal(tbinopnode(p).left));
      end;
{
  $Log$
  Revision 1.3  2000-09-22 21:45:36  florian
    * some updates e.g. getcopy added

  Revision 1.2  2000/09/20 21:52:38  florian
    * removed a lot of errors

  Revision 1.1  2000/08/26 12:27:17  florian
    * createial release

}