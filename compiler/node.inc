{
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

    procedure tnode.set_file_line(from : tnode);

      begin
         if assigned(from) then
           fileinfo:=from.fileinfo;
      end;

    procedure tnode.set_tree_filepos(const filepos : tfileposinfo);

      begin
         fileinfo:=filepos;
      end;

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
  Revision 1.2  2000-09-20 21:52:38  florian
    * removed a lot of errors

  Revision 1.1  2000/08/26 12:27:17  florian
    * createial release

}