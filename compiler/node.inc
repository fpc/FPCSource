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
         registers32:=0;
         registersfpu:=0;
{$ifdef SUPPORT_MMX}
         registersmmx:=0;
{$endif SUPPORT_MMX}
         flags:=[];
      end;

    constructor tnode.createforcopy;

      begin
      end;

    procedure tnode.toggleflag(f : tnodeflags);

      begin
         if f in flags then
           exclude(flags,f)
         else
           include(flags,f);
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
         pass_1:=nil;

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
      begin
        dowritenodetype;
      end;

    procedure tnode.dowritenodetype;
      const nodetype2str : array[tnodetype] of string[20] = (
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
          'pointerconstn',
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
          'loadvmtn');

      begin
         write(writenodeindention,'(',nodetype2str[nodetype]);
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
         if not(assigned(self)) then
           begin
              getcopy:=nil;
              exit;
           end;
         { this is quite tricky because we need a node of the current }
         { node type and not one of tnode!                            }
         p:=tnodeclass(classtype).createforcopy;
         p.nodetype:=nodetype;
         p.location:=location;
         p.parent:=parent;
         p.flags:=flags;
         p.registers32:=registers32;
         p.registersfpu:=registersfpu;
{$ifdef SUPPORT_MMX}
         p.registersmmx:=registersmmx;
         p.registerskni:=registerskni;
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

    function tunarynode.getcopy : tnode;

      var
         p : tunarynode;

      begin
         p:=tunarynode(inherited getcopy);
         if assigned(left) then
           p.left:=left.getcopy
         else
           p.left:=nil;
         getcopy:=p;
      end;

{$ifdef extdebug}
    procedure tunarynode.dowrite;

      begin
         inherited dowrite;
         writeln(',');
         writenodeindention:=writenodeindention+'    ';
         writenode(left);
         write(')');
         delete(writenodeindention,1,4);
      end;
{$endif}

    procedure tunarynode.left_max;

      begin
         registers32:=left.registers32;
         registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
         registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
      end;

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

    function tbinarynode.getcopy : tnode;

      var
         p : tbinarynode;

      begin
         p:=tbinarynode(inherited getcopy);
         if assigned(right) then
           p.right:=right.getcopy
         else
           p.right:=nil;
         getcopy:=p;
      end;


    procedure tbinarynode.swapleftright;

      var
         swapp : tnode;

      begin
         swapp:=right;
         right:=left;
         left:=
         swapp;
         if nf_swaped in flags then
           exclude(flags,nf_swaped)
         else
           include(flags,nf_swaped);
      end;

    procedure tbinarynode.left_right_max;
      begin
        if assigned(left) then
         begin
           if assigned(right) then
            begin
              registers32:=max(left.registers32,right.registers32);
              registersfpu:=max(left.registersfpu,right.registersfpu);
{$ifdef SUPPORT_MMX}
              registersmmx:=max(left.registersmmx,right.registersmmx);
{$endif SUPPORT_MMX}
            end
           else
            begin
              registers32:=left.registers32;
              registersfpu:=left.registersfpu;
{$ifdef SUPPORT_MMX}
              registersmmx:=left.registersmmx;
{$endif SUPPORT_MMX}
            end;
         end;
      end;

{$ifdef extdebug}
    procedure tbinarynode.dowrite;

      begin
         inherited dowrite;
         writeln(',');
         writenodeindention:=writenodeindention+'    ';
         writenode(right);
         write(')');
         delete(writenodeindention,1,4);
      end;
{$endif}

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


{****************************************************************************
                                 WRITENODE
 ****************************************************************************}

{$ifdef EXTDEBUG}
     procedure writenode(t:tnode);
     begin
       if assigned(t) then
        t.dowrite
       else
        write(writenodeindention,'nil');
       if writenodeindention='' then
        writeln;
     end;
{$endif EXTDEBUG}

{
  $Log$
  Revision 1.9  2000-10-14 10:14:51  peter
    * moehrendorf oct 2000 rewrite

  Revision 1.8  2000/10/01 19:48:24  peter
    * lot of compile updates for cg11

  Revision 1.7  2000/09/29 15:45:23  florian
    * make cycle fixed

  Revision 1.6  2000/09/28 19:49:52  florian
  *** empty log message ***

  Revision 1.5  2000/09/27 18:14:31  florian
    * fixed a lot of syntax errors in the n*.pas stuff

  Revision 1.4  2000/09/26 20:06:13  florian
    * hmm, still a lot of work to get things compilable

  Revision 1.3  2000/09/22 21:45:36  florian
    * some updates e.g. getcopy added

  Revision 1.2  2000/09/20 21:52:38  florian
    * removed a lot of errors

  Revision 1.1  2000/08/26 12:27:17  florian
    * createial release

}