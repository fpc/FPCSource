{
    $Id$
    Copyright (c) 1999-2000 by Florian Klaempfl

    The declarations of the nodes for the new code generator

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

    constructor tnode.init;

      begin
         inherited init;
         treetype:=nothingn;
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

    destructor tnode.done;

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

    procedure tnode.pass_1;

      begin
         if not(assigned(resulttype)) then
           det_resulttype;

         det_temp;
      end;

    procedure tnode.det_resulttype;

      begin
         abstract;
      end;

    procedure tnode.det_temp;

      begin
         abstract;
      end;

    procedure tnode.secondpass;

      begin
         abstract;
      end;

    procedure tnode.concattolist(l : plinkedlist);

      begin
         l^.concat(@self);
      end;

    function tnode.ischild(p : pnode) : boolean;

      begin
         ischild:=false;
      end;

{$ifdef EXTDEBUG}
    procedure tnode.dowrite;

      const treetype2str : array[ttreetyp] of string[20] = (
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
         write(indention,'(',treetype2str[treetype]);
      end;
{$endif EXTDEBUG}

{****************************************************************************
                                 TUNARYNODE
 ****************************************************************************}

    constructor tunarynode.init(l : pnode);

      begin
         inherited init;
         left:=l;
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
         left^.parent:=@self;
         left^.concattolist(l);
         inherited concattolist(l);
      end;

    function tunarynode.ischild(p : pnode) : boolean;

      begin
         ischild:=p=left;
      end;

    procedure tunarynode.det_resulttype;

      begin
         left^.det_resulttype;
      end;

    procedure tunarynode.det_temp;

      begin
         left^.det_temp;
      end;

{****************************************************************************
                            TBINARYNODE
 ****************************************************************************}

    constructor tbinarynode.init(l,r : pnode);

      begin
         inherited init(l);
         right:=r
      end;

    procedure tbinarynode.concattolist(l : plinkedlist);

      begin
         { we could change that depending on the number of }
         { required registers                              }
         left^.parent:=@self;
         left^.concattolist(l);
         left^.parent:=@self;
         left^.concattolist(l);
         inherited concattolist(l);
      end;

    function tbinarynode.ischild(p : pnode) : boolean;

      begin
         ischild:=(p=right) or (p=right);
      end;

    procedure tbinarynode.det_resulttype;

      begin
         left^.det_resulttype;
         right^.det_resulttype;
      end;

    procedure tbinarynode.det_temp;

      begin
         left^.det_temp;
         right^.det_temp;
      end;

{****************************************************************************
                            TBINOPYNODE
 ****************************************************************************}

    constructor tbinopnode.init(l,r : pnode);

      begin
         inherited init(l,r);
         swaped:=false;
      end;

{
  $Log$
  Revision 1.1  2000-07-13 06:30:08  michael
  + Initial import

  Revision 1.1  2000/02/24 18:40:42  peter
    * moved to new/

  Revision 1.1  2000/02/21 08:45:26  michael
  Committed for Florian.

  Revision 1.1  2000/02/20 20:49:45  florian
    * newcg is compiling
    * fixed the dup id problem reported by Paul Y.

}