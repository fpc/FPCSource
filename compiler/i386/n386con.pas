{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

    Generate i386 assembler for constants

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
unit n386con;

{$i defines.inc}

interface

    uses
       node,ncon,ncgcon;

    type
       ti386realconstnode = class(tcgrealconstnode)
          function pass_1 : tnode;override;
          procedure pass_2;override;
       end;

implementation

    uses
      systems,
      temp_gen,
      cpubase,
      cga,tgcpu;

{*****************************************************************************
                           TI386REALCONSTNODE
*****************************************************************************}

    function ti386realconstnode.pass_1 : tnode;
      begin
         result:=nil;
         if (value_real=1.0) or (value_real=0.0) then
           begin
              location.loc:=LOC_FPU;
              registersfpu:=1;
           end
         else
           location.loc:=LOC_MEM;
      end;

    procedure ti386realconstnode.pass_2;

      begin
         if (value_real=1.0) then
           begin
              emit_none(A_FLD1,S_NO);
              location.loc:=LOC_FPU;
              inc(fpuvaroffset);
           end
         else if (value_real=0.0) then
           begin
              emit_none(A_FLDZ,S_NO);
              location.loc:=LOC_FPU;
              inc(fpuvaroffset);
           end
         else
           inherited pass_2;
      end;


begin
   crealconstnode:=ti386realconstnode;
end.
{
  $Log$
  Revision 1.11  2001-09-30 16:17:17  jonas
    * made most constant and mem handling processor independent

  Revision 1.10  2001/08/26 13:36:57  florian
    * some cg reorganisation
    * some PPC updates

  Revision 1.9  2001/07/08 21:00:18  peter
    * various widestring updates, it works now mostly without charset
      mapping supported

  Revision 1.8  2001/04/13 01:22:18  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.7  2001/04/02 21:20:37  peter
    * resulttype rewrite

  Revision 1.6  2000/12/25 00:07:32  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.5  2000/11/29 00:30:47  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.4  2000/11/20 15:31:38  jonas
  *  longint typecast to assignment of constant to offset field

  Revision 1.3  2000/11/13 14:44:36  jonas
    * fixes so no more range errors with improved range checking code

  Revision 1.2  2000/10/31 22:02:56  peter
    * symtable splitted, no real code changes

  Revision 1.1  2000/10/15 09:33:31  peter
    * moved n386*.pas to i386/ cpu_target dir

  Revision 1.2  2000/10/14 10:14:48  peter
    * moehrendorf oct 2000 rewrite

  Revision 1.1  2000/09/28 20:48:52  florian
  *** empty log message ***
}
