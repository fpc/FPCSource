{
    $Id$
    Copyright (c) 2002 by Florian Klaempfl

    Generates the argument location information for x86-64 target

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published bymethodpointer
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
{ Generates the argument location information for x86-64 target.
}
unit cpupara;

{$i fpcdefs.inc}

  interface

    uses
      globtype,
      cpubase,
      symconst,symbase,symtype,symdef,
      paramgr;

    type
       { Returns the location for the nr-st 32 Bit int parameter
         if every parameter before is an 32 Bit int parameter as well
         and if the calling conventions for the helper routines of the
         rtl are used.
       }
       tx86_64paramanager = class(tparamanager)
          function getintparaloc(calloption : tproccalloption; nr : longint) : tparalocation;override;
          function create_paraloc_info(p : tabstractprocdef; side: tcallercallee):longint;override;
       end;

  implementation

    uses
       verbose,
       cpuinfo,cgbase,systems,
       defutil;

    const
      paraintsupregs : array[0..5] of tsuperregister = (RS_RDI,RS_RSI,RS_RDX,RS_RCX,RS_R8,RS_R9);
      parammsupregs : array[0..7] of tsuperregister = (RS_XMM0,RS_XMM1,RS_XMM2,RS_XMM3,RS_XMM4,RS_XMM5,RS_XMM6,RS_XMM7);

    function getparaloc(p : tdef) : tcgloc;

      begin
         { Later, the LOC_REFERENCE is in most cases changed into LOC_REGISTER
           if push_addr_param for the def is true
         }
         // !!!!! Fix aggregate types
         case p.deftype of
            orddef:
              getparaloc:=LOC_REGISTER;
            floatdef:
              case tfloatdef(p).typ of
                 s80real:
                   getparaloc:=LOC_REFERENCE;
                 s32real,
                 s64real,
                 s64comp,
                 s64currency,
                 s128real:
                   getparaloc:=LOC_MMREGISTER;
              end;
            enumdef:
              getparaloc:=LOC_REGISTER;
            pointerdef:
              getparaloc:=LOC_REGISTER;
            formaldef:
              getparaloc:=LOC_REGISTER;
            classrefdef:
              getparaloc:=LOC_REGISTER;
            recorddef:
              getparaloc:=LOC_REFERENCE;
            objectdef:
              if is_object(p) then
                getparaloc:=LOC_REFERENCE
              else
                getparaloc:=LOC_REGISTER;
            stringdef:
              if is_shortstring(p) or is_longstring(p) then
                getparaloc:=LOC_REFERENCE
              else
                getparaloc:=LOC_REGISTER;
            procvardef:
              if (po_methodpointer in tprocvardef(p).procoptions) then
                getparaloc:=LOC_REFERENCE
              else
                getparaloc:=LOC_REGISTER;
            filedef:
              getparaloc:=LOC_REGISTER;
            arraydef:
              getparaloc:=LOC_REFERENCE;
            setdef:
              if is_smallset(p) then
                getparaloc:=LOC_REGISTER
              else
                getparaloc:=LOC_REFERENCE;
            variantdef:
              getparaloc:=LOC_REFERENCE;
            { avoid problems with errornous definitions }
            errordef:
              getparaloc:=LOC_REGISTER;
            else
              internalerror(2002071001);
         end;
      end;


    function tx86_64paramanager.getintparaloc(calloption : tproccalloption; nr : longint): tparalocation;
      begin
         fillchar(result,sizeof(tparalocation),0);
         result.size:=OS_INT;
         if nr<1 then
           internalerror(200304303)
         else if nr<=high(paraintsupregs)+1 then
           begin
              result.loc:=LOC_REGISTER;
              result.register:=newreg(R_INTREGISTER,paraintsupregs[nr-1],R_SUBWHOLE);
           end
         else
           begin
              result.loc:=LOC_REFERENCE;
              result.reference.index:=NR_STACK_POINTER_REG;
              result.reference.offset:=(nr-6)*8;
           end;
      end;


    function tx86_64paramanager.create_paraloc_info(p : tabstractprocdef; side: tcallercallee):longint;
      var
        hp : tparaitem;
        paraloc : tparalocation;
        subreg : tsubregister;
        pushaddr : boolean;
        l,intparareg,mmparareg,
        varalign,
        paraalign,
        parasize : longint;
      begin
        intparareg:=0;
        mmparareg:=0;
        parasize:=0;
        paraalign:=get_para_align(p.proccalloption);
        { Register parameters are assigned from left to right }
        hp:=tparaitem(p.para.first);
        while assigned(hp) do
          begin
            pushaddr:=push_addr_param(hp.paratyp,hp.paratype.def,p.proccalloption);
            if pushaddr then
              paraloc.size:=OS_ADDR
            else
              paraloc.size:=def_cgsize(hp.paratype.def);
            paraloc.alignment:=paraalign;
            if (intparareg<=high(paraintsupregs)) and
               not(
                   ((hp.paratype.def.deftype in [floatdef,recorddef,arraydef]) and
                    (not pushaddr))
                  ) then
              begin
                paraloc.loc:=LOC_REGISTER;
                if paraloc.size=OS_NO then
                  subreg:=R_SUBWHOLE
                else
                  subreg:=cgsize2subreg(paraloc.size);
                paraloc.alignment:=paraalign;
                paraloc.register:=newreg(R_INTREGISTER,paraintsupregs[intparareg],subreg);
                inc(intparareg);
              end
            else if (mmparareg<=high(parammsupregs)) then
              begin
              end
            else
              begin
                paraloc.loc:=LOC_REFERENCE;
                if side=callerside then
                  paraloc.reference.index:=NR_STACK_POINTER_REG
                else
                  paraloc.reference.index:=NR_FRAME_POINTER_REG;
                l:=push_size(hp.paratyp,hp.paratype.def,p.proccalloption);
                // varalign:=size_2_align(l);
                paraloc.reference.offset:=parasize;
                // varalign:=used_align(varalign,paraalign,paraalign);
                // parasize:=align(parasize+l,varalign);
              end;
            hp.paraloc[side]:=paraloc;
            hp:=tparaitem(hp.next);
          end;
        { Register parameters are assigned from left-to-right, adapt offset
          for calleeside to be reversed }
        hp:=tparaitem(p.para.first);
        while assigned(hp) do
          begin
            if (hp.paraloc[side].loc=LOC_REFERENCE) then
              begin
                l:=push_size(hp.paratyp,hp.paratype.def,p.proccalloption);
                // varalign:=used_align(size_2_align(l),paraalign,paraalign);
                // l:=align(l,varalign);
                hp.paraloc[side].reference.offset:=parasize-hp.paraloc[side].reference.offset-l;
                if side=calleeside then
                  inc(hp.paraloc[side].reference.offset,target_info.first_parm_offset);
              end;
            hp:=tparaitem(hp.next);
          end;
        { We need to return the size allocated }
        result:=parasize;
      end;


begin
   paramanager:=tx86_64paramanager.create;
end.
{
  $Log$
  Revision 1.6  2004-01-14 23:39:05  florian
    * another bunch of x86-64 fixes mainly calling convention and
      assembler reader related

  Revision 1.5  2003/12/24 00:10:03  florian
    - delete parameter in cg64 methods removed

  Revision 1.4  2003/04/30 20:53:32  florian
    * error when address of an abstract method is taken
    * fixed some x86-64 problems
    * merged some more x86-64 and i386 code

  Revision 1.3  2002/04/25 16:12:09  florian
    * fixed more problems with cpubase and x86-64

  Revision 1.2  2003/01/05 13:36:54  florian
    * x86-64 compiles
    + very basic support for float128 type (x86-64 only)

  Revision 1.1  2002/07/24 22:38:15  florian
    + initial release of x86-64 target code
}
