{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

    Generate assembler for nodes that handle loads and assignments which
    are the same for all (most) processors

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
unit ncgld;

{$i defines.inc}

interface

    uses
      node,nld;

    type
       tcgarrayconstructornode = class(tarrayconstructornode)
          procedure pass_2;override;
       end;

implementation

    uses
      systems,
      verbose,globals,
      symconst,symtype,symdef,symsym,symtable,aasm,types,
      cginfo,cgbase,pass_2,
      cpubase,cpuasm,
      cga,tgobj,ncgutil,regvars,cgobj,cg64f32,rgobj,rgcpu;

{*****************************************************************************
                           SecondArrayConstruct
*****************************************************************************}

      const
        vtInteger    = 0;
        vtBoolean    = 1;
        vtChar       = 2;
        vtExtended   = 3;
        vtString     = 4;
        vtPointer    = 5;
        vtPChar      = 6;
        vtObject     = 7;
        vtClass      = 8;
        vtWideChar   = 9;
        vtPWideChar  = 10;
        vtAnsiString = 11;
        vtCurrency   = 12;
        vtVariant    = 13;
        vtInterface  = 14;
        vtWideString = 15;
        vtInt64      = 16;
        vtQWord      = 17;

    procedure tcgarrayconstructornode.pass_2;
      var
        hp    : tarrayconstructornode;
        href  : treference;
        lt    : tdef;
        vaddr : boolean;
        vtype : longint;
        freetemp,
        dovariant : boolean;
        elesize : longint;
        tmpreg  : tregister;
      begin
        dovariant:=(nf_forcevaria in flags) or tarraydef(resulttype.def).isvariant;
        if dovariant then
         elesize:=8
        else
         elesize:=tarraydef(resulttype.def).elesize;
        if not(nf_cargs in flags) then
         begin
           location_reset(location,LOC_REFERENCE,OS_NO);
           { Allocate always a temp, also if no elements are required, to
             be sure that location is valid (PFV) }
            if tarraydef(resulttype.def).highrange=-1 then
              tg.gettempofsizereference(exprasmlist,elesize,location.reference)
            else
              tg.gettempofsizereference(exprasmlist,(tarraydef(resulttype.def).highrange+1)*elesize,location.reference);
            href:=location.reference;
         end;
        hp:=self;
        while assigned(hp) do
         begin
           if assigned(hp.left) then
            begin
              freetemp:=true;
              secondpass(hp.left);
              if codegenerror then
               exit;
              if dovariant then
               begin
                 { find the correct vtype value }
                 vtype:=$ff;
                 vaddr:=false;
                 lt:=hp.left.resulttype.def;
                 case lt.deftype of
                   enumdef,
                   orddef :
                     begin
                       if is_64bitint(lt) then
                         begin
                            case torddef(lt).typ of
                               s64bit:
                                 vtype:=vtInt64;
                               u64bit:
                                 vtype:=vtQWord;
                            end;
                            freetemp:=false;
                            vaddr:=true;
                         end
                       else if (lt.deftype=enumdef) or
                         is_integer(lt) then
                         vtype:=vtInteger
                       else
                         if is_boolean(lt) then
                           vtype:=vtBoolean
                         else
                           if (lt.deftype=orddef) and (torddef(lt).typ=uchar) then
                             vtype:=vtChar;
                     end;
                   floatdef :
                     begin
                       vtype:=vtExtended;
                       vaddr:=true;
                       freetemp:=false;
                     end;
                   procvardef,
                   pointerdef :
                     begin
                       if is_pchar(lt) then
                         vtype:=vtPChar
                       else
                         vtype:=vtPointer;
                     end;
                   classrefdef :
                     vtype:=vtClass;
                   objectdef :
                     begin
                       vtype:=vtObject;
                     end;
                   stringdef :
                     begin
                       if is_shortstring(lt) then
                        begin
                          vtype:=vtString;
                          vaddr:=true;
                          freetemp:=false;
                        end
                       else
                        if is_ansistring(lt) then
                         begin
                           vtype:=vtAnsiString;
                           freetemp:=false;
                         end
                       else
                        if is_widestring(lt) then
                         begin
                           vtype:=vtWideString;
                           freetemp:=false;
                         end;
                     end;
                 end;
                 if vtype=$ff then
                   internalerror(14357);
                 { write C style pushes or an pascal array }
                 if nf_cargs in flags then
                  begin
                    if vaddr then
                     begin
                       location_force_mem(hp.left.location);
                       cg.a_paramaddr_ref(exprasmlist,hp.left.location.reference,-1);
                       location_release(exprasmlist,hp.left.location);
                       if freetemp then
                        location_freetemp(exprasmlist,hp.left.location);
                     end
                    else
                     cg.a_param_loc(exprasmlist,hp.left.location,-1);
                    inc(pushedparasize,4);
                  end
                 else
                  begin
                    { write changing field update href to the next element }
                    inc(href.offset,4);
                    if vaddr then
                     begin
                       location_force_mem(hp.left.location);
                       tmpreg:=cg.get_scratch_reg(exprasmlist);
                       cg.a_loadaddr_ref_reg(exprasmlist,hp.left.location.reference,tmpreg);
                       cg.a_load_reg_ref(exprasmlist,cg.reg_cgsize(tmpreg),tmpreg,href);
                       cg.free_scratch_reg(exprasmlist,tmpreg);
                       location_release(exprasmlist,hp.left.location);
                       if freetemp then
                        location_freetemp(exprasmlist,hp.left.location);
                     end
                    else
                     begin
                       location_release(exprasmlist,left.location);
                       cg.a_load_loc_ref(exprasmlist,hp.left.location,href);
                     end;
                    { update href to the vtype field and write it }
                    dec(href.offset,4);
                    cg.a_load_const_ref(exprasmlist, OS_INT,vtype,href);
                    { goto next array element }
                    inc(href.offset,8);
                  end;
               end
              else
              { normal array constructor of the same type }
               begin
                 case elesize of
                   1,2,4 :
                     begin
                       location_release(exprasmlist,left.location);
                       cg.a_load_loc_ref(exprasmlist,hp.left.location,href);
                     end;
                   8 :
                     begin
                       if hp.left.location.loc in [LOC_REGISTER,LOC_CREGISTER] then
                        tcg64f32(cg).a_load64_loc_ref(exprasmlist,hp.left.location,href)
                       else
                        cg.g_concatcopy(exprasmlist,hp.left.location.reference,href,elesize,freetemp,false);
                     end;
                   else
                     begin
                       { concatcopy only supports reference }
                       if not(hp.left.location.loc in [LOC_CREFERENCE,LOC_REFERENCE]) then
                        internalerror(200108012);
                       cg.g_concatcopy(exprasmlist,hp.left.location.reference,href,elesize,freetemp,false);
                     end;
                 end;
                 inc(href.offset,elesize);
               end;
            end;
           { load next entry }
           hp:=tarrayconstructornode(hp.right);
         end;
      end;

begin
   carrayconstructornode:=tcgarrayconstructornode;
end.
{
  $Log$
  Revision 1.2  2002-04-21 15:24:38  carl
  + a_jmp_cond -> a_jmp_always (a_jmp_cond is NOT portable)
  + changeregsize -> rg.makeregsize

  Revision 1.1  2002/04/19 15:39:34  peter
    * removed some more routines from cga
    * moved location_force_reg/mem to ncgutil
    * moved arrayconstructnode secondpass to ncgld

}
