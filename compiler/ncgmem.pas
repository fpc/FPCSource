{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate assembler for memory related nodes which are
    the same for all (most?) processors

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
{ This unit generate assembler for memory related nodes.
}
unit ncgmem;

{$i fpcdefs.inc}

interface

    uses
      cpuinfo,cpubase,
      node,nmem;

    type
       tcgloadvmtaddrnode = class(tloadvmtaddrnode)
          procedure pass_2;override;
       end;

       tcgaddrnode = class(taddrnode)
          procedure pass_2;override;
       end;

       tcgdoubleaddrnode = class(tdoubleaddrnode)
          procedure pass_2;override;
       end;

       tcgderefnode = class(tderefnode)
          procedure pass_2;override;
       end;

       tcgsubscriptnode = class(tsubscriptnode)
          procedure pass_2;override;
       end;

       tcgwithnode = class(twithnode)
          procedure pass_2;override;
       end;

       tcgvecnode = class(tvecnode)
       private
         procedure rangecheck_array;
       protected
         function get_mul_size : longint;
         {# This routine is used to calculate the address of the reference.
            On entry reg contains the index in the array,
           and l contains the size of each element in the array.
           This routine should update location.reference correctly,
           so it points to the correct address.
         }
         procedure update_reference_reg_mul(reg:tregister;l:aword);virtual;
         procedure second_wideansistring;virtual;
         procedure second_dynamicarray;virtual;
       public
         procedure pass_2;override;
       end;

implementation

    uses
{$ifdef delphi}
      sysutils,
{$else}
      strings,
{$endif}
{$ifdef GDB}
      gdb,
{$endif GDB}
      globtype,systems,
      cutils,verbose,globals,
      symconst,symdef,symsym,symtable,defutil,paramgr,
      aasmbase,aasmtai,
      cginfo,cgbase,pass_2,
      pass_1,nld,ncon,nadd,
      cgobj,tgobj,rgobj,ncgutil,symbase
      ;


{*****************************************************************************
                            TCGLOADNODE
*****************************************************************************}

    procedure tcgloadvmtaddrnode.pass_2;
      var
       href : treference;

      begin
         location_reset(location,LOC_REGISTER,OS_ADDR);
         if (left.nodetype<>typen) then
          begin
            { left contains self, load vmt from self }
            secondpass(left);
            if is_object(left.resulttype.def) then
             begin
               case left.location.loc of
                  LOC_CREFERENCE,
                  LOC_REFERENCE:
                    begin
                       location_release(exprasmlist,left.location);
                       reference_reset_base(href,rg.getaddressregister(exprasmlist),tobjectdef(left.resulttype.def).vmt_offset);
                       cg.a_loadaddr_ref_reg(exprasmlist,left.location.reference,href.base);
                    end;
                  else
                    internalerror(200305056);
               end;
             end
            else
             begin
               case left.location.loc of
                  LOC_REGISTER:
                    begin
                      if not rg.isaddressregister(left.location.register) then
                        begin
                          location_release(exprasmlist,left.location);
                          reference_reset_base(href,rg.getaddressregister(exprasmlist),tobjectdef(left.resulttype.def).vmt_offset);
                          cg.a_load_reg_reg(exprasmlist,OS_ADDR,OS_ADDR,left.location.register,href.base);
                        end
                      else
                        reference_reset_base(href,left.location.register,tobjectdef(left.resulttype.def).vmt_offset);
                    end;
                  LOC_CREGISTER,
                  LOC_CREFERENCE,
                  LOC_REFERENCE:
                    begin
                       location_release(exprasmlist,left.location);
                       reference_reset_base(href,rg.getaddressregister(exprasmlist),tobjectdef(left.resulttype.def).vmt_offset);
                       cg.a_load_loc_reg(exprasmlist,OS_ADDR,left.location,href.base);
                    end;
                  else
                    internalerror(200305057);
               end;
             end;
            reference_release(exprasmlist,href);
            location.register:=rg.getaddressregister(exprasmlist);
            cg.g_maybe_testself(exprasmlist,href.base);
            cg.a_load_ref_reg(exprasmlist,OS_ADDR,OS_ADDR,href,location.register);
          end
         else
          begin
            reference_reset_symbol(href,
              objectlibrary.newasmsymboldata(tobjectdef(tclassrefdef(resulttype.def).pointertype.def).vmt_mangledname),0);
            location.register:=rg.getaddressregister(exprasmlist);
            cg.a_loadaddr_ref_reg(exprasmlist,href,location.register);
          end;
      end;


{*****************************************************************************
                             TCGADDRNODE
*****************************************************************************}

    procedure tcgaddrnode.pass_2;
      begin
         secondpass(left);

         { when loading procvar we do nothing with this node, so load the
           location of left }
         if nf_procvarload in flags then
          begin
            location_copy(location,left.location);
            exit;
          end;

         location_release(exprasmlist,left.location);
         location_reset(location,LOC_REGISTER,OS_ADDR);
         location.register:=rg.getaddressregister(exprasmlist);
         {@ on a procvar means returning an address to the procedure that
           is stored in it.}
         { yes but left.symtableentry can be nil
           for example on self !! }
         { symtableentry can be also invalid, if left is no tree node }
         if (m_tp_procvar in aktmodeswitches) and
            (left.nodetype=loadn) and
            assigned(tloadnode(left).symtableentry) and
            (tloadnode(left).symtableentry.typ=varsym) and
            (tvarsym(tloadnode(left).symtableentry).vartype.def.deftype=procvardef) then
           cg.a_load_ref_reg(exprasmlist,OS_ADDR,OS_ADDR,left.location.reference,
             location.register)
         else
          begin
           cg.a_loadaddr_ref_reg(exprasmlist,left.location.reference,
             location.register);
          end;
      end;


{*****************************************************************************
                         TCGDOUBLEADDRNODE
*****************************************************************************}

    procedure tcgdoubleaddrnode.pass_2;
      begin
         secondpass(left);

         location_release(exprasmlist,left.location);
         location_reset(location,LOC_REGISTER,OS_ADDR);
         location.register:=rg.getaddressregister(exprasmlist);

         cg.a_loadaddr_ref_reg(exprasmlist,left.location.reference,
           location.register);
      end;


{*****************************************************************************
                           TCGDEREFNODE
*****************************************************************************}

    procedure tcgderefnode.pass_2;

      begin
         secondpass(left);
         location_reset(location,LOC_REFERENCE,def_cgsize(resulttype.def));
         case left.location.loc of
            LOC_REGISTER:
              begin
                if not rg.isaddressregister(left.location.register) then
                  begin
                    location_release(exprasmlist,left.location);
                    location.reference.base := rg.getaddressregister(exprasmlist);
                    cg.a_load_reg_reg(exprasmlist,OS_ADDR,OS_ADDR,left.location.register,
                      location.reference.base);
                  end
                else
                  location.reference.base := left.location.register;
              end;
            LOC_CREGISTER,
            LOC_CREFERENCE,
            LOC_REFERENCE:
              begin
                 location_release(exprasmlist,left.location);
                 location.reference.base:=rg.getaddressregister(exprasmlist);
                 cg.a_load_loc_reg(exprasmlist,OS_ADDR,left.location,location.reference.base);
              end;
         end;
         if (cs_gdb_heaptrc in aktglobalswitches) and
            (cs_checkpointer in aktglobalswitches) and
            not(cs_compilesystem in aktmoduleswitches) and
            (not tpointerdef(left.resulttype.def).is_far) then
          begin
            cg.a_param_reg(exprasmlist, OS_ADDR,location.reference.base,paramanager.getintparaloc(1));
            cg.a_call_name(exprasmlist,'FPC_CHECKPOINTER');
          end;
      end;


{*****************************************************************************
                          TCGSUBSCRIPTNODE
*****************************************************************************}

    procedure tcgsubscriptnode.pass_2;

      begin
         secondpass(left);
         if codegenerror then
           exit;
         { classes and interfaces must be dereferenced implicit }
         if is_class_or_interface(left.resulttype.def) then
           begin
             location_reset(location,LOC_REFERENCE,def_cgsize(resulttype.def));
             case left.location.loc of
                LOC_CREGISTER,
                LOC_REGISTER:
                  begin
                    if not rg.isaddressregister(left.location.register) then
                      begin
                        location_release(exprasmlist,left.location);
                        location.reference.base:=rg.getaddressregister(exprasmlist);
                        cg.a_load_reg_reg(exprasmlist,OS_ADDR,OS_ADDR,
                          left.location.register,location.reference.base);
                      end
                    else
                      location.reference.base := left.location.register;
                  end;
                LOC_CREFERENCE,
                LOC_REFERENCE:
                  begin
                     location_release(exprasmlist,left.location);
                     location.reference.base:=rg.getaddressregister(exprasmlist);
                     cg.a_load_loc_reg(exprasmlist,OS_ADDR,left.location,location.reference.base);
                  end;
             end;
             { implicit deferencing }
             if (cs_gdb_heaptrc in aktglobalswitches) and
                (cs_checkpointer in aktglobalswitches) and
                not(cs_compilesystem in aktmoduleswitches) then
              begin
                cg.a_param_reg(exprasmlist, OS_ADDR,location.reference.base,paramanager.getintparaloc(1));
                cg.a_call_name(exprasmlist,'FPC_CHECKPOINTER');
              end;
           end
         else if is_interfacecom(left.resulttype.def) then
           begin
              tg.GetTemp(exprasmlist,pointer_size,tt_interfacecom,location.reference);
              cg.a_load_loc_ref(exprasmlist,OS_ADDR,left.location,location.reference);
             { implicit deferencing also for interfaces }
             if (cs_gdb_heaptrc in aktglobalswitches) and
                (cs_checkpointer in aktglobalswitches) and
                not(cs_compilesystem in aktmoduleswitches) then
              begin
                cg.a_param_reg(exprasmlist, OS_ADDR,location.reference.base,paramanager.getintparaloc(1));
                cg.a_call_name(exprasmlist,'FPC_CHECKPOINTER');
              end;

           end
         else
           location_copy(location,left.location);

         inc(location.reference.offset,vs.address);
         { also update the size of the location }
         location.size:=def_cgsize(resulttype.def);
      end;


{*****************************************************************************
                            TCGWITHNODE
*****************************************************************************}

    procedure tcgwithnode.pass_2;
{$ifdef GDB}
      const
        withlevel : longint = 0;
      var
        withstartlabel,withendlabel : tasmlabel;
        pp : pchar;
        mangled_length  : longint;
{$endif GDB}
      begin
        location_reset(location,LOC_VOID,OS_NO);

{$ifdef GDB}
        if (cs_debuginfo in aktmoduleswitches) then
          begin
            { load reference }
            if (withrefnode.nodetype=derefn) and
               (tderefnode(withrefnode).left.nodetype=temprefn) then
              secondpass(withrefnode);

            inc(withlevel);
            objectlibrary.getaddrlabel(withstartlabel);
            objectlibrary.getaddrlabel(withendlabel);
            cg.a_label(exprasmlist,withstartlabel);
            withdebugList.concat(Tai_stabs.Create(strpnew(
               '"with'+tostr(withlevel)+':'+tostr(symtablestack.getnewtypecount)+
               '=*'+tstoreddef(left.resulttype.def).numberstring+'",'+
               tostr(N_LSYM)+',0,0,'+tostr(withrefnode.location.reference.offset))));
            mangled_length:=length(current_procdef.mangledname);
            getmem(pp,mangled_length+50);
            strpcopy(pp,'192,0,0,'+withstartlabel.name);
            if (target_info.use_function_relative_addresses) then
              begin
                strpcopy(strend(pp),'-');
                strpcopy(strend(pp),current_procdef.mangledname);
              end;
            withdebugList.concat(Tai_stabn.Create(strnew(pp)));
          end;
{$endif GDB}

        if assigned(left) then
          secondpass(left);

{$ifdef GDB}
        if (cs_debuginfo in aktmoduleswitches) then
          begin
            cg.a_label(exprasmlist,withendlabel);
            strpcopy(pp,'224,0,0,'+withendlabel.name);
           if (target_info.use_function_relative_addresses) then
             begin
               strpcopy(strend(pp),'-');
               strpcopy(strend(pp),current_procdef.mangledname);
             end;
            withdebugList.concat(Tai_stabn.Create(strnew(pp)));
            freemem(pp,mangled_length+50);
            dec(withlevel);
          end;
{$endif GDB}
       end;


{*****************************************************************************
                            TCGVECNODE
*****************************************************************************}

     function tcgvecnode.get_mul_size : longint;
       begin
         if nf_memindex in flags then
          get_mul_size:=1
         else
          begin
            if (left.resulttype.def.deftype=arraydef) then
             get_mul_size:=tarraydef(left.resulttype.def).elesize
            else
             get_mul_size:=resulttype.def.size;
          end
       end;


     procedure tcgvecnode.update_reference_reg_mul(reg:tregister;l:aword);
       begin
         if location.reference.base.number=NR_NO then
          begin
            cg.a_op_const_reg(exprasmlist,OP_IMUL,OS_ADDR,l,reg);
            location.reference.base:=reg;
          end
         else if location.reference.index.number=NR_NO then
          begin
            cg.a_op_const_reg(exprasmlist,OP_IMUL,OS_ADDR,l,reg);
            location.reference.index:=reg;
          end
         else
          begin
            cg.a_loadaddr_ref_reg(exprasmlist,location.reference,location.reference.index);
            rg.ungetregisterint(exprasmlist,location.reference.base);
            reference_reset_base(location.reference,location.reference.index,0);
            { insert new index register }
            cg.a_op_const_reg(exprasmlist,OP_IMUL,OS_ADDR,l,reg);
            location.reference.index:=reg;
          end;
       end;


     procedure tcgvecnode.second_wideansistring;
       begin
       end;

     procedure tcgvecnode.second_dynamicarray;
       begin
       end;


     procedure tcgvecnode.rangecheck_array;
       var
         freereg : boolean;
         hightree : tnode;
         poslabel,
         neglabel : tasmlabel;
         hreg : tregister;
         i:Tsuperregister;
      {$ifndef newra}
         pushed : tpushedsavedint;
      {$endif}
       begin
         if is_open_array(left.resulttype.def) or
            is_array_of_const(left.resulttype.def) then
          begin
            { cdecl functions don't have high() so we can not check the range }
            if not(current_procdef.proccalloption in [pocall_cdecl,pocall_cppdecl]) then
             begin
               { Get high value }
               hightree:=load_high_value_node(tvarsym(tloadnode(left).symtableentry));
               { it must be available }
               if not assigned(hightree) then
                 internalerror(200212201);
               firstpass(hightree);
               secondpass(hightree);
               { generate compares }
               freereg:=false;
               if (right.location.loc in [LOC_REGISTER,LOC_CREGISTER]) then
                 hreg:=right.location.register
               else
                 begin
                 {$ifdef newra}
                   hreg:=rg.getregisterint(exprasmlist,OS_INT);
                 {$else}
                   hreg := cg.get_scratch_reg_int(exprasmlist,OS_INT);
                 {$endif}
                   freereg:=true;
                   cg.a_load_loc_reg(exprasmlist,OS_INT,right.location,hreg);
                 end;
               objectlibrary.getlabel(neglabel);
               objectlibrary.getlabel(poslabel);
               cg.a_cmp_const_reg_label(exprasmlist,OS_INT,OC_LT,0,hreg,poslabel);
               cg.a_cmp_loc_reg_label(exprasmlist,OS_INT,OC_BE,hightree.location,hreg,neglabel);
             {$ifdef newra}
               if freereg then
                 rg.ungetregisterint(exprasmlist,hreg);
             {$else}
               if freereg then
                 cg.free_scratch_reg(exprasmlist,hreg);
             {$endif}
               cg.a_label(exprasmlist,poslabel);
               cg.a_call_name(exprasmlist,'FPC_RANGEERROR');
               cg.a_label(exprasmlist,neglabel);
               { release hightree }
               location_release(exprasmlist,hightree.location);
               hightree.free;
             end;
          end
         else
          if is_dynamic_array(left.resulttype.def) then
            begin
            {$ifndef newra}
               rg.saveusedintregisters(exprasmlist,pushed,all_intregisters);
            {$endif}
               cg.a_param_loc(exprasmlist,right.location,paramanager.getintparaloc(2));
               cg.a_param_loc(exprasmlist,left.location,paramanager.getintparaloc(1));
            {$ifdef newra}
               hreg.enum:=R_INTREGISTER;
               for i:=first_supreg to last_supreg do
                 if i<>RS_FRAME_POINTER_REG then
                   begin
                     hreg.number:=i shl 8 or R_SUBWHOLE;
                     rg.getexplicitregisterint(exprasmlist,hreg.number);
                   end;
            {$else}
               rg.saveintregvars(exprasmlist,all_intregisters);
            {$endif}
               cg.a_call_name(exprasmlist,'FPC_DYNARRAY_RANGECHECK');
            {$ifdef newra}
               for i:=first_supreg to last_supreg do
                 if i<>RS_FRAME_POINTER_REG then
                   begin
                     hreg.number:=i shl 8 or R_SUBWHOLE;
                     rg.ungetregisterint(exprasmlist,hreg);
                   end;
            {$else}
               rg.restoreusedintregisters(exprasmlist,pushed);
            {$endif}
            end
         else
           cg.g_rangecheck(exprasmlist,right.location,right.resulttype.def,left.resulttype.def);
       end;


    procedure tcgvecnode.pass_2;

      var
         extraoffset : longint;
         t : tnode;
         href : treference;
      {$ifdef newra}
         hreg:Tregister;
         i:Tsuperregister;
      {$else}
         pushed : tpushedsavedint;
      {$endif}
         isjump  : boolean;
         otl,ofl : tasmlabel;
         newsize : tcgsize;
         pushedregs : tmaybesave;
      begin
         newsize:=def_cgsize(resulttype.def);
         secondpass(left);
         if left.location.loc=LOC_CREFERENCE then
           location_reset(location,LOC_CREFERENCE,newsize)
         else
           location_reset(location,LOC_REFERENCE,newsize);


         { an ansistring needs to be dereferenced }
         if is_ansistring(left.resulttype.def) or
            is_widestring(left.resulttype.def) then
           begin
              if nf_callunique in flags then
                begin
                   if left.location.loc<>LOC_REFERENCE then
                     internalerror(200304236);
                {$ifndef newra}
                   rg.saveusedintregisters(exprasmlist,pushed,all_intregisters);
                {$endif}
                   cg.a_paramaddr_ref(exprasmlist,left.location.reference,paramanager.getintparaloc(1));
                {$ifdef newra}
                   hreg.enum:=R_INTREGISTER;
                   for i:=first_supreg to last_supreg do
                     if i<>RS_FRAME_POINTER_REG then
                       begin
                         hreg.number:=i shl 8 or R_SUBWHOLE;
                         rg.getexplicitregisterint(exprasmlist,hreg.number);
                       end;
                {$else}
                   rg.saveintregvars(exprasmlist,all_intregisters);
                {$endif}
                   cg.a_call_name(exprasmlist,'FPC_'+upper(tstringdef(left.resulttype.def).stringtypname)+'_UNIQUE');
                {$ifdef newra}
                   for i:=first_supreg to last_supreg do
                     if i<>RS_FRAME_POINTER_REG then
                       begin
                         hreg.number:=i shl 8 or R_SUBWHOLE;
                         rg.ungetregisterint(exprasmlist,hreg);
                       end;
                {$else}
                   rg.restoreusedintregisters(exprasmlist,pushed);
                {$endif}
                end;

              case left.location.loc of
                LOC_REGISTER,
                LOC_CREGISTER :
                  location.reference.base:=left.location.register;
                LOC_CREFERENCE,
                LOC_REFERENCE :
                  begin
                    location_release(exprasmlist,left.location);
                    location.reference.base:=rg.getaddressregister(exprasmlist);
                    cg.a_load_ref_reg(exprasmlist,OS_ADDR,OS_ADDR,left.location.reference,location.reference.base);
                  end;
                else
                  internalerror(2002032218);
              end;

              { check for a zero length string,
                we can use the ansistring routine here }
              if (cs_check_range in aktlocalswitches) then
                begin
                {$ifndef newra}
                   rg.saveusedintregisters(exprasmlist,pushed,all_intregisters);
                {$endif}
                   cg.a_param_reg(exprasmlist,OS_ADDR,location.reference.base,paramanager.getintparaloc(1));
                {$ifdef newra}
                   hreg.enum:=R_INTREGISTER;
                   for i:=first_supreg to last_supreg do
                     if i<>RS_FRAME_POINTER_REG then
                       begin
                         hreg.number:=i shl 8 or R_SUBWHOLE;
                         rg.getexplicitregisterint(exprasmlist,hreg.number);
                       end;
                {$else}
                   rg.saveintregvars(exprasmlist,all_intregisters);
                {$endif}
                   cg.a_call_name(exprasmlist,'FPC_'+upper(tstringdef(left.resulttype.def).stringtypname)+'_CHECKZERO');
                {$ifdef newra}
                   for i:=first_supreg to last_supreg do
                     if i<>RS_FRAME_POINTER_REG then
                       begin
                         hreg.number:=i shl 8 or R_SUBWHOLE;
                         rg.ungetregisterint(exprasmlist,hreg);
                       end;
                {$else}
                   rg.restoreusedintregisters(exprasmlist,pushed);
                {$endif}
                end;

              { in ansistrings/widestrings S[1] is p<w>char(S)[0] !! }
              if is_ansistring(left.resulttype.def) then
                dec(location.reference.offset)
              else
                dec(location.reference.offset,2);
           end
         else if is_dynamic_array(left.resulttype.def) then
           begin
              case left.location.loc of
                LOC_REGISTER,
                LOC_CREGISTER :
                  location.reference.base:=left.location.register;
                LOC_REFERENCE,
                LOC_CREFERENCE :
                  begin
                     location_release(exprasmlist,left.location);
                     location.reference.base:=rg.getaddressregister(exprasmlist);
                     cg.a_load_ref_reg(exprasmlist,OS_ADDR,OS_ADDR,
                      left.location.reference,location.reference.base);
                  end;
                else
                  internalerror(2002032219);
              end;
           end
         else
           location_copy(location,left.location);

         { offset can only differ from 0 if arraydef }
         if (left.resulttype.def.deftype=arraydef) and
            not(is_dynamic_array(left.resulttype.def)) then
           dec(location.reference.offset,get_mul_size*tarraydef(left.resulttype.def).lowrange);

         if right.nodetype=ordconstn then
           begin
              { offset can only differ from 0 if arraydef }
              case left.resulttype.def.deftype of
                arraydef :
                  begin
                     if not(is_open_array(left.resulttype.def)) and
                        not(is_array_of_const(left.resulttype.def)) and
                        not(is_dynamic_array(left.resulttype.def)) then
                       begin
                          if (tordconstnode(right).value>tarraydef(left.resulttype.def).highrange) or
                             (tordconstnode(right).value<tarraydef(left.resulttype.def).lowrange) then
                            begin
                              { this should be caught in the resulttypepass! (JM) }
                              if (cs_check_range in aktlocalswitches) then
                                CGMessage(parser_e_range_check_error)
                              else
                                CGMessage(parser_w_range_check_error);
                            end;
                       end
                     else
                       begin
                          { range checking for open and dynamic arrays needs
                            runtime code }
                          secondpass(right);
                          if (cs_check_range in aktlocalswitches) then
                            rangecheck_array;
                       end;
                  end;
                stringdef :
                  begin
                    if (cs_check_range in aktlocalswitches) then
                     begin
                       case tstringdef(left.resulttype.def).string_typ of
                         { it's the same for ansi- and wide strings }
                         st_widestring,
                         st_ansistring:
                           begin
                            {$ifndef newra}
                              rg.saveusedintregisters(exprasmlist,pushed,all_intregisters);
                            {$endif}
                              cg.a_param_const(exprasmlist,OS_INT,tordconstnode(right).value,paramanager.getintparaloc(2));
                              href:=location.reference;
                              dec(href.offset,7);
                              cg.a_param_ref(exprasmlist,OS_INT,href,paramanager.getintparaloc(1));
                            {$ifdef newra}
                              hreg.enum:=R_INTREGISTER;
                              for i:=first_supreg to last_supreg do
                               if i<>RS_FRAME_POINTER_REG then
                                  begin
                                    hreg.number:=i shl 8 or R_SUBWHOLE;
                                    rg.getexplicitregisterint(exprasmlist,hreg.number);
                                  end;
                            {$else}
                              rg.saveintregvars(exprasmlist,all_intregisters);
                            {$endif}
                              cg.a_call_name(exprasmlist,'FPC_'+upper(tstringdef(left.resulttype.def).stringtypname)+'_RANGECHECK');
                            {$ifdef newra}
                              for i:=first_supreg to last_supreg do
                               if i<>RS_FRAME_POINTER_REG then
                                  begin
                                    hreg.number:=i shl 8 or R_SUBWHOLE;
                                    rg.ungetregisterint(exprasmlist,hreg);
                                  end;
                            {$else}
                              rg.restoreusedintregisters(exprasmlist,pushed);
                            {$endif}
                           end;

                         st_shortstring:
                           begin
                              {!!!!!!!!!!!!!!!!!}
                           end;

                         st_longstring:
                           begin
                              {!!!!!!!!!!!!!!!!!}
                           end;
                       end;
                     end;
                   end;
              end;
              inc(location.reference.offset,
                  get_mul_size*tordconstnode(right).value);
           end
         else
         { not nodetype=ordconstn }
           begin
              if (cs_regalloc in aktglobalswitches) and
                 { if we do range checking, we don't }
                 { need that fancy code (it would be }
                 { buggy)                            }
                 not(cs_check_range in aktlocalswitches) and
                 (left.resulttype.def.deftype=arraydef) then
                begin
                   extraoffset:=0;
                   if (right.nodetype=addn) then
                     begin
                        if taddnode(right).right.nodetype=ordconstn then
                          begin
                             extraoffset:=tordconstnode(taddnode(right).right).value;
                             t:=taddnode(right).left;
                             { First pass processed this with the assumption   }
                             { that there was an add node which may require an }
                             { extra register. Fake it or die with IE10 (JM)   }
                             t.registers32 := taddnode(right).registers32;
                             taddnode(right).left:=nil;
                             right.free;
                             right:=t;
                          end
                        else if taddnode(right).left.nodetype=ordconstn then
                          begin
                             extraoffset:=tordconstnode(taddnode(right).left).value;
                             t:=taddnode(right).right;
                             t.registers32 :=  right.registers32;
                             taddnode(right).right:=nil;
                             right.free;
                             right:=t;
                          end;
                     end
                   else if (right.nodetype=subn) then
                     begin
                        if taddnode(right).right.nodetype=ordconstn then
                          begin
                             extraoffset:=-tordconstnode(taddnode(right).right).value;
                             t:=taddnode(right).left;
                             t.registers32 :=  right.registers32;
                             taddnode(right).left:=nil;
                             right.free;
                             right:=t;
                          end
{ You also have to negate right.right in this case! I can't add an
  unaryminusn without causing a crash, so I've disabled it (JM)
                        else if right.left.nodetype=ordconstn then
                          begin
                             extraoffset:=right.left.value;
                             t:=right.right;
                             t^.registers32 :=  right.registers32;
                             putnode(right);
                             putnode(right.left);
                             right:=t;
                         end;}
                     end;
                   inc(location.reference.offset,
                       get_mul_size*extraoffset);
                end;
              { calculate from left to right }
              if not(location.loc in [LOC_CREFERENCE,LOC_REFERENCE]) then
                internalerror(200304237);
              isjump:=(right.location.loc=LOC_JUMP);
              if isjump then
               begin
                 otl:=truelabel;
                 objectlibrary.getlabel(truelabel);
                 ofl:=falselabel;
                 objectlibrary.getlabel(falselabel);
               end;
            {$ifndef newra}
              maybe_save(exprasmlist,right.registers32,location,pushedregs);
            {$endif}
              secondpass(right);
            {$ifndef newra}
              maybe_restore(exprasmlist,location,pushedregs);
            {$endif}

              if cs_check_range in aktlocalswitches then
               begin
                 if left.resulttype.def.deftype=arraydef then
                   rangecheck_array;
               end;

              location_force_reg(exprasmlist,right.location,OS_32,false);

              if isjump then
               begin
                 truelabel:=otl;
                 falselabel:=ofl;
               end;

            { produce possible range check code: }
              if cs_check_range in aktlocalswitches then
               begin
                 if left.resulttype.def.deftype=arraydef then
                   begin
                     { done defore (PM) }
                   end
                 else if (left.resulttype.def.deftype=stringdef) then
                   begin
                      case tstringdef(left.resulttype.def).string_typ of
                         { it's the same for ansi- and wide strings }
                         st_widestring,
                         st_ansistring:
                           begin
                            {$ifndef newra}
                              rg.saveusedintregisters(exprasmlist,pushed,all_intregisters);
                            {$endif}
                              cg.a_param_reg(exprasmlist,OS_INT,right.location.register,paramanager.getintparaloc(2));
                              href:=location.reference;
                              dec(href.offset,7);
                              cg.a_param_ref(exprasmlist,OS_INT,href,paramanager.getintparaloc(1));
                            {$ifdef newra}
                              hreg.enum:=R_INTREGISTER;
                              for i:=first_supreg to last_supreg do
                               if i<>RS_FRAME_POINTER_REG then
                                  begin
                                    hreg.number:=i shl 8 or R_SUBWHOLE;
                                    rg.getexplicitregisterint(exprasmlist,hreg.number);
                                  end;
                            {$else}
                              rg.saveintregvars(exprasmlist,all_intregisters);
                            {$endif}
                              cg.a_call_name(exprasmlist,'FPC_'+upper(tstringdef(left.resulttype.def).stringtypname)+'_RANGECHECK');
                            {$ifdef newra}
                              for i:=first_supreg to last_supreg do
                               if i<>RS_FRAME_POINTER_REG then
                                  begin
                                    hreg.number:=i shl 8 or R_SUBWHOLE;
                                    rg.ungetregisterint(exprasmlist,hreg);
                                  end;
                            {$else}
                              rg.restoreusedintregisters(exprasmlist,pushed);
                            {$endif}
                           end;
                         st_shortstring:
                           begin
                              {!!!!!!!!!!!!!!!!!}
                           end;
                         st_longstring:
                           begin
                              {!!!!!!!!!!!!!!!!!}
                           end;
                      end;
                   end;
               end;

              { insert the register and the multiplication factor in the
                reference }
              update_reference_reg_mul(right.location.register,get_mul_size);
           end;

        location.size:=newsize;
      end;


begin
   cloadvmtaddrnode:=tcgloadvmtaddrnode;
   caddrnode:=tcgaddrnode;
   cdoubleaddrnode:=tcgdoubleaddrnode;
   cderefnode:=tcgderefnode;
   csubscriptnode:=tcgsubscriptnode;
   cwithnode:=tcgwithnode;
   cvecnode:=tcgvecnode;
end.
{
  $Log$
  Revision 1.59  2003-06-03 21:11:09  peter
    * cg.a_load_* get a from and to size specifier
    * makeregsize only accepts newregister
    * i386 uses generic tcgnotnode,tcgunaryminus

  Revision 1.58  2003/06/03 13:01:59  daniel
    * Register allocator finished

  Revision 1.57  2003/06/02 22:35:45  florian
    * better handling of CREGISTER in subscript nodes

  Revision 1.56  2003/06/01 21:38:06  peter
    * getregisterfpu size parameter added
    * op_const_reg size parameter added
    * sparc updates

  Revision 1.55  2003/05/30 23:49:18  jonas
    * a_load_loc_reg now has an extra size parameter for the destination
      register (properly fixes what I worked around in revision 1.106 of
      ncgutil.pas)

  Revision 1.54  2003/05/15 16:10:37  florian
    * fixed getintparaloc call for ansi- and widestring range checking

  Revision 1.53  2003/05/11 21:37:03  peter
    * moved implicit exception frame from ncgutil to psub
    * constructor/destructor helpers moved from cobj/ncgutil to psub

  Revision 1.52  2003/05/11 14:45:12  peter
    * tloadnode does not support objectsymtable,withsymtable anymore
    * withnode cleanup
    * direct with rewritten to use temprefnode

  Revision 1.51  2003/05/09 17:47:02  peter
    * self moved to hidden parameter
    * removed hdisposen,hnewn,selfn

  Revision 1.50  2003/05/07 09:16:23  mazen
  - non used units removed from uses clause

  Revision 1.49  2003/04/27 11:21:33  peter
    * aktprocdef renamed to current_procdef
    * procinfo renamed to current_procinfo
    * procinfo will now be stored in current_module so it can be
      cleaned up properly
    * gen_main_procsym changed to create_main_proc and release_main_proc
      to also generate a tprocinfo structure
    * fixed unit implicit initfinal

  Revision 1.48  2003/04/22 23:50:22  peter
    * firstpass uses expectloc
    * checks if there are differences between the expectloc and
      location.loc from secondpass in EXTDEBUG

  Revision 1.47  2003/04/22 13:47:08  peter
    * fixed C style array of const
    * fixed C array passing
    * fixed left to right with high parameters

  Revision 1.46  2003/04/22 10:09:35  daniel
    + Implemented the actual register allocator
    + Scratch registers unavailable when new register allocator used
    + maybe_save/maybe_restore unavailable when new register allocator used

  Revision 1.45  2003/04/06 21:11:23  olle
    * changed newasmsymbol to newasmsymboldata for data symbols

  Revision 1.44  2003/03/28 19:16:56  peter
    * generic constructor working for i386
    * remove fixed self register
    * esi added as address register for i386

  Revision 1.43  2003/03/12 22:43:38  jonas
    * more powerpc and generic fixes related to the new register allocator

  Revision 1.42  2003/02/19 22:00:14  daniel
    * Code generator converted to new register notation
    - Horribily outdated todo.txt removed

  Revision 1.41  2003/01/30 21:46:57  peter
    * self fixes for static methods (merged)

  Revision 1.40  2003/01/08 18:43:56  daniel
   * Tregister changed into a record

  Revision 1.39  2002/12/20 18:13:19  peter
    * no rangecheck for openarrays with cdecl

  Revision 1.38  2002/12/17 22:19:33  peter
    * fixed pushing of records>8 bytes with stdcall
    * simplified hightree loading

  Revision 1.37  2002/12/08 13:39:03  carl
    + some documentation added

  Revision 1.36  2002/12/07 14:14:19  carl
    * bugfix on invalid typecast

  Revision 1.35  2002/11/25 17:43:18  peter
    * splitted defbase in defutil,symutil,defcmp
    * merged isconvertable and is_equal into compare_defs(_ext)
    * made operator search faster by walking the list only once

  Revision 1.34  2002/11/24 18:19:20  carl
    + checkpointer for interfaces also

  Revision 1.33  2002/11/23 22:50:06  carl
    * some small speed optimizations
    + added several new warnings/hints

  Revision 1.32  2002/11/15 01:58:51  peter
    * merged changes from 1.0.7 up to 04-11
      - -V option for generating bug report tracing
      - more tracing for option parsing
      - errors for cdecl and high()
      - win32 import stabs
      - win32 records<=8 are returned in eax:edx (turned off by default)
      - heaptrc update
      - more info for temp management in .s file with EXTDEBUG

  Revision 1.31  2002/10/09 20:24:47  florian
    + range checking for dyn. arrays

  Revision 1.30  2002/10/07 21:30:45  peter
    * rangecheck for open arrays added

  Revision 1.29  2002/10/05 12:43:25  carl
    * fixes for Delphi 6 compilation
     (warning : Some features do not work under Delphi)

  Revision 1.28  2002/09/17 18:54:02  jonas
    * a_load_reg_reg() now has two size parameters: source and dest. This
      allows some optimizations on architectures that don't encode the
      register size in the register name.

  Revision 1.27  2002/09/07 15:25:03  peter
    * old logs removed and tabs fixed

  Revision 1.26  2002/09/01 18:46:01  peter
    * fixed generic tcgvecnode
    * move code that updates a reference with index register and multiplier
      to separate method so it can be overriden for scaled indexing
    * i386 uses generic tcgvecnode

  Revision 1.25  2002/08/23 16:14:48  peter
    * tempgen cleanup
    * tt_noreuse temp type added that will be used in genentrycode

  Revision 1.24  2002/08/15 08:13:54  carl
    - a_load_sym_ofs_reg removed
    * loadvmt now calls loadaddr_ref_reg instead

  Revision 1.23  2002/08/11 14:32:26  peter
    * renamed current_library to objectlibrary

  Revision 1.22  2002/08/11 13:24:12  peter
    * saving of asmsymbols in ppu supported
    * asmsymbollist global is removed and moved into a new class
      tasmlibrarydata that will hold the info of a .a file which
      corresponds with a single module. Added librarydata to tmodule
      to keep the library info stored for the module. In the future the
      objectfiles will also be stored to the tasmlibrarydata class
    * all getlabel/newasmsymbol and friends are moved to the new class

  Revision 1.21  2002/08/11 11:36:57  jonas
    * always first try to use base and only then index

  Revision 1.20  2002/08/11 06:14:40  florian
    * fixed powerpc compilation problems

  Revision 1.19  2002/08/10 14:46:29  carl
    + moved target_cpu_string to cpuinfo
    * renamed asmmode enum.
    * assembler reader has now less ifdef's
    * move from nppcmem.pas -> ncgmem.pas vec. node.

  Revision 1.18  2002/07/28 21:34:31  florian
    * more powerpc fixes
    + dummy tcgvecnode

  Revision 1.17  2002/07/11 14:41:28  florian
    * start of the new generic parameter handling

  Revision 1.16  2002/07/07 09:52:32  florian
    * powerpc target fixed, very simple units can be compiled
    * some basic stuff for better callparanode handling, far from being finished

  Revision 1.15  2002/07/01 18:46:23  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.14  2002/07/01 16:23:53  peter
    * cg64 patch
    * basics for currency
    * asnode updates for class and interface (not finished)

  Revision 1.13  2002/05/20 13:30:40  carl
  * bugfix of hdisponen (base must be set, not index)
  * more portability fixes

  Revision 1.12  2002/05/18 13:34:09  peter
    * readded missing revisions

  Revision 1.11  2002/05/16 19:46:37  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.9  2002/05/12 16:53:07  peter
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

  Revision 1.8  2002/04/20 21:32:23  carl
  + generic FPC_CHECKPOINTER
  + first parameter offset in stack now portable
  * rename some constants
  + move some cpu stuff to other units
  - remove unused constents
  * fix stacksize for some targets
  * fix generic size problems which depend now on EXTEND_SIZE constant

  Revision 1.7  2002/04/15 18:58:47  carl
  + target_info.size_of_pointer -> pointer_Size

  Revision 1.6  2002/04/04 19:05:57  peter
    * removed unused units
    * use tlocation.size in cg.a_*loc*() routines

}
