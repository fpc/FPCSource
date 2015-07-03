{
    Copyright (c) 2014 by Jonas Maebe

    Generates code for typed constant declarations for the LLVM target

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
unit nllvmtcon;

{$i fpcdefs.inc}

interface

  uses
    cclasses,constexp,globtype,
    aasmbase,aasmtai,aasmcnst,aasmllvm,
    symconst,symtype,symdef,symsym,
    ngtcon;

  type
    tllvmaggregateinformation = class(taggregateinformation)
     private
      faggai: tai_aggregatetypedconst;
      fanonrecalignpos: longint;
     public
      constructor create(_def: tdef; _typ: ttypedconstkind); override;

      property aggai: tai_aggregatetypedconst read faggai write faggai;
      property anonrecalignpos: longint read fanonrecalignpos write fanonrecalignpos;
    end;

    tllvmtai_typedconstbuilder = class(ttai_typedconstbuilder)
     protected type
      public
       { set the default value for caggregateinformation (= tllvmaggregateinformation) }
       class constructor classcreate;
     protected
      fqueued_def: tdef;
      fqueued_tai,
      flast_added_tai: tai;
      fqueued_tai_opidx: longint;

      procedure finalize_asmlist(sym: tasmsymbol; def: tdef; section: TAsmSectiontype; const secname: TSymStr; alignment: shortint; const options: ttcasmlistoptions); override;
      { outerai: the ai that should become fqueued_tai in case it's still nil,
          or that should be filled in the fqueued_tai_opidx of the current
          fqueued_tai if it's not nil
        innerai: the innermost ai (possibly an operand of outerai) in which
          newindex indicates which operand is empty and can be filled with the
          next queued tai }
      procedure update_queued_tai(resdef: tdef; outerai, innerai: tai; newindex: longint);
      function wrap_with_type(p: tai; def: tdef): tai;
      procedure do_emit_tai(p: tai; def: tdef); override;
      procedure mark_anon_aggregate_alignment; override;
      procedure insert_marked_aggregate_alignment(def: tdef); override;
      procedure begin_aggregate_internal(def: tdef; anonymous: boolean); override;
      procedure end_aggregate_internal(def: tdef; anonymous: boolean); override;

      function get_internal_data_section_start_label: tasmlabel; override;
      function get_internal_data_section_internal_label: tasmlabel; override;
     public
      destructor destroy; override;
      procedure emit_tai(p: tai; def: tdef); override;
      procedure emit_tai_procvar2procdef(p: tai; pvdef: tprocvardef); override;
      procedure emit_string_offset(const ll: tasmlabofs; const strlength: longint; const st: tstringtype; const winlikewidestring: boolean; const charptrdef: tdef); override;
      procedure queue_init(todef: tdef); override;
      procedure queue_vecn(def: tdef; const index: tconstexprint); override;
      procedure queue_subscriptn(def: tabstractrecorddef; vs: tfieldvarsym); override;
      procedure queue_typeconvn(fromdef, todef: tdef); override;
      procedure queue_emit_staticvar(vs: tstaticvarsym); override;
      procedure queue_emit_asmsym(sym: tasmsymbol; def: tdef); override;
      procedure queue_emit_ordconst(value: int64; def: tdef); override;

      class function get_string_symofs(typ: tstringtype; winlikewidestring: boolean): pint; override;
    end;


implementation

  uses
    verbose,systems,
    aasmdata,
    cpubase,cpuinfo,llvmbase,
    symbase,symtable,llvmdef,defutil;

  { tllvmaggregateinformation }

   constructor tllvmaggregateinformation.create(_def: tdef; _typ: ttypedconstkind);
     begin
       inherited;
       fanonrecalignpos:=-1;
     end;


  class constructor tllvmtai_typedconstbuilder.classcreate;
    begin
      caggregateinformation:=tllvmaggregateinformation;
    end;


  procedure tllvmtai_typedconstbuilder.finalize_asmlist(sym: tasmsymbol; def: tdef; section: TAsmSectiontype; const secname: TSymStr; alignment: shortint; const options: ttcasmlistoptions);
    var
      newasmlist: tasmlist;
    begin
      { todo }
      if section = sec_user then
        internalerror(2014052904);
      newasmlist:=tasmlist.create;
      { llvm declaration with as initialisation data all the elements from the
        original asmlist }
      newasmlist.concat(taillvmdecl.create(sym,def,fasmlist,section,alignment));
      fasmlist:=newasmlist;
    end;


  procedure tllvmtai_typedconstbuilder.update_queued_tai(resdef: tdef; outerai, innerai: tai; newindex: longint);
    begin
      { the outer tai must always be a typed constant (possibly a wrapper
        around a taillvm or so), in order for result type information to be
        available }
      if outerai.typ<>ait_typedconst then
        internalerror(2014060401);
      { is the result of the outermost expression different from the type of
        this typed const? -> insert type conversion }
      if not assigned(fqueued_tai) and
         (resdef<>fqueued_def) and
         (llvmencodetypename(resdef)<>llvmencodetypename(fqueued_def)) then
        queue_typeconvn(resdef,fqueued_def);
      if assigned(fqueued_tai) then
        begin
          taillvm(flast_added_tai).loadtai(fqueued_tai_opidx,outerai);
          { already flushed? }
          if fqueued_tai_opidx=-1 then
            internalerror(2014062201);
        end
      else
        begin
          fqueued_tai:=outerai;
          fqueued_def:=resdef;
        end;
      fqueued_tai_opidx:=newindex;
      flast_added_tai:=innerai;
    end;


  function tllvmtai_typedconstbuilder.wrap_with_type(p: tai; def: tdef): tai;
    begin
      result:=tai_simpletypedconst.create(tck_simple,def,p);
    end;


  destructor tllvmtai_typedconstbuilder.destroy;
    begin
      inherited destroy;
    end;


  procedure tllvmtai_typedconstbuilder.emit_tai(p: tai; def: tdef);
    var
      arrdef: tdef;
    begin
      { inside an aggregate, an 80 bit floating point number must be
        emitted as an array of 10 bytes to prevent ABI alignment and
        padding to 16 bytes }
      if (def.typ=floatdef) and
         (tfloatdef(def).floattype=s80real) and
         assigned(curagginfo) then
        do_emit_extended_in_aggregate(p)
      else
        inherited;
    end;


  procedure tllvmtai_typedconstbuilder.do_emit_tai(p: tai; def: tdef);
    var
      ai: tai;
      stc: tai_abstracttypedconst;
      kind: ttypedconstkind;
      info: tllvmaggregateinformation;
    begin
      if queue_is_active then
        begin
          kind:=tck_simple;
          { finalise the queued expression }
          ai:=tai_simpletypedconst.create(kind,def,p);
          { set the new index to -1, so we internalerror should we try to
            add anything further }
          update_queued_tai(def,ai,ai,-1);
          { and emit it }
          stc:=tai_abstracttypedconst(fqueued_tai);
          def:=fqueued_def;
          { ensure we don't try to emit this one again }
          fqueued_tai:=nil;
        end
      else
        stc:=tai_simpletypedconst.create(tck_simple,def,p);
      info:=tllvmaggregateinformation(curagginfo);
      { these elements can be aggregates themselves, e.g. a shortstring can
        be emitted as a series of bytes and string data arrays }
      kind:=aggregate_kind(def);
      if (kind<>tck_simple) then
        begin
          if not assigned(info) or
             (info.aggai.adetyp<>kind) then
           internalerror(2014052906);
        end;
      if assigned(info) then
        info.aggai.addvalue(stc)
      else
        inherited do_emit_tai(stc,def);
    end;


  procedure tllvmtai_typedconstbuilder.mark_anon_aggregate_alignment;
    var
      info: tllvmaggregateinformation;
    begin
      info:=tllvmaggregateinformation(curagginfo);
      info.anonrecalignpos:=info.aggai.valuecount;
    end;


  procedure tllvmtai_typedconstbuilder.insert_marked_aggregate_alignment(def: tdef);
    var
      info: tllvmaggregateinformation;
      fillbytes: asizeint;
    begin
      info:=tllvmaggregateinformation(curagginfo);
      if info.anonrecalignpos=-1 then
        internalerror(2014091501);
      fillbytes:=info.prepare_next_field(def);
      while fillbytes>0 do
        begin
          info.aggai.insertvaluebeforepos(tai_simpletypedconst.create(tck_simple,u8inttype,tai_const.create_8bit(0)),info.anonrecalignpos);
          dec(fillbytes);
        end;
    end;


  procedure tllvmtai_typedconstbuilder.emit_tai_procvar2procdef(p: tai; pvdef: tprocvardef);
    begin
      if not pvdef.is_addressonly then
        pvdef:=cprocvardef.getreusableprocaddr(pvdef);
      emit_tai(p,pvdef);
    end;


  procedure tllvmtai_typedconstbuilder.emit_string_offset(const ll: tasmlabofs; const strlength: longint; const st: tstringtype; const winlikewidestring: boolean; const charptrdef: tdef);
    var
      srsym     : tsym;
      srsymtable: tsymtable;
      strrecdef : trecorddef;
      offset: pint;
      field: tfieldvarsym;
      dataptrdef: tdef;
    begin
      { nil pointer? }
      if not assigned(ll.lab) then
        begin
          if ll.ofs<>0 then
            internalerror(2015030701);
          inherited;
          exit;
        end;
      { if the returned offset is <> 0, then the string data
        starts at that offset -> translate to a field for the
        high level code generator }
      if ll.ofs<>0 then
        begin
          { get the recorddef for this string constant }
          if not searchsym_type(ctai_typedconstbuilder.get_dynstring_rec_name(st,winlikewidestring,strlength),srsym,srsymtable) then
            internalerror(2014080406);
          strrecdef:=trecorddef(ttypesym(srsym).typedef);
          { offset in the record of the the string data }
          offset:=ctai_typedconstbuilder.get_string_symofs(st,winlikewidestring);
          { field corresponding to this offset }
          field:=trecordsymtable(strrecdef.symtable).findfieldbyoffset(offset);
          { pointerdef to the string data array }
          dataptrdef:=cpointerdef.getreusable(field.vardef);
          queue_init(charptrdef);
          queue_addrn(dataptrdef,charptrdef);
          queue_subscriptn(strrecdef,field);
          queue_emit_asmsym(ll.lab,strrecdef);
        end
      else
       { since llvm doesn't support labels in the middle of structs, this
         offset should never be 0  }
       internalerror(2014080506);
    end;


  procedure tllvmtai_typedconstbuilder.begin_aggregate_internal(def: tdef; anonymous: boolean);
    var
      agg: tai_aggregatetypedconst;
      tck: ttypedconstkind;
      curagg: tllvmaggregateinformation;
    begin
      tck:=aggregate_kind(def);
      if tck<>tck_simple then
        begin
          { create new typed const aggregate }
          agg:=tai_aggregatetypedconst.create(tck,def);
          { either add to the current typed const aggregate (if nested), or
            emit to the asmlist (if top level) }
          curagg:=tllvmaggregateinformation(curagginfo);
          if assigned(curagg) then
            curagg.aggai.addvalue(agg)
          else
            fasmlist.concat(agg);
          { create aggregate information for this new aggregate }
          inherited;
          { set new current typed const aggregate }
          tllvmaggregateinformation(curagginfo).aggai:=agg
        end
      else
       inherited;
    end;


  procedure tllvmtai_typedconstbuilder.end_aggregate_internal(def: tdef; anonymous: boolean);
    var
      info: tllvmaggregateinformation;
    begin
      if aggregate_kind(def)<>tck_simple then
        begin
          info:=tllvmaggregateinformation(curagginfo);
          if not assigned(info) then
            internalerror(2014060101);
          info.aggai.finish;
        end;
      inherited;
    end;


  function tllvmtai_typedconstbuilder.get_internal_data_section_start_label: tasmlabel;
    begin
      { let llvm take care of everything by creating internal nameless
        constants }
      current_asmdata.getlocaldatalabel(result);
    end;


  function tllvmtai_typedconstbuilder.get_internal_data_section_internal_label: tasmlabel;
    begin
      current_asmdata.getlocaldatalabel(result);
    end;


  procedure tllvmtai_typedconstbuilder.do_emit_extended_in_aggregate(p: tai);
    type
      p80realval =^t80realval;
      t80realval = packed record
        case byte of
          0: (v: ts80real);
          1: (a: array[0..9] of byte);
      end;

    var
      arrdef: tdef;
      i: longint;
      realval: p80realval;
    begin
      { emit as an array of 10 bytes }
      arrdef:=carraydef.getreusable(u8inttype,10);
      maybe_begin_aggregate(arrdef);
      if (p.typ<>ait_realconst) then
        internalerror(2015062401);
      realval:=p80realval(@tai_realconst(p).value.s80val);
      if target_info.endian=source_info.endian then
        for i:=0 to 9 do
          emit_tai(tai_const.Create_8bit(realval^.a[i]),u8inttype)
      else
        for i:=9 downto 0 do
          emit_tai(tai_const.Create_8bit(realval^.a[i]),u8inttype);
      maybe_end_aggregate(arrdef);
      { free the original constant, since we didn't emit it }
      p.free;
    end;


  procedure tllvmtai_typedconstbuilder.queue_init(todef: tdef);
    begin
      inherited;
      fqueued_tai:=nil;
      flast_added_tai:=nil;
      fqueued_tai_opidx:=-1;
      fqueued_def:=todef;
    end;


  procedure tllvmtai_typedconstbuilder.queue_vecn(def: tdef; const index: tconstexprint);
    var
      ai: taillvm;
      aityped: tai;
      eledef: tdef;
    begin
      { update range checking info }
      inherited;
      ai:=taillvm.getelementptr_reg_tai_size_const(NR_NO,nil,ptrsinttype,index.svalue,true);
      case def.typ of
        arraydef:
          eledef:=tarraydef(def).elementdef;
        stringdef:
          case tstringdef(def).stringtype of
            st_shortstring,
            st_longstring,
            st_ansistring:
              eledef:=cansichartype;
            st_widestring,
            st_unicodestring:
              eledef:=cwidechartype;
            else
              internalerror(2014062202);
          end;
        else
          internalerror(2014062203);
      end;
      aityped:=wrap_with_type(ai,cpointerdef.getreusable(eledef));
      update_queued_tai(cpointerdef.getreusable(eledef),aityped,ai,1);
    end;


  procedure tllvmtai_typedconstbuilder.queue_subscriptn(def: tabstractrecorddef; vs: tfieldvarsym);
    var
      getllvmfieldaddr,
      getpascalfieldaddr,
      getllvmfieldaddrtyped: tai;
      llvmfielddef: tdef;
    begin
      { update range checking info }
      inherited;
      llvmfielddef:=tabstractrecordsymtable(def.symtable).llvmst[vs].def;
      { get the address of the llvm-struct field that corresponds to this
        Pascal field }
      getllvmfieldaddr:=taillvm.getelementptr_reg_tai_size_const(NR_NO,nil,s32inttype,vs.llvmfieldnr,true);
      { getelementptr doesn't contain its own resultdef, so encode it via a
        tai_simpletypedconst tai }
      getllvmfieldaddrtyped:=wrap_with_type(getllvmfieldaddr,cpointerdef.getreusable(llvmfielddef));
      { if it doesn't match the requested field exactly (variant record),
        fixup the result }
      getpascalfieldaddr:=getllvmfieldaddrtyped;
      if (vs.offsetfromllvmfield<>0) or
         (llvmfielddef<>vs.vardef) then
        begin
          { offset of real field relative to llvm-struct field <> 0? }
          if vs.offsetfromllvmfield<>0 then
            begin
              { convert to a pointer to a 1-sized element }
              if llvmfielddef.size<>1 then
                begin
                  getpascalfieldaddr:=taillvm.op_reg_tai_size(la_bitcast,NR_NO,getpascalfieldaddr,u8inttype);
                  { update the current fielddef of the expression }
                  llvmfielddef:=u8inttype;
                end;
              { add the offset }
              getpascalfieldaddr:=taillvm.getelementptr_reg_tai_size_const(NR_NO,getpascalfieldaddr,ptrsinttype,vs.offsetfromllvmfield,true);
              { ... and set the result type of the getelementptr }
              getpascalfieldaddr:=wrap_with_type(getpascalfieldaddr,cpointerdef.getreusable(u8inttype));
              llvmfielddef:=u8inttype;
            end;
          { bitcast the data at the final offset to the right type }
          if llvmfielddef<>vs.vardef then
            getpascalfieldaddr:=wrap_with_type(taillvm.op_reg_tai_size(la_bitcast,NR_NO,getpascalfieldaddr,cpointerdef.getreusable(vs.vardef)),cpointerdef.getreusable(vs.vardef));
        end;
      update_queued_tai(cpointerdef.getreusable(vs.vardef),getpascalfieldaddr,getllvmfieldaddr,1);
    end;


  procedure tllvmtai_typedconstbuilder.queue_typeconvn(fromdef, todef: tdef);
    var
      ai: taillvm;
      typedai: tai;
      tmpintdef: tdef;
      op,
      firstop,
      secondop: tllvmop;
    begin
      inherited;
      { special case: procdef -> procvardef/pointerdef: must take address of
        the procdef }
      if (fromdef.typ=procdef) and
         (todef.typ<>procdef) then
        fromdef:=cprocvardef.getreusableprocaddr(tprocdef(fromdef));
      op:=llvmconvop(fromdef,todef);
      case op of
        la_ptrtoint_to_x,
        la_x_to_inttoptr:
          begin
            { convert via an integer with the same size as "x" }
            if op=la_ptrtoint_to_x then
              begin
                tmpintdef:=cgsize_orddef(def_cgsize(todef));
                firstop:=la_ptrtoint;
                secondop:=la_bitcast
              end
            else
              begin
                tmpintdef:=cgsize_orddef(def_cgsize(fromdef));
                firstop:=la_bitcast;
                secondop:=la_inttoptr;
              end;
            { since we have to queue operations from outer to inner, first queue
              the conversion from the tempintdef to the todef }
            ai:=taillvm.op_reg_tai_size(secondop,NR_NO,nil,todef);
            typedai:=wrap_with_type(ai,todef);
            update_queued_tai(todef,typedai,ai,1);
            todef:=tmpintdef;
            op:=firstop
          end;
      end;
      ai:=taillvm.op_reg_tai_size(op,NR_NO,nil,todef);
      typedai:=wrap_with_type(ai,todef);
      update_queued_tai(todef,typedai,ai,1);
    end;


  procedure tllvmtai_typedconstbuilder.queue_emit_staticvar(vs: tstaticvarsym);
    begin
      { we've already incorporated the offset via the inserted operations above,
        make sure it doesn't get emitted again as part of the tai_const for
        the tasmsymbol }
      fqueue_offset:=0;
      inherited;
    end;


  procedure tllvmtai_typedconstbuilder.queue_emit_asmsym(sym: tasmsymbol; def: tdef);
    begin
      { we've already incorporated the offset via the inserted operations above,
        make sure it doesn't get emitted again as part of the tai_const for
        the tasmsymbol }
      fqueue_offset:=0;
      inherited;
    end;


  procedure tllvmtai_typedconstbuilder.queue_emit_ordconst(value: int64; def: tdef);
    begin
      { no offset into an ordinal constant }
      if fqueue_offset<>0 then
        internalerror(2015030702);
      inherited;
    end;


  class function tllvmtai_typedconstbuilder.get_string_symofs(typ: tstringtype; winlikewidestring: boolean): pint;
    begin
      { LLVM does not support labels in the middle of a declaration }
      result:=get_string_header_size(typ,winlikewidestring);
    end;


begin
  ctai_typedconstbuilder:=tllvmtai_typedconstbuilder;
end.

