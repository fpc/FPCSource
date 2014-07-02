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
    symtype,symdef,symsym,
    ngtcon;

  type
    tllvmtai_typedconstbuilder = class(ttai_lowleveltypedconstbuilder)
     protected
      { aggregates (from outer to inner nested) that have been encountered,
        if any }
      faggregates: tfplist;

      fqueued_def: tdef;
      fqueued_tai,
      flast_added_tai: tai;
      fqueued_tai_opidx: longint;

      procedure finalize_asmlist(sym: tasmsymbol; def: tdef; section: TAsmSectiontype; const secname: TSymStr; alignment: shortint; lab: boolean); override;
      { outerai: the ai that should become fqueued_tai in case it's still nil,
          or that should be filled in the fqueued_tai_opidx of the current
          fqueued_tai if it's not nil
        innerai: the innermost ai (possibly an operand of outerai) in which
          newindex indicates which operand is empty and can be filled with the
          next queued tai }
      procedure update_queued_tai(resdef: tdef; outerai, innerai: tai; newindex: longint);
      procedure emit_tai_intern(p: tai; def: tdef; procvar2procdef: boolean);
     public
      constructor create; override;
      destructor destroy; override;
      procedure emit_tai(p: tai; def: tdef); override;
      procedure emit_tai_procvar2procdef(p: tai; pvdef: tprocvardef); override;
      procedure maybe_begin_aggregate(def: tdef); override;
      procedure maybe_end_aggregate(def: tdef); override;
      procedure queue_init(todef: tdef); override;
      procedure queue_vecn(def: tdef; const index: tconstexprint); override;
      procedure queue_subscriptn(def: tabstractrecorddef; vs: tfieldvarsym); override;
      procedure queue_typeconvn(fromdef, todef: tdef); override;
      procedure queue_emit_asmsym(sym: tasmsymbol; def: tdef); override;
    end;

implementation

  uses
    verbose,
    aasmdata,
    cpubase,llvmbase,
    symconst,symtable,llvmdef,defutil;


  procedure tllvmtai_typedconstbuilder.finalize_asmlist(sym: tasmsymbol; def: tdef; section: TAsmSectiontype; const secname: TSymStr; alignment: shortint; lab: boolean);
    var
      newasmlist: tasmlist;
    begin
      { todo }
      if section = sec_user then
        internalerror(2014052904);
      newasmlist:=tasmlist.create_without_marker;
      { llvm declaration with as initialisation data all the elements from the
        original asmlist }
      { TODO: propagate data/rodata different ("constant") }
      newasmlist.concat(taillvmdecl.create(sym,def,fasmlist));
      fasmlist:=newasmlist;
    end;

  procedure tllvmtai_typedconstbuilder.update_queued_tai(resdef: tdef; outerai, innerai: tai; newindex: longint);
    begin
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


  procedure tllvmtai_typedconstbuilder.emit_tai_intern(p: tai; def: tdef; procvar2procdef: boolean);
    var
      ai: tai;
      stc: tai_simpletypedconst;
      kind: ttypedconstkind;
    begin
      if assigned(fqueued_tai) then
        begin
          if not procvar2procdef then
            kind:=tck_simple
          else
            kind:=tck_simple_procvar2proc;
          { finalise the queued expression }
          ai:=tai_simpletypedconst.create(kind,def,p);
          { set the new index to -1, so we internalerror should we try to
            add anything further }
          update_queued_tai(def,ai,ai,-1);
          { and emit it }
          p:=fqueued_tai;
          def:=fqueued_def;
          { ensure we don't try to emit this one again }
          fqueued_tai:=nil;
        end;
      { these elements can be aggregates themselves, e.g. a shortstring can
        be emitted as a series of bytes and string data arrays }
      if not procvar2procdef then
        kind:=aggregate_kind(def)
      else
        kind:=tck_simple_procvar2proc;
      if not(kind in [tck_simple,tck_simple_procvar2proc]) and
         (not assigned(faggregates) or
          (faggregates.count=0) or
          (tai_aggregatetypedconst(faggregates[faggregates.count-1]).adetyp<>kind)) then
        internalerror(2014052906);
      stc:=tai_simpletypedconst.create(tck_simple,def,p);
      if assigned(faggregates) and
         (faggregates.count>0) then
        tai_aggregatetypedconst(faggregates[faggregates.count-1]).addvalue(stc)
      else
        inherited emit_tai(stc,def);
    end;


  constructor tllvmtai_typedconstbuilder.create;
    begin
      inherited create;
      { constructed as needed }
      faggregates:=nil;
    end;


  destructor tllvmtai_typedconstbuilder.destroy;
    begin
      faggregates.free;
      inherited destroy;
    end;


  procedure tllvmtai_typedconstbuilder.emit_tai(p: tai; def: tdef);
    begin
      emit_tai_intern(p,def,false);
    end;


  procedure tllvmtai_typedconstbuilder.emit_tai_procvar2procdef(p: tai; pvdef: tprocvardef);
    begin
      emit_tai_intern(p,pvdef,true);
    end;


  procedure tllvmtai_typedconstbuilder.maybe_begin_aggregate(def: tdef);
    var
      agg: tai_aggregatetypedconst;
      tck: ttypedconstkind;
    begin
      tck:=aggregate_kind(def);
      if tck<>tck_simple then
        begin
          if not assigned(faggregates) then
            faggregates:=tfplist.create;
          agg:=tai_aggregatetypedconst.create(tck,def);
          { nested aggregate -> add to parent }
          if faggregates.count>0 then
            tai_aggregatetypedconst(faggregates[faggregates.count-1]).addvalue(agg)
          { otherwise add to asmlist }
          else
            fasmlist.concat(agg);
          { new top level aggregate, future data will be added to it }
          faggregates.add(agg);
        end;
      inherited;
    end;


  procedure tllvmtai_typedconstbuilder.maybe_end_aggregate(def: tdef);
    begin
      if aggregate_kind(def)<>tck_simple then
        begin
          if not assigned(faggregates) or
             (faggregates.count=0) then
            internalerror(2014060101);
          tai_aggregatetypedconst(faggregates[faggregates.count-1]).finish;
          { already added to the asmlist if necessary }
          faggregates.count:=faggregates.count-1;
        end;
      inherited;
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
      update_queued_tai(getpointerdef(eledef),ai,ai,1);
    end;


  procedure tllvmtai_typedconstbuilder.queue_subscriptn(def: tabstractrecorddef; vs: tfieldvarsym);
    var
      getllvmfieldaddr,
      getpascalfieldaddr: taillvm;
      llvmfielddef: tdef;
    begin
      { update range checking info }
      inherited;
      llvmfielddef:=tabstractrecordsymtable(def.symtable).llvmst[vs.llvmfieldnr].def;
      { get the address of the llvm-struct field that corresponds to this
        Pascal field }
      getllvmfieldaddr:=taillvm.getelementptr_reg_tai_size_const(NR_NO,nil,s32inttype,vs.llvmfieldnr,true);
      { if it doesn't match the requested field exactly (variant record),
        fixup the result }
      getpascalfieldaddr:=getllvmfieldaddr;
      if (vs.offsetfromllvmfield<>0) or
         (llvmfielddef<>vs.vardef) then
        begin
          { offset of real field relative to llvm-struct field <> 0? }
          if vs.offsetfromllvmfield<>0 then
            begin
              { convert to a pointer to a 1-sized element }
              if llvmfielddef.size<>1 then
                begin
                  getpascalfieldaddr:=taillvm.op_reg_size_tai_size(la_bitcast,NR_NO,getpointerdef(llvmfielddef),getpascalfieldaddr,u8inttype);
                  { update the current fielddef of the expression }
                  llvmfielddef:=u8inttype;
                end;
              { add the offset }
              getpascalfieldaddr:=taillvm.getelementptr_reg_tai_size_const(NR_NO,getpascalfieldaddr,ptrsinttype,vs.offsetfromllvmfield,true);
            end;
          { bitcast the data at the final offset to the right type }
          if llvmfielddef<>vs.vardef then
            getpascalfieldaddr:=taillvm.op_reg_size_tai_size(la_bitcast,NR_NO,getpointerdef(llvmfielddef),getpascalfieldaddr,getpointerdef(vs.vardef));
        end;
      update_queued_tai(getpointerdef(vs.vardef),getpascalfieldaddr,getllvmfieldaddr,1);
    end;


  procedure tllvmtai_typedconstbuilder.queue_typeconvn(fromdef, todef: tdef);
    var
      ai: taillvm;
      tmpintdef: tdef;
      op,
      firstop,
      secondop: tllvmop;
    begin
      inherited;
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
            ai:=taillvm.op_reg_size_tai_size(secondop,NR_NO,tmpintdef,nil,todef);
            update_queued_tai(todef,ai,ai,2);
            todef:=tmpintdef;
            op:=firstop
          end;
      end;
      ai:=taillvm.op_reg_size_tai_size(op,NR_NO,fromdef,nil,todef);
      update_queued_tai(todef,ai,ai,2);
    end;


  procedure tllvmtai_typedconstbuilder.queue_emit_asmsym(sym: tasmsymbol; def: tdef);
    begin
      { we've already incorporated the offset via the inserted operations above,
        make sure it doesn't get emitted again as part of the tai_const for
        the tasmsymbol }
      fqueue_offset:=0;
      inherited;
    end;


begin
  ctai_typedconstbuilder:=tllvmtai_typedconstbuilder;
end.

