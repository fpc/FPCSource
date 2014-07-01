{
    Copyright (c) 2014 by Jonas Maebe, member of the Free Pascal development
    team

    This unit implements typed constant data elements at the assembler level

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
unit aasmcnst;

{$i fpcdefs.inc}

interface

uses
  cclasses,globtype,constexp,
  aasmbase,aasmdata,aasmtai,
  symtype,symdef,symsym;

type
   { Warning: never directly create a ttai_typedconstbuilder instance,
     instead create a cai_typedconstbuilder (this class can be overridden) }
   ttai_lowleveltypedconstbuilder = class abstract
    protected
     { temporary list in which all data is collected }
     fasmlist: tasmlist;
     { while queueing elements of a compound expression, this is the current
       offset in the top-level array/record }
     fqueue_offset: asizeint;

     { ensure that finalize_asmlist is called only once }
     fasmlist_finalized: boolean;

     { returns whether def must be handled as an aggregate on the current
       platform }
     function aggregate_kind(def: tdef): ttypedconstkind; virtual;
     { finalize the asmlist: add the necessary symbols etc }
     procedure finalize_asmlist(sym: tasmsymbol; def: tdef; section: TAsmSectiontype; const secname: TSymStr; alignment: shortint; lab: boolean); virtual;
    public
     { sym is the symbol for which this typed constant data is emitted. }
     constructor create; virtual;
     destructor destroy; override;

     { add a simple constant data element (p) to the typed constant.
       def is the type of the added value }
     procedure emit_tai(p: tai; def: tdef); virtual;
     { same as above, for a special case: when the def is a procvardef and we
       want to use it explicitly as a procvdef (i.e., not as a record with a
       code and data pointer in case of a complex procvardef) }
     procedure emit_tai_procvar2procdef(p: tai; pvdef: tprocvardef); virtual;
     { begin a potential aggregate type. Must be called for any type
       that consists of multiple tai constant data entries, or that
       represents an aggregate at the Pascal level (a record, a non-dynamic
       array, ... }
     procedure maybe_begin_aggregate(def: tdef); virtual;
     { end a potential aggregate type. Must be paired with every
       maybe_begin_aggregate }
     procedure maybe_end_aggregate(def: tdef); virtual;

     { The next group of routines are for constructing complex expressions.
       While parsing a typed constant these operators are encountered from
       outer to inner, so that is also the order in which they should be
       added to the queue. Only one queue can be active at a time. There is
       no default implementation. }
     { Init the queue. Gives an internalerror if a queu was already active }
     procedure queue_init(todef: tdef); virtual;
     { queue an array/string indexing operation (performs all range checking,
       so it doesn't have to be duplicated in all descendents). Returns the
       length of the elements (in bytes) and the index in the vector
       (0-based) }
     procedure queue_vecn(def: tdef; const index: tconstexprint); virtual;
     { queue a subscripting operation }
     procedure queue_subscriptn(def: tabstractrecorddef; vs: tfieldvarsym); virtual;
     { queue a type conversion operation }
     procedure queue_typeconvn(fromdef, todef: tdef); virtual;
     { queue an address taking operation }
     procedure queue_addrn(fromdef, todef: tdef); virtual;
     { (these default implementations don't do anything but indicate the
        queue has been flushed by resetting fqueueu_offset)
        finalise the queue (so a new one can be created) and flush the
        previously queued operations, applying them in reverse order on a...}
     { ... procdef }
     procedure queue_emit_proc(pd: tprocdef); virtual;
     { ... staticvarsym }
     procedure queue_emit_staticvar(vs: tstaticvarsym); virtual;
     { ... labelsym }
     procedure queue_emit_label(l: tlabelsym); virtual;
     { ... constsym }
     procedure queue_emit_const(cs: tconstsym); virtual;
     { ... asmsym }
     procedure queue_emit_asmsym(sym: tasmsymbol; def: tdef); virtual;

     { finalize the asmlist (if necessary) and return it.
       At the end, the generated data can be found in the asmlist. It will
       be freed when the builder is destroyed, so add its contents to
       another list first. This property should only be accessed once all
       data has been added. }
     function get_final_asmlist(sym: tasmsymbol; def: tdef; section: TAsmSectiontype; const secname: TSymStr; alignment: longint; lab: boolean): tasmlist;
   end;
   ttai_lowleveltypedconstbuilderclass = class of ttai_lowleveltypedconstbuilder;

   var
     ctai_typedconstbuilder: ttai_lowleveltypedconstbuilderclass;

implementation

   uses
     verbose,globals,
     symconst,defutil;

 {*****************************************************************************
                              ttai_lowleveltypedconstbuilder
 *****************************************************************************}

   function ttai_lowleveltypedconstbuilder.aggregate_kind(def: tdef): ttypedconstkind;
     begin
       if (def.typ in [recorddef,filedef,variantdef]) or
          is_object(def) or
          ((def.typ=procvardef) and
           not tprocvardef(def).is_addressonly) then
         result:=tck_record
       else if ((def.typ=arraydef) and
           not is_dynamic_array(def)) or
          ((def.typ=setdef) and
           not is_smallset(def)) or
          is_shortstring(def) then
         result:=tck_array
       else
         result:=tck_simple;
     end;


   procedure ttai_lowleveltypedconstbuilder.finalize_asmlist(sym: tasmsymbol; def: tdef; section: TAsmSectiontype; const secname: TSymStr; alignment: shortint; lab: boolean);
     var
       prelist: tasmlist;
     begin
       prelist:=tasmlist.create_without_marker;
       maybe_new_object_file(prelist);
       { only now add items based on the symbolname, because it may be
         modified by the "section" specifier in case of a typed constant }
       new_section(prelist,section,secname,const_align(alignment));
       if not lab then
         if sym.bind=AB_GLOBAL then
           prelist.concat(tai_symbol.Create_Global(sym,0))
         else
           prelist.concat(tai_symbol.Create(sym,0))
       else
         prelist.concat(tai_label.Create(tasmlabel(sym)));
       { insert the symbol information before the data }
       fasmlist.insertlist(prelist);
       { end of the symbol }
       fasmlist.concat(tai_symbol_end.Createname(sym.name));
       { free the temporary list }
       prelist.free;
     end;


   function ttai_lowleveltypedconstbuilder.get_final_asmlist(sym: tasmsymbol; def: tdef; section: TAsmSectiontype; const secname: TSymStr; alignment: longint; lab: boolean): tasmlist;
     begin
       if not fasmlist_finalized then
         begin
           finalize_asmlist(sym,def,section,secname,alignment,lab);
           fasmlist_finalized:=true;
         end;
       result:=fasmlist;
     end;


   constructor ttai_lowleveltypedconstbuilder.create;
     begin
       inherited create;
       fasmlist:=tasmlist.create_without_marker;
       { queue is empty }
       fqueue_offset:=low(fqueue_offset);
     end;


   destructor ttai_lowleveltypedconstbuilder.destroy;
     begin
       { the queue should have been flushed if it was used }
       if fqueue_offset<>low(fqueue_offset) then
         internalerror(2014062901);
       fasmlist.free;
       inherited destroy;
     end;


   procedure ttai_lowleveltypedconstbuilder.emit_tai(p: tai; def: tdef);
     begin
       { by default, we ignore the def info since we don't care about it at the
         the assembler level }
       fasmlist.concat(p);
     end;


   procedure ttai_lowleveltypedconstbuilder.emit_tai_procvar2procdef(p: tai; pvdef: tprocvardef);
     begin
       { nothing special by default, since we don't care about the type }
       emit_tai(p,pvdef);
     end;


   procedure ttai_lowleveltypedconstbuilder.maybe_begin_aggregate(def: tdef);
     begin
       { do nothing }
     end;


   procedure ttai_lowleveltypedconstbuilder.maybe_end_aggregate(def: tdef);
     begin
       { do nothing }
     end;


   procedure ttai_lowleveltypedconstbuilder.queue_init(todef: tdef);
     begin
       { nested call to init? }
       if fqueue_offset<>low(fqueue_offset) then
         internalerror(2014062101);
       fqueue_offset:=0;
     end;


   procedure ttai_lowleveltypedconstbuilder.queue_vecn(def: tdef; const index: tconstexprint);
     var
       elelen,
       vecbase: asizeint;
       v: tconstexprint;
     begin
       elelen:=1;
       vecbase:=0;
       case def.typ of
         stringdef :
           ;
         arraydef :
           begin
             if not is_packed_array(def) then
               begin
                 elelen:=tarraydef(def).elesize;
                 vecbase:=tarraydef(def).lowrange;
               end
             else
               Message(parser_e_packed_dynamic_open_array);
           end;
         else
           Message(parser_e_illegal_expression);
       end;
       { Prevent overflow }
       v:=index-vecbase;
       if (v<int64(low(fqueue_offset))) or (v>int64(high(fqueue_offset))) then
         message3(type_e_range_check_error_bounds,tostr(v),tostr(low(fqueue_offset)),tostr(high(fqueue_offset)));
       if high(fqueue_offset)-fqueue_offset div elelen>v then
         inc(fqueue_offset,elelen*v.svalue)
       else
         message3(type_e_range_check_error_bounds,tostr(index),tostr(vecbase),tostr(high(fqueue_offset)-fqueue_offset div elelen+vecbase))
     end;


   procedure ttai_lowleveltypedconstbuilder.queue_subscriptn(def: tabstractrecorddef; vs: tfieldvarsym);
     begin
       inc(fqueue_offset,vs.fieldoffset);
     end;


   procedure ttai_lowleveltypedconstbuilder.queue_typeconvn(fromdef, todef: tdef);
     begin
       { do nothing }
     end;

   procedure ttai_lowleveltypedconstbuilder.queue_addrn(fromdef, todef: tdef);
     begin
       { do nothing }
     end;


   procedure ttai_lowleveltypedconstbuilder.queue_emit_proc(pd: tprocdef);
     begin
       emit_tai(Tai_const.Createname(pd.mangledname,fqueue_offset),pd);
       fqueue_offset:=low(fqueue_offset);
     end;


   procedure ttai_lowleveltypedconstbuilder.queue_emit_staticvar(vs: tstaticvarsym);
     begin
       { getpointerdef because we are emitting a pointer to the staticvarsym
         data, not the data itself }
       emit_tai(Tai_const.Createname(vs.mangledname,fqueue_offset),getpointerdef(vs.vardef));
       fqueue_offset:=low(fqueue_offset);
     end;


   procedure ttai_lowleveltypedconstbuilder.queue_emit_label(l: tlabelsym);
     begin
       emit_tai(Tai_const.Createname(l.mangledname,fqueue_offset),voidcodepointertype);
       fqueue_offset:=low(fqueue_offset);
     end;


   procedure ttai_lowleveltypedconstbuilder.queue_emit_const(cs: tconstsym);
     begin
       if cs.consttyp<>constresourcestring then
         internalerror(2014062102);
       if fqueue_offset<>0 then
         internalerror(2014062103);
       { warning: update if/when the type of resource strings changes }
       emit_tai(Tai_const.Createname(make_mangledname('RESSTR',cs.owner,cs.name),AT_DATA,sizeof(pint)),cansistringtype);
       fqueue_offset:=low(fqueue_offset);
     end;


   procedure ttai_lowleveltypedconstbuilder.queue_emit_asmsym(sym: tasmsymbol; def: tdef);
     begin
       { getpointerdef, because "sym" represents the address of whatever the
         data is }
       def:=getpointerdef(def);
       emit_tai(Tai_const.Create_sym(sym),def);
       fqueue_offset:=low(fqueue_offset);
     end;


begin
  ctai_typedconstbuilder:=ttai_lowleveltypedconstbuilder;
end.

