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
  symconst,symtype,symdef,symsym;

type
   { typed const: integer/floating point/string/pointer/... const along with
     tdef info }
   ttypedconstkind = (tck_simple, tck_array, tck_record);

   { the type of the element and its def }
   tai_abstracttypedconst = class abstract (tai)
    private
     procedure setdef(def: tdef);
    protected
     fadetyp: ttypedconstkind;
     { the def of this element }
     fdef: tdef;
    public
     constructor create(_adetyp: ttypedconstkind; _def: tdef);
     property adetyp: ttypedconstkind read fadetyp;
     property def: tdef read fdef write setdef;
   end;

   { a simple data element; the value is stored as a tai }
   tai_simpletypedconst = class(tai_abstracttypedconst)
    protected
     fval: tai;
    public
     constructor create(_adetyp: ttypedconstkind; _def: tdef; _val: tai);
     property val: tai read fval;
   end;


   { an aggregate data element (record or array). Values are stored as an
     array of tsimpledataelement. }
   tai_aggregatetypedconst = class(tai_abstracttypedconst)
    public type
     { iterator to walk over all individual items in the aggregate }
     tadeenumerator = class(tobject)
      private
       fvalues: tfplist;
       fvaluespos: longint;
       function getcurrent: tai_abstracttypedconst;
      public
       constructor create(data: tai_aggregatetypedconst);
       function movenext: boolean;
       procedure reset;
       property current: tai_abstracttypedconst read getcurrent;
     end;

    protected
     fvalues: tfplist;
     fisstring: boolean;

     { converts the existing data to a single tai_string }
     procedure convert_to_string;
     procedure add_to_string(strtai: tai_string; othertai: tai);
    public
     constructor create(_adetyp: ttypedconstkind; _fdef: tdef);
     function getenumerator: tadeenumerator;
     procedure addvalue(val: tai_abstracttypedconst);
     function valuecount: longint;
     procedure insertvaluebeforepos(val: tai_abstracttypedconst; pos: longint);
     procedure finish;
     destructor destroy; override;
   end;


    tasmlabofs = record
      lab: tasmlabel;
      ofs: asizeint;
    end;

   { flags for the finalisation of the typed const builder asmlist }
   ttcasmlistoption = (
     { the tasmsymbol is a tasmlabel }
     tcalo_is_lab,
     { start a new section (e.g., because we don't know the current section
       type) }
     tcalo_new_section,
     { this symbol is the start of a block of data that should be
       dead-stripable/smartlinkable; may imply starting a new section, but
       not necessarily (depends on what the platform requirements are) }
     tcalo_make_dead_strippable
   );
   ttcasmlistoptions = set of ttcasmlistoption;


   { information about aggregates we are parsing }
   taggregateinformation = class
    private
     function getcuroffset: asizeint;
     function getfieldoffset(l: longint): asizeint;
    protected
     { type of the aggregate }
     fdef: tdef;
     { type of the aggregate }
     ftyp: ttypedconstkind;
     { symtable entry of the previously emitted field in case of a
       record/object (nil if none emitted yet), used to insert alignment bytes
       if necessary for variant records and objects }
     fcurfield,
     { field corresponding to the data that will be emitted next in case of a
       record/object (nil if not set), used to handle variant records and
       objects }
     fnextfield: tfieldvarsym;
     { similar as the fcurfield/fnextfield above, but instead of fieldvarsyms
       these are indices in the symlist of a recorddef that correspond to
       fieldvarsyms. These are used only for non-variant records, simply
       traversing the fields in order. We could use the above method here as
       well, but to find the next field we'd always have to use
       symlist.indexof(fcurfield), which would be quite slow. These have -1 as
       value if they're not set }
     fcurindex,
     fnextindex: longint;
     { anonymous record that is being built as we add constant data }
     fanonrecord: boolean;

     property curindex: longint read fcurindex write fcurindex;
     property nextindex: longint read fnextindex write fnextindex;
    public
     constructor create(_def: tdef; _typ: ttypedconstkind); virtual;
     { calculated padding bytes for alignment if needed, and add the def of the
       next field in case we are constructing an anonymous record }
     function prepare_next_field(nextfielddef: tdef): asizeint;

     property def: tdef read fdef;
     property typ: ttypedconstkind read ftyp;
     property curfield: tfieldvarsym read fcurfield write fcurfield;
     property nextfield: tfieldvarsym read fnextfield write fnextfield;
     property fieldoffset[l: longint]: asizeint read getfieldoffset;
     property curoffset: asizeint read getcuroffset;
     property anonrecord: boolean read fanonrecord write fanonrecord;
   end;
   taggregateinformationclass = class of taggregateinformation;

   { Warning: never directly create a ttai_typedconstbuilder instance,
     instead create a cai_typedconstbuilder (this class can be overridden) }
   ttai_typedconstbuilder = class abstract
    { class type to use when creating new aggregate information instances }
    protected class var
     caggregateinformation: taggregateinformationclass;
    private
     function getcurragginfo: taggregateinformation;
     procedure set_next_field(AValue: tfieldvarsym);
    protected
     { temporary list in which all data is collected }
     fasmlist: tasmlist;
     { options for the final asmlist }
     foptions: ttcasmlistoptions;

     { while queueing elements of a compound expression, this is the current
       offset in the top-level array/record }
     fqueue_offset: asizeint;

     { array of caggregateinformation instances }
     faggregateinformation: tfpobjectlist;

    { Support for generating data that is only referenced from the typed
      constant data that we are currently generated. Such data can all be put
      in the same dead-strippable unit, as it's either all included or none of
      it is included. This data can be spread over multiple kinds of sections
      though (e.g. rodata and rodata_no_rel), so per section keep track whether
      we already started a dead-strippable unit and if so, what the section
      name was (so that on platforms that perform the dead stripping based on
      sections, we put all data for one typed constant into a single section
      with the same name) }
    protected type
     tinternal_data_section_info = record
       secname: TSymStr;
       sectype: TAsmSectiontype;
     end;
    protected var
     { all internally generated data must be stored in the same list, as it must
       be consecutive (if it's spread over multiple lists, we don't know in
       which order they'll be concatenated) -> keep track of this list }
     finternal_data_asmlist: tasmlist;
     { kind of the last section we started in the finternal_data_asmlist, to
       avoid creating unnecessary section statements }
     finternal_data_current_section: TAsmSectiontype;
     { info about in which kinds of sections we have already emitted internal
       data, and what their names were }
     finternal_data_section_info: array of tinternal_data_section_info;

     { ensure that finalize_asmlist is called only once }
     fasmlist_finalized: boolean;

     { returns whether def must be handled as an aggregate on the current
       platform }
     function aggregate_kind(def: tdef): ttypedconstkind; virtual;
     { finalize the asmlist: add the necessary symbols etc }
     procedure finalize_asmlist(sym: tasmsymbol; def: tdef; section: TAsmSectiontype; const secname: TSymStr; alignment: shortint; const options: ttcasmlistoptions); virtual;

     { called by the public emit_tai() routines to actually add the typed
       constant data; the public ones also take care of adding extra padding
       bytes etc (by calling this one) }
     procedure do_emit_tai(p: tai; def: tdef); virtual;

     { calls prepare_next_field() and adds the padding bytes in the current
       location }
     procedure pad_next_field(nextfielddef: tdef);

     { returns the index in finternal_data_section_info of the info for the
       section of type typ. Returns -1 if there is no such info yet }
     function get_internal_data_section_index(typ: TAsmSectiontype): longint;

     { get a start label for an internal data section (at the start of a
       potentially dead-strippable part) }
     function get_internal_data_section_start_label: tasmlabel; virtual;
     { get a label in the middle of an internal data section (no dead
       stripping) }
     function get_internal_data_section_internal_label: tasmlabel; virtual;

     { easy access to the top level aggregate information instance }
     property curagginfo: taggregateinformation read getcurragginfo;
    public
     constructor create(const options: ttcasmlistoptions); virtual;
     destructor destroy; override;

    public
     { returns a builder for generating data that is only referrenced by the
       typed constant date we are currently generating (e.g. string data for a
       pchar constant). Also returns the label that will be placed at the start
       of that data. list is the tasmlist to which the data will be added.
       secname can be empty to use a default }
     procedure start_internal_data_builder(list: tasmlist; sectype: TAsmSectiontype; const secname: TSymStr; out tcb: ttai_typedconstbuilder; out l: tasmlabel);
     { finish a previously started internal data builder, including
       concatenating all generated data to the provided list and freeing the
       builder }
     procedure finish_internal_data_builder(var tcb: ttai_typedconstbuilder; l: tasmlabel; def: tdef; alignment: longint);

     { add a simple constant data element (p) to the typed constant.
       def is the type of the added value }
     procedure emit_tai(p: tai; def: tdef); virtual;
     { same as above, for a special case: when the def is a procvardef and we
       want to use it explicitly as a procdef (i.e., not as a record with a
       code and data pointer in case of a complex procvardef) }
     procedure emit_tai_procvar2procdef(p: tai; pvdef: tprocvardef); virtual;

    protected
     function emit_string_const_common(stringtype: tstringtype; len: asizeint; encoding: tstringencoding; var startlab: tasmlabel):tasmlabofs;
     procedure begin_aggregate_internal(def: tdef; anonymous: boolean); virtual;
     procedure end_aggregate_internal(def: tdef; anonymous: boolean); virtual;
     { when building an anonymous record, we cannot immediately insert the
       alignment before it in case it's nested, since we only know the required
       alignment once all fields have been inserted -> mark the location before
       the anonymous record, and insert the alignment once it's finished }
     procedure mark_anon_aggregate_alignment; virtual; abstract;
     procedure insert_marked_aggregate_alignment(def: tdef); virtual; abstract;
    public
     class function get_dynstring_rec_name(typ: tstringtype; winlike: boolean; len: asizeint): string;
     { the datalist parameter specifies where the data for the string constant
       will be emitted (via an internal data builder) }
     function emit_ansistring_const(datalist: TAsmList; data: pchar; len: asizeint; encoding: tstringencoding): tasmlabofs;
     function emit_unicodestring_const(datalist: TAsmList; data: pointer; encoding: tstringencoding; winlike: boolean):tasmlabofs;
     { emits a tasmlabofs as returned by emit_*string_const }
     procedure emit_string_offset(const ll: tasmlabofs; const strlength: longint; const st: tstringtype; const winlikewidestring: boolean; const charptrdef: tdef);virtual;

     { emit a shortstring constant, and return its def }
     function emit_shortstring_const(const str: shortstring): tdef;
     { emit a guid constant }
     procedure emit_guid_const(const guid: tguid);
     { emit a procdef constant }
     procedure emit_procdef_const(pd: tprocdef);
     { emit an ordinal constant }
     procedure emit_ord_const(value: int64; def: tdef);

     { begin a potential aggregate type. Must be called for any type
       that consists of multiple tai constant data entries, or that
       represents an aggregate at the Pascal level (a record, a non-dynamic
       array, ... }
     procedure maybe_begin_aggregate(def: tdef);
     { end a potential aggregate type. Must be paired with every
       maybe_begin_aggregate }
     procedure maybe_end_aggregate(def: tdef);
     { similar as above, but in case
        a) it's definitely a record
        b) the def of the record should be automatically constructed based on
           the types of the emitted fields
     }
     function begin_anonymous_record(const optionalname: string; packrecords, recordalignmin, maxcrecordalign: shortint): trecorddef; virtual;
     function end_anonymous_record: trecorddef; virtual;

     { The next group of routines are for constructing complex expressions.
       While parsing a typed constant these operators are encountered from
       outer to inner, so that is also the order in which they should be
       added to the queue. Only one queue can be active at a time. }
     { Init the queue. Gives an internalerror if a queue was already active }
     procedure queue_init(todef: tdef); virtual;
     { queue an array/string indexing operation (performs all range checking,
       so it doesn't have to be duplicated in all descendents). }
     procedure queue_vecn(def: tdef; const index: tconstexprint); virtual;
     { queue a subscripting operation }
     procedure queue_subscriptn(def: tabstractrecorddef; vs: tfieldvarsym); virtual;
     { queue a type conversion operation }
     procedure queue_typeconvn(fromdef, todef: tdef); virtual;
     { queue an address taking operation }
     procedure queue_addrn(fromdef, todef: tdef); virtual;
     { finalise the queue (so a new one can be created) and flush the
        previously queued operations, applying them in reverse order on a...}
     { ... procdef }
     procedure queue_emit_proc(pd: tprocdef); virtual;
     { ... staticvarsym }
     procedure queue_emit_staticvar(vs: tstaticvarsym); virtual;
     { ... labelsym }
     procedure queue_emit_label(l: tlabelsym); virtual;
     { ... constsym }
     procedure queue_emit_const(cs: tconstsym); virtual;
     { ... asmsym/asmlabel }
     procedure queue_emit_asmsym(sym: tasmsymbol; def: tdef); virtual;
     { ... an ordinal constant }
     procedure queue_emit_ordconst(value: int64; def: tdef); virtual;

     { finalize the internal asmlist (if necessary) and return it.
       This asmlist will be freed when the builder is destroyed, so add its
       contents to another list first. This property should only be accessed
       once all data has been added. }
     function get_final_asmlist(sym: tasmsymbol; def: tdef; section: TAsmSectiontype; const secname: TSymStr; alignment: longint): tasmlist;

     { returns the offset of the string data relative to ansi/unicode/widestring
       constant labels. On most platforms, this is 0 (with the header at a
       negative offset), but on some platforms such negative offsets are not
       supported this is equal to the header size }
     class function get_string_symofs(typ: tstringtype; winlikewidestring: boolean): pint; virtual;

     { set the fieldvarsym whose data we will emit next; needed
       in case of variant records, so we know which part of the variant gets
       initialised. Also in case of objects, because the fieldvarsyms are spread
       over the symtables of the entire inheritance tree }
     property next_field: tfieldvarsym write set_next_field;
    protected
     { this one always return the actual offset, called by the above (and
       overridden versions) }
     class function get_string_header_size(typ: tstringtype; winlikewidestring: boolean): pint;
   end;
   ttai_typedconstbuilderclass = class of ttai_typedconstbuilder;

   tlowlevelaggregateinformation = class(taggregateinformation)
    protected
     fanonrecmarker: tai;
    public
     property anonrecmarker: tai read fanonrecmarker write fanonrecmarker;
   end;

   ttai_lowleveltypedconstbuilder = class(ttai_typedconstbuilder)
    protected
     procedure mark_anon_aggregate_alignment; override;
     procedure insert_marked_aggregate_alignment(def: tdef); override;
    public
     { set the default value for caggregateinformation (= tlowlevelaggregateinformation) }
     class constructor classcreate;
   end;

   var
     ctai_typedconstbuilder: ttai_typedconstbuilderclass;

implementation

   uses
     verbose,globals,systems,widestr,
     symbase,symtable,defutil;

{****************************************************************************
                       taggregateinformation
 ****************************************************************************}

    function taggregateinformation.getcuroffset: asizeint;
      var
        field: tfieldvarsym;
      begin
        if assigned(curfield) then
          result:=curfield.fieldoffset+curfield.vardef.size
        else if curindex<>-1 then
          begin
            field:=tfieldvarsym(tabstractrecorddef(def).symtable.symlist[curindex]);
            result:=field.fieldoffset+field.vardef.size
          end
        else
          result:=0
      end;


    function taggregateinformation.getfieldoffset(l: longint): asizeint;
      var
        field: tfieldvarsym;
      begin
        field:=tfieldvarsym(tabstractrecorddef(def).symtable.symlist[l]);
        result:=field.fieldoffset;
      end;


    constructor taggregateinformation.create(_def: tdef; _typ: ttypedconstkind);
      begin
        fdef:=_def;
        ftyp:=_typ;
        fcurindex:=-1;
        fnextindex:=-1;
      end;


    function taggregateinformation.prepare_next_field(nextfielddef: tdef): asizeint;
      var
        currentoffset,nextoffset: asizeint;
        i: longint;
      begin
        { get the next field and its offset, and make that next field the current
          one }
        if assigned(nextfield) then
          begin
            nextoffset:=nextfield.fieldoffset;
            currentoffset:=curoffset;
            curfield:=nextfield;
          end
        else
          begin
            { must set nextfield for unions and objects, as we cannot
              automatically detect the "next" field in that case }
            if ((def.typ=recorddef) and
                trecorddef(def).isunion) or
               is_object(def) then
              internalerror(2014091202);
            { if we are constructing this record as data gets emitted, add a field
              for this data }
            if anonrecord then
              trecorddef(def).add_field_by_def(nextfielddef);
            { find next field }
            i:=curindex;
            repeat
              inc(i);
            until (tsym(tabstractrecorddef(def).symtable.symlist[i]).typ=fieldvarsym) and
              not(sp_static in tsym(tabstractrecorddef(def).symtable.symlist[i]).symoptions);
            nextoffset:=fieldoffset[i];
            currentoffset:=curoffset;
            curindex:=i;
          end;
        { need padding? }
        result:=nextoffset-currentoffset;
      end;


{****************************************************************************
                            tai_abstracttypedconst
 ****************************************************************************}

   procedure tai_abstracttypedconst.setdef(def: tdef);
     begin
       { should not be changed, rewrite the calling code if this happens }
       if assigned(fdef) then
         Internalerror(2014080203);
       fdef:=def;
     end;

   constructor tai_abstracttypedconst.create(_adetyp: ttypedconstkind; _def: tdef);
     begin
       inherited create;
       typ:=ait_typedconst;
       fadetyp:=_adetyp;
       fdef:=_def;
     end;


{****************************************************************************
                                tai_simpletypedconst
 ****************************************************************************}

   constructor tai_simpletypedconst.create(_adetyp: ttypedconstkind; _def: tdef; _val: tai);
     begin
       inherited create(_adetyp,_def);
       fval:=_val;
     end;


{****************************************************************************
              tai_aggregatetypedconst.tadeenumerator
 ****************************************************************************}

   constructor tai_aggregatetypedconst.tadeenumerator.create(data: tai_aggregatetypedconst);
     begin
       fvalues:=data.fvalues;
       fvaluespos:=-1;
     end;


   function tai_aggregatetypedconst.tadeenumerator.getcurrent: tai_abstracttypedconst;
     begin
       result:=tai_abstracttypedconst(fvalues[fvaluespos]);
     end;


   function tai_aggregatetypedconst.tadeenumerator.movenext: boolean;
     begin
       if fvaluespos<pred(fvalues.count) then
         begin
           inc(fvaluespos);
           result:=true
         end
       else
         result:=false;
     end;


   procedure tai_aggregatetypedconst.tadeenumerator.reset;
     begin
       fvaluespos:=0
     end;


{****************************************************************************
                            tai_aggregatetypedconst
 ****************************************************************************}

   procedure tai_aggregatetypedconst.convert_to_string;
     var
       ai: tai_abstracttypedconst;
       newstr: tai_string;
     begin
       newstr:=tai_string.Create('');
       for ai in self do
          begin
            if ai.adetyp<>tck_simple then
              internalerror(2014070103);
            add_to_string(newstr,tai_simpletypedconst(ai).val);
            ai.free;
          end;
       fvalues.count:=0;
       { the "nil" def will be replaced with an array def of the appropriate
         size once we're finished adding data, so we don't create intermediate
         arraydefs all the time }
       fvalues.add(tai_simpletypedconst.create(tck_simple,nil,newstr));
     end;

   procedure tai_aggregatetypedconst.add_to_string(strtai: tai_string; othertai: tai);
     begin
       case othertai.typ of
         ait_string:
           begin
             strtai.str:=reallocmem(strtai.str,strtai.len+tai_string(othertai).len+1);
             { also copy null terminator }
             move(tai_string(othertai).str[0],strtai.str[strtai.len],tai_string(othertai).len+1);
             { the null terminator is not part of the string data }
             strtai.len:=strtai.len+tai_string(othertai).len;
           end;
         ait_const:
           begin
             if tai_const(othertai).size<>1 then
               internalerror(2014070101);
             strtai.str:=reallocmem(strtai.str,strtai.len+1);
             strtai.str[strtai.len]:=ansichar(tai_const(othertai).value);
             strtai.str[strtai.len+1]:=#0;
             inc(strtai.len);
           end;
         else
           internalerror(2014070102);
       end;
     end;


   constructor tai_aggregatetypedconst.create(_adetyp: ttypedconstkind; _fdef: tdef);
     begin
       inherited;
       fisstring:=false;
       fvalues:=tfplist.create;
     end;


   function tai_aggregatetypedconst.getenumerator: tadeenumerator;
     begin
       result:=tadeenumerator.create(self);
     end;


   procedure tai_aggregatetypedconst.addvalue(val: tai_abstracttypedconst);
     begin
       { merge string constants and ordinal constants added in an array of
         char, to unify the length and the string data }
       if fisstring or
          ((val.adetyp=tck_simple) and
           (tai_simpletypedconst(val).val.typ=ait_string)) then
         begin
           if not fisstring and
              (fvalues.count>0) then
             convert_to_string;
           fisstring:=true;
           case fvalues.count of
             0: fvalues.add(val);
             1:
               begin
                 add_to_string(tai_string(tai_simpletypedconst(fvalues[0]).val),tai_simpletypedconst(val).val);
                 val.free
               end
             else
               internalerror(2014070104);
           end;
         end
       else
         fvalues.add(val);
     end;


   function tai_aggregatetypedconst.valuecount: longint;
     begin
       result:=fvalues.count;
     end;


   procedure tai_aggregatetypedconst.insertvaluebeforepos(val: tai_abstracttypedconst; pos: longint);
     begin
       fvalues.insert(pos,val);
     end;


   procedure tai_aggregatetypedconst.finish;
     begin
       if fisstring then
         begin
           { set the def: an array of char with the same length as the string
             data }
           if fvalues.count<>1 then
             internalerror(2014070105);
           tai_simpletypedconst(fvalues[0]).fdef:=
             getarraydef(cansichartype,
               tai_string(tai_simpletypedconst(fvalues[0]).val).len);
         end;
     end;


   destructor tai_aggregatetypedconst.destroy;
     begin
       fvalues.free;
       inherited destroy;
     end;


 {*****************************************************************************
                              ttai_typedconstbuilder
 *****************************************************************************}

   function ttai_typedconstbuilder.getcurragginfo: taggregateinformation;
     begin
       if assigned(faggregateinformation) and
          (faggregateinformation.count>0) then
         result:=taggregateinformation(faggregateinformation[faggregateinformation.count-1])
       else
         result:=nil;
     end;


   procedure ttai_typedconstbuilder.set_next_field(AValue: tfieldvarsym);
     var
       info: taggregateinformation;
     begin
       info:=curagginfo;
       if not assigned(info) then
         internalerror(2014091206);
       info.nextfield:=AValue;
     end;


   procedure ttai_typedconstbuilder.pad_next_field(nextfielddef: tdef);
     var
       fillbytes: asizeint;
     begin
       fillbytes:=curagginfo.prepare_next_field(nextfielddef);
       while fillbytes>0 do
         begin
           do_emit_tai(tai_const.create_8bit(0),u8inttype);
           dec(fillbytes);
         end;
     end;


   function ttai_typedconstbuilder.get_internal_data_section_index(typ: TAsmSectiontype): longint;
     begin
       { avoid wrong warning by -Oodfa }
       result:=-1;
       for result:=low(finternal_data_section_info) to high(finternal_data_section_info) do
         if finternal_data_section_info[result].sectype=typ then
           exit;
       result:=-1;
     end;


   function ttai_typedconstbuilder.get_internal_data_section_start_label: tasmlabel;
     begin
       { on Darwin, dead code/data stripping happens based on non-temporary
         labels (any label that doesn't start with "L" -- it doesn't have
         to be global) }
       if target_info.system in systems_darwin then
         current_asmdata.getstaticdatalabel(result)
       else if create_smartlink_library then
         current_asmdata.getglobaldatalabel(result)
       else
         current_asmdata.getlocaldatalabel(result);
     end;


   function ttai_typedconstbuilder.get_internal_data_section_internal_label: tasmlabel;
     begin
       if create_smartlink_library then
         { all labels need to be global in case they're in another object }
         current_asmdata.getglobaldatalabel(result)
       else
         { no special requirement for the label -> just get a local one }
         current_asmdata.getlocaldatalabel(result);
     end;


   function ttai_typedconstbuilder.aggregate_kind(def: tdef): ttypedconstkind;
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


   procedure ttai_typedconstbuilder.finalize_asmlist(sym: tasmsymbol; def: tdef; section: TAsmSectiontype; const secname: TSymStr; alignment: shortint; const options: ttcasmlistoptions);
     var
       prelist: tasmlist;
     begin
       prelist:=tasmlist.create;
       { only now add items based on the symbolname, because it may be
         modified by the "section" specifier in case of a typed constant }
       if tcalo_make_dead_strippable in options then
         begin
           maybe_new_object_file(prelist);
           { we always need a new section here, since if we started a new
             object file then we have to say what the section is, and otherwise
             we need a new section because that's how the dead stripping works
             (except on Darwin, but that will be addressed in a future commit) }
           new_section(prelist,section,secname,const_align(alignment));
         end
       else if tcalo_new_section in options then
         new_section(prelist,section,secname,const_align(alignment))
       else
         prelist.concat(cai_align.Create(const_align(alignment)));
       if not(tcalo_is_lab in options) then
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


   procedure ttai_typedconstbuilder.do_emit_tai(p: tai; def: tdef);
     begin
       { by default we don't care about the type }
       fasmlist.concat(p);
     end;


   function ttai_typedconstbuilder.get_final_asmlist(sym: tasmsymbol; def: tdef; section: TAsmSectiontype; const secname: TSymStr; alignment: longint): tasmlist;
     begin
       if not fasmlist_finalized then
         begin
           finalize_asmlist(sym,def,section,secname,alignment,foptions);
           fasmlist_finalized:=true;
         end;
       result:=fasmlist;
     end;


   class function ttai_typedconstbuilder.get_string_symofs(typ: tstringtype; winlikewidestring: boolean): pint;
     begin
       { darwin's linker does not support negative offsets }
       if not(target_info.system in systems_darwin) then
         result:=0
       else
         result:=get_string_header_size(typ,winlikewidestring);
     end;


   class function ttai_typedconstbuilder.get_string_header_size(typ: tstringtype; winlikewidestring: boolean): pint;
     const
       ansistring_header_size =
         { encoding }
         2 +
         { elesize }
         2 +
{$ifdef cpu64bitaddr}
         { alignment }
         4 +
{$endif cpu64bitaddr}
         { reference count }
         sizeof(pint) +
         { length }
         sizeof(pint);
       unicodestring_header_size = ansistring_header_size;
     begin
       case typ of
         st_ansistring:
           result:=ansistring_header_size;
         st_unicodestring:
           result:=unicodestring_header_size;
         st_widestring:
           if winlikewidestring then
             result:=0
           else
             result:=unicodestring_header_size;
         else
           result:=0;
       end;
     end;


   constructor ttai_typedconstbuilder.create(const options: ttcasmlistoptions);
     begin
       inherited create;
       fasmlist:=tasmlist.create;
       foptions:=options;
       { queue is empty }
       fqueue_offset:=low(fqueue_offset);
       finternal_data_current_section:=sec_none;
     end;


   destructor ttai_typedconstbuilder.destroy;
     begin
       { the queue should have been flushed if it was used }
       if fqueue_offset<>low(fqueue_offset) then
         internalerror(2014062901);
       faggregateinformation.free;
       fasmlist.free;
       inherited destroy;
     end;


   procedure ttai_typedconstbuilder.start_internal_data_builder(list: tasmlist; sectype: TAsmSectiontype; const secname: TSymStr; out tcb: ttai_typedconstbuilder; out l: tasmlabel);
     var
       options: ttcasmlistoptions;
       foundsec: longint;
     begin
       options:=[tcalo_is_lab];
       { Add a section header if the previous one was different. We'll use the
         same section name in case multiple items are added to the same kind of
         section (rodata, rodata_no_rel, ...), so that everything will still
         end up in the same section even if there are multiple section headers }
       if finternal_data_current_section<>sectype then
         include(options,tcalo_new_section);
       finternal_data_current_section:=sectype;
       l:=nil;
       { did we already create a section of this type for the internal data of
         this builder? }
       foundsec:=get_internal_data_section_index(sectype);
       if foundsec=-1 then
         begin
           { we only need to start a dead-strippable section of data at the
             start of the first subsection of this kind for this block.

             exception: if dead stripping happens based on objects/libraries,
             then we only have to create a new object file for the first
             internal data section of any kind (all the rest will simply be put
             in the same object file) }
           if create_smartlink then
             begin
               if not create_smartlink_library or
                  (length(finternal_data_section_info)=0) then
                 include(options,tcalo_make_dead_strippable);
               { on Darwin, dead code/data stripping happens based on non-
                 temporary labels (any label that doesn't start with "L" -- it
                 doesn't have to be global) -> add a non-temporary lobel at the
                 start of every kind of subsection created in this builder }
               if target_info.system in systems_darwin then
                 l:=get_internal_data_section_start_label;
             end;
           foundsec:=length(finternal_data_section_info);
           setlength(finternal_data_section_info,foundsec+1);
           finternal_data_section_info[foundsec].sectype:=sectype;
         end;
       if not assigned(finternal_data_asmlist) and
          (cs_create_smart in current_settings.moduleswitches) then
         begin
           l:=get_internal_data_section_start_label;
           { the internal data list should only be assigned by this routine,
             the first time that an internal data block is started }
           if not assigned(list) or
              assigned(finternal_data_asmlist) then
             internalerror(2015032101);
           finternal_data_asmlist:=list;
         end
       { all internal data for this tcb must go to the same list (otherwise all
         data we want to add to the dead-strippable block is not guaranteed to
         be sequential and e.g. in the same object file in case of library-based
         dead stripping) }
       else if (assigned(finternal_data_asmlist) and
           (list<>finternal_data_asmlist)) or
           not assigned(list) then
         internalerror(2015032101);
       finternal_data_asmlist:=list;
       if not assigned(l) then
         l:=get_internal_data_section_internal_label;
       { first section of this kind -> set name }
       if finternal_data_section_info[foundsec].secname='' then
         if secname='' then
           finternal_data_section_info[foundsec].secname:=l.Name
         else
           finternal_data_section_info[foundsec].secname:=secname
       { if the name is specified multiple times, it must match }
       else if (secname<>'') and
               (finternal_data_section_info[foundsec].secname<>secname) then
         internalerror(2015032401);
       tcb:=ttai_typedconstbuilderclass(classtype).create(options);
     end;


   procedure ttai_typedconstbuilder.finish_internal_data_builder(var tcb: ttai_typedconstbuilder; l: tasmlabel; def: tdef; alignment: longint);
     begin
       finternal_data_asmlist.concatList(tcb.get_final_asmlist(l,def,
         finternal_data_current_section,
         finternal_data_section_info[get_internal_data_section_index(finternal_data_current_section)].secname,
         alignment));
       tcb.free;
       tcb:=nil;
     end;


   procedure ttai_typedconstbuilder.emit_tai(p: tai; def: tdef);
     var
       kind: ttypedconstkind;
       info: taggregateinformation;
     begin
       { these elements can be aggregates themselves, e.g. a shortstring can
         be emitted as a series of bytes and char arrays }
       kind:=aggregate_kind(def);
       info:=curagginfo;
       if (kind<>tck_simple) and
          (not assigned(info) or
           (info.typ<>kind)) then
         internalerror(2014091001);
       { if we're emitting a record, handle the padding bytes, and in case of
         an anonymous record also add the next field }
       if assigned(info) then
         begin
           if ((info.def.typ=recorddef) or
               is_object(info.def)) and
              { may add support for these later }
              not is_packed_record_or_object(info.def) then
             pad_next_field(def);
         end;
       { emit the data }
       do_emit_tai(p,def);
     end;


   procedure ttai_typedconstbuilder.emit_tai_procvar2procdef(p: tai; pvdef: tprocvardef);
     begin
       { nothing special by default, since we don't care about the type }
       emit_tai(p,pvdef);
     end;


   function ttai_typedconstbuilder.emit_string_const_common(stringtype: tstringtype; len: asizeint; encoding: tstringencoding; var startlab: tasmlabel): tasmlabofs;
     var
       string_symofs: asizeint;
       charptrdef: tdef;
       elesize: word;
     begin
       result.lab:=startlab;
       result.ofs:=0;
       { pack the data, so that we don't add unnecessary null bytes after the
         constant string }
       begin_anonymous_record('$'+get_dynstring_rec_name(stringtype,false,len),1,1,1);
       string_symofs:=get_string_symofs(stringtype,false);
       { encoding }
       emit_tai(tai_const.create_16bit(encoding),u16inttype);
       inc(result.ofs,2);
       { element size }
       case stringtype of
         st_ansistring:
           begin
             elesize:=1;
             charptrdef:=charpointertype;
           end;
         st_unicodestring:
           begin
             elesize:=2;
             charptrdef:=widecharpointertype;
           end
         else
           internalerror(2014080401);
       end;
       emit_tai(tai_const.create_16bit(elesize),u16inttype);
       inc(result.ofs,2);
{$ifdef cpu64bitaddr}
       { dummy for alignment }
       emit_tai(tai_const.create_32bit(0),u32inttype);
       inc(result.ofs,4);
{$endif cpu64bitaddr}
       emit_tai(tai_const.create_pint(-1),ptrsinttype);
       inc(result.ofs,sizeof(pint));
       emit_tai(tai_const.create_pint(len),ptrsinttype);
       inc(result.ofs,sizeof(pint));
       if string_symofs=0 then
         begin
           { results in slightly more efficient code }
           emit_tai(tai_label.create(result.lab),charptrdef);
           result.ofs:=0;
           { create new label of the same kind (including whether or not the
             name starts with target_asm.labelprefix in case it's AB_LOCAL,
             so we keep the difference depending on whether the original was
             allocated via getstatic/getlocal/getglobal datalabel) }
           startlab:=tasmlabel.create(current_asmdata.AsmSymbolDict,startlab.name+'$strlab',startlab.bind,startlab.typ);
         end;
       { sanity check }
       if result.ofs<>string_symofs then
         internalerror(2012051701);
     end;


   procedure ttai_typedconstbuilder.begin_aggregate_internal(def: tdef; anonymous: boolean);
     var
       info: taggregateinformation;
       tck: ttypedconstkind;
     begin
       tck:=aggregate_kind(def);
       if tck=tck_simple then
         exit;
       if not assigned(faggregateinformation) then
         faggregateinformation:=tfpobjectlist.create
       { if we're starting an anonymous record, we can't align it yet because
         the alignment depends on the fields that will be added -> we'll do
         it at the end }
       else if not anonymous then
         begin
           { add padding if necessary, and update the current field/offset }
           info:=curagginfo;
           if is_record(curagginfo.def) or
              is_object(curagginfo.def) then
             pad_next_field(def);
         end
       { if this is the outer record, no padding is required; the alignment
         has to be specified explicitly in that case via get_final_asmlist() }
       else if assigned(curagginfo) and
               (curagginfo.def.typ=recorddef) then
         { mark where we'll have to insert the padding bytes at the end }
         mark_anon_aggregate_alignment;
       info:=caggregateinformation.create(def,aggregate_kind(def));
       faggregateinformation.add(info);
     end;


   procedure ttai_typedconstbuilder.end_aggregate_internal(def: tdef; anonymous: boolean);
     var
       info: taggregateinformation;
       fillbytes: asizeint;
       tck: ttypedconstkind;
     begin
       tck:=aggregate_kind(def);
       if tck=tck_simple then
         exit;
       info:=curagginfo;
       if not assigned(info) then
         internalerror(2014091002);
       if def<>info.def then
         internalerror(2014091205);
       { add tail padding if necessary }
       if (is_record(def) or
           is_object(def)) and
          not is_packed_record_or_object(def) then
         begin
           fillbytes:=def.size-info.curoffset;
           while fillbytes>0 do
             begin
               do_emit_tai(Tai_const.Create_8bit(0),u8inttype);
               dec(fillbytes)
             end;
         end;
       { pop and free the information }
       faggregateinformation.count:=faggregateinformation.count-1;
       info.free;
     end;


   class function ttai_typedconstbuilder.get_dynstring_rec_name(typ: tstringtype; winlike: boolean; len: asizeint): string;
     begin
       case typ of
         st_ansistring:
           result:='ansistrrec';
         st_unicodestring,
         st_widestring:
           if (typ=st_unicodestring) or
              not winlike then
             result:='unicodestrrec'
           else
             result:='widestrrec';
         else
           internalerror(2014080402);
       end;
       result:=result+tostr(len);
     end;


   function ttai_typedconstbuilder.emit_ansistring_const(datalist: TAsmList; data: pchar; len: asizeint; encoding: tstringencoding): tasmlabofs;
     var
       s: PChar;
       startlab: tasmlabel;
       ansistrrecdef: trecorddef;
       datadef: tdef;
       datatcb: ttai_typedconstbuilder;
     begin
       start_internal_data_builder(datalist,sec_rodata_norel,'',datatcb,startlab);
       result:=datatcb.emit_string_const_common(st_ansistring,len,encoding,startlab);

       getmem(s,len+1);
       move(data^,s^,len);
       s[len]:=#0;
       { terminating zero included }
       datadef:=getarraydef(cansichartype,len+1);
       datatcb.maybe_begin_aggregate(datadef);
       datatcb.emit_tai(tai_string.create_pchar(s,len+1),datadef);
       datatcb.maybe_end_aggregate(datadef);
       ansistrrecdef:=datatcb.end_anonymous_record;
       finish_internal_data_builder(datatcb,startlab,ansistrrecdef,const_align(sizeof(pointer)));
     end;


   function ttai_typedconstbuilder.emit_unicodestring_const(datalist: TAsmList; data: pointer; encoding: tstringencoding; winlike: boolean):tasmlabofs;
     var
       i, strlength: longint;
       string_symofs: asizeint;
       startlab: tasmlabel;
       datadef: tdef;
       uniwidestrrecdef: trecorddef;
       datatcb: ttai_typedconstbuilder;
     begin
       start_internal_data_builder(datalist,sec_rodata_norel,'',datatcb,startlab);
       strlength:=getlengthwidestring(pcompilerwidestring(data));
       if winlike then
         begin
           result.lab:=startlab;
           datatcb.begin_anonymous_record('$'+get_dynstring_rec_name(st_widestring,true,strlength),
             4,
             targetinfos[target_info.system]^.alignment.recordalignmin,
             targetinfos[target_info.system]^.alignment.maxCrecordalign);
           datatcb.emit_tai(Tai_const.Create_32bit(strlength*cwidechartype.size),s32inttype);
           { can we optimise by placing the string constant label at the
             required offset? }
           string_symofs:=get_string_symofs(st_widestring,true);
           if string_symofs=0 then
             begin
               { yes }
               datatcb.emit_tai(Tai_label.Create(result.lab),widecharpointertype);
               { allocate a separate label for the start of the data (see
                 emit_string_const_common() for explanation) }
               startlab:=tasmlabel.create(current_asmdata.AsmSymbolDict,startlab.name+'$strlab',startlab.bind,startlab.typ);
             end
           else
             internalerror(2015031502);
           result.ofs:=string_symofs;
         end
       else
         begin
           result:=datatcb.emit_string_const_common(st_unicodestring,strlength,encoding,startlab);
         end;
       if cwidechartype.size = 2 then
         begin
           datadef:=getarraydef(cwidechartype,strlength+1);
           datatcb.maybe_begin_aggregate(datadef);
           for i:=0 to strlength-1 do
             datatcb.emit_tai(Tai_const.Create_16bit(pcompilerwidestring(data)^.data[i]),cwidechartype);
           { ending #0 }
           datatcb.emit_tai(Tai_const.Create_16bit(0),cwidechartype);
           datatcb.maybe_end_aggregate(datadef);
           uniwidestrrecdef:=datatcb.end_anonymous_record;
         end
       else
         { code generation for other sizes must be written }
         internalerror(200904271);
       finish_internal_data_builder(datatcb,startlab,datadef,const_align(sizeof(pint)));
     end;


   procedure ttai_typedconstbuilder.emit_string_offset(const ll: tasmlabofs; const strlength: longint; const st: tstringtype; const winlikewidestring: boolean; const charptrdef: tdef);
     begin
       emit_tai(Tai_const.Create_sym_offset(ll.lab,ll.ofs),charptrdef);
     end;


   function ttai_typedconstbuilder.emit_shortstring_const(const str: shortstring): tdef;
     begin
       { we use an arraydef instead of a shortstringdef, because we don't have
         functionality in place yet to reuse shortstringdefs of the same length
         and neither the lowlevel nor the llvm typedconst builder cares about
         this difference }
       result:=getarraydef(cansichartype,length(str)+1);
       maybe_begin_aggregate(result);
       emit_tai(Tai_const.Create_8bit(length(str)),u8inttype);
       if str<>'' then
         emit_tai(Tai_string.Create(str),getarraydef(cansichartype,length(str)));
       maybe_end_aggregate(result);
     end;


   procedure ttai_typedconstbuilder.emit_guid_const(const guid: tguid);
     var
       i: longint;
     begin
       maybe_begin_aggregate(rec_tguid);
       { variant record -> must specify which fields get initialised }
       next_field:=tfieldvarsym(rec_tguid.symtable.symlist[0]);
       emit_tai(Tai_const.Create_32bit(longint(guid.D1)),u32inttype);
       next_field:=tfieldvarsym(rec_tguid.symtable.symlist[1]);
       emit_tai(Tai_const.Create_16bit(guid.D2),u16inttype);
       next_field:=tfieldvarsym(rec_tguid.symtable.symlist[2]);
       emit_tai(Tai_const.Create_16bit(guid.D3),u16inttype);
       next_field:=tfieldvarsym(rec_tguid.symtable.symlist[3]);
       { the array }
       maybe_begin_aggregate(tfieldvarsym(rec_tguid.symtable.symlist[3]).vardef);
       for i:=Low(guid.D4) to High(guid.D4) do
         emit_tai(Tai_const.Create_8bit(guid.D4[i]),u8inttype);
       maybe_end_aggregate(tfieldvarsym(rec_tguid.symtable.symlist[3]).vardef);
       maybe_end_aggregate(rec_tguid);
     end;

   procedure ttai_typedconstbuilder.emit_procdef_const(pd: tprocdef);
     begin
       emit_tai(Tai_const.Createname(pd.mangledname,AT_FUNCTION,0),pd.getcopyas(procvardef,pc_address_only));
     end;


   procedure ttai_typedconstbuilder.emit_ord_const(value: int64; def: tdef);
     begin
       case def.size of
         1:
           emit_tai(Tai_const.Create_8bit(byte(value)),def);
         2:
           emit_tai(Tai_const.Create_16bit(word(value)),def);
         4:
           emit_tai(Tai_const.Create_32bit(longint(value)),def);
         8:
           emit_tai(Tai_const.Create_64bit(value),def);
         else
           internalerror(2014100501);
       end;
     end;


   procedure ttai_typedconstbuilder.maybe_begin_aggregate(def: tdef);
     begin
       begin_aggregate_internal(def,false);
     end;


   procedure ttai_typedconstbuilder.maybe_end_aggregate(def: tdef);
     begin
       end_aggregate_internal(def,false);
     end;


   function ttai_typedconstbuilder.begin_anonymous_record(const optionalname: string; packrecords, recordalignmin, maxcrecordalign: shortint): trecorddef;
     var
       anonrecorddef: trecorddef;
       srsym: tsym;
       srsymtable: tsymtable;
       found: boolean;
     begin
       { if the name is specified, we create a typesym with that name in order
         to ensure we can find it again later with that name -> reuse here as
         well if possible (and that also avoids duplicate type name issues) }
       if optionalname<>'' then
         begin
           if optionalname[1]='$' then
             found:=searchsym_type(copy(optionalname,2,length(optionalname)),srsym,srsymtable)
           else
             found:=searchsym_type(optionalname,srsym,srsymtable);
           if found then
             begin
               if ttypesym(srsym).typedef.typ<>recorddef then
                 internalerror(2014091207);
               result:=trecorddef(ttypesym(srsym).typedef);
               maybe_begin_aggregate(result);
               exit;
             end;
         end;
       { create skeleton def }
       anonrecorddef:=crecorddef.create_global_internal(optionalname,packrecords,recordalignmin,maxcrecordalign);
       { generic aggregate housekeeping }
       begin_aggregate_internal(anonrecorddef,true);
       { mark as anonymous record }
       curagginfo.anonrecord:=true;
       { in case a descendent wants to do something with the anonrecorddef too }
       result:=anonrecorddef;
     end;


   function ttai_typedconstbuilder.end_anonymous_record: trecorddef;
     var
       info: taggregateinformation;
       anonrecord: boolean;
     begin
       info:=curagginfo;
       if not assigned(info) or
          (info.def.typ<>recorddef) then
         internalerror(2014080201);
       result:=trecorddef(info.def);
       { make a copy, as we need it after info has been freed by
         maybe_end_aggregate(result) }
       anonrecord:=info.anonrecord;
       { finalise the record skeleton (all fields have been added already by
         emit_tai()) -- anonrecord may not be set in case we reused an earlier
         constructed def }
       if anonrecord then
         trecordsymtable(result.symtable).addalignmentpadding;
       end_aggregate_internal(result,true);
       if anonrecord and
          assigned(curagginfo) and
          (curagginfo.def.typ=recorddef) then
         insert_marked_aggregate_alignment(result);
     end;


   procedure ttai_typedconstbuilder.queue_init(todef: tdef);
     begin
       { nested call to init? }
       if fqueue_offset<>low(fqueue_offset) then
         internalerror(2014062101);
       fqueue_offset:=0;
     end;


   procedure ttai_typedconstbuilder.queue_vecn(def: tdef; const index: tconstexprint);
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


   procedure ttai_typedconstbuilder.queue_subscriptn(def: tabstractrecorddef; vs: tfieldvarsym);
     begin
       inc(fqueue_offset,vs.fieldoffset);
     end;


   procedure ttai_typedconstbuilder.queue_typeconvn(fromdef, todef: tdef);
     begin
       { do nothing }
     end;

   procedure ttai_typedconstbuilder.queue_addrn(fromdef, todef: tdef);
     begin
       { do nothing }
     end;


   procedure ttai_typedconstbuilder.queue_emit_proc(pd: tprocdef);
     begin
       if fqueue_offset<>0 then
         internalerror(2014092101);
       emit_procdef_const(pd);
       fqueue_offset:=low(fqueue_offset);
     end;


   procedure ttai_typedconstbuilder.queue_emit_staticvar(vs: tstaticvarsym);
     begin
       { getpointerdef because we are emitting a pointer to the staticvarsym
         data, not the data itself }
       emit_tai(Tai_const.Createname(vs.mangledname,fqueue_offset),getpointerdef(vs.vardef));
       fqueue_offset:=low(fqueue_offset);
     end;


   procedure ttai_typedconstbuilder.queue_emit_label(l: tlabelsym);
     begin
       emit_tai(Tai_const.Createname(l.mangledname,fqueue_offset),voidcodepointertype);
       fqueue_offset:=low(fqueue_offset);
     end;


   procedure ttai_typedconstbuilder.queue_emit_const(cs: tconstsym);
     begin
       if cs.consttyp<>constresourcestring then
         internalerror(2014062102);
       if fqueue_offset<>0 then
         internalerror(2014062103);
       { warning: update if/when the type of resource strings changes }
       emit_tai(Tai_const.Createname(make_mangledname('RESSTR',cs.owner,cs.name),AT_DATA,sizeof(pint)),cansistringtype);
       fqueue_offset:=low(fqueue_offset);
     end;


   procedure ttai_typedconstbuilder.queue_emit_asmsym(sym: tasmsymbol; def: tdef);
     begin
       { getpointerdef, because "sym" represents the address of whatever the
         data is }
       def:=getpointerdef(def);
       emit_tai(Tai_const.Create_sym_offset(sym,fqueue_offset),def);
       fqueue_offset:=low(fqueue_offset);
     end;

   procedure ttai_typedconstbuilder.queue_emit_ordconst(value: int64; def: tdef);
     begin
       emit_ord_const(value,def);
       fqueue_offset:=low(fqueue_offset);
     end;


{****************************************************************************
                           tai_abstracttypedconst
 ****************************************************************************}

   class constructor ttai_lowleveltypedconstbuilder.classcreate;
     begin
       caggregateinformation:=tlowlevelaggregateinformation;
     end;


   procedure ttai_lowleveltypedconstbuilder.mark_anon_aggregate_alignment;
     var
       marker: tai_marker;
     begin
       marker:=tai_marker.Create(mark_position);
       fasmlist.concat(marker);
       tlowlevelaggregateinformation(curagginfo).anonrecmarker:=marker;
     end;


   procedure ttai_lowleveltypedconstbuilder.insert_marked_aggregate_alignment(def: tdef);
     var
       info: tlowlevelaggregateinformation;
       fillbytes: asizeint;
     begin
       info:=tlowlevelaggregateinformation(curagginfo);
       if not assigned(info.anonrecmarker) then
         internalerror(2014091401);
       fillbytes:=info.prepare_next_field(def);
       while fillbytes>0 do
         begin
           fasmlist.insertafter(tai_const.create_8bit(0),info.anonrecmarker);
           dec(fillbytes);
         end;
       fasmlist.remove(info.anonrecmarker);
       info.anonrecmarker.free;
       info.anonrecmarker:=nil;
     end;



begin
  ctai_typedconstbuilder:=ttai_lowleveltypedconstbuilder;
end.

