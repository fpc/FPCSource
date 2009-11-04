{
    Copyright (c) 2003-2004 by Peter Vreman and Florian Klaempfl

    This units contains support for STABS debug info generation

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
unit dbgstabs;

{$i fpcdefs.inc}

interface

    uses
      cclasses,
      dbgbase,cgbase,
      symtype,symdef,symsym,symtable,symbase,
      aasmtai,aasmdata;

    const
      { stab types }
      N_GSYM = $20;
      N_STSYM = 38;     { initialized const }
      N_LCSYM = 40;     { non initialized variable}
      N_Function = $24; { function or const }
      N_TextLine = $44;
      N_DataLine = $46;
      N_BssLine = $48;
      N_RSYM = $40;     { register variable }
      N_LSYM = $80;
      N_tsym = 160;
      N_SourceFile = $64;
{ APPLE LOCAL N_OSO: This is the stab that associated the .o file with the
   N_SO stab, in the case where debug info is mostly stored in the .o file.  }
      N_OSO        = $66;
      N_IncludeFile = $84;
      N_BINCL = $82;
      N_EINCL = $A2;
      N_LBRAC = $C0;
      N_EXCL  = $C2;
      N_RBRAC = $E0;

    type
      TDebugInfoStabs=class(TDebugInfo)
      private
        writing_def_stabs  : boolean;
        global_stab_number : word;
        defnumberlist      : TFPObjectList;
        { tsym writing }
        function  sym_var_value(const s:string;arg:pointer):string;
        function  sym_stabstr_evaluate(sym:tsym;const s:string;const vars:array of string):ansistring;
        procedure write_sym_stabstr(list:TAsmList;sym:tsym;const ss:ansistring);
        { tdef writing }
        function  def_stab_number(def:tdef):string;
        function  def_stab_classnumber(def:tobjectdef):string;
        function  def_var_value(const s:string;arg:pointer):string;
        function  def_stabstr_evaluate(def:tdef;const s:string;const vars:array of string):ansistring;
        procedure write_def_stabstr(list:TAsmList;def:tdef;const ss:ansistring);
        procedure field_add_stabstr(p:TObject;arg:pointer);
        procedure method_add_stabstr(p:TObject;arg:pointer);
        procedure field_write_defs(p:TObject;arg:pointer);
        function  get_enum_defstr(def: tenumdef; lowerbound: longint): ansistring;
        function  get_appendsym_paravar_reg(sym:tparavarsym;const typ,stabstr:string;reg: tregister): ansistring;
      protected
        procedure appendsym_staticvar(list:TAsmList;sym:tstaticvarsym);override;
        procedure appendsym_paravar(list:TAsmList;sym:tparavarsym);override;
        procedure appendsym_localvar(list:TAsmList;sym:tlocalvarsym);override;
        procedure appendsym_fieldvar(list:TAsmList;sym:tfieldvarsym);override;
        procedure appendsym_const(list:TAsmList;sym:tconstsym);override;
        procedure appendsym_type(list:TAsmList;sym:ttypesym);override;
        procedure appendsym_label(list:TAsmList;sym:tlabelsym);override;
        procedure beforeappenddef(list:TAsmList;def:tdef);override;
        procedure appenddef_ord(list:TAsmList;def:torddef);override;
        procedure appenddef_float(list:TAsmList;def:tfloatdef);override;
        procedure appenddef_file(list:TAsmList;def:tfiledef);override;
        procedure appenddef_enum(list:TAsmList;def:tenumdef);override;
        procedure appenddef_array(list:TAsmList;def:tarraydef);override;
        procedure appenddef_record(list:TAsmList;def:trecorddef);override;
        procedure appenddef_object(list:TAsmList;def:tobjectdef);override;
        procedure appenddef_pointer(list:TAsmList;def:tpointerdef);override;
        procedure appenddef_string(list:TAsmList;def:tstringdef);override;
        procedure appenddef_procvar(list:TAsmList;def:tprocvardef);override;
        procedure appenddef_variant(list:TAsmList;def:tvariantdef);override;
        procedure appenddef_set(list:TAsmList;def:tsetdef);override;
        procedure appenddef_formal(list:TAsmList;def:tformaldef);override;
        procedure appenddef_undefined(list:TAsmList;def: tundefineddef);override;
        procedure appendprocdef(list:TAsmList;def:tprocdef);override;
      public
        procedure inserttypeinfo;override;
        procedure insertmoduleinfo;override;
        procedure insertlineinfo(list:TAsmList);override;
        procedure referencesections(list:TAsmList);override;
      end;


implementation

    uses
      SysUtils,cutils,cfileutl,
      systems,globals,globtype,verbose,constexp,
      symconst,defutil,
      cpuinfo,cpubase,paramgr,
      aasmbase,procinfo,
      finput,fmodule,ppu;

    function GetSymName(Sym : TSymEntry) : string;
    begin
      if Not (cs_stabs_preservecase in current_settings.globalswitches) then
        result := Sym.Name
      else
        result := Sym.RealName;
    end;

    function GetSymTableName(SymTable : TSymTable) : string;
    begin
      if Not (cs_stabs_preservecase in current_settings.globalswitches) then
        result := SymTable.Name^
      else
        result := SymTable.RealName^;
    end;

    const
      memsizeinc = 512;

      tagtypes = [
        recorddef,
        enumdef,
        stringdef,
        filedef,
        objectdef
      ];

    type
       get_var_value_proc=function(const s:string;arg:pointer):string of object;


    function string_evaluate(s:string;get_var_value:get_var_value_proc;get_var_value_arg:pointer;const vars:array of string):ansistring;
    (*
     S contains a prototype of a result. Stabstr_evaluate will expand
     variables and parameters.

     Output is s in ASCIIZ format, with the following expanded:

     ${varname}   - The variable name is expanded.
     $n           - The parameter n is expanded.
     $$           - Is expanded to $
    *)

    const maxvalue=9;
          maxdata=1023;

    var i,j:byte;
        varname:string[63];
        varno,varcounter:byte;
        varvalues:array[0..9] of pshortstring;
        {1 kb of parameters is the limit. 256 extra bytes are allocated to
         ensure buffer integrity.}
        varvaluedata:array[0..maxdata+256] of char;
        varptr:Pchar;
        varidx : byte;
        len:longint;
        r:Pchar;

    begin
      {Two pass approach, first, calculate the length and receive variables.}
      i:=1;
      len:=0;
      varcounter:=0;
      varptr:=@varvaluedata[0];
      while i<=length(s) do
        begin
          if (s[i]='$') and (i<length(s)) then
            begin
             if s[i+1]='$' then
               begin
                 inc(len);
                 inc(i);
               end
             else if (s[i+1]='{') and (length(s)>2) and (i<length(s)-2) then
               begin
                 varname:='';
                 inc(i,2);
                 repeat
                   inc(varname[0]);
                   varname[length(varname)]:=s[i];
                   s[i]:=char(varcounter);
                   inc(i);
                 until s[i]='}';
                 varvalues[varcounter]:=pshortstring(varptr);
                 if varptr>@varvaluedata[maxdata] then
                   internalerrorproc(200411152);
                 pshortstring(varptr)^:=get_var_value(varname,get_var_value_arg);
                 inc(len,length(pshortstring(varptr)^));
                 inc(varptr,length(pshortstring(varptr)^)+1);
                 inc(varcounter);
               end
             else if s[i+1] in ['1'..'9'] then
               begin
                 varidx:=byte(s[i+1])-byte('1');
                 if varidx>high(vars) then
                   internalerror(200509263);
                 inc(len,length(vars[varidx]));
                 inc(i);
               end;
            end
          else
            inc(len);
          inc(i);
        end;

      {Second pass, writeout result.}
      setlength(result,len);
      r:=pchar(result);
      i:=1;
      while i<=length(s) do
        begin
          if (s[i]='$') and (i<length(s)) then
            begin
             if s[i+1]='$' then
               begin
                 r^:='$';
                 inc(r);
                 inc(i);
               end
             else if (s[i+1]='{') and (length(s)>2) and (i<length(s)-2) then
               begin
                 varname:='';
                 inc(i,2);
                 varno:=byte(s[i]);
                 repeat
                   inc(i);
                 until s[i]='}';
                 for j:=1 to length(varvalues[varno]^) do
                   begin
                     r^:=varvalues[varno]^[j];
                     inc(r);
                   end;
               end
             else if s[i+1] in ['0'..'9'] then
               begin
                 for j:=1 to length(vars[byte(s[i+1])-byte('1')]) do
                   begin
                     r^:=vars[byte(s[i+1])-byte('1')][j];
                     inc(r);
                   end;
                 inc(i);
               end
            end
          else
            begin
              r^:=s[i];
              inc(r);
            end;
          inc(i);
        end;
      { verify that the length was correct }
      if r^<>#0 then
        internalerror(200802031);
    end;


{****************************************************************************
                               TDef support
****************************************************************************}

    function TDebugInfoStabs.def_stab_number(def:tdef):string;
      begin
        { procdefs only need a number, mark them as already written
          so they won't be written implicitly }
        if (def.typ=procdef) then
          def.dbg_state:=dbg_state_written;

        { Stab must already be written, or we must be busy writing it }
        if writing_def_stabs and
           not(def.dbg_state in [dbg_state_writing,dbg_state_written,dbg_state_queued]) then
          internalerror(200403091);

        { Keep track of used stabs, this info is only usefull for stabs
          referenced by the symbols. Definitions will always include all
          required stabs }
        if def.dbg_state=dbg_state_unused then
          def.dbg_state:=dbg_state_used;
        { Need a new number? }
        if def.stab_number=0 then
          begin
            inc(global_stab_number);
            { classes require 2 numbers }
            if is_class(def) then
              inc(global_stab_number);
            def.stab_number:=global_stab_number;
            if global_stab_number>=defnumberlist.count then
              defnumberlist.count:=global_stab_number+250;
            defnumberlist[global_stab_number]:=def;
          end;
        result:=tostr(def.stab_number);
      end;


    function TDebugInfoStabs.def_stab_classnumber(def:tobjectdef):string;
      begin
        if def.stab_number=0 then
          def_stab_number(def);
        if (def.objecttype=odt_class) then
          result:=tostr(def.stab_number-1)
        else
          result:=tostr(def.stab_number);
      end;


    function TDebugInfoStabs.def_var_value(const s:string;arg:pointer):string;
      var
        def : tdef;
      begin
        def:=tdef(arg);
        result:='';
        if s='numberstring' then
          result:=def_stab_number(def)
        else if s='sym_name' then
          begin
            if assigned(def.typesym) then
               result:=GetSymName(Ttypesym(def.typesym));
          end
        else if s='N_LSYM' then
          result:=tostr(N_LSYM)
        else if s='savesize' then
          result:=tostr(def.size);
      end;


    function TDebugInfoStabs.def_stabstr_evaluate(def:tdef;const s:string;const vars:array of string):ansistring;
      begin
        result:=string_evaluate(s,@def_var_value,def,vars);
      end;


    procedure TDebugInfoStabs.field_add_stabstr(p:TObject;arg:pointer);
      var
        spec    : string[3];
        varsize : aint;
        newss   : ansistring;
        ss      : pansistring absolute arg;
      begin
        if (tsym(p).visibility=vis_hidden) then
          exit;
        { static variables from objects are like global objects }
        if (Tsym(p).typ=fieldvarsym) and
           not(sp_static in Tsym(p).symoptions) then
          begin
           case tsym(p).visibility of
             vis_private,
             vis_strictprivate :
               spec:='/0';
             vis_protected,
             vis_strictprotected :
               spec:='/1';
             else
               spec:='';
           end;
           if (tabstractrecordsymtable(tsym(p).owner).usefieldalignment<>bit_alignment) then
              begin
                varsize:=tfieldvarsym(p).vardef.size;
                { open arrays made overflows !! }
                { how can a record/object/class contain an open array? (JM) }
                if varsize>$fffffff then
                  varsize:=$fffffff;
                newss:=def_stabstr_evaluate(nil,'$1:$2,$3,$4;',[GetSymName(tfieldvarsym(p)),
                                     spec+def_stab_number(tfieldvarsym(p).vardef),
                                     tostr(TConstExprInt(tfieldvarsym(p).fieldoffset)*8),tostr(varsize*8)])
              end
            else
              newss:=def_stabstr_evaluate(nil,'$1:$2,$3,$4;',[GetSymName(tfieldvarsym(p)),
                                   spec+def_stab_number(tfieldvarsym(p).vardef),
                                   tostr(TConstExprInt(tfieldvarsym(p).fieldoffset)),tostr(tfieldvarsym(p).vardef.packedbitsize)]);
            ss^:=ss^+newss;
          end;
      end;


    procedure TDebugInfoStabs.method_add_stabstr(p:TObject;arg:pointer);
      var
        virtualind,argnames : string;
        pd     : tprocdef;
        lindex : longint;
        arglength : byte;
        sp : char;
        i : integer;
        parasym : tparavarsym;
        newss   : ansistring;
        ss      : pansistring absolute arg;
      begin
        if tsym(p).typ = procsym then
         begin
           pd :=tprocdef(tprocsym(p).ProcdefList[0]);
           if (po_virtualmethod in pd.procoptions) then
             begin
               lindex := pd.extnumber;
               {doesnt seem to be necessary
               lindex := lindex or $80000000;}
               virtualind := '*'+tostr(lindex)+';'+def_stab_classnumber(pd._class)+';'
             end
            else
             virtualind := '.';

            { used by gdbpas to recognize constructor and destructors }
            if (pd.proctypeoption=potype_constructor) then
              argnames:='__ct__'
            else if (pd.proctypeoption=potype_destructor) then
              argnames:='__dt__'
            else
              argnames := '';

           { arguments are not listed here }
           {we don't need another definition}
            for i:=0 to pd.paras.count-1 do
              begin
                parasym:=tparavarsym(pd.paras[i]);
                if Parasym.vardef.typ = formaldef then
                  begin
                    case Parasym.varspez of
                      vs_var :
                        argnames := argnames+'3var';
                      vs_const :
                        argnames:=argnames+'5const';
                      vs_out :
                        argnames:=argnames+'3out';
                    end;
                  end
                else
                  begin
                    { if the arg definition is like (v: ^byte;..
                    there is no sym attached to data !!! }
                    if assigned(Parasym.vardef.typesym) then
                      begin
                        arglength := length(GetSymName(Parasym.vardef.typesym));
                        argnames := argnames + tostr(arglength)+GetSymName(Parasym.vardef.typesym);
                      end
                    else
                      argnames:=argnames+'11unnamedtype';
                  end;
              end;
           { here 2A must be changed for private and protected }
           { 0 is private 1 protected and 2 public }
           case tsym(p).visibility of
             vis_private,
             vis_strictprivate :
               sp:='0';
             vis_protected,
             vis_strictprotected :
               sp:='1'
             else
               sp:='2';
           end;
           newss:=def_stabstr_evaluate(nil,'$1::$2=##$3;:$4;$5A$6;',[GetSymName(tsym(p)),def_stab_number(pd),
                                    def_stab_number(pd.returndef),argnames,sp,
                                    virtualind]);
           ss^:=ss^+newss;
         end;
      end;


    procedure TDebugInfoStabs.field_write_defs(p:TObject;arg:pointer);
      begin
        if (Tsym(p).typ=fieldvarsym) and
           not(sp_static in Tsym(p).symoptions) then
          appenddef(TAsmList(arg),tfieldvarsym(p).vardef);
      end;


    procedure TDebugInfoStabs.write_def_stabstr(list:TAsmList;def:tdef;const ss:ansistring);
      var
        stabchar : string[2];
        symname  : string[20];
        st    : ansistring;
        p     : pchar;
      begin
        { type prefix }
        if def.typ in tagtypes then
          stabchar := 'Tt'
        else
          stabchar := 't';
        { in case of writing the class record structure, we always have to
          use the class name (so it refers both to the struct and the
          pointer to the struct), otherwise gdb crashes (see tests/webtbs/tw9766.pp) }
        if is_class(def) and
           tobjectdef(def).writing_class_record_dbginfo then
          st:=def_stabstr_evaluate(def,'"${sym_name}:$1$2=',[stabchar,def_stab_classnumber(tobjectdef(def))])
        else
          begin
            { Type names for types defined in the current unit are already written in
              the typesym }
            if (def.owner.symtabletype=globalsymtable) and
               not(def.owner.iscurrentunit) then
              symname:='${sym_name}'
            else
              symname:='';
            st:=def_stabstr_evaluate(def,'"'+symname+':$1$2=',[stabchar,def_stab_number(def)]);
          end;
        st:=st+ss;
        { line info is set to 0 for all defs, because the def can be in an other
          unit and then the linenumber is invalid in the current sourcefile }
        st:=st+def_stabstr_evaluate(def,'",${N_LSYM},0,0,0',[]);
        { add to list }
        getmem(p,length(st)+1);
        move(pchar(st)^,p^,length(st)+1);
        list.concat(Tai_stab.create(stab_stabs,p));
      end;


    procedure TDebugInfoStabs.appenddef_string(list:TAsmList;def:tstringdef);
      var
        bytest,charst,longst : string;
        ss : ansistring;
        slen : longint;
      begin
        ss:='';
        case def.stringtype of
          st_shortstring:
            begin
              { fix length of openshortstring }
              slen:=def.len;
              if slen=0 then
                slen:=255;
              charst:=def_stab_number(cchartype);
              bytest:=def_stab_number(u8inttype);
              ss:=def_stabstr_evaluate(def,'s$1length:$2,0,8;st:ar$2;1;$3;$4,8,$5;;',
                          [tostr(slen+1),bytest,tostr(slen),charst,tostr(slen*8)]);
            end;
          st_longstring:
            begin
              charst:=def_stab_number(cchartype);
              bytest:=def_stab_number(u8inttype);
              longst:=def_stab_number(u32inttype);
              ss:=def_stabstr_evaluate(def,'s$1length:$2,0,32;dummy:$6,32,8;st:ar$2;1;$3;$4,40,$5;;',
                          [tostr(def.len+5),longst,tostr(def.len),charst,tostr(def.len*8),bytest]);
           end;
         st_ansistring:
           begin
             { looks like a pchar }
             ss:='*'+def_stab_number(cchartype);
           end;
         st_unicodestring,
         st_widestring:
           begin
             { looks like a pwidechar }
             ss:='*'+def_stab_number(cwidechartype);
           end;
        end;
        write_def_stabstr(list,def,ss);
      end;


    function TDebugInfoStabs.get_enum_defstr(def: tenumdef; lowerbound: longint): ansistring;
      var
        i: longint;
        p: tenumsym;
      begin
        { we can specify the size with @s<size>; prefix PM }
        if def.size <> std_param_align then
          result:='@s'+tostr(def.size*8)+';e'
        else
          result:='e';
        p := tenumsym(def.firstenum);
        { the if-test is required because pred(def.minval) might overflow;
          the longint() typecast should be safe because stabs is not
          supported for 64 bit targets }
        if (def.minval<>lowerbound) then
          for i:=lowerbound to pred(longint(def.minval)) do
            result:=result+'<invalid>:'+tostr(i)+',';

        while assigned(p) do
          begin
            result:=result+GetSymName(p)+':'+tostr(p.value)+',';
            p:=p.nextenum;
          end;
        { the final ',' is required to have a valid stabs }
        result:=result+';';
      end;

    procedure TDebugInfoStabs.appenddef_enum(list:TAsmList;def:tenumdef);
      begin
        write_def_stabstr(list,def,get_enum_defstr(def,def.minval));
      end;


    procedure TDebugInfoStabs.appenddef_ord(list:TAsmList;def:torddef);
      var
        ss : ansistring;
      begin
        ss:='';
        if cs_gdb_valgrind in current_settings.globalswitches then
          begin
            case def.ordtype of
              uvoid :
                ss:=def_stab_number(def);
              pasbool,
              bool8bit,
              bool16bit,
              bool32bit,
              bool64bit :
                ss:=def_stabstr_evaluate(def,'r${numberstring};0;255;',[]);
              u32bit,
              s64bit,
              u64bit :
                ss:=def_stabstr_evaluate(def,'r${numberstring};0;-1;',[]);
              else
                ss:=def_stabstr_evaluate(def,'r${numberstring};$1;$2;',[tostr(longint(def.low.svalue)),tostr(longint(def.high.svalue))]);
            end;
          end
        else
          begin
            case def.ordtype of
              uvoid :
                ss:=def_stab_number(def);
              uchar :
                ss:='-20;';
              uwidechar :
                ss:='-30;';
              pasbool,
              bool8bit :
                ss:='-21;';
              bool16bit :
                ss:='-22;';
              bool32bit :
                ss:='-23;';
              bool64bit :
                { no clue if this is correct (FK) }
                ss:='-23;';
              u64bit :
                ss:='-32;';
              s64bit :
                ss:='-31;';
              {u32bit : result:=def_stab_number(s32inttype)+';0;-1;'); }
              else
                ss:=def_stabstr_evaluate(def,'r${numberstring};$1;$2;',[tostr(longint(def.low.svalue)),tostr(longint(def.high.svalue))]);
            end;
         end;
        write_def_stabstr(list,def,ss);
      end;


    procedure TDebugInfoStabs.appenddef_float(list:TAsmList;def:tfloatdef);
      var
        ss : ansistring;
      begin
        ss:='';
        case def.floattype of
          s32real,
          s64real,
          s80real:
            ss:=def_stabstr_evaluate(def,'r$1;${savesize};0;',[def_stab_number(s32inttype)]);
          s64currency,
          s64comp:
            ss:=def_stabstr_evaluate(def,'r$1;-${savesize};0;',[def_stab_number(s32inttype)]);
          else
            internalerror(200509261);
        end;
        write_def_stabstr(list,def,ss);
      end;


    procedure TDebugInfoStabs.appenddef_file(list:TAsmList;def:tfiledef);
      var
        ss : ansistring;
      begin
{$ifdef cpu64bitaddr}
        ss:=def_stabstr_evaluate(def,'s${savesize}HANDLE:$1,0,32;MODE:$1,32,32;RECSIZE:$2,64,64;'+
                                 '_PRIVATE:ar$1;1;64;$3,128,256;USERDATA:ar$1;1;16;$3,384,128;'+
                                 'NAME:ar$1;0;255;$4,512,2048;;',[def_stab_number(s32inttype),
                                 def_stab_number(s64inttype),
                                 def_stab_number(u8inttype),
                                 def_stab_number(cchartype)]);
{$else cpu64bitaddr}
        ss:=def_stabstr_evaluate(def,'s${savesize}HANDLE:$1,0,32;MODE:$1,32,32;RECSIZE:$1,64,32;'+
                                 '_PRIVATE:ar$1;1;32;$3,96,256;USERDATA:ar$1;1;16;$2,352,128;'+
                                 'NAME:ar$1;0;255;$3,480,2048;;',[def_stab_number(s32inttype),
                                 def_stab_number(u8inttype),
                                 def_stab_number(cchartype)]);
{$endif cpu64bitaddr}
        write_def_stabstr(list,def,ss);
      end;


    procedure TDebugInfoStabs.appenddef_record(list:TAsmList;def:trecorddef);
      var
        ss : ansistring;
      begin
        ss:='s'+tostr(def.size);
        def.symtable.SymList.ForEachCall(@field_add_stabstr,@ss);
        ss[length(ss)]:=';';
        write_def_stabstr(list,def,ss);
      end;


    procedure TDebugInfoStabs.appenddef_object(list:TAsmList;def:tobjectdef);

        procedure do_write_object(list:TAsmList;def:tobjectdef);
        var
          ss : ansistring;
          anc    : tobjectdef;
        begin
          ss:='';
          { Write the invisible pointer for the class? }
          if (def.objecttype=odt_class) and
             (not def.writing_class_record_dbginfo) then
            begin
              ss:='*'+def_stab_classnumber(def);
              write_def_stabstr(list,def,ss);
              exit;
            end;

          ss:='s'+tostr(tobjecTSymtable(def.symtable).datasize);
          if assigned(def.childof) then
            begin
              {only one ancestor not virtual, public, at base offset 0 }
              {       !1           ,    0       2         0    ,       }
              ss:=ss+'!1,020,'+def_stab_classnumber(def.childof)+';';
            end;

          {virtual table to implement yet}
          def.symtable.symList.ForEachCall(@field_add_stabstr,@ss);

          if (oo_has_vmt in def.objectoptions) and
             (
              not assigned(def.childof) or
              not(oo_has_vmt in def.childof.objectoptions)
             ) then
            ss:=ss+'$vf'+def_stab_classnumber(def)+':'+def_stab_number(vmtarraytype)+','+tostr(def.vmt_offset*8)+';';
          def.symtable.symList.ForEachCall(@method_add_stabstr,@ss);
          if (oo_has_vmt in def.objectoptions) then
            begin
               anc := def;
               while assigned(anc.childof) and (oo_has_vmt in anc.childof.objectoptions) do
                 anc := anc.childof;
               { just in case anc = self }
               ss:=ss+';~%'+def_stab_classnumber(anc)+';';
            end
          else
            ss:=ss+';';
          write_def_stabstr(list,def,ss);
        end;

      var
        oldtypesym : tsym;
      begin
        { classes require special code to write the record and the invisible pointer }
        if is_class(def) then
          begin
            { Write the record class itself }
            tobjectdef(def).writing_class_record_dbginfo:=true;
            do_write_object(list,def);
            tobjectdef(def).writing_class_record_dbginfo:=false;
            { Write the invisible pointer class }
            oldtypesym:=def.typesym;
            def.typesym:=nil;
            do_write_object(list,def);
            def.typesym:=oldtypesym;
          end
        else
          do_write_object(list,def);
        { VMT symbol }
        if (oo_has_vmt in tobjectdef(def).objectoptions) and
           assigned(def.owner) and
           assigned(def.owner.name) then
          list.concat(Tai_stab.create(stab_stabs,strpnew('"vmt_'+GetSymTableName(def.owner)+tobjectdef(def).objname^+':S'+
                 def_stab_number(vmttype)+'",'+tostr(N_STSYM)+',0,0,'+tobjectdef(def).vmt_mangledname)));
      end;


    procedure TDebugInfoStabs.appenddef_variant(list:TAsmList;def:tvariantdef);
      var
        ss : ansistring;
      begin
        ss:=def_stabstr_evaluate(def,'${numberstring};',[]);
        write_def_stabstr(list,def,ss);
      end;


    procedure TDebugInfoStabs.appenddef_pointer(list:TAsmList;def:tpointerdef);
      var
        ss : ansistring;
      begin
        ss:='*'+def_stab_number(tpointerdef(def).pointeddef);
        write_def_stabstr(list,def,ss);
      end;


    procedure TDebugInfoStabs.appenddef_set(list:TAsmList;def:tsetdef);
      var
        st,
        ss : ansistring;
        p: pchar;
        elementdefstabnr: string;
      begin
        { ugly hack: create a temporary subrange type if the lower bound of
          the set's element type is not a multiple of 8 (because we store them
          as if the lower bound is a multiple of 8) }
        if (def.setbase<>get_min_value(def.elementdef)) then
          begin
            { allocate a def number }
            inc(global_stab_number);
            elementdefstabnr:=tostr(global_stab_number);
            { anonymous subrange def }
            st:='":t'+elementdefstabnr+'=';
            if (def.elementdef.typ = enumdef) then
              st:=st+get_enum_defstr(tenumdef(def.elementdef),def.setbase)
            else
              st:=st+def_stabstr_evaluate(def.elementdef,'r'+elementdefstabnr+';$1;$2;',[tostr(longint(def.setbase)),tostr(longint(get_max_value(def.elementdef).svalue))]);
            st:=st+'",'+tostr(N_LSYM)+',0,0,0';
            { add to list }
            getmem(p,length(st)+1);
            move(pchar(st)^,p^,length(st)+1);
            list.concat(Tai_stab.create(stab_stabs,p));
          end
        else
          elementdefstabnr:=def_stab_number(def.elementdef);
        ss:=def_stabstr_evaluate(def,'@s$1;S$2',[tostr(def.size*8),elementdefstabnr]);
        write_def_stabstr(list,def,ss);
      end;


    procedure TDebugInfoStabs.appenddef_formal(list:TAsmList;def:tformaldef);
      var
        ss : ansistring;
      begin
        ss:=def_stabstr_evaluate(def,'${numberstring};',[]);
        write_def_stabstr(list,def,ss);
      end;


    procedure TDebugInfoStabs.appenddef_array(list:TAsmList;def:tarraydef);
      var
        tempstr: shortstring;
        ss : ansistring;
      begin
        if not is_packed_array(def) then
          begin
            tempstr:='ar$1;$2;$3;$4';
            if is_dynamic_array(def) then
              tempstr:='*'+tempstr;
            ss:=def_stabstr_evaluate(def,tempstr,[def_stab_number(tarraydef(def).rangedef),
                     tostr(tarraydef(def).lowrange),tostr(tarraydef(def).highrange),def_stab_number(tarraydef(def).elementdef)])
          end
        else
          begin
            // the @P seems to be ignored by gdb
            tempstr:=def_stabstr_evaluate(tarraydef(def).rangedef,'r${numberstring};$1;$2;',
              [tostr(tarraydef(def).lowrange),tostr(tarraydef(def).highrange)]);
            // will only show highrange-lowrange+1 bits in gdb
            ss:=def_stabstr_evaluate(def,'@s$1;@S;S$2',
              [tostr(TConstExprInt(tarraydef(def).elepackedbitsize) * tarraydef(def).elecount),tempstr]);
          end;
        write_def_stabstr(list,def,ss);
      end;


    procedure TDebugInfoStabs.appenddef_procvar(list:TAsmList;def:tprocvardef);
      var
        ss : ansistring;
      begin
        ss:='*f'+def_stab_number(tprocvardef(def).returndef);
        write_def_stabstr(list,def,ss);
      end;


    procedure TDebugInfoStabs.appenddef_undefined(list:TAsmList;def:tundefineddef);
      var
        ss : ansistring;
      begin
        ss:=def_stabstr_evaluate(def,'${numberstring};',[]);
        write_def_stabstr(list,def,ss);
      end;


    procedure TDebugInfoStabs.beforeappenddef(list:TAsmList;def:tdef);
      var
        anc : tobjectdef;
        i : longint;
      begin
        { write dependencies first }
        case def.typ of
          stringdef :
            begin
              if tstringdef(def).stringtype in [st_widestring,st_unicodestring] then
                appenddef(list,cwidechartype)
              else
                begin
                  appenddef(list,cchartype);
                  appenddef(list,u8inttype);
                end;
            end;
          floatdef :
            appenddef(list,s32inttype);
          filedef :
            begin
              appenddef(list,s32inttype);
{$ifdef cpu64bitaddr}
              appenddef(list,s64inttype);
{$endif cpu64bitaddr}
              appenddef(list,u8inttype);
              appenddef(list,cchartype);
            end;
          classrefdef :
            appenddef(list,pvmttype);
          pointerdef :
            appenddef(list,tpointerdef(def).pointeddef);
          setdef :
            appenddef(list,tsetdef(def).elementdef);
          procvardef :
            begin
              appenddef(list,tprocvardef(def).returndef);
              if assigned(tprocvardef(def).parast) then
                write_symtable_defs(list,tprocvardef(def).parast);
            end;
          procdef :
            begin
              appenddef(list,tprocdef(def).returndef);
              if assigned(tprocdef(def).parast) then
                write_symtable_defs(list,tprocdef(def).parast);
              if assigned(tprocdef(def).localst) and
                 (tprocdef(def).localst.symtabletype=localsymtable) then
                write_symtable_defs(list,tprocdef(def).localst);
            end;
          arraydef :
            begin
              appenddef(list,tarraydef(def).rangedef);
              appenddef(list,tarraydef(def).elementdef);
            end;
          recorddef :
            trecorddef(def).symtable.symList.ForEachCall(@field_write_defs,list);
          enumdef :
            if assigned(tenumdef(def).basedef) then
              appenddef(list,tenumdef(def).basedef);
          objectdef :
            begin
              { make sure we don't write child classdefs before their parent }
              { classdefs, because this crashes gdb                          }
              anc:=tobjectdef(def);
              while assigned(anc.childof) do
                begin
                  anc:=anc.childof;
                  if (anc.dbg_state=dbg_state_writing) then
                    { happens in case a field of a parent is of the (forward }
                    { defined) child type                                    }
                    begin
                      { We don't explicitly requeue it, but the fact that  }
                      { a child type was used in a parent before the child }
                      { type was fully defined means that it was forward   }
                      { declared, and will still be encountered later (it  }
                      { cannot have been declared in another unit, because }
                      { then this and that other unit would depend on      }
                      { eachother's interface)                             }
                      { Setting the state to queued however allows us to   }
                      { get the def number already without an IE           }
                      def.dbg_state:=dbg_state_queued;
                      exit;
                    end;
                end;
              appenddef(list,vmtarraytype);
              if assigned(tobjectdef(def).ImplementedInterfaces) then
                for i:=0 to tobjectdef(def).ImplementedInterfaces.Count-1 do
                  appenddef(list,TImplementedInterface(tobjectdef(def).ImplementedInterfaces[i]).IntfDef);
              { first the parents }
              anc:=tobjectdef(def);
              while assigned(anc.childof) do
                begin
                  anc:=anc.childof;
                  appenddef(list,anc);
                  if assigned(anc.ImplementedInterfaces) then
                    for i:=0 to anc.ImplementedInterfaces.Count-1 do
                      appenddef(list,TImplementedInterface(anc.ImplementedInterfaces[i]).IntfDef);
                end;
              tobjectdef(def).symtable.symList.ForEachCall(@field_write_defs,list);
            end;
        end;
      end;


    procedure TDebugInfoStabs.appendprocdef(list:TAsmList;def:tprocdef);
      var
        templist : TAsmList;
        stabsendlabel : tasmlabel;
        p  : pchar;
        RType : Char;
        Obj,Info : String;
        hs : string;
        ss : ansistring;
      begin
        if not assigned(def.procstarttai) then
          exit;

        { mark as used so the local type defs also be written }
        def.dbg_state:=dbg_state_used;

        templist:=TAsmList.create;

        { end of procedure }
        current_asmdata.getlabel(stabsendlabel,alt_dbgtype);

        if assigned(def.funcretsym) and
           (tabstractnormalvarsym(def.funcretsym).refs>0) then
          begin
            if tabstractnormalvarsym(def.funcretsym).localloc.loc=LOC_REFERENCE then
              begin
{ TODO: Need to add gdb support for ret in param register calling}
                if paramanager.ret_in_param(def.returndef,def.proccalloption) then
                  hs:='X*'
                else
                  hs:='X';
                templist.concat(Tai_stab.create(stab_stabs,strpnew(
                   '"'+GetSymName(def.procsym)+':'+hs+def_stab_number(def.returndef)+'",'+
                   tostr(N_tsym)+',0,0,'+tostr(tabstractnormalvarsym(def.funcretsym).localloc.reference.offset))));
                if (m_result in current_settings.modeswitches) then
                  templist.concat(Tai_stab.create(stab_stabs,strpnew(
                     '"RESULT:'+hs+def_stab_number(def.returndef)+'",'+
                     tostr(N_tsym)+',0,0,'+tostr(tabstractnormalvarsym(def.funcretsym).localloc.reference.offset))));
              end;
          end;
        // LBRAC
        ss:=tostr(N_LBRAC)+',0,0,';
        if target_info.cpu=cpu_powerpc64 then
          ss:=ss+'.';
        ss:=ss+def.mangledname;
        if (tf_use_function_relative_addresses in target_info.flags) then
          begin
            ss:=ss+'-';
            if target_info.cpu=cpu_powerpc64 then
              ss:=ss+'.';
            ss:=ss+def.mangledname;
          end;
        getmem(p,length(ss)+1);
        move(pchar(ss)^,p^,length(ss)+1);
        templist.concat(Tai_stab.Create(stab_stabn,p));
        // RBRAC
        ss:=tostr(N_RBRAC)+',0,0,'+stabsendlabel.name;
        if (tf_use_function_relative_addresses in target_info.flags) then
          begin
            ss:=ss+'-';
            if target_info.cpu=cpu_powerpc64 then
              ss:=ss+'.';
            ss:=ss+def.mangledname;
          end;
        getmem(p,length(ss)+1);
        move(pchar(ss)^,p^,length(ss)+1);
        templist.concat(Tai_stab.Create(stab_stabn,p));

        { the stabsendlabel must come after all other stabs for this }
        { function                                                   }
        templist.concat(tai_label.create(stabsendlabel));

        { Add a "size" stab as described in the last paragraph of 2.5 at  }
        { http://sourceware.org/gdb/current/onlinedocs/stabs_2.html#SEC12 }
        { This works at least on Darwin (and is needed on Darwin to get   }
        { correct smartlinking of stabs), but I don't know which binutils }
        { version is required on other platforms                          }
        { This stab must come after all other stabs for the procedure,    }
        { including the LBRAC/RBRAC ones                                  }
        if (target_info.system in systems_darwin) then
          templist.concat(Tai_stab.create(stab_stabs,
            strpnew('"",'+tostr(N_FUNCTION)+',0,0,'+stabsendlabel.name+'-'+def.mangledname)));

        current_asmdata.asmlists[al_procedures].insertlistafter(def.procendtai,templist);

        { "The stab representing a procedure is located immediately
          following the code of the procedure. This stab is in turn
          directly followed by a group of other stabs describing
          elements of the procedure. These other stabs describe the
          procedure's parameters, its block local variables, and its
          block structure." (stab docs)                               }
        { this is however incorrect in case "include source" statements }
        { appear in the block, in that case the procedure stab must     }
        { appear before this include stabs (and we generate such an     }
        { stabs for all functions) (JM)                                 }

        { FUNC stabs }
        obj := GetSymName(def.procsym);
        info := '';
        if (po_global in def.procoptions) then
          RType := 'F'
        else
          RType := 'f';
        if assigned(def.owner) then
          begin
            if (def.owner.symtabletype = objecTSymtable) then
              obj := GetSymTableName(def.owner)+'__'+GetSymName(def.procsym);
            if not(cs_gdb_valgrind in current_settings.globalswitches) and
               (def.owner.symtabletype=localsymtable) and
               assigned(def.owner.defowner) and
               assigned(tprocdef(def.owner.defowner).procsym) then
              info := ','+GetSymName(def.procsym)+','+GetSymName(tprocdef(def.owner.defowner).procsym);
          end;
        ss:='"'+ansistring(obj)+':'+RType+def_stab_number(def.returndef)+info+'",'+tostr(n_function)+',0,'+tostr(def.fileinfo.line)+','+ansistring(def.mangledname);
        getmem(p,length(ss)+1);
        move(pchar(ss)^,p^,length(ss)+1);
        templist.concat(Tai_stab.Create(stab_stabs,p));

        current_asmdata.asmlists[al_procedures].insertlistbefore(def.procstarttai,templist);

        { para types }
        if assigned(def.parast) then
          write_symtable_syms(templist,def.parast);
        { local type defs and vars should not be written
          inside the main proc stab }
        if assigned(def.localst) and
           (def.localst.symtabletype=localsymtable) then
          write_symtable_syms(templist,def.localst);

        current_asmdata.asmlists[al_procedures].insertlistbefore(def.procstarttai,templist);

        templist.free;
      end;


{****************************************************************************
                               TSym support
****************************************************************************}

    function TDebugInfoStabs.sym_var_value(const s:string;arg:pointer):string;
      var
        sym : tsym absolute arg;
      begin
        result:='';
        if s='name' then
          result:=GetSymName(sym)
        else if s='mangledname' then
          result:=sym.mangledname
        else if s='ownername' then
          result:=GetSymTableName(sym.owner)
        else if s='line' then
          result:=tostr(sym.fileinfo.line)
        else if s='N_LSYM' then
          result:=tostr(N_LSYM)
        else if s='N_LCSYM' then
          result:=tostr(N_LCSYM)
        else if s='N_RSYM' then
          result:=tostr(N_RSYM)
        else if s='N_TSYM' then
          result:=tostr(N_TSYM)
        else if s='N_STSYM' then
          result:=tostr(N_STSYM)
        else if s='N_FUNCTION' then
          result:=tostr(N_FUNCTION)
        else
          internalerror(200401152);
      end;


    function TDebugInfoStabs.sym_stabstr_evaluate(sym:tsym;const s:string;const vars:array of string):ansistring;
      begin
        result:=string_evaluate(s,@sym_var_value,sym,vars);
      end;


    procedure TDebugInfoStabs.write_sym_stabstr(list:TAsmList;sym:tsym;const ss:ansistring);
      var
        p : pchar;
      begin
        if ss='' then
          exit;
        { add to list }
        getmem(p,length(ss)+1);
        move(pchar(ss)^,p^,length(ss)+1);
        list.concat(Tai_stab.create(stab_stabs,p));
      end;


    procedure TDebugInfoStabs.appendsym_fieldvar(list:TAsmList;sym:tfieldvarsym);
      var
        ss : ansistring;
      begin
        ss:='';
        if (sym.owner.symtabletype=objecTSymtable) and
           (sp_static in sym.symoptions) then
          ss:=sym_stabstr_evaluate(sym,'"${ownername}__${name}:S$1",${N_LCSYM},0,${line},${mangledname}',
              [def_stab_number(sym.vardef)]);
        write_sym_stabstr(list,sym,ss);
      end;


    procedure TDebugInfoStabs.appendsym_staticvar(list:TAsmList;sym:tstaticvarsym);
      var
        ss : ansistring;
        st : string;
        threadvaroffset : string;
        regidx : Tregisterindex;
        nsym : string[7];
      begin
        { external symbols can't be resolved at link time, so we
          can't generate stabs for them }
        if vo_is_external in sym.varoptions then
          exit;
        ss:='';
        st:=def_stab_number(sym.vardef);
        case sym.localloc.loc of
          LOC_REGISTER,
          LOC_CREGISTER,
          LOC_MMREGISTER,
          LOC_CMMREGISTER,
          LOC_FPUREGISTER,
          LOC_CFPUREGISTER :
            begin
              regidx:=findreg_by_number(sym.localloc.register);
              { "eax", "ecx", "edx", "ebx", "esp", "ebp", "esi", "edi", "eip", "ps", "cs", "ss", "ds", "es", "fs", "gs", }
              { this is the register order for GDB}
              if regidx<>0 then
                ss:=sym_stabstr_evaluate(sym,'"${name}:r$1",${N_RSYM},0,${line},$2',[st,tostr(regstabs_table[regidx])]);
            end;
          else
            begin
              if (vo_is_thread_var in sym.varoptions) then
                threadvaroffset:='+'+tostr(sizeof(pint))
              else
                threadvaroffset:='';
              if (vo_is_typed_const in sym.varoptions) then
                nsym:='N_STSYM'
              else
                nsym:='N_LCSYM';
              { Here we used S instead of
                because with G GDB doesn't look at the address field
                but searches the same name or with a leading underscore
                but these names don't exist in pascal !}
              st:='S'+st;
              ss:=sym_stabstr_evaluate(sym,'"${name}:$1",${'+nsym+'},0,${line},${mangledname}$2',[st,threadvaroffset]);
            end;
        end;
        write_sym_stabstr(list,sym,ss);
      end;


    procedure TDebugInfoStabs.appendsym_localvar(list:TAsmList;sym:tlocalvarsym);
      var
        ss : ansistring;
        st : string;
        regidx : Tregisterindex;
      begin
        { There is no space allocated for not referenced locals }
        if (sym.owner.symtabletype=localsymtable) and (sym.refs=0) then
          exit;

        ss:='';
        st:=def_stab_number(sym.vardef);
        case sym.localloc.loc of
          LOC_REGISTER,
          LOC_CREGISTER,
          LOC_MMREGISTER,
          LOC_CMMREGISTER,
          LOC_FPUREGISTER,
          LOC_CFPUREGISTER :
            begin
              regidx:=findreg_by_number(sym.localloc.register);
              { "eax", "ecx", "edx", "ebx", "esp", "ebp", "esi", "edi", "eip", "ps", "cs", "ss", "ds", "es", "fs", "gs", }
              { this is the register order for GDB}
              if regidx<>0 then
                ss:=sym_stabstr_evaluate(sym,'"${name}:r$1",${N_RSYM},0,${line},$2',[st,tostr(regstabs_table[regidx])]);
            end;
          LOC_REFERENCE :
            { offset to ebp => will not work if the framepointer is esp
              so some optimizing will make things harder to debug }
            ss:=sym_stabstr_evaluate(sym,'"${name}:$1",${N_TSYM},0,${line},$2',[st,tostr(sym.localloc.reference.offset)])
          else
            internalerror(2003091814);
        end;
        write_sym_stabstr(list,sym,ss);
      end;


    function TDebugInfoStabs.get_appendsym_paravar_reg(sym:tparavarsym;const typ,stabstr:string;reg: tregister): ansistring;
      var
        ltyp: string[1];
        regidx : Tregisterindex;
      begin
        result:='';
        if typ='p' then
          ltyp:='R'
        else
          ltyp:='a';
        regidx:=findreg_by_number(reg);
        { "eax", "ecx", "edx", "ebx", "esp", "ebp", "esi", "edi", "eip", "ps", "cs", "ss", "ds", "es", "fs", "gs", }
        { this is the register order for GDB}
        if regidx<>0 then
          result:=sym_stabstr_evaluate(sym,'"${name}:$1",${N_RSYM},0,${line},$2',[ltyp+stabstr,tostr(longint(regstabs_table[regidx]))]);
      end;


    procedure TDebugInfoStabs.appendsym_paravar(list:TAsmList;sym:tparavarsym);
      var
        ss : ansistring;
        c  : string[1];
        st : string;
        regidx : Tregisterindex;
      begin
        ss:='';
        { set loc to LOC_REFERENCE to get somewhat usable debugging info for -Or }
        { while stabs aren't adapted for regvars yet                             }
        if (vo_is_self in sym.varoptions) then
          begin
            case sym.localloc.loc of
              LOC_REGISTER,
              LOC_CREGISTER:
                regidx:=findreg_by_number(sym.localloc.register);
              LOC_REFERENCE: ;
              else
                internalerror(2003091815);
            end;
            if (po_classmethod in tabstractprocdef(sym.owner.defowner).procoptions) or
               (po_staticmethod in tabstractprocdef(sym.owner.defowner).procoptions) then
              begin
                if (sym.localloc.loc=LOC_REFERENCE) then
                  ss:=sym_stabstr_evaluate(sym,'"pvmt:p$1",${N_TSYM},0,0,$2',
                    [def_stab_number(pvmttype),tostr(sym.localloc.reference.offset)])
                else
                  begin
                    regidx:=findreg_by_number(sym.localloc.register);
                    ss:=sym_stabstr_evaluate(sym,'"pvmt:r$1",${N_RSYM},0,0,$2',
                      [def_stab_number(pvmttype),tostr(regstabs_table[regidx])]);
                  end
                end
            else
              begin
                if not(is_class(tprocdef(sym.owner.defowner)._class)) then
                  c:='v'
                else
                  c:='p';
                if (sym.localloc.loc=LOC_REFERENCE) then
                  ss:=sym_stabstr_evaluate(sym,'"$$t:$1",${N_TSYM},0,0,$2',
                        [c+def_stab_number(tprocdef(sym.owner.defowner)._class),tostr(sym.localloc.reference.offset)])
                else
                  begin
                    regidx:=findreg_by_number(sym.localloc.register);
                    ss:=sym_stabstr_evaluate(sym,'"$$t:r$1",${N_RSYM},0,0,$2',
                        [c+def_stab_number(tprocdef(sym.owner.defowner)._class),tostr(regstabs_table[regidx])]);
                  end
              end;
          end
        else
          begin
            st:=def_stab_number(sym.vardef);

            if paramanager.push_addr_param(sym.varspez,sym.vardef,tprocdef(sym.owner.defowner).proccalloption) and
               not(vo_has_local_copy in sym.varoptions) and
               not is_open_string(sym.vardef) then
              c:='v' { should be 'i' but 'i' doesn't work }
            else
              c:='p';
            case sym.localloc.loc of
              LOC_REGISTER,
              LOC_CREGISTER,
              LOC_MMREGISTER,
              LOC_CMMREGISTER,
              LOC_FPUREGISTER,
              LOC_CFPUREGISTER :
                begin
                  ss:=get_appendsym_paravar_reg(sym,c,st,sym.localloc.register);
                end;
              LOC_REFERENCE :
                begin
                  { When the *value* of a parameter (so not its address!) is
                    copied into a local variable, you have to generate two
                    stabs: one for the parmeter, and one for the local copy.
                    Not doing this breaks debugging under e.g. SPARC. Doc:
                    http://sourceware.org/gdb/current/onlinedocs/stabs_4.html#SEC26
                  }
                  if (c='p') and
                     not is_open_string(sym.vardef) and
                     ((sym.paraloc[calleeside].location^.loc<>sym.localloc.loc) or
                      ((sym.localloc.loc in [LOC_REFERENCE,LOC_CREFERENCE]) and
                       ((sym.paraloc[calleeside].location^.reference.index<>sym.localloc.reference.base) or
                        (sym.paraloc[calleeside].location^.reference.offset<>sym.localloc.reference.offset))) or
                      ((sym.localloc.loc in [LOC_REGISTER,LOC_CREGISTER,LOC_MMREGISTER,LOC_CMMREGISTER,LOC_FPUREGISTER,LOC_CFPUREGISTER]) and
                       (sym.localloc.register<>sym.paraloc[calleeside].location^.register))) then
                    begin
                      if not(sym.paraloc[calleeside].location^.loc in [LOC_REFERENCE,LOC_CREFERENCE]) then
                        ss:=get_appendsym_paravar_reg(sym,c,st,sym.paraloc[calleeside].location^.register)
                      else
                        ss:=sym_stabstr_evaluate(sym,'"${name}:$1",${N_TSYM},0,${line},$2',[c+st,tostr(sym.paraloc[calleeside].location^.reference.offset)]);
                      write_sym_stabstr(list,sym,ss);
                      { second stab has no parameter specifier }
                      c:='';
                    end;
                  { offset to ebp => will not work if the framepointer is esp
                    so some optimizing will make things harder to debug }
                  ss:=sym_stabstr_evaluate(sym,'"${name}:$1",${N_TSYM},0,${line},$2',[c+st,tostr(sym.localloc.reference.offset)])
                end;
              else
                internalerror(2003091814);
            end;
          end;
        write_sym_stabstr(list,sym,ss);
      end;


    procedure TDebugInfoStabs.appendsym_const(list:TAsmList;sym:tconstsym);
      var
        st : string;
        ss : ansistring;
      begin
        ss:='';
        { Don't write info for default parameter values, the N_Func breaks
          the N_Func for the function itself.
          Valgrind does not support constants }
        if (sym.owner.symtabletype=parasymtable) or
           (cs_gdb_valgrind in current_settings.globalswitches) then
          exit;
        case sym.consttyp of
          conststring:
            begin
              if sym.value.len<200 then
                st:='s'''+backspace_quote(octal_quote(strpas(pchar(sym.value.valueptr)),[#0..#9,#11,#12,#14..#31,'''']),['"','\',#10,#13])+''''
              else
                st:='<constant string too long>';
            end;
          constord:
            st:='i'+tostr(sym.value.valueord);
          constpointer:
            st:='i'+tostr(sym.value.valueordptr);
          constreal:
            begin
              system.str(pbestreal(sym.value.valueptr)^,st);
              st := 'r'+st;
            end;
          else
            begin
              { if we don't know just put zero !! }
              st:='i0';
            end;
        end;
        ss:=sym_stabstr_evaluate(sym,'"${name}:c=$1;",${N_FUNCTION},0,${line},0',[st]);
        write_sym_stabstr(list,sym,ss);
      end;


    procedure TDebugInfoStabs.appendsym_type(list:TAsmList;sym:ttypesym);
      var
        ss : ansistring;
        stabchar : string[2];
      begin
        ss:='';
        if not assigned(sym.typedef) then
          internalerror(200509262);
        if sym.typedef.typ in tagtypes then
          stabchar:='Tt'
        else
          stabchar:='t';
        ss:=sym_stabstr_evaluate(sym,'"${name}:$1$2",${N_LSYM},0,${line},0',[stabchar,def_stab_number(sym.typedef)]);
        write_sym_stabstr(list,sym,ss);
      end;


    procedure TDebugInfoStabs.appendsym_label(list:TAsmList;sym:tlabelsym);
      var
        ss : ansistring;
      begin
        ss:=sym_stabstr_evaluate(sym,'"${name}",${N_LSYM},0,${line},0',[]);
        write_sym_stabstr(list,sym,ss);
      end;


{****************************************************************************
                             Proc/Module support
****************************************************************************}

    procedure tdebuginfostabs.inserttypeinfo;
      var
        stabsvarlist,
        stabstypelist : TAsmList;
        storefilepos  : tfileposinfo;
        i  : longint;
      begin
        storefilepos:=current_filepos;
        current_filepos:=current_module.mainfilepos;

        global_stab_number:=0;
        defnumberlist:=TFPObjectlist.create(false);
        stabsvarlist:=TAsmList.create;
        stabstypelist:=TAsmList.create;

        { include symbol that will be referenced from the main to be sure to
          include this debuginfo .o file }
        current_module.flags:=current_module.flags or uf_has_stabs_debuginfo;
        if not(target_info.system in systems_darwin) then
          begin
            new_section(current_asmdata.asmlists[al_stabs],sec_data,GetSymTableName(current_module.localsymtable),0);
            current_asmdata.asmlists[al_stabs].concat(tai_symbol.Createname_global(make_mangledname('DEBUGINFO',current_module.localsymtable,''),AT_DATA,0));
          end
        else
          new_section(current_asmdata.asmlists[al_stabs],sec_code,GetSymTableName(current_module.localsymtable),0);

        { write all global/local variables. This will flag all required tdefs  }
        if assigned(current_module.globalsymtable) then
          write_symtable_syms(stabsvarlist,current_module.globalsymtable);
        if assigned(current_module.localsymtable) then
          write_symtable_syms(stabsvarlist,current_module.localsymtable);

        { write all procedures and methods. This will flag all required tdefs }
        if assigned(current_module.globalsymtable) then
          write_symtable_procdefs(stabsvarlist,current_module.globalsymtable);
        if assigned(current_module.localsymtable) then
          write_symtable_procdefs(stabsvarlist,current_module.localsymtable);

        { reset unit type info flag }
        reset_unit_type_info;

        { write used types from the used units }
        write_used_unit_type_info(stabstypelist,current_module);
        { last write the types from this unit }
        if assigned(current_module.globalsymtable) then
          write_symtable_defs(stabstypelist,current_module.globalsymtable);
        if assigned(current_module.localsymtable) then
          write_symtable_defs(stabstypelist,current_module.localsymtable);

        current_asmdata.asmlists[al_stabs].concatlist(stabstypelist);
        current_asmdata.asmlists[al_stabs].concatlist(stabsvarlist);

        { reset stab numbers }
        for i:=0 to defnumberlist.count-1 do
          begin
            if assigned(defnumberlist[i]) then
              begin
                tdef(defnumberlist[i]).stab_number:=0;
                tdef(defnumberlist[i]).dbg_state:=dbg_state_unused;
              end;
          end;

        defnumberlist.free;
        defnumberlist:=nil;

        stabsvarlist.free;
        stabstypelist.free;
        current_filepos:=storefilepos;
      end;


    procedure tdebuginfostabs.insertlineinfo(list:TAsmList);
      var
        currfileinfo,
        lastfileinfo : tfileposinfo;
        currfuncname : pshortstring;
        currsectype  : TAsmSectiontype;
        hlabel       : tasmlabel;
        hp : tai;
        infile : tinputfile;
      begin
        FillChar(lastfileinfo,sizeof(lastfileinfo),0);
        currfuncname:=nil;
        currsectype:=sec_code;
        hp:=Tai(list.first);
        while assigned(hp) do
          begin
            case hp.typ of
              ait_section :
                currsectype:=tai_section(hp).sectype;
              ait_function_name :
                currfuncname:=tai_function_name(hp).funcname;
              ait_force_line :
                lastfileinfo.line:=-1;
            end;

            if (currsectype=sec_code) and
               (hp.typ=ait_instruction) then
              begin
                currfileinfo:=tailineinfo(hp).fileinfo;
                { file changed ? (must be before line info) }
                if (currfileinfo.fileindex<>0) and
                   ((lastfileinfo.fileindex<>currfileinfo.fileindex) or
                    (lastfileinfo.moduleindex<>currfileinfo.moduleindex)) then
                  begin
                    infile:=get_module(currfileinfo.moduleindex).sourcefiles.get_file(currfileinfo.fileindex);
                    if assigned(infile) then
                      begin
                        current_asmdata.getlabel(hlabel,alt_dbgfile);
                        { emit stabs }
                        if not(ds_stabs_abs_include_files in current_settings.debugswitches) or
                           path_absolute(infile.path^) then
                          list.insertbefore(Tai_stab.Create_str(stab_stabs,'"'+BsToSlash(FixPath(infile.path^,false))+FixFileName(infile.name^)+'",'+tostr(n_includefile)+
                                            ',0,0,'+hlabel.name),hp)
                        else
                          list.insertbefore(Tai_stab.Create_str(stab_stabs,'"'+BsToSlash(FixPath(getcurrentdir,false)+FixPath(infile.path^,false))+FixFileName(infile.name^)+'",'+tostr(n_includefile)+
                                            ',0,0,'+hlabel.name),hp);
                        list.insertbefore(tai_label.create(hlabel),hp);
                        { force new line info }
                        lastfileinfo.line:=-1;
                      end;
                  end;

                { line changed ? }
                if (currfileinfo.line>lastfileinfo.line) and (currfileinfo.line<>0) then
                  begin
                     if assigned(currfuncname) and
                        (tf_use_function_relative_addresses in target_info.flags) then
                      begin
                        current_asmdata.getlabel(hlabel,alt_dbgline);
                        list.insertbefore(Tai_stab.Create_str(stab_stabn,tostr(n_textline)+',0,'+tostr(currfileinfo.line)+','+
                                          hlabel.name+' - '+{$IFDEF POWERPC64}'.'+{$ENDIF POWERPC64}currfuncname^),hp);
                        list.insertbefore(tai_label.create(hlabel),hp);
                      end
                     else
                      list.insertbefore(Tai_stab.Create_str(stab_stabd,tostr(n_textline)+',0,'+tostr(currfileinfo.line)),hp);
                  end;
                lastfileinfo:=currfileinfo;
              end;

            hp:=tai(hp.next);
          end;
      end;


    procedure tdebuginfostabs.insertmoduleinfo;
      var
        hlabel : tasmlabel;
        infile : tinputfile;
      begin
        { emit main source n_sourcefile for start of module }
        current_asmdata.getlabel(hlabel,alt_dbgfile);
        infile:=current_module.sourcefiles.get_file(1);
        new_section(current_asmdata.asmlists[al_start],sec_code,make_mangledname('DEBUGSTART',current_module.localsymtable,''),0,secorder_begin);
        if not(target_info.system in systems_darwin) then
          current_asmdata.asmlists[al_start].concat(tai_symbol.Createname_global(make_mangledname('DEBUGSTART',current_module.localsymtable,''),AT_DATA,0));
        current_asmdata.asmlists[al_start].concat(Tai_stab.Create_str(stab_stabs,'"'+BsToSlash(FixPath(getcurrentdir,false))+'",'+tostr(n_sourcefile)+
                      ',0,0,'+hlabel.name));
        current_asmdata.asmlists[al_start].concat(Tai_stab.Create_str(stab_stabs,'"'+BsToSlash(FixPath(infile.path^,false))+FixFileName(infile.name^)+'",'+tostr(n_sourcefile)+
                    ',0,0,'+hlabel.name));
        current_asmdata.asmlists[al_start].concat(tai_label.create(hlabel));
        { for darwin, you need a "module marker" too to work around      }
        { either some assembler or gdb bug (radar 4386531 according to a }
        { comment in dbxout.c of Apple's gcc)                            }
        if (target_info.system in systems_darwin) then
          current_asmdata.asmlists[al_end].concat(Tai_stab.Create_str(stab_stabs,'"",'+tostr(N_OSO)+',0,0,0'));
        { emit empty n_sourcefile for end of module }
        current_asmdata.getlabel(hlabel,alt_dbgfile);
        new_section(current_asmdata.asmlists[al_end],sec_code,make_mangledname('DEBUGEND',current_module.localsymtable,''),0,secorder_end);
        if not(target_info.system in systems_darwin) then
          current_asmdata.asmlists[al_end].concat(tai_symbol.Createname_global(make_mangledname('DEBUGEND',current_module.localsymtable,''),AT_DATA,0));
        current_asmdata.asmlists[al_end].concat(Tai_stab.Create_str(stab_stabs,'"",'+tostr(n_sourcefile)+',0,0,'+hlabel.name));
        current_asmdata.asmlists[al_end].concat(tai_label.create(hlabel));
      end;


    procedure tdebuginfostabs.referencesections(list:TAsmList);
      var
        hp : tmodule;
        dbgtable : tai_symbol;
      begin
        { Reference all DEBUGINFO sections from the main .fpc section }
        if (target_info.system in ([system_powerpc_macos]+systems_darwin)) then
          exit;
        list.concat(Tai_section.create(sec_fpc,'links',0));
        { make sure the debuginfo doesn't get stripped out }
        if (target_info.system in systems_darwin) then
          begin
            dbgtable:=tai_symbol.createname('DEBUGINFOTABLE',AT_DATA,0);
            list.concat(tai_directive.create(asd_no_dead_strip,dbgtable.sym.name));
            list.concat(dbgtable);
          end;
        { include reference to all debuginfo sections of used units }
        hp:=tmodule(loaded_units.first);
        while assigned(hp) do
          begin
            If (hp.flags and uf_has_stabs_debuginfo)=uf_has_stabs_debuginfo then
              begin
                list.concat(Tai_const.Createname(make_mangledname('DEBUGINFO',hp.localsymtable,''),0));
                list.concat(Tai_const.Createname(make_mangledname('DEBUGSTART',hp.localsymtable,''),0));
                list.concat(Tai_const.Createname(make_mangledname('DEBUGEND',hp.localsymtable,''),0));
              end;
            hp:=tmodule(hp.next);
          end;
      end;


    const
      dbg_stabs_info : tdbginfo =
         (
           id     : dbg_stabs;
           idtxt  : 'STABS';
         );

initialization
  RegisterDebugInfo(dbg_stabs_info,TDebugInfoStabs);
end.
