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
      dbgbase,
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
        function  sym_stabstr_evaluate(sym:tsym;const s:string;const vars:array of string):Pchar;
        procedure write_symtable_syms(list:TAsmList;st:TSymtable);
        { tdef writing }
        function  def_stab_number(def:tdef):string;
        function  def_stab_classnumber(def:tobjectdef):string;
        function  def_var_value(const s:string;arg:pointer):string;
        function  def_stabstr_evaluate(def:tdef;const s:string;const vars:array of string):Pchar;
        procedure field_add_stabstr(p:TObject;arg:pointer);
        procedure method_add_stabstr(p:TObject;arg:pointer);
        function  def_stabstr(def:tdef):pchar;
        procedure write_def_stabstr(list:TAsmList;def:tdef);
        procedure write_procdef(list:TAsmList;pd:tprocdef);
        procedure insertsym(list:TAsmList;sym:tsym);
      public
        procedure inserttypeinfo;override;
        procedure insertmoduleinfo;override;
        procedure insertlineinfo(list:TAsmList);override;
        procedure referencesections(list:TAsmList);override;
        procedure insertdef(list:TAsmList;def:tdef);override;
        procedure write_symtable_defs(list:TAsmList;st:TSymtable);override;
      end;


implementation

    uses
      SysUtils,cutils,cfileutl,
      systems,globals,globtype,verbose,constexp,
      symconst,defutil,
      cpuinfo,cpubase,cgbase,paramgr,
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

       Trecord_stabgen_state=record
          stabstring:Pchar;
          stabsize,staballoc,recoffset:integer;
       end;
       Precord_stabgen_state=^Trecord_stabgen_state;


    function string_evaluate(s:string;get_var_value:get_var_value_proc;
                             get_var_value_arg:pointer;
                             const vars:array of string):Pchar;

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
      getmem(r,len+1);
      string_evaluate:=r;
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
      r^:=#0;
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


    function TDebugInfoStabs.def_stabstr_evaluate(def:tdef;const s:string;const vars:array of string):Pchar;
      begin
        result:=string_evaluate(s,@def_var_value,def,vars);
      end;


    procedure TDebugInfoStabs.field_add_stabstr(p:TObject;arg:pointer);
      var
        newrec  : Pchar;
        spec    : string[3];
        varsize : aint;
        state   : Precord_stabgen_state;
      begin
        state:=arg;
        { static variables from objects are like global objects }
        if (Tsym(p).typ=fieldvarsym) and
           not(sp_static in Tsym(p).symoptions) then
          begin
            if ([sp_protected,sp_strictprotected]*tsym(p).symoptions)<>[] then
              spec:='/1'
            else if ([sp_private,sp_strictprivate]*tsym(p).symoptions)<>[] then
              spec:='/0'
            else
              spec:='';
            if (tabstractrecordsymtable(tsym(p).owner).usefieldalignment<>bit_alignment) then
              begin
                varsize:=tfieldvarsym(p).vardef.size;
                { open arrays made overflows !! }
                { how can a record/object/class contain an open array? (JM) }
                if varsize>$fffffff then
                  varsize:=$fffffff;
                newrec:=def_stabstr_evaluate(nil,'$1:$2,$3,$4;',[GetSymName(tfieldvarsym(p)),
                                     spec+def_stab_number(tfieldvarsym(p).vardef),
                                     tostr(TConstExprInt(tfieldvarsym(p).fieldoffset)*8),tostr(varsize*8)])
              end
            else
              newrec:=def_stabstr_evaluate(nil,'$1:$2,$3,$4;',[GetSymName(tfieldvarsym(p)),
                                   spec+def_stab_number(tfieldvarsym(p).vardef),
                                   tostr(TConstExprInt(tfieldvarsym(p).fieldoffset)),tostr(tfieldvarsym(p).vardef.packedbitsize)]);
            if state^.stabsize+strlen(newrec)>=state^.staballoc-256 then
              begin
                inc(state^.staballoc,strlen(newrec)+64);
                reallocmem(state^.stabstring,state^.staballoc);
              end;
            strcopy(state^.stabstring+state^.stabsize,newrec);
            inc(state^.stabsize,strlen(newrec));
            freemem(newrec);
            {This should be used for case !!}
            if int64(state^.recoffset)+Tfieldvarsym(p).vardef.size>high(longint) then
              state^.recoffset:=high(longint)
            else
              inc(state^.recoffset,Tfieldvarsym(p).vardef.size);
          end;
      end;


    procedure TDebugInfoStabs.method_add_stabstr(p:TObject;arg:pointer);
      var virtualind,argnames : string;
          newrec : pchar;
          pd     : tprocdef;
          lindex : longint;
          arglength : byte;
          sp : char;
          state:^Trecord_stabgen_state;
          olds:integer;
          i : integer;
          parasym : tparavarsym;
      begin
        state:=arg;
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
           if ([sp_private,sp_strictprivate]*tsym(p).symoptions)<>[] then
             sp:='0'
           else if ([sp_protected,sp_strictprotected]*tsym(p).symoptions)<>[] then
             sp:='1'
           else
             sp:='2';
           newrec:=def_stabstr_evaluate(nil,'$1::$2=##$3;:$4;$5A$6;',[GetSymName(tsym(p)),def_stab_number(pd),
                                    def_stab_number(pd.returndef),argnames,sp,
                                    virtualind]);
           { get spare place for a string at the end }
           olds:=state^.stabsize;
           inc(state^.stabsize,strlen(newrec));
           if state^.stabsize>=state^.staballoc-256 then
             begin
                inc(state^.staballoc,strlen(newrec)+64);
                reallocmem(state^.stabstring,state^.staballoc);
             end;
           strcopy(state^.stabstring+olds,newrec);
           freemem(newrec);
           {This should be used for case !!
           RecOffset := RecOffset + pd.size;}
         end;
      end;


    function TDebugInfoStabs.def_stabstr(def:tdef):pchar;

        function stringdef_stabstr(def:tstringdef):pchar;
          var
            slen : aint;
            bytest,charst,longst : string;
          begin
            case def.stringtype of
              st_shortstring:
                begin
                  { fix length of openshortstring }
                  slen:=def.len;
                  if slen=0 then
                    slen:=255;
                  charst:=def_stab_number(cchartype);
                  bytest:=def_stab_number(u8inttype);
                  result:=def_stabstr_evaluate(def,'s$1length:$2,0,8;st:ar$2;1;$3;$4,8,$5;;',
                              [tostr(slen+1),bytest,tostr(slen),charst,tostr(slen*8)]);
                end;
              st_longstring:
                begin
                  charst:=def_stab_number(cchartype);
                  bytest:=def_stab_number(u8inttype);
                  longst:=def_stab_number(u32inttype);
                  result:=def_stabstr_evaluate(def,'s$1length:$2,0,32;dummy:$6,32,8;st:ar$2;1;$3;$4,40,$5;;',
                              [tostr(def.len+5),longst,tostr(def.len),charst,tostr(def.len*8),bytest]);
               end;
             st_ansistring:
               begin
                 { looks like a pchar }
                 charst:=def_stab_number(cchartype);
                 result:=strpnew('*'+charst);
               end;
             st_unicodestring,
             st_widestring:
               begin
                 { looks like a pwidechar }
                 charst:=def_stab_number(cwidechartype);
                 result:=strpnew('*'+charst);
               end;
            end;
          end;

        function enumdef_stabstr(def:tenumdef):pchar;
          var
            st : Pchar;
            p : Tenumsym;
            s : string;
            memsize,
            stl : aint;
          begin
            memsize:=memsizeinc;
            getmem(st,memsize);
            { we can specify the size with @s<size>; prefix PM }
            if def.size <> std_param_align then
              strpcopy(st,'@s'+tostr(def.size*8)+';e')
            else
              strpcopy(st,'e');
            p := tenumsym(def.firstenum);
            stl:=strlen(st);
            while assigned(p) do
              begin
                s :=GetSymName(p)+':'+tostr(p.value)+',';
                { place for the ending ';' also }
                if (stl+length(s)+1>=memsize) then
                  begin
                    inc(memsize,memsizeinc);
                    reallocmem(st,memsize);
                  end;
                strpcopy(st+stl,s);
                inc(stl,length(s));
                p:=p.nextenum;
              end;
            st[stl]:=';';
            st[stl+1]:=#0;
            reallocmem(st,stl+2);
            result:=st;
          end;

        function orddef_stabstr(def:torddef):pchar;
          begin
            if cs_gdb_valgrind in current_settings.globalswitches then
              begin
                case def.ordtype of
                  uvoid :
                    result:=strpnew(def_stab_number(def));
                  bool8bit,
                  bool16bit,
                  bool32bit,
                  bool64bit :
                    result:=def_stabstr_evaluate(def,'r${numberstring};0;255;',[]);
                  u32bit,
                  s64bit,
                  u64bit :
                    result:=def_stabstr_evaluate(def,'r${numberstring};0;-1;',[]);
                  else
                    result:=def_stabstr_evaluate(def,'r${numberstring};$1;$2;',[tostr(longint(def.low.svalue)),tostr(longint(def.high.svalue))]);
                end;
              end
            else
              begin
                case def.ordtype of
                  uvoid :
                    result:=strpnew(def_stab_number(def));
                  uchar :
                    result:=strpnew('-20;');
                  uwidechar :
                    result:=strpnew('-30;');
                  bool8bit :
                    result:=strpnew('-21;');
                  bool16bit :
                    result:=strpnew('-22;');
                  bool32bit :
                    result:=strpnew('-23;');
                  bool64bit :
                    { no clue if this is correct (FK) }
                    result:=strpnew('-23;');
                  u64bit :
                    result:=strpnew('-32;');
                  s64bit :
                    result:=strpnew('-31;');
                  {u32bit : result:=def_stab_number(s32inttype)+';0;-1;'); }
                  else
                    result:=def_stabstr_evaluate(def,'r${numberstring};$1;$2;',[tostr(longint(def.low.svalue)),tostr(longint(def.high.svalue))]);
                end;
             end;
          end;

        function floatdef_stabstr(def:tfloatdef):Pchar;
          begin
            case def.floattype of
              s32real,
              s64real,
              s80real:
                result:=def_stabstr_evaluate(def,'r$1;${savesize};0;',[def_stab_number(s32inttype)]);
              s64currency,
              s64comp:
                result:=def_stabstr_evaluate(def,'r$1;-${savesize};0;',[def_stab_number(s32inttype)]);
              else
                internalerror(200509261);
            end;
          end;

        function filedef_stabstr(def:tfiledef):pchar;
          begin
{$ifdef cpu64bit}
            result:=def_stabstr_evaluate(def,'s${savesize}HANDLE:$1,0,32;MODE:$1,32,32;RECSIZE:$2,64,64;'+
                                     '_PRIVATE:ar$1;1;64;$3,128,256;USERDATA:ar$1;1;16;$3,384,128;'+
                                     'NAME:ar$1;0;255;$4,512,2048;;',[def_stab_number(s32inttype),
                                     def_stab_number(s64inttype),
                                     def_stab_number(u8inttype),
                                     def_stab_number(cchartype)]);
{$else cpu64bit}
            result:=def_stabstr_evaluate(def,'s${savesize}HANDLE:$1,0,32;MODE:$1,32,32;RECSIZE:$1,64,32;'+
                                     '_PRIVATE:ar$1;1;32;$3,96,256;USERDATA:ar$1;1;16;$2,352,128;'+
                                     'NAME:ar$1;0;255;$3,480,2048;;',[def_stab_number(s32inttype),
                                     def_stab_number(u8inttype),
                                     def_stab_number(cchartype)]);
{$endif cpu64bit}
          end;

        function procdef_stabstr(def:tprocdef):pchar;
          Var
            RType : Char;
            Obj,Info : String;
            stabsstr : string;
            p : pchar;
          begin
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
            stabsstr:=def.mangledname;
            getmem(p,length(stabsstr)+255);
            strpcopy(p,'"'+obj+':'+RType
                  +def_stab_number(def.returndef)+info+'",'+tostr(n_function)
                  +',0,'+
                  tostr(def.fileinfo.line)
                  +',');
            strpcopy(strend(p),stabsstr);
            getmem(result,strlen(p)+1);
            move(p^,result^,strlen(p)+1);
            freemem(p);
          end;

        function recorddef_stabstr(def:trecorddef):pchar;
          var
            state : Trecord_stabgen_state;
          begin
            getmem(state.stabstring,memsizeinc);
            state.staballoc:=memsizeinc;
            strpcopy(state.stabstring,'s'+tostr(def.size));
            state.recoffset:=0;
            state.stabsize:=strlen(state.stabstring);
            def.symtable.SymList.ForEachCall(@field_add_stabstr,@state);
            state.stabstring[state.stabsize]:=';';
            state.stabstring[state.stabsize+1]:=#0;
            reallocmem(state.stabstring,state.stabsize+2);
            result:=state.stabstring;
          end;

        function objectdef_stabstr(def:tobjectdef):pchar;
          var
            anc    : tobjectdef;
            state  :Trecord_stabgen_state;
            ts     : string;
          begin
            { Write the invisible pointer for the class? }
            if (def.objecttype=odt_class) and
               (not def.writing_class_record_dbginfo) then
              begin
                result:=strpnew('*'+def_stab_classnumber(def));
                exit;
              end;

            state.staballoc:=memsizeinc;
            getmem(state.stabstring,state.staballoc);
            strpcopy(state.stabstring,'s'+tostr(tobjecTSymtable(def.symtable).datasize));
            if assigned(def.childof) then
              begin
                {only one ancestor not virtual, public, at base offset 0 }
                {       !1           ,    0       2         0    ,       }
                strpcopy(strend(state.stabstring),'!1,020,'+def_stab_classnumber(def.childof)+';');
              end;
            {virtual table to implement yet}
            state.recoffset:=0;
            state.stabsize:=strlen(state.stabstring);
            def.symtable.symList.ForEachCall(@field_add_stabstr,@state);
            if (oo_has_vmt in def.objectoptions) then
              if not assigned(def.childof) or not(oo_has_vmt in def.childof.objectoptions) then
                 begin
                    ts:='$vf'+def_stab_classnumber(def)+':'+def_stab_number(vmtarraytype)+','+tostr(def.vmt_offset*8)+';';
                    strpcopy(state.stabstring+state.stabsize,ts);
                    inc(state.stabsize,length(ts));
                 end;
            def.symtable.symList.ForEachCall(@method_add_stabstr,@state);
            if (oo_has_vmt in def.objectoptions) then
              begin
                 anc := def;
                 while assigned(anc.childof) and (oo_has_vmt in anc.childof.objectoptions) do
                   anc := anc.childof;
                 { just in case anc = self }
                 ts:=';~%'+def_stab_classnumber(anc)+';';
              end
            else
              ts:=';';
            strpcopy(state.stabstring+state.stabsize,ts);
            inc(state.stabsize,length(ts));
            reallocmem(state.stabstring,state.stabsize+1);
            result:=state.stabstring;
          end;

      var
        tempstr: pchar;

      begin
        result:=nil;
        case def.typ of
          stringdef :
            result:=stringdef_stabstr(tstringdef(def));
          enumdef :
            result:=enumdef_stabstr(tenumdef(def));
          orddef :
            result:=orddef_stabstr(torddef(def));
          floatdef :
            result:=floatdef_stabstr(tfloatdef(def));
          filedef :
            result:=filedef_stabstr(tfiledef(def));
          recorddef :
            result:=recorddef_stabstr(trecorddef(def));
          variantdef :
            result:=def_stabstr_evaluate(def,'${numberstring};',[]);
          pointerdef :
            result:=strpnew('*'+def_stab_number(tpointerdef(def).pointeddef));
          classrefdef :
            result:=strpnew(def_stab_number(pvmttype));
          setdef :
            result:=def_stabstr_evaluate(def,'@s$1;S$2',[tostr(def.size*8),def_stab_number(tsetdef(def).elementdef)]);
          formaldef :
            result:=def_stabstr_evaluate(def,'${numberstring};',[]);
          arraydef :
            if not is_packed_array(def) then
              result:=def_stabstr_evaluate(def,'ar$1;$2;$3;$4',[def_stab_number(tarraydef(def).rangedef),
                 tostr(tarraydef(def).lowrange),tostr(tarraydef(def).highrange),def_stab_number(tarraydef(def).elementdef)])
            else
              begin
// the @P seems to be ignored by gdb
//                result:=def_stabstr_evaluate(def,'ar$1;$2;$3;$4;@P;',[def_stab_number(tarraydef(def).rangedef),tostr(tarraydef(def).lowrange),tostr(tarraydef(def).highrange),def_stab_number(tarraydef(def).elementdef)]);
                tempstr:=def_stabstr_evaluate(tarraydef(def).rangedef,'r${numberstring};$1;$2;',
                  [tostr(tarraydef(def).lowrange),tostr(tarraydef(def).highrange)]);
                // will only show highrange-lowrange+1 bits in gdb
                result:=def_stabstr_evaluate(def,'@s$1;@S;S$2',
                  [tostr(TConstExprInt(tarraydef(def).elepackedbitsize) * tarraydef(def).elecount),tempstr]);
                freemem(tempstr);
              end;
          procdef :
            result:=procdef_stabstr(tprocdef(def));
          procvardef :
            result:=strpnew('*f'+def_stab_number(tprocvardef(def).returndef));
          objectdef :
            result:=objectdef_stabstr(tobjectdef(def));
          undefineddef :
            result:=def_stabstr_evaluate(def,'${numberstring};',[]);
        end;
        if result=nil then
          internalerror(200512203);
      end;


    procedure TDebugInfoStabs.write_def_stabstr(list:TAsmList;def:tdef);
      var
        stabchar : string[2];
        ss,st,su : pchar;
      begin
        { procdefs require a different stabs style without type prefix }
        if def.typ=procdef then
          begin
            st:=def_stabstr(def);
            { add to list }
            list.concat(Tai_stab.create(stab_stabs,st));
          end
        else
          begin
            { type prefix }
            if def.typ in tagtypes then
              stabchar := 'Tt'
            else
              stabchar := 't';
            { Here we maybe generate a type, so we have to use numberstring }
            if is_class(def) and
               tobjectdef(def).writing_class_record_dbginfo then
              st:=def_stabstr_evaluate(def,'"${sym_name}:$1$2=',[stabchar,def_stab_classnumber(tobjectdef(def))])
            else
              st:=def_stabstr_evaluate(def,'"${sym_name}:$1$2=',[stabchar,def_stab_number(def)]);
            ss:=def_stabstr(def);
            reallocmem(st,strlen(ss)+512);
            { line info is set to 0 for all defs, because the def can be in an other
              unit and then the linenumber is invalid in the current sourcefile }
            su:=def_stabstr_evaluate(def,'",${N_LSYM},0,0,0',[]);
            strcopy(strecopy(strend(st),ss),su);
            reallocmem(st,strlen(st)+1);
            freemem(ss);
            freemem(su);
            { add to list }
            list.concat(Tai_stab.create(stab_stabs,st));
          end;
      end;


    procedure TDebugInfoStabs.insertdef(list:TAsmList;def:tdef);
      var
        anc : tobjectdef;
        oldtypesym : tsym;
        i : longint;
      begin
        if (def.dbg_state in [dbg_state_writing,dbg_state_written]) then
          exit;
        { never write generic template defs }
        if df_generic in def.defoptions then
          begin
            def.dbg_state:=dbg_state_written;
            exit;
          end;
        { to avoid infinite loops }
        def.dbg_state := dbg_state_writing;
        { write dependencies first }
        case def.typ of
          stringdef :
            begin
              if tstringdef(def).stringtype in [st_widestring,st_unicodestring] then
                insertdef(list,cwidechartype)
              else
                begin
                  insertdef(list,cchartype);
                  insertdef(list,u8inttype);
                end;
            end;
          floatdef :
            insertdef(list,s32inttype);
          filedef :
            begin
              insertdef(list,s32inttype);
{$ifdef cpu64bit}
              insertdef(list,s64inttype);
{$endif cpu64bit}
              insertdef(list,u8inttype);
              insertdef(list,cchartype);
            end;
          classrefdef :
            insertdef(list,pvmttype);
          pointerdef :
            insertdef(list,tpointerdef(def).pointeddef);
          setdef :
            insertdef(list,tsetdef(def).elementdef);
          procvardef :
            begin
              insertdef(list,tprocvardef(def).returndef);
              if assigned(tprocvardef(def).parast) then
                write_symtable_defs(list,tprocvardef(def).parast);
            end;
          procdef :
            begin
              insertdef(list,tprocdef(def).returndef);
              if assigned(tprocdef(def).parast) then
                write_symtable_defs(list,tprocdef(def).parast);
              if assigned(tprocdef(def).localst) and
                 (tprocdef(def).localst.symtabletype=localsymtable) then
                write_symtable_defs(list,tprocdef(def).localst);
            end;
          arraydef :
            begin
              insertdef(list,tarraydef(def).rangedef);
              insertdef(list,tarraydef(def).elementdef);
            end;
          recorddef :
            trecorddef(def).symtable.symList.ForEachCall(@field_write_defs,list);
          enumdef :
            if assigned(tenumdef(def).basedef) then
              insertdef(list,tenumdef(def).basedef);
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
              insertdef(list,vmtarraytype);
              if assigned(tobjectdef(def).ImplementedInterfaces) then
                for i:=0 to tobjectdef(def).ImplementedInterfaces.Count-1 do
                  insertdef(list,TImplementedInterface(tobjectdef(def).ImplementedInterfaces[i]).IntfDef);
              { first the parents }
              anc:=tobjectdef(def);
              while assigned(anc.childof) do
                begin
                  anc:=anc.childof;
                  insertdef(list,anc);
                  if assigned(anc.ImplementedInterfaces) then
                    for i:=0 to anc.ImplementedInterfaces.Count-1 do
                      insertdef(list,TImplementedInterface(anc.ImplementedInterfaces[i]).IntfDef);
                end;
              tobjectdef(def).symtable.symList.ForEachCall(@field_write_defs,list);
              tobjectdef(def).symtable.symList.ForEachCall(@method_write_defs,list);
            end;
        end;

        case def.typ of
          objectdef :
            begin
              { classes require special code to write the record and the invisible pointer }
              if is_class(def) then
                begin
                  { Write the record class itself }
                  tobjectdef(def).writing_class_record_dbginfo:=true;
                  write_def_stabstr(list,def);
                  tobjectdef(def).writing_class_record_dbginfo:=false;
                  { Write the invisible pointer class }
                  oldtypesym:=def.typesym;
                  def.typesym:=nil;
                  write_def_stabstr(list,def);
                  def.typesym:=oldtypesym;
                end
              else
                write_def_stabstr(list,def);
              { VMT symbol }
              if (oo_has_vmt in tobjectdef(def).objectoptions) and
                 assigned(def.owner) and
                 assigned(def.owner.name) then
                list.concat(Tai_stab.create(stab_stabs,strpnew('"vmt_'+GetSymTableName(def.owner)+tobjectdef(def).objname^+':S'+
                       def_stab_number(vmttype)+'",'+tostr(N_STSYM)+',0,0,'+tobjectdef(def).vmt_mangledname)));
            end;
          procdef :
            begin
              { procdefs are handled separatly }
            end;
          else
            write_def_stabstr(list,def);
        end;

        def.dbg_state := dbg_state_written;
      end;


    procedure TDebugInfoStabs.write_symtable_defs(list:TAsmList;st:TSymtable);

       procedure dowritestabs(list:TAsmList;st:TSymtable);
         var
           def : tdef;
           i   : longint;
         begin
           for i:=0 to st.DefList.Count-1 do
             begin
               def:=tdef(st.DefList[i]);
               if (def.dbg_state in [dbg_state_used,dbg_state_queued]) then
                 insertdef(list,def);
             end;
         end;

      var
        old_writing_def_stabs : boolean;
      begin
        case st.symtabletype of
          staticsymtable :
            list.concat(tai_comment.Create(strpnew('Defs - Begin Staticsymtable')));
          globalsymtable :
            list.concat(tai_comment.Create(strpnew('Defs - Begin unit '+GetSymTableName(st)+' has index '+tostr(st.moduleid))));
        end;
        old_writing_def_stabs:=writing_def_stabs;
        writing_def_stabs:=true;
        dowritestabs(list,st);
        writing_def_stabs:=old_writing_def_stabs;
        case st.symtabletype of
          staticsymtable :
            list.concat(tai_comment.Create(strpnew('Defs - End Staticsymtable')));
          globalsymtable :
            list.concat(tai_comment.Create(strpnew('Defs - End unit '+GetSymTableName(st)+' has index '+tostr(st.moduleid))));
        end;
      end;


    procedure TDebugInfoStabs.write_procdef(list:TAsmList;pd:tprocdef);
      var
        templist : TAsmList;
        stabsendlabel : tasmlabel;
        mangled_length : longint;
        p,p1 : pchar;
        hs : string;
      begin
        if assigned(pd.procstarttai) then
          begin
            { mark as used, so also the local and para defs will be written }
            pd.dbg_state:=dbg_state_used;

            templist:=TAsmList.create;

            { end of procedure }
            current_asmdata.getlabel(stabsendlabel,alt_dbgtype);

            if assigned(pd.funcretsym) and
               (tabstractnormalvarsym(pd.funcretsym).refs>0) then
              begin
                if tabstractnormalvarsym(pd.funcretsym).localloc.loc=LOC_REFERENCE then
                  begin
    {$warning Need to add gdb support for ret in param register calling}
                    if paramanager.ret_in_param(pd.returndef,pd.proccalloption) then
                      hs:='X*'
                    else
                      hs:='X';
                    templist.concat(Tai_stab.create(stab_stabs,strpnew(
                       '"'+GetSymName(pd.procsym)+':'+hs+def_stab_number(pd.returndef)+'",'+
                       tostr(N_tsym)+',0,0,'+tostr(tabstractnormalvarsym(pd.funcretsym).localloc.reference.offset))));
                    if (m_result in current_settings.modeswitches) then
                      templist.concat(Tai_stab.create(stab_stabs,strpnew(
                         '"RESULT:'+hs+def_stab_number(pd.returndef)+'",'+
                         tostr(N_tsym)+',0,0,'+tostr(tabstractnormalvarsym(pd.funcretsym).localloc.reference.offset))));
                  end;
              end;
            mangled_length:=length(pd.mangledname);
            getmem(p,2*mangled_length+50);
            strpcopy(p,tostr(N_LBRAC)+',0,0,');
            {$IFDEF POWERPC64}strpcopy(strend(p), '.');{$ENDIF POWERPC64}
            strpcopy(strend(p),pd.mangledname);
            if (tf_use_function_relative_addresses in target_info.flags) then
              begin
                strpcopy(strend(p),'-');
                {$IFDEF POWERPC64}strpcopy(strend(p), '.');{$ENDIF POWERPC64}
                strpcopy(strend(p),pd.mangledname);
              end;
            getmem(p1,strlen(p)+1);
            move(p^,p1^,strlen(p)+1);
            templist.concat(Tai_stab.Create(stab_stabn,p1));
            strpcopy(p,tostr(N_RBRAC)+',0,0,'+stabsendlabel.name);
            if (tf_use_function_relative_addresses in target_info.flags) then
              begin
                strpcopy(strend(p),'-');
                {$IFDEF POWERPC64}strpcopy(strend(p), '.');{$ENDIF POWERPC64}
                strpcopy(strend(p),pd.mangledname);
              end;
            getmem(p1,strlen(p)+1);
            move(p^,p1^,strlen(p)+1);
            templist.concat(Tai_stab.Create(stab_stabn,p1));
            freemem(p,2*mangled_length+50);

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
                strpnew('"",'+tostr(N_FUNCTION)+',0,0,'+stabsendlabel.name+'-'+pd.mangledname)));

            current_asmdata.asmlists[al_procedures].insertlistafter(pd.procendtai,templist);

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

            { procdef }
            write_def_stabstr(templist,pd);

            current_asmdata.asmlists[al_procedures].insertlistbefore(pd.procstarttai,templist);

            { para types }
            if assigned(pd.parast) then
              write_symtable_syms(templist,pd.parast);
            { local type defs and vars should not be written
              inside the main proc stab }
            if assigned(pd.localst) and
               (pd.localst.symtabletype=localsymtable) then
              write_symtable_syms(templist,pd.localst);

            current_asmdata.asmlists[al_procedures].insertlistbefore(pd.procstarttai,templist);

            templist.free;
          end;
      end;


{****************************************************************************
                               TSym support
****************************************************************************}

    function TDebugInfoStabs.sym_var_value(const s:string;arg:pointer):string;
      var
        sym : tsym;
      begin
        sym:=tsym(arg);
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


    function TDebugInfoStabs.sym_stabstr_evaluate(sym:tsym;const s:string;const vars:array of string):Pchar;
      begin
        result:=string_evaluate(s,@sym_var_value,sym,vars);
      end;


    procedure TDebugInfoStabs.insertsym(list:TAsmList;sym:tsym);

        function fieldvarsym_stabstr(sym:tfieldvarsym):Pchar;
          begin
            result:=nil;
            if (sym.owner.symtabletype=objecTSymtable) and
               (sp_static in sym.symoptions) then
              result:=sym_stabstr_evaluate(sym,'"${ownername}__${name}:S$1",${N_LCSYM},0,${line},${mangledname}',
                  [def_stab_number(sym.vardef)]);
          end;

        function staticvarsym_stabstr(sym:tstaticvarsym):Pchar;
          var
            st : string;
            threadvaroffset : string;
            regidx : Tregisterindex;
            nsym : string[7];
          begin
            result:=nil;
            { external symbols can't be resolved at link time, so we
              can't generate stabs for them }
            if vo_is_external in sym.varoptions then
              exit;
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
                    result:=sym_stabstr_evaluate(sym,'"${name}:r$1",${N_RSYM},0,${line},$2',[st,tostr(regstabs_table[regidx])]);
                end;
              else
                begin
                  if (vo_is_thread_var in sym.varoptions) then
                    threadvaroffset:='+'+tostr(sizeof(aint))
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
                  result:=sym_stabstr_evaluate(sym,'"${name}:$1",${'+nsym+'},0,${line},${mangledname}$2',[st,threadvaroffset]);
                end;
            end;
          end;

        function localvarsym_stabstr(sym:tlocalvarsym):Pchar;
          var
            st : string;
            regidx : Tregisterindex;
          begin
            result:=nil;
            { There is no space allocated for not referenced locals }
            if (sym.owner.symtabletype=localsymtable) and (sym.refs=0) then
              exit;

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
                    result:=sym_stabstr_evaluate(sym,'"${name}:r$1",${N_RSYM},0,${line},$2',[st,tostr(regstabs_table[regidx])]);
                end;
              LOC_REFERENCE :
                { offset to ebp => will not work if the framepointer is esp
                  so some optimizing will make things harder to debug }
                result:=sym_stabstr_evaluate(sym,'"${name}:$1",${N_TSYM},0,${line},$2',[st,tostr(sym.localloc.reference.offset)])
              else
                internalerror(2003091814);
            end;
          end;

        function paravarsym_stabstr(sym:tparavarsym):Pchar;
          var
            st : string;
            regidx : Tregisterindex;
            c : char;
          begin
            result:=nil;
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
                      result:=sym_stabstr_evaluate(sym,'"pvmt:p$1",${N_TSYM},0,0,$2',
                        [def_stab_number(pvmttype),tostr(sym.localloc.reference.offset)])
                    else
                      begin
                        regidx:=findreg_by_number(sym.localloc.register);
                        result:=sym_stabstr_evaluate(sym,'"pvmt:r$1",${N_RSYM},0,0,$2',
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
                      result:=sym_stabstr_evaluate(sym,'"$$t:$1",${N_TSYM},0,0,$2',
                            [c+def_stab_number(tprocdef(sym.owner.defowner)._class),tostr(sym.localloc.reference.offset)])
                    else
                      begin
                        regidx:=findreg_by_number(sym.localloc.register);
                        result:=sym_stabstr_evaluate(sym,'"$$t:r$1",${N_RSYM},0,0,$2',
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
                      if c='p' then
                        c:='R'
                      else
                        c:='a';
                      st:=c+st;
                      regidx:=findreg_by_number(sym.localloc.register);
                      { "eax", "ecx", "edx", "ebx", "esp", "ebp", "esi", "edi", "eip", "ps", "cs", "ss", "ds", "es", "fs", "gs", }
                      { this is the register order for GDB}
                      if regidx<>0 then
                        result:=sym_stabstr_evaluate(sym,'"${name}:$1",${N_RSYM},0,${line},$2',[st,tostr(longint(regstabs_table[regidx]))]);
                    end;
                  LOC_REFERENCE :
                    begin
                      st:=c+st;
                      { offset to ebp => will not work if the framepointer is esp
                        so some optimizing will make things harder to debug }
                      result:=sym_stabstr_evaluate(sym,'"${name}:$1",${N_TSYM},0,${line},$2',[st,tostr(sym.localloc.reference.offset)])
                    end;
                  else
                    internalerror(2003091814);
                end;
              end;
          end;

        function constsym_stabstr(sym:tconstsym):Pchar;
          var
            st : string;
          begin
            result:=nil;
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
            result:=sym_stabstr_evaluate(sym,'"${name}:c=$1;",${N_FUNCTION},0,${line},0',[st]);
          end;

        function typesym_stabstr(sym:ttypesym) : pchar;
          var
            stabchar : string[2];
          begin
            result:=nil;
            if not assigned(sym.typedef) then
              internalerror(200509262);
            if sym.typedef.typ in tagtypes then
              stabchar:='Tt'
            else
              stabchar:='t';
            result:=sym_stabstr_evaluate(sym,'"${name}:$1$2",${N_LSYM},0,${line},0',[stabchar,def_stab_number(sym.typedef)]);
          end;

        function procsym_stabstr(sym:tprocsym) : pchar;
          var
            i : longint;
          begin
            result:=nil;
            for i:=0 to sym.ProcdefList.Count-1 do
              write_procdef(list,tprocdef(sym.ProcdefList[i]));
          end;

      var
        stabstr : Pchar;
      begin
        stabstr:=nil;
        case sym.typ of
          labelsym :
            stabstr:=sym_stabstr_evaluate(sym,'"${name}",${N_LSYM},0,${line},0',[]);
          fieldvarsym :
            stabstr:=fieldvarsym_stabstr(tfieldvarsym(sym));
          staticvarsym :
            stabstr:=staticvarsym_stabstr(tstaticvarsym(sym));
          localvarsym :
            stabstr:=localvarsym_stabstr(tlocalvarsym(sym));
          paravarsym :
            stabstr:=paravarsym_stabstr(tparavarsym(sym));
          constsym :
            stabstr:=constsym_stabstr(tconstsym(sym));
          typesym :
            stabstr:=typesym_stabstr(ttypesym(sym));
          procsym :
            stabstr:=procsym_stabstr(tprocsym(sym));
        end;
        if stabstr<>nil then
          list.concat(Tai_stab.create(stab_stabs,stabstr));
        { For object types write also the symtable entries }
        if (sym.typ=typesym) and (ttypesym(sym).typedef.typ=objectdef) then
          write_symtable_syms(list,tobjectdef(ttypesym(sym).typedef).symtable);
        sym.isdbgwritten:=true;
      end;


    procedure TDebugInfoStabs.write_symtable_syms(list:TAsmList;st:TSymtable);
      var
        sym : tsym;
        i   : longint;
      begin
        case st.symtabletype of
          staticsymtable :
            list.concat(tai_comment.Create(strpnew('Syms - Begin Staticsymtable')));
          globalsymtable :
            list.concat(tai_comment.Create(strpnew('Syms - Begin unit '+GetSymTableName(st)+' has index '+tostr(st.moduleid))));
        end;
        for i:=0 to st.SymList.Count-1 do
          begin
            sym:=tsym(st.SymList[i]);
            if not(sp_hidden in sym.symoptions) and
               (not sym.isdbgwritten) then
              insertsym(list,sym);
          end;
        case st.symtabletype of
          staticsymtable :
            list.concat(tai_comment.Create(strpnew('Syms - End Staticsymtable')));
          globalsymtable :
            list.concat(tai_comment.Create(strpnew('Syms - End unit '+GetSymTableName(st)+' has index '+tostr(st.moduleid))));
        end;
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
        current_module.flags:=current_module.flags or uf_has_debuginfo;
        if not(target_info.system in systems_darwin) then
          begin
            new_section(current_asmdata.asmlists[al_stabs],sec_data,GetSymTableName(current_module.localsymtable),0);
            current_asmdata.asmlists[al_stabs].concat(tai_symbol.Createname_global(make_mangledname('DEBUGINFO',current_module.localsymtable,''),AT_DATA,0));
          end
        else
          new_section(current_asmdata.asmlists[al_stabs],sec_code,GetSymTableName(current_module.localsymtable),0);

        { first write all global/local symbols. This will flag all required tdefs  }
        if assigned(current_module.globalsymtable) then
          write_symtable_syms(stabsvarlist,current_module.globalsymtable);
        if assigned(current_module.localsymtable) then
          write_symtable_syms(stabsvarlist,current_module.localsymtable);

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
                        if (infile.path^<>'') then
                          list.insertbefore(Tai_stab.Create_str(stab_stabs,'"'+BsToSlash(FixPath(infile.path^,false))+'",'+tostr(n_includefile)+
                                            ',0,0,'+hlabel.name),hp);
                        list.insertbefore(Tai_stab.Create_str(stab_stabs,'"'+FixFileName(infile.name^)+'",'+tostr(n_includefile)+
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
        if (infile.path^<>'') then
          current_asmdata.asmlists[al_start].concat(Tai_stab.Create_str(stab_stabs,'"'+BsToSlash(FixPath(infile.path^,false))+'",'+tostr(n_sourcefile)+
                      ',0,0,'+hlabel.name));
        current_asmdata.asmlists[al_start].concat(Tai_stab.Create_str(stab_stabs,'"'+FixFileName(infile.name^)+'",'+tostr(n_sourcefile)+
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
            If (hp.flags and uf_has_debuginfo)=uf_has_debuginfo then
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
