{
    $Id$
    Copyright (c) 1993-98 by Florian Klaempfl, Pierre Muller

    Implementation for the symbols types of the symtable

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

{****************************************************************************
                          TSYM (base for all symtypes)
****************************************************************************}

    constructor tsym.init(const n : string);
      begin
         left:=nil;
         right:=nil;
         setname(n);
         typ:=abstractsym;
         properties:=current_object_option;
{$ifdef GDB}
         isstabwritten := false;
{$endif GDB}
         fileinfo:=aktfilepos;
{$ifdef UseBrowser}
         defref:=nil;
         lastwritten:=nil;
         refcount:=0;
         if (cs_browser in aktmoduleswitches) and make_ref then
          begin
            defref:=new(pref,init(defref,@tokenpos));
            inc(refcount);
          end;
         lastref:=defref;
{$endif UseBrowser}
      end;

    constructor tsym.load;

      begin
         left:=nil;
         right:=nil;
         setname(readstring);
         typ:=abstractsym;
         fillchar(fileinfo,sizeof(fileinfo),0);
         if object_options then
           properties:=symprop(readbyte)
         else
           properties:=sp_public;
{$ifdef UseBrowser}
         lastref:=nil;
         defref:=nil;
         lastwritten:=nil;
         refcount:=0;
{$endif UseBrowser}
{$ifdef GDB}
         isstabwritten := false;
{$endif GDB}
      end;

{$ifdef UseBrowser}

    procedure tsym.load_references;
      var
        pos : tfileposinfo;
      begin
        while (not current_ppu^.endofentry) do
         begin
           readposinfo(pos);
           inc(refcount);
           lastref:=new(pref,init(lastref,@pos));
           if refcount=1 then
            defref:=lastref;
         end;
        lastwritten:=lastref;
      end;

    procedure tsym.write_references;
      var
        ref   : pref;
        prdef : pdef;
      begin
        if lastwritten=lastref then
          exit;
      { write address to this symbol }
        writesymref(@self);
      { write symbol refs }
        if assigned(lastwritten) then
          ref:=lastwritten
        else
          ref:=defref;
        while assigned(ref) do
         begin
           writeposinfo(ref^.posinfo);
           ref:=ref^.nextref;
         end;
        lastwritten:=lastref;
        current_ppu^.writeentry(ibsymref);
      end;


    procedure tsym.add_to_browserlog;
      var
        prdef : pprocdef;
      begin
        if assigned(defref) then
         begin
           Browse.AddLog('***'+name+'***');
           Browse.AddLogRefs(defref);
         end;
      end;
{$endif UseBrowser}


    destructor tsym.done;
      begin
{$ifdef tp}
         if not(use_big) then
{$endif tp}
           strdispose(_name);
{$ifdef UseBrowser}
         if assigned(defref) then
          dispose(defref,done);
{$endif UseBrowser}
         if assigned(left) then
           dispose(left,done);
         if assigned(right) then
           dispose(right,done);
      end;

    procedure tsym.write;

      begin
         writestring(name);
         if object_options then
           writebyte(byte(properties));
{$ifdef UseBrowser}
{         if cs_browser in aktmoduleswitches then
           write_references; }
{$endif UseBrowser}
      end;

    procedure tsym.deref;

      begin
      end;

    function tsym.name : string;
{$ifdef tp}
      var
         s : string;
         b : byte;
{$endif}
      begin
{$ifdef tp}
         if use_big then
           begin
              symbolstream.seek(longint(_name));
              symbolstream.read(b,1);
              symbolstream.read(s[1],b);
              s[0]:=chr(b);
              name:=s;
           end
         else
{$endif}
        if assigned(_name) then
         name:=strpas(_name)
        else
         name:='';
      end;

    function tsym.mangledname : string;
      begin
         mangledname:=name;
      end;

    procedure tsym.setname(const s : string);
      begin
         setstring(_name,s);
      end;

    { for most symbol types ther is nothing to do at all }
    procedure tsym.insert_in_data;
      begin
      end;


{$ifdef GDB}
    function tsym.stabstring : pchar;

      begin
         stabstring:=strpnew('"'+name+'",'+tostr(N_LSYM)+',0,'+
           tostr(fileinfo.line)+',0');
      end;

    procedure tsym.concatstabto(asmlist : paasmoutput);

    var stab_str : pchar;
      begin
         if not isstabwritten then
           begin
              stab_str := stabstring;
              if asmlist = debuglist then do_count_dbx := true;
              { count_dbx(stab_str); moved to GDB.PAS }
              asmlist^.concat(new(pai_stabs,init(stab_str)));
              isstabwritten:=true;
          end;
    end;
{$endif GDB}

{****************************************************************************
                                 TLABELSYM
****************************************************************************}

    constructor tlabelsym.init(const n : string; l : plabel);

      begin
         inherited init(n);
         typ:=labelsym;
         number:=l;
         number^.is_used:=false;
         number^.is_set:=true;
         number^.refcount:=0;
         defined:=false;
      end;

    destructor tlabelsym.done;

      begin
         if not(defined) then
          Message1(sym_e_label_not_defined,name);
         inherited done;
      end;

    function tlabelsym.mangledname : string;

      begin
         { this also sets the is_used field }
         mangledname:=lab2str(number);
      end;

    procedure tlabelsym.write;

      begin
         Message(sym_e_ill_label_decl);
      end;

{****************************************************************************
                                  TUNITSYM
****************************************************************************}

    constructor tunitsym.init(const n : string;ref : punitsymtable);
      var
        old_make_ref : boolean;
      begin
         old_make_ref:=make_ref;
         make_ref:=false;
         inherited init(n);
         make_ref:=old_make_ref;
         typ:=unitsym;
         unitsymtable:=ref;
         prevsym:=ref^.unitsym;
         ref^.unitsym:=@self;
         refs:=0;
      end;

    destructor tunitsym.done;
      begin
         if assigned(unitsymtable) and (unitsymtable^.unitsym=@self) then
           unitsymtable^.unitsym:=prevsym;
         inherited done;
      end;

    procedure tunitsym.write;
      begin
      end;

{$ifdef GDB}
    procedure tunitsym.concatstabto(asmlist : paasmoutput);
      begin
      {Nothing to write to stabs !}
      end;
{$endif GDB}

{****************************************************************************
                                  TPROCSYM
****************************************************************************}

    constructor tprocsym.init(const n : string);

      begin
         tsym.init(n);
         typ:=procsym;
         definition:=nil;
         owner:=nil;
{$ifdef GDB}
         is_global := false;
{$endif GDB}
      end;

    constructor tprocsym.load;

      begin
         tsym.load;
         typ:=procsym;
         definition:=pprocdef(readdefref);
{$ifdef GDB}
         is_global := false;
{$endif GDB}
      end;

    destructor tprocsym.done;

      begin
         check_forward;
         tsym.done;
      end;

    function tprocsym.mangledname : string;

      begin
         mangledname:=definition^.mangledname;
      end;


    function tprocsym.demangledname:string;
      begin
        demangledname:=name+definition^.demangled_paras;
      end;

    procedure tprocsym.write_parameter_lists;

      var
         p : pprocdef;

      begin
         p:=definition;
         while assigned(p) do
           begin
              { force the error to be printed }
              Verbose.Message1(sym_b_param_list,name+p^.demangled_paras);
              p:=p^.nextoverloaded;
           end;
      end;

    procedure tprocsym.check_forward;

      var
         pd : pprocdef;
         oldaktfilepos : tfileposinfo;
      begin
         pd:=definition;
         while assigned(pd) do
           begin
              if pd^.forwarddef then
                begin
                   oldaktfilepos:=aktfilepos;
                   aktfilepos:=fileinfo;
                   if assigned(pd^._class) then
                     Message1(sym_e_forward_not_resolved,pd^._class^.name^+'.'+name+demangledparas(pd^.demangled_paras))
                   else
                     Message1(sym_e_forward_not_resolved,name+pd^.demangled_paras);
                   aktfilepos:=oldaktfilepos;
                end;
              pd:=pd^.nextoverloaded;
           end;
      end;


    procedure tprocsym.deref;
      var t : ttoken;
          last : pprocdef;
      begin
         resolvedef(pdef(definition));
         if (definition^.options and pooperator) <> 0 then
           begin
              last:=definition;
              while assigned(last^.nextoverloaded) do
                last:=last^.nextoverloaded;
              for t:=PLUS to last_overloaded do
              if (name=overloaded_names[t]) then
                begin
                   if assigned(overloaded_operators[t]) then
                     last^.nextoverloaded:=overloaded_operators[t]^.definition;
                   overloaded_operators[t]:=@self;
                end;
           end;
      end;

    procedure tprocsym.write;
      begin
         tsym.write;
         writedefref(pdef(definition));
         current_ppu^.writeentry(ibprocsym);
      end;

{$ifdef UseBrowser}
    procedure tprocsym.load_references;
      var
        prdef : pprocdef;
      begin
         inherited load_references;
         prdef:=definition;
         { take care about operators !!  }
         while assigned(prdef) and (prdef^.owner=definition^.owner) do
          begin
            prdef^.load_references;
            prdef:=prdef^.nextoverloaded;
          end;
      end;

    procedure tprocsym.write_references;
      var
        prdef : pprocdef;
      begin
         inherited write_references;
         prdef:=definition;
         while assigned(prdef) and (prdef^.owner=definition^.owner) do
          begin
            prdef^.write_references;
            prdef:=prdef^.nextoverloaded;
          end;
      end;

    procedure tprocsym.add_to_browserlog;
      var
        prdef : pprocdef;
      begin
         inherited add_to_browserlog;
         prdef:=definition;
         while assigned(prdef) do
           begin
              pprocdef(prdef)^.add_to_browserlog;
              prdef:=pprocdef(prdef)^.nextoverloaded;
           end;
      end;
{$endif UseBrowser}

{$ifdef GDB}
    function tprocsym.stabstring : pchar;
     Var RetType : Char;
         Obj,Info : String;
    begin
      obj := name;
      info := '';
      if is_global then
       RetType := 'F'
      else
       RetType := 'f';
     if assigned(owner) then
      begin
        if (owner^.symtabletype = objectsymtable) then
         obj := owner^.name^+'__'+name;
        if (owner^.symtabletype=localsymtable) and assigned(owner^.name) then
         info := ','+name+','+owner^.name^;
      end;
     stabstring :=strpnew('"'+obj+':'+RetType
           +definition^.retdef^.numberstring+info+'",'+tostr(n_function)
           +',0,'+
           tostr(aktfilepos.line)
           +','+definition^.mangledname);
    end;

    procedure tprocsym.concatstabto(asmlist : paasmoutput);
    begin
      if (definition^.options and pointernproc) <> 0 then exit;
      if not isstabwritten then
        asmlist^.concat(new(pai_stabs,init(stabstring)));
      isstabwritten := true;
      if assigned(definition^.parast) then
        definition^.parast^.concatstabto(asmlist);
      if assigned(definition^.localst) then
        definition^.localst^.concatstabto(asmlist);
      definition^.is_def_stab_written := true;
    end;
{$endif GDB}


{****************************************************************************
                                  TPROGRAMSYM
****************************************************************************}

    constructor tprogramsym.init(const n : string);
      begin
        inherited init(n);
        typ:=programsym;
      end;

{****************************************************************************
                                  TERRORSYM
****************************************************************************}

    constructor terrorsym.init;
      begin
        inherited init('');
        typ:=errorsym;
      end;

{****************************************************************************
                                TPROPERTYSYM
****************************************************************************}

    constructor tpropertysym.init(const n : string);
      begin
         inherited init(n);
         typ:=propertysym;
         options:=0;
         proptype:=nil;
         readaccessdef:=nil;
         writeaccessdef:=nil;
         readaccesssym:=nil;
         writeaccesssym:=nil;
         index:=$0;
      end;

    destructor tpropertysym.done;

      begin
         inherited done;
      end;

    constructor tpropertysym.load;

      begin
         inherited load;
         typ:=propertysym;
         proptype:=readdefref;
         options:=readlong;
         index:=readlong;
         { it's hack ... }
         readaccesssym:=psym(stringdup(readstring));
         writeaccesssym:=psym(stringdup(readstring));
         { now the defs: }
         readaccessdef:=readdefref;
         writeaccessdef:=readdefref;
      end;

    procedure tpropertysym.deref;

      begin
         resolvedef(proptype);
         resolvedef(readaccessdef);
         resolvedef(writeaccessdef);
         { solve the hack we did in load: }
         if pstring(readaccesssym)^<>'' then
           begin
              srsym:=search_class_member(pobjectdef(owner^.defowner),pstring(readaccesssym)^);
              if not(assigned(srsym)) then
                srsym:=generrorsym;
           end
         else
           srsym:=nil;
         stringdispose(pstring(readaccesssym));
         readaccesssym:=srsym;
         if pstring(writeaccesssym)^<>'' then
           begin
              srsym:=search_class_member(pobjectdef(owner^.defowner),pstring(writeaccesssym)^);
              if not(assigned(srsym)) then
                srsym:=generrorsym;
           end
         else
           srsym:=nil;
         stringdispose(pstring(writeaccesssym));
         writeaccesssym:=srsym;
      end;

    function tpropertysym.getsize : longint;

      begin
         getsize:=0;
      end;

    procedure tpropertysym.write;

      begin
         tsym.write;
         writedefref(proptype);
         writelong(options);
         writelong(index);
         if assigned(readaccesssym) then
           writestring(readaccesssym^.name)
         else
           writestring('');
         if assigned(writeaccesssym) then
           writestring(writeaccesssym^.name)
         else
           writestring('');
         writedefref(readaccessdef);
         writedefref(writeaccessdef);
         current_ppu^.writeentry(ibpropertysym);
      end;

{$ifdef GDB}
    function tpropertysym.stabstring : pchar;
      begin
         { !!!! don't know how to handle }
         stabstring:=strpnew('');
      end;

    procedure tpropertysym.concatstabto(asmlist : paasmoutput);
      begin
         { !!!! don't know how to handle }
      end;
{$endif GDB}

{****************************************************************************
                                  TFUNCRETSYM
****************************************************************************}

    constructor tfuncretsym.init(const n : string;approcinfo : pointer{pprocinfo});

      begin
         tsym.init(n);
         typ:=funcretsym;
         funcretprocinfo:=approcinfo;
         funcretdef:=pprocinfo(approcinfo)^.retdef;
         { address valid for ret in param only }
         { otherwise set by insert             }
         address:=pprocinfo(approcinfo)^.retoffset;
      end;

{$ifdef GDB}
    procedure tfuncretsym.concatstabto(asmlist : paasmoutput);
      begin
        { Nothing to do here, it is done in genexitcode  }
      end;
{$endif GDB}


{****************************************************************************
                                  TABSOLUTESYM
****************************************************************************}

{   constructor tabsolutesym.init(const s : string;p : pdef;newref : psym);
     begin
        inherited init(s,p);
        ref:=newref;
        typ:=absolutesym;
     end; }

    constructor tabsolutesym.load;

      begin
         tvarsym.load;
         typ:=absolutesym;
         ref:=nil;
         address:=0;
         asmname:=nil;
         abstyp:=absolutetyp(readbyte);
         absseg:=false;
         case abstyp of
       tovar : begin
                 asmname:=stringdup(readstring);
                 ref:=srsym;
               end;
       toasm : asmname:=stringdup(readstring);
      toaddr : address:=readlong;
         end;
      end;

    procedure tabsolutesym.write;

      begin
         tsym.write;
         writebyte(byte(varspez));
         if read_member then
           writelong(address);
         writedefref(definition);
         writebyte(byte(abstyp));
         case abstyp of
           tovar : writestring(ref^.name);
           toasm : writestring(asmname^);
          toaddr : writelong(address);
         end;
        current_ppu^.writeentry(ibabsolutesym);
      end;

    procedure tabsolutesym.deref;
      begin
         resolvedef(definition);
         if (abstyp=tovar) and (asmname<>nil) then
           begin
              { search previous loaded symtables }
              getsym(asmname^,false);
              if not(assigned(srsym)) then
                getsymonlyin(owner,asmname^);
              if not(assigned(srsym)) then
                srsym:=generrorsym;
              ref:=srsym;
              stringdispose(asmname);
           end;
      end;

    function tabsolutesym.mangledname : string;
      begin
         case abstyp of
           tovar : mangledname:=ref^.mangledname;
           toasm : mangledname:=asmname^;
          toaddr : mangledname:='$'+tostr(address);
         else
           internalerror(10002);
         end;
      end;

      procedure tabsolutesym.insert_in_data;

        begin
        end;


{$ifdef GDB}
    procedure tabsolutesym.concatstabto(asmlist : paasmoutput);
      begin
      { I don't know how to handle this !! }
      end;
{$endif GDB}

{****************************************************************************
                                  TVARSYM
****************************************************************************}

    constructor tvarsym.init(const n : string;p : pdef);
      begin
         tsym.init(n);
         typ:=varsym;
         definition:=p;
         _mangledname:=nil;
         varspez:=vs_value;
         address:=0;
         refs:=0;
         is_valid := 1;
         var_options:=0;
         { can we load the value into a register ? }
         case p^.deftype of
        pointerdef,
           enumdef,
        procvardef : var_options:=var_options or vo_regable;
            orddef : case porddef(p)^.typ of
                       u8bit,u16bit,u32bit,
                       bool8bit,bool16bit,bool32bit,
                       s8bit,s16bit,s32bit :
                         var_options:=var_options or vo_regable;
                     else
                       var_options:=var_options and not vo_regable;
                     end;
         else
           var_options:=var_options and not vo_regable;
         end;
         reg:=R_NO;
      end;

    constructor tvarsym.load;

      begin
         tsym.load;
         typ:=varsym;
         _mangledname:=nil;
         varspez:=tvarspez(readbyte);
         if read_member then
           address:=readlong
         else
           address:=0;
         definition:=readdefref;
         refs := 0;
         is_valid := 1;
         { symbols which are load are never candidates for a register }
         var_options:=0;
         { was regable:=false; }
         reg:=R_NO;
      end;

    constructor tvarsym.init_C(const n,mangled : string;p : pdef);

      begin
      { The tvarsym is necessary for 0.99.5 (PFV) }
         tvarsym.init(n,p);
         var_options:=var_options or vo_is_C_var;
         { C prefix not allways added moved to
           pdecl PM }
         _mangledname:=strpnew(mangled);
      end;

    constructor tvarsym.load_C;

      begin
      { Adding tvarsym removes the warning }
         tvarsym.load;
         typ:=varsym;
         var_options:=readbyte;
         _mangledname:=strpnew(readstring);
      end;

    procedure tvarsym.deref;

      begin
         resolvedef(definition);
      end;

    procedure tvarsym.write;

      begin
         tsym.write;
         writebyte(byte(varspez));

         if read_member then
           writelong(address);

         writedefref(definition);
         if (var_options and vo_is_C_var)<>0 then
           begin
              writebyte(var_options);
              writestring(mangledname);
           end;
         if (var_options and vo_is_C_var)<>0 then
           current_ppu^.writeentry(ibvarsym_C)
         else
           current_ppu^.writeentry(ibvarsym);
      end;


    function tvarsym.mangledname : string;
      var
        prefix : string;
      begin
         if assigned(_mangledname) then
           begin
              mangledname:=strpas(_mangledname);
              exit;
           end;
         case owner^.symtabletype of
           staticsymtable : if (cs_smartlink in aktmoduleswitches) then
                              prefix:='_'+owner^.name^+'$$$_'
                            else
                              prefix:='_';
             unitsymtable,
           globalsymtable : prefix:='U_'+owner^.name^+'_';
           else
             Message(sym_e_invalid_call_tvarsymmangledname);
           end;
         mangledname:=prefix+name;
      end;

    function tvarsym.getsize : longint;
      begin
         { only if the definition is set, we could determine the   }
         { size, this is if an error occurs while reading the type }
         { also used for operator, this allows not to allocate the }
         { return size twice                                       }
         if assigned(definition) then
           begin
              case varspez of
                 vs_value : getsize:=definition^.size;
                   vs_var : begin
                            { open arrays push also the high valye }
                              if (definition^.deftype=arraydef) and
                                 (parraydef(definition)^.lowrange=0) and
                                 (parraydef(definition)^.highrange=-1) then
                                getsize:=sizeof(pointer)+4
                              else
                                getsize:=sizeof(pointer);
                            end;
                 vs_const : begin
                              case definition^.deftype of
                             stringdef,
                             recorddef,
                             objectdef,
                                setdef : getsize:=sizeof(pointer);
                              arraydef : begin
                                         { open arrays push also the high valye }
                                           if (parraydef(definition)^.lowrange=0) and
                                              (parraydef(definition)^.highrange=-1) then
                                             getsize:=sizeof(pointer)+4
                                           else
                                             getsize:=sizeof(pointer);
                                         end;
                              else
                               getsize:=definition^.size;
                              end;
                            end;
              end;
           end
         else
           getsize:=0;
      end;

    procedure tvarsym.insert_in_data;
      var
         l,modulo : longint;
      begin
        if (var_options and vo_is_external)<>0 then
          exit;
        { handle static variables of objects especially }
        if read_member and (owner^.symtabletype=objectsymtable) and
           ((properties and sp_static)<>0) then
         begin
            { the data filed is generated in parser.pas
              with a tobject_FIELDNAME variable }
            { this symbol can't be loaded to a register }
            var_options:=var_options and not vo_regable;
         end
        else
         if not(read_member) then
          begin
             { made problems with parameters etc. ! (FK) }
             {  check for instance of an abstract object or class }
             {
             if (pvarsym(sym)^.definition^.deftype=objectdef) and
               ((pobjectdef(pvarsym(sym)^.definition)^.options and oois_abstract)<>0) then
               Message(sym_e_no_instance_of_abstract_object);
             }

             l:=getsize;
             case owner^.symtabletype of

          stt_exceptsymtable:
            { can contain only one symbol, address calculated later }
            ;
          localsymtable : begin
                            is_valid := 0;
                            modulo:=owner^.datasize and 3;
{$ifdef m68k}
                          { word alignment required for motorola }
                            if (l=1) then
                             l:=2
                            else
{$endif}
                            if (l>=4) and (modulo<>0) then
                             inc(l,4-modulo)
                            else
                             if (l>=2) and ((modulo and 1)<>0) then
                              inc(l,2-(modulo and 1));
                            inc(owner^.datasize,l);
                            address:=owner^.datasize;
                          end;
         staticsymtable : begin
                            if (cs_smartlink in aktmoduleswitches) then
                              bsssegment^.concat(new(pai_cut,init));
{$ifdef GDB}
                            if cs_debuginfo in aktmoduleswitches then
                               concatstabto(bsssegment);
{$endif GDB}
                            if (cs_smartlink in aktmoduleswitches) or
                               ((var_options and vo_is_c_var)<>0) then
                              bsssegment^.concat(new(pai_datablock,init_global(mangledname,l)))
                            else
                              bsssegment^.concat(new(pai_datablock,init(mangledname,l)));
                            { increase datasize }
                            inc(owner^.datasize,l);
                            { this symbol can't be loaded to a register }
                            var_options:=var_options and not vo_regable;
                          end;
         globalsymtable : begin
                            if (cs_smartlink in aktmoduleswitches) then
                              bsssegment^.concat(new(pai_cut,init));
{$ifdef GDB}
                            if cs_debuginfo in aktmoduleswitches then
                              concatstabto(bsssegment);
{$endif GDB}
                            bsssegment^.concat(new(pai_datablock,init_global(mangledname,l)));
                            inc(owner^.datasize,l);
                            { this symbol can't be loaded to a register }
                            var_options:=var_options and not vo_regable;
                          end;
         recordsymtable,
         objectsymtable : begin
                          { this symbol can't be loaded to a register }
                            var_options:=var_options and not vo_regable;
                          { align record and object fields }
                            if (l=1) or (aktpackrecords=1) then
                             begin
                               address:=owner^.datasize;
                               inc(owner^.datasize,l)
                             end
                            else
                             if (l=2) or (aktpackrecords=2) then
                              begin
                                owner^.datasize:=(owner^.datasize+1) and (not 1);
                                address:=owner^.datasize;
                                inc(owner^.datasize,l)
                              end
                            else
                             if (l<=4) or (aktpackrecords=4) then
                              begin
                                owner^.datasize:=(owner^.datasize+3) and (not 3);
                                address:=owner^.datasize;
                                inc(owner^.datasize,l);
                              end
                            else
                             if (l<=16) or (aktpackrecords=16) then
                              begin
                                owner^.datasize:=(owner^.datasize+15) and (not 15);
                                address:=owner^.datasize;
                                inc(owner^.datasize,l);
                              end;
                          end;
           parasymtable : begin
                            address:=owner^.datasize;
                          { needs word alignment }
                            if odd(l) then
                             inc(owner^.datasize,l+1)
                            else
                             inc(owner^.datasize,l);
                          end
             else
               begin
                  modulo:=owner^.datasize and 3 ;
                  if (l>=4) and (modulo<>0) then
                    inc(owner^.datasize,4-modulo)
                  else
                    if (l>=2) and ((modulo and 1)<>0) then
                    { nice piece of code !!
                    inc(owner^.datasize,2-(datasize and 1));
                    2 - (datasize and 1) is allways 1 in this case
                    Florian when will your global stream analyser
                    find this out ?? }
                      inc(owner^.datasize);
                  address:=owner^.datasize;
                  inc(owner^.datasize,l);
               end;
             end;
          end;
      end;

{$ifdef GDB}
    function tvarsym.stabstring : pchar;
    var
      st : char;
    begin
       if (owner^.symtabletype = objectsymtable) and
          ((properties and sp_static)<>0) then
         begin
            if use_gsym then st := 'G' else st := 'S';
            stabstring := strpnew('"'+owner^.name^+'__'+name+':'+
                     +definition^.numberstring+'",'+
                     tostr(N_LCSYM)+',0,'+tostr(fileinfo.line)+','+mangledname);
         end
       else if (owner^.symtabletype = globalsymtable) or
          (owner^.symtabletype = unitsymtable) then
         begin
            { Here we used S instead of
              because with G GDB doesn't look at the address field
              but searches the same name or with a leading underscore
              but these names don't exist in pascal !}
            if use_gsym then st := 'G' else st := 'S';
            stabstring := strpnew('"'+name+':'+st
                     +definition^.numberstring+'",'+
                     tostr(N_LCSYM)+',0,'+tostr(fileinfo.line)+','+mangledname);
         end
       else if owner^.symtabletype = staticsymtable then
         begin
            stabstring := strpnew('"'+name+':S'
                  +definition^.numberstring+'",'+
                  tostr(N_LCSYM)+',0,'+tostr(fileinfo.line)+','+mangledname);
         end
       else if (owner^.symtabletype=parasymtable) then
         begin
            case varspez of
               vs_value : st := 'p';
               vs_var   : st := 'v';
               vs_const : if dont_copy_const_param(definition) then
                            st := 'v'{ should be 'i' but 'i' doesn't work }
                          else
                            st := 'p';
              end;
            stabstring := strpnew('"'+name+':'+st
                  +definition^.numberstring+'",'+
                  tostr(N_PSYM)+',0,'+tostr(fileinfo.line)+','+
                  tostr(address+owner^.call_offset));
                  {offset to ebp => will not work if the framepointer is esp
                  so some optimizing will make things harder to debug }
         end
       else if (owner^.symtabletype=localsymtable) then
   {$ifdef i386}
         if reg<>R_NO then
           begin
              { "eax", "ecx", "edx", "ebx", "esp", "ebp", "esi", "edi", "eip", "ps", "cs", "ss", "ds", "es", "fs", "gs", }
              { this is the register order for GDB}
              stabstring:=strpnew('"'+name+':r'
                        +definition^.numberstring+'",'+
                        tostr(N_RSYM)+',0,'+
                        tostr(fileinfo.line)+','+tostr(GDB_i386index[reg]));
           end
         else
   {$endif i386}
           stabstring := strpnew('"'+name+':'
                  +definition^.numberstring+'",'+
                  tostr(N_LSYM)+',0,'+tostr(fileinfo.line)+',-'+tostr(address))
       else
         stabstring := inherited stabstring;
  end;

    procedure tvarsym.concatstabto(asmlist : paasmoutput);
{$ifdef i386}
      var stab_str : pchar;
{$endif i386}
      begin
         inherited concatstabto(asmlist);
{$ifdef i386}
      if (owner^.symtabletype=parasymtable) and
         (reg<>R_NO) then
           begin
           { "eax", "ecx", "edx", "ebx", "esp", "ebp", "esi", "edi", "eip", "ps", "cs", "ss", "ds", "es", "fs", "gs", }
           { this is the register order for GDB}
              stab_str:=strpnew('"'+name+':r'
                     +definition^.numberstring+'",'+
                     tostr(N_RSYM)+',0,'+
                     tostr(fileinfo.line)+','+tostr(GDB_i386index[reg]));
              asmlist^.concat(new(pai_stabs,init(stab_str)));
           end;
{$endif i386}
      end;
{$endif GDB}

    destructor tvarsym.done;

      begin
         strdispose(_mangledname);
         inherited done;
      end;


{****************************************************************************
                             TTYPEDCONSTSYM
*****************************************************************************}

    constructor ttypedconstsym.init(const n : string;p : pdef);

      begin
         tsym.init(n);
         typ:=typedconstsym;
         definition:=p;
         prefix:=stringdup(procprefix);
      end;

    constructor ttypedconstsym.load;

      begin
         tsym.load;
         typ:=typedconstsym;
         definition:=readdefref;
         prefix:=stringdup(readstring);
      end;

    destructor ttypedconstsym.done;

      begin
         stringdispose(prefix);
         tsym.done;
      end;

    function ttypedconstsym.mangledname : string;

      begin
         mangledname:='TC_'+prefix^+'_'+name;
      end;

    procedure ttypedconstsym.deref;

      begin
         resolvedef(definition);
      end;

    procedure ttypedconstsym.write;

      begin
         tsym.write;
         writedefref(definition);
         writestring(prefix^);
         current_ppu^.writeentry(ibtypedconstsym);
      end;

      { for most symbol types ther is nothing to do at all }
      procedure ttypedconstsym.insert_in_data;

        begin
           { here there is a problem for ansistrings !!                 }
           { we must write the label only after the 12 header bytes (PM)
           if not is_ansistring(definition) then
           }
           { solved, the ansis string is moved to consts (FK) }
             really_insert_in_data;
        end;

      procedure ttypedconstsym.really_insert_in_data;
        begin
           if owner^.symtabletype=globalsymtable then
             begin
                if (cs_smartlink in aktmoduleswitches) then
                  datasegment^.concat(new(pai_cut,init));
{$ifdef GDB}
                if cs_debuginfo in aktmoduleswitches then
                  concatstabto(datasegment);
{$endif GDB}
                datasegment^.concat(new(pai_symbol,init_global(mangledname)));
             end
           else
             if owner^.symtabletype<>unitsymtable then
               begin
                 if (cs_smartlink in aktmoduleswitches) then
                   datasegment^.concat(new(pai_cut,init));
{$ifdef GDB}
                 if cs_debuginfo in aktmoduleswitches then
                   concatstabto(datasegment);
{$endif GDB}
                 if (cs_smartlink in aktmoduleswitches) then
                   datasegment^.concat(new(pai_symbol,init_global(mangledname)))
                 else
                   datasegment^.concat(new(pai_symbol,init(mangledname)));
               end;
           end;

{$ifdef GDB}
    function ttypedconstsym.stabstring : pchar;
    var
      st : char;
    begin
    if use_gsym and (owner^.symtabletype in [unitsymtable,globalsymtable]) then
      st := 'G'
    else
      st := 'S';
    stabstring := strpnew('"'+name+':'+st+
            definition^.numberstring+'",'+tostr(n_STSYM)+',0,'+
            tostr(fileinfo.line)+','+mangledname);
    end;
{$endif GDB}

{****************************************************************************
                                  TCONSTSYM
****************************************************************************}

    constructor tconstsym.init(const n : string;t : tconsttype;v : longint;def : pdef);
      begin
         tsym.init(n);
         typ:=constsym;
         definition:=def;
         consttype:=t;
         value:=v;
      end;


    constructor tconstsym.load;
      var
         pd : pbestreal;
         ps : pnormalset;
      begin
         tsym.load;
         typ:=constsym;
         consttype:=tconsttype(readbyte);
         case consttype of
            constint,
           constbool,
           constchar : value:=readlong;
            constord : begin
                          definition:=readdefref;
                          value:=readlong;
                       end;
         conststring : value:=longint(stringdup(readstring));
           constreal : begin
                         new(pd);
                         pd^:=readreal;
                         value:=longint(pd);
                       end;
           constseta : begin
                         definition:=readdefref;
                         new(ps);
                         readnormalset(ps^);
                         value:=longint(ps);
                       end;
         else
           Message1(unit_f_ppu_invalid_entry,tostr(ord(consttype)));
         end;
      end;


    destructor tconstsym.done;
      begin
        case consttype of
         conststring : stringdispose(pstring(value));
           constreal : dispose(pbestreal(value));
           constseta : dispose(pnormalset(value));
        end;
        inherited done;
      end;


    function tconstsym.mangledname : string;
      begin
         mangledname:=name;
      end;


    procedure tconstsym.deref;
      begin
        if consttype in [constord,constseta] then
         resolvedef(pdef(definition));
      end;


    procedure tconstsym.write;
      begin
         tsym.write;
         writebyte(byte(consttype));
         case consttype of
           constint,
           constbool,
           constchar : writelong(value);
            constord : begin
                         writedefref(definition);
                         writelong(value);
                       end;
         conststring : writestring(pstring(value)^);
           constreal : writereal(pbestreal(value)^);
           constseta : begin
                         writedefref(definition);
                         writenormalset(pointer(value)^);
                       end;
         else
           internalerror(13);
         end;
        current_ppu^.writeentry(ibconstsym);
      end;

{$ifdef GDB}
    function tconstsym.stabstring : pchar;
    var st : string;
    begin
         {even GDB v4.16 only now 'i' 'r' and 'e' !!!}
         case consttype of
            conststring : begin
                          { I had to remove ibm2ascii !! }
                          st := pstring(value)^;
                          {st := ibm2ascii(pstring(value)^);}
                          st := 's'''+st+'''';
                          end;
            constbool, constint, constord, constchar : st := 'i'+tostr(value);
            constreal : begin
                        system.str(pbestreal(value)^,st);
                        st := 'r'+st;
                        end;
         { if we don't know just put zero !! }
         else st:='i0';
            {***SETCONST}
            {constset:;}    {*** I don't know what to do with a set.}
         { sets are not recognized by GDB}
            {***}
        end;
    stabstring := strpnew('"'+name+':c='+st+'",'+tostr(N_function)+',0,'+
                    tostr(fileinfo.line)+',0');
    end;

    procedure tconstsym.concatstabto(asmlist : paasmoutput);
      begin
        if consttype <> conststring then
          inherited concatstabto(asmlist);
      end;
{$endif GDB}

{****************************************************************************
                                  TENUMSYM
****************************************************************************}

    constructor tenumsym.init(const n : string;def : penumdef;v : longint);
      begin
         tsym.init(n);
         typ:=enumsym;
         definition:=def;
         value:=v;
         if def^.min>v then
           def^.setmin(v);
         if def^.max<v then
           def^.setmax(v);
{$ifdef GDB}
         order;
{$endif GDB}
      end;

    constructor tenumsym.load;

      begin
         tsym.load;
         typ:=enumsym;
         definition:=penumdef(readdefref);
         value:=readlong;
{$ifdef GDB}
         next := Nil;
{$endif GDB}
      end;

    procedure tenumsym.deref;

      begin
         resolvedef(pdef(definition));
{$ifdef GDB}
         order;
{$endif}
      end;

{$ifdef GDB}
         procedure tenumsym.order;
         var sym : penumsym;
         begin
         sym := definition^.first;
         if sym = nil then
           begin
           definition^.first := @self;
           next := nil;
           exit;
           end;
         {reorder the symbols in increasing value }
         if value < sym^.value then
           begin
           next := sym;
           definition^.first := @self;
           end else
           begin
           while (sym^.value <= value) and assigned(sym^.next) do
             sym := sym^.next;
           next := sym^.next;
           sym^.next := @self;
           end;
         end;
{$endif GDB}

    procedure tenumsym.write;

      begin
         tsym.write;
         writedefref(definition);
         writelong(value);
         current_ppu^.writeentry(ibenumsym);
      end;

{$ifdef GDB}
    procedure tenumsym.concatstabto(asmlist : paasmoutput);
    begin
    {enum elements have no stab !}
    end;
{$EndIf GDB}

{****************************************************************************
                                  TTYPESYM
****************************************************************************}

    constructor ttypesym.init(const n : string;d : pdef);

      begin
         tsym.init(n);
         typ:=typesym;
         definition:=d;
{$ifdef GDB}
         isusedinstab := false;
{$endif GDB}
         forwardpointer:=nil;
         { this allows to link definitions with the type with declares }
         { them                                                        }
         if assigned(definition) then
           if definition^.sym=nil then
             definition^.sym:=@self;
      end;

    constructor ttypesym.load;

      begin
         tsym.load;
         typ:=typesym;
         forwardpointer:=nil;
{$ifdef GDB}
         isusedinstab := false;
{$endif GDB}
         definition:=readdefref;
      end;

    destructor ttypesym.done;

      begin
         if assigned(definition) then
           if definition^.sym=@self then
             definition^.sym:=nil;
         inherited done;
      end;


    procedure ttypesym.deref;

      begin
         resolvedef(definition);
         if assigned(definition) then
          begin
            if definition^.sym=nil then
             definition^.sym:=@self;
            if definition^.deftype=recorddef then
             precdef(definition)^.symtable^.name:=stringdup('record '+name);
          end;
      end;


    procedure ttypesym.write;

      begin
         tsym.write;
         writedefref(definition);
         current_ppu^.writeentry(ibtypesym);
      end;


{$ifdef UseBrowser}

    procedure ttypesym.load_references;
      var
        prdef : pdef;
      begin
         inherited load_references;
         if (definition^.deftype=recorddef) then
            precdef(definition)^.symtable^.load_browser;
         if (definition^.deftype=objectdef) then
             pobjectdef(definition)^.publicsyms^.load_browser;
      end;

    procedure ttypesym.write_references;
      var
        prdef : pdef;
      begin
         inherited write_references;
         if (definition^.deftype=recorddef) then
            precdef(definition)^.symtable^.write_browser;
         if (definition^.deftype=objectdef) then
             pobjectdef(definition)^.publicsyms^.write_browser;
      end;

    procedure ttypesym.add_to_browserlog;
      var
        aktobjdef : pobjectdef;
      begin
         inherited add_to_browserlog;
         if (definition^.deftype=recorddef) then
            precdef(definition)^.symtable^.writebrowserlog;
         if (definition^.deftype=objectdef) then
             pobjectdef(definition)^.publicsyms^.writebrowserlog;
      end;
{$endif UseBrowser}

{$ifdef GDB}
    function ttypesym.stabstring : pchar;
    var stabchar : string[2];
        short : string;
    begin
      if definition^.deftype in tagtypes then
        stabchar := 'Tt'
      else
        stabchar := 't';
      short := '"'+name+':'+stabchar+definition^.numberstring
               +'",'+tostr(N_LSYM)+',0,'+tostr(fileinfo.line)+',0';
      stabstring := strpnew(short);
    end;

    procedure ttypesym.concatstabto(asmlist : paasmoutput);
      begin
      {not stabs for forward defs }
      if assigned(definition) then
        if (definition^.sym = @self) then
          definition^.concatstabto(asmlist)
        else
          inherited concatstabto(asmlist);
      end;
{$endif GDB}

{****************************************************************************
                                  TSYSSYM
****************************************************************************}

    constructor tsyssym.init(const n : string;l : longint);
      begin
         inherited init(n);
         typ:=syssym;
         number:=l;
      end;

    procedure tsyssym.write;
      begin
      end;

{$ifdef GDB}
    procedure tsyssym.concatstabto(asmlist : paasmoutput);
      begin
      end;
{$endif GDB}

{****************************************************************************
                                  TMACROSYM
****************************************************************************}

    constructor tmacrosym.init(const n : string);
      begin
         inherited init(n);
         defined:=true;
         buftext:=nil;
         buflen:=0;
      end;

    destructor tmacrosym.done;
      begin
         if assigned(buftext) then
           freemem(buftext,buflen);
         inherited done;
      end;


{
  $Log$
  Revision 1.39  1998-09-05 22:11:02  florian
    + switch -vb
    * while/repeat loops accept now also word/longbool conditions
    * makebooltojump did an invalid ungetregister32, fixed

  Revision 1.38  1998/09/01 12:53:26  peter
    + aktpackenum

  Revision 1.37  1998/09/01 07:54:25  pierre
    * UseBrowser a little updated (might still be buggy !!)
    * bug in psub.pas in function specifier removed
    * stdcall allowed in interface and in implementation
      (FPC will not yet complain if it is missing in either part
      because stdcall is only a dummy !!)

  Revision 1.36  1998/08/25 13:09:26  pierre
    * corrected mangling sheme :
      cvar add Cprefix to the mixed case name whereas
      export or public use direct name

  Revision 1.35  1998/08/25 12:42:46  pierre
    * CDECL changed to CVAR for variables
      specifications are read in structures also
    + started adding GPC compatibility mode ( option  -Sp)
    * names changed to lowercase

  Revision 1.34  1998/08/21 14:08:53  pierre
    + TEST_FUNCRET now default (old code removed)
      works also for m68k (at least compiles)

  Revision 1.33  1998/08/20 12:53:27  peter
    * object_options are always written for object syms

  Revision 1.32  1998/08/20 09:26:46  pierre
    + funcret setting in underproc testing
      compile with _dTEST_FUNCRET

  Revision 1.31  1998/08/17 10:10:12  peter
    - removed OLDPPU

  Revision 1.30  1998/08/13 10:57:29  peter
    * constant sets are now written correctly to the ppufile

  Revision 1.29  1998/08/11 15:31:42  peter
    * write extended to ppu file
    * new version 0.99.7

  Revision 1.28  1998/08/11 14:07:27  peter
    * fixed pushing of high value for openarray

  Revision 1.27  1998/08/10 14:50:31  peter
    + localswitches, moduleswitches, globalswitches splitting

  Revision 1.26  1998/08/10 10:18:35  peter
    + Compiler,Comphook unit which are the new interface units to the
      compiler

  Revision 1.25  1998/07/30 11:18:19  florian
    + first implementation of try ... except on .. do end;
    * limitiation of 65535 bytes parameters for cdecl removed

  Revision 1.24  1998/07/20 18:40:16  florian
    * handling of ansi string constants should now work

  Revision 1.23  1998/07/14 21:37:24  peter
    * fixed packrecords as discussed at the alias

  Revision 1.22  1998/07/14 14:47:08  peter
    * released NEWINPUT

  Revision 1.21  1998/07/13 21:17:38  florian
    * changed to compile with TP

  Revision 1.20  1998/07/10 00:00:05  peter
    * fixed ttypesym bug finally
    * fileinfo in the symtable and better using for unused vars

  Revision 1.19  1998/07/07 17:40:39  peter
    * packrecords 4 works
    * word aligning of parameters

  Revision 1.18  1998/07/07 11:20:15  peter
    + NEWINPUT for a better inputfile and scanner object

  Revision 1.17  1998/06/24 14:48:40  peter
    * ifdef newppu -> ifndef oldppu

  Revision 1.16  1998/06/19 15:40:42  peter
    * removed cosntructor/constructor warning and 0.99.5 recompiles it again

  Revision 1.15  1998/06/17 14:10:18  peter
    * small os2 fixes
    * fixed interdependent units with newppu (remake3 under linux works now)

  Revision 1.14  1998/06/16 08:56:34  peter
    + targetcpu
    * cleaner pmodules for newppu

  Revision 1.13  1998/06/15 15:38:10  pierre
    * small bug in systems.pas corrected
    + operators in different units better hanlded

  Revision 1.12  1998/06/15 14:23:44  daniel

  * Reverted my changes.

  Revision 1.10  1998/06/13 00:10:18  peter
    * working browser and newppu
    * some small fixes against crashes which occured in bp7 (but not in
      fpc?!)

  Revision 1.9  1998/06/12 16:15:35  pierre
    * external name 'C_var';
      export name 'intern_C_var';
      cdecl;
      cdecl;external;
      are now supported only with -Sv switch

  Revision 1.8  1998/06/11 10:11:59  peter
    * -gb works again

  Revision 1.7  1998/06/09 16:01:51  pierre
    + added procedure directive parsing for procvars
      (accepted are popstack cdecl and pascal)
    + added C vars with the following syntax
      var C calias 'true_c_name';(can be followed by external)
      reason is that you must add the Cprefix

      which is target dependent

  Revision 1.6  1998/06/08 22:59:53  peter
    * smartlinking works for win32
    * some defines to exclude some compiler parts

  Revision 1.5  1998/06/04 23:52:02  peter
    * m68k compiles
    + .def file creation moved to gendef.pas so it could also be used
      for win32

  Revision 1.4  1998/06/04 09:55:46  pierre
    * demangled name of procsym reworked to become independant of the mangling scheme

  Revision 1.3  1998/06/03 22:14:20  florian
    * problem with sizes of classes fixed (if the anchestor was declared
      forward, the compiler doesn't update the child classes size)

  Revision 1.2  1998/05/28 14:40:29  peter
    * fixes for newppu, remake3 works now with it

  Revision 1.1  1998/05/27 19:45:09  peter
    * symtable.pas splitted into includefiles
    * symtable adapted for $ifndef OLDPPU

}

