{
    Copyright (c) 1998-2010 by the Free Pascal team

    This unit implements the Jasmin assembler writer

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
{ Unit for writing Jasmin assembler (JVM bytecode) output.
}
unit agjasmin;

{$i fpcdefs.inc}

interface

    uses
      cclasses,
      globtype,globals,
      symbase,symdef,
      aasmbase,aasmtai,aasmdata,aasmcpu,
      assemble;

    type
      TJasminInstrWriter = class;
      {# This is a derived class which is used to write
         Jasmin-styled assembler.
      }

      { TJasminAssembler }

      TJasminAssembler=class(texternalassembler)
       protected
        jasminjar: tcmdstr;
        asmfiles: TCmdStrList;

        procedure WriteExtraHeader(obj: tobjectdef);
        procedure WriteInstruction(hp: tai);
        procedure NewAsmFileForObjectDef(obj: tobjectdef);
        procedure WriteProcDef(pd: tprocdef);
        procedure WriteSymtableProcdefs(st: TSymtable);
        procedure WriteSymtableObjectDefs(st: TSymtable);
       public
        constructor Create(smart: boolean); override;
        function MakeCmdLine: TCmdStr;override;
        procedure WriteTree(p:TAsmList);override;
        procedure WriteAsmList;override;
        destructor destroy; override;
       protected
        InstrWriter: TJasminInstrWriter;
      end;


      {# This is the base class for writing instructions.

         The WriteInstruction() method must be overridden
         to write a single instruction to the assembler
         file.
      }

      { TJasminInstrWriter }

      TJasminInstrWriter = class
        constructor create(_owner: TJasminAssembler);
        procedure WriteInstruction(hp : tai); virtual;
       protected
        owner: TJasminAssembler;
      end;


implementation

    uses
      SysUtils,
      cutils,cfileutl,systems,script,
      fmodule,finput,verbose,
      symconst,symtype,
      itcpujas,cpubase,cgutils,
      widestr
      ;

    const
      line_length = 70;

    type
      t64bitarray = array[0..7] of byte;
      t32bitarray = array[0..3] of byte;

{****************************************************************************}
{                          Support routines                                  }
{****************************************************************************}

   function fixline(s:string):string;
   {
     return s with all leading and ending spaces and tabs removed
   }
     var
       i,j,k : integer;
     begin
       i:=length(s);
       while (i>0) and (s[i] in [#9,' ']) do
        dec(i);
       j:=1;
       while (j<i) and (s[j] in [#9,' ']) do
        inc(j);
       for k:=j to i do
        if s[k] in [#0..#31,#127..#255] then
         s[k]:='.';
       fixline:=Copy(s,j,i-j+1);
     end;

{****************************************************************************}
{                       Jasmin Assembler writer                              }
{****************************************************************************}

    destructor TJasminAssembler.Destroy;
      begin
        InstrWriter.free;
        asmfiles.free;
        inherited destroy;
      end;


    procedure TJasminAssembler.WriteTree(p:TAsmList);
      var
        ch       : char;
        hp       : tai;
        hp1      : tailineinfo;
        constdef : taiconst_type;
        s,t      : string;
        i,pos,l  : longint;
        InlineLevel : longint;
        last_align : longint;
        co       : comp;
        sin      : single;
        d        : double;
        do_line  : boolean;

        sepChar : char;
      begin
        if not assigned(p) then
         exit;

        last_align := 2;
        InlineLevel:=0;
        { lineinfo is only needed for al_procedures (PFV) }
        do_line:=(cs_asm_source in current_settings.globalswitches) or
                 ((cs_lineinfo in current_settings.moduleswitches)
                   and (p=current_asmdata.asmlists[al_procedures]));
        hp:=tai(p.first);
        while assigned(hp) do
         begin
           prefetch(pointer(hp.next)^);
           if not(hp.typ in SkipLineInfo) then
            begin
              hp1 := hp as tailineinfo;
              current_filepos:=hp1.fileinfo;
               { no line info for inlined code }
               if do_line and (inlinelevel=0) then
                begin
                  { load infile }
                  if lastfileinfo.fileindex<>hp1.fileinfo.fileindex then
                   begin
                     infile:=current_module.sourcefiles.get_file(hp1.fileinfo.fileindex);
                     if assigned(infile) then
                      begin
                        { open only if needed !! }
                        if (cs_asm_source in current_settings.globalswitches) then
                         infile.open;
                      end;
                     { avoid unnecessary reopens of the same file !! }
                     lastfileinfo.fileindex:=hp1.fileinfo.fileindex;
                     { be sure to change line !! }
                     lastfileinfo.line:=-1;
                   end;
                { write source }
                  if (cs_asm_source in current_settings.globalswitches) and
                     assigned(infile) then
                   begin
                     if (infile<>lastinfile) then
                       begin
                         AsmWriteLn(target_asm.comment+'['+infile.name^+']');
                         if assigned(lastinfile) then
                           lastinfile.close;
                       end;
                     if (hp1.fileinfo.line<>lastfileinfo.line) and
                        ((hp1.fileinfo.line<infile.maxlinebuf) or (InlineLevel>0)) then
                       begin
                         if (hp1.fileinfo.line<>0) and
                            ((infile.linebuf^[hp1.fileinfo.line]>=0) or (InlineLevel>0)) then
                           AsmWriteLn(target_asm.comment+'['+tostr(hp1.fileinfo.line)+'] '+
                             fixline(infile.GetLineStr(hp1.fileinfo.line)));
                         { set it to a negative value !
                         to make that is has been read already !! PM }
                         if (infile.linebuf^[hp1.fileinfo.line]>=0) then
                           infile.linebuf^[hp1.fileinfo.line]:=-infile.linebuf^[hp1.fileinfo.line]-1;
                       end;
                   end;
                  lastfileinfo:=hp1.fileinfo;
                  lastinfile:=infile;
                end;
            end;

           case hp.typ of

             ait_comment :
               Begin
                 AsmWrite(target_asm.comment);
                 AsmWritePChar(tai_comment(hp).str);
                 AsmLn;
               End;

             ait_regalloc :
               begin
                 if (cs_asm_regalloc in current_settings.globalswitches) then
                   begin
                     AsmWrite(#9+target_asm.comment+'Register ');
                     repeat
                       AsmWrite(std_regname(Tai_regalloc(hp).reg));
                       if (hp.next=nil) or
                          (tai(hp.next).typ<>ait_regalloc) or
                          (tai_regalloc(hp.next).ratype<>tai_regalloc(hp).ratype) then
                         break;
                       hp:=tai(hp.next);
                       AsmWrite(',');
                     until false;
                     AsmWrite(' ');
                     AsmWriteLn(regallocstr[tai_regalloc(hp).ratype]);
                   end;
               end;

             ait_tempalloc :
               begin
                 if (cs_asm_tempalloc in current_settings.globalswitches) then
                   begin
  {$ifdef EXTDEBUG}
                     if assigned(tai_tempalloc(hp).problem) then
                       AsmWriteLn(target_asm.comment+'Temp '+tostr(tai_tempalloc(hp).temppos)+','+
                         tostr(tai_tempalloc(hp).tempsize)+' '+tai_tempalloc(hp).problem^)
                     else
  {$endif EXTDEBUG}
                       AsmWriteLn(target_asm.comment+'Temp '+tostr(tai_tempalloc(hp).temppos)+','+
                         tostr(tai_tempalloc(hp).tempsize)+' '+tempallocstr[tai_tempalloc(hp).allocation]);
                   end;
               end;

             ait_align :
               begin

               end;

             ait_section :
               begin

               end;

             ait_datablock :
               begin
                 internalerror(2010122701);
               end;

             ait_const:
               begin
                 AsmWriteln('constant');
//                 internalerror(2010122702);
               end;

             ait_real_64bit :
               begin
                 internalerror(2010122703);
               end;

             ait_real_32bit :
               begin
                 internalerror(2010122703);
               end;

             ait_comp_64bit :
               begin
                 internalerror(2010122704);
               end;

             ait_string :
               begin
                 pos:=0;
                  for i:=1 to tai_string(hp).len do
                   begin
                     if pos=0 then
                      begin
                        AsmWrite(#9'strconst: '#9'"');
                        pos:=20;
                      end;
                     ch:=tai_string(hp).str[i-1];
                     case ch of
                        #0, {This can't be done by range, because a bug in FPC}
                   #1..#31,
                #128..#255 : s:='\'+tostr(ord(ch) shr 6)+tostr((ord(ch) and 63) shr 3)+tostr(ord(ch) and 7);
                       '"' : s:='\"';
                       '\' : s:='\\';
                     else
                      s:=ch;
                     end;
                     AsmWrite(s);
                     inc(pos,length(s));
                     if (pos>line_length) or (i=tai_string(hp).len) then
                      begin
                        AsmWriteLn('"');
                        pos:=0;
                      end;
                   end;
               end;

             ait_label :
               begin
                 if (tai_label(hp).labsym.is_used) then
                  begin
                    AsmWrite(tai_label(hp).labsym.name);
                    AsmWriteLn(':');
                  end;
               end;

             ait_symbol :
               begin
                  if (tai_symbol(hp).sym.typ = AT_FUNCTION) then
                    begin
                      AsmWrite('.method ');
                      AsmWriteLn(tai_symbol(hp).sym.name);
                    end
                  else
                   begin
                     AsmWrite('data symbol: ');
                     AsmWriteln(tai_symbol(hp).sym.name);
//                     internalerror(2010122706);
                   end;
               end;
             ait_symbol_end :
               begin
                 AsmWriteLn('.end method');
                 AsmLn;
               end;

             ait_instruction :
               begin
                 WriteInstruction(hp);
               end;

             ait_force_line,
             ait_function_name : ;

             ait_cutobject :
               begin
               end;

             ait_marker :
               if tai_marker(hp).kind=mark_NoLineInfoStart then
                 inc(InlineLevel)
               else if tai_marker(hp).kind=mark_NoLineInfoEnd then
                 dec(InlineLevel);

             ait_directive :
               begin
                 AsmWrite('.'+directivestr[tai_directive(hp).directive]+' ');
                 if assigned(tai_directive(hp).name) then
                   AsmWrite(tai_directive(hp).name^);
                 AsmLn;
               end;

             else
               internalerror(2010122707);
           end;
           hp:=tai(hp.next);
         end;
      end;


    procedure TJasminAssembler.WriteExtraHeader(obj: tobjectdef);
      var
        superclass,
        intf: tobjectdef;
        n: string;
        i: longint;
      begin
        { JVM 1.5+ }
        AsmWriteLn('.bytecode 49.0');
        // include files are not support by Java, and the directory of the main
        // source file must not be specified
        if assigned(current_module.mainsource) then
          n:=ExtractFileName(current_module.mainsource^)
        else
          n:=InputFileName;
        AsmWriteLn('.source '+ExtractFileName(n));

        { class/interface name }
        if not assigned(obj) then
          begin
            { fake class type for unit -> name=unitname and
              superclass=java.lang.object }
            AsmWriteLn('.class '+ChangeFileExt(ExtractFileName(n),''));
            AsmWriteLn('.super java/lang/Object');
          end
        else
          begin
            case obj.objecttype of
              odt_javaclass:
                begin
                  AsmWriteLn('.class '+obj.objextname^);
                  superclass:=obj.childof;
                end;
              odt_interfacejava:
                begin
                  AsmWriteLn('.interface abstract '+obj.objextname^);
                  { interfaces must always specify Java.lang.object as
                    superclass }
                  superclass:=java_jlobject;
                end
              else
                internalerror(2011010906);
            end;
            { superclass }
            if assigned(superclass) then
              begin
                AsmWrite('.super ');
                if assigned(superclass.import_lib) then
                  AsmWrite(superclass.import_lib^+'/');
                AsmWriteln(superclass.objextname^);
              end;
            { implemented interfaces }
            if assigned(obj.ImplementedInterfaces) then
              begin
                for i:=0 to obj.ImplementedInterfaces.count-1 do
                  begin
                    intf:=TImplementedInterface(obj.ImplementedInterfaces[i]).IntfDef;
                    AsmWrite('.implements ');
                    if assigned(intf.import_lib) then
                      AsmWrite(intf.import_lib^+'/');
                    AsmWriteln(intf.objextname^);
                  end;
              end;
          end;
        AsmLn;
      end;


    procedure TJasminAssembler.WriteInstruction(hp: tai);
      begin
        InstrWriter.WriteInstruction(hp);
      end;


   function TJasminAssembler.MakeCmdLine: TCmdStr;
     const
       jasminjarname = 'jasmin.jar';
     var
       filenames: tcmdstr;
       jasminjarfound: boolean;
     begin
       if jasminjar='' then
         begin
           jasminjarfound:=false;
           if utilsdirectory<>'' then
             jasminjarfound:=FindFile(jasminjarname,utilsdirectory,false,jasminjar);
           if not jasminjarfound then
             jasminjarfound:=FindFileInExeLocations(jasminjarname,false,jasminjar);
           if (not jasminjarfound) and not(cs_asm_extern in current_settings.globalswitches) then
             begin
               Message1(exec_e_assembler_not_found,jasminjarname);
               current_settings.globalswitches:=current_settings.globalswitches+[cs_asm_extern];
             end;
           if jasminjarfound then
             Message1(exec_t_using_assembler,jasminjar);
         end;
       result:=target_asm.asmcmd;
       filenames:=maybequoted(ScriptFixFileName(AsmFileName));
       while not asmfiles.empty do
         filenames:=filenames+' '+asmfiles.GetFirst;
       Replace(result,'$ASM',filenames);
       if (path<>'') then
         Replace(result,'$OBJDIR',maybequoted(ScriptFixFileName(path)))
       else
         Replace(result,'$OBJDIR','.');
       Replace(result,'$JASMINJAR',maybequoted(ScriptFixFileName(jasminjar)));
     end;


   procedure TJasminAssembler.NewAsmFileForObjectDef(obj: tobjectdef);
      var
        enclosingobj: tobjectdef;
        st: tsymtable;
      begin
        if AsmSize<>AsmStartSize then
          begin
            AsmClose;
            asmfiles.Concat(maybequoted(ScriptFixFileName(AsmFileName)));
          end
        else
          AsmClear;

        AsmFileName:=obj.objextname^;
        st:=obj.owner;
        while assigned(st) and
              (st.symtabletype=objectsymtable) do
          begin
            { nested classes are named as "OuterClass$InnerClass" }
            enclosingobj:=tobjectdef(st.defowner);
            AsmFileName:=enclosingobj.objextname^+'$'+AsmFileName;
            st:=enclosingobj.owner;
          end;
        AsmFileName:=Path+FixFileName(AsmFileName)+target_info.asmext;
        AsmCreate(cut_normal);
      end;


    procedure TJasminAssembler.WriteProcDef(pd: tprocdef);
      begin
        { abstract method? }
        if is_javainterface(tdef(pd.owner.defowner)) or
           (po_abstractmethod in pd.procoptions) then
          begin
            AsmWrite('.method ');
            AsmWriteln(pd.mangledname(true));
            AsmWriteln('.end method');
            AsmLn;
            exit;
          end;

        WriteTree(pd.exprasmlist);
      end;

    procedure TJasminAssembler.WriteSymtableProcdefs(st: TSymtable);
      var
        i   : longint;
        def : tdef;
        obj : tobjectdef;
      begin
        if not assigned(st) then
          exit;
        for i:=0 to st.DefList.Count-1 do
          begin
            def:=tdef(st.DefList[i]);
            case def.typ of
              procdef :
                begin
                  { methods are also in the static/globalsymtable of the unit
                    -> make sure they are only written for the objectdefs that
                    own them }
                  if not(st.symtabletype in [staticsymtable,globalsymtable]) or
                     (def.owner=st) then
                    begin
                      WriteProcDef(tprocdef(def));
                      if assigned(tprocdef(def).localst) then
                        WriteSymtableProcdefs(tprocdef(def).localst);
                    end;
                end;
            end;
          end;
      end;

    procedure TJasminAssembler.WriteSymtableObjectDefs(st: TSymtable);
      var
        i   : longint;
        def : tdef;
        obj : tobjectdef;
        nestedclasses: tfpobjectlist;
      begin
        if not assigned(st) then
          exit;
        nestedclasses:=tfpobjectlist.create(false);
        for i:=0 to st.DefList.Count-1 do
          begin
            def:=tdef(st.DefList[i]);
            case def.typ of
              objectdef:
                if not(oo_is_external in tobjectdef(def).objectoptions) then
                  nestedclasses.add(def);
            end;
          end;
        for i:=0 to nestedclasses.count-1 do
          begin
            obj:=tobjectdef(nestedclasses[i]);
            NewAsmFileForObjectDef(obj);
            WriteExtraHeader(obj);
            WriteSymtableProcDefs(obj.symtable);
            WriteSymtableObjectDefs(obj.symtable);
          end;
        nestedclasses.free;
      end;

    constructor TJasminAssembler.Create(smart: boolean);
      begin
        inherited create(smart);
        InstrWriter:=TJasminInstrWriter.Create(self);
        asmfiles:=TCmdStrList.Create;
      end;


    procedure TJasminAssembler.WriteAsmList;
    var
      hal : tasmlisttype;
      i: longint;
    begin
{$ifdef EXTDEBUG}
      if assigned(current_module.mainsource) then
       Comment(V_Debug,'Start writing Jasmin-styled assembler output for '+current_module.mainsource^);
{$endif}

      AsmStartSize:=AsmSize;
      WriteExtraHeader(nil);
(*
      for hal:=low(TasmlistType) to high(TasmlistType) do
        begin
          AsmWriteLn(target_asm.comment+'Begin asmlist '+AsmlistTypeStr[hal]);
          writetree(current_asmdata.asmlists[hal]);
          AsmWriteLn(target_asm.comment+'End asmlist '+AsmlistTypeStr[hal]);
        end;
*)
      { print all global procedures/functions }
      WriteSymtableProcdefs(current_module.globalsymtable);
      WriteSymtableProcdefs(current_module.localsymtable);

      WriteSymtableObjectDefs(current_module.globalsymtable);
      WriteSymtableObjectDefs(current_module.localsymtable);

      AsmLn;
{$ifdef EXTDEBUG}
      if assigned(current_module.mainsource) then
       Comment(V_Debug,'Done writing gas-styled assembler output for '+current_module.mainsource^);
{$endif EXTDEBUG}
    end;

{****************************************************************************}
{                         Jasmin Instruction Writer                          }
{****************************************************************************}

     constructor TJasminInstrWriter.create(_owner: TJasminAssembler);
       begin
         inherited create;
         owner := _owner;
       end;

    function getreferencestring(var ref : treference) : string;
      begin
        if (ref.arrayreftype<>art_none) or
           (ref.index<>NR_NO) then
          internalerror(2010122809);
        if assigned(ref.symbol) then
          begin
            // global symbol -> full type/name
            if (ref.base<>NR_NO) or
               (ref.offset<>0) then
              internalerror(2010122811);
            result:=ref.symbol.name;
          end
        else
          begin
            // local symbol -> stack slot, stored in offset
            if ref.base<>NR_STACK_POINTER_REG then
              internalerror(2010122810);
            result:=tostr(ref.offset);
          end;
      end;


    function getopstr(const o:toper) : ansistring;
      var
        i,runstart,runlen: longint;
        num: string[4];
      begin
        case o.typ of
          top_reg:
            // should have been translated into a memory location by the
            // register allocator)
            if (cs_no_regalloc in current_settings.globalswitches) then
              getopstr:=std_regname(o.reg)
            else
              internalerror(2010122803);
          top_const:
            str(o.val,result);
          top_ref:
            getopstr:=getreferencestring(o.ref^);
          top_single:
            str(o.sval:0:20,result);
          top_double:
            begin
              str(o.dval:0:20,result);
              // force interpretation as double
              result:=result+'d';
            end;
          top_string:
            begin
              { escape control codes }
              runlen:=0;
              runstart:=0;
              for i:=1 to o.pcvallen do
                begin
                  if o.pcval[i]<#32 then
                    begin
                      if runlen>0 then
                        begin
                          setlength(result,length(result)+runlen);
                          move(result[length(result)-runlen],o.pcval[runstart],runlen);
                          runlen:=0;
                        end;
                      result:=result+'\u'+hexstr(ord(o.pcval[i]),4);
                    end
                  else if o.pcval[i]<#127 then
                    begin
                      if runlen=0 then
                        runstart:=i;
                      inc(runlen);
                    end
                  else
                    // since Jasmin expects an UTF-16 string, we can't safely
                    // have high ASCII characters since they'll be
                    // re-interpreted as utf-16 anyway
                    internalerror(2010122808);
                end;
              if runlen>0 then
                begin
                  setlength(result,length(result)+runlen);
                  move(result[length(result)-runlen],o.pcval[runstart],runlen);
                end;
            end;
          top_wstring:
            begin
              { escape control codes }
              for i:=1 to getlengthwidestring(o.pwstrval) do
                begin
                  if (o.pwstrval^.data[i]<32) or
                     (o.pwstrval^.data[i]>127) then
                    result:=result+'\u'+hexstr(o.pwstrval^.data[i],4)
                  else
                    result:=result+char(o.pwstrval^.data[i]);
                end;
            end
          else
            internalerror(2010122802);
        end;
      end;


    procedure TJasminInstrWriter.WriteInstruction(hp: tai);
      var
        s: ansistring;
        i: byte;
        sep: string[3];
      begin
        s:=#9+jas_op2str[taicpu(hp).opcode];
        if taicpu(hp).ops<>0 then
          begin
            sep:=#9;
            for i:=0 to taicpu(hp).ops-1 do
              begin
                 s:=s+sep+getopstr(taicpu(hp).oper[i]^);
                 sep:=',';
              end;
          end;
        owner.AsmWriteLn(s);
      end;

{****************************************************************************}
{                         Jasmin Instruction Writer                          }
{****************************************************************************}

  const
    as_jvm_jasmin_info : tasminfo =
       (
         id     : as_jvm_jasmin;
         idtxt  : 'Jasmin';
         asmbin : 'java';
         asmcmd : '-jar $JASMINJAR $ASM -d $OBJDIR';
         supported_targets : [system_jvm_java32];
         flags : [];
         labelprefix : 'L';
         comment : ' ; ';
       );


begin
  RegisterAssembler(as_jvm_jasmin_info,TJasminAssembler);
end.
