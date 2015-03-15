{
    Copyright (c) 1998-2013 by the Free Pascal team

    This unit implements the generic part of the LLVM IR writer

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
unit agllvm;

{$i fpcdefs.inc}

interface

    uses
      globtype,globals,
      aasmbase,aasmtai,aasmdata,
      assemble;

    type
      TLLVMInstrWriter = class;
      { TLLVMAssember }

      TLLVMAssember=class(texternalassembler)
      protected
        fdecllevel: longint;

        procedure WriteExtraHeader;virtual;
        procedure WriteExtraFooter;virtual;
        procedure WriteInstruction(hp: tai);
        procedure WriteLlvmInstruction(hp: tai);
//        procedure WriteWeakSymbolDef(s: tasmsymbol); virtual;
        procedure WriteDirectiveName(dir: TAsmDirective); virtual;
        procedure WriteWeakSymbolDef(s: tasmsymbol);
        procedure WriteRealConst(hp: tai_realconst; do_line: boolean);
        procedure WriteOrdConst(hp: tai_const);
        procedure WriteTai(const replaceforbidden: boolean; const do_line: boolean; var InlineLevel: cardinal; var hp: tai);
       public
        constructor create(smart: boolean); override;
        procedure AsmLn; override;
        function MakeCmdLine: TCmdStr; override;
        procedure WriteTree(p:TAsmList);override;
        procedure WriteAsmList;override;
        destructor destroy; override;
       protected
        InstrWriter: TLLVMInstrWriter;
      end;


      {# This is the base class for writing instructions.

         The WriteInstruction() method must be overridden
         to write a single instruction to the assembler
         file.
      }
      TLLVMInstrWriter = class
        constructor create(_owner: TLLVMAssember);
        procedure WriteInstruction(hp : tai);
       protected
        owner: TLLVMAssember;
        fstr: TSymStr;

        function getopstr(const o:toper; refwithalign: boolean) : TSymStr;
      end;


implementation

    uses
      SysUtils,
      cutils,cfileutl,systems,
      fmodule,verbose,
      aasmcnst,symconst,symdef,
      llvmbase,aasmllvm,itllvm,llvmdef,
      cgbase,cgutils,cpubase;

    const
      line_length = 70;

    type
{$ifdef cpuextended}
      t80bitarray = array[0..9] of byte;
{$endif cpuextended}
      t64bitarray = array[0..7] of byte;
      t32bitarray = array[0..3] of byte;

{****************************************************************************}
{                          Support routines                                  }
{****************************************************************************}

    function single2str(d : single) : string;
      var
         hs : string;
      begin
         str(d,hs);
      { replace space with + }
         if hs[1]=' ' then
          hs[1]:='+';
         single2str:=hs
      end;

    function double2str(d : double) : string;
      var
         hs : string;
      begin
         str(d,hs);
      { replace space with + }
         if hs[1]=' ' then
          hs[1]:='+';
         double2str:=hs
      end;

    function extended2str(e : extended) : string;
      var
         hs : string;
      begin
         str(e,hs);
      { replace space with + }
         if hs[1]=' ' then
          hs[1]:='+';
         extended2str:=hs
      end;




 {****************************************************************************}
 {                        LLVM Instruction writer                             }
 {****************************************************************************}

    function getregisterstring(reg: tregister): ansistring;
      begin
        if getregtype(reg)=R_TEMPREGISTER then
          result:='%tmp.'
        else
          result:='%reg.'+tostr(byte(getregtype(reg)))+'_';
        result:=result+tostr(getsupreg(reg));
      end;


    function getreferencealignstring(var ref: treference) : ansistring;
      begin
        result:=', align '+tostr(ref.alignment);
      end;


    function getreferencestring(var ref : treference; withalign: boolean) : ansistring;
      begin
        result:='';
        if assigned(ref.relsymbol) or
           (assigned(ref.symbol) =
            (ref.base<>NR_NO)) or
           (ref.index<>NR_NO) or
           (ref.offset<>0) then
          begin
            result:=' **(error ref: ';
            if assigned(ref.symbol) then
              result:=result+'sym='+ref.symbol.name+', ';
            if assigned(ref.relsymbol) then
              result:=result+'sym='+ref.relsymbol.name+', ';
            if ref.base=NR_NO then
              result:=result+'base=NR_NO, ';
            if ref.index<>NR_NO then
              result:=result+'index<>NR_NO, ';
            if ref.offset<>0 then
              result:=result+'offset='+tostr(ref.offset);
            result:=result+')**'
//            internalerror(2013060225);
          end;
         if ref.base<>NR_NO then
           result:=result+getregisterstring(ref.base)
         else
           result:=result+LlvmAsmSymName(ref.symbol);
         if withalign then
           result:=result+getreferencealignstring(ref);
      end;


   function getparas(const o: toper): ansistring;
     var
       i: longint;
       para: pllvmcallpara;
     begin
       result:='(';
       for i:=0 to o.paras.count-1 do
         begin
           if i<>0 then
             result:=result+', ';
           para:=pllvmcallpara(o.paras[i]);
           result:=result+llvmencodetype(para^.def);
           if para^.valueext<>lve_none then
             result:=result+llvmvalueextension2str[para^.valueext];
           case para^.loc of
             LOC_REGISTER,
             LOC_FPUREGISTER,
             LOC_MMREGISTER:
               result:=result+' '+getregisterstring(para^.reg);
             else
               internalerror(2014010801);
           end;
         end;
       result:=result+')';
     end;


   function llvmdoubletostr(const d: double): TSymStr;
     type
       tdoubleval = record
         case byte of
           1: (d: double);
           2: (i: int64);
       end;
     begin
       { "When using the hexadecimal form, constants of types half,
         float, and double are represented using the 16-digit form shown
         above (which matches the IEEE754 representation for double)"

         And always in big endian form (sign bit leftmost)
       }
       result:='0x'+hexstr(tdoubleval(d).i,16);
     end;


{$if defined(cpuextended) and defined(FPC_HAS_TYPE_EXTENDED)}
    function llvmextendedtostr(const e: extended): TSymStr;
      var
        extendedval: record
          case byte of
            1: (e: extended);
            2: (r: packed record
      {$ifdef FPC_LITTLE_ENDIAN}
                  l: int64;
                  h: word;
      {$else FPC_LITTLE_ENDIAN}
                  h: int64;
                  l: word;
      {$endif FPC_LITTLE_ENDIAN}
                end;
               );
        end;
      begin
        extendedval.e:=e;
        { hex format is always big endian in llvm }
        result:='0xK'+hexstr(extendedval.r.h,sizeof(extendedval.r.h)*2)+
                      hexstr(extendedval.r.l,sizeof(extendedval.r.l)*2);
      end;

{$endif cpuextended}


   function TLLVMInstrWriter.getopstr(const o:toper; refwithalign: boolean) : TSymStr;
     var
       hs : ansistring;
       hp: tai;
       tmpinline: cardinal;
     begin
       case o.typ of
         top_reg:
           getopstr:=getregisterstring(o.reg);
         top_const:
           getopstr:=tostr(int64(o.val));
         top_ref:
           if o.ref^.refaddr=addr_full then
             begin
               getopstr:='';
               getopstr:=LlvmAsmSymName(o.ref^.symbol);
               if o.ref^.offset<>0 then
                 internalerror(2013060223);
             end
           else
             getopstr:=getreferencestring(o.ref^,refwithalign);
         top_def:
           begin
             getopstr:=llvmencodetype(o.def);
           end;
         top_cond:
           begin
             getopstr:=llvm_cond2str[o.cond];
           end;
         top_fpcond:
           begin
             getopstr:=llvm_fpcond2str[o.fpcond];
           end;
         top_single,
         top_double:
           begin
             { "When using the hexadecimal form, constants of types half,
               float, and double are represented using the 16-digit form shown
               above (which matches the IEEE754 representation for double)"

               And always in big endian form (sign bit leftmost)
             }
             if o.typ=top_double then
               result:=llvmdoubletostr(o.dval)
             else
               result:=llvmdoubletostr(o.sval)
           end;
         top_para:
           begin
             result:=getparas(o);
           end;
         top_tai:
           begin
             tmpinline:=1;
             hp:=o.ai;
             owner.AsmWrite(fstr);
             fstr:='';
             owner.WriteTai(false,false,tmpinline,hp);
             result:='';
           end;
{$if defined(cpuextended) and defined(FPC_HAS_TYPE_EXTENDED)}
         top_extended80:
           begin
             result:=llvmextendedtostr(o.eval);
           end;
{$endif cpuextended}
         else
           internalerror(2013060227);
       end;
     end;


  procedure TLLVMInstrWriter.WriteInstruction(hp: tai);
    var
      op: tllvmop;
      sep: TSymStr;
      i, opstart: byte;
      nested: boolean;
      done: boolean;
    begin
      op:=taillvm(hp).llvmopcode;
      { we write everything immediately rather than adding it into a string,
        because operands may contain other tai that will also write things out
        (and their output must come after everything that was processed in this
         instruction, such as its opcode or previous operands) }
      if owner.fdecllevel=0 then
        owner.AsmWrite(#9);
      sep:=' ';
      done:=false;
      opstart:=0;
      nested:=false;
      case op of
        la_ret, la_br, la_switch, la_indirectbr,
        la_invoke, la_resume,
        la_unreachable,
        la_store,
        la_fence,
        la_cmpxchg,
        la_atomicrmw:
          begin
            { instructions that never have a result }
          end;
        la_call:
          begin
            if taillvm(hp).oper[0]^.reg<>NR_NO then
              owner.AsmWrite(getregisterstring(taillvm(hp).oper[0]^.reg)+' = ');
            sep:=' ';
            opstart:=1;
          end;
        la_alloca:
          begin
            owner.AsmWrite(getreferencestring(taillvm(hp).oper[0]^.ref^,false)+' = ');
            sep:=' ';
            opstart:=1;
          end;
        la_trunc, la_zext, la_sext, la_fptrunc, la_fpext,
        la_fptoui, la_fptosi, la_uitofp, la_sitofp,
        la_ptrtoint, la_inttoptr,
        la_bitcast:
          begin
            { destination can be empty in case of nested constructs, or
              data initialisers }
            if (taillvm(hp).oper[0]^.typ<>top_reg) or
               (taillvm(hp).oper[0]^.reg<>NR_NO) then
              owner.AsmWrite(getopstr(taillvm(hp).oper[0]^,false)+' = ')
            else
              nested:=true;
            owner.AsmWrite(llvm_op2str[op]);
            if not nested then
              owner.AsmWrite(' ')
            else
              owner.AsmWrite(' (');
            owner.AsmWrite(getopstr(taillvm(hp).oper[1]^,false));
            { if there's a tai operand, its def is used instead of an
              explicit def operand }
            if taillvm(hp).ops=4 then
              begin
                owner.AsmWrite(' ');
                owner.AsmWrite(getopstr(taillvm(hp).oper[2]^,false));
                opstart:=3;
              end
            else
              opstart:=2;
            owner.AsmWrite(' to ');
            owner.AsmWrite(getopstr(taillvm(hp).oper[opstart]^,false));
            done:=true;
          end
        else
          begin
            if (taillvm(hp).oper[0]^.typ<>top_reg) or
               (taillvm(hp).oper[0]^.reg<>NR_NO) then
              begin
                owner.AsmWrite(getopstr(taillvm(hp).oper[0]^,true)+' = ');
              end
            else
              nested:=true;
            sep:=' ';
            opstart:=1
          end;
      end;
      { process operands }
      if not done then
        begin
          owner.AsmWrite(llvm_op2str[op]);
          if nested then
            owner.AsmWrite(' (');
          if taillvm(hp).ops<>0 then
            begin
              for i:=opstart to taillvm(hp).ops-1 do
                begin
                   owner.AsmWrite(sep);
                   owner.AsmWrite(getopstr(taillvm(hp).oper[i]^,op in [la_load,la_store]));
                   if (taillvm(hp).oper[i]^.typ in [top_def,top_cond,top_fpcond]) or
                      (op=la_call) then
                     sep :=' '
                   else
                     sep:=', ';
                end;
            end;
        end;
      if op=la_alloca then
        owner.AsmWrite(getreferencealignstring(taillvm(hp).oper[0]^.ref^));
      if nested then
        owner.AsmWrite(')');
      owner.AsmLn;
    end;

{****************************************************************************}
{                          LLVM Assembler writer                              }
{****************************************************************************}

    destructor TLLVMAssember.Destroy;
      begin
        InstrWriter.free;
        inherited destroy;
      end;


    function TLLVMAssember.MakeCmdLine: TCmdStr;
      var
        optstr: TCmdStr;
      begin
        result := inherited MakeCmdLine;
        { standard optimization flags for llc -- todo: this needs to be split
          into a call to opt and one to llc }
        if cs_opt_level3 in current_settings.optimizerswitches then
          optstr:='-O3'
        else if cs_opt_level2 in current_settings.optimizerswitches then
          optstr:='-O2'
        else if cs_opt_level1 in current_settings.optimizerswitches then
          optstr:='-O1'
        else
          optstr:='-O0';
        { stack frame elimination }
        if not(cs_opt_stackframe in current_settings.optimizerswitches) then
          optstr:=optstr+' -disable-fp-elim';
        { fast math }
        if cs_opt_fastmath in current_settings.optimizerswitches then
          optstr:=optstr+' -enable-unsafe-fp-math -enable-fp-mad -fp-contract=fast';
        { smart linking }
        if cs_create_smart in current_settings.moduleswitches then
          optstr:=optstr+' -fdata-sections -fcode-sections';
        { pic }
        if cs_create_pic in current_settings.moduleswitches then
          optstr:=optstr+' -relocation-model=pic'
        else if not(target_info.system in systems_darwin) then
          optstr:=optstr+' -relocation-model=static'
        else
          optstr:=optstr+' -relocation-model=dynamic-no-pic';
        { our stack alignment is non-standard on some targets. The following
          parameter is however ignored on some targets by llvm, so it may not
          be enough }
        optstr:=optstr+' -stack-alignment='+tostr(target_info.stackalign*8);
        { force object output instead of textual assembler code }
        optstr:=optstr+' -filetype=obj';
        replace(result,'$OPT',optstr);
      end;


    procedure TLLVMAssember.WriteTree(p:TAsmList);
    var
      hp       : tai;
      InlineLevel : cardinal;
      do_line  : boolean;
      replaceforbidden: boolean;
    begin
      if not assigned(p) then
       exit;
      replaceforbidden:=target_asm.dollarsign<>'$';

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
            current_filepos:=tailineinfo(hp).fileinfo;
            { no line info for inlined code }
            if do_line and (inlinelevel=0) then
              WriteSourceLine(hp as tailineinfo);
          end;

         WriteTai(replaceforbidden, do_line, InlineLevel, hp);
         hp:=tai(hp.next);
       end;
    end;


    procedure TLLVMAssember.WriteExtraHeader;
      begin
        AsmWrite('target datalayout = "');
        AsmWrite(target_info.llvmdatalayout);
        AsmWriteln('"');
        AsmWrite('target triple = "');
        AsmWrite(llvm_target_name);
        AsmWriteln('"');
      end;


    procedure TLLVMAssember.WriteExtraFooter;
      begin
      end;


    procedure TLLVMAssember.WriteInstruction(hp: tai);
      begin

      end;


    procedure TLLVMAssember.WriteLlvmInstruction(hp: tai);
      begin
        InstrWriter.WriteInstruction(hp);
      end;


    procedure TLLVMAssember.WriteWeakSymbolDef(s: tasmsymbol);
      begin
        AsmWriteLn(#9'.weak '+LlvmAsmSymName(s));
      end;


    procedure TLLVMAssember.WriteRealConst(hp: tai_realconst; do_line: boolean);
      begin
        if do_line and
           (fdecllevel=0) then
          begin
            case tai_realconst(hp).realtyp of
              aitrealconst_s32bit:
                AsmWriteLn(target_asm.comment+'value: '+single2str(tai_realconst(hp).value.s32val));
              aitrealconst_s64bit:
                AsmWriteLn(target_asm.comment+'value: '+double2str(tai_realconst(hp).value.s64val));
{$if defined(cpuextended) and defined(FPC_HAS_TYPE_EXTENDED)}
              { can't write full 80 bit floating point constants yet on non-x86 }
              aitrealconst_s80bit:
                AsmWriteLn(target_asm.comment+'value: '+extended2str(tai_realconst(hp).value.s80val));
{$endif cpuextended}
              aitrealconst_s64comp:
                AsmWriteLn(target_asm.comment+'value: '+extended2str(tai_realconst(hp).value.s64compval));
              else
                internalerror(2014050604);
            end;
          end;
        case hp.realtyp of
          aitrealconst_s32bit:
            AsmWriteln(llvmdoubletostr(hp.value.s32val));
          aitrealconst_s64bit:
            AsmWriteln(llvmdoubletostr(hp.value.s64val));
{$if defined(cpuextended) and defined(FPC_HAS_TYPE_EXTENDED)}
          aitrealconst_s80bit:
            AsmWriteln(llvmextendedtostr(hp.value.s80val));
{$endif defined(cpuextended)}
          aitrealconst_s64comp:
            { handled as int64 most of the time in llvm }
            AsmWriteln(tostr(round(hp.value.s64compval)));
          else
            internalerror(2014062401);
        end;
      end;


    procedure TLLVMAssember.WriteOrdConst(hp: tai_const);
      var
        consttyp: taiconst_type;
      begin
        if fdecllevel=0 then
          asmwrite(target_asm.comment+' const ');
        consttyp:=hp.consttype;
        case consttyp of
          aitconst_got,
          aitconst_gotoff_symbol,
          aitconst_uleb128bit,
          aitconst_sleb128bit,
          aitconst_rva_symbol,
          aitconst_secrel32_symbol,
          aitconst_darwin_dwarf_delta32,
          aitconst_darwin_dwarf_delta64,
          aitconst_half16bit:
            internalerror(2014052901);
          aitconst_128bit,
          aitconst_64bit,
          aitconst_32bit,
          aitconst_16bit,
          aitconst_8bit,
          aitconst_16bit_unaligned,
          aitconst_32bit_unaligned,
          aitconst_64bit_unaligned:
            begin
              if fdecllevel=0 then
                AsmWrite(target_asm.comment);
              { can't have compile-time differences between symbols; these are
                normally for PIC, but llvm takes care of that for us }
              if assigned(hp.endsym) then
                internalerror(2014052902);
              if assigned(hp.sym) then
                begin
                  AsmWrite(LlvmAsmSymName(hp.sym));
                  { can't have offsets }
                  if hp.value<>0 then
                    if fdecllevel<>0 then
                      internalerror(2014052903)
                    else
                      asmwrite(' -- symbol offset: ' + tostr(hp.value));
                end
              else if hp.value=0 then
                AsmWrite('zeroinitializer')
              else
                AsmWrite(tostr(hp.value));
              AsmLn;
            end;
          else
            internalerror(200704251);
        end;
      end;


    procedure TLLVMAssember.WriteTai(const replaceforbidden: boolean; const do_line: boolean; var InlineLevel: cardinal; var hp: tai);

      procedure WriteTypedConstData(hp: tai_abstracttypedconst);
        var
          p: tai_abstracttypedconst;
          pval: tai;
          defstr: TSymStr;
          first, gotstring: boolean;
        begin
          defstr:=llvmencodetype(hp.def);
          { write the struct, array or simple type }
          case hp.adetyp of
            tck_record:
              begin
                AsmWrite(defstr);
                AsmWrite(' ');
                AsmWrite('<{');
                first:=true;
                for p in tai_aggregatetypedconst(hp) do
                  begin
                    if not first then
                      AsmWrite(', ')
                    else
                      first:=false;
                    WriteTypedConstData(p);
                  end;
                AsmWrite('}>');
              end;
            tck_array:
              begin
                AsmWrite(defstr);
                first:=true;
                gotstring:=false;
                for p in tai_aggregatetypedconst(hp) do
                  begin
                    if not first then
                      AsmWrite(',')
                    else
                      begin
                        AsmWrite(' ');
                        if (tai_abstracttypedconst(p).adetyp=tck_simple) and
                           (tai_simpletypedconst(p).val.typ=ait_string) then
                          begin
                            gotstring:=true;
                          end
                        else
                          begin
                            AsmWrite('[');
                          end;
                        first:=false;
                      end;
                    { cannot concat strings and other things }
                    if gotstring and
                       ((tai_abstracttypedconst(p).adetyp<>tck_simple) or
                        (tai_simpletypedconst(p).val.typ<>ait_string)) then
                      internalerror(2014062701);
                    WriteTypedConstData(p);
                  end;
                if not gotstring then
                  AsmWrite(']');
              end;
            tck_simple:
              begin
                pval:=tai_simpletypedconst(hp).val;
                if pval.typ<>ait_string then
                  begin
                    AsmWrite(defstr);
                    AsmWrite(' ');
                  end;
                WriteTai(replaceforbidden,do_line,InlineLevel,pval);
              end;
          end;
        end;

      var
        hp2: tai;
        s: string;
        i: longint;
        ch: ansichar;
      begin
        case hp.typ of
          ait_comment :
            begin
              AsmWrite(target_asm.comment);
              AsmWritePChar(tai_comment(hp).str);
              AsmLn;
            end;

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
                WriteTempalloc(tai_tempalloc(hp));
            end;

          ait_align,
          ait_section :
            begin
              { ignore, specified as part of declarations -- don't write
                comment, because could appear in the middle of an aggregate
                constant definition }
            end;

          ait_datablock :
            begin
              AsmWrite(target_asm.comment);
              AsmWriteln('datablock');
            end;

          ait_const:
            begin
              WriteOrdConst(tai_const(hp));
            end;

          ait_realconst :
            begin
              WriteRealConst(tai_realconst(hp), do_line);
            end;

          ait_string :
            begin
              if fdecllevel=0 then
                AsmWrite(target_asm.comment);
              AsmWrite('c"');
              for i:=1 to tai_string(hp).len do
               begin
                 ch:=tai_string(hp).str[i-1];
                 case ch of
                           #0, {This can't be done by range, because a bug in FPC}
                      #1..#31,
                   #128..#255,
                          '"',
                          '\' : s:='\'+hexStr(ord(ch),2);
                 else
                   s:=ch;
                 end;
                 AsmWrite(s);
               end;
              AsmWriteLn('"');
            end;

          ait_label :
            begin
              if (tai_label(hp).labsym.is_used) then
                begin
                  if (tai_label(hp).labsym.bind=AB_PRIVATE_EXTERN) then
                    begin
                     { should be emitted as part of the variable/function def }
                     internalerror(2013010703);
                   end;
                 if tai_label(hp).labsym.bind in [AB_GLOBAL, AB_PRIVATE_EXTERN] then
                   begin
                     { should be emitted as part of the variable/function def }
                     //internalerror(2013010704);
                     AsmWriteln(target_asm.comment+'global/privateextern label: '+tai_label(hp).labsym.name);
                   end;
                 if replaceforbidden then
                   AsmWrite(ReplaceForbiddenAsmSymbolChars(tai_label(hp).labsym.name))
                 else
                   AsmWrite(tai_label(hp).labsym.name);
                 AsmWriteLn(':');
               end;
            end;

          ait_symbol :
            begin
              if fdecllevel=0 then
                AsmWrite(target_asm.comment);
              AsmWriteln(LlvmAsmSymName(tai_symbol(hp).sym));
              { todo }
              if tai_symbol(hp).has_value then
                internalerror(2014062402);
            end;
          ait_llvmdecl:
            begin
              if taillvmdecl(hp).def.typ=procdef then
                begin
                  if taillvmdecl(hp).namesym.bind in [AB_EXTERNAL, AB_WEAK_EXTERNAL] then
                    begin
                      asmwrite('declare');
                      asmwriteln(llvmencodeproctype(tprocdef(taillvmdecl(hp).def), taillvmdecl(hp).namesym.name, lpd_decl));
                    end
                  else
                    begin
                      asmwrite('define');
                      asmwrite(llvmencodeproctype(tprocdef(taillvmdecl(hp).def), '', lpd_decl));
                      asmwriteln(' {');
                    end;
                end
              else
                begin
                  asmwrite(LlvmAsmSymName(taillvmdecl(hp).namesym));
                  case taillvmdecl(hp).namesym.bind of
                    AB_EXTERNAL:
                      asmwrite(' = external ');
                    AB_COMMON:
                      asmwrite(' = common ');
                    AB_LOCAL:
                      asmwrite(' = internal ');
                    AB_GLOBAL:
                      asmwrite(' = ');
                    AB_WEAK_EXTERNAL:
                      asmwrite(' = extern_weak ');
                    AB_PRIVATE_EXTERN:
                      asmwrite('= linker_private ');
                    else
                      internalerror(2014020104);
                  end;
                  if taillvmdecl(hp).tls then
                    asmwrite('thread_local ');
                  { todo: handle more different section types (mainly
                      Objective-C }
                  if taillvmdecl(hp).sec in [sec_rodata,sec_rodata_norel] then
                    asmwrite('unnamed_addr constant ')
                  else
                    asmwrite('global ');
                  if not assigned(taillvmdecl(hp).initdata) then
                    begin
                      asmwrite(llvmencodetype(taillvmdecl(hp).def));
                      if not(taillvmdecl(hp).namesym.bind in [AB_EXTERNAL, AB_WEAK_EXTERNAL]) then
                        asmwrite(' zeroinitializer');
                    end
                  else
                    begin
                      inc(fdecllevel);
                      { can't have an external symbol with initialisation data }
                      if taillvmdecl(hp).namesym.bind in [AB_EXTERNAL, AB_WEAK_EXTERNAL] then
                        internalerror(2014052905);
                      { bitcast initialisation data to the type of the constant }
                      { write initialisation data }
                      hp2:=tai(taillvmdecl(hp).initdata.first);
                      while assigned(hp2) do
                        begin
                          WriteTai(replaceforbidden,do_line,InlineLevel,hp2);
                          hp2:=tai(hp2.next);
                        end;
                      dec(fdecllevel);
                    end;
                  { alignment }
                  asmwrite(', align ');
                  asmwriteln(tostr(taillvmdecl(hp).alignment));
                end;
            end;
          ait_llvmalias:
            begin
              asmwrite(LlvmAsmSymName(taillvmalias(hp).newsym));
              asmwrite(' = alias ');
              if taillvmalias(hp).linkage<>lll_default then
                begin
                  str(taillvmalias(hp).linkage, s);
                  asmwrite(copy(s, length('lll_'), 255));
                  asmwrite(' ');
                end
              else
                asmwrite('external ');
              if taillvmalias(hp).vis<>llv_default then
                begin
                  str(taillvmalias(hp).vis, s);
                  asmwrite(copy(s, length('llv_'), 255));
                  asmwrite(' ');
                end;
              asmwrite(llvmencodeproctype(tabstractprocdef(taillvmalias(hp).def), '', lpd_alias));
              asmwrite('* ');
              asmwriteln(LlvmAsmSymName(taillvmalias(hp).oldsym));
            end;
          ait_symbolpair:
            begin
              { should be emitted as part of the symbol def }
              internalerror(2013010708);
            end;

          ait_weak:
            begin
              { should be emitted as part of the symbol def }
              internalerror(2013010709);
            end;

          ait_symbol_end :
            begin
              if tai_symbol_end(hp).sym.typ=AT_FUNCTION then
                asmwriteln('}')
              else
                asmwriteln('; ait_symbol_end error, should not be generated');
//                internalerror(2013010711);
            end;

          ait_instruction :
            begin
              WriteInstruction(hp);
            end;

          ait_llvmins:
            begin
              WriteLlvmInstruction(hp);
            end;

          ait_stab :
            begin
              internalerror(2013010712);
            end;

          ait_force_line,
          ait_function_name :
            ;

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
              WriteDirectiveName(tai_directive(hp).directive);
              if tai_directive(hp).name <>'' then
                AsmWrite(tai_directive(hp).name);
              AsmLn;
            end;

          ait_seh_directive :
            begin
              internalerror(2013010713);
            end;
          ait_varloc:
            begin
              if tai_varloc(hp).newlocationhi<>NR_NO then
                AsmWrite(strpnew('Var '+tai_varloc(hp).varsym.realname+' located in register '+
                  std_regname(tai_varloc(hp).newlocationhi)+':'+std_regname(tai_varloc(hp).newlocation)))
              else
                AsmWrite(strpnew('Var '+tai_varloc(hp).varsym.realname+' located in register '+
                  std_regname(tai_varloc(hp).newlocation)));
              AsmLn;
            end;
           ait_typedconst:
             begin
               WriteTypedConstData(tai_abstracttypedconst(hp));
             end
          else
            internalerror(2006012201);
        end;
      end;


    constructor TLLVMAssember.create(smart: boolean);
      begin
        inherited create(smart);
        InstrWriter:=TLLVMInstrWriter.create(self);
      end;


    procedure TLLVMAssember.AsmLn;
      begin
        { don't write newlines in the middle of declarations }
        if fdecllevel=0 then
          inherited AsmLn;
      end;



    procedure TLLVMAssember.WriteDirectiveName(dir: TAsmDirective);
      begin
        AsmWrite('.'+directivestr[dir]+' ');
      end;


    procedure TLLVMAssember.WriteAsmList;
      var
        n : string;
        hal : tasmlisttype;
        i: longint;
      begin

        if current_module.mainsource<>'' then
          n:=ExtractFileName(current_module.mainsource)
        else
          n:=InputFileName;

        { gcc does not add it either for Darwin. Grep for
          TARGET_ASM_FILE_START_FILE_DIRECTIVE in gcc/config/*.h
        }
        if not(target_info.system in systems_darwin) then
          AsmWriteLn(#9'.file "'+FixFileName(n)+'"');

        WriteExtraHeader;
        AsmStartSize:=AsmSize;

        for hal:=low(TasmlistType) to high(TasmlistType) do
          begin
            AsmWriteLn(target_asm.comment+'Begin asmlist '+AsmlistTypeStr[hal]);
            writetree(current_asmdata.asmlists[hal]);
            AsmWriteLn(target_asm.comment+'End asmlist '+AsmlistTypeStr[hal]);
          end;

        { add weak symbol markers }
        for i:=0 to current_asmdata.asmsymboldict.count-1 do
          if (tasmsymbol(current_asmdata.asmsymboldict[i]).bind=AB_WEAK_EXTERNAL) then
            writeweaksymboldef(tasmsymbol(current_asmdata.asmsymboldict[i]));

        AsmLn;
      end;



{****************************************************************************}
{                        Abstract Instruction Writer                         }
{****************************************************************************}

     constructor TLLVMInstrWriter.create(_owner: TLLVMAssember);
       begin
         inherited create;
         owner := _owner;
       end;


   const
     as_llvm_info : tasminfo =
        (
          id     : as_llvm;

          idtxt  : 'LLVM-AS';
          asmbin : 'llc';
          asmcmd: '$OPT -o $OBJ $ASM';
          supported_targets : [system_x86_64_linux,system_x86_64_darwin,system_powerpc64_darwin];
          flags : [af_smartlink_sections];
          labelprefix : 'L';
          comment : '; ';
          dollarsign: '$';
        );


begin
  RegisterAssembler(as_llvm_info,TLLVMAssember);
end.
