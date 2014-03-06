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
        procedure WriteExtraHeader;virtual;
        procedure WriteExtraFooter;virtual;
        procedure WriteInstruction(hp: tai);
        procedure WriteLlvmInstruction(hp: tai);
//        procedure WriteWeakSymbolDef(s: tasmsymbol); virtual;
        procedure WriteDirectiveName(dir: TAsmDirective); virtual;
        procedure WriteWeakSymbolDef(s: tasmsymbol);
       public
        constructor create(smart: boolean); override;
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
      end;


implementation

    uses
      SysUtils,
      cutils,cfileutl,systems,
      fmodule,verbose,
      llvmbase,aasmllvm,itllvm,llvmdef,
      cgbase,cgutils,cpubase;

    const
      line_length = 70;

    var
      symendcount  : longint;

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
           result:=result+ref.symbol.name;
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


   function getopstr(const o:toper; refwithalign: boolean) : ansistring;
     var
       hs : ansistring;
       doubleval: record
         case byte of
           1: (d: double);
           2: (i: int64);
       end;
{$ifdef cpuextended}
       extendedval: record
         case byte of
           1: (e: extended);
           2: (r: record
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
{$endif cpuextended}

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
               if o.ref^.symbol.typ=AT_LABEL then
                 getopstr:='label %';
               hs:=o.ref^.symbol.name;
               if o.ref^.offset<>0 then
                 internalerror(2013060223);
               getopstr:=getopstr+hs;
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
               doubleval.d:=o.dval
             else
               doubleval.d:=o.sval;
             result:='0x'+hexstr(doubleval.i,16);
           end;
         top_para:
           begin
             result:=getparas(o);
           end;
{$ifdef cpuextended}
         top_extended80:
           begin
             { hex format is always big endian in llvm }
             extendedval.e:=o.eval;
             result:='0xK'+hexstr(extendedval.r.h,sizeof(extendedval.r.h)*2)+hexstr(extendedval.r.l,sizeof(extendedval.r.l)*2);
           end;
{$endif cpuextended}
         else
           internalerror(2013060227);
       end;
     end;


  procedure TLlvmInstrWriter.WriteInstruction(hp: tai);
    var
      op: tllvmop;
      s: string;
      i, opstart: byte;
      sep: string[3];
      done: boolean;
    begin
      op:=taillvm(hp).llvmopcode;
      s:=#9;
      sep:=' ';
      done:=false;
      opstart:=0;
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
              s:=s+getregisterstring(taillvm(hp).oper[0]^.reg)+' = ';
            sep:=' ';
            opstart:=1;
          end;
        la_alloca:
          begin
            s:=s+getreferencestring(taillvm(hp).oper[0]^.ref^,false)+' = ';
            sep:=' ';
            opstart:=1;
          end;
        la_trunc, la_zext, la_sext, la_fptrunc, la_fpext,
        la_fptoui, la_fptosi, la_uitofp, la_sitofp,
        la_ptrtoint, la_inttoptr,
        la_bitcast:
          begin
            s:=s+getopstr(taillvm(hp).oper[0]^,false)+' = '+
              llvm_op2str[op]+' '+
              getopstr(taillvm(hp).oper[1]^,false)+' '+
              getopstr(taillvm(hp).oper[2]^,false)+' to '+
              getopstr(taillvm(hp).oper[3]^,false);
            done:=true;
          end
        else
          begin
            s:=s+getopstr(taillvm(hp).oper[0]^,true)+' = ';
            sep:=' ';
            opstart:=1
          end;
      end;
      { process operands }
      if not done then
        begin
          s:=s+llvm_op2str[op];
          if taillvm(hp).ops<>0 then
            begin
              for i:=opstart to taillvm(hp).ops-1 do
                begin
                   s:=s+sep+getopstr(taillvm(hp).oper[i]^,op in [la_load,la_store]);
                   if (taillvm(hp).oper[i]^.typ in [top_def,top_cond,top_fpcond]) or
                      (op=la_call) then
                     sep :=' '
                   else
                     sep:=', ';
                end;
            end;
        end;
      if op=la_alloca then
        begin
          s:=s+getreferencealignstring(taillvm(hp).oper[0]^.ref^)
        end;
      owner.AsmWriteLn(s);
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
      ch       : char;
      lasthp,
      hp       : tai;
      constdef : taiconst_type;
      s,t      : string;
      i,pos,l  : longint;
      InlineLevel : cardinal;
      last_align : longint;
      co       : comp;
      sin      : single;
      d        : double;
{$ifdef cpuextended}
      e        : extended;
{$endif cpuextended}
      do_line  : boolean;

      sepChar : char;
      replaceforbidden: boolean;
    begin
      if not assigned(p) then
       exit;
      replaceforbidden:=target_asm.dollarsign<>'$';

      last_align := 2;
      InlineLevel:=0;
      { lineinfo is only needed for al_procedures (PFV) }
      do_line:=(cs_asm_source in current_settings.globalswitches) or
               ((cs_lineinfo in current_settings.moduleswitches)
                 and (p=current_asmdata.asmlists[al_procedures]));
      lasthp:=nil;
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
                 WriteTempalloc(tai_tempalloc(hp));
             end;

           ait_align :
             begin
               { has to be specified as part of the symbol declaration }
               AsmWriteln('; error: explicit aligns are forbidden');
//               internalerror(2013010714);
             end;

           ait_section :
             begin
               AsmWrite(target_asm.comment);
               AsmWriteln('section');
             end;

           ait_datablock :
             begin
               AsmWrite(target_asm.comment);
               AsmWriteln('datablock');
             end;

           ait_const:
             begin
               AsmWrite(target_asm.comment);
               AsmWriteln('const');
             end;

           { the "and defined(FPC_HAS_TYPE_EXTENDED)" isn't optimal but currently the only solution
             it prevents proper cross compilation to i386 though
           }
{$if defined(cpuextended) and defined(FPC_HAS_TYPE_EXTENDED)}
           ait_real_80bit :
             begin
//               if do_line then
                AsmWriteLn(target_asm.comment+'value: '+extended2str(tai_real_80bit(hp).value));
             end;
{$endif cpuextended}

           ait_real_32bit,
           ait_real_64bit:
             begin
               if hp.typ=ait_real_32bit then
                 begin
//                   if do_line then
                    AsmWriteLn(target_asm.comment+'value: '+single2str(tai_real_32bit(hp).value));
//                   d:=tai_real_32bit(hp).value
                 end
               else
                 begin
//                   if do_line then
                     AsmWriteLn(target_asm.comment+'value: '+double2str(tai_real_64bit(hp).value));
//                   d:=tai_real_64bit(hp).value;
                 end;
             end;

           ait_comp_64bit :
             begin
//               if do_line then
                AsmWriteLn(target_asm.comment+'value: '+extended2str(tai_comp_64bit(hp).value));
             end;

           ait_string :
             begin
               AsmWrite(target_asm.comment);
               AsmWriteln('string');
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
                  if tai_label(hp).labsym.bind in [AB_GLOBAL,AB_PRIVATE_EXTERN] then
                   begin
                     { should be emitted as part of the variable/function def }
                     internalerror(2013010704);
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
               { should be emitted as part of the variable/function def }
               asmwrite('; (ait_symbol error, should be part of variable/function def) :');
               asmwriteln(tai_symbol(hp).sym.name);
//               internalerror(2013010705);
             end;
           ait_llvmprocdef:
             begin
               asmwrite('define ');
               asmwrite(llvmencodeproctype(tprocdef(taillvmdecl(hp).def),'',lpd_decl));
               asmwriteln(' {');
             end;
           ait_llvmvarsym:
             begin
               asmwrite(taillvmvarsym(hp).varsym.mangledname);
               if not taillvmvarsym(hp).varsym.globalasmsym then
                  asmwrite(' = internal global ')
               else
                  asmwrite(' = global ');
               asmwrite(llvmencodetype(taillvmvarsym(hp).varsym.vardef));
               asmwrite(' zeroinitializer, align ');
               asmwriteln(tostr(taillvmvarsym(hp).varsym.vardef.alignment));
             end;
           ait_llvmalias:
             begin
               asmwrite('@'+taillvmalias(hp).newsym.name);
               asmwrite(' = alias ');
               if taillvmalias(hp).linkage<>lll_default then
                 begin
                   str(taillvmalias(hp).linkage,s);
                   asmwrite(copy(s,length('lll_'),255));
                   asmwrite(' ');
                 end
               else
                 asmwrite('external ');
               if taillvmalias(hp).vis<>llv_default then
                 begin
                   str(taillvmalias(hp).vis,s);
                   asmwrite(copy(s,length('llv_'),255));
                   asmwrite(' ');
                 end;
               asmwrite(llvmencodeproctype(tabstractprocdef(taillvmalias(hp).def),'',lpd_alias));
               asmwrite('* ');
               asmwriteln(taillvmalias(hp).oldsym.name);
             end;
{$ifdef arm}
           ait_thumb_func:
             begin
               { should be emitted as part of the function def }
               internalerror(2013010706);
             end;
           ait_thumb_set:
             begin
               { should be emitted as part of the symbol def }
               internalerror(2013010707);
             end;
{$endif arm}
           ait_set:
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
//               internalerror(2013010711);
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
           else
             internalerror(2006012201);
         end;
         lasthp:=hp;
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
        AsmWriteLn(#9'.weak '+s.name);
      end;


    constructor TLLVMAssember.create(smart: boolean);
      begin
        inherited create(smart);
        InstrWriter:=TLLVMInstrWriter.create(self);
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
      symendcount:=0;

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
