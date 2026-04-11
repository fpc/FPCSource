{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Load the system unit, create required defs for systemunit

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
unit psystem;

{$i fpcdefs.inc}

interface

    uses
      compilerbase,symbase;

type
  TSystemUnitParser = class
  private
    FCompiler: TCompilerBase;
    procedure set_default_int_types;
    procedure set_default_ptr_types;
    property Compiler: TCompilerBase read FCompiler;
  public
    constructor Create(ACompiler: TCompilerBase);

    procedure create_intern_symbols;
    procedure create_intern_types;

    procedure load_intern_types;
  end;

    procedure registernodes;
    procedure registertais;


implementation

    uses
      globals,globtype,verbose,constexp,cpuinfo,compinnr,compiler,
      systems,
      symconst,symtype,symsym,symdef,symcpu,symtable,
      aasmtai,aasmcpu,
      fmodule,
      node,nbas,nflw,nset,ncon,ncnv,nld,nmem,ncal,nmat,nadd,ninl;


    constructor TSystemUnitParser.Create(ACompiler: TCompilerBase);
      begin
        FCompiler:=ACompiler;
      end;


    procedure TSystemUnitParser.create_intern_symbols;
      {
        all intern procedures for the system unit
      }
      begin
        systemunit.insertsym(compiler.syssymlist.create_syssym('Concat',in_concat_x));
        systemunit.insertsym(compiler.syssymlist.create_syssym('Write',in_write_x));
        systemunit.insertsym(compiler.syssymlist.create_syssym('WriteLn',in_writeln_x));
        systemunit.insertsym(compiler.syssymlist.create_syssym('WriteStr',in_writestr_x));
        systemunit.insertsym(compiler.syssymlist.create_syssym('Assigned',in_assigned_x));
        systemunit.insertsym(compiler.syssymlist.create_syssym('Read',in_read_x));
        systemunit.insertsym(compiler.syssymlist.create_syssym('ReadLn',in_readln_x));
        systemunit.insertsym(compiler.syssymlist.create_syssym('ReadStr',in_readstr_x));
        systemunit.insertsym(compiler.syssymlist.create_syssym('Ofs',in_ofs_x));
        systemunit.insertsym(compiler.syssymlist.create_syssym('SizeOf',in_sizeof_x));
        systemunit.insertsym(compiler.syssymlist.create_syssym('BitSizeOf',in_bitsizeof_x));
        systemunit.insertsym(compiler.syssymlist.create_syssym('TypeOf',in_typeof_x));
        systemunit.insertsym(compiler.syssymlist.create_syssym('Low',in_low_x));
        systemunit.insertsym(compiler.syssymlist.create_syssym('High',in_high_x));
        systemunit.insertsym(compiler.syssymlist.create_syssym('Slice',in_slice_x));
        systemunit.insertsym(compiler.syssymlist.create_syssym('Seg',in_seg_x));
        systemunit.insertsym(compiler.syssymlist.create_syssym('Ord',in_ord_x));
        systemunit.insertsym(compiler.syssymlist.create_syssym('Chr',in_chr_byte));
        systemunit.insertsym(compiler.syssymlist.create_syssym('Pred',in_pred_x));
        systemunit.insertsym(compiler.syssymlist.create_syssym('Succ',in_succ_x));
        systemunit.insertsym(compiler.syssymlist.create_syssym('Exclude',in_exclude_x_y));
        systemunit.insertsym(compiler.syssymlist.create_syssym('Include',in_include_x_y));
        systemunit.insertsym(compiler.syssymlist.create_syssym('Pack',in_pack_x_y_z));
        systemunit.insertsym(compiler.syssymlist.create_syssym('Unpack',in_unpack_x_y_z));
        systemunit.insertsym(compiler.syssymlist.create_syssym('Break',in_break));
        systemunit.insertsym(compiler.syssymlist.create_syssym('Exit',in_exit));
        systemunit.insertsym(compiler.syssymlist.create_syssym('Continue',in_continue));
        systemunit.insertsym(compiler.syssymlist.create_syssym('Leave',in_leave)); {macpas only}
        systemunit.insertsym(compiler.syssymlist.create_syssym('Cycle',in_cycle)); {macpas only}
        systemunit.insertsym(compiler.syssymlist.create_syssym('Dec',in_dec_x));
        systemunit.insertsym(compiler.syssymlist.create_syssym('Inc',in_inc_x));
        systemunit.insertsym(compiler.syssymlist.create_syssym('Str',in_str_x_string));
        systemunit.insertsym(compiler.syssymlist.create_syssym('Assert',in_assert_x_y));
        systemunit.insertsym(compiler.syssymlist.create_syssym('Val',in_val_x));
        systemunit.insertsym(compiler.syssymlist.create_syssym('Addr',in_addr_x));
{$ifdef i8086}
        systemunit.insertsym(compiler.syssymlist.create_syssym('FarAddr',in_faraddr_x));
{$endif i8086}
        systemunit.insertsym(compiler.syssymlist.create_syssym('TypeInfo',in_typeinfo_x));
        systemunit.insertsym(compiler.syssymlist.create_syssym('SetLength',in_setlength_x));
        systemunit.insertsym(compiler.syssymlist.create_syssym('Copy',in_copy_x));
        systemunit.insertsym(compiler.syssymlist.create_syssym('Initialize',in_initialize_x));
        systemunit.insertsym(compiler.syssymlist.create_syssym('Finalize',in_finalize_x));
        systemunit.insertsym(compiler.syssymlist.create_syssym('Length',in_length_x));
        systemunit.insertsym(compiler.syssymlist.create_syssym('New',in_new_x));
        systemunit.insertsym(compiler.syssymlist.create_syssym('Dispose',in_dispose_x));
{$ifdef SUPPORT_GET_FRAME}
        systemunit.insertsym(compiler.syssymlist.create_syssym('Get_Frame',in_get_frame));
{$endif SUPPORT_GET_FRAME}
        systemunit.insertsym(compiler.syssymlist.create_syssym('Unaligned',in_unaligned_x));
        systemunit.insertsym(compiler.syssymlist.create_syssym('Aligned',in_aligned_x));
        systemunit.insertsym(compiler.syssymlist.create_syssym('Volatile',in_volatile_x));
        systemunit.insertsym(compiler.syssymlist.create_syssym('ObjCSelector',in_objc_selector_x)); { objc only }
        systemunit.insertsym(compiler.syssymlist.create_syssym('ObjCEncode',in_objc_encode_x)); { objc only }
        systemunit.insertsym(compiler.syssymlist.create_syssym('Default',in_default_x));
        systemunit.insertsym(compiler.syssymlist.create_syssym('SetString',in_setstring_x_y_z));
        systemunit.insertsym(compiler.syssymlist.create_syssym('Insert',in_insert_x_y_z));
        systemunit.insertsym(compiler.syssymlist.create_syssym('Delete',in_delete_x_y_z));
        systemunit.insertsym(compiler.syssymlist.create_syssym('GetTypeKind',in_gettypekind_x));
        systemunit.insertsym(compiler.syssymlist.create_syssym('IsManagedType',in_ismanagedtype_x));
        systemunit.insertsym(compiler.syssymlist.create_syssym('IsConstValue',in_isconstvalue_x));
        systemunit.insertsym(compiler.syssymlist.create_syssym('fpc_eh_return_data_regno', in_const_eh_return_data_regno));
        systemunit.insertsym(compiler.syssymlist.create_syssym('AtomicIncrement',in_atomic_inc));
        systemunit.insertsym(compiler.syssymlist.create_syssym('AtomicDecrement',in_atomic_dec));
        systemunit.insertsym(compiler.syssymlist.create_syssym('AtomicExchange',in_atomic_xchg));
        systemunit.insertsym(compiler.syssymlist.create_syssym('AtomicCmpExchange',in_atomic_cmp_xchg));
        systemunit.insertsym(cconstsym.create_ord('False',constord,0,compiler.deftypes.pasbool1type));
        systemunit.insertsym(cconstsym.create_ord('True',constord,1,compiler.deftypes.pasbool1type));
      end;


    procedure TSystemUnitParser.set_default_int_types;
      begin
{$ifdef cpu64bitalu}
        aluuinttype:=compiler.deftypes.u64inttype;
        alusinttype:=compiler.deftypes.s64inttype;
{$endif cpu64bitalu}
{$ifdef cpu64bitaddr}
        sizeuinttype:=compiler.deftypes.u64inttype;
        sizesinttype:=compiler.deftypes.s64inttype;
        compiler.deftypes.uinttype:=compiler.deftypes.u64inttype;
        compiler.deftypes.sinttype:=compiler.deftypes.s64inttype;
{$endif cpu64bitaddr}
{$ifdef cpu32bitaddr}
        sizeuinttype:=compiler.deftypes.u32inttype;
        sizesinttype:=compiler.deftypes.s32inttype;
        compiler.deftypes.uinttype:=compiler.deftypes.u32inttype;
        compiler.deftypes.sinttype:=compiler.deftypes.s32inttype;
{$endif cpu32bitaddr}
{$ifdef cpu32bitalu}
        compiler.deftypes.uinttype:=compiler.deftypes.u32inttype;
        compiler.deftypes.sinttype:=compiler.deftypes.s32inttype;
        aluuinttype:=compiler.deftypes.u32inttype;
        alusinttype:=compiler.deftypes.s32inttype;
{$endif cpu32bitalu}
{$ifdef cpu16bitaddr}
        sizeuinttype:=compiler.deftypes.u16inttype;
        sizesinttype:=compiler.deftypes.s16inttype;
{$endif cpu16bitaddr}
{$ifdef cpu16bitalu}
        compiler.deftypes.uinttype:=compiler.deftypes.u16inttype;
        compiler.deftypes.sinttype:=compiler.deftypes.s16inttype;
        aluuinttype:=compiler.deftypes.u16inttype;
        alusinttype:=compiler.deftypes.s16inttype;
{$endif cpu16bitalu}
{$ifdef cpu8bitalu}
        compiler.deftypes.uinttype:=compiler.deftypes.u8inttype;
        compiler.deftypes.sinttype:=compiler.deftypes.s8inttype;
        aluuinttype:=compiler.deftypes.u8inttype;
        alusinttype:=compiler.deftypes.s8inttype;
{$endif cpu8bitalu}

        osuinttype:=compiler.deftypes.uinttype;
        ossinttype:=compiler.deftypes.sinttype;
      end;


    procedure TSystemUnitParser.set_default_ptr_types;
      begin
{$ifdef i8086}
        if compiler.globals.current_settings.x86memorymodel in x86_far_code_models then
          compiler.deftypes.voidcodepointertype:=compiler.deftypes.voidfarpointertype
        else if compiler.globals.current_settings.x86memorymodel=mm_tiny then
          compiler.deftypes.voidcodepointertype:=compiler.deftypes.voidnearpointertype
        else
          compiler.deftypes.voidcodepointertype:=compiler.deftypes.voidnearcspointertype;
        compiler.deftypes.voidstackpointertype:=compiler.deftypes.voidnearsspointertype;
{$else i8086}
        compiler.deftypes.voidcodepointertype:=compiler.deftypes.voidpointertype;
        compiler.deftypes.voidstackpointertype:=compiler.deftypes.voidpointertype;
{$endif i8086}
        case compiler.deftypes.voidcodepointertype.size of
          2:
            begin
              codeptruinttype:=compiler.deftypes.u16inttype;
              codeptrsinttype:=compiler.deftypes.s16inttype;
            end;
          4:
            begin
              codeptruinttype:=compiler.deftypes.u32inttype;
              codeptrsinttype:=compiler.deftypes.s32inttype;
            end;
          8:
            begin
              codeptruinttype:=compiler.deftypes.u64inttype;
              codeptrsinttype:=compiler.deftypes.s64inttype;
            end;
          else
            Internalerror(2015112106);
        end;
        case compiler.deftypes.voidpointertype.size of
          2:
            begin
              ptruinttype:=compiler.deftypes.u16inttype;
              ptrsinttype:=compiler.deftypes.s16inttype;
            end;
          4:
            begin
              ptruinttype:=compiler.deftypes.u32inttype;
              ptrsinttype:=compiler.deftypes.s32inttype;
            end;
          8:
            begin
              ptruinttype:=compiler.deftypes.u64inttype;
              ptrsinttype:=compiler.deftypes.s64inttype;
            end;
          else
            Internalerror(2016100301);
        end;
      end;

    procedure TSystemUnitParser.create_intern_types;
      {
        all the types inserted into the system unit
      }

        function addtype(const s:string;def:tdef):ttypesym;
        begin
          result:=ctypesym.create(s,def);
          systemunit.insertsym(result);
        end;

        procedure addfield(recst:tabstractrecordsymtable;sym:tfieldvarsym);
        begin
          recst.insertsym(sym);
          recst.addfield(sym,vis_hidden);
        end;

        procedure create_fpu_types;
        begin
          if compiler.globals.init_settings.fputype<>fpu_none then
            begin
              compiler.deftypes.s32floattype:=cfloatdef.create(s32real,true,compiler);
              s64floattype:=cfloatdef.create(s64real,true,compiler);
              s80floattype:=cfloatdef.create(s80real,true,compiler);
              compiler.deftypes.sc80floattype:=cfloatdef.create(sc80real,true,compiler);
            end
          else
            begin
              compiler.deftypes.s32floattype:=nil;
              s64floattype:=nil;
              s80floattype:=nil;
              compiler.deftypes.sc80floattype:=nil;
            end;
        end;

      var
        hrecst : trecordsymtable;
	pvmt_name : shortstring;
      begin
        compiler.symtablestack.push(systemunit);
        compiler.deftypes.cundefinedtype:=cundefineddef.create(true,compiler);
        compiler.deftypes.cformaltype:=cformaldef.create(false,compiler);
        compiler.deftypes.ctypedformaltype:=cformaldef.create(true,compiler);
        compiler.deftypes.voidtype:=corddef.create(uvoid,0,0,true,compiler);
        compiler.deftypes.voidpointertype:=cpointerdef.create(compiler.deftypes.voidtype,compiler);
        compiler.deftypes.u8inttype:=corddef.create(u8bit,0,255,true,compiler);
        compiler.deftypes.s8inttype:=corddef.create(s8bit,int64(-128),127,true,compiler);
        compiler.deftypes.u16inttype:=corddef.create(u16bit,0,65535,true,compiler);
        compiler.deftypes.s16inttype:=corddef.create(s16bit,int64(-32768),32767,true,compiler);
        compiler.deftypes.s24inttype:=corddef.create(customint,-(int64(1) shl 23),1 shl 23 - 1,true,compiler);
        compiler.deftypes.u24inttype:=corddef.create(customint,0,1 shl 24 - 1,true,compiler);
        compiler.deftypes.u32inttype:=corddef.create(u32bit,0,high(longword),true,compiler);
        compiler.deftypes.s32inttype:=corddef.create(s32bit,int64(low(longint)),int64(high(longint)),true,compiler);
        compiler.deftypes.s40inttype:=corddef.create(customint,-(int64(1) shl 39),int64(1) shl 39 - 1,true,compiler);
        compiler.deftypes.u40inttype:=corddef.create(customint,0,int64(1) shl 40 - 1,true,compiler);
        compiler.deftypes.s48inttype:=corddef.create(customint,-(int64(1) shl 47),int64(1) shl 47 - 1,true,compiler);
        compiler.deftypes.u48inttype:=corddef.create(customint,0,int64(1) shl 48 - 1,true,compiler);
        compiler.deftypes.s56inttype:=corddef.create(customint,-(int64(1) shl 55),int64(1) shl 55 - 1,true,compiler);
        compiler.deftypes.u56inttype:=corddef.create(customint,0,int64(1) shl 56 - 1,true,compiler);
        compiler.deftypes.u64inttype:=corddef.create(u64bit,low(qword),high(qword),true,compiler);
        compiler.deftypes.s64inttype:=corddef.create(s64bit,low(int64),high(int64),true,compiler);
        { upper/lower bound not yet properly set for 128 bit types, as we don't
          support them yet at the Pascal level (nor for tconstexprint); they're
          only used internally by the high level code generator for LLVM to
          implement overflow checking }
        compiler.deftypes.u128inttype:=corddef.create(u128bit,0,0,true,compiler);
        compiler.deftypes.s128inttype:=corddef.create(s128bit,0,0,true,compiler);
        compiler.deftypes.pasbool1type:=corddef.create(pasbool1,0,1,true,compiler);
        compiler.deftypes.pasbool8type:=corddef.create(pasbool8,0,1,true,compiler);
        compiler.deftypes.pasbool16type:=corddef.create(pasbool16,0,1,true,compiler);
        compiler.deftypes.pasbool32type:=corddef.create(pasbool32,0,1,true,compiler);
        compiler.deftypes.pasbool64type:=corddef.create(pasbool64,0,1,true,compiler);
        compiler.deftypes.bool8type:=corddef.create(bool8bit,low(int64),high(int64),true,compiler);
        compiler.deftypes.bool16type:=corddef.create(bool16bit,low(int64),high(int64),true,compiler);
        compiler.deftypes.bool32type:=corddef.create(bool32bit,low(int64),high(int64),true,compiler);
        compiler.deftypes.bool64type:=corddef.create(bool64bit,low(int64),high(int64),true,compiler);
{$ifdef llvm}
        compiler.deftypes.llvmbool1type:=corddef.create(pasbool1,0,1,true,compiler);
{$endif llvm}
        compiler.deftypes.cansichartype:=corddef.create(uchar,0,255,true,compiler);
        compiler.deftypes.cwidechartype:=corddef.create(uwidechar,0,65535,true,compiler);
        compiler.deftypes.cshortstringtype:=cstringdef.createshort(255,true,compiler);
        { should we give a length to the default long and ansi string definition ?? }
        compiler.deftypes.clongstringtype:=cstringdef.createlong(-1,true,compiler);
        compiler.deftypes.cansistringtype:=cstringdef.createansi(0,true,compiler);
        if compiler.target.info.system in systems_windows then
          compiler.deftypes.cwidestringtype:=cstringdef.createwide(true,compiler)
        else
          compiler.deftypes.cwidestringtype:=cstringdef.createunicode(true,compiler);
        compiler.deftypes.cunicodestringtype:=cstringdef.createunicode(true,compiler);
        { length=0 for shortstring is open string (needed for readln(string) }
        compiler.deftypes.openshortstringtype:=cstringdef.createshort(0,true,compiler);
        if compiler.target.info.system=system_i386_watcom then
          pvmt_name:='lower__pvmt'
        else
          pvmt_name:='pvmt';
 {$ifdef x86}
        create_fpu_types;
{$ifndef FPC_SUPPORT_X87_TYPES_ON_WIN64}
        if compiler.target.info.system=system_x86_64_win64 then
          begin
            compiler.deftypes.s64currencytype:=corddef.create(scurrency,low(int64),high(int64),true,compiler);
            pbestrealtype:=@s64floattype;
          end
        else
{$endif FPC_SUPPORT_X87_TYPES_ON_WIN64}
          compiler.deftypes.s64currencytype:=cfloatdef.create(s64currency,true,compiler);
{$endif x86}
{$ifdef powerpc}
        create_fpu_types;
        compiler.deftypes.s64currencytype:=corddef.create(scurrency,low(int64),high(int64),true,compiler);
{$endif powerpc}
{$ifdef POWERPC64}
        create_fpu_types;
        compiler.deftypes.s64currencytype:=corddef.create(scurrency,low(int64),high(int64),true,compiler);
{$endif POWERPC64}
{$ifdef sparc}
        create_fpu_types;
        compiler.deftypes.s64currencytype:=corddef.create(scurrency,low(int64),high(int64),true,compiler);
{$endif sparc}
{$ifdef sparc64}
        create_fpu_types;
        compiler.deftypes.s64currencytype:=corddef.create(scurrency,low(int64),high(int64),true,compiler);
{$endif sparc64}
{$ifdef m68k}
        create_fpu_types;
        compiler.deftypes.s64currencytype:=corddef.create(scurrency,low(int64),high(int64),true,compiler);
{$endif}
{$ifdef arm}
        create_fpu_types;
        compiler.deftypes.s64currencytype:=corddef.create(scurrency,low(int64),high(int64),true,compiler);
{$endif arm}
{$ifdef aarch64}
        create_fpu_types;
        compiler.deftypes.s64currencytype:=corddef.create(scurrency,low(int64),high(int64),true,compiler);
{$endif aarch64}
{$ifdef avr}
        compiler.deftypes.s32floattype:=cfloatdef.create(s32real,true,compiler);
        s64floattype:=cfloatdef.create(s64real,true,compiler);
        s80floattype:=cfloatdef.create(s80real,true,compiler);
        compiler.deftypes.sc80floattype:=cfloatdef.create(sc80real,true,compiler);
        compiler.deftypes.s64currencytype:=corddef.create(scurrency,low(int64),high(int64),true,compiler);
{$endif avr}
{$ifdef z80}
        compiler.deftypes.s32floattype:=cfloatdef.create(s32real,true,compiler);
        s64floattype:=cfloatdef.create(s64real,true,compiler);
        s80floattype:=cfloatdef.create(s80real,true,compiler);
        compiler.deftypes.sc80floattype:=cfloatdef.create(sc80real,true,compiler);
        compiler.deftypes.s64currencytype:=corddef.create(scurrency,low(int64),high(int64),true,compiler);
{$endif z80}
{$ifdef mips}
        create_fpu_types;
        compiler.deftypes.s64currencytype:=corddef.create(scurrency,low(int64),high(int64),true,compiler);
{$endif mips}
{$ifdef riscv32}
        create_fpu_types;
        compiler.deftypes.s64currencytype:=corddef.create(scurrency,low(int64),high(int64),true,compiler);
{$endif riscv32}
{$ifdef riscv64}
        create_fpu_types;
        compiler.deftypes.s64currencytype:=corddef.create(scurrency,low(int64),high(int64),true,compiler);
{$endif riscv64}
{$ifdef jvm}
        create_fpu_types;
        compiler.deftypes.s64currencytype:=corddef.create(scurrency,low(int64),high(int64),true,compiler);
{$endif jvm}
{$ifdef wasm}
        create_fpu_types;
        compiler.deftypes.s64currencytype:=corddef.create(scurrency,low(int64),high(int64),true,compiler);
{$endif wasm}
{$ifdef xtensa}
        create_fpu_types;
        compiler.deftypes.s64currencytype:=corddef.create(scurrency,low(int64),high(int64),true,compiler);
{$endif xtensa}
{$ifdef loongarch64}
        create_fpu_types;
        compiler.deftypes.s64currencytype:=corddef.create(scurrency,low(int64),high(int64),true,compiler);
{$endif loongarch64}
        set_default_int_types;
        { some other definitions }
        compiler.deftypes.charpointertype:=cpointerdef.create(compiler.deftypes.cansichartype,compiler);
        compiler.deftypes.widecharpointertype:=cpointerdef.create(compiler.deftypes.cwidechartype,compiler);
{$ifdef i8086}
        compiler.deftypes.parentfpvoidpointertype:=tcpupointerdefclass(cpointerdef).createx86(compiler.deftypes.voidtype,x86pt_near_ss,compiler);
{$else i8086}
        compiler.deftypes.parentfpvoidpointertype:=cpointerdef.create(compiler.deftypes.voidtype,compiler);
{$endif i8086}
{$ifdef x86}
        compiler.deftypes.voidnearpointertype:=tcpupointerdefclass(cpointerdef).createx86(compiler.deftypes.voidtype,x86pt_near,compiler);
        compiler.deftypes.voidnearcspointertype:=tcpupointerdefclass(cpointerdef).createx86(compiler.deftypes.voidtype,x86pt_near_cs,compiler);
        compiler.deftypes.voidneardspointertype:=tcpupointerdefclass(cpointerdef).createx86(compiler.deftypes.voidtype,x86pt_near_ds,compiler);
        compiler.deftypes.voidnearsspointertype:=tcpupointerdefclass(cpointerdef).createx86(compiler.deftypes.voidtype,x86pt_near_ss,compiler);
        compiler.deftypes.voidnearespointertype:=tcpupointerdefclass(cpointerdef).createx86(compiler.deftypes.voidtype,x86pt_near_es,compiler);
        compiler.deftypes.voidnearfspointertype:=tcpupointerdefclass(cpointerdef).createx86(compiler.deftypes.voidtype,x86pt_near_fs,compiler);
        compiler.deftypes.voidneargspointertype:=tcpupointerdefclass(cpointerdef).createx86(compiler.deftypes.voidtype,x86pt_near_gs,compiler);
  {$ifdef i8086}
        compiler.deftypes.voidfarpointertype:=tcpupointerdefclass(cpointerdef).createx86(compiler.deftypes.voidtype,x86pt_far,compiler);
        compiler.deftypes.voidhugepointertype:=tcpupointerdefclass(cpointerdef).createx86(compiler.deftypes.voidtype,x86pt_huge,compiler);
        compiler.deftypes.charnearpointertype:=tcpupointerdefclass(cpointerdef).createx86(compiler.deftypes.cansichartype,x86pt_near,compiler);
        compiler.deftypes.charfarpointertype:=tcpupointerdefclass(cpointerdef).createx86(compiler.deftypes.cansichartype,x86pt_far,compiler);
        compiler.deftypes.charhugepointertype:=tcpupointerdefclass(cpointerdef).createx86(compiler.deftypes.cansichartype,x86pt_huge,compiler);
        compiler.deftypes.bytefarpointertype:=tcpupointerdefclass(cpointerdef).createx86(compiler.deftypes.u8inttype,x86pt_far,compiler);
        compiler.deftypes.wordfarpointertype:=tcpupointerdefclass(cpointerdef).createx86(compiler.deftypes.u16inttype,x86pt_far,compiler);
        compiler.deftypes.longintfarpointertype:=tcpupointerdefclass(cpointerdef).createx86(compiler.deftypes.s32inttype,x86pt_far,compiler);
  {$endif i8086}
        x86_m64type:=carraydef.create_vector(0,1,compiler.deftypes.s32inttype,compiler);
        x86_m128type:=carraydef.create_vector(0,3,compiler.deftypes.s32inttype,compiler);
        x86_m128dtype:=carraydef.create_vector(0,1,compiler.deftypes.s32inttype,compiler);
        x86_m128itype:=carraydef.create_vector(0,3,compiler.deftypes.s32inttype,compiler);
        x86_m256type:=carraydef.create_vector(0,7,compiler.deftypes.s32inttype,compiler);
        x86_m256dtype:=carraydef.create_vector(0,3,compiler.deftypes.s32inttype,compiler);
        x86_m256itype:=carraydef.create_vector(0,7,compiler.deftypes.s32inttype,compiler);

        tarraydef(x86_m64type).elementdef:=compiler.deftypes.s32floattype;
        tarraydef(x86_m128type).elementdef:=compiler.deftypes.s32floattype;
        tarraydef(x86_m128dtype).elementdef:=s64floattype;
        tarraydef(x86_m128itype).elementdef:=compiler.deftypes.s32floattype;
        tarraydef(x86_m256type).elementdef:=compiler.deftypes.s32floattype;
        tarraydef(x86_m256dtype).elementdef:=s64floattype;
        tarraydef(x86_m256itype).elementdef:=compiler.deftypes.s32floattype;
{$endif x86}
{$ifdef wasm}
        compiler.deftypes.wasmvoidexternreftype:=tcpupointerdefclass.create_externref(compiler.deftypes.voidtype,compiler);
{$endif wasm}
        set_default_ptr_types;
        compiler.deftypes.openchararraytype:=carraydef.create_openarray(compiler);
        tarraydef(compiler.deftypes.openchararraytype).elementdef:=compiler.deftypes.cansichartype;
        compiler.deftypes.cfiletype:=cfiledef.createuntyped(compiler);
        if f_variants in compiler.globals.features then
          begin
            compiler.deftypes.cvarianttype:=cvariantdef.create(vt_normalvariant,compiler);
            compiler.deftypes.colevarianttype:=cvariantdef.create(vt_olevariant,compiler);
          end;

{$ifdef cpufpemu}
        { Normal types }
        (* we use the same types as without emulator, the only
          difference is that direct calls to the emulator are generated
        if (cs_fp_emulation in compiler.globals.current_settings.moduleswitches) then
          begin
            addtype('Single',compiler.deftypes.s32floattype);
            { extended size is the best real type for the target }
            addtype('Real',compiler.deftypes.s32floattype);
            pbestrealtype:=@compiler.deftypes.s32floattype;
            { extended size is the best real type for the target }
            addtype('Extended',pbestrealtype^);
          end
        else
        *)
{$endif cpufpemu}
        if compiler.globals.init_settings.fputype<>fpu_none then
          begin
            addtype('Single',compiler.deftypes.s32floattype);
            addtype('Double',s64floattype);
            { extended size is the best real type for the target }
            addtype('Extended',pbestrealtype^);
            { CExtended corresponds to the C version of the Extended type
              (either "long double" or "double") }
            if compiler.target.info.system in systems_android then
              { Android has "long double"="double" even for x86 }
              addtype('CExtended',s64floattype)
            else
              if tfloatdef(pbestrealtype^).floattype=s80real then
                addtype('CExtended',compiler.deftypes.sc80floattype)
              else
                addtype('CExtended',pbestrealtype^);
          end;
{$ifdef x86}
{$ifndef FPC_SUPPORT_X87_TYPES_ON_WIN64}
        if compiler.target.info.system<>system_x86_64_win64 then
{$endif FPC_SUPPORT_X87_TYPES_ON_WIN64}
          addtype('Comp',cfloatdef.create(s64comp,true,compiler));
{$endif x86}
        addtype('Currency',compiler.deftypes.s64currencytype);
        addtype('Pointer',compiler.deftypes.voidpointertype);
{$ifdef x86}
        addtype('NearPointer',compiler.deftypes.voidnearpointertype);
        addtype('NearCsPointer',compiler.deftypes.voidnearcspointertype);
        addtype('NearDsPointer',compiler.deftypes.voidneardspointertype);
        addtype('NearSsPointer',compiler.deftypes.voidnearsspointertype);
        addtype('NearEsPointer',compiler.deftypes.voidnearespointertype);
        addtype('NearFsPointer',compiler.deftypes.voidnearfspointertype);
        addtype('NearGsPointer',compiler.deftypes.voidneargspointertype);
  {$ifdef i8086}
        addtype('FarPointer',compiler.deftypes.voidfarpointertype);
        addtype('HugePointer',compiler.deftypes.voidhugepointertype);
  {$endif i8086}
        addtype('__m64',x86_m64type);
        addtype('__m128', x86_m128type);
        addtype('__m128d',x86_m128dtype);
        addtype('__m128i',x86_m128itype);
        addtype('__m256', x86_m256type);
        addtype('__m256d',x86_m256dtype);
        addtype('__m256i',x86_m256itype);
{$endif x86}
        addtype('ShortString',compiler.deftypes.cshortstringtype);
{$ifdef support_longstring}
        addtype('LongString',compiler.deftypes.clongstringtype);
{$endif support_longstring}
        addtype('AnsiString',compiler.deftypes.cansistringtype);
        addtype('WideString',compiler.deftypes.cwidestringtype);
        addtype('UnicodeString',compiler.deftypes.cunicodestringtype);

        addtype('OpenString',compiler.deftypes.openshortstringtype);
        addtype('Boolean',compiler.deftypes.pasbool1type);
        addtype('Boolean8',compiler.deftypes.pasbool8type);
        addtype('Boolean16',compiler.deftypes.pasbool16type);
        addtype('Boolean32',compiler.deftypes.pasbool32type);
        addtype('Boolean64',compiler.deftypes.pasbool64type);
        addtype('ByteBool',compiler.deftypes.bool8type);
        addtype('WordBool',compiler.deftypes.bool16type);
        addtype('LongBool',compiler.deftypes.bool32type);
        addtype('QWordBool',compiler.deftypes.bool64type);
{$ifdef llvm}
        addtype('LLVMBool1',compiler.deftypes.llvmbool1type);
{$endif llvm}
        addtype('Byte',compiler.deftypes.u8inttype);
        addtype('ShortInt',compiler.deftypes.s8inttype);
        addtype('Word',compiler.deftypes.u16inttype);
        addtype('SmallInt',compiler.deftypes.s16inttype);
        addtype('LongWord',compiler.deftypes.u32inttype);
        addtype('LongInt',compiler.deftypes.s32inttype);
        addtype('QWord',compiler.deftypes.u64inttype);
        addtype('Int64',compiler.deftypes.s64inttype);
        addtype('AnsiChar',compiler.deftypes.cansichartype);
        addtype('WideChar',compiler.deftypes.cwidechartype);
        addtype('Text',cfiledef.createtext(compiler));
        addtype('TypedFile',cfiledef.createtyped(compiler.deftypes.voidtype,compiler));
        if f_variants in compiler.globals.features then
          begin
            addtype('Variant',compiler.deftypes.cvarianttype);
            addtype('OleVariant',compiler.deftypes.colevarianttype);
          end;
        { Internal types }
        addtype('$undefined',compiler.deftypes.cundefinedtype);
        addtype('$formal',compiler.deftypes.cformaltype);
        addtype('$typedformal',compiler.deftypes.ctypedformaltype);
        addtype('$void',compiler.deftypes.voidtype);
        addtype('$void_pointer',compiler.deftypes.voidpointertype);
        addtype('$byte',compiler.deftypes.u8inttype);
        addtype('$shortint',compiler.deftypes.s8inttype);
        addtype('$word',compiler.deftypes.u16inttype);
        addtype('$smallint',compiler.deftypes.s16inttype);
        addtype('$sint24',compiler.deftypes.s24inttype);
        addtype('$uint24',compiler.deftypes.u24inttype);
        addtype('$ulong',compiler.deftypes.u32inttype);
        addtype('$longint',compiler.deftypes.s32inttype);
        addtype('$sint40',compiler.deftypes.s40inttype);
        addtype('$uint40',compiler.deftypes.u40inttype);
        addtype('$sint48',compiler.deftypes.s48inttype);
        addtype('$uint48',compiler.deftypes.u48inttype);
        addtype('$sint56',compiler.deftypes.s56inttype);
        addtype('$uint56',compiler.deftypes.u56inttype);
        addtype('$qword',compiler.deftypes.u64inttype);
        addtype('$int64',compiler.deftypes.s64inttype);
        addtype('$uint128',compiler.deftypes.u128inttype);
        addtype('$int128',compiler.deftypes.s128inttype);
        addtype('$ansichar',compiler.deftypes.cansichartype);
        addtype('$widechar',compiler.deftypes.cwidechartype);
        addtype('$shortstring',compiler.deftypes.cshortstringtype);
        addtype('$longstring',compiler.deftypes.clongstringtype);
        addtype('$ansistring',compiler.deftypes.cansistringtype);
        addtype('$widestring',compiler.deftypes.cwidestringtype);
        addtype('$unicodestring',compiler.deftypes.cunicodestringtype);
        addtype('$openshortstring',compiler.deftypes.openshortstringtype);
        addtype('$boolean',compiler.deftypes.pasbool1type);
        addtype('$boolean8',compiler.deftypes.pasbool8type);
        addtype('$boolean16',compiler.deftypes.pasbool16type);
        addtype('$boolean32',compiler.deftypes.pasbool32type);
        addtype('$boolean64',compiler.deftypes.pasbool64type);
        addtype('$bytebool',compiler.deftypes.bool8type);
        addtype('$wordbool',compiler.deftypes.bool16type);
        addtype('$longbool',compiler.deftypes.bool32type);
        addtype('$qwordbool',compiler.deftypes.bool64type);
{$ifdef llvm}
        addtype('$llvmbool1',compiler.deftypes.llvmbool1type);
        llvm_metadatatype:=cformaldef.create(false,compiler);
        { if this gets renamed, also adjust agllvm so it still writes the identifier of this type as "metadata" }
        addtype('$metadata',llvm_metadatatype);
        addtype('LLVMMetadata',llvm_metadatatype);
{$endif llvm}
        addtype('$char_pointer',compiler.deftypes.charpointertype);
        addtype('$widechar_pointer',compiler.deftypes.widecharpointertype);
        addtype('$parentfp_void_pointer',compiler.deftypes.parentfpvoidpointertype);
{$ifdef x86}
        addtype('$void_nearpointer',compiler.deftypes.voidnearpointertype);
        addtype('$void_nearcspointer',compiler.deftypes.voidnearcspointertype);
        addtype('$void_neardspointer',compiler.deftypes.voidneardspointertype);
        addtype('$void_nearsspointer',compiler.deftypes.voidnearsspointertype);
        addtype('$void_nearespointer',compiler.deftypes.voidnearespointertype);
        addtype('$void_nearfspointer',compiler.deftypes.voidnearfspointertype);
        addtype('$void_neargspointer',compiler.deftypes.voidneargspointertype);
  {$ifdef i8086}
        addtype('$void_farpointer',compiler.deftypes.voidfarpointertype);
        addtype('$void_hugepointer',compiler.deftypes.voidhugepointertype);
        addtype('$char_nearpointer',compiler.deftypes.charnearpointertype);
        addtype('$char_farpointer',compiler.deftypes.charfarpointertype);
        addtype('$char_hugepointer',compiler.deftypes.charhugepointertype);
        addtype('$byte_farpointer',compiler.deftypes.bytefarpointertype);
        addtype('$word_farpointer',compiler.deftypes.wordfarpointertype);
        addtype('$longint_farpointer',compiler.deftypes.longintfarpointertype);
  {$endif i8086}
        addtype('$__m64',  x86_m64type);
        addtype('$__m128', x86_m128type);
        addtype('$__m128d',x86_m128dtype);
        addtype('$__m128i',x86_m128itype);
        addtype('$__m256', x86_m256type);
        addtype('$__m256d',x86_m256dtype);
        addtype('$__m256i',x86_m256itype);
{$endif x86}
{$ifdef wasm}
        addtype('$wasm_void_externref',compiler.deftypes.wasmvoidexternreftype);
        addtype('WasmExternRef',compiler.deftypes.wasmvoidexternreftype);
{$endif wasm}
        addtype('$openchararray',compiler.deftypes.openchararraytype);
        addtype('$file',compiler.deftypes.cfiletype);
        if f_variants in compiler.globals.features then
          begin
            addtype('$variant',compiler.deftypes.cvarianttype);
            addtype('$olevariant',compiler.deftypes.colevarianttype);
          end;
        if compiler.globals.init_settings.fputype<>fpu_none then
          begin
            addtype('$s32real',compiler.deftypes.s32floattype);
            addtype('$s64real',s64floattype);
            addtype('$s80real',s80floattype);
            addtype('$sc80real',compiler.deftypes.sc80floattype);
          end;
        addtype('$s64currency',compiler.deftypes.s64currencytype);
        if not(compiler.target.info.system in systems_managed_vm) then
          begin
            { Add a type for virtual method tables }
            hrecst:=trecordsymtable.create('',compiler.globals.current_settings.packrecords,compiler.globals.current_settings.alignment.recordalignmin,compiler);
            vmttype:=crecorddef.create('',hrecst,compiler);
            pvmttype:=cpointerdef.create(vmttype,compiler);
            { can't use addtype for pvmt because the rtti of the pointed
              type is not available. The rtti for pvmt will be written implicitly
              by thev tblarray below }
            systemunit.insertsym(ctypesym.create('$'+pvmt_name,pvmttype));
            addfield(hrecst,cfieldvarsym.create('$length',vs_value,sizesinttype,[]));
            addfield(hrecst,cfieldvarsym.create('$mlength',vs_value,sizesinttype,[]));
            addfield(hrecst,cfieldvarsym.create('$parent',vs_value,pvmttype,[]));
            { it seems vmttype is used both for TP objects and Delphi classes,
              so the next entry could either be the first virtual method (vm1)
              (object) or the class name (class). We can't easily create separate
              vtable formats for both, as gdb is hard coded to search for
              __vtbl_ptr_type in all cases (JM) }
            addfield(hrecst,cfieldvarsym.create('$vm1_or_classname',vs_value,cpointerdef.create(compiler.deftypes.cshortstringtype,compiler),[]));
            vmtarraytype:=carraydef.create(0,0,compiler.deftypes.s32inttype,compiler);
            tarraydef(vmtarraytype).elementdef:=compiler.deftypes.voidpointertype;
            addfield(hrecst,cfieldvarsym.create('$__pfn',vs_value,vmtarraytype,[]));
            addtype('$__vtbl_ptr_type',vmttype);
            vmtarraytype:=carraydef.create(0,1,compiler.deftypes.s32inttype,compiler);
            tarraydef(vmtarraytype).elementdef:=pvmttype;
            addtype('$vtblarray',vmtarraytype);
          end;
        { Add a type for methodpointers }
        hrecst:=trecordsymtable.create('',1,compiler.globals.current_settings.alignment.recordalignmin,compiler);
        addfield(hrecst,cfieldvarsym.create('$proc',vs_value,compiler.deftypes.voidcodepointertype,[]));
        addfield(hrecst,cfieldvarsym.create('$self',vs_value,compiler.deftypes.voidpointertype,[]));
        compiler.deftypes.methodpointertype:=crecorddef.create('',hrecst,compiler);
        addtype('$methodpointer',compiler.deftypes.methodpointertype);
        { Add a type for nested proc pointers }
        hrecst:=trecordsymtable.create('',1,compiler.globals.current_settings.alignment.recordalignmin,compiler);
        addfield(hrecst,cfieldvarsym.create('$proc',vs_value,compiler.deftypes.voidcodepointertype,[]));
        addfield(hrecst,cfieldvarsym.create('$parentfp',vs_value,compiler.deftypes.parentfpvoidpointertype,[]));
        compiler.deftypes.nestedprocpointertype:=crecorddef.create('',hrecst,compiler);
        addtype('$nestedprocpointer',compiler.deftypes.nestedprocpointertype);
        compiler.symtablestack.pop(systemunit);
      end;


    procedure TSystemUnitParser.load_intern_types;
      {
        Load all default definitions for consts from the system unit
      }

        procedure loadtype(const s:string;var def:tdef);
        var
          srsym : ttypesym;
        begin
          srsym:=search_system_type(s);
          def:=srsym.typedef;
        end;

      var
        oldcurrentmodule : tmodule;
        pvmt_name : shortstring;
      begin
{$ifndef FPC_SUPPORT_X87_TYPES_ON_WIN64}
        if compiler.target.info.system=system_x86_64_win64 then
          pbestrealtype:=@s64floattype;
{$endif FPC_SUPPORT_X87_TYPES_ON_WIN64}

        oldcurrentmodule:=compiler.current_module;
        compiler.set_current_module(nil);
        loadtype('byte',compiler.deftypes.u8inttype);
        loadtype('shortint',compiler.deftypes.s8inttype);
        loadtype('word',compiler.deftypes.u16inttype);
        loadtype('smallint',compiler.deftypes.s16inttype);
        loadtype('uint24',compiler.deftypes.u24inttype);
        loadtype('sint24',compiler.deftypes.s24inttype);
        loadtype('ulong',compiler.deftypes.u32inttype);
        loadtype('longint',compiler.deftypes.s32inttype);
        loadtype('uint40',compiler.deftypes.u40inttype);
        loadtype('sint40',compiler.deftypes.s40inttype);
        loadtype('uint48',compiler.deftypes.u48inttype);
        loadtype('sint48',compiler.deftypes.s48inttype);
        loadtype('uint56',compiler.deftypes.u56inttype);
        loadtype('sint56',compiler.deftypes.s56inttype);
        loadtype('qword',compiler.deftypes.u64inttype);
        loadtype('int64',compiler.deftypes.s64inttype);
        loadtype('uint128',compiler.deftypes.u128inttype);
        loadtype('int128',compiler.deftypes.s128inttype);
        loadtype('undefined',compiler.deftypes.cundefinedtype);
        loadtype('formal',compiler.deftypes.cformaltype);
        loadtype('typedformal',compiler.deftypes.ctypedformaltype);
        loadtype('void',compiler.deftypes.voidtype);
        loadtype('void_pointer',compiler.deftypes.voidpointertype);
        loadtype('ansichar',compiler.deftypes.cansichartype);
        loadtype('widechar',compiler.deftypes.cwidechartype);
        loadtype('shortstring',compiler.deftypes.cshortstringtype);
        loadtype('longstring',compiler.deftypes.clongstringtype);
        loadtype('ansistring',compiler.deftypes.cansistringtype);
        loadtype('widestring',compiler.deftypes.cwidestringtype);
        loadtype('unicodestring',compiler.deftypes.cunicodestringtype);
        loadtype('openshortstring',compiler.deftypes.openshortstringtype);
        loadtype('openchararray',compiler.deftypes.openchararraytype);
        if compiler.globals.init_settings.fputype <> fpu_none then
          begin
            loadtype('s32real',compiler.deftypes.s32floattype);
            loadtype('s64real',s64floattype);
            loadtype('s80real',s80floattype);
            loadtype('sc80real',compiler.deftypes.sc80floattype);
          end;
        loadtype('s64currency',compiler.deftypes.s64currencytype);
        loadtype('boolean',compiler.deftypes.pasbool1type);
        loadtype('boolean8',compiler.deftypes.pasbool8type);
        loadtype('boolean16',compiler.deftypes.pasbool16type);
        loadtype('boolean32',compiler.deftypes.pasbool32type);
        loadtype('boolean64',compiler.deftypes.pasbool64type);
        loadtype('bytebool',compiler.deftypes.bool8type);
        loadtype('wordbool',compiler.deftypes.bool16type);
        loadtype('longbool',compiler.deftypes.bool32type);
        loadtype('qwordbool',compiler.deftypes.bool64type);
        loadtype('char_pointer',compiler.deftypes.charpointertype);
        loadtype('widechar_pointer',compiler.deftypes.widecharpointertype);
        loadtype('parentfp_void_pointer',compiler.deftypes.parentfpvoidpointertype);
{$ifdef x86}
        loadtype('void_nearpointer',compiler.deftypes.voidnearpointertype);
        loadtype('void_nearcspointer',compiler.deftypes.voidnearcspointertype);
        loadtype('void_neardspointer',compiler.deftypes.voidneardspointertype);
        loadtype('void_nearsspointer',compiler.deftypes.voidnearsspointertype);
        loadtype('void_nearespointer',compiler.deftypes.voidnearespointertype);
        loadtype('void_nearfspointer',compiler.deftypes.voidnearfspointertype);
        loadtype('void_neargspointer',compiler.deftypes.voidneargspointertype);
  {$ifdef i8086}
        loadtype('void_farpointer',compiler.deftypes.voidfarpointertype);
        loadtype('void_hugepointer',compiler.deftypes.voidhugepointertype);
        loadtype('char_nearpointer',compiler.deftypes.charnearpointertype);
        loadtype('char_farpointer',compiler.deftypes.charfarpointertype);
        loadtype('char_hugepointer',compiler.deftypes.charhugepointertype);
        loadtype('byte_farpointer',compiler.deftypes.bytefarpointertype);
        loadtype('word_farpointer',compiler.deftypes.wordfarpointertype);
        loadtype('longint_farpointer',compiler.deftypes.longintfarpointertype);
  {$endif i8086}
        loadtype('__m64',  x86_m64type);
        loadtype('__m128', x86_m128type);
        loadtype('__m128d',x86_m128dtype);
        loadtype('__m128i',x86_m128itype);
        loadtype('__m256', x86_m256type);
        loadtype('__m256d',x86_m256dtype);
        loadtype('__m256i',x86_m256itype);
{$endif x86}
{$ifdef llvm}
        loadtype('llvmbool1',compiler.deftypes.llvmbool1type);
        loadtype('metadata',llvm_metadatatype);
{$endif llvm}
{$ifdef wasm}
        loadtype('wasm_void_externref',compiler.deftypes.wasmvoidexternreftype);
{$endif wasm}
        loadtype('file',compiler.deftypes.cfiletype);
        if compiler.target.info.system=system_i386_watcom then
          pvmt_name:='lower__pvmt'
        else
          pvmt_name:='pvmt';
        if not(compiler.target.info.system in systems_managed_vm) then
          begin
            loadtype(pvmt_name,pvmttype);
            loadtype('vtblarray',vmtarraytype);
            loadtype('__vtbl_ptr_type',vmttype);
          end;
        if f_variants in compiler.globals.features then
          begin
            loadtype('variant',compiler.deftypes.cvarianttype);
            loadtype('olevariant',compiler.deftypes.colevarianttype);
          end;
        loadtype('methodpointer',compiler.deftypes.methodpointertype);
        loadtype('nestedprocpointer',compiler.deftypes.nestedprocpointertype);
        loadtype('HRESULT',compiler.deftypes.hresultdef);
        loadtype('TTYPEKIND',compiler.deftypes.typekindtype);
        set_default_int_types;
        set_default_ptr_types;
        compiler.set_current_module(oldcurrentmodule);
      end;


    procedure registernodes;
      {
        Register all possible nodes in the nodeclass array that
        will be used for loading the nodes from a ppu
      }
      begin
        nodeclass[addn]:=caddnode;
        nodeclass[muln]:=caddnode;
        nodeclass[subn]:=caddnode;
        nodeclass[divn]:=cmoddivnode;
        nodeclass[symdifn]:=caddnode;
        nodeclass[modn]:=cmoddivnode;
        nodeclass[assignn]:=cassignmentnode;
        nodeclass[loadn]:=cloadnode;
        nodeclass[rangen]:=crangenode;
        nodeclass[ltn]:=caddnode;
        nodeclass[lten]:=caddnode;
        nodeclass[gtn]:=caddnode;
        nodeclass[gten]:=caddnode;
        nodeclass[equaln]:=caddnode;
        nodeclass[unequaln]:=caddnode;
        nodeclass[inn]:=cinnode;
        nodeclass[orn]:=caddnode;
        nodeclass[xorn]:=caddnode;
        nodeclass[shrn]:=cshlshrnode;
        nodeclass[shln]:=cshlshrnode;
        nodeclass[slashn]:=caddnode;
        nodeclass[andn]:=caddnode;
        nodeclass[subscriptn]:=csubscriptnode;
        nodeclass[derefn]:=cderefnode;
        nodeclass[addrn]:=caddrnode;
        nodeclass[ordconstn]:=cordconstnode;
        nodeclass[typeconvn]:=ctypeconvnode;
        nodeclass[calln]:=ccallnode;
        nodeclass[callparan]:=ccallparanode;
        nodeclass[realconstn]:=crealconstnode;
        nodeclass[unaryminusn]:=cunaryminusnode;
        nodeclass[unaryplusn]:=cunaryplusnode;
        nodeclass[asmn]:=casmnode;
        nodeclass[vecn]:=cvecnode;
        nodeclass[pointerconstn]:=cpointerconstnode;
        nodeclass[stringconstn]:=cstringconstnode;
        nodeclass[notn]:=cnotnode;
        nodeclass[inlinen]:=cinlinenode;
        nodeclass[niln]:=cnilnode;
        nodeclass[errorn]:=cerrornode;
        nodeclass[typen]:=ctypenode;
        nodeclass[setelementn]:=csetelementnode;
        nodeclass[setconstn]:=csetconstnode;
        nodeclass[blockn]:=cblocknode;
        nodeclass[statementn]:=cstatementnode;
        nodeclass[ifn]:=cifnode;
        nodeclass[breakn]:=cbreaknode;
        nodeclass[continuen]:=ccontinuenode;
        nodeclass[whilerepeatn]:=cwhilerepeatnode;
        nodeclass[forn]:=cfornode;
        nodeclass[exitn]:=cexitnode;
        nodeclass[casen]:=ccasenode;
        nodeclass[labeln]:=clabelnode;
        nodeclass[goton]:=cgotonode;
        nodeclass[tryexceptn]:=ctryexceptnode;
        nodeclass[raisen]:=craisenode;
        nodeclass[tryfinallyn]:=ctryfinallynode;
        nodeclass[onn]:=connode;
        nodeclass[isn]:=cisnode;
        nodeclass[asn]:=casnode;
        nodeclass[starstarn]:=caddnode;
        nodeclass[arrayconstructorn]:=carrayconstructornode;
        nodeclass[arrayconstructorrangen]:=carrayconstructorrangenode;
        nodeclass[tempcreaten]:=ctempcreatenode;
        nodeclass[temprefn]:=ctemprefnode;
        nodeclass[tempdeleten]:=ctempdeletenode;
        nodeclass[addoptn]:=caddnode;
        nodeclass[nothingn]:=cnothingnode;
        nodeclass[loadvmtaddrn]:=cloadvmtaddrnode;
        nodeclass[guidconstn]:=cguidconstnode;
        nodeclass[rttin]:=crttinode;
        nodeclass[loadparentfpn]:=cloadparentfpnode;
      end;


    procedure registertais;
      {
        Register all possible tais in the taiclass array that
        will be used for loading the tais from a ppu
      }
      begin
        aiclass[ait_none]:=nil;
        aiclass[ait_align]:=tai_align;
        aiclass[ait_section]:=tai_section;
        aiclass[ait_comment]:=tai_comment;
        aiclass[ait_string]:=tai_string;
        aiclass[ait_instruction]:=taicpu;
        aiclass[ait_datablock]:=tai_datablock;
        aiclass[ait_symbol]:=tai_symbol;
        aiclass[ait_symbol_end]:=tai_symbol_end;
        aiclass[ait_directive]:=tai_directive;
        aiclass[ait_label]:=tai_label;
        aiclass[ait_const]:=tai_const;
        aiclass[ait_realconst]:=tai_realconst;
        aiclass[ait_stab]:=tai_stab;
        aiclass[ait_force_line]:=tai_force_line;
        aiclass[ait_function_name]:=tai_function_name;
        aiclass[ait_symbolpair]:=tai_symbolpair;
        aiclass[ait_cutobject]:=tai_cutobject;
        aiclass[ait_regalloc]:=tai_regalloc;
        aiclass[ait_tempalloc]:=tai_tempalloc;
        aiclass[ait_marker]:=tai_marker;
        aiclass[ait_seh_directive]:=tai_seh_directive;
{$ifdef JVM}
        aiclass[ait_jvar]:=tai_jvar;
        aiclass[ait_jcatch]:=tai_jcatch;
{$endif JVM}
      end;

end.
