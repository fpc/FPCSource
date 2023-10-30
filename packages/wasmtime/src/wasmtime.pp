{
   This file is part of the Free Pascal run time library.
   Copyright (c) 2022 by Michael Van Canneyt, member of the
   Free Pascal development team

   WasmTime DLL import unit

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

**********************************************************************}

{$mode objfpc}
{$IFNDEF FPC_DOTTEDUNITS}
unit wasmtime;
{$ENDIF FPC_DOTTEDUNITS}
interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.CTypes;
{$ELSE FPC_DOTTEDUNITS}
uses
  ctypes;
{$ENDIF FPC_DOTTEDUNITS}

{
  Automatically converted by H2Pas 1.0.0 from wt.h
  The following command line parameters were used:
    -u
    wasmtime
    -l
    wasmtime
    -o
    wasmtime.pp
    -p
    -P
    -t
    -C
    -c
    wt.h
}
Const
  {$IFDEF WINDOWS}
  libwasmtime = 'wasmtime.dll';
  {$ELSE}
    {$IFDEF DARWIN}
      libwasmtime = 'libwasmtime.dylib';
    {$ELSE}
      libwasmtime = 'libwasmtime.so';
    {$ENDIF}
  {$ENDIF}

Const
  WASMTIME_EXTERN_FUNC     = 0;
  WASMTIME_EXTERN_GLOBAL   = 1;
  WASMTIME_EXTERN_TABLE    = 2;
  WASMTIME_EXTERN_MEMORY   = 3;
  WASMTIME_EXTERN_INSTANCE = 4;
  WASMTIME_EXTERN_MODULE   = 5;

  WASMTIME_I32       = 0;
  WASMTIME_I64       = 1;
  WASMTIME_F32       = 2;
  WASMTIME_F64       = 3;
  WASMTIME_V128      = 4;
  WASMTIME_FUNCREF   = 5;
  WASMTIME_EXTERNREF = 6;

{ Pointers to basic pascal types, inserted by h2pas conversion program.}
{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}
Type
  T_Bool = Byte;
  PLongint  = ^Longint;
  PSmallInt = ^SmallInt;
  PByte     = ^Byte;
  PWord     = ^Word;
  PDWord    = ^DWord;
  PDouble   = ^Double;
  Byte_T    = byte;
  Float32_t = Single;
  FLoat64_t = Double;
  Size_t    = Int64; 
  Uint32_t  = Cardinal;
  Uint64_t  = QWord;
  Uint8_t   = Byte;

Type
  Pbyte_t  = ^byte_t;
  Pfloat32_t  = ^float32_t;
  Pfloat64_t  = ^float64_t;
  Psize_t  = ^size_t;
  Puint32_t  = ^uint32_t;
  Puint64_t  = ^uint64_t;
  Puint8_t  = ^uint8_t;
  Pwasi_config_t  = ^Twasi_config_t;
  Pwasm_byte_t  = ^Twasm_byte_t;
  Pwasm_byte_vec_t  = ^Twasm_byte_vec_t;
  Pwasm_config_t  = ^Twasm_config_t;
  Pwasm_engine_t  = ^Twasm_engine_t;
  Pwasm_exporttype_t  = ^Twasm_exporttype_t;
  Pwasm_exporttype_vec_t  = ^Twasm_exporttype_vec_t;
  Pwasm_extern_t  = ^Twasm_extern_t;
  Pwasm_extern_vec_t  = ^Twasm_extern_vec_t;
  Pwasm_externkind_enum  = ^Twasm_externkind_enum;
  Pwasm_externkind_t  = ^Twasm_externkind_t;
  Pwasm_externtype_t  = ^Twasm_externtype_t;
  Pwasm_externtype_vec_t  = ^Twasm_externtype_vec_t;
  Pwasm_foreign_t  = ^Twasm_foreign_t;
  Pwasm_frame_t  = ^Twasm_frame_t;
  Pwasm_frame_vec_t  = ^Twasm_frame_vec_t;
  Pwasm_func_callback_t  = ^Twasm_func_callback_t;
  Pwasm_func_callback_with_env_t  = ^Twasm_func_callback_with_env_t;
  Pwasm_func_t  = ^Twasm_func_t;
  Pwasm_functype_t  = ^Twasm_functype_t;
  Pwasm_functype_vec_t  = ^Twasm_functype_vec_t;
  Pwasm_global_t  = ^Twasm_global_t;
  Pwasm_globaltype_t  = ^Twasm_globaltype_t;
  Pwasm_globaltype_vec_t  = ^Twasm_globaltype_vec_t;
  Pwasm_importtype_t  = ^Twasm_importtype_t;
  Pwasm_importtype_vec_t  = ^Twasm_importtype_vec_t;
  Pwasm_instance_t  = ^Twasm_instance_t;
  Pwasm_limits_t  = ^Twasm_limits_t;
  Pwasm_memory_pages_t  = ^Twasm_memory_pages_t;
  Pwasm_memory_t  = ^Twasm_memory_t;
  Pwasm_memorytype_t  = ^Twasm_memorytype_t;
  Pwasm_memorytype_vec_t  = ^Twasm_memorytype_vec_t;
  Pwasm_message_t  = ^Twasm_message_t;
  Pwasm_module_t  = ^Twasm_module_t;
  Pwasm_mutability_enum  = ^Twasm_mutability_enum;
  Pwasm_mutability_t  = ^Twasm_mutability_t;
  Pwasm_name_t  = ^Twasm_name_t;
  Pwasm_ref_t  = ^Twasm_ref_t;
  Pwasm_shared_module_t  = ^Twasm_shared_module_t;
  Pwasm_store_t  = ^Twasm_store_t;
  Pwasm_table_size_t  = ^Twasm_table_size_t;
  Pwasm_table_t  = ^Twasm_table_t;
  Pwasm_tabletype_t  = ^Twasm_tabletype_t;
  Pwasm_tabletype_vec_t  = ^Twasm_tabletype_vec_t;
  Pwasm_trap_t  = ^Twasm_trap_t;
  Pwasm_val_t  = ^Twasm_val_t;
  Pwasm_val_vec_t  = ^Twasm_val_vec_t;
  Pwasm_valkind_enum  = ^Twasm_valkind_enum;
  Pwasm_valkind_t  = ^Twasm_valkind_t;
  Pwasm_valtype_t  = ^Twasm_valtype_t;
  Pwasm_valtype_vec_t  = ^Twasm_valtype_vec_t;
  Pwasmtime_caller_t  = ^Twasmtime_caller_t;
  Pwasmtime_context_t  = ^Twasmtime_context_t;
  Pwasmtime_error_t  = ^Twasmtime_error_t;
  Pwasmtime_extern  = ^Twasmtime_extern;
  Pwasmtime_extern_kind_t  = ^Twasmtime_extern_kind_t;
  Pwasmtime_extern_t  = ^Twasmtime_extern_t;
  Pwasmtime_extern_union  = ^Twasmtime_extern_union;
  Pwasmtime_extern_union_t  = ^Twasmtime_extern_union_t;
  Pwasmtime_externref_t  = ^Twasmtime_externref_t;
  Pwasmtime_func  = ^Twasmtime_func;
  Pwasmtime_func_callback_t  = ^Twasmtime_func_callback_t;
  Pwasmtime_func_t  = ^Twasmtime_func_t;
  Pwasmtime_func_unchecked_callback_t  = ^Twasmtime_func_unchecked_callback_t;
  Pwasmtime_global  = ^Twasmtime_global;
  Pwasmtime_global_t  = ^Twasmtime_global_t;
  Pwasmtime_instance  = ^Twasmtime_instance;
  Pwasmtime_instance_t  = ^Twasmtime_instance_t;
  Pwasmtime_instancetype_t  = ^Twasmtime_instancetype_t;
  Pwasmtime_interrupt_handle_t  = ^Twasmtime_interrupt_handle_t;
  Pwasmtime_linker_t  = ^Twasmtime_linker_t;
  Pwasmtime_memory  = ^Twasmtime_memory;
  Pwasmtime_memory_t  = ^Twasmtime_memory_t;
  Pwasmtime_module_t  = ^Twasmtime_module_t;
  Pwasmtime_moduletype_t  = ^Twasmtime_moduletype_t;
  Pwasmtime_opt_level_enum  = ^Twasmtime_opt_level_enum;
  Pwasmtime_opt_level_t  = ^Twasmtime_opt_level_t;
  Pwasmtime_profiling_strategy_enum  = ^Twasmtime_profiling_strategy_enum;
  Pwasmtime_profiling_strategy_t  = ^Twasmtime_profiling_strategy_t;
  Pwasmtime_store_t  = ^Twasmtime_store_t;
  Pwasmtime_strategy_enum  = ^Twasmtime_strategy_enum;
  Pwasmtime_strategy_t  = ^Twasmtime_strategy_t;
  Pwasmtime_table  = ^Twasmtime_table;
  Pwasmtime_table_t  = ^Twasmtime_table_t;
  Pwasmtime_trap_code_enum  = ^Twasmtime_trap_code_enum;
  Pwasmtime_trap_code_t  = ^Twasmtime_trap_code_t;
  Pwasmtime_v128  = ^Twasmtime_v128;
  Pwasmtime_val  = ^Twasmtime_val;
  Pwasmtime_val_raw  = ^Twasmtime_val_raw;
  Pwasmtime_val_raw_t  = ^Twasmtime_val_raw_t;
  Pwasmtime_val_t  = ^Twasmtime_val_t;
  Pwasmtime_valkind_t  = ^Twasmtime_valkind_t;
  Pwasmtime_valunion  = ^Twasmtime_valunion;
  Pwasmtime_valunion_t  = ^Twasmtime_valunion_t;
  PPwasm_trap_t= ^Pwasm_trap_t;
  PPwasmtime_module_t = ^Pwasmtime_module_t;
  
  Tcint = cint;
  Tbyte_t = byte;
  Tfloat32_t = single;
  Tfloat64_t = double;
  Twasm_byte_t = Tbyte_t;

  Twasm_byte_vec_t = record
      size : size_t;
      data : Pwasm_byte_t;
    end;
  Twasm_name_t = Twasm_byte_vec_t;
{
static inline void wasm_name_new_from_string(
  wasm_name_t* out, const AnsiChar* s
) 
  wasm_byte_vec_new(out, strlen(s), s);


static inline void wasm_name_new_from_string_nt(
  wasm_name_t* out, const AnsiChar* s
) 
  wasm_byte_vec_new(out, strlen(s) + 1, s);


 }
  Tuint8_t = byte;
  Tuint32_t = cardinal;
  Tuint64_t = qword;
  TSize_t = size_t;
  Tint32_t = longint;
  Tint64_t = int64;
  
  // Opaque record defs
  Twasmtime_moduletype_t = record
  end;
  Twasmtime_module_t = record
  end;
  Twasmtime_store_t = record
  end;
  Twasmtime_context_t = record
  end;
  Twasmtime_interrupt_handle_t = record
  end;
  Twasmtime_instancetype_t = record
  end;  
  Twasmtime_linker_t = record
  end;  
  Twasmtime_caller_t = record
  end;  
  Twasmtime_externref_t = record
  end;
  Twasi_config_t = record
  end;   
  Twasm_config_t = record
  end;   
  Twasm_engine_t = record
  end;   
  Twasm_exportype_t = record
  end;   
  Twasm_extern_t = record
  end;   
  Twasm_externtype_t = record
  end;   
  Twasm_foreign_t = record
  end;   
  Twasm_frame_t = record
  end;   
  Twasm_func_t = record
  end;   
  Twasm_functype_t = record
  end;   
  Twasm_global_t = record
  end;   
  Twasm_globaltype_t = record
  end;   
  Twasm_instance_t = record
  end;   
  Twasm_memory_t = record
  end;   
  Twasm_memorytype_t = record
  end;   
  Twasm_module_t = record
  end;   
  Twasm_sharedmodule_t = record
  end;   
  Twasm_store_t = record
  end;   
  Twasm_table_t = record
  end;   
  Twasm_tabletype_t = record
  end;   
  Twasm_trap_t = record
  end;   
  Twasm_valtime_t = record
  end;   
  Twasm_exporttype_t = record
  end;
  Twasm_importtype_t = record
  end;
  Twasm_shared_module_t = record
  end;
  Twasm_valtype_t = record
  end;
  PPwasm_functype_t = ^Pwasm_functype_t;

  Twasm_mutability_t = Tuint8_t;
  Twasm_mutability_enum = (WASM_CONST,WASM_VAR);

  Twasmtime_opt_level_enum = (WASMTIME_OPT_LEVEL_NONE,WASMTIME_OPT_LEVEL_SPEED,
      WASMTIME_OPT_LEVEL_SPEED_AND_SIZE);
      
  Twasm_limits_t = record
      min : Tuint32_t;
      max : Tuint32_t;
    end;
{
static const uint32_t wasm_limits_max_default = 0xffffffff;
 }
  Twasm_valtype_vec_t = record
      size : Tsize_t;
      data : ^Pwasm_valtype_t;
    end;
  Twasm_valkind_t = Tuint8_t;
  Twasm_valkind_enum = (WASM_I32,WASM_I64,WASM_F32,WASM_F64,WASM_ANYREF := 128,
    WASM_FUNCREF);

  Twasm_functype_vec_t = record
      size : Tsize_t;
      data : ^Pwasm_functype_t;
    end;
  Twasm_globaltype_vec_t = record
      size : Tsize_t;
      data : ^Pwasm_globaltype_t;
    end;
  Twasm_tabletype_vec_t = record
      size : Tsize_t;
      data : ^Pwasm_tabletype_t;
    end;
  Twasm_memorytype_vec_t = record
      size : Tsize_t;
      data : ^Pwasm_memorytype_t;
    end;
  Twasm_externtype_vec_t = record
      size : Tsize_t;
      data : ^Pwasm_externtype_t;
    end;
  Twasm_externkind_t = Tuint8_t;
  Twasm_externkind_enum = (WASM_EXTERN_FUNC,WASM_EXTERN_GLOBAL,WASM_EXTERN_TABLE,
    WASM_EXTERN_MEMORY);

  Twasm_importtype_vec_t = record
      size : Tsize_t;
      data : ^Pwasm_importtype_t;
    end;
  Twasm_exporttype_vec_t = record
      size : Tsize_t;
      data : ^Pwasm_exporttype_t;
    end;
  Twasm_ref_t = record
      {undefined structure}
    end;


  Twasm_val_t = record
      kind : Twasm_valkind_t;
      of_ : record
          case longint of
            0 : ( i32 : Tint32_t );
            1 : ( i64 : Tint64_t );
            2 : ( f32 : Tfloat32_t );
            3 : ( f64 : Tfloat64_t );
            4 : ( ref : Pwasm_ref_t );
          end;
    end;
  Twasm_val_vec_t = record
      size : Tsize_t;
      data : Pwasm_val_t;
    end;
  Twasm_frame_vec_t = record
      size : Tsize_t;
      data : ^Pwasm_frame_t;
    end;
  Twasm_message_t = Twasm_name_t;
  Twasm_func_callback_t = function (args:Pwasm_val_vec_t; results:Pwasm_val_vec_t):Pwasm_trap_t;cdecl;
  
  Twasm_func_callback_with_env_t = function (env:pointer; args:Pwasm_val_vec_t; results:Pwasm_val_vec_t):Pwasm_trap_t;cdecl;
  Twasm_table_size_t = Tuint32_t;
  Twasm_memory_pages_t = Tuint32_t;
{
static  MEMORY_PAGE_SIZE = 0x10000;
 }
  Twasm_extern_vec_t = record
      size : Tsize_t;
      data : ^Pwasm_extern_t;
    end;
  Twasmtime_error_t = cint;
  Twasmtime_error = Twasmtime_error_t;
  Twasmtime_strategy_t = Tuint8_t;
  Twasmtime_strategy_enum = (WASMTIME_STRATEGY_AUTO,WASMTIME_STRATEGY_CRANELIFT
    );


  Twasmtime_opt_level_t = Tuint8_t;


  Twasmtime_profiling_strategy_t = Tuint8_t;
  Twasmtime_profiling_strategy_enum = (WASMTIME_PROFILING_STRATEGY_NONE,WASMTIME_PROFILING_STRATEGY_JITDUMP,
    WASMTIME_PROFILING_STRATEGY_VTUNE);



  Twasmtime_moduletype = Twasmtime_moduletype_t;
  Twasmtime_module = Twasmtime_module_t;
  Twasmtime_store = Twasmtime_store_t;
  Twasmtime_context = Twasmtime_context_t;
  Twasmtime_interrupt_handle = Twasmtime_interrupt_handle_t;
  Twasmtime_func = record
      store_id : Tuint64_t;
      index : Tsize_t;
    end;
  Twasmtime_func_t = Twasmtime_func;

  Twasmtime_table = record
      store_id : Tuint64_t;
      index : Tsize_t;
    end;
  Twasmtime_table_t = Twasmtime_table;

  Twasmtime_memory = record
      store_id : Tuint64_t;
      index : Tsize_t;
    end;
  Twasmtime_memory_t = Twasmtime_memory;

  Twasmtime_instance = record
      store_id : Tuint64_t;
      index : Tsize_t;
    end;
  Twasmtime_instance_t = Twasmtime_instance;

  Twasmtime_global = record
      store_id : Tuint64_t;
      index : Tsize_t;
    end;
  Twasmtime_global_t = Twasmtime_global;

  Twasmtime_extern_kind_t = Tuint8_t;

  Twasmtime_extern_union = record
      case longint of
        0 : ( func : Twasmtime_func_t );
        1 : ( global : Twasmtime_global_t );
        2 : ( table : Twasmtime_table_t );
        3 : ( memory : Twasmtime_memory_t );
        4 : ( instance : Twasmtime_instance_t );
        5 : ( module : Pwasmtime_module_t );
      end;
  Twasmtime_extern_union_t = Twasmtime_extern_union;

  Twasmtime_extern = record
      kind : Twasmtime_extern_kind_t;
      of_ : Twasmtime_extern_union_t;
    end;
  Twasmtime_extern_t = Twasmtime_extern;
  Twasmtime_externref = Twasmtime_externref_t;
  Twasmtime_valkind_t = Tuint8_t;

  Twasmtime_v128 = array[0..15] of Tuint8_t;

  Twasmtime_valunion = record
      case longint of
        0 : ( i32 : Tint32_t );
        1 : ( i64 : Tint64_t );
        2 : ( f32 : Tfloat32_t );
        3 : ( f64 : Tfloat64_t );
        4 : ( funcref : Twasmtime_func_t );
        5 : ( externref : Pwasmtime_externref_t );
        6 : ( v128 : Twasmtime_v128 );
      end;
  Twasmtime_valunion_t = Twasmtime_valunion;

  Twasmtime_val_raw = record
      case longint of
        0 : ( i32 : Tint32_t );
        1 : ( i64 : Tint64_t );
        2 : ( f32 : Tfloat32_t );
        3 : ( f64 : Tfloat64_t );
        4 : ( v128 : Twasmtime_v128 );
        5 : ( funcref : Tsize_t );
        6 : ( externref : Tsize_t );
      end;
  Twasmtime_val_raw_t = Twasmtime_val_raw;

  Twasmtime_val = record
      kind : Twasmtime_valkind_t;
      of_ : Twasmtime_valunion_t;
    end;
  Twasmtime_val_t = Twasmtime_val;
  Twasmtime_caller = Twasmtime_caller_t;

  Twasmtime_func_callback_t = function (env:pointer; caller:Pwasmtime_caller_t; args:Pwasmtime_val_t; nargs:Tsize_t; results:Pwasmtime_val_t; 
               nresults:Tsize_t):Pwasm_trap_t;cdecl;
  Twasmtime_func_unchecked_callback_t = function (env:pointer; caller:Pwasmtime_caller_t; args_and_results:Pwasmtime_val_raw_t):Pwasm_trap_t;cdecl;
  Twasmtime_instancetype = Twasmtime_instancetype_t;
  Twasmtime_linker = Twasmtime_linker_t;
  Twasmtime_trap_code_t = Tuint8_t;
  Twasmtime_trap_code_enum = (WASMTIME_TRAP_CODE_STACK_OVERFLOW,WASMTIME_TRAP_CODE_MEMORY_OUT_OF_BOUNDS,
    WASMTIME_TRAP_CODE_HEAP_MISALIGNED,WASMTIME_TRAP_CODE_TABLE_OUT_OF_BOUNDS,
    WASMTIME_TRAP_CODE_INDIRECT_CALL_TO_NULL,
    WASMTIME_TRAP_CODE_BAD_SIGNATURE,WASMTIME_TRAP_CODE_INTEGER_OVERFLOW,
    WASMTIME_TRAP_CODE_INTEGER_DIVISION_BY_ZERO,
    WASMTIME_TRAP_CODE_BAD_CONVERSION_TO_INTEGER,
    WASMTIME_TRAP_CODE_UNREACHABLE_CODE_REACHED,
    WASMTIME_TRAP_CODE_INTERRUPT);
    
  TFinalizer = procedure (_para1:pointer); cdecl;

var

  wasm_byte_vec_new_empty : procedure(_out:Pwasm_byte_vec_t); cdecl;
  wasm_byte_vec_new_uninitialized : procedure(_out:Pwasm_byte_vec_t; _para2:Tsize_t); cdecl;
  wasm_byte_vec_new : procedure(_out:Pwasm_byte_vec_t; _para2:Tsize_t; _para3:Pwasm_byte_t); cdecl;
  wasm_byte_vec_copy : procedure(_out:Pwasm_byte_vec_t; _para2:Pwasm_byte_vec_t); cdecl;
  wasm_byte_vec_delete : procedure(_para1:Pwasm_byte_vec_t); cdecl;
  wasm_config_delete : procedure(_para1:Pwasm_config_t); cdecl;
  wasm_config_new : function:Pwasm_config_t; cdecl;
  wasm_engine_delete : procedure(_para1:Pwasm_engine_t); cdecl;
  wasm_engine_new : function:Pwasm_engine_t; cdecl;
  wasm_engine_new_with_config : function(_para1:Pwasm_config_t):Pwasm_engine_t; cdecl;
  wasm_store_delete : procedure(_para1:Pwasm_store_t); cdecl;
  wasm_store_new : function(_para1:Pwasm_engine_t):Pwasm_store_t; cdecl;
  wasm_valtype_delete : procedure(_para1:Pwasm_valtype_t); cdecl;
  wasm_valtype_vec_new_empty : procedure(_out:Pwasm_valtype_vec_t); cdecl;
  wasm_valtype_vec_new_uninitialized : procedure(_out:Pwasm_valtype_vec_t; _para2:Tsize_t); cdecl;
  wasm_valtype_vec_new : procedure(_out:Pwasm_valtype_vec_t; _para2:Tsize_t; data:Pwasm_valtype_t); cdecl;
  wasm_valtype_vec_copy : procedure(_out:Pwasm_valtype_vec_t; _para2:Pwasm_valtype_vec_t); cdecl;
  wasm_valtype_vec_delete : procedure(_para1:Pwasm_valtype_vec_t); cdecl;
  wasm_valtype_copy : function(_para1:Pwasm_valtype_t):Pwasm_valtype_t; cdecl;
  wasm_valtype_new : function(_para1:Twasm_valkind_t):Pwasm_valtype_t; cdecl;
  wasm_valtype_kind : function(_para1:Pwasm_valtype_t):Twasm_valkind_t; cdecl;
  wasm_functype_delete : procedure(_para1:Pwasm_functype_t); cdecl;
  wasm_functype_vec_new_empty : procedure(_out:Pwasm_functype_vec_t); cdecl;
  wasm_functype_vec_new_uninitialized : procedure(_out:Pwasm_functype_vec_t; _para2:Tsize_t); cdecl;
  wasm_functype_vec_new : procedure(_out:Pwasm_functype_vec_t; _para2:Tsize_t; _para3:PPwasm_functype_t); cdecl;
  wasm_functype_vec_copy : procedure(_out:Pwasm_functype_vec_t; _para2:Pwasm_functype_vec_t); cdecl;
  wasm_functype_vec_delete : procedure(_para1:Pwasm_functype_vec_t); cdecl;
  wasm_functype_copy : function(_para1:Pwasm_functype_t):Pwasm_functype_t; cdecl;
  wasm_functype_new : function(params:Pwasm_valtype_vec_t; results:Pwasm_valtype_vec_t):Pwasm_functype_t; cdecl;
  wasm_functype_params : function(_para1:Pwasm_functype_t):Pwasm_valtype_vec_t; cdecl;
  wasm_functype_results : function(_para1:Pwasm_functype_t):Pwasm_valtype_vec_t; cdecl;
  wasm_globaltype_delete : procedure(_para1:Pwasm_globaltype_t); cdecl;
  wasm_globaltype_vec_new_empty : procedure(_out:Pwasm_globaltype_vec_t); cdecl;
  wasm_globaltype_vec_new_uninitialized : procedure(_out:Pwasm_globaltype_vec_t; _para2:Tsize_t); cdecl;
  wasm_globaltype_vec_new : procedure(_out:Pwasm_globaltype_vec_t; _para2:Tsize_t; data:Pwasm_globaltype_t); cdecl;
  wasm_globaltype_vec_copy : procedure(_out:Pwasm_globaltype_vec_t; data:Pwasm_globaltype_vec_t); cdecl;
  wasm_globaltype_vec_delete : procedure(_para1:Pwasm_globaltype_vec_t); cdecl;
  wasm_globaltype_copy : function(_para1:Pwasm_globaltype_t):Pwasm_globaltype_t; cdecl;
  wasm_globaltype_new : function(_para1:Pwasm_valtype_t; _para2:Twasm_mutability_t):Pwasm_globaltype_t; cdecl;
  wasm_globaltype_content : function(_para1:Pwasm_globaltype_t):Pwasm_valtype_t; cdecl;
  wasm_globaltype_mutability : function(_para1:Pwasm_globaltype_t):Twasm_mutability_t; cdecl;
  wasm_tabletype_delete : procedure(_para1:Pwasm_tabletype_t); cdecl;
  wasm_tabletype_vec_new_empty : procedure(_out:Pwasm_tabletype_vec_t); cdecl;
  wasm_tabletype_vec_new_uninitialized : procedure(_out:Pwasm_tabletype_vec_t; _para2:Tsize_t); cdecl;
  wasm_tabletype_vec_new : procedure(_out:Pwasm_tabletype_vec_t; _para2:Tsize_t; data:Pwasm_tabletype_t); cdecl;
  wasm_tabletype_vec_copy : procedure(_out:Pwasm_tabletype_vec_t; data:Pwasm_tabletype_vec_t); cdecl;
  wasm_tabletype_vec_delete : procedure(_para1:Pwasm_tabletype_vec_t); cdecl;
  wasm_tabletype_copy : function(_para1:Pwasm_tabletype_t):Pwasm_tabletype_t; cdecl;
  wasm_tabletype_new : function(_para1:Pwasm_valtype_t; _para2:Pwasm_limits_t):Pwasm_tabletype_t; cdecl;
  wasm_tabletype_element : function(_para1:Pwasm_tabletype_t):Pwasm_valtype_t; cdecl;
  wasm_tabletype_limits : function(_para1:Pwasm_tabletype_t):Pwasm_limits_t; cdecl;
  wasm_memorytype_delete : procedure(_para1:Pwasm_memorytype_t); cdecl;
  wasm_memorytype_vec_new_empty : procedure(_out:Pwasm_memorytype_vec_t); cdecl;
  wasm_memorytype_vec_new_uninitialized : procedure(_out:Pwasm_memorytype_vec_t; _para2:Tsize_t); cdecl;
  wasm_memorytype_vec_new : procedure(_out:Pwasm_memorytype_vec_t; _para2:Tsize_t; data:Pwasm_memorytype_t); cdecl;
  wasm_memorytype_vec_copy : procedure(_out:Pwasm_memorytype_vec_t; data:Pwasm_memorytype_vec_t); cdecl;
  wasm_memorytype_vec_delete : procedure(_para1:Pwasm_memorytype_vec_t); cdecl;
  wasm_memorytype_copy : function(_para1:Pwasm_memorytype_t):Pwasm_memorytype_t; cdecl;
  wasm_memorytype_new : function(_para1:Pwasm_limits_t):Pwasm_memorytype_t; cdecl;
  wasm_memorytype_limits : function(_para1:Pwasm_memorytype_t):Pwasm_limits_t; cdecl;
  wasm_externtype_delete : procedure(_para1:Pwasm_externtype_t); cdecl;
  wasm_externtype_vec_new_empty : procedure(_out:Pwasm_externtype_vec_t); cdecl;
  wasm_externtype_vec_new_uninitialized : procedure(_out:Pwasm_externtype_vec_t; _para2:Tsize_t); cdecl;
  wasm_externtype_vec_new : procedure(_out:Pwasm_externtype_vec_t; _para2:Tsize_t; data:Pwasm_externtype_t); cdecl;
  wasm_externtype_vec_copy : procedure(_out:Pwasm_externtype_vec_t; _para2:Pwasm_externtype_vec_t); cdecl;
  wasm_externtype_vec_delete : procedure(_para1:Pwasm_externtype_vec_t); cdecl;
  wasm_externtype_copy : function(_para1:Pwasm_externtype_t):Pwasm_externtype_t; cdecl;
  wasm_externtype_kind : function(_para1:Pwasm_externtype_t):Twasm_externkind_t; cdecl;
  wasm_functype_as_externtype : function(_para1:Pwasm_functype_t):Pwasm_externtype_t; cdecl;
  wasm_globaltype_as_externtype : function(_para1:Pwasm_globaltype_t):Pwasm_externtype_t; cdecl;
  wasm_tabletype_as_externtype : function(_para1:Pwasm_tabletype_t):Pwasm_externtype_t; cdecl;
  wasm_memorytype_as_externtype : function(_para1:Pwasm_memorytype_t):Pwasm_externtype_t; cdecl;
  wasm_externtype_as_functype : function(_para1:Pwasm_externtype_t):Pwasm_functype_t; cdecl;
  wasm_externtype_as_globaltype : function(_para1:Pwasm_externtype_t):Pwasm_globaltype_t; cdecl;
  wasm_externtype_as_tabletype : function(_para1:Pwasm_externtype_t):Pwasm_tabletype_t; cdecl;
  wasm_externtype_as_memorytype : function(_para1:Pwasm_externtype_t):Pwasm_memorytype_t; cdecl;
  wasm_functype_as_externtype_const : function(_para1:Pwasm_functype_t):Pwasm_externtype_t; cdecl;
  wasm_globaltype_as_externtype_const : function(_para1:Pwasm_globaltype_t):Pwasm_externtype_t; cdecl;
  wasm_tabletype_as_externtype_const : function(_para1:Pwasm_tabletype_t):Pwasm_externtype_t; cdecl;
  wasm_memorytype_as_externtype_const : function(_para1:Pwasm_memorytype_t):Pwasm_externtype_t; cdecl;
  wasm_externtype_as_functype_const : function(_para1:Pwasm_externtype_t):Pwasm_functype_t; cdecl;
  wasm_externtype_as_globaltype_const : function(_para1:Pwasm_externtype_t):Pwasm_globaltype_t; cdecl;
  wasm_externtype_as_tabletype_const : function(_para1:Pwasm_externtype_t):Pwasm_tabletype_t; cdecl;
  wasm_externtype_as_memorytype_const : function(_para1:Pwasm_externtype_t):Pwasm_memorytype_t; cdecl;
  wasm_importtype_delete : procedure(_para1:Pwasm_importtype_t); cdecl;
  wasm_importtype_vec_new_empty : procedure(_out:Pwasm_importtype_vec_t); cdecl;
  wasm_importtype_vec_new_uninitialized : procedure(_out:Pwasm_importtype_vec_t; _para2:Tsize_t); cdecl;
  wasm_importtype_vec_new : procedure(_out:Pwasm_importtype_vec_t; _para2:Tsize_t; data:Pwasm_importtype_t); cdecl;
  wasm_importtype_vec_copy : procedure(_out:Pwasm_importtype_vec_t; _para2:Pwasm_importtype_vec_t); cdecl;
  wasm_importtype_vec_delete : procedure(_para1:Pwasm_importtype_vec_t); cdecl;
  wasm_importtype_copy : function(_para1:Pwasm_importtype_t):Pwasm_importtype_t; cdecl;
  wasm_importtype_new : function(module:Pwasm_name_t; name:Pwasm_name_t; _para3:Pwasm_externtype_t):Pwasm_importtype_t; cdecl;
  wasm_importtype_module : function(_para1:Pwasm_importtype_t):Pwasm_name_t; cdecl;
  wasm_importtype_name : function(_para1:Pwasm_importtype_t):Pwasm_name_t; cdecl;
  wasm_importtype_type : function(_para1:Pwasm_importtype_t):Pwasm_externtype_t; cdecl;
  wasm_exporttype_delete : procedure(_para1:Pwasm_exporttype_t); cdecl;
  wasm_exporttype_vec_new_empty : procedure(_out:Pwasm_exporttype_vec_t); cdecl;
  wasm_exporttype_vec_new_uninitialized : procedure(_out:Pwasm_exporttype_vec_t; _para2:Tsize_t); cdecl;
  wasm_exporttype_vec_new : procedure(_out:Pwasm_exporttype_vec_t; _para2:Tsize_t; data:Pwasm_exporttype_t); cdecl;
  wasm_exporttype_vec_copy : procedure(_out:Pwasm_exporttype_vec_t; _para2:Pwasm_exporttype_vec_t); cdecl;
  wasm_exporttype_vec_delete : procedure(_para1:Pwasm_exporttype_vec_t); cdecl;
  wasm_exporttype_copy : function(_para1:Pwasm_exporttype_t):Pwasm_exporttype_t; cdecl;
  wasm_exporttype_new : function(_para1:Pwasm_name_t; _para2:Pwasm_externtype_t):Pwasm_exporttype_t; cdecl;
  wasm_exporttype_name : function(_para1:Pwasm_exporttype_t):Pwasm_name_t; cdecl;
  wasm_exporttype_type : function(_para1:Pwasm_exporttype_t):Pwasm_externtype_t; cdecl;
  wasm_val_delete : procedure(v:Pwasm_val_t); cdecl;
  wasm_val_copy : procedure(_out:Pwasm_val_t; _para2:Pwasm_val_t); cdecl;
  wasm_val_vec_new_empty : procedure(_out:Pwasm_val_vec_t); cdecl;
  wasm_val_vec_new_uninitialized : procedure(_out:Pwasm_val_vec_t; _para2:Tsize_t); cdecl;
  wasm_val_vec_new : procedure(_out:Pwasm_val_vec_t; _para2:Tsize_t; data:Pwasm_val_t); cdecl;
  wasm_val_vec_copy : procedure(_out:Pwasm_val_vec_t; _para2:Pwasm_val_vec_t); cdecl;
  wasm_val_vec_delete : procedure(_para1:Pwasm_val_vec_t); cdecl;
  wasm_ref_delete : procedure(_para1:Pwasm_ref_t); cdecl;
  wasm_ref_copy : function(_para1:Pwasm_ref_t):Pwasm_ref_t; cdecl;
  wasm_ref_same : function(_para1:Pwasm_ref_t; _para2:Pwasm_ref_t):T_Bool;
  wasm_ref_get_host_info : function(_para1:Pwasm_ref_t):pointer;
  wasm_ref_set_host_info : procedure(_para1:Pwasm_ref_t; _para2:pointer); cdecl;
  wasm_ref_set_host_info_with_finalizer : procedure(_para1:Pwasm_ref_t; _para2:pointer; _para3:TFInalizer); cdecl;
  wasm_frame_delete : procedure(_para1:Pwasm_frame_t); cdecl;
  wasm_frame_vec_new_empty : procedure(_out:Pwasm_frame_vec_t); cdecl;
  wasm_frame_vec_new_uninitialized : procedure(_out:Pwasm_frame_vec_t; _para2:Tsize_t); cdecl;
  wasm_frame_vec_new : procedure(_out:Pwasm_frame_vec_t; _para2:Tsize_t; data:Pwasm_frame_t); cdecl;
  wasm_frame_vec_copy : procedure(_out:Pwasm_frame_vec_t; _para2:Pwasm_frame_vec_t); cdecl;
  wasm_frame_vec_delete : procedure(_para1:Pwasm_frame_vec_t); cdecl;
  wasm_frame_copy : function(_para1:Pwasm_frame_t):Pwasm_frame_t; cdecl;
  wasm_frame_instance : function(_para1:Pwasm_frame_t):Pwasm_instance_t; cdecl;
  wasm_frame_func_index : function(_para1:Pwasm_frame_t):Tuint32_t; cdecl;
  wasm_frame_func_offset : function(_para1:Pwasm_frame_t):Tsize_t; cdecl;
  wasm_frame_module_offset : function(_para1:Pwasm_frame_t):Tsize_t; cdecl;
  wasm_trap_delete : procedure(_para1:Pwasm_trap_t); cdecl;
  wasm_trap_copy : function(_para1:Pwasm_trap_t):Pwasm_trap_t; cdecl;
  wasm_trap_same : function(_para1:Pwasm_trap_t; _para2:Pwasm_trap_t):T_Bool;
  wasm_trap_get_host_info : function(_para1:Pwasm_trap_t):pointer;
  wasm_trap_set_host_info : procedure(_para1:Pwasm_trap_t; _para2:pointer); cdecl;
  wasm_trap_set_host_info_with_finalizer : procedure(_para1:Pwasm_trap_t; _para2:pointer; _para3:TFinalizer); cdecl;
  wasm_trap_as_ref : function(_para1:Pwasm_trap_t):Pwasm_ref_t; cdecl;
  wasm_ref_as_trap : function(_para1:Pwasm_ref_t):Pwasm_trap_t; cdecl;
  wasm_trap_as_ref_const : function(_para1:Pwasm_trap_t):Pwasm_ref_t; cdecl;
  wasm_ref_as_trap_const : function(_para1:Pwasm_ref_t):Pwasm_trap_t; cdecl;
  wasm_trap_new : function(store:Pwasm_store_t; _para2:Pwasm_message_t):Pwasm_trap_t; cdecl;
  wasm_trap_message : procedure(_para1:Pwasm_trap_t; out_:Pwasm_message_t); cdecl;
  wasm_trap_origin : function(_para1:Pwasm_trap_t):Pwasm_frame_t; cdecl;
  wasm_trap_trace : procedure(_para1:Pwasm_trap_t; out_:Pwasm_frame_vec_t); cdecl;
  wasm_foreign_delete : procedure(_para1:Pwasm_foreign_t); cdecl;
  wasm_foreign_copy : function(_para1:Pwasm_foreign_t):Pwasm_foreign_t; cdecl;
  wasm_foreign_same : function(_para1:Pwasm_foreign_t; _para2:Pwasm_foreign_t):T_Bool;
  wasm_foreign_get_host_info : function(_para1:Pwasm_foreign_t):pointer;
  wasm_foreign_set_host_info : procedure(_para1:Pwasm_foreign_t; _para2:pointer); cdecl;
  wasm_foreign_set_host_info_with_finalizer : procedure(_para1:Pwasm_foreign_t; _para2:pointer; _para3:TFinalizer); cdecl;
  wasm_foreign_as_ref : function(_para1:Pwasm_foreign_t):Pwasm_ref_t; cdecl;
  wasm_ref_as_foreign : function(_para1:Pwasm_ref_t):Pwasm_foreign_t; cdecl;
  wasm_foreign_as_ref_const : function(_para1:Pwasm_foreign_t):Pwasm_ref_t; cdecl;
  wasm_ref_as_foreign_const : function(_para1:Pwasm_ref_t):Pwasm_foreign_t; cdecl;
  wasm_foreign_new : function(_para1:Pwasm_store_t):Pwasm_foreign_t; cdecl;
  wasm_module_delete : procedure(_para1:Pwasm_module_t); cdecl;
  wasm_module_copy : function(_para1:Pwasm_module_t):Pwasm_module_t; cdecl;
  wasm_module_same : function(_para1:Pwasm_module_t; _para2:Pwasm_module_t):T_Bool;
  wasm_module_get_host_info : function(_para1:Pwasm_module_t):pointer;
  wasm_module_set_host_info : procedure(_para1:Pwasm_module_t; _para2:pointer); cdecl;
  wasm_module_set_host_info_with_finalizer : procedure(_para1:Pwasm_module_t; _para2:pointer; _para3: TFinalizer); cdecl;
  wasm_module_as_ref : function(_para1:Pwasm_module_t):Pwasm_ref_t; cdecl;
  wasm_ref_as_module : function(_para1:Pwasm_ref_t):Pwasm_module_t; cdecl;
  wasm_module_as_ref_const : function(_para1:Pwasm_module_t):Pwasm_ref_t; cdecl;
  wasm_ref_as_module_const : function(_para1:Pwasm_ref_t):Pwasm_module_t; cdecl;
  wasm_shared_module_delete : procedure(_para1:Pwasm_shared_module_t); cdecl;
  wasm_module_share : function(_para1:Pwasm_module_t):Pwasm_shared_module_t; cdecl;
  wasm_module_obtain : function(_para1:Pwasm_store_t; _para2:Pwasm_shared_module_t):Pwasm_module_t; cdecl;
  wasm_module_new : function(_para1:Pwasm_store_t; binary:Pwasm_byte_vec_t):Pwasm_module_t; cdecl;
  wasm_module_validate : function(_para1:Pwasm_store_t; binary:Pwasm_byte_vec_t):T_Bool;
  wasm_module_imports : procedure(_para1:Pwasm_module_t; out_:Pwasm_importtype_vec_t); cdecl;
  wasm_module_exports : procedure(_para1:Pwasm_module_t; out_:Pwasm_exporttype_vec_t); cdecl;
  wasm_module_serialize : procedure(_para1:Pwasm_module_t; out_:Pwasm_byte_vec_t); cdecl;
  wasm_module_deserialize : function(_para1:Pwasm_store_t; _para2:Pwasm_byte_vec_t):Pwasm_module_t; cdecl;
  wasm_func_delete : procedure(_para1:Pwasm_func_t); cdecl;
  wasm_func_copy : function(_para1:Pwasm_func_t):Pwasm_func_t; cdecl;
  wasm_func_same : function(_para1:Pwasm_func_t; _para2:Pwasm_func_t):T_Bool;
  wasm_func_get_host_info : function(_para1:Pwasm_func_t):pointer;
  wasm_func_set_host_info : procedure(_para1:Pwasm_func_t; _para2:pointer); cdecl;
  wasm_func_set_host_info_with_finalizer : procedure(_para1:Pwasm_func_t; _para2:pointer; _para3:TFinalizer); cdecl;
  wasm_func_as_ref : function(_para1:Pwasm_func_t):Pwasm_ref_t; cdecl;
  wasm_ref_as_func : function(_para1:Pwasm_ref_t):Pwasm_func_t; cdecl;
  wasm_func_as_ref_const : function(_para1:Pwasm_func_t):Pwasm_ref_t; cdecl;
  wasm_ref_as_func_const : function(_para1:Pwasm_ref_t):Pwasm_func_t; cdecl;
  wasm_func_new : function(_para1:Pwasm_store_t; _para2:Pwasm_functype_t; _para3:Twasm_func_callback_t):Pwasm_func_t; cdecl;
  wasm_func_new_with_env : function(_para1:Pwasm_store_t; _type:Pwasm_functype_t; _para3:Twasm_func_callback_with_env_t; env:pointer; finalizer:TFinalizer):Pwasm_func_t; cdecl;
  wasm_func_type : function(_para1:Pwasm_func_t):Pwasm_functype_t; cdecl;
  wasm_func_param_arity : function(_para1:Pwasm_func_t):Tsize_t; cdecl;
  wasm_func_result_arity : function(_para1:Pwasm_func_t):Tsize_t; cdecl;
  wasm_func_call : function(_para1:Pwasm_func_t; args:Pwasm_val_vec_t; results:Pwasm_val_vec_t):Pwasm_trap_t; cdecl;
  wasm_global_delete : procedure(_para1:Pwasm_global_t); cdecl;
  wasm_global_copy : function(_para1:Pwasm_global_t):Pwasm_global_t; cdecl;
  wasm_global_same : function(_para1:Pwasm_global_t; _para2:Pwasm_global_t):T_Bool;
  wasm_global_get_host_info : function(_para1:Pwasm_global_t):pointer;
  wasm_global_set_host_info : procedure(_para1:Pwasm_global_t; _para2:pointer); cdecl;
  wasm_global_set_host_info_with_finalizer : procedure(_para1:Pwasm_global_t; _para2:pointer; _para3:TFinalizer); cdecl;
  wasm_global_as_ref : function(_para1:Pwasm_global_t):Pwasm_ref_t; cdecl;
  wasm_ref_as_global : function(_para1:Pwasm_ref_t):Pwasm_global_t; cdecl;
  wasm_global_as_ref_const : function(_para1:Pwasm_global_t):Pwasm_ref_t; cdecl;
  wasm_ref_as_global_const : function(_para1:Pwasm_ref_t):Pwasm_global_t; cdecl;
  wasm_global_new : function(_para1:Pwasm_store_t; _para2:Pwasm_globaltype_t; _para3:Pwasm_val_t):Pwasm_global_t; cdecl;
  wasm_global_type : function(_para1:Pwasm_global_t):Pwasm_globaltype_t; cdecl;
  wasm_global_get : procedure(_para1:Pwasm_global_t; out_:Pwasm_val_t); cdecl;
  wasm_global_set : procedure(_para1:Pwasm_global_t; _para2:Pwasm_val_t); cdecl;
  wasm_table_delete : procedure(_para1:Pwasm_table_t); cdecl;
  wasm_table_copy : function(_para1:Pwasm_table_t):Pwasm_table_t; cdecl;
  wasm_table_same : function(_para1:Pwasm_table_t; _para2:Pwasm_table_t):T_Bool;
  wasm_table_get_host_info : function(_para1:Pwasm_table_t):pointer;
  wasm_table_set_host_info : procedure(_para1:Pwasm_table_t; _para2:pointer); cdecl;
  wasm_table_set_host_info_with_finalizer : procedure(_para1:Pwasm_table_t; _para2:pointer; _para3:TFinalizer); cdecl;
  wasm_table_as_ref : function(_para1:Pwasm_table_t):Pwasm_ref_t; cdecl;
  wasm_ref_as_table : function(_para1:Pwasm_ref_t):Pwasm_table_t; cdecl;
  wasm_table_as_ref_const : function(_para1:Pwasm_table_t):Pwasm_ref_t; cdecl;
  wasm_ref_as_table_const : function(_para1:Pwasm_ref_t):Pwasm_table_t; cdecl;
  wasm_table_new : function(_para1:Pwasm_store_t; _para2:Pwasm_tabletype_t; init:Pwasm_ref_t):Pwasm_table_t; cdecl;
  wasm_table_type : function(_para1:Pwasm_table_t):Pwasm_tabletype_t; cdecl;
  wasm_table_get : function(_para1:Pwasm_table_t; index:Twasm_table_size_t):Pwasm_ref_t; cdecl;
  wasm_table_set : function(_para1:Pwasm_table_t; index:Twasm_table_size_t; _para3:Pwasm_ref_t):T_Bool;
  wasm_table_size : function(_para1:Pwasm_table_t):Twasm_table_size_t; cdecl;
  wasm_table_grow : function(_para1:Pwasm_table_t; delta:Twasm_table_size_t; init:Pwasm_ref_t):T_Bool;
  wasm_memory_delete : procedure(_para1:Pwasm_memory_t); cdecl;
  wasm_memory_copy : function(_para1:Pwasm_memory_t):Pwasm_memory_t; cdecl;
  wasm_memory_same : function(_para1:Pwasm_memory_t; _para2:Pwasm_memory_t):T_Bool;
  wasm_memory_get_host_info : function(_para1:Pwasm_memory_t):pointer;
  wasm_memory_set_host_info : procedure(_para1:Pwasm_memory_t; _para2:pointer); cdecl;
  wasm_memory_set_host_info_with_finalizer : procedure(_para1:Pwasm_memory_t; _para2:pointer; _para3:TFinalizer); cdecl;
  wasm_memory_as_ref : function(_para1:Pwasm_memory_t):Pwasm_ref_t; cdecl;
  wasm_ref_as_memory : function(_para1:Pwasm_ref_t):Pwasm_memory_t; cdecl;
  wasm_memory_as_ref_const : function(_para1:Pwasm_memory_t):Pwasm_ref_t; cdecl;
  wasm_ref_as_memory_const : function(_para1:Pwasm_ref_t):Pwasm_memory_t; cdecl;
  wasm_memory_new : function(_para1:Pwasm_store_t; _para2:Pwasm_memorytype_t):Pwasm_memory_t; cdecl;
  wasm_memory_type : function(_para1:Pwasm_memory_t):Pwasm_memorytype_t; cdecl;
  wasm_memory_data : function(_para1:Pwasm_memory_t):Pbyte_t; cdecl;
  wasm_memory_data_size : function(_para1:Pwasm_memory_t):Tsize_t; cdecl;
  wasm_memory_size : function(_para1:Pwasm_memory_t):Twasm_memory_pages_t; cdecl;
  wasm_memory_grow : function(_para1:Pwasm_memory_t; delta:Twasm_memory_pages_t):T_Bool;
  wasm_extern_delete : procedure(_para1:Pwasm_extern_t); cdecl;
  wasm_extern_copy : function(_para1:Pwasm_extern_t):Pwasm_extern_t; cdecl;
  wasm_extern_same : function(_para1:Pwasm_extern_t; _para2:Pwasm_extern_t):T_Bool;
  wasm_extern_get_host_info : function(_para1:Pwasm_extern_t):pointer;
  wasm_extern_set_host_info : procedure(_para1:Pwasm_extern_t; _para2:pointer); cdecl;
  wasm_extern_set_host_info_with_finalizer : procedure(_para1:Pwasm_extern_t; _para2:pointer; _para3: TFinalizer); cdecl;
  wasm_extern_as_ref : function(_para1:Pwasm_extern_t):Pwasm_ref_t; cdecl;
  wasm_ref_as_extern : function(_para1:Pwasm_ref_t):Pwasm_extern_t; cdecl;
  wasm_extern_as_ref_const : function(_para1:Pwasm_extern_t):Pwasm_ref_t; cdecl;
  wasm_ref_as_extern_const : function(_para1:Pwasm_ref_t):Pwasm_extern_t; cdecl;
  wasm_extern_vec_new_empty : procedure(_out:Pwasm_extern_vec_t); cdecl;
  wasm_extern_vec_new_uninitialized : procedure(_out:Pwasm_extern_vec_t; _para2:Tsize_t); cdecl;
  wasm_extern_vec_new : procedure(_out:Pwasm_extern_vec_t; _para2:Tsize_t; data:Pwasm_extern_t); cdecl;
  wasm_extern_vec_copy : procedure(_out:Pwasm_extern_vec_t; _para2:Pwasm_extern_vec_t); cdecl;
  wasm_extern_vec_delete : procedure(_para1:Pwasm_extern_vec_t); cdecl;
  wasm_extern_kind : function(_para1:Pwasm_extern_t):Twasm_externkind_t; cdecl;
  wasm_extern_type : function(_para1:Pwasm_extern_t):Pwasm_externtype_t; cdecl;
  wasm_func_as_extern : function(_para1:Pwasm_func_t):Pwasm_extern_t; cdecl;
  wasm_global_as_extern : function(_para1:Pwasm_global_t):Pwasm_extern_t; cdecl;
  wasm_table_as_extern : function(_para1:Pwasm_table_t):Pwasm_extern_t; cdecl;
  wasm_memory_as_extern : function(_para1:Pwasm_memory_t):Pwasm_extern_t; cdecl;
  wasm_extern_as_func : function(_para1:Pwasm_extern_t):Pwasm_func_t; cdecl;
  wasm_extern_as_global : function(_para1:Pwasm_extern_t):Pwasm_global_t; cdecl;
  wasm_extern_as_table : function(_para1:Pwasm_extern_t):Pwasm_table_t; cdecl;
  wasm_extern_as_memory : function(_para1:Pwasm_extern_t):Pwasm_memory_t; cdecl;
  wasm_func_as_extern_const : function(_para1:Pwasm_func_t):Pwasm_extern_t; cdecl;
  wasm_global_as_extern_const : function(_para1:Pwasm_global_t):Pwasm_extern_t; cdecl;
  wasm_table_as_extern_const : function(_para1:Pwasm_table_t):Pwasm_extern_t; cdecl;
  wasm_memory_as_extern_const : function(_para1:Pwasm_memory_t):Pwasm_extern_t; cdecl;
  wasm_extern_as_func_const : function(_para1:Pwasm_extern_t):Pwasm_func_t; cdecl;
  wasm_extern_as_global_const : function(_para1:Pwasm_extern_t):Pwasm_global_t; cdecl;
  wasm_extern_as_table_const : function(_para1:Pwasm_extern_t):Pwasm_table_t; cdecl;
  wasm_extern_as_memory_const : function(_para1:Pwasm_extern_t):Pwasm_memory_t; cdecl;
  wasm_instance_delete : procedure(_para1:Pwasm_instance_t); cdecl;
  wasm_instance_copy : function(_para1:Pwasm_instance_t):Pwasm_instance_t; cdecl;
  wasm_instance_same : function(_para1:Pwasm_instance_t; _para2:Pwasm_instance_t):T_Bool;
  wasm_instance_get_host_info : function(_para1:Pwasm_instance_t):pointer;
  wasm_instance_set_host_info : procedure(_para1:Pwasm_instance_t; _para2:pointer); cdecl;
  wasm_instance_set_host_info_with_finalizer : procedure(_para1:Pwasm_instance_t; _para2:pointer; _para3: TFInalizer); cdecl;
  wasm_instance_as_ref : function(_para1:Pwasm_instance_t):Pwasm_ref_t; cdecl;
  wasm_ref_as_instance : function(_para1:Pwasm_ref_t):Pwasm_instance_t; cdecl;
  wasm_instance_as_ref_const : function(_para1:Pwasm_instance_t):Pwasm_ref_t; cdecl;
  wasm_ref_as_instance_const : function(_para1:Pwasm_ref_t):Pwasm_instance_t; cdecl;
  wasm_instance_new : function(_para1:Pwasm_store_t; _para2:Pwasm_module_t; imports:Pwasm_extern_vec_t; _para4:PPwasm_trap_t):Pwasm_instance_t; cdecl;
  wasm_instance_exports : procedure(_para1:Pwasm_instance_t; out_:Pwasm_extern_vec_t); cdecl;
  wasi_config_new : function:Pwasi_config_t; cdecl;
  wasi_config_set_argv : procedure(config:Pwasi_config_t; argc:Tcint; argv:PPAnsiChar); cdecl;
  wasi_config_inherit_argv : procedure(config:Pwasi_config_t); cdecl;
  wasi_config_set_env : procedure(config:Pwasi_config_t; envc:Tcint; names:PPAnsiChar; values:PPAnsiChar); cdecl;
  wasi_config_inherit_env : procedure(config:Pwasi_config_t); cdecl;
  wasi_config_set_stdin_file : function(config:Pwasi_config_t; path:PAnsiChar):T_Bool;
  wasi_config_inherit_stdin : procedure(config:Pwasi_config_t); cdecl;
  wasi_config_set_stdout_file : function(config:Pwasi_config_t; path:PAnsiChar):T_Bool;
  wasi_config_inherit_stdout : procedure(config:Pwasi_config_t); cdecl;
  wasi_config_set_stderr_file : function(config:Pwasi_config_t; path:PAnsiChar):T_Bool;
  wasi_config_inherit_stderr : procedure(config:Pwasi_config_t); cdecl;
  wasi_config_preopen_dir : function(config:Pwasi_config_t; path:PAnsiChar; guest_path:PAnsiChar):T_Bool;
  wasmtime_error_delete : procedure(error:Pwasmtime_error_t); cdecl;
  wasmtime_error_message : procedure(error:Pwasmtime_error_t; message:Pwasm_name_t); cdecl;
  wasmtime_error_exit_status : function(p1: pwasmtime_error_t; status : pcint) : T_bool; cdecl;
  wasmtime_error_wasm_trace : procedure(p1: pwasmtime_error_t; res: pwasm_frame_vec_t); cdecl ;
  wasmtime_config_debug_info_set : procedure(_para1:Pwasm_config_t; _para2:T_Bool); cdecl;
  wasmtime_config_interruptable_set : procedure(_para1:Pwasm_config_t; _para2:T_Bool); cdecl;
  wasmtime_config_consume_fuel_set : procedure(_para1:Pwasm_config_t; _para2:T_Bool); cdecl;
  wasmtime_config_max_wasm_stack_set : procedure(_para1:Pwasm_config_t; _para2:Tsize_t); cdecl;
  wasmtime_config_wasm_threads_set : procedure(_para1:Pwasm_config_t; _para2:T_Bool); cdecl;
  wasmtime_config_wasm_reference_types_set : procedure(_para1:Pwasm_config_t; _para2:T_Bool); cdecl;
  wasmtime_config_wasm_simd_set : procedure(_para1:Pwasm_config_t; _para2:T_Bool); cdecl;
  wasmtime_config_wasm_bulk_memory_set : procedure(_para1:Pwasm_config_t; _para2:T_Bool); cdecl;
  wasmtime_config_wasm_multi_value_set : procedure(_para1:Pwasm_config_t; _para2:T_Bool); cdecl;
  wasmtime_config_wasm_multi_memory_set : procedure(_para1:Pwasm_config_t; _para2:T_Bool); cdecl;
  wasmtime_config_wasm_module_linking_set : procedure(_para1:Pwasm_config_t; _para2:T_Bool); cdecl;
  wasmtime_config_wasm_memory64_set : procedure(_para1:Pwasm_config_t; _para2:T_Bool); cdecl;
  wasmtime_config_strategy_set : function(_para1:Pwasm_config_t; _para2:Twasmtime_strategy_t):Pwasmtime_error_t; cdecl;
  wasmtime_config_cranelift_debug_verifier_set : procedure(_para1:Pwasm_config_t; _para2:T_Bool); cdecl;
  wasmtime_config_cranelift_opt_level_set : procedure(_para1:Pwasm_config_t; _para2:Twasmtime_opt_level_t); cdecl;
  wasmtime_config_profiler_set : function(_para1:Pwasm_config_t; _para2:Twasmtime_profiling_strategy_t):Pwasmtime_error_t; cdecl;
  wasmtime_config_static_memory_maximum_size_set : procedure(_para1:Pwasm_config_t; _para2:Tuint64_t); cdecl;
  wasmtime_config_static_memory_guard_size_set : procedure(_para1:Pwasm_config_t; _para2:Tuint64_t); cdecl;
  wasmtime_config_dynamic_memory_guard_size_set : procedure(_para1:Pwasm_config_t; _para2:Tuint64_t); cdecl;
  wasmtime_config_cache_config_load : function(_para1:Pwasm_config_t; _para2:PAnsiChar):Pwasmtime_error_t; cdecl;
  wasmtime_moduletype_delete : procedure(ty:Pwasmtime_moduletype_t); cdecl;
  wasmtime_moduletype_imports : procedure(_para1:Pwasmtime_moduletype_t; out_:Pwasm_importtype_vec_t); cdecl;
  wasmtime_moduletype_exports : procedure(_para1:Pwasmtime_moduletype_t; out_:Pwasm_exporttype_vec_t); cdecl;
  wasmtime_moduletype_as_externtype : function(_para1:Pwasmtime_moduletype_t):Pwasm_externtype_t; cdecl;
  wasmtime_externtype_as_moduletype : function(_para1:Pwasm_externtype_t):Pwasmtime_moduletype_t; cdecl;
  wasmtime_module_new : function(engine:Pwasm_engine_t; wasm:Puint8_t; wasm_len:Tsize_t; ret:PPwasmtime_module_t):Pwasmtime_error_t; cdecl;
  wasmtime_module_delete : procedure(m:Pwasmtime_module_t); cdecl;
  wasmtime_module_clone : function(m:Pwasmtime_module_t):Pwasmtime_module_t; cdecl;
  wasmtime_module_validate : function(engine:Pwasm_engine_t; wasm:Puint8_t; wasm_len:Tsize_t):Pwasmtime_error_t; cdecl;
  wasmtime_module_type : function(_para1:Pwasmtime_module_t):Pwasmtime_moduletype_t; cdecl;
  wasmtime_module_serialize : function(module:Pwasmtime_module_t; ret:Pwasm_byte_vec_t):Pwasmtime_error_t; cdecl;
  wasmtime_module_deserialize : function(engine:Pwasm_engine_t; bytes:Puint8_t; bytes_len:Tsize_t; ret:PPwasmtime_module_t):Pwasmtime_error_t; cdecl;
  wasmtime_module_deserialize_file : function(engine:Pwasm_engine_t; path:PAnsiChar; ret:PPwasmtime_module_t):Pwasmtime_error_t; cdecl;
  wasmtime_store_new : function(engine:Pwasm_engine_t; data:pointer; finalizer: TFinalizer):Pwasmtime_store_t; cdecl;
  wasmtime_store_context : function(store:Pwasmtime_store_t):Pwasmtime_context_t; cdecl;
  wasmtime_store_delete : procedure(store:Pwasmtime_store_t); cdecl;
  wasmtime_context_get_data : function(context:Pwasmtime_context_t):pointer;
  wasmtime_context_set_data : procedure(context:Pwasmtime_context_t; data:pointer); cdecl;
  wasmtime_context_gc : procedure(context:Pwasmtime_context_t); cdecl;
  wasmtime_context_add_fuel : function(store:Pwasmtime_context_t; fuel:Tuint64_t):Pwasmtime_error_t; cdecl;
  wasmtime_context_fuel_consumed : function(context:Pwasmtime_context_t; fuel:Puint64_t):T_Bool;
  wasmtime_context_consume_fuel : function(context:Pwasmtime_context_t; fuel:Tuint64_t; remaining:Puint64_t):Pwasmtime_error_t; cdecl;
  wasmtime_context_set_wasi : function(context:Pwasmtime_context_t; wasi:Pwasi_config_t):Pwasmtime_error_t; cdecl;
  wasmtime_interrupt_handle_new : function(context:Pwasmtime_context_t):Pwasmtime_interrupt_handle_t; cdecl;
  wasmtime_interrupt_handle_interrupt : procedure(handle:Pwasmtime_interrupt_handle_t); cdecl;
  wasmtime_interrupt_handle_delete : procedure(handle:Pwasmtime_interrupt_handle_t); cdecl;
  wasmtime_extern_delete : procedure(val:Pwasmtime_extern_t); cdecl;
  wasmtime_extern_type : function(context:Pwasmtime_context_t; val:Pwasmtime_extern_t):Pwasm_externtype_t; cdecl;
  wasmtime_externref_new : function(data:pointer; finalizer: TFInalizer):Pwasmtime_externref_t; cdecl;
  wasmtime_externref_data : function(data:Pwasmtime_externref_t):pointer;
  wasmtime_externref_clone : function(ref:Pwasmtime_externref_t):Pwasmtime_externref_t; cdecl;
  wasmtime_externref_delete : procedure(ref:Pwasmtime_externref_t); cdecl;
  wasmtime_externref_from_raw : function(context:Pwasmtime_context_t; raw:Tsize_t):Pwasmtime_externref_t; cdecl;
  wasmtime_externref_to_raw : function(context:Pwasmtime_context_t; ref:Pwasmtime_externref_t):Tsize_t; cdecl;
  wasmtime_val_delete : procedure(val:Pwasmtime_val_t); cdecl;
  wasmtime_val_copy : procedure(dst:Pwasmtime_val_t; src:Pwasmtime_val_t); cdecl;
  wasmtime_func_new : procedure(store:Pwasmtime_context_t; _type:Pwasm_functype_t; callback:Twasmtime_func_callback_t; env:pointer; finalizer:TFinalizer; 
      ret:Pwasmtime_func_t); cdecl;
  wasmtime_func_new_unchecked : procedure(store:Pwasmtime_context_t; _type:Pwasm_functype_t; callback:Twasmtime_func_unchecked_callback_t; env:pointer; finalizer: TFinalizer; 
      ret:Pwasmtime_func_t); cdecl;
  wasmtime_func_type : function(store:Pwasmtime_context_t; func:Pwasmtime_func_t):Pwasm_functype_t; cdecl;
  wasmtime_func_call : function(store:Pwasmtime_context_t; func:Pwasmtime_func_t; args:Pwasmtime_val_t; nargs:Tsize_t; results:Pwasmtime_val_t; 
      nresults:Tsize_t; trap:PPwasm_trap_t):Pwasmtime_error_t; cdecl;
  wasmtime_func_call_unchecked : function(store:Pwasmtime_context_t; func:Pwasmtime_func_t; args_and_results:Pwasmtime_val_raw_t):Pwasm_trap_t; cdecl;
  wasmtime_caller_export_get : function(caller:Pwasmtime_caller_t; name:PAnsiChar; name_len:Tsize_t; item:Pwasmtime_extern_t):T_Bool;
  wasmtime_caller_context : function(caller:Pwasmtime_caller_t):Pwasmtime_context_t; cdecl;
  wasmtime_func_from_raw : procedure(context:Pwasmtime_context_t; raw:Tsize_t; ret:Pwasmtime_func_t); cdecl;
  wasmtime_func_to_raw : function(context:Pwasmtime_context_t; func:Pwasmtime_func_t):Tsize_t; cdecl;
  wasmtime_global_new : function(store:Pwasmtime_context_t; _type:Pwasm_globaltype_t; val:Pwasmtime_val_t; ret:Pwasmtime_global_t):Pwasmtime_error_t; cdecl;
  wasmtime_global_type : function(store:Pwasmtime_context_t; global:Pwasmtime_global_t):Pwasm_globaltype_t; cdecl;
  wasmtime_global_get : procedure(store:Pwasmtime_context_t; global:Pwasmtime_global_t; out_:Pwasmtime_val_t); cdecl;
  wasmtime_global_set : function(store:Pwasmtime_context_t; global:Pwasmtime_global_t; val:Pwasmtime_val_t):Pwasmtime_error_t; cdecl;
  wasmtime_instancetype_delete : procedure(ty:Pwasmtime_instancetype_t); cdecl;
  wasmtime_instancetype_exports : procedure(_para1:Pwasmtime_instancetype_t; out_:Pwasm_exporttype_vec_t); cdecl;
  wasmtime_instancetype_as_externtype : function(_para1:Pwasmtime_instancetype_t):Pwasm_externtype_t; cdecl;
  wasmtime_externtype_as_instancetype : function(_para1:Pwasm_externtype_t):Pwasmtime_instancetype_t; cdecl;
  wasmtime_instance_new : function(store:Pwasmtime_context_t; module:Pwasmtime_module_t; imports:Pwasmtime_extern_t; nimports:Tsize_t; instance:Pwasmtime_instance_t; 
      trap:PPwasm_trap_t):Pwasmtime_error_t; cdecl;
  wasmtime_instance_type : function(store:Pwasmtime_context_t; instance:Pwasmtime_instance_t):Pwasmtime_instancetype_t; cdecl;
  wasmtime_instance_export_get : function(store:Pwasmtime_context_t; instance:Pwasmtime_instance_t; name:PAnsiChar; name_len:Tsize_t; item:Pwasmtime_extern_t):T_Bool;
  wasmtime_instance_export_nth : function(store:Pwasmtime_context_t; instance:Pwasmtime_instance_t; index:Tsize_t; name:PPAnsiChar; name_len:Psize_t; 
      item:Pwasmtime_extern_t):T_Bool;
  wasmtime_linker_new : function(engine:Pwasm_engine_t):Pwasmtime_linker_t; cdecl;
  wasmtime_linker_delete : procedure(linker:Pwasmtime_linker_t); cdecl;
  wasmtime_linker_allow_shadowing : procedure(linker:Pwasmtime_linker_t; allow_shadowing:T_Bool); cdecl;
  wasmtime_linker_define : function(linker:Pwasmtime_linker_t; module:PAnsiChar; module_len:Tsize_t; name:PAnsiChar; name_len:Tsize_t; 
      item:Pwasmtime_extern_t):Pwasmtime_error_t; cdecl;
  wasmtime_linker_define_func : function(linker:Pwasmtime_linker_t; module:PAnsiChar; module_len:Tsize_t; name:PAnsiChar; name_len:Tsize_t; 
      ty:Pwasm_functype_t; cb:Twasmtime_func_callback_t; data:pointer; finalizer: TFInalizer):Pwasmtime_error_t; cdecl;
  wasmtime_linker_define_func_unchecked : function(linker:Pwasmtime_linker_t; module:PAnsiChar; module_len:Tsize_t; name:PAnsiChar; name_len:Tsize_t; 
      ty:Pwasm_functype_t; cb:Twasmtime_func_unchecked_callback_t; data:pointer; finalizer: TFInalizer):Pwasmtime_error_t; cdecl;
  wasmtime_linker_define_wasi : function(linker:Pwasmtime_linker_t):Pwasmtime_error_t; cdecl;
  wasmtime_linker_define_instance : function(linker:Pwasmtime_linker_t; store:Pwasmtime_context_t; name:PAnsiChar; name_len:Tsize_t; instance:Pwasmtime_instance_t):Pwasmtime_error_t; cdecl;
  wasmtime_linker_instantiate : function(linker:Pwasmtime_linker_t; store:Pwasmtime_context_t; module:Pwasmtime_module_t; instance:Pwasmtime_instance_t; trap:PPwasm_trap_t):Pwasmtime_error_t; cdecl;
  wasmtime_linker_module : function(linker:Pwasmtime_linker_t; store:Pwasmtime_context_t; name:PAnsiChar; name_len:Tsize_t; module:Pwasmtime_module_t):Pwasmtime_error_t; cdecl;
  wasmtime_linker_get_default : function(linker:Pwasmtime_linker_t; store:Pwasmtime_context_t; name:PAnsiChar; name_len:Tsize_t; func:Pwasmtime_func_t):Pwasmtime_error_t; cdecl;
  wasmtime_linker_get : function(linker:Pwasmtime_linker_t; store:Pwasmtime_context_t; module:PAnsiChar; module_len:Tsize_t; name:PAnsiChar; 
      name_len:Tsize_t; item:Pwasmtime_extern_t):T_Bool;
  wasmtime_memorytype_new : function(min:Tuint64_t; max_present:T_Bool; max:Tuint64_t; is_64:T_Bool):Pwasm_memorytype_t; cdecl;
  wasmtime_memorytype_minimum : function(ty:Pwasm_memorytype_t):Tuint64_t; cdecl;
  wasmtime_memorytype_maximum : function(ty:Pwasm_memorytype_t; max:Puint64_t):T_Bool;
  wasmtime_memorytype_is64 : function(ty:Pwasm_memorytype_t):T_Bool;
  wasmtime_memory_new : function(store:Pwasmtime_context_t; ty:Pwasm_memorytype_t; ret:Pwasmtime_memory_t):Pwasmtime_error_t; cdecl;
  wasmtime_memory_type : function(store:Pwasmtime_context_t; memory:Pwasmtime_memory_t):Pwasm_memorytype_t; cdecl;
  wasmtime_memory_data : function(store:Pwasmtime_context_t; memory:Pwasmtime_memory_t):Puint8_t; cdecl;
  wasmtime_memory_data_size : function(store:Pwasmtime_context_t; memory:Pwasmtime_memory_t):Tsize_t; cdecl;
  wasmtime_memory_size : function(store:Pwasmtime_context_t; memory:Pwasmtime_memory_t):Tuint64_t; cdecl;
  wasmtime_memory_grow : function(store:Pwasmtime_context_t; memory:Pwasmtime_memory_t; delta:Tuint64_t; prev_size:Puint64_t):Pwasmtime_error_t; cdecl;
  wasmtime_table_new : function(store:Pwasmtime_context_t; ty:Pwasm_tabletype_t; init:Pwasmtime_val_t; table:Pwasmtime_table_t):Pwasmtime_error_t; cdecl;
  wasmtime_table_type : function(store:Pwasmtime_context_t; table:Pwasmtime_table_t):Pwasm_tabletype_t; cdecl;
  wasmtime_table_get : function(store:Pwasmtime_context_t; table:Pwasmtime_table_t; index:Tuint32_t; val:Pwasmtime_val_t):T_Bool;
  wasmtime_table_set : function(store:Pwasmtime_context_t; table:Pwasmtime_table_t; index:Tuint32_t; value:Pwasmtime_val_t):Pwasmtime_error_t; cdecl;
  wasmtime_table_size : function(store:Pwasmtime_context_t; table:Pwasmtime_table_t):Tuint32_t; cdecl;
  wasmtime_table_grow : function(store:Pwasmtime_context_t; table:Pwasmtime_table_t; delta:Tuint32_t; init:Pwasmtime_val_t; prev_size:Puint32_t):Pwasmtime_error_t; cdecl;
  wasmtime_trap_new : function(msg:PAnsiChar; msg_len:Tsize_t):Pwasm_trap_t; cdecl;
  wasmtime_trap_code : function(_para1:Pwasm_trap_t; code:Pwasmtime_trap_code_t):T_Bool;
  wasmtime_trap_exit_status : function(_para1:Pwasm_trap_t; status:pcint):T_Bool;
  wasmtime_frame_func_name : function(_para1:Pwasm_frame_t):Pwasm_name_t; cdecl;
  wasmtime_frame_module_name : function(_para1:Pwasm_frame_t):Pwasm_name_t; cdecl;
  wasmtime_wat2wasm : function(wat:PAnsiChar; wat_len:Tsize_t; ret:Pwasm_byte_vec_t):Pwasmtime_error_t; cdecl;
  wasi_config_delete : procedure(_para1:Pwasi_config_t); cdecl;

// Converted Inline functions

function wasm_valkind_is_num(k: twasm_valkind_t) : boolean; 
function wasm_valkind_is_ref(k: twasm_valkind_t) : boolean; 
function wasm_valtype_is_num(t: Pwasm_valtype_t) : Boolean;
function wasm_valtype_is_ref(t: Pwasm_valtype_t) : Boolean;

function wasm_valtype_new_i32 : Pwasm_valtype_t;
function wasm_valtype_new_i64 : Pwasm_valtype_t;
function wasm_valtype_new_f32 : Pwasm_valtype_t;
function wasm_valtype_new_f64 : Pwasm_valtype_t;
function wasm_valtype_new_anyref : Pwasm_valtype_t;
function wasm_valtype_new_funcref : Pwasm_valtype_t;

function wasm_functype_new_0_0 : Pwasm_functype_t;
function wasm_functype_new_1_0 (P : pwasm_valtype_t): Pwasm_functype_t;
function wasm_functype_new_2_0 (P1,p2 : pwasm_valtype_t): Pwasm_functype_t;
function wasm_functype_new_3_0 (P1,p2,p3 : pwasm_valtype_t): Pwasm_functype_t;
function wasm_functype_new_0_1 (R : pwasm_valtype_t): Pwasm_functype_t;
function wasm_functype_new_1_1 (P,R : pwasm_valtype_t): Pwasm_functype_t;
function wasm_functype_new_2_1 (P1,P2,R : pwasm_valtype_t): Pwasm_functype_t;
function wasm_functype_new_3_1 (P1,P2,P3,R : pwasm_valtype_t): Pwasm_functype_t;
function wasm_functype_new_0_2 (R1,R2 : pwasm_valtype_t): Pwasm_functype_t;
function wasm_functype_new_1_2 (P,R1,R2 : pwasm_valtype_t): Pwasm_functype_t;
function wasm_functype_new_2_2 (P1,P2,R1,R2 : pwasm_valtype_t): Pwasm_functype_t;
function wasm_functype_new_3_2 (P1,P2,P3,R1,R2 : pwasm_valtype_t): Pwasm_functype_t;
procedure wasm_val_init_ptr(out_: Pwasm_val_t; P : Pointer);
function wasm_val_ptr(val : Pwasm_val_t) : Pointer;


procedure LoadWasmTime(const lib : string);
procedure FreeWasmTime;
function WasmTimeLoaded : Boolean;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.SysUtils, System.DynLibs;
{$ELSE FPC_DOTTEDUNITS}
uses
  SysUtils, dynlibs;
{$ENDIF FPC_DOTTEDUNITS}

{ Converted static functions }

function wasm_valkind_is_num(k: twasm_valkind_t) : boolean; 

begin
  result:= k < ord(WASM_ANYREF);
end;

function wasm_valkind_is_ref(k: twasm_valkind_t) : boolean; 
begin
  Result:= k >= ord(WASM_ANYREF);
end;

function wasm_valtype_is_num(t: Pwasm_valtype_t) : Boolean;

begin
  Result:=wasm_valkind_is_num(wasm_valtype_kind(t)); 
end;

function wasm_valtype_is_ref(t: Pwasm_valtype_t) : Boolean;
 
begin
  Result:=wasm_valkind_is_ref(wasm_valtype_kind(t));
end;


function wasm_valtype_new_i32 : Pwasm_valtype_t;
begin
  result:=wasm_valtype_new(Ord(WASM_I32));
end;

function wasm_valtype_new_i64 : Pwasm_valtype_t;
begin
  result:=wasm_valtype_new(Ord(WASM_I64));
end;

function wasm_valtype_new_f32 : Pwasm_valtype_t;
begin
  result:=wasm_valtype_new(Ord(WASM_F32));
end;

function wasm_valtype_new_f64 : Pwasm_valtype_t;
begin
  result:=wasm_valtype_new(Ord(WASM_F64));
end;

function wasm_valtype_new_anyref : Pwasm_valtype_t;
begin
  result:=wasm_valtype_new(Ord(WASM_ANYREF));
end;

function wasm_valtype_new_funcref : Pwasm_valtype_t;
begin
  result:=wasm_valtype_new(Ord(WASM_FUNCREF));
end;

function wasm_functype_new_0_0 : Pwasm_functype_t;

var
 params, results : Twasm_valtype_vec_t ;

begin
  wasm_valtype_vec_new_empty(@params);
  wasm_valtype_vec_new_empty(@results);
  Result:=wasm_functype_new(@params, @results);
end;

function wasm_functype_new_1_0 (P : pwasm_valtype_t): Pwasm_functype_t;

var
  params, results : Twasm_valtype_vec_t ;
  ps : Array[0..0] of pwasm_valtype_t;

begin
  ps[0]:=P;
  wasm_valtype_vec_new(@params,1,@ps);
  wasm_valtype_vec_new_empty(@results);
  Result:=wasm_functype_new(@params, @results);
end;

function wasm_functype_new_2_0 (P1,P2 : pwasm_valtype_t): Pwasm_functype_t;

var
  params, results : Twasm_valtype_vec_t ;
  ps : Array[0..1] of pwasm_valtype_t;

begin
  ps[0]:=P1;
  ps[1]:=P2;
  wasm_valtype_vec_new(@params,2,@ps);
  wasm_valtype_vec_new_empty(@results);
  Result:=wasm_functype_new(@params, @results);
end;

function wasm_functype_new_3_0 (P1,p2,p3 : pwasm_valtype_t): Pwasm_functype_t;
var
  params, results : Twasm_valtype_vec_t ;
  ps : Array[0..2] of pwasm_valtype_t;

begin
  ps[0]:=P1;
  ps[1]:=P2;
  ps[2]:=P3;
  wasm_valtype_vec_new(@params,3,@ps);
  wasm_valtype_vec_new_empty(@results);
  Result:=wasm_functype_new(@params, @results);
end;

function wasm_functype_new_0_1 (R : pwasm_valtype_t): Pwasm_functype_t;

var
  params, results : Twasm_valtype_vec_t ;
  rs : Array[0..0] of pwasm_valtype_t;

begin
  rs[0]:=R;
  wasm_valtype_vec_new_empty(@params);
  wasm_valtype_vec_new(@results,1,@rs);
  Result:=wasm_functype_new(@params, @results);
end;

function wasm_functype_new_0_2 (R1,R2 : pwasm_valtype_t): Pwasm_functype_t;

var
  params, results : Twasm_valtype_vec_t ;
  rs : Array[0..1] of pwasm_valtype_t;

begin
  rs[0]:=R1;
  rs[1]:=R2;
  wasm_valtype_vec_new_empty(@params);
  wasm_valtype_vec_new(@results,2,@rs);
  Result:=wasm_functype_new(@params, @results);
end;


function wasm_functype_new_1_1 (P,R : pwasm_valtype_t): Pwasm_functype_t;

var
  params, results : Twasm_valtype_vec_t ;
  ps,rs : Array[0..0] of pwasm_valtype_t;

begin
  ps[0]:=P;
  rs[0]:=R;
  wasm_valtype_vec_new(@params,1,@ps);
  wasm_valtype_vec_new(@results,1,@rs);
  Result:=wasm_functype_new(@params, @results);
end;

function wasm_functype_new_2_1 (P1,P2,R : pwasm_valtype_t): Pwasm_functype_t;

var
  params, results : Twasm_valtype_vec_t ;
  ps : Array[0..1] of pwasm_valtype_t;
  rs : Array[0..0] of pwasm_valtype_t;

begin
  ps[0]:=P1;
  ps[1]:=P2;
  rs[0]:=R;
  wasm_valtype_vec_new(@params,2,@ps);
  wasm_valtype_vec_new(@results,1,@rs);
  Result:=wasm_functype_new(@params, @results);
end;


function wasm_functype_new_1_2 (P,R1,R2 : pwasm_valtype_t): Pwasm_functype_t;

var
  params, results : Twasm_valtype_vec_t ;
  ps : Array[0..0] of pwasm_valtype_t;
  rs : Array[0..1] of pwasm_valtype_t;

begin
  ps[0]:=P;
  rs[0]:=R1;
  rs[1]:=R2;
  wasm_valtype_vec_new(@params,1,@ps);
  wasm_valtype_vec_new(@results,2,@rs);
  Result:=wasm_functype_new(@params, @results);
end;

function wasm_functype_new_2_2 (P1,P2,R1,R2 : pwasm_valtype_t): Pwasm_functype_t;

var
  params, results : Twasm_valtype_vec_t ;
  ps : Array[0..1] of pwasm_valtype_t;
  rs : Array[0..1] of pwasm_valtype_t;

begin
  ps[0]:=P1;
  ps[1]:=P2;
  rs[0]:=R1;
  rs[1]:=R2;
  wasm_valtype_vec_new(@params,2,@ps);
  wasm_valtype_vec_new(@results,2,@rs);
  Result:=wasm_functype_new(@params, @results);
end;

function wasm_functype_new_3_2 (P1,P2,P3,R1,R2 : pwasm_valtype_t): Pwasm_functype_t;

var
  params, results : Twasm_valtype_vec_t ;
  ps : Array[0..2] of pwasm_valtype_t;
  rs : Array[0..1] of pwasm_valtype_t;

begin
  ps[0]:=P1;
  ps[1]:=P2;
  ps[2]:=P3;
  rs[0]:=R1;
  rs[1]:=R2;
  wasm_valtype_vec_new(@params,3,@ps);
  wasm_valtype_vec_new(@results,2,@rs);
  Result:=wasm_functype_new(@params, @results);
end;



function wasm_functype_new_3_1 (P1,P2,P3,R : pwasm_valtype_t): Pwasm_functype_t;

var
  params, results : Twasm_valtype_vec_t ;
  ps : Array[0..2] of pwasm_valtype_t;
  rs : Array[0..0] of pwasm_valtype_t;

begin
  ps[0]:=P1;
  ps[1]:=P2;
  ps[2]:=P3;
  rs[0]:=R;
  wasm_valtype_vec_new(@params,3,@ps);
  wasm_valtype_vec_new(@results,1,@rs);
  Result:=wasm_functype_new(@params, @results);
end;

procedure wasm_val_init_ptr(out_: Pwasm_val_t; P : Pointer);

begin
  out_^.kind :=Ord(WASM_I64);
  out_^.of_.i64 := PtrInt(p);
end;

function wasm_val_ptr(val : Pwasm_val_t) : Pointer;

begin
  Result:=Pointer(PtrInt(val^.of_.i64));
end;

{ ----------------------------------------------------------------------
  Load library
  ----------------------------------------------------------------------}


var
  hlib : tlibhandle;


function WasmTimeLoaded : Boolean;

begin
  Result:=(Hlib<>NilHandle)
end;
procedure FreeWasmTime;

begin
  if WasmTimeLoaded then
    begin
    FreeLibrary(hlib);
    HLib:=NilHandle;
    end;
  wasm_byte_vec_new_empty:=nil;
  wasm_byte_vec_new_uninitialized:=nil;
  wasm_byte_vec_new:=nil;
  wasm_byte_vec_copy:=nil;
  wasm_byte_vec_delete:=nil;
  wasm_config_delete:=nil;
  wasm_config_new:=nil;
  wasm_engine_delete:=nil;
  wasm_engine_new:=nil;
  wasm_engine_new_with_config:=nil;
  wasm_store_delete:=nil;
  wasm_store_new:=nil;
  wasm_valtype_delete:=nil;
  wasm_valtype_vec_new_empty:=nil;
  wasm_valtype_vec_new_uninitialized:=nil;
  wasm_valtype_vec_new:=nil;
  wasm_valtype_vec_copy:=nil;
  wasm_valtype_vec_delete:=nil;
  wasm_valtype_copy:=nil;
  wasm_valtype_new:=nil;
  wasm_valtype_kind:=nil;
  wasm_functype_delete:=nil;
  wasm_functype_vec_new_empty:=nil;
  wasm_functype_vec_new_uninitialized:=nil;
  wasm_functype_vec_new:=nil;
  wasm_functype_vec_copy:=nil;
  wasm_functype_vec_delete:=nil;
  wasm_functype_copy:=nil;
  wasm_functype_new:=nil;
  wasm_functype_params:=nil;
  wasm_functype_results:=nil;
  wasm_globaltype_delete:=nil;
  wasm_globaltype_vec_new_empty:=nil;
  wasm_globaltype_vec_new_uninitialized:=nil;
  wasm_globaltype_vec_new:=nil;
  wasm_globaltype_vec_copy:=nil;
  wasm_globaltype_vec_delete:=nil;
  wasm_globaltype_copy:=nil;
  wasm_globaltype_new:=nil;
  wasm_globaltype_content:=nil;
  wasm_globaltype_mutability:=nil;
  wasm_tabletype_delete:=nil;
  wasm_tabletype_vec_new_empty:=nil;
  wasm_tabletype_vec_new_uninitialized:=nil;
  wasm_tabletype_vec_new:=nil;
  wasm_tabletype_vec_copy:=nil;
  wasm_tabletype_vec_delete:=nil;
  wasm_tabletype_copy:=nil;
  wasm_tabletype_new:=nil;
  wasm_tabletype_element:=nil;
  wasm_tabletype_limits:=nil;
  wasm_memorytype_delete:=nil;
  wasm_memorytype_vec_new_empty:=nil;
  wasm_memorytype_vec_new_uninitialized:=nil;
  wasm_memorytype_vec_new:=nil;
  wasm_memorytype_vec_copy:=nil;
  wasm_memorytype_vec_delete:=nil;
  wasm_memorytype_copy:=nil;
  wasm_memorytype_new:=nil;
  wasm_memorytype_limits:=nil;
  wasm_externtype_delete:=nil;
  wasm_externtype_vec_new_empty:=nil;
  wasm_externtype_vec_new_uninitialized:=nil;
  wasm_externtype_vec_new:=nil;
  wasm_externtype_vec_copy:=nil;
  wasm_externtype_vec_delete:=nil;
  wasm_externtype_copy:=nil;
  wasm_externtype_kind:=nil;
  wasm_functype_as_externtype:=nil;
  wasm_globaltype_as_externtype:=nil;
  wasm_tabletype_as_externtype:=nil;
  wasm_memorytype_as_externtype:=nil;
  wasm_externtype_as_functype:=nil;
  wasm_externtype_as_globaltype:=nil;
  wasm_externtype_as_tabletype:=nil;
  wasm_externtype_as_memorytype:=nil;
  wasm_functype_as_externtype_const:=nil;
  wasm_globaltype_as_externtype_const:=nil;
  wasm_tabletype_as_externtype_const:=nil;
  wasm_memorytype_as_externtype_const:=nil;
  wasm_externtype_as_functype_const:=nil;
  wasm_externtype_as_globaltype_const:=nil;
  wasm_externtype_as_tabletype_const:=nil;
  wasm_externtype_as_memorytype_const:=nil;
  wasm_importtype_delete:=nil;
  wasm_importtype_vec_new_empty:=nil;
  wasm_importtype_vec_new_uninitialized:=nil;
  wasm_importtype_vec_new:=nil;
  wasm_importtype_vec_copy:=nil;
  wasm_importtype_vec_delete:=nil;
  wasm_importtype_copy:=nil;
  wasm_importtype_new:=nil;
  wasm_importtype_module:=nil;
  wasm_importtype_name:=nil;
  wasm_importtype_type:=nil;
  wasm_exporttype_delete:=nil;
  wasm_exporttype_vec_new_empty:=nil;
  wasm_exporttype_vec_new_uninitialized:=nil;
  wasm_exporttype_vec_new:=nil;
  wasm_exporttype_vec_copy:=nil;
  wasm_exporttype_vec_delete:=nil;
  wasm_exporttype_copy:=nil;
  wasm_exporttype_new:=nil;
  wasm_exporttype_name:=nil;
  wasm_exporttype_type:=nil;
  wasm_val_delete:=nil;
  wasm_val_copy:=nil;
  wasm_val_vec_new_empty:=nil;
  wasm_val_vec_new_uninitialized:=nil;
  wasm_val_vec_new:=nil;
  wasm_val_vec_copy:=nil;
  wasm_val_vec_delete:=nil;
  wasm_ref_delete:=nil;
  wasm_ref_copy:=nil;
  wasm_ref_same:=nil;
  wasm_ref_get_host_info:=nil;
  wasm_ref_set_host_info:=nil;
  wasm_ref_set_host_info_with_finalizer:=nil;
  wasm_frame_delete:=nil;
  wasm_frame_vec_new_empty:=nil;
  wasm_frame_vec_new_uninitialized:=nil;
  wasm_frame_vec_new:=nil;
  wasm_frame_vec_copy:=nil;
  wasm_frame_vec_delete:=nil;
  wasm_frame_copy:=nil;
  wasm_frame_instance:=nil;
  wasm_frame_func_index:=nil;
  wasm_frame_func_offset:=nil;
  wasm_frame_module_offset:=nil;
  wasm_trap_delete:=nil;
  wasm_trap_copy:=nil;
  wasm_trap_same:=nil;
  wasm_trap_get_host_info:=nil;
  wasm_trap_set_host_info:=nil;
  wasm_trap_set_host_info_with_finalizer:=nil;
  wasm_trap_as_ref:=nil;
  wasm_ref_as_trap:=nil;
  wasm_trap_as_ref_const:=nil;
  wasm_ref_as_trap_const:=nil;
  wasm_trap_new:=nil;
  wasm_trap_message:=nil;
  wasm_trap_origin:=nil;
  wasm_trap_trace:=nil;
  wasm_foreign_delete:=nil;
  wasm_foreign_copy:=nil;
  wasm_foreign_same:=nil;
  wasm_foreign_get_host_info:=nil;
  wasm_foreign_set_host_info:=nil;
  wasm_foreign_set_host_info_with_finalizer:=nil;
  wasm_foreign_as_ref:=nil;
  wasm_ref_as_foreign:=nil;
  wasm_foreign_as_ref_const:=nil;
  wasm_ref_as_foreign_const:=nil;
  wasm_foreign_new:=nil;
  wasm_module_delete:=nil;
  wasm_module_copy:=nil;
  wasm_module_same:=nil;
  wasm_module_get_host_info:=nil;
  wasm_module_set_host_info:=nil;
  wasm_module_set_host_info_with_finalizer:=nil;
  wasm_module_as_ref:=nil;
  wasm_ref_as_module:=nil;
  wasm_module_as_ref_const:=nil;
  wasm_ref_as_module_const:=nil;
  wasm_shared_module_delete:=nil;
  wasm_module_share:=nil;
  wasm_module_obtain:=nil;
  wasm_module_new:=nil;
  wasm_module_validate:=nil;
  wasm_module_imports:=nil;
  wasm_module_exports:=nil;
  wasm_module_serialize:=nil;
  wasm_module_deserialize:=nil;
  wasm_func_delete:=nil;
  wasm_func_copy:=nil;
  wasm_func_same:=nil;
  wasm_func_get_host_info:=nil;
  wasm_func_set_host_info:=nil;
  wasm_func_set_host_info_with_finalizer:=nil;
  wasm_func_as_ref:=nil;
  wasm_ref_as_func:=nil;
  wasm_func_as_ref_const:=nil;
  wasm_ref_as_func_const:=nil;
  wasm_func_new:=nil;
  wasm_func_new_with_env:=nil;
  wasm_func_type:=nil;
  wasm_func_param_arity:=nil;
  wasm_func_result_arity:=nil;
  wasm_func_call:=nil;
  wasm_global_delete:=nil;
  wasm_global_copy:=nil;
  wasm_global_same:=nil;
  wasm_global_get_host_info:=nil;
  wasm_global_set_host_info:=nil;
  wasm_global_set_host_info_with_finalizer:=nil;
  wasm_global_as_ref:=nil;
  wasm_ref_as_global:=nil;
  wasm_global_as_ref_const:=nil;
  wasm_ref_as_global_const:=nil;
  wasm_global_new:=nil;
  wasm_global_type:=nil;
  wasm_global_get:=nil;
  wasm_global_set:=nil;
  wasm_table_delete:=nil;
  wasm_table_copy:=nil;
  wasm_table_same:=nil;
  wasm_table_get_host_info:=nil;
  wasm_table_set_host_info:=nil;
  wasm_table_set_host_info_with_finalizer:=nil;
  wasm_table_as_ref:=nil;
  wasm_ref_as_table:=nil;
  wasm_table_as_ref_const:=nil;
  wasm_ref_as_table_const:=nil;
  wasm_table_new:=nil;
  wasm_table_type:=nil;
  wasm_table_get:=nil;
  wasm_table_set:=nil;
  wasm_table_size:=nil;
  wasm_table_grow:=nil;
  wasm_memory_delete:=nil;
  wasm_memory_copy:=nil;
  wasm_memory_same:=nil;
  wasm_memory_get_host_info:=nil;
  wasm_memory_set_host_info:=nil;
  wasm_memory_set_host_info_with_finalizer:=nil;
  wasm_memory_as_ref:=nil;
  wasm_ref_as_memory:=nil;
  wasm_memory_as_ref_const:=nil;
  wasm_ref_as_memory_const:=nil;
  wasm_memory_new:=nil;
  wasm_memory_type:=nil;
  wasm_memory_data:=nil;
  wasm_memory_data_size:=nil;
  wasm_memory_size:=nil;
  wasm_memory_grow:=nil;
  wasm_extern_delete:=nil;
  wasm_extern_copy:=nil;
  wasm_extern_same:=nil;
  wasm_extern_get_host_info:=nil;
  wasm_extern_set_host_info:=nil;
  wasm_extern_set_host_info_with_finalizer:=nil;
  wasm_extern_as_ref:=nil;
  wasm_ref_as_extern:=nil;
  wasm_extern_as_ref_const:=nil;
  wasm_ref_as_extern_const:=nil;
  wasm_extern_vec_new_empty:=nil;
  wasm_extern_vec_new_uninitialized:=nil;
  wasm_extern_vec_new:=nil;
  wasm_extern_vec_copy:=nil;
  wasm_extern_vec_delete:=nil;
  wasm_extern_kind:=nil;
  wasm_extern_type:=nil;
  wasm_func_as_extern:=nil;
  wasm_global_as_extern:=nil;
  wasm_table_as_extern:=nil;
  wasm_memory_as_extern:=nil;
  wasm_extern_as_func:=nil;
  wasm_extern_as_global:=nil;
  wasm_extern_as_table:=nil;
  wasm_extern_as_memory:=nil;
  wasm_func_as_extern_const:=nil;
  wasm_global_as_extern_const:=nil;
  wasm_table_as_extern_const:=nil;
  wasm_memory_as_extern_const:=nil;
  wasm_extern_as_func_const:=nil;
  wasm_extern_as_global_const:=nil;
  wasm_extern_as_table_const:=nil;
  wasm_extern_as_memory_const:=nil;
  wasm_instance_delete:=nil;
  wasm_instance_copy:=nil;
  wasm_instance_same:=nil;
  wasm_instance_get_host_info:=nil;
  wasm_instance_set_host_info:=nil;
  wasm_instance_set_host_info_with_finalizer:=nil;
  wasm_instance_as_ref:=nil;
  wasm_ref_as_instance:=nil;
  wasm_instance_as_ref_const:=nil;
  wasm_ref_as_instance_const:=nil;
  wasm_instance_new:=nil;
  wasm_instance_exports:=nil;
  wasi_config_delete:=nil;
  wasi_config_new:=nil;
  wasi_config_set_argv:=nil;
  wasi_config_inherit_argv:=nil;
  wasi_config_set_env:=nil;
  wasi_config_inherit_env:=nil;
  wasi_config_set_stdin_file:=nil;
  wasi_config_inherit_stdin:=nil;
  wasi_config_set_stdout_file:=nil;
  wasi_config_inherit_stdout:=nil;
  wasi_config_set_stderr_file:=nil;
  wasi_config_inherit_stderr:=nil;
  wasi_config_preopen_dir:=nil;
  wasmtime_error_delete:=nil;
  wasmtime_error_message:=nil;
  wasmtime_error_exit_status:=nil;
  wasmtime_error_wasm_trace:=nil;
  wasmtime_config_debug_info_set:=nil;
  wasmtime_config_interruptable_set:=nil;
  wasmtime_config_consume_fuel_set:=nil;
  wasmtime_config_max_wasm_stack_set:=nil;
  wasmtime_config_wasm_threads_set:=nil;
  wasmtime_config_wasm_reference_types_set:=nil;
  wasmtime_config_wasm_simd_set:=nil;
  wasmtime_config_wasm_bulk_memory_set:=nil;
  wasmtime_config_wasm_multi_value_set:=nil;
  wasmtime_config_wasm_multi_memory_set:=nil;
  wasmtime_config_wasm_module_linking_set:=nil;
  wasmtime_config_wasm_memory64_set:=nil;
  wasmtime_config_strategy_set:=nil;
  wasmtime_config_cranelift_debug_verifier_set:=nil;
  wasmtime_config_cranelift_opt_level_set:=nil;
  wasmtime_config_profiler_set:=nil;
  wasmtime_config_static_memory_maximum_size_set:=nil;
  wasmtime_config_static_memory_guard_size_set:=nil;
  wasmtime_config_dynamic_memory_guard_size_set:=nil;
  wasmtime_config_cache_config_load:=nil;
  wasmtime_moduletype_delete:=nil;
  wasmtime_moduletype_imports:=nil;
  wasmtime_moduletype_exports:=nil;
  wasmtime_moduletype_as_externtype:=nil;
  wasmtime_externtype_as_moduletype:=nil;
  wasmtime_module_new:=nil;
  wasmtime_module_delete:=nil;
  wasmtime_module_clone:=nil;
  wasmtime_module_validate:=nil;
  wasmtime_module_type:=nil;
  wasmtime_module_serialize:=nil;
  wasmtime_module_deserialize:=nil;
  wasmtime_module_deserialize_file:=nil;
  wasmtime_store_new:=nil;
  wasmtime_store_context:=nil;
  wasmtime_store_delete:=nil;
  wasmtime_context_get_data:=nil;
  wasmtime_context_set_data:=nil;
  wasmtime_context_gc:=nil;
  wasmtime_context_add_fuel:=nil;
  wasmtime_context_fuel_consumed:=nil;
  wasmtime_context_consume_fuel:=nil;
  wasmtime_context_set_wasi:=nil;
  wasmtime_interrupt_handle_new:=nil;
  wasmtime_interrupt_handle_interrupt:=nil;
  wasmtime_interrupt_handle_delete:=nil;
  wasmtime_extern_delete:=nil;
  wasmtime_extern_type:=nil;
  wasmtime_externref_new:=nil;
  wasmtime_externref_data:=nil;
  wasmtime_externref_clone:=nil;
  wasmtime_externref_delete:=nil;
  wasmtime_externref_from_raw:=nil;
  wasmtime_externref_to_raw:=nil;
  wasmtime_val_delete:=nil;
  wasmtime_val_copy:=nil;
  wasmtime_func_new:=nil;
  wasmtime_func_new_unchecked:=nil;
  wasmtime_func_type:=nil;
  wasmtime_func_call:=nil;
  wasmtime_func_call_unchecked:=nil;
  wasmtime_caller_export_get:=nil;
  wasmtime_caller_context:=nil;
  wasmtime_func_from_raw:=nil;
  wasmtime_func_to_raw:=nil;
  wasmtime_global_new:=nil;
  wasmtime_global_type:=nil;
  wasmtime_global_get:=nil;
  wasmtime_global_set:=nil;
  wasmtime_instancetype_delete:=nil;
  wasmtime_instancetype_exports:=nil;
  wasmtime_instancetype_as_externtype:=nil;
  wasmtime_externtype_as_instancetype:=nil;
  wasmtime_instance_new:=nil;
  wasmtime_instance_type:=nil;
  wasmtime_instance_export_get:=nil;
  wasmtime_instance_export_nth:=nil;
  wasmtime_linker_new:=nil;
  wasmtime_linker_delete:=nil;
  wasmtime_linker_allow_shadowing:=nil;
  wasmtime_linker_define:=nil;
  wasmtime_linker_define_func:=nil;
  wasmtime_linker_define_func_unchecked:=nil;
  wasmtime_linker_define_wasi:=nil;
  wasmtime_linker_define_instance:=nil;
  wasmtime_linker_instantiate:=nil;
  wasmtime_linker_module:=nil;
  wasmtime_linker_get_default:=nil;
  wasmtime_linker_get:=nil;
  wasmtime_memorytype_new:=nil;
  wasmtime_memorytype_minimum:=nil;
  wasmtime_memorytype_maximum:=nil;
  wasmtime_memorytype_is64:=nil;
  wasmtime_memory_new:=nil;
  wasmtime_memory_type:=nil;
  wasmtime_memory_data:=nil;
  wasmtime_memory_data_size:=nil;
  wasmtime_memory_size:=nil;
  wasmtime_memory_grow:=nil;
  wasmtime_table_new:=nil;
  wasmtime_table_type:=nil;
  wasmtime_table_get:=nil;
  wasmtime_table_set:=nil;
  wasmtime_table_size:=nil;
  wasmtime_table_grow:=nil;
  wasmtime_trap_new:=nil;
  wasmtime_trap_code:=nil;
  wasmtime_trap_exit_status:=nil;
  wasmtime_frame_func_name:=nil;
  wasmtime_frame_module_name:=nil;
  wasmtime_wat2wasm:=nil;
end;


procedure LoadWasmTime(const lib : string);
begin
  Freewasmtime;
  hlib:=LoadLibrary(lib);
  if hlib=NilHandle then
    raise Exception.Create(format('Could not load library: %s',[lib]));

  pointer(wasm_byte_vec_new_empty):=GetProcAddress(hlib,'wasm_byte_vec_new_empty');
  pointer(wasm_byte_vec_new_uninitialized):=GetProcAddress(hlib,'wasm_byte_vec_new_uninitialized');
  pointer(wasm_byte_vec_new):=GetProcAddress(hlib,'wasm_byte_vec_new');
  pointer(wasm_byte_vec_copy):=GetProcAddress(hlib,'wasm_byte_vec_copy');
  pointer(wasm_byte_vec_delete):=GetProcAddress(hlib,'wasm_byte_vec_delete');
  pointer(wasm_config_delete):=GetProcAddress(hlib,'wasm_config_delete');
  pointer(wasm_config_new):=GetProcAddress(hlib,'wasm_config_new');
  pointer(wasm_engine_delete):=GetProcAddress(hlib,'wasm_engine_delete');
  pointer(wasm_engine_new):=GetProcAddress(hlib,'wasm_engine_new');
  pointer(wasm_engine_new_with_config):=GetProcAddress(hlib,'wasm_engine_new_with_config');
  pointer(wasm_store_delete):=GetProcAddress(hlib,'wasm_store_delete');
  pointer(wasm_store_new):=GetProcAddress(hlib,'wasm_store_new');
  pointer(wasm_valtype_delete):=GetProcAddress(hlib,'wasm_valtype_delete');
  pointer(wasm_valtype_vec_new_empty):=GetProcAddress(hlib,'wasm_valtype_vec_new_empty');
  pointer(wasm_valtype_vec_new_uninitialized):=GetProcAddress(hlib,'wasm_valtype_vec_new_uninitialized');
  pointer(wasm_valtype_vec_new):=GetProcAddress(hlib,'wasm_valtype_vec_new');
  pointer(wasm_valtype_vec_copy):=GetProcAddress(hlib,'wasm_valtype_vec_copy');
  pointer(wasm_valtype_vec_delete):=GetProcAddress(hlib,'wasm_valtype_vec_delete');
  pointer(wasm_valtype_copy):=GetProcAddress(hlib,'wasm_valtype_copy');
  pointer(wasm_valtype_new):=GetProcAddress(hlib,'wasm_valtype_new');
  pointer(wasm_valtype_kind):=GetProcAddress(hlib,'wasm_valtype_kind');
  pointer(wasm_functype_delete):=GetProcAddress(hlib,'wasm_functype_delete');
  pointer(wasm_functype_vec_new_empty):=GetProcAddress(hlib,'wasm_functype_vec_new_empty');
  pointer(wasm_functype_vec_new_uninitialized):=GetProcAddress(hlib,'wasm_functype_vec_new_uninitialized');
  pointer(wasm_functype_vec_new):=GetProcAddress(hlib,'wasm_functype_vec_new');
  pointer(wasm_functype_vec_copy):=GetProcAddress(hlib,'wasm_functype_vec_copy');
  pointer(wasm_functype_vec_delete):=GetProcAddress(hlib,'wasm_functype_vec_delete');
  pointer(wasm_functype_copy):=GetProcAddress(hlib,'wasm_functype_copy');
  pointer(wasm_functype_new):=GetProcAddress(hlib,'wasm_functype_new');
  pointer(wasm_functype_params):=GetProcAddress(hlib,'wasm_functype_params');
  pointer(wasm_functype_results):=GetProcAddress(hlib,'wasm_functype_results');
  pointer(wasm_globaltype_delete):=GetProcAddress(hlib,'wasm_globaltype_delete');
  pointer(wasm_globaltype_vec_new_empty):=GetProcAddress(hlib,'wasm_globaltype_vec_new_empty');
  pointer(wasm_globaltype_vec_new_uninitialized):=GetProcAddress(hlib,'wasm_globaltype_vec_new_uninitialized');
  pointer(wasm_globaltype_vec_new):=GetProcAddress(hlib,'wasm_globaltype_vec_new');
  pointer(wasm_globaltype_vec_copy):=GetProcAddress(hlib,'wasm_globaltype_vec_copy');
  pointer(wasm_globaltype_vec_delete):=GetProcAddress(hlib,'wasm_globaltype_vec_delete');
  pointer(wasm_globaltype_copy):=GetProcAddress(hlib,'wasm_globaltype_copy');
  pointer(wasm_globaltype_new):=GetProcAddress(hlib,'wasm_globaltype_new');
  pointer(wasm_globaltype_content):=GetProcAddress(hlib,'wasm_globaltype_content');
  pointer(wasm_globaltype_mutability):=GetProcAddress(hlib,'wasm_globaltype_mutability');
  pointer(wasm_tabletype_delete):=GetProcAddress(hlib,'wasm_tabletype_delete');
  pointer(wasm_tabletype_vec_new_empty):=GetProcAddress(hlib,'wasm_tabletype_vec_new_empty');
  pointer(wasm_tabletype_vec_new_uninitialized):=GetProcAddress(hlib,'wasm_tabletype_vec_new_uninitialized');
  pointer(wasm_tabletype_vec_new):=GetProcAddress(hlib,'wasm_tabletype_vec_new');
  pointer(wasm_tabletype_vec_copy):=GetProcAddress(hlib,'wasm_tabletype_vec_copy');
  pointer(wasm_tabletype_vec_delete):=GetProcAddress(hlib,'wasm_tabletype_vec_delete');
  pointer(wasm_tabletype_copy):=GetProcAddress(hlib,'wasm_tabletype_copy');
  pointer(wasm_tabletype_new):=GetProcAddress(hlib,'wasm_tabletype_new');
  pointer(wasm_tabletype_element):=GetProcAddress(hlib,'wasm_tabletype_element');
  pointer(wasm_tabletype_limits):=GetProcAddress(hlib,'wasm_tabletype_limits');
  pointer(wasm_memorytype_delete):=GetProcAddress(hlib,'wasm_memorytype_delete');
  pointer(wasm_memorytype_vec_new_empty):=GetProcAddress(hlib,'wasm_memorytype_vec_new_empty');
  pointer(wasm_memorytype_vec_new_uninitialized):=GetProcAddress(hlib,'wasm_memorytype_vec_new_uninitialized');
  pointer(wasm_memorytype_vec_new):=GetProcAddress(hlib,'wasm_memorytype_vec_new');
  pointer(wasm_memorytype_vec_copy):=GetProcAddress(hlib,'wasm_memorytype_vec_copy');
  pointer(wasm_memorytype_vec_delete):=GetProcAddress(hlib,'wasm_memorytype_vec_delete');
  pointer(wasm_memorytype_copy):=GetProcAddress(hlib,'wasm_memorytype_copy');
  pointer(wasm_memorytype_new):=GetProcAddress(hlib,'wasm_memorytype_new');
  pointer(wasm_memorytype_limits):=GetProcAddress(hlib,'wasm_memorytype_limits');
  pointer(wasm_externtype_delete):=GetProcAddress(hlib,'wasm_externtype_delete');
  pointer(wasm_externtype_vec_new_empty):=GetProcAddress(hlib,'wasm_externtype_vec_new_empty');
  pointer(wasm_externtype_vec_new_uninitialized):=GetProcAddress(hlib,'wasm_externtype_vec_new_uninitialized');
  pointer(wasm_externtype_vec_new):=GetProcAddress(hlib,'wasm_externtype_vec_new');
  pointer(wasm_externtype_vec_copy):=GetProcAddress(hlib,'wasm_externtype_vec_copy');
  pointer(wasm_externtype_vec_delete):=GetProcAddress(hlib,'wasm_externtype_vec_delete');
  pointer(wasm_externtype_copy):=GetProcAddress(hlib,'wasm_externtype_copy');
  pointer(wasm_externtype_kind):=GetProcAddress(hlib,'wasm_externtype_kind');
  pointer(wasm_functype_as_externtype):=GetProcAddress(hlib,'wasm_functype_as_externtype');
  pointer(wasm_globaltype_as_externtype):=GetProcAddress(hlib,'wasm_globaltype_as_externtype');
  pointer(wasm_tabletype_as_externtype):=GetProcAddress(hlib,'wasm_tabletype_as_externtype');
  pointer(wasm_memorytype_as_externtype):=GetProcAddress(hlib,'wasm_memorytype_as_externtype');
  pointer(wasm_externtype_as_functype):=GetProcAddress(hlib,'wasm_externtype_as_functype');
  pointer(wasm_externtype_as_globaltype):=GetProcAddress(hlib,'wasm_externtype_as_globaltype');
  pointer(wasm_externtype_as_tabletype):=GetProcAddress(hlib,'wasm_externtype_as_tabletype');
  pointer(wasm_externtype_as_memorytype):=GetProcAddress(hlib,'wasm_externtype_as_memorytype');
  pointer(wasm_functype_as_externtype_const):=GetProcAddress(hlib,'wasm_functype_as_externtype_const');
  pointer(wasm_globaltype_as_externtype_const):=GetProcAddress(hlib,'wasm_globaltype_as_externtype_const');
  pointer(wasm_tabletype_as_externtype_const):=GetProcAddress(hlib,'wasm_tabletype_as_externtype_const');
  pointer(wasm_memorytype_as_externtype_const):=GetProcAddress(hlib,'wasm_memorytype_as_externtype_const');
  pointer(wasm_externtype_as_functype_const):=GetProcAddress(hlib,'wasm_externtype_as_functype_const');
  pointer(wasm_externtype_as_globaltype_const):=GetProcAddress(hlib,'wasm_externtype_as_globaltype_const');
  pointer(wasm_externtype_as_tabletype_const):=GetProcAddress(hlib,'wasm_externtype_as_tabletype_const');
  pointer(wasm_externtype_as_memorytype_const):=GetProcAddress(hlib,'wasm_externtype_as_memorytype_const');
  pointer(wasm_importtype_delete):=GetProcAddress(hlib,'wasm_importtype_delete');
  pointer(wasm_importtype_vec_new_empty):=GetProcAddress(hlib,'wasm_importtype_vec_new_empty');
  pointer(wasm_importtype_vec_new_uninitialized):=GetProcAddress(hlib,'wasm_importtype_vec_new_uninitialized');
  pointer(wasm_importtype_vec_new):=GetProcAddress(hlib,'wasm_importtype_vec_new');
  pointer(wasm_importtype_vec_copy):=GetProcAddress(hlib,'wasm_importtype_vec_copy');
  pointer(wasm_importtype_vec_delete):=GetProcAddress(hlib,'wasm_importtype_vec_delete');
  pointer(wasm_importtype_copy):=GetProcAddress(hlib,'wasm_importtype_copy');
  pointer(wasm_importtype_new):=GetProcAddress(hlib,'wasm_importtype_new');
  pointer(wasm_importtype_module):=GetProcAddress(hlib,'wasm_importtype_module');
  pointer(wasm_importtype_name):=GetProcAddress(hlib,'wasm_importtype_name');
  pointer(wasm_importtype_type):=GetProcAddress(hlib,'wasm_importtype_type');
  pointer(wasm_exporttype_delete):=GetProcAddress(hlib,'wasm_exporttype_delete');
  pointer(wasm_exporttype_vec_new_empty):=GetProcAddress(hlib,'wasm_exporttype_vec_new_empty');
  pointer(wasm_exporttype_vec_new_uninitialized):=GetProcAddress(hlib,'wasm_exporttype_vec_new_uninitialized');
  pointer(wasm_exporttype_vec_new):=GetProcAddress(hlib,'wasm_exporttype_vec_new');
  pointer(wasm_exporttype_vec_copy):=GetProcAddress(hlib,'wasm_exporttype_vec_copy');
  pointer(wasm_exporttype_vec_delete):=GetProcAddress(hlib,'wasm_exporttype_vec_delete');
  pointer(wasm_exporttype_copy):=GetProcAddress(hlib,'wasm_exporttype_copy');
  pointer(wasm_exporttype_new):=GetProcAddress(hlib,'wasm_exporttype_new');
  pointer(wasm_exporttype_name):=GetProcAddress(hlib,'wasm_exporttype_name');
  pointer(wasm_exporttype_type):=GetProcAddress(hlib,'wasm_exporttype_type');
  pointer(wasm_val_delete):=GetProcAddress(hlib,'wasm_val_delete');
  pointer(wasm_val_copy):=GetProcAddress(hlib,'wasm_val_copy');
  pointer(wasm_val_vec_new_empty):=GetProcAddress(hlib,'wasm_val_vec_new_empty');
  pointer(wasm_val_vec_new_uninitialized):=GetProcAddress(hlib,'wasm_val_vec_new_uninitialized');
  pointer(wasm_val_vec_new):=GetProcAddress(hlib,'wasm_val_vec_new');
  pointer(wasm_val_vec_copy):=GetProcAddress(hlib,'wasm_val_vec_copy');
  pointer(wasm_val_vec_delete):=GetProcAddress(hlib,'wasm_val_vec_delete');
  pointer(wasm_ref_delete):=GetProcAddress(hlib,'wasm_ref_delete');
  pointer(wasm_ref_copy):=GetProcAddress(hlib,'wasm_ref_copy');
  pointer(wasm_ref_same):=GetProcAddress(hlib,'wasm_ref_same');
  pointer(wasm_ref_get_host_info):=GetProcAddress(hlib,'wasm_ref_get_host_info');
  pointer(wasm_ref_set_host_info):=GetProcAddress(hlib,'wasm_ref_set_host_info');
  pointer(wasm_ref_set_host_info_with_finalizer):=GetProcAddress(hlib,'wasm_ref_set_host_info_with_finalizer');
  pointer(wasm_frame_delete):=GetProcAddress(hlib,'wasm_frame_delete');
  pointer(wasm_frame_vec_new_empty):=GetProcAddress(hlib,'wasm_frame_vec_new_empty');
  pointer(wasm_frame_vec_new_uninitialized):=GetProcAddress(hlib,'wasm_frame_vec_new_uninitialized');
  pointer(wasm_frame_vec_new):=GetProcAddress(hlib,'wasm_frame_vec_new');
  pointer(wasm_frame_vec_copy):=GetProcAddress(hlib,'wasm_frame_vec_copy');
  pointer(wasm_frame_vec_delete):=GetProcAddress(hlib,'wasm_frame_vec_delete');
  pointer(wasm_frame_copy):=GetProcAddress(hlib,'wasm_frame_copy');
  pointer(wasm_frame_instance):=GetProcAddress(hlib,'wasm_frame_instance');
  pointer(wasm_frame_func_index):=GetProcAddress(hlib,'wasm_frame_func_index');
  pointer(wasm_frame_func_offset):=GetProcAddress(hlib,'wasm_frame_func_offset');
  pointer(wasm_frame_module_offset):=GetProcAddress(hlib,'wasm_frame_module_offset');
  pointer(wasm_trap_delete):=GetProcAddress(hlib,'wasm_trap_delete');
  pointer(wasm_trap_copy):=GetProcAddress(hlib,'wasm_trap_copy');
  pointer(wasm_trap_same):=GetProcAddress(hlib,'wasm_trap_same');
  pointer(wasm_trap_get_host_info):=GetProcAddress(hlib,'wasm_trap_get_host_info');
  pointer(wasm_trap_set_host_info):=GetProcAddress(hlib,'wasm_trap_set_host_info');
  pointer(wasm_trap_set_host_info_with_finalizer):=GetProcAddress(hlib,'wasm_trap_set_host_info_with_finalizer');
  pointer(wasm_trap_as_ref):=GetProcAddress(hlib,'wasm_trap_as_ref');
  pointer(wasm_ref_as_trap):=GetProcAddress(hlib,'wasm_ref_as_trap');
  pointer(wasm_trap_as_ref_const):=GetProcAddress(hlib,'wasm_trap_as_ref_const');
  pointer(wasm_ref_as_trap_const):=GetProcAddress(hlib,'wasm_ref_as_trap_const');
  pointer(wasm_trap_new):=GetProcAddress(hlib,'wasm_trap_new');
  pointer(wasm_trap_message):=GetProcAddress(hlib,'wasm_trap_message');
  pointer(wasm_trap_origin):=GetProcAddress(hlib,'wasm_trap_origin');
  pointer(wasm_trap_trace):=GetProcAddress(hlib,'wasm_trap_trace');
  pointer(wasm_foreign_delete):=GetProcAddress(hlib,'wasm_foreign_delete');
  pointer(wasm_foreign_copy):=GetProcAddress(hlib,'wasm_foreign_copy');
  pointer(wasm_foreign_same):=GetProcAddress(hlib,'wasm_foreign_same');
  pointer(wasm_foreign_get_host_info):=GetProcAddress(hlib,'wasm_foreign_get_host_info');
  pointer(wasm_foreign_set_host_info):=GetProcAddress(hlib,'wasm_foreign_set_host_info');
  pointer(wasm_foreign_set_host_info_with_finalizer):=GetProcAddress(hlib,'wasm_foreign_set_host_info_with_finalizer');
  pointer(wasm_foreign_as_ref):=GetProcAddress(hlib,'wasm_foreign_as_ref');
  pointer(wasm_ref_as_foreign):=GetProcAddress(hlib,'wasm_ref_as_foreign');
  pointer(wasm_foreign_as_ref_const):=GetProcAddress(hlib,'wasm_foreign_as_ref_const');
  pointer(wasm_ref_as_foreign_const):=GetProcAddress(hlib,'wasm_ref_as_foreign_const');
  pointer(wasm_foreign_new):=GetProcAddress(hlib,'wasm_foreign_new');
  pointer(wasm_module_delete):=GetProcAddress(hlib,'wasm_module_delete');
  pointer(wasm_module_copy):=GetProcAddress(hlib,'wasm_module_copy');
  pointer(wasm_module_same):=GetProcAddress(hlib,'wasm_module_same');
  pointer(wasm_module_get_host_info):=GetProcAddress(hlib,'wasm_module_get_host_info');
  pointer(wasm_module_set_host_info):=GetProcAddress(hlib,'wasm_module_set_host_info');
  pointer(wasm_module_set_host_info_with_finalizer):=GetProcAddress(hlib,'wasm_module_set_host_info_with_finalizer');
  pointer(wasm_module_as_ref):=GetProcAddress(hlib,'wasm_module_as_ref');
  pointer(wasm_ref_as_module):=GetProcAddress(hlib,'wasm_ref_as_module');
  pointer(wasm_module_as_ref_const):=GetProcAddress(hlib,'wasm_module_as_ref_const');
  pointer(wasm_ref_as_module_const):=GetProcAddress(hlib,'wasm_ref_as_module_const');
  pointer(wasm_shared_module_delete):=GetProcAddress(hlib,'wasm_shared_module_delete');
  pointer(wasm_module_share):=GetProcAddress(hlib,'wasm_module_share');
  pointer(wasm_module_obtain):=GetProcAddress(hlib,'wasm_module_obtain');
  pointer(wasm_module_new):=GetProcAddress(hlib,'wasm_module_new');
  pointer(wasm_module_validate):=GetProcAddress(hlib,'wasm_module_validate');
  pointer(wasm_module_imports):=GetProcAddress(hlib,'wasm_module_imports');
  pointer(wasm_module_exports):=GetProcAddress(hlib,'wasm_module_exports');
  pointer(wasm_module_serialize):=GetProcAddress(hlib,'wasm_module_serialize');
  pointer(wasm_module_deserialize):=GetProcAddress(hlib,'wasm_module_deserialize');
  pointer(wasm_func_delete):=GetProcAddress(hlib,'wasm_func_delete');
  pointer(wasm_func_copy):=GetProcAddress(hlib,'wasm_func_copy');
  pointer(wasm_func_same):=GetProcAddress(hlib,'wasm_func_same');
  pointer(wasm_func_get_host_info):=GetProcAddress(hlib,'wasm_func_get_host_info');
  pointer(wasm_func_set_host_info):=GetProcAddress(hlib,'wasm_func_set_host_info');
  pointer(wasm_func_set_host_info_with_finalizer):=GetProcAddress(hlib,'wasm_func_set_host_info_with_finalizer');
  pointer(wasm_func_as_ref):=GetProcAddress(hlib,'wasm_func_as_ref');
  pointer(wasm_ref_as_func):=GetProcAddress(hlib,'wasm_ref_as_func');
  pointer(wasm_func_as_ref_const):=GetProcAddress(hlib,'wasm_func_as_ref_const');
  pointer(wasm_ref_as_func_const):=GetProcAddress(hlib,'wasm_ref_as_func_const');
  pointer(wasm_func_new):=GetProcAddress(hlib,'wasm_func_new');
  pointer(wasm_func_new_with_env):=GetProcAddress(hlib,'wasm_func_new_with_env');
  pointer(wasm_func_type):=GetProcAddress(hlib,'wasm_func_type');
  pointer(wasm_func_param_arity):=GetProcAddress(hlib,'wasm_func_param_arity');
  pointer(wasm_func_result_arity):=GetProcAddress(hlib,'wasm_func_result_arity');
  pointer(wasm_func_call):=GetProcAddress(hlib,'wasm_func_call');
  pointer(wasm_global_delete):=GetProcAddress(hlib,'wasm_global_delete');
  pointer(wasm_global_copy):=GetProcAddress(hlib,'wasm_global_copy');
  pointer(wasm_global_same):=GetProcAddress(hlib,'wasm_global_same');
  pointer(wasm_global_get_host_info):=GetProcAddress(hlib,'wasm_global_get_host_info');
  pointer(wasm_global_set_host_info):=GetProcAddress(hlib,'wasm_global_set_host_info');
  pointer(wasm_global_set_host_info_with_finalizer):=GetProcAddress(hlib,'wasm_global_set_host_info_with_finalizer');
  pointer(wasm_global_as_ref):=GetProcAddress(hlib,'wasm_global_as_ref');
  pointer(wasm_ref_as_global):=GetProcAddress(hlib,'wasm_ref_as_global');
  pointer(wasm_global_as_ref_const):=GetProcAddress(hlib,'wasm_global_as_ref_const');
  pointer(wasm_ref_as_global_const):=GetProcAddress(hlib,'wasm_ref_as_global_const');
  pointer(wasm_global_new):=GetProcAddress(hlib,'wasm_global_new');
  pointer(wasm_global_type):=GetProcAddress(hlib,'wasm_global_type');
  pointer(wasm_global_get):=GetProcAddress(hlib,'wasm_global_get');
  pointer(wasm_global_set):=GetProcAddress(hlib,'wasm_global_set');
  pointer(wasm_table_delete):=GetProcAddress(hlib,'wasm_table_delete');
  pointer(wasm_table_copy):=GetProcAddress(hlib,'wasm_table_copy');
  pointer(wasm_table_same):=GetProcAddress(hlib,'wasm_table_same');
  pointer(wasm_table_get_host_info):=GetProcAddress(hlib,'wasm_table_get_host_info');
  pointer(wasm_table_set_host_info):=GetProcAddress(hlib,'wasm_table_set_host_info');
  pointer(wasm_table_set_host_info_with_finalizer):=GetProcAddress(hlib,'wasm_table_set_host_info_with_finalizer');
  pointer(wasm_table_as_ref):=GetProcAddress(hlib,'wasm_table_as_ref');
  pointer(wasm_ref_as_table):=GetProcAddress(hlib,'wasm_ref_as_table');
  pointer(wasm_table_as_ref_const):=GetProcAddress(hlib,'wasm_table_as_ref_const');
  pointer(wasm_ref_as_table_const):=GetProcAddress(hlib,'wasm_ref_as_table_const');
  pointer(wasm_table_new):=GetProcAddress(hlib,'wasm_table_new');
  pointer(wasm_table_type):=GetProcAddress(hlib,'wasm_table_type');
  pointer(wasm_table_get):=GetProcAddress(hlib,'wasm_table_get');
  pointer(wasm_table_set):=GetProcAddress(hlib,'wasm_table_set');
  pointer(wasm_table_size):=GetProcAddress(hlib,'wasm_table_size');
  pointer(wasm_table_grow):=GetProcAddress(hlib,'wasm_table_grow');
  pointer(wasm_memory_delete):=GetProcAddress(hlib,'wasm_memory_delete');
  pointer(wasm_memory_copy):=GetProcAddress(hlib,'wasm_memory_copy');
  pointer(wasm_memory_same):=GetProcAddress(hlib,'wasm_memory_same');
  pointer(wasm_memory_get_host_info):=GetProcAddress(hlib,'wasm_memory_get_host_info');
  pointer(wasm_memory_set_host_info):=GetProcAddress(hlib,'wasm_memory_set_host_info');
  pointer(wasm_memory_set_host_info_with_finalizer):=GetProcAddress(hlib,'wasm_memory_set_host_info_with_finalizer');
  pointer(wasm_memory_as_ref):=GetProcAddress(hlib,'wasm_memory_as_ref');
  pointer(wasm_ref_as_memory):=GetProcAddress(hlib,'wasm_ref_as_memory');
  pointer(wasm_memory_as_ref_const):=GetProcAddress(hlib,'wasm_memory_as_ref_const');
  pointer(wasm_ref_as_memory_const):=GetProcAddress(hlib,'wasm_ref_as_memory_const');
  pointer(wasm_memory_new):=GetProcAddress(hlib,'wasm_memory_new');
  pointer(wasm_memory_type):=GetProcAddress(hlib,'wasm_memory_type');
  pointer(wasm_memory_data):=GetProcAddress(hlib,'wasm_memory_data');
  pointer(wasm_memory_data_size):=GetProcAddress(hlib,'wasm_memory_data_size');
  pointer(wasm_memory_size):=GetProcAddress(hlib,'wasm_memory_size');
  pointer(wasm_memory_grow):=GetProcAddress(hlib,'wasm_memory_grow');
  pointer(wasm_extern_delete):=GetProcAddress(hlib,'wasm_extern_delete');
  pointer(wasm_extern_copy):=GetProcAddress(hlib,'wasm_extern_copy');
  pointer(wasm_extern_same):=GetProcAddress(hlib,'wasm_extern_same');
  pointer(wasm_extern_get_host_info):=GetProcAddress(hlib,'wasm_extern_get_host_info');
  pointer(wasm_extern_set_host_info):=GetProcAddress(hlib,'wasm_extern_set_host_info');
  pointer(wasm_extern_set_host_info_with_finalizer):=GetProcAddress(hlib,'wasm_extern_set_host_info_with_finalizer');
  pointer(wasm_extern_as_ref):=GetProcAddress(hlib,'wasm_extern_as_ref');
  pointer(wasm_ref_as_extern):=GetProcAddress(hlib,'wasm_ref_as_extern');
  pointer(wasm_extern_as_ref_const):=GetProcAddress(hlib,'wasm_extern_as_ref_const');
  pointer(wasm_ref_as_extern_const):=GetProcAddress(hlib,'wasm_ref_as_extern_const');
  pointer(wasm_extern_vec_new_empty):=GetProcAddress(hlib,'wasm_extern_vec_new_empty');
  pointer(wasm_extern_vec_new_uninitialized):=GetProcAddress(hlib,'wasm_extern_vec_new_uninitialized');
  pointer(wasm_extern_vec_new):=GetProcAddress(hlib,'wasm_extern_vec_new');
  pointer(wasm_extern_vec_copy):=GetProcAddress(hlib,'wasm_extern_vec_copy');
  pointer(wasm_extern_vec_delete):=GetProcAddress(hlib,'wasm_extern_vec_delete');
  pointer(wasm_extern_kind):=GetProcAddress(hlib,'wasm_extern_kind');
  pointer(wasm_extern_type):=GetProcAddress(hlib,'wasm_extern_type');
  pointer(wasm_func_as_extern):=GetProcAddress(hlib,'wasm_func_as_extern');
  pointer(wasm_global_as_extern):=GetProcAddress(hlib,'wasm_global_as_extern');
  pointer(wasm_table_as_extern):=GetProcAddress(hlib,'wasm_table_as_extern');
  pointer(wasm_memory_as_extern):=GetProcAddress(hlib,'wasm_memory_as_extern');
  pointer(wasm_extern_as_func):=GetProcAddress(hlib,'wasm_extern_as_func');
  pointer(wasm_extern_as_global):=GetProcAddress(hlib,'wasm_extern_as_global');
  pointer(wasm_extern_as_table):=GetProcAddress(hlib,'wasm_extern_as_table');
  pointer(wasm_extern_as_memory):=GetProcAddress(hlib,'wasm_extern_as_memory');
  pointer(wasm_func_as_extern_const):=GetProcAddress(hlib,'wasm_func_as_extern_const');
  pointer(wasm_global_as_extern_const):=GetProcAddress(hlib,'wasm_global_as_extern_const');
  pointer(wasm_table_as_extern_const):=GetProcAddress(hlib,'wasm_table_as_extern_const');
  pointer(wasm_memory_as_extern_const):=GetProcAddress(hlib,'wasm_memory_as_extern_const');
  pointer(wasm_extern_as_func_const):=GetProcAddress(hlib,'wasm_extern_as_func_const');
  pointer(wasm_extern_as_global_const):=GetProcAddress(hlib,'wasm_extern_as_global_const');
  pointer(wasm_extern_as_table_const):=GetProcAddress(hlib,'wasm_extern_as_table_const');
  pointer(wasm_extern_as_memory_const):=GetProcAddress(hlib,'wasm_extern_as_memory_const');
  pointer(wasm_instance_delete):=GetProcAddress(hlib,'wasm_instance_delete');
  pointer(wasm_instance_copy):=GetProcAddress(hlib,'wasm_instance_copy');
  pointer(wasm_instance_same):=GetProcAddress(hlib,'wasm_instance_same');
  pointer(wasm_instance_get_host_info):=GetProcAddress(hlib,'wasm_instance_get_host_info');
  pointer(wasm_instance_set_host_info):=GetProcAddress(hlib,'wasm_instance_set_host_info');
  pointer(wasm_instance_set_host_info_with_finalizer):=GetProcAddress(hlib,'wasm_instance_set_host_info_with_finalizer');
  pointer(wasm_instance_as_ref):=GetProcAddress(hlib,'wasm_instance_as_ref');
  pointer(wasm_ref_as_instance):=GetProcAddress(hlib,'wasm_ref_as_instance');
  pointer(wasm_instance_as_ref_const):=GetProcAddress(hlib,'wasm_instance_as_ref_const');
  pointer(wasm_ref_as_instance_const):=GetProcAddress(hlib,'wasm_ref_as_instance_const');
  pointer(wasm_instance_new):=GetProcAddress(hlib,'wasm_instance_new');
  pointer(wasm_instance_exports):=GetProcAddress(hlib,'wasm_instance_exports');
  pointer(wasi_config_delete):=GetProcAddress(hlib,'wasi_config_delete');
  pointer(wasi_config_new):=GetProcAddress(hlib,'wasi_config_new');
  pointer(wasi_config_set_argv):=GetProcAddress(hlib,'wasi_config_set_argv');
  pointer(wasi_config_inherit_argv):=GetProcAddress(hlib,'wasi_config_inherit_argv');
  pointer(wasi_config_set_env):=GetProcAddress(hlib,'wasi_config_set_env');
  pointer(wasi_config_inherit_env):=GetProcAddress(hlib,'wasi_config_inherit_env');
  pointer(wasi_config_set_stdin_file):=GetProcAddress(hlib,'wasi_config_set_stdin_file');
  pointer(wasi_config_inherit_stdin):=GetProcAddress(hlib,'wasi_config_inherit_stdin');
  pointer(wasi_config_set_stdout_file):=GetProcAddress(hlib,'wasi_config_set_stdout_file');
  pointer(wasi_config_inherit_stdout):=GetProcAddress(hlib,'wasi_config_inherit_stdout');
  pointer(wasi_config_set_stderr_file):=GetProcAddress(hlib,'wasi_config_set_stderr_file');
  pointer(wasi_config_inherit_stderr):=GetProcAddress(hlib,'wasi_config_inherit_stderr');
  pointer(wasi_config_preopen_dir):=GetProcAddress(hlib,'wasi_config_preopen_dir');
  pointer(wasmtime_error_delete):=GetProcAddress(hlib,'wasmtime_error_delete');
  pointer(wasmtime_error_message):=GetProcAddress(hlib,'wasmtime_error_message');
  pointer(wasmtime_error_exit_status):=GetProcAddress(hlib,'wasmtime_error_exit_status');
  pointer(wasmtime_error_wasm_trace):=GetProcAddress(hlib,'wasmtime_error_wasm_trace');
  pointer(wasmtime_config_debug_info_set):=GetProcAddress(hlib,'wasmtime_config_debug_info_set');
  pointer(wasmtime_config_interruptable_set):=GetProcAddress(hlib,'wasmtime_config_interruptable_set');
  pointer(wasmtime_config_consume_fuel_set):=GetProcAddress(hlib,'wasmtime_config_consume_fuel_set');
  pointer(wasmtime_config_max_wasm_stack_set):=GetProcAddress(hlib,'wasmtime_config_max_wasm_stack_set');
  pointer(wasmtime_config_wasm_threads_set):=GetProcAddress(hlib,'wasmtime_config_wasm_threads_set');
  pointer(wasmtime_config_wasm_reference_types_set):=GetProcAddress(hlib,'wasmtime_config_wasm_reference_types_set');
  pointer(wasmtime_config_wasm_simd_set):=GetProcAddress(hlib,'wasmtime_config_wasm_simd_set');
  pointer(wasmtime_config_wasm_bulk_memory_set):=GetProcAddress(hlib,'wasmtime_config_wasm_bulk_memory_set');
  pointer(wasmtime_config_wasm_multi_value_set):=GetProcAddress(hlib,'wasmtime_config_wasm_multi_value_set');
  pointer(wasmtime_config_wasm_multi_memory_set):=GetProcAddress(hlib,'wasmtime_config_wasm_multi_memory_set');
  pointer(wasmtime_config_wasm_module_linking_set):=GetProcAddress(hlib,'wasmtime_config_wasm_module_linking_set');
  pointer(wasmtime_config_wasm_memory64_set):=GetProcAddress(hlib,'wasmtime_config_wasm_memory64_set');
  pointer(wasmtime_config_strategy_set):=GetProcAddress(hlib,'wasmtime_config_strategy_set');
  pointer(wasmtime_config_cranelift_debug_verifier_set):=GetProcAddress(hlib,'wasmtime_config_cranelift_debug_verifier_set');
  pointer(wasmtime_config_cranelift_opt_level_set):=GetProcAddress(hlib,'wasmtime_config_cranelift_opt_level_set');
  pointer(wasmtime_config_profiler_set):=GetProcAddress(hlib,'wasmtime_config_profiler_set');
  pointer(wasmtime_config_static_memory_maximum_size_set):=GetProcAddress(hlib,'wasmtime_config_static_memory_maximum_size_set');
  pointer(wasmtime_config_static_memory_guard_size_set):=GetProcAddress(hlib,'wasmtime_config_static_memory_guard_size_set');
  pointer(wasmtime_config_dynamic_memory_guard_size_set):=GetProcAddress(hlib,'wasmtime_config_dynamic_memory_guard_size_set');
  pointer(wasmtime_config_cache_config_load):=GetProcAddress(hlib,'wasmtime_config_cache_config_load');
  pointer(wasmtime_moduletype_delete):=GetProcAddress(hlib,'wasmtime_moduletype_delete');
  pointer(wasmtime_moduletype_imports):=GetProcAddress(hlib,'wasmtime_moduletype_imports');
  pointer(wasmtime_moduletype_exports):=GetProcAddress(hlib,'wasmtime_moduletype_exports');
  pointer(wasmtime_moduletype_as_externtype):=GetProcAddress(hlib,'wasmtime_moduletype_as_externtype');
  pointer(wasmtime_externtype_as_moduletype):=GetProcAddress(hlib,'wasmtime_externtype_as_moduletype');
  pointer(wasmtime_module_new):=GetProcAddress(hlib,'wasmtime_module_new');
  pointer(wasmtime_module_delete):=GetProcAddress(hlib,'wasmtime_module_delete');
  pointer(wasmtime_module_clone):=GetProcAddress(hlib,'wasmtime_module_clone');
  pointer(wasmtime_module_validate):=GetProcAddress(hlib,'wasmtime_module_validate');
  pointer(wasmtime_module_type):=GetProcAddress(hlib,'wasmtime_module_type');
  pointer(wasmtime_module_serialize):=GetProcAddress(hlib,'wasmtime_module_serialize');
  pointer(wasmtime_module_deserialize):=GetProcAddress(hlib,'wasmtime_module_deserialize');
  pointer(wasmtime_module_deserialize_file):=GetProcAddress(hlib,'wasmtime_module_deserialize_file');
  pointer(wasmtime_store_new):=GetProcAddress(hlib,'wasmtime_store_new');
  pointer(wasmtime_store_context):=GetProcAddress(hlib,'wasmtime_store_context');
  pointer(wasmtime_store_delete):=GetProcAddress(hlib,'wasmtime_store_delete');
  pointer(wasmtime_context_get_data):=GetProcAddress(hlib,'wasmtime_context_get_data');
  pointer(wasmtime_context_set_data):=GetProcAddress(hlib,'wasmtime_context_set_data');
  pointer(wasmtime_context_gc):=GetProcAddress(hlib,'wasmtime_context_gc');
  pointer(wasmtime_context_add_fuel):=GetProcAddress(hlib,'wasmtime_context_add_fuel');
  pointer(wasmtime_context_fuel_consumed):=GetProcAddress(hlib,'wasmtime_context_fuel_consumed');
  pointer(wasmtime_context_consume_fuel):=GetProcAddress(hlib,'wasmtime_context_consume_fuel');
  pointer(wasmtime_context_set_wasi):=GetProcAddress(hlib,'wasmtime_context_set_wasi');
  pointer(wasmtime_interrupt_handle_new):=GetProcAddress(hlib,'wasmtime_interrupt_handle_new');
  pointer(wasmtime_interrupt_handle_interrupt):=GetProcAddress(hlib,'wasmtime_interrupt_handle_interrupt');
  pointer(wasmtime_interrupt_handle_delete):=GetProcAddress(hlib,'wasmtime_interrupt_handle_delete');
  pointer(wasmtime_extern_delete):=GetProcAddress(hlib,'wasmtime_extern_delete');
  pointer(wasmtime_extern_type):=GetProcAddress(hlib,'wasmtime_extern_type');
  pointer(wasmtime_externref_new):=GetProcAddress(hlib,'wasmtime_externref_new');
  pointer(wasmtime_externref_data):=GetProcAddress(hlib,'wasmtime_externref_data');
  pointer(wasmtime_externref_clone):=GetProcAddress(hlib,'wasmtime_externref_clone');
  pointer(wasmtime_externref_delete):=GetProcAddress(hlib,'wasmtime_externref_delete');
  pointer(wasmtime_externref_from_raw):=GetProcAddress(hlib,'wasmtime_externref_from_raw');
  pointer(wasmtime_externref_to_raw):=GetProcAddress(hlib,'wasmtime_externref_to_raw');
  pointer(wasmtime_val_delete):=GetProcAddress(hlib,'wasmtime_val_delete');
  pointer(wasmtime_val_copy):=GetProcAddress(hlib,'wasmtime_val_copy');
  pointer(wasmtime_func_new):=GetProcAddress(hlib,'wasmtime_func_new');
  pointer(wasmtime_func_new_unchecked):=GetProcAddress(hlib,'wasmtime_func_new_unchecked');
  pointer(wasmtime_func_type):=GetProcAddress(hlib,'wasmtime_func_type');
  pointer(wasmtime_func_call):=GetProcAddress(hlib,'wasmtime_func_call');
  pointer(wasmtime_func_call_unchecked):=GetProcAddress(hlib,'wasmtime_func_call_unchecked');
  pointer(wasmtime_caller_export_get):=GetProcAddress(hlib,'wasmtime_caller_export_get');
  pointer(wasmtime_caller_context):=GetProcAddress(hlib,'wasmtime_caller_context');
  pointer(wasmtime_func_from_raw):=GetProcAddress(hlib,'wasmtime_func_from_raw');
  pointer(wasmtime_func_to_raw):=GetProcAddress(hlib,'wasmtime_func_to_raw');
  pointer(wasmtime_global_new):=GetProcAddress(hlib,'wasmtime_global_new');
  pointer(wasmtime_global_type):=GetProcAddress(hlib,'wasmtime_global_type');
  pointer(wasmtime_global_get):=GetProcAddress(hlib,'wasmtime_global_get');
  pointer(wasmtime_global_set):=GetProcAddress(hlib,'wasmtime_global_set');
  pointer(wasmtime_instancetype_delete):=GetProcAddress(hlib,'wasmtime_instancetype_delete');
  pointer(wasmtime_instancetype_exports):=GetProcAddress(hlib,'wasmtime_instancetype_exports');
  pointer(wasmtime_instancetype_as_externtype):=GetProcAddress(hlib,'wasmtime_instancetype_as_externtype');
  pointer(wasmtime_externtype_as_instancetype):=GetProcAddress(hlib,'wasmtime_externtype_as_instancetype');
  pointer(wasmtime_instance_new):=GetProcAddress(hlib,'wasmtime_instance_new');
  pointer(wasmtime_instance_type):=GetProcAddress(hlib,'wasmtime_instance_type');
  pointer(wasmtime_instance_export_get):=GetProcAddress(hlib,'wasmtime_instance_export_get');
  pointer(wasmtime_instance_export_nth):=GetProcAddress(hlib,'wasmtime_instance_export_nth');
  pointer(wasmtime_linker_new):=GetProcAddress(hlib,'wasmtime_linker_new');
  pointer(wasmtime_linker_delete):=GetProcAddress(hlib,'wasmtime_linker_delete');
  pointer(wasmtime_linker_allow_shadowing):=GetProcAddress(hlib,'wasmtime_linker_allow_shadowing');
  pointer(wasmtime_linker_define):=GetProcAddress(hlib,'wasmtime_linker_define');
  pointer(wasmtime_linker_define_func):=GetProcAddress(hlib,'wasmtime_linker_define_func');
  pointer(wasmtime_linker_define_func_unchecked):=GetProcAddress(hlib,'wasmtime_linker_define_func_unchecked');
  pointer(wasmtime_linker_define_wasi):=GetProcAddress(hlib,'wasmtime_linker_define_wasi');
  pointer(wasmtime_linker_define_instance):=GetProcAddress(hlib,'wasmtime_linker_define_instance');
  pointer(wasmtime_linker_instantiate):=GetProcAddress(hlib,'wasmtime_linker_instantiate');
  pointer(wasmtime_linker_module):=GetProcAddress(hlib,'wasmtime_linker_module');
  pointer(wasmtime_linker_get_default):=GetProcAddress(hlib,'wasmtime_linker_get_default');
  pointer(wasmtime_linker_get):=GetProcAddress(hlib,'wasmtime_linker_get');
  pointer(wasmtime_memorytype_new):=GetProcAddress(hlib,'wasmtime_memorytype_new');
  pointer(wasmtime_memorytype_minimum):=GetProcAddress(hlib,'wasmtime_memorytype_minimum');
  pointer(wasmtime_memorytype_maximum):=GetProcAddress(hlib,'wasmtime_memorytype_maximum');
  pointer(wasmtime_memorytype_is64):=GetProcAddress(hlib,'wasmtime_memorytype_is64');
  pointer(wasmtime_memory_new):=GetProcAddress(hlib,'wasmtime_memory_new');
  pointer(wasmtime_memory_type):=GetProcAddress(hlib,'wasmtime_memory_type');
  pointer(wasmtime_memory_data):=GetProcAddress(hlib,'wasmtime_memory_data');
  pointer(wasmtime_memory_data_size):=GetProcAddress(hlib,'wasmtime_memory_data_size');
  pointer(wasmtime_memory_size):=GetProcAddress(hlib,'wasmtime_memory_size');
  pointer(wasmtime_memory_grow):=GetProcAddress(hlib,'wasmtime_memory_grow');
  pointer(wasmtime_table_new):=GetProcAddress(hlib,'wasmtime_table_new');
  pointer(wasmtime_table_type):=GetProcAddress(hlib,'wasmtime_table_type');
  pointer(wasmtime_table_get):=GetProcAddress(hlib,'wasmtime_table_get');
  pointer(wasmtime_table_set):=GetProcAddress(hlib,'wasmtime_table_set');
  pointer(wasmtime_table_size):=GetProcAddress(hlib,'wasmtime_table_size');
  pointer(wasmtime_table_grow):=GetProcAddress(hlib,'wasmtime_table_grow');
  pointer(wasmtime_trap_new):=GetProcAddress(hlib,'wasmtime_trap_new');
  pointer(wasmtime_trap_code):=GetProcAddress(hlib,'wasmtime_trap_code');
  pointer(wasmtime_trap_exit_status):=GetProcAddress(hlib,'wasmtime_trap_exit_status');
  pointer(wasmtime_frame_func_name):=GetProcAddress(hlib,'wasmtime_frame_func_name');
  pointer(wasmtime_frame_module_name):=GetProcAddress(hlib,'wasmtime_frame_module_name');
  pointer(wasmtime_wat2wasm):=GetProcAddress(hlib,'wasmtime_wat2wasm');
end;


end.
