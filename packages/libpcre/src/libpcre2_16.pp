{$mode objfpc}
{$h+}
{$IFNDEF FPC_DOTTEDUNITS}
unit libpcre2_16;
{$ENDIF}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.CTypes;
{$ELSE}
  ctypes;
{$ENDIF}

const
  {$IFDEF LINUX}
    pcrelibname = 'libpcre2-16.so.0';
  {$ELSE}
    {$IFDEF windows}
      pcrelibname = 'libpcre16-0.dll'; // As used in Mingw64
    {$ELSE}
      {$ERROR: platform not supported by pcre}
    {$ENDIF}
  {$ENDIF}

{$i pcreconsts.inc}

{
  Automatically converted by H2Pas 1.0.0 from pre_pcre_8.h
  The following command line parameters were used:
    -c
    -l
    libpcre2-8
    pre_pcre_8.h
    -C
    -o
    libpcre28.pp
    -P
    -P
    -t
}

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

Type
  tsize_t = csize_t;
  PCRE2_SIZE = tsize_t;
  PTsize_t = ^Tsize_t;
  PPTsize_t = ^PTsize_t;
  tuint32_t = cardinal;
  Tint32_t = cuint32;
  Tcint = cint;

  TPCRE2_UCHAR8 = ansichar;
  TPCRE2_SPTR8 = PAnsichar;
  PTPCRE2_UCHAR8 = ^TPCRE2_UCHAR8;
  PCRE2_SPTR8 = TPCRE2_SPTR8;
  PTPCRE2_SPTR8 = PCRE2_SPTR8;
  PPTPCRE2_UCHAR8 = ^PTPCRE2_UCHAR8;
  PPPTPCRE2_UCHAR8 = ^PPTPCRE2_UCHAR8;

  Ppcre2_callout_block_16  = ^Tpcre2_callout_block_16;
  Ppcre2_callout_block = Ppcre2_callout_block_16;

  Ppcre2_callout_enumerate_block_16  = ^Tpcre2_callout_enumerate_block_16;
  Ppcre2_callout_enumerate_block = Ppcre2_callout_enumerate_block_16;

  Ppcre2_code_16  = ^Tpcre2_code_16;
  Ppcre2_code = Ppcre2_code_16;

  Ppcre2_compile_context_16  = ^Tpcre2_compile_context_16;
  Ppcre2_compile_context = Ppcre2_compile_context_16;

  Ppcre2_convert_context_16  = ^Tpcre2_convert_context_16;
  Ppcre2_convert_context = Ppcre2_convert_context_16;

  Ppcre2_general_context_16  = ^Tpcre2_general_context_16;
  Ppcre2_general_context = Ppcre2_general_context_16;

  Ppcre2_jit_stack_16  = ^Tpcre2_jit_stack_16;
  Ppcre2_jit_stack = Ppcre2_jit_stack_16;

  Ppcre2_match_context_16  = ^Tpcre2_match_context_16;
  Ppcre2_match_context = Ppcre2_match_context_16;

  Ppcre2_match_data_16  = ^Tpcre2_match_data_16;
  Ppcre2_match_data = Ppcre2_match_data_16;

  TPCRE2_UCHAR16 = widechar;
  PPCRE2_SPTR8  = ^TPCRE2_SPTR8;
  Ppcre2_substitute_callout_block_16  = ^Tpcre2_substitute_callout_block_16;
  PPCRE2_UCHAR8  = ^TPCRE2_UCHAR8;
  PPCRE2_UCHAR16  = ^TPCRE2_UCHAR16;

  Psize_t  = ^tsize_t;
  Puint8_t  = ^byte;
  PTuint8_t = Puint8_t;
  PPTuint8_t = ^PTuint8_t;

  TPCRE2_SPTR16 = ^TPCRE2_UCHAR16;
  TPCRE2_SPTR = TPCRE2_SPTR16;

  PTPCRE2_UCHAR16 = ^TPCRE2_UCHAR16;
  PTPCRE2_UCHAR = PTPCRE2_UCHAR16;

  PCRE2_SPTR16 = TPCRE2_SPTR16;
  PCRE2_SPTR = PCRE2_SPTR16;

  PTPCRE2_SPTR16 = PCRE2_SPTR16;
  PTPCRE2_SPTR = PTPCRE2_SPTR16;

  PPTPCRE2_UCHAR16 = ^PTPCRE2_UCHAR16;
  PPTPCRE2_UCHAR = PPTPCRE2_UCHAR16;

  PPPTPCRE2_UCHAR16 = ^PPTPCRE2_UCHAR16;
  PPPTPCRE2_UCHAR = PPPTPCRE2_UCHAR16;

  Tpcre2_real_general_context_16 = record
  end;
  Tpcre2_general_context_16 =  Tpcre2_real_general_context_16;
  Tpcre2_general_context = Tpcre2_general_context_16;

  Ppcre2_real_general_context_16 = ^tpcre2_real_general_context_16;
  PTpcre2_general_context_16 = Ppcre2_real_general_context_16;

  Tpcre2_real_compile_context_16 = record
  end;
  Tpcre2_compile_context_16 = Tpcre2_real_compile_context_16;
  Tpcre2_compile_context = Tpcre2_compile_context_16;

  Ppcre2_real_compile_context_16 = ^Tpcre2_real_compile_context_16;
  PTpcre2_compile_context_16 = Ppcre2_real_compile_context_16;

  Tpcre2_real_match_context_16 = record
  end;
  Tpcre2_match_context_16 = Tpcre2_real_match_context_16;
  Tpcre2_match_context = Tpcre2_match_context_16;

  Ppcre2_real_match_context_16 = ^Tpcre2_real_match_context_16;
  PTpcre2_match_context_16 = Ppcre2_real_match_context_16;

  Tpcre2_real_convert_context_16 = record
  end;
  Tpcre2_convert_context_16 = Tpcre2_real_convert_context_16;
  Tpcre2_convert_context = Tpcre2_convert_context_16;

  Ppcre2_real_convert_context_16 = ^Tpcre2_real_convert_context_16;
  PTpcre2_convert_context_16 = Ppcre2_real_convert_context_16;

  Tpcre2_real_code_16 = record
  end;
  Tpcre2_code_16 = Tpcre2_real_code_16;
  Tpcre2_code = Tpcre2_code_16;

  Ppcre2_real_code_16 = ^Tpcre2_real_code_16;
  PTpcre2_code_16  = Ppcre2_real_code_16;
  PPTpcre2_code_16 = ^PTpcre2_code_16;


  Tpcre2_real_match_data_16 = record
  end;
  Tpcre2_match_data_16 = Tpcre2_real_match_data_16;
  Tpcre2_match_data = Tpcre2_match_data_16;

  Ppcre2_real_match_data_16 = ^Tpcre2_real_match_data_16;
  PTpcre2_match_data_16 = Ppcre2_real_match_data_16;



  Tpcre2_real_jit_stack_16 = record
  end;
  Tpcre2_jit_stack_16 = Tpcre2_real_jit_stack_16;
  Tpcre2_jit_stack = Tpcre2_jit_stack_16;
  
  Ppcre2_real_jit_stack_16 = ^Tpcre2_real_jit_stack_16;
  PTpcre2_jit_stack_16 = Ppcre2_real_jit_stack_16;


  Tpcre2_jit_callback_16 = function (_para1:pointer): PTpcre2_jit_stack_16;cdecl;

  Tpcre2_callout_block_16 = record
    version : Tuint32_t;
    callout_number : Tuint32_t;
    capture_top : Tuint32_t;
    capture_last : Tuint32_t;
    offset_vector : ^Tsize_t;
    mark : TPCRE2_SPTR8;
    subject : TPCRE2_SPTR8;
    subject_length : Tsize_t;
    start_match : Tsize_t;
    current_position : Tsize_t;
    pattern_position : Tsize_t;
    next_item_length : Tsize_t;
    callout_string_offset : Tsize_t;
    callout_string_length : Tsize_t;
    callout_string : TPCRE2_SPTR8;
    callout_flags : Tuint32_t;
  end;


  Tpcre2_callout_enumerate_block_16 = record
    version : Tuint32_t;
    pattern_position : Tsize_t;
    next_item_length : Tsize_t;
    callout_number : Tuint32_t;
    callout_string_offset : Tsize_t;
    callout_string_length : Tsize_t;
    callout_string : TPCRE2_SPTR8;
  end;
  PTpcre2_callout_enumerate_block_16 = ^Tpcre2_callout_enumerate_block_16;

  Tpcre2_substitute_callout_block_16 = record
    version : Tuint32_t;
    input : TPCRE2_SPTR8;
    output : TPCRE2_SPTR8;
    output_offsets : array[0..1] of Tsize_t;
    ovector : ^Tsize_t;
    oveccount : Tuint32_t;
    subscount : Tuint32_t;

  end;
  PTpcre2_substitute_callout_block_16 = ^  Tpcre2_substitute_callout_block_16;

  tpcre2_malloc = function (_para1:Tsize_t; _para2:pointer) : Pointer; cdecl;
  tpcre2_free = procedure (_para1:pointer; _para2:pointer); cdecl;
  tpcre2_set_compile_recursion_guard_16_callback = function (_para1:Tuint32_t; _para2:pointer):Tcint; cdecl;
  tpcre2_set_callout_16_callback = function (_para1:Ppcre2_callout_block_16; _para2:pointer):Tcint; cdecl;
  tpcre2_callout_enumerate_16_callback = function (_para1: PTpcre2_callout_enumerate_block_16; _para2:pointer):Tcint; cdecl;
  tpcre2_set_substitute_callout_16_callback =   function (_para1:Ppcre2_substitute_callout_block_16; _para2:pointer):Tcint; cdecl;
     
var
  pcre2_config : function(_para1:Tuint32_t; _para2:pointer):Tcint;cdecl;
  pcre2_general_context_copy : function(_para1:PTpcre2_general_context_16): PTpcre2_general_context_16;cdecl;
  pcre2_general_context_create : function(_para1:tpcre2_malloc; _para2:tpcre2_free; _para3:pointer):PTpcre2_general_context_16;cdecl;
  pcre2_general_context_free : procedure(_para1:PTpcre2_general_context_16);cdecl;
  pcre2_compile_context_copy : function(_para1:PTpcre2_compile_context_16):PTpcre2_compile_context_16;cdecl;
  pcre2_compile_context_create : function(_para1:PTpcre2_general_context_16):PTpcre2_compile_context_16;cdecl;
  pcre2_compile_context_free : procedure(_para1:PTpcre2_compile_context_16);cdecl;
  pcre2_set_bsr : function(_para1:PTpcre2_compile_context_16; _para2:Tuint32_t):Tcint;cdecl;
  pcre2_set_character_tables : function(_para1:PTpcre2_compile_context_16; _para2: Puint8_t):Tcint;cdecl;
  pcre2_set_compile_extra_options : function(_para1:PTpcre2_compile_context_16; _para2:Tuint32_t):Tcint;cdecl;
  pcre2_set_max_pattern_length : function(_para1:PTpcre2_compile_context_16; _para2:Tsize_t):Tcint;cdecl;
  pcre2_set_newline : function(_para1:PTpcre2_compile_context_16; _para2:Tuint32_t):Tcint;cdecl;
  pcre2_set_parens_nest_limit : function(_para1:PTpcre2_compile_context_16; _para2:Tuint32_t):Tcint;cdecl;
  pcre2_set_compile_recursion_guard : function(_para1:PTpcre2_compile_context_16; _para2: tpcre2_set_compile_recursion_guard_16_callback; _para3:pointer):Tcint;cdecl;
  pcre2_convert_context_copy : function(_para1:PTpcre2_convert_context_16):PTpcre2_convert_context_16;cdecl;
  pcre2_convert_context_create : function(_para1:PTpcre2_general_context_16):PTpcre2_convert_context_16;cdecl;
  pcre2_convert_context_free : procedure(_para1:PTpcre2_convert_context_16);cdecl;
  pcre2_set_glob_escape : function(_para1:PTpcre2_convert_context_16; _para2:Tuint32_t):Tcint;cdecl;
  pcre2_set_glob_separator : function(_para1:PTpcre2_convert_context_16; _para2:Tuint32_t):Tcint;cdecl;
  pcre2_pattern_convert : function(_para1:TPCRE2_SPTR8; _para2:Tsize_t; _para3:Tuint32_t; _para4:PPTPCRE2_UCHAR8; _para5: Psize_t;  _para6:PTpcre2_convert_context_16):Tcint;cdecl;
  pcre2_converted_pattern_free : procedure(_para1:PTPCRE2_UCHAR8);cdecl;
  pcre2_match_context_copy : function(_para1:PTpcre2_match_context_16):PTpcre2_match_context_16;cdecl;
  pcre2_match_context_create : function(_para1:PTpcre2_general_context_16):PTpcre2_match_context_16;cdecl;
  pcre2_match_context_free : procedure(_para1:PTpcre2_match_context_16);cdecl;
  pcre2_set_callout : function(_para1:PTpcre2_match_context_16; _para2: tpcre2_set_callout_16_callback; _para3:pointer):Tcint;cdecl;
  pcre2_set_substitute_callout : function(_para1:PTpcre2_match_context_16; _para2:tpcre2_set_substitute_callout_16_callback; _para3:pointer):Tcint;cdecl;
  pcre2_set_depth_limit : function(_para1:PTpcre2_match_context_16; _para2:Tuint32_t):Tcint;cdecl;
  pcre2_set_heap_limit : function(_para1:PTpcre2_match_context_16; _para2:Tuint32_t):Tcint;cdecl;
  pcre2_set_match_limit : function(_para1:PTpcre2_match_context_16; _para2:Tuint32_t):Tcint;cdecl;
  pcre2_set_offset_limit : function(_para1:PTpcre2_match_context_16; _para2:Tsize_t):Tcint;cdecl;
  pcre2_set_recursion_limit : function(_para1:PTpcre2_match_context_16; _para2:Tuint32_t):Tcint;cdecl;
  pcre2_set_recursion_memory_management : function(_para1:PTpcre2_match_context_16; _para2: tpcre2_malloc; _para3:tpcre2_free; _para4:pointer):Tcint;cdecl;
  pcre2_compile : function(_para1:TPCRE2_SPTR8; _para2:Tsize_t; _para3:Tuint32_t; _para4:Pcint; _para5:Psize_t; _para6:PTpcre2_compile_context_16):Ppcre2_code_16;cdecl;
  // PCRE actually uses TPCRE2_SPTR8. With this we avoid a typecast
  pcre2_compile_w : function(_para1:TPCRE2_SPTR16; _para2:Tsize_t; _para3:Tuint32_t; _para4:Pcint; _para5:Psize_t; _para6:PTpcre2_compile_context_16):Ppcre2_code_16;cdecl;
  pcre2_code_free : procedure(_para1:PTpcre2_code_16);cdecl;
  pcre2_code_copy : function(_para1:PTpcre2_code_16):PTpcre2_code_16;cdecl;
  pcre2_code_copy_with_tables : function(_para1:PTpcre2_code_16):PTpcre2_code_16;cdecl;
  pcre2_pattern_info : function(_para1:PTpcre2_code_16; _para2:Tuint32_t; _para3:pointer):Tcint;cdecl;
  pcre2_callout_enumerate : function(_para1:PTpcre2_code_16; _para2: tpcre2_callout_enumerate_16_callback; _para3:pointer):Tcint;cdecl;
  pcre2_match_data_create : function(_para1:Tuint32_t; _para2:PTpcre2_general_context_16):PTpcre2_match_data_16;cdecl;
  pcre2_match_data_create_from_pattern : function(_para1:PTpcre2_code_16; _para2:PTpcre2_general_context_16):PTpcre2_match_data_16;cdecl;
  pcre2_dfa_match : function(_para1:PTpcre2_code_16; _para2:TPCRE2_SPTR8; _para3:Tsize_t; _para4:Tsize_t; _para5:Tuint32_t; _para6:PTpcre2_match_data_16; _para7:PTpcre2_match_context_16; _para8:Pcint; _para9:Tsize_t):Tcint;cdecl;
  // PCRE actually uses TPCRE2_SPTR8. With this we avoid a typecast
  pcre2_dfa_match_w : function(_para1:PTpcre2_code_16; _para2:TPCRE2_SPTR16; _para3:Tsize_t; _para4:Tsize_t; _para5:Tuint32_t; _para6:PTpcre2_match_data_16; _para7:PTpcre2_match_context_16; _para8:Pcint; _para9:Tsize_t):Tcint;cdecl;
  pcre2_match : function(_para1:PTpcre2_code_16; _para2:TPCRE2_SPTR8; _para3:Tsize_t; _para4:Tsize_t; _para5:Tuint32_t; _para6:PTpcre2_match_data_16; _para7:PTpcre2_match_context_16):Tcint;cdecl;
  // PCRE actually uses TPCRE2_SPTR8. With this we avoid a typecast
  pcre2_match_w : function(_para1:PTpcre2_code_16; _para2:TPCRE2_SPTR16; _para3:Tsize_t; _para4:Tsize_t; _para5:Tuint32_t; _para6:PTpcre2_match_data_16; _para7:PTpcre2_match_context_16):Tcint;cdecl;
  pcre2_match_data_free : procedure(_para1:PTpcre2_match_data_16);cdecl;
  pcre2_get_mark : function(_para1:PTpcre2_match_data_16):TPCRE2_SPTR8;cdecl;
  pcre2_get_match_data_size : function(_para1:PTpcre2_match_data_16):Tsize_t;cdecl;
  pcre2_get_ovector_count : function(_para1:PTpcre2_match_data_16):Tuint32_t;cdecl;
  pcre2_get_ovector_pointer : function(_para1:PTpcre2_match_data_16):Psize_t;cdecl;
  pcre2_get_startchar : function(_para1:PTpcre2_match_data_16):Tsize_t;cdecl;
  pcre2_substring_copy_byname : function(_para1:PTpcre2_match_data_16; _para2:TPCRE2_SPTR8; _para3:PTPCRE2_UCHAR8; _para4:PTsize_t):Tcint;cdecl;
  pcre2_substring_copy_bynumber : function(_para1:PTpcre2_match_data_16; _para2:Tuint32_t; _para3:PTPCRE2_UCHAR8; _para4:PTsize_t):Tcint;cdecl;
  pcre2_substring_free : procedure(_para1:PTPCRE2_UCHAR8);cdecl;
  pcre2_substring_get_byname : function(_para1:PTpcre2_match_data_16; _para2:TPCRE2_SPTR8; _para3:PPTPCRE2_UCHAR8; _para4:PTsize_t):Tcint;cdecl;
  pcre2_substring_get_bynumber : function(_para1:PTpcre2_match_data_16; _para2:Tuint32_t; _para3:PPTPCRE2_UCHAR8; _para4:PTsize_t):Tcint;cdecl;
  pcre2_substring_length_byname : function(_para1:PTpcre2_match_data_16; _para2:TPCRE2_SPTR8; _para3:PTsize_t):Tcint;cdecl;
  pcre2_substring_length_bynumber : function(_para1:PTpcre2_match_data_16; _para2:Tuint32_t; _para3:PTsize_t):Tcint;cdecl;
  pcre2_substring_nametable_scan : function(_para1:PTpcre2_code_16; _para2:TPCRE2_SPTR8; _para3:PTPCRE2_SPTR8; _para4:PTPCRE2_SPTR8):Tcint;cdecl;
  pcre2_substring_number_from_name : function(_para1:PTpcre2_code_16; _para2:TPCRE2_SPTR8):Tcint;cdecl;
  pcre2_substring_list_free : procedure(_para1:PTPCRE2_SPTR8);cdecl;
  pcre2_substring_list_get : function(_para1:PTpcre2_match_data_16; _para2:PPPTPCRE2_UCHAR8; _para3:PPTsize_t):Tcint;cdecl;
  pcre2_serialize_encode : function(_para1:PPTpcre2_code_16; _para2:Tint32_t; _para3:PPTuint8_t; _para4:PTsize_t; _para5:PTpcre2_general_context_16):Tint32_t;cdecl;
  pcre2_serialize_decode : function(_para1:PPTpcre2_code_16; _para2:Tint32_t; _para3:PTuint8_t; _para4:PTpcre2_general_context_16):Tint32_t;cdecl;
  pcre2_serialize_get_number_of_codes : function(_para1:PTuint8_t):Tint32_t;cdecl;
  pcre2_serialize_free : procedure(_para1:PTuint8_t);cdecl;
  pcre2_substitute : function(_para1:PTpcre2_code_16; _para2:TPCRE2_SPTR8; _para3:Tsize_t; _para4:Tsize_t; _para5:Tuint32_t; _para6:PTpcre2_match_data_16; _para7:PTpcre2_match_context_16; _para8:TPCRE2_SPTR8; _para9:Tsize_t; _para10:PTPCRE2_UCHAR8;  _para11:PTsize_t):Tcint;cdecl;
  pcre2_jit_compile : function(_para1:PTpcre2_code_16; _para2:Tuint32_t):Tcint;cdecl;
  pcre2_jit_match : function(_para1:PTpcre2_code_16; _para2:TPCRE2_SPTR8; _para3:Tsize_t; _para4:Tsize_t; _para5:Tuint32_t; _para6:PTpcre2_match_data_16; _para7:PTpcre2_match_context_16):Tcint;cdecl;
  pcre2_jit_free_unused_memory : procedure(_para1:PTpcre2_general_context_16);cdecl;
  pcre2_jit_stack_create : function(_para1:Tsize_t; _para2:Tsize_t; _para3:PTpcre2_general_context_16):PTpcre2_jit_stack_16;cdecl;
  pcre2_jit_stack_assign : procedure(_para1:PTpcre2_match_context_16; _para2:Tpcre2_jit_callback_16; _para3:pointer);cdecl;
  pcre2_jit_stack_free : procedure(_para1:PTpcre2_jit_stack_16);cdecl;
  pcre2_get_error_message : function(_para1:Tcint; _para2:PTPCRE2_UCHAR8; _para3:Tsize_t):Tcint;cdecl;
  pcre2_maketables : function(_para1:PTpcre2_general_context_16):PTuint8_t;cdecl;
  pcre2_maketables_free : procedure(_para1:PTpcre2_general_context_16; _para2:PTuint8_t);cdecl;

function libpcre28loaded : Boolean;
procedure Loadlibpcre28; overload;
procedure Loadlibpcre28(const lib : string); overload;
procedure Freelibpcre28;

implementation

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils, System.DynLibs;
{$ELSE}
  SysUtils, dynlibs;
{$ENDIF}


var
  hlib : tlibhandle;


function libpcre28loaded : Boolean;
begin
  Result:=(hLib<>NilHandle);
end;

procedure Freelibpcre28;

begin
  if hlib=nilhandle then exit;
  FreeLibrary(hlib);
  hlib:=NilHandle;
  pcre2_config:=nil;
  pcre2_general_context_copy:=nil;
  pcre2_general_context_create:=nil;
  pcre2_general_context_free:=nil;
  pcre2_compile_context_copy:=nil;
  pcre2_compile_context_create:=nil;
  pcre2_compile_context_free:=nil;
  pcre2_set_bsr:=nil;
  pcre2_set_character_tables:=nil;
  pcre2_set_compile_extra_options:=nil;
  pcre2_set_max_pattern_length:=nil;
  pcre2_set_newline:=nil;
  pcre2_set_parens_nest_limit:=nil;
  pcre2_set_compile_recursion_guard:=nil;
  pcre2_convert_context_copy:=nil;
  pcre2_convert_context_create:=nil;
  pcre2_convert_context_free:=nil;
  pcre2_set_glob_escape:=nil;
  pcre2_set_glob_separator:=nil;
  pcre2_pattern_convert:=nil;
  pcre2_converted_pattern_free:=nil;
  pcre2_match_context_copy:=nil;
  pcre2_match_context_create:=nil;
  pcre2_match_context_free:=nil;
  pcre2_set_callout:=nil;
  pcre2_set_substitute_callout:=nil;
  pcre2_set_depth_limit:=nil;
  pcre2_set_heap_limit:=nil;
  pcre2_set_match_limit:=nil;
  pcre2_set_offset_limit:=nil;
  pcre2_set_recursion_limit:=nil;
  pcre2_set_recursion_memory_management:=nil;
  pcre2_compile:=nil;
  pcre2_compile_w:=Nil;
  pcre2_code_free:=nil;
  pcre2_code_copy:=nil;
  pcre2_code_copy_with_tables:=nil;
  pcre2_pattern_info:=nil;
  pcre2_callout_enumerate:=nil;
  pcre2_match_data_create:=nil;
  pcre2_match_data_create_from_pattern:=nil;
  pcre2_dfa_match:=nil;
  pcre2_dfa_match_w:=nil;
  pcre2_match:=nil;
  pcre2_match_w:=nil;
  pcre2_match_data_free:=nil;
  pcre2_get_mark:=nil;
  pcre2_get_match_data_size:=nil;
  pcre2_get_ovector_count:=nil;
  pcre2_get_ovector_pointer:=nil;
  pcre2_get_startchar:=nil;
  pcre2_substring_copy_byname:=nil;
  pcre2_substring_copy_bynumber:=nil;
  pcre2_substring_free:=nil;
  pcre2_substring_get_byname:=nil;
  pcre2_substring_get_bynumber:=nil;
  pcre2_substring_length_byname:=nil;
  pcre2_substring_length_bynumber:=nil;
  pcre2_substring_nametable_scan:=nil;
  pcre2_substring_number_from_name:=nil;
  pcre2_substring_list_free:=nil;
  pcre2_substring_list_get:=nil;
  pcre2_serialize_encode:=nil;
  pcre2_serialize_decode:=nil;
  pcre2_serialize_get_number_of_codes:=nil;
  pcre2_serialize_free:=nil;
  pcre2_substitute:=nil;
  pcre2_jit_compile:=nil;
  pcre2_jit_match:=nil;
  pcre2_jit_free_unused_memory:=nil;
  pcre2_jit_stack_create:=nil;
  pcre2_jit_stack_assign:=nil;
  pcre2_jit_stack_free:=nil;
  pcre2_get_error_message:=nil;
  pcre2_maketables:=nil;
  pcre2_maketables_free:=nil;
end;


procedure Loadlibpcre28;
begin
  Loadlibpcre28(pcrelibname);
end;


procedure Loadlibpcre28(const lib : string);

  Function GetAddr(const aName : string) : Pointer;

  begin
    Result:=GetProcAddress(hlib,aname);
    if Result=Nil then
      if IsConsole then
        Writeln(stdErr,'Could not load procedure: ',aName)
  end;

begin
  Freelibpcre28;
  hlib:=LoadLibrary(lib);
  if (hlib=NilHandle) then
    raise Exception.Create(format('Could not load library: %s',[lib]));
  pointer(pcre2_config):=GetAddr('pcre2_config_16');
  pointer(pcre2_general_context_copy):=GetAddr('pcre2_general_context_copy_16');
  pointer(pcre2_general_context_create):=GetAddr('pcre2_general_context_create_16');
  pointer(pcre2_general_context_free):=GetAddr('pcre2_general_context_free_16');
  pointer(pcre2_compile_context_copy):=GetAddr('pcre2_compile_context_copy_16');
  pointer(pcre2_compile_context_create):=GetAddr('pcre2_compile_context_create_16');
  pointer(pcre2_compile_context_free):=GetAddr('pcre2_compile_context_free_16');
  pointer(pcre2_set_bsr):=GetAddr('pcre2_set_bsr_16');
  pointer(pcre2_set_character_tables):=GetAddr('pcre2_set_character_tables_16');
  pointer(pcre2_set_compile_extra_options):=GetAddr('pcre2_set_compile_extra_options_16');
  pointer(pcre2_set_max_pattern_length):=GetAddr('pcre2_set_max_pattern_length_16');
  pointer(pcre2_set_newline):=GetAddr('pcre2_set_newline_16');
  pointer(pcre2_set_parens_nest_limit):=GetAddr('pcre2_set_parens_nest_limit_16');
  pointer(pcre2_set_compile_recursion_guard):=GetAddr('pcre2_set_compile_recursion_guard_16');
  pointer(pcre2_convert_context_copy):=GetAddr('pcre2_convert_context_copy_16');
  pointer(pcre2_convert_context_create):=GetAddr('pcre2_convert_context_create_16');
  pointer(pcre2_convert_context_free):=GetAddr('pcre2_convert_context_free_16');
  pointer(pcre2_set_glob_escape):=GetAddr('pcre2_set_glob_escape_16');
  pointer(pcre2_set_glob_separator):=GetAddr('pcre2_set_glob_separator_16');
  pointer(pcre2_pattern_convert):=GetAddr('pcre2_pattern_convert_16');
  pointer(pcre2_converted_pattern_free):=GetAddr('pcre2_converted_pattern_free_16');
  pointer(pcre2_match_context_copy):=GetAddr('pcre2_match_context_copy_16');
  pointer(pcre2_match_context_create):=GetAddr('pcre2_match_context_create_16');
  pointer(pcre2_match_context_free):=GetAddr('pcre2_match_context_free_16');
  pointer(pcre2_set_callout):=GetAddr('pcre2_set_callout_16');
  pointer(pcre2_set_substitute_callout):=GetAddr('pcre2_set_substitute_callout_16');
  pointer(pcre2_set_depth_limit):=GetAddr('pcre2_set_depth_limit_16');
  pointer(pcre2_set_heap_limit):=GetAddr('pcre2_set_heap_limit_16');
  pointer(pcre2_set_match_limit):=GetAddr('pcre2_set_match_limit_16');
  pointer(pcre2_set_offset_limit):=GetAddr('pcre2_set_offset_limit_16');
  pointer(pcre2_set_recursion_limit):=GetAddr('pcre2_set_recursion_limit_16');
  pointer(pcre2_set_recursion_memory_management):=GetAddr('pcre2_set_recursion_memory_management_16');
  pointer(pcre2_compile):=GetAddr('pcre2_compile_16');
  pointer(pcre2_compile_w):=GetAddr('pcre2_compile_16');
  pointer(pcre2_code_free):=GetAddr('pcre2_code_free_16');
  pointer(pcre2_code_copy):=GetAddr('pcre2_code_copy_16');
  pointer(pcre2_code_copy_with_tables):=GetAddr('pcre2_code_copy_with_tables_16');
  pointer(pcre2_pattern_info):=GetAddr('pcre2_pattern_info_16');
  pointer(pcre2_callout_enumerate):=GetAddr('pcre2_callout_enumerate_16');
  pointer(pcre2_match_data_create):=GetAddr('pcre2_match_data_create_16');
  pointer(pcre2_match_data_create_from_pattern):=GetAddr('pcre2_match_data_create_from_pattern_16');
  pointer(pcre2_dfa_match):=GetAddr('pcre2_dfa_match_16');
  pointer(pcre2_dfa_match_w):=GetAddr('pcre2_dfa_match_16');
  pointer(pcre2_match):=GetAddr('pcre2_match_16');
  pointer(pcre2_match_w):=GetAddr('pcre2_match_16');
  pointer(pcre2_match_data_free):=GetAddr('pcre2_match_data_free_16');
  pointer(pcre2_get_mark):=GetAddr('pcre2_get_mark_16');
  pointer(pcre2_get_match_data_size):=GetAddr('pcre2_get_match_data_size_16');
  pointer(pcre2_get_ovector_count):=GetAddr('pcre2_get_ovector_count_16');
  pointer(pcre2_get_ovector_pointer):=GetAddr('pcre2_get_ovector_pointer_16');
  pointer(pcre2_get_startchar):=GetAddr('pcre2_get_startchar_16');
  pointer(pcre2_substring_copy_byname):=GetAddr('pcre2_substring_copy_byname_16');
  pointer(pcre2_substring_copy_bynumber):=GetAddr('pcre2_substring_copy_bynumber_16');
  pointer(pcre2_substring_free):=GetAddr('pcre2_substring_free_16');
  pointer(pcre2_substring_get_byname):=GetAddr('pcre2_substring_get_byname_16');
  pointer(pcre2_substring_get_bynumber):=GetAddr('pcre2_substring_get_bynumber_16');
  pointer(pcre2_substring_length_byname):=GetAddr('pcre2_substring_length_byname_16');
  pointer(pcre2_substring_length_bynumber):=GetAddr('pcre2_substring_length_bynumber_16');
  pointer(pcre2_substring_nametable_scan):=GetAddr('pcre2_substring_nametable_scan_16');
  pointer(pcre2_substring_number_from_name):=GetAddr('pcre2_substring_number_from_name_16');
  pointer(pcre2_substring_list_free):=GetAddr('pcre2_substring_list_free_16');
  pointer(pcre2_substring_list_get):=GetAddr('pcre2_substring_list_get_16');
  pointer(pcre2_serialize_encode):=GetAddr('pcre2_serialize_encode_16');
  pointer(pcre2_serialize_decode):=GetAddr('pcre2_serialize_decode_16');
  pointer(pcre2_serialize_get_number_of_codes):=GetAddr('pcre2_serialize_get_number_of_codes_16');
  pointer(pcre2_serialize_free):=GetAddr('pcre2_serialize_free_16');
  pointer(pcre2_substitute):=GetAddr('pcre2_substitute_16');
  pointer(pcre2_jit_compile):=GetAddr('pcre2_jit_compile_16');
  pointer(pcre2_jit_match):=GetAddr('pcre2_jit_match_16');
  pointer(pcre2_jit_free_unused_memory):=GetAddr('pcre2_jit_free_unused_memory_16');
  pointer(pcre2_jit_stack_create):=GetAddr('pcre2_jit_stack_create_16');
  pointer(pcre2_jit_stack_assign):=GetAddr('pcre2_jit_stack_assign_16');
  pointer(pcre2_jit_stack_free):=GetAddr('pcre2_jit_stack_free_16');
  pointer(pcre2_get_error_message):=GetAddr('pcre2_get_error_message_16');
  pointer(pcre2_maketables):=GetAddr('pcre2_maketables_16');
  pointer(pcre2_maketables_free):=GetAddr('pcre2_maketables_free_16');
end;


initialization
  Loadlibpcre28;
finalization
  Freelibpcre28;

end.
