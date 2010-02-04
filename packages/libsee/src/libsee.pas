{ $define libseehelper}

{$mode objfpc}
{$h+}
unit libsee;
interface

uses
  ctypes;

{
  Automatically converted by H2Pas 1.0.0 from libsee.c
  The following command line parameters were used:
    -D
    -l
    see
    -C
    -u
    libsee
    -T
    -o
    libsee.pas
    -P
    libsee.c
}

  const
{$ifdef unix}
    LibSeeLibraryName='libsee.so';
{$else}
    LibSeeLibraryName='libsee.dll';
{$endif}

Type
  Tsize_t = csize_t;
  tcuchar = char;
  Tuint16_t = word;
  Tuint32_t = cardinal;
  Tuint64_t = qword;
  Tint16_t = integer;
  Tint32_t = longint;
  Tint64_t = int64;
  TDouble = double;  
  tcuint = word;
  tcint = integer;
  PTcchar = pchar;
  PPTcchar = ^PTcchar;
  Ptcint = ^tcint;
  PTcuint = ^tcuint;
  tcchar = char;

var
  SEE_literal_NaN : array[0..7] of Tcuchar;cvar;external;

  Type
  PFILE  = Pointer;
  PTFILE = Pointer;
  PPSEE_string  = ^PSEE_string;
  PSEE_context  = ^TSEE_context;
  PSEE_enum  = ^TSEE_enum;
  PSEE_growable  = ^TSEE_growable;
  PSEE_input  = ^TSEE_input;
  PSEE_interpreter  = ^TSEE_interpreter;
  PSEE_module  = ^TSEE_module;
  PSEE_native  = ^TSEE_native;
  PSEE_object  = ^TSEE_object;
  PSEE_objectclass  = ^TSEE_objectclass;
  PSEE_scope  = ^TSEE_scope;
  PSEE_string  = ^TSEE_string;
  PSEE_throw_location  = ^TSEE_throw_location;
  PSEE_try_context  = ^TSEE_try_context;
  PSEE_unicode_t  = ^TSEE_unicode_t;
  PSEE_value  = ^TSEE_value;
  PTSEE_char_t = ^TSEE_Char_t;
  PTSEE_Code = Pointer;
  
  PTSEE_context  = ^TSEE_context;
  PTSEE_enum  = ^TSEE_enum;
  PTSEE_growable  = ^TSEE_growable;
  PTSEE_input  = ^TSEE_input;
  PTSEE_interpreter  = ^TSEE_interpreter;
  PTSEE_module  = ^TSEE_module;
  PTSEE_native  = ^TSEE_native;
  PTSEE_object  = ^TSEE_object;
  PTSEE_objectclass  = ^TSEE_objectclass;
  PTSEE_scope  = ^TSEE_scope;
  PTSEE_string  = ^TSEE_string;
  TPSEE_string = PTSEE_string;
  PTSEE_throw_location  = ^TSEE_throw_location;
  PTSEE_try_context  = ^TSEE_try_context;
  PTSEE_unicode_t  = ^TSEE_unicode_t;
  PTSEE_value  = ^TSEE_value;
  PPTSEE_value  = ^PSEE_value;
  PTSEE_enumclass = ^TSEE_enumclass;
  PSEE_system = ^TSEE_System;
//  PTSEE_property = ^TSEE_property;
  PTSEE_property = Pointer;
  TPSEE_object = ^TSEE_object;
  PTSEE_inputclass = ^TSEE_inputclass; 
  PTSEE_traceback = ^TSEE_traceback;
  PTSEE_regex_engine = Pointer;
  PTSEE_stringclass = ^TSEE_stringclass;
  TPSEE_enum = PSEE_Enum;
  TPSEE_input = PSEE_INPUT;
  PTPSEE_string = ^TSEE_STRING;
  TPSEE_interpreter_state = Pointer; //^TSEE_interpreter_state;
{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

    TSEE_uint16_t = Tuint16_t;
    TSEE_uint32_t = Tuint32_t;
    TSEE_int32_t = Tint32_t;
    TSEE_uint64_t = Tuint64_t;
    TSEE_int64_t = Tint64_t;
    TSEE_number_t = Tdouble;
    TSEE_boolean_t = Tcuchar;
    TSEE_char_t = TSEE_uint16_t;
    TSEE_unicode_t = TSEE_uint32_t;

    TSEE_type = (SEE_UNDEFINED,SEE_NULL,SEE_BOOLEAN,SEE_NUMBER,
      SEE_STRING,SEE_OBJECT,SEE_REFERENCE,SEE_COMPLETION
      );

    TSEE_reference = record
        base : PTSEE_object;
        _property : PTSEE_string;
      end;

    TSEE_completion = record
        value : PTSEE_value;
        target : Tcuint;
        _type : (SEE_COMPLETION_NORMAL,SEE_COMPLETION_BREAK,
          SEE_COMPLETION_CONTINUE,SEE_COMPLETION_RETURN,
          SEE_COMPLETION_THROW);
      end;

    TSEE_value = record
        _type : TSEE_type;
        u : record
            case longint of
              0 : ( number : TSEE_number_t );
              1 : ( boolean : TSEE_boolean_t );
              2 : ( _object : PTSEE_object );
              3 : ( _string : PTSEE_string );
              4 : ( reference : TSEE_reference );
              5 : ( completion : TSEE_completion );
              6 : ( _padding : array[0..3] of pointer );
            end;
      end;



    TSEE_get_fn_t = procedure (i:PTSEE_interpreter; obj:PTSEE_object; prop:PTSEE_string; res:PTSEE_value);cdecl;
    TSEE_put_fn_t = procedure (i:PTSEE_interpreter; obj:PTSEE_object; prop:PTSEE_string; res:PTSEE_value; flags:Tcint);cdecl;
    TSEE_boolean_fn_t = function (i:PTSEE_interpreter; obj:PTSEE_object; prop:PTSEE_string):Tcint;cdecl;
    TSEE_hasinstance_fn_t = function (i:PTSEE_interpreter; obj:PTSEE_object; instance:PTSEE_value):Tcint;cdecl;
    TSEE_default_fn_t = procedure (i:PTSEE_interpreter; obj:PTSEE_object; hint:PTSEE_value; res:PTSEE_value);cdecl;
    TSEE_call_fn_t = procedure (i:PTSEE_interpreter; obj:PTSEE_object; thisobj:PTSEE_object; argc:Tcint; argv:PPTSEE_value; 
                 res:PTSEE_value);cdecl;
    TSEE_enumerator_fn_t = function (i:PTSEE_interpreter; obj:PTSEE_object):PTSEE_enum;cdecl;
    TSEE_get_sec_domain_fn_t = function (i:PTSEE_interpreter; obj:PTSEE_object):pointer;cdecl;


(* Const before type ignored *)
    TSEE_objectclass = record
        _Class : PTcchar;
        Get : TSEE_get_fn_t;
        Put : TSEE_put_fn_t;
        CanPut : TSEE_boolean_fn_t;
        HasProperty : TSEE_boolean_fn_t;
        Delete : TSEE_boolean_fn_t;
        DefaultValue : TSEE_default_fn_t;
        enumerator : TSEE_enumerator_fn_t;
        Construct : TSEE_call_fn_t;
        Call : TSEE_call_fn_t;
        HasInstance : TSEE_hasinstance_fn_t;
        get_sec_domain : TSEE_get_sec_domain_fn_t;
      end;

    TSEE_object = record
        objectclass : PTSEE_objectclass;
        Prototype : PTSEE_object;
        host_data : pointer;
      end;
      
    TSEE_enumclass = record
        unused : pointer;
        next : function (i:PTSEE_interpreter; e:PTSEE_enum; flags_return:pTcint):TPSEE_string;cdecl;
      end;
    
    TSEE_enum = record
        enumclass : PTSEE_enumclass;
      end;

    TSEE_native = record
        _object : TSEE_object;
        properties : array[0..256] of PTSEE_property;
        lru : PTSEE_property;
      end;
    TSEE_scope = record
        next : PTSEE_scope;
        obj : TPSEE_object;
      end;

    TSEE_inputclass = record
        next : function (para1:PTSEE_input):TSEE_unicode_t;cdecl;
        close : procedure (para1:PTSEE_input);
      end;

    TSEE_input = record
        inputclass : PTSEE_inputclass;
        eof : TSEE_boolean_t;
        lookahead : TSEE_unicode_t;
        filename : TPSEE_string;
        first_lineno : Tcint;
        interpreter : PTSEE_interpreter;
      end;

    TSEE_trace_event = (SEE_TRACE_CALL,SEE_TRACE_RETURN,SEE_TRACE_STATEMENT,
      SEE_TRACE_THROW);

    TSEE_interpreter = record
        host_data : pointer;
        compatibility : Tcint;
        Global : TPSEE_object;
        _Object : TPSEE_object;
        Object_prototype : TPSEE_object;
        Error : TPSEE_object;
        EvalError : TPSEE_object;
        RangeError : TPSEE_object;
        ReferenceError : TPSEE_object;
        SyntaxError : TPSEE_object;
        TypeError : TPSEE_object;
        URIError : TPSEE_object;
        _String : TPSEE_object;
        String_prototype : TPSEE_object;
        _Function : TPSEE_object;
        Function_prototype : TPSEE_object;
        _Array : TPSEE_object;
        Array_prototype : TPSEE_object;
        Number : TPSEE_object;
        Number_prototype : TPSEE_object;
        Boolean : TPSEE_object;
        Boolean_prototype : TPSEE_object;
        Math : TPSEE_object;
        RegExp : TPSEE_object;
        RegExp_prototype : TPSEE_object;
        Date : TPSEE_object;
        Date_prototype : TPSEE_object;
        Global_eval : TPSEE_object;
        Global_scope : PTSEE_scope;
        try_context : PTSEE_try_context;
        try_location : PTSEE_throw_location;
        traceback : PTSEE_traceback;
        module_private : Ppointer;
        intern_tab : pointer;
        random_seed : Tcuint;
        locale : PTcchar;
        recursion_limit : Tcint;
        sec_domain : pointer;
        trace : procedure (para1:PTSEE_interpreter; para2:PTSEE_throw_location; para3:PTSEE_context; para4:TSEE_trace_event);cdecl;
        regex_engine : PTSEE_regex_engine;
      end;

    TSEE_traceback = record
        call_location : PTSEE_throw_location;
        callee : TPSEE_object;
        call_type : Tcint;
        prev : PTSEE_traceback;
      end;

    TSEE_context = record
        interpreter : PTSEE_interpreter;
        activation : TPSEE_object;
        variable : TPSEE_object;
        varattr : Tcint;
        thisobj : TPSEE_object;
        scope : PTSEE_scope;
      end;

    TSEE_growable = record
        data_ptr : Ppointer;
        length_ptr : PTcuint;
        element_size : Tsize_t;
        allocated : Tsize_t;
        flag0 : word;
      end;

    TSEE_module = record
        magic : TSEE_uint32_t;
        name : PTcchar;
        version : PTcchar;
        index : Tcuint;
        mod_init : function :Tcint;cdecl;
        alloc : procedure (para1:PTSEE_interpreter);cdecl;
        init : procedure (para1:PTSEE_interpreter);cdecl;
      end;

    TSEE_string = record
        length : Tcuint;
        data : PTSEE_char_t;
        stringclass : PTSEE_stringclass;
        interpreter : PTSEE_interpreter;
        flags : Tcint;
      end;

    TSEE_stringclass = record
        growby : procedure (para1:TPSEE_string; para2:Tcuint);cdecl;
      end;

    TFinalProc = procedure (para1:PTSEE_interpreter; para2:pointer; para3:pointer); cdecl;
    TSEE_system = record
        default_locale : PTcchar;
        default_recursion_limit : Tcint;
        default_trace : procedure (para1:PTSEE_interpreter; para2:PTSEE_throw_location; para3:PTSEE_context; para4:TSEE_trace_event);cdecl;
        default_compat_flags : Tcint;
        random_seed : function :Tcuint;
        abort : procedure (para1:PTSEE_interpreter; para2:PTcchar);
        periodic : procedure (para1:PTSEE_interpreter);
        malloc : function (para1:PTSEE_interpreter; para2:Tsize_t; para3:PTcchar; para4:Tcint):pointer;
        malloc_finalize : function (para1:PTSEE_interpreter; para2:Tsize_t; para3:tfinalproc; para4:pointer; para5:PTcchar; para6:Tcint):pointer;
        malloc_string : function (para1:PTSEE_interpreter; para2:Tsize_t; para3:PTcchar; para4:Tcint):pointer;
        free : procedure (para1:PTSEE_interpreter; para2:pointer; para3:PTcchar; para4:Tcint);
        mem_exhausted : procedure (para1:PTSEE_interpreter);
        gcollect : procedure (para1:PTSEE_interpreter);
        transit_sec_domain : function (para1:PTSEE_interpreter; para2:pointer):pointer;
        code_alloc : function (para1:PTSEE_interpreter): PTSEE_code;
        object_construct : procedure (interp:PTSEE_interpreter; self:TPSEE_object; thisobj:TPSEE_object; argc:Tcint; argv:PPTSEE_value; 
                      res:PTSEE_value);
        default_regex_engine : PTSEE_regex_engine;
      end;

    TSEE_throw_location = record
        filename : TPSEE_string;
        lineno : Tcint;
      end;
      
    Tjmp_buf = record
      a : array[1..200] of byte;
    end;
    
    TSEE_try_context_t = record
        interpreter : PTSEE_interpreter;
        previous : PTSEE_try_context;
        thrown : TSEE_value;
        done : Tcint;
        env : Tjmp_buf; // 
        throw_file : PTcchar;
        throw_line : Tcint;
        saved_traceback : PTSEE_traceback;
        traceback : PTSEE_traceback;
      end;

    TSEE_try_context = TSEE_try_context_t;

  const
    bm_TSEE_growable_is_string = $1;
    bp_TSEE_growable_is_string = 0;

  var
    _SEE_isnan : function(n:TSEE_number_t):Tcint;cdecl;
    _SEE_isfinite : function(n:TSEE_number_t):Tcint;cdecl;
    _SEE_copysign : function(x:TSEE_number_t; y:TSEE_number_t):TSEE_number_t;cdecl;
    _SEE_ispinf : function(n:TSEE_number_t):Tcint;cdecl;
    _SEE_isninf : function(n:TSEE_number_t):Tcint;cdecl;
    SEE_ToPrimitive : procedure(i:PTSEE_interpreter; val:PTSEE_value; _type:PTSEE_value; res:PTSEE_value);cdecl;
    SEE_ToBoolean : procedure(i:PTSEE_interpreter; val:PTSEE_value; res:PTSEE_value);cdecl;
    SEE_ToNumber : procedure(i:PTSEE_interpreter; val:PTSEE_value; res:PTSEE_value);cdecl;
    SEE_ToInteger : procedure(i:PTSEE_interpreter; val:PTSEE_value; res:PTSEE_value);cdecl;
    SEE_ToString : procedure(i:PTSEE_interpreter; val:PTSEE_value; res:PTSEE_value);cdecl;
    SEE_ToObject : procedure(i:PTSEE_interpreter; val:PTSEE_value; res:PTSEE_value);cdecl;
    SEE_ToInt32 : function(i:PTSEE_interpreter; val:PTSEE_value):TSEE_int32_t;cdecl;
    SEE_ToUint32 : function(i:PTSEE_interpreter; val:PTSEE_value):TSEE_uint32_t;cdecl;
    SEE_ToUint16 : function(i:PTSEE_interpreter; val:PTSEE_value):TSEE_uint16_t;cdecl;
      SEE_hexstr_lowercase : array[0..15] of Tcchar;cvar;external;


    _SEE_intern_assert : function(i:PTSEE_interpreter; s:TPSEE_string):TPSEE_string;cdecl;
    SEE_intern_ascii : function(para1:PTSEE_interpreter; para2:PTcchar):TPSEE_string;cdecl;
    SEE_function_is_joined : function(a:PTSEE_object; b:PTSEE_object):Tcint;cdecl;
    SEE_Object_new : function(para1:PTSEE_interpreter):TPSEE_object;cdecl;
    SEE_object_call : procedure(para1:PTSEE_interpreter; para2:TPSEE_object; para3:TPSEE_object; para4:Tcint; para5:PPTSEE_value; 
      para6:PTSEE_value);cdecl;
    SEE_object_construct : procedure(para1:PTSEE_interpreter; para2:TPSEE_object; para3:TPSEE_object; para4:Tcint; para5:PPTSEE_value; 
      para6:PTSEE_value);cdecl;
    SEE_object_instanceof : function(interp:PTSEE_interpreter; val:PTSEE_value; obj:TPSEE_object):Tcint;cdecl;

    SEE_native_get : procedure(i:PTSEE_interpreter; obj:TPSEE_object; prop:TPSEE_string; res:PTSEE_value);cdecl;
    SEE_native_put : procedure(i:PTSEE_interpreter; obj:TPSEE_object; prop:TPSEE_string; val:PTSEE_value; flags:Tcint);cdecl;
    SEE_native_canput : function(i:PTSEE_interpreter; obj:TPSEE_object; prop:TPSEE_string):Tcint;cdecl;
    SEE_native_hasproperty : function(i:PTSEE_interpreter; obj:TPSEE_object; prop:TPSEE_string):Tcint;cdecl;
    SEE_native_hasownproperty : function(i:PTSEE_interpreter; obj:TPSEE_object; prop:TPSEE_string):Tcint;cdecl;
    SEE_native_getownattr : function(i:PTSEE_interpreter; obj:TPSEE_object; prop:TPSEE_string):Tcint;cdecl;
    SEE_native_delete : function(i:PTSEE_interpreter; obj:TPSEE_object; prop:TPSEE_string):Tcint;cdecl;
    SEE_native_defaultvalue : procedure(i:PTSEE_interpreter; obj:TPSEE_object; hint:PTSEE_value; res:PTSEE_value);cdecl;
    SEE_native_enumerator : function(i:PTSEE_interpreter; obj:TPSEE_object):TPSEE_enum;cdecl;
    SEE_native_new : function(i:PTSEE_interpreter):TPSEE_object;cdecl;
    SEE_native_init : procedure(obj:PTSEE_native; i:PTSEE_interpreter; obj_class:PTSEE_objectclass; prototype:TPSEE_object);cdecl;

    SEE_cfunction_make : function(i:PTSEE_interpreter; func:TSEE_call_fn_t; name:TPSEE_string; length:Tcint):TPSEE_object;cdecl;
    SEE_parse_args : procedure(i:PTSEE_interpreter; argc:Tcint; argv:PPTSEE_value; fmt:pTcchar);cdecl;varargs;
    SEE_call_args : procedure(i:PTSEE_interpreter; func:TPSEE_object; thisobj:TPSEE_object; ret:PTSEE_value; fmt:pTcchar);cdecl;varargs;

    SEE_PrintValue : procedure(i:PTSEE_interpreter; v:PTSEE_value; f:PTFILE);cdecl;
    SEE_PrintObject : procedure(i:PTSEE_interpreter; o:TPSEE_object; f:PTFILE);cdecl;
    SEE_PrintString : procedure(i:PTSEE_interpreter; s:TPSEE_string; f:PTFILE);cdecl;
    SEE_PrintTraceback : procedure(i:PTSEE_interpreter; f:PTFILE);cdecl;
    SEE_PrintContextTraceback : procedure(i:PTSEE_interpreter; context:PTSEE_try_context; f:PTFILE);cdecl;

    SEE_Global_eval : procedure(i:PTSEE_interpreter; input:PTSEE_input; res:PTSEE_value);cdecl;
    SEE_eval : procedure(i:PTSEE_interpreter; input:PTSEE_input; thisobj:TPSEE_object; variable:TPSEE_object; scope:PTSEE_scope; 
      res:PTSEE_value);cdecl;
    SEE_Function_new : function(i:PTSEE_interpreter; name:TPSEE_string; param_input:PTSEE_input; body_input:PTSEE_input):TPSEE_object;cdecl;

    SEE_error__throw_string : procedure(i:PTSEE_interpreter; errorobj:TPSEE_object; filename:pTcchar; lineno:Tcint; message:TPSEE_string);cdecl;
    SEE_error__throw : procedure(i:PTSEE_interpreter; errorobj:TPSEE_object; filename:pTcchar; lineno:Tcint; fmt:pTcchar);cdecl; varargs;
    SEE_error__throw_sys : procedure(i:PTSEE_interpreter; errorobj:TPSEE_object; filename:pTcchar; lineno:Tcint; fmt:pTcchar);cdecl; varargs;
    SEE_Error_make : function(i:PTSEE_interpreter; name:TPSEE_string):TPSEE_object;cdecl;
    SEE_error__throw0 : procedure(i:PTSEE_interpreter; errorobj:TPSEE_object; fmt:pTcchar);cdecl;varargs;
    SEE_error__throw_sys0 : procedure(i:PTSEE_interpreter; errorobj:TPSEE_object; fmt:pTcchar);cdecl;varargs;


    SEE_input_file : function(i:PTSEE_interpreter; f:PTFILE; filename:pTcchar; encoding:pTcchar):TPSEE_input;cdecl;
    SEE_input_string : function(i:PTSEE_interpreter; s:TPSEE_string):TPSEE_input;cdecl;
    SEE_input_utf8 : function(i:PTSEE_interpreter; s:pTcchar):TPSEE_input;cdecl;
    SEE_input_lookahead : function(i:TPSEE_input; maxlookahead:Tcint):TPSEE_input;cdecl;
    SEE_input_lookahead_copy : function(li:TPSEE_input; buf:PTSEE_unicode_t; buflen:Tcint):Tcint;cdecl;

    _SEE_intern_init : procedure(i:PTSEE_interpreter);cdecl;
    SEE_intern : function(i:PTSEE_interpreter; s:TPSEE_string):TPSEE_string;cdecl;
    SEE_intern_and_free : procedure(i:PTSEE_interpreter; s:PTPSEE_string);cdecl;
//    SEE_intern_ascii : function(i:PTSEE_interpreter; s:pTcchar):TPSEE_string;cdecl;
    SEE_intern_global : function(s:pTcchar):TPSEE_string;cdecl;

    SEE_interpreter_init : procedure(i:PTSEE_interpreter);cdecl;
    SEE_interpreter_init_compat : procedure(i:PTSEE_interpreter; compat_flags:Tcint);cdecl;
    SEE_interpreter_save_state : function(i:PTSEE_interpreter):TPSEE_interpreter_state;cdecl;
    SEE_interpreter_restore_state : procedure(i:PTSEE_interpreter; state:TPSEE_interpreter_state);cdecl;

    SEE_context_eval : procedure(context:PTSEE_context; expr:TPSEE_string; res:PTSEE_value);cdecl;

    Type
      Tfinalizefn = procedure( i:PTSEE_interpreter; p:pointer; closure:pointer);cdecl;
Var
    SEE_malloc : function(i:PTSEE_interpreter; sz:Tsize_t):pointer;cdecl;
    SEE_malloc_string : function(i:PTSEE_interpreter; sz:Tsize_t):pointer;cdecl;
    SEE_malloc_finalize : function(i:PTSEE_interpreter; sz:Tsize_t; finalizefn:Tfinalizefn; closure:pointer):pointer;cdecl;
    SEE_free : procedure(i:PTSEE_interpreter; memp:Ppointer);cdecl;
    SEE_gcollect : procedure(i:PTSEE_interpreter);cdecl;
    _SEE_malloc_debug : function(i:PTSEE_interpreter; sz:Tsize_t; _file:pTcchar; line:Tcint):pointer;cdecl;
    _SEE_malloc_string_debug : function(i:PTSEE_interpreter; sz:Tsize_t; _file:pTcchar; line:Tcint):pointer;cdecl;
    _SEE_malloc_finalize_debug : function(i:PTSEE_interpreter; sz:Tsize_t; finalizefn:Tfinalizefn; closure:pointer; _file:pTcchar; line:Tcint):pointer;cdecl;
    _SEE_free_debug : procedure(i:PTSEE_interpreter; memp:Ppointer; _file:pTcchar; line:Tcint);cdecl;


  function is_string(var a : TSEE_growable) : boolean;
  procedure set_is_string(var a : TSEE_growable; __is_string : boolean);

  var
    SEE_grow_to : procedure(i:PTSEE_interpreter; grow:PTSEE_growable; new_len:Tcuint);cdecl;
    _SEE_grow_to_debug : procedure(i:PTSEE_interpreter; grow:PTSEE_growable; new_len:Tcuint; _file:pTcchar; line:Tcint);cdecl;



    SEE_module_add : function(module:PTSEE_module):Tcint;cdecl;

    SEE_no_get : procedure(para1:PTSEE_interpreter; para2:TPSEE_object; para3:TPSEE_string; val:PTSEE_value);cdecl;
    SEE_no_put : procedure(para1:PTSEE_interpreter; para2:TPSEE_object; para3:TPSEE_string; val:PTSEE_value; para5:Tcint);cdecl;
    SEE_no_canput : function(para1:PTSEE_interpreter; para2:TPSEE_object; para3:TPSEE_string):Tcint;cdecl;
    SEE_no_hasproperty : function(para1:PTSEE_interpreter; para2:TPSEE_object; para3:TPSEE_string):Tcint;cdecl;
    SEE_no_delete : function(para1:PTSEE_interpreter; para2:TPSEE_object; para3:TPSEE_string):Tcint;cdecl;
    SEE_no_defaultvalue : procedure(para1:PTSEE_interpreter; para2:TPSEE_object; para3:PTSEE_value; para4:PTSEE_value);cdecl;
    SEE_no_enumerator : function(para1:PTSEE_interpreter; para2:TPSEE_object):TPSEE_enum;cdecl;

  var
    SEE_string_addch : procedure(s:TPSEE_string; ch:Tcint);cdecl;
    SEE_string_append : procedure(s:TPSEE_string; sffx:TPSEE_string);cdecl;
    SEE_string_append_ascii : procedure(s:TPSEE_string; ascii:pTcchar);cdecl;
    SEE_string_append_int : procedure(s:TPSEE_string; i:Tcint);cdecl;
    SEE_string_append_unicode : procedure(s:TPSEE_string; uch:TSEE_unicode_t);cdecl;
    SEE_string_fputs : function(s:TPSEE_string; _file:PTFILE):Tcint;cdecl;
    SEE_string_cmp : function(s1:TPSEE_string; s2:TPSEE_string):Tcint;cdecl;
    SEE_string_cmp_ascii : function(s1:TPSEE_string; s2:pTcchar):Tcint;cdecl;
    SEE_string_new : function(i:PTSEE_interpreter; space:Tcuint):TPSEE_string;cdecl;
    SEE_string_dup : function(i:PTSEE_interpreter; s:TPSEE_string):TPSEE_string;cdecl;
    SEE_string_substr : function(i:PTSEE_interpreter; s:TPSEE_string; index:Tcint; length:Tcint):TPSEE_string;cdecl;
    SEE_string_concat : function(i:PTSEE_interpreter; s1:TPSEE_string; s2:TPSEE_string):TPSEE_string;cdecl;
    SEE_string_sprintf : function(i:PTSEE_interpreter; fmt:pTcchar):TPSEE_string;cdecl; varargs;
    SEE_string_literal : function(i:PTSEE_interpreter; s:TPSEE_string):TPSEE_string;cdecl;
    SEE_string_fix : function(s:TPSEE_string):TPSEE_string;cdecl;
    SEE_string_free : procedure(i:PTSEE_interpreter; sp:PTPSEE_string);cdecl;
    SEE_string_toutf8 : procedure(i:PTSEE_interpreter; buf:pTcchar; buflen:Tsize_t; s:TPSEE_string);cdecl;
    SEE_string_utf8_size : function(interp:PTSEE_interpreter; s:TPSEE_string):Tsize_t;cdecl;
    _SEE_string_dup_fix : function(para1:PTSEE_interpreter; para2:TPSEE_string):TPSEE_string;cdecl;



  var
      SEE_system : TSEE_system;cvar;external;
    SEE_init : procedure;cdecl;
    SEE_regex_engine_list : function:PPTcchar;cdecl;
    SEE_regex_engine : function(name:pTcchar):PTSEE_regex_engine;cdecl;


  var
    SEE_throw_abort : procedure(para1:PTSEE_interpreter; para2:PTSEE_value; para3:PTcchar; para4:Tcint);cdecl;
    SEE_location_string : function(i:PTSEE_interpreter; loc:PTSEE_throw_location):TPSEE_string;cdecl;
    SEE_throw : procedure;cdecl;
    SEE_version : function: PTcchar;cdecl;

procedure see_input_close(Inp : PSEE_INPUT);

function SEE_VALUE_GET_TYPE(v : PSEE_value) : TSEE_type;
procedure SEE_VALUE_COPY(dst, src : PSEE_value);
procedure SEE_SET_UNDEFINED(v : PSEE_value);
procedure SEE_SET_NULL(v : PSEE_value);
procedure SEE_SET_BOOLEAN(v : PSEE_value; b : Boolean);
procedure SEE_SET_NUMBER(v : PSEE_value; n : TSEE_number_t);
procedure SEE_SET_STRING(v :PSEE_value; s: PSEE_STRING);
procedure SEE_SET_OBJECT(v :PSEE_value; o: PSEE_OBJECT);
function SEE_isnan (n:TSEE_number_t):Tcint;
function SEE_isfinite (n:TSEE_number_t):Tcint;
function SEE_copysign (x:TSEE_number_t; y:TSEE_number_t):TSEE_number_t;
function SEE_ispinf (n:TSEE_number_t):Tcint;
function SEE_isninf (n:TSEE_number_t):Tcint;
Function SEE_MODULE_PRIVATE(i : PTSEE_interpreter; m : PSEE_module) : Pointer;

procedure SEE_OBJECT_PUT(Interp : PTSEE_interpreter; Obj : PSEE_object; prop:TPSEE_string; val:PTSEE_value; flags:Tcint);

CONST
  SEE_ATTR_READONLY   = $01;
  SEE_ATTR_DONTENUM   = $02;
  SEE_ATTR_DONTDELETE = $04;
  SEE_ATTR_INTERNAL   = $08;

  SEE_MODULE_MAGIC = $5345456d;
  SEE_ATTR_DEFAULT = SEE_ATTR_DONTENUM;
  SEE_ATTR_LENGTH  = SEE_ATTR_READONLY or SEE_ATTR_DONTDELETE or  SEE_ATTR_DONTENUM;

{$ifdef libseehelper}
{$l libsee.so}
{$l libseewrap.o}
function  SEE_help_Global_eval (para1 : PSEE_Interpreter; para2: PSEE_input; para3 : PSEE_value) : cint; cdecl; external;
function  SEE_help_CAUGHT(para1: TSEE_try_context) : PSEE_value; cdecl; external;
procedure SEE_help_THROW(para1: PSEE_Interpreter; para2: PSEE_value); cdecl; external;
procedure SEE_help_RETHROW(para1: PSEE_Interpreter; para2: PSEE_value); cdecl; external;
procedure SEE_help_DEFAULT_CATCH(para1: PSEE_Interpreter; para2: PSEE_try_context); cdecl; external;
function new_SEE_interpreter : PSEE_Interpreter; cdecl; external;
function new_SEE_value : PSEE_Value; cdecl; external;
function new_SEE_objectclass : PSEE_objectclass; cdecl; external;
function new_SEE_object : PSEE_object; cdecl; external;
function new_SEE_enumclass : PTSEE_enumclass; cdecl; external;
function new_SEE_enum : PSEE_enum; cdecl; external;
function new_SEE_native : PSEE_native; cdecl; external;
function new_SEE_scope : PSEE_scope; cdecl; external;
function new_SEE_inputclass : PTSEE_inputclass; cdecl; external;
function new_SEE_input : PSEE_input; cdecl; external;
function new_SEE_traceback : PTSEE_traceback; cdecl; external;
function new_SEE_context : PSEE_context; cdecl; external;
function new_SEE_stringclass : PTSEE_stringclass; cdecl; external;
function new_SEE_throw_location : PSEE_throw_location; cdecl; external;
function new_SEE_try_context : PSEE_try_context; cdecl; external;
procedure free_SEE_struct(P : pointer); cdecl; external;
{$else}
function new_SEE_interpreter : PSEE_Interpreter;
function new_SEE_value : PSEE_Value;
function new_SEE_objectclass : PSEE_objectclass;
function new_SEE_object : PSEE_object;
function new_SEE_enumclass : PTSEE_enumclass;
function new_SEE_enum : PSEE_enum;
function new_SEE_native : PSEE_native;
function new_SEE_scope : PSEE_scope;
function new_SEE_inputclass : PTSEE_inputclass;
function new_SEE_input : PSEE_input;
function new_SEE_traceback : PTSEE_traceback;
function new_SEE_context : PSEE_context;
function new_SEE_stringclass : PTSEE_stringclass;
function new_SEE_throw_location : PSEE_throw_location;
function new_SEE_try_context : PSEE_try_context;
procedure free_SEE_struct(P : pointer);
{$endif}
procedure Loadlibsee(Const Alib : string);
procedure Freelibsee;
Function  LibseeLoaded : Boolean;

implementation

uses
    SysUtils, dynlibs;
    
{$ifndef libseehelper}
function new_SEE_interpreter : PSEE_Interpreter;

begin
  Result:=SEE_malloc(Nil,SizeOf(TSEE_interpreter));
end;

function new_SEE_value : PSEE_Value;

begin
  Result:=SEE_malloc(Nil,SizeOf(TSEE_Value));
end;

function new_SEE_objectclass : PSEE_objectclass;

begin
  Result:=SEE_malloc(Nil,SizeOf(TSEE_objectclass));
end;

function new_SEE_object : PSEE_object;

begin
  Result:=SEE_malloc(Nil,SizeOf(TSEE_object));
end;

function new_SEE_enumclass : PTSEE_enumclass;

begin
  Result:=SEE_malloc(Nil,SizeOf(TSEE_enumclass));
end;

function new_SEE_enum : PSEE_enum;

begin
  Result:=SEE_malloc(Nil,SizeOf(TSEE_enum));
end;

function new_SEE_native : PSEE_native;

begin
  Result:=SEE_malloc(Nil,SizeOf(TSEE_native));

end;

function new_SEE_scope : PSEE_scope;

begin
  Result:=SEE_malloc(Nil,SizeOf(TSEE_scope));
end;

function new_SEE_inputclass : PTSEE_inputclass;

begin
  Result:=SEE_malloc(Nil,SizeOf(TSEE_inputclass));
end;

function new_SEE_input : PSEE_input;

begin
  Result:=SEE_malloc(Nil,SizeOf(TSEE_input));

end;

function new_SEE_traceback : PTSEE_traceback;

begin
  Result:=SEE_malloc(Nil,SizeOf(TSEE_traceback));
end;

function new_SEE_context : PSEE_context;

begin
  Result:=SEE_malloc(Nil,SizeOf(TSEE_context));
end;

function new_SEE_stringclass : PTSEE_stringclass;

begin
  Result:=SEE_malloc(Nil,SizeOf(TSEE_stringclass));
end;

function new_SEE_throw_location : PSEE_throw_location;

begin
  Result:=SEE_malloc(Nil,SizeOf(TSEE_throw_location));
end;

function new_SEE_try_context : PSEE_try_context;

begin
  Result:=SEE_malloc(Nil,SizeOf(TSEE_try_context));
end;

procedure free_SEE_struct(P : pointer);
begin
  SEE_free(Nil,P);
end;

{$endif}

procedure SEE_OBJECT_PUT(Interp : PTSEE_interpreter; Obj : PSEE_object; prop:TPSEE_string; val:PTSEE_value; flags:Tcint);

begin
  obj^.objectclass^.Put(Interp,obj,prop,val,flags);
end;
// SEE_native_put : procedure(i:PTSEE_interpreter; obj:TPSEE_object; prop:TPSEE_string; val:PTSEE_value; flags:Tcint);cdecl;
function SEE_VALUE_GET_TYPE(v : PSEE_value) : TSEE_type;

begin
  Result:=v^._type;
end;

procedure SEE_VALUE_COPY(dst, src : PSEE_value);

begin
  dst^:=src^;
end;

procedure SEE_SET_TYPE(v: PSEE_VALUE; t : TSEE_type);

begin
  v^._type:=t;
end;

procedure SEE_SET_UNDEFINED(v : PSEE_value);

begin
  SEE_SET_TYPE(v,SEE_UNDEFINED);
end;

procedure SEE_SET_NULL(v : PSEE_value);

begin
  SEE_SET_TYPE(V,SEE_NULL);
end;

procedure SEE_SET_BOOLEAN(v : PSEE_value; b : Boolean);

begin
  SEE_SET_TYPE(V,SEE_BOOLEAN);
  V^.u.boolean:=char(ord(B));
end;

procedure SEE_SET_NUMBER(v : PSEE_value; n : TSEE_number_t);

begin
 SEE_SET_TYPE(V,SEE_NUMBER);
  V^.u.number:=N;
end;

procedure SEE_SET_STRING(v :PSEE_value; s: PSEE_STRING);

begin
  SEE_SET_TYPE(V,SEE_STRING);
  V^.u._string:=s
end;

procedure SEE_SET_OBJECT(v :PSEE_value; o: PSEE_OBJECT);
begin
  SEE_SET_TYPE(V,SEE_OBJECT);
  V^.u._object:=o;
end;

function SEE_isnan (n:TSEE_number_t):Tcint;
begin
  Result:=_see_isnan(n);
end;
function SEE_isfinite (n:TSEE_number_t):Tcint;

begin
  result:=_see_isfinite(n);
end;

function SEE_copysign (x:TSEE_number_t; y:TSEE_number_t):TSEE_number_t;

begin
  result:=_see_copysign(x,y);
end;

function SEE_ispinf (n:TSEE_number_t):Tcint;

begin
  result:=_see_ispinf(n);
end;

function  SEE_isninf (n:TSEE_number_t):Tcint;

begin
  result:=_see_isninf(n);
end;

  function is_string(var a : TSEE_growable) : Boolean;
    begin
      is_string:=((a.flag0 and bm_TSEE_growable_is_string) shr bp_TSEE_growable_is_string)<>0;
    end;

  procedure set_is_string(var a : TSEE_growable; __is_string : Boolean);
    begin
      a.flag0:=a.flag0 or ((ord(__is_string) shl bp_TSEE_growable_is_string) and bm_TSEE_growable_is_string);
    end;

Function SEE_MODULE_PRIVATE(i : PTSEE_interpreter; m : PSEE_module) : Pointer;

begin
  Result:=@I^.module_private[Integer(m^.index)];
end;

procedure see_input_close(Inp : PSEE_INPUT);

begin
  Inp^.inputclass^.close(inp);
end;

var
  hlib : tlibhandle;

procedure Freelibsee;

begin
  If HLib<>NilHandle then
    FreeLibrary(hlib);
  _SEE_isnan:=nil;
  _SEE_isfinite:=nil;
  _SEE_copysign:=nil;
  _SEE_ispinf:=nil;
  _SEE_isninf:=nil;
  SEE_ToPrimitive:=nil;
  SEE_ToBoolean:=nil;
  SEE_ToNumber:=nil;
  SEE_ToInteger:=nil;
  SEE_ToString:=nil;
  SEE_ToObject:=nil;
  SEE_ToInt32:=nil;
  SEE_ToUint32:=nil;
  SEE_ToUint16:=nil;
  _SEE_intern_assert:=nil;
  SEE_intern_ascii:=nil;
  SEE_function_is_joined:=nil;
  SEE_Object_new:=nil;
  SEE_object_call:=nil;
  SEE_object_construct:=nil;
  SEE_object_instanceof:=nil;
  SEE_native_get:=nil;
  SEE_native_put:=nil;
  SEE_native_canput:=nil;
  SEE_native_hasproperty:=nil;
  SEE_native_hasownproperty:=nil;
  SEE_native_getownattr:=nil;
  SEE_native_delete:=nil;
  SEE_native_defaultvalue:=nil;
  SEE_native_enumerator:=nil;
  SEE_native_new:=nil;
  SEE_native_init:=nil;
  SEE_cfunction_make:=nil;
  SEE_parse_args:=nil;
  SEE_parse_args:=nil;
  SEE_call_args:=nil;
  SEE_call_args:=nil;
  SEE_PrintValue:=nil;
  SEE_PrintObject:=nil;
  SEE_PrintString:=nil;
  SEE_PrintTraceback:=nil;
  SEE_PrintContextTraceback:=nil;
  SEE_Global_eval:=nil;
  SEE_eval:=nil;
  SEE_Function_new:=nil;
  SEE_error__throw_string:=nil;
  SEE_error__throw:=nil;
  SEE_error__throw:=nil;
  SEE_error__throw_sys:=nil;
  SEE_error__throw_sys:=nil;
  SEE_Error_make:=nil;
  SEE_error__throw0:=nil;
  SEE_error__throw0:=nil;
  SEE_error__throw_sys0:=nil;
  SEE_error__throw_sys0:=nil;
  SEE_input_file:=nil;
  SEE_input_string:=nil;
  SEE_input_utf8:=nil;
  SEE_input_lookahead:=nil;
  SEE_input_lookahead_copy:=nil;
  _SEE_intern_init:=nil;
  SEE_intern:=nil;
  SEE_intern_and_free:=nil;
  SEE_intern_ascii:=nil;
  SEE_intern_global:=nil;
  SEE_interpreter_init:=nil;
  SEE_interpreter_init_compat:=nil;
  SEE_interpreter_save_state:=nil;
  SEE_interpreter_restore_state:=nil;
  SEE_context_eval:=nil;
  SEE_malloc:=nil;
  SEE_malloc_string:=nil;
  SEE_malloc_finalize:=nil;
  SEE_free:=nil;
  SEE_gcollect:=nil;
  _SEE_malloc_debug:=nil;
  _SEE_malloc_string_debug:=nil;
  _SEE_malloc_finalize_debug:=nil;
  _SEE_free_debug:=nil;
  SEE_grow_to:=nil;
  _SEE_grow_to_debug:=nil;
  SEE_module_add:=nil;
  SEE_no_get:=nil;
  SEE_no_put:=nil;
  SEE_no_canput:=nil;
  SEE_no_hasproperty:=nil;
  SEE_no_delete:=nil;
  SEE_no_defaultvalue:=nil;
  SEE_no_enumerator:=nil;
  SEE_string_addch:=nil;
  SEE_string_append:=nil;
  SEE_string_append_ascii:=nil;
  SEE_string_append_int:=nil;
  SEE_string_append_unicode:=nil;
  SEE_string_fputs:=nil;
  SEE_string_cmp:=nil;
  SEE_string_cmp_ascii:=nil;
  SEE_string_new:=nil;
  SEE_string_dup:=nil;
  SEE_string_substr:=nil;
  SEE_string_concat:=nil;
  SEE_string_sprintf:=nil;
  SEE_string_sprintf:=nil;
  SEE_string_literal:=nil;
  SEE_string_fix:=nil;
  SEE_string_free:=nil;
  SEE_string_toutf8:=nil;
  SEE_string_utf8_size:=nil;
  _SEE_string_dup_fix:=nil;
  SEE_init:=nil;
  SEE_regex_engine_list:=nil;
  SEE_regex_engine:=nil;
  SEE_throw_abort:=nil;
  SEE_location_string:=nil;
  SEE_throw:=nil;
  SEE_version:=nil;
end;

Function  LibseeLoaded : Boolean;

begin
  Result:=hlib<>nilhandle;
end;

procedure Loadlibsee(Const Alib : string);

begin
  Freelibsee;
  hlib:=LoadLibrary(pchar(Alib));
  if hlib=0 then
    raise Exception.Create(format('Could not load library: %s',[Alib]));
  pointer(_SEE_isnan):=GetProcAddress(hlib,'_SEE_isnan');
  pointer(_SEE_isfinite):=GetProcAddress(hlib,'_SEE_isfinite');
  pointer(_SEE_copysign):=GetProcAddress(hlib,'_SEE_copysign');
  pointer(_SEE_ispinf):=GetProcAddress(hlib,'_SEE_ispinf');
  pointer(_SEE_isninf):=GetProcAddress(hlib,'_SEE_isninf');
  pointer(SEE_ToPrimitive):=GetProcAddress(hlib,'SEE_ToPrimitive');
  pointer(SEE_ToBoolean):=GetProcAddress(hlib,'SEE_ToBoolean');
  pointer(SEE_ToNumber):=GetProcAddress(hlib,'SEE_ToNumber');
  pointer(SEE_ToInteger):=GetProcAddress(hlib,'SEE_ToInteger');
  pointer(SEE_ToString):=GetProcAddress(hlib,'SEE_ToString');
  pointer(SEE_ToObject):=GetProcAddress(hlib,'SEE_ToObject');
  pointer(SEE_ToInt32):=GetProcAddress(hlib,'SEE_ToInt32');
  pointer(SEE_ToUint32):=GetProcAddress(hlib,'SEE_ToUint32');
  pointer(SEE_ToUint16):=GetProcAddress(hlib,'SEE_ToUint16');
  pointer(_SEE_intern_assert):=GetProcAddress(hlib,'_SEE_intern_assert');
  pointer(SEE_intern_ascii):=GetProcAddress(hlib,'SEE_intern_ascii');
  pointer(SEE_function_is_joined):=GetProcAddress(hlib,'SEE_function_is_joined');
  pointer(SEE_Object_new):=GetProcAddress(hlib,'SEE_Object_new');
  pointer(SEE_object_call):=GetProcAddress(hlib,'SEE_object_call');
  pointer(SEE_object_construct):=GetProcAddress(hlib,'SEE_object_construct');
  pointer(SEE_object_instanceof):=GetProcAddress(hlib,'SEE_object_instanceof');
  pointer(SEE_native_get):=GetProcAddress(hlib,'SEE_native_get');
  pointer(SEE_native_put):=GetProcAddress(hlib,'SEE_native_put');
  pointer(SEE_native_canput):=GetProcAddress(hlib,'SEE_native_canput');
  pointer(SEE_native_hasproperty):=GetProcAddress(hlib,'SEE_native_hasproperty');
  pointer(SEE_native_hasownproperty):=GetProcAddress(hlib,'SEE_native_hasownproperty');
  pointer(SEE_native_getownattr):=GetProcAddress(hlib,'SEE_native_getownattr');
  pointer(SEE_native_delete):=GetProcAddress(hlib,'SEE_native_delete');
  pointer(SEE_native_defaultvalue):=GetProcAddress(hlib,'SEE_native_defaultvalue');
  pointer(SEE_native_enumerator):=GetProcAddress(hlib,'SEE_native_enumerator');
  pointer(SEE_native_new):=GetProcAddress(hlib,'SEE_native_new');
  pointer(SEE_native_init):=GetProcAddress(hlib,'SEE_native_init');
  pointer(SEE_cfunction_make):=GetProcAddress(hlib,'SEE_cfunction_make');
  pointer(SEE_parse_args):=GetProcAddress(hlib,'SEE_parse_args');
  pointer(SEE_parse_args):=GetProcAddress(hlib,'SEE_parse_args');
  pointer(SEE_call_args):=GetProcAddress(hlib,'SEE_call_args');
  pointer(SEE_call_args):=GetProcAddress(hlib,'SEE_call_args');
  pointer(SEE_PrintValue):=GetProcAddress(hlib,'SEE_PrintValue');
  pointer(SEE_PrintObject):=GetProcAddress(hlib,'SEE_PrintObject');
  pointer(SEE_PrintString):=GetProcAddress(hlib,'SEE_PrintString');
  pointer(SEE_PrintTraceback):=GetProcAddress(hlib,'SEE_PrintTraceback');
  pointer(SEE_PrintContextTraceback):=GetProcAddress(hlib,'SEE_PrintContextTraceback');
  pointer(SEE_Global_eval):=GetProcAddress(hlib,'SEE_Global_eval');
  pointer(SEE_eval):=GetProcAddress(hlib,'SEE_eval');
  pointer(SEE_Function_new):=GetProcAddress(hlib,'SEE_Function_new');
  pointer(SEE_error__throw_string):=GetProcAddress(hlib,'SEE_error__throw_string');
  pointer(SEE_error__throw):=GetProcAddress(hlib,'SEE_error__throw');
  pointer(SEE_error__throw):=GetProcAddress(hlib,'SEE_error__throw');
  pointer(SEE_error__throw_sys):=GetProcAddress(hlib,'SEE_error__throw_sys');
  pointer(SEE_error__throw_sys):=GetProcAddress(hlib,'SEE_error__throw_sys');
  pointer(SEE_Error_make):=GetProcAddress(hlib,'SEE_Error_make');
  pointer(SEE_error__throw0):=GetProcAddress(hlib,'SEE_error__throw0');
  pointer(SEE_error__throw0):=GetProcAddress(hlib,'SEE_error__throw0');
  pointer(SEE_error__throw_sys0):=GetProcAddress(hlib,'SEE_error__throw_sys0');
  pointer(SEE_error__throw_sys0):=GetProcAddress(hlib,'SEE_error__throw_sys0');
  pointer(SEE_input_file):=GetProcAddress(hlib,'SEE_input_file');
  pointer(SEE_input_string):=GetProcAddress(hlib,'SEE_input_string');
  pointer(SEE_input_utf8):=GetProcAddress(hlib,'SEE_input_utf8');
  pointer(SEE_input_lookahead):=GetProcAddress(hlib,'SEE_input_lookahead');
  pointer(SEE_input_lookahead_copy):=GetProcAddress(hlib,'SEE_input_lookahead_copy');
  pointer(_SEE_intern_init):=GetProcAddress(hlib,'_SEE_intern_init');
  pointer(SEE_intern):=GetProcAddress(hlib,'SEE_intern');
  pointer(SEE_intern_and_free):=GetProcAddress(hlib,'SEE_intern_and_free');
  pointer(SEE_intern_ascii):=GetProcAddress(hlib,'SEE_intern_ascii');
  pointer(SEE_intern_global):=GetProcAddress(hlib,'SEE_intern_global');
  pointer(SEE_interpreter_init):=GetProcAddress(hlib,'SEE_interpreter_init');
  pointer(SEE_interpreter_init_compat):=GetProcAddress(hlib,'SEE_interpreter_init_compat');
  pointer(SEE_interpreter_save_state):=GetProcAddress(hlib,'SEE_interpreter_save_state');
  pointer(SEE_interpreter_restore_state):=GetProcAddress(hlib,'SEE_interpreter_restore_state');
  pointer(SEE_context_eval):=GetProcAddress(hlib,'SEE_context_eval');
  pointer(SEE_malloc):=GetProcAddress(hlib,'SEE_malloc');
  pointer(SEE_malloc_string):=GetProcAddress(hlib,'SEE_malloc_string');
  pointer(SEE_malloc_finalize):=GetProcAddress(hlib,'SEE_malloc_finalize');
  pointer(SEE_free):=GetProcAddress(hlib,'SEE_free');
  pointer(SEE_gcollect):=GetProcAddress(hlib,'SEE_gcollect');
  pointer(_SEE_malloc_debug):=GetProcAddress(hlib,'_SEE_malloc_debug');
  pointer(_SEE_malloc_string_debug):=GetProcAddress(hlib,'_SEE_malloc_string_debug');
  pointer(_SEE_malloc_finalize_debug):=GetProcAddress(hlib,'_SEE_malloc_finalize_debug');
  pointer(_SEE_free_debug):=GetProcAddress(hlib,'_SEE_free_debug');
  pointer(SEE_grow_to):=GetProcAddress(hlib,'SEE_grow_to');
  pointer(_SEE_grow_to_debug):=GetProcAddress(hlib,'_SEE_grow_to_debug');
  pointer(SEE_module_add):=GetProcAddress(hlib,'SEE_module_add');
  pointer(SEE_no_get):=GetProcAddress(hlib,'SEE_no_get');
  pointer(SEE_no_put):=GetProcAddress(hlib,'SEE_no_put');
  pointer(SEE_no_canput):=GetProcAddress(hlib,'SEE_no_canput');
  pointer(SEE_no_hasproperty):=GetProcAddress(hlib,'SEE_no_hasproperty');
  pointer(SEE_no_delete):=GetProcAddress(hlib,'SEE_no_delete');
  pointer(SEE_no_defaultvalue):=GetProcAddress(hlib,'SEE_no_defaultvalue');
  pointer(SEE_no_enumerator):=GetProcAddress(hlib,'SEE_no_enumerator');
  pointer(SEE_string_addch):=GetProcAddress(hlib,'SEE_string_addch');
  pointer(SEE_string_append):=GetProcAddress(hlib,'SEE_string_append');
  pointer(SEE_string_append_ascii):=GetProcAddress(hlib,'SEE_string_append_ascii');
  pointer(SEE_string_append_int):=GetProcAddress(hlib,'SEE_string_append_int');
  pointer(SEE_string_append_unicode):=GetProcAddress(hlib,'SEE_string_append_unicode');
  pointer(SEE_string_fputs):=GetProcAddress(hlib,'SEE_string_fputs');
  pointer(SEE_string_cmp):=GetProcAddress(hlib,'SEE_string_cmp');
  pointer(SEE_string_cmp_ascii):=GetProcAddress(hlib,'SEE_string_cmp_ascii');
  pointer(SEE_string_new):=GetProcAddress(hlib,'SEE_string_new');
  pointer(SEE_string_dup):=GetProcAddress(hlib,'SEE_string_dup');
  pointer(SEE_string_substr):=GetProcAddress(hlib,'SEE_string_substr');
  pointer(SEE_string_concat):=GetProcAddress(hlib,'SEE_string_concat');
  pointer(SEE_string_sprintf):=GetProcAddress(hlib,'SEE_string_sprintf');
  pointer(SEE_string_sprintf):=GetProcAddress(hlib,'SEE_string_sprintf');
  pointer(SEE_string_literal):=GetProcAddress(hlib,'SEE_string_literal');
  pointer(SEE_string_fix):=GetProcAddress(hlib,'SEE_string_fix');
  pointer(SEE_string_free):=GetProcAddress(hlib,'SEE_string_free');
  pointer(SEE_string_toutf8):=GetProcAddress(hlib,'SEE_string_toutf8');
  pointer(SEE_string_utf8_size):=GetProcAddress(hlib,'SEE_string_utf8_size');
  pointer(_SEE_string_dup_fix):=GetProcAddress(hlib,'_SEE_string_dup_fix');
  pointer(SEE_init):=GetProcAddress(hlib,'SEE_init');
  pointer(SEE_regex_engine_list):=GetProcAddress(hlib,'SEE_regex_engine_list');
  pointer(SEE_regex_engine):=GetProcAddress(hlib,'SEE_regex_engine');
  pointer(SEE_throw_abort):=GetProcAddress(hlib,'SEE_throw_abort');
  pointer(SEE_location_string):=GetProcAddress(hlib,'SEE_location_string');
  pointer(SEE_throw):=GetProcAddress(hlib,'SEE_throw');
  pointer(SEE_version):=GetProcAddress(hlib,'SEE_version');
end;


initialization
  try
    Loadlibsee(LibSeeLibraryName);
  except
    // Ignore errors.
  end;

finalization
  Freelibsee;
end.
