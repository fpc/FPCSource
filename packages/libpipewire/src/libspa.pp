{$IFNDEF FPC_DOTTEDUNITS}
unit libspa;
{$ENDIF}

{$mode objfpc}
{$h+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.CTypes;
{$ELSE}
uses ctypes;
{$ENDIF}

{$l spabridge.o}

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


// Aliases
Type
  tuint32_t = uint32;
  PTuint32_t = ^tuint32_t;
  Tint32_t = int32;
  Pint32_t = ^Tint32_t;
  Tsize_t = csize_t;
  Tssize_t = csize_t;
  Tuint64_t = uint64;
  PTuint64_t = ^Tuint64_t;
  TTimespec = record end;
  PTTimespec = ^TTimespec;
  Tint64_t = int64;
  Tdouble = double;
  Pint64_t = ^int64;

  T_Bool = byte;
  Tbool = T_bool;
  _Bool = T_Bool;
  P_Bool = ^T_Bool;
  PBool = P_Bool;
  PT_Bool = P_Bool;
  Toff_t = ^coff_t;

{
  Automatically converted by H2Pas 1.0.0 from libspa.h
  The following command line parameters were used:
    -t
    -e
    libspa.h
}

// Enums

Type
  Tspa_direction =  Longint;

Const
  SPA_DIRECTION_INPUT = 0;
  SPA_DIRECTION_OUTPUT = 1;


Type
  Tspa_param_type = Longint;

Const
  SPA_PARAM_Invalid = 0;
  SPA_PARAM_PropInfo = 1;
  SPA_PARAM_Props = 2;
  SPA_PARAM_EnumFormat = 3;
  SPA_PARAM_Format = 4;
  SPA_PARAM_Buffers = 5;
  SPA_PARAM_Meta = 6;
  SPA_PARAM_IO = 7;
  SPA_PARAM_EnumProfile = 8;
  SPA_PARAM_Profile = 9;
  SPA_PARAM_EnumPortConfig = 10;
  SPA_PARAM_PortConfig = 11;
  SPA_PARAM_EnumRoute = 12;
  SPA_PARAM_Route = 13;
  SPA_PARAM_Control = 14;
  SPA_PARAM_Latency = 15;
  SPA_PARAM_ProcessLatency = 16;

Type
  Tspa_param_buffers =  Longint;
Const
  SPA_PARAM_BUFFERS_START = 0;
  SPA_PARAM_BUFFERS_buffers = 1;
  SPA_PARAM_BUFFERS_blocks = 2;
  SPA_PARAM_BUFFERS_size = 3;
  SPA_PARAM_BUFFERS_stride = 4;
  SPA_PARAM_BUFFERS_align = 5;
  SPA_PARAM_BUFFERS_dataType = 6;


type
  Tspa_param_meta =  Longint;
Const
  SPA_PARAM_META_START = 0;
  SPA_PARAM_META_type = 1;
  SPA_PARAM_META_size = 2;


type
  Tspa_param_io =  Longint;
Const
  SPA_PARAM_IO_START = 0;
  SPA_PARAM_IO_id = 1;
  SPA_PARAM_IO_size = 2;


type
  Tspa_param_availability =  Longint;
Const
  SPA_PARAM_AVAILABILITY_unknown = 0;
  SPA_PARAM_AVAILABILITY_no = 1;
  SPA_PARAM_AVAILABILITY_yes = 2;


type
  Tspa_param_profile =  Longint;
Const
  SPA_PARAM_PROFILE_START = 0;
  SPA_PARAM_PROFILE_index = 1;
  SPA_PARAM_PROFILE_name = 2;
  SPA_PARAM_PROFILE_description = 3;
  SPA_PARAM_PROFILE_priority = 4;
  SPA_PARAM_PROFILE_available = 5;
  SPA_PARAM_PROFILE_info = 6;
  SPA_PARAM_PROFILE_classes = 7;
  SPA_PARAM_PROFILE_save = 8;


type
  Tspa_param_port_config_mode =  Longint;
Const
  SPA_PARAM_PORT_CONFIG_MODE_none = 0;
  SPA_PARAM_PORT_CONFIG_MODE_passthrough = 1;
  SPA_PARAM_PORT_CONFIG_MODE_convert = 2;
  SPA_PARAM_PORT_CONFIG_MODE_dsp = 3;


type
  Tspa_param_port_config =  Longint;
Const
  SPA_PARAM_PORT_CONFIG_START = 0;
  SPA_PARAM_PORT_CONFIG_direction = 1;
  SPA_PARAM_PORT_CONFIG_mode = 2;
  SPA_PARAM_PORT_CONFIG_monitor = 3;
  SPA_PARAM_PORT_CONFIG_control = 4;
  SPA_PARAM_PORT_CONFIG_format = 5;


type
  Tspa_param_route =  Longint;

Const
  SPA_PARAM_ROUTE_START = 0;
  SPA_PARAM_ROUTE_index = 1;
  SPA_PARAM_ROUTE_direction = 2;
  SPA_PARAM_ROUTE_device = 3;
  SPA_PARAM_ROUTE_name = 4;
  SPA_PARAM_ROUTE_description = 5;
  SPA_PARAM_ROUTE_priority = 6;
  SPA_PARAM_ROUTE_available = 7;
  SPA_PARAM_ROUTE_info = 8;
  SPA_PARAM_ROUTE_profiles = 9;
  SPA_PARAM_ROUTE_props = 10;
  SPA_PARAM_ROUTE_devices = 11;
  SPA_PARAM_ROUTE_profile = 12;
  SPA_PARAM_ROUTE_save = 13;


Type
  Tspa_param_latency =  Longint;

Const
    SPA_PARAM_LATENCY_START = 0;
    SPA_PARAM_LATENCY_direction = 1;
    SPA_PARAM_LATENCY_minQuantum = 2;
    SPA_PARAM_LATENCY_maxQuantum = 3;
    SPA_PARAM_LATENCY_minRate = 4;
    SPA_PARAM_LATENCY_maxRate = 5;
    SPA_PARAM_LATENCY_minNs = 6;
    SPA_PARAM_LATENCY_maxNs = 7;

type
  Tspa_param_process_latency =  Longint;
Const
  SPA_PARAM_PROCESS_LATENCY_START = 0;
  SPA_PARAM_PROCESS_LATENCY_quantum = 1;
  SPA_PARAM_PROCESS_LATENCY_rate = 2;
  SPA_PARAM_PROCESS_LATENCY_ns = 3;


type
  Tspa_param_bitorder =  Longint;

Const
  SPA_PARAM_BITORDER_unknown = 0;
  SPA_PARAM_BITORDER_msb = 1;
  SPA_PARAM_BITORDER_lsb = 2;

Type
  Tspa_type =  Longint;

Const
  SPA_TYPE_START = $00000;
  SPA_TYPE_None = 1;
  SPA_TYPE_Bool = 2;
  SPA_TYPE_Id = 3;
  SPA_TYPE_Int = 4;
  SPA_TYPE_Long = 5;
  SPA_TYPE_Float = 6;
  SPA_TYPE_Double = 7;
  SPA_TYPE_String = 8;
  SPA_TYPE_Bytes = 9;
  SPA_TYPE_Rectangle = 10;
  SPA_TYPE_Fraction = 11;
  SPA_TYPE_Bitmap = 12;
  SPA_TYPE_Array = 13;
  SPA_TYPE_Struct = 14;
  SPA_TYPE_Object = 15;
  SPA_TYPE_Sequence = 16;
  SPA_TYPE_Pointer = 17;
  SPA_TYPE_Fd = 18;
  SPA_TYPE_Choice = 19;
  SPA_TYPE_Pod = 20;
  _SPA_TYPE_LAST = 21;
  SPA_TYPE_POINTER_START = $10000;
  SPA_TYPE_POINTER_Buffer = 65537;
  SPA_TYPE_POINTER_Meta = 65538;
  SPA_TYPE_POINTER_Dict = 65539;
  _SPA_TYPE_POINTER_LAST = 65540;
  SPA_TYPE_EVENT_START = $20000;
  SPA_TYPE_EVENT_Device = 131073;
  SPA_TYPE_EVENT_Node = 131074;
  _SPA_TYPE_EVENT_LAST = 131075;
  SPA_TYPE_COMMAND_START = $30000;
  SPA_TYPE_COMMAND_Device = 196609;
  SPA_TYPE_COMMAND_Node = 196610;
  _SPA_TYPE_COMMAND_LAST = 196611;
  SPA_TYPE_OBJECT_START = $40000;
  SPA_TYPE_OBJECT_PropInfo = 262145;
  SPA_TYPE_OBJECT_Props = 262146;
  SPA_TYPE_OBJECT_Format = 262147;
  SPA_TYPE_OBJECT_ParamBuffers = 262148;
  SPA_TYPE_OBJECT_ParamMeta = 262149;
  SPA_TYPE_OBJECT_ParamIO = 262150;
  SPA_TYPE_OBJECT_ParamProfile = 262151;
  SPA_TYPE_OBJECT_ParamPortConfig = 262152;
  SPA_TYPE_OBJECT_ParamRoute = 262153;
  SPA_TYPE_OBJECT_Profiler = 262154;
  SPA_TYPE_OBJECT_ParamLatency = 262155;
  SPA_TYPE_OBJECT_ParamProcessLatency = 262156;
  _SPA_TYPE_OBJECT_LAST = 262157;
  SPA_TYPE_VENDOR_PipeWire = $02000000;
  SPA_TYPE_VENDOR_Other = $7f000000;

type
  Tspa_choice_type =  Longint;
Const
  SPA_CHOICE_None = 0;
  SPA_CHOICE_Range = 1;
  SPA_CHOICE_Step = 2;
  SPA_CHOICE_Enum = 3;
  SPA_CHOICE_Flags = 4;

Type
  Tspa_meta_type =  Longint;
Const
  SPA_META_Invalid = 0;
  SPA_META_Header = 1;
  SPA_META_VideoCrop = 2;
  SPA_META_VideoDamage = 3;
  SPA_META_Bitmap = 4;
  SPA_META_Cursor = 5;
  SPA_META_Control = 6;
  SPA_META_Busy = 7;
  _SPA_META_LAST = 8;

Type
  Tspa_data_type =  Longint;

Const
  SPA_DATA_Invalid = 0;
  SPA_DATA_MemPtr = 1;
  SPA_DATA_MemFd = 2;
  SPA_DATA_DmaBuf = 3;
  SPA_DATA_MemId = 4;
  _SPA_DATA_LAST = 5;

Type
  Tspa_node_event =  Longint;
Const
  SPA_NODE_EVENT_Error = 0;
  SPA_NODE_EVENT_Buffering = 1;
  SPA_NODE_EVENT_RequestRefresh = 2;
  SPA_NODE_EVENT_RequestProcess = 3;


Type
  Tspa_event_node =  Longint;

Const
  SPA_EVENT_NODE_START = 0;

type
  Tspa_node_command =  Longint;

Const
  SPA_NODE_COMMAND_Suspend = 0;
  SPA_NODE_COMMAND_Pause = 1;
  SPA_NODE_COMMAND_Start = 2;
  SPA_NODE_COMMAND_Enable = 3;
  SPA_NODE_COMMAND_Disable = 4;
  SPA_NODE_COMMAND_Flush = 5;
  SPA_NODE_COMMAND_Drain = 6;
  SPA_NODE_COMMAND_Marker = 7;
  SPA_NODE_COMMAND_ParamBegin = 8;
  SPA_NODE_COMMAND_ParamEnd = 9;
  SPA_NODE_COMMAND_RequestProcess = 10;

type
  Tspa_log_level =  Longint;
Const
  SPA_LOG_LEVEL_NONE = 0;
  SPA_LOG_LEVEL_ERROR = 1;
  SPA_LOG_LEVEL_WARN = 2;
  SPA_LOG_LEVEL_INFO = 3;
  SPA_LOG_LEVEL_DEBUG = 4;
  SPA_LOG_LEVEL_TRACE = 5;


type
  Tspa_io_type =  Longint;
Const
  SPA_IO_Invalid = 0;
  SPA_IO_Buffers = 1;
  SPA_IO_Range = 2;
  SPA_IO_Clock = 3;
  SPA_IO_Latency = 4;
  SPA_IO_Control = 5;
  SPA_IO_Notify = 6;
  SPA_IO_Position = 7;
  SPA_IO_RateMatch = 8;
  SPA_IO_Memory = 9;

Type
  Tspa_io_position_state =  Longint;
Const
  SPA_IO_POSITION_STATE_STOPPED = 0;
  SPA_IO_POSITION_STATE_STARTING = 1;
  SPA_IO_POSITION_STATE_RUNNING = 2;

Type
  Pitimerspec  = ^Titimerspec;
  Plongint  = ^longint;
  Pspa_buffer  = ^Tspa_buffer;
  PPspa_buffer = ^Pspa_buffer;
  Pspa_command  = ^Tspa_command;
  Pspa_dict  = ^Tspa_dict;
  Pspa_event  = ^Tspa_event;
  Pspa_handle  = ^Tspa_handle;
  Pspa_handle_factory  = ^Tspa_handle_factory;
  Pspa_hook  = ^Tspa_hook;
  Pspa_interface_info  = ^Tspa_interface_info;
  Pspa_log_topic  = ^Tspa_log_topic;
  Pspa_loop  = ^Tspa_loop;
  Pspa_loop_control_hooks  = ^Tspa_loop_control_hooks;
  Pspa_node_callbacks  = ^Tspa_node_callbacks;
  Pspa_node_events  = ^Tspa_node_events;
  Pspa_node_info  = ^Tspa_node_info;
  Pspa_pod  = ^Tspa_pod;
  PPspa_pod = ^Pspa_pod;
  Pspa_poll_event  = ^Tspa_poll_event;
  Pspa_port_info  = ^Tspa_port_info;
  Pspa_source  = ^Tspa_source;
  Pspa_support  = ^Tspa_support;
  Ptimespec  = ^Ttimespec;
  Puint32_t  = ^Tuint32_t;
  Puint64_t  = ^Tuint64_t;

  Tspa_rectangle = record
    width : Tuint32_t;
    height : Tuint32_t;
  end;
  Pspa_rectangle = ^Tspa_rectangle;

  Tspa_point = record
    x : Tint32_t;
    y : Tint32_t;
  end;

  Tspa_region = record
    position : Tspa_point;
    size : Tspa_rectangle;
  end;

  Tspa_fraction = record
    num : Tuint32_t;
    denom : Tuint32_t;
  end;
  Pspa_fraction = ^Tspa_fraction;



  Tspa_dict_item = record
    key : pchar;
    value : pchar;
  end;
  Pspa_dict_item = ^Tspa_dict_item;


  Tspa_dict = record
    flags : Tuint32_t;
    n_items : Tuint32_t;
    items : ^Tspa_dict_item;
  end;
  PTspa_dict = ^Tspa_dict;

  tspa_callbacks = record
    funcs : pointer;
    data : pointer;
  end;


  Tspa_interface  = record
    type_ : pchar;
    version : tuint32_t ;
    cb : tspa_callbacks ;
  end;
  Pspa_interface = ^tspa_interface;


  PTspa_handle = ^Tspa_handle;
  Tspa_handle = record
    version : Tuint32_t;
    get_interface : function (handle:PTspa_handle; _type:Pchar; interface_:Ppointer):longint;cdecl;
    clear : function (handle:PTspa_handle):longint;cdecl;
  end;

  Tspa_interface_info = record
    _type : pchar;
  end;
  PTspa_interface_info = pspa_interface_info;

  Tspa_support = record
    _type : pchar;
    data : pointer;
  end;

  PTspa_handle_factory = ^Tspa_handle_factory;
  PTspa_support = Pspa_support;
  PPTspa_interface_info = ^PTspa_interface_info;
  PPTspa_handle_factory = ^PTspa_handle_factory;
  Tspa_handle_factory = record
    version : Tuint32_t;
    name : pchar;
    info : PTspa_dict;
    get_size : function (factory:PTspa_handle_factory; params:PTspa_dict):Tsize_t;cdecl;
    init : function (factory:PTspa_handle_factory; handle:PTspa_handle; info:PTspa_dict; support:PTspa_support; n_support:Tuint32_t):longint;cdecl;
    enum_interface_info : function (factory:PTspa_handle_factory; info:PPTspa_interface_info; index:PTuint32_t):longint;cdecl;
  end;



  Tspa_handle_factory_enum_func_t = function (factory:PPTspa_handle_factory; index:PTuint32_t):longint;cdecl;


// This needs to be defined in a plugin
// function spa_handle_factory_enum(factory:PPTspa_handle_factory; index:PTuint32_t):longint;


  Tpw_array = record
    data : pointer;
    size : Tsize_t;
    alloc : Tsize_t;
    extend : Tsize_t;
  end;



  Tspa_param_info = record
    id : Tuint32_t;
    flags : Tuint32_t;
    user : Tuint32_t;
    padding : array[0..4] of Tuint32_t;
  end;
  Pspa_param_info = ^Tspa_param_info;


  Tspa_list = record
    next : ^Tspa_list;
    prev : ^Tspa_list;
  end;
  pspa_list = ^tspa_list;

  tspa_ringbuffer = record
      readindex : tuint32_t;
      writeindex : tuint32_t;
    end;
  Pspa_ringbuffer = ^Tspa_ringbuffer;

  Tspa_system = record
    iface : Tspa_interface;
  end;
  Pspa_system = ^Tspa_system;
  PTspa_system = Pspa_system;

  PTspa_poll_event = ^Tspa_poll_event;
  Tspa_poll_event = record
    events : Tuint32_t;
    data : pointer;
  end;

  Titimerspec = record end;
  PTitimerspec = ^Titimerspec;

  Tspa_system_methods = record
    version : Tuint32_t;
    read : function (object_:pointer; fd:longint; buf:pointer; count:Tsize_t):Tssize_t;cdecl;
    write : function (object_:pointer; fd:longint; buf:pointer; count:Tsize_t):Tssize_t;cdecl;
    ioctl : function (object_:pointer; fd:longint; request:dword; args:array of const):longint;cdecl;
    close : function (object_:pointer; fd:longint):longint;cdecl;
    clock_gettime : function (object_:pointer; clockid:longint; value:PTtimespec):longint;cdecl;
    clock_getres : function (object_:pointer; clockid:longint; res:PTtimespec):longint;cdecl;
    pollfd_create : function (object_:pointer; flags:longint):longint;cdecl;
    pollfd_add : function (object_:pointer; pfd:longint; fd:longint; events:Tuint32_t; data:pointer):longint;cdecl;
    pollfd_mod : function (object_:pointer; pfd:longint; fd:longint; events:Tuint32_t; data:pointer):longint;cdecl;
    pollfd_del : function (object_:pointer; pfd:longint; fd:longint):longint;cdecl;
    pollfd_wait : function (object_:pointer; pfd:longint; ev:PTspa_poll_event; n_ev:longint; timeout:longint):longint;cdecl;
    timerfd_create : function (object_:pointer; clockid:longint; flags:longint):longint;cdecl;
    timerfd_settime : function (object_:pointer; fd:longint; flags:longint; new_value:PTitimerspec; old_value:PTitimerspec):longint;cdecl;
    timerfd_gettime : function (object_:pointer; fd:longint; curr_value:PTitimerspec):longint;cdecl;
    timerfd_read : function (object_:pointer; fd:longint; expirations:PTuint64_t):longint;cdecl;
    eventfd_create : function (object_:pointer; flags:longint):longint;cdecl;
    eventfd_write : function (object_:pointer; fd:longint; count:Tuint64_t):longint;cdecl;
    eventfd_read : function (object_:pointer; fd:longint; count:PTuint64_t):longint;cdecl;
    signalfd_create : function (object_:pointer; signal:longint; flags:longint):longint;cdecl;
    signalfd_read : function (object_:pointer; fd:longint; signal:Plongint):longint;cdecl;
  end;

  Tspa_loop = record
    iface : Tspa_interface;
  end;
  PTspa_loop = ^Tspa_loop;

  Tspa_loop_control = record
    iface : Tspa_interface;
  end;
  pspa_loop_control = ^tspa_loop_control;

  Tspa_loop_utils = record
    iface : Tspa_interface;
  end;
  Pspa_loop_utils = ^tspa_loop_utils;


  PTspa_source = ^Tspa_source;
  Tspa_source_func_t = procedure (source:PTspa_source);cdecl;
  Tspa_source = record
    loop : ^Tspa_loop;
    func : Tspa_source_func_t;
    data : pointer;
    fd : longint;
    mask : Tuint32_t;
    rmask : Tuint32_t;
    priv : pointer;
  end;



  Tspa_invoke_func_t = function (loop:PTspa_loop; async:T_Bool; seq:Tuint32_t; data:pointer; size:Tsize_t;
               user_data:pointer):longint;cdecl;

  Tspa_loop_methods = record
    version : Tuint32_t;
    add_source : function (object_:pointer; source:PTspa_source):longint;cdecl;
    update_source : function (object_:pointer; source:PTspa_source):longint;cdecl;
    remove_source : function (object_:pointer; source:PTspa_source):longint;cdecl;
    invoke : function (object_:pointer; func:Tspa_invoke_func_t; seq:Tuint32_t; data:pointer; size:Tsize_t;
                 block:T_Bool; user_data:pointer):longint;cdecl;
  end;

  Tspa_loop_control_hooks = record
    version : Tuint32_t;
    before : procedure (data:pointer);cdecl;
    after : procedure (data:pointer);cdecl;
  end;

  PTspa_hook = Pspa_hook;
  PTspa_loop_control_hooks = Pspa_loop_control_hooks;

  Tspa_loop_control_methods = record
    version : Tuint32_t;
    get_fd : function (object_:pointer):longint;cdecl;
    add_hook : procedure (object_:pointer; hook:PTspa_hook; hooks:PTspa_loop_control_hooks; data:pointer);cdecl;
    enter : procedure (object_:pointer);cdecl;
    leave : procedure (object_:pointer);cdecl;
    iterate : function (object_:pointer; timeout:longint):longint;cdecl;
  end;


  Tspa_source_io_func_t = procedure (data:pointer; fd:longint; mask:Tuint32_t);cdecl;

  Tspa_source_idle_func_t = procedure (data:pointer);cdecl;

  Tspa_source_event_func_t = procedure (data:pointer; count:Tuint64_t);cdecl;

  Tspa_source_timer_func_t = procedure (data:pointer; expirations:Tuint64_t);cdecl;

  Tspa_source_signal_func_t = procedure (data:pointer; signal_number:longint);cdecl;
  Tspa_loop_utils_methods = record
    version : Tuint32_t;
    add_io : function (object_:pointer; fd:longint; mask:Tuint32_t; close:T_Bool; func:Tspa_source_io_func_t;
                 data:pointer):PTspa_source;cdecl;
    update_io : function (object_:pointer; source:PTspa_source; mask:Tuint32_t):longint;cdecl;
    add_idle : function (object_:pointer; enabled:T_Bool; func:Tspa_source_idle_func_t; data:pointer):PTspa_source;cdecl;
    enable_idle : function (object_:pointer; source:PTspa_source; enabled:T_Bool):longint;cdecl;
    add_event : function (object_:pointer; func:Tspa_source_event_func_t; data:pointer):PTspa_source;cdecl;
    signal_event : function (object_:pointer; source:PTspa_source):longint;cdecl;
    add_timer : function (object_:pointer; func:Tspa_source_timer_func_t; data:pointer):PTspa_source;cdecl;
    update_timer : function (object_:pointer; source:PTspa_source; value:PTtimespec; interval:PTtimespec; absolute:T_Bool):longint;cdecl;
    add_signal : function (object_:pointer; signal_number:longint; func:Tspa_source_signal_func_t; data:pointer):PTspa_source;cdecl;
    destroy_source : procedure (object_:pointer; source:PTspa_source);cdecl;
  end;
  Pspa_loop_utils_methods = ^Tspa_loop_utils_methods;

  Tspa_type_info = record
    _type : Tuint32_t;
    parent : Tuint32_t;
    name : ^char;
    values : ^Tspa_type_info;
  end;
  Pspa_type_info =  ^Tspa_type_info;
  Tspa_pod = record
    size : Tuint32_t;
    _type : Tuint32_t;
  end;

  Tspa_pod_bool = record
    pod : Tspa_pod;
    value : Tint32_t;
    _padding : Tint32_t;
  end;

  Tspa_pod_id = record
    pod : Tspa_pod;
    value : Tuint32_t;
    _padding : Tint32_t;
  end;

  Tspa_pod_int = record
    pod : Tspa_pod;
    value : Tint32_t;
    _padding : Tint32_t;
  end;

  Tspa_pod_long = record
    pod : Tspa_pod;
    value : Tint64_t;
  end;

  Tspa_pod_float = record
    pod : Tspa_pod;
    value : single;
    _padding : Tint32_t;
  end;

  Tspa_pod_double = record
    pod : Tspa_pod;
    value : Tdouble;
  end;

  Tspa_pod_string = record
    pod : Tspa_pod;
  end;

  Tspa_pod_bytes = record
    pod : Tspa_pod;
  end;

  Tspa_pod_rectangle = record
    pod : Tspa_pod;
    value : Tspa_rectangle;
  end;

  Tspa_pod_fraction = record
    pod : Tspa_pod;
    value : Tspa_fraction;
  end;

  Tspa_pod_bitmap = record
    pod : Tspa_pod;
  end;

  Tspa_pod_array_body = record
    child : Tspa_pod;
  end;

  Tspa_pod_array = record
    pod : Tspa_pod;
    body : Tspa_pod_array_body;
  end;

  Tspa_pod_choice_body = record
    _type : Tuint32_t;
    flags : Tuint32_t;
    child : Tspa_pod;
  end;
  pspa_pod_choice_body = ^tspa_pod_choice_body;

  Tspa_pod_choice = record
    pod : Tspa_pod;
    body : Tspa_pod_choice_body;
  end;
  Pspa_pod_choice = ^Tspa_pod_choice;

  Tspa_pod_struct = record
    pod : Tspa_pod;
  end;
  pspa_pod_struct = ^Tspa_pod_struct;

  Tspa_pod_object_body = record
    _type : Tuint32_t;
    id : Tuint32_t;
  end;
  Pspa_pod_object_body = ^Tspa_pod_object_body;

  Tspa_pod_object = record
    pod : Tspa_pod;
    body : Tspa_pod_object_body;
  end;
  Pspa_pod_object = ^Tspa_pod_object;

  Tspa_pod_pointer_body = record
    _type : Tuint32_t;
    _padding : Tuint32_t;
    value : pointer;
  end;
  Pspa_pod_pointer_body = ^Tspa_pod_pointer_body;

  Tspa_pod_pointer = record
    pod : Tspa_pod;
    body : Tspa_pod_pointer_body;
  end;
  Pspa_pod_pointer = ^Tspa_pod_pointer;

  Tspa_pod_fd = record
    pod : Tspa_pod;
    value : Tint64_t;
  end;
  Pspa_pod_fd = ^Tspa_pod_fd;

  Tspa_pod_prop = record
    key : Tuint32_t;
    flags : Tuint32_t;
    value : Tspa_pod;
  end;
  pspa_pod_prop = ^Tspa_pod_prop;

  Tspa_pod_control = record
    offset : Tuint32_t;
    _type : Tuint32_t;
    value : Tspa_pod;
  end;
  pspa_pod_control = ^Tspa_pod_control;

  Tspa_pod_sequence_body = record
    unit_ : Tuint32_t;
    pad : Tuint32_t;
  end;
  Pspa_pod_sequence_body = ^Tspa_pod_sequence_body;

  Tspa_pod_sequence = record
    pod : Tspa_pod;
    body : Tspa_pod_sequence_body;
  end;
  Pspa_pod_sequence = ^Tspa_pod_sequence;


  Tspa_meta = record
    _type : Tuint32_t;
    size : Tuint32_t;
    data : pointer;
  end;
  Pspa_meta = ^tspa_meta;
  Tspa_meta_header = record
    flags : Tuint32_t;
    offset : Tuint32_t;
    pts : Tint64_t;
    dts_offset : Tint64_t;
    seq : Tuint64_t;
  end;

  Tspa_meta_region = record
    region : Tspa_region;
  end;
  Pspa_meta_region = ^Tspa_meta_region;
  PTspa_meta_region = Pspa_meta_region;

  Tspa_meta_bitmap = record
    format : Tuint32_t;
    size : Tspa_rectangle;
    stride : Tint32_t;
    offset : Tuint32_t;
  end;
  PTspa_meta_bitmap = ^Tspa_meta_bitmap;
  Pspa_meta_bitmap = PTspa_meta_bitmap;

  Tspa_meta_cursor = record
    id : Tuint32_t;
    flags : Tuint32_t;
    position : Tspa_point;
    hotspot : Tspa_point;
    bitmap_offset : Tuint32_t;
  end;
  Pspa_meta_cursor = ^Tspa_meta_cursor;
  PTspa_meta_cursor = Pspa_meta_cursor;

  Tspa_meta_control = record
    sequence : Tspa_pod_sequence;
  end;

  Tspa_meta_busy = record
    flags : Tuint32_t;
    count : Tuint32_t;
  end;



  Tspa_chunk = record
    offset : Tuint32_t;
    size : Tuint32_t;
    stride : Tint32_t;
    flags : Tint32_t;
  end;
  Pspa_chunk = ^Tspa_chunk;

  Pspa_data = ^Tspa_data;
  Tspa_data = record
    _type : Tuint32_t;
    flags : Tuint32_t;
    fd : Tint64_t;
    mapoffset : Tuint32_t;
    maxsize : Tuint32_t;
    data : pointer;
    chunk : ^Tspa_chunk;
  end;

  Tspa_buffer = record
    n_metas : Tuint32_t;
    n_datas : Tuint32_t;
    metas : ^Tspa_meta;
    datas : ^Tspa_data;
  end;

  Tspa_event_body = record
    body : Tspa_pod_object_body;
  end;

  Tspa_event = record
    pod : Tspa_pod;
    body : Tspa_event_body;
  end;



  Tspa_command_body = record
    body : Tspa_pod_object_body;
  end;

  Tspa_command = record
    pod : Tspa_pod;
    body : Tspa_command_body;
  end;


  Tspa_node = record
    iface : Tspa_interface;
  end;

  Tspa_node_info = record
    max_input_ports : Tuint32_t;
    max_output_ports : Tuint32_t;
    change_mask : Tuint64_t;
    flags : Tuint64_t;
    props : ^Tspa_dict;
    params : ^Tspa_param_info;
    n_params : Tuint32_t;
  end;


  Tspa_port_info = record
    change_mask : Tuint64_t;
    flags : Tuint64_t;
    rate : Tspa_fraction;
    props : ^Tspa_dict;
    params : ^Tspa_param_info;
    n_params : Tuint32_t;
  end;

  Tspa_result_node_error = record
    message : ^char;
  end;

  Tspa_result_node_params = record
    id : Tuint32_t;
    index : Tuint32_t;
    next : Tuint32_t;
    param : ^Tspa_pod;
  end;

  PTspa_node_info =  Pspa_node_info;
  PTspa_port_info = Pspa_port_info;
  PTspa_event = Pspa_event;
  PTspa_pod = Pspa_pod;

  Tspa_node_events = record
    version : Tuint32_t;
    info : procedure (data:pointer; info: PTspa_node_info);cdecl;
    port_info : procedure (data:pointer; direction:Tspa_direction; port:Tuint32_t; info:PTspa_port_info);cdecl;
    result : procedure (data:pointer; seq:longint; res:longint; _type:Tuint32_t; result:pointer);cdecl;
    event : procedure (data:pointer; event:PTspa_event);cdecl;
  end;
  PTspa_node_events = Pspa_node_events;

  Tspa_node_callbacks = record
    version : Tuint32_t;
    ready : function (data:pointer; state:longint):longint;cdecl;
    reuse_buffer : function (data:pointer; port_id:Tuint32_t; buffer_id:Tuint32_t):longint;cdecl;
    xrun : function (data:pointer; trigger:Tuint64_t; delay:Tuint64_t; info:PTspa_pod):longint;cdecl;
  end;

  PTspa_node_callbacks = pspa_node_callbacks;
  PTspa_command = Pspa_command;
  PPTspa_buffer = ^PTspa_buffer;
  PTspa_buffer = ^Tspa_buffer;

  Tspa_node_methods = record
    version : Tuint32_t;
    add_listener : function (object_:pointer; listener:PTspa_hook; events:PTspa_node_events; data:pointer):longint;cdecl;
    set_callbacks : function (object_:pointer; callbacks:PTspa_node_callbacks; data:pointer):longint;cdecl;
    sync : function (object_:pointer; seq:longint):longint;cdecl;
    enum_params : function (object_:pointer; seq:longint; id:Tuint32_t; start:Tuint32_t; max:Tuint32_t;
                 filter:PTspa_pod):longint;cdecl;
    set_param : function (object_:pointer; id:Tuint32_t; flags:Tuint32_t; param:PTspa_pod):longint;cdecl;
    set_io : function (object_:pointer; id:Tuint32_t; data:pointer; size:Tsize_t):longint;cdecl;
    send_command : function (object_:pointer; command:PTspa_command):longint;cdecl;
    add_port : function (object_:pointer; direction:Tspa_direction; port_id:Tuint32_t; props:PTspa_dict):longint;cdecl;
    remove_port : function (object_:pointer; direction:Tspa_direction; port_id:Tuint32_t):longint;cdecl;
    port_enum_params : function (object_:pointer; seq:longint; direction:Tspa_direction; port_id:Tuint32_t; id:Tuint32_t;
                 start:Tuint32_t; max:Tuint32_t; filter:PTspa_pod):longint;cdecl;
    port_set_param : function (object_:pointer; direction:Tspa_direction; port_id:Tuint32_t; id:Tuint32_t; flags:Tuint32_t;
                 param:PTspa_pod):longint;cdecl;
    port_use_buffers : function (object_:pointer; direction:Tspa_direction; port_id:Tuint32_t; flags:Tuint32_t; buffers:PPTspa_buffer;
                 n_buffers:Tuint32_t):longint;cdecl;
    port_set_io : function (object_:pointer; direction:Tspa_direction; port_id:Tuint32_t; id:Tuint32_t; data:pointer;
                 size:Tsize_t):longint;cdecl;
    port_reuse_buffer : function (object_:pointer; port_id:Tuint32_t; buffer_id:Tuint32_t):longint;cdecl;
    process : function (object_:pointer):longint;cdecl;
  end;

  Tspa_log = record
    iface : Tspa_interface;
    level : Tspa_log_level;
  end;


  Tspa_log_topic = record
    version : Tuint32_t;
    topic : ^char;
    level : Tspa_log_level;
    has_custom_level : T_Bool;
  end;

  PTspa_log_topic = Pspa_log_topic;

  Tspa_log_methods = record
    version : Tuint32_t;
    log : procedure (object_:pointer; level:Tspa_log_level; file_:Pchar; line:longint; func:Pchar;
                  fmt:Pchar; args:array of const);cdecl;
    logv : procedure (object_:pointer; level:Tspa_log_level; file_:Pchar; line:longint; func:Pchar;
                  fmt:Pchar; args: pointer);cdecl;
    logt : procedure (object_:pointer; level:Tspa_log_level; topic: PTspa_log_topic; file_:Pchar; line:longint;
                  func:Pchar; fmt:Pchar; args:array of const);cdecl;
    logtv : procedure (object_:pointer; level:Tspa_log_level; topic:PTspa_log_topic; file_:Pchar; line:longint;
                  func:Pchar; fmt:Pchar; args:pointer);cdecl;
    topic_init : procedure (object_:pointer; topic:PTspa_log_topic);cdecl;
  end;


  Tspa_io_buffers = record
    status : Tint32_t;
    buffer_id : Tuint32_t;
  end;

  Tspa_io_memory = record
    status : Tint32_t;
    size : Tuint32_t;
    data : pointer;
  end;

  Tspa_io_range = record
    offset : Tuint64_t;
    min_size : Tuint32_t;
    max_size : Tuint32_t;
  end;

  Tspa_io_clock = record
    flags : Tuint32_t;
    id : Tuint32_t;
    name : array[0..63] of char;
    nsec : Tuint64_t;
    rate : Tspa_fraction;
    position : Tuint64_t;
    duration : Tuint64_t;
    delay : Tint64_t;
    rate_diff : Tdouble;
    next_nsec : Tuint64_t;
    padding : array[0..7] of Tuint32_t;
  end;

  Tspa_io_video_size = record
    flags : Tuint32_t;
    stride : Tuint32_t;
    size : Tspa_rectangle;
    framerate : Tspa_fraction;
    padding : array[0..3] of Tuint32_t;
  end;

  Tspa_io_latency = record
    rate : Tspa_fraction;
    min : Tuint64_t;
    max : Tuint64_t;
  end;

  Tspa_io_sequence = record
    sequence : Tspa_pod_sequence;
  end;

  Tspa_io_segment_bar = record
    flags : Tuint32_t;
    offset : Tuint32_t;
    signature_num : single;
    signature_denom : single;
    bpm : Tdouble;
    beat : Tdouble;
    padding : array[0..7] of Tuint32_t;
  end;

  Tspa_io_segment_video = record
    flags : Tuint32_t;
    offset : Tuint32_t;
    framerate : Tspa_fraction;
    hours : Tuint32_t;
    minutes : Tuint32_t;
    seconds : Tuint32_t;
    frames : Tuint32_t;
    field_count : Tuint32_t;
    padding : array[0..10] of Tuint32_t;
  end;

  Tspa_io_segment = record
    version : Tuint32_t;
    flags : Tuint32_t;
    start : Tuint64_t;
    duration : Tuint64_t;
    rate : Tdouble;
    position : Tuint64_t;
    bar : Tspa_io_segment_bar;
    video : Tspa_io_segment_video;
  end;



  Tspa_io_position = record
    clock : Tspa_io_clock;
    video : Tspa_io_video_size;
    offset : Tint64_t;
    state : Tuint32_t;
    n_segments : Tuint32_t;
    segments : array[0..7] of Tspa_io_segment;
  end;
  Pspa_io_position = ^Tspa_io_position;

  Tspa_io_rate_match = record
    delay : Tuint32_t;
    size : Tuint32_t;
    rate : Tdouble;
    flags : Tuint32_t;
    padding : array[0..6] of Tuint32_t;
  end;

  tspa_hook = record
    link : tspa_list ;
    cb: tspa_callbacks;
    removed : procedure (hook: pspa_hook);
    priv : pointer;
  end;

  tspa_hook_list = record
    list : tspa_list ;
  end;

  Pspa_node = ^Tspa_node;
  Pspa_log = ^Tspa_log;
  Pspa_hook_list = ^Tspa_hook_list;
  Tspa_thread = record end;
  Pspa_thread = ^Tspa_thread;


const
  SPA_VIDEO_MAX_PLANES = 4;
  SPA_VIDEO_MAX_COMPONENTS = 4;

type
  Tspa_video_multiview_mode =  Longint;
Const
  SPA_VIDEO_MULTIVIEW_MODE_NONE = -(1);
  SPA_VIDEO_MULTIVIEW_MODE_MONO = 0;
  SPA_VIDEO_MULTIVIEW_MODE_LEFT = 1;
  SPA_VIDEO_MULTIVIEW_MODE_RIGHT = 2;
  SPA_VIDEO_MULTIVIEW_MODE_SIDE_BY_SIDE = 3;
  SPA_VIDEO_MULTIVIEW_MODE_SIDE_BY_SIDE_QUINCUNX = 4;
  SPA_VIDEO_MULTIVIEW_MODE_COLUMN_INTERLEAVED = 5;
  SPA_VIDEO_MULTIVIEW_MODE_ROW_INTERLEAVED = 6;
  SPA_VIDEO_MULTIVIEW_MODE_TOP_BOTTOM = 7;
  SPA_VIDEO_MULTIVIEW_MODE_CHECKERBOARD = 8;
  SPA_VIDEO_MULTIVIEW_MODE_FRAME_BY_FRAME = 32;
  SPA_VIDEO_MULTIVIEW_MODE_MULTIVIEW_FRAME_BY_FRAME = 33;
  SPA_VIDEO_MULTIVIEW_MODE_SEPARATED = 34;

type
  Tspa_video_multiview_flags =  Longint;

Const
  SPA_VIDEO_MULTIVIEW_FLAGS_NONE = 0;
  SPA_VIDEO_MULTIVIEW_FLAGS_RIGHT_VIEW_FIRST = 1 shl 0;
  SPA_VIDEO_MULTIVIEW_FLAGS_LEFT_FLIPPED = 1 shl 1;
  SPA_VIDEO_MULTIVIEW_FLAGS_LEFT_FLOPPED = 1 shl 2;
  SPA_VIDEO_MULTIVIEW_FLAGS_RIGHT_FLIPPED = 1 shl 3;
  SPA_VIDEO_MULTIVIEW_FLAGS_RIGHT_FLOPPED = 1 shl 4;
  SPA_VIDEO_MULTIVIEW_FLAGS_HALF_ASPECT = 1 shl 14;
  SPA_VIDEO_MULTIVIEW_FLAGS_MIXED_MONO = 1 shl 15;

type
  Tspa_video_format =  Longint;
Const
  SPA_VIDEO_FORMAT_UNKNOWN = 0;
  SPA_VIDEO_FORMAT_ENCODED = 1;
  SPA_VIDEO_FORMAT_I420 = 2;
  SPA_VIDEO_FORMAT_YV12 = 3;
  SPA_VIDEO_FORMAT_YUY2 = 4;
  SPA_VIDEO_FORMAT_UYVY = 5;
  SPA_VIDEO_FORMAT_AYUV = 6;
  SPA_VIDEO_FORMAT_RGBx = 7;
  SPA_VIDEO_FORMAT_BGRx = 8;
  SPA_VIDEO_FORMAT_xRGB = 9;
  SPA_VIDEO_FORMAT_xBGR = 10;
  SPA_VIDEO_FORMAT_RGBA = 11;
  SPA_VIDEO_FORMAT_BGRA = 12;
  SPA_VIDEO_FORMAT_ARGB = 13;
  SPA_VIDEO_FORMAT_ABGR = 14;
  SPA_VIDEO_FORMAT_RGB = 15;
  SPA_VIDEO_FORMAT_BGR = 16;
  SPA_VIDEO_FORMAT_Y41B = 17;
  SPA_VIDEO_FORMAT_Y42B = 18;
  SPA_VIDEO_FORMAT_YVYU = 19;
  SPA_VIDEO_FORMAT_Y444 = 20;
  SPA_VIDEO_FORMAT_v210 = 21;
  SPA_VIDEO_FORMAT_v216 = 22;
  SPA_VIDEO_FORMAT_NV12 = 23;
  SPA_VIDEO_FORMAT_NV21 = 24;
  SPA_VIDEO_FORMAT_GRAY8 = 25;
  SPA_VIDEO_FORMAT_GRAY16_BE = 26;
  SPA_VIDEO_FORMAT_GRAY16_LE = 27;
  SPA_VIDEO_FORMAT_v308 = 28;
  SPA_VIDEO_FORMAT_RGB16 = 29;
  SPA_VIDEO_FORMAT_BGR16 = 30;
  SPA_VIDEO_FORMAT_RGB15 = 31;
  SPA_VIDEO_FORMAT_BGR15 = 32;
  SPA_VIDEO_FORMAT_UYVP = 33;
  SPA_VIDEO_FORMAT_A420 = 34;
  SPA_VIDEO_FORMAT_RGB8P = 35;
  SPA_VIDEO_FORMAT_YUV9 = 36;
  SPA_VIDEO_FORMAT_YVU9 = 37;
  SPA_VIDEO_FORMAT_IYU1 = 38;
  SPA_VIDEO_FORMAT_ARGB64 = 39;
  SPA_VIDEO_FORMAT_AYUV64 = 40;
  SPA_VIDEO_FORMAT_r210 = 41;
  SPA_VIDEO_FORMAT_I420_10BE = 42;
  SPA_VIDEO_FORMAT_I420_10LE = 43;
  SPA_VIDEO_FORMAT_I422_10BE = 44;
  SPA_VIDEO_FORMAT_I422_10LE = 45;
  SPA_VIDEO_FORMAT_Y444_10BE = 46;
  SPA_VIDEO_FORMAT_Y444_10LE = 47;
  SPA_VIDEO_FORMAT_GBR = 48;
  SPA_VIDEO_FORMAT_GBR_10BE = 49;
  SPA_VIDEO_FORMAT_GBR_10LE = 50;
  SPA_VIDEO_FORMAT_NV16 = 51;
  SPA_VIDEO_FORMAT_NV24 = 52;
  SPA_VIDEO_FORMAT_NV12_64Z32 = 53;
  SPA_VIDEO_FORMAT_A420_10BE = 54;
  SPA_VIDEO_FORMAT_A420_10LE = 55;
  SPA_VIDEO_FORMAT_A422_10BE = 56;
  SPA_VIDEO_FORMAT_A422_10LE = 57;
  SPA_VIDEO_FORMAT_A444_10BE = 58;
  SPA_VIDEO_FORMAT_A444_10LE = 59;
  SPA_VIDEO_FORMAT_NV61 = 60;
  SPA_VIDEO_FORMAT_P010_10BE = 61;
  SPA_VIDEO_FORMAT_P010_10LE = 62;
  SPA_VIDEO_FORMAT_IYU2 = 63;
  SPA_VIDEO_FORMAT_VYUY = 64;
  SPA_VIDEO_FORMAT_GBRA = 65;
  SPA_VIDEO_FORMAT_GBRA_10BE = 66;
  SPA_VIDEO_FORMAT_GBRA_10LE = 67;
  SPA_VIDEO_FORMAT_GBR_12BE = 68;
  SPA_VIDEO_FORMAT_GBR_12LE = 69;
  SPA_VIDEO_FORMAT_GBRA_12BE = 70;
  SPA_VIDEO_FORMAT_GBRA_12LE = 71;
  SPA_VIDEO_FORMAT_I420_12BE = 72;
  SPA_VIDEO_FORMAT_I420_12LE = 73;
  SPA_VIDEO_FORMAT_I422_12BE = 74;
  SPA_VIDEO_FORMAT_I422_12LE = 75;
  SPA_VIDEO_FORMAT_Y444_12BE = 76;
  SPA_VIDEO_FORMAT_Y444_12LE = 77;
  SPA_VIDEO_FORMAT_RGBA_F16 = 78;
  SPA_VIDEO_FORMAT_RGBA_F32 = 79;
  SPA_VIDEO_FORMAT_xRGB_210LE = 80;
  SPA_VIDEO_FORMAT_xBGR_210LE = 81;
  SPA_VIDEO_FORMAT_RGBx_102LE = 82;
  SPA_VIDEO_FORMAT_BGRx_102LE = 83;
  SPA_VIDEO_FORMAT_ARGB_210LE = 84;
  SPA_VIDEO_FORMAT_ABGR_210LE = 85;
  SPA_VIDEO_FORMAT_RGBA_102LE = 86;
  SPA_VIDEO_FORMAT_BGRA_102LE = 87;
  SPA_VIDEO_FORMAT_DSP_F32 = SPA_VIDEO_FORMAT_RGBA_F32;


type
  Tspa_video_flags =  Longint;
Const
  SPA_VIDEO_FLAG_NONE = 0;
  SPA_VIDEO_FLAG_VARIABLE_FPS = 1 shl 0;
  SPA_VIDEO_FLAG_PREMULTIPLIED_ALPHA = 1 shl 1;

Type
  Tspa_video_interlace_mode =  Longint;
Const
  SPA_VIDEO_INTERLACE_MODE_PROGRESSIVE = 0;
  SPA_VIDEO_INTERLACE_MODE_INTERLEAVED = 1;
  SPA_VIDEO_INTERLACE_MODE_MIXED = 2;
  SPA_VIDEO_INTERLACE_MODE_FIELDS = 3;

type
  Tspa_video_chroma_site =  Longint;
Const
  SPA_VIDEO_CHROMA_SITE_UNKNOWN = 0;
  SPA_VIDEO_CHROMA_SITE_NONE = 1 shl 0;
  SPA_VIDEO_CHROMA_SITE_H_COSITED = 1 shl 1;
  SPA_VIDEO_CHROMA_SITE_V_COSITED = 1 shl 2;
  SPA_VIDEO_CHROMA_SITE_ALT_LINE = 1 shl 3;
  SPA_VIDEO_CHROMA_SITE_COSITED = SPA_VIDEO_CHROMA_SITE_H_COSITED or SPA_VIDEO_CHROMA_SITE_V_COSITED;
  SPA_VIDEO_CHROMA_SITE_JPEG = SPA_VIDEO_CHROMA_SITE_NONE;
  SPA_VIDEO_CHROMA_SITE_MPEG2 = SPA_VIDEO_CHROMA_SITE_H_COSITED;
  SPA_VIDEO_CHROMA_SITE_DV = SPA_VIDEO_CHROMA_SITE_COSITED or SPA_VIDEO_CHROMA_SITE_ALT_LINE;

type
    Tspa_video_color_range =  Longint;
    Const
      SPA_VIDEO_COLOR_RANGE_UNKNOWN = 0;
      SPA_VIDEO_COLOR_RANGE_0_255 = 1;
      SPA_VIDEO_COLOR_RANGE_16_235 = 2;

  {*
   * The color matrix is used to convert between Y'PbPr and
   * non-linear RGB (R'G'B')
    }
  {*< unknown matrix  }
  {*< identity matrix  }
  {*< FCC color matrix  }
  {*< ITU BT.709 color matrix  }
  {*< ITU BT.601 color matrix  }
  {*< SMTPE  240M color matrix  }
  {*<  ITU-R BT.2020 color matrix. since 1.6.  }

  type
    Tspa_video_color_matrix =  Longint;
    Const
      SPA_VIDEO_COLOR_MATRIX_UNKNOWN = 0;
      SPA_VIDEO_COLOR_MATRIX_RGB = 1;
      SPA_VIDEO_COLOR_MATRIX_FCC = 2;
      SPA_VIDEO_COLOR_MATRIX_BT709 = 3;
      SPA_VIDEO_COLOR_MATRIX_BT601 = 4;
      SPA_VIDEO_COLOR_MATRIX_SMPTE240M = 5;
      SPA_VIDEO_COLOR_MATRIX_BT2020 = 6;

  {*
   * The video transfer function defines the formula for converting between
   * non-linear RGB (R'G'B') and linear RGB
    }
  {*< unknown transfer function  }
  {*< linear RGB, gamma 1.0 curve  }
  {*< Gamma 1.8 curve  }
  {*< Gamma 2.0 curve  }
  {*< Gamma 2.2 curve  }
  {*< Gamma 2.2 curve with a linear segment in the lower range  }
  {*< Gamma 2.2 curve with a linear segment in the lower range  }
  {*< Gamma 2.4 curve with a linear segment in the lower range  }
  {*< Gamma 2.8 curve  }
  {*< Logarithmic transfer characteristic 100:1 range  }
  {*< Logarithmic transfer characteristic 316.22777:1 range  }
  {*< Gamma 2.2 curve with a linear segment in the lower
  					 *   range. Used for BT.2020 with 12 bits per
  					 *   component. \since 1.6.  }
  {*< Gamma 2.19921875. \since 1.8  }

  type
    Tspa_video_transfer_function =  Longint;
    Const
      SPA_VIDEO_TRANSFER_UNKNOWN = 0;
      SPA_VIDEO_TRANSFER_GAMMA10 = 1;
      SPA_VIDEO_TRANSFER_GAMMA18 = 2;
      SPA_VIDEO_TRANSFER_GAMMA20 = 3;
      SPA_VIDEO_TRANSFER_GAMMA22 = 4;
      SPA_VIDEO_TRANSFER_BT709 = 5;
      SPA_VIDEO_TRANSFER_SMPTE240M = 6;
      SPA_VIDEO_TRANSFER_SRGB = 7;
      SPA_VIDEO_TRANSFER_GAMMA28 = 8;
      SPA_VIDEO_TRANSFER_LOG100 = 9;
      SPA_VIDEO_TRANSFER_LOG316 = 10;
      SPA_VIDEO_TRANSFER_BT2020_12 = 11;
      SPA_VIDEO_TRANSFER_ADOBERGB = 12;

  {*
   * The color primaries define the how to transform linear RGB values to and from
   * the CIE XYZ colorspace.
    }
  {*< unknown color primaries  }
  {*< BT709 primaries  }
  {*< BT470M primaries  }
  {*< BT470BG primaries  }
  {*< SMPTE170M primaries  }
  {*< SMPTE240M primaries  }
  {*< Generic film  }
  {*< BT2020 primaries. \since 1.6.  }
  {*< Adobe RGB primaries. \since 1.8  }

type
  Tspa_video_color_primaries =  Longint;
Const
  SPA_VIDEO_COLOR_PRIMARIES_UNKNOWN = 0;
  SPA_VIDEO_COLOR_PRIMARIES_BT709 = 1;
  SPA_VIDEO_COLOR_PRIMARIES_BT470M = 2;
  SPA_VIDEO_COLOR_PRIMARIES_BT470BG = 3;
  SPA_VIDEO_COLOR_PRIMARIES_SMPTE170M = 4;
  SPA_VIDEO_COLOR_PRIMARIES_SMPTE240M = 5;
  SPA_VIDEO_COLOR_PRIMARIES_FILM = 6;
  SPA_VIDEO_COLOR_PRIMARIES_BT2020 = 7;
  SPA_VIDEO_COLOR_PRIMARIES_ADOBERGB = 8;

type
  Tspa_video_colorimetry = record
    range : Tspa_video_color_range;
    matrix : Tspa_video_color_matrix;
    transfer : Tspa_video_transfer_function;
    primaries : Tspa_video_color_primaries;
  end;
  Pspa_video_colorimetry = ^Tspa_video_colorimetry;


  Tspa_video_info_raw = record
    format : Tspa_video_format;
    modifier : Tint64_t;
    size : Tspa_rectangle;
    framerate : Tspa_fraction;
    max_framerate : Tspa_fraction;
    views : Tuint32_t;
    interlace_mode : Tspa_video_interlace_mode;
    pixel_aspect_ratio : Tspa_fraction;
    multiview_mode : Tspa_video_multiview_mode;
    multiview_flags : Tspa_video_multiview_flags;
    chroma_site : Tspa_video_chroma_site;
    color_range : Tspa_video_color_range;
    color_matrix : Tspa_video_color_matrix;
    transfer_function : Tspa_video_transfer_function;
    color_primaries : Tspa_video_color_primaries;
  end;
  Pspa_video_info_raw = ^tspa_video_info_raw;

  Tspa_video_info_dsp = record
    format : Tspa_video_format;
    modifier : Tint64_t;
  end;
  Pspa_video_info_dsp = ^Tspa_video_info_dsp;

  Pspa_pod_frame = ^Tspa_pod_frame;
  Tspa_pod_frame = record
          pod : Tspa_pod ;
          parent : Pspa_pod_frame;
          offset : tuint32_t ;
          flags : tuint32_t ;
  end;


  Tspa_pod_parser_state = record
    offset : Tuint32_t;
    flags : Tuint32_t;
    frame : ^Tspa_pod_frame;
  end;

  Tspa_pod_parser = record
    data : pointer;
    size : Tuint32_t;
    _padding : Tuint32_t;
    state : Tspa_pod_parser_state;
  end;
  PTspa_pod_parser = ^Tspa_pod_parser;
  Pspa_pod_parser = ^Tspa_pod_parser;

  Pspa_graph_node  = ^Tspa_graph_node;
  PTspa_graph_node = Pspa_graph_node;

  Tspa_graph_link = record
      {undefined structure}
  end;
  Pspa_graph_link = ^Tspa_graph_link;

  Tspa_graph_state = record
    status : longint;
    required : Tint32_t;
    pending : Tint32_t;
  end;
  Pspa_graph_state = ^Tspa_graph_state;

  Tspa_graph = record
    flags : Tuint32_t;
    parent : pspa_graph_node;
    state : pspa_graph_state;
    nodes : Tspa_list;
  end;
  Pspa_graph = ^Tspa_graph;

  Tspa_graph_node_callbacks = record
    version : Tuint32_t;
    process : function (data:pointer; node:PTspa_graph_node):longint;cdecl;
    reuse_buffer : function (data:pointer; node:PTspa_graph_node; port_id:Tuint32_t; buffer_id:Tuint32_t):longint;cdecl;
  end;
  Pspa_graph_node_callbacks = ^Tspa_graph_node_callbacks;

  Tspa_graph_node = record
    link : Tspa_list;
    graph : ^Tspa_graph;
    ports : array[0..1] of Tspa_list;
    links : Tspa_list;
    flags : Tuint32_t;
    state : ^Tspa_graph_state;
    graph_link : Tspa_graph_link;
    subgraph : ^Tspa_graph;
    callbacks : Tspa_callbacks;
    sched_link : Tspa_list;
  end;

  Tspa_graph_port = record
    link : Tspa_list;
    node : ^Tspa_graph_node;
    direction : Tspa_direction;
    port_id : Tuint32_t;
    flags : Tuint32_t;
    peer : ^Tspa_graph_port;
  end;
  Pspa_graph_port = ^Tspa_graph_port;

  Tspa_pod_builder_state = record
    offset : Tuint32_t;
    flags : Tuint32_t;
    frame : ^Tspa_pod_frame;
  end;
  Pspa_pod_builder_state  = ^Tspa_pod_builder_state ;

  Tspa_pod_builder_callbacks = record
    version : Tuint32_t;
    overflow : function (data:pointer; size:Tuint32_t):longint;cdecl;
  end;
  Pspa_pod_builder_callbacks = ^Tspa_pod_builder_callbacks;

  Tspa_pod_builder = record
    data : pointer;
    size : Tuint32_t;
    _padding : Tuint32_t;
    state : Tspa_pod_builder_state;
    callbacks : Tspa_callbacks;
  end;
  Pspa_pod_builder = ^Tspa_pod_builder;

  Tspa_pod_dynamic_builder = record
    b : Tspa_pod_builder;
    data : pointer;
    extend : Tuint32_t;
    _padding : Tuint32_t;
  end;
  pspa_pod_dynamic_builder = ^tspa_pod_dynamic_builder;

  Pspa_json = ^Tspa_json;
  Tspa_json = record
      cur : pchar;
      end_ : pchar;
      parent : Pspa_json;
      state : Tuint32_t;
      depth : Tuint32_t;
    end;

type
  Tspa_media_type =  Longint;
Const
  SPA_MEDIA_TYPE_unknown = 0;
  SPA_MEDIA_TYPE_audio = 1;
  SPA_MEDIA_TYPE_video = 2;
  SPA_MEDIA_TYPE_image = 3;
  SPA_MEDIA_TYPE_binary = 4;
  SPA_MEDIA_TYPE_stream = 5;
  SPA_MEDIA_TYPE_application = 6;

type
  Tspa_media_subtype =  Longint;
Const
  SPA_MEDIA_SUBTYPE_unknown = 0;
  SPA_MEDIA_SUBTYPE_raw = 1;
  SPA_MEDIA_SUBTYPE_dsp = 2;
  SPA_MEDIA_SUBTYPE_iec958 = 3;
  SPA_MEDIA_SUBTYPE_dsd = 4;
  SPA_MEDIA_SUBTYPE_START_Audio = $10000;
  SPA_MEDIA_SUBTYPE_mp3 = 65537;
  SPA_MEDIA_SUBTYPE_aac = 65538;
  SPA_MEDIA_SUBTYPE_vorbis = 65539;
  SPA_MEDIA_SUBTYPE_wma = 65540;
  SPA_MEDIA_SUBTYPE_ra = 65541;
  SPA_MEDIA_SUBTYPE_sbc = 65542;
  SPA_MEDIA_SUBTYPE_adpcm = 65543;
  SPA_MEDIA_SUBTYPE_g723 = 65544;
  SPA_MEDIA_SUBTYPE_g726 = 65545;
  SPA_MEDIA_SUBTYPE_g729 = 65546;
  SPA_MEDIA_SUBTYPE_amr = 65547;
  SPA_MEDIA_SUBTYPE_gsm = 65548;
  SPA_MEDIA_SUBTYPE_START_Video = $20000;
  SPA_MEDIA_SUBTYPE_h264 = 131073;
  SPA_MEDIA_SUBTYPE_mjpg = 131074;
  SPA_MEDIA_SUBTYPE_dv = 131075;
  SPA_MEDIA_SUBTYPE_mpegts = 131076;
  SPA_MEDIA_SUBTYPE_h263 = 131077;
  SPA_MEDIA_SUBTYPE_mpeg1 = 131078;
  SPA_MEDIA_SUBTYPE_mpeg2 = 131079;
  SPA_MEDIA_SUBTYPE_mpeg4 = 131080;
  SPA_MEDIA_SUBTYPE_xvid = 131081;
  SPA_MEDIA_SUBTYPE_vc1 = 131082;
  SPA_MEDIA_SUBTYPE_vp8 = 131083;
  SPA_MEDIA_SUBTYPE_vp9 = 131084;
  SPA_MEDIA_SUBTYPE_bayer = 131085;
  SPA_MEDIA_SUBTYPE_START_Image = $30000;
  SPA_MEDIA_SUBTYPE_jpeg = 196609;
  SPA_MEDIA_SUBTYPE_START_Binary = $40000;
  SPA_MEDIA_SUBTYPE_START_Stream = $50000;
  SPA_MEDIA_SUBTYPE_midi = 327681;
  SPA_MEDIA_SUBTYPE_START_Application = $60000;
  SPA_MEDIA_SUBTYPE_control = 393217;

type
  Tspa_format =  Longint;
Const
  SPA_FORMAT_START = 0;
  SPA_FORMAT_mediaType = 1;
  SPA_FORMAT_mediaSubtype = 2;
  SPA_FORMAT_START_Audio = $10000;
  SPA_FORMAT_AUDIO_format = 65537;
  SPA_FORMAT_AUDIO_flags = 65538;
  SPA_FORMAT_AUDIO_rate = 65539;
  SPA_FORMAT_AUDIO_channels = 65540;
  SPA_FORMAT_AUDIO_position = 65541;
  SPA_FORMAT_AUDIO_iec958Codec = 65542;
  SPA_FORMAT_AUDIO_bitorder = 65543;
  SPA_FORMAT_AUDIO_interleave = 65544;
  SPA_FORMAT_START_Video = $20000;
  SPA_FORMAT_VIDEO_format = 131073;
  SPA_FORMAT_VIDEO_modifier = 131074;
  SPA_FORMAT_VIDEO_size = 131075;
  SPA_FORMAT_VIDEO_framerate = 131076;
  SPA_FORMAT_VIDEO_maxFramerate = 131077;
  SPA_FORMAT_VIDEO_views = 131078;
  SPA_FORMAT_VIDEO_interlaceMode = 131079;
  SPA_FORMAT_VIDEO_pixelAspectRatio = 131080;
  SPA_FORMAT_VIDEO_multiviewMode = 131081;
  SPA_FORMAT_VIDEO_multiviewFlags = 131082;
  SPA_FORMAT_VIDEO_chromaSite = 131083;
  SPA_FORMAT_VIDEO_colorRange = 131084;
  SPA_FORMAT_VIDEO_colorMatrix = 131085;
  SPA_FORMAT_VIDEO_transferFunction = 131086;
  SPA_FORMAT_VIDEO_colorPrimaries = 131087;
  SPA_FORMAT_VIDEO_profile = 131088;
  SPA_FORMAT_VIDEO_level = 131089;
  SPA_FORMAT_VIDEO_H264_streamFormat = 131090;
  SPA_FORMAT_VIDEO_H264_alignment = 131091;
  SPA_FORMAT_START_Image = $30000;
  SPA_FORMAT_START_Binary = $40000;
  SPA_FORMAT_START_Stream = $50000;
  SPA_FORMAT_START_Application = $60000;

  SPA_KEY_FORMAT_DSP = 'format.dsp';

const
  SPA_POD_PROP_FLAG_READONLY = 1 shl 0;
  SPA_POD_PROP_FLAG_HARDWARE = 1 shl 1;
  SPA_POD_PROP_FLAG_HINT_DICT = 1 shl 2;
  SPA_POD_PROP_FLAG_MANDATORY = 1 shl 3;
  SPA_POD_PROP_FLAG_DONT_FIXATE = 1 shl 4;

{$i spabridge.inc}

Function spa_meta_cursor_is_valid(m : Pspa_meta_cursor) : boolean;


implementation

  function spa_handle_factory_enum(factory:PPTspa_handle_factory; index:PTuint32_t):longint;
  begin
    Result:=0;
  end;

Function spa_meta_cursor_is_valid(m : Pspa_meta_cursor) : boolean;

begin
  Result:=(m^.id <> 0);
end;

end.
