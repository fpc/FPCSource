

type
  Tspa_buffer_alloc_info = record
  end;
  Pspa_buffer_alloc_info = ^Tspa_buffer_alloc_info;

  Tspa_latency_info = record
  end;
  Pspa_latency_info = ^Tspa_latency_info;

  Tspa_process_latency_info = record
  end;
  Pspa_process_latency_info = ^Tspa_process_latency_info;

  Tspa_audio_info_raw = record
  end;
  Pspa_audio_info_raw = ^Tspa_audio_info_raw;

  Tspa_audio_info_dsp = record
  end;
  Pspa_audio_info_dsp = ^Tspa_audio_info_dsp;

  Tspa_audio_info_iec958 = record
  end;
  Pspa_audio_info_iec958 = ^Tspa_audio_info_iec958;

  Tspa_audio_info_dsd = record
  end;
  Pspa_audio_info_dsd = ^Tspa_audio_info_dsd;


  Tspa_video_info_mjpg = record
  end;
  Pspa_video_info_mjpg = ^Tspa_video_info_mjpg;

  Tspa_video_info_h264 = record
  end;
  Pspa_video_info_h264 = ^Tspa_video_info_h264;

  Pspa_pod_parser_state = ^Tspa_pod_parser_state;

  tspa_dbus_connection = record
  end;
  pspa_dbus_connection = ^Tspa_dbus_connection;

  tspa_dbus_type = record
  end;
  pspa_dbus_type = ^tspa_dbus_type;

  tspa_dbus = record
  end;
  pspa_dbus = ^Tspa_dbus;

  Tspa_thread_utils = record
  end;
  Pspa_thread_utils = ^Tspa_thread_utils;

  Tspa_thread_utils_createStart = function(para1: pointer) : pointer; cdecl;

  tspa_plugin_loader = record
  end;
  pspa_plugin_loader = ^tspa_plugin_loader;

  Tspa_i18n = record
  end;
  Pspa_i18n = ^Tspa_i18n;

  Tspa_device = record
  end;
  Pspa_device = ^Tspa_device;

procedure spa_log_impl_topic_init(object_: pointer; topic: Pspa_log_topic); cdecl; external;

function spa_thread_utils_create(o: Pspa_thread_utils; props: Pspa_dict; start_routine: Tspa_thread_utils_createStart; arg: pointer) : Pspa_thread; cdecl; external;

function spa_thread_utils_join(o: Pspa_thread_utils; thread: Pspa_thread; retval: Ppointer) : longint; cdecl; external;

function spa_thread_utils_get_rt_range(o: Pspa_thread_utils; props: Pspa_dict; min: Plongint; max: Plongint) : longint; cdecl; external;

function spa_thread_utils_acquire_rt(o: Pspa_thread_utils; thread: Pspa_thread; priority: longint) : longint; cdecl; external;

function spa_thread_utils_drop_rt(o: Pspa_thread_utils; thread: Pspa_thread) : longint; cdecl; external;

function spa_support_find(support: Pspa_support; n_support: Tuint32_t; _type: PChar) : pointer; cdecl; external;

function spa_dbus_get_connection(dbus: Pspa_dbus; _type: Tspa_dbus_type) : Pspa_dbus_connection; cdecl; external;

function spa_plugin_loader_load(loader: Pspa_plugin_loader; factory_name: PChar; info: Pspa_dict) : Pspa_handle; cdecl; external;

function spa_plugin_loader_unload(loader: Pspa_plugin_loader; handle: Pspa_handle) : longint; cdecl; external;

function spa_i18n_text(i18n: Pspa_i18n; msgid: PChar) : PChar; cdecl; external;

function spa_i18n_ntext(i18n: Pspa_i18n; msgid: PChar; msgid_plural: PChar; n: dword) : PChar; cdecl; external;

procedure spa_graph_state_reset(state: Pspa_graph_state); cdecl; external;

function spa_graph_link_trigger(link: Pspa_graph_link) : longint; cdecl; external;

function spa_graph_node_trigger(node: Pspa_graph_node) : longint; cdecl; external;

function spa_graph_run(graph: Pspa_graph) : longint; cdecl; external;

function spa_graph_finish(graph: Pspa_graph) : longint; cdecl; external;

function spa_graph_link_signal_node(Data: pointer) : longint; cdecl; external;

function spa_graph_link_signal_graph(Data: pointer) : longint; cdecl; external;

procedure spa_graph_init(graph: Pspa_graph; state: Pspa_graph_state); cdecl; external;

procedure spa_graph_link_add(_out: Pspa_graph_node; state: Pspa_graph_state; link: Pspa_graph_link); cdecl; external;

procedure spa_graph_link_remove(link: Pspa_graph_link); cdecl; external;

procedure spa_graph_node_init(node: Pspa_graph_node; state: Pspa_graph_state); cdecl; external;

function spa_graph_node_impl_sub_process(Data: pointer; node: Pspa_graph_node) : longint; cdecl; external;

procedure spa_graph_node_set_subgraph(node: Pspa_graph_node; subgraph: Pspa_graph); cdecl; external;

procedure spa_graph_node_set_callbacks(node: Pspa_graph_node; callbacks: Pspa_graph_node_callbacks; Data: pointer); cdecl; external;

procedure spa_graph_node_add(graph: Pspa_graph; node: Pspa_graph_node); cdecl; external;

procedure spa_graph_node_remove(node: Pspa_graph_node); cdecl; external;

procedure spa_graph_port_init(port: Pspa_graph_port; direction: Tspa_direction; port_id: Tuint32_t; flags: Tuint32_t); cdecl; external;

procedure spa_graph_port_add(node: Pspa_graph_node; port: Pspa_graph_port); cdecl; external;

procedure spa_graph_port_remove(port: Pspa_graph_port); cdecl; external;

procedure spa_graph_port_link(_out: Pspa_graph_port; _in: Pspa_graph_port); cdecl; external;

procedure spa_graph_port_unlink(port: Pspa_graph_port); cdecl; external;

function spa_graph_node_impl_process(Data: pointer; node: Pspa_graph_node) : longint; cdecl; external;

function spa_graph_node_impl_reuse_buffer(Data: pointer; node: Pspa_graph_node; port_id: Tuint32_t; buffer_id: Tuint32_t) : longint; cdecl; external;

function spa_debug_dict(indent: longint; dict: Pspa_dict) : longint; cdecl; external;

function spa_debug_pod_value(indent: longint; info: Pspa_type_info; _type: Tuint32_t; body: pointer; size: Tuint32_t) : longint; cdecl; external;

function spa_debug_pod(indent: longint; info: Pspa_type_info; pod: Pspa_pod) : longint; cdecl; external;

function spa_debug_mem(indent: longint; Data: pointer; size: Tsize_t) : longint; cdecl; external;

function spa_debug_buffer(indent: longint; buffer: Pspa_buffer) : longint; cdecl; external;

function spa_debug_type_find(info: Pspa_type_info; _type: Tuint32_t) : Pspa_type_info; cdecl; external;

function spa_debug_type_short_name(Name: PChar) : PChar; cdecl; external;

function spa_debug_type_find_name(info: Pspa_type_info; _type: Tuint32_t) : PChar; cdecl; external;

function spa_debug_type_find_short_name(info: Pspa_type_info; _type: Tuint32_t) : PChar; cdecl; external;

function spa_debug_type_find_type(info: Pspa_type_info; Name: PChar) : Tuint32_t; cdecl; external;

function spa_debug_type_find_short(info: Pspa_type_info; Name: PChar) : Pspa_type_info; cdecl; external;

function spa_debug_type_find_type_short(info: Pspa_type_info; Name: PChar) : Tuint32_t; cdecl; external;

function spa_debug_format_value(info: Pspa_type_info; _type: Tuint32_t; body: pointer; size: Tuint32_t) : longint; cdecl; external;

function spa_debug_format(indent: longint; info: Pspa_type_info; format: Pspa_pod) : longint; cdecl; external;

function spa_debug_port_info(indent: longint; info: Pspa_port_info) : longint; cdecl; external;

procedure spa_pod_parser_init(parser: Pspa_pod_parser; Data: pointer; size: Tuint32_t); cdecl; external;

procedure spa_pod_parser_pod(parser: Pspa_pod_parser; pod: Pspa_pod); cdecl; external;

procedure spa_pod_parser_get_state(parser: Pspa_pod_parser; state: Pspa_pod_parser_state); cdecl; external;

procedure spa_pod_parser_reset(parser: Pspa_pod_parser; state: Pspa_pod_parser_state); cdecl; external;

function spa_pod_parser_deref(parser: Pspa_pod_parser; offset: Tuint32_t; size: Tuint32_t) : Pspa_pod; cdecl; external;

function spa_pod_parser_frame(parser: Pspa_pod_parser; frame: Pspa_pod_frame) : Pspa_pod; cdecl; external;

procedure spa_pod_parser_push(parser: Pspa_pod_parser; frame: Pspa_pod_frame; pod: Pspa_pod; offset: Tuint32_t); cdecl; external;

function spa_pod_parser_current(parser: Pspa_pod_parser) : Pspa_pod; cdecl; external;

procedure spa_pod_parser_advance(parser: Pspa_pod_parser; pod: Pspa_pod); cdecl; external;

function spa_pod_parser_next(parser: Pspa_pod_parser) : Pspa_pod; cdecl; external;

function spa_pod_parser_pop(parser: Pspa_pod_parser; frame: Pspa_pod_frame) : longint; cdecl; external;

function spa_pod_parser_get_bool(parser: Pspa_pod_parser; Value: Pbool) : longint; cdecl; external;

function spa_pod_parser_get_id(parser: Pspa_pod_parser; Value: Puint32_t) : longint; cdecl; external;

function spa_pod_parser_get_int(parser: Pspa_pod_parser; Value: Pint32_t) : longint; cdecl; external;

function spa_pod_parser_get_long(parser: Pspa_pod_parser; Value: Pint64_t) : longint; cdecl; external;

function spa_pod_parser_get_float(parser: Pspa_pod_parser; Value: Psingle) : longint; cdecl; external;

function spa_pod_parser_get_double(parser: Pspa_pod_parser; Value: Pdouble) : longint; cdecl; external;

function spa_pod_parser_get_string(parser: Pspa_pod_parser; Value: PPchar) : longint; cdecl; external;

function spa_pod_parser_get_bytes(parser: Pspa_pod_parser; Value: Ppointer; len: Puint32_t) : longint; cdecl; external;

function spa_pod_parser_get_pointer(parser: Pspa_pod_parser; _type: Puint32_t; Value: Ppointer) : longint; cdecl; external;

function spa_pod_parser_get_fd(parser: Pspa_pod_parser; Value: Pint64_t) : longint; cdecl; external;

function spa_pod_parser_get_rectangle(parser: Pspa_pod_parser; Value: Pspa_rectangle) : longint; cdecl; external;

function spa_pod_parser_get_fraction(parser: Pspa_pod_parser; Value: Pspa_fraction) : longint; cdecl; external;

function spa_pod_parser_get_pod(parser: Pspa_pod_parser; Value: PPspa_pod) : longint; cdecl; external;

function spa_pod_parser_push_struct(parser: Pspa_pod_parser; frame: Pspa_pod_frame) : longint; cdecl; external;

function spa_pod_parser_push_object(parser: Pspa_pod_parser; frame: Pspa_pod_frame; _type: Tuint32_t; id: Puint32_t) : longint; cdecl; external;

function spa_pod_parser_can_collect(pod: Pspa_pod; _type: char) : Tbool; cdecl; external;

function spa_pod_parser_getv(parser: Pspa_pod_parser; args: pointer) : longint; cdecl; external;

function spa_pod_parser_get(parser: Pspa_pod_parser; args: array of const) : longint; cdecl; external; overload;

function spa_pod_parser_get(parser: Pspa_pod_parser) : longint; cdecl; external; overload;

function spa_pod_is_inside(pod: pointer; size: Tuint32_t; iter: pointer) : Tbool; cdecl; external;

function spa_pod_next(iter: pointer) : pointer; cdecl; external;

function spa_pod_prop_first(body: Pspa_pod_object_body) : Pspa_pod_prop; cdecl; external;

function spa_pod_prop_is_inside(body: Pspa_pod_object_body; size: Tuint32_t; iter: Pspa_pod_prop) : Tbool; cdecl; external;

function spa_pod_prop_next(iter: Pspa_pod_prop) : Pspa_pod_prop; cdecl; external;

function spa_pod_control_first(body: Pspa_pod_sequence_body) : Pspa_pod_control; cdecl; external;

function spa_pod_control_is_inside(body: Pspa_pod_sequence_body; size: Tuint32_t; iter: Pspa_pod_control) : Tbool; cdecl; external;

function spa_pod_control_next(iter: Pspa_pod_control) : Pspa_pod_control; cdecl; external;

function spa_pod_from_data(Data: pointer; maxsize: Tsize_t; offset: Toff_t; size: Tsize_t) : pointer; cdecl; external;

function spa_pod_is_none(pod: Pspa_pod) : longint; cdecl; external;

function spa_pod_is_bool(pod: Pspa_pod) : longint; cdecl; external;

function spa_pod_get_bool(pod: Pspa_pod; Value: Pbool) : longint; cdecl; external;

function spa_pod_is_id(pod: Pspa_pod) : longint; cdecl; external;

function spa_pod_get_id(pod: Pspa_pod; Value: Puint32_t) : longint; cdecl; external;

function spa_pod_is_int(pod: Pspa_pod) : longint; cdecl; external;

function spa_pod_get_int(pod: Pspa_pod; Value: Pint32_t) : longint; cdecl; external;

function spa_pod_is_long(pod: Pspa_pod) : longint; cdecl; external;

function spa_pod_get_long(pod: Pspa_pod; Value: Pint64_t) : longint; cdecl; external;

function spa_pod_is_float(pod: Pspa_pod) : longint; cdecl; external;

function spa_pod_get_float(pod: Pspa_pod; Value: Psingle) : longint; cdecl; external;

function spa_pod_is_double(pod: Pspa_pod) : longint; cdecl; external;

function spa_pod_get_double(pod: Pspa_pod; Value: Pdouble) : longint; cdecl; external;

function spa_pod_is_string(pod: Pspa_pod) : longint; cdecl; external;

function spa_pod_get_string(pod: Pspa_pod; Value: PPchar) : longint; cdecl; external;

function spa_pod_copy_string(pod: Pspa_pod; maxlen: Tsize_t; dest: PChar) : longint; cdecl; external;

function spa_pod_is_bytes(pod: Pspa_pod) : longint; cdecl; external;

function spa_pod_get_bytes(pod: Pspa_pod; Value: Ppointer; len: Puint32_t) : longint; cdecl; external;

function spa_pod_is_pointer(pod: Pspa_pod) : longint; cdecl; external;

function spa_pod_get_pointer(pod: Pspa_pod; _type: Puint32_t; Value: Ppointer) : longint; cdecl; external;

function spa_pod_is_fd(pod: Pspa_pod) : longint; cdecl; external;

function spa_pod_get_fd(pod: Pspa_pod; Value: Pint64_t) : longint; cdecl; external;

function spa_pod_is_rectangle(pod: Pspa_pod) : longint; cdecl; external;

function spa_pod_get_rectangle(pod: Pspa_pod; Value: Pspa_rectangle) : longint; cdecl; external;

function spa_pod_is_fraction(pod: Pspa_pod) : longint; cdecl; external;

function spa_pod_get_fraction(pod: Pspa_pod; Value: Pspa_fraction) : longint; cdecl; external;

function spa_pod_is_bitmap(pod: Pspa_pod) : longint; cdecl; external;

function spa_pod_is_array(pod: Pspa_pod) : longint; cdecl; external;

function spa_pod_get_array(pod: Pspa_pod; n_values: Puint32_t) : pointer; cdecl; external;

function spa_pod_copy_array(pod: Pspa_pod; _type: Tuint32_t; values: pointer; max_values: Tuint32_t) : Tuint32_t; cdecl; external;

function spa_pod_is_choice(pod: Pspa_pod) : longint; cdecl; external;

function spa_pod_get_values(pod: Pspa_pod; n_vals: Puint32_t; choice: Puint32_t) : Pspa_pod; cdecl; external;

function spa_pod_is_struct(pod: Pspa_pod) : longint; cdecl; external;

function spa_pod_is_object(pod: Pspa_pod) : longint; cdecl; external;

function spa_pod_is_object_type(pod: Pspa_pod; _type: Tuint32_t) : Tbool; cdecl; external;

function spa_pod_is_object_id(pod: Pspa_pod; id: Tuint32_t) : Tbool; cdecl; external;

function spa_pod_is_sequence(pod: Pspa_pod) : longint; cdecl; external;

function spa_pod_object_find_prop(pod: Pspa_pod_object; start: Pspa_pod_prop; key: Tuint32_t) : Pspa_pod_prop; cdecl; external;

function spa_pod_find_prop(pod: Pspa_pod; start: Pspa_pod_prop; key: Tuint32_t) : Pspa_pod_prop; cdecl; external;

function spa_pod_object_fixate(pod: Pspa_pod_object) : longint; cdecl; external;

function spa_pod_fixate(pod: Pspa_pod) : longint; cdecl; external;

function spa_pod_object_is_fixated(pod: Pspa_pod_object) : longint; cdecl; external;

function spa_pod_is_fixated(pod: Pspa_pod) : longint; cdecl; external;

procedure spa_pod_builder_get_state(builder: Pspa_pod_builder; state: Pspa_pod_builder_state); cdecl; external;

procedure spa_pod_builder_set_callbacks(builder: Pspa_pod_builder; callbacks: Pspa_pod_builder_callbacks; Data: pointer); cdecl; external;

procedure spa_pod_builder_reset(builder: Pspa_pod_builder; state: Pspa_pod_builder_state); cdecl; external;

procedure spa_pod_builder_init(builder: Pspa_pod_builder; Data: pointer; size: Tuint32_t); cdecl; external;

function spa_pod_builder_deref(builder: Pspa_pod_builder; offset: Tuint32_t) : Pspa_pod; cdecl; external;

function spa_pod_builder_frame(builder: Pspa_pod_builder; frame: Pspa_pod_frame) : Pspa_pod; cdecl; external;

procedure spa_pod_builder_push(builder: Pspa_pod_builder; frame: Pspa_pod_frame; pod: Pspa_pod; offset: Tuint32_t); cdecl; external;

function spa_pod_builder_raw(builder: Pspa_pod_builder; Data: pointer; size: Tuint32_t) : longint; cdecl; external;

function spa_pod_builder_pad(builder: Pspa_pod_builder; size: Tuint32_t) : longint; cdecl; external;

function spa_pod_builder_raw_padded(builder: Pspa_pod_builder; Data: pointer; size: Tuint32_t) : longint; cdecl; external;

function spa_pod_builder_pop(builder: Pspa_pod_builder; frame: Pspa_pod_frame) : pointer; cdecl; external;

function spa_pod_builder_primitive(builder: Pspa_pod_builder; p: Pspa_pod) : longint; cdecl; external;

function spa_pod_builder_none(builder: Pspa_pod_builder) : longint; cdecl; external;

function spa_pod_builder_child(builder: Pspa_pod_builder; size: Tuint32_t; _type: Tuint32_t) : longint; cdecl; external;

function spa_pod_builder_bool(builder: Pspa_pod_builder; val: Tbool) : longint; cdecl; external;

function spa_pod_builder_id(builder: Pspa_pod_builder; val: Tuint32_t) : longint; cdecl; external;

function spa_pod_builder_int(builder: Pspa_pod_builder; val: Tint32_t) : longint; cdecl; external;

function spa_pod_builder_long(builder: Pspa_pod_builder; val: Tint64_t) : longint; cdecl; external;

function spa_pod_builder_float(builder: Pspa_pod_builder; val: single) : longint; cdecl; external;

function spa_pod_builder_double(builder: Pspa_pod_builder; val: Tdouble) : longint; cdecl; external;

function spa_pod_builder_write_string(builder: Pspa_pod_builder; str: PChar; len: Tuint32_t) : longint; cdecl; external;

function spa_pod_builder_string_len(builder: Pspa_pod_builder; str: PChar; len: Tuint32_t) : longint; cdecl; external;

function spa_pod_builder_string(builder: Pspa_pod_builder; str: PChar) : longint; cdecl; external;

function spa_pod_builder_bytes(builder: Pspa_pod_builder; bytes: pointer; len: Tuint32_t) : longint; cdecl; external;

function spa_pod_builder_reserve_bytes(builder: Pspa_pod_builder; len: Tuint32_t) : pointer; cdecl; external;

function spa_pod_builder_pointer(builder: Pspa_pod_builder; _type: Tuint32_t; val: pointer) : longint; cdecl; external;

function spa_pod_builder_fd(builder: Pspa_pod_builder; fd: Tint64_t) : longint; cdecl; external;

function spa_pod_builder_rectangle(builder: Pspa_pod_builder; Width: Tuint32_t; Height: Tuint32_t) : longint; cdecl; external;

function spa_pod_builder_fraction(builder: Pspa_pod_builder; num: Tuint32_t; denom: Tuint32_t) : longint; cdecl; external;

function spa_pod_builder_push_array(builder: Pspa_pod_builder; frame: Pspa_pod_frame) : longint; cdecl; external;

function spa_pod_builder_array(builder: Pspa_pod_builder; child_size: Tuint32_t; child_type: Tuint32_t; n_elems: Tuint32_t; elems: pointer) : longint; cdecl; external;

function spa_pod_builder_push_choice(builder: Pspa_pod_builder; frame: Pspa_pod_frame; _type: Tuint32_t; flags: Tuint32_t) : longint; cdecl; external;

function spa_pod_builder_push_struct(builder: Pspa_pod_builder; frame: Pspa_pod_frame) : longint; cdecl; external;

function spa_pod_builder_push_object(builder: Pspa_pod_builder; frame: Pspa_pod_frame; _type: Tuint32_t; id: Tuint32_t) : longint; cdecl; external;

function spa_pod_builder_prop(builder: Pspa_pod_builder; key: Tuint32_t; flags: Tuint32_t) : longint; cdecl; external;

function spa_pod_builder_push_sequence(builder: Pspa_pod_builder; frame: Pspa_pod_frame; _unit: Tuint32_t) : longint; cdecl; external;

function spa_pod_builder_control(builder: Pspa_pod_builder; offset: Tuint32_t; _type: Tuint32_t) : Tuint32_t; cdecl; external;

function spa_choice_from_id(id: char) : Tuint32_t; cdecl; external;

function spa_pod_builder_addv(builder: Pspa_pod_builder; args: pointer) : longint; cdecl; external;

function spa_pod_builder_add(builder: Pspa_pod_builder; args: array of const) : longint; cdecl; external; overload;

function spa_pod_builder_add(builder: Pspa_pod_builder) : longint; cdecl; external; overload;

function spa_pod_copy(pod: Pspa_pod) : Pspa_pod; cdecl; external;

procedure spa_pod_dynamic_builder_init(builder: Pspa_pod_dynamic_builder; Data: pointer; size: Tuint32_t; extend: Tuint32_t); cdecl; external;

procedure spa_pod_dynamic_builder_clean(builder: Pspa_pod_dynamic_builder); cdecl; external;

function spa_pod_choice_fix_default(choice: Pspa_pod_choice) : longint; cdecl; external;

function spa_pod_filter_flags_value(b: Pspa_pod_builder; _type: Tuint32_t; r1: pointer; r2: pointer; size: Tuint32_t) : longint; cdecl; external;

function spa_pod_filter_prop(b: Pspa_pod_builder; p1: Pspa_pod_prop; p2: Pspa_pod_prop) : longint; cdecl; external;

function spa_pod_filter_part(b: Pspa_pod_builder; pod: Pspa_pod; pod_size: Tuint32_t; filter: Pspa_pod; filter_size: Tuint32_t) : longint; cdecl; external;

function spa_pod_filter(b: Pspa_pod_builder; Result: PPspa_pod; pod: Pspa_pod; filter: Pspa_pod) : longint; cdecl; external;

function spa_pod_compare_value(_type: Tuint32_t; r1: pointer; r2: pointer; size: Tuint32_t) : longint; cdecl; external;

function spa_pod_compare(pod1: Pspa_pod; pod2: Pspa_pod) : longint; cdecl; external;

procedure spa_result_func_device_params(Data: pointer; seq: longint; res: longint; _type: Tuint32_t; Result: pointer); cdecl; external;

function spa_device_enum_params_sync(device: Pspa_device; id: Tuint32_t; index: Puint32_t; filter: Pspa_pod; param: PPspa_pod; builder: Pspa_pod_builder) : longint; cdecl; external;

function spa_format_parse(format: Pspa_pod; media_type: Puint32_t; media_subtype: Puint32_t) : longint; cdecl; external;

function spa_format_video_raw_parse(format: Pspa_pod; info: Pspa_video_info_raw) : longint; cdecl; external;

function spa_format_video_dsp_parse(format: Pspa_pod; info: Pspa_video_info_dsp) : longint; cdecl; external;

function spa_format_video_raw_build(builder: Pspa_pod_builder; id: Tuint32_t; info: Pspa_video_info_raw) : Pspa_pod; cdecl; external;

function spa_format_video_dsp_build(builder: Pspa_pod_builder; id: Tuint32_t; info: Pspa_video_info_dsp) : Pspa_pod; cdecl; external;

function spa_format_video_h264_parse(format: Pspa_pod; info: Pspa_video_info_h264) : longint; cdecl; external;

function spa_format_video_h264_build(builder: Pspa_pod_builder; id: Tuint32_t; info: Pspa_video_info_h264) : Pspa_pod; cdecl; external;

function spa_format_video_mjpg_parse(format: Pspa_pod; info: Pspa_video_info_mjpg) : longint; cdecl; external;

function spa_format_video_mjpg_build(builder: Pspa_pod_builder; id: Tuint32_t; info: Pspa_video_info_mjpg) : Pspa_pod; cdecl; external;

function spa_format_audio_raw_parse(format: Pspa_pod; info: Pspa_audio_info_raw) : longint; cdecl; external;

function spa_format_audio_dsp_parse(format: Pspa_pod; info: Pspa_audio_info_dsp) : longint; cdecl; external;

function spa_format_audio_iec958_parse(format: Pspa_pod; info: Pspa_audio_info_iec958) : longint; cdecl; external;

function spa_format_audio_dsd_parse(format: Pspa_pod; info: Pspa_audio_info_dsd) : longint; cdecl; external;

function spa_format_audio_raw_build(builder: Pspa_pod_builder; id: Tuint32_t; info: Pspa_audio_info_raw) : Pspa_pod; cdecl; external;

function spa_format_audio_dsp_build(builder: Pspa_pod_builder; id: Tuint32_t; info: Pspa_audio_info_dsp) : Pspa_pod; cdecl; external;

function spa_format_audio_iec958_build(builder: Pspa_pod_builder; id: Tuint32_t; info: Pspa_audio_info_iec958) : Pspa_pod; cdecl; external;

function spa_format_audio_dsd_build(builder: Pspa_pod_builder; id: Tuint32_t; info: Pspa_audio_info_dsd) : Pspa_pod; cdecl; external;

function spa_latency_info_compare(a: Pspa_latency_info; b: Pspa_latency_info) : longint; cdecl; external;

procedure spa_latency_info_combine_start(info: Pspa_latency_info; direction: Tspa_direction); cdecl; external;

procedure spa_latency_info_combine_finish(info: Pspa_latency_info); cdecl; external;

function spa_latency_info_combine(info: Pspa_latency_info; other: Pspa_latency_info) : longint; cdecl; external;

function spa_latency_parse(latency: Pspa_pod; info: Pspa_latency_info) : longint; cdecl; external;

function spa_latency_build(builder: Pspa_pod_builder; id: Tuint32_t; info: Pspa_latency_info) : Pspa_pod; cdecl; external;

function spa_process_latency_parse(latency: Pspa_pod; info: Pspa_process_latency_info) : longint; cdecl; external;

function spa_process_latency_build(builder: Pspa_pod_builder; id: Tuint32_t; info: Pspa_process_latency_info) : Pspa_pod; cdecl; external;

function spa_process_latency_info_add(process: Pspa_process_latency_info; info: Pspa_latency_info) : longint; cdecl; external;

procedure spa_result_func_node_params(Data: pointer; seq: longint; res: longint; _type: Tuint32_t; Result: pointer); cdecl; external;

function spa_node_enum_params_sync(node: Pspa_node; id: Tuint32_t; index: Puint32_t; filter: Pspa_pod; param: PPspa_pod; builder: Pspa_pod_builder) : longint; cdecl; external;

function spa_node_port_enum_params_sync(node: Pspa_node; direction: Tspa_direction; port_id: Tuint32_t; id: Tuint32_t; index: Puint32_t; filter: Pspa_pod;
  param: PPspa_pod; builder: Pspa_pod_builder) : longint; cdecl; external;

function spa_buffer_alloc_fill_info(info: Pspa_buffer_alloc_info; n_metas: Tuint32_t; metas: Pspa_meta; n_datas: Tuint32_t; datas: Pspa_data; data_aligns: Puint32_t) : longint; cdecl; external;

function spa_buffer_alloc_layout(info: Pspa_buffer_alloc_info; skel_mem: pointer; data_mem: pointer) : Pspa_buffer; cdecl; external;

function spa_buffer_alloc_layout_array(info: Pspa_buffer_alloc_info; n_buffers: Tuint32_t; buffers: PPspa_buffer; skel_mem: pointer; data_mem: pointer) : longint; cdecl; external;

function spa_buffer_alloc_array(n_buffers: Tuint32_t; flags: Tuint32_t; n_metas: Tuint32_t; metas: Pspa_meta; n_datas: Tuint32_t; datas: Pspa_data;
  data_aligns: Puint32_t) : PPspa_buffer; cdecl; external;

function spa_buffer_find_meta(b: Pspa_buffer; _type: Tuint32_t) : Pspa_meta; cdecl; external;

function spa_buffer_find_meta_data(b: Pspa_buffer; _type: Tuint32_t; size: Tsize_t) : pointer; cdecl; external;

procedure spa_list_init(list: Pspa_list); cdecl; external;

procedure spa_list_insert(list: Pspa_list; elem: Pspa_list); cdecl; external;

procedure spa_list_insert_list(list: Pspa_list; other: Pspa_list); cdecl; external;

procedure spa_list_remove(elem: Pspa_list); cdecl; external;

function spa_json_to_pod_part(b: Pspa_pod_builder; flags: Tuint32_t; id: Tuint32_t; info: Pspa_type_info; iter: Pspa_json; Value: PChar; len: longint) : longint; cdecl; external;

function spa_json_to_pod(b: Pspa_pod_builder; flags: Tuint32_t; info: Pspa_type_info; Value: PChar; len: longint) : longint; cdecl; external;

function spa_dict_item_compare(i1: pointer; i2: pointer) : longint; cdecl; external;

procedure spa_dict_qsort(dict: Pspa_dict); cdecl; external;

function spa_dict_lookup_item(dict: Pspa_dict; key: PChar) : Pspa_dict_item; cdecl; external;

function spa_dict_lookup(dict: Pspa_dict; key: PChar) : PChar; cdecl; external;

procedure spa_hook_list_init(list: Pspa_hook_list); cdecl; external;

function spa_hook_list_is_empty(list: Pspa_hook_list) : Tbool; cdecl; external;

procedure spa_hook_list_append(list: Pspa_hook_list; hook: Pspa_hook; funcs: pointer; Data: pointer); cdecl; external;

procedure spa_hook_list_prepend(list: Pspa_hook_list; hook: Pspa_hook; funcs: pointer; Data: pointer); cdecl; external;

procedure spa_hook_remove(hook: Pspa_hook); cdecl; external;

procedure spa_hook_list_clean(list: Pspa_hook_list); cdecl; external;

procedure spa_hook_list_isolate(list: Pspa_hook_list; save: Pspa_hook_list; hook: Pspa_hook; funcs: pointer; Data: pointer); cdecl; external;

procedure spa_hook_list_join(list: Pspa_hook_list; save: Pspa_hook_list); cdecl; external;

procedure spa_ringbuffer_init(rbuf: Pspa_ringbuffer); cdecl; external;

procedure spa_ringbuffer_set_avail(rbuf: Pspa_ringbuffer; size: Tuint32_t); cdecl; external;

function spa_ringbuffer_get_read_index(rbuf: Pspa_ringbuffer; index: Puint32_t) : Tint32_t; cdecl; external;

procedure spa_ringbuffer_read_data(rbuf: Pspa_ringbuffer; buffer: pointer; size: Tuint32_t; offset: Tuint32_t; Data: pointer; len: Tuint32_t); cdecl; external;

procedure spa_ringbuffer_read_update(rbuf: Pspa_ringbuffer; index: Tint32_t); cdecl; external;

function spa_ringbuffer_get_write_index(rbuf: Pspa_ringbuffer; index: Puint32_t) : Tint32_t; cdecl; external;

procedure spa_ringbuffer_write_data(rbuf: Pspa_ringbuffer; buffer: pointer; size: Tuint32_t; offset: Tuint32_t; Data: pointer; len: Tuint32_t); cdecl; external;

procedure spa_ringbuffer_write_update(rbuf: Pspa_ringbuffer; index: Tint32_t); cdecl; external;

procedure spa_json_init(iter: Pspa_json; Data: PChar; size: Tsize_t); cdecl; external;

procedure spa_json_enter(iter: Pspa_json; sub: Pspa_json); cdecl; external;

function spa_json_next(iter: Pspa_json; Value: PPchar) : longint; cdecl; external;

function spa_json_enter_container(iter: Pspa_json; sub: Pspa_json; _type: char) : longint; cdecl; external;

function spa_json_is_container(val: PChar; len: longint) : longint; cdecl; external;

function spa_json_container_len(iter: Pspa_json; Value: PChar; len: longint) : longint; cdecl; external;

function spa_json_is_object(val: PChar; len: longint) : longint; cdecl; external;

function spa_json_enter_object(iter: Pspa_json; sub: Pspa_json) : longint; cdecl; external;

function spa_json_is_array(val: PChar; len: longint) : Tbool; cdecl; external;

function spa_json_enter_array(iter: Pspa_json; sub: Pspa_json) : longint; cdecl; external;

function spa_json_is_null(val: PChar; len: longint) : Tbool; cdecl; external;

function spa_json_parse_float(val: PChar; len: longint; Result: Psingle) : longint; cdecl; external;

function spa_json_is_float(val: PChar; len: longint) : Tbool; cdecl; external;

function spa_json_get_float(iter: Pspa_json; res: Psingle) : longint; cdecl; external;

function spa_json_parse_int(val: PChar; len: longint; Result: Plongint) : longint; cdecl; external;

function spa_json_is_int(val: PChar; len: longint) : Tbool; cdecl; external;

function spa_json_get_int(iter: Pspa_json; res: Plongint) : longint; cdecl; external;

function spa_json_is_true(val: PChar; len: longint) : Tbool; cdecl; external;

function spa_json_is_false(val: PChar; len: longint) : Tbool; cdecl; external;

function spa_json_is_bool(val: PChar; len: longint) : Tbool; cdecl; external;

function spa_json_parse_bool(val: PChar; len: longint; Result: Pbool) : longint; cdecl; external;

function spa_json_get_bool(iter: Pspa_json; res: Pbool) : longint; cdecl; external;

function spa_json_is_string(val: PChar; len: longint) : Tbool; cdecl; external;

function spa_json_parse_stringn(val: PChar; len: longint; Result: PChar; maxlen: longint) : longint; cdecl; external;

function spa_json_parse_string(val: PChar; len: longint; Result: PChar) : longint; cdecl; external;

function spa_json_get_string(iter: Pspa_json; res: PChar; maxlen: longint) : longint; cdecl; external;

function spa_json_encode_string(str: PChar; size: longint; val: PChar) : longint; cdecl; external;

function spa_streq(s1: PChar; s2: PChar) : Tbool; cdecl; external;

function spa_strneq(s1: PChar; s2: PChar; len: Tsize_t) : Tbool; cdecl; external;

function spa_strstartswith(s: PChar; prefix: PChar) : Tbool; cdecl; external;

function spa_strendswith(s: PChar; suffix: PChar) : Tbool; cdecl; external;

function spa_atoi32(str: PChar; val: Pint32_t; base: longint) : Tbool; cdecl; external;

function spa_atou32(str: PChar; val: Puint32_t; base: longint) : Tbool; cdecl; external;

function spa_atoi64(str: PChar; val: Pint64_t; base: longint) : Tbool; cdecl; external;

function spa_atou64(str: PChar; val: Puint64_t; base: longint) : Tbool; cdecl; external;

function spa_atob(str: PChar) : Tbool; cdecl; external;

function spa_vscnprintf(buffer: PChar; size: Tsize_t; format: PChar; args: pointer) : longint; cdecl; external;

function spa_scnprintf(buffer: PChar; size: Tsize_t; format: PChar; args: array of const) : longint; cdecl; external; overload;

function spa_scnprintf(buffer: PChar; size: Tsize_t; format: PChar) : longint; cdecl; external; overload;

function spa_atof(str: PChar; val: Psingle) : Tbool; cdecl; external;

function spa_atod(str: PChar; val: Pdouble) : Tbool; cdecl; external;

function spa_type_is_a(_type: PChar; parent: PChar) : Tbool; cdecl; external;



{$l spabridge.o}


