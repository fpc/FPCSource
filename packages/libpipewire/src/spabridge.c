#include <spa/control/control.h>
#include <spa/control/type-info.h>
#include <spa/support/log-impl.h>
#include <spa/support/thread.h>
#include <spa/support/cpu.h>
#include <spa/support/plugin.h>
#include <spa/support/system.h>
#include <spa/support/dbus.h>
#include <spa/support/log.h>
#include <spa/support/plugin-loader.h>
#include <spa/support/i18n.h>
#include <spa/support/loop.h>
#include <spa/graph/graph.h>
#include <spa/debug/dict.h>
#include <spa/debug/pod.h>
#include <spa/debug/mem.h>
#include <spa/debug/log.h>
#include <spa/debug/buffer.h>
#include <spa/debug/types.h>
#include <spa/debug/format.h>
#include <spa/debug/node.h>
#include <spa/pod/parser.h>
#include <spa/pod/iter.h>
#include <spa/pod/command.h>
#include <spa/pod/vararg.h>
#include <spa/pod/pod.h>
#include <spa/pod/event.h>
#include <spa/pod/builder.h>
#include <spa/pod/dynamic.h>
#include <spa/pod/filter.h>
#include <spa/pod/compare.h>
#include <spa/monitor/device.h>
#include <spa/monitor/event.h>
#include <spa/monitor/utils.h>
#include <spa/monitor/type-info.h>
#include <spa/interfaces/audio/aec.h>
#include <spa/param/format-utils.h>
#include <spa/param/param.h>
#include <spa/param/profiler.h>
#include <spa/param/props.h>
#include <spa/param/bluetooth/audio.h>
#include <spa/param/bluetooth/type-info.h>
#include <spa/param/video/format-utils.h>
#include <spa/param/video/color.h>
#include <spa/param/video/encoded.h>
#include <spa/param/video/multiview.h>
#include <spa/param/video/raw.h>
#include <spa/param/video/type-info.h>
#include <spa/param/video/chroma.h>
#include <spa/param/video/format.h>
#include <spa/param/audio/format-utils.h>
#include <spa/param/audio/raw.h>
#include <spa/param/audio/layout.h>
#include <spa/param/audio/iec958.h>
#include <spa/param/audio/dsd.h>
#include <spa/param/audio/type-info.h>
#include <spa/param/audio/format.h>
#include <spa/param/type-info.h>
#include <spa/param/format.h>
#include <spa/param/latency-utils.h>
#include <spa/node/command.h>
#include <spa/node/io.h>
#include <spa/node/event.h>
#include <spa/node/utils.h>
#include <spa/node/keys.h>
#include <spa/node/type-info.h>
#include <spa/node/node.h>
#include <spa/buffer/alloc.h>
#include <spa/buffer/meta.h>
#include <spa/buffer/buffer.h>
#include <spa/buffer/type-info.h>
#include <spa/utils/list.h>
#include <spa/utils/names.h>
#include <spa/utils/json-pod.h>
#include <spa/utils/dict.h>
#include <spa/utils/hook.h>
#include <spa/utils/defs.h>
#include <spa/utils/type.h>
#include <spa/utils/ansi.h>
#include <spa/utils/ringbuffer.h>
#include <spa/utils/json.h>
#include <spa/utils/result.h>
#include <spa/utils/keys.h>
#include <spa/utils/string.h>
#include <spa/utils/type-info.h>
 int dummy; 
 void * spamethodtable[284+1] = {
    &spa_atob,
    &spa_atod,
    &spa_atof,
    &spa_atoi32,
    &spa_atoi64,
    &spa_atou32,
    &spa_atou64,
    &spa_buffer_alloc_fill_info,
    &spa_buffer_alloc_layout,
    &spa_buffer_alloc_layout_array,
    &spa_buffer_find_meta,
    &spa_buffer_find_meta_data,
    &spa_choice_from_id,
    &spa_dbus_get_connection,
    &spa_debug_buffer,
    &spa_debug_dict,
    &spa_debug_format,
    &spa_debug_format_value,
    &spa_debug_mem,
    &spa_debug_pod,
    &spa_debug_pod_value,
    &spa_debug_port_info,
    &spa_debug_type_find,
    &spa_debug_type_find_name,
    &spa_debug_type_find_short,
    &spa_debug_type_find_short_name,
    &spa_debug_type_find_type,
    &spa_debug_type_find_type_short,
    &spa_debug_type_short_name,
    &spa_device_enum_params_sync,
    &spa_dict_item_compare,
    &spa_dict_lookup,
    &spa_dict_lookup_item,
    &spa_dict_qsort,
    &spa_format_audio_dsd_build,
    &spa_format_audio_dsd_parse,
    &spa_format_audio_dsp_build,
    &spa_format_audio_dsp_parse,
    &spa_format_audio_iec958_build,
    &spa_format_audio_iec958_parse,
    &spa_format_audio_raw_build,
    &spa_format_audio_raw_parse,
    &spa_format_parse,
    &spa_format_video_dsp_build,
    &spa_format_video_dsp_parse,
    &spa_format_video_h264_build,
    &spa_format_video_h264_parse,
    &spa_format_video_mjpg_build,
    &spa_format_video_mjpg_parse,
    &spa_format_video_raw_build,
    &spa_format_video_raw_parse,
    &spa_graph_finish,
    &spa_graph_init,
    &spa_graph_link_add,
    &spa_graph_link_remove,
    &spa_graph_link_signal_graph,
    &spa_graph_link_signal_node,
    &spa_graph_link_trigger,
    &spa_graph_node_add,
    &spa_graph_node_impl_process,
    &spa_graph_node_impl_reuse_buffer,
    &spa_graph_node_impl_sub_process,
    &spa_graph_node_init,
    &spa_graph_node_remove,
    &spa_graph_node_set_callbacks,
    &spa_graph_node_set_subgraph,
    &spa_graph_node_trigger,
    &spa_graph_port_add,
    &spa_graph_port_init,
    &spa_graph_port_link,
    &spa_graph_port_remove,
    &spa_graph_port_unlink,
    &spa_graph_run,
    &spa_graph_state_reset,
    &spa_hook_list_append,
    &spa_hook_list_clean,
    &spa_hook_list_init,
    &spa_hook_list_is_empty,
    &spa_hook_list_isolate,
    &spa_hook_list_join,
    &spa_hook_list_prepend,
    &spa_hook_remove,
    &spa_i18n_ntext,
    &spa_i18n_text,
    &spa_json_container_len,
    &spa_json_encode_string,
    &spa_json_enter,
    &spa_json_enter_array,
    &spa_json_enter_container,
    &spa_json_enter_object,
    &spa_json_get_bool,
    &spa_json_get_float,
    &spa_json_get_int,
    &spa_json_get_string,
    &spa_json_init,
    &spa_json_is_array,
    &spa_json_is_bool,
    &spa_json_is_container,
    &spa_json_is_false,
    &spa_json_is_float,
    &spa_json_is_int,
    &spa_json_is_null,
    &spa_json_is_object,
    &spa_json_is_string,
    &spa_json_is_true,
    &spa_json_next,
    &spa_json_parse_bool,
    &spa_json_parse_float,
    &spa_json_parse_int,
    &spa_json_parse_string,
    &spa_json_parse_stringn,
    &spa_json_to_pod,
    &spa_json_to_pod_part,
    &spa_latency_build,
    &spa_latency_info_combine,
    &spa_latency_info_combine_finish,
    &spa_latency_info_combine_start,
    &spa_latency_info_compare,
    &spa_latency_parse,
    &spa_list_init,
    &spa_list_insert,
    &spa_list_insert_list,
    &spa_list_remove,
    &spa_log_impl_topic_init,
    &spa_node_enum_params_sync,
    &spa_node_port_enum_params_sync,
    &spa_plugin_loader_load,
    &spa_plugin_loader_unload,
    &spa_pod_builder_add,
    &spa_pod_builder_addv,
    &spa_pod_builder_array,
    &spa_pod_builder_bool,
    &spa_pod_builder_bytes,
    &spa_pod_builder_child,
    &spa_pod_builder_control,
    &spa_pod_builder_deref,
    &spa_pod_builder_double,
    &spa_pod_builder_fd,
    &spa_pod_builder_float,
    &spa_pod_builder_fraction,
    &spa_pod_builder_frame,
    &spa_pod_builder_get_state,
    &spa_pod_builder_id,
    &spa_pod_builder_init,
    &spa_pod_builder_int,
    &spa_pod_builder_long,
    &spa_pod_builder_none,
    &spa_pod_builder_pad,
    &spa_pod_builder_pointer,
    &spa_pod_builder_pop,
    &spa_pod_builder_primitive,
    &spa_pod_builder_prop,
    &spa_pod_builder_push,
    &spa_pod_builder_push_array,
    &spa_pod_builder_push_choice,
    &spa_pod_builder_push_object,
    &spa_pod_builder_push_sequence,
    &spa_pod_builder_push_struct,
    &spa_pod_builder_raw,
    &spa_pod_builder_raw_padded,
    &spa_pod_builder_rectangle,
    &spa_pod_builder_reserve_bytes,
    &spa_pod_builder_reset,
    &spa_pod_builder_set_callbacks,
    &spa_pod_builder_string,
    &spa_pod_builder_string_len,
    &spa_pod_builder_write_string,
    &spa_pod_choice_fix_default,
    &spa_pod_compare,
    &spa_pod_compare_value,
    &spa_pod_control_first,
    &spa_pod_control_is_inside,
    &spa_pod_control_next,
    &spa_pod_copy,
    &spa_pod_copy_array,
    &spa_pod_copy_string,
    &spa_pod_dynamic_builder_clean,
    &spa_pod_dynamic_builder_init,
    &spa_pod_filter,
    &spa_pod_filter_flags_value,
    &spa_pod_filter_part,
    &spa_pod_filter_prop,
    &spa_pod_find_prop,
    &spa_pod_fixate,
    &spa_pod_from_data,
    &spa_pod_get_array,
    &spa_pod_get_bool,
    &spa_pod_get_bytes,
    &spa_pod_get_double,
    &spa_pod_get_fd,
    &spa_pod_get_float,
    &spa_pod_get_fraction,
    &spa_pod_get_id,
    &spa_pod_get_int,
    &spa_pod_get_long,
    &spa_pod_get_pointer,
    &spa_pod_get_rectangle,
    &spa_pod_get_string,
    &spa_pod_get_values,
    &spa_pod_is_array,
    &spa_pod_is_bitmap,
    &spa_pod_is_bool,
    &spa_pod_is_bytes,
    &spa_pod_is_choice,
    &spa_pod_is_double,
    &spa_pod_is_fd,
    &spa_pod_is_fixated,
    &spa_pod_is_float,
    &spa_pod_is_fraction,
    &spa_pod_is_id,
    &spa_pod_is_inside,
    &spa_pod_is_int,
    &spa_pod_is_long,
    &spa_pod_is_none,
    &spa_pod_is_object,
    &spa_pod_is_object_id,
    &spa_pod_is_object_type,
    &spa_pod_is_pointer,
    &spa_pod_is_rectangle,
    &spa_pod_is_sequence,
    &spa_pod_is_string,
    &spa_pod_is_struct,
    &spa_pod_next,
    &spa_pod_object_find_prop,
    &spa_pod_object_fixate,
    &spa_pod_object_is_fixated,
    &spa_pod_parser_advance,
    &spa_pod_parser_can_collect,
    &spa_pod_parser_current,
    &spa_pod_parser_deref,
    &spa_pod_parser_frame,
    &spa_pod_parser_get,
    &spa_pod_parser_get_bool,
    &spa_pod_parser_get_bytes,
    &spa_pod_parser_get_double,
    &spa_pod_parser_get_fd,
    &spa_pod_parser_get_float,
    &spa_pod_parser_get_fraction,
    &spa_pod_parser_get_id,
    &spa_pod_parser_get_int,
    &spa_pod_parser_get_long,
    &spa_pod_parser_get_pod,
    &spa_pod_parser_get_pointer,
    &spa_pod_parser_get_rectangle,
    &spa_pod_parser_get_state,
    &spa_pod_parser_get_string,
    &spa_pod_parser_getv,
    &spa_pod_parser_init,
    &spa_pod_parser_next,
    &spa_pod_parser_pod,
    &spa_pod_parser_pop,
    &spa_pod_parser_push,
    &spa_pod_parser_push_object,
    &spa_pod_parser_push_struct,
    &spa_pod_parser_reset,
    &spa_pod_prop_first,
    &spa_pod_prop_is_inside,
    &spa_pod_prop_next,
    &spa_process_latency_build,
    &spa_process_latency_info_add,
    &spa_process_latency_parse,
    &spa_result_func_device_params,
    &spa_result_func_node_params,
    &spa_ringbuffer_get_read_index,
    &spa_ringbuffer_get_write_index,
    &spa_ringbuffer_init,
    &spa_ringbuffer_read_data,
    &spa_ringbuffer_read_update,
    &spa_ringbuffer_set_avail,
    &spa_ringbuffer_write_data,
    &spa_ringbuffer_write_update,
    &spa_scnprintf,
    &spa_strendswith,
    &spa_streq,
    &spa_strneq,
    &spa_strstartswith,
    &spa_support_find,
    &spa_thread_utils_acquire_rt,
    &spa_thread_utils_create,
    &spa_thread_utils_drop_rt,
    &spa_thread_utils_get_rt_range,
    &spa_thread_utils_join,
    &spa_type_is_a,
    &spa_vscnprintf,
&dummy
}; 
