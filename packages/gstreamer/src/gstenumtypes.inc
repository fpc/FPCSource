

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}



  function gst_allocator_flags_get_type:TGType;cdecl;external gstreamerlib name 'gst_allocator_flags_get_type';

  function gst_bin_flags_get_type:TGType;cdecl;external gstreamerlib name 'gst_bin_flags_get_type';

  function gst_buffer_flags_get_type:TGType;cdecl;external gstreamerlib name 'gst_buffer_flags_get_type';

  function gst_buffer_copy_flags_get_type:TGType;cdecl;external gstreamerlib name 'gst_buffer_copy_flags_get_type';

  function gst_buffer_pool_acquire_flags_get_type:TGType;cdecl;external gstreamerlib name 'gst_buffer_pool_acquire_flags_get_type';

  function gst_bus_flags_get_type:TGType;cdecl;external gstreamerlib name 'gst_bus_flags_get_type';

  function gst_bus_sync_reply_get_type:TGType;cdecl;external gstreamerlib name 'gst_bus_sync_reply_get_type';

  function gst_caps_flags_get_type:TGType;cdecl;external gstreamerlib name 'gst_caps_flags_get_type';

  function gst_caps_intersect_mode_get_type:TGType;cdecl;external gstreamerlib name 'gst_caps_intersect_mode_get_type';

  function gst_clock_return_get_type:TGType;cdecl;external gstreamerlib name 'gst_clock_return_get_type';

  function gst_clock_entry_type_get_type:TGType;cdecl;external gstreamerlib name 'gst_clock_entry_type_get_type';

  function gst_clock_flags_get_type:TGType;cdecl;external gstreamerlib name 'gst_clock_flags_get_type';

  function gst_debug_graph_details_get_type:TGType;cdecl;external gstreamerlib name 'gst_debug_graph_details_get_type';

  function gst_state_get_type:TGType;cdecl;external gstreamerlib name 'gst_state_get_type';

  function gst_state_change_return_get_type:TGType;cdecl;external gstreamerlib name 'gst_state_change_return_get_type';

  function gst_state_change_get_type:TGType;cdecl;external gstreamerlib name 'gst_state_change_get_type';

  function gst_element_flags_get_type:TGType;cdecl;external gstreamerlib name 'gst_element_flags_get_type';

  function gst_core_error_get_type:TGType;cdecl;external gstreamerlib name 'gst_core_error_get_type';

  function gst_library_error_get_type:TGType;cdecl;external gstreamerlib name 'gst_library_error_get_type';

  function gst_resource_error_get_type:TGType;cdecl;external gstreamerlib name 'gst_resource_error_get_type';

  function gst_stream_error_get_type:TGType;cdecl;external gstreamerlib name 'gst_stream_error_get_type';

  function gst_event_type_flags_get_type:TGType;cdecl;external gstreamerlib name 'gst_event_type_flags_get_type';

  function gst_event_type_get_type:TGType;cdecl;external gstreamerlib name 'gst_event_type_get_type';

  function gst_stream_flags_get_type:TGType;cdecl;external gstreamerlib name 'gst_stream_flags_get_type';

  function gst_qos_type_get_type:TGType;cdecl;external gstreamerlib name 'gst_qos_type_get_type';

  function gst_format_get_type:TGType;cdecl;external gstreamerlib name 'gst_format_get_type';

  function gst_debug_level_get_type:TGType;cdecl;external gstreamerlib name 'gst_debug_level_get_type';

  function gst_debug_color_flags_get_type:TGType;cdecl;external gstreamerlib name 'gst_debug_color_flags_get_type';

  function gst_stack_trace_flags_get_type:TGType;cdecl;external gstreamerlib name 'gst_stack_trace_flags_get_type';

  function gst_debug_color_mode_get_type:TGType;cdecl;external gstreamerlib name 'gst_debug_color_mode_get_type';

  function gst_iterator_result_get_type:TGType;cdecl;external gstreamerlib name 'gst_iterator_result_get_type';

  function gst_iterator_item_get_type:TGType;cdecl;external gstreamerlib name 'gst_iterator_item_get_type';

  function gst_memory_flags_get_type:TGType;cdecl;external gstreamerlib name 'gst_memory_flags_get_type';

  function gst_map_flags_get_type:TGType;cdecl;external gstreamerlib name 'gst_map_flags_get_type';

  function gst_message_type_get_type:TGType;cdecl;external gstreamerlib name 'gst_message_type_get_type';

  function gst_structure_change_type_get_type:TGType;cdecl;external gstreamerlib name 'gst_structure_change_type_get_type';

  function gst_stream_status_type_get_type:TGType;cdecl;external gstreamerlib name 'gst_stream_status_type_get_type';

  function gst_progress_type_get_type:TGType;cdecl;external gstreamerlib name 'gst_progress_type_get_type';

  function gst_meta_flags_get_type:TGType;cdecl;external gstreamerlib name 'gst_meta_flags_get_type';

  function gst_mini_object_flags_get_type:TGType;cdecl;external gstreamerlib name 'gst_mini_object_flags_get_type';

  function gst_lock_flags_get_type:TGType;cdecl;external gstreamerlib name 'gst_lock_flags_get_type';

  function gst_object_flags_get_type:TGType;cdecl;external gstreamerlib name 'gst_object_flags_get_type';

  function gst_pad_direction_get_type:TGType;cdecl;external gstreamerlib name 'gst_pad_direction_get_type';

  function gst_pad_mode_get_type:TGType;cdecl;external gstreamerlib name 'gst_pad_mode_get_type';

  function gst_pad_link_return_get_type:TGType;cdecl;external gstreamerlib name 'gst_pad_link_return_get_type';

  function gst_flow_return_get_type:TGType;cdecl;external gstreamerlib name 'gst_flow_return_get_type';

  function gst_pad_link_check_get_type:TGType;cdecl;external gstreamerlib name 'gst_pad_link_check_get_type';

  function gst_pad_probe_type_get_type:TGType;cdecl;external gstreamerlib name 'gst_pad_probe_type_get_type';

  function gst_pad_probe_return_get_type:TGType;cdecl;external gstreamerlib name 'gst_pad_probe_return_get_type';

  function gst_pad_flags_get_type:TGType;cdecl;external gstreamerlib name 'gst_pad_flags_get_type';

  function gst_pad_presence_get_type:TGType;cdecl;external gstreamerlib name 'gst_pad_presence_get_type';

  function gst_pad_template_flags_get_type:TGType;cdecl;external gstreamerlib name 'gst_pad_template_flags_get_type';

  function gst_parse_error_get_type:TGType;cdecl;external gstreamerlib name 'gst_parse_error_get_type';

  function gst_parse_flags_get_type:TGType;cdecl;external gstreamerlib name 'gst_parse_flags_get_type';

  function gst_pipeline_flags_get_type:TGType;cdecl;external gstreamerlib name 'gst_pipeline_flags_get_type';

  function gst_plugin_error_get_type:TGType;cdecl;external gstreamerlib name 'gst_plugin_error_get_type';

  function gst_plugin_flags_get_type:TGType;cdecl;external gstreamerlib name 'gst_plugin_flags_get_type';

  function gst_plugin_dependency_flags_get_type:TGType;cdecl;external gstreamerlib name 'gst_plugin_dependency_flags_get_type';

  function gst_rank_get_type:TGType;cdecl;external gstreamerlib name 'gst_rank_get_type';

  function gst_promise_result_get_type:TGType;cdecl;external gstreamerlib name 'gst_promise_result_get_type';

  function gst_query_type_flags_get_type:TGType;cdecl;external gstreamerlib name 'gst_query_type_flags_get_type';

  function gst_query_type_get_type:TGType;cdecl;external gstreamerlib name 'gst_query_type_get_type';

  function gst_buffering_mode_get_type:TGType;cdecl;external gstreamerlib name 'gst_buffering_mode_get_type';

  function gst_scheduling_flags_get_type:TGType;cdecl;external gstreamerlib name 'gst_scheduling_flags_get_type';

  function gst_seek_type_get_type:TGType;cdecl;external gstreamerlib name 'gst_seek_type_get_type';

  function gst_seek_flags_get_type:TGType;cdecl;external gstreamerlib name 'gst_seek_flags_get_type';

  function gst_segment_flags_get_type:TGType;cdecl;external gstreamerlib name 'gst_segment_flags_get_type';

  function gst_stream_type_get_type:TGType;cdecl;external gstreamerlib name 'gst_stream_type_get_type';

  function gst_clock_type_get_type:TGType;cdecl;external gstreamerlib name 'gst_clock_type_get_type';

  function gst_tag_merge_mode_get_type:TGType;cdecl;external gstreamerlib name 'gst_tag_merge_mode_get_type';

  function gst_tag_flag_get_type:TGType;cdecl;external gstreamerlib name 'gst_tag_flag_get_type';

  function gst_tag_scope_get_type:TGType;cdecl;external gstreamerlib name 'gst_tag_scope_get_type';

  function gst_task_state_get_type:TGType;cdecl;external gstreamerlib name 'gst_task_state_get_type';

  function gst_toc_scope_get_type:TGType;cdecl;external gstreamerlib name 'gst_toc_scope_get_type';

  function gst_toc_entry_type_get_type:TGType;cdecl;external gstreamerlib name 'gst_toc_entry_type_get_type';

  function gst_toc_loop_type_get_type:TGType;cdecl;external gstreamerlib name 'gst_toc_loop_type_get_type';

  function gst_tracer_value_scope_get_type:TGType;cdecl;external gstreamerlib name 'gst_tracer_value_scope_get_type';

  function gst_tracer_value_flags_get_type:TGType;cdecl;external gstreamerlib name 'gst_tracer_value_flags_get_type';

  function gst_type_find_probability_get_type:TGType;cdecl;external gstreamerlib name 'gst_type_find_probability_get_type';

  function gst_uri_error_get_type:TGType;cdecl;external gstreamerlib name 'gst_uri_error_get_type';

  function gst_uri_type_get_type:TGType;cdecl;external gstreamerlib name 'gst_uri_type_get_type';

  function gst_search_mode_get_type:TGType;cdecl;external gstreamerlib name 'gst_search_mode_get_type';


