
{
  Automatically converted by H2Pas 0.99.16 from /usr/include/libheif/heif.h
  The following command line parameters were used:
    -l
    libheif1.so
    -e
    -S
    -T
    -c
    -C
    -P
    -p
    -1
    -E
    /usr/include/libheif/heif.h
    -o
    libheif.pp
    -u
    libheif
}

{$mode objfpc}
{$IFNDEF FPC_DOTTEDUNITS}
unit libheif;
{$ENDIF}

interface

{$IFNDEF FPC_DOTTEDUNITS}
uses
  ctypes;
{$ELSE}
uses 
  System.CTypes;
{$ENDIF}

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

const
  {$IFDEF WINDOWS}
  libheif_library = 'libheif.dll';
  {$ENDIF}
  {$IFDEF DARWIN}
  libheif_library = 'libheif.dylib';
  {$ENDIF}
  {$IFDEF UNIX}
  libheif_library = 'libheif.so';
  {$ENDIF}
  
    heif_error_Ok = 0;
    heif_error_Input_does_not_exist = 1;
    heif_error_Invalid_input = 2;
    heif_error_Unsupported_filetype = 3;
    heif_error_Unsupported_feature = 4;
    heif_error_Usage_error = 5;
    heif_error_Memory_allocation_error = 6;
    heif_error_Decoder_plugin_error = 7;
    heif_error_Encoder_plugin_error = 8;
    heif_error_Encoding_error = 9;
    heif_error_Color_profile_does_not_exist = 10;
    heif_suberror_Unspecified = 0;
    heif_suberror_End_of_data = 100;
    heif_suberror_Invalid_box_size = 101;
    heif_suberror_No_ftyp_box = 102;
    heif_suberror_No_idat_box = 103;
    heif_suberror_No_meta_box = 104;
    heif_suberror_No_hdlr_box = 105;
    heif_suberror_No_hvcC_box = 106;
    heif_suberror_No_pitm_box = 107;
    heif_suberror_No_ipco_box = 108;
    heif_suberror_No_ipma_box = 109;
    heif_suberror_No_iloc_box = 110;
    heif_suberror_No_iinf_box = 111;
    heif_suberror_No_iprp_box = 112;
    heif_suberror_No_iref_box = 113;
    heif_suberror_No_pict_handler = 114;
    heif_suberror_Ipma_box_references_nonexisting_property = 115;
    heif_suberror_No_properties_assigned_to_item = 116;
    heif_suberror_No_item_data = 117;
    heif_suberror_Invalid_grid_data = 118;
    heif_suberror_Missing_grid_images = 119;
    heif_suberror_Invalid_clean_aperture = 120;
    heif_suberror_Invalid_overlay_data = 121;
    heif_suberror_Overlay_image_outside_of_canvas = 122;
    heif_suberror_Auxiliary_image_type_unspecified = 123;
    heif_suberror_No_or_invalid_primary_item = 124;
    heif_suberror_No_infe_box = 125;
    heif_suberror_Unknown_color_profile_type = 126;
    heif_suberror_Wrong_tile_image_chroma_format = 127;
    heif_suberror_Invalid_fractional_number = 128;
    heif_suberror_Invalid_image_size = 129;
    heif_suberror_Invalid_pixi_box = 130;
    heif_suberror_No_av1C_box = 131;
    heif_suberror_Wrong_tile_image_pixel_depth = 132;
    heif_suberror_Security_limit_exceeded = 1000;
    heif_suberror_Nonexisting_item_referenced = 2000;
    heif_suberror_Null_pointer_argument = 2001;
    heif_suberror_Nonexisting_image_channel_referenced = 2002;
    heif_suberror_Unsupported_plugin_version = 2003;
    heif_suberror_Unsupported_writer_version = 2004;
    heif_suberror_Unsupported_parameter = 2005;
    heif_suberror_Invalid_parameter_value = 2006;
    heif_suberror_Unsupported_codec = 3000;
    heif_suberror_Unsupported_image_type = 3001;
    heif_suberror_Unsupported_data_version = 3002;
    heif_suberror_Unsupported_color_conversion = 3003;
    heif_suberror_Unsupported_item_construction_method = 3004;
    heif_suberror_Unsupported_bit_depth = 4000;
    heif_suberror_Cannot_write_output_data = 5000;
    heif_filetype_no = 0;
    heif_filetype_yes_supported = 1;
    heif_filetype_yes_unsupported = 2;
    heif_filetype_maybe = 3;
    heif_unknown_brand = 0;
    heif_heic = 1;
    heif_heix = 2;
    heif_hevc = 3;
    heif_hevx = 4;
    heif_heim = 5;
    heif_heis = 6;
    heif_hevm = 7;
    heif_hevs = 8;
    heif_mif1 = 9;
    heif_msf1 = 10;
    heif_avif = 11;
    heif_avis = 12;
    heif_reader_grow_status_size_reached = 0;
    heif_reader_grow_status_timeout = 1;
    heif_reader_grow_status_size_beyond_eof = 2;
    heif_depth_representation_type_uniform_inverse_Z = 0;
    heif_depth_representation_type_uniform_disparity = 1;
    heif_depth_representation_type_uniform_Z = 2;
    heif_depth_representation_type_nonuniform_disparity = 3;

  LIBHEIF_AUX_IMAGE_FILTER_OMIT_ALPHA = 1 shl 1;  
  LIBHEIF_AUX_IMAGE_FILTER_OMIT_DEPTH = 2 shl 1;  
    heif_color_profile_type_not_present = 0;
    heif_color_profile_type_nclx = (((ord('n') shl 24) or (ord('c') shl 16)) or (ord('l') shl 8)) or ord('x');
    heif_color_profile_type_rICC = (((ord('r') shl 24) or (ord('I') shl 16)) or (ord('C') shl 8)) or ord('C');
    heif_color_profile_type_prof = (((ord('p') shl 24) or (ord('r') shl 16)) or (ord('o') shl 8)) or ord('f');
    heif_color_primaries_ITU_R_BT_709_5 = 1;
    heif_color_primaries_unspecified = 2;
    heif_color_primaries_ITU_R_BT_470_6_System_M = 4;
    heif_color_primaries_ITU_R_BT_470_6_System_B_G = 5;
    heif_color_primaries_ITU_R_BT_601_6 = 6;
    heif_color_primaries_SMPTE_240M = 7;
    heif_color_primaries_generic_film = 8;
    heif_color_primaries_ITU_R_BT_2020_2_and_2100_0 = 9;
    heif_color_primaries_SMPTE_ST_428_1 = 10;
    heif_color_primaries_SMPTE_RP_431_2 = 11;
    heif_color_primaries_SMPTE_EG_432_1 = 12;
    heif_color_primaries_EBU_Tech_3213_E = 22;
    heif_transfer_characteristic_ITU_R_BT_709_5 = 1;
    heif_transfer_characteristic_unspecified = 2;
    heif_transfer_characteristic_ITU_R_BT_470_6_System_M = 4;
    heif_transfer_characteristic_ITU_R_BT_470_6_System_B_G = 5;
    heif_transfer_characteristic_ITU_R_BT_601_6 = 6;
    heif_transfer_characteristic_SMPTE_240M = 7;
    heif_transfer_characteristic_linear = 8;
    heif_transfer_characteristic_logarithmic_100 = 9;
    heif_transfer_characteristic_logarithmic_100_sqrt10 = 10;
    heif_transfer_characteristic_IEC_61966_2_4 = 11;
    heif_transfer_characteristic_ITU_R_BT_1361 = 12;
    heif_transfer_characteristic_IEC_61966_2_1 = 13;
    heif_transfer_characteristic_ITU_R_BT_2020_2_10bit = 14;
    heif_transfer_characteristic_ITU_R_BT_2020_2_12bit = 15;
    heif_transfer_characteristic_ITU_R_BT_2100_0_PQ = 16;
    heif_transfer_characteristic_SMPTE_ST_428_1 = 17;
    heif_transfer_characteristic_ITU_R_BT_2100_0_HLG = 18;
    heif_matrix_coefficients_RGB_GBR = 0;
    heif_matrix_coefficients_ITU_R_BT_709_5 = 1;
    heif_matrix_coefficients_unspecified = 2;
    heif_matrix_coefficients_US_FCC_T47 = 4;
    heif_matrix_coefficients_ITU_R_BT_470_6_System_B_G = 5;
    heif_matrix_coefficients_ITU_R_BT_601_6 = 6;
    heif_matrix_coefficients_SMPTE_240M = 7;
    heif_matrix_coefficients_YCgCo = 8;
    heif_matrix_coefficients_ITU_R_BT_2020_2_non_constant_luminance = 9;
    heif_matrix_coefficients_ITU_R_BT_2020_2_constant_luminance = 10;
    heif_matrix_coefficients_SMPTE_ST_2085 = 11;
    heif_matrix_coefficients_chromaticity_derived_non_constant_luminance = 12;
    heif_matrix_coefficients_chromaticity_derived_constant_luminance = 13;
    heif_matrix_coefficients_ICtCp = 14;
    heif_compression_undefined = 0;
    heif_compression_HEVC = 1;
    heif_compression_AVC = 2;
    heif_compression_JPEG = 3;
    heif_compression_AV1 = 4;
    heif_chroma_undefined = 99;
    heif_chroma_monochrome = 0;
    heif_chroma_420 = 1;
    heif_chroma_422 = 2;
    heif_chroma_444 = 3;
    heif_chroma_interleaved_RGB = 10;
    heif_chroma_interleaved_RGBA = 11;
    heif_chroma_interleaved_RRGGBB_BE = 12;
    heif_chroma_interleaved_RRGGBBAA_BE = 13;
    heif_chroma_interleaved_RRGGBB_LE = 14;
    heif_chroma_interleaved_RRGGBBAA_LE = 15;

  heif_chroma_interleaved_24bit = heif_chroma_interleaved_RGB;  
  heif_chroma_interleaved_32bit = heif_chroma_interleaved_RGBA;  
    heif_colorspace_undefined = 99;
    heif_colorspace_YCbCr = 0;
    heif_colorspace_RGB = 1;
    heif_colorspace_monochrome = 2;
    heif_channel_Y = 0;
    heif_channel_Cb = 1;
    heif_channel_Cr = 2;
    heif_channel_R = 3;
    heif_channel_G = 4;
    heif_channel_B = 5;
    heif_channel_Alpha = 6;
    heif_channel_interleaved = 10;
    heif_progress_step_total = 0;
    heif_progress_step_load_tile = 1;
    heif_encoder_parameter_type_integer = 1;
    heif_encoder_parameter_type_boolean = 2;
    heif_encoder_parameter_type_string = 3;
type
  Pheif_context = ^Theif_context;
  Pheif_decoder_plugin = ^Theif_decoder_plugin;
  Pheif_encoder = ^Theif_encoder;
  Pheif_encoder_descriptor = ^Theif_encoder_descriptor;
  Pheif_encoder_parameter = ^Theif_encoder_parameter;
  Pheif_encoder_plugin = ^Theif_encoder_plugin;
  Pheif_image = ^Theif_image;
  Pheif_image_handle = ^Theif_image_handle;
  Pheif_reading_options = ^Theif_reading_options;
  Pheif_scaling_options = ^Theif_scaling_options;
  Ppcchar = ^pcchar;
  Ppcint = ^pcint;
  PPheif_brand2 = ^Pheif_brand2;
  PPheif_color_profile_nclx = ^Pheif_color_profile_nclx;
  PPheif_depth_representation_info = ^Pheif_depth_representation_info;
  PPheif_encoder = ^Pheif_encoder;
  PPheif_encoder_descriptor = ^Pheif_encoder_descriptor;
  PPheif_encoder_parameter = ^Pheif_encoder_parameter;
  PPheif_image = ^Pheif_image;
  PPheif_image_handle = ^Pheif_image_handle;
  PPpcchar = ^Ppcchar;

  Theif_context = record
      {undefined structure}
    end;
  Theif_image_handle = record
      {undefined structure}
    end;
  Theif_image = record
      {undefined structure}
    end;
  Theif_error_code =  Longint;

  Theif_suberror_code =  Longint;

  Pheif_error = ^Theif_error;
  Theif_error = record
      code : Theif_error_code;
      subcode : Theif_suberror_code;
      message : pcchar;
    end;


  Pheif_item_id = ^Theif_item_id;
  Theif_item_id = cuint32;
  Theif_filetype_result =  Longint;
  Theif_brand =  Longint;
  Pheif_brand2 = ^Theif_brand2;
  Theif_brand2 = cuint32;
  Theif_reading_options = record
      {undefined structure}
    end;
  Theif_reader_grow_status =  Longint;

  Pheif_reader = ^Theif_reader;
  Theif_reader = record
      reader_api_version : cint;
      get_position : function (userdata:pointer):cint64;cdecl;
      read : function (data:pointer; size:csize_t; userdata:pointer):cint;cdecl;
      seek : function (position:cint64; userdata:pointer):cint;cdecl;
      wait_for_file_size : function (target_size:cint64; userdata:pointer):Theif_reader_grow_status;cdecl;
    end;
  Theif_depth_representation_type =  Longint;

  Pheif_depth_representation_info = ^Theif_depth_representation_info;
  Theif_depth_representation_info = record
      version : cuint8;
      has_z_near : cuint8;
      has_z_far : cuint8;
      has_d_min : cuint8;
      has_d_max : cuint8;
      z_near : cdouble;
      z_far : cdouble;
      d_min : cdouble;
      d_max : cdouble;
      depth_representation_type : Theif_depth_representation_type;
      disparity_reference_view : cuint32;
      depth_nonlinear_representation_model_size : cuint32;
      depth_nonlinear_representation_model : pcuint8;
    end;
  Theif_color_profile_type =  Longint;
  Theif_color_primaries =  Longint;

  Theif_transfer_characteristics =  Longint;

  Theif_matrix_coefficients =  Longint;

  Pheif_color_profile_nclx = ^Theif_color_profile_nclx;
  Theif_color_profile_nclx = record
      version : cuint8;
      color_primaries : Theif_color_primaries;
      transfer_characteristics : Theif_transfer_characteristics;
      matrix_coefficients : Theif_matrix_coefficients;
      full_range_flag : cuint8;
      color_primary_red_x : cfloat;
      color_primary_red_y : cfloat;
      color_primary_green_x : cfloat;
      color_primary_green_y : cfloat;
      color_primary_blue_x : cfloat;
      color_primary_blue_y : cfloat;
      color_primary_white_x : cfloat;
      color_primary_white_y : cfloat;
    end;
  Theif_compression_format =  Longint;

  Theif_chroma =  Longint;
  Theif_colorspace =  Longint;

  Theif_channel =  Longint;

  Theif_progress_step =  Longint;

  Pheif_decoding_options = ^Theif_decoding_options;
  Theif_decoding_options = record
      version : cuint8;
      ignore_transformations : cuint8;
      start_progress : procedure (step:Theif_progress_step; max_progress:cint; progress_user_data:pointer);cdecl;
      on_progress : procedure (step:Theif_progress_step; progress:cint; progress_user_data:pointer);cdecl;
      end_progress : procedure (step:Theif_progress_step; progress_user_data:pointer);cdecl;
      progress_user_data : pointer;
      convert_hdr_to_8bit : cuint8;
    end;
  Theif_scaling_options = record
      {undefined structure}
    end;
  Pheif_writer = ^Theif_writer;
  Theif_writer = record
      writer_api_version : cint;
      write : function (ctx:Pheif_context; data:pointer; size:csize_t; userdata:pointer):Theif_error;cdecl;
    end;
  Theif_encoder = record
      {undefined structure}
    end;
  Theif_encoder_descriptor = record
      {undefined structure}
    end;
  Theif_encoder_parameter = record
      {undefined structure}
    end;
  Theif_encoder_parameter_type =  Longint;
  Pheif_encoding_options = ^Theif_encoding_options;
  Theif_encoding_options = record
      version : cuint8;
      save_alpha_channel : cuint8;
      macOS_compatibility_workaround : cuint8;
      save_two_colr_boxes_when_ICC_and_nclx_available : cuint8;
      output_nclx_profile : Pheif_color_profile_nclx;
      macOS_compatibility_workaround_no_nclx_profile : cuint8;
    end;
  Theif_decoder_plugin = record
      {undefined structure}
    end;
  Theif_encoder_plugin = record
      {undefined structure}
    end;



function heif_fourcc(a,b,c,d : longint) : longint;
var


heif_get_version : function:pcchar;cdecl;
heif_get_version_number : function:cuint32;cdecl;
heif_get_version_number_major : function:cint;cdecl;
heif_get_version_number_minor : function:cint;cdecl;
heif_get_version_number_maintenance : function:cint;cdecl;

function LIBHEIF_MAKE_VERSION(h,m,l : longint) : longint;

var

heif_check_filetype : function(data:pcuint8; len:cint):Theif_filetype_result;cdecl;

heif_main_brand : function(data:pcuint8; len:cint):Theif_brand;cdecl;
heif_read_main_brand : function(data:pcuint8; len:cint):Theif_brand2;cdecl;
heif_fourcc_to_brand : function(brand_fourcc:pcchar):Theif_brand2;cdecl;
heif_brand_to_fourcc : procedure(brand:Theif_brand2; out_fourcc:pcchar);cdecl;
heif_has_compatible_brand : function(data:pcuint8; len:cint; brand_fourcc:pcchar):cint;cdecl;
heif_list_compatible_brands : function(data:pcuint8; len:cint; out_brands:PPheif_brand2; out_size:pcint):Theif_error;cdecl;
heif_free_list_of_compatible_brands : procedure(brands_list:Pheif_brand2);cdecl;
heif_get_file_mime_type : function(data:pcuint8; len:cint):pcchar;cdecl;
heif_context_alloc : function:Pheif_context;cdecl;
heif_context_free : procedure(para1:Pheif_context);cdecl;

heif_context_read_from_file : function(para1:Pheif_context; filename:pcchar; para3:Pheif_reading_options):Theif_error;cdecl;
heif_context_read_from_memory : function(para1:Pheif_context; mem:pointer; size:csize_t; para4:Pheif_reading_options):Theif_error;cdecl;
heif_context_read_from_memory_without_copy : function(para1:Pheif_context; mem:pointer; size:csize_t; para4:Pheif_reading_options):Theif_error;cdecl;
heif_context_read_from_reader : function(para1:Pheif_context; reader:Pheif_reader; userdata:pointer; para4:Pheif_reading_options):Theif_error;cdecl;
heif_context_get_number_of_top_level_images : function(ctx:Pheif_context):cint;cdecl;
heif_context_is_top_level_image_ID : function(ctx:Pheif_context; id:Theif_item_id):cint;cdecl;
heif_context_get_list_of_top_level_image_IDs : function(ctx:Pheif_context; ID_array:Pheif_item_id; count:cint):cint;cdecl;
heif_context_get_primary_image_ID : function(ctx:Pheif_context; id:Pheif_item_id):Theif_error;cdecl;
heif_context_get_primary_image_handle : function(ctx:Pheif_context; para2:PPheif_image_handle):Theif_error;cdecl;
heif_context_get_image_handle : function(ctx:Pheif_context; id:Theif_item_id; para3:PPheif_image_handle):Theif_error;cdecl;
heif_context_debug_dump_boxes_to_file : procedure(ctx:Pheif_context; fd:cint);cdecl;
heif_context_set_maximum_image_size_limit : procedure(ctx:Pheif_context; maximum_width:cint);cdecl;
heif_image_handle_release : procedure(para1:Pheif_image_handle);cdecl;
heif_image_handle_is_primary_image : function(handle:Pheif_image_handle):cint;cdecl;
heif_image_handle_get_width : function(handle:Pheif_image_handle):cint;cdecl;
heif_image_handle_get_height : function(handle:Pheif_image_handle):cint;cdecl;
heif_image_handle_has_alpha_channel : function(para1:Pheif_image_handle):cint;cdecl;
heif_image_handle_is_premultiplied_alpha : function(para1:Pheif_image_handle):cint;cdecl;
heif_image_handle_get_luma_bits_per_pixel : function(para1:Pheif_image_handle):cint;cdecl;
heif_image_handle_get_chroma_bits_per_pixel : function(para1:Pheif_image_handle):cint;cdecl;
heif_image_handle_get_ispe_width : function(handle:Pheif_image_handle):cint;cdecl;
heif_image_handle_get_ispe_height : function(handle:Pheif_image_handle):cint;cdecl;
heif_image_handle_has_depth_image : function(para1:Pheif_image_handle):cint;cdecl;
heif_image_handle_get_number_of_depth_images : function(handle:Pheif_image_handle):cint;cdecl;
heif_image_handle_get_list_of_depth_image_IDs : function(handle:Pheif_image_handle; ids:Pheif_item_id; count:cint):cint;cdecl;
heif_image_handle_get_depth_image_handle : function(handle:Pheif_image_handle; depth_image_id:Theif_item_id; out_depth_handle:PPheif_image_handle):Theif_error;cdecl;

heif_depth_representation_info_free : procedure(info:Pheif_depth_representation_info);cdecl;
heif_image_handle_get_depth_image_representation_info : function(handle:Pheif_image_handle; depth_image_id:Theif_item_id; _out:PPheif_depth_representation_info):cint;cdecl;
heif_image_handle_get_number_of_thumbnails : function(handle:Pheif_image_handle):cint;cdecl;
heif_image_handle_get_list_of_thumbnail_IDs : function(handle:Pheif_image_handle; ids:Pheif_item_id; count:cint):cint;cdecl;
heif_image_handle_get_thumbnail : function(main_image_handle:Pheif_image_handle; thumbnail_id:Theif_item_id; out_thumbnail_handle:PPheif_image_handle):Theif_error;cdecl;
heif_image_handle_get_number_of_auxiliary_images : function(handle:Pheif_image_handle; aux_filter:cint):cint;cdecl;
heif_image_handle_get_list_of_auxiliary_image_IDs : function(handle:Pheif_image_handle; aux_filter:cint; ids:Pheif_item_id; count:cint):cint;cdecl;
heif_image_handle_get_auxiliary_type : function(handle:Pheif_image_handle; out_type:Ppcchar):Theif_error;cdecl;
heif_image_handle_free_auxiliary_types : procedure(handle:Pheif_image_handle; out_type:Ppcchar);cdecl;
heif_image_handle_get_auxiliary_image_handle : function(main_image_handle:Pheif_image_handle; auxiliary_id:Theif_item_id; out_auxiliary_handle:PPheif_image_handle):Theif_error;cdecl;
heif_image_handle_get_number_of_metadata_blocks : function(handle:Pheif_image_handle; type_filter:pcchar):cint;cdecl;
heif_image_handle_get_list_of_metadata_block_IDs : function(handle:Pheif_image_handle; type_filter:pcchar; ids:Pheif_item_id; count:cint):cint;cdecl;
heif_image_handle_get_metadata_type : function(handle:Pheif_image_handle; metadata_id:Theif_item_id):pcchar;cdecl;
heif_image_handle_get_metadata_content_type : function(handle:Pheif_image_handle; metadata_id:Theif_item_id):pcchar;cdecl;
heif_image_handle_get_metadata_size : function(handle:Pheif_image_handle; metadata_id:Theif_item_id):csize_t;cdecl;
heif_image_handle_get_metadata : function(handle:Pheif_image_handle; metadata_id:Theif_item_id; out_data:pointer):Theif_error;cdecl;

heif_image_handle_get_color_profile_type : function(handle:Pheif_image_handle):Theif_color_profile_type;cdecl;
heif_image_handle_get_raw_color_profile_size : function(handle:Pheif_image_handle):csize_t;cdecl;
heif_image_handle_get_raw_color_profile : function(handle:Pheif_image_handle; out_data:pointer):Theif_error;cdecl;

heif_image_handle_get_nclx_color_profile : function(handle:Pheif_image_handle; out_data:PPheif_color_profile_nclx):Theif_error;cdecl;
heif_nclx_color_profile_alloc : function:Pheif_color_profile_nclx;cdecl;
heif_nclx_color_profile_free : procedure(nclx_profile:Pheif_color_profile_nclx);cdecl;
heif_image_get_color_profile_type : function(image:Pheif_image):Theif_color_profile_type;cdecl;
heif_image_get_raw_color_profile_size : function(image:Pheif_image):csize_t;cdecl;
heif_image_get_raw_color_profile : function(image:Pheif_image; out_data:pointer):Theif_error;cdecl;
heif_image_get_nclx_color_profile : function(image:Pheif_image; out_data:PPheif_color_profile_nclx):Theif_error;cdecl;

heif_decoding_options_alloc : function:Pheif_decoding_options;cdecl;
heif_decoding_options_free : procedure(para1:Pheif_decoding_options);cdecl;
heif_decode_image : function(in_handle:Pheif_image_handle; out_img:PPheif_image; colorspace:Theif_colorspace; chroma:Theif_chroma; options:Pheif_decoding_options):Theif_error;cdecl;
heif_image_get_colorspace : function(para1:Pheif_image):Theif_colorspace;cdecl;
heif_image_get_chroma_format : function(para1:Pheif_image):Theif_chroma;cdecl;
heif_image_get_width : function(para1:Pheif_image; channel:Theif_channel):cint;cdecl;
heif_image_get_height : function(para1:Pheif_image; channel:Theif_channel):cint;cdecl;
heif_image_get_primary_width : function(para1:Pheif_image):cint;cdecl;
heif_image_get_primary_height : function(para1:Pheif_image):cint;cdecl;
heif_image_crop : function(img:Pheif_image; left:cint; right:cint; top:cint; bottom:cint):Theif_error;cdecl;
heif_image_get_bits_per_pixel : function(para1:Pheif_image; channel:Theif_channel):cint;cdecl;
heif_image_get_bits_per_pixel_range : function(para1:Pheif_image; channel:Theif_channel):cint;cdecl;
heif_image_has_channel : function(para1:Pheif_image; channel:Theif_channel):cint;cdecl;
heif_image_get_plane_readonly : function(para1:Pheif_image; channel:Theif_channel; out_stride:pcint):pcuint8;cdecl;
heif_image_get_plane : function(para1:Pheif_image; channel:Theif_channel; out_stride:pcint):pcuint8;cdecl;
heif_image_scale_image : function(input:Pheif_image; output:PPheif_image; width:cint; height:cint; options:Pheif_scaling_options):Theif_error;cdecl;
heif_image_set_raw_color_profile : function(image:Pheif_image; profile_type_fourcc_string:pcchar; profile_data:pointer; profile_size:csize_t):Theif_error;cdecl;
heif_image_set_nclx_color_profile : function(image:Pheif_image; color_profile:Pheif_color_profile_nclx):Theif_error;cdecl;
heif_image_release : procedure(para1:Pheif_image);cdecl;
heif_context_write_to_file : function(para1:Pheif_context; filename:pcchar):Theif_error;cdecl;

heif_context_write : function(para1:Pheif_context; writer:Pheif_writer; userdata:pointer):Theif_error;cdecl;
heif_context_get_encoder_descriptors : function(para1:Pheif_context; format_filter:Theif_compression_format; name_filter:pcchar; out_encoders:PPheif_encoder_descriptor; count:cint):cint;cdecl;
heif_encoder_descriptor_get_name : function(para1:Pheif_encoder_descriptor):pcchar;cdecl;
heif_encoder_descriptor_get_id_name : function(para1:Pheif_encoder_descriptor):pcchar;cdecl;
heif_encoder_descriptor_get_compression_format : function(para1:Pheif_encoder_descriptor):Theif_compression_format;cdecl;
heif_encoder_descriptor_supports_lossy_compression : function(para1:Pheif_encoder_descriptor):cint;cdecl;
heif_encoder_descriptor_supports_lossless_compression : function(para1:Pheif_encoder_descriptor):cint;cdecl;
heif_context_get_encoder : function(context:Pheif_context; para2:Pheif_encoder_descriptor; out_encoder:PPheif_encoder):Theif_error;cdecl;
heif_have_decoder_for_format : function(format:Theif_compression_format):cint;cdecl;
heif_have_encoder_for_format : function(format:Theif_compression_format):cint;cdecl;
heif_context_get_encoder_for_format : function(context:Pheif_context; format:Theif_compression_format; para3:PPheif_encoder):Theif_error;cdecl;
heif_encoder_release : procedure(para1:Pheif_encoder);cdecl;
heif_encoder_get_name : function(para1:Pheif_encoder):pcchar;cdecl;
heif_encoder_set_lossy_quality : function(para1:Pheif_encoder; quality:cint):Theif_error;cdecl;
heif_encoder_set_lossless : function(para1:Pheif_encoder; enable:cint):Theif_error;cdecl;
heif_encoder_set_logging_level : function(para1:Pheif_encoder; level:cint):Theif_error;cdecl;
heif_encoder_list_parameters : function(para1:Pheif_encoder):PPheif_encoder_parameter;cdecl;
heif_encoder_parameter_get_name : function(para1:Pheif_encoder_parameter):pcchar;cdecl;

heif_encoder_parameter_get_type : function(para1:Pheif_encoder_parameter):Theif_encoder_parameter_type;cdecl;
heif_encoder_parameter_get_valid_integer_range : function(para1:Pheif_encoder_parameter; have_minimum_maximum:pcint; minimum:pcint; maximum:pcint):Theif_error;cdecl;
heif_encoder_parameter_get_valid_integer_values : function(para1:Pheif_encoder_parameter; have_minimum:pcint; have_maximum:pcint; minimum:pcint; maximum:pcint; 
    num_valid_values:pcint; out_integer_array:Ppcint):Theif_error;cdecl;
heif_encoder_parameter_get_valid_string_values : function(para1:Pheif_encoder_parameter; out_stringarray:PPpcchar):Theif_error;cdecl;
heif_encoder_set_parameter_integer : function(para1:Pheif_encoder; parameter_name:pcchar; value:cint):Theif_error;cdecl;
heif_encoder_get_parameter_integer : function(para1:Pheif_encoder; parameter_name:pcchar; value:pcint):Theif_error;cdecl;
heif_encoder_parameter_integer_valid_range : function(para1:Pheif_encoder; parameter_name:pcchar; have_minimum_maximum:pcint; minimum:pcint; maximum:pcint):Theif_error;cdecl;
heif_encoder_set_parameter_boolean : function(para1:Pheif_encoder; parameter_name:pcchar; value:cint):Theif_error;cdecl;
heif_encoder_get_parameter_boolean : function(para1:Pheif_encoder; parameter_name:pcchar; value:pcint):Theif_error;cdecl;
heif_encoder_set_parameter_string : function(para1:Pheif_encoder; parameter_name:pcchar; value:pcchar):Theif_error;cdecl;
heif_encoder_get_parameter_string : function(para1:Pheif_encoder; parameter_name:pcchar; value:pcchar; value_size:cint):Theif_error;cdecl;
heif_encoder_parameter_string_valid_values : function(para1:Pheif_encoder; parameter_name:pcchar; out_stringarray:PPpcchar):Theif_error;cdecl;
heif_encoder_parameter_integer_valid_values : function(para1:Pheif_encoder; parameter_name:pcchar; have_minimum:pcint; have_maximum:pcint; minimum:pcint; 
    maximum:pcint; num_valid_values:pcint; out_integer_array:Ppcint):Theif_error;cdecl;
heif_encoder_set_parameter : function(para1:Pheif_encoder; parameter_name:pcchar; value:pcchar):Theif_error;cdecl;
heif_encoder_get_parameter : function(para1:Pheif_encoder; parameter_name:pcchar; value_ptr:pcchar; value_size:cint):Theif_error;cdecl;
heif_encoder_has_default : function(para1:Pheif_encoder; parameter_name:pcchar):cint;cdecl;

heif_encoding_options_alloc : function:Pheif_encoding_options;cdecl;
heif_encoding_options_free : procedure(para1:Pheif_encoding_options);cdecl;
heif_context_encode_image : function(para1:Pheif_context; image:Pheif_image; encoder:Pheif_encoder; options:Pheif_encoding_options; out_image_handle:PPheif_image_handle):Theif_error;cdecl;
heif_context_set_primary_image : function(para1:Pheif_context; image_handle:Pheif_image_handle):Theif_error;cdecl;
heif_context_encode_thumbnail : function(para1:Pheif_context; image:Pheif_image; master_image_handle:Pheif_image_handle; encoder:Pheif_encoder; options:Pheif_encoding_options; 
    bbox_size:cint; out_thumb_image_handle:PPheif_image_handle):Theif_error;cdecl;
heif_context_assign_thumbnail : function(para1:Pheif_context; master_image:Pheif_image_handle; thumbnail_image:Pheif_image_handle):Theif_error;cdecl;
heif_context_add_exif_metadata : function(para1:Pheif_context; image_handle:Pheif_image_handle; data:pointer; size:cint):Theif_error;cdecl;
heif_context_add_XMP_metadata : function(para1:Pheif_context; image_handle:Pheif_image_handle; data:pointer; size:cint):Theif_error;cdecl;
heif_context_add_generic_metadata : function(ctx:Pheif_context; image_handle:Pheif_image_handle; data:pointer; size:cint; item_type:pcchar; 
    content_type:pcchar):Theif_error;cdecl;
heif_image_create : function(width:cint; height:cint; colorspace:Theif_colorspace; chroma:Theif_chroma; out_image:PPheif_image):Theif_error;cdecl;
heif_image_add_plane : function(image:Pheif_image; channel:Theif_channel; width:cint; height:cint; bit_depth:cint):Theif_error;cdecl;
heif_image_set_premultiplied_alpha : procedure(image:Pheif_image; is_premultiplied_alpha:cint);cdecl;
heif_image_is_premultiplied_alpha : function(image:Pheif_image):cint;cdecl;
heif_register_decoder : function(heif:Pheif_context; para2:Pheif_decoder_plugin):Theif_error;cdecl;
heif_register_decoder_plugin : function(para1:Pheif_decoder_plugin):Theif_error;cdecl;
heif_register_encoder_plugin : function(para1:Pheif_encoder_plugin):Theif_error;cdecl;
heif_encoder_descriptor_supportes_lossy_compression : function(para1:Pheif_encoder_descriptor):cint;cdecl;
heif_encoder_descriptor_supportes_lossless_compression : function(para1:Pheif_encoder_descriptor):cint;cdecl;

implementation

uses
{$IFDEF FPC_DOTTEDUNITS}
    System.SysUtils, System.DynLibs;
{$ELSE}
    SysUtils, dynlibs;
{$ENDIF}

function heif_fourcc(a,b,c,d : longint) : longint;
begin
  heif_fourcc:=(((a shl 24) or (b shl 16)) or (c shl 8)) or d;
end;

function LIBHEIF_MAKE_VERSION(h,m,l : longint) : longint;
begin
  LIBHEIF_MAKE_VERSION:=((h shl 24) or (m shl 16)) or (l shl 8);
end;

  var
    hlib : tlibhandle;


  procedure Freelibheif;
    begin
      FreeLibrary(hlib);
      heif_get_version:=nil;
      heif_get_version_number:=nil;
      heif_get_version_number_major:=nil;
      heif_get_version_number_minor:=nil;
      heif_get_version_number_maintenance:=nil;
      heif_check_filetype:=nil;
      heif_main_brand:=nil;
      heif_read_main_brand:=nil;
      heif_fourcc_to_brand:=nil;
      heif_brand_to_fourcc:=nil;
      heif_has_compatible_brand:=nil;
      heif_list_compatible_brands:=nil;
      heif_free_list_of_compatible_brands:=nil;
      heif_get_file_mime_type:=nil;
      heif_context_alloc:=nil;
      heif_context_free:=nil;
      heif_context_read_from_file:=nil;
      heif_context_read_from_memory:=nil;
      heif_context_read_from_memory_without_copy:=nil;
      heif_context_read_from_reader:=nil;
      heif_context_get_number_of_top_level_images:=nil;
      heif_context_is_top_level_image_ID:=nil;
      heif_context_get_list_of_top_level_image_IDs:=nil;
      heif_context_get_primary_image_ID:=nil;
      heif_context_get_primary_image_handle:=nil;
      heif_context_get_image_handle:=nil;
      heif_context_debug_dump_boxes_to_file:=nil;
      heif_context_set_maximum_image_size_limit:=nil;
      heif_image_handle_release:=nil;
      heif_image_handle_is_primary_image:=nil;
      heif_image_handle_get_width:=nil;
      heif_image_handle_get_height:=nil;
      heif_image_handle_has_alpha_channel:=nil;
      heif_image_handle_is_premultiplied_alpha:=nil;
      heif_image_handle_get_luma_bits_per_pixel:=nil;
      heif_image_handle_get_chroma_bits_per_pixel:=nil;
      heif_image_handle_get_ispe_width:=nil;
      heif_image_handle_get_ispe_height:=nil;
      heif_image_handle_has_depth_image:=nil;
      heif_image_handle_get_number_of_depth_images:=nil;
      heif_image_handle_get_list_of_depth_image_IDs:=nil;
      heif_image_handle_get_depth_image_handle:=nil;
      heif_depth_representation_info_free:=nil;
      heif_image_handle_get_depth_image_representation_info:=nil;
      heif_image_handle_get_number_of_thumbnails:=nil;
      heif_image_handle_get_list_of_thumbnail_IDs:=nil;
      heif_image_handle_get_thumbnail:=nil;
      heif_image_handle_get_number_of_auxiliary_images:=nil;
      heif_image_handle_get_list_of_auxiliary_image_IDs:=nil;
      heif_image_handle_get_auxiliary_type:=nil;
      heif_image_handle_free_auxiliary_types:=nil;
      heif_image_handle_get_auxiliary_image_handle:=nil;
      heif_image_handle_get_number_of_metadata_blocks:=nil;
      heif_image_handle_get_list_of_metadata_block_IDs:=nil;
      heif_image_handle_get_metadata_type:=nil;
      heif_image_handle_get_metadata_content_type:=nil;
      heif_image_handle_get_metadata_size:=nil;
      heif_image_handle_get_metadata:=nil;
      heif_image_handle_get_color_profile_type:=nil;
      heif_image_handle_get_raw_color_profile_size:=nil;
      heif_image_handle_get_raw_color_profile:=nil;
      heif_image_handle_get_nclx_color_profile:=nil;
      heif_nclx_color_profile_alloc:=nil;
      heif_nclx_color_profile_free:=nil;
      heif_image_get_color_profile_type:=nil;
      heif_image_get_raw_color_profile_size:=nil;
      heif_image_get_raw_color_profile:=nil;
      heif_image_get_nclx_color_profile:=nil;
      heif_decoding_options_alloc:=nil;
      heif_decoding_options_free:=nil;
      heif_decode_image:=nil;
      heif_image_get_colorspace:=nil;
      heif_image_get_chroma_format:=nil;
      heif_image_get_width:=nil;
      heif_image_get_height:=nil;
      heif_image_get_primary_width:=nil;
      heif_image_get_primary_height:=nil;
      heif_image_crop:=nil;
      heif_image_get_bits_per_pixel:=nil;
      heif_image_get_bits_per_pixel_range:=nil;
      heif_image_has_channel:=nil;
      heif_image_get_plane_readonly:=nil;
      heif_image_get_plane:=nil;
      heif_image_scale_image:=nil;
      heif_image_set_raw_color_profile:=nil;
      heif_image_set_nclx_color_profile:=nil;
      heif_image_release:=nil;
      heif_context_write_to_file:=nil;
      heif_context_write:=nil;
      heif_context_get_encoder_descriptors:=nil;
      heif_encoder_descriptor_get_name:=nil;
      heif_encoder_descriptor_get_id_name:=nil;
      heif_encoder_descriptor_get_compression_format:=nil;
      heif_encoder_descriptor_supports_lossy_compression:=nil;
      heif_encoder_descriptor_supports_lossless_compression:=nil;
      heif_context_get_encoder:=nil;
      heif_have_decoder_for_format:=nil;
      heif_have_encoder_for_format:=nil;
      heif_context_get_encoder_for_format:=nil;
      heif_encoder_release:=nil;
      heif_encoder_get_name:=nil;
      heif_encoder_set_lossy_quality:=nil;
      heif_encoder_set_lossless:=nil;
      heif_encoder_set_logging_level:=nil;
      heif_encoder_list_parameters:=nil;
      heif_encoder_parameter_get_name:=nil;
      heif_encoder_parameter_get_type:=nil;
      heif_encoder_parameter_get_valid_integer_range:=nil;
      heif_encoder_parameter_get_valid_integer_values:=nil;
      heif_encoder_parameter_get_valid_string_values:=nil;
      heif_encoder_set_parameter_integer:=nil;
      heif_encoder_get_parameter_integer:=nil;
      heif_encoder_parameter_integer_valid_range:=nil;
      heif_encoder_set_parameter_boolean:=nil;
      heif_encoder_get_parameter_boolean:=nil;
      heif_encoder_set_parameter_string:=nil;
      heif_encoder_get_parameter_string:=nil;
      heif_encoder_parameter_string_valid_values:=nil;
      heif_encoder_parameter_integer_valid_values:=nil;
      heif_encoder_set_parameter:=nil;
      heif_encoder_get_parameter:=nil;
      heif_encoder_has_default:=nil;
      heif_encoding_options_alloc:=nil;
      heif_encoding_options_free:=nil;
      heif_context_encode_image:=nil;
      heif_context_set_primary_image:=nil;
      heif_context_encode_thumbnail:=nil;
      heif_context_assign_thumbnail:=nil;
      heif_context_add_exif_metadata:=nil;
      heif_context_add_XMP_metadata:=nil;
      heif_context_add_generic_metadata:=nil;
      heif_image_create:=nil;
      heif_image_add_plane:=nil;
      heif_image_set_premultiplied_alpha:=nil;
      heif_image_is_premultiplied_alpha:=nil;
      heif_register_decoder:=nil;
      heif_register_decoder_plugin:=nil;
      heif_register_encoder_plugin:=nil;
      heif_encoder_descriptor_supportes_lossy_compression:=nil;
      heif_encoder_descriptor_supportes_lossless_compression:=nil;
    end;


  procedure Loadlibheif(lib : pchar);
    begin
      Freelibheif;
      hlib:=LoadLibrary(lib);
      if hlib=0 then
        raise Exception.Create(format('Could not load library: %s',[lib]));

      pointer(heif_get_version):=GetProcAddress(hlib,'heif_get_version');
      pointer(heif_get_version_number):=GetProcAddress(hlib,'heif_get_version_number');
      pointer(heif_get_version_number_major):=GetProcAddress(hlib,'heif_get_version_number_major');
      pointer(heif_get_version_number_minor):=GetProcAddress(hlib,'heif_get_version_number_minor');
      pointer(heif_get_version_number_maintenance):=GetProcAddress(hlib,'heif_get_version_number_maintenance');
      pointer(heif_check_filetype):=GetProcAddress(hlib,'heif_check_filetype');
      pointer(heif_main_brand):=GetProcAddress(hlib,'heif_main_brand');
      pointer(heif_read_main_brand):=GetProcAddress(hlib,'heif_read_main_brand');
      pointer(heif_fourcc_to_brand):=GetProcAddress(hlib,'heif_fourcc_to_brand');
      pointer(heif_brand_to_fourcc):=GetProcAddress(hlib,'heif_brand_to_fourcc');
      pointer(heif_has_compatible_brand):=GetProcAddress(hlib,'heif_has_compatible_brand');
      pointer(heif_list_compatible_brands):=GetProcAddress(hlib,'heif_list_compatible_brands');
      pointer(heif_free_list_of_compatible_brands):=GetProcAddress(hlib,'heif_free_list_of_compatible_brands');
      pointer(heif_get_file_mime_type):=GetProcAddress(hlib,'heif_get_file_mime_type');
      pointer(heif_context_alloc):=GetProcAddress(hlib,'heif_context_alloc');
      pointer(heif_context_free):=GetProcAddress(hlib,'heif_context_free');
      pointer(heif_context_read_from_file):=GetProcAddress(hlib,'heif_context_read_from_file');
      pointer(heif_context_read_from_memory):=GetProcAddress(hlib,'heif_context_read_from_memory');
      pointer(heif_context_read_from_memory_without_copy):=GetProcAddress(hlib,'heif_context_read_from_memory_without_copy');
      pointer(heif_context_read_from_reader):=GetProcAddress(hlib,'heif_context_read_from_reader');
      pointer(heif_context_get_number_of_top_level_images):=GetProcAddress(hlib,'heif_context_get_number_of_top_level_images');
      pointer(heif_context_is_top_level_image_ID):=GetProcAddress(hlib,'heif_context_is_top_level_image_ID');
      pointer(heif_context_get_list_of_top_level_image_IDs):=GetProcAddress(hlib,'heif_context_get_list_of_top_level_image_IDs');
      pointer(heif_context_get_primary_image_ID):=GetProcAddress(hlib,'heif_context_get_primary_image_ID');
      pointer(heif_context_get_primary_image_handle):=GetProcAddress(hlib,'heif_context_get_primary_image_handle');
      pointer(heif_context_get_image_handle):=GetProcAddress(hlib,'heif_context_get_image_handle');
      pointer(heif_context_debug_dump_boxes_to_file):=GetProcAddress(hlib,'heif_context_debug_dump_boxes_to_file');
      pointer(heif_context_set_maximum_image_size_limit):=GetProcAddress(hlib,'heif_context_set_maximum_image_size_limit');
      pointer(heif_image_handle_release):=GetProcAddress(hlib,'heif_image_handle_release');
      pointer(heif_image_handle_is_primary_image):=GetProcAddress(hlib,'heif_image_handle_is_primary_image');
      pointer(heif_image_handle_get_width):=GetProcAddress(hlib,'heif_image_handle_get_width');
      pointer(heif_image_handle_get_height):=GetProcAddress(hlib,'heif_image_handle_get_height');
      pointer(heif_image_handle_has_alpha_channel):=GetProcAddress(hlib,'heif_image_handle_has_alpha_channel');
      pointer(heif_image_handle_is_premultiplied_alpha):=GetProcAddress(hlib,'heif_image_handle_is_premultiplied_alpha');
      pointer(heif_image_handle_get_luma_bits_per_pixel):=GetProcAddress(hlib,'heif_image_handle_get_luma_bits_per_pixel');
      pointer(heif_image_handle_get_chroma_bits_per_pixel):=GetProcAddress(hlib,'heif_image_handle_get_chroma_bits_per_pixel');
      pointer(heif_image_handle_get_ispe_width):=GetProcAddress(hlib,'heif_image_handle_get_ispe_width');
      pointer(heif_image_handle_get_ispe_height):=GetProcAddress(hlib,'heif_image_handle_get_ispe_height');
      pointer(heif_image_handle_has_depth_image):=GetProcAddress(hlib,'heif_image_handle_has_depth_image');
      pointer(heif_image_handle_get_number_of_depth_images):=GetProcAddress(hlib,'heif_image_handle_get_number_of_depth_images');
      pointer(heif_image_handle_get_list_of_depth_image_IDs):=GetProcAddress(hlib,'heif_image_handle_get_list_of_depth_image_IDs');
      pointer(heif_image_handle_get_depth_image_handle):=GetProcAddress(hlib,'heif_image_handle_get_depth_image_handle');
      pointer(heif_depth_representation_info_free):=GetProcAddress(hlib,'heif_depth_representation_info_free');
      pointer(heif_image_handle_get_depth_image_representation_info):=GetProcAddress(hlib,'heif_image_handle_get_depth_image_representation_info');
      pointer(heif_image_handle_get_number_of_thumbnails):=GetProcAddress(hlib,'heif_image_handle_get_number_of_thumbnails');
      pointer(heif_image_handle_get_list_of_thumbnail_IDs):=GetProcAddress(hlib,'heif_image_handle_get_list_of_thumbnail_IDs');
      pointer(heif_image_handle_get_thumbnail):=GetProcAddress(hlib,'heif_image_handle_get_thumbnail');
      pointer(heif_image_handle_get_number_of_auxiliary_images):=GetProcAddress(hlib,'heif_image_handle_get_number_of_auxiliary_images');
      pointer(heif_image_handle_get_list_of_auxiliary_image_IDs):=GetProcAddress(hlib,'heif_image_handle_get_list_of_auxiliary_image_IDs');
      pointer(heif_image_handle_get_auxiliary_type):=GetProcAddress(hlib,'heif_image_handle_get_auxiliary_type');
      pointer(heif_image_handle_free_auxiliary_types):=GetProcAddress(hlib,'heif_image_handle_free_auxiliary_types');
      pointer(heif_image_handle_get_auxiliary_image_handle):=GetProcAddress(hlib,'heif_image_handle_get_auxiliary_image_handle');
      pointer(heif_image_handle_get_number_of_metadata_blocks):=GetProcAddress(hlib,'heif_image_handle_get_number_of_metadata_blocks');
      pointer(heif_image_handle_get_list_of_metadata_block_IDs):=GetProcAddress(hlib,'heif_image_handle_get_list_of_metadata_block_IDs');
      pointer(heif_image_handle_get_metadata_type):=GetProcAddress(hlib,'heif_image_handle_get_metadata_type');
      pointer(heif_image_handle_get_metadata_content_type):=GetProcAddress(hlib,'heif_image_handle_get_metadata_content_type');
      pointer(heif_image_handle_get_metadata_size):=GetProcAddress(hlib,'heif_image_handle_get_metadata_size');
      pointer(heif_image_handle_get_metadata):=GetProcAddress(hlib,'heif_image_handle_get_metadata');
      pointer(heif_image_handle_get_color_profile_type):=GetProcAddress(hlib,'heif_image_handle_get_color_profile_type');
      pointer(heif_image_handle_get_raw_color_profile_size):=GetProcAddress(hlib,'heif_image_handle_get_raw_color_profile_size');
      pointer(heif_image_handle_get_raw_color_profile):=GetProcAddress(hlib,'heif_image_handle_get_raw_color_profile');
      pointer(heif_image_handle_get_nclx_color_profile):=GetProcAddress(hlib,'heif_image_handle_get_nclx_color_profile');
      pointer(heif_nclx_color_profile_alloc):=GetProcAddress(hlib,'heif_nclx_color_profile_alloc');
      pointer(heif_nclx_color_profile_free):=GetProcAddress(hlib,'heif_nclx_color_profile_free');
      pointer(heif_image_get_color_profile_type):=GetProcAddress(hlib,'heif_image_get_color_profile_type');
      pointer(heif_image_get_raw_color_profile_size):=GetProcAddress(hlib,'heif_image_get_raw_color_profile_size');
      pointer(heif_image_get_raw_color_profile):=GetProcAddress(hlib,'heif_image_get_raw_color_profile');
      pointer(heif_image_get_nclx_color_profile):=GetProcAddress(hlib,'heif_image_get_nclx_color_profile');
      pointer(heif_decoding_options_alloc):=GetProcAddress(hlib,'heif_decoding_options_alloc');
      pointer(heif_decoding_options_free):=GetProcAddress(hlib,'heif_decoding_options_free');
      pointer(heif_decode_image):=GetProcAddress(hlib,'heif_decode_image');
      pointer(heif_image_get_colorspace):=GetProcAddress(hlib,'heif_image_get_colorspace');
      pointer(heif_image_get_chroma_format):=GetProcAddress(hlib,'heif_image_get_chroma_format');
      pointer(heif_image_get_width):=GetProcAddress(hlib,'heif_image_get_width');
      pointer(heif_image_get_height):=GetProcAddress(hlib,'heif_image_get_height');
      pointer(heif_image_get_primary_width):=GetProcAddress(hlib,'heif_image_get_primary_width');
      pointer(heif_image_get_primary_height):=GetProcAddress(hlib,'heif_image_get_primary_height');
      pointer(heif_image_crop):=GetProcAddress(hlib,'heif_image_crop');
      pointer(heif_image_get_bits_per_pixel):=GetProcAddress(hlib,'heif_image_get_bits_per_pixel');
      pointer(heif_image_get_bits_per_pixel_range):=GetProcAddress(hlib,'heif_image_get_bits_per_pixel_range');
      pointer(heif_image_has_channel):=GetProcAddress(hlib,'heif_image_has_channel');
      pointer(heif_image_get_plane_readonly):=GetProcAddress(hlib,'heif_image_get_plane_readonly');
      pointer(heif_image_get_plane):=GetProcAddress(hlib,'heif_image_get_plane');
      pointer(heif_image_scale_image):=GetProcAddress(hlib,'heif_image_scale_image');
      pointer(heif_image_set_raw_color_profile):=GetProcAddress(hlib,'heif_image_set_raw_color_profile');
      pointer(heif_image_set_nclx_color_profile):=GetProcAddress(hlib,'heif_image_set_nclx_color_profile');
      pointer(heif_image_release):=GetProcAddress(hlib,'heif_image_release');
      pointer(heif_context_write_to_file):=GetProcAddress(hlib,'heif_context_write_to_file');
      pointer(heif_context_write):=GetProcAddress(hlib,'heif_context_write');
      pointer(heif_context_get_encoder_descriptors):=GetProcAddress(hlib,'heif_context_get_encoder_descriptors');
      pointer(heif_encoder_descriptor_get_name):=GetProcAddress(hlib,'heif_encoder_descriptor_get_name');
      pointer(heif_encoder_descriptor_get_id_name):=GetProcAddress(hlib,'heif_encoder_descriptor_get_id_name');
      pointer(heif_encoder_descriptor_get_compression_format):=GetProcAddress(hlib,'heif_encoder_descriptor_get_compression_format');
      pointer(heif_encoder_descriptor_supports_lossy_compression):=GetProcAddress(hlib,'heif_encoder_descriptor_supports_lossy_compression');
      pointer(heif_encoder_descriptor_supports_lossless_compression):=GetProcAddress(hlib,'heif_encoder_descriptor_supports_lossless_compression');
      pointer(heif_context_get_encoder):=GetProcAddress(hlib,'heif_context_get_encoder');
      pointer(heif_have_decoder_for_format):=GetProcAddress(hlib,'heif_have_decoder_for_format');
      pointer(heif_have_encoder_for_format):=GetProcAddress(hlib,'heif_have_encoder_for_format');
      pointer(heif_context_get_encoder_for_format):=GetProcAddress(hlib,'heif_context_get_encoder_for_format');
      pointer(heif_encoder_release):=GetProcAddress(hlib,'heif_encoder_release');
      pointer(heif_encoder_get_name):=GetProcAddress(hlib,'heif_encoder_get_name');
      pointer(heif_encoder_set_lossy_quality):=GetProcAddress(hlib,'heif_encoder_set_lossy_quality');
      pointer(heif_encoder_set_lossless):=GetProcAddress(hlib,'heif_encoder_set_lossless');
      pointer(heif_encoder_set_logging_level):=GetProcAddress(hlib,'heif_encoder_set_logging_level');
      pointer(heif_encoder_list_parameters):=GetProcAddress(hlib,'heif_encoder_list_parameters');
      pointer(heif_encoder_parameter_get_name):=GetProcAddress(hlib,'heif_encoder_parameter_get_name');
      pointer(heif_encoder_parameter_get_type):=GetProcAddress(hlib,'heif_encoder_parameter_get_type');
      pointer(heif_encoder_parameter_get_valid_integer_range):=GetProcAddress(hlib,'heif_encoder_parameter_get_valid_integer_range');
      pointer(heif_encoder_parameter_get_valid_integer_values):=GetProcAddress(hlib,'heif_encoder_parameter_get_valid_integer_values');
      pointer(heif_encoder_parameter_get_valid_string_values):=GetProcAddress(hlib,'heif_encoder_parameter_get_valid_string_values');
      pointer(heif_encoder_set_parameter_integer):=GetProcAddress(hlib,'heif_encoder_set_parameter_integer');
      pointer(heif_encoder_get_parameter_integer):=GetProcAddress(hlib,'heif_encoder_get_parameter_integer');
      pointer(heif_encoder_parameter_integer_valid_range):=GetProcAddress(hlib,'heif_encoder_parameter_integer_valid_range');
      pointer(heif_encoder_set_parameter_boolean):=GetProcAddress(hlib,'heif_encoder_set_parameter_boolean');
      pointer(heif_encoder_get_parameter_boolean):=GetProcAddress(hlib,'heif_encoder_get_parameter_boolean');
      pointer(heif_encoder_set_parameter_string):=GetProcAddress(hlib,'heif_encoder_set_parameter_string');
      pointer(heif_encoder_get_parameter_string):=GetProcAddress(hlib,'heif_encoder_get_parameter_string');
      pointer(heif_encoder_parameter_string_valid_values):=GetProcAddress(hlib,'heif_encoder_parameter_string_valid_values');
      pointer(heif_encoder_parameter_integer_valid_values):=GetProcAddress(hlib,'heif_encoder_parameter_integer_valid_values');
      pointer(heif_encoder_set_parameter):=GetProcAddress(hlib,'heif_encoder_set_parameter');
      pointer(heif_encoder_get_parameter):=GetProcAddress(hlib,'heif_encoder_get_parameter');
      pointer(heif_encoder_has_default):=GetProcAddress(hlib,'heif_encoder_has_default');
      pointer(heif_encoding_options_alloc):=GetProcAddress(hlib,'heif_encoding_options_alloc');
      pointer(heif_encoding_options_free):=GetProcAddress(hlib,'heif_encoding_options_free');
      pointer(heif_context_encode_image):=GetProcAddress(hlib,'heif_context_encode_image');
      pointer(heif_context_set_primary_image):=GetProcAddress(hlib,'heif_context_set_primary_image');
      pointer(heif_context_encode_thumbnail):=GetProcAddress(hlib,'heif_context_encode_thumbnail');
      pointer(heif_context_assign_thumbnail):=GetProcAddress(hlib,'heif_context_assign_thumbnail');
      pointer(heif_context_add_exif_metadata):=GetProcAddress(hlib,'heif_context_add_exif_metadata');
      pointer(heif_context_add_XMP_metadata):=GetProcAddress(hlib,'heif_context_add_XMP_metadata');
      pointer(heif_context_add_generic_metadata):=GetProcAddress(hlib,'heif_context_add_generic_metadata');
      pointer(heif_image_create):=GetProcAddress(hlib,'heif_image_create');
      pointer(heif_image_add_plane):=GetProcAddress(hlib,'heif_image_add_plane');
      pointer(heif_image_set_premultiplied_alpha):=GetProcAddress(hlib,'heif_image_set_premultiplied_alpha');
      pointer(heif_image_is_premultiplied_alpha):=GetProcAddress(hlib,'heif_image_is_premultiplied_alpha');
      pointer(heif_register_decoder):=GetProcAddress(hlib,'heif_register_decoder');
      pointer(heif_register_decoder_plugin):=GetProcAddress(hlib,'heif_register_decoder_plugin');
      pointer(heif_register_encoder_plugin):=GetProcAddress(hlib,'heif_register_encoder_plugin');
      pointer(heif_encoder_descriptor_supportes_lossy_compression):=GetProcAddress(hlib,'heif_encoder_descriptor_supportes_lossy_compression');
      pointer(heif_encoder_descriptor_supportes_lossless_compression):=GetProcAddress(hlib,'heif_encoder_descriptor_supportes_lossless_compression');
    end;


initialization
  Loadlibheif(libheif_library);
finalization
  Freelibheif;

end.
