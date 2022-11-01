{
/* GStreamer
 * Copyright (C) 2003 Benjamin Otte <in7y118@public.uni-hamburg.de>
 *
 * gsttaglist.h: Header for tag support
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 */

}
const
  GST_TAG_TITLE = 'title';
  GST_TAG_TITLE_SORTNAME = 'title-sortname';
  GST_TAG_ARTIST = 'artist';
  GST_TAG_ARTIST_SORTNAME = 'artist-sortname';
  GST_TAG_ALBUM = 'album';
  GST_TAG_ALBUM_SORTNAME = 'album-sortname';
  GST_TAG_ALBUM_ARTIST = 'album-artist';
  GST_TAG_ALBUM_ARTIST_SORTNAME = 'album-artist-sortname';
  GST_TAG_COMPOSER = 'composer';
  GST_TAG_CONDUCTOR = 'conductor';
  GST_TAG_DATE = 'date';
  GST_TAG_DATE_TIME = 'datetime';
  GST_TAG_GENRE = 'genre';
  GST_TAG_COMMENT = 'comment';
  GST_TAG_EXTENDED_COMMENT = 'extended-comment';
  GST_TAG_TRACK_NUMBER = 'track-number';
  GST_TAG_TRACK_COUNT = 'track-count';
  GST_TAG_ALBUM_VOLUME_NUMBER = 'album-disc-number';
  GST_TAG_ALBUM_VOLUME_COUNT = 'album-disc-count';
  GST_TAG_LOCATION = 'location';
  GST_TAG_HOMEPAGE = 'homepage';
  GST_TAG_DESCRIPTION = 'description';
  GST_TAG_VERSION = 'version';
  GST_TAG_ISRC = 'isrc';
  GST_TAG_ORGANIZATION = 'organization';
  GST_TAG_COPYRIGHT = 'copyright';
  GST_TAG_COPYRIGHT_URI = 'copyright-uri';
  GST_TAG_ENCODED_BY = 'encoded-by';
  GST_TAG_CONTACT = 'contact';
  GST_TAG_LICENSE = 'license';
  GST_TAG_LICENSE_URI = 'license-uri';
  GST_TAG_PERFORMER = 'performer';
  GST_TAG_DURATION = 'duration';
  GST_TAG_CODEC = 'codec';
  GST_TAG_VIDEO_CODEC = 'video-codec';
  GST_TAG_AUDIO_CODEC = 'audio-codec';
  GST_TAG_SUBTITLE_CODEC = 'subtitle-codec';
  GST_TAG_CONTAINER_FORMAT = 'container-format';
  GST_TAG_BITRATE = 'bitrate';
  GST_TAG_NOMINAL_BITRATE = 'nominal-bitrate';
  GST_TAG_MINIMUM_BITRATE = 'minimum-bitrate';
  GST_TAG_MAXIMUM_BITRATE = 'maximum-bitrate';
  GST_TAG_SERIAL = 'serial';
  GST_TAG_ENCODER = 'encoder';
  GST_TAG_ENCODER_VERSION = 'encoder-version';
  GST_TAG_TRACK_GAIN = 'replaygain-track-gain';
  GST_TAG_TRACK_PEAK = 'replaygain-track-peak';
  GST_TAG_ALBUM_GAIN = 'replaygain-album-gain';
  GST_TAG_ALBUM_PEAK = 'replaygain-album-peak';
  GST_TAG_REFERENCE_LEVEL = 'replaygain-reference-level';
  GST_TAG_LANGUAGE_CODE = 'language-code';
  GST_TAG_LANGUAGE_NAME = 'language-name';
  GST_TAG_IMAGE = 'image';
  GST_TAG_PREVIEW_IMAGE = 'preview-image';
  GST_TAG_ATTACHMENT = 'attachment';
  GST_TAG_BEATS_PER_MINUTE = 'beats-per-minute';
  GST_TAG_KEYWORDS = 'keywords';
  GST_TAG_GEO_LOCATION_NAME = 'geo-location-name';
  GST_TAG_GEO_LOCATION_LATITUDE = 'geo-location-latitude';
  GST_TAG_GEO_LOCATION_LONGITUDE = 'geo-location-longitude';
  GST_TAG_GEO_LOCATION_ELEVATION = 'geo-location-elevation';
  GST_TAG_GEO_LOCATION_COUNTRY = 'geo-location-country';
  GST_TAG_GEO_LOCATION_CITY = 'geo-location-city';
  GST_TAG_GEO_LOCATION_SUBLOCATION = 'geo-location-sublocation';
  GST_TAG_GEO_LOCATION_HORIZONTAL_ERROR = 'geo-location-horizontal-error';
  GST_TAG_GEO_LOCATION_MOVEMENT_SPEED = 'geo-location-movement-speed';
  GST_TAG_GEO_LOCATION_MOVEMENT_DIRECTION = 'geo-location-movement-direction';
  GST_TAG_GEO_LOCATION_CAPTURE_DIRECTION = 'geo-location-capture-direction';
  GST_TAG_SHOW_NAME = 'show-name';
  GST_TAG_SHOW_SORTNAME = 'show-sortname';
  GST_TAG_SHOW_EPISODE_NUMBER = 'show-episode-number';
  GST_TAG_SHOW_SEASON_NUMBER = 'show-season-number';
  GST_TAG_LYRICS = 'lyrics';
  GST_TAG_COMPOSER_SORTNAME = 'composer-sortname';
  GST_TAG_GROUPING = 'grouping';
  GST_TAG_USER_RATING = 'user-rating';
  GST_TAG_DEVICE_MANUFACTURER = 'device-manufacturer';
  GST_TAG_DEVICE_MODEL = 'device-model';
  GST_TAG_APPLICATION_NAME = 'application-name';
  GST_TAG_APPLICATION_DATA = 'application-data';
  GST_TAG_IMAGE_ORIENTATION = 'image-orientation';
  GST_TAG_PUBLISHER = 'publisher';
  GST_TAG_INTERPRETED_BY = 'interpreted-by';
  GST_TAG_MIDI_BASE_NOTE = 'midi-base-note';
  GST_TAG_PRIVATE_DATA = 'private-data';

  Type
    P_GstTagList  = ^T_GstTagList;
    PGstTagFlag  = ^TGstTagFlag;
    PGstTagList  = ^TGstTagList;
    PPGstTagList  = ^PGstTagList;
    PGstTagList_autoptr  = ^TGstTagList_autoptr;
    PGstTagList_listautoptr  = ^TGstTagList_listautoptr;
    PGstTagList_queueautoptr  = ^TGstTagList_queueautoptr;
    PGstTagList_slistautoptr  = ^TGstTagList_slistautoptr;
    PGstTagMergeMode  = ^TGstTagMergeMode;
    PGstTagScope  = ^TGstTagScope;

    TGstTagMergeMode = (GST_TAG_MERGE_UNDEFINED,
      GST_TAG_MERGE_REPLACE_ALL,
      GST_TAG_MERGE_REPLACE,GST_TAG_MERGE_APPEND,
      GST_TAG_MERGE_PREPEND,GST_TAG_MERGE_KEEP,
      GST_TAG_MERGE_KEEP_ALL,GST_TAG_MERGE_COUNT
      );

    TGstTagFlag = (
      GST_TAG_FLAG_UNDEFINED,
      GST_TAG_FLAG_META,
      GST_TAG_FLAG_ENCODED,
      GST_TAG_FLAG_DECODED,
      GST_TAG_FLAG_COUNT
    );

    T_GstTagList = record
        mini_object : TGstMiniObject;
    end;
    TGstTagList = T_GstTagList;


  TGstTagList_autoptr = PGstTagList;

  TGstTagList_listautoptr = PGList;

  TGstTagList_slistautoptr = PGSList;

  TGstTagList_queueautoptr = PGQueue;


    TGstTagForeachFunc = procedure (list:PGstTagList; tag:Pgchar; user_data:Tgpointer);cdecl;
(* Const before type ignored *)

    TGstTagMergeFunc = procedure (dest:PGValue; src:PGValue);cdecl;

  TGstTagScope = (GST_TAG_SCOPE_STREAM,GST_TAG_SCOPE_GLOBAL);

  function gst_tag_list_get_type:TGType;cdecl;external gstreamerlib name 'gst_tag_list_get_type';

(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)
  procedure gst_tag_register(name:Pgchar; flag:TGstTagFlag; _type:TGType; nick:Pgchar; blurb:Pgchar; 
              func:TGstTagMergeFunc);cdecl;external gstreamerlib name 'gst_tag_register';

(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)
  procedure gst_tag_register_static(name:Pgchar; flag:TGstTagFlag; _type:TGType; nick:Pgchar; blurb:Pgchar; 
              func:TGstTagMergeFunc);cdecl;external gstreamerlib name 'gst_tag_register_static';

(* Const before type ignored *)
  procedure gst_tag_merge_use_first(dest:PGValue; src:PGValue);cdecl;external gstreamerlib name 'gst_tag_merge_use_first';

(* Const before type ignored *)
  procedure gst_tag_merge_strings_with_comma(dest:PGValue; src:PGValue);cdecl;external gstreamerlib name 'gst_tag_merge_strings_with_comma';

(* Const before type ignored *)
  function gst_tag_exists(tag:Pgchar):Tgboolean;cdecl;external gstreamerlib name 'gst_tag_exists';

(* Const before type ignored *)
  function gst_tag_get_type(tag:Pgchar):TGType;cdecl;external gstreamerlib name 'gst_tag_get_type';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_tag_get_nick(tag:Pgchar):Pgchar;cdecl;external gstreamerlib name 'gst_tag_get_nick';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_tag_get_description(tag:Pgchar):Pgchar;cdecl;external gstreamerlib name 'gst_tag_get_description';

(* Const before type ignored *)
  function gst_tag_get_flag(tag:Pgchar):TGstTagFlag;cdecl;external gstreamerlib name 'gst_tag_get_flag';

(* Const before type ignored *)
  function gst_tag_is_fixed(tag:Pgchar):Tgboolean;cdecl;external gstreamerlib name 'gst_tag_is_fixed';



  function gst_tag_list_new_empty:PGstTagList;cdecl;external gstreamerlib name 'gst_tag_list_new_empty';

(* Const before type ignored *)
  function gst_tag_list_new(tag:Pgchar; args:array of const):PGstTagList;cdecl;external gstreamerlib name 'gst_tag_list_new';

  function gst_tag_list_new(tag:Pgchar):PGstTagList;cdecl;external gstreamerlib name 'gst_tag_list_new';

  function gst_tag_list_new_valist(var_args:Tva_list):PGstTagList;cdecl;external gstreamerlib name 'gst_tag_list_new_valist';

  procedure gst_tag_list_set_scope(list:PGstTagList; scope:TGstTagScope);cdecl;external gstreamerlib name 'gst_tag_list_set_scope';

(* Const before type ignored *)
  function gst_tag_list_get_scope(list:PGstTagList):TGstTagScope;cdecl;external gstreamerlib name 'gst_tag_list_get_scope';

(* Const before type ignored *)
  function gst_tag_list_to_string(list:PGstTagList):Pgchar;cdecl;external gstreamerlib name 'gst_tag_list_to_string';

(* Const before type ignored *)
  function gst_tag_list_new_from_string(str:Pgchar):PGstTagList;cdecl;external gstreamerlib name 'gst_tag_list_new_from_string';

(* Const before type ignored *)
  function gst_tag_list_n_tags(list:PGstTagList):Tgint;cdecl;external gstreamerlib name 'gst_tag_list_n_tags';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_tag_list_nth_tag_name(list:PGstTagList; index:Tguint):Pgchar;cdecl;external gstreamerlib name 'gst_tag_list_nth_tag_name';

(* Const before type ignored *)
  function gst_tag_list_is_empty(list:PGstTagList):Tgboolean;cdecl;external gstreamerlib name 'gst_tag_list_is_empty';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_tag_list_is_equal(list1:PGstTagList; list2:PGstTagList):Tgboolean;cdecl;external gstreamerlib name 'gst_tag_list_is_equal';

(* Const before type ignored *)
  procedure gst_tag_list_insert(into:PGstTagList; from:PGstTagList; mode:TGstTagMergeMode);cdecl;external gstreamerlib name 'gst_tag_list_insert';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_tag_list_merge(list1:PGstTagList; list2:PGstTagList; mode:TGstTagMergeMode):PGstTagList;cdecl;external gstreamerlib name 'gst_tag_list_merge';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_tag_list_get_tag_size(list:PGstTagList; tag:Pgchar):Tguint;cdecl;external gstreamerlib name 'gst_tag_list_get_tag_size';

(* Const before type ignored *)
  procedure gst_tag_list_add(list:PGstTagList; mode:TGstTagMergeMode; tag:Pgchar; args:array of const);cdecl;external gstreamerlib name 'gst_tag_list_add';

  procedure gst_tag_list_add(list:PGstTagList; mode:TGstTagMergeMode; tag:Pgchar);cdecl;external gstreamerlib name 'gst_tag_list_add';

(* Const before type ignored *)
  procedure gst_tag_list_add_values(list:PGstTagList; mode:TGstTagMergeMode; tag:Pgchar; args:array of const);cdecl;external gstreamerlib name 'gst_tag_list_add_values';

  procedure gst_tag_list_add_values(list:PGstTagList; mode:TGstTagMergeMode; tag:Pgchar);cdecl;external gstreamerlib name 'gst_tag_list_add_values';

(* Const before type ignored *)
  procedure gst_tag_list_add_valist(list:PGstTagList; mode:TGstTagMergeMode; tag:Pgchar; var_args:Tva_list);cdecl;external gstreamerlib name 'gst_tag_list_add_valist';

(* Const before type ignored *)
  procedure gst_tag_list_add_valist_values(list:PGstTagList; mode:TGstTagMergeMode; tag:Pgchar; var_args:Tva_list);cdecl;external gstreamerlib name 'gst_tag_list_add_valist_values';

(* Const before type ignored *)
(* Const before type ignored *)
  procedure gst_tag_list_add_value(list:PGstTagList; mode:TGstTagMergeMode; tag:Pgchar; value:PGValue);cdecl;external gstreamerlib name 'gst_tag_list_add_value';

(* Const before type ignored *)
  procedure gst_tag_list_remove_tag(list:PGstTagList; tag:Pgchar);cdecl;external gstreamerlib name 'gst_tag_list_remove_tag';

(* Const before type ignored *)
  procedure gst_tag_list_foreach(list:PGstTagList; func:TGstTagForeachFunc; user_data:Tgpointer);cdecl;external gstreamerlib name 'gst_tag_list_foreach';

(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)
  function gst_tag_list_get_value_index(list:PGstTagList; tag:Pgchar; index:Tguint):PGValue;cdecl;external gstreamerlib name 'gst_tag_list_get_value_index';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_tag_list_copy_value(dest:PGValue; list:PGstTagList; tag:Pgchar):Tgboolean;cdecl;external gstreamerlib name 'gst_tag_list_copy_value';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_tag_list_get_boolean(list:PGstTagList; tag:Pgchar; value:Pgboolean):Tgboolean;cdecl;external gstreamerlib name 'gst_tag_list_get_boolean';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_tag_list_get_boolean_index(list:PGstTagList; tag:Pgchar; index:Tguint; value:Pgboolean):Tgboolean;cdecl;external gstreamerlib name 'gst_tag_list_get_boolean_index';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_tag_list_get_int(list:PGstTagList; tag:Pgchar; value:Pgint):Tgboolean;cdecl;external gstreamerlib name 'gst_tag_list_get_int';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_tag_list_get_int_index(list:PGstTagList; tag:Pgchar; index:Tguint; value:Pgint):Tgboolean;cdecl;external gstreamerlib name 'gst_tag_list_get_int_index';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_tag_list_get_uint(list:PGstTagList; tag:Pgchar; value:Pguint):Tgboolean;cdecl;external gstreamerlib name 'gst_tag_list_get_uint';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_tag_list_get_uint_index(list:PGstTagList; tag:Pgchar; index:Tguint; value:Pguint):Tgboolean;cdecl;external gstreamerlib name 'gst_tag_list_get_uint_index';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_tag_list_get_int64(list:PGstTagList; tag:Pgchar; value:Pgint64):Tgboolean;cdecl;external gstreamerlib name 'gst_tag_list_get_int64';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_tag_list_get_int64_index(list:PGstTagList; tag:Pgchar; index:Tguint; value:Pgint64):Tgboolean;cdecl;external gstreamerlib name 'gst_tag_list_get_int64_index';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_tag_list_get_uint64(list:PGstTagList; tag:Pgchar; value:Pguint64):Tgboolean;cdecl;external gstreamerlib name 'gst_tag_list_get_uint64';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_tag_list_get_uint64_index(list:PGstTagList; tag:Pgchar; index:Tguint; value:Pguint64):Tgboolean;cdecl;external gstreamerlib name 'gst_tag_list_get_uint64_index';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_tag_list_get_float(list:PGstTagList; tag:Pgchar; value:Pgfloat):Tgboolean;cdecl;external gstreamerlib name 'gst_tag_list_get_float';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_tag_list_get_float_index(list:PGstTagList; tag:Pgchar; index:Tguint; value:Pgfloat):Tgboolean;cdecl;external gstreamerlib name 'gst_tag_list_get_float_index';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_tag_list_get_double(list:PGstTagList; tag:Pgchar; value:Pgdouble):Tgboolean;cdecl;external gstreamerlib name 'gst_tag_list_get_double';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_tag_list_get_double_index(list:PGstTagList; tag:Pgchar; index:Tguint; value:Pgdouble):Tgboolean;cdecl;external gstreamerlib name 'gst_tag_list_get_double_index';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_tag_list_get_string(list:PGstTagList; tag:Pgchar; value:PPgchar):Tgboolean;cdecl;external gstreamerlib name 'gst_tag_list_get_string';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_tag_list_get_string_index(list:PGstTagList; tag:Pgchar; index:Tguint; value:PPgchar):Tgboolean;cdecl;external gstreamerlib name 'gst_tag_list_get_string_index';

(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)
  function gst_tag_list_peek_string_index(list:PGstTagList; tag:Pgchar; index:Tguint; value:PPgchar):Tgboolean;cdecl;external gstreamerlib name 'gst_tag_list_peek_string_index';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_tag_list_get_pointer(list:PGstTagList; tag:Pgchar; value:Pgpointer):Tgboolean;cdecl;external gstreamerlib name 'gst_tag_list_get_pointer';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_tag_list_get_pointer_index(list:PGstTagList; tag:Pgchar; index:Tguint; value:Pgpointer):Tgboolean;cdecl;external gstreamerlib name 'gst_tag_list_get_pointer_index';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_tag_list_get_date(list:PGstTagList; tag:Pgchar; value:PPGDate):Tgboolean;cdecl;external gstreamerlib name 'gst_tag_list_get_date';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_tag_list_get_date_index(list:PGstTagList; tag:Pgchar; index:Tguint; value:PPGDate):Tgboolean;cdecl;external gstreamerlib name 'gst_tag_list_get_date_index';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_tag_list_get_date_time(list:PGstTagList; tag:Pgchar; value:PPGstDateTime):Tgboolean;cdecl;external gstreamerlib name 'gst_tag_list_get_date_time';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_tag_list_get_date_time_index(list:PGstTagList; tag:Pgchar; index:Tguint; value:PPGstDateTime):Tgboolean;cdecl;external gstreamerlib name 'gst_tag_list_get_date_time_index';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_tag_list_get_sample(list:PGstTagList; tag:Pgchar; sample:PPGstSample):Tgboolean;cdecl;external gstreamerlib name 'gst_tag_list_get_sample';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_tag_list_get_sample_index(list:PGstTagList; tag:Pgchar; index:Tguint; sample:PPGstSample):Tgboolean;cdecl;external gstreamerlib name 'gst_tag_list_get_sample_index';

  {
  static inline GstTagList *
  gst_tag_list_ref (GstTagList * taglist)
  
    return (GstTagList *) gst_mini_object_ref (((GstMiniObject*)(taglist)));
  
  
  
  
  
  
  
  
  static inline void
  gst_tag_list_unref (GstTagList * taglist)
  
    gst_mini_object_unref (((GstMiniObject*)(taglist)));
  
  # 497 "gsttaglist.h"
  static inline void
  gst_clear_tag_list (GstTagList ** taglist_ptr)
  
    do  _Static_assert (sizeof *(((GstMiniObject **) taglist_ptr)) == sizeof (gpointer), "Expression evaluates to false"); __typeof__((((GstMiniObject **) taglist_ptr))) _pp = (((GstMiniObject **) taglist_ptr)); __typeof__(*(((GstMiniObject **) taglist_ptr))) _ptr = *_pp; *_pp = 
  # 500 "gsttaglist.h" 3 4
   ((void *)0)
  # 500 "gsttaglist.h"
   ; if (_ptr) (gst_mini_object_unref) (_ptr);  while (0) ;
  
  # 519 "gsttaglist.h"
  static inline GstTagList *
  gst_tag_list_copy (const GstTagList * taglist)
  
    return ((GstTagList *) (gst_mini_object_copy (((GstMiniObject*)(taglist)))));
  
  # 544 "gsttaglist.h"
  static inline gboolean
  gst_tag_list_replace (GstTagList **old_taglist, GstTagList *new_taglist)
  
      return gst_mini_object_replace ((GstMiniObject **) old_taglist,
          (GstMiniObject *) new_taglist);
  
  # 566 "gsttaglist.h"
  static inline gboolean
  gst_tag_list_take (GstTagList **old_taglist, GstTagList *new_taglist)
  
    return gst_mini_object_take ((GstMiniObject **) old_taglist,
        (GstMiniObject *) new_taglist);
  
  
   }

  var
    _gst_tag_list_type : TGType;cvar;external;

