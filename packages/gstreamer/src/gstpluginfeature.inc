{
/* GStreamer
 * Copyright (C) 1999,2000 Erik Walthinsen <omega@cse.ogi.edu>
 *                    2000 Wim Taymans <wtay@chello.be>
 *
 * gstpluginfeature.h: Header for base GstPluginFeature
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

  Type
    PGstPluginFeature  = ^TGstPluginFeature;
    PGstPluginFeature_autoptr  = ^TGstPluginFeature_autoptr;
    PGstPluginFeature_listautoptr  = ^TGstPluginFeature_listautoptr;
    PGstPluginFeature_queueautoptr  = ^TGstPluginFeature_queueautoptr;
    PGstPluginFeature_slistautoptr  = ^TGstPluginFeature_slistautoptr;
    PGstRank  = ^TGstRank;


    TGstPluginFeature = record end;
    T_GstPluginFeature = TGstPluginFeature;
    TGstPluginFeatureClass = record end;
    T_GstPluginFeatureClass = TGstPluginFeatureClass;

    TGstRank = (
      GST_RANK_NONE := 0,
      GST_RANK_MARGINAL := 64,
      GST_RANK_SECONDARY := 128,
      GST_RANK_PRIMARY := 256
    );

  TGstPluginFeature_autoptr = PGstPluginFeature;

  TGstPluginFeature_listautoptr = PGList;

  TGstPluginFeature_slistautoptr = PGSList;

  TGstPluginFeature_queueautoptr = PGQueue;

  TGstPluginFeatureFilter = function (feature:PGstPluginFeature; user_data:Tgpointer):Tgboolean;cdecl;

  function gst_plugin_feature_get_type:TGType;cdecl;external gstreamerlib name 'gst_plugin_feature_get_type';

  function gst_plugin_feature_load(feature:PGstPluginFeature):PGstPluginFeature;cdecl;external gstreamerlib name 'gst_plugin_feature_load';

  procedure gst_plugin_feature_set_rank(feature:PGstPluginFeature; rank:Tguint);cdecl;external gstreamerlib name 'gst_plugin_feature_set_rank';

  function gst_plugin_feature_get_rank(feature:PGstPluginFeature):Tguint;cdecl;external gstreamerlib name 'gst_plugin_feature_get_rank';

  function gst_plugin_feature_get_plugin(feature:PGstPluginFeature):PGstPlugin;cdecl;external gstreamerlib name 'gst_plugin_feature_get_plugin';


  function gst_plugin_feature_get_plugin_name(feature:PGstPluginFeature):Pgchar;cdecl;external gstreamerlib name 'gst_plugin_feature_get_plugin_name';

  procedure gst_plugin_feature_list_free(list:PGList);cdecl;external gstreamerlib name 'gst_plugin_feature_list_free';

  function gst_plugin_feature_list_copy(list:PGList):PGList;cdecl;external gstreamerlib name 'gst_plugin_feature_list_copy';

  procedure gst_plugin_feature_list_debug(list:PGList);cdecl;external gstreamerlib name 'gst_plugin_feature_list_debug';

  function gst_plugin_feature_check_version(feature:PGstPluginFeature; min_major:Tguint; min_minor:Tguint; min_micro:Tguint):Tgboolean;cdecl;external gstreamerlib name 'gst_plugin_feature_check_version';

  function gst_plugin_feature_rank_compare_func(p1:Tgconstpointer; p2:Tgconstpointer):Tgint;cdecl;external gstreamerlib name 'gst_plugin_feature_rank_compare_func';


