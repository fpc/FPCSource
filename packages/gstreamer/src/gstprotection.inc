{
/* GStreamer
 * Copyright (C) <2015> YouView TV Ltd.
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
    P_GstProtectionMeta  = ^T_GstProtectionMeta;
    PGstProtectionMeta  = ^TGstProtectionMeta;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}



    T_GstProtectionMeta = record
        meta : TGstMeta;
        info : PGstStructure;
      end;
    TGstProtectionMeta  = T_GstProtectionMeta;


  function gst_protection_meta_api_get_type:TGType;cdecl;external gstreamerlib name 'gst_protection_meta_api_get_type';

(* Const before type ignored *)
  function gst_protection_meta_get_info:PGstMetaInfo;cdecl;external gstreamerlib name 'gst_protection_meta_get_info';

  function gst_buffer_add_protection_meta(buffer:PGstBuffer; info:PGstStructure):PGstProtectionMeta;cdecl;external gstreamerlib name 'gst_buffer_add_protection_meta';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_protection_select_system(system_identifiers:PPgchar):Pgchar;cdecl;external gstreamerlib name 'gst_protection_select_system';

(* Const before type ignored *)
  function gst_protection_filter_systems_by_available_decryptors(system_identifiers:PPgchar):PPgchar;cdecl;external gstreamerlib name 'gst_protection_filter_systems_by_available_decryptors';


