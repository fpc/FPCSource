{
/* GStreamer
 * Copyright (C) 2003 Benjamin Otte <in7y118@public.uni-hamburg.de>
 *
 * gsttypefind.h: typefinding subsystem
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
    P_GstTypeFind  = ^T_GstTypeFind;
    PGstTypeFind  = ^TGstTypeFind;
    PGstTypeFindProbability  = ^TGstTypeFindProbability;


    TGstTypeFindFunction = procedure (find:PGstTypeFind; user_data:Tgpointer);cdecl;

    TGstTypeFindProbability = (
      GST_TYPE_FIND_NONE := 0,
      GST_TYPE_FIND_MINIMUM := 1,
      GST_TYPE_FIND_POSSIBLE := 50,
      GST_TYPE_FIND_LIKELY := 80,
      GST_TYPE_FIND_NEARLY_CERTAIN := 99,
      GST_TYPE_FIND_MAXIMUM := 100
    );

    T_GstTypeFind = record
        peek : function (data:Tgpointer; offset:Tgint64; size:Tguint):Pguint8;cdecl;
        suggest : procedure (data:Tgpointer; probability:Tguint; caps:PGstCaps);cdecl;
        data : Tgpointer;
        get_length : function (data:Tgpointer):Tguint64;cdecl;
        _gst_reserved : array[0..3] of Tgpointer;
      end;
    TGstTypeFind = T_GstTypeFind ;


  function gst_type_find_get_type:TGType;cdecl;external gstreamerlib name 'gst_type_find_get_type';

(* Const before type ignored *)
  function gst_type_find_peek(find:PGstTypeFind; offset:Tgint64; size:Tguint):Pguint8;cdecl;external gstreamerlib name 'gst_type_find_peek';

  procedure gst_type_find_suggest(find:PGstTypeFind; probability:Tguint; caps:PGstCaps);cdecl;external gstreamerlib name 'gst_type_find_suggest';

(* Const before type ignored *)
(* Const before type ignored *)
  procedure gst_type_find_suggest_simple(find:PGstTypeFind; probability:Tguint; media_type:Pchar; fieldname:Pchar; args:array of const);cdecl;external gstreamerlib name 'gst_type_find_suggest_simple';

  procedure gst_type_find_suggest_simple(find:PGstTypeFind; probability:Tguint; media_type:Pchar; fieldname:Pchar);cdecl;external gstreamerlib name 'gst_type_find_suggest_simple';

  function gst_type_find_get_length(find:PGstTypeFind):Tguint64;cdecl;external gstreamerlib name 'gst_type_find_get_length';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_type_find_register(plugin:PGstPlugin; name:Pgchar; rank:Tguint; func:TGstTypeFindFunction; extensions:Pgchar; 
             possible_caps:PGstCaps; data:Tgpointer; data_notify:TGDestroyNotify):Tgboolean;cdecl;external gstreamerlib name 'gst_type_find_register';


