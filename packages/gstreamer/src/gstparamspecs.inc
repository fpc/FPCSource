{
/* GStreamer - GParamSpecs for some of our types
 * Copyright (C) 2007 Tim-Philipp Müller  <tim centricular net>
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
    P_GstParamSpecArray  = ^T_GstParamSpecArray;
    P_GstParamSpecFraction  = ^T_GstParamSpecFraction;




    T_GstParamSpecFraction = record
        parent_instance : TGParamSpec;
        min_num : Tgint;
        min_den : Tgint;
        max_num : Tgint;
        max_den : Tgint;
        def_num : Tgint;
        def_den : Tgint;
      end;
    TGstParamSpecFraction = T_GstParamSpecFraction;

    T_GstParamSpecArray = record
        parent_instance : TGParamSpec;
        element_spec : PGParamSpec;
      end;
    TGstParamSpecArray = T_GstParamSpecArray;

function gst_param_spec_fraction_get_type:TGType;cdecl;external gstreamerlib name 'gst_param_spec_fraction_get_type';

function gst_param_spec_array_get_type:TGType;cdecl;external gstreamerlib name 'gst_param_spec_array_get_type';

  function gst_param_spec_fraction(name:Pgchar; nick:Pgchar; blurb:Pgchar; min_num:Tgint; min_denom:Tgint;
             max_num:Tgint; max_denom:Tgint; default_num:Tgint; default_denom:Tgint; flags:TGParamFlags):PGParamSpec;cdecl;external gstreamerlib name 'gst_param_spec_fraction';

(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)
  function gst_param_spec_array(name:Pgchar; nick:Pgchar; blurb:Pgchar; element_spec:PGParamSpec; flags:TGParamFlags):PGParamSpec;cdecl;external gstreamerlib name 'gst_param_spec_array';


