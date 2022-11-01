{
/* GStreamer
 * Copyright (C) 2015 Jan Schmidt <jan@centricular.com>
 *
 * gstdynamictypefactory.h: Header for GstDynamicTypeFactory
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



  function gst_dynamic_type_factory_get_type:TGType;cdecl;external gstreamerlib name 'gst_dynamic_type_factory_get_type';

(* Const before type ignored *)
  function gst_dynamic_type_factory_load(factoryname:Pgchar):TGType;cdecl;external gstreamerlib name 'gst_dynamic_type_factory_load';

  function gst_dynamic_type_register(plugin:PGstPlugin; _type:TGType):Tgboolean;cdecl;external gstreamerlib name 'gst_dynamic_type_register';


