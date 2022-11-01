{
/* GStreamer
 * Copyright (C) 2010, 2012 Alexander Saprykin <xelfium@gmail.com>
 *
 * gsttocsetter.h: Interfaces for TOC
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
    P_GstTocSetterInterface  = ^T_GstTocSetterInterface;
    PGstTocSetter  = ^TGstTocSetter;

    TGstTocSetter = record end;
    T_GstTocSetter = TGstTocSetter;


    T_GstTocSetterInterface = record
        g_iface : TGTypeInterface;
      end;
    TGstTocSetterInterface = T_GstTocSetterInterface;


  function gst_toc_setter_get_type:TGType;cdecl;external gstreamerlib name 'gst_toc_setter_get_type';

  procedure gst_toc_setter_reset(setter:PGstTocSetter);cdecl;external gstreamerlib name 'gst_toc_setter_reset';

  function gst_toc_setter_get_toc(setter:PGstTocSetter):PGstToc;cdecl;external gstreamerlib name 'gst_toc_setter_get_toc';

  procedure gst_toc_setter_set_toc(setter:PGstTocSetter; toc:PGstToc);cdecl;external gstreamerlib name 'gst_toc_setter_set_toc';


