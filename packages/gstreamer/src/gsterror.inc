{* GStreamer
 * Copyright (C) 2004 Thomas Vander Stichele <thomas at apestaart dot org>
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
 *}

  Type
    PGstCoreError  = ^TGstCoreError;
    PGstLibraryError  = ^TGstLibraryError;
    PGstResourceError  = ^TGstResourceError;
    PGstStreamError  = ^TGstStreamError;

    TGstCoreError = (
      GST_CORE_ERROR_FAILED := 1,
      GST_CORE_ERROR_TOO_LAZY,
      GST_CORE_ERROR_NOT_IMPLEMENTED,
      GST_CORE_ERROR_STATE_CHANGE,
      GST_CORE_ERROR_PAD,
      GST_CORE_ERROR_THREAD,
      GST_CORE_ERROR_NEGOTIATION,
      GST_CORE_ERROR_EVENT,
      GST_CORE_ERROR_SEEK,
      GST_CORE_ERROR_CAPS,
      GST_CORE_ERROR_TAG,
      GST_CORE_ERROR_MISSING_PLUGIN,
      GST_CORE_ERROR_CLOCK,
      GST_CORE_ERROR_DISABLED,
      GST_CORE_ERROR_NUM_ERRORS);

    TGstLibraryError = (
      GST_LIBRARY_ERROR_FAILED := 1,
      GST_LIBRARY_ERROR_TOO_LAZY,
      GST_LIBRARY_ERROR_INIT,
      GST_LIBRARY_ERROR_SHUTDOWN,
      GST_LIBRARY_ERROR_SETTINGS,
      GST_LIBRARY_ERROR_ENCODE,
      GST_LIBRARY_ERROR_NUM_ERRORS);

    TGstResourceError = (
      GST_RESOURCE_ERROR_FAILED := 1,
      GST_RESOURCE_ERROR_TOO_LAZY,
      GST_RESOURCE_ERROR_NOT_FOUND,
      GST_RESOURCE_ERROR_BUSY,
      GST_RESOURCE_ERROR_OPEN_READ,
      GST_RESOURCE_ERROR_OPEN_WRITE,
      GST_RESOURCE_ERROR_OPEN_READ_WRITE,
      GST_RESOURCE_ERROR_CLOSE,
      GST_RESOURCE_ERROR_READ,
      GST_RESOURCE_ERROR_WRITE,
      GST_RESOURCE_ERROR_SEEK,
      GST_RESOURCE_ERROR_SYNC,
      GST_RESOURCE_ERROR_SETTINGS,
      GST_RESOURCE_ERROR_NO_SPACE_LEFT,
      GST_RESOURCE_ERROR_NOT_AUTHORIZED,
      GST_RESOURCE_ERROR_NUM_ERRORS
    );

    TGstStreamError = (GST_STREAM_ERROR_FAILED := 1,GST_STREAM_ERROR_TOO_LAZY,
      GST_STREAM_ERROR_NOT_IMPLEMENTED,GST_STREAM_ERROR_TYPE_NOT_FOUND,
      GST_STREAM_ERROR_WRONG_TYPE,GST_STREAM_ERROR_CODEC_NOT_FOUND,
      GST_STREAM_ERROR_DECODE,GST_STREAM_ERROR_ENCODE,
      GST_STREAM_ERROR_DEMUX,GST_STREAM_ERROR_MUX,
      GST_STREAM_ERROR_FORMAT,GST_STREAM_ERROR_DECRYPT,
      GST_STREAM_ERROR_DECRYPT_NOKEY,GST_STREAM_ERROR_NUM_ERRORS
      );

  function gst_error_get_message(domain:TGQuark; code:Tgint):Pgchar;cdecl;external gstreamerlib name 'gst_error_get_message';

  function gst_stream_error_quark:TGQuark;cdecl;external gstreamerlib name 'gst_stream_error_quark';

  function gst_core_error_quark:TGQuark;cdecl;external gstreamerlib name 'gst_core_error_quark';

  function gst_resource_error_quark:TGQuark;cdecl;external gstreamerlib name 'gst_resource_error_quark';

  function gst_library_error_quark:TGQuark;cdecl;external gstreamerlib name 'gst_library_error_quark';


