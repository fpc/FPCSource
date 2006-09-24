{
  Translation of the libmatroska headers for FreePascal
  Copyright (C) 2006 by Ivo Steinmann
}

(****************************************************************************
** libmatroska : parse Matroska files, see http://www.matroska.org/
**
** <file/class description>
**
** Copyright (C) 2002-2003 Steve Lhomme.  All rights reserved.
**
** This file is part of libmatroska.
**
** This library is free software; you can redistribute it and/or
** modify it under the terms of the GNU Lesser General Public
** License as published by the Free Software Foundation; either
** version 2.1 of the License, or (at your option) any later version.
** 
** This library is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
** Lesser General Public License for more details.
** 
** You should have received a copy of the GNU Lesser General Public
** License along with this library; if not, write to the Free Software
** Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
**
** See http://www.matroska.org/license/lgpl/ for LGPL licensing information.**
** Contact license@matroska.org if any conditions of this licensing are
** not clear to you.
**
**********************************************************************)

unit matroska;

{$mode objfpc}
{$MINENUMSIZE 4}

interface

uses
  ctypes;

{$IFDEF WINDOWS}
  {$DEFINE DYNLINK}
{$ENDIF}

{$IFDEF DYNLINK}
const
{$IF Defined(WINDOWS)}
  matroskalib = 'libmatroska.dll';
{$ELSEIF Defined(UNIX)}
  matroskalib = 'libmatroska.so';
{$ELSE}
  {$MESSAGE ERROR 'DYNLINK not supported'}
{$IFEND}
{$ELSE}
  {$LINKLIB a52}
{$ENDIF}



(*!
    \file libebml_t.h
    \version \$Id: libebml_t.h 1011 2005-01-05 16:15:25Z robux4 $
    \author Steve Lhomme     <robux4 @ users.sf.net>
    \author Ingo Ralf Blum   <ingoralfblum @ users.sf.net>
    \author Moritz Bunkus <moritz@bunkus.org>

    \brief Misc type definitions for the C API of LIBEBML

    \note These types should be compiler/language independant (just platform dependant)
    \todo recover the sized types (uint16, int32, etc) here too (or maybe here only)
*)

type
  binary = cuint8;

  open_mode = (
    MODE_READ,
    MODE_WRITE,
    MODE_CREATE,
    MODE_SAFE
  );


(*!
    \file libmatroska_t.h
    \version \$Id: libmatroska_t.h,v 1.3 2004/04/14 23:26:17 robux4 Exp $
    \author Steve Lhomme     <robux4 @ users.sf.net>
    \author Ingo Ralf Blum   <ingoralfblum @ users.sf.net>

    \brief Misc type definitions for the C API of libmatroska

    \note These types should be compiler/language independant (just platform dependant)
    \todo recover the sized types (uint16, int32, etc) here too (or maybe here only)
*)

(*!
    \enum track_type
*)
type
  track_type = cuint32;
const
  track_video       = $01; ///< Rectangle-shaped non-transparent pictures aka video
  track_audio       = $02; ///< Anything you can hear
  track_complex     = $03; ///< Audio and video in same track, used by DV

  track_logo        = $10; ///< Overlay-pictures, displayed over video
  track_subtitle    = $11; ///< Text-subtitles. One track contains one language and only one track can be active (player-side configuration)
  track_buttons     = $12; ///< Buttons meta-infos.

  track_control     = $20; ///< Control-codes for menus and other stuff


(*!
    \enum matroska_error_t
    \brief a callback that the library use to inform of errors happening
    \note this should be used by the libmatroska internals
*)
type
  matroska_error_t = (
    error_null_pointer  ///< NULL pointer where something else is expected
  );

  matroska_stream = pointer;

(*!
    \var void* matroska_id
    \brief UID used to access an Matroska file instance
*)
  matroska_id = pointer;
(*!
    \var void* matroska_track
    \brief UID used to access a track
*)
  matroska_track = pointer;
(*!
    \var char* c_string
    \brief C-String, ie a buffer with characters terminated by \0
*)
  c_string = pcchar;
(*!
    \var unsigned int matroska_file_mode
    \brief A bit buffer, each bit representing a value for the file opening
    \todo replace the unsigned int by a sized type (8 or 16 bits)
*)
  matroska_file_mode = pcchar;
(*!
    \var void ( *matroska_error_callback)(matroska_error_t error_code, char* error_message)
    \brief a callback that the library use to inform of errors happening
*)
  matroska_error_callback = procedure(error_code: matroska_error_t; error_message: pcchar); cdecl;

  track_info = pointer;



(*!
    \file libmatroska.h
    \version \$Id: libmatroska.h,v 1.2 2004/04/14 23:26:17 robux4 Exp $
    \author Steve Lhomme     <robux4 @ users.sf.net>
    \author Ingo Ralf Blum   <ingoralfblum @ users.sf.net>

    \brief C API to the libmatroska library
    \note These are the functions that should be exported (visible from outisde the library)
    \todo Put a function here for all the MUST in the Matroska spec
    \todo Put a brief description of each function, and a description of the params and return value
    \todo Change the int values to sized types
*)

(*!
    \fn int matroska_plug_log(matroska_error_callback)
    \brief Attach a callback to be informed when error occurs
    \param callback The callback that will be called when logging occurs \return 0 if successfull
*)
function matroska_plug_log(callback: matroska_error_callback): cint; cdecl; external {$IFDEF DYNLINK}matroskalib{$ENDIF};

(*!
    \fn int matroska_unplug_log(matroska_error_callback)
    \brief Unattach an attached callback to be informed when error occurs
    \param callback The callback that was called when logging occurs \return 0 if successfull
*)
function matroska_unplug_log(callback: matroska_error_callback): cint; cdecl; external {$IFDEF DYNLINK}matroskalib{$ENDIF};

(*!
    \fn matroska_id matroska_open_file(c_string,matroska_file_mode)
    \brief Open an instance of an Matroska file
    \param string The name of the file to open (including OS depedant path) \param mode The mode to open the file (read, write, etc)
    \return NULL if the opening failed or an ID that will be used to access this file from the API
*)
function matroska_open_stream_file(_string: c_string; mode: open_mode): matroska_stream; cdecl; external {$IFDEF DYNLINK}matroskalib{$ENDIF};

function matroska_open_stream(a_stream: matroska_stream): matroska_id; cdecl; external {$IFDEF DYNLINK}matroskalib{$ENDIF};

(*!
    \fn matroska_id matroska_open_url(c_string)
    \brief Open an instance of an Matroska file from a URL
    \param string The name of the URL to open \return NULL if the opening failed or an ID that will be used to access this file from the API
    \warning Open only for reading ?
    \note It requires that Curl is compiled or installed
*)
function matroska_open_url(_string: c_string): matroska_id; cdecl; external {$IFDEF DYNLINK}matroskalib{$ENDIF};

(*!
    \fn int matroska_close(matroska_id)
    \brief Close the specified Matroska instance
    \param id The instance to close \return 0 if successfull
*)
procedure matroska_close(id: matroska_id); cdecl; external {$IFDEF DYNLINK}matroskalib{$ENDIF};

procedure matroska_end(id: matroska_id; totaltime: cuint32); cdecl; external {$IFDEF DYNLINK}matroskalib{$ENDIF};

function matroska_create_track(id: matroska_id; _type: track_type): matroska_track; cdecl; external {$IFDEF DYNLINK}matroskalib{$ENDIF};

procedure matroska_read_head(id: matroska_id); cdecl; external {$IFDEF DYNLINK}matroskalib{$ENDIF};
procedure matroska_read_tracks(id: matroska_id); cdecl; external {$IFDEF DYNLINK}matroskalib{$ENDIF};

function matroska_get_number_track(id: matroska_id): cuint8; cdecl; external {$IFDEF DYNLINK}matroskalib{$ENDIF};

function matroska_get_track(id: matroska_id; track_index: cuint8): matroska_track; cdecl; external {$IFDEF DYNLINK}matroskalib{$ENDIF};

procedure matroska_get_track_info(id: matroska_id; track: matroska_track; var infos: track_info); cdecl; external {$IFDEF DYNLINK}matroskalib{$ENDIF};

(*
int matroska_track_write_block(matroska_track, void* buffer, unsigned int size);
int matroska_track_close(matroska_track);
*)

implementation

end.