{  Free Pascal port by Nikolay Nikolov <nickysn@users.sourceforge.net>  }

(**
 * \file include/mixer.h
 * \brief Application interface library for the ALSA driver
 * \author Jaroslav Kysela <perex@perex.cz>
 * \author Abramo Bagnara <abramo@alsa-project.org>
 * \author Takashi Iwai <tiwai@suse.de>
 * \date 1998-2001
 *
 * Application interface library for the ALSA driver
 *)
(*
 *   This library is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU Lesser General Public License as
 *   published by the Free Software Foundation; either version 2.1 of
 *   the License, or (at your option) any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU Lesser General Public License for more details.
 *
 *   You should have received a copy of the GNU Lesser General Public
 *   License along with this library; if not, write to the Free Software
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307 USA
 *
 *)

//#ifndef __ALSA_MIXER_H
//#define __ALSA_MIXER_H

//#ifdef __cplusplus
//extern "C" {
//#endif

(**
 *  \defgroup Mixer Mixer Interface
 *  The mixer interface.
 *  \{
 *)

type
(** Mixer handle *)
  PPsnd_mixer_t = ^Psnd_mixer_t;
  Psnd_mixer_t = ^snd_mixer_t;
  snd_mixer_t = record
  end;
(** Mixer elements class handle *)
  PPsnd_mixer_class_t = ^Psnd_mixer_class_t;
  Psnd_mixer_class_t = ^snd_mixer_class_t;
  snd_mixer_class_t = record
  end;
(** Mixer element handle *)
  PPsnd_mixer_elem_t = ^Psnd_mixer_elem_t;
  Psnd_mixer_elem_t = ^snd_mixer_elem_t;
  snd_mixer_elem_t = record
  end;

(** 
 * \brief Mixer callback function
 * \param mixer Mixer handle
 * \param mask event mask
 * \param elem related mixer element (if any)
 * \return 0 on success otherwise a negative error code
 *)
  snd_mixer_callback_t = function(ctl: Psnd_mixer_t;
				  mask: cuint;
				  elem: Psnd_mixer_elem_t): cint; cdecl;

(** 
 * \brief Mixer element callback function
 * \param elem Mixer element
 * \param mask event mask
 * \return 0 on success otherwise a negative error code
 *)
  snd_mixer_elem_callback_t = function(elem: Psnd_mixer_elem_t;
				       mask: cuint): cint; cdecl;

(**
 * \brief Compare function for sorting mixer elements
 * \param e1 First element
 * \param e2 Second element
 * \return -1 if e1 < e2, 0 if e1 == e2, 1 if e1 > e2
 *)
  snd_mixer_compare_t = function(e1: Psnd_mixer_elem_t;
				 e2: Psnd_mixer_elem_t): cint; cdecl;

(**
 * \brief Event callback for the mixer class
 * \param class_ Mixer class
 * \param mask Event mask (SND_CTL_EVENT_* )
 * \param helem HCTL element which invoked the event
 * \param melem Mixer element associated to HCTL element
 * \return zero if success, otherwise a negative error value
 *)
  snd_mixer_event_t = function(class_: Psnd_mixer_class_t; mask: cuint;
			       helem: Psnd_hctl_elem_t; melem: Psnd_mixer_elem_t): cint; cdecl;


(** Mixer element type *)
  Psnd_mixer_elem_type_t = ^snd_mixer_elem_type_t;
  snd_mixer_elem_type_t = (
	{ Simple mixer elements }
	SND_MIXER_ELEM_SIMPLE,
	SND_MIXER_ELEM_LAST = SND_MIXER_ELEM_SIMPLE);

function snd_mixer_open(mixer: PPsnd_mixer_t; mode: cint): cint; cdecl; external libasound;
function snd_mixer_close(mixer: Psnd_mixer_t): cint; cdecl; external libasound;
function snd_mixer_first_elem(mixer: Psnd_mixer_t): Psnd_mixer_elem_t; cdecl; external libasound;
function snd_mixer_last_elem(mixer: Psnd_mixer_t): Psnd_mixer_elem_t; cdecl; external libasound;
function snd_mixer_handle_events(mixer: Psnd_mixer_t): cint; cdecl; external libasound;
function snd_mixer_attach(mixer: Psnd_mixer_t; name: PChar): cint; cdecl; external libasound;
function snd_mixer_attach_hctl(mixer: Psnd_mixer_t; hctl: Psnd_hctl_t): cint; cdecl; external libasound;
function snd_mixer_detach(mixer: Psnd_mixer_t; name: PChar): cint; cdecl; external libasound;
function snd_mixer_detach_hctl(mixer: Psnd_mixer_t; hctl: Psnd_hctl_t): cint; cdecl; external libasound;
function snd_mixer_get_hctl(mixer: Psnd_mixer_t; name: PChar; hctl: PPsnd_hctl_t): cint; cdecl; external libasound;
function snd_mixer_poll_descriptors_count(mixer: Psnd_mixer_t): cint; cdecl; external libasound;
{$INFO pollfd?!}
function snd_mixer_poll_descriptors(mixer: Psnd_mixer_t; pfds: {struct pollfd*} Pointer; space: cuint): cint; cdecl; external libasound;
{$INFO pollfd?!}
function snd_mixer_poll_descriptors_revents(mixer: Psnd_mixer_t; pfds: {struct pollfd*} Pointer; nfds: cuint; revents: Pcushort): cint; cdecl; external libasound;
function snd_mixer_load(mixer: Psnd_mixer_t): cint; cdecl; external libasound;
procedure snd_mixer_free(mixer: Psnd_mixer_t); cdecl; external libasound;
function snd_mixer_wait(mixer: Psnd_mixer_t; timeout: cint): cint; cdecl; external libasound;
function snd_mixer_set_compare(mixer: Psnd_mixer_t; msort: snd_mixer_compare_t): cint; cdecl; external libasound;
procedure snd_mixer_set_callback(obj: Psnd_mixer_t; val: snd_mixer_callback_t); cdecl; external libasound;
function snd_mixer_get_callback_private(obj: Psnd_mixer_t): Pointer; cdecl; external libasound;
procedure snd_mixer_set_callback_private(obj: Psnd_mixer_t; val: Pointer); cdecl; external libasound;
function snd_mixer_get_count(obj: Psnd_mixer_t): cuint; cdecl; external libasound;
function snd_mixer_class_unregister(clss: Psnd_mixer_class_t): cint; cdecl; external libasound;

function snd_mixer_elem_next(elem: Psnd_mixer_elem_t): Psnd_mixer_elem_t; cdecl; external libasound;
function snd_mixer_elem_prev(elem: Psnd_mixer_elem_t): Psnd_mixer_elem_t; cdecl; external libasound;
procedure snd_mixer_elem_set_callback(obj: Psnd_mixer_elem_t; val: snd_mixer_elem_callback_t); cdecl; external libasound;
function snd_mixer_elem_get_callback_private(obj: Psnd_mixer_elem_t): Pointer; cdecl; external libasound;
procedure snd_mixer_elem_set_callback_private(obj: Psnd_mixer_elem_t; val: Pointer); cdecl; external libasound;
function snd_mixer_elem_get_type(obj: Psnd_mixer_elem_t): snd_mixer_elem_type_t; cdecl; external libasound;

function snd_mixer_class_register(class_: Psnd_mixer_class_t; mixer: Psnd_mixer_t): cint; cdecl; external libasound;
type
  Tsnd_mixer_elem_new_private_free_proc = procedure(elem: Psnd_mixer_elem_t); cdecl; {$INFO anonymous type in the C version}
function snd_mixer_elem_new(elem: PPsnd_mixer_elem_t;
		            type_: snd_mixer_elem_type_t;
		            compare_weight: cint;
		            private_data: Pointer;
		            private_free: Tsnd_mixer_elem_new_private_free_proc): cint; cdecl; external libasound;
function snd_mixer_elem_add(elem: Psnd_mixer_elem_t; class_: Psnd_mixer_class_t): cint; cdecl; external libasound;
function snd_mixer_elem_remove(elem: Psnd_mixer_elem_t): cint; cdecl; external libasound;
procedure snd_mixer_elem_free(elem: Psnd_mixer_elem_t); cdecl; external libasound;
function snd_mixer_elem_info(elem: Psnd_mixer_elem_t): cint; cdecl; external libasound;
function snd_mixer_elem_value(elem: Psnd_mixer_elem_t): cint; cdecl; external libasound;
function snd_mixer_elem_attach(melem: Psnd_mixer_elem_t; helem: Psnd_hctl_elem_t): cint; cdecl; external libasound;
function snd_mixer_elem_detach(melem: Psnd_mixer_elem_t; helem: Psnd_hctl_elem_t): cint; cdecl; external libasound;
function snd_mixer_elem_empty(melem: Psnd_mixer_elem_t): cint; cdecl; external libasound;
function snd_mixer_elem_get_private(melem: Psnd_mixer_elem_t): Pointer; cdecl; external libasound;

function snd_mixer_class_sizeof: size_t; cdecl; external libasound;
(** \hideinitializer
 * \brief allocate an invalid #snd_mixer_class_t using standard alloca
 * \param ptr returned pointer
 *)
//#define snd_mixer_class_alloca(ptr) __snd_alloca(ptr, snd_mixer_class)
function snd_mixer_class_malloc(ptr: PPsnd_mixer_class_t): cint; cdecl; external libasound;
procedure snd_mixer_class_free(obj: Psnd_mixer_class_t); cdecl; external libasound;
procedure snd_mixer_class_copy(dst: Psnd_mixer_class_t; src: Psnd_mixer_class_t); cdecl; external libasound;
function snd_mixer_class_get_mixer(class_: Psnd_mixer_class_t): Psnd_mixer_t; cdecl; external libasound;
function snd_mixer_class_get_event(class_: Psnd_mixer_class_t): snd_mixer_event_t; cdecl; external libasound;
function snd_mixer_class_get_private(class_: Psnd_mixer_class_t): Pointer; cdecl; external libasound;
function snd_mixer_class_get_compare(class_: Psnd_mixer_class_t): snd_mixer_compare_t; cdecl; external libasound;
function snd_mixer_class_set_event(class_: Psnd_mixer_class_t; event: snd_mixer_event_t): cint; cdecl; external libasound;
function snd_mixer_class_set_private(class_: Psnd_mixer_class_t; private_data: Pointer): cint; cdecl; external libasound;
type
  Tsnd_mixer_class_set_private_free_proc = procedure(elem: Psnd_mixer_elem_t); cdecl; {$INFO anonymous type in the C version}
function snd_mixer_class_set_private_free(class_: Psnd_mixer_class_t; private_free: Tsnd_mixer_class_set_private_free_proc): cint; cdecl; external libasound;
function snd_mixer_class_set_compare(class_: Psnd_mixer_class_t; compare: snd_mixer_compare_t): cint; cdecl; external libasound;

(**
 *  \defgroup SimpleMixer Simple Mixer Interface
 *  \ingroup Mixer
 *  The simple mixer interface.
 *  \{
 *)

(* Simple mixer elements API *)

(** Mixer simple element channel identifier *)
type
  snd_mixer_selem_channel_id_t = (
	(** Unknown *)
	SND_MIXER_SCHN_UNKNOWN = -1,
	(** Front left *)
	SND_MIXER_SCHN_FRONT_LEFT = 0,
	(** Front right *)
	SND_MIXER_SCHN_FRONT_RIGHT,
	(** Rear left *)
	SND_MIXER_SCHN_REAR_LEFT,
	(** Rear right *)
	SND_MIXER_SCHN_REAR_RIGHT,
	(** Front center *)
	SND_MIXER_SCHN_FRONT_CENTER,
	(** Woofer *)
	SND_MIXER_SCHN_WOOFER,
	(** Side Left *)
	SND_MIXER_SCHN_SIDE_LEFT,
	(** Side Right *)
	SND_MIXER_SCHN_SIDE_RIGHT,
	(** Rear Center *)
	SND_MIXER_SCHN_REAR_CENTER,
	SND_MIXER_SCHN_LAST = 31,
	(** Mono (Front left alias) *)
	SND_MIXER_SCHN_MONO = SND_MIXER_SCHN_FRONT_LEFT);

(** Mixer simple element - register options - abstraction level *)
  snd_mixer_selem_regopt_abstract = (
	(** no abstraction - try use all universal controls from driver *)
	SND_MIXER_SABSTRACT_NONE = 0,
	(** basic abstraction - Master,PCM,CD,Aux,record-Gain etc. *)
	SND_MIXER_SABSTRACT_BASIC);

(** Mixer simple element - register options *)
  Psnd_mixer_selem_regopt = ^snd_mixer_selem_regopt;
  snd_mixer_selem_regopt = record
	(** structure version *)
	ver: cint;
	(** v1: abstract layer selection *)
	abstract_: snd_mixer_selem_regopt_abstract;
	(** v1: device name (must be NULL when playback_pcm or capture_pcm != NULL) *)
	device: PChar;
	(** v1: playback PCM connected to mixer device (NULL == none) *)
	playback_pcm: Psnd_pcm_t;
	(** v1: capture PCM connected to mixer device (NULL == none) *)
	capture_pcm: Psnd_pcm_t;
  end;

(** Mixer simple element identifier *)
  PPsnd_mixer_selem_id_t = ^Psnd_mixer_selem_id_t;
  Psnd_mixer_selem_id_t = ^snd_mixer_selem_id_t;
  snd_mixer_selem_id_t = record
  end;

function snd_mixer_selem_channel_name(channel: snd_mixer_selem_channel_id_t): PChar; cdecl; external libasound;

function snd_mixer_selem_register(mixer: Psnd_mixer_t;
			          options: Psnd_mixer_selem_regopt;
			          classp: PPsnd_mixer_class_t): cint; cdecl; external libasound;
procedure snd_mixer_selem_get_id(element: Psnd_mixer_elem_t;
			         id: Psnd_mixer_selem_id_t); cdecl; external libasound;
function snd_mixer_selem_get_name(elem: Psnd_mixer_elem_t): PChar; cdecl; external libasound;
function snd_mixer_selem_get_index(elem: Psnd_mixer_elem_t): cuint; cdecl; external libasound;
function snd_mixer_find_selem(mixer: Psnd_mixer_t;
			      id: Psnd_mixer_selem_id_t): Psnd_mixer_elem_t; cdecl; external libasound;

function snd_mixer_selem_is_active(elem: Psnd_mixer_elem_t): cint; cdecl; external libasound;
function snd_mixer_selem_is_playback_mono(elem: Psnd_mixer_elem_t): cint; cdecl; external libasound;
function snd_mixer_selem_has_playback_channel(obj: Psnd_mixer_elem_t; channel: snd_mixer_selem_channel_id_t): cint; cdecl; external libasound;
function snd_mixer_selem_is_capture_mono(elem: Psnd_mixer_elem_t): cint; cdecl; external libasound;
function snd_mixer_selem_has_capture_channel(obj: Psnd_mixer_elem_t; channel: snd_mixer_selem_channel_id_t): cint; cdecl; external libasound;
function snd_mixer_selem_get_capture_group(elem: Psnd_mixer_elem_t): cint; cdecl; external libasound;
function snd_mixer_selem_has_common_volume(elem: Psnd_mixer_elem_t): cint; cdecl; external libasound;
function snd_mixer_selem_has_playback_volume(elem: Psnd_mixer_elem_t): cint; cdecl; external libasound;
function snd_mixer_selem_has_playback_volume_joined(elem: Psnd_mixer_elem_t): cint; cdecl; external libasound;
function snd_mixer_selem_has_capture_volume(elem: Psnd_mixer_elem_t): cint; cdecl; external libasound;
function snd_mixer_selem_has_capture_volume_joined(elem: Psnd_mixer_elem_t): cint; cdecl; external libasound;
function snd_mixer_selem_has_common_switch(elem: Psnd_mixer_elem_t): cint; cdecl; external libasound;
function snd_mixer_selem_has_playback_switch(elem: Psnd_mixer_elem_t): cint; cdecl; external libasound;
function snd_mixer_selem_has_playback_switch_joined(elem: Psnd_mixer_elem_t): cint; cdecl; external libasound;
function snd_mixer_selem_has_capture_switch(elem: Psnd_mixer_elem_t): cint; cdecl; external libasound;
function snd_mixer_selem_has_capture_switch_joined(elem: Psnd_mixer_elem_t): cint; cdecl; external libasound;
function snd_mixer_selem_has_capture_switch_exclusive(elem: Psnd_mixer_elem_t): cint; cdecl; external libasound;

function snd_mixer_selem_ask_playback_vol_dB(elem: Psnd_mixer_elem_t; value: clong; dBvalue: Pclong): cint; cdecl; external libasound;
function snd_mixer_selem_ask_capture_vol_dB(elem: Psnd_mixer_elem_t; value: clong; dBvalue: Pclong): cint; cdecl; external libasound;
function snd_mixer_selem_ask_playback_dB_vol(elem: Psnd_mixer_elem_t; dBvalue: clong; dir: cint; value: Pclong): cint; cdecl; external libasound;
function snd_mixer_selem_ask_capture_dB_vol(elem: Psnd_mixer_elem_t; dBvalue: clong; dir: cint; value: Pclong): cint; cdecl; external libasound;
function snd_mixer_selem_get_playback_volume(elem: Psnd_mixer_elem_t; channel: snd_mixer_selem_channel_id_t; value: Pclong): cint; cdecl; external libasound;
function snd_mixer_selem_get_capture_volume(elem: Psnd_mixer_elem_t; channel: snd_mixer_selem_channel_id_t; value: Pclong): cint; cdecl; external libasound;
function snd_mixer_selem_get_playback_dB(elem: Psnd_mixer_elem_t; channel: snd_mixer_selem_channel_id_t; value: Pclong): cint; cdecl; external libasound;
function snd_mixer_selem_get_capture_dB(elem: Psnd_mixer_elem_t; channel: snd_mixer_selem_channel_id_t; value: Pclong): cint; cdecl; external libasound;
function snd_mixer_selem_get_playback_switch(elem: Psnd_mixer_elem_t; channel: snd_mixer_selem_channel_id_t; value: Pcint): cint; cdecl; external libasound;
function snd_mixer_selem_get_capture_switch(elem: Psnd_mixer_elem_t; channel: snd_mixer_selem_channel_id_t; value: Pcint): cint; cdecl; external libasound;
function snd_mixer_selem_set_playback_volume(elem: Psnd_mixer_elem_t; channel: snd_mixer_selem_channel_id_t; value: clong): cint; cdecl; external libasound;
function snd_mixer_selem_set_capture_volume(elem: Psnd_mixer_elem_t; channel: snd_mixer_selem_channel_id_t; value: clong): cint; cdecl; external libasound;
function snd_mixer_selem_set_playback_dB(elem: Psnd_mixer_elem_t; channel: snd_mixer_selem_channel_id_t; value: clong; dir: cint): cint; cdecl; external libasound;
function snd_mixer_selem_set_capture_dB(elem: Psnd_mixer_elem_t; channel: snd_mixer_selem_channel_id_t; value: clong; dir: cint): cint; cdecl; external libasound;
function snd_mixer_selem_set_playback_volume_all(elem: Psnd_mixer_elem_t; value: clong): cint; cdecl; external libasound;
function snd_mixer_selem_set_capture_volume_all(elem: Psnd_mixer_elem_t; value: clong): cint; cdecl; external libasound;
function snd_mixer_selem_set_playback_dB_all(elem: Psnd_mixer_elem_t; value: clong; dir: cint): cint; cdecl; external libasound;
function snd_mixer_selem_set_capture_dB_all(elem: Psnd_mixer_elem_t; value: clong; dir: cint): cint; cdecl; external libasound;
function snd_mixer_selem_set_playback_switch(elem: Psnd_mixer_elem_t; channel: snd_mixer_selem_channel_id_t; value: cint): cint; cdecl; external libasound;
function snd_mixer_selem_set_capture_switch(elem: Psnd_mixer_elem_t; channel: snd_mixer_selem_channel_id_t; value: cint): cint; cdecl; external libasound;
function snd_mixer_selem_set_playback_switch_all(elem: Psnd_mixer_elem_t; value: cint): cint; cdecl; external libasound;
function snd_mixer_selem_set_capture_switch_all(elem: Psnd_mixer_elem_t; value: cint): cint; cdecl; external libasound;
function snd_mixer_selem_get_playback_volume_range(elem: Psnd_mixer_elem_t;
					           min, max: Pclong): cint; cdecl; external libasound;
function snd_mixer_selem_get_playback_dB_range(elem: Psnd_mixer_elem_t;
					       min, max: Pclong): cint; cdecl; external libasound;
function snd_mixer_selem_set_playback_volume_range(elem: Psnd_mixer_elem_t;
					           min, max: clong): cint; cdecl; external libasound;
function snd_mixer_selem_get_capture_volume_range(elem: Psnd_mixer_elem_t;
					          min, max: Pclong): cint; cdecl; external libasound;
function snd_mixer_selem_get_capture_dB_range(elem: Psnd_mixer_elem_t;
					      min, max: Pclong): cint; cdecl; external libasound;
function snd_mixer_selem_set_capture_volume_range(elem: Psnd_mixer_elem_t;
					          min, max: clong): cint; cdecl; external libasound;

function snd_mixer_selem_is_enumerated(elem: Psnd_mixer_elem_t): cint; cdecl; external libasound;
function snd_mixer_selem_is_enum_playback(elem: Psnd_mixer_elem_t): cint; cdecl; external libasound;
function snd_mixer_selem_is_enum_capture(elem: Psnd_mixer_elem_t): cint; cdecl; external libasound;
function snd_mixer_selem_get_enum_items(elem: Psnd_mixer_elem_t): cint; cdecl; external libasound;
function snd_mixer_selem_get_enum_item_name(elem: Psnd_mixer_elem_t; idx: cuint; maxlen: size_t; str: PChar): cint; cdecl; external libasound;
function snd_mixer_selem_get_enum_item(elem: Psnd_mixer_elem_t; channel: snd_mixer_selem_channel_id_t; idxp: Pcuint): cint; cdecl; external libasound;
function snd_mixer_selem_set_enum_item(elem: Psnd_mixer_elem_t; channel: snd_mixer_selem_channel_id_t; idx: cuint): cint; cdecl; external libasound;

function snd_mixer_selem_id_sizeof: size_t; cdecl; external libasound;
(** \hideinitializer
 * \brief allocate an invalid #snd_mixer_selem_id_t using standard alloca
 * \param ptr returned pointer
 *)
//#define snd_mixer_selem_id_alloca(ptr) __snd_alloca(ptr, snd_mixer_selem_id)
function snd_mixer_selem_id_malloc(ptr: PPsnd_mixer_selem_id_t): cint; cdecl; external libasound;
procedure snd_mixer_selem_id_free(obj: Psnd_mixer_selem_id_t); cdecl; external libasound;
procedure snd_mixer_selem_id_copy(dst: Psnd_mixer_selem_id_t; src: Psnd_mixer_selem_id_t); cdecl; external libasound;
function snd_mixer_selem_id_get_name(obj: Psnd_mixer_selem_id_t): PChar; cdecl; external libasound;
function snd_mixer_selem_id_get_index(obj: Psnd_mixer_selem_id_t): cuint; cdecl; external libasound;
procedure snd_mixer_selem_id_set_name(obj: Psnd_mixer_selem_id_t; val: PChar); cdecl; external libasound;
procedure snd_mixer_selem_id_set_index(obj: Psnd_mixer_selem_id_t; val: cuint); cdecl; external libasound;

(** \} *)

(** \} *)

//#ifdef __cplusplus
//}
//#endif

//#endif /* __ALSA_MIXER_H */*)

