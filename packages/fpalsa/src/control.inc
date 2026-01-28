{  Free Pascal port by Nikolay Nikolov <nickysn@users.sourceforge.net>  }

{**
 * \file include/control.h
 * \brief Application interface library for the ALSA driver
 * \author Jaroslav Kysela <perex@perex.cz>
 * \author Abramo Bagnara <abramo@alsa-project.org>
 * \author Takashi Iwai <tiwai@suse.de>
 * \date 1998-2001
 *
 * Application interface library for the ALSA driver
 *}
{*
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
 *}

//#ifndef __ALSA_CONTROL_H
//#define __ALSA_CONTROL_H

//#ifdef __cplusplus
//extern "C" {
//#endif

(**
 *  \defgroup Control Control Interface
 *  The control interface.
 *  See \ref control page for more details.
 *  \{
 *)

///** dlsym version for interface entry callback */
//#define SND_CONTROL_DLSYM_VERSION	_dlsym_control_001

type
{ IEC958 structure }
  Psnd_aes_iec958_t = ^snd_aes_iec958_t;
  snd_aes_iec958_t = record
    status: array [0..23] of cuchar;       { AES/IEC958 channel status bits }
    subcode: array [0..146] of cuchar;     { AES/IEC958 subcode bits }
    pad: cuchar;                           { nothing }
    dig_subframe: array [0..3] of cuchar;  { AES/IEC958 subframe bits }
  end;

{ CTL card info container }
  PPsnd_ctl_card_info_t = ^Psnd_ctl_card_info_t;
  Psnd_ctl_card_info_t = ^snd_ctl_card_info_t;
  snd_ctl_card_info_t = record
  end;

{ CTL element identifier container }
  PPsnd_ctl_elem_id_t = ^Psnd_ctl_elem_id_t;
  Psnd_ctl_elem_id_t = ^snd_ctl_elem_id_t;
  snd_ctl_elem_id_t = record
  end;

{ CTL element identifier list container }
  PPsnd_ctl_elem_list_t = ^Psnd_ctl_elem_list_t;
  Psnd_ctl_elem_list_t = ^snd_ctl_elem_list_t;
  snd_ctl_elem_list_t = record
  end;

{ CTL element info container }
  PPsnd_ctl_elem_info_t = ^Psnd_ctl_elem_info_t;
  Psnd_ctl_elem_info_t = ^snd_ctl_elem_info_t;
  snd_ctl_elem_info_t = record
  end;

{ CTL element value container }
  PPsnd_ctl_elem_value_t = ^Psnd_ctl_elem_value_t;
  Psnd_ctl_elem_value_t = ^snd_ctl_elem_value_t;
  snd_ctl_elem_value_t = record
  end;

{ CTL event container }
  PPsnd_ctl_event_t = ^Psnd_ctl_event_t;
  Psnd_ctl_event_t = ^snd_ctl_event_t;
  snd_ctl_event_t = record
  end;

{ CTL element type }
  Psnd_ctl_elem_type_t = ^snd_ctl_elem_type_t;
  snd_ctl_elem_type_t = (
	{ Invalid type }
	SND_CTL_ELEM_TYPE_NONE = 0,
	{ Boolean contents }
	SND_CTL_ELEM_TYPE_BOOLEAN,
	{ Integer contents }
	SND_CTL_ELEM_TYPE_INTEGER,
	{ Enumerated contents }
	SND_CTL_ELEM_TYPE_ENUMERATED,
	{ Bytes contents }
	SND_CTL_ELEM_TYPE_BYTES,
	{ IEC958 (S/PDIF) setting content }
	SND_CTL_ELEM_TYPE_IEC958,
	{ 64-bit integer contents }
	SND_CTL_ELEM_TYPE_INTEGER64,
	SND_CTL_ELEM_TYPE_LAST = SND_CTL_ELEM_TYPE_INTEGER64);

{ CTL related interface }
  Psnd_ctl_elem_iface_t = ^snd_ctl_elem_iface_t;
  snd_ctl_elem_iface_t = (
	{ Card level }
	SND_CTL_ELEM_IFACE_CARD = 0,
	{ Hardware dependent device }
	SND_CTL_ELEM_IFACE_HWDEP,
	{ Mixer }
	SND_CTL_ELEM_IFACE_MIXER,
	{ PCM }
	SND_CTL_ELEM_IFACE_PCM,
	{ RawMidi }
	SND_CTL_ELEM_IFACE_RAWMIDI,
	{ Timer }
	SND_CTL_ELEM_IFACE_TIMER,
	{ Sequencer }
	SND_CTL_ELEM_IFACE_SEQUENCER,
	SND_CTL_ELEM_IFACE_LAST = SND_CTL_ELEM_IFACE_SEQUENCER);

{ Event class }
  Psnd_ctl_event_type_t = ^snd_ctl_event_type_t;
  snd_ctl_event_type_t = (
	{ Elements related event }
	SND_CTL_EVENT_ELEM = 0,
	SND_CTL_EVENT_LAST = SND_CTL_EVENT_ELEM);

const
(** Element has been removed (Warning: test this first and if set don't
  * test the other masks) \hideinitializer *)
  SND_CTL_EVENT_MASK_REMOVE = {(~0U)}$FFFFFFFF;
(** Element value has been changed \hideinitializer *)
  SND_CTL_EVENT_MASK_VALUE  = 1 shl 0;
(** Element info has been changed \hideinitializer *)
  SND_CTL_EVENT_MASK_INFO   = 1 shl 1;
(** Element has been added \hideinitializer *)
  SND_CTL_EVENT_MASK_ADD    = 1 shl 2;
(** Element's TLV value has been changed \hideinitializer *)
  SND_CTL_EVENT_MASK_TLV    = 1 shl 3;

{ CTL name helper }
  SND_CTL_NAME_NONE              = '';
{ CTL name helper }
  SND_CTL_NAME_PLAYBACK          = 'Playback ';
{ CTL name helper }
  SND_CTL_NAME_CAPTURE           = 'Capture ';

{ CTL name helper }
  SND_CTL_NAME_IEC958_NONE       = '';
{ CTL name helper }
  SND_CTL_NAME_IEC958_SWITCH     = 'Switch';
{ CTL name helper }
  SND_CTL_NAME_IEC958_VOLUME     = 'Volume';
{ CTL name helper }
  SND_CTL_NAME_IEC958_DEFAULT    = 'Default';
{ CTL name helper }
  SND_CTL_NAME_IEC958_MASK       = 'Mask';
{ CTL name helper }
  SND_CTL_NAME_IEC958_CON_MASK   = 'Con Mask';
{ CTL name helper }
  SND_CTL_NAME_IEC958_PRO_MASK   = 'Pro Mask';
{ CTL name helper }
  SND_CTL_NAME_IEC958_PCM_STREAM = 'PCM Stream';
{ Element name for IEC958 (S/PDIF) }
{$info ugly macro?!}
//#define SND_CTL_NAME_IEC958(expl,direction,what)	"IEC958 " expl SND_CTL_NAME_##direction SND_CTL_NAME_IEC958_##what

{ Mask for the major Power State identifier }
  SND_CTL_POWER_MASK   = $ff00;
{ ACPI/PCI Power State D0 }
  SND_CTL_POWER_D0     = $0000;
{ ACPI/PCI Power State D1 }
  SND_CTL_POWER_D1     = $0100;
{ ACPI/PCI Power State D2 }
  SND_CTL_POWER_D2     = $0200;
{ ACPI/PCI Power State D3 }
  SND_CTL_POWER_D3     = $0300;
{ ACPI/PCI Power State D3hot }
  SND_CTL_POWER_D3hot  = SND_CTL_POWER_D3 or $0000;
{ ACPI/PCI Power State D3cold }
  SND_CTL_POWER_D3cold = SND_CTL_POWER_D3 or $0001;

{ TLV type - Container }
  SND_CTL_TLVT_CONTAINER      = $0000;
{ TLV type - basic dB scale }
  SND_CTL_TLVT_DB_SCALE       = $0001;
{ TLV type - linear volume }
  SND_CTL_TLVT_DB_LINEAR      = $0002;
{ TLV type - dB range container }
  SND_CTL_TLVT_DB_RANGE       = $0003;
{ TLV type - dB scale specified by min/max values }
  SND_CTL_TLVT_DB_MINMAX      = $0004;
{ TLV type - dB scale specified by min/max values (with mute) }
  SND_CTL_TLVT_DB_MINMAX_MUTE = $0005;

{ Mute state }
  SND_CTL_TLV_DB_GAIN_MUTE = -9999999;

{ TLV type - fixed channel map positions }
  SND_CTL_TLVT_CHMAP_FIXED  = $00101;
{ TLV type - freely swappable channel map positions }
  SND_CTL_TLVT_CHMAP_VAR    = $00102;
{ TLV type - pair-wise swappable channel map positions }
  SND_CTL_TLVT_CHMAP_PAIRED = $00103;

type
{ CTL type }
  Psnd_ctl_type_t = ^snd_ctl_type_t;
  snd_ctl_type_t = (
	{ Kernel level CTL }
	SND_CTL_TYPE_HW,
	{ Shared memory client CTL }
	SND_CTL_TYPE_SHM,
	{ INET client CTL (not yet implemented) }
	SND_CTL_TYPE_INET,
	{ external control plugin }
	SND_CTL_TYPE_EXT);

const
{ Non blocking mode (flag for open mode)  }
  SND_CTL_NONBLOCK = $0001;

{ Async notification (flag for open mode) }
  SND_CTL_ASYNC    = $0002;

{ Read only (flag for open mode) }
  SND_CTL_READONLY = $0004;

type
{ CTL handle }
  PPsnd_ctl_t = ^Psnd_ctl_t;
  Psnd_ctl_t = ^snd_ctl_t;
  snd_ctl_t = record
  end;

const
{ Don't destroy the ctl handle when close }
  SND_SCTL_NOFREE = $0001;

type
{ SCTL type }
  PPsnd_sctl_t = ^Psnd_sctl_t;
  Psnd_sctl_t = ^snd_sctl_t;
  snd_sctl_t = record
  end;

function snd_card_load(card: cint): cint; cdecl; external libasound;
function snd_card_next(card: Pcint): cint; cdecl; external libasound;
function snd_card_get_index(name: PChar): cint; cdecl; external libasound;
function snd_card_get_name(card: cint; name: PPChar): cint; cdecl; external libasound;
function snd_card_get_longname(card: cint; name: PPChar): cint; cdecl; external libasound;

function snd_device_name_hint(card: cint; iface: PChar; hints: PPPointer): cint; cdecl; external libasound;
function snd_device_name_free_hint(hints: PPointer): cint; cdecl; external libasound;
function snd_device_name_get_hint(hint: Pointer; id: PChar): PChar; cdecl; external libasound;

function snd_ctl_open(ctl: PPsnd_ctl_t; name: PChar; mode: cint): cint; cdecl; external libasound;
function snd_ctl_open_lconf(ctl: PPsnd_ctl_t; name: PChar; mode: cint; lconf: Psnd_config_t): cint; cdecl; external libasound;
function snd_ctl_open_fallback(ctl: PPsnd_ctl_t; root: Psnd_config_t; name, orig_name: PChar; mode: cint): cint; cdecl; external libasound;
function snd_ctl_close(ctl: Psnd_ctl_t): cint; cdecl; external libasound;
{$INFO snd_ctl_nonblock renamed to snd_ctl_set_nonblock to avoid name clash, because Pascal isn't case sensitive}
function snd_ctl_set_nonblock(ctl: Psnd_ctl_t; nonblock: cint): cint; cdecl; external libasound Name 'snd_ctl_nonblock';
function snd_ctl_abort(ctl: Psnd_ctl_t): cint; inline;
function snd_async_add_ctl_handler(handler: PPsnd_async_handler_t; ctl: Psnd_ctl_t;
			           callback: snd_async_callback_t; private_data: Pointer): cint; cdecl; external libasound;
function snd_async_handler_get_ctl(handler: Psnd_async_handler_t): Psnd_ctl_t; cdecl; external libasound;
function snd_ctl_poll_descriptors_count(ctl: Psnd_ctl_t): cint; cdecl; external libasound;
{$INFO pollfd?!}
function snd_ctl_poll_descriptors(ctl: Psnd_ctl_t; pfds: {struct pollfd*}Pointer; space: cuint): cint; cdecl; external libasound;
{$INFO pollfd?!}
function snd_ctl_poll_descriptors_revents(ctl: Psnd_ctl_t; pfds: {struct pollfd*}Pointer; nfds: cuint; revents: Pcushort): cint; cdecl; external libasound;
function snd_ctl_subscribe_events(ctl: Psnd_ctl_t; subscribe: cint): cint; cdecl; external libasound;
function snd_ctl_card_info(ctl: Psnd_ctl_t; info: Psnd_ctl_card_info_t): cint; cdecl; external libasound;
function snd_ctl_elem_list(ctl: Psnd_ctl_t; list: Psnd_ctl_elem_list_t): cint; cdecl; external libasound;
function snd_ctl_elem_info(ctl: Psnd_ctl_t; info: Psnd_ctl_elem_info_t): cint; cdecl; external libasound;
function snd_ctl_elem_read(ctl: Psnd_ctl_t; value: Psnd_ctl_elem_value_t): cint; cdecl; external libasound;
function snd_ctl_elem_write(ctl: Psnd_ctl_t; value: Psnd_ctl_elem_value_t): cint; cdecl; external libasound;
function snd_ctl_elem_lock(ctl: Psnd_ctl_t; id: Psnd_ctl_elem_id_t): cint; cdecl; external libasound;
function snd_ctl_elem_unlock(ctl: Psnd_ctl_t; id: Psnd_ctl_elem_id_t): cint; cdecl; external libasound;
function snd_ctl_elem_tlv_read(ctl: Psnd_ctl_t; id: Psnd_ctl_elem_id_t;
			       tlv: Pcuint; tlv_size: cuint): cint; cdecl; external libasound;
function snd_ctl_elem_tlv_write(ctl: Psnd_ctl_t; id: Psnd_ctl_elem_id_t;
			        tlv: Pcuint): cint; cdecl; external libasound;
function snd_ctl_elem_tlv_command(ctl: Psnd_ctl_t; id: Psnd_ctl_elem_id_t;
			          tlv: Pcuint): cint; cdecl; external libasound;
//#ifdef __ALSA_HWDEP_H
function snd_ctl_hwdep_next_device(ctl: Psnd_ctl_t; device: Pcint): cint; cdecl; external libasound;
function snd_ctl_hwdep_info(ctl: Psnd_ctl_t; info: Psnd_hwdep_info_t): cint; cdecl; external libasound;
//#endif
//#ifdef __ALSA_PCM_H
function snd_ctl_pcm_next_device(ctl: Psnd_ctl_t; device: Pcint): cint; cdecl; external libasound;
function snd_ctl_pcm_info(ctl: Psnd_ctl_t; info: Psnd_pcm_info_t): cint; cdecl; external libasound;
function snd_ctl_pcm_prefer_subdevice(ctl: Psnd_ctl_t; subdev: cint): cint; cdecl; external libasound;
//#endif
//#ifdef __ALSA_RAWMIDI_H
function snd_ctl_rawmidi_next_device(ctl: Psnd_ctl_t; device: Pcint): cint; cdecl; external libasound;
function snd_ctl_rawmidi_info(ctl: Psnd_ctl_t; info: Psnd_rawmidi_info_t): cint; cdecl; external libasound;
function snd_ctl_rawmidi_prefer_subdevice(ctl: Psnd_ctl_t; subdev: cint): cint; cdecl; external libasound;
//#endif
function snd_ctl_set_power_state(ctl: Psnd_ctl_t; state: cuint): cint; cdecl; external libasound;
function snd_ctl_get_power_state(ctl: Psnd_ctl_t; state: Pcuint): cint; cdecl; external libasound;

function snd_ctl_read(ctl: Psnd_ctl_t; event: Psnd_ctl_event_t): cint; cdecl; external libasound;
function snd_ctl_wait(ctl: Psnd_ctl_t; timeout: cint): cint; cdecl; external libasound;
function snd_ctl_name(ctl: Psnd_ctl_t): PChar; cdecl; external libasound;
function snd_ctl_type(ctl: Psnd_ctl_t): snd_ctl_type_t; cdecl; external libasound;

function snd_ctl_elem_type_name(_type: snd_ctl_elem_type_t): PChar; cdecl; external libasound;
function snd_ctl_elem_iface_name(iface: snd_ctl_elem_iface_t): PChar; cdecl; external libasound;
function snd_ctl_event_type_name(_type: snd_ctl_event_type_t): PChar; cdecl; external libasound;

function snd_ctl_event_elem_get_mask(obj: Psnd_ctl_event_t): cuint; cdecl; external libasound;
function snd_ctl_event_elem_get_numid(obj: Psnd_ctl_event_t): cuint; cdecl; external libasound;
procedure snd_ctl_event_elem_get_id(obj: Psnd_ctl_event_t; ptr: Psnd_ctl_elem_id_t); cdecl; external libasound;
function snd_ctl_event_elem_get_interface(obj: Psnd_ctl_event_t): snd_ctl_elem_iface_t; cdecl; external libasound;
function snd_ctl_event_elem_get_device(obj: Psnd_ctl_event_t): cuint; cdecl; external libasound;
function snd_ctl_event_elem_get_subdevice(obj: Psnd_ctl_event_t): cuint; cdecl; external libasound;
function snd_ctl_event_elem_get_name(obj: Psnd_ctl_event_t): PChar; cdecl; external libasound;
function snd_ctl_event_elem_get_index(obj: Psnd_ctl_event_t): cuint; cdecl; external libasound;

function snd_ctl_elem_list_alloc_space(obj: Psnd_ctl_elem_list_t; entries: cuint): cint; cdecl; external libasound;
procedure snd_ctl_elem_list_free_space(obj: Psnd_ctl_elem_list_t); cdecl; external libasound;

function snd_ctl_ascii_elem_id_get(id: Psnd_ctl_elem_id_t): PChar; cdecl; external libasound;
function snd_ctl_ascii_elem_id_parse(dst: Psnd_ctl_elem_id_t; str: PChar): cint; cdecl; external libasound;
function snd_ctl_ascii_value_parse(handle: Psnd_ctl_t;
                                   dst: Psnd_ctl_elem_value_t;
                                   info: Psnd_ctl_elem_info_t;
                                   value: PChar): cint; cdecl; external libasound;

function snd_ctl_elem_id_sizeof: size_t; cdecl; external libasound;
(** \hideinitializer
 * \brief allocate an invalid #snd_ctl_elem_id_t using standard alloca
 * \param ptr returned pointer
 *)
//#define snd_ctl_elem_id_alloca(ptr) __snd_alloca(ptr, snd_ctl_elem_id)
function snd_ctl_elem_id_malloc(ptr: PPsnd_ctl_elem_id_t): cint; cdecl; external libasound;
procedure snd_ctl_elem_id_free(obj: Psnd_ctl_elem_id_t); cdecl; external libasound;
procedure snd_ctl_elem_id_clear(obj: Psnd_ctl_elem_id_t); cdecl; external libasound;
procedure snd_ctl_elem_id_copy(dst: Psnd_ctl_elem_id_t; src: Psnd_ctl_elem_id_t); cdecl; external libasound;
function snd_ctl_elem_id_get_numid(obj: Psnd_ctl_elem_id_t): cuint; cdecl; external libasound;
function snd_ctl_elem_id_get_interface(obj: Psnd_ctl_elem_id_t): snd_ctl_elem_iface_t; cdecl; external libasound;
function snd_ctl_elem_id_get_device(obj: Psnd_ctl_elem_id_t): cuint; cdecl; external libasound;
function snd_ctl_elem_id_get_subdevice(obj: Psnd_ctl_elem_id_t): cuint; cdecl; external libasound;
function snd_ctl_elem_id_get_name(obj: Psnd_ctl_elem_id_t): PChar; cdecl; external libasound;
function snd_ctl_elem_id_get_index(obj: Psnd_ctl_elem_id_t): cuint; cdecl; external libasound;
procedure snd_ctl_elem_id_set_numid(obj: Psnd_ctl_elem_id_t; val: cuint); cdecl; external libasound;
procedure snd_ctl_elem_id_set_interface(obj: Psnd_ctl_elem_id_t; val: snd_ctl_elem_iface_t); cdecl; external libasound;
procedure snd_ctl_elem_id_set_device(obj: Psnd_ctl_elem_id_t; val: cuint); cdecl; external libasound;
procedure snd_ctl_elem_id_set_subdevice(obj: Psnd_ctl_elem_id_t; val: cuint); cdecl; external libasound;
procedure snd_ctl_elem_id_set_name(obj: Psnd_ctl_elem_id_t; val: PChar); cdecl; external libasound;
procedure snd_ctl_elem_id_set_index(obj: Psnd_ctl_elem_id_t; val: cuint); cdecl; external libasound;

function snd_ctl_card_info_sizeof: size_t; cdecl; external libasound;
(** \hideinitializer
 * \brief allocate an invalid #snd_ctl_card_info_t using standard alloca
 * \param ptr returned pointer
 *)
//#define snd_ctl_card_info_alloca(ptr) __snd_alloca(ptr, snd_ctl_card_info)
function snd_ctl_card_info_malloc(ptr: PPsnd_ctl_card_info_t): cint; cdecl; external libasound;
procedure snd_ctl_card_info_free(obj: Psnd_ctl_card_info_t); cdecl; external libasound;
procedure snd_ctl_card_info_clear(obj: Psnd_ctl_card_info_t); cdecl; external libasound;
procedure snd_ctl_card_info_copy(dst: Psnd_ctl_card_info_t; src: Psnd_ctl_card_info_t); cdecl; external libasound;
function snd_ctl_card_info_get_card(obj: Psnd_ctl_card_info_t): cint; cdecl; external libasound;
function snd_ctl_card_info_get_id(obj: Psnd_ctl_card_info_t): PChar; cdecl; external libasound;
function snd_ctl_card_info_get_driver(obj: Psnd_ctl_card_info_t): PChar; cdecl; external libasound;
function snd_ctl_card_info_get_name(obj: Psnd_ctl_card_info_t): PChar; cdecl; external libasound;
function snd_ctl_card_info_get_longname(obj: Psnd_ctl_card_info_t): PChar; cdecl; external libasound;
function snd_ctl_card_info_get_mixername(obj: Psnd_ctl_card_info_t): PChar; cdecl; external libasound;
function snd_ctl_card_info_get_components(obj: Psnd_ctl_card_info_t): PChar; cdecl; external libasound;

function snd_ctl_event_sizeof: size_t; cdecl; external libasound;
(** \hideinitializer
 * \brief allocate an invalid #snd_ctl_event_t using standard alloca
 * \param ptr returned pointer
 *)
//#define snd_ctl_event_alloca(ptr) __snd_alloca(ptr, snd_ctl_event)
function snd_ctl_event_malloc(ptr: PPsnd_ctl_event_t): cint; cdecl; external libasound;
procedure snd_ctl_event_free(obj: Psnd_ctl_event_t); cdecl; external libasound;
procedure snd_ctl_event_clear(obj: Psnd_ctl_event_t); cdecl; external libasound;
procedure snd_ctl_event_copy(dst: Psnd_ctl_event_t; src: Psnd_ctl_event_t); cdecl; external libasound;
function snd_ctl_event_get_type(obj: Psnd_ctl_event_t): snd_ctl_event_type_t; cdecl; external libasound;

function snd_ctl_elem_list_sizeof: size_t; cdecl; external libasound;
(** \hideinitializer
 * \brief allocate an invalid #snd_ctl_elem_list_t using standard alloca
 * \param ptr returned pointer
 *)
//#define snd_ctl_elem_list_alloca(ptr) __snd_alloca(ptr, snd_ctl_elem_list)
function snd_ctl_elem_list_malloc(ptr: PPsnd_ctl_elem_list_t): cint; cdecl; external libasound;
procedure snd_ctl_elem_list_free(obj: Psnd_ctl_elem_list_t); cdecl; external libasound;
procedure snd_ctl_elem_list_clear(obj: Psnd_ctl_elem_list_t); cdecl; external libasound;
procedure snd_ctl_elem_list_copy(dst: Psnd_ctl_elem_list_t; src: Psnd_ctl_elem_list_t); cdecl; external libasound;
procedure snd_ctl_elem_list_set_offset(obj: Psnd_ctl_elem_list_t; val: cuint); cdecl; external libasound;
function snd_ctl_elem_list_get_used(obj: Psnd_ctl_elem_list_t): cuint; cdecl; external libasound;
function snd_ctl_elem_list_get_count(obj: Psnd_ctl_elem_list_t): cuint; cdecl; external libasound;
procedure snd_ctl_elem_list_get_id(obj: Psnd_ctl_elem_list_t; idx: cuint; ptr: Psnd_ctl_elem_id_t); cdecl; external libasound;
function snd_ctl_elem_list_get_numid(obj: Psnd_ctl_elem_list_t; idx: cuint): cuint; cdecl; external libasound;
function snd_ctl_elem_list_get_interface(obj: Psnd_ctl_elem_list_t; idx: cuint): snd_ctl_elem_iface_t; cdecl; external libasound;
function snd_ctl_elem_list_get_device(obj: Psnd_ctl_elem_list_t; idx: cuint): cuint; cdecl; external libasound;
function snd_ctl_elem_list_get_subdevice(obj: Psnd_ctl_elem_list_t; idx: cuint): cuint; cdecl; external libasound;
function snd_ctl_elem_list_get_name(obj: Psnd_ctl_elem_list_t; idx: cuint): PChar; cdecl; external libasound;
function snd_ctl_elem_list_get_index(obj: Psnd_ctl_elem_list_t; idx: cuint): cuint; cdecl; external libasound;

function snd_ctl_elem_info_sizeof: size_t; cdecl; external libasound;
(** \hideinitializer
 * \brief allocate an invalid #snd_ctl_elem_info_t using standard alloca
 * \param ptr returned pointer
 *)
//#define snd_ctl_elem_info_alloca(ptr) __snd_alloca(ptr, snd_ctl_elem_info)
function snd_ctl_elem_info_malloc(ptr: PPsnd_ctl_elem_info_t): cint; cdecl; external libasound;
procedure snd_ctl_elem_info_free(obj: Psnd_ctl_elem_info_t); cdecl; external libasound;
procedure snd_ctl_elem_info_clear(obj: Psnd_ctl_elem_info_t); cdecl; external libasound;
procedure snd_ctl_elem_info_copy(dst: Psnd_ctl_elem_info_t; src: Psnd_ctl_elem_info_t); cdecl; external libasound;
function snd_ctl_elem_info_get_type(obj: Psnd_ctl_elem_info_t): snd_ctl_elem_type_t; cdecl; external libasound;
function snd_ctl_elem_info_is_readable(obj: Psnd_ctl_elem_info_t): cint; cdecl; external libasound;
function snd_ctl_elem_info_is_writable(obj: Psnd_ctl_elem_info_t): cint; cdecl; external libasound;
function snd_ctl_elem_info_is_volatile(obj: Psnd_ctl_elem_info_t): cint; cdecl; external libasound;
function snd_ctl_elem_info_is_inactive(obj: Psnd_ctl_elem_info_t): cint; cdecl; external libasound;
function snd_ctl_elem_info_is_locked(obj: Psnd_ctl_elem_info_t): cint; cdecl; external libasound;
function snd_ctl_elem_info_is_tlv_readable(obj: Psnd_ctl_elem_info_t): cint; cdecl; external libasound;
function snd_ctl_elem_info_is_tlv_writable(obj: Psnd_ctl_elem_info_t): cint; cdecl; external libasound;
function snd_ctl_elem_info_is_tlv_commandable(obj: Psnd_ctl_elem_info_t): cint; cdecl; external libasound;
function snd_ctl_elem_info_is_owner(obj: Psnd_ctl_elem_info_t): cint; cdecl; external libasound;
function snd_ctl_elem_info_is_user(obj: Psnd_ctl_elem_info_t): cint; cdecl; external libasound;
function snd_ctl_elem_info_get_owner(obj: Psnd_ctl_elem_info_t): pid_t; cdecl; external libasound;
function snd_ctl_elem_info_get_count(obj: Psnd_ctl_elem_info_t): cuint; cdecl; external libasound;
function snd_ctl_elem_info_get_min(obj: Psnd_ctl_elem_info_t): clong; cdecl; external libasound;
function snd_ctl_elem_info_get_max(obj: Psnd_ctl_elem_info_t): clong; cdecl; external libasound;
function snd_ctl_elem_info_get_step(obj: Psnd_ctl_elem_info_t): clong; cdecl; external libasound;
function snd_ctl_elem_info_get_min64(obj: Psnd_ctl_elem_info_t): clonglong; cdecl; external libasound;
function snd_ctl_elem_info_get_max64(obj: Psnd_ctl_elem_info_t): clonglong; cdecl; external libasound;
function snd_ctl_elem_info_get_step64(obj: Psnd_ctl_elem_info_t): clonglong; cdecl; external libasound;
function snd_ctl_elem_info_get_items(obj: Psnd_ctl_elem_info_t): cuint; cdecl; external libasound;
procedure snd_ctl_elem_info_set_item(obj: Psnd_ctl_elem_info_t; val: cuint); cdecl; external libasound;
function snd_ctl_elem_info_get_item_name(obj: Psnd_ctl_elem_info_t): PChar; cdecl; external libasound;
function snd_ctl_elem_info_get_dimensions(obj: Psnd_ctl_elem_info_t): cint; cdecl; external libasound;
function snd_ctl_elem_info_get_dimension(obj: Psnd_ctl_elem_info_t; idx: cuint): cint; cdecl; external libasound;
procedure snd_ctl_elem_info_get_id(obj: Psnd_ctl_elem_info_t; ptr: Psnd_ctl_elem_id_t); cdecl; external libasound;
function snd_ctl_elem_info_get_numid(obj: Psnd_ctl_elem_info_t): cuint; cdecl; external libasound;
function snd_ctl_elem_info_get_interface(obj: Psnd_ctl_elem_info_t): snd_ctl_elem_iface_t; cdecl; external libasound;
function snd_ctl_elem_info_get_device(obj: Psnd_ctl_elem_info_t): cuint; cdecl; external libasound;
function snd_ctl_elem_info_get_subdevice(obj: Psnd_ctl_elem_info_t): cuint; cdecl; external libasound;
function snd_ctl_elem_info_get_name(obj: Psnd_ctl_elem_info_t): PChar; cdecl; external libasound;
function snd_ctl_elem_info_get_index(obj: Psnd_ctl_elem_info_t): cuint; cdecl; external libasound;
procedure snd_ctl_elem_info_set_id(obj: Psnd_ctl_elem_info_t; ptr: Psnd_ctl_elem_id_t); cdecl; external libasound;
procedure snd_ctl_elem_info_set_numid(obj: Psnd_ctl_elem_info_t; val: cuint); cdecl; external libasound;
procedure snd_ctl_elem_info_set_interface(obj: Psnd_ctl_elem_info_t; val: snd_ctl_elem_iface_t); cdecl; external libasound;
procedure snd_ctl_elem_info_set_device(obj: Psnd_ctl_elem_info_t; val: cuint); cdecl; external libasound;
procedure snd_ctl_elem_info_set_subdevice(obj: Psnd_ctl_elem_info_t; val: cuint); cdecl; external libasound;
procedure snd_ctl_elem_info_set_name(obj: Psnd_ctl_elem_info_t; val: PChar); cdecl; external libasound;
procedure snd_ctl_elem_info_set_index(obj: Psnd_ctl_elem_info_t; val: cuint); cdecl; external libasound;

function snd_ctl_elem_add_integer(ctl: Psnd_ctl_t; id: Psnd_ctl_elem_id_t; count: cuint; imin, imax, istep: clong): cint; cdecl; external libasound;
function snd_ctl_elem_add_integer64(ctl: Psnd_ctl_t; id: Psnd_ctl_elem_id_t; count: cuint; imin, imax, istep: clonglong): cint; cdecl; external libasound;
function snd_ctl_elem_add_boolean(ctl: Psnd_ctl_t; id: Psnd_ctl_elem_id_t; count: cuint): cint; cdecl; external libasound;
function snd_ctl_elem_add_enumerated(ctl: Psnd_ctl_t; id: Psnd_ctl_elem_id_t; count, items: cuint; names: PPChar): cint; cdecl; external libasound;
function snd_ctl_elem_add_iec958(ctl: Psnd_ctl_t; id: Psnd_ctl_elem_id_t): cint; cdecl; external libasound;
function snd_ctl_elem_remove(ctl: Psnd_ctl_t; id: Psnd_ctl_elem_id_t): cint; cdecl; external libasound;

function snd_ctl_elem_value_sizeof: size_t; cdecl; external libasound;
(** \hideinitializer
 * \brief allocate an invalid #snd_ctl_elem_value_t using standard alloca
 * \param ptr returned pointer
 *)
//#define snd_ctl_elem_value_alloca(ptr) __snd_alloca(ptr, snd_ctl_elem_value)
function snd_ctl_elem_value_malloc(ptr: PPsnd_ctl_elem_value_t): cint; cdecl; external libasound;
procedure snd_ctl_elem_value_free(obj: Psnd_ctl_elem_value_t); cdecl; external libasound;
procedure snd_ctl_elem_value_clear(obj: Psnd_ctl_elem_value_t); cdecl; external libasound;
procedure snd_ctl_elem_value_copy(dst: Psnd_ctl_elem_value_t; src: Psnd_ctl_elem_value_t); cdecl; external libasound;
function snd_ctl_elem_value_compare(left: Psnd_ctl_elem_value_t; right: Psnd_ctl_elem_value_t): cint; cdecl; external libasound;
procedure snd_ctl_elem_value_get_id(obj: Psnd_ctl_elem_value_t; ptr: Psnd_ctl_elem_id_t); cdecl; external libasound;
function snd_ctl_elem_value_get_numid(obj: Psnd_ctl_elem_value_t): cuint; cdecl; external libasound;
function snd_ctl_elem_value_get_interface(obj: Psnd_ctl_elem_value_t): snd_ctl_elem_iface_t; cdecl; external libasound;
function snd_ctl_elem_value_get_device(obj: Psnd_ctl_elem_value_t): cuint; cdecl; external libasound;
function snd_ctl_elem_value_get_subdevice(obj: Psnd_ctl_elem_value_t): cuint; cdecl; external libasound;
function snd_ctl_elem_value_get_name(obj: Psnd_ctl_elem_value_t): PChar; cdecl; external libasound;
function snd_ctl_elem_value_get_index(obj: Psnd_ctl_elem_value_t): cuint; cdecl; external libasound;
procedure snd_ctl_elem_value_set_id(obj: Psnd_ctl_elem_value_t; ptr: Psnd_ctl_elem_id_t); cdecl; external libasound;
procedure snd_ctl_elem_value_set_numid(obj: Psnd_ctl_elem_value_t; val: cuint); cdecl; external libasound;
procedure snd_ctl_elem_value_set_interface(obj: Psnd_ctl_elem_value_t; val: snd_ctl_elem_iface_t); cdecl; external libasound;
procedure snd_ctl_elem_value_set_device(obj: Psnd_ctl_elem_value_t; val: cuint); cdecl; external libasound;
procedure snd_ctl_elem_value_set_subdevice(obj: Psnd_ctl_elem_value_t; val: cuint); cdecl; external libasound;
procedure snd_ctl_elem_value_set_name(obj: Psnd_ctl_elem_value_t; val: PChar); cdecl; external libasound;
procedure snd_ctl_elem_value_set_index(obj: Psnd_ctl_elem_value_t; val: cuint); cdecl; external libasound;
function snd_ctl_elem_value_get_boolean(obj: Psnd_ctl_elem_value_t; idx: cuint): cint; cdecl; external libasound;
function snd_ctl_elem_value_get_integer(obj: Psnd_ctl_elem_value_t; idx: cuint): clong; cdecl; external libasound;
function snd_ctl_elem_value_get_integer64(obj: Psnd_ctl_elem_value_t; idx: cuint): clonglong; cdecl; external libasound;
function snd_ctl_elem_value_get_enumerated(obj: Psnd_ctl_elem_value_t; idx: cuint): cuint; cdecl; external libasound;
function snd_ctl_elem_value_get_byte(obj: Psnd_ctl_elem_value_t; idx: cuint): cuchar; cdecl; external libasound;
procedure snd_ctl_elem_value_set_boolean(obj: Psnd_ctl_elem_value_t; idx: cuint; val: clong); cdecl; external libasound;
procedure snd_ctl_elem_value_set_integer(obj: Psnd_ctl_elem_value_t; idx: cuint; val: clong); cdecl; external libasound;
procedure snd_ctl_elem_value_set_integer64(obj: Psnd_ctl_elem_value_t; idx: cuint; val: clonglong); cdecl; external libasound;
procedure snd_ctl_elem_value_set_enumerated(obj: Psnd_ctl_elem_value_t; idx: cuint; val: cuint); cdecl; external libasound;
procedure snd_ctl_elem_value_set_byte(obj: Psnd_ctl_elem_value_t; idx: cuint; val: cuchar); cdecl; external libasound;
procedure snd_ctl_elem_set_bytes(obj: Psnd_ctl_elem_value_t; data: Pointer; size: size_t); cdecl; external libasound;
function snd_ctl_elem_value_get_bytes(obj: Psnd_ctl_elem_value_t): Pointer; cdecl; external libasound;
procedure snd_ctl_elem_value_get_iec958(obj: Psnd_ctl_elem_value_t; ptr: Psnd_aes_iec958_t); cdecl; external libasound;
procedure snd_ctl_elem_value_set_iec958(obj: Psnd_ctl_elem_value_t; ptr: Psnd_aes_iec958_t); cdecl; external libasound;

{$INFO unit ctypes has no PPcuint!!!}
type
  PPcuint = ^Pcuint;
function snd_tlv_parse_dB_info(tlv: Pcuint; tlv_size: cuint;
			       db_tlvp: PPcuint): cint; cdecl; external libasound;
function snd_tlv_get_dB_range(tlv: Pcuint; rangemin, rangemax: clong;
			      min, max: Pclong): cint; cdecl; external libasound;
function snd_tlv_convert_to_dB(tlv: Pcuint; rangemin, rangemax,
			       volume: clong; db_gain: Pclong): cint; cdecl; external libasound;
function snd_tlv_convert_from_dB(tlv: Pcuint; rangemin, rangemax,
			         db_gain: clong; value: Pclong; xdir: cint): cint; cdecl; external libasound;
function snd_ctl_get_dB_range(ctl: Psnd_ctl_t; id: Psnd_ctl_elem_id_t;
			      min, max: Pclong): cint; cdecl; external libasound;
function snd_ctl_convert_to_dB(ctl: Psnd_ctl_t; id: Psnd_ctl_elem_id_t;
			       volume: clong; db_gain: Pclong): cint; cdecl; external libasound;
function snd_ctl_convert_from_dB(ctl: Psnd_ctl_t; id: Psnd_ctl_elem_id_t;
			         db_gain: clong; value: Pclong; xdir: cint): cint; cdecl; external libasound;

(**
 *  \defgroup HControl High level Control Interface
 *  \ingroup Control
 *  The high level control interface.
 *  See \ref hcontrol page for more details.
 *  \{
 *)

type
(** HCTL element handle *)
  PPsnd_hctl_elem_t = ^Psnd_hctl_elem_t;
  Psnd_hctl_elem_t = ^snd_hctl_elem_t;
  snd_hctl_elem_t = record
  end;

(** HCTL handle *)
  PPsnd_hctl_t = ^Psnd_hctl_t;
  Psnd_hctl_t = ^snd_hctl_t;
  snd_hctl_t = record
  end;

(**
 * \brief Compare function for sorting HCTL elements
 * \param e1 First element
 * \param e2 Second element
 * \return -1 if e1 < e2, 0 if e1 == e2, 1 if e1 > e2
 *)
  snd_hctl_compare_t = function(e1, e2: Psnd_hctl_elem_t): cint; cdecl;
function snd_hctl_compare_fast(c1, c2: Psnd_hctl_elem_t): cint; cdecl; external libasound;

type
(** 
 * \brief HCTL callback function
 * \param hctl HCTL handle
 * \param mask event mask
 * \param elem related HCTL element (if any)
 * \return 0 on success otherwise a negative error code
 *)
  snd_hctl_callback_t = function(hctl: Psnd_hctl_t;
				 mask: cuint;
				 elem: Psnd_hctl_elem_t): cint; cdecl;
(** 
 * \brief HCTL element callback function
 * \param elem HCTL element
 * \param mask event mask
 * \return 0 on success otherwise a negative error code
 *)
  snd_hctl_elem_callback_t = function(elem: Psnd_hctl_elem_t;
				      mask: cuint): cint; cdecl;

function snd_hctl_open(hctl: PPsnd_hctl_t; name: PChar; mode: cint): cint; cdecl; external libasound;
function snd_hctl_open_ctl(hctlp: PPsnd_hctl_t; ctl: Psnd_ctl_t): cint; cdecl; external libasound;
function snd_hctl_close(hctl: Psnd_hctl_t): cint; cdecl; external libasound;
function snd_hctl_nonblock(hctl: Psnd_hctl_t; nonblock: cint): cint; cdecl; external libasound;
function snd_hctl_abort(hctl: Psnd_hctl_t): cint; inline;
function snd_hctl_poll_descriptors_count(hctl: Psnd_hctl_t): cint; cdecl; external libasound;
{$INFO pollfd?!}
function snd_hctl_poll_descriptors(hctl: Psnd_hctl_t; pfds: {struct pollfd*}Pointer; space: cuint): cint; cdecl; external libasound;
{$INFO pollfd?!}
function snd_hctl_poll_descriptors_revents(ctl: Psnd_hctl_t; pfds: {struct pollfd*}Pointer; nfds: cuint; revents: Pcushort): cint; cdecl; external libasound;
function snd_hctl_get_count(hctl: Psnd_hctl_t): cuint; cdecl; external libasound;
function snd_hctl_set_compare(hctl: Psnd_hctl_t; hsort: snd_hctl_compare_t): cint; cdecl; external libasound;
function snd_hctl_first_elem(hctl: Psnd_hctl_t): Psnd_hctl_elem_t; cdecl; external libasound;
function snd_hctl_last_elem(hctl: Psnd_hctl_t): Psnd_hctl_elem_t; cdecl; external libasound;
function snd_hctl_find_elem(hctl: Psnd_hctl_t; id: Psnd_ctl_elem_id_t): Psnd_hctl_elem_t; cdecl; external libasound;
procedure snd_hctl_set_callback(hctl: Psnd_hctl_t; callback: snd_hctl_callback_t); cdecl; external libasound;
procedure snd_hctl_set_callback_private(hctl: Psnd_hctl_t; data: Pointer); cdecl; external libasound;
function snd_hctl_get_callback_private(hctl: Psnd_hctl_t): Pointer; cdecl; external libasound;
function snd_hctl_load(hctl: Psnd_hctl_t): cint; cdecl; external libasound;
function snd_hctl_free(hctl: Psnd_hctl_t): cint; cdecl; external libasound;
function snd_hctl_handle_events(hctl: Psnd_hctl_t): cint; cdecl; external libasound;
function snd_hctl_name(hctl: Psnd_hctl_t): PChar; cdecl; external libasound;
function snd_hctl_wait(hctl: Psnd_hctl_t; timeout: cint): cint; cdecl; external libasound;
function snd_hctl_ctl(hctl: Psnd_hctl_t): Psnd_ctl_t; cdecl; external libasound;

function snd_hctl_elem_next(elem: Psnd_hctl_elem_t): Psnd_hctl_elem_t; cdecl; external libasound;
function snd_hctl_elem_prev(elem: Psnd_hctl_elem_t): Psnd_hctl_elem_t; cdecl; external libasound;
function snd_hctl_elem_info(elem: Psnd_hctl_elem_t; info: Psnd_ctl_elem_info_t): cint; cdecl; external libasound;
function snd_hctl_elem_read(elem: Psnd_hctl_elem_t; value: Psnd_ctl_elem_value_t): cint; cdecl; external libasound;
function snd_hctl_elem_write(elem: Psnd_hctl_elem_t; value: Psnd_ctl_elem_value_t): cint; cdecl; external libasound;
function snd_hctl_elem_tlv_read(elem: Psnd_hctl_elem_t; tlv: Pcuint; tlv_size: cuint): cint; cdecl; external libasound;
function snd_hctl_elem_tlv_write(elem: Psnd_hctl_elem_t; tlv: Pcuint): cint; cdecl; external libasound;
function snd_hctl_elem_tlv_command(elem: Psnd_hctl_elem_t; tlv: Pcuint): cint; cdecl; external libasound;

function snd_hctl_elem_get_hctl(elem: Psnd_hctl_elem_t): Psnd_hctl_t; cdecl; external libasound;

procedure snd_hctl_elem_get_id(obj: Psnd_hctl_elem_t; ptr: Psnd_ctl_elem_id_t); cdecl; external libasound;
function snd_hctl_elem_get_numid(obj: Psnd_hctl_elem_t): cuint; cdecl; external libasound;
function snd_hctl_elem_get_interface(obj: Psnd_hctl_elem_t): snd_ctl_elem_iface_t; cdecl; external libasound;
function snd_hctl_elem_get_device(obj: Psnd_hctl_elem_t): cuint; cdecl; external libasound;
function snd_hctl_elem_get_subdevice(obj: Psnd_hctl_elem_t): cuint; cdecl; external libasound;
function snd_hctl_elem_get_name(obj: Psnd_hctl_elem_t): PChar; cdecl; external libasound;
function snd_hctl_elem_get_index(obj: Psnd_hctl_elem_t): cuint; cdecl; external libasound;
procedure snd_hctl_elem_set_callback(obj: Psnd_hctl_elem_t; val: snd_hctl_elem_callback_t); cdecl; external libasound;
function snd_hctl_elem_get_callback_private(obj: Psnd_hctl_elem_t): Pointer; cdecl; external libasound;
procedure snd_hctl_elem_set_callback_private(obj: Psnd_hctl_elem_t; val: Pointer); cdecl; external libasound;

(** \} *)

(** \} *)

(**
 *  \defgroup SControl Setup Control Interface
 *  \ingroup Control
 *  The setup control interface - set or modify control elements from a configuration file.
 *  \{
 *)

function snd_sctl_build(ctl: PPsnd_sctl_t; handle: Psnd_ctl_t; config: Psnd_config_t;
		        private_data: Psnd_config_t; mode: cint): cint; cdecl; external libasound;
function snd_sctl_free(handle: Psnd_sctl_t): cint; cdecl; external libasound;
function snd_sctl_install(handle: Psnd_sctl_t): cint; cdecl; external libasound;
function snd_sctl_remove(handle: Psnd_sctl_t): cint; cdecl; external libasound;

(** \} *)

//#ifdef __cplusplus
//}
//#endif

//#endif /* __ALSA_CONTROL_H */
