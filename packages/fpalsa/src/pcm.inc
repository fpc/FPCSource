{  Free Pascal port by Nikolay Nikolov <nickysn@users.sourceforge.net>  }

{**
 * \file include/pcm.h
 * \brief Application interface library for the ALSA driver
 * \author Jaroslav Kysela <perex@perex.cz>
 * \author Abramo Bagnara <abramo@alsa-project.org>
 * \author Takashi Iwai <tiwai@suse.de>
 * \date 1998-2001
 *
 * Application interface library for the ALSA driver.
 * See the \ref pcm page for more details.
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

//#ifndef __ALSA_PCM_H
//#define __ALSA_PCM_H

//#ifdef __cplusplus
//extern "C" {
//#endif

(**
 *  \defgroup PCM PCM Interface
 *  See the \ref pcm page for more details.
 *  \{
 *)

{ dlsym version for interface entry callback }
//#define SND_PCM_DLSYM_VERSION		_dlsym_pcm_001

type
{ PCM generic info container }
  PPsnd_pcm_info_t = ^Psnd_pcm_info_t;
  Psnd_pcm_info_t = ^snd_pcm_info_t;
  snd_pcm_info_t = record
  end;

{** PCM hardware configuration space container
 *
 *  snd_pcm_hw_params_t is an opaque structure which contains a set of possible
 *  PCM hardware configurations. For example, a given instance might include a
 *  range of buffer sizes, a range of period sizes, and a set of several sample
 *  formats. Some subset of all possible combinations these sets may be valid,
 *  but not necessarily any combination will be valid.
 *
 *  When a parameter is set or restricted using a snd_pcm_hw_params_set*
 *  function, all of the other ranges will be updated to exclude as many
 *  impossible configurations as possible. Attempting to set a parameter
 *  outside of its acceptable range will result in the function failing
 *  and an error code being returned.
 *}
  PPsnd_pcm_hw_params_t = ^Psnd_pcm_hw_params_t;
  Psnd_pcm_hw_params_t = ^snd_pcm_hw_params_t;
  snd_pcm_hw_params_t = record
  end;

{ PCM software configuration container }
  PPsnd_pcm_sw_params_t = ^Psnd_pcm_sw_params_t;
  Psnd_pcm_sw_params_t = ^snd_pcm_sw_params_t;
  snd_pcm_sw_params_t = record
  end;
{ PCM status container }
  PPsnd_pcm_status_t = ^Psnd_pcm_status_t;
  Psnd_pcm_status_t = ^snd_pcm_status_t;
  snd_pcm_status_t = record
  end;
{ PCM access types mask }
  PPsnd_pcm_access_mask_t = ^Psnd_pcm_access_mask_t;
  Psnd_pcm_access_mask_t = ^snd_pcm_access_mask_t;
  snd_pcm_access_mask_t = record
  end;
{ PCM formats mask }
  PPsnd_pcm_format_mask_t = ^Psnd_pcm_format_mask_t;
  Psnd_pcm_format_mask_t = ^snd_pcm_format_mask_t;
  snd_pcm_format_mask_t = record
  end;
{ PCM subformats mask }
  PPsnd_pcm_subformat_mask_t = ^Psnd_pcm_subformat_mask_t;
  Psnd_pcm_subformat_mask_t = ^snd_pcm_subformat_mask_t;
  snd_pcm_subformat_mask_t = record
  end;

{ PCM class }
  snd_pcm_class_t = (
	{ standard device }

	SND_PCM_CLASS_GENERIC = 0,
	{ multichannel device }
	SND_PCM_CLASS_MULTI,
	{ software modem device }
	SND_PCM_CLASS_MODEM,
	{ digitizer device }
	SND_PCM_CLASS_DIGITIZER,
	SND_PCM_CLASS_LAST = SND_PCM_CLASS_DIGITIZER);

{ PCM subclass }
  snd_pcm_subclass_t = (
	{ subdevices are mixed together }
	SND_PCM_SUBCLASS_GENERIC_MIX = 0,
	{ multichannel subdevices are mixed together }
	SND_PCM_SUBCLASS_MULTI_MIX,
	SND_PCM_SUBCLASS_LAST = SND_PCM_SUBCLASS_MULTI_MIX);

{ PCM stream (direction) }
  Psnd_pcm_stream_t = ^snd_pcm_stream_t;
  snd_pcm_stream_t = (
	{ Playback stream }
	SND_PCM_STREAM_PLAYBACK = 0,
	{ Capture stream }
	SND_PCM_STREAM_CAPTURE,
	SND_PCM_STREAM_LAST = SND_PCM_STREAM_CAPTURE);

{ PCM access type }
  Psnd_pcm_access_t = ^snd_pcm_access_t;
  snd_pcm_access_t = (
	{ mmap access with simple interleaved channels }
	SND_PCM_ACCESS_MMAP_INTERLEAVED = 0,
	{ mmap access with simple non interleaved channels }
	SND_PCM_ACCESS_MMAP_NONINTERLEAVED,
	{ mmap access with complex placement }
	SND_PCM_ACCESS_MMAP_COMPLEX,
	{ snd_pcm_readi/snd_pcm_writei access }
	SND_PCM_ACCESS_RW_INTERLEAVED,
	{ snd_pcm_readn/snd_pcm_writen access }
	SND_PCM_ACCESS_RW_NONINTERLEAVED,
	SND_PCM_ACCESS_LAST = SND_PCM_ACCESS_RW_NONINTERLEAVED);

{ PCM sample format }
  Psnd_pcm_format_t = ^snd_pcm_format_t;
  snd_pcm_format_t = (
	{ Unknown }
	SND_PCM_FORMAT_UNKNOWN = -1,
	{ Signed 8 bit }
	SND_PCM_FORMAT_S8 = 0,
	{ Unsigned 8 bit }
	SND_PCM_FORMAT_U8,
	{ Signed 16 bit Little Endian }
	SND_PCM_FORMAT_S16_LE,
	{ Signed 16 bit Big Endian }
	SND_PCM_FORMAT_S16_BE,
	{ Unsigned 16 bit Little Endian }
	SND_PCM_FORMAT_U16_LE,
	{ Unsigned 16 bit Big Endian }
	SND_PCM_FORMAT_U16_BE,
	{ Signed 24 bit Little Endian using low three bytes in 32-bit word }
	SND_PCM_FORMAT_S24_LE,
	{ Signed 24 bit Big Endian using low three bytes in 32-bit word }
	SND_PCM_FORMAT_S24_BE,
	{ Unsigned 24 bit Little Endian using low three bytes in 32-bit word }
	SND_PCM_FORMAT_U24_LE,
	{ Unsigned 24 bit Big Endian using low three bytes in 32-bit word }
	SND_PCM_FORMAT_U24_BE,
	{ Signed 32 bit Little Endian }
	SND_PCM_FORMAT_S32_LE,
	{ Signed 32 bit Big Endian }
	SND_PCM_FORMAT_S32_BE,
	{ Unsigned 32 bit Little Endian }
	SND_PCM_FORMAT_U32_LE,
	{ Unsigned 32 bit Big Endian }
	SND_PCM_FORMAT_U32_BE,
	{ Float 32 bit Little Endian, Range -1.0 to 1.0 }
	SND_PCM_FORMAT_FLOAT_LE,
	{ Float 32 bit Big Endian, Range -1.0 to 1.0 }
	SND_PCM_FORMAT_FLOAT_BE,
	{ Float 64 bit Little Endian, Range -1.0 to 1.0 }
	SND_PCM_FORMAT_FLOAT64_LE,
	{ Float 64 bit Big Endian, Range -1.0 to 1.0 }
	SND_PCM_FORMAT_FLOAT64_BE,
	{ IEC-958 Little Endian }
	SND_PCM_FORMAT_IEC958_SUBFRAME_LE,
	{ IEC-958 Big Endian }
	SND_PCM_FORMAT_IEC958_SUBFRAME_BE,
	{ Mu-Law }
	SND_PCM_FORMAT_MU_LAW,
	{ A-Law }
	SND_PCM_FORMAT_A_LAW,
	{ Ima-ADPCM }
	SND_PCM_FORMAT_IMA_ADPCM,
	{ MPEG }
	SND_PCM_FORMAT_MPEG,
	{ GSM }
	SND_PCM_FORMAT_GSM,
	{ Special }
	SND_PCM_FORMAT_SPECIAL = 31,
	{ Signed 24bit Little Endian in 3bytes format }
	SND_PCM_FORMAT_S24_3LE = 32,
	{ Signed 24bit Big Endian in 3bytes format }
	SND_PCM_FORMAT_S24_3BE,
	{ Unsigned 24bit Little Endian in 3bytes format }
	SND_PCM_FORMAT_U24_3LE,
	{ Unsigned 24bit Big Endian in 3bytes format }
	SND_PCM_FORMAT_U24_3BE,
	{ Signed 20bit Little Endian in 3bytes format }
	SND_PCM_FORMAT_S20_3LE,
	{ Signed 20bit Big Endian in 3bytes format }
	SND_PCM_FORMAT_S20_3BE,
	{ Unsigned 20bit Little Endian in 3bytes format }
	SND_PCM_FORMAT_U20_3LE,
	{ Unsigned 20bit Big Endian in 3bytes format }
	SND_PCM_FORMAT_U20_3BE,
	{ Signed 18bit Little Endian in 3bytes format }
	SND_PCM_FORMAT_S18_3LE,
	{ Signed 18bit Big Endian in 3bytes format }
	SND_PCM_FORMAT_S18_3BE,
	{ Unsigned 18bit Little Endian in 3bytes format }
	SND_PCM_FORMAT_U18_3LE,
	{ Unsigned 18bit Big Endian in 3bytes format }
	SND_PCM_FORMAT_U18_3BE,
        { G.723 (ADPCM) 24 kbit/s, 8 samples in 3 bytes }
        SND_PCM_FORMAT_G723_24,
        { G.723 (ADPCM) 24 kbit/s, 1 sample in 1 byte }
        SND_PCM_FORMAT_G723_24_1B,
        { G.723 (ADPCM) 40 kbit/s, 8 samples in 3 bytes }
        SND_PCM_FORMAT_G723_40,
        { G.723 (ADPCM) 40 kbit/s, 1 sample in 1 byte }
        SND_PCM_FORMAT_G723_40_1B,
        { Direct Stream Digital (DSD) in 1-byte samples (x8) }
        SND_PCM_FORMAT_DSD_U8,
        { Direct Stream Digital (DSD) in 2-byte samples (x16) }
        SND_PCM_FORMAT_DSD_U16_LE,
        { Direct Stream Digital (DSD) in 4-byte samples (x32) }
        SND_PCM_FORMAT_DSD_U32_LE,
        { Direct Stream Digital (DSD) in 2-byte samples (x16) }
        SND_PCM_FORMAT_DSD_U16_BE,
        { Direct Stream Digital (DSD) in 4-byte samples (x32) }
        SND_PCM_FORMAT_DSD_U32_BE,
        SND_PCM_FORMAT_LAST = SND_PCM_FORMAT_DSD_U32_BE,


{$IFDEF FPC_LITTLE_ENDIAN}
	{ Signed 16 bit CPU endian }
	SND_PCM_FORMAT_S16 = SND_PCM_FORMAT_S16_LE,
	{ Unsigned 16 bit CPU endian }
	SND_PCM_FORMAT_U16 = SND_PCM_FORMAT_U16_LE,
	{ Signed 24 bit CPU endian }
	SND_PCM_FORMAT_S24 = SND_PCM_FORMAT_S24_LE,
	{ Unsigned 24 bit CPU endian }
	SND_PCM_FORMAT_U24 = SND_PCM_FORMAT_U24_LE,
	{ Signed 32 bit CPU endian }
	SND_PCM_FORMAT_S32 = SND_PCM_FORMAT_S32_LE,
	{ Unsigned 32 bit CPU endian }
	SND_PCM_FORMAT_U32 = SND_PCM_FORMAT_U32_LE,
	{ Float 32 bit CPU endian }
	SND_PCM_FORMAT_FLOAT = SND_PCM_FORMAT_FLOAT_LE,
	{ Float 64 bit CPU endian }
	SND_PCM_FORMAT_FLOAT64 = SND_PCM_FORMAT_FLOAT64_LE,
	{ IEC-958 CPU Endian }
	SND_PCM_FORMAT_IEC958_SUBFRAME = SND_PCM_FORMAT_IEC958_SUBFRAME_LE
{$ELSE FPC_LITTLE_ENDIAN}
{$IFDEF FPC_BIG_ENDIAN}
	{ Signed 16 bit CPU endian }
	SND_PCM_FORMAT_S16 = SND_PCM_FORMAT_S16_BE,
	{ Unsigned 16 bit CPU endian }
	SND_PCM_FORMAT_U16 = SND_PCM_FORMAT_U16_BE,
	{ Signed 24 bit CPU endian }
	SND_PCM_FORMAT_S24 = SND_PCM_FORMAT_S24_BE,
	{ Unsigned 24 bit CPU endian }
	SND_PCM_FORMAT_U24 = SND_PCM_FORMAT_U24_BE,
	{ Signed 32 bit CPU endian }
	SND_PCM_FORMAT_S32 = SND_PCM_FORMAT_S32_BE,
	{ Unsigned 32 bit CPU endian }
	SND_PCM_FORMAT_U32 = SND_PCM_FORMAT_U32_BE,
	{ Float 32 bit CPU endian }
	SND_PCM_FORMAT_FLOAT = SND_PCM_FORMAT_FLOAT_BE,
	{ Float 64 bit CPU endian }
	SND_PCM_FORMAT_FLOAT64 = SND_PCM_FORMAT_FLOAT64_BE,
	{ IEC-958 CPU Endian }
	SND_PCM_FORMAT_IEC958_SUBFRAME = SND_PCM_FORMAT_IEC958_SUBFRAME_BE
{$ELSE FPC_BIG_ENDIAN}
{$ERROR Unknown endian}
{$ENDIF FPC_BIG_ENDIAN}
{$ENDIF FPC_LITTLE_ENDIAN}
);

{ PCM sample subformat }
  Psnd_pcm_subformat_t = ^snd_pcm_subformat_t;
  snd_pcm_subformat_t = (
	{ Standard }
	SND_PCM_SUBFORMAT_STD = 0,
	SND_PCM_SUBFORMAT_LAST = SND_PCM_SUBFORMAT_STD);

{ PCM state }
  snd_pcm_state_t = (
	{ Open }
	SND_PCM_STATE_OPEN = 0,
	{ Setup installed }
	SND_PCM_STATE_SETUP,
	{ Ready to start }
	SND_PCM_STATE_PREPARED,
	{ Running }
	SND_PCM_STATE_RUNNING,
	{ Stopped: underrun (playback) or overrun (capture) detected }
	SND_PCM_STATE_XRUN,
	{ Draining: running (playback) or stopped (capture) }
	SND_PCM_STATE_DRAINING,
	{ Paused }
	SND_PCM_STATE_PAUSED,
	{ Hardware is suspended }
	SND_PCM_STATE_SUSPENDED,
	{ Hardware is disconnected }
	SND_PCM_STATE_DISCONNECTED,
	SND_PCM_STATE_LAST = SND_PCM_STATE_DISCONNECTED);

{ PCM start mode }
  snd_pcm_start_t = (
	{ Automatic start on data read/write }
	SND_PCM_START_DATA = 0,
	{ Explicit start }
	SND_PCM_START_EXPLICIT,
	SND_PCM_START_LAST = SND_PCM_START_EXPLICIT);

{ PCM xrun mode }
  snd_pcm_xrun_t = (
	{ Xrun detection disabled }
	SND_PCM_XRUN_NONE = 0,
	{ Stop on xrun detection }
	SND_PCM_XRUN_STOP,
	SND_PCM_XRUN_LAST = SND_PCM_XRUN_STOP);

{ PCM timestamp mode }
  Psnd_pcm_tstamp_t = ^snd_pcm_tstamp_t;
  snd_pcm_tstamp_t = (
	{ No timestamp }
	SND_PCM_TSTAMP_NONE = 0,
	{ Update timestamp at every hardware position update }
	SND_PCM_TSTAMP_ENABLE,
	{** Equivalent with #SND_PCM_TSTAMP_ENABLE,
	 * just for compatibility with older versions
	 *}
	SND_PCM_TSTAMP_MMAP = SND_PCM_TSTAMP_ENABLE,
	SND_PCM_TSTAMP_LAST = SND_PCM_TSTAMP_ENABLE);

  Psnd_pcm_tstamp_type_t = ^snd_pcm_tstamp_type_t;
  snd_pcm_tstamp_type_t = (
	SND_PCM_TSTAMP_TYPE_GETTIMEOFDAY = 0,   { gettimeofday equivalent }
	SND_PCM_TSTAMP_TYPE_MONOTONIC,  { posix_clock_monotonic equivalent }
	SND_PCM_TSTAMP_TYPE_MONOTONIC_RAW,      { monotonic_raw (no NTP) }
	SND_PCM_TSTAMP_TYPE_LAST = SND_PCM_TSTAMP_TYPE_MONOTONIC_RAW);

  Psnd_pcm_audio_tstamp_config_t = ^snd_pcm_audio_tstamp_config_t;
  snd_pcm_audio_tstamp_config_t = bitpacked record
	{ 5 of max 16 bits used }
	type_requested: 0..15;
	report_delay: 0..1; { add total delay to A/D or D/A }
  end;

  Psnd_pcm_audio_tstamp_report_t = ^snd_pcm_audio_tstamp_report_t;
  snd_pcm_audio_tstamp_report_t = bitpacked record
	{ 6 of max 16 bits used for bit-fields }

	{ for backwards compatibility }
	valid: 0..1;

	{ actual type if hardware could not support requested timestamp }
	actual_type: 0..15;

	{ accuracy represented in ns units }
	accuracy_report: 0..1; { 0 if accuracy unknown, 1 if accuracy field is valid }
	accuracy: cuint; { up to 4.29s, will be packed in separate field  }
  end;

{ Unsigned frames quantity }
  Psnd_pcm_uframes_t = ^snd_pcm_uframes_t;
  snd_pcm_uframes_t = culong;
{ Signed frames quantity }
  Psnd_pcm_sframes_t = ^snd_pcm_sframes_t;
  snd_pcm_sframes_t = clong;

const
{ Non blocking mode (flag for open mode) }
  SND_PCM_NONBLOCK         = $00000001;
{ Async notification (flag for open mode) }
  SND_PCM_ASYNC            = $00000002;
{ In an abort state (internal, not allowed for open) }
{$INFO SND_PCM_ABORT const renamed to SND_PCM_ABORT_STATE to avoid clash with the snd_pcm_abort function, since pascal is not case sensitive }
  SND_PCM_ABORT_STATE      = $00008000;
{ Disable automatic (but not forced!) rate resamplinig }
  SND_PCM_NO_AUTO_RESAMPLE = $00010000;
{ Disable automatic (but not forced!) channel conversion }
  SND_PCM_NO_AUTO_CHANNELS = $00020000;
{ Disable automatic (but not forced!) format conversion }
  SND_PCM_NO_AUTO_FORMAT   = $00040000;
{ Disable soft volume control }
  SND_PCM_NO_SOFTVOL       = $00080000;

type
{ PCM handle }
  PPsnd_pcm_t = ^Psnd_pcm_t;
  Psnd_pcm_t = ^snd_pcm_t;
  snd_pcm_t = record
  end;

{ PCM type }
  snd_pcm_type_t = (
	{ Kernel level PCM }
	SND_PCM_TYPE_HW = 0,
	{ Hooked PCM }
	SND_PCM_TYPE_HOOKS,
	{ One or more linked PCM with exclusive access to selected
	  channels }
	SND_PCM_TYPE_MULTI,
	{ File writing plugin }
	SND_PCM_TYPE_FILE,
	{ Null endpoint PCM }
	SND_PCM_TYPE_NULL,
	{ Shared memory client PCM }
	SND_PCM_TYPE_SHM,
	{ INET client PCM (not yet implemented) }
	SND_PCM_TYPE_INET,
	{ Copying plugin }
	SND_PCM_TYPE_COPY,
	{ Linear format conversion PCM }
	SND_PCM_TYPE_LINEAR,
	{ A-Law format conversion PCM }
	SND_PCM_TYPE_ALAW,
	{ Mu-Law format conversion PCM }
	SND_PCM_TYPE_MULAW,
	{ IMA-ADPCM format conversion PCM }
	SND_PCM_TYPE_ADPCM,
	{ Rate conversion PCM }
	SND_PCM_TYPE_RATE,
	{ Attenuated static route PCM }
	SND_PCM_TYPE_ROUTE,
	{ Format adjusted PCM }
	SND_PCM_TYPE_PLUG,
	{ Sharing PCM }
	SND_PCM_TYPE_SHARE,
	{ Meter plugin }
	SND_PCM_TYPE_METER,
	{ Mixing PCM }
	SND_PCM_TYPE_MIX,
	{ Attenuated dynamic route PCM (not yet implemented) }
	SND_PCM_TYPE_DROUTE,
	{ Loopback server plugin (not yet implemented) }
	SND_PCM_TYPE_LBSERVER,
	{ Linear Integer <-> Linear Float format conversion PCM }
	SND_PCM_TYPE_LINEAR_FLOAT,
	{ LADSPA integration plugin }
	SND_PCM_TYPE_LADSPA,
	{ Direct Mixing plugin }
	SND_PCM_TYPE_DMIX,
	{ Jack Audio Connection Kit plugin }
	SND_PCM_TYPE_JACK,
	{ Direct Snooping plugin }
	SND_PCM_TYPE_DSNOOP,
	{ Direct Sharing plugin }
	SND_PCM_TYPE_DSHARE,
	{ IEC958 subframe plugin }
	SND_PCM_TYPE_IEC958,
	{ Soft volume plugin }
	SND_PCM_TYPE_SOFTVOL,
	{ external I/O plugin }
	SND_PCM_TYPE_IOPLUG,
	{ external filter plugin }
	SND_PCM_TYPE_EXTPLUG,
	{ Mmap-emulation plugin }
	SND_PCM_TYPE_MMAP_EMUL,
	SND_PCM_TYPE_LAST = SND_PCM_TYPE_MMAP_EMUL);

{ PCM area specification }
  PPsnd_pcm_channel_area_t = ^Psnd_pcm_channel_area_t;
  Psnd_pcm_channel_area_t = ^snd_pcm_channel_area_t;
  snd_pcm_channel_area_t = record
	{ base address of channel samples }
	addr: Pointer;
	{ offset to first sample in bits }
	first: cuint;
	{ samples distance in bits }
	step: cuint;
  end;

{ PCM synchronization ID }
  snd_pcm_sync_id_t = record
    case Integer of
    0: (
          { 8-bit ID }
          id: array [0..15] of cuchar;
	);
    1: (
          { 16-bit ID }
          id16: array [0..7] of cushort;
	);
    2: (
          { 32-bit ID }
          id32: array [0..3] of cuint;
	);
  end;

{ #SND_PCM_TYPE_METER scope handle }
  PPsnd_pcm_scope_t = ^Psnd_pcm_scope_t;
  Psnd_pcm_scope_t = ^snd_pcm_scope_t;
  snd_pcm_scope_t = record
  end;

function snd_pcm_open(pcm: PPsnd_pcm_t; name: PChar;
		 stream: snd_pcm_stream_t; mode: cint): cint; cdecl; external libasound;
function snd_pcm_open_lconf(pcm: PPsnd_pcm_t; name: PChar;
		       stream: snd_pcm_stream_t; mode: cint;
		       lconf: Psnd_config_t): cint; cdecl; external libasound;
function snd_pcm_open_fallback(pcm: PPsnd_pcm_t; root: Psnd_config_t;
                         name, orig_name: PChar;
                         stream: snd_pcm_stream_t; mode: cint): cint; cdecl; external libasound;

function snd_pcm_close(pcm: Psnd_pcm_t): cint; cdecl; external libasound;
function snd_pcm_name(pcm: Psnd_pcm_t): PChar; cdecl; external libasound;
function snd_pcm_type(pcm: Psnd_pcm_t): snd_pcm_type_t; cdecl; external libasound;
function snd_pcm_stream(pcm: Psnd_pcm_t): snd_pcm_stream_t; cdecl; external libasound;
function snd_pcm_poll_descriptors_count(pcm: Psnd_pcm_t): cint; cdecl; external libasound;
{$INFO pollfd* ???}
function snd_pcm_poll_descriptors(pcm: Psnd_pcm_t; pfds: {Pfppollfd}Pointer; space: cuint): cint; cdecl; external libasound;
{$INFO pollfd* ???}
function snd_pcm_poll_descriptors_revents(pcm: Psnd_pcm_t; pfds: {Pfppollfd}Pointer; nfds: cuint; revents: Pcushort): cint; cdecl; external libasound;
{$INFO snd_pcm_nonblock() renamed to snd_pcm_set_nonblock, to avoid name clash, since pascal isn't case sensitive}
function snd_pcm_set_nonblock(pcm: Psnd_pcm_t; nonblock: cint): cint; cdecl; external libasound Name 'snd_pcm_nonblock';
function snd_pcm_abort(pcm: Psnd_pcm_t): cint; inline;
function snd_async_add_pcm_handler(handler: PPsnd_async_handler_t; pcm: Psnd_pcm_t;
			      callback: snd_async_callback_t; private_data: Pointer): cint; cdecl; external libasound;
function snd_async_handler_get_pcm(handler: Psnd_async_handler_t): Psnd_pcm_t; cdecl; external libasound;
function snd_pcm_info(pcm: Psnd_pcm_t; info: Psnd_pcm_info_t): cint; cdecl; external libasound;
function snd_pcm_hw_params_current(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t): cint; cdecl; external libasound;
function snd_pcm_hw_params(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t): cint; cdecl; external libasound;
function snd_pcm_hw_free(pcm: Psnd_pcm_t): cint; cdecl; external libasound;
function snd_pcm_sw_params_current(pcm: Psnd_pcm_t; params: Psnd_pcm_sw_params_t): cint; cdecl; external libasound;
function snd_pcm_sw_params(pcm: Psnd_pcm_t; params: Psnd_pcm_sw_params_t): cint; cdecl; external libasound;
function snd_pcm_prepare(pcm: Psnd_pcm_t): cint; cdecl; external libasound;
function snd_pcm_reset(pcm: Psnd_pcm_t): cint; cdecl; external libasound;
function snd_pcm_status(pcm: Psnd_pcm_t; status: Psnd_pcm_status_t): cint; cdecl; external libasound;
function snd_pcm_start(pcm: Psnd_pcm_t): cint; cdecl; external libasound;
function snd_pcm_drop(pcm: Psnd_pcm_t): cint; cdecl; external libasound;
function snd_pcm_drain(pcm: Psnd_pcm_t): cint; cdecl; external libasound;
function snd_pcm_pause(pcm: Psnd_pcm_t; enable: cint): cint; cdecl; external libasound;
function snd_pcm_state(pcm: Psnd_pcm_t): snd_pcm_state_t; cdecl; external libasound;
function snd_pcm_hwsync(pcm: Psnd_pcm_t): cint; cdecl; external libasound;
function snd_pcm_delay(pcm: Psnd_pcm_t; delayp: Psnd_pcm_sframes_t): cint; cdecl; external libasound;
function snd_pcm_resume(pcm: Psnd_pcm_t): cint; cdecl; external libasound;
function snd_pcm_htimestamp(pcm: Psnd_pcm_t; avail: Psnd_pcm_uframes_t; tstamp: Psnd_htimestamp_t): cint; cdecl; external libasound;
function snd_pcm_avail(pcm: Psnd_pcm_t): snd_pcm_sframes_t; cdecl; external libasound;
function snd_pcm_avail_update(pcm: Psnd_pcm_t): snd_pcm_sframes_t; cdecl; external libasound;
function snd_pcm_avail_delay(pcm: Psnd_pcm_t; availp: Psnd_pcm_sframes_t; delayp: Psnd_pcm_sframes_t): cint; cdecl; external libasound;
function snd_pcm_rewindable(pcm: Psnd_pcm_t): snd_pcm_sframes_t; cdecl; external libasound;
function snd_pcm_rewind(pcm: Psnd_pcm_t; frames: snd_pcm_uframes_t): snd_pcm_sframes_t; cdecl; external libasound;
function snd_pcm_forwardable(pcm: Psnd_pcm_t): snd_pcm_sframes_t; cdecl; external libasound;
function snd_pcm_forward(pcm: Psnd_pcm_t; frames: snd_pcm_uframes_t): snd_pcm_sframes_t; cdecl; external libasound;
function snd_pcm_writei(pcm: Psnd_pcm_t; buffer: Pointer; size: snd_pcm_uframes_t): snd_pcm_sframes_t; cdecl; external libasound;
function snd_pcm_readi(pcm: Psnd_pcm_t; buffer: Pointer; size: snd_pcm_uframes_t): snd_pcm_sframes_t; cdecl; external libasound;
function snd_pcm_writen(pcm: Psnd_pcm_t; bufs: PPointer; size: snd_pcm_uframes_t): snd_pcm_sframes_t; cdecl; external libasound;
function snd_pcm_readn(pcm: Psnd_pcm_t; bufs: PPointer; size: snd_pcm_uframes_t): snd_pcm_sframes_t; cdecl; external libasound;
function snd_pcm_wait(pcm: Psnd_pcm_t; timeout: cint): cint; cdecl; external libasound;

function snd_pcm_link(pcm1, pcm2: Psnd_pcm_t): cint; cdecl; external libasound;
function snd_pcm_unlink(pcm: Psnd_pcm_t): cint; cdecl; external libasound;

{ channel mapping API version number }
const
  SND_CHMAP_API_VERSION = (1 shl 16) or (0 shl 8) or 1;

{ channel map list type }
type
  snd_pcm_chmap_type = (
       SND_CHMAP_TYPE_NONE = 0,{ unspecified channel position }
       SND_CHMAP_TYPE_FIXED,   { fixed channel position }
       SND_CHMAP_TYPE_VAR,     { freely swappable channel position }
       SND_CHMAP_TYPE_PAIRED,  { pair-wise swappable channel position }
       SND_CHMAP_TYPE_LAST = SND_CHMAP_TYPE_PAIRED  { last entry }
);

{ channel positions }
  snd_pcm_chmap_position = (
       SND_CHMAP_UNKNOWN = 0,  { unspecified }
       SND_CHMAP_NA,           { N/A, silent }
       SND_CHMAP_MONO,         { mono stream }
       SND_CHMAP_FL,           { front left }
       SND_CHMAP_FR,           { front right }
       SND_CHMAP_RL,           { rear left }
       SND_CHMAP_RR,           { rear right }
       SND_CHMAP_FC,           { front center }
       SND_CHMAP_LFE,          { LFE }
       SND_CHMAP_SL,           { side left }
       SND_CHMAP_SR,           { side right }
       SND_CHMAP_RC,           { rear center }
       SND_CHMAP_FLC,          { front left center }
       SND_CHMAP_FRC,          { front right center }
       SND_CHMAP_RLC,          { rear left center }
       SND_CHMAP_RRC,          { rear right center }
       SND_CHMAP_FLW,          { front left wide }
       SND_CHMAP_FRW,          { front right wide }
       SND_CHMAP_FLH,          { front left high }
       SND_CHMAP_FCH,          { front center high }
       SND_CHMAP_FRH,          { front right high }
       SND_CHMAP_TC,           { top center }
       SND_CHMAP_TFL,          { top front left }
       SND_CHMAP_TFR,          { top front right }
       SND_CHMAP_TFC,          { top front center }
       SND_CHMAP_TRL,          { top rear left }
       SND_CHMAP_TRR,          { top rear right }
       SND_CHMAP_TRC,          { top rear center }
       SND_CHMAP_TFLC,         { top front left center }
       SND_CHMAP_TFRC,         { top front right center }
       SND_CHMAP_TSL,          { top side left }
       SND_CHMAP_TSR,          { top side right }
       SND_CHMAP_LLFE,         { left LFE }
       SND_CHMAP_RLFE,         { right LFE }
       SND_CHMAP_BC,           { bottom center }
       SND_CHMAP_BLC,          { bottom left center }
       SND_CHMAP_BRC,          { bottom right center }
       SND_CHMAP_LAST = SND_CHMAP_BRC
);

{ bitmask for channel position }
const
  SND_CHMAP_POSITION_MASK                = $ffff;

{ bit flag indicating the channel is phase inverted }
  SND_CHMAP_PHASE_INVERSE                = $01 shl 16;
{ bit flag indicating the non-standard channel value }
  SND_CHMAP_DRIVER_SPEC                  = $02 shl 16;

{ the channel map header }
type
  PPsnd_pcm_chmap_t = ^Psnd_pcm_chmap_t;
  Psnd_pcm_chmap_t = ^snd_pcm_chmap_t;
  snd_pcm_chmap_t = record
       channels: cuint;        { number of channels }
       pos: array [0..0] of cuint;    { channel position array }
  end;

{ the header of array items returned from snd_pcm_query_chmaps() }
  PPsnd_pcm_chmap_query_t = ^Psnd_pcm_chmap_query_t;
  Psnd_pcm_chmap_query_t = ^snd_pcm_chmap_query_t;
  snd_pcm_chmap_query_t = record
       _type: snd_pcm_chmap_type;   { channel map type }
       map: snd_pcm_chmap_t;        { available channel map }
  end;


function snd_pcm_query_chmaps(pcm: Psnd_pcm_t): PPsnd_pcm_chmap_query_t; cdecl; external libasound;
function snd_pcm_query_chmaps_from_hw(card: cint; dev: cint;
                                      subdev: cint;
                                      stream: snd_pcm_stream_t): PPsnd_pcm_chmap_query_t; cdecl; external libasound;
procedure snd_pcm_free_chmaps(maps: PPsnd_pcm_chmap_query_t); cdecl; external libasound;
function snd_pcm_get_chmap(pcm: Psnd_pcm_t): Psnd_pcm_chmap_t; cdecl; external libasound;
function snd_pcm_set_chmap(pcm: Psnd_pcm_t; map: Psnd_pcm_chmap_t): cint; cdecl; external libasound;

function snd_pcm_chmap_type_name(val: snd_pcm_chmap_type): PChar; cdecl; external libasound;
function snd_pcm_chmap_name(val: snd_pcm_chmap_position): PChar; cdecl; external libasound;
function snd_pcm_chmap_long_name(val: snd_pcm_chmap_position): PChar; cdecl; external libasound;
function snd_pcm_chmap_print(map: Psnd_pcm_chmap_t; maxlen: size_t; buf: PChar): cint; cdecl; external libasound;
function snd_pcm_chmap_from_string(str: PChar): cuint; cdecl; external libasound;
function snd_pcm_chmap_parse_string(str: PChar): Psnd_pcm_chmap_t; cdecl; external libasound;

//int snd_pcm_mixer_element(snd_pcm_t *pcm, snd_mixer_t *mixer, snd_mixer_elem_t **elem);

(*
 * application helpers - these functions are implemented on top
 * of the basic API
 *)

function snd_pcm_recover(pcm: Psnd_pcm_t; err, silent: cint): cint; cdecl; external libasound;
function snd_pcm_set_params(pcm: Psnd_pcm_t;
                       format: snd_pcm_format_t;
                       access: snd_pcm_access_t;
                       channels,
                       rate: cuint;
                       soft_resample: cint;
                       latency: cuint): cint; cdecl; external libasound;
function snd_pcm_get_params(pcm: Psnd_pcm_t;
                       buffer_size,
                       period_size: Psnd_pcm_uframes_t): cint; cdecl; external libasound;

(** \} *)

(**
 * \defgroup PCM_Info Stream Information
 * \ingroup PCM
 * See the \ref pcm page for more details.
 * \{
 *)

function snd_pcm_info_sizeof: size_t; cdecl; external libasound;
(** \hideinitializer
 * \brief allocate an invalid #snd_pcm_info_t using standard alloca
 * \param ptr returned pointer
 *)
//#define snd_pcm_info_alloca(ptr) __snd_alloca(ptr, snd_pcm_info)
function snd_pcm_info_malloc(ptr: PPsnd_pcm_info_t): cint; cdecl; external libasound;
procedure snd_pcm_info_free(obj: Psnd_pcm_info_t); cdecl; external libasound;
procedure snd_pcm_info_copy(dst: Psnd_pcm_info_t; src: Psnd_pcm_info_t); cdecl; external libasound;
function snd_pcm_info_get_device(obj: Psnd_pcm_info_t): cuint; cdecl; external libasound;
function snd_pcm_info_get_subdevice(obj: Psnd_pcm_info_t): cuint; cdecl; external libasound;
function snd_pcm_info_get_stream(obj: Psnd_pcm_info_t): snd_pcm_stream_t; cdecl; external libasound;
function snd_pcm_info_get_card(obj: Psnd_pcm_info_t): cint; cdecl; external libasound;
function snd_pcm_info_get_id(obj: Psnd_pcm_info_t): PChar; cdecl; external libasound;
function snd_pcm_info_get_name(obj: Psnd_pcm_info_t): PChar; cdecl; external libasound;
function snd_pcm_info_get_subdevice_name(obj: Psnd_pcm_info_t): PChar; cdecl; external libasound;
function snd_pcm_info_get_class(obj: Psnd_pcm_info_t): snd_pcm_class_t; cdecl; external libasound;
function snd_pcm_info_get_subclass(obj: Psnd_pcm_info_t): snd_pcm_subclass_t; cdecl; external libasound;
function snd_pcm_info_get_subdevices_count(obj: Psnd_pcm_info_t): cuint; cdecl; external libasound;
function snd_pcm_info_get_subdevices_avail(obj: Psnd_pcm_info_t): cuint; cdecl; external libasound;
function snd_pcm_info_get_sync(obj: Psnd_pcm_info_t): snd_pcm_sync_id_t; cdecl; external libasound;
procedure snd_pcm_info_set_device(obj: Psnd_pcm_info_t; val: cuint); cdecl; external libasound;
procedure snd_pcm_info_set_subdevice(obj: Psnd_pcm_info_t; val: cuint); cdecl; external libasound;
procedure snd_pcm_info_set_stream(obj: Psnd_pcm_info_t; val: snd_pcm_stream_t); cdecl; external libasound;

(** \} *)

(**
 * \defgroup PCM_HW_Params Hardware Parameters
 * \ingroup PCM
 * See the \ref pcm page for more details.
 * \{
 *)

function snd_pcm_hw_params_any(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t): cint; cdecl; external libasound;

function snd_pcm_hw_params_can_mmap_sample_resolution(params: Psnd_pcm_hw_params_t): cint; cdecl; external libasound;
function snd_pcm_hw_params_is_double(params: Psnd_pcm_hw_params_t): cint; cdecl; external libasound;
function snd_pcm_hw_params_is_batch(params: Psnd_pcm_hw_params_t): cint; cdecl; external libasound;
function snd_pcm_hw_params_is_block_transfer(params: Psnd_pcm_hw_params_t): cint; cdecl; external libasound;
function snd_pcm_hw_params_is_monotonic(params: Psnd_pcm_hw_params_t): cint; cdecl; external libasound;
function snd_pcm_hw_params_can_overrange(params: Psnd_pcm_hw_params_t): cint; cdecl; external libasound;
function snd_pcm_hw_params_can_pause(params: Psnd_pcm_hw_params_t): cint; cdecl; external libasound;
function snd_pcm_hw_params_can_resume(params: Psnd_pcm_hw_params_t): cint; cdecl; external libasound;
function snd_pcm_hw_params_is_half_duplex(params: Psnd_pcm_hw_params_t): cint; cdecl; external libasound;
function snd_pcm_hw_params_is_joint_duplex(params: Psnd_pcm_hw_params_t): cint; cdecl; external libasound;
function snd_pcm_hw_params_can_sync_start(params: Psnd_pcm_hw_params_t): cint; cdecl; external libasound;
function snd_pcm_hw_params_can_disable_period_wakeup(params: Psnd_pcm_hw_params_t): cint; cdecl; external libasound;
function snd_pcm_hw_params_supports_audio_wallclock_ts(params: Psnd_pcm_hw_params_t): cint; cdecl; external libasound; { deprecated, use audio_ts_type }
function snd_pcm_hw_params_supports_audio_ts_type(params: Psnd_pcm_hw_params_t; _type: cint): cint; cdecl; external libasound;
function snd_pcm_hw_params_get_rate_numden(params: Psnd_pcm_hw_params_t;
				           rate_num,
				           rate_den: Pcuint): cint; cdecl; external libasound;
function snd_pcm_hw_params_get_sbits(params: Psnd_pcm_hw_params_t): cint; cdecl; external libasound;
function snd_pcm_hw_params_get_fifo_size(params: Psnd_pcm_hw_params_t): cint; cdecl; external libasound;

(*#if 0
typedef struct _snd_pcm_hw_strategy snd_pcm_hw_strategy_t;

/* choices need to be sorted on ascending badness */
typedef struct _snd_pcm_hw_strategy_simple_choices_list {
	unsigned int value;
	unsigned int badness;
} snd_pcm_hw_strategy_simple_choices_list_t;

int snd_pcm_hw_params_strategy(snd_pcm_t *pcm, snd_pcm_hw_params_t *params,
			       const snd_pcm_hw_strategy_t *strategy,
			       unsigned int badness_min,
			       unsigned int badness_max);

void snd_pcm_hw_strategy_free(snd_pcm_hw_strategy_t *strategy);
int snd_pcm_hw_strategy_simple(snd_pcm_hw_strategy_t **strategyp,
			       unsigned int badness_min,
			       unsigned int badness_max);
int snd_pcm_hw_params_try_explain_failure(snd_pcm_t *pcm,
					  snd_pcm_hw_params_t *fail,
					  snd_pcm_hw_params_t *success,
					  unsigned int depth,
					  snd_output_t *out);

#endif*)

function snd_pcm_hw_params_sizeof: size_t; cdecl; external libasound;
(** \hideinitializer
 * \brief allocate an invalid #snd_pcm_hw_params_t using standard alloca
 * \param ptr returned pointer
 *)
//#define snd_pcm_hw_params_alloca(ptr) __snd_alloca(ptr, snd_pcm_hw_params)
function snd_pcm_hw_params_malloc(ptr: PPsnd_pcm_hw_params_t): cint; cdecl; external libasound;
procedure snd_pcm_hw_params_free(obj: Psnd_pcm_hw_params_t); cdecl; external libasound;
procedure snd_pcm_hw_params_copy(dst: Psnd_pcm_hw_params_t; src: Psnd_pcm_hw_params_t); cdecl; external libasound;

//#if !defined(ALSA_LIBRARY_BUILD) && !defined(ALSA_PCM_OLD_HW_PARAMS_API)
{$IFNDEF ALSA_PCM_OLD_HW_PARAMS_API}

function snd_pcm_hw_params_get_access(params: Psnd_pcm_hw_params_t; _access: Psnd_pcm_access_t): cint; cdecl; external libasound;
function snd_pcm_hw_params_test_access(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; _access: Psnd_pcm_access_t): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_access(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; _access: snd_pcm_access_t): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_access_first(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; _access: Psnd_pcm_access_t): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_access_last(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; _access: Psnd_pcm_access_t): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_access_mask(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; mask: Psnd_pcm_access_mask_t): cint; cdecl; external libasound;
function snd_pcm_hw_params_get_access_mask(params: Psnd_pcm_hw_params_t; mask: Psnd_pcm_access_mask_t): cint; cdecl; external libasound;

function snd_pcm_hw_params_get_format(params: Psnd_pcm_hw_params_t; val: Psnd_pcm_format_t): cint; cdecl; external libasound;
function snd_pcm_hw_params_test_format(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; val: snd_pcm_format_t): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_format(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; val: snd_pcm_format_t): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_format_first(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; format: Psnd_pcm_format_t): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_format_last(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; format: Psnd_pcm_format_t): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_format_mask(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; mask: Psnd_pcm_format_mask_t): cint; cdecl; external libasound;
procedure snd_pcm_hw_params_get_format_mask(params: Psnd_pcm_hw_params_t; mask: Psnd_pcm_format_mask_t); cdecl; external libasound;

function snd_pcm_hw_params_get_subformat(params: Psnd_pcm_hw_params_t; subformat: Psnd_pcm_subformat_t): cint; cdecl; external libasound;
function snd_pcm_hw_params_test_subformat(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; subformat: snd_pcm_subformat_t): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_subformat(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; subformat: snd_pcm_subformat_t): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_subformat_first(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; subformat: Psnd_pcm_subformat_t): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_subformat_last(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; subformat: Psnd_pcm_subformat_t): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_subformat_mask(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; mask: Psnd_pcm_subformat_mask_t): cint; cdecl; external libasound;
procedure snd_pcm_hw_params_get_subformat_mask(params: Psnd_pcm_hw_params_t; mask: Psnd_pcm_subformat_mask_t); cdecl; external libasound;

function snd_pcm_hw_params_get_channels(params: Psnd_pcm_hw_params_t; val: Pcuint): cint; cdecl; external libasound;
function snd_pcm_hw_params_get_channels_min(params: Psnd_pcm_hw_params_t; val: PCuint): cint; cdecl; external libasound;
function snd_pcm_hw_params_get_channels_max(params: Psnd_pcm_hw_params_t; val: PCuint): cint; cdecl; external libasound;
function snd_pcm_hw_params_test_channels(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; val: cuint): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_channels(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; val: cuint): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_channels_min(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; val: Pcuint): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_channels_max(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; val: Pcuint): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_channels_minmax(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; min, max: Pcuint): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_channels_near(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; val: Pcuint): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_channels_first(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; val: Pcuint): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_channels_last(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; val: Pcuint): cint; cdecl; external libasound;

function snd_pcm_hw_params_get_rate(params: Psnd_pcm_hw_params_t; val: Pcuint; dir: Pcint): cint; cdecl; external libasound;
function snd_pcm_hw_params_get_rate_min(params: Psnd_pcm_hw_params_t; val: Pcuint; dir: Pcint): cint; cdecl; external libasound;
function snd_pcm_hw_params_get_rate_max(params: Psnd_pcm_hw_params_t; val: Pcuint; dir: Pcint): cint; cdecl; external libasound;
function snd_pcm_hw_params_test_rate(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; val: cuint; dir: cint): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_rate(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; val: cuint; dir: cint): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_rate_min(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; val: Pcuint; dir: Pcint): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_rate_max(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; val: Pcuint; dir: Pcint): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_rate_minmax(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; min: Pcuint; mindir: Pcint; max: Pcuint; maxdir: Pcint): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_rate_near(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; val: Pcuint; dir: Pcint): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_rate_first(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; val: Pcuint; dir: Pcint): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_rate_last(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; val: Pcuint; dir: Pcint): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_rate_resample(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; val: cuint): cint; cdecl; external libasound;
function snd_pcm_hw_params_get_rate_resample(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; val: Pcuint): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_export_buffer(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; val: cuint): cint; cdecl; external libasound;
function snd_pcm_hw_params_get_export_buffer(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; val: Pcuint): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_period_wakeup(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; val: cuint): cint; cdecl; external libasound;
function snd_pcm_hw_params_get_period_wakeup(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; val: Pcuint): cint; cdecl; external libasound;

function snd_pcm_hw_params_get_period_time(params: Psnd_pcm_hw_params_t; val: Pcuint; dir: Pcint): cint; cdecl; external libasound;
function snd_pcm_hw_params_get_period_time_min(params: Psnd_pcm_hw_params_t; val: Pcuint; dir: Pcint): cint; cdecl; external libasound;
function snd_pcm_hw_params_get_period_time_max(params: Psnd_pcm_hw_params_t; val: Pcuint; dir: Pcint): cint; cdecl; external libasound;
function snd_pcm_hw_params_test_period_time(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; val: cuint; dir: cint): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_period_time(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; val: cuint; dir: cint): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_period_time_min(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; val: Pcuint; dir: Pcint): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_period_time_max(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; val: Pcuint; dir: Pcint): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_period_time_minmax(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; min: Pcuint; mindir: Pcint; max: Pcuint; maxdir: Pcint): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_period_time_near(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; val: Pcuint; dir: Pcint): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_period_time_first(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; val: Pcuint; dir: Pcint): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_period_time_last(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; val: Pcuint; dir: Pcint): cint; cdecl; external libasound;

function snd_pcm_hw_params_get_period_size(params: Psnd_pcm_hw_params_t; frames: Psnd_pcm_uframes_t; dir: Pcint): cint; cdecl; external libasound;
function snd_pcm_hw_params_get_period_size_min(params: Psnd_pcm_hw_params_t; frames: Psnd_pcm_uframes_t; dir: Pcint): cint; cdecl; external libasound;
function snd_pcm_hw_params_get_period_size_max(params: Psnd_pcm_hw_params_t; frames: Psnd_pcm_uframes_t; dir: Pcint): cint; cdecl; external libasound;
function snd_pcm_hw_params_test_period_size(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; val: snd_pcm_uframes_t; dir: cint): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_period_size(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; val: snd_pcm_uframes_t; dir: cint): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_period_size_min(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; val: Psnd_pcm_uframes_t; dir: Pcint): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_period_size_max(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; val: Psnd_pcm_uframes_t; dir: Pcint): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_period_size_minmax(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; min: Psnd_pcm_uframes_t; mindir: Pcint; max: Psnd_pcm_uframes_t; maxdir: Pcint): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_period_size_near(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; val: Psnd_pcm_uframes_t; dir: Pcint): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_period_size_first(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; val: Psnd_pcm_uframes_t; dir: Pcint): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_period_size_last(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; val: Psnd_pcm_uframes_t; dir: Pcint): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_period_size_integer(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t): cint; cdecl; external libasound;

function snd_pcm_hw_params_get_periods(params: Psnd_pcm_hw_params_t; val: Pcuint; dir: Pcint): cint; cdecl; external libasound;
function snd_pcm_hw_params_get_periods_min(params: Psnd_pcm_hw_params_t; val: Pcuint; dir: Pcint): cint; cdecl; external libasound;
function snd_pcm_hw_params_get_periods_max(params: Psnd_pcm_hw_params_t; val: Pcuint; dir: Pcint): cint; cdecl; external libasound;
function snd_pcm_hw_params_test_periods(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; val: cuint; dir: cint): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_periods(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; val: cuint; dir: cint): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_periods_min(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; val: Pcuint; dir: Pcint): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_periods_max(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; val: Pcuint; dir: Pcint): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_periods_minmax(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; min: Pcuint; mindir: Pcint; max: Pcuint; maxdir: Pcint): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_periods_near(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; val: Pcuint; dir: Pcint): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_periods_first(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; val: Pcuint; dir: Pcint): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_periods_last(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; val: Pcuint; dir: Pcint): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_periods_integer(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t): cint; cdecl; external libasound;

function snd_pcm_hw_params_get_buffer_time(params: Psnd_pcm_hw_params_t; val: Pcuint; dir: Pcint): cint; cdecl; external libasound;
function snd_pcm_hw_params_get_buffer_time_min(params: Psnd_pcm_hw_params_t; val: Pcuint; dir: Pcint): cint; cdecl; external libasound;
function snd_pcm_hw_params_get_buffer_time_max(params: Psnd_pcm_hw_params_t; val: Pcuint; dir: Pcint): cint; cdecl; external libasound;
function snd_pcm_hw_params_test_buffer_time(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; val: cuint; dir: cint): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_buffer_time(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; val: cuint; dir: cint): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_buffer_time_min(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; val: Pcuint; dir: Pcint): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_buffer_time_max(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; val: Pcuint; dir: Pcint): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_buffer_time_minmax(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; min: Pcuint; mindir: Pcint; max: Pcuint; maxdir: Pcint): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_buffer_time_near(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; val: Pcuint; dir: Pcint): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_buffer_time_first(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; val: Pcuint; dir: Pcint): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_buffer_time_last(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; val: Pcuint; dir: Pcint): cint; cdecl; external libasound;

function snd_pcm_hw_params_get_buffer_size(params: Psnd_pcm_hw_params_t; val: Psnd_pcm_uframes_t): cint; cdecl; external libasound;
function snd_pcm_hw_params_get_buffer_size_min(params: Psnd_pcm_hw_params_t; val: Psnd_pcm_uframes_t): cint; cdecl; external libasound;
function snd_pcm_hw_params_get_buffer_size_max(params: Psnd_pcm_hw_params_t; val: Psnd_pcm_uframes_t): cint; cdecl; external libasound;
function snd_pcm_hw_params_test_buffer_size(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; val: snd_pcm_uframes_t): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_buffer_size(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; val: snd_pcm_uframes_t): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_buffer_size_min(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; val: Psnd_pcm_uframes_t): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_buffer_size_max(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; val: Psnd_pcm_uframes_t): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_buffer_size_minmax(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; min: Psnd_pcm_uframes_t; max: Psnd_pcm_uframes_t): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_buffer_size_near(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; val: Psnd_pcm_uframes_t): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_buffer_size_first(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; val: Psnd_pcm_uframes_t): cint; cdecl; external libasound;
function snd_pcm_hw_params_set_buffer_size_last(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; val: Psnd_pcm_uframes_t): cint; cdecl; external libasound;

//#endif /* !ALSA_LIBRARY_BUILD && !ALSA_PCM_OLD_HW_PARAMS_API */
{$ENDIF ALSA_PCM_OLD_HW_PARAMS_API}

function snd_pcm_hw_params_get_min_align(params: Psnd_pcm_hw_params_t; val: Psnd_pcm_uframes_t): cint; cdecl; external libasound;

(** \} *)

(**
 * \defgroup PCM_SW_Params Software Parameters
 * \ingroup PCM
 * See the \ref pcm page for more details.
 * \{
 *)

function snd_pcm_sw_params_sizeof: size_t; cdecl; external libasound;
(** \hideinitializer
 * \brief allocate an invalid #snd_pcm_sw_params_t using standard alloca
 * \param ptr returned pointer
 *)
//#define snd_pcm_sw_params_alloca(ptr) __snd_alloca(ptr, snd_pcm_sw_params)
function snd_pcm_sw_params_malloc(ptr: PPsnd_pcm_sw_params_t): cint; cdecl; external libasound;
procedure snd_pcm_sw_params_free(obj: Psnd_pcm_sw_params_t); cdecl; external libasound;
procedure snd_pcm_sw_params_copy(dst: Psnd_pcm_sw_params_t; src: Psnd_pcm_sw_params_t); cdecl; external libasound;
function snd_pcm_sw_params_get_boundary(params: Psnd_pcm_sw_params_t; val: Psnd_pcm_uframes_t): cint; cdecl; external libasound;

//#if !defined(ALSA_LIBRARY_BUILD) && !defined(ALSA_PCM_OLD_SW_PARAMS_API)
{$IFNDEF ALSA_PCM_OLD_SW_PARAMS_API}

function snd_pcm_sw_params_set_tstamp_mode(pcm: Psnd_pcm_t; params: Psnd_pcm_sw_params_t; val: snd_pcm_tstamp_t): cint; cdecl; external libasound;
function snd_pcm_sw_params_get_tstamp_mode(params: Psnd_pcm_sw_params_t; val: Psnd_pcm_tstamp_t): cint; cdecl; external libasound;
function snd_pcm_sw_params_set_tstamp_type(pcm: Psnd_pcm_t; params: Psnd_pcm_sw_params_t; val: snd_pcm_tstamp_type_t): cint; cdecl; external libasound;
function snd_pcm_sw_params_get_tstamp_type(params: Psnd_pcm_sw_params_t; val: Psnd_pcm_tstamp_type_t): cint; cdecl; external libasound;
function snd_pcm_sw_params_set_avail_min(pcm: Psnd_pcm_t; params: Psnd_pcm_sw_params_t; val: snd_pcm_uframes_t): cint; cdecl; external libasound;
function snd_pcm_sw_params_get_avail_min(params: Psnd_pcm_sw_params_t; val: Psnd_pcm_uframes_t): cint; cdecl; external libasound;
function snd_pcm_sw_params_set_period_event(pcm: Psnd_pcm_t; params: Psnd_pcm_sw_params_t; val: cint): cint; cdecl; external libasound;
function snd_pcm_sw_params_get_period_event(params: Psnd_pcm_sw_params_t; val: Pcint): cint; cdecl; external libasound;
function snd_pcm_sw_params_set_start_threshold(pcm: Psnd_pcm_t; params: Psnd_pcm_sw_params_t; val: snd_pcm_uframes_t): cint; cdecl; external libasound;
function snd_pcm_sw_params_get_start_threshold(paramsm: Psnd_pcm_sw_params_t; val: Psnd_pcm_uframes_t): cint; cdecl; external libasound;
function snd_pcm_sw_params_set_stop_threshold(pcm: Psnd_pcm_t; params: Psnd_pcm_sw_params_t; val: snd_pcm_uframes_t): cint; cdecl; external libasound;
function snd_pcm_sw_params_get_stop_threshold(params: Psnd_pcm_sw_params_t; val: Psnd_pcm_uframes_t): cint; cdecl; external libasound;
function snd_pcm_sw_params_set_silence_threshold(pcm: Psnd_pcm_t; params: Psnd_pcm_sw_params_t; val: snd_pcm_uframes_t): cint; cdecl; external libasound;
function snd_pcm_sw_params_get_silence_threshold(params: Psnd_pcm_sw_params_t; val: Psnd_pcm_uframes_t): cint; cdecl; external libasound;
function snd_pcm_sw_params_set_silence_size(pcm: Psnd_pcm_t; params: Psnd_pcm_sw_params_t; val: snd_pcm_uframes_t): cint; cdecl; external libasound;
function snd_pcm_sw_params_get_silence_size(params: Psnd_pcm_sw_params_t; val: Psnd_pcm_uframes_t): cint; cdecl; external libasound;

//#endif /* !ALSA_LIBRARY_BUILD && !ALSA_PCM_OLD_SW_PARAMS_API */
{$ENDIF ALSA_PCM_OLD_SW_PARAMS_API}

(** \} *)

(* include old API *)
//#ifndef ALSA_LIBRARY_BUILD
//#if defined(ALSA_PCM_OLD_HW_PARAMS_API) || defined(ALSA_PCM_OLD_SW_PARAMS_API)
{$IF defined(ALSA_PCM_OLD_HW_PARAMS_API) or defined(ALSA_PCM_OLD_SW_PARAMS_API)}
//#include "pcm_old.h"
{$INCLUDE pcm_old.inc}
//#endif
{$ENDIF defined(ALSA_PCM_OLD_HW_PARAMS_API) or defined(ALSA_PCM_OLD_SW_PARAMS_API)}
//#endif

(**
 * \defgroup PCM_Access Access Mask Functions
 * \ingroup PCM
 * See the \ref pcm page for more details.
 * \{
 *)

function snd_pcm_access_mask_sizeof: size_t; cdecl; external libasound;
(** \hideinitializer
 * \brief allocate an empty #snd_pcm_access_mask_t using standard alloca
 * \param ptr returned pointer
 *)
//#define snd_pcm_access_mask_alloca(ptr) __snd_alloca(ptr, snd_pcm_access_mask)
function snd_pcm_access_mask_malloc(ptr: PPsnd_pcm_access_mask_t): cint; cdecl; external libasound;
procedure snd_pcm_access_mask_free(obj: Psnd_pcm_access_mask_t); cdecl; external libasound;
procedure snd_pcm_access_mask_copy(dst: Psnd_pcm_access_mask_t; src: Psnd_pcm_access_mask_t); cdecl; external libasound;
procedure snd_pcm_access_mask_none(mask: Psnd_pcm_access_mask_t); cdecl; external libasound;
procedure snd_pcm_access_mask_any(mask: Psnd_pcm_access_mask_t); cdecl; external libasound;
function snd_pcm_access_mask_test(mask: Psnd_pcm_access_mask_t; val: snd_pcm_access_t): cint; cdecl; external libasound;
function snd_pcm_access_mask_empty(mask: Psnd_pcm_access_mask_t): cint; cdecl; external libasound;
procedure snd_pcm_access_mask_set(mask: Psnd_pcm_access_mask_t; val: snd_pcm_access_t); cdecl; external libasound;
procedure snd_pcm_access_mask_reset(mask: Psnd_pcm_access_mask_t; val: snd_pcm_access_t); cdecl; external libasound;

(** \} *)

(**
 * \defgroup PCM_Format Format Mask Functions
 * \ingroup PCM
 * See the \ref pcm page for more details.
 * \{
 *)

function snd_pcm_format_mask_sizeof: size_t; cdecl; external libasound;
(** \hideinitializer
 * \brief allocate an empty #snd_pcm_format_mask_t using standard alloca
 * \param ptr returned pointer
 *)
//#define snd_pcm_format_mask_alloca(ptr) __snd_alloca(ptr, snd_pcm_format_mask)
function snd_pcm_format_mask_malloc(ptr: PPsnd_pcm_format_mask_t): cint; cdecl; external libasound;
procedure snd_pcm_format_mask_free(obj: Psnd_pcm_format_mask_t); cdecl; external libasound;
procedure snd_pcm_format_mask_copy(dst: Psnd_pcm_format_mask_t; src: Psnd_pcm_format_mask_t); cdecl; external libasound;
procedure snd_pcm_format_mask_none(mask: Psnd_pcm_format_mask_t); cdecl; external libasound;
procedure snd_pcm_format_mask_any(mask: Psnd_pcm_format_mask_t); cdecl; external libasound;
function snd_pcm_format_mask_test(mask: Psnd_pcm_format_mask_t; val: snd_pcm_format_t): cint; cdecl; external libasound;
function snd_pcm_format_mask_empty(mask: Psnd_pcm_format_mask_t): cint; cdecl; external libasound;
procedure snd_pcm_format_mask_set(mask: Psnd_pcm_format_mask_t; val: snd_pcm_format_t); cdecl; external libasound;
procedure snd_pcm_format_mask_reset(mask: Psnd_pcm_format_mask_t; val: snd_pcm_format_t); cdecl; external libasound;

(** \} *)

(**
 * \defgroup PCM_SubFormat Subformat Mask Functions
 * \ingroup PCM
 * See the \ref pcm page for more details.
 * \{
 *)

function snd_pcm_subformat_mask_sizeof: size_t; cdecl; external libasound;
(** \hideinitializer
 * \brief allocate an empty #snd_pcm_subformat_mask_t using standard alloca
 * \param ptr returned pointer
 *)
//#define snd_pcm_subformat_mask_alloca(ptr) __snd_alloca(ptr, snd_pcm_subformat_mask)
function snd_pcm_subformat_mask_malloc(ptr: PPsnd_pcm_subformat_mask_t): cint; cdecl; external libasound;
procedure snd_pcm_subformat_mask_free(obj: Psnd_pcm_subformat_mask_t); cdecl; external libasound;
procedure snd_pcm_subformat_mask_copy(dst: Psnd_pcm_subformat_mask_t; src: Psnd_pcm_subformat_mask_t); cdecl; external libasound;
procedure snd_pcm_subformat_mask_none(mask: Psnd_pcm_subformat_mask_t); cdecl; external libasound;
procedure snd_pcm_subformat_mask_any(mask: Psnd_pcm_subformat_mask_t); cdecl; external libasound;
function snd_pcm_subformat_mask_test(mask: Psnd_pcm_subformat_mask_t; val: snd_pcm_subformat_t): cint; cdecl; external libasound;
function snd_pcm_subformat_mask_empty(mask: Psnd_pcm_subformat_mask_t): cint; cdecl; external libasound;
procedure snd_pcm_subformat_mask_set(mask: Psnd_pcm_subformat_mask_t; val: snd_pcm_subformat_t); cdecl; external libasound;
procedure snd_pcm_subformat_mask_reset(mask: Psnd_pcm_subformat_mask_t; val: snd_pcm_subformat_t); cdecl; external libasound;

(** \} *)

(**
 * \defgroup PCM_Status Status Functions
 * \ingroup PCM
 * See the \ref pcm page for more details.
 * \{
 *)

function snd_pcm_status_sizeof: size_t; cdecl; external libasound;
(** \hideinitializer
 * \brief allocate an invalid #snd_pcm_status_t using standard alloca
 * \param ptr returned pointer
 *)
//#define snd_pcm_status_alloca(ptr) __snd_alloca(ptr, snd_pcm_status)
function snd_pcm_status_malloc(ptr: PPsnd_pcm_status_t): cint; cdecl; external libasound;
procedure snd_pcm_status_free(obj: Psnd_pcm_status_t); cdecl; external libasound;
procedure snd_pcm_status_copy(dst: Psnd_pcm_status_t; src: Psnd_pcm_status_t); cdecl; external libasound;
function snd_pcm_status_get_state(obj: Psnd_pcm_status_t): snd_pcm_state_t; cdecl; external libasound;
procedure snd_pcm_status_get_trigger_tstamp(obj: Psnd_pcm_status_t; ptr: Psnd_timestamp_t); cdecl; external libasound;
procedure snd_pcm_status_get_trigger_htstamp(obj: Psnd_pcm_status_t; ptr: Psnd_htimestamp_t); cdecl; external libasound;
procedure snd_pcm_status_get_tstamp(obj: Psnd_pcm_status_t; ptr: Psnd_timestamp_t); cdecl; external libasound;
procedure snd_pcm_status_get_htstamp(obj: Psnd_pcm_status_t; ptr: Psnd_htimestamp_t); cdecl; external libasound;
procedure snd_pcm_status_get_audio_htstamp(obj: Psnd_pcm_status_t; ptr: Psnd_htimestamp_t); cdecl; external libasound;
procedure snd_pcm_status_get_driver_htstamp(obj: Psnd_pcm_status_t; ptr: Psnd_htimestamp_t); cdecl; external libasound;
procedure snd_pcm_status_get_audio_htstamp_report(obj: Psnd_pcm_status_t;
     audio_tstamp_report: Psnd_pcm_audio_tstamp_report_t); cdecl; external libasound;
procedure snd_pcm_status_set_audio_htstamp_config(obj: Psnd_pcm_status_t;
     audio_tstamp_config: Psnd_pcm_audio_tstamp_config_t); cdecl; external libasound;
procedure snd_pcm_pack_audio_tstamp_config(data: Pcuint;
                        config: Psnd_pcm_audio_tstamp_config_t); inline;
procedure snd_pcm_unpack_audio_tstamp_report(data: cuint; accuracy: cuint;
                        report: Psnd_pcm_audio_tstamp_report_t); inline;
function snd_pcm_status_get_delay(obj: Psnd_pcm_status_t): snd_pcm_sframes_t; cdecl; external libasound;
function snd_pcm_status_get_avail(obj: Psnd_pcm_status_t): snd_pcm_uframes_t; cdecl; external libasound;
function snd_pcm_status_get_avail_max(obj: Psnd_pcm_status_t): snd_pcm_uframes_t; cdecl; external libasound;
function snd_pcm_status_get_overrange(obj: Psnd_pcm_status_t): snd_pcm_uframes_t; cdecl; external libasound;

(** \} *)

(**
 * \defgroup PCM_Description Description Functions
 * \ingroup PCM
 * See the \ref pcm page for more details.
 * \{
 *)

function snd_pcm_type_name(_type: snd_pcm_type_t): PChar; cdecl; external libasound;
function snd_pcm_stream_name(stream: snd_pcm_stream_t): PChar; cdecl; external libasound;
function snd_pcm_access_name(_access: snd_pcm_access_t): PChar; cdecl; external libasound;
function snd_pcm_format_name(format: snd_pcm_format_t): PChar; cdecl; external libasound;
function snd_pcm_format_description(format: snd_pcm_format_t): PChar; cdecl; external libasound;
function snd_pcm_subformat_name(subformat: snd_pcm_subformat_t): PChar; cdecl; external libasound;
function snd_pcm_subformat_description(subformat: snd_pcm_subformat_t): PChar; cdecl; external libasound;
function snd_pcm_format_value(name: PChar): snd_pcm_format_t; cdecl; external libasound;
function snd_pcm_tstamp_mode_name(mode: snd_pcm_tstamp_t): PChar; cdecl; external libasound;
function snd_pcm_state_name(state: snd_pcm_state_t): PChar; cdecl; external libasound;

(** \} *)

(**
 * \defgroup PCM_Dump Debug Functions
 * \ingroup PCM
 * See the \ref pcm page for more details.
 * \{
 *)

function snd_pcm_dump(pcm: Psnd_pcm_t; _out: Psnd_output_t): cint; cdecl; external libasound;
function snd_pcm_dump_hw_setup(pcm: Psnd_pcm_t; _out: Psnd_output_t): cint; cdecl; external libasound;
function snd_pcm_dump_sw_setup(pcm: Psnd_pcm_t; _out: Psnd_output_t): cint; cdecl; external libasound;
function snd_pcm_dump_setup(pcm: Psnd_pcm_t; _out: Psnd_output_t): cint; cdecl; external libasound;
function snd_pcm_hw_params_dump(params: Psnd_pcm_hw_params_t; _out: Psnd_output_t): cint; cdecl; external libasound;
function snd_pcm_sw_params_dump(params: Psnd_pcm_sw_params_t; _out: Psnd_output_t): cint; cdecl; external libasound;
function snd_pcm_status_dump(status: Psnd_pcm_status_t; _out: Psnd_output_t): cint; cdecl; external libasound;

(** \} *)

(**
 * \defgroup PCM_Direct Direct Access (MMAP) Functions
 * \ingroup PCM
 * See the \ref pcm page for more details.
 * \{
 *)

function snd_pcm_mmap_begin(pcm: Psnd_pcm_t;
		            areas: PPsnd_pcm_channel_area_t;
		            offset: Psnd_pcm_uframes_t;
		            frames: Psnd_pcm_uframes_t): cint; cdecl; external libasound;
function snd_pcm_mmap_commit(pcm: Psnd_pcm_t;
			     offset: snd_pcm_uframes_t;
			     frames: snd_pcm_uframes_t): snd_pcm_sframes_t; cdecl; external libasound;
function snd_pcm_mmap_writei(pcm: Psnd_pcm_t; buffer: Pointer; size: snd_pcm_uframes_t): snd_pcm_sframes_t; cdecl; external libasound;
function snd_pcm_mmap_readi(pcm: Psnd_pcm_t; buffer: Pointer; size: snd_pcm_uframes_t): snd_pcm_sframes_t; cdecl; external libasound;
function snd_pcm_mmap_writen(pcm: Psnd_pcm_t; bufs: PPointer; size: snd_pcm_uframes_t): snd_pcm_sframes_t; cdecl; external libasound;
function snd_pcm_mmap_readn(pcm: Psnd_pcm_t; bufs: PPointer; size: snd_pcm_uframes_t): snd_pcm_sframes_t; cdecl; external libasound;

(** \} *)

(**
 * \defgroup PCM_Helpers Helper Functions
 * \ingroup PCM
 * See the \ref pcm page for more details.
 * \{
 *)

function snd_pcm_format_signed(format: snd_pcm_format_t): cint; cdecl; external libasound;
function snd_pcm_format_unsigned(format: snd_pcm_format_t): cint; cdecl; external libasound;
function snd_pcm_format_linear(format: snd_pcm_format_t): cint; cdecl; external libasound;
{$INFO snd_pcm_format_float() renamed to snd_pcm_format_is_float, to avoid name clash, since pascal isn't case sensitive}
function snd_pcm_format_is_float(format: snd_pcm_format_t): cint; cdecl; external libasound Name 'snd_pcm_format_float';
function snd_pcm_format_little_endian(format: snd_pcm_format_t): cint; cdecl; external libasound;
function snd_pcm_format_big_endian(format: snd_pcm_format_t): cint; cdecl; external libasound;
function snd_pcm_format_cpu_endian(format: snd_pcm_format_t): cint; cdecl; external libasound;
function snd_pcm_format_width(format: snd_pcm_format_t): cint; cdecl; external libasound;           { in bits }
function snd_pcm_format_physical_width(format: snd_pcm_format_t): cint; cdecl; external libasound;  { in bits }
function snd_pcm_build_linear_format(width, pwidth, unsignd, big_endian: cint): snd_pcm_format_t; cdecl; external libasound;
function snd_pcm_format_size(format: snd_pcm_format_t; samples: size_t): ssize_t; cdecl; external libasound;
function snd_pcm_format_silence(format: snd_pcm_format_t): cuint8; cdecl; external libasound;
function snd_pcm_format_silence_16(format: snd_pcm_format_t): cuint16; cdecl; external libasound;
function snd_pcm_format_silence_32(format: snd_pcm_format_t): cuint32; cdecl; external libasound;
function snd_pcm_format_silence_64(format: snd_pcm_format_t): cuint64; cdecl; external libasound;
function snd_pcm_format_set_silence(format: snd_pcm_format_t; buf: Pointer; samples: cuint): cint; cdecl; external libasound;

function snd_pcm_bytes_to_frames(pcm: Psnd_pcm_t; bytes: ssize_t): snd_pcm_sframes_t; cdecl; external libasound;
function snd_pcm_frames_to_bytes(pcm: Psnd_pcm_t; frames: snd_pcm_sframes_t): ssize_t; cdecl; external libasound;
function snd_pcm_bytes_to_samples(pcm: Psnd_pcm_t; bytes: ssize_t): clong; cdecl; external libasound;
function snd_pcm_samples_to_bytes(pcm: Psnd_pcm_t; samples: clong): ssize_t; cdecl; external libasound;

function snd_pcm_area_silence(dst_channel: Psnd_pcm_channel_area_t; dst_offset: snd_pcm_uframes_t;
                              samples: cuint; format: snd_pcm_format_t): cint; cdecl; external libasound;
function snd_pcm_areas_silence(dst_channels: Psnd_pcm_channel_area_t; dst_offset: snd_pcm_uframes_t;
                               channels: cuint; frames: snd_pcm_uframes_t; format: snd_pcm_format_t): cint; cdecl; external libasound;
function snd_pcm_area_copy(dst_channel: Psnd_pcm_channel_area_t; dst_offset: snd_pcm_uframes_t;
                           src_channel: Psnd_pcm_channel_area_t; src_offset: snd_pcm_uframes_t;
                           samples: cuint; format: snd_pcm_format_t): cint; cdecl; external libasound;
function snd_pcm_areas_copy(dst_channels: Psnd_pcm_channel_area_t; dst_offset: snd_pcm_uframes_t;
                            src_channels: Psnd_pcm_channel_area_t; src_offset: snd_pcm_uframes_t;
                            channels: cuint; frames: snd_pcm_uframes_t; format: snd_pcm_format_t): cint; cdecl; external libasound;

(** \} *)

(**
 * \defgroup PCM_Hook Hook Extension
 * \ingroup PCM
 * See the \ref pcm page for more details.
 * \{
 *)

type
{ type of pcm hook }
  snd_pcm_hook_type_t = (
	SND_PCM_HOOK_TYPE_HW_PARAMS = 0,
	SND_PCM_HOOK_TYPE_HW_FREE,
	SND_PCM_HOOK_TYPE_CLOSE,
	SND_PCM_HOOK_TYPE_LAST = SND_PCM_HOOK_TYPE_CLOSE);

{ PCM hook container }
  PPsnd_pcm_hook_t = ^Psnd_pcm_hook_t;
  Psnd_pcm_hook_t = ^snd_pcm_hook_t;
  snd_pcm_hook_t = record
  end;
{ PCM hook callback function }
  snd_pcm_hook_func_t = function(hook: Psnd_pcm_hook_t): cint; cdecl;

function snd_pcm_hook_get_pcm(hook: Psnd_pcm_hook_t): Psnd_pcm_t; cdecl; external libasound;
function snd_pcm_hook_get_private(hook: Psnd_pcm_hook_t): Pointer; cdecl; external libasound;
procedure snd_pcm_hook_set_private(hook: Psnd_pcm_hook_t; private_data: Pointer); cdecl; external libasound;
function snd_pcm_hook_add(hookp: PPsnd_pcm_hook_t; pcm: Psnd_pcm_t;
                          _type: snd_pcm_hook_type_t;
                          func: snd_pcm_hook_func_t; private_data: Pointer): cint; cdecl; external libasound;
function snd_pcm_hook_remove(hook: Psnd_pcm_hook_t): cint; cdecl; external libasound;

(** \} *)

(**
 * \defgroup PCM_Scope Scope Plugin Extension
 * \ingroup PCM
 * See the \ref pcm page for more details.
 * \{
 *)

type
{ #SND_PCM_TYPE_METER scope functions }
  Psnd_pcm_scope_ops_t = ^snd_pcm_scope_ops_t;
  snd_pcm_scope_ops_t = record
	(** \brief Enable and prepare it using current params
	 * \param scope scope handle
	 *)
	enable: function(scope: Psnd_pcm_scope_t): cint; cdecl;
	(** \brief Disable
	 * \param scope scope handle
	 *)
	disable: procedure(scope: Psnd_pcm_scope_t); cdecl;
	(** \brief PCM has been started
	 * \param scope scope handle
	 *)
	start: procedure(scope: Psnd_pcm_scope_t); cdecl;
	(** \brief PCM has been stopped
	 * \param scope scope handle
	 *)
	stop: procedure(scope: Psnd_pcm_scope_t); cdecl;
	(** \brief New frames are present
	 * \param scope scope handle
	 *)
	update: procedure(scope: Psnd_pcm_scope_t); cdecl;
	(** \brief Reset status
	 * \param scope scope handle
	 *)
	reset: procedure(scope: Psnd_pcm_scope_t); cdecl;
	(** \brief PCM is closing
	 * \param scope scope handle
	 *)
	close: procedure(scope: Psnd_pcm_scope_t); cdecl;
  end;

function snd_pcm_meter_get_bufsize(pcm: Psnd_pcm_t): snd_pcm_uframes_t; cdecl; external libasound;
function snd_pcm_meter_get_channels(pcm: Psnd_pcm_t): cuint; cdecl; external libasound;
function snd_pcm_meter_get_rate(pcm: Psnd_pcm_t): cuint; cdecl; external libasound;
function snd_pcm_meter_get_now(pcm: Psnd_pcm_t): snd_pcm_uframes_t; cdecl; external libasound;
function snd_pcm_meter_get_boundary(pcm: Psnd_pcm_t): snd_pcm_uframes_t; cdecl; external libasound;
function snd_pcm_meter_add_scope(pcm: Psnd_pcm_t; scope: Psnd_pcm_scope_t): cint; cdecl; external libasound;
function snd_pcm_meter_search_scope(pcm: Psnd_pcm_t; name: PChar): Psnd_pcm_scope_t; cdecl; external libasound;
function snd_pcm_scope_malloc(ptr: PPsnd_pcm_scope_t): cint; cdecl; external libasound;
procedure snd_pcm_scope_set_ops(scope: Psnd_pcm_scope_t; val: Psnd_pcm_scope_ops_t); cdecl; external libasound;
procedure snd_pcm_scope_set_name(scope: Psnd_pcm_scope_t; val: PChar); cdecl; external libasound;
function snd_pcm_scope_get_name(scope: Psnd_pcm_scope_t): PChar; cdecl; external libasound;
function snd_pcm_scope_get_callback_private(scope: Psnd_pcm_scope_t): Pointer; cdecl; external libasound;
procedure snd_pcm_scope_set_callback_private(scope: Psnd_pcm_scope_t; val: Pointer); cdecl; external libasound;
function snd_pcm_scope_s16_open(pcm: Psnd_pcm_t; name: PChar;
                                scopep: PPsnd_pcm_scope_t): cint; cdecl; external libasound;
function snd_pcm_scope_s16_get_channel_buffer(scope: Psnd_pcm_scope_t;
                                              channel: cuint): Pcint16; cdecl; external libasound;

(** \} *)

(**
 * \defgroup PCM_Simple Simple setup functions
 * \ingroup PCM
 * See the \ref pcm page for more details.
 * \{
 *)

type
{ Simple PCM latency type }
  snd_spcm_latency_t = (
	{ standard latency - for standard playback or capture
          (estimated latency in one direction 350ms) }
	SND_SPCM_LATENCY_STANDARD = 0,
	{ medium latency - software phones etc.
	  (estimated latency in one direction maximally 25ms }
	SND_SPCM_LATENCY_MEDIUM,
	{ realtime latency - realtime applications (effect processors etc.)
	    (estimated latency in one direction 5ms and better) }
	SND_SPCM_LATENCY_REALTIME);

{ Simple PCM xrun type }
  snd_spcm_xrun_type_t = (
	{ driver / library will ignore all xruns, the stream runs forever }
	SND_SPCM_XRUN_IGNORE = 0,
	{ driver / library stops the stream when an xrun occurs }
	SND_SPCM_XRUN_STOP);

{ Simple PCM duplex type }
  snd_spcm_duplex_type_t = (
	{ liberal duplex - the buffer and period sizes might not match }
	SND_SPCM_DUPLEX_LIBERAL = 0,
	{ pedantic duplex - the buffer and period sizes MUST match }
	SND_SPCM_DUPLEX_PEDANTIC);

function snd_spcm_init(pcm: Psnd_pcm_t;
                       rate: cuint;
                       channels: cuint;
                       format: snd_pcm_format_t;
                       subformat: snd_pcm_subformat_t;
                       latency: snd_spcm_latency_t;
                       _access: snd_pcm_access_t;
                       xrun_type: snd_spcm_xrun_type_t): cint; cdecl; external libasound;

function snd_spcm_init_duplex(playback_pcm: Psnd_pcm_t;
                              capture_pcm: Psnd_pcm_t;
                              rate: cuint;
                              channels: cuint;
                              format: snd_pcm_format_t;
                              subformat: snd_pcm_subformat_t;
                              latency: snd_spcm_latency_t;
                              _access: snd_pcm_access_t;
                              xrun_type: snd_spcm_xrun_type_t;
                              duplex_type: snd_spcm_duplex_type_t): cint; cdecl; external libasound;

function snd_spcm_init_get_params(pcm: Psnd_pcm_t;
                                  rate: Pcuint;
                                  buffer_size: Psnd_pcm_uframes_t;
                                  period_size: Psnd_pcm_uframes_t): cint; cdecl; external libasound;

(** \} *)

(**
 * \defgroup PCM_Deprecated deprecated Functions
 * \ingroup PCM
 * See the \ref pcm page for more details.
 * \{
 *)

{ deprecated functions, for compatibility }
function snd_pcm_start_mode_name(mode: snd_pcm_start_t) {__attribute__((deprecated))}: PChar; cdecl; external libasound; deprecated;
function snd_pcm_xrun_mode_name(mode: snd_pcm_xrun_t): PChar; cdecl; external libasound; deprecated;
function snd_pcm_sw_params_set_start_mode(pcm: Psnd_pcm_t; params: Psnd_pcm_sw_params_t; val: snd_pcm_start_t): cint; cdecl; external libasound; deprecated;
function snd_pcm_sw_params_get_start_mode(params: Psnd_pcm_sw_params_t): snd_pcm_start_t; cdecl; external libasound; deprecated;
function snd_pcm_sw_params_set_xrun_mode(pcm: Psnd_pcm_t; params: Psnd_pcm_sw_params_t; val: snd_pcm_xrun_t): cint; cdecl; external libasound; deprecated;
function snd_pcm_sw_params_get_xrun_mode(params: Psnd_pcm_sw_params_t): snd_pcm_xrun_t; cdecl; external libasound; deprecated;
//#if !defined(ALSA_LIBRARY_BUILD) && !defined(ALSA_PCM_OLD_SW_PARAMS_API)
{$IFNDEF ALSA_PCM_OLD_SW_PARAMS_API}
function snd_pcm_sw_params_set_xfer_align(pcm: Psnd_pcm_t; params: Psnd_pcm_sw_params_t; val: snd_pcm_uframes_t): cint; cdecl; external libasound; deprecated;
function snd_pcm_sw_params_get_xfer_align(params: Psnd_pcm_sw_params_t; val: Psnd_pcm_uframes_t): cint; cdecl; external libasound; deprecated;
function snd_pcm_sw_params_set_sleep_min(pcm: Psnd_pcm_t; params: Psnd_pcm_sw_params_t; val: cuint): cint; cdecl; external libasound; deprecated;
function snd_pcm_sw_params_get_sleep_min(params: Psnd_pcm_sw_params_t; val: Pcuint): cint; cdecl; external libasound; deprecated;
{$ENDIF ALSA_PCM_OLD_SW_PARAMS_API}
{$IFNDEF ALSA_PCM_OLD_HW_PARAMS_API}
function snd_pcm_hw_params_get_tick_time(params: Psnd_pcm_hw_params_t; val: Pcuint; dir: Pcint): cint; cdecl; external libasound; deprecated;
function snd_pcm_hw_params_get_tick_time_min(params: Psnd_pcm_hw_params_t; val: Pcuint; dir: Pcint): cint; cdecl; external libasound; deprecated;
function snd_pcm_hw_params_get_tick_time_max(params: Psnd_pcm_hw_params_t; val: Pcuint; dir: Pcint): cint; cdecl; external libasound; deprecated;
function snd_pcm_hw_params_test_tick_time(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; val: cuint; dir: cint): cint; cdecl; external libasound; deprecated;
function snd_pcm_hw_params_set_tick_time(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; val: cuint; dir: cint): cint; cdecl; external libasound; deprecated;
function snd_pcm_hw_params_set_tick_time_min(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; val: Pcuint; dir: Pcint): cint; cdecl; external libasound; deprecated;
function snd_pcm_hw_params_set_tick_time_max(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; val: Pcuint; dir: Pcint): cint; cdecl; external libasound; deprecated;
function snd_pcm_hw_params_set_tick_time_minmax(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; min: Pcuint; mindir: Pcint; max: Pcuint; maxdir: Pcint): cint; cdecl; external libasound; deprecated;
function snd_pcm_hw_params_set_tick_time_near(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; val: Pcuint; dir: Pcint): cint; cdecl; external libasound; deprecated;
function snd_pcm_hw_params_set_tick_time_first(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; val: Pcuint; dir: Pcint): cint; cdecl; external libasound; deprecated;
function snd_pcm_hw_params_set_tick_time_last(pcm: Psnd_pcm_t; params: Psnd_pcm_hw_params_t; val: Pcuint; dir: Pcint): cint; cdecl; external libasound; deprecated;
//#endif /* !ALSA_LIBRARY_BUILD && !ALSA_PCM_OLD_HW_PARAMS_API */
{$ENDIF ALSA_PCM_OLD_HW_PARAMS_API}

(** \} *)

//#ifdef __cplusplus
//}
//#endif

//#endif /* __ALSA_PCM_H */
