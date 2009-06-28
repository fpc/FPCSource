(****************************************************************************
 *                                                          __              *
 *                ____ ___  ____ __  ______ ___  ____  ____/ /              *
 *               / __ `__ \/ __ `/ |/ / __ `__ \/ __ \/ __  /               *
 *              / / / / / / /_/ />  </ / / / / / /_/ / /_/ /                *
 *             /_/ /_/ /_/\__,_/_/|_/_/ /_/ /_/\____/\__,_/                 *
 *                                                                          *
 *                            ARM7 Definitions                              *
 *                                                                          *
 *         Copyright (c) 2008, Mukunda Johnson (mukunda@maxmod.org)         *
 *                                                                          *
 * Permission to use, copy, modify, and/or distribute this software for any *
 * purpose with or without fee is hereby granted, provided that the above   *
 * copyright notice and this permission notice appear in all copies.        *
 *                                                                          *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES *
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF         *
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR  *
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES   *
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN    *
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF  *
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.           *
 ****************************************************************************)

{$ifdef NDS_INTERFACE}
const
  // precalculated mix buffer lengths (in bytes)
  MM_MIXLEN_8KHZ    = 544;    // (8121 hz)
  MM_MIXLEN_10KHZ   = 704;    // (10512 hz)	
  MM_MIXLEN_13KHZ   = 896;    // (13379 hz)	
  MM_MIXLEN_16KHZ   = 1056;   // (15768 hz)
  MM_MIXLEN_18KHZ   = 1216;   // (18157 hz)
  MM_MIXLEN_21KHZ   = 1408;   // (21024 hz)
  MM_MIXLEN_27KHZ   = 1792;   // (26758 hz)
  MM_MIXLEN_31KHZ   = 2112;   // (31536 hz)
  
  // measurements of channel types (bytes)
  MM_SIZEOF_MODCH   = 40;
  MM_SIZEOF_ACTCH   = 28;
  MM_SIZEOF_MIXCH   = 24;
 
procedure mmInitDefault(soundbank:  mm_addr; number_of_channels: mm_word); cdecl; external;
procedure mmInit(var setup: mm_gba_system); cdecl; external;
procedure mmVBlank(); cdecl; external;
procedure mmSetVBlankHandler(func: pointer); cdecl; external;
procedure mmSetEventHandler(handler: mm_callback); cdecl; external; 
procedure mmFrame(); cdecl; external; //__attribute((long_call)) ???
procedure mmStart(id: mm_word; mode: mm_pmode); cdecl; external;
procedure mmPause(); cdecl; external;
procedure mmResume(); cdecl; external;
procedure mmStop(); cdecl; external;
procedure mmPosition(position: mm_word); cdecl; external;
function mmActive(): cint; cdecl; external;
procedure mmJingle(module_ID: mm_word); cdecl; external;
function mmActiveSub(): cint; cdecl; external;
procedure mmSetModuleVolume(volume: mm_word); cdecl; external;
procedure mmSetJingleVolume(volume: mm_word); cdecl; external;
procedure mmSetModuleTempo(tempo: mm_word); cdecl; external;
procedure mmSetModulePitch(pitch: mm_word); cdecl; external;
procedure mmPlayModule(address, mode, layer: mm_word); cdecl; external;
function mmEffect(sample_ID: mm_word): mm_sfxhand; cdecl; external;
function mmEffectEx(sound: pmm_sound_effect): mm_sfxhand; cdecl; external;
procedure mmEffectVolume(handle: mm_sfxhand; volume: mm_word); cdecl; external;
procedure mmEffectPanning(handle: mm_sfxhand; panning: mm_byte); cdecl; external;
procedure mmEffectRate(handle: mm_sfxhand; rate: mm_word); cdecl; external;
procedure mmEffectScaleRate(handle: mm_sfxhand; factor: mm_word); cdecl; external;
procedure mmEffectCancel(handle: mm_sfxhand); cdecl; external;
procedure mmEffectRelease(handle: mm_sfxhand); cdecl; external;
procedure mmSetEffectsVolume(volume: mm_word); cdecl; external;
procedure mmEffectCancelAll(); cdecl; external;

const 
  MMCB_SONGMESSAGE = $2A;
  MMCB_SONGFINISHED = $2B;

var
  mp_mix_seg: mm_byte; cvar; external;			// current mixing segment
  mp_writepos: mm_word; cvar; external;		// mixer's write position

{$endif NDS_INTERFACE}
