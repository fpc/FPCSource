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

{$ifdef ARM9}
  {$error maxmod7 is for ARM7!}
{$endif ARM9}


{$ifdef NDS_INTERFACE}

procedure mmInstall(fifo_channel: cint); cdecl; external;
procedure mmLockChannels(bitmask: mm_word); cdecl; external;
procedure mmUnlockChannels(bitmask: mm_word); cdecl; external;
function mmIsInitialized(): mm_bool; cdecl; external;
procedure mmSelectMode(mode: mm_mode_enum); cdecl; external;
procedure mmFrame(); cdecl; external;
procedure mmStart(module_ID: mm_word; mode: mm_pmode); cdecl; external;
procedure mmPause(); cdecl; external;
procedure mmResume(); cdecl; external;
procedure mmStop(); cdecl; external;
procedure mmPosition(position: mm_word); cdecl; external;
function mmActive(): mm_bool; cdecl; external;
procedure mmJingle(module_ID: mm_word); cdecl; external;
function mmActiveSub(): mm_bool; cdecl; external;
procedure mmSetModuleVolume(volume: mm_word); cdecl; external;
procedure mmSetJingleVolume(volume:  mm_word); cdecl; external;
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

procedure mmStreamOpen(stream: pmm_stream; memory: mm_addr); cdecl; external;
procedure mmStreamUpdate(); cdecl; external;
procedure mmCloseStream(); cdecl; external;

procedure mmReverbEnable(); cdecl; external;
procedure mmReverbConfigure(config: pmm_reverb_cfg); cdecl; external;
procedure mmReverbStart(channels: mm_reverbch); cdecl; external;
procedure mmReverbStop(channels: mm_reverbch); cdecl; external;
function mmReverbBufferSize(bit_depth, sampling_rate, delay: mm_word): mm_word; inline;
procedure mmReverbDisable(); cdecl; external;

const
  MMCB_SONGMESSAGE  = $2A;
  MMCB_SONGFINISHED = $2B;

var
  mmLayerMain: mm_modlayer; cvar; external;
  mmLayerSub: mm_modlayer; cvar; external;
{$endif NDS_INTERFACE}



{$ifdef NDS_IMPLEMENTATION}
function mmReverbBufferSize(bit_depth, sampling_rate, delay: mm_word): mm_word; inline;
begin
  if bit_depth = 16 then
    result := ((((sampling_rate * delay * 2) div 1000) + 3) and (not 3)) div 4
  else
    result := ((((sampling_rate * delay) div 1000) + 3) and (not 3)) div 4;
end;
{$endif NDS_IMPLEMENTATION}
