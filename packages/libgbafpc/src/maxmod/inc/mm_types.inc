(****************************************************************************
 *                                                          __              *
 *                ____ ___  ____ __  ______ ___  ____  ____/ /              *
 *               / __ `__ \/ __ `/ |/ / __ `__ \/ __ \/ __  /               *
 *              / / / / / / /_/ />  </ / / / / / /_/ / /_/ /                *
 *             /_/ /_/ /_/\__,_/_/|_/_/ /_/ /_/\____/\__,_/                 *
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
type
  mm_word     = cuint;
  pmm_word    = ^mm_word;

  mm_hword    = cushort;
  pmm_hword   = ^mm_hword;

  mm_byte     = cuchar;
  pmm_byte    = ^mm_byte;

  mm_sfxhand  = cushort;
  pmm_sfxhand = ^mm_sfxhand;

  mm_bool     = cuchar;

  mm_addr     = pointer;
  mm_reg      = pointer;

type
  mm_mode_enum = integer;
const
  MM_MODE_A: mm_mode_enum = 0;
  MM_MODE_B: mm_mode_enum = 1;
  MM_MODE_C: mm_mode_enum = 2;

type
  mm_stream_formats = integer;
const
  MM_STREAM_8BIT_MONO   : mm_stream_formats = $0;
  MM_STREAM_8BIT_STEREO : mm_stream_formats = $1;
  MM_STREAM_16BIT_MONO  : mm_stream_formats = $2;
  MM_STREAM_16BIT_STEREO: mm_stream_formats = $3;


type
  mm_callback     = function  (msg, param: mm_word): mm_word;
  mm_stream_func  = function (length: mm_word; dest: mm_addr; format: mm_stream_formats): mm_word;


type
  mm_reverbflags = integer;
const
  MMRF_MEMORY     : mm_reverbflags = $01;
  MMRF_DELAY      : mm_reverbflags = $02;
  MMRF_RATE       : mm_reverbflags = $04;
  MMRF_FEEDBACK   : mm_reverbflags = $08;
  MMRF_PANNING    : mm_reverbflags = $10;
  MMRF_LEFT       : mm_reverbflags = $20;
  MMRF_RIGHT      : mm_reverbflags = $40;
  MMRF_BOTH       : mm_reverbflags = $60;
  
  MMRF_INVERSEPAN : mm_reverbflags = $80;
  MMRF_NODRYLEFT  : mm_reverbflags = $100;
  MMRF_NODRYRIGHT : mm_reverbflags = $200;
  MMRF_8BITLEFT   : mm_reverbflags = $400;
  MMRF_16BITLEFT  : mm_reverbflags = $800;
  MMRF_8BITRIGHT  : mm_reverbflags = $1000;
  MMRF_16BITRIGHT : mm_reverbflags = $2000;
  MMRF_DRYLEFT    : mm_reverbflags = $4000;
  MMRF_DRYRIGHT   : mm_reverbflags = $8000;
  
type
  mm_reverbch = integer;
const
  MMRC_LEFT   : mm_reverbch = 1;
  MMRC_RIGHT  : mm_reverbch = 2;
  MMRC_BOTH   : mm_reverbch = 3;

type
  mmreverbcfg = record
    flags    : mm_word;
    memory   : mm_addr;
    delay    : mm_hword;
    rate     : mm_hword;
    feedback : mm_hword;
    panning  : mm_byte;
  end;
  mm_reverb_cfg  = mmreverbcfg;
  pmm_reverb_cfg = ^mm_reverb_cfg;

type
  mm_pmode = integer;
const
  MM_PLAY_LOOP: mm_pmode = 0;
  MM_PLAY_ONCE: mm_pmode = 1;

type
  mm_mixmode = integer;
const
  MM_MIX_8KHZ : mm_mixmode = 0;
  MM_MIX_10KHZ: mm_mixmode = 1;
  MM_MIX_13KHZ: mm_mixmode = 2;
  MM_MIX_16KHZ: mm_mixmode = 3;
  MM_MIX_18KHZ: mm_mixmode = 4;
  MM_MIX_21KHZ: mm_mixmode = 5;
  MM_MIX_27KHZ: mm_mixmode = 6;
  MM_MIX_31KHZ: mm_mixmode = 7;

type
  mm_stream_timer = integer;
const
  MM_TIMER0: mm_stream_timer = 0;
  MM_TIMER1: mm_stream_timer = 1;
  MM_TIMER2: mm_stream_timer = 2;
  MM_TIMER3: mm_stream_timer = 3;

type
  t_mmdssample = record
    loop_start  : mm_word;
    case integer of
      0: (loop_length : mm_word);
      1: (length      : mm_word;
          format      : mm_byte;
          repeat_mode : mm_byte;
          base_rate   : mm_hword;
          data        : mm_addr;
      );
  end;
  mm_ds_sample  = t_mmdssample;
  pmm_ds_sample = ^t_mmdssample;

  t_mmsoundeffect = record
    case integer of
      0: (id     : mm_word);
      1: (sample : pmm_ds_sample;
          rate    : mm_hword;
          handle  : mm_sfxhand;
          volume  : mm_byte;
          panning : mm_byte;
    );
  end;
  mm_sound_effect = t_mmsoundeffect;
  pmm_sound_effect = ^t_mmsoundeffect;

  t_mmgbasystem = record
    mixing_mode       : mm_mixmode;
    mod_channel_count : mm_word;
    mix_channel_count : mm_word;
    module_channels   : mm_addr;
    active_channels   : mm_addr;
    mixing_channels   : mm_addr;
    mixing_memory     : mm_addr;
    wave_memory       : mm_addr;
    soundbank         : mm_addr;
  end;
  mm_gba_system  = t_mmgbasystem;
  pmm_gba_system = ^t_mmgbasystem;

  t_mmdssystem = record
    mod_count    : mm_word;
    samp_count   : mm_word;
    mem_bank     : pmm_word;
    fifo_channel : mm_word;
  end;
  mm_ds_system  = t_mmdssystem;
  pmm_ds_system = ^t_mmdssystem;

  t_mmstream = record
    sampling_rate : mm_word;
    buffer_length : mm_word;
    callback      : mm_stream_func;
    format        : mm_word;
    timer         : mm_word;
    manual        : mm_bool;
  end;	
  mm_stream  = t_mmstream;
  pmm_stream = ^t_mmstream;


  t_mmlayer = record
    tick          : mm_byte;
    row           : mm_byte;
    position      : mm_byte;
    nrows         : mm_byte;
    global_volume : mm_byte;
    speed         : mm_byte;
    active        : mm_byte;
    bpm           : mm_byte;
  end;
  mm_modlayer  = t_mmlayer;
  pmm_modlayer = ^t_mmlayer;



  tmm_voice = record
    source     : mm_addr;
    length     : mm_word;
    loop_start : mm_hword;
    timer      : mm_hword;
    flags      : mm_byte;
    format     : mm_byte;
    rept       : mm_byte;
    volume     : mm_byte;
    divider    : mm_byte;
    panning    : mm_byte;
    index      : mm_byte;
    reserved   : array [0..0] of mm_byte;
  end;
  mm_voice  = tmm_voice;
  pmm_voice = ^tmm_voice;

const
  MMVF_FREQ     = 2;
  MMVF_VOLUME   = 4;
  MMVF_PANNING  = 8;
  MMVF_SOURCE   = 16;
  MMVF_STOP     = 32;

{$endif NDS_INTERFACE}

