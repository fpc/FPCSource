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

{$define _____badbear_____}

type
  tmm_mas_prefix = record
    size     : mm_word;
    _type    : mm_byte;
    version  : mm_byte;
    reserved : array [0..1] of mm_byte;
  end;
  mm_mas_prefix  = tmm_mas_prefix;
  pmm_mas_prefix = ^tmm_mas_prefix;

  tmm_mas_head = record
    order_count     : mm_byte;
    instr_count     : mm_byte;
    sampl_count     : mm_byte;
    pattn_count     : mm_byte;
    flags           : mm_byte;
    global_volume   : mm_byte;
    initial_speed   : mm_byte;
    initial_tempo   : mm_byte;
    repeat_position : mm_byte;
    reserved        : array [0..2] of mm_byte;
    channel_volume  : array [0..31] of mm_byte;
    channel_panning : array [0..31] of mm_byte;
    sequence        : array [0..199] of mm_byte;

    {$ifdef _____badbear_____}
      tables        : array [0..0] of mm_addr;
    {$endif}
    // ::instrument table
    // ::sample info table
    // ::pattern table
  end;
  mm_mas_head = tmm_mas_head;
  pmm_mas_head = ^tmm_mas_head;

  tmm_mas_instrument = record
    global_volume: mm_byte;
    fadeout: mm_byte;
    random_volume: mm_byte;
    nna: mm_byte;
    dct: mm_byte;
    dca: mm_byte;
    env_flags: mm_byte;
    panning: mm_byte;
    note_map: array [0..119] of mm_hword;
    {$ifdef _____badbear_____}
       envelopes: array [0..0] of mm_byte;
    {$endif}
    // ::envelopes
  end;
  mm_mas_instrument = tmm_mas_instrument;
  pmm_mas_instrument = ^tmm_mas_instrument;

  tmm_mas_envelope = record
    size: mm_byte;
    loop_start: mm_byte;
    loop_end: mm_byte;
    sus_start: mm_byte;
    sus_end: mm_byte;
    node_count: mm_byte;
    is_filter: mm_byte; // (maybe supported someday :)
    wasted: mm_byte;
    {$ifdef _____badbear_____}
      env_nodes: array [0..0] of mm_byte;
    {$endif}
    // ::envelope nodes
  end;
  mm_mas_envelope = tmm_mas_envelope;
  pmm_mas_envelope = ^tmm_mas_envelope;

  tmm_mas_sample_info = record
    global_volume: mm_byte;
    default_volume: mm_byte;
    frequency: mm_hword;
    av_type: mm_byte; // (auto vibrato)
    av_depth: mm_byte;
    av_speed: mm_byte;
    panning: mm_byte;
    av_rate: mm_hword;
    msl_id: mm_hword;
    {$ifdef _____badbear_____}
      data: array [0..0] of mm_byte;
    {$endif}
    // ::sample may follow
  end;
  mm_mas_sample_info = tmm_mas_sample_info;
  pmm_mas_sample_info = ^tmm_mas_sample_info;

  tmm_mas_pattern = record
    row_count: mm_byte;
    {$ifdef _____badbear_____}
      pattern_data: array [0..0] of mm_byte;
    {$endif}
    // ::pattern data
  end;
  mm_mas_pattern = tmm_mas_pattern;
  pmm_mas_pattern = ^tmm_mas_pattern;

  tmm_mas_gba_sample = record
    length: mm_word;
    loop_length: mm_word;
    reserved: mm_hword;
    default_frequency: mm_hword;
    {$ifdef _____badbear_____}
      data: array [0..0] of mm_byte;
    {$endif}    
    // ::8-bit sample data
  end;
  mm_mas_gba_sample = tmm_mas_gba_sample;
  pmm_mas_gba_sample = ^tmm_mas_gba_sample;

  tmm_mas_ds_sample = record
    loop_start: mm_word;
    format: mm_byte;
    repeat_mode: mm_byte;
    default_frequency: mm_hword;
    {$ifdef _____badbear_____}
      data: array [0..0] of mm_byte;
    {$endif}    
  	// ::sample data
    case integer of
      0: (loop_length: mm_word;);
      1: (length: mm_word;);
  end;
  mm_mas_ds_sample = tmm_mas_ds_sample;
  pmm_mas_ds_sample = ^tmm_mas_ds_sample;

const
  MM_SFORMAT_8BIT   = 0;
  MM_SFORMAT_16BIT  = 1;
  MM_SFORMAT_ADPCM  = 2;

  MM_SREPEAT_FORWARD  = 1;
  MM_SREPEAT_OFF      = 2;




{$endif NDS_INTERFACE}

