{  Free Pascal port by Nikolay Nikolov <nickysn@users.sourceforge.net>  }

{**
 * \file include/asoundef.h
 * \brief Application interface library for the ALSA driver
 * \author Jaroslav Kysela <perex@perex.cz>
 * \author Abramo Bagnara <abramo@alsa-project.org>
 * \author Takashi Iwai <tiwai@suse.de>
 * \date 1998-2001
 *
 * Definitions of constants for the ALSA driver
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

//#ifndef __ALSA_ASOUNDEF_H
//#define __ALSA_ASOUNDEF_H

//#ifdef __cplusplus
//extern "C" {
//#endif

(**
 * \defgroup Digital_Audio_Interface Constants for Digital Audio Interfaces
 * AES/IEC958 channel status bits.
 * \{
 *)

const
  IEC958_AES0_PROFESSIONAL           = 1 shl 0;   { 0 = consumer, 1 = professional }
  IEC958_AES0_NONAUDIO               = 1 shl 1;   { 0 = audio, 1 = non-audio }
  IEC958_AES0_PRO_EMPHASIS           = 7 shl 2;   { mask - emphasis }
  IEC958_AES0_PRO_EMPHASIS_NOTID     = 0 shl 2;   { emphasis not indicated }
  IEC958_AES0_PRO_EMPHASIS_NONE      = 1 shl 2;   { no emphasis }
  IEC958_AES0_PRO_EMPHASIS_5015      = 3 shl 2;   { 50/15us emphasis }
  IEC958_AES0_PRO_EMPHASIS_CCITT     = 7 shl 2;   { CCITT J.17 emphasis }
  IEC958_AES0_PRO_FREQ_UNLOCKED      = 1 shl 5;   { source sample frequency: 0 = locked, 1 = unlocked }
  IEC958_AES0_PRO_FS                 = 3 shl 6;   { mask - sample frequency }
  IEC958_AES0_PRO_FS_NOTID           = 0 shl 6;   { fs not indicated }
  IEC958_AES0_PRO_FS_44100           = 1 shl 6;   { 44.1kHz }
  IEC958_AES0_PRO_FS_48000           = 2 shl 6;   { 48kHz }
  IEC958_AES0_PRO_FS_32000           = 3 shl 6;   { 32kHz }
  IEC958_AES0_CON_NOT_COPYRIGHT      = 1 shl 2;   { 0 = copyright, 1 = not copyright }
  IEC958_AES0_CON_EMPHASIS           = 7 shl 3;   { mask - emphasis }
  IEC958_AES0_CON_EMPHASIS_NONE      = 0 shl 3;   { no emphasis }
  IEC958_AES0_CON_EMPHASIS_5015      = 1 shl 3;   { 50/15us emphasis }
  IEC958_AES0_CON_MODE               = 3 shl 6;   { mask - mode }
  IEC958_AES1_PRO_MODE               = 15 shl 0;  { mask - channel mode }
  IEC958_AES1_PRO_MODE_NOTID         = 0 shl 0;   { mode not indicated }
  IEC958_AES1_PRO_MODE_STEREOPHONIC  = 2 shl 0;   { stereophonic - ch A is left }
  IEC958_AES1_PRO_MODE_SINGLE        = 4 shl 0;   { single channel }
  IEC958_AES1_PRO_MODE_TWO           = 8 shl 0;   { two channels }
  IEC958_AES1_PRO_MODE_PRIMARY       = 12 shl 0;  { primary/secondary }
  IEC958_AES1_PRO_MODE_BYTE3         = 15 shl 0;  { vector to byte 3 }
  IEC958_AES1_PRO_USERBITS           = 15 shl 4;  { mask - user bits }
  IEC958_AES1_PRO_USERBITS_NOTID     = 0 shl 4;   { user bits not indicated }
  IEC958_AES1_PRO_USERBITS_192       = 8 shl 4;   { 192-bit structure }
  IEC958_AES1_PRO_USERBITS_UDEF      = 12 shl 4;  { user defined application }
  IEC958_AES1_CON_CATEGORY           = $7f;       { consumer category }
  IEC958_AES1_CON_GENERAL            = $00;       { general category }
  IEC958_AES1_CON_LASEROPT_MASK      = $07;       { Laser-optical mask }
  IEC958_AES1_CON_LASEROPT_ID        = $01;       { Laser-optical ID }
  IEC958_AES1_CON_IEC908_CD          = IEC958_AES1_CON_LASEROPT_ID or $00;    { IEC958 CD compatible device }
  IEC958_AES1_CON_NON_IEC908_CD      = IEC958_AES1_CON_LASEROPT_ID or $08;    { non-IEC958 CD compatible device }
  IEC958_AES1_CON_MINI_DISC          = IEC958_AES1_CON_LASEROPT_ID or $48;    { Mini-Disc device }
  IEC958_AES1_CON_DVD                = IEC958_AES1_CON_LASEROPT_ID or $18;    { DVD device }
  IEC958_AES1_CON_LASTEROPT_OTHER    = IEC958_AES1_CON_LASEROPT_ID or $78;    { Other laser-optical product }
  IEC958_AES1_CON_DIGDIGCONV_MASK    = $07;       { digital<->digital converter mask }
  IEC958_AES1_CON_DIGDIGCONV_ID      = $02;       { digital<->digital converter id }
  IEC958_AES1_CON_PCM_CODER          = IEC958_AES1_CON_DIGDIGCONV_ID or $00;  { PCM coder }
  IEC958_AES1_CON_MIXER              = IEC958_AES1_CON_DIGDIGCONV_ID or $10;  { Digital signal mixer }
  IEC958_AES1_CON_RATE_CONVERTER     = IEC958_AES1_CON_DIGDIGCONV_ID or $18;  { Rate converter }
  IEC958_AES1_CON_SAMPLER            = IEC958_AES1_CON_DIGDIGCONV_ID or $20;  { PCM sampler }
  IEC958_AES1_CON_DSP                = IEC958_AES1_CON_DIGDIGCONV_ID or $28;  { Digital sound processor }
  IEC958_AES1_CON_DIGDIGCONV_OTHER   = IEC958_AES1_CON_DIGDIGCONV_ID or $78;  { Other digital<->digital product }
  IEC958_AES1_CON_MAGNETIC_MASK      = $07;       { Magnetic device mask }
  IEC958_AES1_CON_MAGNETIC_ID        = $03;       { Magnetic device ID }
  IEC958_AES1_CON_DAT                = IEC958_AES1_CON_MAGNETIC_ID or $00;    { Digital Audio Tape }
  IEC958_AES1_CON_VCR                = IEC958_AES1_CON_MAGNETIC_ID or $08;    { Video recorder }
  IEC958_AES1_CON_DCC                = IEC958_AES1_CON_MAGNETIC_ID or $40;    { Digital compact cassette }
  IEC958_AES1_CON_MAGNETIC_DISC      = IEC958_AES1_CON_MAGNETIC_ID or $18;    { Magnetic disc digital audio device }
  IEC958_AES1_CON_MAGNETIC_OTHER     = IEC958_AES1_CON_MAGNETIC_ID or $78;    { Other magnetic device }
  IEC958_AES1_CON_BROADCAST1_MASK    = $07;       { Broadcast mask }
  IEC958_AES1_CON_BROADCAST1_ID      = $04;       { Broadcast ID }
  IEC958_AES1_CON_DAB_JAPAN          = IEC958_AES1_CON_BROADCAST1_ID or $00;  { Digital audio broadcast (Japan) }
  IEC958_AES1_CON_DAB_EUROPE         = IEC958_AES1_CON_BROADCAST1_ID or $08;  { Digital audio broadcast (Europe) }
  IEC958_AES1_CON_DAB_USA            = IEC958_AES1_CON_BROADCAST1_ID or $60;  { Digital audio broadcast (USA) }
  IEC958_AES1_CON_SOFTWARE           = IEC958_AES1_CON_BROADCAST1_ID or $40;  { Electronic software delivery }
  IEC958_AES1_CON_IEC62105           = IEC958_AES1_CON_BROADCAST1_ID or $20;  { Used by another standard (IEC 62105) }
  IEC958_AES1_CON_BROADCAST1_OTHER   = IEC958_AES1_CON_BROADCAST1_ID or $78;  { Other broadcast product }
  IEC958_AES1_CON_BROADCAST2_MASK    = $0f;       { Broadcast alternative mask }
  IEC958_AES1_CON_BROADCAST2_ID      = $0e;       { Broadcast alternative ID }
  IEC958_AES1_CON_MUSICAL_MASK       = $07;       { Musical device mask }
  IEC958_AES1_CON_MUSICAL_ID         = $05;       { Musical device ID }
  IEC958_AES1_CON_SYNTHESIZER        = IEC958_AES1_CON_MUSICAL_ID or $00;     { Synthesizer }
  IEC958_AES1_CON_MICROPHONE         = IEC958_AES1_CON_MUSICAL_ID or $08;     { Microphone }
  IEC958_AES1_CON_MUSICAL_OTHER      = IEC958_AES1_CON_MUSICAL_ID or $78;     { Other musical device }
  IEC958_AES1_CON_ADC_MASK           = $1f;       { ADC Mask }
  IEC958_AES1_CON_ADC_ID             = $06;       { ADC ID }
  IEC958_AES1_CON_ADC                = IEC958_AES1_CON_ADC_ID or $00;         { ADC without copyright information }
  IEC958_AES1_CON_ADC_OTHER          = IEC958_AES1_CON_ADC_ID or $60;         { Other ADC product (with no copyright information) }
  IEC958_AES1_CON_ADC_COPYRIGHT_MASK = $1f;       { ADC Copyright mask }
  IEC958_AES1_CON_ADC_COPYRIGHT_ID   = $16;       { ADC Copyright ID }
  IEC958_AES1_CON_ADC_COPYRIGHT      = IEC958_AES1_CON_ADC_COPYRIGHT_ID or $00;     { ADC with copyright information }
  IEC958_AES1_CON_ADC_COPYRIGHT_OTHER = IEC958_AES1_CON_ADC_COPYRIGHT_ID or $60;    { Other ADC with copyright information product }
  IEC958_AES1_CON_SOLIDMEM_MASK      = $0f;       { Solid memory based products mask }
  IEC958_AES1_CON_SOLIDMEM_ID        = $08;       { Solid memory based products ID }
  IEC958_AES1_CON_SOLIDMEM_DIGITAL_RECORDER_PLAYER = IEC958_AES1_CON_SOLIDMEM_ID or $00;    { Digital audio recorder and player using solid state memory }
  IEC958_AES1_CON_SOLIDMEM_OTHER     = IEC958_AES1_CON_SOLIDMEM_ID or $70;      { Other solid state memory based product }
  IEC958_AES1_CON_EXPERIMENTAL       = $40;       { experimental category }
  IEC958_AES1_CON_ORIGINAL           = 1 shl 7;   { this bits depends on the category code }
  IEC958_AES2_PRO_SBITS              = 7 shl 0;   { mask - sample bits }
  IEC958_AES2_PRO_SBITS_20           = 2 shl 0;   { 20-bit - coordination }
  IEC958_AES2_PRO_SBITS_24           = 4 shl 0;   { 24-bit - main audio }
  IEC958_AES2_PRO_SBITS_UDEF         = 6 shl 0;   { user defined application }
  IEC958_AES2_PRO_WORDLEN            = 7 shl 3;   { mask - source word length }
  IEC958_AES2_PRO_WORDLEN_NOTID      = 0 shl 3;   { source word length not indicated }
  IEC958_AES2_PRO_WORDLEN_22_18      = 2 shl 3;   { 22-bit or 18-bit }
  IEC958_AES2_PRO_WORDLEN_23_19      = 4 shl 3;   { 23-bit or 19-bit }
  IEC958_AES2_PRO_WORDLEN_24_20      = 5 shl 3;   { 24-bit or 20-bit }
  IEC958_AES2_PRO_WORDLEN_20_16      = 6 shl 3;   { 20-bit or 16-bit }
  IEC958_AES2_CON_SOURCE             = 15 shl 0;  { mask - source number }
  IEC958_AES2_CON_SOURCE_UNSPEC      = 0 shl 0;   { source number unspecified }
  IEC958_AES2_CON_CHANNEL            = 15 shl 4;  { mask - channel number }
  IEC958_AES2_CON_CHANNEL_UNSPEC     = 0 shl 4;   { channel number unspecified }
  IEC958_AES3_CON_FS                 = 15 shl 0;  { mask - sample frequency }
  IEC958_AES3_CON_FS_44100           = 0 shl 0;   { 44.1kHz }
  IEC958_AES3_CON_FS_NOTID           = 1 shl 0;   { sample frequency non indicated }
  IEC958_AES3_CON_FS_48000           = 2 shl 0;   { 48kHz }
  IEC958_AES3_CON_FS_32000           = 3 shl 0;   { 32kHz }
  IEC958_AES3_CON_FS_22050           = 4 shl 0;   { 22.05kHz }
  IEC958_AES3_CON_FS_24000           = 6 shl 0;   { 24kHz }
  IEC958_AES3_CON_FS_88200           = 8 shl 0;   { 88.2kHz }
  IEC958_AES3_CON_FS_768000          = 9 shl 0;   { 768kHz }
  IEC958_AES3_CON_FS_96000           = 10 shl 0;  { 96kHz }
  IEC958_AES3_CON_FS_176400          = 12 shl 0;  { 176.4kHz }
  IEC958_AES3_CON_FS_192000          = 14 shl 0;  { 192kHz }
  IEC958_AES3_CON_CLOCK              = 3 shl 4;   { mask - clock accuracy }
  IEC958_AES3_CON_CLOCK_1000PPM      = 0 shl 4;   { 1000 ppm }
  IEC958_AES3_CON_CLOCK_50PPM        = 1 shl 4;   { 50 ppm }
  IEC958_AES3_CON_CLOCK_VARIABLE     = 2 shl 4;   { variable pitch }
  IEC958_AES4_CON_MAX_WORDLEN_24     = 1 shl 0;   { 0 = 20-bit, 1 = 24-bit }
  IEC958_AES4_CON_WORDLEN            = 7 shl 1;   { mask - sample word length }
  IEC958_AES4_CON_WORDLEN_NOTID      = 0 shl 1;   { not indicated }
  IEC958_AES4_CON_WORDLEN_20_16      = 1 shl 1;   { 20-bit or 16-bit }
  IEC958_AES4_CON_WORDLEN_22_18      = 2 shl 1;   { 22-bit or 18-bit }
  IEC958_AES4_CON_WORDLEN_23_19      = 4 shl 1;   { 23-bit or 19-bit }
  IEC958_AES4_CON_WORDLEN_24_20      = 5 shl 1;   { 24-bit or 20-bit }
  IEC958_AES4_CON_WORDLEN_21_17      = 6 shl 1;   { 21-bit or 17-bit }
  IEC958_AES4_CON_ORIGFS             = 15 shl 4;  { mask - original sample frequency }
  IEC958_AES4_CON_ORIGFS_NOTID       = 0 shl 4;   { original sample frequency not indicated }
  IEC958_AES4_CON_ORIGFS_192000      = 1 shl 4;   { 192kHz }
  IEC958_AES4_CON_ORIGFS_12000       = 2 shl 4;   { 12kHz }
  IEC958_AES4_CON_ORIGFS_176400      = 3 shl 4;   { 176.4kHz }
  IEC958_AES4_CON_ORIGFS_96000       = 5 shl 4;   { 96kHz }
  IEC958_AES4_CON_ORIGFS_8000        = 6 shl 4;   { 8kHz }
  IEC958_AES4_CON_ORIGFS_88200       = 7 shl 4;   { 88.2kHz }
  IEC958_AES4_CON_ORIGFS_16000       = 8 shl 4;   { 16kHz }
  IEC958_AES4_CON_ORIGFS_24000       = 9 shl 4;   { 24kHz }
  IEC958_AES4_CON_ORIGFS_11025       = 10 shl 4;  { 11.025kHz }
  IEC958_AES4_CON_ORIGFS_22050       = 11 shl 4;  { 22.05kHz }
  IEC958_AES4_CON_ORIGFS_32000       = 12 shl 4;  { 32kHz }
  IEC958_AES4_CON_ORIGFS_48000       = 13 shl 4;  { 48kHz }
  IEC958_AES4_CON_ORIGFS_44100       = 15 shl 4;  { 44.1kHz }
  IEC958_AES5_CON_CGMSA              = 3 shl 0;   { mask - CGMS-A }
  IEC958_AES5_CON_CGMSA_COPYFREELY   = 0 shl 0;   { copying is permitted without restriction }
  IEC958_AES5_CON_CGMSA_COPYONCE     = 1 shl 0;   { one generation of copies may be made }
  IEC958_AES5_CON_CGMSA_COPYNOMORE   = 2 shl 0;   { condition not be used }
  IEC958_AES5_CON_CGMSA_COPYNEVER    = 3 shl 0;   { no copying is permitted }

(** \} *)

(**
 * \defgroup MIDI_Interface Constants for MIDI v1.0
 * Constants for MIDI v1.0.
 * \{
 *)

  MIDI_CHANNELS                      = 16;        { Number of channels per port/cable. }
  MIDI_GM_DRUM_CHANNEL               = 10 - 1;    { Channel number for GM drums. }

(**
 * \defgroup MIDI_Commands MIDI Commands
 * MIDI command codes.
 * \{
 *)

  MIDI_CMD_NOTE_OFF                  = $80;       { note off }
  MIDI_CMD_NOTE_ON                   = $90;       { note on }
  MIDI_CMD_NOTE_PRESSURE             = $a0;       { key pressure }
  MIDI_CMD_CONTROL                   = $b0;       { control change }
  MIDI_CMD_PGM_CHANGE                = $c0;       { program change }
  MIDI_CMD_CHANNEL_PRESSURE          = $d0;       { channel pressure }
  MIDI_CMD_BENDER                    = $e0;       { pitch bender }

  MIDI_CMD_COMMON_SYSEX              = $f0;       { sysex (system exclusive) begin }
  MIDI_CMD_COMMON_MTC_QUARTER        = $f1;       { MTC quarter frame }
  MIDI_CMD_COMMON_SONG_POS           = $f2;       { song position }
  MIDI_CMD_COMMON_SONG_SELECT        = $f3;       { song select }
  MIDI_CMD_COMMON_TUNE_REQUEST       = $f6;       { tune request }
  MIDI_CMD_COMMON_SYSEX_END          = $f7;       { end of sysex }
  MIDI_CMD_COMMON_CLOCK              = $f8;       { clock }
  MIDI_CMD_COMMON_START              = $fa;       { start }
  MIDI_CMD_COMMON_CONTINUE           = $fb;       { continue }
  MIDI_CMD_COMMON_STOP               = $fc;       { stop }
  MIDI_CMD_COMMON_SENSING            = $fe;       { active sensing }
  MIDI_CMD_COMMON_RESET              = $ff;       { reset }

(** \} *)

(**
 * \defgroup MIDI_Controllers MIDI Controllers
 * MIDI controller numbers.
 * \{
 *)

  MIDI_CTL_MSB_BANK                  = $00;       { Bank selection }
  MIDI_CTL_MSB_MODWHEEL              = $01;       { Modulation }
  MIDI_CTL_MSB_BREATH                = $02;       { Breath }
  MIDI_CTL_MSB_FOOT                  = $04;       { Foot }
  MIDI_CTL_MSB_PORTAMENTO_TIME       = $05;       { Portamento time }
  MIDI_CTL_MSB_DATA_ENTRY            = $06;       { Data entry }
  MIDI_CTL_MSB_MAIN_VOLUME           = $07;       { Main volume }
  MIDI_CTL_MSB_BALANCE               = $08;       { Balance }
  MIDI_CTL_MSB_PAN                   = $0a;       { Panpot }
  MIDI_CTL_MSB_EXPRESSION            = $0b;       { Expression }
  MIDI_CTL_MSB_EFFECT1               = $0c;       { Effect1 }
  MIDI_CTL_MSB_EFFECT2               = $0d;       { Effect2 }
  MIDI_CTL_MSB_GENERAL_PURPOSE1      = $10;       { General purpose 1 }
  MIDI_CTL_MSB_GENERAL_PURPOSE2      = $11;       { General purpose 2 }
  MIDI_CTL_MSB_GENERAL_PURPOSE3      = $12;       { General purpose 3 }
  MIDI_CTL_MSB_GENERAL_PURPOSE4      = $13;       { General purpose 4 }
  MIDI_CTL_LSB_BANK                  = $20;       { Bank selection }
  MIDI_CTL_LSB_MODWHEEL              = $21;       { Modulation }
  MIDI_CTL_LSB_BREATH                = $22;       { Breath }
  MIDI_CTL_LSB_FOOT                  = $24;       { Foot }
  MIDI_CTL_LSB_PORTAMENTO_TIME       = $25;       { Portamento time }
  MIDI_CTL_LSB_DATA_ENTRY            = $26;       { Data entry }
  MIDI_CTL_LSB_MAIN_VOLUME           = $27;       { Main volume }
  MIDI_CTL_LSB_BALANCE               = $28;       { Balance }
  MIDI_CTL_LSB_PAN                   = $2a;       { Panpot }
  MIDI_CTL_LSB_EXPRESSION            = $2b;       { Expression }
  MIDI_CTL_LSB_EFFECT1               = $2c;       { Effect1 }
  MIDI_CTL_LSB_EFFECT2               = $2d;       { Effect2 }
  MIDI_CTL_LSB_GENERAL_PURPOSE1      = $30;       { General purpose 1 }
  MIDI_CTL_LSB_GENERAL_PURPOSE2      = $31;       { General purpose 2 }
  MIDI_CTL_LSB_GENERAL_PURPOSE3      = $32;       { General purpose 3 }
  MIDI_CTL_LSB_GENERAL_PURPOSE4      = $33;       { General purpose 4 }
  MIDI_CTL_SUSTAIN                   = $40;       { Sustain pedal }
  MIDI_CTL_PORTAMENTO                = $41;       { Portamento }
  MIDI_CTL_SOSTENUTO                 = $42;       { Sostenuto }
  MIDI_CTL_SUSTENUTO                 = $42;       { Sostenuto (a typo in the older version) }
  MIDI_CTL_SOFT_PEDAL                = $43;       { Soft pedal }
  MIDI_CTL_LEGATO_FOOTSWITCH         = $44;       { Legato foot switch }
  MIDI_CTL_HOLD2                     = $45;       { Hold2 }
  MIDI_CTL_SC1_SOUND_VARIATION       = $46;       { SC1 Sound Variation }
  MIDI_CTL_SC2_TIMBRE                = $47;       { SC2 Timbre }
  MIDI_CTL_SC3_RELEASE_TIME          = $48;       { SC3 Release Time }
  MIDI_CTL_SC4_ATTACK_TIME           = $49;       { SC4 Attack Time }
  MIDI_CTL_SC5_BRIGHTNESS            = $4a;       { SC5 Brightness }
  MIDI_CTL_SC6                       = $4b;       { SC6 }
  MIDI_CTL_SC7                       = $4c;       { SC7 }
  MIDI_CTL_SC8                       = $4d;       { SC8 }
  MIDI_CTL_SC9                       = $4e;       { SC9 }
  MIDI_CTL_SC10                      = $4f;       { SC10 }
  MIDI_CTL_GENERAL_PURPOSE5          = $50;       { General purpose 5 }
  MIDI_CTL_GENERAL_PURPOSE6          = $51;       { General purpose 6 }
  MIDI_CTL_GENERAL_PURPOSE7          = $52;       { General purpose 7 }
  MIDI_CTL_GENERAL_PURPOSE8          = $53;       { General purpose 8 }
  MIDI_CTL_PORTAMENTO_CONTROL        = $54;       { Portamento control }
  MIDI_CTL_E1_REVERB_DEPTH           = $5b;       { E1 Reverb Depth }
  MIDI_CTL_E2_TREMOLO_DEPTH          = $5c;       { E2 Tremolo Depth }
  MIDI_CTL_E3_CHORUS_DEPTH           = $5d;       { E3 Chorus Depth }
  MIDI_CTL_E4_DETUNE_DEPTH           = $5e;       { E4 Detune Depth }
  MIDI_CTL_E5_PHASER_DEPTH           = $5f;       { E5 Phaser Depth }
  MIDI_CTL_DATA_INCREMENT            = $60;       { Data Increment }
  MIDI_CTL_DATA_DECREMENT            = $61;       { Data Decrement }
  MIDI_CTL_NONREG_PARM_NUM_LSB       = $62;       { Non-registered parameter number }
  MIDI_CTL_NONREG_PARM_NUM_MSB       = $63;       { Non-registered parameter number }
  MIDI_CTL_REGIST_PARM_NUM_LSB       = $64;       { Registered parameter number }
  MIDI_CTL_REGIST_PARM_NUM_MSB       = $65;       { Registered parameter number }
  MIDI_CTL_ALL_SOUNDS_OFF            = $78;       { All sounds off }
  MIDI_CTL_RESET_CONTROLLERS         = $79;       { Reset Controllers }
  MIDI_CTL_LOCAL_CONTROL_SWITCH      = $7a;       { Local control switch }
  MIDI_CTL_ALL_NOTES_OFF             = $7b;       { All notes off }
  MIDI_CTL_OMNI_OFF                  = $7c;       { Omni off }
  MIDI_CTL_OMNI_ON                   = $7d;       { Omni on }
  MIDI_CTL_MONO1                     = $7e;       { Mono1 }
  MIDI_CTL_MONO2                     = $7f;       { Mono2 }

(** \} *)

(** \} *)

//#ifdef __cplusplus
//}
//#endif

//#endif /* __ALSA_ASOUNDEF_H */
