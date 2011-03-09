unit gcmodplay;
{$mode objfpc} 
{$J+}
{$INLINE ON}
{$MACRO ON}
{$ASSERTIONS ON}

interface

uses
  ctypes, gctypes;

const
  MAX_VOICES = 32;

type
  _modinstr = record
    name_ : array [0..22] of cint8;  (* 000..021  *)
    length : cuint32;  (* 022..023  *)
    finetune : cuint8;  (* 024  *)
    volume : cuint8;  (* 025  *)
    loop_start : cuint32;  (* 026..027  *)
    loop_end : cuint32;  (* 028..029  *)
    loop_length : cuint32;
    looped : cbool;
    data : pcint8;
  end;
  MOD_INSTR = _modinstr;

  Pcbool = ^cbool;
  _mod = record
    loaded : cbool;
    name_ : array [0..20] of cint8;
    instrument : array [0..30] of MOD_INSTR;
    num_patterns : cint32;
    song_length : cuint8;
    ciaa : cuint8;
    song : array [0..127] of cuint8;
    id : array [0..3] of cint8;
    patterndata : pcuint8;
    num_instr : cint32;
    num_voices : cint32;  (* Number of voices in the MOD  *)
    num_channels : cint32;  (* Number of channels to actually mix (num_channels-num_voices = number of sfx channels)  *)
    mixingbuf : pcuint8;
    mixingbuflen : cint32;
    shiftval : cint32;  (* Number of bits to lshift every mixed 16bit word by  *)
    (* Player variables  *)
    channel_active : array [0..MAX_VOICES-1] of cbool;
    patterndelay : cint32;
    speed : cint32;
    bpm : cint32;
    songpos : cint32;  (* In the song  *)
    patternline : cint32;  (* In the pattern  *)
    patternline_jumpto : cint32;  (* For the E6 effect  *)
    patternline_jumpcount : cint32;  (* For the E6 effect  *)
    speedcounter : cint32;
    freq : cint32;
    bits : cint32;
    channels : cint32;  (* 1 = mono, 2 = stereo  *)
    playpos : array [0..MAX_VOICES-1] of cuint32;  (* Playing position for each channel  *)
    instnum : array [0..MAX_VOICES-1] of cuint8;  (* Current instrument  *)
    chanfreq : array [0..MAX_VOICES-1] of cuint16;  (* Current frequency  *)
    channote : array [0..MAX_VOICES-1] of cuint16;  (* Last note triggered  *)
    volume : array [0..MAX_VOICES-1] of cuint8;  (* Current volume  *)
    effect : array [0..MAX_VOICES-1] of cuint8;  (* Current effect  *)
    effectop : array [0..MAX_VOICES-1] of cuint8;  (* Current effect operand  *)
    last_effect : array [0..MAX_VOICES-1] of cuint8;
    (* Effects handling  *)
    portamento_to : array [0..MAX_VOICES-1] of cuint16;
    porta_speed : array [0..MAX_VOICES-1] of cuint8;
    retrigger_counter : array [0..MAX_VOICES-1] of cuint8;
    arp_counter : cuint8;
    sintabpos : array [0..MAX_VOICES-1] of cuint8;
    vib_freq : array [0..MAX_VOICES-1] of cuint8;
    vib_depth : array [0..MAX_VOICES-1] of cuint8;
    vib_basefreq : array [0..MAX_VOICES-1] of cuint16;
    trem_basevol : array [0..MAX_VOICES-1] of cuint8;
    trem_freq : array [0..MAX_VOICES-1] of cuint8;
    trem_depth : array [0..MAX_VOICES-1] of cuint8;
    glissando : array [0..MAX_VOICES-1] of cbool;
    trem_wave : array [0..MAX_VOICES-1] of cuint8;
    vib_wave : array [0..MAX_VOICES-1] of cuint8;
    nextinstr : array [0..MAX_VOICES-1] of cuint8;
    nextnote : array [0..MAX_VOICES-1] of cuint16;
    samplespertick : cuint32;
    samplescounter : cuint32;
    modraw : pcuint8;
    bpmtab : pcuint32;
    inctab : pcuint32;
    notebeats : cuint32;
    callback : procedure(par0: pointer); cdecl;
    musicvolume : cuint8;
    sfxvolume : cuint8;
    set_ : cbool;
    notify : Pcbool;
  end;
  TMOD = _mod;
  PMOD = ^_mod;

  union_word = record
  case Integer of
    0:(abyte: record
         high: cuint8;
         low : cuint8;
       end;
      );
    1:(aword : cuint16;);
  end;

  union_dword = record
  case Integer of
    0:(aword: record
         high: cuint16;
         low: cuint16;
       end;
      );
    1:(adword : cuint32;);
  end;

  TCallback = procedure(par0: pointer; par1: pcuint8; par2: cuint32); cdecl;
  _modsndbuf = record
    freq : cuint32;
    fmt : cuint16;
    chans : cuint32;
    samples : f32;
    usr_data : pointer;
    callback : TCallback;
  end;
  MODSNDBUF = _modsndbuf;
  PMODSNDBUF = ^_modsndbuf;

  _modplay = record
    mod_ : TMOD;
    playing : cbool;
    paused : cbool;
    bits : cbool;
    stereo : cbool;
    manual_polling : cbool;
    playfreq : cuint32;
    numSFXChans : cuint32;
    soundBuf : MODSNDBUF;
  end;
  TMODPlay = _modplay;
  PMODPlay = ^_modplay;

var
  semitonetab : array [0..4095] of cuint8; cvar; external;
  freqtab: array [0..0] of cuint16; cvar; external;


procedure MODPlay_Init(mod_: PMODPlay); cdecl; external;
function MODPlay_SetFrequency(mod_: PMODPlay; freq: cuint32): cint32; cdecl; external;
procedure MODPlay_SetStereo(mod_: PMODPlay; stereo: cbool); cdecl; external;
function MODPlay_SetMOD(mod_: PMODPlay; mem: pointer): cint32; cdecl; external;
procedure MODPlay_Unload(mod_: PMODPlay); cdecl; external;
function MODPlay_AllocSFXChannels(mod_: PMODPlay; sfxchans: cuint32): cint32; cdecl; external;
function MODPlay_Start(mod_: PMODPlay): cint32; cdecl; external;
function MODPlay_Stop(mod_: PMODPlay): cint32; cdecl; external;
function MODPlay_TriggerNote(mod_: PMODPlay; chan: cuint32; inst: cuint8; freq: cuint16; vol: cuint8): cint32; cdecl; external;
function MODPlay_Pause(mod_: PMODPlay; par1: cbool): cint32; cdecl; external;
procedure MODPlay_SetVolume(mod_: PMODPlay; musicvolume, sfxvolume: cint32); cdecl; external;

function MOD_SetMOD(par0: PMOD; par1: pcuint8): cint32; cdecl; external;
function MOD_Load(par0: PMOD; par1: pcchar): cint32; cdecl; external;
procedure MOD_Free(par0: PMOD); cdecl; external;
procedure MOD_Start(par0: PMOD); cdecl; external;
function MOD_Player(par0: PMOD): cuint32; cdecl; external;
function MOD_TriggerNote(par0: PMOD; par1: cint32; par2: cuint8;
  par3: cuint16; par4: cuint8): cint32; cdecl; external;
function MOD_AllocSFXChannels(par0: PMOD; par1: cint32): cint32; cdecl; external;
function getNote(par0: PMOD; par1, par2: cint32): cuint16; cdecl; external;
function getInstr(par0: PMOD; par1, par2: cint32): cuint8; cdecl; external;
function getEffect(par0: PMOD; par1, par2: cint32): cuint8; cdecl; external;
function getEffectOp(par0: PMOD; par1, par2: cint32): cuint8; cdecl; external;

function mix_mono_16bit(mod_: PMOD; buf: pcint16; numSamples: cint): cint; cdecl; external;
function mix_stereo_16bit(mod_: PMOD; buf: pcint16; numSamples: cint): cint; cdecl; external;

implementation

initialization
{$linklib aesnd}
{$linklib modplay}
end.
