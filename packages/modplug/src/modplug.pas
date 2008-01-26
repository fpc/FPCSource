{
  Translation of the libmodplug headers for FreePascal
  Copyright (C) 2006 by Ivo Steinmann
}

(*
 * This source code is public domain.
 *
 * Authors: Kenton Varda <temporal@gauge3d.org> (C interface wrapper)
 *)

unit modplug;

{$mode objfpc}
{$MINENUMSIZE 4}

interface

uses
  ctypes;

{$IFDEF WINDOWS}
  {$DEFINE DYNLINK}
{$ENDIF}

//{$DEFINE DYNLINK}

{$IFDEF DYNLINK}
const
{$IF Defined(WINDOWS)}
  modpluglib = 'libmodplug.dll';
{$ELSEIF Defined(UNIX)}
  modpluglib = 'libmodplug.so';
{$ELSE}
  {$MESSAGE ERROR 'DYNLINK not supported'}
{$IFEND}
{$ELSE}
  //{$LINKLIB stdc++}
  {$LINKLIB modplug}
{$ENDIF}


type
  PModPlugFile = ^ModPlugFile;
  ModPlugFile = record
  end;

(* Load a mod file.  [data] should point to a block of memory containing the complete
 * file, and [size] should be the size of that block.
 * Return the loaded mod file on success, or NULL on failure. *)
function ModPlug_Load(data: pointer; size: cint): PModPlugFile; cdecl; external {$IFDEF DYNLINK}modpluglib{$ENDIF};

(* Unload a mod file. *)
procedure ModPlug_Unload(_file: PModPlugFile); cdecl; external {$IFDEF DYNLINK}modpluglib{$ENDIF};

(* Read sample data into the buffer.  Returns the number of bytes read.  If the end
 * of the mod has been reached, zero is returned. *)
function ModPlug_Read(_file: PModPlugFile; buffer: pointer; size: cint): cint; cdecl; external {$IFDEF DYNLINK}modpluglib{$ENDIF};

(* Get the name of the mod.  The returned buffer is stored within the ModPlugFile
 * structure and will remain valid until you unload the file. *)
function ModPlug_GetName(_file: PModPlugFile): pcchar; cdecl; external {$IFDEF DYNLINK}modpluglib{$ENDIF};

(* Get the length of the mod, in milliseconds.  Note that this result is not always
 * accurate, especially in the case of mods with loops. *)
function ModPlug_GetLength(_file: PModPlugFile): cint; cdecl; external {$IFDEF DYNLINK}modpluglib{$ENDIF};

(* Seek to a particular position in the song.  Note that seeking and MODs don't mix very
 * well.  Some mods will be missing instruments for a short time after a seek, as ModPlug
 * does not scan the sequence backwards to find out which instruments were supposed to be
 * playing at that time.  (Doing so would be difficult and not very reliable.)  Also,
 * note that seeking is not very exact in some mods -- especially those for which
 * ModPlug_GetLength() does not report the full length. *)
procedure ModPlug_Seek(_file: PModPlugFile; millisecond: cint); cdecl; external {$IFDEF DYNLINK}modpluglib{$ENDIF};


const
// _ModPlug_Flags
  MODPLUG_ENABLE_OVERSAMPLING     = 1 shl 0;  (* Enable oversampling (highly recommended) *)
  MODPLUG_ENABLE_NOISE_REDUCTION  = 1 shl 1;  (* Enable noise reduction *)
  MODPLUG_ENABLE_REVERB           = 1 shl 2;  (* Enable reverb *)
  MODPLUG_ENABLE_MEGABASS         = 1 shl 3;  (* Enable megabass *)
  MODPLUG_ENABLE_SURROUND         = 1 shl 4;  (* Enable surround sound. *)

// _ModPlug_ResamplingMode
  MODPLUG_RESAMPLE_NEAREST = 0;  (* No interpolation (very fast, extremely bad sound quality) *)
  MODPLUG_RESAMPLE_LINEAR  = 1;  (* Linear interpolation (fast, good quality) *)
  MODPLUG_RESAMPLE_SPLINE  = 2;  (* Cubic spline interpolation (high quality) *)
  MODPLUG_RESAMPLE_FIR     = 3;  (* 8-tap fir filter (extremely high quality) *)

type
  PModPlug_Settings = ^ModPlug_Settings;
  ModPlug_Settings = record
    mFlags          : cint;  (* One or more of the MODPLUG_ENABLE_* flags above, bitwise-OR'ed *)

    (* Note that ModPlug always decodes sound at 44100kHz, 32 bit, stereo and then
     * down-mixes to the settings you choose. *)
    mChannels       : cint;  (* Number of channels - 1 for mono or 2 for stereo *)
    mBits           : cint;  (* Bits per sample - 8, 16, or 32 *)
    mFrequency      : cint;  (* Sampling rate - 11025, 22050, or 44100 *)
    mResamplingMode : cint;  (* One of MODPLUG_RESAMPLE_*, above *)

    mReverbDepth    : cint;  (* Reverb level 0(quiet)-100(loud)      *)
    mReverbDelay    : cint;  (* Reverb delay in ms, usually 40-200ms *)
    mBassAmount     : cint;  (* XBass level 0(quiet)-100(loud)       *)
    mBassRange      : cint;  (* XBass cutoff in Hz 10-100            *)
    mSurroundDepth  : cint;  (* Surround level 0(quiet)-100(heavy)   *)
    mSurroundDelay  : cint;  (* Surround delay in ms, usually 5-40ms *)
    mLoopCount      : cint;  (* Number of times to loop.  Zero prevents looping. -1 loops forever. *)
  end;

(* Get and set the mod decoder settings.  All options, except for channels, bits-per-sample,
 * sampling rate, and loop count, will take effect immediately.  Those options which don't
 * take effect immediately will take effect the next time you load a mod. *)
procedure ModPlug_GetSettings(settings: PModPlug_Settings); cdecl; external {$IFDEF DYNLINK}modpluglib{$ENDIF};
procedure ModPlug_SetSettings(const settings: PModPlug_Settings); cdecl; external {$IFDEF DYNLINK}modpluglib{$ENDIF};

implementation

function cppNew(s: cint): pointer; cdecl; public; alias : '_Znaj'; alias : '_Znwj';
begin
  GetMem(Result, s);
end;

procedure cppDelete(p: pointer); cdecl; public; alias : '_ZdlPv'; alias : '_ZdaPv';
begin
  FreeMem(p);
end;

end.