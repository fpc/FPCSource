unit sdl_sound;
{
  $Id: sdl_sound.pas,v 1.5 2004/12/23 23:38:40 savage Exp $
  
}
(*
 * SDL_sound -- An abstract sound format decoding API.
 * Copyright (C) 2001  Ryan C. Gordon.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)
(**
 * @overview
 *
 * The basic gist of SDL_sound is that you use an SDL_RWops to get sound data
 *  into this library, and SDL_sound will take that data, in one of several
 *  popular formats, and decode it into raw waveform data in the format of
 *  your choice. This gives you a nice abstraction for getting sound into your
 *  game or application; just feed it to SDL_sound, and it will handle
 *  decoding and converting, so you can just pass it to your SDL audio
 *  callback (or whatever). Since it gets data from an SDL_RWops, you can get
 *  the initial sound data from any number of sources: file, memory buffer,
 *  network connection, etc.
 *
 * As the name implies, this library depends on SDL: Simple Directmedia Layer,
 *  which is a powerful, free, and cross-platform multimedia library. It can
 *  be found at http://www.libsdl.org/
 *
 * Support is in place or planned for the following sound formats:
 *   - .WAV  (Microsoft WAVfile RIFF data, internal.)
 *   - .VOC  (Creative Labs' Voice format, internal.)
 *   - .MP3  (MPEG-1 Layer 3 support, via the SMPEG library.)
 *   - .MID  (MIDI music converted to Waveform data, internal.)
 *   - .MOD  (MOD files, via MikMod and ModPlug.)
 *   - .OGG  (Ogg files, via Ogg Vorbis libraries.)
 *   - .SHN  (Shorten files, internal.)
 *   - .RAW  (Raw sound data in any format, internal.)
 *   - .AU   (Sun's Audio format, internal.)
 *   - .AIFF (Audio Interchange format, internal.)
 *   - .FLAC (Lossless audio compression, via libFLAC.)
 *
 *   (...and more to come...)
 *
 * Please see the file COPYING in the source's root directory.
 *
 *  This file written by Ryan C. Gordon. (icculus@clutteredmind.org)
 *)
{   April   03 2003 - DL : Added jedi-sdl.inc include file to support more     }
{                          Pascal compilers. Initial support is now included   }
{                          for GnuPascal, VirtualPascal, TMT and obviously     }
{                          continue support for Delphi Kylix and FreePascal.   }
{                                                                              }
{   May     03 2003 - DL : under instruction from David Mears AKA              }
{                          Jason Siletto, I have added FPC Linux support.      }
{                                                                              }
{
  $Log: sdl_sound.pas,v $
  Revision 1.5  2004/12/23 23:38:40  savage
  Applied Patches supplied by Michalis Kamburelis ( THANKS! ), for tidier code.

  Revision 1.4  2004/08/14 22:54:30  savage
  Updated so that Library name defines are correctly defined for MacOS X.

  Revision 1.3  2004/05/10 14:10:04  savage
  Initial MacOS X support. Fixed defines for MACOS ( Classic ) and DARWIN ( MacOS X ).

  Revision 1.2  2004/03/30 20:23:28  savage
  Tidied up use of UNIX compiler directive.

  Revision 1.1  2004/02/16 22:16:40  savage
  v1.0 changes

  
}
{******************************************************************************}

{$I jedi-sdl.inc}

interface

uses
{$IFDEF WIN32}
  Windows,
{$ENDIF}
  sdl;

const
{$IFDEF WIN32}
  SDLSoundLibName = 'SDL_sound.dll';
{$ENDIF}

{$IFDEF UNIX}
{$IFDEF DARWIN}
  SDLSoundLibName = 'libSDL_sound.dylib';
{$ELSE}
  SDLSoundLibName = 'libSDL_sound.so';
{$ENDIF}
{$ENDIF}

{$IFDEF MACOS}
  SDLSoundLibName = 'SDL_sound';
{$ENDIF}

  SOUND_VER_MAJOR = 0;
{$EXTERNALSYM SOUND_VER_MAJOR}
  SOUND_VER_MINOR = 1;
{$EXTERNALSYM SOUND_VER_MINOR}
  SOUND_VER_PATCH = 5;
{$EXTERNALSYM SOUND_VER_PATCH}

  (**
   * These are flags that are used in a Sound_Sample to show various states.
   *
   *   To use: 'if (sample.flags  and SOUND_SAMPLEFLAG_ERROR) begin dosomething; end;'
   *
   *  @param SOUND_SAMPLEFLAG_NONE     nil flag.
   *  @param SOUND_SAMPLEFLAG_NEEDSEEK SDL_RWops must be able to seek.
   *  @param SOUND_SAMPLEFLAG_EOF      end of input stream.
   *  @param SOUND_SAMPLEFLAG_ERROR    unrecoverable error.
   *  @param SOUND_SAMPLEFLAG_EAGAIN   function would block, or temp error.
   *)
  SOUND_SAMPLEFLAG_NONE = 0;
{$EXTERNALSYM SOUND_SAMPLEFLAG_NONE}
  (* these are set at sample creation time... *)
  SOUND_SAMPLEFLAG_NEEDSEEK = 1;
{$EXTERNALSYM SOUND_SAMPLEFLAG_NEEDSEEK}
  (* these are set during decoding... *)
  SOUND_SAMPLEFLAG_EOF = 1 shl 29;
{$EXTERNALSYM SOUND_SAMPLEFLAG_EOF}
  SOUND_SAMPLEFLAG_ERROR = 1 shl 30;
{$EXTERNALSYM SOUND_SAMPLEFLAG_ERROR}
  SOUND_SAMPLEFLAG_EAGAIN = 1 shl 31;
{$EXTERNALSYM SOUND_SAMPLEFLAG_EAGAIN}

  (**
   * These are the basics of a decoded sample's data type = recordure: data format
   *  (see AUDIO_U8 and friends in SDL_audio.h), number of channels, and sample
   *  rate. If you need more explanation than that, you should stop developing
   *  sound code right now.
   *
   *   @param format Equivalent of SDL_AudioSpec.format.
   *   @param channels Number of sound channels. 1 = mono, 2 = stereo.
   *   @param rate Sample rate; frequency of sample points per second (44100,
   *                22050, 8000, etc.)
   *)
type
  TSound_SampleFlags = Integer;

  PSound_AudioInfo = ^Sound_AudioInfo;
  Sound_AudioInfo = record
    format: Uint16;
    channels: Uint8;
    rate: Uint32;
  end;
  TSound_AudioInfo = Sound_AudioInfo;
{$EXTERNALSYM Sound_AudioInfo}

  (**
   * Each decoder sets up one of these type = records, which can be retrieved via
   *  the Sound_AvailableDecoders function. EVERY FIELD IN THIS IS READ-ONLY.
   *
   *   @param extensions File extensions, list ends with nil. Read it like this:
   *           const  : PChar *ext;
   *           for (ext := info.extensions; *ext <> nil; ext++)
   *                printf('   File extension \'%s\'', *ext);
   *   @param description Human readable description of decoder.
   *   @param author 'Name Of Author <email@emailhost.dom>'
   *   @param url URL specific to this decoder.
   *)
  PSound_DecoderInfo = ^Sound_DecoderInfo;
  Sound_DecoderInfo = record
    extensions: PChar;
    description: PChar;
    author: PChar;
    url: PChar;
  end;
  TSound_DecoderInfo = Sound_DecoderInfo;
{$EXTERNALSYM Sound_DecoderInfo}

  (**
   * The Sound_Sample type = recordure is the heart of SDL_sound. This holds
   *  information about a source of sound data as it is being decoded.
   *  EVERY FIELD IN THIS IS READ-ONLY. Please use the API functions to
   *  change them.
   *
   *  @param opaque Internal use only. Don't touch.
   *  @param decoder Decoder used for this sample.
   *  @param desired Desired audio format for conversion.
   *  @param actual Actual audio format of sample.
   *  @param buffer Decoded sound data lands in here.
   *  @param buffer_size Current size of (buffer), in bytes (Uint8).
   *  @param flags Flags relating to this sample.
   *)
  PSound_Sample = ^Sound_Sample;
  Sound_Sample = record
    opaque: Pointer;
    decoder: PSound_DecoderInfo;
    desired: Sound_AudioInfo;
    actual: Sound_AudioInfo;
    buffer: Pointer;
    buffer_size: Uint32;
    flags: TSound_SampleFlags;
  end;
  TSound_Sample = Sound_Sample;
{$EXTERNALSYM Sound_Sample}

  (**
   * Just what it says: a major.minor.patch style version number...
   *
   *   @param major The major version number.
   *   @param minor The minor version number.
   *   @param patch The patchlevel version number.
   *)
  PSound_Version = ^Sound_Version;
  Sound_Version = record
    major: integer;
    minor: integer;
    patch: integer;
  end;
  TSound_Version = Sound_Version;
{$EXTERNALSYM Sound_Version}

  (* functions and macros... *)

procedure SOUND_GETVERSION(var x: TSound_Version);

(**
 * Get the version of SDL_sound that is linked against your program. If you
 *  are using a shared library (DLL) version of SDL_sound, then it is possible
 *  that it will be different than the version you compiled against.
 *
 * This is a real function; the macro SOUND_VERSION tells you what version
 *  of SDL_sound you compiled against:
 *
 * Sound_Version compiled;
 * Sound_Version linked;
 *
 * SOUND_VERSION(@compiled);
 * Sound_GetLinkedVersion(@linked);
 * printf('We compiled against SDL_sound version %d.%d.%d ...',
 *           compiled.major, compiled.minor, compiled.patch);
 * printf('But we linked against SDL_sound version %d.%d.%d.',
 *           linked.major, linked.minor, linked.patch);
 *
 * This function may be called safely at any time, even before Sound_Init.
 *
 * @param ver Sound_Version type = recordure to fill with shared library's version.
 *)
procedure Sound_GetLinkedVersion(ver: PSound_Version);
cdecl; external {$IFDEF __GPC__}name 'Sound_GetLinkedVersion'{$ELSE} SDLSoundLibName{$ENDIF __GPC__};
{$EXTERNALSYM Sound_GetLinkedVersion}

(**
 * Initialize SDL_sound. This must be called before any other SDL_sound
 *  function (except perhaps Sound_GetLinkedVersion). You should call
 *  SDL_Init before calling this. Sound_Init will attempt to call
 *  SDL_Init(SDL_INIT_AUDIO), just in . This is a safe behaviour, but it
 *  may not configure SDL to your liking by itself.
 *
 *  @returns nonzero on success, zero on error. Specifics of the
 *           error can be gleaned from Sound_GetError.
 *)
function Sound_Init: integer;
cdecl; external {$IFDEF __GPC__}name 'Sound_Init'{$ELSE} SDLSoundLibName{$ENDIF __GPC__};
{$EXTERNALSYM Sound_Init}

(**
 * Shutdown SDL_sound. This closes any SDL_RWops that were being used as
 *  sound sources, and frees any resources in use by SDL_sound.
 *
 * All Sound_Sample pointers you had prior to this call are INVALIDATED.
 *
 * Once successfully deinitialized, Sound_Init can be called again to
 *  restart the subsystem. All default API states are restored at this
 *  point.
 *
 * You should call this BEFORE SDL_Quit. This will NOT call SDL_Quit
 *  for younot
 *
 *  @returns nonzero on success, zero on error. Specifics of the error
 *           can be gleaned from Sound_GetError. If failure, state of
 *           SDL_sound is undefined, and probably badly screwed up.
 *)
function Sound_Quit: integer;
cdecl; external {$IFDEF __GPC__}name 'Sound_Quit'{$ELSE} SDLSoundLibName{$ENDIF __GPC__};
{$EXTERNALSYM Sound_Quit}

(**
 * Get a list of sound formats supported by this implementation of SDL_sound.
 *  This is for informational purposes only. Note that the extension listed is
 *  merely convention: if we list 'MP3', you can open an MPEG-1 Layer 3 audio
 *  file with an extension of 'XYZ', if you like. The file extensions are
 *  informational, and only required as a h : integer to choosing the correct
 *  decoder, since the sound data may not be coming from a file at all, thanks
 *  to the abstraction that an SDL_RWops provides.
 *
 * The result value is an array of pointers to Sound_DecoderInfo type = recordures,
 *  with a nil entry to signify the end of the list:
 *
 * Sound_DecoderInfo **i;
 *
 * for (i := Sound_AvailableDecoders; *i <> nil; i++)
 * begin
 *     printf('Supported sound format:  : array[ 0..%s- 1 ] of , which is  : array[ 0..%s- 1 ] of .',
 *              i.extension, i.description);
 *     // ...and other fields...
 * end;
 *
 * The return values are pointers to static internal memory, and should
 *  be considered READ ONLY, and never freed.
 *
 *  @returns READ ONLY nil-terminated array of READ ONLY type = recordures.
 *)
function Sound_AvailableDecoders: PSound_DecoderInfo;
cdecl; external {$IFDEF __GPC__}name 'Sound_AvailableDecoders'{$ELSE} SDLSoundLibName{$ENDIF __GPC__};
{$EXTERNALSYM Sound_AvailableDecoders}

(**
 * Get the last SDL_sound error message as a nil-terminated string.
 *  This will be nil if there's been no error since the last call to this
 *  function. The pointer returned by this call points to an internal buffer.
 *  Each thread has a unique error state associated with it, but each time
 *  a new error message is set, it will overwrite the previous one associated
 *  with that thread. It is safe to call this function at anytime, even
 *  before Sound_Init.
 *
 *  @returns READ ONLY string of last error message.
 *)
function Sound_GetError: PChar;
cdecl; external {$IFDEF __GPC__}name 'Sound_GetError'{$ELSE} SDLSoundLibName{$ENDIF __GPC__};
{$EXTERNALSYM Sound_GetError}

(**
 * Clear the current error message, so the next call to Sound_GetError will
 *  return nil.
 *)
procedure Sound_ClearError;
cdecl; external {$IFDEF __GPC__}name 'Sound_ClearError'{$ELSE} SDLSoundLibName{$ENDIF __GPC__};
{$EXTERNALSYM Sound_ClearError}

(**
 * Start decoding a new sound sample. The data is read via an SDL_RWops
 *  type = recordure (see SDL_rwops.h in the SDL include directory), so it may be
 *  coming from memory, disk, network stream, etc. The (ext) parameter is
 *  merely a h : integer to determining the correct decoder; if you specify, for
 *  example, 'mp3' for an extension, and one of the decoders lists that
 *  as a handled extension, then that decoder is given first shot at trying
 *  to claim the data for decoding. If none of the extensions match (or the
 *  extension is nil), then every decoder examines the data to determine if
 *  it can handle it, until one accepts it. In such a  your SDL_RWops will
 *  need to be capable of rewinding to the start of the stream.
 * If no decoders can handle the data, a nil value is returned, and a human
 *  readable error message can be fetched from Sound_GetError.
 * Optionally, a desired audio format can be specified. If the incoming data
 *  is in a different format, SDL_sound will convert it to the desired format
 *  on the fly. Note that this can be an expensive operation, so it may be
 *  wise to convert data before you need to play it back, if possible, or
 *  make sure your data is initially in the format that you need it in.
 *  If you don't want to convert the data, you can specify nil for a desired
 *  format. The incoming format of the data, preconversion, can be found
 *  in the Sound_Sample type = recordure.
 * Note that the raw sound data 'decoder' needs you to specify both the
 *  extension 'RAW' and a 'desired' format, or it will refuse to handle
 *  the data. This is to prevent it from catching all formats unsupported
 *  by the other decoders.
 * Finally, specify an initial buffer size; this is the number of bytes that
 *  will be allocated to store each read from the sound buffer. The more you
 *  can safely allocate, the more decoding can be done in one block, but the
 *  more resources you have to use up, and the longer each decoding call will
 *  take. Note that different data formats require more or less space to
 *  store. This buffer can be resized via Sound_SetBufferSize ...
 * The buffer size specified must be a multiple of the size of a single
 *  sample point. So, if you want 16-bit, stereo samples, then your sample
 *  po : integer size is (2 channels * 16 bits), or 32 bits per sample, which is four
 *  bytes. In such a , you could specify 128 or 132 bytes for a buffer,
 *  but not 129, 130, or 131 (although in reality, you'll want to specify a
 *  MUCH larger buffer).
 * When you are done with this Sound_Sample pointer, you can dispose of it
 *  via Sound_FreeSample.
 * You do not have to keep a reference to (rw) around. If this function
 *  suceeds, it stores (rw) internally (and disposes of it during the call
 *  to Sound_FreeSample). If this function fails, it will dispose of the
 *  SDL_RWops for you.
 *
 *    @param rw SDL_RWops with sound data.
 *    @param ext File extension normally associated with a data format.
 *               Can usually be nil.
 *    @param desired Format to convert sound data into. Can usually be nil,
 *                   if you don't need conversion.
 *   @returns Sound_Sample pointer, which is used as a handle to several other
 *            SDL_sound APIs. nil on error. If error, use
 *            Sound_GetError to see what went wrong.
 *)
function Sound_NewSample(rw: PSDL_RWops; const ext: PChar;
  desired: PSound_AudioInfo; bufferSize: Uint32): PSound_Sample;
cdecl; external {$IFDEF __GPC__}name 'Sound_NewSample'{$ELSE} SDLSoundLibName{$ENDIF __GPC__};
{$EXTERNALSYM Sound_NewSample}

(**
 * This is identical to Sound_NewSample, but it creates an SDL_RWops for you
 *  from the file located in (filename). Note that (filename) is specified in
 *  platform-dependent notation. ('C:\\music\\mysong.mp3' on windows, and
 *  '/home/icculus/music/mysong.mp3' or whatever on Unix, etc.)
 * Sound_NewSample's 'ext' parameter is gleaned from the contents of
 *  (filename).
 *
 *    @param filename file containing sound data.
 *    @param desired Format to convert sound data into. Can usually be nil,
 *                   if you don't need conversion.
 *    @param bufferSize size, in bytes, of initial read buffer.
 *   @returns Sound_Sample pointer, which is used as a handle to several other
 *            SDL_sound APIs. nil on error. If error, use
 *            Sound_GetError to see what went wrong.
 *)
function Sound_NewSampleFromFile(const filename: PChar; desired:
  PSound_AudioInfo; bufferSize: Uint32): PSound_Sample;
cdecl; external {$IFDEF __GPC__}name 'Sound_NewSampleFromFile'{$ELSE} SDLSoundLibName{$ENDIF __GPC__};
{$EXTERNALSYM Sound_NewSampleFromFile}

(**
 * Dispose of a Sound_Sample pointer that was returned from Sound_NewSample.
 *  This will also close/dispose of the SDL_RWops that was used at creation
 *  time, so there's no need to keep a reference to that around.
 * The Sound_Sample pointer is invalid after this call, and will almost
 *  certainly result in a crash if you attempt to keep using it.
 *
 *    @param sample The Sound_Sample to delete.
 *)
procedure Sound_FreeSample(sample: PSound_Sample);
cdecl; external {$IFDEF __GPC__}name 'Sound_FreeSample'{$ELSE} SDLSoundLibName{$ENDIF __GPC__};
{$EXTERNALSYM Sound_FreeSample}

(**
 * Change the current buffer size for a sample. If the buffer size could
 *  be changed, then the sample.buffer and sample.buffer_size fields will
 *  reflect that. If they could not be changed, then your original sample
 *  state is preserved. If the buffer is shrinking, the data at the end of
 *  buffer is truncated. If the buffer is growing, the contents of the new
 *  space at the end is undefined until you decode more into it or initialize
 *  it yourself.
 *
 * The buffer size specified must be a multiple of the size of a single
 *  sample point. So, if you want 16-bit, stereo samples, then your sample
 *  po : integer size is (2 channels * 16 bits), or 32 bits per sample, which is four
 *  bytes. In such a , you could specify 128 or 132 bytes for a buffer,
 *  but not 129, 130, or 131 (although in reality, you'll want to specify a
 *  MUCH larger buffer).
 *
 *    @param sample The Sound_Sample whose buffer to modify.
 *    @param new_size The desired size, in bytes, of the new buffer.
 *  @returns non-zero if buffer size changed, zero on failure.
 *)
function Sound_SetBufferSize(sample: PSound_Sample; new_size: Uint32): integer;
cdecl; external {$IFDEF __GPC__}name 'Sound_SetBufferSize'{$ELSE} SDLSoundLibName{$ENDIF __GPC__};
{$EXTERNALSYM Sound_SetBufferSize}

(**
 * Decode more of the sound data in a Sound_Sample. It will decode at most
 *  sample.buffer_size bytes into sample.buffer in the desired format, and
 *  return the number of decoded bytes.
 * If sample.buffer_size bytes could not be decoded, then please refer to
 *  sample.flags to determine if this was an End-of-stream or error condition.
 *
 *    @param sample Do more decoding to this Sound_Sample.
 *  @returns number of bytes decoded into sample.buffer. If it is less than
 *           sample.buffer_size, then you should check sample.flags to see
 *           what the current state of the sample is (EOF, error, read again).
 *)
function Sound_Decode(sample: PSound_Sample): Uint32;
cdecl; external {$IFDEF __GPC__}name 'Sound_Decode'{$ELSE} SDLSoundLibName{$ENDIF __GPC__};
{$EXTERNALSYM Sound_Decode}

(**
 * Decode the remainder of the sound data in a Sound_Sample. This will
 *  dynamically allocate memory for the ENTIRE remaining sample.
 *  sample.buffer_size and sample.buffer will be updated to reflect the
 *  new buffer. Please refer to sample.flags to determine if the decoding
 *  finished due to an End-of-stream or error condition.
 *
 * Be aware that sound data can take a large amount of memory, and that
 *  this function may block for quite awhile while processing. Also note
 *  that a streaming source (for example, from a SDL_RWops that is getting
 *  fed from an Internet radio feed that doesn't end) may fill all available
 *  memory before giving up...be sure to use this on finite sound sources
 *  onlynot
 *
 * When decoding the sample in its entirety, the work is done one buffer at a
 *  time. That is, sound is decoded in sample.buffer_size blocks, and
 *  appended to a continually-growing buffer until the decoding completes.
 *  That means that this function will need enough RAM to hold approximately
 *  sample.buffer_size bytes plus the complete decoded sample at most. The
 *  larger your buffer size, the less overhead this function needs, but beware
 *  the possibility of paging to disk. Best to make this user-configurable if
 *  the sample isn't specific and small.
 *
 *    @param sample Do all decoding for this Sound_Sample.
 *   @returns number of bytes decoded into sample.buffer. You should check
 *           sample.flags to see what the current state of the sample is
 *           (EOF, error, read again).
 *)
function Sound_DecodeAll(sample: PSound_Sample): Uint32;
cdecl; external {$IFDEF __GPC__}name 'Sound_DecodeAll'{$ELSE} SDLSoundLibName{$ENDIF __GPC__};
{$EXTERNALSYM Sound_DecodeAll}

(**
 * Restart a sample at the start of its waveform data, as if newly
 *  created with Sound_NewSample. If successful, the next call to
 *  Sound_Decode : array[ 0..All- 1 ] of  will give audio data from the earliest point
 *  in the stream.
 *
 * Beware that this function will fail if the SDL_RWops that feeds the
 *  decoder can not be rewound via it's seek method, but this can
 *  theoretically be aprocedure ed by wrapping it in some sort of buffering
 *  SDL_RWops.
 *
 * This function should ONLY fail if the RWops is not seekable, or
 *  SDL_sound is not initialized. Both can be controlled by the application,
 *  and thus, it is up to the developer's paranoia to dictate whether this
 *  function's return value need be checked at all.
 *
 * If this function fails, the state of the sample is undefined, but it
 *  is still safe to call Sound_FreeSample to dispose of it.
 *
 * On success, ERROR, EOF, and EAGAIN are cleared from sample.flags. The
 *  ERROR flag is set on error.
 *
 *    @param sample The Sound_Sample to rewind.
 *   @return nonzero on success, zero on error. Specifics of the
 *           error can be gleaned from Sound_GetError.
 *)
function Sound_Rewind(sample: PSound_Sample): integer;
cdecl; external {$IFDEF __GPC__}name 'Sound_Rewind'{$ELSE} SDLSoundLibName{$ENDIF __GPC__};
{$EXTERNALSYM Sound_Rewind}

implementation

{$IFDEF __GPC__}
  {$L sdl_sound}  { link sdl.dll.a or libsdl.so or libsdl.a }
{$ELSE}
{$ENDIF}

procedure SOUND_GETVERSION(var x: TSound_Version);
begin
  x.major := SOUND_VER_MAJOR;
  x.minor := SOUND_VER_MINOR;
  x.patch := SOUND_VER_PATCH;
end;

end.

