(* 
  $Id$
  ------------------------------------------------------------------------------
  	Copyright (C) 2005
      Christer Andersson (c) 2001
      Willem Kokke         
  	
  	This software is provided 'as-is', without any express or implied
  	warranty.  In no event will the authors be held liable for any
  	damages arising from the use of this software.
  	
  	Permission is granted to anyone to use this software for any
  	purpose, including commercial applications, and to alter it and
  	redistribute it freely, subject to the following restrictions:
  	
  	1.	The origin of this software must not be misrepresented; you
  		must not claim that you wrote the original software. If you use
  		this software in a product, an acknowledgment in the product
  		documentation would be appreciated but is not required.
  	
  	2.	Altered source versions must be plainly marked as such, and
  		must not be misrepresented as being the original software.
  	
  	3.	This notice may not be removed or altered from any source
  		distribution.
  ------------------------------------------------------------------------------
    
    
  Conversion by Legolas (http://itaprogaming.free.fr) for freepascal compiler
  (http://www.freepascal.org)
   
  Copyright (C) 2006  Francesco Lombardi
  Check http://sourceforge.net/projects/libndsfpc for updates
   
  ------------------------------------------------------------------------------

  $Log$

*)


{$ifdef GBA_INTERFACE}
// DEFINES ////////
const
  PATTERN_PARAMETER_EMPTY	= 10000;

  SOUND_CHANNEL_COUNT	    = 4;

  SOUND1_PARAMETER_COUNT	= 7;
  SOUND2_PARAMETER_COUNT	= 5;
  SOUND3_PARAMETER_COUNT	= 4;
  SOUND4_PARAMETER_COUNT	= 7;


// Play states
  PLAYSTATE_STOP	= 1;
  PLAYSTATE_PLAY	= 2;
  PLAYSTATE_LOOP	= 4;


// STRUCTS ////////
type
// Iterator for traversing compressed data
  SRLEIterator = record
    pData: pcuchar;
    iValue: cuchar;
    mask: cuchar;
    cValue: cuchar;
    nValue: cshort;
    iValuePos: cuchar;
  end;
  TSRLEIterator = SRLEIterator;
  PSRLEIterator = ^TSRLEIterator;

// Sound 1 pattern
  SSound1Pattern = record
    nLength: cushort;
    apParams: array [0..SOUND1_PARAMETER_COUNT - 1] of pcuchar;
  end;
  TSSound1Pattern = SSound1Pattern;
  PSSound1Pattern = ^SSound1Pattern;

// Sound 2 pattern
  SSound2Pattern = record
    nLength: cushort;
    apParams: array [0..SOUND2_PARAMETER_COUNT-1] of pcuchar;
  end;
  TSSound2Pattern = SSound2Pattern;
  PSSound2Pattern = ^SSound2Pattern;

// Sound 3 pattern
  SSound3Pattern = record
    nLength: cushort;
    apParams: array [0..SOUND3_PARAMETER_COUNT-1] of pcuchar;
  end;
  TSSound3Pattern = SSound3Pattern;
  PSSound3Pattern = ^SSound3Pattern;

// Sound 4 pattern
  SSound4Pattern = record
    nLength: cushort;
    apParams: array [0..SOUND4_PARAMETER_COUNT-1] of pcuchar;
  end;
  TSSound4Pattern = SSound4Pattern;
  PSSound4Pattern = ^SSound4Pattern;

// PROTOTYPES /////

procedure RLEISet(pData: pcuchar; pRLEIterator: PSRLEIterator); cdecl; external;
procedure RLEINext(pRLEIterator: PSRLEIterator); cdecl; external;

procedure BoyScoutInitialize(); cdecl; external;
procedure BoyScoutOpenSong(const pSongData: pcuchar); cdecl; external;
procedure BoyScoutPlaySong(nLoop: cint); cdecl; external;
procedure BoyScoutStopSong(); cdecl; external;
function BoyScoutGetNeededSongMemory(const pSongData: pcuchar): cushort; cdecl; external;
procedure BoyScoutSetMemoryArea(nMemoryAddress: cuint); cdecl; external;
function BoyScoutGetMemoryArea(): cuint; cdecl; external;
function BoyScoutUpdateSong(): cint; cdecl; external;

procedure DMA3Copy32(Src: cuint; Dst: cuint; Count: cuint); cdecl; external;

// WK Set to 1 to use dma, or to 0 to use software copy
// Software copy is the preffered option, it's faster and more stable
const
  USE_DMA = 0;

// WK  some functions to manipulate the speed of the original song
// You directly manipulate the time a beat takes
// a beat takes x/60, where x is the value you give
// increasing this value slows the song down, decreasing speeds it up

// TODO?  move the timing to a timer to get a better control through increades granularity

function BoyScoutGetNormalSpeed(): cuchar; cdecl; external;
function BoyScoutGetSpeed(): cuchar; cdecl; external;

procedure BoyScoutIncSpeed(speed: cuchar); cdecl; external;
procedure BoyScoutDecSpeed(speed: cuchar); cdecl; external;
procedure BoyScoutSetSpeed(speed: cuchar); cdecl; external;


// WK  some functions to mute individual channels
procedure BoyScoutMuteChannel1(mute: cint); cdecl; external;
procedure BoyScoutMuteChannel2(mute: cint); cdecl; external;
procedure BoyScoutMuteChannel3(mute: cint); cdecl; external;
procedure BoyScoutMuteChannel4(mute: cint); cdecl; external;
{$endif GBA_INTERFACE}

