{$IFNDEF FPC_DOTTEDUNITS}
unit mp3player;
{$ENDIF FPC_DOTTEDUNITS}
{$mode objfpc} 
{$J+}
{$INLINE ON}
{$MACRO ON}
{$PACKRECORDS C}
{$ASSERTIONS ON}

{$define HW_RVL}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.CTypes, WiiApi.Gctypes, Api.Mad;
{$ELSE FPC_DOTTEDUNITS}
uses
  ctypes, gctypes, mad;
{$ENDIF FPC_DOTTEDUNITS}

type 
  pmad_stream=^mad_stream;
  pmad_frame=^mad_frame;

procedure MP3Player_Init; cdecl; external;
procedure MP3Player_Stop; cdecl; external;
function MP3Player_IsPlaying: cbool; cdecl; external;
procedure MP3Player_Volume(volume: cuint32); cdecl; external;

type
  TCBFilterFunc = procedure(par0: Pmad_stream; par1: Pmad_frame); cdecl;
function MP3Player_PlayBuffer(buffer: pointer; len: cint32; filterfunc: TCBFilterFunc): cint32; cdecl; external;

type
  TCBReader = function(par0, par1: pointer; par2: cint32): cint32; cdecl;
function MP3Player_PlayFile(cb_data: pointer; reader: TCBReader; filterfunc: TCBFilterFunc): cint32; cdecl; external;

implementation

initialization

end.
