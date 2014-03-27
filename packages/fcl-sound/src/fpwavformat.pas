{*****************************************************************************}
{
    This file is part of the Free Pascal's "Free Components Library".
    Copyright (c) 2014 by Mazen NEIFER of the Free Pascal development team
    and was adapted from wavopenal.pas copyright (c) 2010 Dmitry Boyarintsev.

    RIFF/WAVE sound file basic types and constants.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}
unit fpwavformat;

{$mode objfpc}{$H+}

interface

const
  AUDIO_CHUNK_ID_RIFF = 'RIFF';
  AUDIO_CHUNK_ID_WAVE = 'WAVE';
  AUDIO_CHUNK_ID_fmt  = 'fmt ';
  AUDIO_CHUNK_ID_data = 'data';
  AUDIO_FORMAT_PCM = 1;

type
  TChunkID = array [0..3] of char;
  TChunkHeader = packed record
    ID: TChunkID;
    Size: UInt32;
  end;
  TRiffHeader = packed record
    ChunkHeader: TChunkHeader;
    Format: TChunkID;
  end;
  TWaveFormat = packed record
    ChunkHeader: TChunkHeader;
    Format: UInt16;
    Channels: UInt16;
    SampleRate: UInt32;
    ByteRate: UInt32;
    BlockAlign: UInt16;
    BitsPerSample: UInt16;
  end;

implementation

end.

