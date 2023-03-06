{$mode delphi}
{$modeswitch objectivec1}
{$modeswitch cvar}
{$packrecords c}

{$IFNDEF FPC_DOTTEDUNITS}
unit DefinedClassesiTunesLibrary;
{$ENDIF FPC_DOTTEDUNITS}
interface

type
  ITLibAlbum = objcclass external;
  ITLibArtist = objcclass external;
  ITLibArtwork = objcclass external;
  ITLibMediaEntity = objcclass external;
  ITLibMediaItem = objcclass external;
  ITLibMediaItemVideoInfo = objcclass external;
  ITLibPlaylist = objcclass external;
  ITLibrary = objcclass external;

type
  ITLibLibraryData = objcclass external;

implementation
end.
