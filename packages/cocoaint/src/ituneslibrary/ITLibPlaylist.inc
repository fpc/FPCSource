{ Parsed from ITunesLibrary.framework ITLibPlaylist.h }
{ Created on Sun Mar 8 1:48:57 2015 }


{$ifdef TYPES}
type
  ITLibPlaylistPtr = ^ITLibPlaylist;
{$endif}

{$ifdef TYPES}
const
  ITLibDistinguishedPlaylistKindNone = 0;
  ITLibDistinguishedPlaylistKindMovies = 1;
  ITLibDistinguishedPlaylistKindTVShows = 2;
  ITLibDistinguishedPlaylistKindMusic = 3;
  ITLibDistinguishedPlaylistKindBooks = 4;
  ITLibDistinguishedPlaylistKindRingtones = 5;
  ITLibDistinguishedPlaylistKindPodcasts = 7;
  ITLibDistinguishedPlaylistKindVoiceMemos = 14;
  ITLibDistinguishedPlaylistKindPurchases = 16;
  ITLibDistinguishedPlaylistKindiTunesU = 26;
  ITLibDistinguishedPlaylistKind90sMusic = 42;
  ITLibDistinguishedPlaylistKindMyTopRated = 43;
  ITLibDistinguishedPlaylistKindTop25MostPlayed = 44;
  ITLibDistinguishedPlaylistKindRecentlyPlayed = 45;
  ITLibDistinguishedPlaylistKindRecentlyAdded = 46;
  ITLibDistinguishedPlaylistKindMusicVideos = 47;
  ITLibDistinguishedPlaylistKindClassicalMusic = 48;
  ITLibDistinguishedPlaylistKindLibraryMusicVideos = 49;
  ITLibDistinguishedPlaylistKindHomeVideos = 50;
  ITLibDistinguishedPlaylistKindApplications = 51;

type
  ITLibDistinguishedPlaylistKind = NSUInteger;
  ITLibDistinguishedPlaylistKindPtr = ^ITLibDistinguishedPlaylistKind;

const
  ITLibPlaylistKindRegular = 0;
  ITLibPlaylistKindSmart = 1;
  ITLibPlaylistKindGenius = 2;
  ITLibPlaylistKindFolder = 3;
  ITLibPlaylistKindGeniusMix = 4;

type
  ITLibPlaylistKind = NSUInteger;
  ITLibPlaylistKindPtr = ^ITLibPlaylistKind;
{$endif}

{$ifdef CLASSES}

type
  ITLibPlaylist = objcclass external (ITLibMediaEntity)
  private
    _items: NSMutableArray;
  public
    function name: NSString; message 'name';
    function isMaster: ObjCBOOL; message 'isMaster';
    function parentID: NSNumber; message 'parentID';
    function isVisible: ObjCBOOL; message 'isVisible';
    function isAllItemsPlaylist: ObjCBOOL; message 'isAllItemsPlaylist';
    function items: NSArray; message 'items';
    function distinguishedKind: ITLibDistinguishedPlaylistKind; message 'distinguishedKind';
    function kind: ITLibPlaylistKind; message 'kind';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  ITLibPlaylistPropertyName: NSString; cvar; external;
  ITLibPlaylistPropertyAllItemsPlaylist: NSString; cvar; external;
  ITLibPlaylistPropertyDistinguisedKind: NSString; cvar; external;
  ITLibPlaylistPropertyMaster: NSString; cvar; external;
  ITLibPlaylistPropertyParentPersistentID: NSString; cvar; external;
  ITLibPlaylistPropertyVisible: NSString; cvar; external;
  ITLibPlaylistPropertyItems: NSString; cvar; external;
  ITLibPlaylistPropertyKind: NSString; cvar; external;
{$endif}

