{ Parsed from ITunesLibrary.framework ITLibMediaItem.h }
{ Created on Sun Mar 8 1:48:57 2015 }


{$ifdef TYPES}
type
  ITLibMediaItemPtr = ^ITLibMediaItem;
{$endif}

{$ifdef TYPES}
const
  ITLibMediaItemMediaKindUnknown = 1;
  ITLibMediaItemMediaKindSong = 2;
  ITLibMediaItemMediaKindMovie = 3;
  ITLibMediaItemMediaKindPodcast = 4;
  ITLibMediaItemMediaKindAudiobook = 5;
  ITLibMediaItemMediaKindPDFBooklet = 6;
  ITLibMediaItemMediaKindMusicVideo = 7;
  ITLibMediaItemMediaKindTVShow = 8;
  ITLibMediaItemMediaKindInteractiveBooklet = 9;
  ITLibMediaItemMediaKindHomeVideo = 12;
  ITLibMediaItemMediaKindRingtone = 14;
  ITLibMediaItemMediaKindDigitalBooklet = 15;
  ITLibMediaItemMediaKindIOSApplication = 16;
  ITLibMediaItemMediaKindVoiceMemo = 17;
  ITLibMediaItemMediaKindiTunesU = 18;
  ITLibMediaItemMediaKindBook = 19;
  ITLibMediaItemMediaKindPDFBook = 20;
  ITLibMediaItemMediaKindAlertTone = 21;

type
  ITLibMediaItemMediaKind = NSUInteger;
  ITLibMediaItemMediaKindPtr = ^ITLibMediaItemMediaKind;

const
  ITLibMediaItemLyricsContentRatingNone = 0;
  ITLibMediaItemLyricsContentRatingExplicit = 1;
  ITLibMediaItemLyricsContentRatingClean = 2;

type
  ITLibMediaItemLyricsContentRating = NSUInteger;
  ITLibMediaItemLyricsContentRatingPtr = ^ITLibMediaItemLyricsContentRating;

const
  ITLibMediaItemLocationTypeUnknown = 0;
  ITLibMediaItemLocationTypeFile = 1;
  ITLibMediaItemLocationTypeURL = 2;
  ITLibMediaItemLocationTypeRemote = 3;

type
  ITLibMediaItemLocationType = NSUInteger;
  ITLibMediaItemLocationTypePtr = ^ITLibMediaItemLocationType;
{$endif}

{$ifdef CLASSES}

type
  ITLibMediaItem = objcclass external (ITLibMediaEntity)
  private
    _data: pointer;
  public
    function title: NSString; message 'title';
    function sortTitle: NSString; message 'sortTitle';
    function artist: ITLibArtist; message 'artist';
    function composer: NSString; message 'composer';
    function sortComposer: NSString; message 'sortComposer';
    function rating: NSInteger; message 'rating';
    function isRatingComputed: ObjCBOOL; message 'isRatingComputed';
    function startTime: NSUInteger; message 'startTime';
    function stopTime: NSUInteger; message 'stopTime';
    function album: ITLibAlbum; message 'album';
    function genre: NSString; message 'genre';
    function kind: NSString; message 'kind';
    function mediaKind: ITLibMediaItemMediaKind; message 'mediaKind';
    function fileSize: culonglong; message 'fileSize';
    function size: NSUInteger; message 'size';
    function totalTime: NSUInteger; message 'totalTime';
    function trackNumber: NSUInteger; message 'trackNumber';
    function category: NSString; message 'category';
    function description: NSString; message 'description';
    function lyricsContentRating: ITLibMediaItemLyricsContentRating; message 'lyricsContentRating';
    function contentRating: NSString; message 'contentRating';
    function modifiedDate: NSDate; message 'modifiedDate';
    function addedDate: NSDate; message 'addedDate';
    function bitrate: NSUInteger; message 'bitrate';
    function sampleRate: NSUInteger; message 'sampleRate';
    function beatsPerMinute: NSUInteger; message 'beatsPerMinute';
    function playCount: NSUInteger; message 'playCount';
    function lastPlayedDate: NSDate; message 'lastPlayedDate';
    function location: NSURL; message 'location';
    function hasArtworkAvailable: ObjCBOOL; message 'hasArtworkAvailable';
    function artwork: ITLibArtwork; message 'artwork';
    function comments: NSString; message 'comments';
    function isPurchased: ObjCBOOL; message 'isPurchased';
    function isCloud: ObjCBOOL; message 'isCloud';
    function isDRMProtected: ObjCBOOL; message 'isDRMProtected';
    function isVideo: ObjCBOOL; message 'isVideo';
    function videoInfo: ITLibMediaItemVideoInfo; message 'videoInfo';
    function releaseDate: NSDate; message 'releaseDate';
    function year: NSUInteger; message 'year';
    function fileType: NSUInteger; message 'fileType';
    function skipCount: NSUInteger; message 'skipCount';
    function skipDate: NSDate; message 'skipDate';
    function voiceOverLanguage: NSString; message 'voiceOverLanguage';
    function volumeAdjustment: NSInteger; message 'volumeAdjustment';
    function volumeNormalizationEnergy: NSUInteger; message 'volumeNormalizationEnergy';
    function isUserDisabled: ObjCBOOL; message 'isUserDisabled';
    function grouping: NSString; message 'grouping';
    function locationType: ITLibMediaItemLocationType; message 'locationType';
  end;
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
var
  ITLibMediaItemPropertyAlbumTitle: NSString; cvar; external;
  ITLibMediaItemPropertySortAlbumTitle: NSString; cvar; external;
  ITLibMediaItemPropertyAlbumArtist: NSString; cvar; external;
  ITLibMediaItemPropertyAlbumRating: NSString; cvar; external;
  ITLibMediaItemPropertyAlbumRatingComputed: NSString; cvar; external;
  ITLibMediaItemPropertySortAlbumArtist: NSString; cvar; external;
  ITLibMediaItemPropertyAlbumIsGapless: NSString; cvar; external;
  ITLibMediaItemPropertyAlbumIsCompilation: NSString; cvar; external;
  ITLibMediaItemPropertyAlbumDiscCount: NSString; cvar; external;
  ITLibMediaItemPropertyAlbumDiscNumber: NSString; cvar; external;
  ITLibMediaItemPropertyAlbumTrackCount: NSString; cvar; external;
  ITLibMediaItemPropertyArtistName: NSString; cvar; external;
  ITLibMediaItemPropertySortArtistName: NSString; cvar; external;
  ITLibMediaItemPropertyVideoIsHD: NSString; cvar; external;
  ITLibMediaItemPropertyVideoWidth: NSString; cvar; external;
  ITLibMediaItemPropertyVideoHeight: NSString; cvar; external;
  ITLibMediaItemPropertyVideoSeries: NSString; cvar; external;
  ITLibMediaItemPropertyVideoSortSeries: NSString; cvar; external;
  ITLibMediaItemPropertyVideoSeason: NSString; cvar; external;
  ITLibMediaItemPropertyVideoEpisode: NSString; cvar; external;
  ITLibMediaItemPropertyVideoEpisodeOrder: NSString; cvar; external;
  ITLibMediaItemPropertyHasArtwork: NSString; cvar; external;
  ITLibMediaItemPropertyBitRate: NSString; cvar; external;
  ITLibMediaItemPropertyBeatsPerMinute: NSString; cvar; external;
  ITLibMediaItemPropertyCategory: NSString; cvar; external;
  ITLibMediaItemPropertyComments: NSString; cvar; external;
  ITLibMediaItemPropertyComposer: NSString; cvar; external;
  ITLibMediaItemPropertySortComposer: NSString; cvar; external;
  ITLibMediaItemPropertyContentRating: NSString; cvar; external;
  ITLibMediaItemPropertyLyricsContentRating: NSString; cvar; external;
  ITLibMediaItemPropertyAddedDate: NSString; cvar; external;
  ITLibMediaItemPropertyModifiedDate: NSString; cvar; external;
  ITLibMediaItemPropertyDescription: NSString; cvar; external;
  ITLibMediaItemPropertyIsUserDisabled: NSString; cvar; external;
  ITLibMediaItemPropertyFileType: NSString; cvar; external;
  ITLibMediaItemPropertyGenre: NSString; cvar; external;
  ITLibMediaItemPropertyGrouping: NSString; cvar; external;
  ITLibMediaItemPropertyIsVideo: NSString; cvar; external;
  ITLibMediaItemPropertyKind: NSString; cvar; external;
  ITLibMediaItemPropertyTitle: NSString; cvar; external;
  ITLibMediaItemPropertySortTitle: NSString; cvar; external;
  ITLibMediaItemPropertyVolumeNormalizationEnergy: NSString; cvar; external;
  ITLibMediaItemPropertyPlayCount: NSString; cvar; external;
  ITLibMediaItemPropertyLastPlayDate: NSString; cvar; external;
  ITLibMediaItemPropertyIsDRMProtected: NSString; cvar; external;
  ITLibMediaItemPropertyIsPurchased: NSString; cvar; external;
  ITLibMediaItemPropertyRating: NSString; cvar; external;
  ITLibMediaItemPropertyRatingComputed: NSString; cvar; external;
  ITLibMediaItemPropertyReleaseDate: NSString; cvar; external;
  ITLibMediaItemPropertySampleRate: NSString; cvar; external;
  ITLibMediaItemPropertySize: NSString; cvar; external;
  ITLibMediaItemPropertyFileSize: NSString; cvar; external;
  ITLibMediaItemPropertyUserSkipCount: NSString; cvar; external;
  ITLibMediaItemPropertySkipDate: NSString; cvar; external;
  ITLibMediaItemPropertyStartTime: NSString; cvar; external;
  ITLibMediaItemPropertyStopTime: NSString; cvar; external;
  ITLibMediaItemPropertyTotalTime: NSString; cvar; external;
  ITLibMediaItemPropertyTrackNumber: NSString; cvar; external;
  ITLibMediaItemPropertyLocationType: NSString; cvar; external;
  ITLibMediaItemPropertyVoiceOverLanguage: NSString; cvar; external;
  ITLibMediaItemPropertyVolumeAdjustment: NSString; cvar; external;
  ITLibMediaItemPropertyYear: NSString; cvar; external;
  ITLibMediaItemPropertyMediaKind: NSString; cvar; external;
  ITLibMediaItemPropertyLocation: NSString; cvar; external;
  ITLibMediaItemPropertyArtwork: NSString; cvar; external;
{$endif}

