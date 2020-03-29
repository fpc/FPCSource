{ Parsed from ITunesLibrary.framework ITLibMediaItemVideoInfo.h }
{ Created on Sun Mar 8 1:48:57 2015 }


{$ifdef TYPES}
type
  ITLibMediaItemVideoInfoPtr = ^ITLibMediaItemVideoInfo;
{$endif}

{$ifdef CLASSES}

type
  ITLibMediaItemVideoInfo = objcclass external (NSObject)
  private
    _series: NSString;
    _sortSeries: NSString;
    _season: NSUInteger;
    _episode: NSString;
    _episodeOrder: NSInteger;
    _hd: ObjCBOOL;
    _videoWidth: NSUInteger;
    _videoHeight: NSUInteger;
  public
    function series: NSString; message 'series';
    function sortSeries: NSString; message 'sortSeries';
    function season: NSUInteger; message 'season';
    function episode: NSString; message 'episode';
    function episodeOrder: NSInteger; message 'episodeOrder';
    function isHD: ObjCBOOL; message 'isHD';
    function videoWidth: NSUInteger; message 'videoWidth';
    function videoHeight: NSUInteger; message 'videoHeight';
  end;
{$endif}

