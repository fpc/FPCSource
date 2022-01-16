{ Parsed from ITunesLibrary.framework ITLibArtist.h }
{ Created on Sun Mar 8 1:48:57 2015 }


{$ifdef TYPES}
type
  ITLibArtistPtr = ^ITLibArtist;
{$endif}

{$ifdef CLASSES}

type
  ITLibArtist = objcclass external (NSObject)
  private
    _name: NSString;
    _sortName: NSString;
  public
    function name: NSString; message 'name';
    function sortName: NSString; message 'sortName';
  end;
{$endif}

