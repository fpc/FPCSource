{ Parsed from CoreImage.framework CIRAWFilter.h }

{$ifdef CLASSES}

type
  CIRAWFilter = objccategory external (CIFilter)
    class function filterWithImageURL_options (url: NSURL; options: NSDictionary): CIFilter; message 'filterWithImageURL:options:';
    class function filterWithImageData_options (data: NSData; options: NSDictionary): CIFilter; message 'filterWithImageData:options:';
  end;
{$endif}

