{ Parsed from Foundation.framework NSHFSFileTypes.h }

{$ifdef FUNCTIONS}
function NSFileTypeForHFSTypeCode(hfsFileTypeCode: OSType): NSString; cdecl; external;
function NSHFSTypeCodeFromFileType(fileTypeString: NSString): OSType; cdecl; external;
function NSHFSTypeOfFile(fullFilePath: NSString): NSString; cdecl; external;
{$endif}

