{ Parsed from Foundation.framework NSHFSFileTypes.h }

{$ifdef TYPES}
{$ifndef NSHFSFILETYPES_PAS_T}
{$define NSHFSFILETYPES_PAS_T}

{$endif}
{$endif}

{$ifdef RECORDS}
{$ifndef NSHFSFILETYPES_PAS_R}
{$define NSHFSFILETYPES_PAS_R}

{$endif}
{$endif}

{$ifdef FUNCTIONS}
{$ifndef NSHFSFILETYPES_PAS_F}
{$define NSHFSFILETYPES_PAS_F}

{ Functions }
function NSFileTypeForHFSTypeCode(hfsFileTypeCode: OSType): NSString; cdecl; external;
function NSHFSTypeCodeFromFileType(fileTypeString: NSString): OSType; cdecl; external;
function NSHFSTypeOfFile(fullFilePath: NSString): NSString; cdecl; external;

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSHFSFILETYPES_PAS_S}
{$define NSHFSFILETYPES_PAS_S}

{$endif}
{$endif}
