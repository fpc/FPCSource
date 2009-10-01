{ Parsed from Foundation.framework NSHFSFileTypes.h }
{ Version FrameworkParser: 1.3. PasCocoa 0.3, Objective-P 0.4 - Thu Oct 1 18:54:06 ICT 2009 }


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
function NSFileTypeForHFSTypeCode(hfsFileTypeCode: OSType): NSString; cdecl; external name 'NSFileTypeForHFSTypeCode';
function NSHFSTypeCodeFromFileType(fileTypeStringPointer: NSString): OSType; cdecl; external name 'NSHFSTypeCodeFromFileType';
function NSHFSTypeOfFile(fullFilePathPointer: NSString): NSString; cdecl; external name 'NSHFSTypeOfFile';

{$endif}
{$endif}

{$ifdef EXTERNAL_SYMBOLS}
{$ifndef NSHFSFILETYPES_PAS_S}
{$define NSHFSFILETYPES_PAS_S}

{$endif}
{$endif}
