{	CFURL.h
	Copyright (c) 1998-2005, Apple, Inc. All rights reserved.
}
{	  Pascal Translation Updated:  Peter N Lewis, <peter@stairways.com.au>, November 2005 }
{
    Modified for use with Free Pascal
    Version 210
    Please report any bugs to <gpc@microbizz.nl>
}

{$mode macpas}
{$packenum 1}
{$macro on}
{$inline on}
{$calling mwpascal}

unit CFURL;
interface
{$setc UNIVERSAL_INTERFACES_VERSION := $0342}
{$setc GAP_INTERFACES_VERSION := $0210}

{$ifc not defined USE_CFSTR_CONSTANT_MACROS}
    {$setc USE_CFSTR_CONSTANT_MACROS := TRUE}
{$endc}

{$ifc defined CPUPOWERPC and defined CPUI386}
	{$error Conflicting initial definitions for CPUPOWERPC and CPUI386}
{$endc}
{$ifc defined FPC_BIG_ENDIAN and defined FPC_LITTLE_ENDIAN}
	{$error Conflicting initial definitions for FPC_BIG_ENDIAN and FPC_LITTLE_ENDIAN}
{$endc}

{$ifc not defined __ppc__ and defined CPUPOWERPC}
	{$setc __ppc__ := 1}
{$elsec}
	{$setc __ppc__ := 0}
{$endc}
{$ifc not defined __i386__ and defined CPUI386}
	{$setc __i386__ := 1}
{$elsec}
	{$setc __i386__ := 0}
{$endc}

{$ifc defined __ppc__ and __ppc__ and defined __i386__ and __i386__}
	{$error Conflicting definitions for __ppc__ and __i386__}
{$endc}

{$ifc defined __ppc__ and __ppc__}
	{$setc TARGET_CPU_PPC := TRUE}
	{$setc TARGET_CPU_X86 := FALSE}
{$elifc defined __i386__ and __i386__}
	{$setc TARGET_CPU_PPC := FALSE}
	{$setc TARGET_CPU_X86 := TRUE}
{$elsec}
	{$error Neither __ppc__ nor __i386__ is defined.}
{$endc}
{$setc TARGET_CPU_PPC_64 := FALSE}

{$ifc defined FPC_BIG_ENDIAN}
	{$setc TARGET_RT_BIG_ENDIAN := TRUE}
	{$setc TARGET_RT_LITTLE_ENDIAN := FALSE}
{$elifc defined FPC_LITTLE_ENDIAN}
	{$setc TARGET_RT_BIG_ENDIAN := FALSE}
	{$setc TARGET_RT_LITTLE_ENDIAN := TRUE}
{$elsec}
	{$error Neither FPC_BIG_ENDIAN nor FPC_LITTLE_ENDIAN are defined.}
{$endc}
{$setc ACCESSOR_CALLS_ARE_FUNCTIONS := TRUE}
{$setc CALL_NOT_IN_CARBON := FALSE}
{$setc OLDROUTINENAMES := FALSE}
{$setc OPAQUE_TOOLBOX_STRUCTS := TRUE}
{$setc OPAQUE_UPP_TYPES := TRUE}
{$setc OTCARBONAPPLICATION := TRUE}
{$setc OTKERNEL := FALSE}
{$setc PM_USE_SESSION_APIS := TRUE}
{$setc TARGET_API_MAC_CARBON := TRUE}
{$setc TARGET_API_MAC_OS8 := FALSE}
{$setc TARGET_API_MAC_OSX := TRUE}
{$setc TARGET_CARBON := TRUE}
{$setc TARGET_CPU_68K := FALSE}
{$setc TARGET_CPU_MIPS := FALSE}
{$setc TARGET_CPU_SPARC := FALSE}
{$setc TARGET_OS_MAC := TRUE}
{$setc TARGET_OS_UNIX := FALSE}
{$setc TARGET_OS_WIN32 := FALSE}
{$setc TARGET_RT_MAC_68881 := FALSE}
{$setc TARGET_RT_MAC_CFM := FALSE}
{$setc TARGET_RT_MAC_MACHO := TRUE}
{$setc TYPED_FUNCTION_POINTERS := TRUE}
{$setc TYPE_BOOL := FALSE}
{$setc TYPE_EXTENDED := FALSE}
{$setc TYPE_LONGLONG := TRUE}
uses MacTypes,CFBase,CFData,CFString,Files;
{$ALIGN POWER}


type
	CFURLPathStyle = SInt32;
const
	kCFURLPOSIXPathStyle = 0;
	kCFURLHFSPathStyle = 1;
	kCFURLWindowsPathStyle = 2;
    
type
	CFURLRef = ^SInt32; { an opaque 32-bit type }
	CFURLRefPtr = ^CFURLRef;

{ CFURLs are composed of two fundamental pieces - their string, and a }
{ (possibly NULL) base URL.  A relative URL is one in which the string }
{ by itself does not fully specify the URL (for instance "myDir/image.tiff"); }
{ an absolute URL is one in which the string does fully specify the URL }
{ ("file://localhost/myDir/image.tiff").  Absolute URLs always have NULL }
{ base URLs; however, it is possible for a URL to have a NULL base, and still }
{ not be absolute.  Such a URL has only a relative string, and cannot be }
{ resolved.  Two CFURLs are considered equal if and only if their strings }
{ are equal and their bases are equal.  In other words, }
{ "file://localhost/myDir/image.tiff" is NOT equal to the URL with relative }
{ string "myDir/image.tiff" and base URL "file://localhost/".  Clients that }
{ need these less strict form of equality should convert all URLs to their }
{ absolute form via CFURLCopyAbsoluteURL(), then compare the absolute forms. }

function CFURLGetTypeID: CFTypeID; external name '_CFURLGetTypeID';

{ encoding will be used both to interpret the bytes of URLBytes, and to }
{ interpret any percent-escapes within the bytes. }
function CFURLCreateWithBytes( allocator: CFAllocatorRef; URLBytes: UInt8Ptr; length: CFIndex; encoding: CFStringEncoding; baseURL: CFURLRef ): CFURLRef; external name '_CFURLCreateWithBytes';

{ Escapes any character that is not 7-bit ASCII with the byte-code }
{ for the given encoding.  If escapeWhitespace is true, whitespace }
{ characters (' ', '\t', '\r', '\n') will be escaped also (desirable }
{ if embedding the URL into a larger text stream like HTML) }
function CFURLCreateData( allocator: CFAllocatorRef; url: CFURLRef; encoding: CFStringEncoding; escapeWhitespace: Boolean ): CFDataRef; external name '_CFURLCreateData';

{ Any escape sequences in URLString will be interpreted via UTF-8. }
function CFURLCreateWithString( allocator: CFAllocatorRef; URLString: CFStringRef; baseURL: CFURLRef ): CFURLRef; external name '_CFURLCreateWithString';

{#if MAC_OS_X_VERSION_10_3 <= MAC_OS_X_VERSION_MAX_ALLOWED}

{ Create an absolute URL directly, without requiring the extra step }
{ of calling CFURLCopyAbsoluteURL().  If useCompatibilityMode is  }
{ true, the rules historically used on the web are used to resolve }
{ relativeString against baseURL - these rules are generally listed }
{ in the RFC as optional or alternate interpretations.  Otherwise, }
{ the strict rules from the RFC are used.  The major differences are }
{ that in compatibility mode, we are lenient of the scheme appearing }
{ in relative portion, leading "../" components are removed from the }
{ final URL's path, and if the relative portion contains only }
{ resource specifier pieces (query, parameters, and fragment), then }
{ the last path component of the base URL will not be deleted  }
function CFURLCreateAbsoluteURLWithBytes( alloc: CFAllocatorRef; relativeURLBytes: UInt8Ptr; length: CFIndex; encoding: CFStringEncoding; baseURL: CFURLRef; useCompatibilityMode: Boolean ): CFURLRef; external name '_CFURLCreateAbsoluteURLWithBytes';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)
{#endif}

{ filePath should be the URL's path expressed as a path of the type }
{ fsType.  If filePath is not absolute, the resulting URL will be }
{ considered relative to the current working directory (evaluated }
{ at creation time).  isDirectory determines whether filePath is }
{ treated as a directory path when resolving against relative path }
{ components }
function CFURLCreateWithFileSystemPath( allocator: CFAllocatorRef; filePath: CFStringRef; pathStyle: CFURLPathStyle; isDirectory: Boolean ): CFURLRef; external name '_CFURLCreateWithFileSystemPath';

function CFURLCreateFromFileSystemRepresentation( allocator: CFAllocatorRef; buffer: CStringPtr; bufLen: CFIndex; isDirectory: Boolean ): CFURLRef; external name '_CFURLCreateFromFileSystemRepresentation';

function CFURLCreateWithFileSystemPathRelativeToBase( allocator: CFAllocatorRef; filePath: CFStringRef; pathStyle: CFURLPathStyle; isDirectory: Boolean; baseURL: CFURLRef ): CFURLRef; external name '_CFURLCreateWithFileSystemPathRelativeToBase';

function CFURLCreateFromFileSystemRepresentationRelativeToBase( allocator: CFAllocatorRef; buffer: CStringPtr; bufLen: CFIndex; isDirectory: Boolean; baseURL: CFURLRef ): CFURLRef; external name '_CFURLCreateFromFileSystemRepresentationRelativeToBase';
                                                                         
{ Fills buffer with the file system's native representation of }
{ url's path. No more than maxBufLen bytes are written to buffer. }
{ The buffer should be at least the maximum path length for }
{ the file system in question to avoid failures for insufficiently }
{ large buffers.  If resolveAgainstBase is true, the url's relative }
{ portion is resolved against its base before the path is computed. }
{ Returns success or failure. }
function CFURLGetFileSystemRepresentation( url: CFURLRef; resolveAgainstBase: Boolean; buffer: CStringPtr; maxBufLen: CFIndex ): Boolean; external name '_CFURLGetFileSystemRepresentation';

{ Creates a new URL by resolving the relative portion of relativeURL against its base. }
function CFURLCopyAbsoluteURL( relativeURL: CFURLRef ): CFURLRef; external name '_CFURLCopyAbsoluteURL';

{ Returns the URL's string. }
function CFURLGetString( anURL: CFURLRef ): CFStringRef; external name '_CFURLGetString';

{ Returns the base URL if it exists }
function CFURLGetBaseURL( anURL: CFURLRef ): CFURLRef; external name '_CFURLGetBaseURL';

{
All URLs can be broken into two pieces - the scheme (preceding the
first colon) and the resource specifier (following the first colon).
Most URLs are also "standard" URLs conforming to RFC 1808 (available
from www.w3c.org).  This category includes URLs of the file, http,
https, and ftp schemes, to name a few.  Standard URLs start the
resource specifier with two slashes ("//"), and can be broken into
four distinct pieces - the scheme, the net location, the path, and
further resource specifiers (typically an optional parameter, query,
and/or fragment).  The net location appears immediately following
the two slashes and goes up to the next slash; it's format is
scheme-specific, but is usually composed of some or all of a username,
password, host name, and port.  The path is a series of path components
separated by slashes; if the net location is present, the path always
begins with a slash.  Standard URLs can be relative to another URL,
in which case at least the scheme and possibly other pieces as well
come from the base URL (see RFC 1808 for precise details when resolving
a relative URL against its base).  The full URL is therefore

<scheme> "://" <net location> <path, always starting with slash> <add'l resource specifiers>

If a given CFURL can be decomposed (that is, conforms to RFC 1808), you
can ask for each of the four basic pieces (scheme, net location, path,
and resource specifer) separately, as well as for its base URL.  The
basic pieces are returned with any percent escape sequences still in
place (although note that the scheme may not legally include any
percent escapes); this is to allow the caller to distinguish between
percent sequences that may have syntactic meaning if replaced by the
character being escaped (for instance, a '/' in a path component).
Since only the individual schemes know which characters are
syntactically significant, CFURL cannot safely replace any percent
escape sequences.  However, you can use
CFURLCreateStringByReplacingPercentEscapes() to create a new string with
the percent escapes removed; see below.

If a given CFURL can not be decomposed, you can ask for its scheme and its
resource specifier; asking it for its net location or path will return NULL.

To get more refined information about the components of a decomposable
CFURL, you may ask for more specific pieces of the URL, expressed with
the percent escapes removed.  The available functions are CFURLCopyHostName(),
CFURLGetPortNumber() (returns an Int32), CFURLCopyUserName(),
CFURLCopyPassword(), CFURLCopyQuery(), CFURLCopyParameters(), and
CFURLCopyFragment().  Because the parameters, query, and fragment of an
URL may contain scheme-specific syntaxes, these methods take a second
argument, giving a list of characters which should NOT be replaced if
percent escaped.  For instance, the ftp parameter syntax gives simple
key-value pairs as "<key>=<value>;"  Clearly if a key or value includes
either '=' or ';', it must be escaped to avoid corrupting the meaning of
the parameters, so the caller may request the parameter string as

CFStringRef myParams = CFURLCopyParameters(ftpURL, CFSTR("=;%"));

requesting that all percent escape sequences be replaced by the represented
characters, except for escaped '=', '%' or ';' characters.  Pass the empty
string (CFSTR("")) to request that all percent escapes be replaced, or NULL
to request that none be.
}

{ Returns true if anURL conforms to RFC 1808 }
function CFURLCanBeDecomposed( anURL: CFURLRef ): Boolean; external name '_CFURLCanBeDecomposed';

{ The next several methods leave any percent escape sequences intact }

function CFURLCopyScheme( anURL: CFURLRef ): CFStringRef; external name '_CFURLCopyScheme';

{ NULL if CFURLCanBeDecomposed(anURL) is false }
function CFURLCopyNetLocation( anURL: CFURLRef ): CFStringRef; external name '_CFURLCopyNetLocation';

{ NULL if CFURLCanBeDecomposed(anURL) is false; also does not resolve the URL }
{ against its base.  See also CFURLCopyAbsoluteURL().  Note that, strictly }
{ speaking, any leading '/' is not considered part of the URL's path, although }
{ its presence or absence determines whether the path is absolute. }
{ CFURLCopyPath()'s return value includes any leading slash (giving the path }
{ the normal POSIX appearance); CFURLCopyStrictPath()'s return value omits any }
{ leading slash, and uses isAbsolute to report whether the URL's path is absolute. }

{ CFURLCopyFileSystemPath() returns the URL's path as a file system path for the }
{ given path style.  All percent escape sequences are replaced.  The URL is not }
{ resolved against its base before computing the path. }
function CFURLCopyPath( anURL: CFURLRef ): CFStringRef; external name '_CFURLCopyPath';

function CFURLCopyStrictPath( anURL: CFURLRef; var isAbsolute: Boolean ): CFStringRef; external name '_CFURLCopyStrictPath';

function CFURLCopyFileSystemPath( anURL: CFURLRef; pathStyle: CFURLPathStyle ): CFStringRef; external name '_CFURLCopyFileSystemPath';

{ Returns whether anURL's path represents a directory }
{ (true returned) or a simple file (false returned) }
function CFURLHasDirectoryPath( anURL: CFURLRef ): Boolean; external name '_CFURLHasDirectoryPath';

{ Any additional resource specifiers after the path.  For URLs }
{ that cannot be decomposed, this is everything except the scheme itself. }
function CFURLCopyResourceSpecifier( anURL: CFURLRef ): CFStringRef; external name '_CFURLCopyResourceSpecifier';

function CFURLCopyHostName( anURL: CFURLRef ): CFStringRef; external name '_CFURLCopyHostName';

function CFURLGetPortNumber( anURL: CFURLRef ): SInt32; external name '_CFURLGetPortNumber'; { Returns -1 if no port number is specified }

function CFURLCopyUserName( anURL: CFURLRef ): CFStringRef; external name '_CFURLCopyUserName';

function CFURLCopyPassword( anURL: CFURLRef ): CFStringRef; external name '_CFURLCopyPassword';

{ These remove all percent escape sequences except those for }
{ characters in charactersToLeaveEscaped.  If charactersToLeaveEscaped }
{ is empty (""), all percent escape sequences are replaced by their }
{ corresponding characters.  If charactersToLeaveEscaped is NULL, }
{ then no escape sequences are removed at all }
function CFURLCopyParameterString( anURL: CFURLRef; charactersToLeaveEscaped: CFStringRef ): CFStringRef; external name '_CFURLCopyParameterString';

function CFURLCopyQueryString( anURL: CFURLRef; charactersToLeaveEscaped: CFStringRef ): CFStringRef; external name '_CFURLCopyQueryString';

function CFURLCopyFragment( anURL: CFURLRef; charactersToLeaveEscaped: CFStringRef ): CFStringRef; external name '_CFURLCopyFragment';

function CFURLCopyLastPathComponent( url: CFURLRef ): CFStringRef; external name '_CFURLCopyLastPathComponent';

function CFURLCopyPathExtension( url: CFURLRef ): CFStringRef; external name '_CFURLCopyPathExtension';

{ These functions all treat the base URL of the supplied url as }
{ invariant.  In other words, the URL returned will always have }
{ the same base as the URL supplied as an argument. }

function CFURLCreateCopyAppendingPathComponent( allocator: CFAllocatorRef; url: CFURLRef; pathComponent: CFStringRef; isDirectory: Boolean ): CFURLRef; external name '_CFURLCreateCopyAppendingPathComponent';

function CFURLCreateCopyDeletingLastPathComponent( allocator: CFAllocatorRef; url: CFURLRef ): CFURLRef; external name '_CFURLCreateCopyDeletingLastPathComponent';

function CFURLCreateCopyAppendingPathExtension( allocator: CFAllocatorRef; url: CFURLRef; extension: CFStringRef ): CFURLRef; external name '_CFURLCreateCopyAppendingPathExtension';

function CFURLCreateCopyDeletingPathExtension( allocator: CFAllocatorRef; url: CFURLRef ): CFURLRef; external name '_CFURLCreateCopyDeletingPathExtension';

{#if MAC_OS_X_VERSION_10_3 <= MAC_OS_X_VERSION_MAX_ALLOWED}
{ Fills buffer with the bytes for url, returning the number of bytes }
{ filled.  If buffer is of insufficient size, returns -1 and no bytes }
{ are placed in buffer.  If buffer is NULL, the needed length is }
{ computed and returned.  The returned bytes are the original bytes } 
{ from which the URL was created; if the URL was created from a }
{ string, the bytes will be the bytes of the string encoded via UTF-8  }
function CFURLGetBytes( url: CFURLRef; buffer: CStringPtr; bufferLength: CFIndex ): CFIndex; external name '_CFURLGetBytes';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)

type
	CFURLComponentType = SInt32;
const
	kCFURLComponentScheme = 1;
	kCFURLComponentNetLocation = 2;
	kCFURLComponentPath = 3;
	kCFURLComponentResourceSpecifier = 4;
	
	kCFURLComponentUser = 5;
	kCFURLComponentPassword = 6;
	kCFURLComponentUserInfo = 7;
	kCFURLComponentHost = 8;
	kCFURLComponentPort = 9;
	kCFURLComponentParameterString = 10;
	kCFURLComponentQuery = 11;
	kCFURLComponentFragment = 12;
 
{ 
Gets the  range of the requested component in the bytes of url, as
returned by CFURLGetBytes().  This range is only good for use in the
bytes returned by CFURLGetBytes!

If non-NULL, rangeIncludingSeparators gives the range of component
including the sequences that separate component from the previous and
next components.  If there is no previous or next component, that end of
rangeIncludingSeparators will match the range of the component itself.
If url does not contain the given component type, (kCFNotFound, 0) is
returned, and rangeIncludingSeparators is set to the location where the
component would be inserted.  Some examples -

For the URL http://www.apple.com/hotnews/

Component           returned range      rangeIncludingSeparators
scheme              (0, 4)              (0, 7)
net location        (7, 13)             (4, 16)
path                (20, 9)             (20, 9)    
resource specifier  (kCFNotFound, 0)    (29, 0)
user                (kCFNotFound, 0)    (7, 0)
password            (kCFNotFound, 0)    (7, 0)
user info           (kCFNotFound, 0)    (7, 0)
host                (7, 13)             (4, 16)
port                (kCFNotFound, 0)    (20, 0)
parameter           (kCFNotFound, 0)    (29, 0)
query               (kCFNotFound, 0)    (29, 0)
fragment            (kCFNotFound, 0)    (29, 0)


For the URL ./relPath/file.html#fragment

Component           returned range      rangeIncludingSeparators
scheme              (kCFNotFound, 0)    (0, 0)
net location        (kCFNotFound, 0)    (0, 0)
path                (0, 19)             (0, 20)
resource specifier  (20, 8)             (19, 9)
user                (kCFNotFound, 0)    (0, 0)
password            (kCFNotFound, 0)    (0, 0)
user info           (kCFNotFound, 0)    (0, 0)
host                (kCFNotFound, 0)    (0, 0)
port                (kCFNotFound, 0)    (0, 0)
parameter           (kCFNotFound, 0)    (19, 0)
query               (kCFNotFound, 0)    (19, 0)
fragment            (20, 8)             (19, 9)


For the URL scheme://user:pass@host:1/path/path2/file.html;params?query#fragment

Component           returned range      rangeIncludingSeparators
scheme              (0, 6)              (0, 9)
net location        (9, 16)             (6, 19)
path                (25, 21)            (25, 22) 
resource specifier  (47, 21)            (46, 22)
user                (9, 4)              (6, 8)
password            (14, 4)             (13, 6)
user info           (9, 9)              (6, 13)
host                (19, 4)             (18, 6)
port                (24, 1)             (23, 2)
parameter           (47, 6)             (46, 8)
query               (54, 5)             (53, 7)
fragment            (60, 8)             (59, 9)
}
function CFURLGetByteRangeForComponent( url: CFURLRef; component: CFURLComponentType; var rangeIncludingSeparators: CFRange ): CFRange; external name '_CFURLGetByteRangeForComponent';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)
{#endif}

{ Returns a string with any percent escape sequences that do NOT }
{ correspond to characters in charactersToLeaveEscaped with their }
{ equivalent.  Returns NULL on failure (if an invalid percent sequence }
{ is encountered), or the original string (retained) if no characters }
{ need to be replaced. Pass NULL to request that no percent escapes be }
{ replaced, or the empty string (CFSTR("")) to request that all percent }
{ escapes be replaced.  Uses UTF8 to interpret percent escapes. }
function CFURLCreateStringByReplacingPercentEscapes( allocator: CFAllocatorRef; originalString: CFStringRef; charactersToLeaveEscaped: CFStringRef ): CFStringRef; external name '_CFURLCreateStringByReplacingPercentEscapes';

{#if MAC_OS_X_VERSION_10_3 <= MAC_OS_X_VERSION_MAX_ALLOWED}
{ As above, but allows you to specify the encoding to use when interpreting percent escapes }
function CFURLCreateStringByReplacingPercentEscapesUsingEncoding( allocator: CFAllocatorRef; origString: CFStringRef; charsToLeaveEscaped: CFStringRef; encoding: CFStringEncoding ): CFStringRef; external name '_CFURLCreateStringByReplacingPercentEscapesUsingEncoding';
(* AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER *)
{#endif}

{ Creates a copy or originalString, replacing certain characters with }
{ the equivalent percent escape sequence based on the encoding specified. }
{ If the originalString does not need to be modified (no percent escape }
{ sequences are missing), may retain and return originalString. }
{ If you are uncertain of the correct encoding, you should use UTF-8, }
{ which is the encoding designated by RFC 2396 as the correct encoding }
{ for use in URLs.  The characters so escaped are all characters that }
{ are not legal URL characters (based on RFC 2396), plus any characters }
{ in legalURLCharactersToBeEscaped, less any characters in }
{ charactersToLeaveUnescaped.  To simply correct any non-URL characters }
{ in an otherwise correct URL string, do: }

{ newString = CFURLCreateStringByAddingPercentEscapes(NULL, origString, NULL, NULL, kCFStringEncodingUTF8); }
function CFURLCreateStringByAddingPercentEscapes( allocator: CFAllocatorRef; originalString: CFStringRef; charactersToLeaveUnescaped: CFStringRef; legalURLCharactersToBeEscaped: CFStringRef; encoding: CFStringEncoding ): CFStringRef; external name '_CFURLCreateStringByAddingPercentEscapes';

{#ifndef CF_OPEN_SOURCE}

function CFURLCreateFromFSRef( allocator: CFAllocatorRef; const (*var*) fsRef_: FSRef ): CFURLRef; external name '_CFURLCreateFromFSRef';

function CFURLGetFSRef( url: CFURLRef; var fsRef_: FSRef ): Boolean; external name '_CFURLGetFSRef';

{#endif} // !CF_OPEN_SOURCE


end.
