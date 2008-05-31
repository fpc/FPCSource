
{     File:       UTType.p(.pas)																	    }
{ 																										}
{     Contains:   CodeWarrior Pascal(GPC) translation of Apple's Mac OS X 10.3 introduced UTType.h	    }
{				  Translation compatible with make-gpc-interfaces.pl generated MWPInterfaces            }
{                 (GPCPInterfaces) and Mac OS X 10.3.x or higher.  The GPC translation is linkable with }
{                 Mac OS X 10.3.x or higher Mach-O ApplicationServices.framework.                       }  
{                 (Note:  ApplicationServices is the “umbrella” framework encapsulating the             }
{                 LaunchServices subframework which contains the UTType declared data and function      }
{                 symbols.)                                                                             }
{                                                                                                       }
{                The CodeWarrior Pascal translation is NOT linkable with Mac OS X 10.3.x or higher      }
{                CFM CarbonLib.  The declared symbols are exported only from the Mach-O                 }
{				 ApplicationServices “umbrella” framework (encapsulates the LaunchServices subframework)}
{				 and references to symbols used by CFM code must be established at runtime.  Individual }
{				 symbol references can be resolved using CFBundleGetFunctionPointerForName and          }
{				 CFBundleGetDataPointerForName (see CFBundle.p) respectively for function and data      }
{				 symbols.  For bulk resolution of Mach-O symbol references,  CFMLateImport technology   }
{				 can be used.                                                                           }
{ 																										}
{     Version:    1.0																					}
{ 																										}
{	  Pascal Translation:  Gale Paeper, <gpaeper@empirenet.com>, 2004									}
{ 																										}
{     Copyright:  Subject to the constraints of Apple's original rights, you're free to use this		}
{				  translation as you deem fit.															}
{ 																										}
{     Bugs?:      This is an AS IS translation with no express guarentees of any kind.					}
{                 If you do find a bug, please help out the Macintosh Pascal programming community by   }
{				  reporting your bug finding and possible fix to either personal e-mail to Gale Paeper	}
{				  or a posting to the MacPascal mailing list.											}
{                                                                                                       }
{
      Change History (most recent first) (DD/MM/YY):

         <1>      06/12/04    GRP     First Pascal translation of UTType.h, version LaunchServices-98~1.
}
{     Translation assisted by:                                                                          }
{This file was processed by Dan's Source Converter}
{version 1.3 (this version modified by Ingemar Ragnemalm)}

{The original source on which this file is based: }
{
     File:       LaunchServices/UTType.h
 
     Contains:   Public interfaces for Uniform Type Indentification
 
     Version:    LaunchServices-98~1
 
     Copyright:  © 2003 by Apple Computer, Inc., all rights reserved.
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://www.freepascal.org/bugs.html
 
}


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

unit UTType;
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
uses MacTypes,CFBase,CFArray,CFDictionary,CFURL;

{$ALIGN POWER}

{ ======================================================================================================== }
{ Uniform Type Identification API                                                                          }
{ ======================================================================================================== }
{
    Uniform Type Identification Primer

    Uniform Type Identifiers (or UTIs) are strings which uniquely identify
    abstract types. They can be used to describe a file format or
    data type, but can also be used to describe type information for
    other sorts of entities, such as directories, volumes, or packages.

    The syntax of a uniform type identifier looks like a bundle identifier.
    It has the form of a reversed DNS name, although some special top-level 
    UTI domains are reserved by Apple and are outside the currect IANA 
    top-level Internet domain name space.

    Examples:

        public.jpeg
        public.utf16-plain-text
        com.apple.xml-plist
        com.apple.appleworks.doc

    Types which are standard or not controlled by any one organization 
    are declared in the "public" domain. Currently, public types may  
    be declared only by Apple.

    Types specific to Mac OS are declared with identifiers in the 
    com.apple.macos domain.

    Third parties should declare their own uniform type identifiers
    in their respective registered Internet domain spaces.

    Type declarations appear in bundle property lists and tell
    the system several things about a type, including the following:

    Conformance

    A type may "conform" to one or more other types. For example, the
    type com.apple.macos.xml-property-list conforms to both the
    com.apple.macos.property-list and public.xml types. The public.xml 
    type in turn conforms to type public.text. Finally, type public.text  
    conforms to public.data, which is the base type for all types 
    describing bytes stream formats. Conformance relationships between 
    types are established in type declarations.

    Conformance relationships establish a multiple inheritanace hierarchy
    between types. Type property values may be inherited at runtime
    according to the conformance relationships each type. When a type's 
    declaration does not include a value for particular type property, 
    then the type's supertypes are searched for a value. Supertypes are 
    searched depth-first, in the order given in the type declaration. 
    This is the only way in which the declared order of the conforms-to 
    supertypes is signitificant.

    Tags

    A "tag" is a string which indicates a type in some other type 
    identification space, such as a filename extension, MIME Type,
    or NSPboardType. Each type declaration may include a 
    "tag specification", which is a dictionary listing all of the 
    tags associated with the type.

    A tag's "class" is the namespace of a tag: filename extension, 
    MIME type, OSType, etc. Tag classes are themselves identified by 
    uniform type identifiers so that the set of valid tag classes is 
    easily extendend in the future.

    Other Type Properties

    Type declarations may include several other properties: a localizable
    user description of the type, the name of an icon resource in
    the declaring bundle, a reference URL identifying technical 
    documentation about the type itself, and finally a version number, 
    which can be incremented as a type evolves. All of these properties
    are optional.

    Exported vs. Imported Type Declarations
    
    Type declarations are either exported or imported. An exported
    type declaration means that the type itself is defined or owned 
    by the organization making the declaration. For example, a propietary
    document type declaration should only be exported by the application
    which controls the document format.

    An imported declatation is for applications which depend on the
    existence of someone else's type declaration. If application A can
    open application B's document format, then application A makes
    an imported declaration of application B's document type so that
    even if application B is not present on the system, there is an
    acessible declaration of its document type.

    An exported declaration of a particular type identifier is always
    preferred over an imported declaration.

    Example XML Type Declaration

    Appearing below is an XML excerpt from a bundle Info.plist file which 
    declares the public type "public.jpeg":
    
        <key>UTExportedTypeDeclarations</key>
        <array>
            <dict>
                <key>UTTypeIdentifier</key>
                <string>public.jpeg</string>
                <key>UTTypeReferenceURL</key>
                <string>http://www.w3.org/Graphics/JPEG/</string>
                <key>UTTypeDescription</key>
                <string>JPEG image</string>
                <key>UTTypeIconName</key>
                <string>public.jpeg.icns</string>
                <key>UTTypeConformsTo</key>
                <array>
                    <string>public.image</string>
                    <string>public.data</string>
                </array>
                <key>UTTypeTagSpecification</key>
                <dict>
                    <key>com.apple.macos.ostype</key>
                    <string>JPEG</string>
                    <key>public.filename-extension</key>
                    <array>
                        <string>jpeg</string>
                        <string>jpg</string>
                    </array>
                    <key>public.mime-type</key>
                    <string>image/jpeg</string>
                </dict>
            </dict>
        </array>


    Dynamic Type Identifiers

    Uniform Type Identifiation uses dynamic type identifiers to
    represent types for which no identifier has been declared. A
    dynamic type identifier is syntactially a regular uniform
    type identifier in the "dyn" domain. However, after the
    initial domain label, a dynamic type identifier is an 
    opaque encoding of a tag specification. Dynamic type 
    identifiers cannot be declared. They are generated on-demand
    with whatever type information is available at the time, often 
    a single (otherwise unknown) type tag.

    A dynamic identifier therefore carries within it a minimal
    amount of type information, but enough to work well with the
    Uniform Type Identification API. For example, a client can
    extract from a dynamic type identifier the original tag
    specification with which it was created. A client can also
    test a dynamic type identifier for equality to another
    uniform type identifier. If the dynamic identifier's
    tag specification is a subset of the other identifier's
    tags, the two are considered equal.

    Dynamic type identifiers do not express the full richness
    of type information associated with a declared type 
    identifier, but dynamic type identifiers allow the behavior
    to degrade gracefully in the presence of incomplete 
    declared type information.

    A dynamic type identifier may be transmitted across processes
    on a given system, but it should never be stored persistently
    or transmitted over the wire to another system. In particular,
    dynamic identifiers should not appear in bundle info property
    lists, and they will generally be ignored when they do. Apple 
    reserves the right to change the opaque format of dynamic
    identifiers in future versions of Mac OS X.
}

{
    Type Declaration Dictionary Keys

    The following keys are used in type declarations
}
{
 *  kUTExportedTypeDeclarationsKey
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in ApplicationServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
 
// AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER
var kUTExportedTypeDeclarationsKey: CFStringRef; external name '_kUTExportedTypeDeclarationsKey'; (* attribute const *)

{
 *  kUTImportedTypeDeclarationsKey
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in ApplicationServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
}

// AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER
var kUTImportedTypeDeclarationsKey: CFStringRef; external name '_kUTImportedTypeDeclarationsKey'; (* attribute const *)

{
 *  kUTTypeIdentifierKey
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in ApplicationServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }

// AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER
var kUTTypeIdentifierKey: CFStringRef; external name '_kUTTypeIdentifierKey'; (* attribute const *)

{
 *  kUTTypeTagSpecificationKey
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in ApplicationServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
 
// AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER
var kUTTypeTagSpecificationKey: CFStringRef; external name '_kUTTypeTagSpecificationKey'; (* attribute const *)

{
 *  kUTTypeConformsToKey
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in ApplicationServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 } 
 
// AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER
var kUTTypeConformsToKey: CFStringRef; external name '_kUTTypeConformsToKey'; (* attribute const *)

{
 *  kUTTypeDescriptionKey
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in ApplicationServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }

// AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER
var kUTTypeDescriptionKey: CFStringRef; external name '_kUTTypeDescriptionKey'; (* attribute const *)

{
 *  kUTTypeIconFileKey
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in ApplicationServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }

// AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER
var kUTTypeIconFileKey: CFStringRef; external name '_kUTTypeIconFileKey'; (* attribute const *)

{
 *  kUTTypeReferenceURLKey
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in ApplicationServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
 
// AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER
var kUTTypeReferenceURLKey: CFStringRef; external name '_kUTTypeReferenceURLKey'; (* attribute const *)

{
 *  kUTTypeVersionKey
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in ApplicationServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }
 
// AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER
var kUTTypeVersionKey: CFStringRef; external name '_kUTTypeVersionKey'; (* attribute const *)


{
    Type Tag Classes

    The following constant strings identify tag classes for use 
    when converting uniform type identifiers to and from
    equivalent tags.
}
{
 *  kUTTagClassFilenameExtension
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in ApplicationServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }

// AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER:
var kUTTagClassFilenameExtension: CFStringRef; external name '_kUTTagClassFilenameExtension'; (* attribute const *)

{
 *  kUTTagClassMIMEType
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in ApplicationServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }

// AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER
var kUTTagClassMIMEType: CFStringRef; external name '_kUTTagClassMIMEType'; (* attribute const *)

{
 *  kUTTagClassNSPboardType
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in ApplicationServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }

// AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER
var kUTTagClassNSPboardType: CFStringRef; external name '_kUTTagClassNSPboardType'; (* attribute const *)

{
 *  kUTTagClassOSType
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in ApplicationServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }

// AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER
var kUTTagClassOSType: CFStringRef; external name '_kUTTagClassOSType'; (* attribute const *)

{
 *  UTTypeCreatePreferredIdentifierForTag()
 *  
 *  Discussion:
 *    Creates a uniform type identifier for the type indicated by the
 *    specified tag. This is the primary function to use for going from
 *    tag (extension/MIMEType/OSType) to uniform type identifier.
 *    Optionally, the returned type identifiers must conform to the
 *    identified "conforming-to" type argument. This is a hint to the
 *    implementation to constrain the search to a particular tree of
 *    types. For example, the client may want to know the type
 *    indicated by a particular extension tag. If the client knows that
 *    the extension is associated with a directory (rather than a
 *    file), the client may specify "public.directory" for the
 *    conforming-to argument. This will allow the implementation to
 *    ignore all types associated with byte data formats (public.data
 *    base type). If more than one type is indicated, preference is
 *    given to a public type over a non-public type on the theory that
 *    instances of public types are more common, and therefore more
 *    likely to be correct. When there a choice must be made between
 *    multiple public types or multiple non-public types, the selection
 *    rules are undefined. Clients needing finer control should use
 *    UTTypeCreateAllIdentifiersWithTag. If no declared type is
 *    indicated, a dynamic type identifier is generated which satisfies
 *    the parameters.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Parameters:
 *    
 *    inTagClass:
 *      the class identifier of the tag argument
 *    
 *    inTag:
 *      the tag string
 *    
 *    inConformingToTypeIdentifier:
 *      the identifier of a type to which the result must conform
 *  
 *  Result:
 *    a new CFStringRef containing the type identifier, or NULL if
 *    inTagClass is not a known tag class
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in ApplicationServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }

// AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER 
function UTTypeCreatePreferredIdentifierForTag(
  inTagClass: CFStringRef;
  inTag: CFStringRef;
  inConformingToTypeIdentifier: CFStringRef): CFStringRef; external name '_UTTypeCreatePreferredIdentifierForTag'; { can be NULL }


{
 *  UTTypeCreateAllIdentifiersForTag()
 *  
 *  Discussion:
 *    Creates an array of all uniform type identifiers indicated by the
 *    specified tag. An overloaded tag (e.g., an extension used by
 *    several applications for different file formats) may indicate
 *    multiple types. If no declared type identifiers have the
 *    specified tag, then a single dynamic type identifier will be
 *    created for the tag. Optionally, the returned type identifiers
 *    must conform to the identified "conforming-to" type argument.
 *    This is a hint to the implementation to constrain the search to a
 *    particular tree of types. For example, the client may want to
 *    know the type indicated by a particular extension tag. If the
 *    client knows that the extension is associated with a directory
 *    (rather than a file), the client may specify "public.directory"
 *    for the conforming-to argument. This will allow the
 *    implementation to ignore all types associated with byte data
 *    formats (public.data base type).
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Parameters:
 *    
 *    inTagClass:
 *      the class identifier of the tag argument
 *    
 *    inTag:
 *      the tag string
 *    
 *    inConformingToTypeIdentifier:
 *      the identifier of a type to which the results must conform
 *  
 *  Result:
 *    An array of uniform type identifiers, or NULL if inTagClass is
 *    not a known tag class
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in ApplicationServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }

// AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER
function UTTypeCreateAllIdentifiersForTag(
  inTagClass: CFStringRef;
  inTag: CFStringRef;
  inConformingToTypeIdentifier: CFStringRef): CFArrayRef; external name '_UTTypeCreateAllIdentifiersForTag'; { can be NULL } 


{
 *  UTTypeCopyPreferredTagWithClass()
 *  
 *  Discussion:
 *    Returns the identified type's preferred tag with the specified
 *    tag class as a CSString. This is the primary function to use for
 *    going from uniform type identifier to tag. If the type
 *    declaration included more than one tag with the specified class,
 *    the first tag in the declared tag array is the preferred tag.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Parameters:
 *    
 *    inTypeIdentifier:
 *      the uniform type identifier
 *    
 *    inTagClass:
 *      the class of tags to return
 *  
 *  Result:
 *    an array of tags (as CFStrings), or NULL if there are no tags
 *    with the specified class.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in ApplicationServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }

// AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER
function UTTypeCopyPreferredTagWithClass(
  inTypeIdentifier: CFStringRef;
  inTagClass: CFStringRef): CFStringRef; external name '_UTTypeCopyPreferredTagWithClass';


{
 *  UTTypeEqual()
 *  
 *  Discussion:
 *    Compares two identified types for equality. Types are equal if
 *    their identifier strings are equal using a case-insensitive
 *    comparison. In addition, if one or both of the identifiers is a
 *    dynamic identifier, then the types are equal if either
 *    identifier's tag specification is a subset of the other
 *    identifier's tag specification.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Parameters:
 *    
 *    inTypeIdentifier1:
 *      a uniform type identifier
 *    
 *    inTypeIdentifier2:
 *      another uniform type identifier
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in ApplicationServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }

// AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER
function UTTypeEqual(
  inTypeIdentifier1: CFStringRef;
  inTypeIdentifier2: CFStringRef): Boolean; external name '_UTTypeEqual';


{
 *  UTTypeConformsTo()
 *  
 *  Discussion:
 *    Tests for a conformance relationship between the two identified
 *    types. Returns true if the types are equal, or if the first type
 *    conforms, directly or indirectly, to the second type.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Parameters:
 *    
 *    inTypeIdentifier:
 *      the uniform type identifier to test
 *    
 *    inConformsToTypeIdentifier:
 *      the uniform type identifier against which to test conformance.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in ApplicationServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }

// AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER
function UTTypeConformsTo(
  inTypeIdentifier: CFStringRef;
  inConformsToTypeIdentifier: CFStringRef): Boolean; external name '_UTTypeConformsTo';


{
 *  UTTypeCopyDescription()
 *  
 *  Discussion:
 *    Returns the localized, user-readable type description string
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Parameters:
 *    
 *    inTypeIdentifier:
 *      the uniform type identifier
 *  
 *  Result:
 *    a localized string, or NULL of no type description is available
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in ApplicationServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }

// AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER
function UTTypeCopyDescription(inTypeIdentifier: CFStringRef): CFStringRef; external name '_UTTypeCopyDescription';


{
 *  UTTypeCopyDeclaration()
 *  
 *  Discussion:
 *    Returns the identified type's declaration dictionary, as it
 *    appears in the declaring bundle's info property list. This the
 *    access path to other type properties for which direct access is
 *    rarely needed.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Parameters:
 *    
 *    inTypeIdentifier:
 *      the uniform type identifier
 *  
 *  Result:
 *    a tag declaration dictionary, or NULL if the type is not declared
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in ApplicationServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }

// AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER
function UTTypeCopyDeclaration(inTypeIdentifier: CFStringRef): CFDictionaryRef; external name '_UTTypeCopyDeclaration';


{
 *  UTTypeCopyDeclaringBundleURL()
 *  
 *  Discussion:
 *    Returns the URL of the bundle containing the type declaration of
 *    the identified type.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Parameters:
 *    
 *    inTypeIdentifier:
 *      the uniform type identifier
 *  
 *  Result:
 *    a URL, or NULL if the bundle cannot be located.
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in ApplicationServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }

// AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER
function UTTypeCopyDeclaringBundleURL(inTypeIdentifier: CFStringRef): CFURLRef; external name '_UTTypeCopyDeclaringBundleURL';


{
 *  UTCreateStringForOSType()
 *  
 *  Discussion:
 *    A helper function to canonically encode an OSType as a CFString
 *    suitable for use as a tag argument.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Parameters:
 *    
 *    inOSType:
 *      the OSType value to encode
 *  
 *  Result:
 *    a new CFString representing the OSType, or NULL if the argument
 *    is 0 or '????'
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in ApplicationServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }

// AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER
function UTCreateStringForOSType(inOSType: OSType): CFStringRef; external name '_UTCreateStringForOSType';


{
 *  UTGetOSTypeFromString()
 *  
 *  Discussion:
 *    A helper function to canonically decode a string-encoded OSType
 *    back to the original OSType value.
 *  
 *  Mac OS X threading:
 *    Thread safe since version 10.3
 *  
 *  Parameters:
 *    
 *    inString:
 *      the string to decode
 *  
 *  Result:
 *    the OSType value encoded in the string, or 0 if the string is not
 *    a valid encoding of an OSType
 *  
 *  Availability:
 *    Mac OS X:         in version 10.3 and later in ApplicationServices.framework
 *    CarbonLib:        not available in CarbonLib 1.x
 *    Non-Carbon CFM:   not available
 }

// AVAILABLE_MAC_OS_X_VERSION_10_3_AND_LATER
function UTGetOSTypeFromString(inString: CFStringRef): OSType; external name '_UTGetOSTypeFromString';


{$ALIGN MAC68K}


end.
