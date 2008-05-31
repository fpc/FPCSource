{
     File:       PMDefinitions.p
 
     Contains:   Carbon Printing Manager Interfaces.
 
     Version:    Technology: Mac OS X
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1998-2002 by Apple Computer, Inc., all rights reserved
 
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

unit PMDefinitions;
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
uses MacTypes,MacErrors,CFString;


{$ALIGN MAC68K}

{ Printing objects }

type
	PMObject							= Ptr;
	PMDialog    = ^SInt32; { an opaque 32-bit type }
	PMDialogPtr = ^PMDialog;  { when a var xx:PMDialog parameter can be nil, it is changed to xx: PMDialogPtr }
	PMPrintSettings    = ^SInt32; { an opaque 32-bit type }
	PMPrintSettingsPtr = ^PMPrintSettings;  { when a var xx:PMPrintSettings parameter can be nil, it is changed to xx: PMPrintSettingsPtr }
	PMPageFormat    = ^SInt32; { an opaque 32-bit type }
	PMPageFormatPtr = ^PMPageFormat;  { when a var xx:PMPageFormat parameter can be nil, it is changed to xx: PMPageFormatPtr }
	PMPrintContext    = ^SInt32; { an opaque 32-bit type }
	PMPrintContextPtr = ^PMPrintContext;  { when a var xx:PMPrintContext parameter can be nil, it is changed to xx: PMPrintContextPtr }
	PMPrintSession    = ^SInt32; { an opaque 32-bit type }
	PMPrintSessionPtr = ^PMPrintSession;  { when a var xx:PMPrintSession parameter can be nil, it is changed to xx: PMPrintSessionPtr }
	PMPrinter    = ^SInt32; { an opaque 32-bit type }
	PMPrinterPtr = ^PMPrinter;  { when a var xx:PMPrinter parameter can be nil, it is changed to xx: PMPrinterPtr }

const
	kPMCancel					= $0080;						{  user hit cancel button in dialog  }

	kPMNoData					= nil; { for general use }
	kPMDontWantSize				= nil; { for parameters which return size information }
	kPMDontWantData				= nil; { for parameters which return data }
	kPMDontWantBoolean			= nil; { for parameters which take a boolean reference }
	kPMNoReference				= nil; { for parameters which take an address pointer }

	{	 for parameters which take a PrintSettings reference 	}
	kPMNoPrintSettings			= nil;

	{	 for parameters which take a PageFormat reference 	}
	kPMNoPageFormat				= nil;

	{ for parameters which take a Server reference }
	kPMServerLocal				= nil;

type
	PMDestinationType 			= UInt16;
const
	kPMDestinationInvalid		= 0;
	kPMDestinationPrinter		= 1;
	kPMDestinationFile			= 2;
	kPMDestinationFax			= 3;
	kPMDestinationPreview		= 4;

	kPMDestinationTypeDefault	= kPMDestinationPrinter;
	
type
	PMTag 						= UInt32;
const
																{  common tags  }
	kPMCurrentValue				= FourCharCode('curr');						{  current setting or value  }
	kPMDefaultValue				= FourCharCode('dflt');						{  default setting or value  }
	kPMMinimumValue				= FourCharCode('minv');						{  the minimum setting or value  }
	kPMMaximumValue				= FourCharCode('maxv');						{  the maximum setting or value  }
																{  profile tags  }
	kPMSourceProfile			= FourCharCode('srcp');						{  source profile  }
																{  resolution tags  }
	kPMMinRange					= FourCharCode('mnrg');						{  Min range supported by a printer  }
	kPMMaxRange					= FourCharCode('mxrg');						{  Max range supported by a printer  }
	kPMMinSquareResolution		= FourCharCode('mins');						{  Min with X and Y resolution equal  }
	kPMMaxSquareResolution		= FourCharCode('maxs');						{  Max with X and Y resolution equal  }
	kPMDefaultResolution		= FourCharCode('dftr');						{  printer default resolution  }


type
	PMOrientation 				= UInt16;
const
	kPMPortrait					= 1;
	kPMLandscape				= 2;
	kPMReversePortrait			= 3;							{  will revert to kPortrait for current drivers  }
	kPMReverseLandscape			= 4;							{  will revert to kLandscape for current drivers  }

{ Printer states }
type
	PMPrinterState				= UInt16;
const
	kPMPrinterIdle				= 3;
	kPMPrinterProcessing		= 4;
	kPMPrinterStopped			= 5;

	kSizeOfTPrint				= 120;							{  size of old TPrint record  }


type
	PMColorMode 				= UInt16;
const
	kPMBlackAndWhite			= 1;
	kPMGray						= 2;
	kPMColor					= 3;
	kPMColorModeDuotone			= 4;							{  2 channels  }
	kPMColorModeSpecialColor	= 5;							{  to allow for special colors such as metalic, light cyan, etc.  }


type
	PMColorSpaceModel			= UInt32;
const
	kPMUnknownColorSpaceModel	= 0;
	kPMGrayColorSpaceModel		= 1;
	kPMRGBColorSpaceModel		= 2;
	kPMCMYKColorSpaceModel		= 3;
	kPMDevNColorSpaceModel		= 4;
	
	kPMColorSpaceModelCount		= 4; { total number of color space models supported }
	
	{	 Constants to define the ColorSync Intents. These intents may be used 	}
	{	 to set an intent part way through a page or for an entire document. 	}

type
	PMColorSyncIntent 			= UInt32;
const
	kPMColorIntentUndefined		= $0000;						{  User or application have not declared an intent, use the printer's default.  }
	kPMColorIntentAutomatic		= $0001;						{  Automatically match for photos and graphics anywhere on the page.  }
	kPMColorIntentPhoto			= $0002;						{  Use Photographic (cmPerceptual) intent for all contents.  }
	kPMColorIntentBusiness		= $0004;						{  Use Business Graphics (cmSaturation) intent for all contents.  }
	kPMColorIntentRelColor		= $0008;						{  Use Relative Colormetrics (Logo Colors) for the page.  }
	kPMColorIntentAbsColor		= $0010;						{  Use absolute colormetric for the page.  }
	kPMColorIntentUnused		= $FFE0;						{  Remaining bits unused at this time.  }

	{	 Print quality modes "standard options" 	}

type
	PMQualityMode 				= UInt32;
const
	kPMQualityLowest			= $0000;						{  Absolute lowest print quality  }
	kPMQualityInkSaver			= $0001;						{  Saves ink but may be slower  }
	kPMQualityDraft				= $0004;						{  Print at highest speed, ink used is secondary consideration  }
	kPMQualityNormal			= $0008;						{  Print in printers "general usage" mode for good balance between quality and speed  }
	kPMQualityPhoto				= $000B;						{  Optimize quality of photos on the page. Speed is not a concern  }
	kPMQualityBest				= $000D;						{  Get best quality output for all objects and photos.  }
	kPMQualityHighest			= $000F;						{  Absolute highest quality attained from a printers  }


	{	 Constants for our "standard" paper types 	}

type
	PMPaperType 				= UInt32;
const
	kPMPaperTypeUnknown			= $0000;						{  Not sure yet what paper type we have.  }
	kPMPaperTypePlain			= $0001;						{  Plain paper  }
	kPMPaperTypeCoated			= $0002;						{  Has a special coating for sharper images and text  }
	kPMPaperTypePremium			= $0003;						{  Special premium coated paper  }
	kPMPaperTypeGlossy			= $0004;						{  High gloss special coating  }
	kPMPaperTypeTransparency	= $0005;						{  Used for overheads  }
	kPMPaperTypeTShirt			= $0006;						{  Used to iron on t-shirts  }

	{	 Scaling alignment: 	}

type
	PMScalingAlignment 			= UInt16;
const
	kPMScalingPinTopLeft		= 1;
	kPMScalingPinTopRight		= 2;
	kPMScalingPinBottomLeft		= 3;
	kPMScalingPinBottomRight	= 4;
	kPMScalingCenterOnPaper		= 5;
	kPMScalingCenterOnImgArea	= 6;

	{	 Duplex binding directions: 	}

type
	PMDuplexBinding 			= UInt16;
const
	kPMDuplexBindingLeftRight	= 1;
	kPMDuplexBindingTopDown		= 2;

	{	 Layout directions: 	}

type
	PMLayoutDirection 			= UInt16;
const
																{  Horizontal-major directions:  }
	kPMLayoutLeftRightTopBottom	= 1;							{  English reading direction.  }
	kPMLayoutLeftRightBottomTop	= 2;
	kPMLayoutRightLeftTopBottom	= 3;
	kPMLayoutRightLeftBottomTop	= 4;							{  Vertical-major directions:  }
	kPMLayoutTopBottomLeftRight	= 5;
	kPMLayoutTopBottomRightLeft	= 6;
	kPMLayoutBottomTopLeftRight	= 7;
	kPMLayoutBottomTopRightLeft	= 8;

	{	 Page borders: 	}

type
	PMBorderType 				= UInt16;
const
	kPMBorderSingleHairline		= 1;
	kPMBorderDoubleHairline		= 2;
	kPMBorderSingleThickline	= 3;
	kPMBorderDoubleThickline	= 4;

	{	 Useful Constants for PostScript Injection 	}
	kPSPageInjectAllPages		= -1;							{  specifies to inject on all pages  }
	kPSInjectionMaxDictSize		= 5;							{  maximum size of a dictionary used for PSInjection  }

	{	 PostScript Injection values for kPSInjectionPlacementKey 	}

type
	PSInjectionPlacement 		= UInt16;
const
	kPSInjectionBeforeSubsection = 1;
	kPSInjectionAfterSubsection	= 2;
	kPSInjectionReplaceSubsection = 3;

	{	 PostScript Injection values for kPSInjectionSectionKey 	}

type
	PSInjectionSection 			= SInt32;
const
																{  Job  }
	kInjectionSectJob			= 1;							{  CoverPage  }
	kInjectionSectCoverPage		= 2;

	{	 PostScript Injection values for kPSInjectionSubSectionKey 	}

type
	PSInjectionSubsection 		= SInt32;
const
	kInjectionSubPSAdobe		= 1;							{  %!PS-Adobe            }
	kInjectionSubPSAdobeEPS		= 2;							{  %!PS-Adobe-3.0 EPSF-3.0     }
	kInjectionSubBoundingBox	= 3;							{  BoundingBox           }
	kInjectionSubEndComments	= 4;							{  EndComments           }
	kInjectionSubOrientation	= 5;							{  Orientation           }
	kInjectionSubPages			= 6;							{  Pages             }
	kInjectionSubPageOrder		= 7;							{  PageOrder           }
	kInjectionSubBeginProlog	= 8;							{  BeginProlog           }
	kInjectionSubEndProlog		= 9;							{  EndProlog           }
	kInjectionSubBeginSetup		= 10;							{  BeginSetup           }
	kInjectionSubEndSetup		= 11;							{  EndSetup              }
	kInjectionSubBeginDefaults	= 12;							{  BeginDefaults        }
	kInjectionSubEndDefaults	= 13;							{  EndDefaults           }
	kInjectionSubDocFonts		= 14;							{  DocumentFonts        }
	kInjectionSubDocNeededFonts	= 15;							{  DocumentNeededFonts        }
	kInjectionSubDocSuppliedFonts = 16;							{  DocumentSuppliedFonts   }
	kInjectionSubDocNeededRes	= 17;							{  DocumentNeededResources     }
	kInjectionSubDocSuppliedRes	= 18;							{  DocumentSuppliedResources }
	kInjectionSubDocCustomColors = 19;							{  DocumentCustomColors      }
	kInjectionSubDocProcessColors = 20;							{  DocumentProcessColors   }
	kInjectionSubPlateColor		= 21;							{  PlateColor           }
	kInjectionSubPageTrailer	= 22;							{  PageTrailer            }
	kInjectionSubTrailer		= 23;							{  Trailer               }
	kInjectionSubEOF			= 24;							{  EOF                  }
	kInjectionSubBeginFont		= 25;							{  BeginFont           }
	kInjectionSubEndFont		= 26;							{  EndFont               }
	kInjectionSubBeginResource	= 27;							{  BeginResource        }
	kInjectionSubEndResource	= 28;							{  EndResource           }
	kInjectionSubPage			= 29;							{  Page                }
	kInjectionSubBeginPageSetup	= 30;							{  BeginPageSetup         }
	kInjectionSubEndPageSetup	= 31;							{  EndPageSetup           }

type
	PMPPDDomain 				= UInt16;
const
	kAllPPDDomains				= 1;
	kSystemPPDDomain			= 2;
	kLocalPPDDomain				= 3;
	kNetworkPPDDomain			= 4;
	kUserPPDDomain				= 5;
	kCUPSPPDDomain				= 6;

	{	 Description types 	}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kPMPPDDescriptionType CFSTRP('PMPPDDescriptionType')}
{$endc}
	{	 Document format strings 	}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kPMDocumentFormatDefault CFSTRP('com.apple.documentformat.default')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kPMDocumentFormatPDF CFSTRP('application/pdf')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kPMDocumentFormatPICT CFSTRP('application/vnd.apple.printing-pict')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kPMDocumentFormatPICTPS CFSTRP('application/vnd.apple.printing-pict-ps')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kPMDocumentFormatPICTPSwPSNormalizer CFSTRP('application/vnd.apple.printing-pict-ps-viapsnormalizer')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kPMDocumentFormatPostScript CFSTRP('application/postscript')}
{$endc}
	{	 Graphic context strings 	}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kPMGraphicsContextDefault CFSTRP('com.apple.graphicscontext.default')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kPMGraphicsContextQuickdraw CFSTRP('com.apple.graphicscontext.quickdraw')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kPMGraphicsContextCoreGraphics CFSTRP('com.apple.graphicscontext.coregraphics')}
{$endc}
	{	 Data format strings 	}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kPMDataFormatPS CFSTRP('application/postscript')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kPMDataFormatPDF CFSTRP('application/pdf')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kPMDataFormatPICT CFSTRP('application/vnd.apple.printing-pict-ps')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kPMDataFormatPICTwPS CFSTRP('application/vnd.apple.printing-pict-ps')}
{$endc}
	{	 PostScript Injection Dictionary Keys 	}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kPSInjectionSectionKey CFSTRP('section')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kPSInjectionSubSectionKey CFSTRP('subsection')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kPSInjectionPageKey CFSTRP('page')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kPSInjectionPlacementKey CFSTRP('place')}
{$endc}
{$ifc USE_CFSTR_CONSTANT_MACROS}
{$definec kPSInjectionPostScriptKey CFSTRP('psdata')}
{$endc}
	{	 PDF Workflow Keys 	}
{ kPDFWorkFlowItemURLKey          CFSTR("itemURL"); defined in GPCMacros.inc }
{ kPDFWorkflowForlderURLKey       CFSTR("folderURL"); defined in GPCMacros.inc }
{ kPDFWorkflowDisplayNameKey      CFSTR("displayName"); defined in GPCMacros.inc }
{ kPDFWorkflowItemsKey            CFSTR("items"); defined in GPCMacros.inc }
	{	 OSStatus return codes 	}
	kPMNoError					= 0;
	kPMGeneralError				= -30870;
	kPMOutOfScope				= -30871;						{  an API call is out of scope  }
	kPMInvalidParameter			= -50;							{  a required parameter is missing or invalid  }
	kPMNoDefaultPrinter			= -30872;						{  no default printer selected  }
	kPMNotImplemented			= -30873;						{  this API call is not supported  }
	kPMNoSuchEntry				= -30874;						{  no such entry  }
	kPMInvalidPrintSettings		= -30875;						{  the printsettings reference is invalid  }
	kPMInvalidPageFormat		= -30876;						{  the pageformat reference is invalid  }
	kPMValueOutOfRange			= -30877;						{  a value passed in is out of range  }
	kPMLockIgnored				= -30878;						{  the lock value was ignored  }

	kPMInvalidPrintSession		= -30879;						{  the print session is invalid  }
	kPMInvalidPrinter			= -30880;						{  the printer reference is invalid  }
	kPMObjectInUse				= -30881;						{  the object is in use  }
	kPMInvalidPreset			= -30882;						{  the preset is invalid }


	kPMPrintAllPages			= $FFFFFFFF;

	kPMUnlocked					= false;
	kPMLocked					= true;


type
	PMRectPtr = ^PMRect;
	PMRect = record
		top:					Double;
		left:					Double;
		bottom:					Double;
		right:					Double;
	end;

	PMResolutionPtr = ^PMResolution;
	PMResolution = record
		hRes:					Double;
		vRes:					Double;
	end;

	PMLanguageInfoPtr = ^PMLanguageInfo;
	PMLanguageInfo = record
		level:					Str32;
		version:				Str32;
		release:				Str32;
		pad:					SInt8
	end;
	
	PMPaperMargins = PMRect;

{$ALIGN MAC68K}


end.
