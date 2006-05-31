{
     File:       FileTypesAndCreators.p
 
     Contains:   Symbolic constants for FileTypes and signatures of popular documents.
 
     Version:    Technology: Macintosh Easy Open 1.1
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1992-2002 by Apple Computer, Inc., all rights reserved.
 
     Bugs?:      For bug reports, consult the following page on
                 the World Wide Web:
 
                     http://www.freepascal.org/bugs.html
 
}


{
    Modified for use with Free Pascal
    Version 200
    Please report any bugs to <gpc@microbizz.nl>
}

{$mode macpas}
{$packenum 1}
{$macro on}
{$inline on}
{$CALLING MWPASCAL}

unit FileTypesAndCreators;
interface
{$setc UNIVERSAL_INTERFACES_VERSION := $0342}
{$setc GAP_INTERFACES_VERSION := $0200}

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
uses MacTypes,ConditionalMacros;


{$ALIGN MAC68K}

{

Motivation for FileTypesAndCreators.h

    The TranslationMgr (Macintosh Easy Open) uses “FileTypes” to express and manage
    translation capabilities.  This works well for most Macintosh applications.  It
    simply uses the file type stored in the catalog info by HFS.  But, the TranslationMgr
    needs to work with non-Macintosh files.  These do not have file types (or creators).
    Another problem is some Macintosh applications keep the same file type for a file
    after changing the file format from one version to the next.  Thus, the HFS file type
    cannot be used to distinguish between formats.

    This file is an effort to define FileTypes and creator codes for non-Macintosh file
    formats and define FileTypes that distinguish between formats.


There are two main categories of constants defined in FileTypesAndCreators.h.  One is
creator signatures of applications the other is file types.  To make this easier to use,
some style guidelines are provided.

Creators:

    The symbolic constant for all creator codes begin with “sig”. Following “sig” is the
    application name.  Example:

        sigSurfWriter = 'SURF'

    If the application name is localized for different markets, the name of the U.S.
    version is always used. It is optional to make symbolic constants for localized names.
    If a U.S. is not available, another Roman name is used (this is for ease of editing
    in MPW.)  Example:

        sigSurfWriter = 'SURF',
        sigSkiWriter =  'SURF'  // optional definition of Canadian name of app

    If multiple versions of an application exist, and the creator codes are different,
    then the version number is added to the end of the symbolic name, for the first version
    in which the signature changed.  Example:

        sigDirtWriter =     'DIRT', // version 1 and 2 used same signature
        sigDirtWriter3 =    'DRT3'

    If a developer has the same application on another platform as is on the Mac, then the
    creator for a file from the other platform should be the same as the creator of the
    Macintosh application.   Example:

        sigMicrosoftWord =  'MSWD'  // used for Mac and Windows documents

    If there is no similar Macintosh product, then the signature of the platform is used.
    Example:

        sigPCDOS =          'mdos', // for DOS and Windows
        sigAppleProDos =    'pdos'


FileTypes:

    The symbolic constant for all file types begin with “ft”. Following “ft” is the ideal
    kind for that document.  Example:

        ftSurfWriterDocument =  'SRFD'

    If the application name is localized for different markets, the kind string of the U.S.
    version is always used. It is optional to make symbolic constants for localized kind
    strings.  If a U.S. is not available, another Roman kind string is used (this is for
    ease of editing in MPW.)  Example:

        ftSurfWriterDocument =  'SRFD,
        ftSkiWriterDocument =   'SRFD'  // optional Canadian name

    If multiple versions of an application exist, and the document for files created by each
    are different (meaning one version of the app can not read the documents of a newer
    version), then the version number is added to the end of the symbolic name, for the
    first version in which the format changed.  Example:

       // version 1 and 2 docs have same format, version 3 has a different format
        ftDirtWriterDocument =          'DDRT',
        ftDirtWriter3Document =         '3DRT'

    If multiple versions of an application exist, and the document for files created by each
    are different, but the catInfo file type has always been the same, then constants are
    made for each unique format and one is made for the catInfo type actually used.

        ftWordDocument =        'WDBN',
        ftWord3Document =       'WDB3',
        ftWord4Document =       'WDB4',
        ftWord5Document =       'WDB5'

    If a developer has the same application on another platform as is on the Mac and the file
    formats are the same (binary interchangeable), then the file type for a file from the other
    platform should be the same as the file type of the Macintosh document.   Example:

        ftPageMakerPublication = 'ALB4' // used for Mac and PC documents

    If there is no similar Macintosh document format, then a file type is made up.  If the
    program is used on two different non-Mac platforms and their file formats are not the
    same, the platform name is added to the name.  Typically after the program name and before
    document.  Example:

        ftXyWriteIIIDocument =  'XyWr'  // XyWriteIII only has a PC format
        ftSurfWindowsDocument = 'SRFW'  // Surf Windows file format
        ftSurfPCDocument =      'SRFP'  // Surf PC file format
}

{************* Macintosh applications and FileTypes *************}


const
																{  Mac word processors  }
	sigWord						= $4D535744 (* 'MSWD' *);
	ftWord3Document				= $4D535733 (* 'MSW3' *);
	ftWord4Document				= $4D535734 (* 'MSW4' *);
	ftWord5Document				= $4D535735 (* 'MSW5' *);
	ftWordDocument				= $5744424E (* 'WDBN' *);
	ftWordDocumentPC			= $4D575043 (* 'MWPC' *);						{  not registered  }
	ftWord1DocumentWindows		= $57573120 (* 'WW1 ' *);						{  not registered  }
	ftWord2DocumentWindows		= $57573220 (* 'WW2 ' *);						{  not registered  }
	ftRTFDocument				= $52544620 (* 'RTF ' *);						{  not registered  }
	sigWordPerfect				= $53534957 (* 'SSIW' *);
	ftWordPerfectDocument		= $57504430 (* 'WPD0' *);
	sigWordPerfect2				= $57504332 (* 'WPC2' *);
	ftWordPerfect2Document		= $57504431 (* 'WPD1' *);
	ftWordPerfect21Document		= $57504432 (* 'WPD2' *);
	ftWordPerfect42DocumentPC	= $2E575034 (* '.WP4' *);						{  not registered  }
	ftWordPerfect50DocumentPC	= $2E575035 (* '.WP5' *);						{  not registered  }
	ftWordPerfect51DocumentPC	= $57503531 (* 'WP51' *);						{  not registered  }
	ftWordPerfectGraphicsPC		= $57504766 (* 'WPGf' *);						{  not registered  }
	sigMacWriteII				= $4D574949 (* 'MWII' *);
	ftMacWriteIIDocument		= $4D573244 (* 'MW2D' *);
	sigWriteNow					= $6E585E6E (* 'nX^n' *);
	ftWriteNow2Document			= $6E585E64 (* 'nX^d' *);
	ftWriteNow3Document			= $6E585E32 (* 'nX^2' *);
	sigMacWrite					= $4D414341 (* 'MACA' *);
	ftMacWrite5Document			= $574F5244 (* 'WORD' *);
	sigFrameMaker				= $4672616D (* 'Fram' *);
	ftFrameMakerDocument		= $4641534C (* 'FASL' *);
	ftFrameMakerMIFDocument		= $4D494620 (* 'MIF ' *);
	ftFrameMakerMIF2Document	= $4D494632 (* 'MIF2' *);
	ftFrameMakerMIF3Document	= $4D494633 (* 'MIF3' *);
	sigMSWrite					= $4D535754 (* 'MSWT' *);
	sigActa						= $41435441 (* 'ACTA' *);
	sigTHINKPascal				= $504A4D4D (* 'PJMM' *);
	sigTHINKC					= $4B41484C (* 'KAHL' *);
	sigFullWrite				= $46575254 (* 'FWRT' *);
	sigTeachText				= $74747874 (* 'ttxt' *);
	ftTeachTextDocument			= $7474726F (* 'ttro' *);
	sigSimpleText				= $74747874 (* 'ttxt' *);
	ftSimpleTextDocument		= $7474726F (* 'ttro' *);
	sigMPWShell					= $4D505320 (* 'MPS ' *);
	sigQuarkXPress				= $58505233 (* 'XPR3' *);
	sigNisus					= $4E495349 (* 'NISI' *);
	sigOmniPage					= $50525443 (* 'PRTC' *);
	sigPersonalPress			= $53435047 (* 'SCPG' *);
	sigPublishItEZ				= $32435459 (* '2CTY' *);
	sigReadySetGo				= $4D454D52 (* 'MEMR' *);
	sigRagTime					= $52232B41 (* 'R#+A' *);
	sigLetraStudio				= $4C535450 (* 'LSTP' *);
	sigLetterPerfect			= $57504349 (* 'WPCI' *);
	sigTheWritingCenter			= $0A1A5750;					{  this 'unprintable unprintable WP' One of the unprintables is a line feed.   }
	sigInstantUpdate			= $49554130 (* 'IUA0' *);

																{  databases  }
	sig4thDimension				= $34443033 (* '4D03' *);
	ft4thDimensionDB			= $42415333 (* 'BAS3' *);
	sigFileMakerPro				= $464D5052 (* 'FMPR' *);
	ftFileMakerProDatabase		= $464D5052 (* 'FMPR' *);
	sigHyperCard				= $57494C44 (* 'WILD' *);
	ftHyperCard					= $5354414B (* 'STAK' *);
	sigSmartFormAsst			= $4B43464D (* 'KCFM' *);
	ftSmartFormAsst				= $5354434B (* 'STCK' *);
	sigSmartFormDesign			= $4B434644 (* 'KCFD' *);
	ftSmartFormDesign			= $4346524D (* 'CFRM' *);
	sigFileForce				= $34443933 (* '4D93' *);
	ftFileForceDatabase			= $46494C33 (* 'FIL3' *);
	sigFileMaker2				= $464D4B34 (* 'FMK4' *);
	ftFileMaker2Database		= $464D4B24 (* 'FMK$' *);
	sigSuperCard				= $52554E54 (* 'RUNT' *);
	sigDoubleHelix				= $48454C58 (* 'HELX' *);
	sigGeoQuery					= $52476771 (* 'RGgq' *);
	sigFoxBASE					= $464F582B (* 'FOX+' *);
	sigINSPIRATION				= $43455233 (* 'CER3' *);
	sigPanorama					= $4B415331 (* 'KAS1' *);
	sigSilverrunLDM				= $43444D4C (* 'CDML' *);
	sigSilverrunDFD				= $43444446 (* 'CDDF' *);						{  finance  }
	sigQuicken					= $494E5455 (* 'INTU' *);
	sigMacInTax91				= $4D495431 (* 'MIT1' *);
	ftMacInTax91				= $4D495446 (* 'MITF' *);
	sigAccountantInc			= $4150524F (* 'APRO' *);
	sigAtOnce					= $4B495353 (* 'KISS' *);
	sigCAT3						= $74436174 (* 'tCat' *);
	sigDollarsNSense			= $45414750 (* 'EAGP' *);
	sigInsightExpert			= $4C53474C (* 'LSGL' *);
	sigMYOB						= $4D594F42 (* 'MYOB' *);
	sigMacMoney					= $53534C41 (* 'SSLA' *);
	sigManagingYourMoney		= $4D594D43 (* 'MYMC' *);
	sigPlainsAndSimple			= $50454747 (* 'PEGG' *);						{  scheduling  }
	sigMacProject2				= $4D505258 (* 'MPRX' *);
	ftMacProject				= $4D505244 (* 'MPRD' *);
	sigMSProject				= $4D53504A (* 'MSPJ' *);
	sigMacProjectPro			= $4D505250 (* 'MPRP' *);						{  utilities  }
	sigStuffIt					= $53495421 (* 'SIT!' *);
	ftStuffItArchive			= $53495421 (* 'SIT!' *);
	sigCompactPro				= $43504354 (* 'CPCT' *);
	ftCompactProArchive			= $50414354 (* 'PACT' *);
	sigFontographer				= $61436132 (* 'aCa2' *);
	sigMetamorphosis			= $4D457450 (* 'MEtP' *);
	sigCorrectGrammar			= $4C734347 (* 'LsCG' *);
	sigDynodex					= $44594E4F (* 'DYNO' *);
	sigMariah					= $4D617248 (* 'MarH' *);
	sigAddressBook				= $4164426B (* 'AdBk' *);
	sigThePrintShop				= $50534850 (* 'PSHP' *);
	sigQuicKeys2				= $516B7932 (* 'Qky2' *);
	sigReadStar2Plus			= $494E4F56 (* 'INOV' *);
	sigSoftPC					= $50435854 (* 'PCXT' *);
	sigMacMenlo					= $4D4E4C4F (* 'MNLO' *);
	sigDisinfectant				= $44324354 (* 'D2CT' *);						{  communications  }
	sigSmartcom2				= $53434F4D (* 'SCOM' *);
	sigVersaTermPRO				= $5650524F (* 'VPRO' *);
	sigVersaTerm				= $5641544D (* 'VATM' *);
	sigWhiteKnight				= $574B3131 (* 'WK11' *);
	sigNCSATelnet				= $4E435341 (* 'NCSA' *);
	sigDynaComm					= $50415232 (* 'PAR2' *);
	sigQMForms					= $4D4C544D (* 'MLTM' *);						{  math and statistics  }
	sigMathematica				= $4F4D4547 (* 'OMEG' *);
	sigMathCAD					= $4D434144 (* 'MCAD' *);
	sigStatView2				= $53544154 (* 'STAT' *);
	sigDataDesk					= $4444534B (* 'DDSK' *);
	sigPowerMath2				= $4D415448 (* 'MATH' *);
	sigSuperANOVA				= $53757041 (* 'SupA' *);
	sigSystat					= $53595431 (* 'SYT1' *);
	sigTheorist					= $5468656F (* 'Theo' *);

																{  spreadsheets  }
	sigExcel					= $5843454C (* 'XCEL' *);
	ftExcel2Spreadsheet			= $584C5320 (* 'XLS ' *);
	ftExcel2Macro				= $584C4D20 (* 'XLM ' *);
	ftExcel2Chart				= $584C4320 (* 'XLC ' *);
	ftExcel3Spreadsheet			= $584C5333 (* 'XLS3' *);
	ftExcel3Macro				= $584C4D33 (* 'XLM3' *);
	ftExcel3Chart				= $584C4333 (* 'XLC3' *);
	ftExcel4Spreadsheet			= $584C5334 (* 'XLS4' *);
	ftExcel4Macro				= $584C4D34 (* 'XLM4' *);
	ftSYLKSpreadsheet			= $53594C4B (* 'SYLK' *);
	sigLotus123					= $4C313233 (* 'L123' *);
	ft123Spreadsheet			= $4C574B53 (* 'LWKS' *);
	sigWingz					= $574E475A (* 'WNGZ' *);
	ftWingzSpreadsheet			= $575A5353 (* 'WZSS' *);
	ftWingzScript				= $575A5343 (* 'WZSC' *);
	sigResolve					= $52736C76 (* 'Rslv' *);
	ftResolve					= $52735773 (* 'RsWs' *);
	ftResolveScript				= $52735763 (* 'RsWc' *);
	sigFullImpact2				= $466C7632 (* 'Flv2' *);

																{  graphics  }
	sigIllustrator				= $41525435 (* 'ART5' *);
	ftPostScriptMac				= $45505346 (* 'EPSF' *);
	sigMacPaint					= $4D504E54 (* 'MPNT' *);
	ftMacPaintGraphic			= $504E5447 (* 'PNTG' *);
	sigSuperPaint				= $53504E54 (* 'SPNT' *);
	ftSuperPaintGraphic			= $53505447 (* 'SPTG' *);
	sigCanvas					= $44414432 (* 'DAD2' *);
	ftCanvasGraphic				= $64727732 (* 'drw2' *);
	sigUltraPaint				= $554C5452 (* 'ULTR' *);
	ftUltraPaint				= $55504E54 (* 'UPNT' *);
	sigPhotoshop				= $3842494D (* '8BIM' *);
	ftPhotoshopGraphic			= $3842494D (* '8BIM' *);
	sigMacDrawPro				= $6450726F (* 'dPro' *);
	ftMacDrawProDrawing			= $64446F63 (* 'dDoc' *);
	sigPageMaker				= $414C4434 (* 'ALD4' *);
	ftPageMakerPublication		= $414C4234 (* 'ALB4' *);
	sigFreeHand					= $46484133 (* 'FHA3' *);
	ftFreeHandDrawing			= $46484433 (* 'FHD3' *);
	sigClarisCAD				= $43434144 (* 'CCAD' *);
	ftClarisCAD					= $43414432 (* 'CAD2' *);
	sigMacDrawII				= $4D44504C (* 'MDPL' *);
	ftMacDrawIIDrawing			= $44525747 (* 'DRWG' *);
	sigMacroMindDirector		= $4D4D4452 (* 'MMDR' *);
	ftMMDirectorMovie			= $56574D44 (* 'VWMD' *);
	ftMMDirectorSound			= $4D4D5344 (* 'MMSD' *);
	sigOptix					= $5049584C (* 'PIXL' *);						{  was previously PixelPerfect  }
	sigPixelPaint				= $50495852 (* 'PIXR' *);
	ftPixelPaint				= $50583031 (* 'PX01' *);
	sigAldusSuper3D				= $53503344 (* 'SP3D' *);
	ftSuper3DDrawing			= $33444258 (* '3DBX' *);
	sigSwivel3D					= $5357564C (* 'SWVL' *);
	ftSwivel3DDrawing			= $534D444C (* 'SMDL' *);
	sigCricketDraw				= $43524457 (* 'CRDW' *);
	ftCricketDrawing			= $434B4454 (* 'CKDT' *);
	sigCricketGraph				= $43475246 (* 'CGRF' *);
	ftCricketChart				= $43475043 (* 'CGPC' *);
	sigDesignCAD				= $41534243 (* 'ASBC' *);
	ftDesignCADDrawing			= $44434144 (* 'DCAD' *);
	sigImageStudio				= $46535045 (* 'FSPE' *);
	ftImageStudioGraphic		= $52494646 (* 'RIFF' *);
	sigVersaCad					= $56434144 (* 'VCAD' *);
	ftVersaCADDrawing			= $32442020 (* '2D  ' *);
	sigAdobePremiere			= $50724D72 (* 'PrMr' *);						{  was previously misspelled as sigAdobePremier }
	ftAdobePremiereMovie		= $4D6F6F56 (* 'MooV' *);						{  was previously misspelled as ftAdobePremierMovie }
	sigAfterDark				= $4144726B (* 'ADrk' *);
	ftAfterDarkModule			= $4144676D (* 'ADgm' *);
	sigClip3D					= $455A3345 (* 'EZ3E' *);
	ftClip3Dgraphic				= $455A3344 (* 'EZ3D' *);
	sigKaleidaGraph				= $514B5054 (* 'QKPT' *);
	ftKaleidaGraphGraphic		= $51504354 (* 'QPCT' *);
	sigMacFlow					= $4D43464C (* 'MCFL' *);
	ftMacFlowChart				= $464C4348 (* 'FLCH' *);
	sigMoviePlayer				= $54564F44 (* 'TVOD' *);
	ftMoviePlayerMovie			= $4D6F6F56 (* 'MooV' *);
	sigMacSpin					= $44325350 (* 'D2SP' *);
	ftMacSpinDataSet			= $4432424E (* 'D2BN' *);
	sigAutoCAD					= $41434144 (* 'ACAD' *);
	sigLabVIEW					= $4C425657 (* 'LBVW' *);
	sigColorMacCheese			= $434D43E2 (* 'CMC∆' *);
	sigMiniCad					= $43445033 (* 'CDP3' *);
	sigDreams					= $50484E58 (* 'PHNX' *);
	sigOmnis5					= $51322424 (* 'Q2$$' *);
	sigPhotoMac					= $504D4143 (* 'PMAC' *);
	sigGraphMaster				= $4752414D (* 'GRAM' *);
	sigInfiniD					= $5349E288 (* 'SI∞D' *);
	sigOfoto					= $41504C53 (* 'APLS' *);
	sigMacDraw					= $4D445257 (* 'MDRW' *);
	sigDeltagraphPro			= $44475248 (* 'DGRH' *);
	sigDesign2					= $44455347 (* 'DESG' *);
	sigDesignStudio				= $4D524A4E (* 'MRJN' *);
	sigDynaperspective			= $50455253 (* 'PERS' *);
	sigGenericCADD				= $43414433 (* 'CAD3' *);
	sigMacDraft					= $4D443230 (* 'MD20' *);
	sigModelShop				= $4D445350 (* 'MDSP' *);
	sigOasis					= $54414F41 (* 'TAOA' *);
	sigOBJECTMASTER				= $42524F57 (* 'BROW' *);
	sigMovieRecorder			= $6D726372 (* 'mrcr' *);
	sigPictureCompressor		= $70707869 (* 'ppxi' *);
	sigPICTViewer				= $4D445453 (* 'MDTS' *);
	sigSmoothie					= $536D6F6F (* 'Smoo' *);
	sigScreenPlay				= $53504C59 (* 'SPLY' *);
	sigStudio1					= $53542F31 (* 'ST/1' *);
	sigStudio32					= $53543332 (* 'ST32' *);
	sigStudio8					= $53542F38 (* 'ST/8' *);
	sigKidPix					= $4B696432 (* 'Kid2' *);
	sigDigDarkroom				= $44494452 (* 'DIDR' *);

																{  presentations  }
	sigMore						= $4D4F5232 (* 'MOR2' *);
	ftMore3Document				= $4D4F5233 (* 'MOR3' *);
	ftMore2Document				= $4D4F5232 (* 'MOR2' *);
	sigPersuasion				= $504C5032 (* 'PLP2' *);
	ftPersuasion1Presentation	= $50525331 (* 'PRS1' *);
	ftPersuasion2Presentation	= $50525332 (* 'PRS2' *);
	sigPowerPoint				= $50504E54 (* 'PPNT' *);
	ftPowerPointPresentation	= $534C4453 (* 'SLDS' *);
	sigCricketPresents			= $43525052 (* 'CRPR' *);
	ftCricketPresentation		= $50524446 (* 'PRDF' *);						{  works  }
	sigMSWorks					= $50534932 (* 'PSI2' *);
	sigMSWorks3					= $4D53574B (* 'MSWK' *);
	ftMSWorksWordProcessor		= $41575750 (* 'AWWP' *);
	ftMSWorksSpreadsheet		= $41575353 (* 'AWSS' *);
	ftMSWorksDataBase			= $41574442 (* 'AWDB' *);
	ftMSWorksComm				= $41574443 (* 'AWDC' *);
	ftMSWorksMacros				= $41574D43 (* 'AWMC' *);
	ftMSWorks1WordProcessor		= $41575731 (* 'AWW1' *);						{  not registered  }
	ftMSWorks1Spreadsheet		= $41575331 (* 'AWS1' *);						{  not registered  }
	ftMSWorks1DataBase			= $41574431 (* 'AWD1' *);						{  not registered  }
	ftMSWorks2WordProcessor		= $41575732 (* 'AWW2' *);						{  not registered  }
	ftMSWorks2Spreadsheet		= $41575332 (* 'AWS2' *);						{  not registered  }
	ftMSWorks2DataBase			= $41574432 (* 'AWD2' *);						{  not registered  }
	ftMSWorks3WordProcessor		= $41575733 (* 'AWW3' *);						{  not registered  }
	ftMSWorks3Spreadsheet		= $41575333 (* 'AWS3' *);						{  not registered  }
	ftMSWorks3DataBase			= $41574433 (* 'AWD3' *);						{  not registered  }
	ftMSWorks3Comm				= $41574333 (* 'AWC3' *);						{  not registered  }
	ftMSWorks3Macro				= $41574D33 (* 'AWM3' *);						{  not registered  }
	ftMSWorks3Draw				= $41575233 (* 'AWR3' *);						{  not registered  }
	ftMSWorks2WordProcessorPC	= $50575732 (* 'PWW2' *);						{  not registered  }
	ftMSWorks2DatabasePC		= $50574442 (* 'PWDB' *);						{  not registered  }
	sigGreatWorks				= $5A454252 (* 'ZEBR' *);
	ftGreatWorksWordProcessor	= $5A575254 (* 'ZWRT' *);
	ftGreatWorksSpreadsheet		= $5A43414C (* 'ZCAL' *);
	ftGreatWorksPaint			= $5A504E54 (* 'ZPNT' *);
	sigClarisWorks				= $424F424F (* 'BOBO' *);
	ftClarisWorksWordProcessor	= $43575750 (* 'CWWP' *);
	ftClarisWorksSpreadsheet	= $43575353 (* 'CWSS' *);
	ftClarisWorksGraphics		= $43574752 (* 'CWGR' *);
	sigBeagleWorks				= $42576B73 (* 'BWks' *);
	ftBeagleWorksWordProcessor	= $42577770 (* 'BWwp' *);
	ftBeagleWorksDatabase		= $42576462 (* 'BWdb' *);
	ftBeagleWorksSpreadsheet	= $42577373 (* 'BWss' *);
	ftBeagleWorksComm			= $4257636D (* 'BWcm' *);
	ftBeagleWorksDrawing		= $42576472 (* 'BWdr' *);
	ftBeagleWorksGraphic		= $42577074 (* 'BWpt' *);
	ftPICTFile					= $50494354 (* 'PICT' *);

																{  entertainment  }
	sigPGATourGolf				= $674F4C46 (* 'gOLF' *);
	sigSimCity					= $4D435250 (* 'MCRP' *);
	sigHellCats					= $48454C4C (* 'HELL' *);						{  education  }
	sigReaderRabbit3			= $52445233 (* 'RDR3' *);						{  Translation applications  }
	sigDataVizDesktop			= $44564454 (* 'DVDT' *);
	sigSotwareBridge			= $6D646F73 (* 'mdos' *);
	sigWordForWord				= $4D535452 (* 'MSTR' *);
	sigAppleFileExchange		= $50535054 (* 'PSPT' *);						{  Apple software  }
	sigAppleLink				= $47454F4C (* 'GEOL' *);
	ftAppleLinkAddressBook		= $41445253 (* 'ADRS' *);
	ftAppleLinkImageFile		= $53494D41 (* 'SIMA' *);
	ftAppleLinkPackage			= $48425346 (* 'HBSF' *);
	ftAppleLinkConnFile			= $50455445 (* 'PETE' *);
	ftAppleLinkHelp				= $484C5046 (* 'HLPF' *);
	sigInstaller				= $626A6263 (* 'bjbc' *);
	ftInstallerScript			= $626A6263 (* 'bjbc' *);
	sigDiskCopy					= $64437079 (* 'dCpy' *);
	ftDiskCopyImage				= $64496D67 (* 'dImg' *);
	sigResEdit					= $52534544 (* 'RSED' *);
	ftResEditResourceFile		= $72737263 (* 'rsrc' *);
	sigAardvark					= $41415244 (* 'AARD' *);
	sigCompatibilityChkr		= $776B7270 (* 'wkrp' *);
	sigMacTerminal				= $5465726D (* 'Term' *);
	sigSADE						= $73616465 (* 'sade' *);
	sigCurare					= $43757261 (* 'Cura' *);
	sigPCXChange				= $646F7361 (* 'dosa' *);
	sigAtEase					= $6D666472 (* 'mfdr' *);
	sigStockItToMe				= $5349544D (* 'SITM' *);
	sigAppleSearch				= $61736973 (* 'asis' *);
	sigAppleSearchToo			= $686F6273 (* 'hobs' *);						{  the following are files types for system files  }
	ftScriptSystemResourceCollection = $6966696C (* 'ifil' *);
	ftSoundFile					= $7366696C (* 'sfil' *);
	ftFontFile					= $6666696C (* 'ffil' *);
	ftTrueTypeFontFile			= $7466696C (* 'tfil' *);
	ftKeyboardLayout			= $6B66696C (* 'kfil' *);
	ftFontSuitcase				= $4646494C (* 'FFIL' *);
	ftDASuitcase				= $4446494C (* 'DFIL' *);
	ftSystemExtension			= $494E4954 (* 'INIT' *);
	ftDAMQueryDocument			= $71657279 (* 'qery' *);


	{	************* Special FileTypes and creators *************	}
	ftApplicationName			= $61706E6D (* 'apnm' *);						{  this is the type used to define the application name in a kind resource  }
	sigIndustryStandard			= $69737464 (* 'istd' *);						{  this is the creator used to define a kind string in a kind resource for a FileType that has many creators   }
	ftXTND13TextImport			= $78743133 (* 'xt13' *);						{  this is a pseduo-format used by "XTND for Apps". The taDstIsAppTranslation bit is set  }


	{	************* Apple][ applications and FileTypes *************	}
	sigAppleProDOS				= $70646F73 (* 'pdos' *);						{  not registered  }
	ftAppleWorksWordProcessor	= $31412020 (* '1A  ' *);						{  not registered  }
	ftAppleWorks1WordProcessor	= $31413120 (* '1A1 ' *);						{  not registered  }
	ftAppleWorks2WordProcessor	= $31413220 (* '1A2 ' *);						{  not registered  }
	ftAppleWorks3WordProcessor	= $31413320 (* '1A3 ' *);						{  not registered  }
	ftAppleWorksDataBase		= $31392020 (* '19  ' *);						{  not registered  }
	ftAppleWorks1DataBase		= $31393120 (* '191 ' *);						{  not registered  }
	ftAppleWorks2DataBase		= $31393220 (* '192 ' *);						{  not registered  }
	ftAppleWorks3DataBase		= $31393320 (* '193 ' *);						{  not registered  }
	ftAppleWorksSpreadsheet		= $31422020 (* '1B  ' *);						{  not registered  }
	ftAppleWorks1Spreadsheet	= $31423120 (* '1B1 ' *);						{  not registered  }
	ftAppleWorks2Spreadsheet	= $31423220 (* '1B2 ' *);						{  not registered  }
	ftAppleWorks3Spreadsheet	= $31423320 (* '1B3 ' *);						{  not registered  }
	ftAppleWorksWordProcessorGS	= $35302020 (* '50  ' *);						{  not registered  }
	ftApple2GS_SuperHiRes		= $41325355 (* 'A2SU' *);						{  not registered  }
	ftApple2GS_SuperHiResPacked	= $41325350 (* 'A2SP' *);						{  not registered  }
	ftApple2GS_PaintWorks		= $41325057 (* 'A2PW' *);						{  not registered  }
	ftApple2_DoubleHiRes		= $41324455 (* 'A2DU' *);						{  not registered  }
	ftApple2_DoubleHiResPacked	= $41324450 (* 'A2DP' *);						{  not registered  }
	ftApple2_DoubleHiRes16colors = $41324443 (* 'A2DC' *);						{  not registered  }
	ftApple2_SingleHiRes		= $41324855 (* 'A2HU' *);						{  not registered  }
	ftApple2_SingleHiResPacked	= $41324850 (* 'A2HP' *);						{  not registered  }
	ftApple2_SingleHiRes8colors	= $41324843 (* 'A2HC' *);						{  not registered  }


	{	************* PC-DOS applications and FileTypes *************	}
	sigPCDOS					= $6D646F73 (* 'mdos' *);						{  not registered  }
	ftGenericDocumentPC			= $54455854 (* 'TEXT' *);						{     word processor formats  }
	ftWordStarDocumentPC		= $57537472 (* 'WStr' *);						{  not registered  }
	ftWordStar4DocumentPC		= $57537434 (* 'WSt4' *);						{  not registered  }
	ftWordStar5DocumentPC		= $57537435 (* 'WSt5' *);						{  not registered  }
	ftWordStar55DocumentPC		= $57533535 (* 'WS55' *);						{  not registered  }
	ftWordStar6DocumentPC		= $57537436 (* 'WSt6' *);						{  not registered  }
	ftWordStar2000DocumentPC	= $57533230 (* 'WS20' *);						{  not registered  }
	ftXyWriteIIIDocumentPC		= $58795772 (* 'XyWr' *);						{  registered???  }
	ftDecDXDocumentPC			= $44582020 (* 'DX  ' *);						{  registered???  }
	ftDecWPSPlusDocumentPC		= $5750532B (* 'WPS+' *);						{  registered???  }
	ftDisplayWrite3DocumentPC	= $44573320 (* 'DW3 ' *);						{  registered???  }
	ftDisplayWrite4DocumentPC	= $44573420 (* 'DW4 ' *);						{  registered???  }
	ftDisplayWrite5DocumentPC	= $44573520 (* 'DW5 ' *);						{  registered???  }
	ftIBMWritingAsstDocumentPC	= $41535354 (* 'ASST' *);						{  registered???  }
	ftManuscript1DocumentPC		= $4D414E31 (* 'MAN1' *);						{  registered???  }
	ftManuscript2DocumentPC		= $4D414E32 (* 'MAN2' *);						{  registered???  }
	ftMass11PCDocumentPC		= $4D313150 (* 'M11P' *);						{  registered???  }
	ftMass11VaxDocumentPC		= $4D313156 (* 'M11V' *);						{  registered???  }
	ftMultiMateDocumentPC		= $4D4D4154 (* 'MMAT' *);						{  registered???  }
	ftMultiMate36DocumentPC		= $4D4D3336 (* 'MM36' *);						{  registered???  }
	ftMultiMate40DocumentPC		= $4D4D3430 (* 'MM40' *);						{  registered???  }
	ftMultiMateAdvDocumentPC	= $4D4D4144 (* 'MMAD' *);						{  registered???  }
	ftMultiMateNoteDocumentPC	= $4D4D4E54 (* 'MMNT' *);						{  registered???  }
	ftOfficeWriterDocumentPC	= $4F464657 (* 'OFFW' *);						{  registered???  }
	ftPCFileLetterDocumentPC	= $5043464C (* 'PCFL' *);						{  registered???  }
	ftPFSWriteADocumentPC		= $50465341 (* 'PFSA' *);						{  registered???  }
	ftPFSWriteBDocumentPC		= $50465342 (* 'PFSB' *);						{  registered???  }
	ftPFSPlanDocumentPC			= $50465350 (* 'PFSP' *);						{  registered???  }
	ftProWrite1DocumentPC		= $50573120 (* 'PW1 ' *);						{  registered???  }
	ftProWrite2DocumentPC		= $50573220 (* 'PW2 ' *);						{  registered???  }
	ftProWritePlusDocumentPC	= $50572B20 (* 'PW+ ' *);						{  registered???  }
	ftFirstChoiceDocumentPC		= $46434820 (* 'FCH ' *);						{  registered???  }
	ftFirstChoice3DocumentPC	= $46434833 (* 'FCH3' *);						{  registered???  }
	ftDCARFTDocumentPC			= $52465420 (* 'RFT ' *);						{  registered???  }
	ftSamnaDocumentPC			= $53414D4E (* 'SAMN' *);						{  registered???  }
	ftSmartDocumentPC			= $534D5254 (* 'SMRT' *);						{  registered???  }
	ftSprintDocumentPC			= $53505254 (* 'SPRT' *);						{  registered???  }
	ftTotalWordDocumentPC		= $544F544C (* 'TOTL' *);						{  registered???  }
	ftVolksWriterDocumentPC		= $564F4C4B (* 'VOLK' *);						{  registered???  }
	ftWangWPSDocumentPC			= $57505320 (* 'WPS ' *);						{  registered???  }
	ftWordMarcDocumentPC		= $4D415243 (* 'MARC' *);						{  registered???  }
	ftAmiDocumentPC				= $414D4920 (* 'AMI ' *);						{  registered???  }
	ftAmiProDocumentPC			= $4150524F (* 'APRO' *);						{  registered???  }
	ftAmiPro2DocumentPC			= $41505232 (* 'APR2' *);						{  registered???  }
	ftEnableDocumentPC			= $454E5750 (* 'ENWP' *);						{  registered???  }
																{     data base formats  }
	ftdBaseDatabasePC			= $44424620 (* 'DBF ' *);						{  registered???  }
	ftdBase3DatabasePC			= $44423320 (* 'DB3 ' *);						{  registered???  }
	ftdBase4DatabasePC			= $44423420 (* 'DB4 ' *);						{  registered???  }
	ftDataEaseDatabasePC		= $4454455A (* 'DTEZ' *);						{  registered???  }
	ftFrameWorkIIIDatabasePC	= $46574B33 (* 'FWK3' *);						{  registered???  }
	ftRBaseVDatabasePC			= $52427356 (* 'RBsV' *);						{  registered???  }
	ftRBase5000DatabasePC		= $52423530 (* 'RB50' *);						{  registered???  }
	ftRBaseFile1DatabasePC		= $52427331 (* 'RBs1' *);						{  registered???  }
	ftRBaseFile3DatabasePC		= $52427333 (* 'RBs3' *);						{  registered???  }
	ftReflexDatabasePC			= $52464C58 (* 'RFLX' *);						{  registered???  }
	ftQAWriteDatabasePC			= $51415754 (* 'QAWT' *);						{  registered???  }
	ftQADBaseDatabasePC			= $51414442 (* 'QADB' *);						{  registered???  }
	ftSmartDataBasePC			= $534D5444 (* 'SMTD' *);						{  registered???  }
	ftFirstChoiceDataBasePC		= $46434442 (* 'FCDB' *);						{  registered???  }

																{     spread sheet formats  }
	ftDIFSpreadsheetPC			= $44494620 (* 'DIF ' *);						{  registered???  }
	ftEnableSpreadsheetPC		= $454E4142 (* 'ENAB' *);						{  registered???  }
	ft123R1SpreadsheetPC		= $574B5331 (* 'WKS1' *);						{  registered???  }
	ft123R2SpreadsheetPC		= $574B5332 (* 'WKS2' *);						{  registered???  }
	ft123R3SpreadsheetPC		= $574B5333 (* 'WKS3' *);						{  registered???  }
	ftParadox3SpreadsheetPC		= $50445833 (* 'PDX3' *);						{  registered???  }
	ftParadox35SpreadsheetPC	= $50443335 (* 'PD35' *);						{  registered???  }
	ftQuattroSpreadsheetPC		= $5154524F (* 'QTRO' *);						{  registered???  }
	ftQuattroProSpreadsheetPC	= $51545235 (* 'QTR5' *);						{  registered???  }
	ftSuperCalc5SpreadsheetPC	= $53504335 (* 'SPC5' *);						{  registered???  }
	ftSymphony1SpreadsheetPC	= $53594D31 (* 'SYM1' *);						{  registered???  }
	ftTwinSpreadsheetPC			= $5457494E (* 'TWIN' *);						{  registered???  }
	ftVPPlannerSpreadsheetPC	= $5650504C (* 'VPPL' *);						{  registered???  }
	ftSmartSpeadsheetPC			= $534D5348 (* 'SMSH' *);						{  registered???  }
	ftFirstChoiceSpeadsheetPC	= $46435353 (* 'FCSS' *);						{  registered???  }
																{     graphics formats  }
	ftPCPaintBrushGraphicPC		= $50435820 (* 'PCX ' *);						{  not registered  }
	ftLotusPICGraphicPC			= $2E504943 (* '.PIC' *);						{  not registered  }
	ftCGMGraphicPC				= $2E43474D (* '.CGM' *);						{  not registered  }
	ftGEMGraphicPC				= $2E47454D (* '.GEM' *);						{  not registered  }
	ftIMGGraphicPC				= $2E494D47 (* '.IMG' *);						{  not registered  }
	ftDXFGraphicPC				= $2E445846 (* '.DXF' *);						{  not registered  }
	ftBitmapWindows				= $2E424D50 (* '.BMP' *);						{  not registered  }
	ftMetaFileWindows			= $2E574D46 (* '.WMF' *);						{  not registered  }
	ftTIFFGraphic				= $54494646 (* 'TIFF' *);						{  not registered  }
	ftPostScriptPC				= $45505350 (* 'EPSP' *);
	ftPostScriptWindows			= $45505357 (* 'EPSW' *);						{  not registered  }
	ftDigitalFX_TitleMan		= $5444494D (* 'TDIM' *);						{  registered???  }
	ftDigitalFX_VideoFX			= $47524146 (* 'GRAF' *);						{  registered???  }
	ftAutodeskFLIandFLC			= $464C4943 (* 'FLIC' *);						{  registered???  }
	ftGIF						= $47494666 (* 'GIFf' *);						{  registered???  }
	ftIFF						= $494C424D (* 'ILBM' *);						{  registered???  }
	ftMicrosoftPaint			= $2E4D5350 (* '.MSP' *);						{  registered???  }
	ftPixar						= $50584152 (* 'PXAR' *);						{  registered???  }
	ftQDV						= $2E514456 (* '.QDV' *);						{  registered???  }
	ftRLE_Compuserve			= $524C4543 (* 'RLEC' *);						{  registered???  }
																{     Generic vector formats  }
	ftIGESGraphicPC				= $49474553 (* 'IGES' *);						{  not registered  }
	ftDDES2GraphicPC			= $44444553 (* 'DDES' *);						{  not registered  }
	ft3DGFGraphicPC				= $33444746 (* '3DGF' *);						{  not registered  }
																{     Plotter formats  }
	ftHPGLGraphicPC				= $4850474C (* 'HPGL' *);						{  not registered  }
	ftDMPLGraphicPC				= $444D504C (* 'DMPL' *);						{  not registered  }
	ftCalComp906GraphicPC		= $43393036 (* 'C906' *);						{  not registered  }
	ftCalComp907GraphicPC		= $43393037 (* 'C907' *);						{  not registered  }
																{     Vendor-specific formats  }
	ftStereoLithographyGraphicPC = $53544C20 (* 'STL ' *);						{     3D Systems     - not registered  }
	ftZoomGraphicPC				= $5A4F4F4D (* 'ZOOM' *);						{     Abvent          - not registered  }
	ftFocusGraphicPC			= $464F4353 (* 'FOCS' *);						{     Abvent          - not registered  }
	ftWaveFrontGraphicPC		= $574F424A (* 'WOBJ' *);						{     WaveFront      - not registered  }
	ftSculpt4DGraphicPC			= $53636E32 (* 'Scn2' *);						{     Byte By Byte   - not registered  }
	ftMiniPascal3GraphicPC		= $4D505433 (* 'MPT3' *);						{     Graphsoft      - not registered  }
	ftMiniPascal4GraphicPC		= $4D505434 (* 'MPT4' *);						{     Graphsoft      - not registered  }
	ftWalkThroughGraphicPC		= $56574C4B (* 'VWLK' *);						{     Virtus          - not registered  }
	ftSiliconGraphics			= $2E534749 (* '.SGI' *);						{  registered???  }
	ftSunRaster					= $2E53554E (* '.SUN' *);						{  registered???  }
	ftTarga						= $54504943 (* 'TPIC' *);						{  registered???  }
																{  misc DOS   }
	ftDOSComPC					= $2E434F4D (* '.COM' *);						{  registered???  }
	ftDOSExecutablePC			= $2E455845 (* '.EXE' *);						{  registered???  }
	ftDOSArcPC					= $2E415243 (* '.ARC' *);						{  registered???  }
	ftAbekas					= $4142454B (* 'ABEK' *);						{  registered???  }
	ftDrHaloCUT					= $2E435554 (* '.CUT' *);						{  registered???  }
																{  misc Atari  }
	ftDegas						= $44454741 (* 'DEGA' *);						{  not registered  }
	ftNEO						= $2E4E454F (* '.NEO' *);						{  not registered  }


{$ALIGN MAC68K}


end.
