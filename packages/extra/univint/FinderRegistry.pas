{
     File:       FinderRegistry.p
 
     Contains:   Data types for Finder AppleEvents
 
     Version:    Technology: Mac OS 8
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1985-2002 by Apple Computer, Inc., all rights reserved.
 
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

unit FinderRegistry;
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
uses MacTypes,AERegistry,OSA;


{$ALIGN MAC68K}


{
  //////////////////////////////////////
   Finder Suite
  //////////////////////////////////////
}

{
   The old Finder Event suite was 'FNDR'
   The new suite is 'fndr'
}

const
	kAEFinderSuite				= $666E6472 (* 'fndr' *);

	{
	  //////////////////////////////////////
	   Finder Events
	  //////////////////////////////////////
	}
	kAECleanUp					= $66636C75 (* 'fclu' *);
	kAEEject					= $656A6374 (* 'ejct' *);
	kAEEmpty					= $656D7074 (* 'empt' *);
	kAEErase					= $66657261 (* 'fera' *);
	kAEGestalt					= $6773746C (* 'gstl' *);
	kAEPutAway					= $70747779 (* 'ptwy' *);
	kAERebuildDesktopDB			= $72646462 (* 'rddb' *);
	kAESync						= $66757064 (* 'fupd' *);
	kAEInterceptOpen			= $666F706E (* 'fopn' *);

	{  "Sort" from the database suite: }
	kAEDatabaseSuite			= $44415441 (* 'DATA' *);
	kAESort						= $534F5254 (* 'SORT' *);

	{
	  ////////////////////////////////////////////////////////////////////////
	   Classes
	   Note: all classes are defined up front so that the property definitions
	   can reference classes.
	  ////////////////////////////////////////////////////////////////////////
	}

	cInternalFinderObject		= $6F626A20 (* 'obj ' *);						{  cReference - used to distinguish objects used inside the Finder only }

	{
	   Main Finder class definitions
	   Indentation implies object model hierarchy
	}
																{  We do not use class cItem from AERegistry.r. Instead our class Item is a cObject }
																{          cItem                        = 'citm',   // defined in AERegistry.r }
																{           cFile                    = 'file',  // defined in AERegistry.r }
	cAliasFile					= $616C6961 (* 'alia' *);
	cApplicationFile			= $61707066 (* 'appf' *);
	cControlPanelFile			= $63636476 (* 'ccdv' *);
	cDeskAccessoryFile			= $64616669 (* 'dafi' *);
	cDocumentFile				= $646F6366 (* 'docf' *);
	cFontFile					= $666E7466 (* 'fntf' *);
	cSoundFile					= $736E6466 (* 'sndf' *);
	cClippingFile				= $636C7066 (* 'clpf' *);
	cContainer					= $63746E72 (* 'ctnr' *);
	cDesktop					= $6364736B (* 'cdsk' *);
	cSharableContainer			= $73637472 (* 'sctr' *);
	cDisk						= $63646973 (* 'cdis' *);
	cFolder						= $63666F6C (* 'cfol' *);
	cSuitcase					= $73746373 (* 'stcs' *);
	cAccessorySuitcase			= $64737574 (* 'dsut' *);
	cFontSuitcase				= $66737574 (* 'fsut' *);
	cTrash						= $63747273 (* 'ctrs' *);
	cDesktopPrinter				= $64736B70 (* 'dskp' *);
	cPackage					= $7061636B (* 'pack' *);
	cContentSpace				= $64776E64 (* 'dwnd' *);						{           cWindow                    = 'cwin',       // defined in AERegistry.r }
	cContainerWindow			= $63776E64 (* 'cwnd' *);
	cInfoWindow					= $69776E64 (* 'iwnd' *);
	cSharingWindow				= $73776E64 (* 'swnd' *);
	cStatusWindow				= $71776E64 (* 'qwnd' *);
	cClippingWindow				= $6C776E64 (* 'lwnd' *);
	cPreferencesWindow			= $70776E64 (* 'pwnd' *);
	cDTPWindow					= $64747077 (* 'dtpw' *);
	cProcess					= $70726373 (* 'prcs' *);
	cAccessoryProcess			= $70636461 (* 'pcda' *);
	cApplicationProcess			= $70636170 (* 'pcap' *);
	cGroup						= $73677270 (* 'sgrp' *);
	cUser						= $63757365 (* 'cuse' *);						{          cApplication                  = 'capp',     // defined in AERegistry.r }
	cSharingPrivileges			= $70726976 (* 'priv' *);
	cPreferences				= $63707266 (* 'cprf' *);
	cLabel						= $636C626C (* 'clbl' *);
	cSound						= $736E6420 (* 'snd ' *);
	cAliasList					= $616C7374 (* 'alst' *);
	cSpecialFolders				= $7370666C (* 'spfl' *);						{  For use by viewer search engines: }
	cOnlineDisk					= $636F6473 (* 'cods' *);
	cOnlineLocalDisk			= $636C6473 (* 'clds' *);
	cOnlineRemoteDisk			= $63726473 (* 'crds' *);						{  Miscellaneous class definitions }
	cEntireContents				= $65637473 (* 'ects' *);
	cIconFamily					= $6966616D (* 'ifam' *);


	{
	  //////////////////////////////////////
	   Properties
	  //////////////////////////////////////
	}

	{  Properties of class cItem (really cObject) }
																{     pBounds                        = 'pbnd',       // defined in AERegistry.r }
	pComment					= $636F6D74 (* 'comt' *);
	pContainer					= $63746E72 (* 'ctnr' *);
	pContentSpace				= $64776E64 (* 'dwnd' *);
	pCreationDateOld			= $63727464 (* 'crtd' *);						{  to support pre-Finder 8 scripts }
	pCreationDate				= $61736364 (* 'ascd' *);						{  from File Commands OSAX }
	pDescription				= $64736372 (* 'dscr' *);
	pDisk						= $63646973 (* 'cdis' *);
	pFolderOld					= $63666F6C (* 'cfol' *);						{  to support pre-Finder 8 scripts }
	pFolder						= $61736472 (* 'asdr' *);						{  from File Commands OSAX }
	pIconBitmap					= $69696D67 (* 'iimg' *);						{     pID                           = 'ID  ',        // defined in AERegistry.r }
	pInfoWindow					= $69776E64 (* 'iwnd' *);
	pKind						= $6B696E64 (* 'kind' *);
	pLabelIndex					= $6C616269 (* 'labi' *);
	pModificationDateOld		= $6D6F6464 (* 'modd' *);						{  to support pre-Finder 8 scripts }
	pModificationDate			= $61736D6F (* 'asmo' *);						{  from File Commands OSAX }
																{     pName                      = 'pnam',         // defined in AERegistry.r }
	pPhysicalSize				= $70687973 (* 'phys' *);
	pPosition					= $706F736E (* 'posn' *);
	pIsSelected					= $6973736C (* 'issl' *);
	pSize						= $7074737A (* 'ptsz' *);						{  pPointSize defined in AERegistry.r }
	pWindow						= $6377696E (* 'cwin' *);
	pPreferencesWindow			= $70776E64 (* 'pwnd' *);


	{  Properties of class cFile (subclass of cItem) }
	pFileCreator				= $66637274 (* 'fcrt' *);
	pFileType					= $61737479 (* 'asty' *);						{  from File Commands OSAX }
	pFileTypeOld				= $66697470 (* 'fitp' *);						{  to support pre-Finder 8 scripts }
	pIsLocked					= $61736C6B (* 'aslk' *);						{  from File Commands OSAX }
	pIsLockedOld				= $69736C6B (* 'islk' *);						{  to support pre-Finder 8 scripts }
																{     pIsStationeryPad               = 'pspd',         // defined in AERegistry.r                 }
																{     pVersion                    = 'vers',       // defined in AERegistry.r }
	pProductVersion				= $76657232 (* 'ver2' *);


	{  Properties of class cAliasFile (subclass of cFile) }
	pOriginalItem				= $6F726967 (* 'orig' *);

	{  Properties of class cApplicationFile (subclass of cFile) }
	pMinAppPartition			= $6D707274 (* 'mprt' *);
	pAppPartition				= $61707074 (* 'appt' *);
	pSuggestedAppPartition		= $73707274 (* 'sprt' *);
	pIsScriptable				= $69736162 (* 'isab' *);

	{  Properties of class cURLFile (subclass of cFile) }
	pInternetLocation			= $696C6F63 (* 'iloc' *);

	{  Properties of class cSoundFile (subclass of cFile) }
	pSound						= $736E6420 (* 'snd ' *);


	{
	   Properties of class cControlPanel (Views CP only) (subclass of cFile)
	   Note: the other view-like preference settings are not available in the Views
	   control panel. These properties are only offered here for backward compatability.
	   To set the full range of Finder Preferences, use the Preferences object.
	}
	pShowFolderSize				= $7366737A (* 'sfsz' *);						{  Moved to a per-folder basis in Finder 8.0 HIS }
	pShowComment				= $73636F6D (* 'scom' *);						{  Moved to a per-folder basis in Finder 8.0 HIS }
	pShowDate					= $73646174 (* 'sdat' *);						{  Moved to a per-folder basis in Finder 8.0 HIS }
	pShowCreationDate			= $73636461 (* 'scda' *);						{  Moved to a per-folder basis in Finder 8.0 HIS }
	pShowKind					= $736B6E64 (* 'sknd' *);						{  Moved to a per-folder basis in Finder 8.0 HIS }
	pShowLabel					= $736C626C (* 'slbl' *);						{  Moved to a per-folder basis in Finder 8.0 HIS }
	pShowSize					= $7373697A (* 'ssiz' *);						{  Moved to a per-folder basis in Finder 8.0 HIS }
	pShowVersion				= $73767273 (* 'svrs' *);						{  Moved to a per-folder basis in Finder 8.0 HIS }
	pSortDirection				= $736F7264 (* 'sord' *);
	pShowDiskInfo				= $7364696E (* 'sdin' *);						{  Always on in Finder 8.0 HIS }
	pListViewIconSize			= $6C766973 (* 'lvis' *);						{  Moved to a per-folder basis in Finder 8.0 HIS }
	pGridIcons					= $66677264 (* 'fgrd' *);						{  Moved to a per-folder basis in Finder 8.0 HIS }
	pStaggerIcons				= $66737467 (* 'fstg' *);						{  No longer part of the Finder 8.0 HIS }
	pViewFont					= $76666E74 (* 'vfnt' *);
	pViewFontSize				= $7666737A (* 'vfsz' *);

	{  Properties of class cContainer (subclass of cItem) }
	pCompletelyExpanded			= $70657863 (* 'pexc' *);
	pContainerWindow			= $63776E64 (* 'cwnd' *);
	pEntireContents				= $65637473 (* 'ects' *);
	pExpandable					= $70657861 (* 'pexa' *);
	pExpanded					= $70657870 (* 'pexp' *);
	pPreviousView				= $73766577 (* 'svew' *);						{     pSelection                    = 'sele',       // defined in AERegistry.r }
	pView						= $70766577 (* 'pvew' *);
	pIconSize					= $6C766973 (* 'lvis' *);						{  defined above }
	pKeepArranged				= $61727267 (* 'arrg' *);						{  OBSOLETE in Finder 9 or later }
	pKeepArrangedBy				= $61726279 (* 'arby' *);						{  OBSOLETE in Finder 9 or later }

	{  Properties of class cDesktop (subclass of cContainer) }
	pStartupDisk				= $7364736B (* 'sdsk' *);
	pTrash						= $74727368 (* 'trsh' *);

	{  Properties of class cSharableContainer (subclass of cContainer) }
	pOwner						= $736F776E (* 'sown' *);
	pOwnerPrivileges			= $6F776E72 (* 'ownr' *);
	pGroup						= $73677270 (* 'sgrp' *);
	pGroupPrivileges			= $67707072 (* 'gppr' *);
	pGuestPrivileges			= $67737470 (* 'gstp' *);
	pArePrivilegesInherited		= $69707276 (* 'iprv' *);
	pExported					= $73657870 (* 'sexp' *);
	pMounted					= $736D6F75 (* 'smou' *);
	pSharingProtection			= $7370726F (* 'spro' *);
	pSharing					= $73686172 (* 'shar' *);
	pSharingWindow				= $73776E64 (* 'swnd' *);

	{  Properties of class cDisk (subclass of cSharableContainer) }
	pCapacity					= $63617061 (* 'capa' *);
	pEjectable					= $6973656A (* 'isej' *);
	pFreeSpace					= $66727370 (* 'frsp' *);
	pLocal						= $69737276 (* 'isrv' *);
	pIsStartup					= $69737464 (* 'istd' *);

	{  Properties of class cTrash (subclass of cSharableContainer) }
	pWarnOnEmpty				= $7761726E (* 'warn' *);

	{  Properties of class cWindow (subclass of cContentSpace) }
																{     pBounds                        = 'pbnd',   // defined in AERegistry.r }
																{     pHasCloseBox                = 'hclb',     // defined in AERegistry.r }
																{     pIsFloating                    = 'isfl',     // defined in AERegistry.r }
																{     pIndex                     = 'pidx',     // defined in AERegistry.r }
																{     pIsModal                    = 'pmod',   // defined in AERegistry.r }
																{     pPosition                    = 'posn',     // defined above }
																{     pIsResizable                = 'prsz',     // defined in AERegistry.r }
																{     pHasTitleBar                = 'ptit',     // defined in AERegistry.r }
																{     pVisible                    = 'pvis',   // defined in AERegistry.r }
																{     pIsZoomable                    = 'iszm',     // defined in AERegistry.r }
																{     pIsZoomed                    = 'pzum',     // defined in AERegistry.r }
	pIsZoomedFull				= $7A756D66 (* 'zumf' *);
	pIsPopup					= $64727772 (* 'drwr' *);
	pIsPulledOpen				= $70756C6C (* 'pull' *);						{  only applies to popup windows }
	pIsCollapsed				= $77736864 (* 'wshd' *);						{  only applies to normal windows }

	{  Properties of class cContainerWindow (subclass of cWindow) }
	pObject						= $636F626A (* 'cobj' *);

	{  Properties of class cSharingWindow (subclass of cWindow) }
	pSharableContainer			= $73637472 (* 'sctr' *);

	{  Properties of class cInfoWindow (subclass of cWindow) }
	pInfoPanel					= $70616E6C (* 'panl' *);


	{  Properties of networking support }
	pFileShareOn				= $66736872 (* 'fshr' *);
	pFileShareStartingUp		= $66737570 (* 'fsup' *);
	pProgramLinkingOn			= $69616320 (* 'iac ' *);

	{  Properties of class cPreferencesWindow (subclass of cWindow) }
																{     pShowFolderSize                   = 'sfsz',         // defined above for Views CP }
																{     pShowComment                = 'scom',      // defined above for Views CP }
	pShowModificationDate		= $73646174 (* 'sdat' *);						{  pShowDate defined above for Views CP }
																{     pShowKind                    = 'sknd',        // defined above for Views CP }
																{     pShowLabel                    = 'slbl',         // defined above for Views CP }
																{     pShowSize                    = 'ssiz',        // defined above for Views CP }
																{     pShowVersion                = 'svrs',      // defined above for Views CP }
																{     pShowCreationDate             = 'scda',      // Removed from Finder 8.0 HIS }
																{     pShowFileType                 = 'sfty',       // Removed from Finder 8.0 HIS }
																{     pShowFileCreator               = 'sfcr',         // Removed from Finder 8.0 HIS }
																{     pListViewIconSize             = 'lvis',      // defined above for Views CP }
																{     pGridIcons                    = 'fgrd',         // defined above for Views CP }
																{     pStaggerIcons                 = 'fstg',       // defined above for Views CP }
																{     pViewFont                    = 'vfnt',        // defined above for Views CP }
																{     pViewFontSize                 = 'vfsz',       // defined above for Views CP }
	pUseRelativeDate			= $75726474 (* 'urdt' *);						{  Moved to a per-folder basis in Finder 8.0 HIS }
	pDelayBeforeSpringing		= $64656C61 (* 'dela' *);
	pSpringOpenFolders			= $73707267 (* 'sprg' *);
	pUseShortMenus				= $75736D65 (* 'usme' *);
	pUseWideGrid				= $75737767 (* 'uswg' *);
	pLabel1						= $6C626C31 (* 'lbl1' *);
	pLabel2						= $6C626C32 (* 'lbl2' *);
	pLabel3						= $6C626C33 (* 'lbl3' *);
	pLabel4						= $6C626C34 (* 'lbl4' *);
	pLabel5						= $6C626C35 (* 'lbl5' *);
	pLabel6						= $6C626C36 (* 'lbl6' *);
	pLabel7						= $6C626C37 (* 'lbl7' *);
	pDefaultIconViewIconSize	= $6969737A (* 'iisz' *);
	pDefaultButtonViewIconSize	= $6269737A (* 'bisz' *);
	pDefaultListViewIconSize	= $6C69737A (* 'lisz' *);						{  old use of this name is now pIconSize }
	pIconViewArrangement		= $69617272 (* 'iarr' *);
	pButtonViewArrangement		= $62617272 (* 'barr' *);

	{
	   The next bunch are the various arrangements that make up
	   enumArrangement
	}
	pNoArrangement				= $6E617272 (* 'narr' *);
	pSnapToGridArrangement		= $67726461 (* 'grda' *);
	pByNameArrangement			= $6E616D61 (* 'nama' *);
	pByModificationDateArrangement = $6D647461 (* 'mdta' *);
	pByCreationDateArrangement	= $63647461 (* 'cdta' *);
	pBySizeArrangement			= $73697A61 (* 'siza' *);
	pByKindArrangement			= $6B696E61 (* 'kina' *);
	pByLabelArrangement			= $6C616261 (* 'laba' *);

	{   #define pObject                                 cObject         // defined above }

	{  Properties of class cProcess (subclass of cObject) }
																{     pName                      = 'pnam',         // defined in AERegistry.r }
	pFile						= $66696C65 (* 'file' *);						{     pCreatorType                = 'fcrt',      // defined above }
																{     pFileType                    = 'asty',        // defined above }
																{     pIsFrontProcess                   = 'pisf',         // defined in AERegistry.r }
																{     pAppPartition                 = 'appt',       // defined above }
	pPartitionSpaceUsed			= $70757364 (* 'pusd' *);						{     pIsScriptable                 = 'isab',       // defined in AERegistry.r }
																{     pVisible                    = 'pvis'      // defined in AERegistry.r }
	pLocalAndRemoteEvents		= $72657674 (* 'revt' *);
	pHasScriptingTerminology	= $68736372 (* 'hscr' *);

	{  Properties of class cAccessoryProcess (subclass of cProcess) }
	pDeskAccessoryFile			= $64616669 (* 'dafi' *);

	{  Properties of class cApplicationProcess (subclass of cProcess) }
	pApplicationFile			= $61707066 (* 'appf' *);

	{
	   Properties of class cGroup (subclass of cObject)
	  enum (
	    pBounds
	    pIconBitmap
	    pLabelIndex
	    pName
	    pPosition
	    pWindow                                 = cWindow           // defined above
	  );
	}

	{  Properties of class cUser (subclass of cObject) }
																{     pBounds }
																{     pIconBitmap }
																{     pLabelIndex }
																{     pName }
																{     pPosition }
																{     pWindow                        = cWindow,        // defined above }
	pCanConnect					= $63636F6E (* 'ccon' *);
	pCanChangePassword			= $63637077 (* 'ccpw' *);
	pCanDoProgramLinking		= $63696163 (* 'ciac' *);
	pIsOwner					= $69736F77 (* 'isow' *);
	pARADialIn					= $61726164 (* 'arad' *);
	pShouldCallBack				= $63616C62 (* 'calb' *);
	pCallBackNumber				= $63626E6D (* 'cbnm' *);

	{
	   Properties of class cApplication (subclass of cObject)
	   NOTE: properties for the special folders must match their respective kXXXFolderType constants
	}
	pAboutMacintosh				= $61626278 (* 'abbx' *);
	pAppleMenuItemsFolder		= $616D6E75 (* 'amnu' *);						{  kAppleMenuFolderType }
																{     pClipboard                    = 'pcli',         // defined in AERegistry.r }
	pControlPanelsFolder		= $6374726C (* 'ctrl' *);						{  kControlPanelFolderType }
	pDesktop					= $6465736B (* 'desk' *);						{  kDesktopFolderType }
	pExtensionsFolder			= $6578746E (* 'extn' *);						{  kExtensionFolderType }
																{     pFileShareOn                = 'fshr',      // defined above }
	pFinderPreferences			= $70667270 (* 'pfrp' *);
	pFontsFolder				= $666F6E74 (* 'font' *);
	pFontsFolderPreAllegro		= $66666E74 (* 'ffnt' *);						{  DO NOT USE THIS - FOR BACKWARDS COMPAT ONLY }
																{     pIsFrontProcess                   = 'pisf',         // defined in AERegistry.r }
																{     pInsertionLoc                 = 'pins',       // defined in AERegistry.r }
	pLargestFreeBlock			= $6D667265 (* 'mfre' *);
	pPreferencesFolder			= $70726566 (* 'pref' *);						{  kPreferencesFolderType }
																{     pProductVersion                   = 'ver2',         // defined above }
																{     pUserSelection                  = 'pusl',        // defined in AERegistry.r }
																{     pFileShareStartingUp             = 'fsup',        // defined above }
	pShortCuts					= $73637574 (* 'scut' *);
	pShutdownFolder				= $73686466 (* 'shdf' *);
	pStartupItemsFolder			= $73747274 (* 'strt' *);						{  kStartupFolderType }
	pSystemFolder				= $6D616373 (* 'macs' *);						{  kSystemFolderType }
	pTemporaryFolder			= $74656D70 (* 'temp' *);						{  kTemporaryFolderType }
																{     pVersion                    = 'vers',       // defined in AERegistry.r }
	pViewPreferences			= $70767770 (* 'pvwp' *);						{     pVisible                    = 'pvis',       // defined in AERegistry.r }
	pStartingUp					= $6177616B (* 'awak' *);						{  private property to tell whether the Finder is fully up and running }

	{  Properties of class cSharingPrivileges (subclass of cObject) }
	pSeeFiles					= $70727672 (* 'prvr' *);
	pSeeFolders					= $70727673 (* 'prvs' *);
	pMakeChanges				= $70727677 (* 'prvw' *);

	{
	   Properties of class cPreferences (subclass of cObject)
	  enum (
	    pShowFolderSize                         = 'sfsz',           // defined above for Views CP
	    pShowComment                            = 'scom',           // defined above for Views CP
	    pShowModificationDate                   = pShowDate,            // pShowDate defined above for Views CP
	    pShowKind                               = 'sknd',           // defined above for Views CP
	    pShowLabel                              = 'slbl',           // defined above for Views CP
	    pShowSize                               = 'ssiz',           // defined above for Views CP
	    pShowVersion                            = 'svrs',           // defined above for Views CP
	    pShowCreationDate                       = 'scda',           // defined in cPreferencesWindow
	    pShowFileType                           = 'sfty',           // defined in cPreferencesWindow
	    pShowFileCreator                        = 'sfcr',           // defined in cPreferencesWindow
	    pListViewIconSize                       = 'lvis',           // defined above for Views CP
	    pGridIcons                              = 'fgrd',           // defined above for Views CP
	    pStaggerIcons                           = 'fstg',           // defined above for Views CP
	    pViewFont                               = 'vfnt',           // defined above for Views CP
	    pViewFontSize                           = 'vfsz',           // defined above for Views CP
	    pUseRelativeDate                        = 'urdt',           // defined in cPreferencesWindow
	    pDelayBeforeSpringing                   = 'dela',           // defined in cPreferencesWindow
	    pShowMacOSFolder                        = 'sosf',           // defined in cPreferencesWindow
	    pUseShortMenus                          = 'usme',           // defined in cPreferencesWindow
	    pUseCustomNewMenu                       = 'ucnm',           // defined in cPreferencesWindow
	    pShowDesktopInBackground                = 'sdtb',           // defined in cPreferencesWindow
	    pActivateDesktopOnClick                 = 'adtc',           // defined in cPreferencesWindow
	    pLabel1                                 = 'lbl1',           // defined in cPreferencesWindow
	    pLabel2                                 = 'lbl2',           // defined in cPreferencesWindow
	    pLabel3                                 = 'lbl3',           // defined in cPreferencesWindow
	    pLabel4                                 = 'lbl4',           // defined in cPreferencesWindow
	    pLabel5                                 = 'lbl5',           // defined in cPreferencesWindow
	    pLabel6                                 = 'lbl6',           // defined in cPreferencesWindow
	    pLabel7                                 = 'lbl7',           // defined in cPreferencesWindow
	    pWindow                                 = cWindow           // defined above
	  );
	}

	{
	   Properties of class cLabel (subclass of cObject)
	  enum (
	    pName                                   = 'pnam',           // defined in AERegistry.r
	    pColor                                  = 'colr',           // defined in AERegistry.r
	  );
	}

	{  Misc Properties }
	pSmallIcon					= $736D6963 (* 'smic' *);
	pSmallButton				= $736D6275 (* 'smbu' *);
	pLargeButton				= $6C676275 (* 'lgbu' *);
	pGrid						= $67726964 (* 'grid' *);

	{
	  //////////////////////////////////////
	   Enumerations defined by the Finder
	  //////////////////////////////////////
	}

	enumViewBy					= $76776279 (* 'vwby' *);
	enumGestalt					= $6773656E (* 'gsen' *);
	enumConflicts				= $63666C63 (* 'cflc' *);
	enumExistingItems			= $65787369 (* 'exsi' *);
	enumOlderItems				= $6F6C6472 (* 'oldr' *);

	enumDate					= $656E6461 (* 'enda' *);
	enumAnyDate					= $616E7964 (* 'anyd' *);
	enumToday					= $74646179 (* 'tday' *);
	enumYesterday				= $79646179 (* 'yday' *);
	enumThisWeek				= $7477656B (* 'twek' *);
	enumLastWeek				= $6C77656B (* 'lwek' *);
	enumThisMonth				= $746D6F6E (* 'tmon' *);
	enumLastMonth				= $6C6D6F6E (* 'lmon' *);
	enumThisYear				= $74796572 (* 'tyer' *);
	enumLastYear				= $6C796572 (* 'lyer' *);
	enumBeforeDate				= $62666474 (* 'bfdt' *);
	enumAfterDate				= $61666474 (* 'afdt' *);
	enumBetweenDate				= $62746474 (* 'btdt' *);
	enumOnDate					= $6F6E6474 (* 'ondt' *);

	enumAllDocuments			= $616C6C64 (* 'alld' *);
	enumFolders					= $666F6C64 (* 'fold' *);
	enumAliases					= $616C6961 (* 'alia' *);
	enumStationery				= $73746174 (* 'stat' *);

	enumWhere					= $77686572 (* 'wher' *);
	enumAllLocalDisks			= $616C646B (* 'aldk' *);
	enumAllRemoteDisks			= $6172646B (* 'ardk' *);
	enumAllDisks				= $616C6C64 (* 'alld' *);
	enumAllOpenFolders			= $616F666F (* 'aofo' *);


	enumIconSize				= $6973697A (* 'isiz' *);
	enumSmallIconSize			= $736D6963 (* 'smic' *);
	enumMiniIconSize			= $6D696963 (* 'miic' *);
	enumLargeIconSize			= $6C676963 (* 'lgic' *);

	enumSortDirection			= $736F6472 (* 'sodr' *);
	enumSortDirectionNormal		= $736E726D (* 'snrm' *);
	enumSortDirectionReverse	= $73727673 (* 'srvs' *);

	enumArrangement				= $65617272 (* 'earr' *);

	{  Get Info Window panel enumeration }
	enumInfoWindowPanel			= $69706E6C (* 'ipnl' *);
	enumGeneralPanel			= $67706E6C (* 'gpnl' *);
	enumSharingPanel			= $73706E6C (* 'spnl' *);
	enumStatusNConfigPanel		= $73636E6C (* 'scnl' *);
	enumFontsPanel				= $66706E6C (* 'fpnl' *);
	enumMemoryPanel				= $6D706E6C (* 'mpnl' *);


	{  Preferences panel enumeration }
	enumPrefsWindowPanel		= $70706C65 (* 'pple' *);
	enumPrefsGeneralPanel		= $70676E70 (* 'pgnp' *);
	enumPrefsLabelPanel			= $706C6270 (* 'plbp' *);
	enumPrefsIconViewPanel		= $70697670 (* 'pivp' *);
	enumPrefsButtonViewPanel	= $70627670 (* 'pbvp' *);
	enumPrefsListViewPanel		= $706C7670 (* 'plvp' *);

	{
	  //////////////////////////////////////
	   Types defined by the Finder
	  //////////////////////////////////////
	}

	typeIconFamily				= $6966616D (* 'ifam' *);						{  An AEList of typeIconAndMask, type8BitIcon, & c. }
	typeIconAndMask				= $49434E23 (* 'ICN#' *);
	type8BitMask				= $6C386D6B (* 'l8mk' *);
	type32BitIcon				= $696C3332 (* 'il32' *);
	type8BitIcon				= $69636C38 (* 'icl8' *);
	type4BitIcon				= $69636C34 (* 'icl4' *);
	typeSmallIconAndMask		= $69637323 (* 'ics#' *);
	typeSmall8BitMask			= $73386D6B (* 's8mk' *);
	typeSmall32BitIcon			= $69733332 (* 'is32' *);
	typeSmall8BitIcon			= $69637338 (* 'ics8' *);
	typeSmall4BitIcon			= $69637334 (* 'ics4' *);
	typeRelativeTime			= $7274696D (* 'rtim' *);
	typeConceptualTime			= $74696D63 (* 'timc' *);

	{
	  //////////////////////////////////////
	   Keywords defined by the Finder
	  //////////////////////////////////////
	}

	keyIconAndMask				= $49434E23 (* 'ICN#' *);
	key32BitIcon				= $696C3332 (* 'il32' *);
	key8BitIcon					= $69636C38 (* 'icl8' *);
	key4BitIcon					= $69636C34 (* 'icl4' *);
	key8BitMask					= $6C386D6B (* 'l8mk' *);
	keySmallIconAndMask			= $69637323 (* 'ics#' *);
	keySmall8BitIcon			= $69637338 (* 'ics8' *);
	keySmall4BitIcon			= $69637334 (* 'ics4' *);
	keySmall32BitIcon			= $69733332 (* 'is32' *);
	keySmall8BitMask			= $73386D6B (* 's8mk' *);
	keyMini1BitMask				= $69636D23 (* 'icm#' *);
	keyMini4BitIcon				= $69636D34 (* 'icm4' *);
	keyMini8BitIcon				= $69636D38 (* 'icm8' *);
	keyAEUsing					= $7573696E (* 'usin' *);
	keyAEReplacing				= $616C7270 (* 'alrp' *);
	keyAENoAutoRouting			= $726F7574 (* 'rout' *);
	keyLocalPositionList		= $6D76706C (* 'mvpl' *);
	keyGlobalPositionList		= $6D767067 (* 'mvpg' *);
	keyRedirectedDocumentList	= $6670646C (* 'fpdl' *);

	{
	  //////////////////////////////////////
	   New prepositions used by the Finder
	  //////////////////////////////////////
	}

	keyASPrepositionHas			= $68617320 (* 'has ' *);
	keyAll						= $6B79616C (* 'kyal' *);
	keyOldFinderItems			= $6673656C (* 'fsel' *);

	{
	  //////////////////////////////////////
	   New key forms used by the Finder
	  //////////////////////////////////////
	}

	formAlias					= $616C6973 (* 'alis' *);
	formCreator					= $66637274 (* 'fcrt' *);


	{
	  //////////////////////////////////////
	   Finder error codes
	  //////////////////////////////////////
	}

	errFinderIsBusy				= -15260;
	errFinderWindowNotOpen		= -15261;
	errFinderCannotPutAway		= -15262;
	errFinderWindowMustBeIconView = -15263;						{  RequireWindowInIconView }
	errFinderWindowMustBeListView = -15264;						{  RequireWindowInListView }
	errFinderCantMoveToDestination = -15265;
	errFinderCantMoveSource		= -15266;
	errFinderCantOverwrite		= -15267;
	errFinderIncestuousMove		= -15268;						{  Could just use errFinderCantMoveSource }
	errFinderCantMoveToAncestor	= -15269;						{  Could also use errFinderCantMoveSource }
	errFinderCantUseTrashedItems = -15270;
	errFinderItemAlreadyInDest	= -15271;						{  Move from folder A to folder A }
	errFinderUnknownUser		= -15272;						{  Includes unknown group }
	errFinderSharePointsCantInherit = -15273;
	errFinderWindowWrongType	= -15274;
	errFinderPropertyNowWindowBased = -15275;
	errFinderAppFolderProtected	= -15276;						{  used by General controls when folder protection is on }
	errFinderSysFolderProtected	= -15277;						{  used by General controls when folder protection is on }
	errFinderBoundsWrong		= -15278;
	errAEValueOutOfRange		= -15279;
	errFinderPropertyDoesNotApply = -15280;
	errFinderFileSharingMustBeOn = -15281;
	errFinderMustBeActive		= -15282;
	errFinderVolumeNotFound		= -15283;						{  more descriptive than what we get with nsvErr }
	errFinderLockedItemsInTrash	= -15284;						{  there are some locked items in the trash }
	errFinderOnlyLockedItemsInTrash = -15285;					{  all the items (except folders) in the trash are locked }
	errFinderProgramLinkingMustBeOn = -15286;
	errFinderWindowMustBeButtonView = -15287;
	errFinderBadPackageContents	= -15288;						{  something is wrong within the package    }
	errFinderUnsupportedInsidePackages = -15289;				{  operation cannot be used on items within a package      }
	errFinderCorruptOpenFolderList = -15290;					{  was -15276 in Finder 8.6 and earlier, but that conflicted with General Controls }
	errFinderNoInvisibleFiles	= -15291;						{  was -15277 in Finder 8.6 and earlier, but that conflicted with General Controls }
	errFinderCantDeleteImmediately = -15292;					{  cannot delete immediately via scripting }
	errFinderLastReserved		= -15379;

{$ALIGN MAC68K}


end.
