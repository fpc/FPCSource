{
     File:       AERegistry.p
 
     Contains:   AppleEvents Registry Interface.
 
     Version:    Technology: Mac OS 8.5
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1993-2002 by Apple Computer, Inc., all rights reserved
 
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

unit AERegistry;
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
uses MacTypes,ATSTypes,MacErrors,AppleEvents;


{$ALIGN MAC68K}


const
	cAEList						= $6C697374 (* 'list' *);						{   0x6c697374   }
	cApplication				= $63617070 (* 'capp' *);						{   0x63617070   }
	cArc						= $63617263 (* 'carc' *);						{   0x63617263   }
	cBoolean					= $626F6F6C (* 'bool' *);						{   0x626f6f6c   }
	cCell						= $6363656C (* 'ccel' *);						{   0x6363656c   }
	cChar						= $63686120 (* 'cha ' *);						{   0x63686120   }
	cColorTable					= $636C7274 (* 'clrt' *);						{   0x636c7274   }
	cColumn						= $63636F6C (* 'ccol' *);						{   0x63636f6c   }
	cDocument					= $646F6375 (* 'docu' *);						{   0x646f6375   }
	cDrawingArea				= $63647277 (* 'cdrw' *);						{   0x63647277   }
	cEnumeration				= $656E756D (* 'enum' *);						{   0x656e756d   }
	cFile						= $66696C65 (* 'file' *);						{   0x66696c65   }
	cFixed						= $66697864 (* 'fixd' *);						{   0x66697864   }
	cFixedPoint					= $66706E74 (* 'fpnt' *);						{   0x66706e74   }
	cFixedRectangle				= $66726374 (* 'frct' *);						{   0x66726374   }
	cGraphicLine				= $676C696E (* 'glin' *);						{   0x676c696e   }
	cGraphicObject				= $63676F62 (* 'cgob' *);						{   0x63676f62   }
	cGraphicShape				= $63677368 (* 'cgsh' *);						{   0x63677368   }
	cGraphicText				= $63677478 (* 'cgtx' *);						{   0x63677478   }
	cGroupedGraphic				= $63706963 (* 'cpic' *);						{   0x63706963   }

	cInsertionLoc				= $696E736C (* 'insl' *);						{   0x696e736c   }
	cInsertionPoint				= $63696E73 (* 'cins' *);						{   0x63696e73   }
	cIntlText					= $69747874 (* 'itxt' *);						{   0x69747874   }
	cIntlWritingCode			= $696E746C (* 'intl' *);						{   0x696e746c   }
	cItem						= $6369746D (* 'citm' *);						{   0x6369746d   }
	cLine						= $636C696E (* 'clin' *);						{   0x636c696e   }
	cLongDateTime				= $6C647420 (* 'ldt ' *);						{   0x6c647420   }
	cLongFixed					= $6C667864 (* 'lfxd' *);						{   0x6c667864   }
	cLongFixedPoint				= $6C667074 (* 'lfpt' *);						{   0x6c667074   }
	cLongFixedRectangle			= $6C667263 (* 'lfrc' *);						{   0x6c667263   }
	cLongInteger				= $6C6F6E67 (* 'long' *);						{   0x6c6f6e67   }
	cLongPoint					= $6C706E74 (* 'lpnt' *);						{   0x6c706e74   }
	cLongRectangle				= $6C726374 (* 'lrct' *);						{   0x6c726374   }
	cMachineLoc					= $6D4C6F63 (* 'mLoc' *);						{   0x6d4c6f63   }
	cMenu						= $636D6E75 (* 'cmnu' *);						{   0x636d6e75   }
	cMenuItem					= $636D656E (* 'cmen' *);						{   0x636d656e   }
	cObject						= $636F626A (* 'cobj' *);						{   0x636f626a   }
	cObjectSpecifier			= $6F626A20 (* 'obj ' *);						{   0x6f626a20   }
	cOpenableObject				= $636F6F62 (* 'coob' *);						{   0x636f6f62   }
	cOval						= $636F766C (* 'covl' *);						{   0x636f766c   }

	cParagraph					= $63706172 (* 'cpar' *);						{   0x63706172   }
	cPICT						= $50494354 (* 'PICT' *);						{   0x50494354   }
	cPixel						= $6370786C (* 'cpxl' *);						{   0x6370786c   }
	cPixelMap					= $63706978 (* 'cpix' *);						{   0x63706978   }
	cPolygon					= $6370676E (* 'cpgn' *);						{   0x6370676e   }
	cProperty					= $70726F70 (* 'prop' *);						{   0x70726f70   }
	cQDPoint					= $51447074 (* 'QDpt' *);						{   0x51447074   }
	cQDRectangle				= $71647274 (* 'qdrt' *);						{   0x71647274   }
	cRectangle					= $63726563 (* 'crec' *);						{   0x63726563   }
	cRGBColor					= $63524742 (* 'cRGB' *);						{   0x63524742   }
	cRotation					= $74726F74 (* 'trot' *);						{   0x74726f74   }
	cRoundedRectangle			= $63727263 (* 'crrc' *);						{   0x63727263   }
	cRow						= $63726F77 (* 'crow' *);						{   0x63726f77   }
	cSelection					= $6373656C (* 'csel' *);						{   0x6373656c   }
	cShortInteger				= $73686F72 (* 'shor' *);						{   0x73686f72   }
	cTable						= $6374626C (* 'ctbl' *);						{   0x6374626c   }
	cText						= $63747874 (* 'ctxt' *);						{   0x63747874   }
	cTextFlow					= $63666C6F (* 'cflo' *);						{   0x63666c6f   }
	cTextStyles					= $74737479 (* 'tsty' *);						{   0x74737479   }
	cType						= $74797065 (* 'type' *);						{   0x74797065   }

	cVersion					= $76657273 (* 'vers' *);						{   0x76657273   }
	cWindow						= $6377696E (* 'cwin' *);						{   0x6377696e   }
	cWord						= $63776F72 (* 'cwor' *);						{   0x63776f72   }
	enumArrows					= $6172726F (* 'arro' *);						{   0x6172726f   }
	enumJustification			= $6A757374 (* 'just' *);						{   0x6a757374   }
	enumKeyForm					= $6B66726D (* 'kfrm' *);						{   0x6b66726d   }
	enumPosition				= $706F7369 (* 'posi' *);						{   0x706f7369   }
	enumProtection				= $7072746E (* 'prtn' *);						{   0x7072746e   }
	enumQuality					= $7175616C (* 'qual' *);						{   0x7175616c   }
	enumSaveOptions				= $7361766F (* 'savo' *);						{   0x7361766f   }
	enumStyle					= $7374796C (* 'styl' *);						{   0x7374796c   }
	enumTransferMode			= $7472616E (* 'tran' *);						{   0x7472616e   }
	formUniqueID				= $49442020 (* 'ID  ' *);						{   0x49442020   }
	kAEAbout					= $61626F75 (* 'abou' *);						{   0x61626f75   }
	kAEAfter					= $61667465 (* 'afte' *);						{   0x61667465   }
	kAEAliasSelection			= $73616C69 (* 'sali' *);						{   0x73616c69   }
	kAEAllCaps					= $616C6370 (* 'alcp' *);						{   0x616c6370   }
	kAEArrowAtEnd				= $6172656E (* 'aren' *);						{   0x6172656e   }
	kAEArrowAtStart				= $61727374 (* 'arst' *);						{   0x61727374   }
	kAEArrowBothEnds			= $6172626F (* 'arbo' *);						{   0x6172626f   }

	kAEAsk						= $61736B20 (* 'ask ' *);						{   0x61736b20   }
	kAEBefore					= $6265666F (* 'befo' *);						{   0x6265666f   }
	kAEBeginning				= $62676E67 (* 'bgng' *);						{   0x62676e67   }
	kAEBeginsWith				= $62677774 (* 'bgwt' *);						{   0x62677774   }
	kAEBeginTransaction			= $62656769 (* 'begi' *);						{   0x62656769   }
	kAEBold						= $626F6C64 (* 'bold' *);						{   0x626f6c64   }
	kAECaseSensEquals			= $63736571 (* 'cseq' *);						{   0x63736571   }
	kAECentered					= $63656E74 (* 'cent' *);						{   0x63656e74   }
	kAEChangeView				= $76696577 (* 'view' *);						{   0x76696577   }
	kAEClone					= $636C6F6E (* 'clon' *);						{   0x636c6f6e   }
	kAEClose					= $636C6F73 (* 'clos' *);						{   0x636c6f73   }
	kAECondensed				= $636F6E64 (* 'cond' *);						{   0x636f6e64   }
	kAEContains					= $636F6E74 (* 'cont' *);						{   0x636f6e74   }
	kAECopy						= $636F7079 (* 'copy' *);						{   0x636f7079   }
	kAECoreSuite				= $636F7265 (* 'core' *);						{   0x636f7265   }
	kAECountElements			= $636E7465 (* 'cnte' *);						{   0x636e7465   }
	kAECreateElement			= $6372656C (* 'crel' *);						{   0x6372656c   }
	kAECreatePublisher			= $63707562 (* 'cpub' *);						{   0x63707562   }
	kAECut						= $63757420 (* 'cut ' *);						{   0x63757420   }
	kAEDelete					= $64656C6F (* 'delo' *);						{   0x64656c6f   }

	kAEDoObjectsExist			= $646F6578 (* 'doex' *);						{   0x646f6578   }
	kAEDoScript					= $646F7363 (* 'dosc' *);						{   0x646f7363   }
	kAEDrag						= $64726167 (* 'drag' *);						{   0x64726167   }
	kAEDuplicateSelection		= $73647570 (* 'sdup' *);						{   0x73647570   }
	kAEEditGraphic				= $65646974 (* 'edit' *);						{   0x65646974   }
	kAEEmptyTrash				= $656D7074 (* 'empt' *);						{   0x656d7074   }
	kAEEnd						= $656E6420 (* 'end ' *);						{   0x656e6420   }
	kAEEndsWith					= $656E6473 (* 'ends' *);						{   0x656e6473   }
	kAEEndTransaction			= $656E6474 (* 'endt' *);						{   0x656e6474   }
	kAEEquals					= $3D202020 (* '=   ' *);						{   0x3d202020   }
	kAEExpanded					= $70657870 (* 'pexp' *);						{   0x70657870   }
	kAEFast						= $66617374 (* 'fast' *);						{   0x66617374   }
	kAEFinderEvents				= $464E4452 (* 'FNDR' *);						{   0x464e4452   }
	kAEFormulaProtect			= $6670726F (* 'fpro' *);						{   0x6670726f   }
	kAEFullyJustified			= $66756C6C (* 'full' *);						{   0x66756c6c   }
	kAEGetClassInfo				= $716F626A (* 'qobj' *);						{   0x716f626a   }
	kAEGetData					= $67657464 (* 'getd' *);						{   0x67657464   }
	kAEGetDataSize				= $6473697A (* 'dsiz' *);						{   0x6473697a   }
	kAEGetEventInfo				= $67746569 (* 'gtei' *);						{   0x67746569   }
	kAEGetInfoSelection			= $73696E66 (* 'sinf' *);						{   0x73696e66   }

	kAEGetPrivilegeSelection	= $73707276 (* 'sprv' *);						{   0x73707276   }
	kAEGetSuiteInfo				= $67747369 (* 'gtsi' *);						{   0x67747369   }
	kAEGreaterThan				= $3E202020 (* '>   ' *);						{   0x3e202020   }
	kAEGreaterThanEquals		= $3E3D2020 (* '>=  ' *);						{   0x3e3d2020   }
	kAEGrow						= $67726F77 (* 'grow' *);						{   0x67726f77   }
	kAEHidden					= $6869646E (* 'hidn' *);						{   0x6869646e   }
	kAEHiQuality				= $68697175 (* 'hiqu' *);						{   0x68697175   }
	kAEImageGraphic				= $696D6772 (* 'imgr' *);						{   0x696d6772   }
	kAEIsUniform				= $6973756E (* 'isun' *);						{   0x6973756e   }
	kAEItalic					= $6974616C (* 'ital' *);						{   0x6974616c   }
	kAELeftJustified			= $6C656674 (* 'left' *);						{   0x6c656674   }
	kAELessThan					= $3C202020 (* '<   ' *);						{   0x3c202020   }
	kAELessThanEquals			= $3C3D2020 (* '<=  ' *);						{   0x3c3d2020   }
	kAELowercase				= $6C6F7763 (* 'lowc' *);						{   0x6c6f7763   }
	kAEMakeObjectsVisible		= $6D766973 (* 'mvis' *);						{   0x6d766973   }
	kAEMiscStandards			= $6D697363 (* 'misc' *);						{   0x6d697363   }
	kAEModifiable				= $6D6F6466 (* 'modf' *);						{   0x6d6f6466   }
	kAEMove						= $6D6F7665 (* 'move' *);						{   0x6d6f7665   }
	kAENo						= $6E6F2020 (* 'no  ' *);						{   0x6e6f2020   }
	kAENoArrow					= $61726E6F (* 'arno' *);						{   0x61726e6f   }

	kAENonmodifiable			= $6E6D6F64 (* 'nmod' *);						{   0x6e6d6f64   }
	kAEOpen						= $6F646F63 (* 'odoc' *);						{   0x6f646f63   }
	kAEOpenSelection			= $736F7065 (* 'sope' *);						{   0x736f7065   }
	kAEOutline					= $6F75746C (* 'outl' *);						{   0x6f75746c   }
	kAEPageSetup				= $70677375 (* 'pgsu' *);						{   0x70677375   }
	kAEPaste					= $70617374 (* 'past' *);						{   0x70617374   }
	kAEPlain					= $706C616E (* 'plan' *);						{   0x706c616e   }
	kAEPrint					= $70646F63 (* 'pdoc' *);						{   0x70646f63   }
	kAEPrintSelection			= $73707269 (* 'spri' *);						{   0x73707269   }
	kAEPrintWindow				= $7077696E (* 'pwin' *);						{   0x7077696e   }
	kAEPutAwaySelection			= $73707574 (* 'sput' *);						{   0x73707574   }
	kAEQDAddOver				= $6164646F (* 'addo' *);						{   0x6164646f   }
	kAEQDAddPin					= $61646470 (* 'addp' *);						{   0x61646470   }
	kAEQDAdMax					= $61646D78 (* 'admx' *);						{   0x61646d78   }
	kAEQDAdMin					= $61646D6E (* 'admn' *);						{   0x61646d6e   }
	kAEQDBic					= $62696320 (* 'bic ' *);						{   0x62696320   }
	kAEQDBlend					= $626C6E64 (* 'blnd' *);						{   0x626c6e64   }
	kAEQDCopy					= $63707920 (* 'cpy ' *);						{   0x63707920   }
	kAEQDNotBic					= $6E626963 (* 'nbic' *);						{   0x6e626963   }
	kAEQDNotCopy				= $6E637079 (* 'ncpy' *);						{   0x6e637079   }

	kAEQDNotOr					= $6E746F72 (* 'ntor' *);						{   0x6e746f72   }
	kAEQDNotXor					= $6E786F72 (* 'nxor' *);						{   0x6e786f72   }
	kAEQDOr						= $6F722020 (* 'or  ' *);						{   0x6f722020   }
	kAEQDSubOver				= $7375626F (* 'subo' *);						{   0x7375626f   }
	kAEQDSubPin					= $73756270 (* 'subp' *);						{   0x73756270   }
	kAEQDSupplementalSuite		= $71647370 (* 'qdsp' *);						{   0x71647370   }
	kAEQDXor					= $786F7220 (* 'xor ' *);						{   0x786f7220   }
	kAEQuickdrawSuite			= $71647277 (* 'qdrw' *);						{   0x71647277   }
	kAEQuitAll					= $71756961 (* 'quia' *);						{   0x71756961   }
	kAERedo						= $7265646F (* 'redo' *);						{   0x7265646f   }
	kAERegular					= $7265676C (* 'regl' *);						{   0x7265676c   }
	kAEReopenApplication		= $72617070 (* 'rapp' *);						{   0x72617070   }
	kAEReplace					= $72706C63 (* 'rplc' *);						{   0x72706c63   }
	kAERequiredSuite			= $72657164 (* 'reqd' *);						{   0x72657164   }
	kAERestart					= $72657374 (* 'rest' *);						{   0x72657374   }
	kAERevealSelection			= $73726576 (* 'srev' *);						{   0x73726576   }
	kAERevert					= $72767274 (* 'rvrt' *);						{   0x72767274   }
	kAERightJustified			= $72676874 (* 'rght' *);						{   0x72676874   }
	kAESave						= $73617665 (* 'save' *);						{   0x73617665   }
	kAESelect					= $736C6374 (* 'slct' *);						{   0x736c6374   }
	kAESetData					= $73657464 (* 'setd' *);						{   0x73657464   }

	kAESetPosition				= $706F736E (* 'posn' *);						{   0x706f736e   }
	kAEShadow					= $73686164 (* 'shad' *);						{   0x73686164   }
	kAEShowClipboard			= $7368636C (* 'shcl' *);						{   0x7368636c   }
	kAEShutDown					= $73687574 (* 'shut' *);						{   0x73687574   }
	kAESleep					= $736C6570 (* 'slep' *);						{   0x736c6570   }
	kAESmallCaps				= $736D6370 (* 'smcp' *);						{   0x736d6370   }
	kAESpecialClassProperties	= $63402321 (* 'c@#!' *);						{   0x63402321   }
	kAEStrikethrough			= $7374726B (* 'strk' *);						{   0x7374726b   }
	kAESubscript				= $73627363 (* 'sbsc' *);						{   0x73627363   }
	kAESuperscript				= $73707363 (* 'spsc' *);						{   0x73707363   }
	kAETableSuite				= $74626C73 (* 'tbls' *);						{   0x74626c73   }
	kAETextSuite				= $54455854 (* 'TEXT' *);						{   0x54455854   }
	kAETransactionTerminated	= $7474726D (* 'ttrm' *);						{   0x7474726d   }
	kAEUnderline				= $756E646C (* 'undl' *);						{   0x756e646c   }
	kAEUndo						= $756E646F (* 'undo' *);						{   0x756e646f   }
	kAEWholeWordEquals			= $77776571 (* 'wweq' *);						{   0x77776571   }
	kAEYes						= $79657320 (* 'yes ' *);						{   0x79657320   }
	kAEZoom						= $7A6F6F6D (* 'zoom' *);						{   0x7a6f6f6d   }

    { events that can be sent to the "system" process (eg, loginwindow) on OS X 10.2 or later }
	kAELogOut                   = $6C6F676F (* 'logo' *);
	kAEReallyLogOut             = $726C676F (* 'rlgo' *);
	kAEShowRestartDialog        = $72727374 (* 'rrst' *);
	kAEShowShutdownDialog       = $7273646E (* 'rsdn' *);

	{	 EventRecord Classes and EventIDs 	}
	kAEMouseClass				= $6D6F7573 (* 'mous' *);
	kAEDown						= $646F776E (* 'down' *);
	kAEUp						= $75702020 (* 'up  ' *);
	kAEMoved					= $6D6F7665 (* 'move' *);
	kAEStoppedMoving			= $73746F70 (* 'stop' *);
	kAEWindowClass				= $77696E64 (* 'wind' *);
	kAEUpdate					= $75706474 (* 'updt' *);
	kAEActivate					= $61637476 (* 'actv' *);
	kAEDeactivate				= $64616374 (* 'dact' *);
	kAECommandClass				= $636D6E64 (* 'cmnd' *);						{  Modern Command Event Class  }
	kAEKeyClass					= $6B657963 (* 'keyc' *);
	kAERawKey					= $726B6579 (* 'rkey' *);						{  Modern Raw Key Event  }
	kAEVirtualKey				= $6B657963 (* 'keyc' *);						{  Modern Virtual Key Event  }
	kAENavigationKey			= $6E617665 (* 'nave' *);						{  Modern Navigation Key Event  }
	kAEAutoDown					= $6175746F (* 'auto' *);
	kAEApplicationClass			= $6170706C (* 'appl' *);
	kAESuspend					= $73757370 (* 'susp' *);
	kAEResume					= $72736D65 (* 'rsme' *);
	kAEDiskEvent				= $6469736B (* 'disk' *);
	kAENullEvent				= $6E756C6C (* 'null' *);
	kAEWakeUpEvent				= $77616B65 (* 'wake' *);
	kAEScrapEvent				= $73637270 (* 'scrp' *);
	kAEHighLevel				= $68696768 (* 'high' *);

	keyAEAngle					= $6B616E67 (* 'kang' *);						{   0x6b616e67   }
	keyAEArcAngle				= $70617263 (* 'parc' *);						{   0x70617263   }

	keyAEBaseAddr				= $62616464 (* 'badd' *);						{   0x62616464   }
	keyAEBestType				= $70627374 (* 'pbst' *);						{   0x70627374   }
	keyAEBgndColor				= $6B62636C (* 'kbcl' *);						{   0x6b62636c   }
	keyAEBgndPattern			= $6B627074 (* 'kbpt' *);						{   0x6b627074   }
	keyAEBounds					= $70626E64 (* 'pbnd' *);						{   0x70626e64   }
	keyAECellList				= $6B636C74 (* 'kclt' *);						{   0x6b636c74   }
	keyAEClassID				= $636C4944 (* 'clID' *);						{   0x636c4944   }
	keyAEColor					= $636F6C72 (* 'colr' *);						{   0x636f6c72   }
	keyAEColorTable				= $636C7462 (* 'cltb' *);						{   0x636c7462   }
	keyAECurveHeight			= $6B636864 (* 'kchd' *);						{   0x6b636864   }
	keyAECurveWidth				= $6B637764 (* 'kcwd' *);						{   0x6b637764   }
	keyAEDashStyle				= $70647374 (* 'pdst' *);						{   0x70647374   }
	keyAEData					= $64617461 (* 'data' *);						{   0x64617461   }
	keyAEDefaultType			= $64656674 (* 'deft' *);						{   0x64656674   }
	keyAEDefinitionRect			= $70647274 (* 'pdrt' *);						{   0x70647274   }
	keyAEDescType				= $64737470 (* 'dstp' *);						{   0x64737470   }
	keyAEDestination			= $64657374 (* 'dest' *);						{   0x64657374   }
	keyAEDoAntiAlias			= $616E7461 (* 'anta' *);						{   0x616e7461   }
	keyAEDoDithered				= $67646974 (* 'gdit' *);						{   0x67646974   }
	keyAEDoRotate				= $6B647274 (* 'kdrt' *);						{   0x6b647274   }

	keyAEDoScale				= $6B736361 (* 'ksca' *);						{   0x6b736361   }
	keyAEDoTranslate			= $6B747261 (* 'ktra' *);						{   0x6b747261   }
	keyAEEditionFileLoc			= $656C6F63 (* 'eloc' *);						{   0x656c6f63   }
	keyAEElements				= $656C6D73 (* 'elms' *);						{   0x656c6d73   }
	keyAEEndPoint				= $70656E64 (* 'pend' *);						{   0x70656e64   }
	keyAEEventClass				= $6576636C (* 'evcl' *);						{   0x6576636c   }
	keyAEEventID				= $65767469 (* 'evti' *);						{   0x65767469   }
	keyAEFile					= $6B66696C (* 'kfil' *);						{   0x6b66696c   }
	keyAEFileType				= $666C7470 (* 'fltp' *);						{   0x666c7470   }
	keyAEFillColor				= $666C636C (* 'flcl' *);						{   0x666c636c   }
	keyAEFillPattern			= $666C7074 (* 'flpt' *);						{   0x666c7074   }
	keyAEFlipHorizontal			= $6B66686F (* 'kfho' *);						{   0x6b66686f   }
	keyAEFlipVertical			= $6B667674 (* 'kfvt' *);						{   0x6b667674   }
	keyAEFont					= $666F6E74 (* 'font' *);						{   0x666f6e74   }
	keyAEFormula				= $70666F72 (* 'pfor' *);						{   0x70666f72   }
	keyAEGraphicObjects			= $676F6273 (* 'gobs' *);						{   0x676f6273   }
	keyAEID						= $49442020 (* 'ID  ' *);						{   0x49442020   }
	keyAEImageQuality			= $67717561 (* 'gqua' *);						{   0x67717561   }
	keyAEInsertHere				= $696E7368 (* 'insh' *);						{   0x696e7368   }
	keyAEKeyForms				= $6B657966 (* 'keyf' *);						{   0x6b657966   }

	keyAEKeyword				= $6B797764 (* 'kywd' *);						{   0x6b797764   }
	keyAELevel					= $6C65766C (* 'levl' *);						{   0x6c65766c   }
	keyAELineArrow				= $6172726F (* 'arro' *);						{   0x6172726f   }
	keyAEName					= $706E616D (* 'pnam' *);						{   0x706e616d   }
	keyAENewElementLoc			= $706E656C (* 'pnel' *);						{   0x706e656c   }
	keyAEObject					= $6B6F626A (* 'kobj' *);						{   0x6b6f626a   }
	keyAEObjectClass			= $6B6F636C (* 'kocl' *);						{   0x6b6f636c   }
	keyAEOffStyles				= $6F667374 (* 'ofst' *);						{   0x6f667374   }
	keyAEOnStyles				= $6F6E7374 (* 'onst' *);						{   0x6f6e7374   }
	keyAEParameters				= $70726D73 (* 'prms' *);						{   0x70726d73   }
	keyAEParamFlags				= $706D6667 (* 'pmfg' *);						{   0x706d6667   }
	keyAEPenColor				= $7070636C (* 'ppcl' *);						{   0x7070636c   }
	keyAEPenPattern				= $70707061 (* 'pppa' *);						{   0x70707061   }
	keyAEPenWidth				= $70707764 (* 'ppwd' *);						{   0x70707764   }
	keyAEPixelDepth				= $70647074 (* 'pdpt' *);						{   0x70647074   }
	keyAEPixMapMinus			= $6B706D6D (* 'kpmm' *);						{   0x6b706d6d   }
	keyAEPMTable				= $6B706D74 (* 'kpmt' *);						{   0x6b706d74   }
	keyAEPointList				= $70746C74 (* 'ptlt' *);						{   0x70746c74   }
	keyAEPointSize				= $7074737A (* 'ptsz' *);						{   0x7074737a   }
	keyAEPosition				= $6B706F73 (* 'kpos' *);						{   0x6b706f73   }

	keyAEPropData				= $70726474 (* 'prdt' *);						{   0x70726474   }
	keyAEProperties				= $7170726F (* 'qpro' *);						{   0x7170726f   }
	keyAEProperty				= $6B707270 (* 'kprp' *);						{   0x6b707270   }
	keyAEPropFlags				= $70726667 (* 'prfg' *);						{   0x70726667   }
	keyAEPropID					= $70726F70 (* 'prop' *);						{   0x70726f70   }
	keyAEProtection				= $7070726F (* 'ppro' *);						{   0x7070726f   }
	keyAERenderAs				= $6B72656E (* 'kren' *);						{   0x6b72656e   }
	keyAERequestedType			= $72747970 (* 'rtyp' *);						{   0x72747970   }
	keyAEResult					= $2D2D2D2D (* '----' *);						{   0x2d2d2d2d   }
	keyAEResultInfo				= $7273696E (* 'rsin' *);						{   0x7273696e   }
	keyAERotation				= $70726F74 (* 'prot' *);						{   0x70726f74   }
	keyAERotPoint				= $6B727470 (* 'krtp' *);						{   0x6b727470   }
	keyAERowList				= $6B726C73 (* 'krls' *);						{   0x6b726c73   }
	keyAESaveOptions			= $7361766F (* 'savo' *);						{   0x7361766f   }
	keyAEScale					= $7073636C (* 'pscl' *);						{   0x7073636c   }
	keyAEScriptTag				= $70736374 (* 'psct' *);						{   0x70736374   }
	keyAEShowWhere				= $73686F77 (* 'show' *);						{   0x73686f77   }
	keyAEStartAngle				= $70616E67 (* 'pang' *);						{   0x70616e67   }
	keyAEStartPoint				= $70737470 (* 'pstp' *);						{   0x70737470   }
	keyAEStyles					= $6B737479 (* 'ksty' *);						{   0x6b737479   }

	keyAESuiteID				= $73756974 (* 'suit' *);						{   0x73756974   }
	keyAEText					= $6B747874 (* 'ktxt' *);						{   0x6b747874   }
	keyAETextColor				= $70747863 (* 'ptxc' *);						{   0x70747863   }
	keyAETextFont				= $70747866 (* 'ptxf' *);						{   0x70747866   }
	keyAETextPointSize			= $70747073 (* 'ptps' *);						{   0x70747073   }
	keyAETextStyles				= $74787374 (* 'txst' *);						{   0x74787374   }
	keyAETextLineHeight			= $6B746C68 (* 'ktlh' *);						{  type ShortInteger  }
	keyAETextLineAscent			= $6B746173 (* 'ktas' *);						{  type ShortInteger  }
	keyAETheText				= $74687478 (* 'thtx' *);						{   0x74687478   }
	keyAETransferMode			= $7070746D (* 'pptm' *);						{   0x7070746d   }
	keyAETranslation			= $70747273 (* 'ptrs' *);						{   0x70747273   }
	keyAETryAsStructGraf		= $746F6F67 (* 'toog' *);						{   0x746f6f67   }
	keyAEUniformStyles			= $7573746C (* 'ustl' *);						{   0x7573746c   }
	keyAEUpdateOn				= $70757064 (* 'pupd' *);						{   0x70757064   }
	keyAEUserTerm				= $7574726D (* 'utrm' *);						{   0x7574726d   }
	keyAEWindow					= $776E6477 (* 'wndw' *);						{   0x776e6477   }
	keyAEWritingCode			= $77726364 (* 'wrcd' *);						{   0x77726364   }

	keyMiscellaneous			= $666D7363 (* 'fmsc' *);						{   0x666d7363   }
	keySelection				= $6673656C (* 'fsel' *);						{   0x6673656c   }
	keyWindow					= $6B776E64 (* 'kwnd' *);						{   0x6b776e64   }
																{  EventRecord keys  }
	keyWhen						= $7768656E (* 'when' *);
	keyWhere					= $77686572 (* 'wher' *);
	keyModifiers				= $6D6F6473 (* 'mods' *);
	keyKey						= $6B657920 (* 'key ' *);
	keyKeyCode					= $636F6465 (* 'code' *);
	keyKeyboard					= $6B657962 (* 'keyb' *);
	keyDriveNumber				= $64727623 (* 'drv#' *);
	keyErrorCode				= $65727223 (* 'err#' *);
	keyHighLevelClass			= $68636C73 (* 'hcls' *);
	keyHighLevelID				= $68696420 (* 'hid ' *);

	pArcAngle					= $70617263 (* 'parc' *);						{   0x70617263   }
	pBackgroundColor			= $7062636C (* 'pbcl' *);						{   0x7062636c   }
	pBackgroundPattern			= $70627074 (* 'pbpt' *);						{   0x70627074   }
	pBestType					= $70627374 (* 'pbst' *);						{   0x70627374   }
	pBounds						= $70626E64 (* 'pbnd' *);						{   0x70626e64   }
	pClass						= $70636C73 (* 'pcls' *);						{   0x70636c73   }
	pClipboard					= $70636C69 (* 'pcli' *);						{   0x70636c69   }
	pColor						= $636F6C72 (* 'colr' *);						{   0x636f6c72   }
	pColorTable					= $636C7462 (* 'cltb' *);						{   0x636c7462   }
	pContents					= $70636E74 (* 'pcnt' *);						{   0x70636e74   }
	pCornerCurveHeight			= $70636864 (* 'pchd' *);						{   0x70636864   }
	pCornerCurveWidth			= $70637764 (* 'pcwd' *);						{   0x70637764   }
	pDashStyle					= $70647374 (* 'pdst' *);						{   0x70647374   }
	pDefaultType				= $64656674 (* 'deft' *);						{   0x64656674   }
	pDefinitionRect				= $70647274 (* 'pdrt' *);						{   0x70647274   }
	pEnabled					= $656E626C (* 'enbl' *);						{   0x656e626c   }
	pEndPoint					= $70656E64 (* 'pend' *);						{   0x70656e64   }
	pFillColor					= $666C636C (* 'flcl' *);						{   0x666c636c   }
	pFillPattern				= $666C7074 (* 'flpt' *);						{   0x666c7074   }
	pFont						= $666F6E74 (* 'font' *);						{   0x666f6e74   }

	pFormula					= $70666F72 (* 'pfor' *);						{   0x70666f72   }
	pGraphicObjects				= $676F6273 (* 'gobs' *);						{   0x676f6273   }
	pHasCloseBox				= $68636C62 (* 'hclb' *);						{   0x68636c62   }
	pHasTitleBar				= $70746974 (* 'ptit' *);						{   0x70746974   }
	pID							= $49442020 (* 'ID  ' *);						{   0x49442020   }
	pIndex						= $70696478 (* 'pidx' *);						{   0x70696478   }
	pInsertionLoc				= $70696E73 (* 'pins' *);						{   0x70696e73   }
	pIsFloating					= $6973666C (* 'isfl' *);						{   0x6973666c   }
	pIsFrontProcess				= $70697366 (* 'pisf' *);						{   0x70697366   }
	pIsModal					= $706D6F64 (* 'pmod' *);						{   0x706d6f64   }
	pIsModified					= $696D6F64 (* 'imod' *);						{   0x696d6f64   }
	pIsResizable				= $7072737A (* 'prsz' *);						{   0x7072737a   }
	pIsStationeryPad			= $70737064 (* 'pspd' *);						{   0x70737064   }
	pIsZoomable					= $69737A6D (* 'iszm' *);						{   0x69737a6d   }
	pIsZoomed					= $707A756D (* 'pzum' *);						{   0x707a756d   }
	pItemNumber					= $69746D6E (* 'itmn' *);						{   0x69746d6e   }
	pJustification				= $706A7374 (* 'pjst' *);						{   0x706a7374   }
	pLineArrow					= $6172726F (* 'arro' *);						{   0x6172726f   }
	pMenuID						= $6D6E6964 (* 'mnid' *);						{   0x6d6e6964   }
	pName						= $706E616D (* 'pnam' *);						{   0x706e616d   }

	pNewElementLoc				= $706E656C (* 'pnel' *);						{   0x706e656c   }
	pPenColor					= $7070636C (* 'ppcl' *);						{   0x7070636c   }
	pPenPattern					= $70707061 (* 'pppa' *);						{   0x70707061   }
	pPenWidth					= $70707764 (* 'ppwd' *);						{   0x70707764   }
	pPixelDepth					= $70647074 (* 'pdpt' *);						{   0x70647074   }
	pPointList					= $70746C74 (* 'ptlt' *);						{   0x70746c74   }
	pPointSize					= $7074737A (* 'ptsz' *);						{   0x7074737a   }
	pProtection					= $7070726F (* 'ppro' *);						{   0x7070726f   }
	pRotation					= $70726F74 (* 'prot' *);						{   0x70726f74   }
	pScale						= $7073636C (* 'pscl' *);						{   0x7073636c   }
	pScript						= $73637074 (* 'scpt' *);						{   0x73637074   }
	pScriptTag					= $70736374 (* 'psct' *);						{   0x70736374   }
	pSelected					= $73656C63 (* 'selc' *);						{   0x73656c63   }
	pSelection					= $73656C65 (* 'sele' *);						{   0x73656c65   }
	pStartAngle					= $70616E67 (* 'pang' *);						{   0x70616e67   }
	pStartPoint					= $70737470 (* 'pstp' *);						{   0x70737470   }
	pTextColor					= $70747863 (* 'ptxc' *);						{   0x70747863   }
	pTextFont					= $70747866 (* 'ptxf' *);						{   0x70747866   }
	pTextItemDelimiters			= $7478646C (* 'txdl' *);						{   0x7478646c   }
	pTextPointSize				= $70747073 (* 'ptps' *);						{   0x70747073   }

	pTextStyles					= $74787374 (* 'txst' *);						{   0x74787374   }
	pTransferMode				= $7070746D (* 'pptm' *);						{   0x7070746d   }
	pTranslation				= $70747273 (* 'ptrs' *);						{   0x70747273   }
	pUniformStyles				= $7573746C (* 'ustl' *);						{   0x7573746c   }
	pUpdateOn					= $70757064 (* 'pupd' *);						{   0x70757064   }
	pUserSelection				= $7075736C (* 'pusl' *);						{   0x7075736c   }
	pVersion					= $76657273 (* 'vers' *);						{   0x76657273   }
	pVisible					= $70766973 (* 'pvis' *);						{   0x70766973   }

	typeAEText					= $74545854 (* 'tTXT' *);						{   0x74545854   }
	typeArc						= $63617263 (* 'carc' *);						{   0x63617263   }
	typeBest					= $62657374 (* 'best' *);						{   0x62657374   }
	typeCell					= $6363656C (* 'ccel' *);						{   0x6363656c   }
	typeClassInfo				= $67636C69 (* 'gcli' *);						{   0x67636c69   }
	typeColorTable				= $636C7274 (* 'clrt' *);						{   0x636c7274   }
	typeColumn					= $63636F6C (* 'ccol' *);						{   0x63636f6c   }
	typeDashStyle				= $74646173 (* 'tdas' *);						{   0x74646173   }
	typeData					= $74647461 (* 'tdta' *);						{   0x74647461   }
	typeDrawingArea				= $63647277 (* 'cdrw' *);						{   0x63647277   }
	typeElemInfo				= $656C696E (* 'elin' *);						{   0x656c696e   }
	typeEnumeration				= $656E756D (* 'enum' *);						{   0x656e756d   }
	typeEPS						= $45505320 (* 'EPS ' *);						{   0x45505320   }
	typeEventInfo				= $6576696E (* 'evin' *);						{   0x6576696e   }

	typeFinderWindow			= $6677696E (* 'fwin' *);						{   0x6677696e   }
	typeFixedPoint				= $66706E74 (* 'fpnt' *);						{   0x66706e74   }
	typeFixedRectangle			= $66726374 (* 'frct' *);						{   0x66726374   }
	typeGraphicLine				= $676C696E (* 'glin' *);						{   0x676c696e   }
	typeGraphicText				= $63677478 (* 'cgtx' *);						{   0x63677478   }
	typeGroupedGraphic			= $63706963 (* 'cpic' *);						{   0x63706963   }
	typeInsertionLoc			= $696E736C (* 'insl' *);						{   0x696e736c   }
	typeIntlText				= $69747874 (* 'itxt' *);						{   0x69747874   }
	typeIntlWritingCode			= $696E746C (* 'intl' *);						{   0x696e746c   }
	typeLongDateTime			= $6C647420 (* 'ldt ' *);						{   0x6c647420   }
	typeISO8601DateTime         = $69736F74 (* 'isot' *);                       {   0x69736f74  data is ascii text of an ISO8601 date }
	typeLongFixed				= $6C667864 (* 'lfxd' *);						{   0x6c667864   }
	typeLongFixedPoint			= $6C667074 (* 'lfpt' *);						{   0x6c667074   }
	typeLongFixedRectangle		= $6C667263 (* 'lfrc' *);						{   0x6c667263   }
	typeLongPoint				= $6C706E74 (* 'lpnt' *);						{   0x6c706e74   }
	typeLongRectangle			= $6C726374 (* 'lrct' *);						{   0x6c726374   }
	typeMachineLoc				= $6D4C6F63 (* 'mLoc' *);						{   0x6d4c6f63   }
	typeOval					= $636F766C (* 'covl' *);						{   0x636f766c   }
	typeParamInfo				= $706D696E (* 'pmin' *);						{   0x706d696e   }
	typePict					= $50494354 (* 'PICT' *);						{   0x50494354   }

	typePixelMap				= $63706978 (* 'cpix' *);						{   0x63706978   }
	typePixMapMinus				= $74706D6D (* 'tpmm' *);						{   0x74706d6d   }
	typePolygon					= $6370676E (* 'cpgn' *);						{   0x6370676e   }
	typePropInfo				= $70696E66 (* 'pinf' *);						{   0x70696e66   }
	typePtr						= $70747220 (* 'ptr ' *);						{   0x70747220   }
	typeQDPoint					= $51447074 (* 'QDpt' *);						{   0x51447074   }
	typeQDRegion				= $5172676E (* 'Qrgn' *);						{   0x51447074  (data is actual region data, including rectangle and size, _not_ region handle or ptr) }
	typeRectangle				= $63726563 (* 'crec' *);						{   0x63726563   }
	typeRGB16					= $74723136 (* 'tr16' *);						{   0x74723136   }
	typeRGB96					= $74723936 (* 'tr96' *);						{   0x74723936   }
	typeRGBColor				= $63524742 (* 'cRGB' *);						{   0x63524742   }
	typeRotation				= $74726F74 (* 'trot' *);						{   0x74726f74   }
	typeRoundedRectangle		= $63727263 (* 'crrc' *);						{   0x63727263   }
	typeRow						= $63726F77 (* 'crow' *);						{   0x63726f77   }
	typeScrapStyles				= $7374796C (* 'styl' *);						{   0x7374796c   }
	typeScript					= $73637074 (* 'scpt' *);						{   0x73637074   }
	typeStyledText				= $53545854 (* 'STXT' *);						{   0x53545854   }
	typeSuiteInfo				= $7375696E (* 'suin' *);						{   0x7375696e   }
	typeTable					= $6374626C (* 'ctbl' *);						{   0x6374626c   }
	typeTextStyles				= $74737479 (* 'tsty' *);						{   0x74737479   }

	typeTIFF					= $54494646 (* 'TIFF' *);						{   0x54494646   }
	typeJPEG                    = $4A504547 (* 'JPEG' *);
	typeGIF                     = $47494666 (* 'GIFf' *);
	typeVersion					= $76657273 (* 'vers' *);						{   0x76657273   }

	kAEMenuClass				= $6D656E75 (* 'menu' *);
	kAEMenuSelect				= $6D686974 (* 'mhit' *);
	kAEMouseDown				= $6D64776E (* 'mdwn' *);
	kAEMouseDownInBack			= $6D64626B (* 'mdbk' *);
	kAEKeyDown					= $6B64776E (* 'kdwn' *);
	kAEResized					= $7273697A (* 'rsiz' *);
	kAEPromise					= $70726F6D (* 'prom' *);

	keyMenuID					= $6D696420 (* 'mid ' *);
	keyMenuItem					= $6D69746D (* 'mitm' *);
	keyCloseAllWindows			= $63617720 (* 'caw ' *);
	keyOriginalBounds			= $6F626E64 (* 'obnd' *);
	keyNewBounds				= $6E626E64 (* 'nbnd' *);
	keyLocalWhere				= $6C776872 (* 'lwhr' *);

	typeHIMenu					= $6D6F626A (* 'mobj' *);
	typeHIWindow				= $776F626A (* 'wobj' *);

	kBySmallIcon				= 0;
	kByIconView					= 1;
	kByNameView					= 2;
	kByDateView					= 3;
	kBySizeView					= 4;
	kByKindView					= 5;
	kByCommentView				= 6;
	kByLabelView				= 7;
	kByVersionView				= 8;

	kAEInfo						= 11;
	kAEMain						= 0;
	kAESharing					= 13;

	kAEZoomIn					= 7;
	kAEZoomOut					= 8;

	kTextServiceClass			= $74737663 (* 'tsvc' *);
	kUpdateActiveInputArea		= $75706474 (* 'updt' *);						{  update the active input area  }
	kShowHideInputWindow		= $73686977 (* 'shiw' *);						{  show or hide the input window  }
	kPos2Offset					= $70327374 (* 'p2st' *);						{  converting global coordinates to char position  }
	kOffset2Pos					= $73743270 (* 'st2p' *);						{  converting char position to global coordinates  }
	kUnicodeNotFromInputMethod	= $756E696D (* 'unim' *);						{  Unicode text when event not handled by Input Method or no Input Method  }
	kGetSelectedText			= $67747874 (* 'gtxt' *);						{  Get text for current selection  }
	keyAETSMDocumentRefcon		= $72656663 (* 'refc' *);						{  TSM document refcon, typeLongInteger  }
	keyAEServerInstance			= $73727669 (* 'srvi' *);						{  component instance  }
	keyAETheData				= $6B646174 (* 'kdat' *);						{  typeText  }
	keyAEFixLength				= $6669786C (* 'fixl' *);						{  fix len  }
	keyAEUpdateRange			= $75646E67 (* 'udng' *);						{  typeTextRangeArray  }
	keyAECurrentPoint			= $63706F73 (* 'cpos' *);						{  current point  }
	keyAEBufferSize				= $62756666 (* 'buff' *);						{  buffer size to get the text  }
	keyAEMoveView				= $6D767677 (* 'mvvw' *);						{  move view flag  }
	keyAENextBody				= $6E786264 (* 'nxbd' *);						{  next or previous body  }
	keyAETSMScriptTag			= $73636C67 (* 'sclg' *);
	keyAETSMTextFont			= $6B747866 (* 'ktxf' *);						{  FMFontFamily or FOND ID  }
	keyAETSMTextFMFont			= $6B74786D (* 'ktxm' *);						{  FMFont  }
	keyAETSMTextPointSize		= $6B747073 (* 'ktps' *);
	keyAETSMEventRecord			= $74657674 (* 'tevt' *);						{  Low level Event Record, typeLowLevelEventRecord  }
	keyAETSMEventRef			= $74657672 (* 'tevr' *);						{  Carbon EventRef, typeEventRef  }
	keyAETextServiceEncoding	= $7473656E (* 'tsen' *);						{  Text Service encoding, mac or Unicode in UpdateActiveInputArea or GetSelectedText events.  }
	keyAETextServiceMacEncoding	= $746D656E (* 'tmen' *);						{  Target mac encoding for TSM conversion of text from Unicode text service.  }
	typeTextRange				= $7478726E (* 'txrn' *);						{  TextRange  }
	typeComponentInstance		= $636D7069 (* 'cmpi' *);						{  server instance  }
	typeOffsetArray				= $6F666179 (* 'ofay' *);						{  offset array  }
	typeTextRangeArray			= $74726179 (* 'tray' *);
	typeLowLevelEventRecord		= $65767472 (* 'evtr' *);						{  Low Level Event Record  }
	typeEventRef				= $65767266 (* 'evrf' *);						{  Carbon EventRef  }
	typeText					= $54455854 (* 'TEXT' *);						{  Plain text  }


	{	 Desc type constants 	}
	kTSMOutsideOfBody			= 1;
	kTSMInsideOfBody			= 2;
	kTSMInsideOfActiveInputArea	= 3;

	kNextBody					= 1;
	kPreviousBody				= 2;


type
	TextRangePtr = ^TextRange;
	TextRange = record
		fStart:					SInt32;
		fEnd:					SInt32;
		fHiliteStyle:			SInt16;
	end;

	TextRangeHandle						= ^TextRangePtr;
	TextRangeArrayPtr = ^TextRangeArray;
	TextRangeArray = record
		fNumOfRanges:			SInt16;								{  specify the size of the fRange array  }
		fRange:					array [0..0] of TextRange;				{  when fNumOfRanges > 1, the size of this array has to be calculated  }
	end;

	TextRangeArrayHandle				= ^TextRangeArrayPtr;
	OffsetArrayPtr = ^OffsetArray;
	OffsetArray = record
		fNumOfOffsets:			SInt16;								{  specify the size of the fOffset array  }
		fOffset:				array [0..0] of SInt32;				{  when fNumOfOffsets > 1, the size of this array has to be calculated  }
	end;

	OffsetArrayHandle					= ^OffsetArrayPtr;
	WritingCodePtr = ^WritingCode;
	WritingCode = record
		theScriptCode:			ScriptCode;
		theLangCode:			LangCode;
	end;

	IntlTextPtr = ^IntlText;
	IntlText = record
		theScriptCode:			ScriptCode;
		theLangCode:			LangCode;
		theText:				SInt8;									{  variable length data  }
	end;


	{	 Hilite styles 	}

const
	kTSMHiliteCaretPosition     = 1;    { specify caret position }
	kTSMHiliteRawText           = 2;    { specify range of raw text }
	kTSMHiliteSelectedRawText   = 3;    { specify range of selected raw text }
	kTSMHiliteConvertedText     = 4;    { specify range of converted text }
	kTSMHiliteSelectedConvertedText = 5;  { specify range of selected converted text }
	kTSMHiliteBlockFillText     = 6;    { Block Fill hilite style }
	kTSMHiliteOutlineText       = 7;    { Outline hilite style }
	kTSMHiliteSelectedText      = 8;    { Selected hilite style }
	kTSMHiliteNoHilite          = 9;     { specify range of non-hilited text }

{$ifc OLDROUTINENAMES}
{ Hilite styles }
const
	kCaretPosition             = kTSMHiliteCaretPosition;
	kRawText                   = kTSMHiliteRawText;
	kSelectedRawText           = kTSMHiliteSelectedRawText;
	kConvertedText             = kTSMHiliteConvertedText;
	kSelectedConvertedText     = kTSMHiliteSelectedConvertedText;
	kBlockFillText             = kTSMHiliteBlockFillText;
	kOutlineText               = kTSMHiliteOutlineText;
	kSelectedText              = kTSMHiliteSelectedText;

{$endc}  {OLDROUTINENAMES}


	keyAEHiliteRange			= $68726E67 (* 'hrng' *);						{  typeTextRangeArray for System 7, typeHiliteRangeArray for System 8  }
	keyAEPinRange				= $706E7267 (* 'pnrg' *);						{  typeTextRange for System 7, typeTextRegionRange for System 8    }
	keyAEClauseOffsets			= $636C6175 (* 'clau' *);						{  typeOffsetArray for System 7, typeClauseOffsetArray for System 8  }
	keyAEOffset					= $6F667374 (* 'ofst' *);						{  typeLongInteger for System 7, typeByteOffset for System 8   }
	keyAEPoint					= $67706F73 (* 'gpos' *);						{  typePoint for System 7, typeQDPoint for System 8  }
	keyAELeftSide				= $6B6C6566 (* 'klef' *);						{  typeBoolean  }
	keyAERegionClass			= $72676E63 (* 'rgnc' *);						{  typeShortInteger for System 7, typeRegionClass for System 8  }
	keyAEDragging				= $626F6F6C (* 'bool' *);						{  typeBoolean  }


{$ifc OLDROUTINENAMES}
	keyAELeadingEdge			= $6B6C6566 (* 'klef' *);

{$endc}  {OLDROUTINENAMES}

																{  AppleScript 1.3: New Text types  }
{
 * The following descriptor types are deprecated due to their lack of
 * explicit encoding or byte order definition.  Please use
 * typeUTF16ExternalRepresentation or typeUTF8Text instead. }

	typeUnicodeText				= $75747874 (* 'utxt' *); { native byte ordering, optional BOM 
	typeStyledUnicodeText		= $73757478 (* 'sutx' *); { Not implemented }
	typeEncodedString			= $656E6373 (* 'encs' *); { Not implemented }
	typeCString					= $63737472 (* 'cstr' *); { MacRoman characters followed by a NULL byte }
	typePString					= $70737472 (* 'pstr' *); { Unsigned length byte followed by MacRoman characters }

																{  AppleScript 1.3: Unit types  }
	typeMeters					= $6D657472 (* 'metr' *);						{  Base Unit  }
	typeInches					= $696E6368 (* 'inch' *);
	typeFeet					= $66656574 (* 'feet' *);
	typeYards					= $79617264 (* 'yard' *);
	typeMiles					= $6D696C65 (* 'mile' *);
	typeKilometers				= $6B6D7472 (* 'kmtr' *);
	typeCentimeters				= $636D7472 (* 'cmtr' *);
	typeSquareMeters			= $7371726D (* 'sqrm' *);						{  Base Unit  }
	typeSquareFeet				= $73716674 (* 'sqft' *);
	typeSquareYards				= $73717964 (* 'sqyd' *);
	typeSquareMiles				= $73716D69 (* 'sqmi' *);
	typeSquareKilometers		= $73716B6D (* 'sqkm' *);
	typeLiters					= $6C697472 (* 'litr' *);						{  Base Unit  }
	typeQuarts					= $71727473 (* 'qrts' *);
	typeGallons					= $67616C6E (* 'galn' *);
	typeCubicMeters				= $636D6574 (* 'cmet' *);						{  Base Unit  }
	typeCubicFeet				= $63666574 (* 'cfet' *);
	typeCubicInches				= $6375696E (* 'cuin' *);
	typeCubicCentimeter			= $63636D74 (* 'ccmt' *);
	typeCubicYards				= $63797264 (* 'cyrd' *);
	typeKilograms				= $6B67726D (* 'kgrm' *);						{  Base Unit  }
	typeGrams					= $6772616D (* 'gram' *);
	typeOunces					= $6F7A7320 (* 'ozs ' *);
	typePounds					= $6C627320 (* 'lbs ' *);
	typeDegreesC				= $64656763 (* 'degc' *);						{  Base Unit  }
	typeDegreesF				= $64656766 (* 'degf' *);
	typeDegreesK				= $6465676B (* 'degk' *);

																{  AppleScript 1.3: Folder Actions  }
	kFAServerApp				= $73737276 (* 'ssrv' *);						{  Creator code for Folder Actions Server }
	kDoFolderActionEvent		= $666F6C61 (* 'fola' *);						{  Event the Finder sends to the Folder Actions FBA }
	kFolderActionCode			= $6163746E (* 'actn' *);						{  Parameter that contains the Folder Action }
	kFolderOpenedEvent			= $666F706E (* 'fopn' *);						{  Value of kFolderActionCode parameter; sent to script as event }
	kFolderClosedEvent			= $66636C6F (* 'fclo' *);
	kFolderWindowMovedEvent		= $6673697A (* 'fsiz' *);
	kFolderItemsAddedEvent		= $66676574 (* 'fget' *);
	kFolderItemsRemovedEvent	= $666C6F73 (* 'flos' *);
	kItemList					= $666C7374 (* 'flst' *);						{  List parameter for added and removed items }
	kNewSizeParameter			= $666E737A (* 'fnsz' *);						{  Parameter for moved window }
	kFASuiteCode				= $6661636F (* 'faco' *);						{  Suite code for the following events }
	kFAAttachCommand			= $61746661 (* 'atfa' *);						{  Attach event id }
	kFARemoveCommand			= $726D6661 (* 'rmfa' *);						{  Remove event id }
	kFAEditCommand				= $65646661 (* 'edfa' *);						{  Edit event id }
	kFAFileParam				= $6661616C (* 'faal' *);						{  Key for file parameter for Attach }
	kFAIndexParam				= $696E6478 (* 'indx' *);						{  Key for index (0-based) parameter for Remove and Edit }

	{	 AppleScript 1.3 Internet Suite 	}
																{  Suite code  }
	kAEInternetSuite			= $6775726C (* 'gurl' *);
	kAEISWebStarSuite			= $575757CE (* 'WWW½' *);

																{  Events  }
	kAEISGetURL					= $6775726C (* 'gurl' *);
	KAEISHandleCGI				= $73646F63 (* 'sdoc' *);

																{  Classes  }
	cURL						= $75726C20 (* 'url ' *);
	cInternetAddress			= $49504144 (* 'IPAD' *);
	cHTML						= $68746D6C (* 'html' *);
	cFTPItem					= $66747020 (* 'ftp ' *);

																{  Parameters  }
	kAEISHTTPSearchArgs			= $6B666F72 (* 'kfor' *);
	kAEISPostArgs				= $706F7374 (* 'post' *);
	kAEISMethod					= $6D657468 (* 'meth' *);
	kAEISClientAddress			= $61646472 (* 'addr' *);
	kAEISUserName				= $75736572 (* 'user' *);
	kAEISPassword				= $70617373 (* 'pass' *);
	kAEISFromUser				= $66726D75 (* 'frmu' *);
	kAEISServerName				= $73766E6D (* 'svnm' *);
	kAEISServerPort				= $73767074 (* 'svpt' *);
	kAEISScriptName				= $73636E6D (* 'scnm' *);
	kAEISContentType			= $63747970 (* 'ctyp' *);
	kAEISReferrer				= $72656672 (* 'refr' *);
	kAEISUserAgent				= $41676E74 (* 'Agnt' *);
	kAEISAction					= $4B616374 (* 'Kact' *);
	kAEISActionPath				= $4B617074 (* 'Kapt' *);
	kAEISClientIP				= $4B636970 (* 'Kcip' *);
	kAEISFullRequest			= $4B667271 (* 'Kfrq' *);

																{  Properties  }
	pScheme						= $70757363 (* 'pusc' *);
	pHost						= $484F5354 (* 'HOST' *);
	pPath						= $46545063 (* 'FTPc' *);
	pUserName					= $5241756E (* 'RAun' *);
	pUserPassword				= $52417077 (* 'RApw' *);
	pDNSForm					= $70444E53 (* 'pDNS' *);
	pURL						= $7055524C (* 'pURL' *);
	pTextEncoding				= $70747865 (* 'ptxe' *);
	pFTPKind					= $6B696E64 (* 'kind' *);

																{  Scheme enumerations  }
	eScheme						= $65736368 (* 'esch' *);
	eurlHTTP					= $68747470 (* 'http' *);						{  RFC 2068  }
	eurlHTTPS					= $68747073 (* 'htps' *);
	eurlFTP						= $66747020 (* 'ftp ' *);						{  RFC 1738  }
	eurlMail					= $6D61696C (* 'mail' *);						{  RFC 2638  }
	eurlFile					= $66696C65 (* 'file' *);						{  RFC 1738  }
	eurlGopher					= $67706872 (* 'gphr' *);						{  RFC 1738  }
	eurlTelnet					= $746C6E74 (* 'tlnt' *);						{  RFC 1738  }
	eurlNews					= $6E657773 (* 'news' *);						{  RFC 1738  }
	eurlSNews					= $736E7773 (* 'snws' *);
	eurlNNTP					= $6E6E7470 (* 'nntp' *);						{  RFC 1738  }
	eurlMessage					= $6D657373 (* 'mess' *);
	eurlMailbox					= $6D626F78 (* 'mbox' *);
	eurlMulti					= $6D756C74 (* 'mult' *);
	eurlLaunch					= $6C61756E (* 'laun' *);
	eurlAFP						= $61667020 (* 'afp ' *);
	eurlAT						= $61742020 (* 'at  ' *);
	eurlEPPC					= $65707063 (* 'eppc' *);
	eurlRTSP					= $72747370 (* 'rtsp' *);						{  RFC 2326  }
	eurlIMAP					= $696D6170 (* 'imap' *);						{  RFC 2192  }
	eurlNFS						= $756E6673 (* 'unfs' *);						{  RFC 2224  }
	eurlPOP						= $75706F70 (* 'upop' *);						{  RFC 2384  }
	eurlLDAP					= $756C6470 (* 'uldp' *);						{  RFC 2255  }
	eurlUnknown					= $75726C3F (* 'url?' *);

																{  AppleScript 1.3: Connectivity Suite in aeut  }
	kConnSuite					= $6D616363 (* 'macc' *);
	cDevSpec					= $63646576 (* 'cdev' *);
	cAddressSpec				= $63616472 (* 'cadr' *);
	cADBAddress					= $63616462 (* 'cadb' *);
	cAppleTalkAddress			= $63617420 (* 'cat ' *);
	cBusAddress					= $63627573 (* 'cbus' *);
	cEthernetAddress			= $63656E20 (* 'cen ' *);
	cFireWireAddress			= $63667720 (* 'cfw ' *);
	cIPAddress					= $63697020 (* 'cip ' *);
	cLocalTalkAddress			= $636C7420 (* 'clt ' *);
	cSCSIAddress				= $63736373 (* 'cscs' *);
	cTokenRingAddress			= $63746F6B (* 'ctok' *);
	cUSBAddress					= $63757362 (* 'cusb' *);						{   }
																{  Properties  }
	pDeviceType					= $70647674 (* 'pdvt' *);
	pDeviceAddress				= $70647661 (* 'pdva' *);
	pConduit					= $70636F6E (* 'pcon' *);
	pProtocol					= $70707274 (* 'pprt' *);						{  cde 4/27/98 was 'ppro' conflicted with DB suite  }
	pATMachine					= $7061746D (* 'patm' *);
	pATZone						= $7061747A (* 'patz' *);
	pATType						= $70617474 (* 'patt' *);
	pDottedDecimal				= $70697064 (* 'pipd' *);
	pDNS						= $70646E73 (* 'pdns' *);
	pPort						= $70706F72 (* 'ppor' *);
	pNetwork					= $706E6574 (* 'pnet' *);
	pNode						= $706E6F64 (* 'pnod' *);
	pSocket						= $70736F63 (* 'psoc' *);
	pSCSIBus					= $70736362 (* 'pscb' *);
	pSCSILUN					= $70736C75 (* 'pslu' *);						{  cde 5/22/98 per WWDC developer request  }
																{  Enumerations and enumerators  }
	eDeviceType					= $65647674 (* 'edvt' *);
	eAddressSpec				= $65616473 (* 'eads' *);
	eConduit					= $65636F6E (* 'econ' *);
	eProtocol					= $6570726F (* 'epro' *);
	eADB						= $65616462 (* 'eadb' *);
	eAnalogAudio				= $65706175 (* 'epau' *);
	eAppleTalk					= $65706174 (* 'epat' *);
	eAudioLineIn				= $65636169 (* 'ecai' *);
	eAudioLineOut				= $6563616C (* 'ecal' *);						{  cde 4/24/98 changed from 'ecao' to not conflict  }
	eAudioOut					= $6563616F (* 'ecao' *);
	eBus						= $65627573 (* 'ebus' *);
	eCDROM						= $65636420 (* 'ecd ' *);
	eCommSlot					= $6563636D (* 'eccm' *);
	eDigitalAudio				= $65706461 (* 'epda' *);
	eDisplay					= $65646473 (* 'edds' *);
	eDVD						= $65647664 (* 'edvd' *);
	eEthernet					= $6563656E (* 'ecen' *);
	eFireWire					= $65636677 (* 'ecfw' *);
	eFloppy						= $65666420 (* 'efd ' *);
	eHD							= $65686420 (* 'ehd ' *);
	eInfrared					= $65636972 (* 'ecir' *);
	eIP							= $65706970 (* 'epip' *);
	eIrDA						= $65706972 (* 'epir' *);
	eIRTalk						= $65706974 (* 'epit' *);
	eKeyboard					= $656B6264 (* 'ekbd' *);
	eLCD						= $65646C63 (* 'edlc' *);
	eLocalTalk					= $65636C74 (* 'eclt' *);
	eMacIP						= $65706D69 (* 'epmi' *);
	eMacVideo					= $65706D76 (* 'epmv' *);
	eMicrophone					= $65636D69 (* 'ecmi' *);
	eModemPort					= $65636D70 (* 'ecmp' *);
	eModemPrinterPort			= $656D7070 (* 'empp' *);
	eModem						= $65646D6D (* 'edmm' *);
	eMonitorOut					= $65636D6E (* 'ecmn' *);
	eMouse						= $656D6F75 (* 'emou' *);
	eNuBusCard					= $65646E62 (* 'ednb' *);
	eNuBus						= $656E7562 (* 'enub' *);
	ePCcard						= $65637063 (* 'ecpc' *);
	ePCIbus						= $65637069 (* 'ecpi' *);
	ePCIcard					= $65647069 (* 'edpi' *);
	ePDSslot					= $65637064 (* 'ecpd' *);
	ePDScard					= $65706473 (* 'epds' *);
	ePointingDevice				= $65647064 (* 'edpd' *);
	ePostScript					= $65707073 (* 'epps' *);
	ePPP						= $65707070 (* 'eppp' *);
	ePrinterPort				= $65637070 (* 'ecpp' *);
	ePrinter					= $65647072 (* 'edpr' *);
	eSvideo						= $65707376 (* 'epsv' *);
	eSCSI						= $65637363 (* 'ecsc' *);
	eSerial						= $65707372 (* 'epsr' *);
	eSpeakers					= $65647370 (* 'edsp' *);
	eStorageDevice				= $65647374 (* 'edst' *);
	eSVGA						= $65707367 (* 'epsg' *);
	eTokenRing					= $65746F6B (* 'etok' *);
	eTrackball					= $6574726B (* 'etrk' *);
	eTrackpad					= $65647470 (* 'edtp' *);
	eUSB						= $65637573 (* 'ecus' *);
	eVideoIn					= $65637669 (* 'ecvi' *);
	eVideoMonitor				= $6564766D (* 'edvm' *);
	eVideoOut					= $6563766F (* 'ecvo' *);

																{  AppleScript 1.3: Keystroke class  }
	cKeystroke					= $6B707273 (* 'kprs' *);
	pKeystrokeKey				= $6B4D7367 (* 'kMsg' *);
	pModifiers					= $6B4D6F64 (* 'kMod' *);
	pKeyKind					= $6B6B6E64 (* 'kknd' *);
	eModifiers					= $654D6473 (* 'eMds' *);
	eOptionDown					= $4B6F7074 (* 'Kopt' *);
	eCommandDown				= $4B636D64 (* 'Kcmd' *);
	eControlDown				= $4B63746C (* 'Kctl' *);
	eShiftDown					= $4B736674 (* 'Ksft' *);
	eCapsLockDown				= $4B636C6B (* 'Kclk' *);
	eKeyKind					= $656B7374 (* 'ekst' *);						{   }
																{  Special keys all start with 'ks'  }
	eEscapeKey					= $6B733500;					{  Third byte is virtual key code byte         }
	eDeleteKey					= $6B733300;					{  (see IM Mac Toolbox Essentials, pp. 2-43)  }
	eTabKey						= $6B733000;
	eReturnKey					= $6B732400;
	eClearKey					= $6B734700;
	eEnterKey					= $6B734C00;
	eUpArrowKey					= $6B737E00;
	eDownArrowKey				= $6B737D00;
	eLeftArrowKey				= $6B737B00;
	eRightArrowKey				= $6B737C00;
	eHelpKey					= $6B737200;
	eHomeKey					= $6B737300;
	ePageUpKey					= $6B737400;
	ePageDownKey				= $6B737900;
	eForwardDelKey				= $6B737500;
	eEndKey						= $6B737700;
	eF1Key						= $6B737A00;
	eF2Key						= $6B737800;
	eF3Key						= $6B736300;
	eF4Key						= $6B737600;
	eF5Key						= $6B736000;
	eF6Key						= $6B736100;
	eF7Key						= $6B736200;
	eF8Key						= $6B736400;
	eF9Key						= $6B736500;
	eF10Key						= $6B736D00;
	eF11Key						= $6B736700;
	eF12Key						= $6B736F00;
	eF13Key						= $6B736900;
	eF14Key						= $6B736B00;
	eF15Key						= $6B737100;

	keyAEQuitWithoutUI          = $6E6F7569 (* 'noui' *); { If present in a kAEQuitApplication event, autosave any documents with uncommitted changes and quit }

{$ALIGN MAC68K}


end.
