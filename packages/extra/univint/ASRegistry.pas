{
     File:       ASRegistry.p
 
     Contains:   AppleScript Registry constants.
 
     Version:    Technology: AppleScript 1.3
                 Release:    Universal Interfaces 3.4.2
 
     Copyright:  © 1991-2002 by Apple Computer, Inc., all rights reserved
 
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

unit ASRegistry;
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
uses MacTypes,AERegistry,AEObjects;


{$ALIGN MAC68K}


const
	keyAETarget					= $74617267 (* 'targ' *);
	keySubjectAttr				= $7375626A (* 'subj' *);						{  Magic 'returning' parameter:  }
	keyASReturning				= $4B72746E (* 'Krtn' *);						{  AppleScript Specific Codes:  }
	kASAppleScriptSuite			= $61736372 (* 'ascr' *);
	kASScriptEditorSuite		= $546F7953 (* 'ToyS' *);
	kASTypeNamesSuite			= $74706E6D (* 'tpnm' *);						{  dynamic terminologies  }
	typeAETE					= $61657465 (* 'aete' *);
	typeAEUT					= $61657574 (* 'aeut' *);
	kGetAETE					= $67647465 (* 'gdte' *);
	kGetAEUT					= $67647574 (* 'gdut' *);
	kUpdateAEUT					= $75647574 (* 'udut' *);
	kUpdateAETE					= $75647465 (* 'udte' *);
	kCleanUpAEUT				= $63647574 (* 'cdut' *);
	kASComment					= $636D6E74 (* 'cmnt' *);
	kASLaunchEvent				= $6E6F6F70 (* 'noop' *);
	keyScszResource				= $7363737A (* 'scsz' *);
	typeScszResource			= $7363737A (* 'scsz' *);						{  subroutine calls  }
	kASSubroutineEvent			= $70736272 (* 'psbr' *);
	keyASSubroutineName			= $736E616D (* 'snam' *);
	kASPrepositionalSubroutine	= $70736272 (* 'psbr' *);
	keyASPositionalArgs			= $70617267 (* 'parg' *);

																{  Add this parameter to a Get Data result if your app handled the 'as' parameter  }
	keyAppHandledCoercion		= $69646173 (* 'idas' *);

																{  Miscellaneous AppleScript commands  }
	kASStartLogEvent			= $6C6F6731 (* 'log1' *);
	kASStopLogEvent				= $6C6F6730 (* 'log0' *);
	kASCommentEvent				= $636D6E74 (* 'cmnt' *);


	{	 Operator Events: 	}
																{  Binary:  }
	kASAdd						= $2B202020 (* '+   ' *);
	kASSubtract					= $2D202020 (* '-   ' *);
	kASMultiply					= $2A202020 (* '*   ' *);
	kASDivide					= $2F202020 (* '/   ' *);
	kASQuotient					= $64697620 (* 'div ' *);
	kASRemainder				= $6D6F6420 (* 'mod ' *);
	kASPower					= $5E202020 (* '^   ' *);
	kASEqual					= $3D202020 (* '=   ' *);
	kASNotEqual					= $E289A020 (* '­   ' *);
	kASGreaterThan				= $3E202020 (* '>   ' *);
	kASGreaterThanOrEqual		= $3E3D2020 (* '>=  ' *);
	kASLessThan					= $3C202020 (* '<   ' *);
	kASLessThanOrEqual			= $3C3D2020 (* '<=  ' *);
	kASComesBefore				= $63626672 (* 'cbfr' *);
	kASComesAfter				= $63616672 (* 'cafr' *);
	kASConcatenate				= $63636174 (* 'ccat' *);
	kASStartsWith				= $62677774 (* 'bgwt' *);
	kASEndsWith					= $656E6473 (* 'ends' *);
	kASContains					= $636F6E74 (* 'cont' *);

	kASAnd						= $414E4420 (* 'AND ' *);
	kASOr						= $4F522020 (* 'OR  ' *);						{  Unary:  }
	kASNot						= $4E4F5420 (* 'NOT ' *);
	kASNegate					= $6E656720 (* 'neg ' *);
	keyASArg					= $61726720 (* 'arg ' *);

																{  event code for the 'error' statement  }
	kASErrorEventCode			= $65727220 (* 'err ' *);
	kOSAErrorArgs				= $65727261 (* 'erra' *);
	keyAEErrorObject			= $65726F62 (* 'erob' *);						{  Properties:  }
	pLength						= $6C656E67 (* 'leng' *);
	pReverse					= $72767365 (* 'rvse' *);
	pRest						= $72657374 (* 'rest' *);
	pInherits					= $6340235E (* 'c@#^' *);
	pProperties					= $70414C4C (* 'pALL' *);						{  User-Defined Record Fields:  }
	keyASUserRecordFields		= $75737266 (* 'usrf' *);
	typeUserRecordFields		= $6C697374 (* 'list' *);

	{	 Prepositions: 	}
	keyASPrepositionAt			= $61742020 (* 'at  ' *);
	keyASPrepositionIn			= $696E2020 (* 'in  ' *);
	keyASPrepositionFrom		= $66726F6D (* 'from' *);
	keyASPrepositionFor			= $666F7220 (* 'for ' *);
	keyASPrepositionTo			= $746F2020 (* 'to  ' *);
	keyASPrepositionThru		= $74687275 (* 'thru' *);
	keyASPrepositionThrough		= $74686768 (* 'thgh' *);
	keyASPrepositionBy			= $62792020 (* 'by  ' *);
	keyASPrepositionOn			= $6F6E2020 (* 'on  ' *);
	keyASPrepositionInto		= $696E746F (* 'into' *);
	keyASPrepositionOnto		= $6F6E746F (* 'onto' *);
	keyASPrepositionBetween		= $6274776E (* 'btwn' *);
	keyASPrepositionAgainst		= $61677374 (* 'agst' *);
	keyASPrepositionOutOf		= $6F75746F (* 'outo' *);
	keyASPrepositionInsteadOf	= $6973746F (* 'isto' *);
	keyASPrepositionAsideFrom	= $61736466 (* 'asdf' *);
	keyASPrepositionAround		= $61726E64 (* 'arnd' *);
	keyASPrepositionBeside		= $62736964 (* 'bsid' *);
	keyASPrepositionBeneath		= $626E7468 (* 'bnth' *);
	keyASPrepositionUnder		= $756E6472 (* 'undr' *);

	keyASPrepositionOver		= $6F766572 (* 'over' *);
	keyASPrepositionAbove		= $61627665 (* 'abve' *);
	keyASPrepositionBelow		= $62656C77 (* 'belw' *);
	keyASPrepositionApartFrom	= $61707274 (* 'aprt' *);
	keyASPrepositionGiven		= $6769766E (* 'givn' *);
	keyASPrepositionWith		= $77697468 (* 'with' *);
	keyASPrepositionWithout		= $776F7574 (* 'wout' *);
	keyASPrepositionAbout		= $61626F75 (* 'abou' *);
	keyASPrepositionSince		= $736E6365 (* 'snce' *);
	keyASPrepositionUntil		= $74696C6C (* 'till' *);

																{  Terminology & Dialect things:  }
	kDialectBundleResType		= $4462646C (* 'Dbdl' *);						{  AppleScript Classes and Enums:  }
	cConstant					= $656E756D (* 'enum' *);
	cClassIdentifier			= $70636C73 (* 'pcls' *);
	cObjectBeingExamined		= $65786D6E (* 'exmn' *);
	cList						= $6C697374 (* 'list' *);
	cSmallReal					= $73696E67 (* 'sing' *);
	cReal						= $646F7562 (* 'doub' *);
	cRecord						= $7265636F (* 'reco' *);
	cReference					= $6F626A20 (* 'obj ' *);
	cUndefined					= $756E6466 (* 'undf' *);
	cMissingValue				= $6D736E67 (* 'msng' *);
	cSymbol						= $73796D62 (* 'symb' *);
	cLinkedList					= $6C6C7374 (* 'llst' *);
	cVector						= $76656374 (* 'vect' *);
	cEventIdentifier			= $65766E74 (* 'evnt' *);
	cKeyIdentifier				= $6B796964 (* 'kyid' *);
	cUserIdentifier				= $75696420 (* 'uid ' *);
	cPreposition				= $70726570 (* 'prep' *);
	cKeyForm					= $6B66726D (* 'kfrm' *);
	cScript						= $73637074 (* 'scpt' *);
	cHandler					= $68616E64 (* 'hand' *);
	cProcedure					= $70726F63 (* 'proc' *);

	cHandleBreakpoint			= $6272616B (* 'brak' *);

	cClosure					= $636C7372 (* 'clsr' *);
	cRawData					= $72646174 (* 'rdat' *);
	cStringClass				= $54455854 (* 'TEXT' *);
	cNumber						= $6E6D6272 (* 'nmbr' *);
	cListElement				= $63656C6D (* 'celm' *);
	cListOrRecord				= $6C722020 (* 'lr  ' *);
	cListOrString				= $6C732020 (* 'ls  ' *);
	cListRecordOrString			= $6C727320 (* 'lrs ' *);
	cNumberOrString				= $6E732020 (* 'ns  ' *);
	cNumberOrDateTime			= $6E642020 (* 'nd  ' *);
	cNumberDateTimeOrString		= $6E647320 (* 'nds ' *);
	cAliasOrString				= $73662020 (* 'sf  ' *);
	cSeconds					= $73636E64 (* 'scnd' *);
	typeSound					= $736E6420 (* 'snd ' *);
	enumBooleanValues			= $626F6F76 (* 'boov' *);						{   Use this instead of typeBoolean to avoid with/without conversion   }
	kAETrue						= $74727565 (* 'true' *);
	kAEFalse					= $66616C73 (* 'fals' *);
	enumMiscValues				= $6D697363 (* 'misc' *);
	kASCurrentApplication		= $63757261 (* 'cura' *);						{  User-defined property ospecs:  }
	formUserPropertyID			= $75737270 (* 'usrp' *);

//	cString						= $54455854 (* 'TEXT' *);						{  old name for cStringClass - can't be used in .r files }

																{  Global properties:  }
	pASIt						= $69742020 (* 'it  ' *);
	pASMe						= $6D652020 (* 'me  ' *);
	pASResult					= $72736C74 (* 'rslt' *);
	pASSpace					= $73706163 (* 'spac' *);
	pASReturn					= $72657420 (* 'ret ' *);
	pASTab						= $74616220 (* 'tab ' *);
	pASPi						= $70692020 (* 'pi  ' *);
	pASParent					= $70617265 (* 'pare' *);
	kASInitializeEventCode		= $696E6974 (* 'init' *);
	pASPrintLength				= $70726C6E (* 'prln' *);
	pASPrintDepth				= $70726470 (* 'prdp' *);
	pASTopLevelScript			= $61736372 (* 'ascr' *);

																{  Considerations  }
	kAECase						= $63617365 (* 'case' *);
	kAEDiacritic				= $64696163 (* 'diac' *);
	kAEWhiteSpace				= $77686974 (* 'whit' *);
	kAEHyphens					= $68797068 (* 'hyph' *);
	kAEExpansion				= $65787061 (* 'expa' *);
	kAEPunctuation				= $70756E63 (* 'punc' *);
	kAEZenkakuHankaku			= $7A6B686B (* 'zkhk' *);
	kAESmallKana				= $736B6E61 (* 'skna' *);
	kAEKataHiragana				= $68696B61 (* 'hika' *);
	kASConsiderReplies			= $726D7465 (* 'rmte' *);
	enumConsiderations			= $636F6E73 (* 'cons' *);

	{	 Considerations bit masks 	}
	kAECaseConsiderMask			= $00000001;
	kAEDiacriticConsiderMask	= $00000002;
	kAEWhiteSpaceConsiderMask	= $00000004;
	kAEHyphensConsiderMask		= $00000008;
	kAEExpansionConsiderMask	= $00000010;
	kAEPunctuationConsiderMask	= $00000020;
	kASConsiderRepliesConsiderMask = $00000040;
	kAECaseIgnoreMask			= $00010000;
	kAEDiacriticIgnoreMask		= $00020000;
	kAEWhiteSpaceIgnoreMask		= $00040000;
	kAEHyphensIgnoreMask		= $00080000;
	kAEExpansionIgnoreMask		= $00100000;
	kAEPunctuationIgnoreMask	= $00200000;
	kASConsiderRepliesIgnoreMask = $00400000;
	enumConsidsAndIgnores		= $63736967 (* 'csig' *);

	cCoercion					= $636F6563 (* 'coec' *);
	cCoerceUpperCase			= $74787570 (* 'txup' *);
	cCoerceLowerCase			= $74786C6F (* 'txlo' *);
	cCoerceRemoveDiacriticals	= $74786463 (* 'txdc' *);
	cCoerceRemovePunctuation	= $74787063 (* 'txpc' *);
	cCoerceRemoveHyphens		= $74786879 (* 'txhy' *);
	cCoerceOneByteToTwoByte		= $74786578 (* 'txex' *);
	cCoerceRemoveWhiteSpace		= $74787773 (* 'txws' *);
	cCoerceSmallKana			= $7478736B (* 'txsk' *);
	cCoerceZenkakuhankaku		= $74787A65 (* 'txze' *);
	cCoerceKataHiragana			= $74786B68 (* 'txkh' *);						{  Lorax things:  }
	cZone						= $7A6F6E65 (* 'zone' *);
	cMachine					= $6D616368 (* 'mach' *);
	cAddress					= $61646472 (* 'addr' *);
	cRunningAddress				= $72616464 (* 'radd' *);
	cStorage					= $73746F72 (* 'stor' *);

																{  DateTime things:  }
	pASWeekday					= $776B6479 (* 'wkdy' *);
	pASMonth					= $6D6E7468 (* 'mnth' *);
	pASDay						= $64617920 (* 'day ' *);
	pASYear						= $79656172 (* 'year' *);
	pASTime						= $74696D65 (* 'time' *);
	pASDateString				= $64737472 (* 'dstr' *);
	pASTimeString				= $74737472 (* 'tstr' *);						{  Months  }
	cMonth						= $6D6E7468 (* 'mnth' *);
	cJanuary					= $6A616E20 (* 'jan ' *);
	cFebruary					= $66656220 (* 'feb ' *);
	cMarch						= $6D617220 (* 'mar ' *);
	cApril						= $61707220 (* 'apr ' *);
	cMay						= $6D617920 (* 'may ' *);
	cJune						= $6A756E20 (* 'jun ' *);
	cJuly						= $6A756C20 (* 'jul ' *);
	cAugust						= $61756720 (* 'aug ' *);
	cSeptember					= $73657020 (* 'sep ' *);
	cOctober					= $6F637420 (* 'oct ' *);
	cNovember					= $6E6F7620 (* 'nov ' *);
	cDecember					= $64656320 (* 'dec ' *);

																{  Weekdays  }
	cWeekday					= $776B6479 (* 'wkdy' *);
	cSunday						= $73756E20 (* 'sun ' *);
	cMonday						= $6D6F6E20 (* 'mon ' *);
	cTuesday					= $74756520 (* 'tue ' *);
	cWednesday					= $77656420 (* 'wed ' *);
	cThursday					= $74687520 (* 'thu ' *);
	cFriday						= $66726920 (* 'fri ' *);
	cSaturday					= $73617420 (* 'sat ' *);						{  AS 1.1 Globals:  }
	pASQuote					= $71756F74 (* 'quot' *);
	pASSeconds					= $73656373 (* 'secs' *);
	pASMinutes					= $6D696E20 (* 'min ' *);
	pASHours					= $686F7572 (* 'hour' *);
	pASDays						= $64617973 (* 'days' *);
	pASWeeks					= $7765656B (* 'week' *);						{  Writing Code things:  }
	cWritingCodeInfo			= $6369746C (* 'citl' *);
	pScriptCode					= $70736364 (* 'pscd' *);
	pLangCode					= $706C6364 (* 'plcd' *);						{  Magic Tell and End Tell events for logging:  }
	kASMagicTellEvent			= $74656C6C (* 'tell' *);
	kASMagicEndTellEvent		= $74656E64 (* 'tend' *);

{$ALIGN MAC68K}


end.
