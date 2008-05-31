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
    Version 210
    Please report any bugs to <gpc@microbizz.nl>
}

{$mode macpas}
{$packenum 1}
{$macro on}
{$inline on}
{$calling mwpascal}

unit ASRegistry;
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
uses MacTypes,AERegistry,AEObjects;


{$ALIGN MAC68K}


const
	keyAETarget					= FourCharCode('targ');
	keySubjectAttr				= FourCharCode('subj');						{  Magic 'returning' parameter:  }
	keyASReturning				= FourCharCode('Krtn');						{  AppleScript Specific Codes:  }
	kASAppleScriptSuite			= FourCharCode('ascr');
	kASScriptEditorSuite		= FourCharCode('ToyS');
	kASTypeNamesSuite			= FourCharCode('tpnm');						{  dynamic terminologies  }
	typeAETE					= FourCharCode('aete');
	typeAEUT					= FourCharCode('aeut');
	kGetAETE					= FourCharCode('gdte');
	kGetAEUT					= FourCharCode('gdut');
	kUpdateAEUT					= FourCharCode('udut');
	kUpdateAETE					= FourCharCode('udte');
	kCleanUpAEUT				= FourCharCode('cdut');
	kASComment					= FourCharCode('cmnt');
	kASLaunchEvent				= FourCharCode('noop');
	keyScszResource				= FourCharCode('scsz');
	typeScszResource			= FourCharCode('scsz');						{  subroutine calls  }
	kASSubroutineEvent			= FourCharCode('psbr');
	keyASSubroutineName			= FourCharCode('snam');
	kASPrepositionalSubroutine	= FourCharCode('psbr');
	keyASPositionalArgs			= FourCharCode('parg');

																{  Add this parameter to a Get Data result if your app handled the 'as' parameter  }
	keyAppHandledCoercion		= FourCharCode('idas');

																{  Miscellaneous AppleScript commands  }
	kASStartLogEvent			= FourCharCode('log1');
	kASStopLogEvent				= FourCharCode('log0');
	kASCommentEvent				= FourCharCode('cmnt');


	{	 Operator Events: 	}
																{  Binary:  }
	kASAdd						= FourCharCode('+   ');
	kASSubtract					= FourCharCode('-   ');
	kASMultiply					= FourCharCode('*   ');
	kASDivide					= FourCharCode('/   ');
	kASQuotient					= FourCharCode('div ');
	kASRemainder				= FourCharCode('mod ');
	kASPower					= FourCharCode('^   ');
	kASEqual					= FourCharCode('=   ');
	kASNotEqual					= FourCharCode('­   ');
	kASGreaterThan				= FourCharCode('>   ');
	kASGreaterThanOrEqual		= FourCharCode('>=  ');
	kASLessThan					= FourCharCode('<   ');
	kASLessThanOrEqual			= FourCharCode('<=  ');
	kASComesBefore				= FourCharCode('cbfr');
	kASComesAfter				= FourCharCode('cafr');
	kASConcatenate				= FourCharCode('ccat');
	kASStartsWith				= FourCharCode('bgwt');
	kASEndsWith					= FourCharCode('ends');
	kASContains					= FourCharCode('cont');

	kASAnd						= FourCharCode('AND ');
	kASOr						= FourCharCode('OR  ');						{  Unary:  }
	kASNot						= FourCharCode('NOT ');
	kASNegate					= FourCharCode('neg ');
	keyASArg					= FourCharCode('arg ');

																{  event code for the 'error' statement  }
	kASErrorEventCode			= FourCharCode('err ');
	kOSAErrorArgs				= FourCharCode('erra');
	keyAEErrorObject			= FourCharCode('erob');						{  Properties:  }
	pLength						= FourCharCode('leng');
	pReverse					= FourCharCode('rvse');
	pRest						= FourCharCode('rest');
	pInherits					= FourCharCode('c@#^');
	pProperties					= FourCharCode('pALL');						{  User-Defined Record Fields:  }
	keyASUserRecordFields		= FourCharCode('usrf');
	typeUserRecordFields		= FourCharCode('list');

	{	 Prepositions: 	}
	keyASPrepositionAt			= FourCharCode('at  ');
	keyASPrepositionIn			= FourCharCode('in  ');
	keyASPrepositionFrom		= FourCharCode('from');
	keyASPrepositionFor			= FourCharCode('for ');
	keyASPrepositionTo			= FourCharCode('to  ');
	keyASPrepositionThru		= FourCharCode('thru');
	keyASPrepositionThrough		= FourCharCode('thgh');
	keyASPrepositionBy			= FourCharCode('by  ');
	keyASPrepositionOn			= FourCharCode('on  ');
	keyASPrepositionInto		= FourCharCode('into');
	keyASPrepositionOnto		= FourCharCode('onto');
	keyASPrepositionBetween		= FourCharCode('btwn');
	keyASPrepositionAgainst		= FourCharCode('agst');
	keyASPrepositionOutOf		= FourCharCode('outo');
	keyASPrepositionInsteadOf	= FourCharCode('isto');
	keyASPrepositionAsideFrom	= FourCharCode('asdf');
	keyASPrepositionAround		= FourCharCode('arnd');
	keyASPrepositionBeside		= FourCharCode('bsid');
	keyASPrepositionBeneath		= FourCharCode('bnth');
	keyASPrepositionUnder		= FourCharCode('undr');

	keyASPrepositionOver		= FourCharCode('over');
	keyASPrepositionAbove		= FourCharCode('abve');
	keyASPrepositionBelow		= FourCharCode('belw');
	keyASPrepositionApartFrom	= FourCharCode('aprt');
	keyASPrepositionGiven		= FourCharCode('givn');
	keyASPrepositionWith		= FourCharCode('with');
	keyASPrepositionWithout		= FourCharCode('wout');
	keyASPrepositionAbout		= FourCharCode('abou');
	keyASPrepositionSince		= FourCharCode('snce');
	keyASPrepositionUntil		= FourCharCode('till');

																{  Terminology & Dialect things:  }
	kDialectBundleResType		= FourCharCode('Dbdl');						{  AppleScript Classes and Enums:  }
	cConstant					= FourCharCode('enum');
	cClassIdentifier			= FourCharCode('pcls');
	cObjectBeingExamined		= FourCharCode('exmn');
	cList						= FourCharCode('list');
	cSmallReal					= FourCharCode('sing');
	cReal						= FourCharCode('doub');
	cRecord						= FourCharCode('reco');
	cReference					= FourCharCode('obj ');
	cUndefined					= FourCharCode('undf');
	cMissingValue				= FourCharCode('msng');
	cSymbol						= FourCharCode('symb');
	cLinkedList					= FourCharCode('llst');
	cVector						= FourCharCode('vect');
	cEventIdentifier			= FourCharCode('evnt');
	cKeyIdentifier				= FourCharCode('kyid');
	cUserIdentifier				= FourCharCode('uid ');
	cPreposition				= FourCharCode('prep');
	cKeyForm					= FourCharCode('kfrm');
	cScript						= FourCharCode('scpt');
	cHandler					= FourCharCode('hand');
	cProcedure					= FourCharCode('proc');

	cHandleBreakpoint			= FourCharCode('brak');

	cClosure					= FourCharCode('clsr');
	cRawData					= FourCharCode('rdat');
	cStringClass				= FourCharCode('TEXT');
	cNumber						= FourCharCode('nmbr');
	cListElement				= FourCharCode('celm');
	cListOrRecord				= FourCharCode('lr  ');
	cListOrString				= FourCharCode('ls  ');
	cListRecordOrString			= FourCharCode('lrs ');
	cNumberOrString				= FourCharCode('ns  ');
	cNumberOrDateTime			= FourCharCode('nd  ');
	cNumberDateTimeOrString		= FourCharCode('nds ');
	cAliasOrString				= FourCharCode('sf  ');
	cSeconds					= FourCharCode('scnd');
	typeSound					= FourCharCode('snd ');
	enumBooleanValues			= FourCharCode('boov');						{   Use this instead of typeBoolean to avoid with/without conversion   }
	kAETrue						= FourCharCode('true');
	kAEFalse					= FourCharCode('fals');
	enumMiscValues				= FourCharCode('misc');
	kASCurrentApplication		= FourCharCode('cura');						{  User-defined property ospecs:  }
	formUserPropertyID			= FourCharCode('usrp');

//	cString						= FourCharCode('TEXT');						{  old name for cStringClass - can't be used in .r files }

																{  Global properties:  }
	pASIt						= FourCharCode('it  ');
	pASMe						= FourCharCode('me  ');
	pASResult					= FourCharCode('rslt');
	pASSpace					= FourCharCode('spac');
	pASReturn					= FourCharCode('ret ');
	pASTab						= FourCharCode('tab ');
	pASPi						= FourCharCode('pi  ');
	pASParent					= FourCharCode('pare');
	kASInitializeEventCode		= FourCharCode('init');
	pASPrintLength				= FourCharCode('prln');
	pASPrintDepth				= FourCharCode('prdp');
	pASTopLevelScript			= FourCharCode('ascr');

																{  Considerations  }
	kAECase						= FourCharCode('case');
	kAEDiacritic				= FourCharCode('diac');
	kAEWhiteSpace				= FourCharCode('whit');
	kAEHyphens					= FourCharCode('hyph');
	kAEExpansion				= FourCharCode('expa');
	kAEPunctuation				= FourCharCode('punc');
	kAEZenkakuHankaku			= FourCharCode('zkhk');
	kAESmallKana				= FourCharCode('skna');
	kAEKataHiragana				= FourCharCode('hika');
	kASConsiderReplies			= FourCharCode('rmte');
	enumConsiderations			= FourCharCode('cons');

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
	enumConsidsAndIgnores		= FourCharCode('csig');

	cCoercion					= FourCharCode('coec');
	cCoerceUpperCase			= FourCharCode('txup');
	cCoerceLowerCase			= FourCharCode('txlo');
	cCoerceRemoveDiacriticals	= FourCharCode('txdc');
	cCoerceRemovePunctuation	= FourCharCode('txpc');
	cCoerceRemoveHyphens		= FourCharCode('txhy');
	cCoerceOneByteToTwoByte		= FourCharCode('txex');
	cCoerceRemoveWhiteSpace		= FourCharCode('txws');
	cCoerceSmallKana			= FourCharCode('txsk');
	cCoerceZenkakuhankaku		= FourCharCode('txze');
	cCoerceKataHiragana			= FourCharCode('txkh');						{  Lorax things:  }
	cZone						= FourCharCode('zone');
	cMachine					= FourCharCode('mach');
	cAddress					= FourCharCode('addr');
	cRunningAddress				= FourCharCode('radd');
	cStorage					= FourCharCode('stor');

																{  DateTime things:  }
	pASWeekday					= FourCharCode('wkdy');
	pASMonth					= FourCharCode('mnth');
	pASDay						= FourCharCode('day ');
	pASYear						= FourCharCode('year');
	pASTime						= FourCharCode('time');
	pASDateString				= FourCharCode('dstr');
	pASTimeString				= FourCharCode('tstr');						{  Months  }
	cMonth						= FourCharCode('mnth');
	cJanuary					= FourCharCode('jan ');
	cFebruary					= FourCharCode('feb ');
	cMarch						= FourCharCode('mar ');
	cApril						= FourCharCode('apr ');
	cMay						= FourCharCode('may ');
	cJune						= FourCharCode('jun ');
	cJuly						= FourCharCode('jul ');
	cAugust						= FourCharCode('aug ');
	cSeptember					= FourCharCode('sep ');
	cOctober					= FourCharCode('oct ');
	cNovember					= FourCharCode('nov ');
	cDecember					= FourCharCode('dec ');

																{  Weekdays  }
	cWeekday					= FourCharCode('wkdy');
	cSunday						= FourCharCode('sun ');
	cMonday						= FourCharCode('mon ');
	cTuesday					= FourCharCode('tue ');
	cWednesday					= FourCharCode('wed ');
	cThursday					= FourCharCode('thu ');
	cFriday						= FourCharCode('fri ');
	cSaturday					= FourCharCode('sat ');						{  AS 1.1 Globals:  }
	pASQuote					= FourCharCode('quot');
	pASSeconds					= FourCharCode('secs');
	pASMinutes					= FourCharCode('min ');
	pASHours					= FourCharCode('hour');
	pASDays						= FourCharCode('days');
	pASWeeks					= FourCharCode('week');						{  Writing Code things:  }
	cWritingCodeInfo			= FourCharCode('citl');
	pScriptCode					= FourCharCode('pscd');
	pLangCode					= FourCharCode('plcd');						{  Magic Tell and End Tell events for logging:  }
	kASMagicTellEvent			= FourCharCode('tell');
	kASMagicEndTellEvent		= FourCharCode('tend');

{$ALIGN MAC68K}


end.
