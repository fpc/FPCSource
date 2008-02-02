(******************************************************************************
 *
 * Copyright (c) 2000 Palm, Inc. or its subsidiaries.
 * All rights reserved.
 *
 * File: PalmLocale.h
 *
 * Release: Palm OS SDK 4.0 (63220)
 *
 * Description:
 * Public header for simple constants that support locales (information
 * specific to locales and regions).  This file is designed to support
 * Rez in addition to C/C++.
 *
 * History:
 * 05/31/00 CS Created by Chris Schneider (from LocaleMgr.h).
 * 06/02/00 CS Moved character encoding constants in here (from TextMgr.h).
 * 06/05/00 kwk   Moved character encoding names in here (from TextMgr.h)
 *             Updated CP1252 charset name to "Windows-1252", as the new
 *             preferred standard. It was "ISO-8859-1-Windows-3.1-Latin-1".
 *             Added encodingNameUCS2. Fixed up some encoding constant names.
 * 06/05/00 CS Hacked in changes for Rez, which doesn't support macro
 *             parameters in #defines.
 * 06/06/00 CS Changed name form Locale.h to PalmLocale.h to avoid
 *             name collision with the Metrowerks Standard Library
 *             header Locale.h.
 *          CS Now use PALM_LOCALE_HAS_TYPES to control whether to
 *             use types (rather than just rez).
 * 06/07/00 kwk   Moved encoding name #defines out of conditional sections,
 *             since these don't depend on PALM_LOCALE_HAS_TYPES.
 *          kwk   Fixed charEncodingMax - was 76, should be 75.
 * 06/27/00 kwk   Fixed charEncodingMax in the automatically generated section
 *             to also be 75.
 *          kwk   Renumbered char encodings so that charEncodingUCS2 stays
 *             at 9 (where it was before I integrated with Michel), since
 *             the Sony version of TxtConvertEncoding relies on this.
 *             Basically swapped charEncodingUCS2 (was 25) with charEncodingEucJp
 *             (was 9).
 * 07/21/00 kwk   Added charEncodingUTF16, charEncodingUTF16BE, and charEncodingUTF16LE.
 *             The UTF16 encoding requires a BOM (byte order mark) in the text
 *             stream to identify big vs. little endian, while the other two
 *             are explicit. Note that for Palm OS, charEncodingUCS2 is the
 *             same as charEncodingUTF16BE without support for surrogates, but
 *             since we currently don't support characters > 16 bits, in reality
 *             charEncodingUTF16BE is more like charEncodingUCS2.
 *          kwk   Updated charEncodingMax to be 77 (was 74).
 * 08/01/00 kwk   Changed cNewCalidonia to cNewCaledonia.
 *
 *****************************************************************************)

unit palmlocale;

interface

uses localemgr, textmgr;

// Names of the known encodings.
const
  encodingNameAscii     = 'us-ascii';
  encodingNameISO8859_1 = 'ISO-8859-1';
  encodingNameCP1252    = 'Windows-1252';
  encodingNameShiftJIS  = 'Shift_JIS';
  encodingNameCP932     = 'Windows-31J';
  encodingNameUTF8      = 'UTF-8';
  encodingNameUCS2      = 'ISO-10646-UCS-2';

// Maximum length of any encoding name.

const
  maxEncodingNameLength = 40;

(***********************************************************************
 * Locale constants
 ***********************************************************************)

(* Language codes (ISO 639).  The first 8 preserve the old values for the deprecated
LanguageType; the rest are sorted by the 2-character language code.

WARNING! Keep in sync with BOTH:
         1) LanguageCode array in OverlayMgr.c
         2) localeLanguage #define in UIResDefs.r
*)

// Leave the following line unchanged before 1st #define to be copied to rez section:
// TAG SOURCE START

const
  lEnglish       = LanguageType(0); // EN
  lFrench        = LanguageType(1); // FR
  lGerman        = LanguageType(2); // DE
  lItalian       = LanguageType(3); // IT
  lSpanish       = LanguageType(4); // ES
  lUnused        = LanguageType(5); // Reserved

// New in 3.1
  lJapanese      = LanguageType(6); // JA (Palm calls this jp)
  lDutch         = LanguageType(7); // NL

// New in 4.0
  lAfar          = LanguageType(8); // AA
  lAbkhazian     = LanguageType(9); // AB
  lAfrikaans     = LanguageType(10); // AF
  lAmharic       = LanguageType(11); // AM
  lArabic        = LanguageType(12); // AR
  lAssamese      = LanguageType(13); // AS
  lAymara        = LanguageType(14); // AY
  lAzerbaijani   = LanguageType(15); // AZ
  lBashkir       = LanguageType(16); // BA
  lByelorussian  = LanguageType(17); // BE
  lBulgarian     = LanguageType(18); // BG
  lBihari        = LanguageType(19); // BH
  lBislama       = LanguageType(20); // BI
  lBengali       = LanguageType(21); // BN (Bangla)
  lTibetan       = LanguageType(22); // BO
  lBreton        = LanguageType(23); // BR
  lCatalan       = LanguageType(24); // CA
  lCorsican      = LanguageType(25); // CO
  lCzech         = LanguageType(26); // CS
  lWelsh         = LanguageType(27); // CY
  lDanish        = LanguageType(28); // DA
  lBhutani       = LanguageType(29); // DZ
  lGreek         = LanguageType(30); // EL
  lEsperanto     = LanguageType(31); // EO
  lEstonian      = LanguageType(32); // ET
  lBasque        = LanguageType(33); // EU
  lPersian       = LanguageType(34); // FA (Farsi)
  lFinnish       = LanguageType(35); // FI
  lFiji          = LanguageType(36); // FJ
  lFaroese       = LanguageType(37); // FO
  lFrisian       = LanguageType(38); // FY
  lIrish         = LanguageType(39); // GA
  lScotsGaelic   = LanguageType(40); // GD
  lGalician      = LanguageType(41); // GL
  lGuarani       = LanguageType(42); // GN
  lGujarati      = LanguageType(43); // GU
  lHausa         = LanguageType(44); // HA
  lHindi         = LanguageType(45); // HI
  lCroatian      = LanguageType(46); // HR
  lHungarian     = LanguageType(47); // HU
  lArmenian      = LanguageType(48); // HY
  lInterlingua   = LanguageType(49); // IA
  lInterlingue   = LanguageType(50); // IE
  lInupiak       = LanguageType(51); // IK
  lIndonesian    = LanguageType(52); // IN
  lIcelandic     = LanguageType(53); // IS
  lHebrew        = LanguageType(54); // IW
  lYiddish       = LanguageType(55); // JI
  lJavanese      = LanguageType(56); // JW
  lGeorgian      = LanguageType(57); // KA
  lKazakh        = LanguageType(58); // KK
  lGreenlandic   = LanguageType(59); // KL
  lCambodian     = LanguageType(60); // KM
  lKannada       = LanguageType(61); // KN
  lKorean        = LanguageType(62); // KO
  lKashmiri      = LanguageType(63); // KS
  lKurdish       = LanguageType(64); // KU
  lKirghiz       = LanguageType(65); // KY
  lLatin         = LanguageType(66); // LA
  lLingala       = LanguageType(67); // LN
  lLaothian      = LanguageType(68); // LO
  lLithuanian    = LanguageType(69); // LT
  lLatvian       = LanguageType(70); // LV (Lettish)
  lMalagasy      = LanguageType(71); // MG
  lMaori         = LanguageType(72); // MI
  lMacedonian    = LanguageType(73); // MK
  lMalayalam     = LanguageType(74); // ML
  lMongolian     = LanguageType(75); // MN
  lMoldavian     = LanguageType(76); // MO
  lMarathi       = LanguageType(77); // MR
  lMalay         = LanguageType(78); // MS
  lMaltese       = LanguageType(79); // MT
  lBurmese       = LanguageType(80); // MY
  lNauru         = LanguageType(81); // NA
  lNepali        = LanguageType(82); // NE
  lNorwegian     = LanguageType(83); // NO
  lOccitan       = LanguageType(84); // OC
  lAfan          = LanguageType(85); // OM (Oromo)
  lOriya         = LanguageType(86); // OR
  lPunjabi       = LanguageType(87); // PA
  lPolish        = LanguageType(88); // PL
  lPashto        = LanguageType(89); // PS (Pushto)
  lPortuguese    = LanguageType(90); // PT
  lQuechua       = LanguageType(91); // QU
  lRhaetoRomance = LanguageType(92); // RM
  lKurundi       = LanguageType(93); // RN
  lRomanian      = LanguageType(94); // RO
  lRussian       = LanguageType(95); // RU
  lKinyarwanda   = LanguageType(96); // RW
  lSanskrit      = LanguageType(97); // SA
  lSindhi        = LanguageType(98); // SD
  lSangho        = LanguageType(99); // SG
  lSerboCroatian = LanguageType(100); // SH
  lSinghalese    = LanguageType(101); // SI
  lSlovak        = LanguageType(102); // SK
  lSlovenian     = LanguageType(103); // SL
  lSamoan        = LanguageType(104); // SM
  lShona         = LanguageType(105); // SN
  lSomali        = LanguageType(106); // SO
  lAlbanian      = LanguageType(107); // SQ
  lSerbian       = LanguageType(108); // SR
  lSiswati       = LanguageType(109); // SS
  lSesotho       = LanguageType(110); // ST
  lSudanese      = LanguageType(111); // SU
  lSwedish       = LanguageType(112); // SV
  lSwahili       = LanguageType(113); // SW
  lTamil         = LanguageType(114); // TA
  lTelugu        = LanguageType(115); // TE
  lTajik         = LanguageType(116); // TG
  lThai          = LanguageType(117); // TH
  lTigrinya      = LanguageType(118); // TI
  lTurkmen       = LanguageType(119); // TK
  lTagalog       = LanguageType(120); // TL
  lSetswana      = LanguageType(121); // TN
  lTonga         = LanguageType(122); // TO
  lTurkish       = LanguageType(123); // TR
  lTsonga        = LanguageType(124); // TS
  lTatar         = LanguageType(125); // TT
  lTwi           = LanguageType(126); // TW
  lUkrainian     = LanguageType(127); // UK
  lUrdu          = LanguageType(128); // UR
  lUzbek         = LanguageType(129); // UZ
  lVietnamese    = LanguageType(130); // VI
  lVolapuk       = LanguageType(131); // VO
  lWolof         = LanguageType(132); // WO
  lXhosa         = LanguageType(133); // XH
  lYoruba        = LanguageType(134); // YO
  lChinese       = LanguageType(135); // ZH
  lZulu          = LanguageType(136); // ZU

(* Country codes (ISO 3166).  The first 33 preserve the old values for the
deprecated CountryType; the rest are sorted by the 2-character country code.

WARNING! Keep in sync with BOTH:
         1) CountryCode array in OverlayMgr.c
         2) localeCountry #define in UIResDefs.r
*)

const
  cAustralia                              = CountryType(0);   // AU
  cAustria                                = CountryType(1);   // AT
  cBelgium                                = CountryType(2);   // BE
  cBrazil                                 = CountryType(3);   // BR
  cCanada                                 = CountryType(4);   // CA
  cDenmark                                = CountryType(5);   // DK
  cFinland                                = CountryType(6);   // FI
  cFrance                                 = CountryType(7);   // FR
  cGermany                                = CountryType(8);   // DE
  cHongKong                               = CountryType(9);   // HK
  cIceland                                = CountryType(10);  // IS
  cIreland                                = CountryType(11);  // IE
  cItaly                                  = CountryType(12);  // IT
  cJapan                                  = CountryType(13);  // JP
  cLuxembourg                             = CountryType(14);  // LU
  cMexico                                 = CountryType(15);  // MX
  cNetherlands                            = CountryType(16);  // NL
  cNewZealand                             = CountryType(17);  // NZ
  cNorway                                 = CountryType(18);  // NO
  cSpain                                  = CountryType(19);  // ES
  cSweden                                 = CountryType(20);  // SE
  cSwitzerland                            = CountryType(21);  // CH
  cUnitedKingdom                          = CountryType(22);  // GB (UK)
  cUnitedStates                           = CountryType(23);  // US
  cIndia                                  = CountryType(24);  // IN
  cIndonesia                              = CountryType(25);  // ID
  cRepublicOfKorea                        = CountryType(26);  // KR
  cMalaysia                               = CountryType(27);  // MY
  cChina                                  = CountryType(28);  // CN
  cPhilippines                            = CountryType(29);  // PH
  cSingapore                              = CountryType(30);  // SG
  cThailand                               = CountryType(31);  // TH
  cTaiwan                                 = CountryType(32);  // TW

// New in 4.0
  cAndorra                                = CountryType(33);  // AD
  cUnitedArabEmirates                     = CountryType(34);  // AE
  cAfghanistan                            = CountryType(35);  // AF
  cAntiguaAndBarbuda                      = CountryType(36);  // AG
  cAnguilla                               = CountryType(37);  // AI
  cAlbania                                = CountryType(38);  // AL
  cArmenia                                = CountryType(39);  // AM
  cNetherlandsAntilles                    = CountryType(40);  // AN
  cAngola                                 = CountryType(41);  // AO
  cAntarctica                             = CountryType(42);  // AQ
  cArgentina                              = CountryType(43);  // AR
  cAmericanSamoa                          = CountryType(44);  // AS
  cAruba                                  = CountryType(45);  // AW
  cAzerbaijan                             = CountryType(46);  // AZ
  cBosniaAndHerzegovina                   = CountryType(47);  // BA
  cBarbados                               = CountryType(48);  // BB
  cBangladesh                             = CountryType(49);  // BD
  cBurkinaFaso                            = CountryType(50);  // BF
  cBulgaria                               = CountryType(51);  // BG
  cBahrain                                = CountryType(52);  // BH
  cBurundi                                = CountryType(53);  // BI
  cBenin                                  = CountryType(54);  // BJ
  cBermuda                                = CountryType(55);  // BM
  cBruneiDarussalam                       = CountryType(56);  // BN
  cBolivia                                = CountryType(57);  // BO
  cBahamas                                = CountryType(58);  // BS
  cBhutan                                 = CountryType(59);  // BT
  cBouvetIsland                           = CountryType(60);  // BV
  cBotswana                               = CountryType(61);  // BW
  cBelarus                                = CountryType(62);  // BY
  cBelize                                 = CountryType(63);  // BZ
  cCocosIslands                           = CountryType(64);  // CC
  cDemocraticRepublicOfTheCongo           = CountryType(65);  // CD
  cCentralAfricanRepublic                 = CountryType(66);  // CF
  cCongo                                  = CountryType(67);  // CG
  cIvoryCoast                             = CountryType(68);  // CI
  cCookIslands                            = CountryType(69);  // CK
  cChile                                  = CountryType(70);  // CL
  cCameroon                               = CountryType(71);  // CM
  cColumbia                               = CountryType(72);  // CO
  cCostaRica                              = CountryType(73);  // CR
  cCuba                                   = CountryType(74);  // CU
  cCapeVerde                              = CountryType(75);  // CV
  cChristmasIsland                        = CountryType(76);  // CX
  cCyprus                                 = CountryType(77);  // CY
  cCzechRepublic                          = CountryType(78);  // CZ
  cDjibouti                               = CountryType(79);  // DJ
  cDominica                               = CountryType(80);  // DM
  cDominicanRepublic                      = CountryType(81);  // DO
  cAlgeria                                = CountryType(82);  // DZ
  cEcuador                                = CountryType(83);  // EC
  cEstonia                                = CountryType(84);  // EE
  cEgypt                                  = CountryType(85);  // EG
  cWesternSahara                          = CountryType(86);  // EH
  cEritrea                                = CountryType(87);  // ER
  cEthiopia                               = CountryType(88);  // ET
  cFiji                                   = CountryType(89);  // FJ
  cFalklandIslands                        = CountryType(90);  // FK
  cMicronesia                             = CountryType(91);  // FM
  cFaeroeIslands                          = CountryType(92);  // FO
  cMetropolitanFrance                     = CountryType(93);  // FX
  cGabon                                  = CountryType(94);  // GA
  cGrenada                                = CountryType(95);  // GD
  cGeorgia                                = CountryType(96);  // GE
  cFrenchGuiana                           = CountryType(97);  // GF
  cGhana                                  = CountryType(98);  // GH
  cGibraltar                              = CountryType(99);  // GI
  cGreenland                              = CountryType(100); // GL
  cGambia                                 = CountryType(101); // GM
  cGuinea                                 = CountryType(102); // GN
  cGuadeloupe                             = CountryType(103); // GP
  cEquatorialGuinea                       = CountryType(104); // GQ
  cGreece                                 = CountryType(105); // GR
  cSouthGeorgiaAndTheSouthSandwichIslands = CountryType(106); // GS
  cGuatemala                              = CountryType(107); // GT
  cGuam                                   = CountryType(108); // GU
  cGuineaBisseu                           = CountryType(109); // GW
  cGuyana                                 = CountryType(110); // GY
  cHeardAndMcDonaldIslands                = CountryType(111); // HM
  cHonduras                               = CountryType(112); // HN
  cCroatia                                = CountryType(113); // HR
  cHaiti                                  = CountryType(114); // HT
  cHungary                                = CountryType(115); // HU
  cIsrael                                 = CountryType(116); // IL
  cBritishIndianOceanTerritory            = CountryType(117); // IO
  cIraq                                   = CountryType(118); // IQ
  cIran                                   = CountryType(119); // IR
  cJamaica                                = CountryType(120); // JM
  cJordan                                 = CountryType(121); // JO
  cKenya                                  = CountryType(122); // KE
  cKyrgyzstan                             = CountryType(123); // KG (Kirgistan)
  cCambodia                               = CountryType(124); // KH
  cKiribati                               = CountryType(125); // KI
  cComoros                                = CountryType(126); // KM
  cStKittsAndNevis                        = CountryType(127); // KN
  cDemocraticPeoplesRepublicOfKorea       = CountryType(128); // KP
  cKuwait                                 = CountryType(129); // KW
  cCaymanIslands                          = CountryType(130); // KY
  cKazakhstan                             = CountryType(131); // KK
  cLaos                                   = CountryType(132); // LA
  cLebanon                                = CountryType(133); // LB
  cStLucia                                = CountryType(134); // LC
  cLiechtenstein                          = CountryType(135); // LI
  cSriLanka                               = CountryType(136); // LK
  cLiberia                                = CountryType(137); // LR
  cLesotho                                = CountryType(138); // LS
  cLithuania                              = CountryType(139); // LT
  cLatvia                                 = CountryType(140); // LV
  cLibya                                  = CountryType(141); // LY
  cMorrocco                               = CountryType(142); // MA
  cMonaco                                 = CountryType(143); // MC
  cMoldova                                = CountryType(144); // MD
  cMadagascar                             = CountryType(145); // MG
  cMarshallIslands                        = CountryType(146); // MH
  cMacedonia                              = CountryType(147); // MK
  cMali                                   = CountryType(148); // ML
  cMyanmar                                = CountryType(149); // MM
  cMongolia                               = CountryType(150); // MN
  cMacau                                  = CountryType(151); // MO
  cNorthernMarianaIslands                 = CountryType(152); // MP
  cMartinique                             = CountryType(153); // MQ
  cMauritania                             = CountryType(154); // MR
  cMontserrat                             = CountryType(155); // MS
  cMalta                                  = CountryType(156); // MT
  cMauritius                              = CountryType(157); // MU
  cMaldives                               = CountryType(158); // MV
  cMalawi                                 = CountryType(159); // MW
  cMozambique                             = CountryType(160); // MZ
  cNamibia                                = CountryType(161); // NA
  cNewCaledonia                           = CountryType(162); // NC
  cNiger                                  = CountryType(163); // NE
  cNorfolkIsland                          = CountryType(164); // NF
  cNigeria                                = CountryType(165); // NG
  cNicaragua                              = CountryType(166); // NI
  cNepal                                  = CountryType(167); // NP
  cNauru                                  = CountryType(168); // NR
  cNiue                                   = CountryType(169); // NU
  cOman                                   = CountryType(170); // OM
  cPanama                                 = CountryType(171); // PA
  cPeru                                   = CountryType(172); // PE
  cFrenchPolynesia                        = CountryType(173); // PF
  cPapuaNewGuinea                         = CountryType(174); // PG
  cPakistan                               = CountryType(175); // PK
  cPoland                                 = CountryType(176); // PL
  cStPierreAndMiquelon                    = CountryType(177); // PM
  cPitcairn                               = CountryType(178); // PN
  cPuertoRico                             = CountryType(179); // PR
  cPortugal                               = CountryType(180); // PT
  cPalau                                  = CountryType(181); // PW
  cParaguay                               = CountryType(182); // PY
  cQatar                                  = CountryType(183); // QA
  cReunion                                = CountryType(184); // RE
  cRomania                                = CountryType(185); // RO
  cRussianFederation                      = CountryType(186); // RU
  cRwanda                                 = CountryType(187); // RW
  cSaudiArabia                            = CountryType(188); // SA
  cSolomonIslands                         = CountryType(189); // SB
  cSeychelles                             = CountryType(190); // SC
  cSudan                                  = CountryType(191); // SD
  cStHelena                               = CountryType(192); // SH
  cSlovenia                               = CountryType(193); // SI
  cSvalbardAndJanMayenIslands             = CountryType(194); // SJ
  cSlovakia                               = CountryType(195); // SK
  cSierraLeone                            = CountryType(196); // SL
  cSanMarino                              = CountryType(197); // SM
  cSenegal                                = CountryType(198); // SN
  cSomalia                                = CountryType(199); // SO
  cSuriname                               = CountryType(200); // SR
  cSaoTomeAndPrincipe                     = CountryType(201); // ST
  cElSalvador                             = CountryType(202); // SV
  cSyranArabRepublic                      = CountryType(203); // SY
  cSwaziland                              = CountryType(204); // SZ
  cTurksAndCaicosIslands                  = CountryType(205); // TC
  cChad                                   = CountryType(206); // TD
  cFrenchSouthernTerritories              = CountryType(207); // TF
  cTogo                                   = CountryType(208); // TG
  cTajikistan                             = CountryType(209); // TJ
  cTokelau                                = CountryType(210); // TK
  cTurkmenistan                           = CountryType(211); // TM
  cTunisia                                = CountryType(212); // TN
  cTonga                                  = CountryType(213); // TO
  cEastTimor                              = CountryType(214); // TP
  cTurkey                                 = CountryType(215); // TR
  cTrinidadAndTobago                      = CountryType(216); // TT
  cTuvalu                                 = CountryType(217); // TV
  cTanzania                               = CountryType(218); // TZ
  cUkraine                                = CountryType(219); // UA
  cUganda                                 = CountryType(220); // UG
  cUnitedStatesMinorOutlyingIslands       = CountryType(221); // UM
  cUruguay                                = CountryType(222); // UY
  cUzbekistan                             = CountryType(223); // UZ
  cHolySee                                = CountryType(224); // VA
  cStVincentAndTheGrenadines              = CountryType(225); // VC
  cVenezuela                              = CountryType(226); // VE
  cBritishVirginIslands                   = CountryType(227); // VG
  cUSVirginIslands                        = CountryType(228); // VI
  cVietNam                                = CountryType(229); // VN
  cVanuatu                                = CountryType(230); // VU
  cWallisAndFutunaIslands                 = CountryType(231); // WF
  cSamoa                                  = CountryType(232); // WS
  cYemen                                  = CountryType(233); // YE
  cMayotte                                = CountryType(234); // YT
  cYugoslavia                             = CountryType(235); // YU
  cSouthAfrica                            = CountryType(236); // ZA
  cZambia                                 = CountryType(237); // ZM
  cZimbabwe                               = CountryType(238); // ZW

(* Various character encodings supported by the PalmOS. Actually these
are a mixture of character sets (repetoires or coded character sets
in Internet lingo) and character encodings (CES - character encoding
standard). Many, however, are some of both (e.g. CP932 is the Shift-JIS
encoding of the JIS character set + Microsoft's extensions).

The following character set values are used by:
   a) Palm devices
   b) Palm wireless servers

WARNING! Be aware that a device supporting a new character set
         will require some character set definition and maybe
         some development on the wireless server side.
*)

// Unknown to this version of PalmOS.
  charEncodingUnknown     = CharEncodingType(0);

// Maximum character encoding _currently_ defined
  charEncodingMax         = CharEncodingType(77);

// Latin Palm OS character encoding, and subsets.
// PalmOS version of CP1252
  charEncodingPalmLatin   = CharEncodingType(3);
// Windows variant of 8859-1
 charEncodingCP1252       = CharEncodingType(7);
// ISO 8859 Part 1
  charEncodingISO8859_1   = CharEncodingType(2);
// ISO 646-1991
  charEncodingAscii       = CharEncodingType(1);

// Japanese Palm OS character encoding, and subsets.
// PalmOS version of CP932
  charEncodingPalmSJIS    = CharEncodingType(5);
// Windows variant of ShiftJIS
  charEncodingCP932       = CharEncodingType(8);
// Encoding for JIS 0208-1990 + 1-byte katakana
  charEncodingShiftJIS    = CharEncodingType(4);

// Unicode character encodings
  charEncodingUCS2        = CharEncodingType(9);
  charEncodingUTF8        = CharEncodingType(6);
  charEncodingUTF7        = CharEncodingType(24);
  charEncodingUTF16       = CharEncodingType(75);
  charEncodingUTF16BE     = CharEncodingType(76);
  charEncodingUTF16LE     = CharEncodingType(77);

// Latin character encodings
  charEncodingCP850       = CharEncodingType(12);
  charEncodingCP437       = CharEncodingType(13);
  charEncodingCP865       = CharEncodingType(14);
  charEncodingCP860       = CharEncodingType(15);
  charEncodingCP861       = CharEncodingType(16);
  charEncodingCP863       = CharEncodingType(17);
  charEncodingCP775       = CharEncodingType(18);
  charEncodingMacIslande  = CharEncodingType(19);
  charEncodingMacintosh   = CharEncodingType(20);
  charEncodingCP1257      = CharEncodingType(21);
  charEncodingISO8859_3   = CharEncodingType(22);
  charEncodingISO8859_4   = CharEncodingType(23);

// Extended Latin character encodings
  charEncodingISO8859_2   = CharEncodingType(26);
  charEncodingCP1250      = CharEncodingType(27);
  charEncodingCP852       = CharEncodingType(28);
  charEncodingXKamenicky  = CharEncodingType(29);
  charEncodingMacXCroate  = CharEncodingType(30);
  charEncodingMacXLat2    = CharEncodingType(31);
  charEncodingMacXRomania = CharEncodingType(32);

// Japanese character encodings
  charEncodingEucJp       = CharEncodingType(25);
  charEncodingISO2022Jp   = CharEncodingType(10);
  charEncodingXAutoJp     = CharEncodingType(11);

// Greek character encodings
  charEncodingISO8859_7   = CharEncodingType(33);
  charEncodingCP1253      = CharEncodingType(34);
  charEncodingCP869       = CharEncodingType(35);
  charEncodingCP737       = CharEncodingType(36);
  charEncodingMacXGr      = CharEncodingType(37);

// Cyrillic character encodings
  charEncodingCP1251      = CharEncodingType(38);
  charEncodingISO8859_5   = CharEncodingType(39);
  charEncodingKoi8R       = CharEncodingType(40);
  charEncodingKoi8        = CharEncodingType(41);
  charEncodingCP855       = CharEncodingType(42);
  charEncodingCP866       = CharEncodingType(43);
  charEncodingMacCyr      = CharEncodingType(44);
  charEncodingMacUkraine  = CharEncodingType(45);

// Turkish character encodings
  charEncodingCP1254      = CharEncodingType(46);
  charEncodingISO8859_9   = CharEncodingType(47);
  charEncodingCP857       = CharEncodingType(48);
  charEncodingMacTurc     = CharEncodingType(49);
  charEncodingCP853       = CharEncodingType(50);

// Arabic character encodings
  charEncodingISO8859_6   = CharEncodingType(51);
  charEncodingAsmo708     = CharEncodingType(52);
  charEncodingCP1256      = CharEncodingType(53);
  charEncodingCP864       = CharEncodingType(54);
  charEncodingAsmo708Plus = CharEncodingType(55);
  charEncodingAsmo708Fr   = CharEncodingType(56);
  charEncodingMacAra      = CharEncodingType(57);

// Chinese character encodings
  charEncodingGB2312      = CharEncodingType(58);
  charEncodingHZ          = CharEncodingType(59);
  charEncodingBig5        = CharEncodingType(60);

// Vietnamese character encodings
  charEncodingViscii      = CharEncodingType(61);
  charEncodingViqr        = CharEncodingType(62);
  charEncodingVncii       = CharEncodingType(63);
  charEncodingVietnet     = CharEncodingType(65);
  charEncodingCP1258      = CharEncodingType(66);

// Korean character encodings
  charEncodingKsc5601     = CharEncodingType(67);
  charEncodingCP949       = CharEncodingType(68);
  charEncodingISO2022Kr   = CharEncodingType(69);

// Hebrew character encodings
  charEncodingISO8859_8I  = CharEncodingType(70);
  charEncodingISO8859_8   = CharEncodingType(71);
  charEncodingCP1255      = CharEncodingType(72);
  charEncodingCP1255V     = CharEncodingType(73);

// Thai character encodings
  charEncodingTis620      = CharEncodingType(74);
  charEncodingCP874       = CharEncodingType(64);

implementation

end.
