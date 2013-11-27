{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2013 by the Free Pascal development team.

    String code page support functions for unix styled systems.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 **********************************************************************}

{$mode objfpc}
unit unixcp;

interface

uses baseunix;

{ source: http://win-iconv.googlecode.com/svn-history/r6/trunk/win_iconv.c
  public domain
}

type
  TUnixCpData = record
   cp: word;
   name: rawbytestring; { for null-termination }
  end;
(*
* Code Page Identifiers
* http://msdn2.microsoft.com/en-us/library/ms776446.aspx
*)
const
  UnixCpMapLimit = 406{$ifndef aix}-83{$endif}{$ifdef ACCEPT_646}+1{$endif};
  UnixCpMap: array[-1..UnixCpMapLimit] of TUnixCpData =
  ((cp:0; name: 'UTF-8'), { invalid/unknown -> utf-8 }
   (cp:37; name:'IBM037'), (* IBM EBCDIC US-Canada *)
   (cp:37; name:'IBM-037'), (* IBM EBCDIC US-Canada, AIX *)
   (cp:154; name:'CP154'),
   (cp:154; name:'CYRILLIC-ASIAN'),
   (cp:154; name:'PT154'),
   (cp:154; name:'PTCP154'),
   (cp:154; name:'CSPTCP154'),
   (cp:437; name:'437'),
   (cp:437; name:'CP437'),
   (cp:437; name:'IBM-437'),
   (cp:437; name:'CSPC8CODEPAGE437'),
   (cp:437; name:'IBM437'), (* OEM United States *)
   (cp:500; name:'IBM500'), (* IBM EBCDIC International *)
   (cp:500; name:'IBM-500'), (* IBM EBCDIC International, AIX *)
   (cp:708; name:'ASMO-708'), (* Arabic (ASMO 708) *)
   (cp:720; name:'DOS-720'), (* Arabic (Transparent ASMO); Arabic (DOS) *)
   (cp:737; name:'CP737'),
   (cp:737; name:'ibm737'), (* OEM Greek (formerly 437G); Greek (DOS) *)
   (cp:775; name:'CP775'),
   (cp:775; name:'IBM775'),
   (cp:775; name:'CSPC775BALTIC'),
   (cp:775; name:'ibm775'), (* OEM Baltic; Baltic (DOS) *)
   (cp:850; name:'850'),
  {$ifdef aix}
   (cp:850; name:'IBM-850'), (* AIX *)
  {$endif}
   (cp:850; name:'CP850'),
   (cp:850; name:'IBM850'),
   (cp:850; name:'CSPC850MULTILINGUAL'),
   (cp:850; name:'ibm850'), (* OEM Multilingual Latin 1; Western European (DOS) *)
   (cp:852; name:'852'),
  {$ifdef aix}
   (cp:852; name:'IBM-852'), (* AIX *)
  {$endif}
   (cp:852; name:'CP852'),
   (cp:852; name:'IBM852'),
   (cp:852; name:'CSPCP852'),
   (cp:852; name:'ibm852'), (* OEM Latin 2; Central European (DOS) *)
   (cp:853; name:'CP853'),
   (cp:855; name:'855'),
  {$ifdef aix}
   (cp:855; name:'IBM-855'), (* AIX *)
  {$endif}
   (cp:855; name:'CP855'),
   (cp:855; name:'IBM855'),
   (cp:855; name:'CSIBM855'),
   (cp:855; name:'IBM855'), (* OEM Cyrillic (primarily Russian) *)
   (cp:857; name:'857'),
  {$ifdef aix}
   (cp:857; name:'IBM-857'), (* AIX *)
  {$endif}
   (cp:857; name:'CP857'),
   (cp:857; name:'IBM857'),
   (cp:857; name:'CSIBM857'),
   (cp:857; name:'ibm857'), (* OEM Turkish; Turkish (DOS) *)
   (cp:858; name:'CP858'),
   (cp:858; name:'IBM00858'), (* OEM Multilingual Latin 1 + Euro symbol *)
   (cp:860; name:'860'),
  {$ifdef aix}
   (cp:860; name:'IBM-860'), (* AIX *)
  {$endif}
   (cp:860; name:'CP860'),
   (cp:860; name:'IBM860'),
   (cp:860; name:'CSIBM860'),
   (cp:860; name:'IBM860'), (* OEM Portuguese; Portuguese (DOS) *)
   (cp:861; name:'861'),
  {$ifdef aix}
   (cp:861; name:'IBM-861'), (* AIX *)
  {$endif}
   (cp:861; name:'CP-IS'),
   (cp:861; name:'CP861'),
   (cp:861; name:'IBM861'),
   (cp:861; name:'CSIBM861'),
   (cp:861; name:'ibm861'), (* OEM Icelandic; Icelandic (DOS) *)
   (cp:862; name:'862'),
  {$ifdef aix}
   (cp:862; name:'IBM-862'), (* AIX *)
  {$endif}
   (cp:862; name:'CP862'),
   (cp:862; name:'IBM862'),
   (cp:862; name:'CSPC862LATINHEBREW'),
   (cp:862; name:'DOS-862'), (* OEM Hebrew; Hebrew (DOS) *)
   (cp:863; name:'863'),
   (cp:863; name:'CP863'),
  {$ifdef aix}
   (cp:863; name:'IBM-863'), (* AIX *)
  {$endif}
   (cp:863; name:'CSIBM863'),
   (cp:863; name:'IBM863'), (* OEM French Canadian; French Canadian (DOS) *)
   (cp:864; name:'CP864'),
  {$ifdef aix}
   (cp:864; name:'IBM-864'), (* AIX *)
  {$endif}
   (cp:864; name:'CSIBM864'),
   (cp:864; name:'IBM864'), (* OEM Arabic; Arabic (864) *)
   (cp:865; name:'865'),
   (cp:865; name:'IBM-865'), (*AIX *)
   (cp:865; name:'CP865'),
   (cp:865; name:'CSIBM865'),
   (cp:865; name:'IBM865'), (* OEM Nordic; Nordic (DOS) *)
   (cp:866; name:'866'),
  {$ifdef aix}
   (cp:866; name:'IBM-866'), (* AIX *)
  {$endif}
   (cp:866; name:'CP866'),
   (cp:866; name:'IBM866'),
   (cp:866; name:'CSIBM866'),
   (cp:866; name:'cp866'), (* OEM Russian; Cyrillic (DOS) *)
   (cp:869; name:'869'),
   (cp:869; name:'IBM-869'),
   (cp:869; name:'CP-GR'),
   (cp:869; name:'CP869'),
   (cp:869; name:'IBM869'),
   (cp:869; name:'CSIBM869'),
   (cp:869; name:'ibm869'), (* OEM Modern Greek; Greek, Modern (DOS) *)
   (cp:870; name:'IBM870'), (* IBM EBCDIC Multilingual/ROECE (Latin 2); IBM EBCDIC Multilingual Latin 2 *)
  {$ifdef aix}
   (cp:870; name:'IBM-870'), (* AIX *)
  {$endif}
   (cp:874; name:'CP874'),
   (cp:874; name:'WINDOWS-874'),
   (cp:874; name:'windows-874'), (* ANSI/OEM Thai (same as 28605, ISO 8859-15); Thai (Windows) *)
   (cp:875; name:'cp875'), (* IBM EBCDIC Greek Modern *)
   (cp:932; name:'CP932'),
   (cp:932; name:'IBM-943'), (* AIX -- note: IBM-943 = MS 932; IBM-932 is something different*)
   (cp:932; name:'MS932'),
   (cp:932; name:'SHIFFT_JIS'),
   (cp:932; name:'SHIFFT_JIS-MS'),
   (cp:932; name:'SJIS'),
   (cp:932; name:'SJIS-MS'),
   (cp:932; name:'SJIS-OPEN'),
   (cp:932; name:'SJIS-WIN'),
   (cp:932; name:'WINDOWS-31J'),
   (cp:932; name:'WINDOWS-932'),
   (cp:932; name:'CSWINDOWS31J'),
   (cp:932; name:'shift_jis'), (* ANSI/OEM Japanese; Japanese (Shift-JIS) *)
   (cp:932; name:'shift-jis'), (* alternative name for it *)
   (cp:936; name:'CP936'),
  {$ifdef aix}
   (cp:936; name:'IBM-eucCN'), (* AIX *)
  {$endif}
   (cp:936; name:'GBK'),
   (cp:936; name:'MS936'),
   (cp:936; name:'WINDOWS-936'),
   (cp:936; name:'gb2312'), (* ANSI/OEM Simplified Chinese (PRC, Singapore); Chinese Simplified (GB2312) *)
   (cp:949; name:'CP949'),
  {$ifdef aix}
   (cp:949; name:'IBM-eucKR'), (* AIX *)
  {$endif}
   (cp:949; name:'UHC'),
   (cp:949; name:'EUC-KR'),
   (cp:949; name:'ks_c_5601-1987'), (* ANSI/OEM Korean (Unified Hangul Code) *)
   (cp:950; name:'CP950'),
   (cp:950; name:'BIG5'),
   (cp:950; name:'big5'), (* ANSI/OEM Traditional Chinese (Taiwan; Hong Kong SAR, PRC); Chinese Traditional (Big5) *)
   (cp:1026; name:'IBM1026'), (* IBM EBCDIC Turkish (Latin 5) *)
  {$ifdef aix}
   (cp:1026; name:'IBM-1026'), (* AIX *)
  {$endif}
   (cp:1047; name:'IBM01047'), (* IBM EBCDIC Latin 1/Open System *)
  {$ifdef aix}
   (cp:1047; name:'IBM-1047'), (* AIX *)
  {$endif}
   (cp:1125; name:'CP1125'),
   (cp:1125; name:'IBM-1125'),
   (cp:1133; name:'CP1133'),
   (cp:1133; name:'IBM-1133'),
   (cp:1133; name:'IBM-CP1133'),
   (cp:1140; name:'IBM01140'), (* IBM EBCDIC US-Canada (037 + Euro symbol); IBM EBCDIC (US-Canada-Euro) *)
  {$ifdef aix}
   (cp:1140; name:'IBM-1140'), (* AIX *)
  {$endif}
   (cp:1141; name:'IBM01141'), (* IBM EBCDIC Germany (20273 + Euro symbol); IBM EBCDIC (Germany-Euro) *)
  {$ifdef aix}
   (cp:1141; name:'IBM-1141'), (* AIX *)
  {$endif}
   (cp:1142; name:'IBM01142'), (* IBM EBCDIC Denmark-Norway (20277 + Euro symbol); IBM EBCDIC (Denmark-Norway-Euro) *)
  {$ifdef aix}
   (cp:1142; name:'IBM-1142'), (* AIX *)
  {$endif}
   (cp:1143; name:'IBM01143'), (* IBM EBCDIC Finland-Sweden (20278 + Euro symbol); IBM EBCDIC (Finland-Sweden-Euro) *)
  {$ifdef aix}
   (cp:1143; name:'IBM-1143'), (* AIX *)
  {$endif}
   (cp:1144; name:'IBM01144'), (* IBM EBCDIC Italy (20280 + Euro symbol); IBM EBCDIC (Italy-Euro) *)
  {$ifdef aix}
   (cp:1144; name:'IBM-1144'), (* AIX *)
  {$endif}
   (cp:1145; name:'IBM01145'), (* IBM EBCDIC Latin America-Spain (20284 + Euro symbol); IBM EBCDIC (Spain-Euro) *)
  {$ifdef aix}
   (cp:1145; name:'IBM-1145'), (* AIX *)
  {$endif}
   (cp:1146; name:'IBM01146'), (* IBM EBCDIC United Kingdom (20285 + Euro symbol); IBM EBCDIC (UK-Euro) *)
  {$ifdef aix}
   (cp:1146; name:'IBM-1146'), (* AIX *)
  {$endif}
   (cp:1147; name:'IBM01147'), (* IBM EBCDIC France (20297 + Euro symbol); IBM EBCDIC (France-Euro) *)
  {$ifdef aix}
   (cp:1147; name:'IBM-1147'), (* AIX *)
  {$endif}
   (cp:1148; name:'IBM01148'), (* IBM EBCDIC International (500 + Euro symbol); IBM EBCDIC (International-Euro) *)
  {$ifdef aix}
   (cp:1148; name:'IBM-1148'), (* AIX *)
  {$endif}
   (cp:1149; name:'IBM01149'), (* IBM EBCDIC Icelandic (20871 + Euro symbol); IBM EBCDIC (Icelandic-Euro) *)
  {$ifdef aix}
   (cp:1149; name:'IBM-1149'), (* AIX *)
  {$endif}
   (cp:1200; name:'UTF-16LE'),
  {$ifdef aix}
   (cp:1200; name:'UTF-16le'), (* AIX *)
  {$endif}
   (cp:1200; name:'UTF16LE'),
   (cp:1200; name:'UCS-2LE'),
   (cp:1200; name:'CP1200'),
{$ifdef FPC_LITTLE_ENDIAN}
   (* Default is little endian, because the platform is *)
   (cp:1200; name:'UTF16'),
   (cp:1200; name:'UTF-16'),
   (cp:1200; name:'UCS-2'),
{$endif}
   (cp:1201; name:'UTF-16BE'),
   (cp:1201; name:'UTF16BE'),
   (cp:1201; name:'UCS-2BE'),
   (cp:1201; name:'unicodeFFFE'),
   (cp:1201; name:'CP1201'),
{$ifdef FPC_BIG_ENDIAN}
   (*
    * Default is big endian.
    * See rfc2781 4.3 Interpreting text labelled as UTF-16.
    *)
   (cp:1201; name:'UTF16'),
   (cp:1201; name:'UTF-16'),
   (cp:1201; name:'UCS-2'),
{$endif}
   (cp:1250; name:'CP1250'),
   (cp:1250; name:'MS-EE'),
   (cp:1250; name:'WINDOWS-1250'),
   (cp:1250; name:'windows-1250'), (* ANSI Central European; Central European (Windows) *)
   (cp:1251; name:'CP1251'),
   (cp:1251; name:'MS-CYRL'),
   (cp:1251; name:'WINDOWS-1251'),
   (cp:1251; name:'windows-1251'), (* ANSI Cyrillic; Cyrillic (Windows) *)
   (cp:1252; name:'CP819'),
   (cp:1252; name:'IBM819'),
   (cp:1252; name:'CP1252'),
   (cp:1252; name:'MS-ANSI'),
   (cp:1252; name:'WINDOWS-1252'),
   (cp:1252; name:'windows-1252'), (* ANSI Latin 1; Western European (Windows) *)
   (cp:1253; name:'CP1253'),
   (cp:1253; name:'MS-GREEK'),
   (cp:1253; name:'WINDOWS-1253'),
   (cp:1253; name:'windows-1253'), (* ANSI Greek; Greek (Windows) *)
   (cp:1254; name:'CP1254'),
   (cp:1254; name:'MS-TURK'),
   (cp:1254; name:'WINDOWS-1254'),
   (cp:1254; name:'windows-1254'), (* ANSI Turkish; Turkish (Windows) *)
   (cp:1255; name:'CP1255'),
   (cp:1255; name:'MS-HEBR'),
   (cp:1255; name:'WINDOWS-1255'),
   (cp:1255; name:'windows-1255'), (* ANSI Hebrew; Hebrew (Windows) *)
   (cp:1256; name:'CP1256'),
   (cp:1256; name:'MS-ARAB'),
   (cp:1256; name:'WINDOWS-1256'),
   (cp:1256; name:'windows-1256'), (* ANSI Arabic; Arabic (Windows) *)
   (cp:1257; name:'CP1257'),
   (cp:1257; name:'WINBALTRIM'),
   (cp:1257; name:'WINDOWS-1257'),
   (cp:1257; name:'windows-1257'), (* ANSI Baltic; Baltic (Windows) *)
   (cp:1258; name:'CP1258'),
   (cp:1258; name:'WINDOWS-1258'),
   (cp:1258; name:'windows-1258'), (* ANSI/OEM Vietnamese; Vietnamese (Windows) *)
   (cp:1361; name:'CP1361'),
   (cp:1361; name:'JOHAB'),
   (cp:1361; name:'Johab'), (* Korean (Johab) *)
   (cp:10000; name:'macintosh'), (* MAC Roman; Western European (Mac) *)
   (cp:10001; name:'x-mac-japanese'), (* Japanese (Mac) *)
   (cp:10002; name:'x-mac-chinesetrad'), (* MAC Traditional Chinese (Big5); Chinese Traditional (Mac) *)
   (cp:10003; name:'x-mac-korean'), (* Korean (Mac) *)
   (cp:10004; name:'x-mac-arabic'), (* Arabic (Mac) *)
   (cp:10005; name:'x-mac-hebrew'), (* Hebrew (Mac) *)
   (cp:10006; name:'x-mac-greek'), (* Greek (Mac) *)
   (cp:10007; name:'x-mac-cyrillic'), (* Cyrillic (Mac) *)
   (cp:10008; name:'x-mac-chinesesimp'), (* MAC Simplified Chinese (GB 2312); Chinese Simplified (Mac) *)
   (cp:10010; name:'x-mac-romanian'), (* Romanian (Mac) *)
   (cp:10017; name:'x-mac-ukrainian'), (* Ukrainian (Mac) *)
   (cp:10021; name:'x-mac-thai'), (* Thai (Mac) *)
   (cp:10029; name:'x-mac-ce'), (* MAC Latin 2; Central European (Mac) *)
   (cp:10079; name:'x-mac-icelandic'), (* Icelandic (Mac) *)
   (cp:10081; name:'x-mac-turkish'), (* Turkish (Mac) *)
   (cp:10082; name:'x-mac-croatian'), (* Croatian (Mac) *)
   (cp:12000; name:'UTF-32LE'),
   (cp:12000; name:'CP12000'),
   (cp:12000; name:'UTF32LE'),
{$ifdef FPC_LITTLE_ENDIAN}
   (cp:12000; name:'UTF32'),
   (cp:12000; name:'UTF-32'),
{$endif}
   (cp:12001; name:'UTF-32BE'),
   (cp:12001; name:'CP12001'),
   (cp:12001; name:'UTF32BE'),
{$ifdef FPC_BIG_ENDIAN}
   (cp:12001; name:'UTF32'),
   (cp:12001; name:'UTF-32'),
{$endif}
   (cp:20000; name:'x-Chinese_CNS'), (* CNS Taiwan; Chinese Traditional (CNS) *)
   (cp:20001; name:'x-cp20001'), (* TCA Taiwan *)
   (cp:20002; name:'x_Chinese-Eten'), (* Eten Taiwan; Chinese Traditional (Eten) *)
   (cp:20003; name:'x-cp20003'), (* IBM5550 Taiwan *)
   (cp:20004; name:'x-cp20004'), (* TeleText Taiwan *)
   (cp:20005; name:'x-cp20005'), (* Wang Taiwan *)
   (cp:20105; name:'x-IA5'), (* IA5 (IRV International Alphabet No. 5, 7-bit); Western European (IA5) *)
   (cp:20106; name:'x-IA5-German'), (* IA5 German (7-bit) *)
   (cp:20107; name:'x-IA5-Swedish'), (* IA5 Swedish (7-bit) *)
   (cp:20108; name:'x-IA5-Norwegian'), (* IA5 Norwegian (7-bit) *)
   (cp:20127; name:'US-ASCII'),
   (cp:20127; name:'ASCII'),
  {$ifdef aix}
   (cp:20127; name:'ASCII-GR'), (* AIX *)
  {$endif}
   (cp:20127; name:'ANSI_X3.4-1968'),
   (cp:20127; name:'ANSI_X3.4-1986'),
   (cp:20127; name:'CP367'),
  {$ifdef aix}
   (cp:20127; name:'IBM-367'), (* AIX *)
  {$endif}
   (cp:20127; name:'IBM367'),
   (cp:20127; name:'ISO-IR-6'),
{$ifdef ACCEPT_646}
   (cp:20127; name:'646'),
{$endif ACCEPT_646}
   (cp:20127; name:'ISO646-US'),
   (cp:20127; name:'ISO_646.IRV:1991'),
   (cp:20127; name:'US'),
   (cp:20127; name:'CSASCII'),
   (cp:20127; name:'us-ascii'), (* US-ASCII (7-bit) *)
   (cp:20261; name:'x-cp20261'), (* T.61 *)
   (cp:20269; name:'x-cp20269'), (* ISO 6937 Non-Spacing Accent *)
   (cp:20273; name:'IBM273'), (* IBM EBCDIC Germany *)
  {$ifdef aix}
   (cp:20273; name:'IBM-273'), (* AIX *)
  {$endif}
   (cp:20277; name:'IBM277'), (* IBM EBCDIC Denmark-Norway *)
  {$ifdef aix}
   (cp:20277; name:'IBM-277'), (* AIX *)
  {$endif}
   (cp:20278; name:'IBM278'), (* IBM EBCDIC Finland-Sweden *)
  {$ifdef aix}
   (cp:20278; name:'IBM-278'), (* AIX *)
  {$endif}
   (cp:20280; name:'IBM280'), (* IBM EBCDIC Italy *)
  {$ifdef aix}
   (cp:20280; name:'IBM-280'), (* AIX *)
  {$endif}
   (cp:20284; name:'IBM284'), (* IBM EBCDIC Latin America-Spain *)
  {$ifdef aix}
   (cp:20284; name:'IBM-284'), (* AIX *)
  {$endif}
   (cp:20285; name:'IBM285'), (* IBM EBCDIC United Kingdom *)
  {$ifdef aix}
   (cp:20285; name:'IBM-285'), (* AIX *)
  {$endif}
   (cp:20290; name:'IBM290'), (* IBM EBCDIC Japanese Katakana Extended *)
  {$ifdef aix}
   (cp:20290; name:'IBM-290'), (* AIX *)
  {$endif}
   (cp:20297; name:'IBM297'), (* IBM EBCDIC France *)
  {$ifdef aix}
   (cp:20297; name:'IBM-297'), (* AIX *)
  {$endif}
   (cp:20420; name:'IBM420'), (* IBM EBCDIC Arabic *)
  {$ifdef aix}
   (cp:20420; name:'IBM-420'), (* AIX *)
  {$endif}
   (cp:20423; name:'IBM423'), (* IBM EBCDIC Greek *)
  {$ifdef aix}
   (cp:20423; name:'IBM-423'), (* AIX *)
  {$endif}
   (cp:20424; name:'IBM424'), (* IBM EBCDIC Hebrew *)
  {$ifdef aix}
   (cp:20424; name:'IBM-424'), (* AIX *)
  {$endif}
   (cp:20833; name:'x-EBCDIC-KoreanExtended'), (* IBM EBCDIC Korean Extended *)
   (cp:20838; name:'IBM-Thai'), (* IBM EBCDIC Thai *)
  {$ifdef aix}
   (cp:20838; name:'TIS-620'),  (* AIX *)
  {$endif}
   (cp:20866; name:'koi8-r'), (* Russian (KOI8-R); Cyrillic (KOI8-R) *)
   (cp:20871; name:'IBM871'), (* IBM EBCDIC Icelandic *)
  {$ifdef aix}
   (cp:20871; name:'IBM-871'), (* AIX *)
  {$endif}
   (cp:20880; name:'IBM880'), (* IBM EBCDIC Cyrillic Russian *)
  {$ifdef aix}
   (cp:20880; name:'IBM-880'), (* AIX *)
  {$endif}
   (cp:20905; name:'IBM905'), (* IBM EBCDIC Turkish *)
  {$ifdef aix}
   (cp:20905; name:'IBM-905'), (* AIX *)
  {$endif}
   (cp:20924; name:'IBM00924'), (* IBM EBCDIC Latin 1/Open System (1047 + Euro symbol) *)
  {$ifdef aix}
   (cp:20924; name:'IBM-924'), (* AIX *)
  {$endif}
   (cp:20932; name:'EUC-JP'), (* Japanese (JIS 0208-1990 and 0121-1990) *)
  {$ifdef aix}
   (cp:20932; name:'IBM-eucJP'), (* AIX *)
  {$endif}
   (cp:20936; name:'x-cp20936'), (* Simplified Chinese (GB2312); Chinese Simplified (GB2312-80) *)
  {$ifdef aix}
   (cp:20936; name:'GB2312.1980-0'), (* AIX *)
  {$endif}
  {$ifdef aix}
   (cp:20936; name:'GB2312.1980-0-GL'), (* AIX *)
  {$endif}
  {$ifdef aix}
   (cp:20936; name:'GB2312.1980-0-GR'), (* AIX *)
  {$endif}
   (cp:20949; name:'x-cp20949'), (* Korean Wansung *)
   (cp:21025; name:'cp1025'), (* IBM EBCDIC Cyrillic Serbian-Bulgarian *)
  {$ifdef aix}
   (cp:21025; name:'IBM-1025'), (* AIX *)
  {$endif}
   (cp:21866; name:'koi8-u'), (* Ukrainian (KOI8-U); Cyrillic (KOI8-U) *)
  {$ifdef aix}
   (cp:21866; name:'IBM-1124'), (* AIX *)
  {$endif}
   (cp:28591; name:'ISO-8859-1'),
   (cp:28591; name:'ISO-IR-100'),
   (cp:28591; name:'ISO8859-1'),
   (cp:28591; name:'ISO_8859-1'),
   (cp:28591; name:'ISO_8859-1:1987'),
   (cp:28591; name:'L1'),
   (cp:28591; name:'LATIN1'),
   (cp:28591; name:'CSISOLATIN1'),
   (cp:28591; name:'iso-8859-1'), (* ISO 8859-1 Latin 1; Western European (ISO) *)
  {$ifdef aix}
   (cp:28591; name:'ISO8859-1'), (* AIX *)
  {$endif}
  {$ifdef aix}
   (cp:28591; name:'ISO8859-1-GL'), (* AIX *)
  {$endif}
  {$ifdef aix}
   (cp:28591; name:'ISO8859-1-GT'), (* AIX *)
  {$endif}
   (cp:28591; name:'iso8859-1'), (* ISO 8859-1 Latin 1; Western European (ISO) *)
   (cp:28592; name:'iso-8859-2'), (* ISO 8859-2 Central European; Central European (ISO) *)
  {$ifdef aix}
   (cp:28592; name:'ISO8859-2'), (* AIX *)
  {$endif}
  {$ifdef aix}
   (cp:28592; name:'ISO8859-2-GL'), (* AIX *)
  {$endif}
  {$ifdef aix}
   (cp:28593; name:'ISO8859-2-GT'), (* AIX *)
  {$endif}
   (cp:28592; name:'iso8859-2'), (* ISO 8859-2 Central European; Central European (ISO) *)
   (cp:28593; name:'iso-8859-3'), (* ISO 8859-3 Latin 3 *)
  {$ifdef aix}
   (cp:28593; name:'ISO8859-3'), (* AIX *)
  {$endif}
  {$ifdef aix}
   (cp:28593; name:'ISO8859-3-GL'), (* AIX *)
  {$endif}
  {$ifdef aix}
   (cp:28593; name:'ISO8859-3-GT'), (* AIX *)
  {$endif}
   (cp:28593; name:'iso8859-3'), (* ISO 8859-3 Latin 3 *)
   (cp:28594; name:'iso-8859-4'), (* ISO 8859-4 Baltic *)
  {$ifdef aix}
   (cp:28594; name:'ISO8859-4'), (* AIX *)
  {$endif}
  {$ifdef aix}
   (cp:28594; name:'ISO8859-4-GL'), (* AIX *)
  {$endif}
  {$ifdef aix}
   (cp:28594; name:'ISO8859-4-GT'), (* AIX *)
  {$endif}
   (cp:28594; name:'iso8859-4'), (* ISO 8859-4 Baltic *)
   (cp:28595; name:'iso-8859-5'), (* ISO 8859-5 Cyrillic *)
  {$ifdef aix}
   (cp:28595; name:'ISO8859-5'), (* AIX *)
  {$endif}
  {$ifdef aix}
   (cp:28595; name:'ISO8859-5-GL'), (* AIX *)
  {$endif}
  {$ifdef aix}
   (cp:28595; name:'ISO-8859-5-GT'), (* AIX *)
  {$endif}
   (cp:28595; name:'iso8859-5'), (* ISO 8859-5 Cyrillic *)
   (cp:28596; name:'iso-8859-6'), (* ISO 8859-6 Arabic *)
  {$ifdef aix}
   (cp:28596; name:'ISO8859-6'), (* AIX *)
  {$endif}
  {$ifdef aix}
   (cp:28596; name:'ISO-8859-6-GL'), (* AIX *)
  {$endif}
  {$ifdef aix}
   (cp:28596; name:'ISO-8859-6-GT'), (* AIX *)
  {$endif}
   (cp:28596; name:'iso8859-6'), (* ISO 8859-6 Arabic *)
   (cp:28597; name:'iso-8859-7'), (* ISO 8859-7 Greek *)
  {$ifdef aix}
   (cp:28597; name:'ISO8859-7'), (* AIX *)
  {$endif}
  {$ifdef aix}
   (cp:28597; name:'ISO-8859-7-GL'), (* AIX *)
  {$endif}
  {$ifdef aix}
   (cp:28597; name:'ISO8859-7-GT'), (* AIX *)
  {$endif}
   (cp:28597; name:'iso8859-7'), (* ISO 8859-7 Greek *)
   (cp:28598; name:'iso-8859-8'), (* ISO 8859-8 Hebrew; Hebrew (ISO-Visual) *)
  {$ifdef aix}
   (cp:28598; name:'ISO8859-8'), (* AIX *)
  {$endif}
  {$ifdef aix}
   (cp:28598; name:'ISO8859-8-GL'), (* AIX *)
  {$endif}
  {$ifdef aix}
   (cp:28598; name:'ISO8859-8-GT'), (* AIX *)
  {$endif}
   (cp:28598; name:'iso8859-8'), (* ISO 8859-8 Hebrew; Hebrew (ISO-Visual) *)
   (cp:28599; name:'iso-8859-9'), (* ISO 8859-9 Turkish *)
  {$ifdef aix}
   (cp:28599; name:'ISO8859-9'), (* AIX *)
  {$endif}
  {$ifdef aix}
   (cp:28599; name:'ISO8859-9-GL'), (* AIX *)
  {$endif}
  {$ifdef aix}
   (cp:28599; name:'ISO8859-9-GT'), (* AIX *)
  {$endif}
   (cp:28599; name:'iso8859-9'), (* ISO 8859-9 Turkish *)
   (cp:28603; name:'iso-8859-13'), (* ISO 8859-13 Estonian *)
  {$ifdef aix}
   (cp:28603; name:'ISO8859-13'), (* AIX *)
  {$endif}
  {$ifdef aix}
   (cp:28603; name:'ISO8859-13-GL'), (* AIX *)
  {$endif}
  {$ifdef aix}
   (cp:28603; name:'ISO8859-13-GT'), (* AIX *)
  {$endif}
   (cp:28603; name:'iso8859-13'), (* ISO 8859-13 Estonian *)
   (cp:28605; name:'iso-8859-15'), (* ISO 8859-15 Latin 9 *)
  {$ifdef aix}
   (cp:28605; name:'ISO8859-15'), (* AIX *)
  {$endif}
  {$ifdef aix}
   (cp:28605; name:'ISO8859-15-GL'), (* AIX *)
  {$endif}
  {$ifdef aix}
   (cp:28605; name:'ISO8859-15-GT'), (* AIX *)
  {$endif}
   (cp:28605; name:'iso8859-15'), (* ISO 8859-15 Latin 9 *)
   (cp:29001; name:'x-Europa'), (* Europa 3 *)
   (cp:38598; name:'iso-8859-8-i'), (* ISO 8859-8 Hebrew; Hebrew (ISO-Logical) *)
   (cp:38598; name:'iso8859-8-i'), (* ISO 8859-8 Hebrew; Hebrew (ISO-Logical) *)
   (cp:50220; name:'iso-2022-jp'), (* ISO 2022 Japanese with no halfwidth Katakana; Japanese (JIS) *)
   (cp:50221; name:'ISO-2022-JP'),
   (cp:50221; name:'CP50221'),
   (cp:50221; name:'ISO-2022-JP-MS'),
   (cp:50221; name:'ISO2022-JP'),
   (cp:50221; name:'ISO2022-JP-MS'),
   (cp:50221; name:'MS50221'),
   (cp:50221; name:'WINDOWS-50221'),
   (cp:50221; name:'csISO2022JP'), (* ISO 2022 Japanese with halfwidth Katakana; Japanese (JIS-Allow 1 byte Kana) *)
   (cp:50222; name:'iso-2022-jp'), (* ISO 2022 Japanese JIS X 0201-1989; Japanese (JIS-Allow 1 byte Kana - SO/SI) *)
   (cp:50225; name:'iso-2022-kr'), (* ISO 2022 Korean *)
   (cp:50225; name:'iso2022-kr'), (* ISO 2022 Korean *)
   (cp:50227; name:'x-cp50227'), (* ISO 2022 Simplified Chinese; Chinese Simplified (ISO 2022) *)
   (cp:51932; name:'EUC-JP'),
   (cp:51932; name:'CP51932'),
   (cp:51932; name:'MS51932'),
   (cp:51932; name:'WINDOWS-51932'),
   (cp:51932; name:'euc-jp'), (* EUC Japanese *)
   (cp:51936; name:'EUC-CN'), (* EUC Simplified Chinese; Chinese Simplified (EUC) *)
   (cp:51949; name:'euc-kr'), (* EUC Korean *)
   (cp:52936; name:'hz-gb-2312'), (* HZ-GB2312 Simplified Chinese; Chinese Simplified (HZ) *)
   (cp:54936; name:'GB18030'), (* Windows XP and later: GB18030 Simplified Chinese (4 byte); Chinese Simplified (GB18030) *)
   (cp:57002; name:'x-iscii-de'), (* ISCII Devanagari *)
   (cp:57003; name:'x-iscii-be'), (* ISCII Bengali *)
   (cp:57004; name:'x-iscii-ta'), (* ISCII Tamil *)
   (cp:57005; name:'x-iscii-te'), (* ISCII Telugu *)
   (cp:57006; name:'x-iscii-as'), (* ISCII Assamese *)
   (cp:57007; name:'x-iscii-or'), (* ISCII Oriya *)
   (cp:57008; name:'x-iscii-ka'), (* ISCII Kannada *)
   (cp:57009; name:'x-iscii-ma'), (* ISCII Malayalam *)
   (cp:57010; name:'x-iscii-gu'), (* ISCII Gujarati *)
   (cp:57011; name:'x-iscii-pa'), (* ISCII Punjabi *)
   (cp:65001; name:'UTF-8'),
   (cp:65001; name:'CP65001'),
   (cp:65001; name:'UTF8'));
   
{ returns index in UnixCpMap with first code page name with matching
cp number (so that multiple names can be tried if necessary) }
function GetCodepageData(cp: TSystemCodePage): longint;
function GetCodepageByName(cpname: rawbytestring): TSystemCodePage;
function GetSystemCodepage: TSystemCodePage;

implementation

{ returns index in UnixCpMap with first code page name with matching
cp number (so that multiple names can be tried if necessary) }
function GetCodepageData(cp: TSystemCodePage): longint;
var
  l, h, i, ccp: longint;
begin
  l:=low(UnixCpMap);
  h:=high(UnixCpMap);
  repeat
    i:=(l+h+1) shr 1;
    ccp:=UnixCpMap[i].cp;
    if cp=ccp then
      break;
    if cp>=ccp then
      l:=i
    else
      h:=i-1;
  until l>=h;
  if cp=UnixCpMap[i].cp then
    begin
      { the array has been ordered so that in case multiple alias names
        exist, the first entry for the cp is the most commonly supported
        one
      }
      while (i>low(UnixCpMap)) and
            (UnixCpMap[i-1].cp=cp) do
        dec(i);
      result:=i;
    end
  else
    { or better raise an error? }
    result:=-1;
end;

function GetCodepageByName(cpname: rawbytestring): TSystemCodePage;
var
  i: longint;
begin
  { clear encoding to prevent nonsense code page conversion of the input
    ansistring (encoding names are always ascii) }
  SetCodePage(cpname,$ffff,false);

  { Linux uses cpXXXX instead of CPXXXX }
  if (length(cpname)>2) and
     (cpname[1]='c') and
     (cpname[2]='p') and
     (cpname[3] in ['0'..'9']) then
    begin
      cpname[1]:='C';
      cpname[2]:='P';
    end;

  { simple linear scan, not a common operation and hence not worth
    building a separate array for -- start from index 1 rather than
    0, because 0 = fake "code page 0" that maps to UTF-8 as default
  }
  for i:=low(UnixCpMap)+1 to high(UnixCpMap) do
    with UnixCpMap[i] do
      if name=cpname then
        begin
          result:=cp;
          exit;
        end;
  { rawbytestring (or better raise an error?) }
  result:=CP_NONE;
end;

function GetSystemCodepage: TSystemCodePage;
var
  p: SizeInt;
  lang: ansistring;
  cp: TSystemCodePage;
begin
  // Get one of non-empty environment variables in the next order:
  // LC_ALL, LC_CTYPE, LANG. Default is UTF-8 or ASCII.
{$ifdef linux}
  Result:=CP_UTF8;
{$else}
  Result:=CP_ASCII;
{$endif linux}
  lang:=FpGetEnv('LC_ALL');
  if lang='' then
    lang:=FpGetEnv('LC_CTYPE');
  if lang='' then
    lang:=FpGetEnv('LANG');
  if lang<>'' then
    begin
      // clean up, for example en_US.UTF-8 => UTF-8
      p:=Pos('.',lang);
      if p>0 then
        Delete(lang,1,p);
      p:=Pos('@',lang);
      if p>0 then
        Delete(lang,p,length(lang)-p+1);
      cp:=GetCodepageByName(lang);
      if cp <> CP_NONE then
        Result:=cp;
    end;
end;

end.
