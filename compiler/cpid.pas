{
    Copyright (c) 2008 by Florian Klaempfl

    Basic stuff for encoding sensitive strings

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 ****************************************************************************
}
unit cpid;


{$i fpcdefs.inc}

  interface

    type
      TEncodingEntry = record
        id : TStringEncoding;
        name : Ansistring;
      end;

    const Encodings : array[0..150] of TEncodingEntry = (
      id : 037; name : 'IBM037';
      id : 437; name : 'IBM437';
      id : 500; name : 'IBM500';
      id : 708; name : 'ASMO-708';
      id : 709; name : 'ASMO-449+';
      id : 710; name : 'Arabic';
      id : 720; name : 'DOS-720';
      id : 737; name : 'ibm737';
      id : 775; name : 'ibm775';
      id : 850; name : 'ibm850';
      id : 852; name : 'ibm852';
      id : 855; name : 'IBM855';
      id : 857; name : 'ibm857';
      id : 858; name : 'IBM00858';
      id : 860; name : 'IBM860';
      id : 861; name : 'ibm861';
      id : 862; name : 'DOS-862';
      id : 863; name : 'IBM863';
      id : 864; name : 'IBM864';
      id : 865; name : 'IBM865';
      id : 866; name : 'cp866'';;
      id : 869; name : 'ibm869';
      id : 870; name : 'IBM870';
      id : 874; name : 'windows-874';
      id : 875; name : 'cp875';
      id : 932; name : 'shift_jis';
      id : 936; name : 'gb2312';
      id : 949; name : 'ks_c_5601-1987';
      id : 950; name : 'big5';
      id : 1026; name : 'IBM1026';
      id : 1047; name : 'IBM01047';
      id : 1140; name : 'IBM01140';
      id : 1141; name : 'IBM01141';
      id : 1142; name : 'IBM01142';
      id : 1143; name : 'IBM01143';
      id : 1144; name : 'IBM01144';
      id : 1145; name : 'IBM01145';
      id : 1146; name : 'IBM01146';
      id : 1147; name : 'IBM01147';
      id : 1148; name : 'IBM01148';
      id : 1149; name : 'IBM01149';
      id : 1200; name : 'utf-16';
      id : 1201; name : 'unicodeFFFE';
      id : 1250; name : 'windows-1250';
      id : 1251; name : 'windows-1251';
      id : 1252; name : 'windows-1252';
      id : 1253; name : 'windows-1253';
      id : 1254; name : 'windows-1254';
      id : 1255; name : 'windows-1255';
      id : 1256; name : 'windows-1256';
      id : 1257; name : 'windows-1257';
      id : 1258; name : 'windows-1258';
      id : 1361; name : 'Johab';
      id : 10000; name : 'macintosh';
      id : 10001; name : 'x-mac-japanese';
      id : 10002; name : 'x-mac-chinesetrad';
      id : 10003; name : 'x-mac-korean';
      id : 10004; name : 'x-mac-arabic';
      id : 10005; name : 'x-mac-hebrew';
      id : 10006; name : 'x-mac-greek';
      id : 10007; name : 'x-mac-cyrillic';
      id : 10008; name : 'x-mac-chinesesimp';
      id : 10010; name : 'x-mac-romanian';
      id : 10017; name : 'x-mac-ukrainian';
      id : 10021; name : 'x-mac-thai';
      id : 10029; name : 'x-mac-ce';
      id : 10079; name : 'x-mac-icelandic';
      id : 10081; name : 'x-mac-turkish';
      id : 10082; name : 'x-mac-croatian';
      id : 12000; name : 'utf-32';
      id : 12001; name : 'utf-32BE';
      id : 20000; name : 'x-Chinese_CNS';
      id : 20001; name : 'x-cp20001';
      id : 20002; name : 'x_Chinese-Eten';
      id : 20003; name : 'x-cp20003';
      id : 20004; name : 'x-cp20004';
      id : 20005; name : 'x-cp20005';
      id : 20105; name : 'x-IA5';
      id : 20106; name : 'x-IA5-German';
      id : 20107; name : 'x-IA5-Swedish';
      id : 20108; name : 'x-IA5-Norwegian';
      id : 20127; name : 'us-ascii';
      id : 20261; name : 'x-cp20261';
      id : 20269; name : 'x-cp20269';
      id : 20273; name : 'IBM273';
      id : 20277; name : 'IBM277';
      id : 20278; name : 'IBM278';
      id : 20280; name : 'IBM280';
      id : 20284; name : 'IBM284';
      id : 20285; name : 'IBM285';
      id : 20290; name : 'IBM290';
      id : 20297; name : 'IBM297';
      id : 20420; name : 'IBM420';
      id : 20423; name : 'IBM423';
      id : 20424; name : 'IBM424';
      id : 20833; name : 'x-EBCDIC-KoreanExtended';
      id : 20838; name : 'IBM-Thai';
      id : 20866; name : 'koi8-r';
      id : 20871; name : 'IBM871';
      id : 20880; name : 'IBM880';
      id : 20905; name : 'IBM905';
      id : 20924; name : 'IBM00924';
      id : 20932; name : 'EUC-JP';
      id : 20936; name : 'x-cp20936';
      id : 20949; name : 'x-cp20949';
      id : 21025; name : 'cp1025';
      id : 21866; name : 'koi8-u';
      id : 28591; name : 'iso-8859-1';
      id : 28592; name : 'iso-8859-2';
      id : 28593; name : 'iso-8859-3';
      id : 28594; name : 'iso-8859-4';
      id : 28595; name : 'iso-8859-5';
      id : 28596; name : 'iso-8859-6';
      id : 28597; name : 'iso-8859-7';
      id : 28598; name : 'iso-8859-8';
      id : 28599; name : 'iso-8859-9';
      id : 28603; name : 'iso-8859-13';
      id : 28605; name : 'iso-8859-15';
      id : 29001; name : 'x-Europa';
      id : 38598; name : 'iso-8859-8-i';
      id : 50220; name : 'iso-2022-jp';
      id : 50221; name : 'csISO2022JP';
      id : 50222; name : 'iso-2022-jp';
      id : 50225; name : 'iso-2022-kr';
      id : 50227; name : 'x-cp50227';
      id : 50229; name : 'ISO 2022';
      { not unique
      id : 50930; name : 'EBCDIC
      id : 50931; name : 'EBCDIC
      id : 50933; name : 'EBCDIC
      id : 50935; name : 'EBCDIC
      id : 50936; name : 'EBCDIC
      id : 50937; name : 'EBCDIC
      id : 50939; name : 'EBCDIC
      }
      id : 51932; name : 'euc-jp';
      id : 51936; name : 'EUC-CN';
      id : 51949; name : 'euc-kr';
      id : 51950; name : 'EUC';
      id : 52936; name : 'hz-gb-2312';
      id : 54936; name : 'GB18030';
      id : 57002; name : 'x-iscii-de';
      id : 57003; name : 'x-iscii-be';
      id : 57004; name : 'x-iscii-ta';
      id : 57005; name : 'x-iscii-te';
      id : 57006; name : 'x-iscii-as';
      id : 57007; name : 'x-iscii-or';
      id : 57008; name : 'x-iscii-ka';
      id : 57009; name : 'x-iscii-ma';
      id : 57010; name : 'x-iscii-gu';
      id : 57011; name : 'x-iscii-pa';
      id : 65000; name : 'utf-7';
      id : 65001; name : 'utf-8');

  implementation

end.
