    { source: http://download.java.net/jdk8/docs/technotes/guides/intl/encoding.doc.html
    }
    
 type
   twin2javacs = record
     cp: word;
     name: unicodestring;
   end;
 (*
  * Code Page Identifiers
  * http://msdn.microsoft.com/en-us/library/dd317756.aspx
  *)
  const 
    win2javacs_arr: array[0..154] of twin2javacs =
       (* (cp:; name:'x-MacDingbat'), not supported ? *)
       (* (cp:; name:'x-MacSymbol'), not supported ? *)
       ((cp:037; name:'IBM037'),
        (cp:437; name:'IBM437'),
        (cp:500; name:'IBM500'),
        (cp:737; name:'x-IBM737'),
        (cp:775; name:'IBM775'),
        (cp:850; name:'IBM850'),
        (cp:852; name:'IBM852'),
        (cp:855; name:'IBM855'),
        (cp:857; name:'IBM857'),
        (cp:858; name:'IBM00858'),
        (cp:860; name:'IBM860'),
        (cp:861; name:'IBM861'),
        (cp:862; name:'IBM862'),
        (cp:863; name:'IBM863'),
        (cp:864; name:'IBM864'),
        (cp:865; name:'IBM865'),
        (cp:866; name:'IBM866'),
        (cp:868; name:'IBM868'),
        (cp:869; name:'IBM869'),
        (cp:870; name:'IBM870'),
        (cp:874; name:'x-windows-874'),
        (cp:918; name:'IBM918'),
        (cp:932; name:'Shift_JIS'),
        (cp:932; name:'x-PCK'),
        (cp:936; name:'GB2312'),
        (cp:936; name:'x-mswin-936'),
        (cp:942; name:'x-IBM942'),
        (cp:943; name:'x-IBM943'),
        (cp:948; name:'x-IBM948'),
        (cp:949; name:'x-IBM949'),
        (cp:949; name:'x-windows-949'),
        (cp:950; name:'Big5'),
        (cp:950; name:'x-IBM950'),
        (cp:950; name:'x-windows-950'),
        (cp:951; name:'x-Big5-HKSCS-2001'),
        (cp:951; name:'Big5-HKSCS'),
        (cp:951; name:'x-MS950-HKSCS'),
        (cp:951; name:'x-MS950-HKSCS-XP'),
        (cp:964; name:'x-IBM964'),
        (cp:970; name:'x-IBM970'),
        (cp:1006; name:'x-IBM1006'),
        (cp:1025; name:'x-IBM1025'),
        (cp:1026; name:'IBM1026'),
        (cp:1046; name:'x-IBM1046'),
        (cp:1047; name:'IBM1047'),
        (cp:1097; name:'x-IBM1097'),
        (cp:1098; name:'x-IBM1098'),
        (cp:1112; name:'x-IBM1112'),
        (cp:1122; name:'x-IBM1122'),
        (cp:1123; name:'x-IBM1123'),
        (cp:1124; name:'x-IBM1124'),
        (cp:1140; name:'IBM01140'),
        (cp:1141; name:'IBM01141'),
        (cp:1142; name:'IBM01142'),
        (cp:1143; name:'IBM01143'),
        (cp:1144; name:'IBM01144'),
        (cp:1145; name:'IBM01145'),
        (cp:1146; name:'IBM01146'),
        (cp:1147; name:'IBM01147'),
        (cp:1148; name:'IBM01148'),
        (cp:1149; name:'IBM01149'),
        (cp:1200; name:'UTF-16LE'),
        (cp:1200; name:'x-UTF-16LE-BOM'),
        (cp:1201; name:'UTF-16'),
        (cp:1201; name:'UTF-16BE'),
        (cp:1250; name:'windows-1250'),
        (cp:1251; name:'windows-1251'),
        (cp:1252; name:'windows-1252'),
        (cp:1253; name:'windows-1253'),
        (cp:1254; name:'windows-1254'),
        (cp:1255; name:'windows-1255'),
        (cp:1256; name:'windows-1256'),
        (cp:1257; name:'windows-1257'),
        (cp:1258; name:'windows-1258'),
        (cp:1259; name:'windows-31j'),
        (cp:1361; name:'x-Johab'),
        (cp:1381; name:'x-IBM1381'),
        (cp:1383; name:'x-IBM1383'),
        (cp:10000; name:'MacRoman'),
        (cp:10004; name:'x-MacArabic'),
        (cp:10005; name:'x-MacHebrew'),
        (cp:10006; name:'x-MacGreek'),
        (cp:10007; name:'x-MacCyrillic'),
        (cp:10010; name:'x-MacRomania'),
        (cp:10017; name:'x-MacUkraine'),
        (cp:10021; name:'x-MacThai'),
        (cp:10029; name:'x-MacCentralEurope'),
        (cp:10079; name:'x-MacIceland'),
        (cp:10081; name:'x-MacTurkish'),
        (cp:10082; name:'x-MacCroatian'),
        (cp:12000; name:'UTF-32LE'),
        (cp:12000; name:'X-UTF-32LE-BOM'),
        (cp:12001; name:'UTF-32'),
        (cp:12001; name:'UTF-32BE'),
        (cp:12001; name:'X-UTF-32BE-BOM'),
        (cp:20127; name:'US-ASCII'),
        (cp:20273; name:'IBM273'),
        (cp:20277; name:'IBM277'),
        (cp:20278; name:'IBM278'),
        (cp:20280; name:'IBM280'),
        (cp:20284; name:'IBM284'),
        (cp:20285; name:'IBM285'),
        (cp:20297; name:'IBM297'),
        (cp:20420; name:'IBM420'),
        (cp:20424; name:'IBM424'),
        (cp:20833; name:'x-IBM833'),
        (cp:20834; name:'x-IBM834'),
        (cp:20838; name:'IBM-Thai'),
        (cp:20856; name:'x-IBM856'),
        (cp:20866; name:'KOI8-R'),
        (cp:20871; name:'IBM871'),
        (cp:20874; name:'x-IBM874'),
        (cp:20875; name:'x-IBM875'),
        (cp:20921; name:'x-IBM921'),
        (cp:20922; name:'x-IBM922'),
        (cp:20932; name:'EUC-JP'),
        (cp:20932; name:'x-JIS0208'),
        (cp:20932; name:'x-JISAutoDetect'),
        (cp:21866; name:'KOI8-U'),
        (cp:28591; name:'ISO-8859-1'),
        (cp:28592; name:'ISO-8859-2'),
        (cp:28593; name:'ISO-8859-3'),
        (cp:28594; name:'ISO-8859-4'),
        (cp:28595; name:'ISO-8859-5'),
        (cp:28596; name:'ISO-8859-6'),
        (cp:28597; name:'ISO-8859-7'),
        (cp:28598; name:'ISO-8859-8'),
        (cp:28599; name:'ISO-8859-9'),
        (cp:28601; name:'TIS-620'),
        (cp:28601; name:'x-iso-8859-11'),
        (cp:28603; name:'ISO-8859-13'),
        (cp:28605; name:'ISO-8859-15'),
        (cp:33722; name:'x-IBM33722'),
        (cp:50220; name:'x-windows-50220'),
        (cp:50220; name:'x-windows-iso2022jp'),
        (cp:50221; name:'JIS_X0201'),
        (cp:50221; name:'x-windows-50221'),
        (cp:50222; name:'ISO-2022-JP'),
        (cp:50222; name:'ISO-2022-JP-2'), (* not exact, Windows does not support ISO-2022-JP-2 *)
        (cp:50222; name:'JIS_X0212-1990'),
        (cp:50225; name:'ISO-2022-KR'),
        (cp:50227; name:'ISO-2022-CN'),
        (cp:50227; name:'x-ISO-2022-CN-CNS'),
        (cp:50229; name:'x-ISO-2022-CN-GB'),
        (cp:50930; name:'x-IBM930'),
        (cp:50933; name:'x-IBM933'),
        (cp:50935; name:'x-IBM935'),
        (cp:50937; name:'x-IBM937'),
        (cp:50939; name:'x-IBM939'),
        (cp:51932; name:'x-MS932_0213'),
        (cp:51932; name:'x-SJIS_0213'),
        (cp:51949; name:'EUC-KR'),
        (cp:54936; name:'GB18030'),
        (cp:57002; name:'x-ISCII91'),
        (cp:65001; name:'UTF-8'));


  function win2javacs(cp: word): unicodestring;
    var
      l, h, i, ccp: longint;
    begin
      l:=low(win2javacs_arr);
      h:=high(win2javacs_arr);
      repeat
        i:=(l+h+1) shr 1;
        ccp:=win2javacs_arr[i].cp;
        if cp=ccp then
          break;
        if cp>=ccp then
          l:=i
        else
          h:=i-1;
      until l>=h;
      if cp=win2javacs_arr[i].cp then
        begin
          { the array has been ordered so that in case multiple alias names
            exist, the first entry for the cp is the most commonly supported
            one
          }
          while (i>low(win2javacs_arr)) and
                (win2javacs_arr[i-1].cp=cp) do
            dec(i);
          result:=win2javacs_arr[i].name;
        end
      else
        { or better raise an error? }
        result:='<unsupported>';
    end;
    
    
  function javacs2win(cpname: unicodestring): word;
    var
      i: longint;
    begin
      { simple linear scan, not a common operation and hence not worth
        building a separate array for }
      for i:=low(win2javacs_arr) to high(win2javacs_arr) do
        if win2javacs_arr[i].name=cpname then
          begin
            result:=win2javacs_arr[i].cp;
            exit;
          end;
      { rawbytestring (or better raise an error?) }
      result:=65535;
    end;
    
