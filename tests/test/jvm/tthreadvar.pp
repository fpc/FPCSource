program tthreadvar;

{$mode delphi}
{$modeswitch unicodestrings}

uses
  {$ifdef java}jdk15{$else}androidr14{$endif};

type
  tc = class
  end;

  tmythread = class(JLThread)
    procedure run; override;
  end;


type
  tthreadvarkind = (fboolean,fbyte,fsmallint,fcardinal,fint64,fchar,fwidechar,fsingle,fdouble,fsetint,fsetenum,frec,fshortstring,funicodestring,farrbyte,farrset);

  tsetint = set of 30..40;
  tsetenum = set of tthreadvarkind;
  tarrbyte = array[4..6] of byte;
  tarrset = array[1..2] of tsetint;
  trec = record
    a: longint;
    b: array[3..4] of ansistring;
  end;


const
  cenumin: tthreadvarkind = fcardinal;
  cbooleanin: boolean = true;
  cbytein: byte = 35;
  csmallintin: smallint = 1234;
  ccardinalin: cardinal = $1234567;
  cint64in: int64 = $deadcafebabe;
  ccharin: ansichar = 'S';
  cwidecharin: widechar = 'U';
  csinglein: single = 1234.5;
  cdoublein: double = 1239643.75;
  csetintin: tsetint = [36..39];
  csetenumin: tsetenum = [fsmallint,fint64,funicodestring];
  crecin: trec = (a:98765; b:('abc','def'));
  cshortstringin: shortstring = 'greaT';
  cunicodestringin: unicodestring = 'a bit longer!';
  carrbytein: tarrbyte = (4,2,5);
  carrsetin: tarrset = ([31,33,37],[]);
  
  cenumout: tthreadvarkind = farrbyte;
  cbooleanout: boolean = false;
  cbyteout: byte = 128;
  csmallintout: smallint = 4321;
  ccardinalout: cardinal = $7654321;
  cint64out: int64 = $B4B3154713;
  ccharout: ansichar = 's';
  cwidecharout: widechar = 'u';
  csingleout: single = 4321.5;
  cdoubleout: double = 9876543.75;
  csetintout: tsetint = [31..36];
  csetenumout: tsetenum = [fbyte];
  crecout: trec = (a:4365246; b:('cbax','iiiiii'));
  cshortstringout: shortstring = 'tiny';
  cunicodestringout: unicodestring = 'yet another bit longer!';
  carrbyteout: tarrbyte = (6,6,6);
  carrsetout: tarrset = ([30,31],[33..38]);

  threadvar
    venum: tthreadvarkind;
    vboolean: boolean;
    vbyte: byte;
    vsmallint: smallint;
    vcardinal: cardinal;
    vint64: int64;
    vchar: ansichar;
    vwidechar: widechar;
    vsingle: single;
    vdouble: double;
    vsetint: tsetint;
    vsetenum: tsetenum;
    vrec: trec;
    vshortstring: shortstring;
    vunicodestring: unicodestring;
    varrbyte: tarrbyte;
    varrset: tarrset;
    i: longint;


procedure checkmainthreadvarsinit;
  var
    i: longint;
  begin
    if venum<>fboolean then
      raise jlexception.create('enum in');    
    venum:=cenumout;
    if venum<>cenumout then
      raise jlexception.create('enum out');
    if vboolean<>false then
      raise jlexception.create('boolean in');
    vboolean:=cbooleanout;
    if vboolean<>cbooleanout then
      raise jlexception.create('boolean out');
    if vbyte<>0 then
      raise jlexception.create('byte in');
    vbyte:=cbyteout;
    if vbyte<>cbyteout then
      raise jlexception.create('byte out');
    if vsmallint<>0 then
      raise jlexception.create('smallint in');
    vsmallint:=csmallintout;
    if vsmallint<>csmallintout then
      raise jlexception.create('smallint out');
    if vcardinal<>0 then
      raise jlexception.create('cardinal in');
    vcardinal:=ccardinalout;
    if vcardinal<>ccardinalout then
      raise jlexception.create('cardinal out');
    if vint64<>0 then
      raise jlexception.create('int64 in');
    vint64:=cint64out;
    if vint64<>cint64out then
      raise jlexception.create('int64 out');
    if vchar<>#0 then
      raise jlexception.create('char in');
    vchar:=ccharout;
    if vchar<>ccharout then
      raise jlexception.create('char out');
    if vwidechar<>#0 then
      raise jlexception.create('widechar in');
    vwidechar:=cwidecharout;
    if vwidechar<>cwidecharout then
      raise jlexception.create('widechar out');
    if vsingle<>0 then
      raise jlexception.create('single in');
    vsingle:=csingleout;
    if vsingle<>csingleout then
      raise jlexception.create('single out');
    if vdouble<>0 then
      raise jlexception.create('double in');
    vdouble:=cdoubleout;
    if vdouble<>cdoubleout then
      raise jlexception.create('double out');
    if vsetint<>[] then
      raise jlexception.create('setint in');
    vsetint:=csetintout;
    if vsetint<>csetintout then
      raise jlexception.create('setint out');
    if vsetenum<>[] then
      raise jlexception.create('setenum in');
    vsetenum:=csetenumout;
    if vsetenum<>csetenumout then
      raise jlexception.create('setenum out');
    if vrec.a<>0 then
      raise jlexception.create('rec.a in');
    if vrec.b[3]<>'' then
      raise jlexception.create('rec.b[3] in');
    if vrec.b[4]<>'' then
      raise jlexception.create('rec.b[4] in');
    vrec:=crecout;
    if crecout.a<>vrec.a then
      raise jlexception.create('rec.a out');
    if crecout.b[3]<>vrec.b[3] then
      raise jlexception.create('rec.b[3] out');
    if crecout.b[4]<>vrec.b[4] then
      raise jlexception.create('rec.b[4] out');
    if vshortstring<>'' then
      raise jlexception.create('shortstring in');
    vshortstring:=cshortstringout;
    if vshortstring<>cshortstringout then
      raise jlexception.create('shortstring out');
    if vunicodestring<>'' then
      raise jlexception.create('unicodestring in');
    vunicodestring:=cunicodestringout;
    if vunicodestring<>cunicodestringout then
      raise jlexception.create('unicodestring out');
    for i:=low(varrbyte) to high(varrbyte) do
      if varrbyte[i]<>0 then
        raise jlexception.create('arrbyte in');
    varrbyte:=carrbyteout;
    for i:=low(carrbyteout) to high(carrbyteout) do
      if carrbyteout[i]<>varrbyte[i] then
        raise jlexception.create('arrbyte out');
    for i:=low(varrset) to high(varrset) do
      if varrset[i]<>[] then
        raise jlexception.create('arrset in');
    varrset:=carrsetout;
    for i:=low(carrsetout) to high(carrsetout) do
      if varrset[i]<>carrsetout[i] then
        raise jlexception.create('arrset out');
  end;


procedure testmainthreadvarsafterwards;
  var
    i: longint;
  begin
    if venum<>cenumout then
      raise jlexception.create('venum out2');
    if vboolean<>cbooleanout then
      raise jlexception.create('boolean out2');
    if vbyte<>cbyteout then
      raise jlexception.create('byte out2');
    if vsmallint<>csmallintout then
      raise jlexception.create('smallint out2');
    if vcardinal<>ccardinalout then
      raise jlexception.create('cardinal out2');
    if vint64<>cint64out then
      raise jlexception.create('int64 out2');
    if vchar<>ccharout then
      raise jlexception.create('char out2');
    if vwidechar<>cwidecharout then
      raise jlexception.create('widechar out2');
    if vsingle<>csingleout then
      raise jlexception.create('single out2');
    if vdouble<>cdoubleout then
      raise jlexception.create('double out2');
    if vsetint<>csetintout then
      raise jlexception.create('setint out2');
    if vsetenum<>csetenumout then
      raise jlexception.create('setenum out2');
    if crecout.a<>vrec.a then
      raise jlexception.create('rec.a out2');
    if crecout.b[3]<>vrec.b[3] then
      raise jlexception.create('rec.b[3] out2');
    if crecout.b[4]<>vrec.b[4] then
      raise jlexception.create('rec.b[4] out2');
    if vshortstring<>cshortstringout then
      raise jlexception.create('shortstring out2');
    if vunicodestring<>cunicodestringout then
      raise jlexception.create('unicodestring out2');
    for i:=low(carrbyteout) to high(carrbyteout) do
      if carrbyteout[i]<>varrbyte[i] then
        raise jlexception.create('arrbyte out2');
    for i:=low(carrsetout) to high(carrsetout) do
      if varrset[i]<>carrsetout[i] then
        raise jlexception.create('arrset out2');
  end;


procedure tmythread.run;
  var
    i: longint;
  begin
    if venum<>fboolean then
      raise jlexception.create('enum in');    
    venum:=cenumin;
    if venum<>cenumin then
      raise jlexception.create('enum out');
    if vboolean<>false then
      raise jlexception.create('boolean in');
    vboolean:=cbooleanin;
    if vboolean<>cbooleanin then
      raise jlexception.create('boolean out');
    if vbyte<>0 then
      raise jlexception.create('byte in');
    vbyte:=cbytein;
    if vbyte<>cbytein then
      raise jlexception.create('byte out');
    if vsmallint<>0 then
      raise jlexception.create('smallint in');
    vsmallint:=csmallintin;
    if vsmallint<>csmallintin then
      raise jlexception.create('smallint out');
    if vcardinal<>0 then
      raise jlexception.create('cardinal in');
    vcardinal:=ccardinalin;
    if vcardinal<>ccardinalin then
      raise jlexception.create('cardinal out');
    if vint64<>0 then
      raise jlexception.create('int64 in');
    vint64:=cint64in;
    if vint64<>cint64in then
      raise jlexception.create('int64 out');
    if vchar<>#0 then
      raise jlexception.create('char in');
    vchar:=ccharin;
    if vchar<>ccharin then
      raise jlexception.create('char out');
    if vwidechar<>#0 then
      raise jlexception.create('widechar in');
    vwidechar:=cwidecharin;
    if vwidechar<>cwidecharin then
      raise jlexception.create('widechar out');
    if vsingle<>0 then
      raise jlexception.create('single in');
    vsingle:=csinglein;
    if vsingle<>csinglein then
      raise jlexception.create('single out');
    if vdouble<>0 then
      raise jlexception.create('double in');
    vdouble:=cdoublein;
    if vdouble<>cdoublein then
      raise jlexception.create('double out');
    if vsetint<>[] then
      raise jlexception.create('setint in');
    vsetint:=csetintin;
    if vsetint<>csetintin then
      raise jlexception.create('setint out');
    if vsetenum<>[] then
      raise jlexception.create('setenum in');
    vsetenum:=csetenumin;
    if vsetenum<>csetenumin then
      raise jlexception.create('setenum out');
    if vrec.a<>0 then
      raise jlexception.create('rec.a in');
    if vrec.b[3]<>'' then
      raise jlexception.create('rec.b[3] in');
    if vrec.b[4]<>'' then
      raise jlexception.create('rec.b[4] in');
    vrec:=crecin;
    if crecin.a<>vrec.a then
      raise jlexception.create('rec.a out');
    if crecin.b[3]<>vrec.b[3] then
      raise jlexception.create('rec.b[3] out');
    if crecin.b[4]<>vrec.b[4] then
      raise jlexception.create('rec.b[4] out');
    if vshortstring<>'' then
      raise jlexception.create('shortstring in');
    vshortstring:=cshortstringin;
    if vshortstring<>cshortstringin then
      raise jlexception.create('shortstring out');
    if vunicodestring<>'' then
      raise jlexception.create('unicodestring in');
    vunicodestring:=cunicodestringin;
    if vunicodestring<>cunicodestringin then
      raise jlexception.create('unicodestring out');
    for i:=low(varrbyte) to high(varrbyte) do
      if varrbyte[i]<>0 then
        raise jlexception.create('arrbyte in');
    varrbyte:=carrbytein;
    for i:=low(carrbytein) to high(carrbytein) do
      if carrbytein[i]<>varrbyte[i] then
        raise jlexception.create('arrbyte out');
    for i:=low(varrset) to high(varrset) do
      if varrset[i]<>[] then
        raise jlexception.create('arrset in');
    varrset:=carrsetin;
    for i:=low(carrsetin) to high(carrsetin) do
      if varrset[i]<>carrsetin[i] then
        raise jlexception.create('arrset out');
  end;


procedure test;
var
  t1, t2: tmythread;
begin
  checkmainthreadvarsinit;
  t1:=tmythread.create;
  t1.start;
  t2:=tmythread.create;
  t2.start;
  t1.join;
  t2.join;
  testmainthreadvarsafterwards;
end;

begin
  test;
end.
