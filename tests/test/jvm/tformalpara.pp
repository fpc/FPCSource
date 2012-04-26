unit tformalpara;

{$mode delphi}
{$modeswitch unicodestrings}

interface

procedure main(args: array of string);

implementation

uses
  {$ifdef java}jdk15{$else}androidr14{$endif};

type
  tc = class
  end;

procedure freeandnil(var obj);
begin
  obj:=nil;
end;

procedure test;
var
  c: tc;
begin
  c:=tc.create;
  freeandnil(c);
  if assigned(c) then
    raise jlexception.create('help');
end;

type
  tformalkind = (fboolean,fbyte,fsmallint,fcardinal,fint64,fchar,fwidechar,fsingle,fdouble,fsetint,fsetenum,frec,fshortstring,funicodestring,farrbyte,farrset,fenum);

  tsetint = set of 30..40;
  tsetenum = set of tformalkind;
  tarrbyte = array[4..6] of byte;
  tarrset = array[1..2] of tsetint;
  trec = record
    a: longint;
    b: array[3..4] of ansistring;
  end;

const
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
  cenumin: tformalkind = fsmallint;
  
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
  cenumout: tformalkind = farrbyte;

procedure testformalvar(var x; typ: tformalkind);
  var
    i: longint;
  begin
    case typ of
      fboolean:
        begin
          if cbooleanin<>boolean(x) then
            raise jlexception.create('boolean in');
          x:=cbooleanout;
        end;
      fbyte:
        begin
          if cbytein<>byte(x) then
            raise jlexception.create('byte in');
          x:=cbyteout;
        end;
      fsmallint:
        begin
          if csmallintin<>smallint(x) then
            raise jlexception.create('smallint in');
          x:=csmallintout;
        end;
      fcardinal:
        begin
          if ccardinalin<>cardinal(x) then
            raise jlexception.create('cardinal in');
          x:=ccardinalout;
        end;
      fint64:
        begin
          if cint64in<>int64(x) then
            raise jlexception.create('int64 in');
          x:=cint64out;
        end;
      fchar:
        begin
          if ccharin<>ansichar(x) then
            raise jlexception.create('char in');
          x:=ccharout;
        end;
      fwidechar:
        begin
          if cwidecharin<>widechar(x) then
            raise jlexception.create('widechar in');
          x:=cwidecharout;
        end;
      fsingle:
        begin
          if csinglein<>single(x) then
            raise jlexception.create('single in');
          x:=csingleout;
        end;
      fdouble:
        begin
          if cdoublein<>double(x) then
            raise jlexception.create('double in');
          x:=cdoubleout;
        end;
      fsetint:
        begin
          if csetintin<>tsetint(x) then
            raise jlexception.create('setint in');
          x:=csetintout;
        end;
      fsetenum:
        begin
          if csetenumin<>tsetenum(x) then
            raise jlexception.create('setenum in');
          x:=csetenumout;
        end;
      frec:
        begin
          if crecin.a<>trec(x).a then
            raise jlexception.create('rec.a in');
          if crecin.b[3]<>trec(x).b[3] then
            raise jlexception.create('rec.b[3] in');
          if crecin.b[4]<>trec(x).b[4] then
            raise jlexception.create('rec.b[4] in');
          x:=crecout;
        end;
      fshortstring:
        begin
          if cshortstringin<>shortstring(x) then
            raise jlexception.create('shortstring in');
          x:=cshortstringout;
        end;
      funicodestring:
        begin
          if cunicodestringin<>unicodestring(x) then
            raise jlexception.create('unicodestring in');
          x:=cunicodestringout;
        end;
      farrbyte:
        begin
          for i:=low(carrbytein) to high(carrbytein) do
            if carrbytein[i]<>tarrbyte(x)[i] then
              raise jlexception.create('arrbyte in');
          x:=carrbyteout;
        end;
      farrset:
        begin
          for i:=low(carrsetin) to high(carrsetin) do
            if carrsetin[i]<>tarrset(x)[i] then
              raise jlexception.create('arrset in');
          x:=carrsetout;
        end;
      fenum:
        begin
          if cenumin<>tformalkind(x) then
            raise jlexception.create('enum in');
          x:=cenumout;
        end;
    end;
  end;


procedure testformalout(out x; typ: tformalkind);
  var
    i: longint;
  begin
    case typ of
      fboolean:
        begin
          x:=cbooleanout;
        end;
      fbyte:
        begin
          x:=cbyteout;
        end;
      fsmallint:
        begin
          x:=csmallintout;
        end;
      fcardinal:
        begin
          x:=ccardinalout;
        end;
      fint64:
        begin
          x:=cint64out;
        end;
      fchar:
        begin
          x:=ccharout;
        end;
      fwidechar:
        begin
          x:=cwidecharout;
        end;
      fsingle:
        begin
          x:=csingleout;
        end;
      fdouble:
        begin
          x:=cdoubleout;
        end;
      fsetint:
        begin
          x:=csetintout;
        end;
      fsetenum:
        begin
          x:=csetenumout;
        end;
      frec:
        begin
        { fpc only decreases the reference, it doesn't finalize/init with empty/nil
          if ''<>trec(x).b[3] then
            raise jlexception.create('out rec.b[3] in');
          if ''<>trec(x).b[4] then
            raise jlexception.create('out rec.b[4] in');
        }
          x:=crecout;
        end;
      fshortstring:
        begin
          x:=cshortstringout;
        end;
      funicodestring:
        begin
        { fpc only decreases the reference, it doesn't finalize/init with           if ''<>unicodestring(x) then
            raise jlexception.create('out unicodestring in');
        }
          x:=cunicodestringout;
        end;
      farrbyte:
        begin
          x:=carrbyteout;
        end;
      farrset:
        begin
          x:=carrsetout;
        end;
      fenum:
        begin
          x:=cenumout;
        end;
    end;
  end;


procedure testformalconst(const x; typ: tformalkind);
  var
    i: longint;
  begin
    case typ of
      fboolean:
        begin
          if cbooleanin<>boolean(x) then
            raise jlexception.create('const boolean in');
        end;
      fbyte:
        begin
          if cbytein<>byte(x) then
            raise jlexception.create('const byte in');
        end;
      fsmallint:
        begin
          if csmallintin<>smallint(x) then
            raise jlexception.create('const smallint in');
        end;
      fcardinal:
        begin
          if ccardinalin<>cardinal(x) then
            raise jlexception.create('const cardinal in');
        end;
      fint64:
        begin
          if cint64in<>int64(x) then
            raise jlexception.create('const int64 in');
        end;
      fchar:
        begin
          if ccharin<>ansichar(x) then
            raise jlexception.create('const char in');
        end;
      fwidechar:
        begin
          if cwidecharin<>widechar(x) then
            raise jlexception.create('const widechar in');
        end;
      fsingle:
        begin
          if csinglein<>single(x) then
            raise jlexception.create('const single in');
        end;
      fdouble:
        begin
          if cdoublein<>double(x) then
            raise jlexception.create('const double in');
        end;
      fsetint:
        begin
          if csetintin<>tsetint(x) then
            raise jlexception.create('const setint in');
        end;
      fsetenum:
        begin
          if csetenumin<>tsetenum(x) then
            raise jlexception.create('const setenum in');
        end;
      frec:
        begin
          if crecin.a<>trec(x).a then
            raise jlexception.create('const rec.a in');
          if crecin.b[3]<>trec(x).b[3] then
            raise jlexception.create('const rec.b[3] in');
          if crecin.b[4]<>trec(x).b[4] then
            raise jlexception.create('const rec.b[4] in');
        end;
      fshortstring:
        begin
          if cshortstringin<>shortstring(x) then
            raise jlexception.create('const shortstring in');
        end;
      funicodestring:
        begin
          if cunicodestringin<>unicodestring(x) then
            raise jlexception.create('const unicodestring in');
        end;
      farrbyte:
        begin
          for i:=low(carrbytein) to high(carrbytein) do
            if carrbytein[i]<>tarrbyte(x)[i] then
              raise jlexception.create('const arrbyte in');
        end;
      farrset:
        begin
          for i:=low(carrsetin) to high(carrsetin) do
            if carrsetin[i]<>tarrset(x)[i] then
              raise jlexception.create('const arrset in');
        end;
      fenum:
        begin
          if cenumin<>tformalkind(x) then
            raise jlexception.create('const enum in');
        end;
    end;
  end;


procedure testformalvars;
  var
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
    venum: tformalkind;
    i: longint;
  begin
    vboolean:=cbooleanin;
    testformalvar(vboolean,fboolean);
    if vboolean<>cbooleanout then
      raise jlexception.create('boolean out');
    vbyte:=cbytein;
    testformalvar(vbyte,fbyte);
    if vbyte<>cbyteout then
      raise jlexception.create('byte out');
    vsmallint:=csmallintin;
    testformalvar(vsmallint,fsmallint);
    if vsmallint<>csmallintout then
      raise jlexception.create('smallint out');
    vunicodestring:=widechar(csmallintin);
    testformalvar(smallint(vunicodestring[1]),fsmallint);
    if smallint(vunicodestring[1])<>csmallintout then
      raise jlexception.create('stringsmallint out');
    vcardinal:=ccardinalin;
    testformalvar(vcardinal,fcardinal);
    if vcardinal<>ccardinalout then
      raise jlexception.create('cardinal out');
    vint64:=cint64in;
    testformalvar(vint64,fint64);
    if vint64<>cint64out then
      raise jlexception.create('int64 out');
    vchar:=ccharin;
    testformalvar(vchar,fchar);
    if vchar<>ccharout then
      raise jlexception.create('char out');
    vwidechar:=cwidecharin;
    testformalvar(vwidechar,fwidechar);
    if vwidechar<>cwidecharout then
      raise jlexception.create('widechar out');
    vunicodestring:=cwidecharin;
    testformalvar(vunicodestring[1],fwidechar);
    if vunicodestring[1]<>cwidecharout then
      raise jlexception.create('stringwidechar out');
    vsingle:=csinglein;
    testformalvar(vsingle,fsingle);
    if vsingle<>csingleout then
      raise jlexception.create('single out');
    vdouble:=cdoublein;
    testformalvar(vdouble,fdouble);
    if vdouble<>cdoubleout then
      raise jlexception.create('double out');
    vsetint:=csetintin;
    testformalvar(vsetint,fsetint);
    if vsetint<>csetintout then
      raise jlexception.create('setint out');
    vsetenum:=csetenumin;
    testformalvar(vsetenum,fsetenum);
    if vsetenum<>csetenumout then
      raise jlexception.create('setenum out');
    vrec:=crecin;
    testformalvar(vrec,frec);
    if crecout.a<>vrec.a then
      raise jlexception.create('rec.a out');
    if crecout.b[3]<>vrec.b[3] then
      raise jlexception.create('rec.b[3] out');
    if crecout.b[4]<>vrec.b[4] then
      raise jlexception.create('rec.b[4] out');
    vshortstring:=cshortstringin;
    testformalvar(vshortstring,fshortstring);
    if vshortstring<>cshortstringout then
      raise jlexception.create('shortstring out');
    vunicodestring:=cunicodestringin;
    testformalvar(vunicodestring,funicodestring);
    if vunicodestring<>cunicodestringout then
      raise jlexception.create('unicodestring out');
    varrbyte:=carrbytein;
    testformalvar(varrbyte,farrbyte);
    for i:=low(carrbyteout) to high(carrbyteout) do
      if carrbyteout[i]<>varrbyte[i] then
        raise jlexception.create('arrbyte out');
    varrset:=carrsetin;
    testformalvar(varrset,farrset);
    for i:=low(carrsetout) to high(carrsetout) do
      if varrset[i]<>carrsetout[i] then
        raise jlexception.create('arrset out');
    venum:=cenumin;
    testformalvar(venum,fenum);
    if venum<>cenumout then
      raise jlexception.create('enum out');
  end;


procedure testformalouts;
  var
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
    venum: tformalkind;
    i: longint;
  begin
    vboolean:=cbooleanin;
    testformalout(vboolean,fboolean);
    if vboolean<>cbooleanout then
      raise jlexception.create('out boolean out');
    vbyte:=cbytein;
    testformalout(vbyte,fbyte);
    if vbyte<>cbyteout then
      raise jlexception.create('out byte out');
    vsmallint:=csmallintin;
    testformalout(vsmallint,fsmallint);
    if vsmallint<>csmallintout then
      raise jlexception.create('out smallint out');
    vunicodestring:=widechar(csmallintin);
    testformalout(smallint(vunicodestring[1]),fsmallint);
    if smallint(vunicodestring[1])<>csmallintout then
      raise jlexception.create('out stringsmallint out');
    vcardinal:=ccardinalin;
    testformalout(vcardinal,fcardinal);
    if vcardinal<>ccardinalout then
      raise jlexception.create('out cardinal out');
    vint64:=cint64in;
    testformalout(vint64,fint64);
    if vint64<>cint64out then
      raise jlexception.create('out int64 out');
    vchar:=ccharin;
    testformalout(vchar,fchar);
    if vchar<>ccharout then
      raise jlexception.create('out char out');
    vwidechar:=cwidecharin;
    testformalout(vwidechar,fwidechar);
    if vwidechar<>cwidecharout then
      raise jlexception.create('out widechar out');
    vunicodestring:=cwidecharin;
    testformalout(vunicodestring[1],fwidechar);
    if vunicodestring[1]<>cwidecharout then
      raise jlexception.create('out stringwidechar out');
    vsingle:=csinglein;
    testformalout(vsingle,fsingle);
    if vsingle<>csingleout then
      raise jlexception.create('out single out');
    vdouble:=cdoublein;
    testformalout(vdouble,fdouble);
    if vdouble<>cdoubleout then
      raise jlexception.create('out double out');
    vsetint:=csetintin;
    testformalout(vsetint,fsetint);
    if vsetint<>csetintout then
      raise jlexception.create('out setint out');
    vsetenum:=csetenumin;
    testformalout(vsetenum,fsetenum);
    if vsetenum<>csetenumout then
      raise jlexception.create('out setenum out');
    vrec:=crecin;
    testformalout(vrec,frec);
    if crecout.a<>vrec.a then
      raise jlexception.create('out rec.a out');
    if crecout.b[3]<>vrec.b[3] then
      raise jlexception.create('out rec.b[3] out');
    if crecout.b[4]<>vrec.b[4] then
      raise jlexception.create('out rec.b[4] out');
    vshortstring:=cshortstringin;
    testformalout(vshortstring,fshortstring);
    if vshortstring<>cshortstringout then
      raise jlexception.create('out shortstring out');
    vunicodestring:=cunicodestringin;
    testformalout(vunicodestring,funicodestring);
    if vunicodestring<>cunicodestringout then
      raise jlexception.create('out unicodestring out');
    varrbyte:=carrbytein;
    testformalout(varrbyte,farrbyte);
    for i:=low(carrbyteout) to high(carrbyteout) do
      if carrbyteout[i]<>varrbyte[i] then
        raise jlexception.create('out arrbyte out');
    varrset:=carrsetin;
    testformalout(varrset,farrset);
    for i:=low(carrsetout) to high(carrsetout) do
      if varrset[i]<>carrsetout[i] then
        raise jlexception.create('out arrset out');
    venum:=cenumin;
    testformalout(venum,fenum);
    if venum<>cenumout then
      raise jlexception.create('out enum out');
  end;


procedure testformalconsts;
  var
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
    venum: tformalkind;
    i: longint;
  begin
    vboolean:=cbooleanin;
    testformalconst(vboolean,fboolean);
    if vboolean<>cbooleanin then
      raise jlexception.create('const boolean out');
    vbyte:=cbytein;
    testformalconst(vbyte,fbyte);
    if vbyte<>cbytein then
      raise jlexception.create('const byte out');
    vsmallint:=csmallintin;
    testformalconst(vsmallint,fsmallint);
    if vsmallint<>csmallintin then
      raise jlexception.create('const smallint out');
    vunicodestring:=widechar(csmallintin);
    testformalconst(smallint(vunicodestring[1]),fsmallint);
    if smallint(vunicodestring[1])<>csmallintin then
      raise jlexception.create('const stringsmallint out');
    vcardinal:=ccardinalin;
    testformalconst(vcardinal,fcardinal);
    if vcardinal<>ccardinalin then
      raise jlexception.create('const cardinal out');
    vint64:=cint64in;
    testformalconst(vint64,fint64);
    if vint64<>cint64in then
      raise jlexception.create('const int64 out');
    vchar:=ccharin;
    testformalconst(vchar,fchar);
    if vchar<>ccharin then
      raise jlexception.create('const char out');
    vwidechar:=cwidecharin;
    testformalconst(vwidechar,fwidechar);
    if vwidechar<>cwidecharin then
      raise jlexception.create('const widechar out');
    vunicodestring:=cwidecharin;
    testformalconst(vunicodestring[1],fwidechar);
    if vunicodestring[1]<>cwidecharin then
      raise jlexception.create('const stringwidechar out');
    vsingle:=csinglein;
    testformalconst(vsingle,fsingle);
    if vsingle<>csinglein then
      raise jlexception.create('const single out');
    vdouble:=cdoublein;
    testformalconst(vdouble,fdouble);
    if vdouble<>cdoublein then
      raise jlexception.create('const double out');
    vsetint:=csetintin;
    testformalconst(vsetint,fsetint);
    if vsetint<>csetintin then
      raise jlexception.create('const setint out');
    vsetenum:=csetenumin;
    testformalconst(vsetenum,fsetenum);
    if vsetenum<>csetenumin then
      raise jlexception.create('const setenum out');
    vrec:=crecin;
    testformalconst(vrec,frec);
    if crecin.a<>vrec.a then
      raise jlexception.create('const rec.a out');
    if crecin.b[3]<>vrec.b[3] then
      raise jlexception.create('const rec.b[3] out');
    if crecin.b[4]<>vrec.b[4] then
      raise jlexception.create('const rec.b[4] out');
    vshortstring:=cshortstringin;
    testformalconst(vshortstring,fshortstring);
    if vshortstring<>cshortstringin then
      raise jlexception.create('const shortstring out');
    vunicodestring:=cunicodestringin;
    testformalconst(vunicodestring,funicodestring);
    if vunicodestring<>cunicodestringin then
      raise jlexception.create('const unicodestring out');
    varrbyte:=carrbytein;
    testformalconst(varrbyte,farrbyte);
    for i:=low(carrbytein) to high(carrbytein) do
      if carrbytein[i]<>varrbyte[i] then
        raise jlexception.create('const arrbyte out');
    varrset:=carrsetin;
    testformalconst(varrset,farrset);
    for i:=low(carrsetin) to high(carrsetin) do
      if varrset[i]<>carrsetin[i] then
        raise jlexception.create('const arrset out');
    venum:=cenumin;
    testformalconst(venum,fenum);
    if venum<>cenumin then
      raise jlexception.create('const enum out');
  end;


procedure main(args: array of string);
begin
  test;
  testformalvars;
  testformalouts;
  testformalconsts;
end;

end.
