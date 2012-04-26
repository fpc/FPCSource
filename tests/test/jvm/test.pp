{$mode delphi}
{$codepage utf-8}

{$namespace org.freepascal.test}

{$j-}

Unit test;

interface

const
  unitintconst = 3;
  unitfloatconst = 2.0;
  unitdoubleconst = 0.1;

const
  tcl: longint = 4;

type
  trec = record
    a,b,c,d,e: longint;
  end;
  
const
  tcrec: trec = (a:1;b:2;c:3;d:4;e:5);

type
  TMyClass = class
   const
    classintconst = 4;
    classfloatconst = 3.0;
    classdoubleconst = 0.3;
    classtcstringconst: unicodestring = 'abcdef';
   class var
    rec: trec;
   var
    intfield: jint;

    staticbytefield: jbyte; static;

    constructor create; overload;
    constructor create(l: longint);overload;
    constructor create(l1, l2: longint);overload;
    function sub(a1, a2: longint): longint;
    function test(l1, l2: longint): longint;
    class function staticmul3(l: longint): longint; static;

    procedure longboolobj(l: jlong; b: boolean; obj: tobject);

    procedure setintfield(l: jint);
    function getintfield: jint;
    property propintfield: jint read getintfield write setintfield;
    procedure setstaticbytefield(b: byte);
    function getstaticbytefield: byte;

    class procedure setstaticbytefieldstatic(b: byte); static;
    class function getstaticbytefieldstatic: byte; static;

    class procedure settestglobal(l: longint); static;
    class function gettestglobal: longint; static;
  end;

  tisinterface = interface
  end;
  tisclassbase = class
    procedure abstr; virtual; abstract;
  end;
  tisclassbase2 = class(tisclassbase)
  end;
  tisclass1 = class(tisclassbase2)
    type
      tisclass1nested = class(tisinterface)
        var
          anonrec: record c: char; end;
        type
          tisclass1nestedl2 = class
            anonrec: record l: longint; end;
            constructor create;
            function testl2: jint;
          end;
        constructor create;
        function testl1: jint;
      end;
    constructor create;
    procedure abstr; override;
  end;
  
  tisclass1ref = class of tisclass1;

type
  tnestrec = record
    r: trec;
    arr: array[3..4] of byte;
  end;

const
  tcnestrec: tnestrec = (r:(a:1;b:2;c:3;d:4;e:5);arr:(7,6));

var
  anonrec: record s: string; end;

function testset: jint;
function testloop: longint;
function testfloat: jint;
function testcnvint1: longint;
function testint2real: longint;
function TestCmpListOneShort: longint;
function TestCmpListTwoShort: longint;
function TestCmpListOneWord: longint;
function TestCmpListTwoWord: longint;
function TestCmpListOneInt64: longint;
function TestCmpListTwoInt64: longint;
function TestCmpListThreeInt64: longint;
function TestCmpListRangesOneShort: longint;
function TestCmpListRangesTwoShort: longint;
function TestCmpListRangesOneWord: longint;
function TestCmpListRangesTwoWord: longint;
function TestCmpListRangesThreeWord: longint;
function TestCmpListRangesOneInt64: longint;
function TestCmpListRangesTwoInt64: longint;
function testsqr: longint;
function testtrunc: longint;
function testdynarr: longint;
function testdynarr2: longint;
function testbitcastintfloat: jint;
function testis: longint;
function testneg: longint;
function testtry1: longint;
function testtry2: longint;
function testtryfinally1: longint;
function testtryfinally2: longint;
function testtryfinally3: longint;
function testsmallarr1: longint;
function testopenarr1: longint;
function testopenarr2: longint;
function testopenarr3: longint;
function testopendynarr: longint;
function testsmallarr2: longint;
function testsmallarr3: longint;
function testsmallarr4: longint;

function testrec1: longint;
function testopenarr1rec: longint;
function testrec2: longint;


function testunicodestring: JLString;
function testunicodestring2: JLString;
function testunicodestring3(a: unicodestring): unicodestring;
function testunicodestring4(a: unicodestring): unicodestring;
function testunicodestring5: unicodestring;
function testunicodestring6: unicodestring;
function testunicodestring7: unicodestring;

procedure main(const args: array of string);


var
  myrec: trec;

implementation

uses
  {$ifdef java}jdk15{$else}androidr14{$endif};

{ package visibility }
var
  testglobal: jint;

var
  funkyl: longint;

function funky: longint;
  begin
    result:=funkyl;
    inc(funkyl);
  end;


function testset: jint;
var
  s,s2: set of 0..31;
  c1, c2: cardinal;
const
  exit1: jint = 1;
begin
  result:=0;
  s:=[3..6];
  s:=s+[10..20];
  if not([3..4]<=s) then
    exit(exit1);
  s:=s-[15..20];
  s2:=[15..20];
  if s2<=s then
    exit(2);
  s:=s+s2;
  if not(s2<=s) then
    exit(3);
  if s<=s2 then
    exit(4);
  c1:=1234;
  c2:=c1 mod 5;
  if c2<>4 then
    exit(5);
end;

function testloop: longint;
var
  i,j: longint;
const
  exit1: jint = 1;
begin
  result:=0;
  i:=0;
  while i<10 do
    i:=i+1;
  if i<>10 then
    exit(exit1);

  i:=0;
  repeat
    i:=i+5;
  until i=20;
  if (i<20) or
     (i>20)  then
    exit(2);

  j:=0;
  for i:=1 to 10 do
    j:=j+i;
  if (j<(i*(i+1) div 2)) or
    (j>(i*(i+1) div 2)) then
   exit(3);
end;

function testfloat: jint;
var
  s1, s2: single;
  d1, d2: double;
begin
  result:=0;
  s1:=0.5;
  s1:=s1+1.5;
  s2:=2.0;
  if (s1 < s2) or
     (s1 > s2) or
     (s1 <> s2) then
    exit(1);
  s1:=s1+s2;
  if s1<>4.0 then
    exit(2);
  s1:=s1-s2;
  if s1<>s2 then
    exit(3);
  s1:=s1*s2;
  if s1<>4.0 then
    exit(4);
  s1:=s1/s2;
  if s1<>s2 then
    exit(5);

  d1:=0.5;
  d1:=d1+1.5;
  d2:=2.0;
  if (d1 < d2) or
     (d1 > d2) or
     (d1 <> d2) then
    exit(6);
  d1:=d1+d2;
  if d1<>4.0 then
    exit(7);
  d1:=d1-d2;
  if d1<>d2 then
    exit(8);
  d1:=d1*d2;
  if d1<>4.0 then
    exit(9);
  d1:=d1/d2;
  if d1<>d2 then
    exit(10);
end;
     
function testcnvint1: longint;
var
 tobyte : byte;
 toword : word;
 tolong : longint;
{$ifndef tp}
 toint64 : int64;
{$endif}
 b1  : boolean;
 bb1 : bytebool;
 wb1 : wordbool;
 lb1 : longbool;
 b2  : boolean;
 bb2 : bytebool;
 wb2 : wordbool;
 lb2 : longbool;
begin
 result:=0;
 { left : LOC_REGISTER  }
 { from : LOC_REFERENCE/LOC_REGISTER }
 b1 := TRUE;
 tobyte := byte(b1);
 if tobyte <> 1 then 
   exit(1);
 b1 := FALSE;
 tobyte := byte(b1);
 if tobyte <> 0 then 
   exit(2);
 b1 := TRUE;
 toword := word(b1);
 if toword <> 1 then 
   exit(3);
 b1 := FALSE;
 toword := word(b1);
 if toword <> 0 then 
   exit(4);
 b1 := TRUE;
 tolong := longint(b1);
 if tolong <> 1 then 
   exit(5);
 b1 := FALSE;
 tolong := longint(b1);
 if tolong <> 0 then 
   exit(6);
 bb1 := TRUE;
 tobyte := byte(bb1);
 if tobyte <> 255 then 
   exit(7);
 bb1 := FALSE;
 tobyte := byte(bb1);
 if tobyte <> 0 then 
   exit(8);
 bb1 := TRUE;
 toword := word(bb1);
 if toword <> 65535 then 
   exit(9);
 bb1 := FALSE;
 toword := word(bb1);
 if toword <> 0 then 
   exit(10);
 bb1 := TRUE;
 tolong := longint(bb1);
 if tolong <> -1 then 
   exit(11);
 bb1 := FALSE;
 tolong := longint(bb1);
 if tolong <> 0 then 
   exit(12);
 wb1 := TRUE;
 tobyte := byte(wb1);
 if tobyte <> 255 then 
   exit(13);
 wb1 := FALSE;
 tobyte := byte(wb1);
 if tobyte <> 0 then 
   exit(14);
 wb1 := TRUE;
 toword := word(wb1);
 if toword <> 65535 then 
   exit(15);
 wb1 := FALSE;
 toword := word(wb1);
 if toword <> 0 then 
   exit(16);
 wb1 := TRUE;
 tolong := longint(wb1);
 if tolong <> -1 then 
   exit(17);
 wb1 := FALSE;
 tolong := longint(wb1);
 if tolong <> 0 then 
   exit(18);
{$ifndef tp}
 b1 := TRUE;
 toint64 :=int64(b1);
 if toint64 <> 1 then 
   exit(19);
 b1 := FALSE;
 toint64 :=int64(b1);
 if toint64 <> 0 then 
   exit(20);
 bb1 := TRUE;
 toint64 :=int64(bb1);
 if toint64 <> -1 then 
   exit(21);
 bb1 := FALSE;
 toint64 :=int64(bb1);
 if toint64 <> 0 then 
   exit(22);
 wb1 := TRUE;
 toint64 :=int64(wb1);
 if toint64 <> -1 then 
   exit(23);
 wb1 := FALSE;
 toint64 :=int64(wb1);
 if toint64 <> 0 then 
   exit(24);
{$endif}
 lb1 := TRUE;
 tobyte := byte(lb1);
 if tobyte <> 255 then 
   exit(25);
 lb1 := FALSE;
 tobyte := byte(lb1);
 if tobyte <> 0 then 
   exit(26);
 lb1 := TRUE;
 toword := word(lb1);
 if toword <> 65535 then 
   exit(27);
 lb1 := FALSE;
 toword := word(lb1);
 if toword <> 0 then 
   exit(28);
 lb1 := TRUE;
 tolong := longint(lb1);
 if tolong <> -1 then 
   exit(29);
 lb1 := FALSE;
 tolong := longint(lb1);
 if tolong <> 0 then 
   exit(30);
 { left : LOC_REGISTER }
 { from : LOC_REFERENCE }
 wb1 := TRUE;
 b2 := wb1;
 if not b2 then 
   exit(31);
 wb1 := FALSE;
 b2 := wb1;
 if b2 then 
   exit(32);
 lb1 := TRUE;
 b2 := lb1;
 if not b2 then 
   exit(33);
 lb1 := FALSE;
 b2 := lb1;
 if b2 then 
   exit(34);

 wb1 := TRUE;
 bb2 := wb1;
 if not bb2 then 
   exit(35);
 wb1 := FALSE;
 bb2 := wb1;
 if bb2 then 
   exit(36);
 lb1 := TRUE;
 bb2 := lb1;
 if not bb2 then 
   exit(37);
 lb1 := FALSE;
 bb2 := lb1;
 if bb2 then 
   exit(38);
 b1 := TRUE;
 lb2 := b1;
 if not lb2 then 
   exit(39);
 b1 := FALSE;
 lb2 := b1;
 if lb2 then 
   exit(40);
 bb1 := TRUE;
 lb2 := bb1;
 if not lb2 then 
   exit(41);
 bb1 := FALSE;
 lb2 := bb1;
 if lb2 then 
   exit(42);
 { left : LOC_REGISTER }
 { from : LOC_JUMP     }
 toword := 0;
 tobyte := 1;
 tobyte:=byte(toword > tobyte);
 if tobyte <> 0 then 
   exit(43);
 toword := 2;
 tobyte := 1;
 tobyte:=byte(toword > tobyte);
 if tobyte <> 1 then 
   exit(44);
 toword := 0;
 tobyte := 1;
 toword:=word(toword > tobyte);
 if toword <> 0 then 
   exit(45);
 toword := 2;
 tobyte := 1;
 toword:=word(toword > tobyte);
 if toword <> 1 then 
   exit(46);
 toword := 0;
 tobyte := 1;
 tolong:=longint(toword > tobyte);
 if tolong <> 0 then 
   exit(47);
 toword := 2;
 tobyte := 1;
 tolong:=longint(toword > tobyte);
 if tolong <> 1 then 
   exit(48);
{$ifndef tp}
 toword := 0;
 tobyte := 1;
 toint64:=int64(toword > tobyte);
 if toint64 <> 0 then 
   exit(49);
 toword := 2;
 tobyte := 1;
 toint64:=int64(toword > tobyte);
 if toint64 <> 1 then 
   exit(50);
{$endif}
 { left : LOC_REGISTER }
 { from : LOC_FLAGS     }
 wb1 := TRUE;
 bb1 := FALSE;
 bb1 := (wb1 <> bb1);
 if not bb1 then 
   exit(51);
 wb1 := FALSE;
 bb1 := FALSE;
 bb1 := (wb1 <> bb1);
 if bb1 then 
   exit(52);
 lb1 := TRUE;
 bb1 := FALSE;
 bb1 := (bb1 = lb1);
 if bb1 then 
   exit(53);
 lb1 := FALSE;
 bb1 := TRUE;
 bb1 := (bb1 <> lb1);
 if not bb1 then 
   exit(54);
 lb1 := TRUE;
 bb1 := FALSE;
 wb1 := (bb1 = lb1);
 if wb1 then 
   exit(55);
 lb1 := TRUE;
 bb1 := TRUE;
 wb1 := (bb1 = lb1);
 if not wb1 then 
   exit(56);
 lb1 := TRUE;
 bb1 := FALSE;
 lb1 := (bb1 = lb1);
 if lb1 then 
   exit(57);
 lb1 := FALSE;
 bb1 := FALSE;
 lb1 := (bb1 = lb1);
 if not lb1 then 
   exit(58);
 bb1 := TRUE;
 bb2 := FALSE;
 lb1 := (bb1 <> bb2);
 if not lb1 then 
   exit(59);
 bb1 := FALSE;
 bb2 := TRUE;
 lb1 := (bb1 = bb2);
 if lb1 then 
   exit(60);
end;

function testint2real: longint;
var
  l: longint;
  c: cardinal;
  i: int64;
  q: qword;
  s: single;
  d: double;
begin
  result:=0;
  l:=-12345;
  c:=high(longint)+33;
  i:=-56789;
  q:=qword(high(int64))+48;

  s:=l;
  if s<>-12345 then
    exit(1);
  s:=c;
  if s<>high(longint)+33 then
    exit(2);
  s:=i;
  if s<>-56789 then
    exit(3);
  s:=q;
  if s<>qword(high(int64))+48 then
    exit(4);
  
  l:=-12345;
  c:=high(longint)+33;
  i:=-56789;
  q:=qword(high(int64))+48;

  d:=l;
  if d<>-12345 then
    exit(5);
  d:=c;
  if d<>high(longint)+33 then
    exit(6);
  d:=i;
  if d<>-56789 then
    exit(7);
  d:=q;
  if d<>qword(high(int64))+48 then
    exit(8);


  l:=123456789;
  c:=987654321;
  i:=high(cardinal)+12345;
  q:=12345;

  s:=l;
  if s<>123456789 then
    exit(11);
  s:=c;
  if s<>987654321 then
    exit(12);
  s:=i;
  if s<>high(cardinal)+12345 then
    exit(13);
  s:=q;
  if s<>12345 then
    exit(14);

  l:=123456789;
  c:=987654321;
  i:=high(cardinal)+12345;
  q:=12345;

  d:=l;
  if d<>123456789 then
    exit(16);
  d:=c;
  if d<>987654321 then
    exit(17);
  d:=i;
  if d<>high(cardinal)+12345 then
    exit(18);
  d:=q;
  if d<>12345 then
    exit(19);
end;

{   low = high           }
function TestCmpListOneShort: longint;
 var
  s: smallint;
  failed :boolean;
 begin
   s := -12;
   failed := true;
   case s of
   -12 : failed := false;
   -10 : ;
   3 : ;
   else
   end;
   if failed then
     result:=1
   else
     result:=0;
 end;

{   low = high           }
function TestCmpListTwoShort: longint;
 var
  s: smallint;
  failed :boolean;
 begin
   s := 30000;
   failed := true;
   case s of
   -12 : ;
   -10 : ;
   3 : ;
   else
     failed := false;
   end;
   if failed then
     result:=1
   else
     result:=0;
 end;


{   low = high           }
function TestCmpListOneWord: longint;
 var
  s: word;
  failed :boolean;
 begin
   s := 12;
   failed := true;
   case s of
   12 : failed := false;
   10 : ;
   3 : ;
   end;
   if failed then
     result:=1
   else
     result:=0;
 end;

{   low = high           }
function TestCmpListTwoWord: longint;
 var
  s: word;
  failed :boolean;
 begin
   s := 30000;
   failed := true;
   case s of
   0 : ;
   512 : ;
   3 : ;
   else
     failed := false;
   end;
   if failed then
     result:=1
   else
     result:=0;
 end;

{   low = high           }
function TestCmpListOneInt64: longint;
 var
  s: int64;
  failed :boolean;
 begin
   s := 3000000;
   failed := true;
   case s of
   3000000 : failed := false;
   10 : ;
   3 : ;
   end;
   if failed then
     result:=1
   else
     result:=0;
 end;

{   low = high           }
function TestCmpListTwoInt64: longint;
 var
  s: int64;
  failed :boolean;
 begin
   s := 30000;
   failed := true;
   case s of
   0 : ;
   512 : ;
   3 : ;
   else
     failed := false;
   end;
   if failed then
     result:=1
   else
     result:=0;
 end;

 {   low = high           }
 function TestCmpListThreeInt64: longint;
  var
   s: int64;
   l : longint;
   failed :boolean;
  begin
    l:=3000000;
    s := (int64(l) shl 32);
    failed := true;
    case s of
    (int64(3000000) shl 32) : failed := false;
    10 : ;
    3 : ;
    end;
    if failed then
      result:=1
    else
      result:=0;
  end;


function TestCmpListRangesOneShort: longint;
 var
  s: smallint;
  failed :boolean;
 begin
   s := -12;
   failed := true;
   case s of
   -12..-8 : failed := false;
   -7 : ;
   3 : ;
   else
   end;
   if failed then
     result:=1
   else
     result:=0;
 end;

function TestCmpListRangesTwoShort: longint;
 var
  s: smallint;
  failed :boolean;
 begin
   s := 30000;
   failed := true;
   case s of
   -12..-8 : ;
   -7 : ;
   3 : ;
   else
     failed := false;
   end;
   if failed then
     result:=1
   else
     result:=0;
 end;


{   low = high           }
function TestCmpListRangesOneWord: longint;
 var
  s: word;
  failed :boolean;
 begin
   s := 12;
   failed := true;
   case s of
   12..13 : failed := false;
   10 : ;
   3..7 : ;
   end;
   if failed then
     result:=1
   else
     result:=0;
 end;

{   low = high           }
function TestCmpListRangesTwoWord: longint;
 var
  s: word;
  failed :boolean;
 begin
   s := 30000;
   failed := true;
   case s of
   0..2 : ;
   3..29999 : ;
   else
     failed := false;
   end;
   if failed then
     result:=1
   else
     result:=0;
 end;


 function TestCmpListRangesThreeWord: longint;
  var
   s: word;
   failed :boolean;
  begin
    s := 3;
    failed := true;
    case s of
    12..13 : ;
    10 : ;
    3..7 : failed := false;
    end;
    if failed then
      result:=1
    else
      result:=0;
  end;


{   low = high           }
function TestCmpListRangesOneInt64: longint;
 var
  s: int64;
  failed :boolean;
 begin
   s := 3000000;
   failed := true;
   case s of
   11..3000000 : failed := false;
   10 : ;
   0..2 : ;
   end;
   if failed then
     result:=1
   else
     result:=0;
 end;

{   low = high           }
function TestCmpListRangesTwoInt64: longint;
 var
  s: int64;
  failed :boolean;
 begin
   s := 30000;
   failed := true;
   case s of
   513..10000 : ;
   512 : ;
   0..3 : ;
   else
     failed := false;
   end;
   if failed then
     result:=1
   else
     result:=0;
 end;

function testsqr: longint;
  var
    s1, s2: single;
    d1, d2: double;
  begin
    result:=0;
    s1:=25.0;
    s2:=sqr(s1);
    if s2<>625.0 then
      exit(1);
    d2:=sqr(s1);
    if d2<>625.0 then
      exit(2);
    d1:=7.0;
    d2:=sqr(d1);
    if d2<>49.0 then
      exit(3);
    d2:=sqr(d1);
    if d2<>49.0 then
      exit(4);
  end;

function testtrunc: longint;
  var
    s1: single;
    d1: double;
    l: longint;
    i: int64;
  begin
    result:=0;
    s1:=123.99;
    l:=trunc(s1);
    if l<>123 then
      exit(1);
    i:=trunc(s1);
    if i<>123 then
      exit(2);
    d1:=67533.345923;
    l:=trunc(d1);
    if l<>67533 then
      exit(3);
    i:=trunc(d1);
    if i<>67533 then
      exit(4);
  end;

function testdynarr: longint;
  type
    TReal1DArray        = array of Double;
    TReal2DArray        = array of array of Double;
  var
    MaxMN : Integer;
    PassCount : Integer;
    Threshold : Double;
    AEffective : TReal2DArray;
    AParam : TReal2DArray;
    XE : TReal1DArray;
    B : TReal1DArray;
    N : Integer;
    Pass : Integer;
    I : Integer;
    J : Integer;
    CntS : Integer;
    CntU : Integer;
    CntT : Integer;
    CntM : Integer;
    WasErrors : Boolean;
    IsUpper : Boolean;
    IsTrans : Boolean;
    IsUnit : Boolean;
    V : Double;
    S : Double;
  begin    
    SetLength(AEffective, 2, 2);           // crash occurs at this line
    WasErrors := False;
    MaxMN := 10;
    PassCount := 5;
    N:=2;
    isupper:=false;
    isunit:=true;
    istrans:=false;
    while N<=MaxMN do
    begin
        for i:=low(aeffective) to pred(length(aeffective)) do
          for j:=low(aeffective[i]) to pred(length(aeffective[i])) do
            aeffective[i,j]:=i*10+j;
        SetLength(AEffective, N+1, N+1);
        for i:=low(aeffective) to pred(length(aeffective))-1 do
          for j:=low(aeffective[i]) to pred(length(aeffective[i]))-1 do
            if aeffective[i,j]<>i*10+j then
              begin
                result:=-1;
                exit;
              end;
        for i:=low(aeffective) to pred(length(aeffective))-1 do
          if aeffective[i,pred(length(aeffective[i]))]<>0 then
            begin
              result:=-2;
              exit;
            end;
        Inc(N);
    end;
    { check shallow copy }
    AParam:=aeffective;
    aeffective[1,1]:=123;
    if AParam[1,1]<>123 then
      exit(-3);
    result:=0;
  end;


function testdynarr2: longint;
  type
    tstaticarr = array[0..1] of longint;
    tstaticarr2 = array[0..1] of array of array of longint;
  var
    a,b: array of array of tstaticarr;
    c,d: tstaticarr2;
    w: word;
    arrb: array of byte;
    arrc: array of char;
    arrw: array of word;
    arrwc: array of unicodechar;
    arrd: array of dword;
    arrq: array of qword;
    arra: array of ansistring;
    arrs: array of shortstring;
  begin
    setlength(a,2,2);
    a[0,0,0]:=1;
    b:=a;
    a[0,0,1]:=1;
    funkyl:=1;
    setlength(a[funky],35);
    if b[0,0,0]<>1 then
      exit(1);
    if b[0,0,1]<>1 then
      exit(2);
    if length(b[1])<>35 then
      exit(3);
    setlength(c[0],2,2);
    d:=c;
    c[0,0,0]:=1;
    setlength(c[1],42);
    if d[0,0,0]<>1 then
      exit(4);
    if length(d[1])<>0 then
      exit(5);
    b[1,0,0]:=555;
    a:=copy(b,1,1);
    if length(a)<>1 then
      exit(6);
    if a[0,0,0]<>555 then
      exit(7);
    
    setlength(arrb,4);
    if length(arrb)<>4 then
      exit(8);
    for w:=low(arrb) to high(arrb) do
      if arrb[w]<>0 then
        exit(9);
    
    setlength(arrc,32);
    if length(arrc)<>32 then
      exit(10);
    for w:=low(arrc) to high(arrc) do
      if arrc[w]<>#0 then
        exit(11);

    setlength(arrw,666);
    if length(arrw)<>666 then
      exit(11);
    for w:=low(arrw) to high(arrw) do
      if arrw[w]<>0 then
        exit(12);

    setlength(arrwc,12346);
    if length(arrwc)<>12346 then
      exit(13);
    for w:=low(arrwc) to high(arrwc) do
      if arrwc[w]<>#0 then
        exit(14);

    setlength(arrd,20000);
    if length(arrd)<>20000 then
      exit(15);
    for w:=low(arrd) to high(arrd) do
      if arrd[w]<>0 then
        exit(16);

    setlength(arrq,21532);
    if length(arrq)<>21532 then
      exit(17);
    for w:=low(arrq) to high(arrq) do
      if arrq[w]<>0 then
        exit(18);

    setlength(arra,21533);
    if length(arra)<>21533 then
      exit(19);
    for w:=low(arra) to high(arra) do
      if arra[w]<>'' then
        exit(20);

    setlength(arrs,21534);
    if length(arrs)<>21534 then
      exit(21);
    for w:=low(arrs) to high(arrs) do
      if arrs[w]<>'' then
        exit(12);

    result:=0;
  end;


function testbitcastintfloat: jint;
var
  f: jfloat;
  d: jdouble;
  i: jint;
  l: jlong;
begin
  result:=-1;
  f:=123.125;
  i:=jint(f);
  f:=1.0;
  f:=jfloat(i);
  if f<>123.125 then
    exit;

  result:=-2;
  d:=9876.0625;
  l:=jlong(d);
  d:=1.0;
  d:=jdouble(l);
  if d<>9876.0625 then
    exit;
  result:=0;
end;

{ ********************** Is test  ******************** }

type
 tisclass2 = class(tisclass1)
   constructor create;
 end;
 
 constructor tisclass1.create;
   begin
   end;
   
 constructor tisclass1.tisclass1nested.create;
   begin
     anonrec.c:='x';
   end;
   
 function tisclass1.tisclass1nested.testl1: jint;
   begin
     if anonrec.c='x' then
       result:=12345
     else
       result:=-1;
   end;
   
 constructor tisclass1.tisclass1nested.tisclass1nestedl2.create;
   begin
     anonrec.l:=961;
   end;
   
 function tisclass1.tisclass1nested.tisclass1nestedl2.testl2: jint;
   begin
     if anonrec.l=961 then
       result:=42
    else
      result:=-1;
   end;
   
 procedure tisclass1.abstr;
   begin
   end;
   
   
 constructor tisclass2.create;
   begin
   end;
   
   
function testispara(cref: tisclass1ref): longint;
begin
  if cref<>tisclass2 then
    result:=14;
  result:=0;
end;

function testis: longint;
var
 myclass1 : tisclass1;
 myclass2 : tisclass2;
 nested1  : tisclass1.tisclass1nested;
 nested2  : tisclass1.tisclass1nested.tisclass1nestedl2;
 myclassref : tisclass1ref;
begin
  { create class instance }
  myclass1:=tisclass1.create;
  myclass2:=tisclass2.create;
  {if myclass1 is tisclass1 }
  if not(myclass1 is tisclass1) then
    exit(1);
  if (myclass1 is tisclass2) then
    exit(2);
  if not (myclass2 is tisclass2) then
    exit(3);
  if (myclass1 is tisclass2) then
    exit(4);
    
  nested1:=tisclass1.tisclass1nested.create;
  nested2:=tisclass1.tisclass1nested.tisclass1nestedl2.create;
  if not(nested1 is tisclass1.tisclass1nested) then
    exit(5);
  if nested1.testl1<>12345 then
    exit(6);
  if not(nested2 is tisclass2.tisclass1nested.tisclass1nestedl2) then
    exit(7);
  if nested2.testl2<>42 then
    exit(8);

    
{$ifndef oldcomp}
  myclassref:=tisclass1;
  if not(myclass1 is myclassref) then
    exit(10);
  if not(myclass2 is myclassref) then
    exit(11);

  myclassref:=tisclass2;
  if (myclass1 is myclassref) then
    exit(12);
  if not(myclass2 is myclassref) then
    exit(13);
    
  myclass1:=myclass2;
  myclass1.abstr;
  myclass2:=tisclass2(myclass1 as myclassref);

  result:=testispara(tisclass2);
  if result<>0 then
    exit(14);
  
  if not(nested1 is tisinterface) then
    exit(15);
    
  if nested2 is tisinterface then
    exit(16);
  
{$endif}

  result:=0;
end;

function testneg: longint;
var
  b: shortint;
  l: longint;
  i: int64;
  s: single;
  d: double;
begin
  b:=1;
  b:=-b;
  if b<>-1 then
    exit(1);
  l:=-1234567;
  l:=-l;
  if l<>1234567 then
    exit(2);
  i:=-123456789012345;
  i:=-i;
  if i<>123456789012345 then
    exit(3);
  s:=123.5;
  s:=-s;
  if s<>-123.5 then
    exit(4);
  d:=-4567.78;
  d:=-d;
  if d<>4567.78 then
    exit(5);
  result:=0;
end;



{ ******************** End Is test  ****************** }

{ ****************** Exception test  ***************** }

function testtry1: longint;
  begin
    result:=-1;
    try
      raise JLException.create;
    except
      result:=0;
    end;
  end;

function testtry2: longint;
  begin
    result:=-1;
    try
      raise JLException.create;
    except
      on JLException do
        result:=0;
      else
        result:=-2
    end;
    if result<>0 then
      exit;
    result:=-3;
    try
      try
        raise JLException.create;
      except
        result:=-4;
        raise
      end;
    except
      on JLException do
        if result=-4 then
          result:=0;
    end;
  end;

function testtryfinally1: longint;
  begin
    result:=-1;
    try
      try
        try
          raise JLException.create;
        except
          on JLException do
            begin
              result:=1;
              raise;
            end
          else
            result:=-2
        end;
      finally
        if result=1 then
          result:=0;
      end;
    except
      on JLException do
        if result<>0 then
          raise
    end;
  end;

function testtryfinally2: longint;
var
  i,j: longint;
  check1, check2: byte;
begin
  j:=0;
  check1:=0;
  check2:=0;
  result:=-1;
  try
    for i:=1 to 10 do
      try
        inc(j);
        if j=1 then
          begin
            inc(check1);
            continue;
          end;
        if j=2 then
          begin
            inc(check2);
            break;
          end;
      finally
        if j=1 then
          inc(check1);
        if j=2 then
          inc(check2);
      end;
  finally
    if check1<>2 then
      result:=-1
    else if check2<>2 then
      result:=-2
    else if j<>2 then
      result:=-3
    else
      result:=0;
  end;
end;

function testtryfinally3: longint;
var
  i,j: longint;
  check1, check2: byte;
begin
  j:=0;
  check1:=0;
  check2:=0;
  result:=-1;
  try
    for i:=1 to 10 do
      try
        inc(j);
        if j=1 then
          begin
            inc(check1);
            continue;
          end;
        if j=2 then
          begin
            inc(check2);
            exit;
          end;
      finally
        if j=1 then
          inc(check1);
        if j=2 then
          inc(check2);
      end;
  finally
    if check1<>2 then
      result:=-10
    else if check2<>2 then
      result:=-20
    else if j<>2 then
      result:=-30
    else
      result:=0;
  end;
end;


{ **************** End Exception test  *************** }

{ **************** Begin array test  *************** }

function testsmallarr1: longint;
  type
    tarr = array[4..6] of longint;
  var
    a1,a2: tarr;
    a3,a4: array[1..2,3..5] of tarr;
    i,j,k: longint;
  begin
    a1[4]:=1;
    a1[5]:=2;
    a1[6]:=3;
    { plain copy }
    a2:=a1;
    if (a2[4]<>1) or
       (a2[5]<>2) or
       (a2[6]<>3) then
      exit(1);
    { has to be deep copy }
    a1[5]:=255;
    if a2[5]<>2 then
      exit(2);
    { copy to multi-dim array }
    a3[1,4]:=a1;
    if (a3[1,4,4]<>1) or
       (a3[1,4,5]<>255) or
       (a3[1,4,6]<>3) then
      exit(3);
   
    i:=2;
    j:=3;
    a1[4]:=38;
    a1[5]:=39;
    a1[6]:=40;
    { copy to multi-dim array }
    a3[i,j]:=a1;
    if (a3[i,j,4]<>38) or
       (a3[i,j,5]<>39) or
       (a3[i,j,6]<>40) then
      exit(4);
      
    { copy multi-dim array to multi-dim array }
    a4:=a3;
    { check for deep copy }
    for i:=low(a3) to high(a3) do
      for j:=low(a3[i]) to high(a3[i]) do
        for k:=low(a3[i,j]) to high(a3[i,j]) do
          a3[i,j,k]:=-1;
    
    if (a4[1,4,4]<>1) or
       (a4[1,4,5]<>255) or
       (a4[1,4,6]<>3) then
      exit(5);
    i:=2;
    j:=3;
    if (a4[i,j,4]<>38) or
       (a4[i,j,5]<>39) or
       (a4[i,j,6]<>40) then
      exit(6);

    result:=0;
  end;


function testopenarrval(a1: longint; arr: array of jfloat; a2: longint): longint;
  var
    i: longint;
  begin
    result:=a1+length(arr)+trunc(arr[high(arr)])+a2;
    for i:=low(arr) to high(arr) do
      arr[i]:=1.0;
  end;
  
function testopenarrconst(a1: longint; const arr: array of jfloat; a2: longint): longint;
  begin
    result:=a1+length(arr)+trunc(arr[high(arr)])+a2;
  end;

function testopenarrvar(a1: longint; var arr: array of jfloat; a2: longint): longint;
  begin
    result:=a1+length(arr)+trunc(arr[high(arr)])+a2;
    arr[0]:=3.0;
  end;

function testopenarr1: longint;
  var
    arr: array[4..10] of jfloat;
    i: longint;
  begin
    result:=0;
    arr[10]:=2.0;
    if testopenarrval(1,arr,3)<>13 then
      exit(1);
    for i:=4 to 9 do
      if arr[i]<>0.0 then
        exit(2);
    if arr[10]<>2.0 then
      exit(3);
      
    if testopenarrconst(2,arr,4)<>15 then
      exit(4);
    if testopenarrvar(3,arr,5)<>17 then
      exit(5);
    if arr[4]<>3.0 then
      exit(6);
  end;

type
  tarrdynarr = array[1..10,1..4] of array of array of byte;
function testoutopenarrdyn(out arr: array of tarrdynarr): longint;
  var
    i, j, k: longint;
  begin
    for i:=low(arr) to high(arr) do
      for j:=low(arr[i]) to high(arr[i]) do
        for k:=low(arr[i][j]) to high(arr[i][j]) do
          begin
            if length(arr[i][j,k])<>0 then
              exit(-1);
            setlength(arr[i][j,k],j,k);
          end;
    result:=0;
  end;

function testopenarr2: longint;
  var
    arr: array[20..30] of tarrdynarr;
    dynarr: array of tarrdynarr;
    i,j,k: longint;
    barr, barr2: array of byte;
    rarr: array of trec;
    rarr2: array of array of trec;
  begin
    setlength(barr,4);
    barr[1]:=4;
    if barr[1]<>4 then
      exit(-40);
    barr2:=copy(barr);
    if barr2[1]<>4 then
      exit(-50);
    barr2[2]:=48;
    if barr[2]=48 then
      exit(-60);
    setlength(rarr,5);
    rarr[4].a:=135;
    if rarr[4].a<>135 then
      exit(-70);
    setlength(rarr2,4,5);
    rarr2[3,4].b:=124;
    if rarr2[3,4].b<>124 then
      exit(-80);
    for i:=low(arr) to high(arr) do
      for j:=low(arr[i]) to high(arr[i]) do
        for k:=low(arr[i][j]) to high(arr[i][j]) do
          begin
            setlength(arr[i][j,k],20,20);
          end;
    result:=testoutopenarrdyn(arr);
    if result<>0 then
      exit;
    for i:=low(arr) to high(arr) do
      for j:=low(arr[i]) to high(arr[i]) do
        for k:=low(arr[i][j]) to high(arr[i][j]) do
          begin
            if (length(arr[i][j,k])<>j) then
              exit(-2);
            if (length(arr[i][j,k][0])<>k) then
              exit(-3);
            if (length(arr[i][j,k][j-1])<>k) then
              exit(-4);
          end;
    setlength(dynarr,31);
    result:=testoutopenarrdyn(dynarr);
    for i:=low(arr) to high(arr) do
      for j:=low(arr[i]) to high(arr[i]) do
        for k:=low(arr[i][j]) to high(arr[i][j]) do
          begin
            if (length(arr[i][j,k])<>j) then
              exit(-5);
            if (length(arr[i][j,k][0])<>k) then
              exit(-6);
            if (length(arr[i][j,k][j-1])<>k) then
              exit(-7);
          end;
  end;


function testopenarr3: longint;
  var
    arr: array[4..10] of jfloat;
    i: longint;
  begin
    result:=0;
    arr[10]:=2.0;
    if testopenarrval(1,[1.0,2.0,3.0,4.0,5.0,6.0,2.0],3)<>13 then
      exit(1);
      
    if testopenarrconst(2,[1.0,2.0,3.0,4.0,5.0,6.0,7.0],4)<>20 then
      exit(2);
  end;

type
 ByteArray = array of byte;

procedure FillChar(var X: Array of Byte; Count: integer; Value: byte; FirstIndex: integer);
  var
   i: integer;
   y: bytearray;
  begin
   for i := FirstIndex to (FirstIndex + Count) - 1 do
     X[i] := Value;
  end;

function Err : ByteArray;
  begin
   SetLength(Result, 10);
   FillChar(Result, Length(Result)-2, 1, 2);  // !!!!
  end;

function testopendynarr: longint;
  var
    x: bytearray; 
    i: longint;
  begin
    x:=err;
    for i:=0 to 1 do
      if x[i]<>0 then
        exit(1);
    for i:=2 to high(x) do
      if x[i]<>1 then
        exit(2);
    result:=0;
  end;


type
  tdoublearray10 = array[1..10] of jdouble;
  
function testarrval(arr: tdoublearray10): double;
  var
    i: longint;
  begin
    result:=0.0;
    for i:=low(arr) to high(arr) do
      begin
        result:=result+arr[i];
        arr[i]:=-1.0;
      end;
  end;

function testsmallarr2: longint;
  var
    arr: tdoublearray10;
    i: longint;
    barr1,barr2: array[1..2] of byte;
  begin
    result:=0;
    for i:=low(arr) to high(arr) do
      arr[i]:=i;
    if testarrval(arr)<>(10*11 div 2) then
      exit(1);
    for i:=low(arr) to high(arr) do
      if arr[i]<>i then
        exit(2);
    barr1[1]:=1;
    barr1[2]:=2;
    barr2:=barr1;
    if barr2[1]<>1 then
      exit(3);
    if barr2[2]<>2 then
      exit(4);
  end;

type
  tsmall2darr = array[1..10,5..9] of longint;

function smallarr2dfunc: tsmall2darr;
  var
    i, j: longint;
  begin
    for i:=low(result) to high(result) do
      for j:=low(result[i]) to high(result[i]) do
        result[i,j]:=i*(high(result[i])-low(result[i])+1)+(j-low(result[i]));
  end;

function testsmallarr3: longint;
  var
    a: tsmall2darr;
  begin
    a:=smallarr2dfunc;
    if a[1,5]<>5 then
      exit(1);
    if a[2,9]<>14 then
      exit(2);
    result:=0;
  end;

function testoutarrdyn(out arr: tarrdynarr): longint;
  var
    i, j: longint;
  begin
    for i:=low(arr) to high(arr) do
      for j:=low(arr[i]) to high(arr[i]) do
        begin
          if length(arr[i,j])<>0 then
            exit(-1);
          setlength(arr[i,j],i,j);
        end;
    result:=0;
  end;

function testsmallarr4: longint;
  var
    arr: tarrdynarr;
    i,j: longint;
  begin
    for i:=low(arr) to high(arr) do
      for j:=low(arr[i]) to high(arr[i]) do
        begin
          setlength(arr[i,j],20,20);
        end;
    result:=testoutarrdyn(arr);
    if result<>0 then
      exit;
    for i:=low(arr) to high(arr) do
      for j:=low(arr[i]) to high(arr[i]) do
        begin
          if (length(arr[i,j])<>i) then
            exit(-2);
          if (length(arr[i,j][0])<>j) then
            exit(-3);
          if (length(arr[i,j][i-1])<>j) then
            exit(-4);
        end;
  end;
  
function testrec1: longint;
  var
    r1, r2: trec;
  begin
    r1.a:=1;
    r1.b:=2;
    r1.c:=3;
    r1.d:=4;
    r1.e:=5;
    if r1.a<>1 then
      exit(1);
    if r1.b<>2 then
      exit(2);
    if r1.c<>3 then
      exit(3);
    if r1.d<>4 then
      exit(4);
    if r1.e<>5 then
      exit(5);
    r2:=r1;
    if r2.a<>1 then
      exit(6);
    if r2.b<>2 then
      exit(7);
    if r2.c<>3 then
      exit(8);
    if r2.d<>4 then
      exit(9);
    if r2.e<>5 then
      exit(10);
    r2.a:=10;
    if r1.a<>1 then
      exit(11);
    result:=0;
  end;

function testrec2: longint;
  var
    r1, r2: tnestrec;
  begin
    r1:=tcnestrec;
    r1.r.a:=1;
    r1.r.b:=2;
    r1.r.c:=3;
    r1.r.d:=4;
    r1.r.e:=5;
    r1.arr[4]:=6;
    if r1.r.a<>1 then
      exit(1);
    if r1.r.b<>2 then
      exit(2);
    if r1.r.c<>3 then
      exit(3);
    if r1.r.d<>4 then
      exit(4);
    if r1.r.e<>5 then
      exit(5);
    if r1.arr[4]<>6 then
      exit(12);
    r2:=r1;
    if r2.r.a<>1 then
      exit(6);
    if r2.r.b<>2 then
      exit(7);
    if r2.r.c<>3 then
      exit(8);
    if r2.r.d<>4 then
      exit(9);
    if r2.r.e<>5 then
      exit(10);
    if r1.arr[4]<>6 then
      exit(13);
    r2.r.a:=10;
    r2.arr[4]:=7;
    if r1.r.a<>1 then
      exit(11);
    if r1.arr[4]<>6 then
      exit(14);
    anonrec.s:='abcdef';
    if anonrec.s<>'abcdef' then
      exit(15);
    result:=0;
  end;


function testopenarrvalrec(a1: longint; arr: array of trec; a2: longint): longint;
  var
    i: longint;
  begin
    result:=a1+length(arr)+arr[high(arr)].a+a2;
    for i:=low(arr) to high(arr) do
      arr[i].a:=123;
  end;
  
function testopenarrconstrec(a1: longint; const arr: array of trec; a2: longint): longint;
  begin
    result:=a1+length(arr)+arr[high(arr)].b+a2;
  end;

function testopenarrvarrec(a1: longint; var arr: array of trec; a2: longint): longint;
  begin
    result:=a1+length(arr)+arr[high(arr)].c+a2;
    arr[0].d:=987;
  end;

function testopenarr1rec: longint;
  var
    arr: array[4..10] of trec;
    i: longint;
  begin
    result:=0;
    arr[10].a:=2;
    arr[10].b:=2;
    arr[10].c:=2;
    arr[10].d:=2;
    arr[10].e:=2;
    if testopenarrvalrec(1,arr,3)<>13 then
      exit(1);
    for i:=4 to 9 do
      if arr[i].a<>0.0 then
        exit(2);
    if arr[10].a<>2.0 then
      exit(3);
      
    if testopenarrconstrec(2,arr,4)<>15 then
      exit(4);
    if testopenarrvarrec(3,arr,5)<>17 then
      exit(5);
    if arr[4].d<>987 then
      exit(6);
  end;

  
function testunicodestring: JLString;
  var
    s1, s2: unicodestring;
    sarr: array[0..0] of unicodestring;
  begin
    s1:='abc';
    sarr[0]:=s1;
    funkyl:=0;
    if length(sarr[funky])<>3 then
      begin
        result:='';
        exit;
      end;
    s2:=s1;
    s2:='~ê∂êºîƒ~©¬';
    result:=s2;
  end;

function testunicodestring2: JLString;
  begin
    result:='\'#13#10'"';
  end;
  
function testunicodestring3(a: unicodestring): unicodestring;
  begin
    result:=a+'def';
  end;
  
function testunicodestring4(a: unicodestring): unicodestring;
  begin
//    JLSystem.fout.println(JLString('in testunicodestring4'));
//    JLSystem.fout.println(JLString(a));
    result:=a;
//    JLSystem.fout.println(JLString(result));
    result[2]:='x';
//    JLSystem.fout.println(JLString(result));
    result[3]:='2';
//    JLSystem.fout.println(JLString(result));
  end;

function testunicodestring5: unicodestring;
  var
    arr: array[0..3] of ansichar;
    arr2: array[1..5] of ansichar;
    c: ansichar;
    wc: widechar;
  begin
    arr:='abc'#0;
    arr2:='defgh';
    c:='i';
    wc:='j';
    result:=arr+arr2;
    result:=copy(result,1,length(result))+c;
    result:=result+wc;
  end;

function testunicodestring6: unicodestring;
  const
    tcstr: string = 'ab';
  var
    arr: array[0..3] of widechar;
    arr2: array[1..5] of widechar;
    swap: ansichar;
    wc: widechar;
    i: longint;
  begin
    arr:='ab';
    arr2:='cdefg';
    swap:='h';
    wc:='i';
    result:=arr+arr2+swap;
    result:=result+wc;
  end;


function testunicodestring7: unicodestring;
  const
    tcstr: string = 'ab';
  var
    arr: array[0..3] of unicodechar;
    arr2: array[1..5] of unicodechar;
    c: ansichar = 'h';
    wc: unicodechar;
  begin
    funkyl:=1;
    arr:=tcstr;
    arr2:='cdefg';
    wc:='i';
    result:=arr+arr2;
    result:=result+c;
    result:=result+wc;
    result[funky]:='x';
  end;

{ **************** End array test  *************** }


constructor TMyClass.create;
begin
end;


constructor TMyClass.create(l: longint);
var
  dummy: TMyClass;
begin
  dummy:=TMyClass.create;
  create(l,l);
end;

constructor TMyClass.create(l1,l2: longint);
begin
  inherited create;
  propintfield:=4;
  if propintfield<>4 then
    jlsystem.fout.println('WRONG!!!!!!!!!!!!!!!!!!!');
end;

function TMyClass.sub(a1, a2: longint): longint;
begin
  result:=a1-a2;
end;


function TMyClass.test(l1, l2: longint): longint;
var
  locall: longint;
  localsub: TMyClass;
begin
  localsub:=TMyClass.create(1245);
  locall:=localsub.sub(l1,l2);
  result:=locall+1;
  if result>4 then
    result:=-1;
end;

class function tmyclass.staticmul3(l: longint): longint; static;
begin
  result:=l*3;
end;

procedure tmyclass.longboolobj(l: jlong; b: boolean; obj: tobject);
begin
  l:=5;
  b:=true;
  obj:=nil;
end;


procedure tmyclass.setintfield(l: jint);
  const
    xxx: longint = 4;
  begin
    intfield:=l;
    longboolobj(xxx,true,self);
  end;

function tmyclass.getintfield: jint;
  begin
    result:=intfield;
  end;

procedure tmyclass.setstaticbytefield(b: byte);
  begin
    staticbytefield:=b;
    myrec.a:=b;
  end;


function tmyclass.getstaticbytefield: byte;
  begin
    result:=staticbytefield;
  end;


class procedure tmyclass.setstaticbytefieldstatic(b: byte);
  begin
    staticbytefield:=b;
  end;


class function tmyclass.getstaticbytefieldstatic: byte;
  begin
    result:=staticbytefield;
  end;


class procedure tmyclass.settestglobal(l: longint);
  begin
    testglobal:=l;
  end;

class function tmyclass.gettestglobal: longint;
  begin
    result:=testglobal;
  end;

procedure main(const args: array of string);
  begin
    JLSystem.fout.println('This is the entry point');
  end;


begin
  myrec.b:=1234;
  TMyClass.rec.c:=5678;
end.
