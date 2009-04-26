{**********************************************************************

    This file is part of the Free Component Library (FCL)

    Test suite for the xpath.pp unit.
    Largely based on expressions from libxml2 source tree.
    Copyright (c) 2009 by Sergei Gorelkin, sergei_gorelkin@mail.ru

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program xpathts;
{$mode delphi}{$h+}

uses
  Classes, SysUtils, Math,
  dom, xmlread, xmlwrite, xpath;

type
  TResultType = (rtString, rtNumber, rtBool, rtNodeset);

  TTestRec = record
    data: string;              // UTF-8 encoded
    expr: DOMString;
  case rt: TResultType of
    rtString: (s: DOMPChar);   // cannot use DOMString here
    rtNumber: (n: Extended);
    rtBool:   (b: Boolean);
  end;

{$warnings off}
const
  BaseTests: array[0..4] of TTestRec = (
    (expr: '1';           rt: rtNumber; n: 1),
    (expr: '1+2';         rt: rtNumber; n: 3),
    (expr: '2*3';         rt: rtNumber; n: 6),
    (expr: '1+2*3+4';     rt: rtNumber; n: 11),
    (expr: '(1+2)*(3+4)'; rt: rtNumber; n: 21)
  );

  CompareTests: array[0..46] of TTestRec = (
    (expr: '0<0';        rt: rtBool; b: False),
    (expr: '0<=0';       rt: rtBool; b: True),
    (expr: '0>0';        rt: rtBool; b: False),
    (expr: '0>=0';       rt: rtBool; b: True),
    (expr: '0<1';        rt: rtBool; b: True),
    (expr: '0<=1';       rt: rtBool; b: True),
    (expr: '0>1';        rt: rtBool; b: False),
    (expr: '0>=1';       rt: rtBool; b: False),
    (expr: '1<0';        rt: rtBool; b: False),
    (expr: '1<=0';       rt: rtBool; b: False),
    (expr: '1>0';        rt: rtBool; b: True),
    (expr: '1>=0';       rt: rtBool; b: True),
    (expr: '1<1';        rt: rtBool; b: False),
    (expr: '1<=1';       rt: rtBool; b: True),
    (expr: '1>1';        rt: rtBool; b: False),
    (expr: '1>=1';       rt: rtBool; b: True),
    (expr: '0>-0';       rt: rtBool; b: False),
    (expr: '"0"<1';      rt: rtBool; b: True),
    (expr: '"0"<=1';     rt: rtBool; b: True),
    (expr: '"0">1';      rt: rtBool; b: False),
    (expr: '"0">=1';     rt: rtBool; b: False),
    (expr: '0<"1.2"';    rt: rtBool; b: True),
    (expr: '0<="1.2"';   rt: rtBool; b: True),
    (expr: '0>"1.2"';    rt: rtBool; b: False),
    (expr: '0>="1.2"';   rt: rtBool; b: False),
    (expr: '0<"-0.2"';   rt: rtBool; b: False),
    (expr: '0<="-0.2"';  rt: rtBool; b: False),
    (expr: '0>"-0.2"';   rt: rtBool; b: True),
    (expr: '0>="-0.2"';  rt: rtBool; b: True),
    (expr: 'false()<1';  rt: rtBool; b: True),
    (expr: 'false()<=1'; rt: rtBool; b: True),
    (expr: '0>true()';   rt: rtBool; b: False),
    (expr: '0>=true()';  rt: rtBool; b: False),
    (expr: '"a" > "a"';  rt: rtBool; b: False),
    (expr: '"a" > "b"';  rt: rtBool; b: False),
    (expr: '"b" > "a"';  rt: rtBool; b: False),
    (expr: '"a" < "a"';  rt: rtBool; b: False),
    (expr: '"a" < "b"';  rt: rtBool; b: False),
    (expr: '"b" < "a"';  rt: rtBool; b: False),
    (expr: '"a" >= "a"'; rt: rtBool; b: False),
    (expr: '"a" >= "b"'; rt: rtBool; b: False),
    (expr: '"b" >= "a"'; rt: rtBool; b: False),
    (expr: '"a" <= "a"'; rt: rtBool; b: False),
    (expr: '"a" <= "b"'; rt: rtBool; b: False),
    (expr: '"b" <= "a"'; rt: rtBool; b: False),
    (expr: '"a" > "0.0"'; rt: rtBool; b: False),
    (expr: '"a" < "0.0"'; rt: rtBool; b: False)
  );

  nscmp = '<doc>'#10+
    '<j l="12" w="33">first</j>'#10+
    '<j l="17" w="45">second</j>'#10+
    '<j l="16" w="78">third</j>'#10+
    '<j l="12" w="33">fourth</j>'#10+
    '</doc>';

  nscmp2 = '<doc>'#10+
    '<j l="12" w="45">first</j>'#10+
    '<j l="17" w="45">second</j>'#10+
    '<j l="16" w="78">third</j>'#10+
    '<j l="12" w="33">fourth</j>'#10+
    '</doc>';

  simple = '<doc>test</doc>';

  bool58 = '<doc>'#10+
  '<av>'#10+
  ' <a>'#10+
  '   <b>b</b>'#10+
  '   <c>c</c>'#10+
  '   <d>d</d>'#10+
  '   <e>e</e>'#10+
  ' </a>'#10+
  ' <v>'#10+
  '   <w>w</w>'#10+
  '   <x>x</x>'#10+
  '   <y>y</y>'#10+
  '   <z>z</z>'#10+
  ' </v>'#10+
  ' <a>'#10+
  '   <b>fe</b>'#10+
  '   <c>fi</c>'#10+
  '   <d>fo</d>'#10+
  '   <e>fu</e>'#10+
  ' </a>'#10+
  ' <v>'#10+
  '   <w>fee</w>'#10+
  '   <x>fii</x>'#10+
  '   <y>foo</y>'#10+
  '   <z>fom</z>'#10+
  ' </v>'#10+
  ' <j>foo</j>'#10+
  ' <j>foo</j>'#10+
  ' <j>foo</j>'#10+
  ' <j>foo</j>'#10+
  '</av>'#10+
  '</doc>';

  bool84='<doc>'#10+
  '<avj>'#10+
  '  <good>'#10+
  '   <b>12</b>'#10+
  '   <c>34</c>'#10+
  '   <d>56</d>'#10+
  '   <e>78</e>'#10+
  ' </good>'#10+
  '</avj>'#10+
  '</doc>';

  bool85='<doc>'#10+
  '<avj>'#10+
  '  <bool>'#10+
  '   <b>true</b>'#10+
  '   <c></c>'#10+
  '   <d>false?</d>'#10+
  '   <e>1</e>'#10+
  '   <f>0</f>'#10+
  ' </bool>'#10+
  '</avj>'#10+
  '</doc>';

  str04='<doc>'#10+
  '<a>Testing this</a>'#10+
  '<b>and this too</b>'#10+
  '</doc>';
  
  NodesetCompareTests: array[0..38] of TTestRec = (
   { same nodeset }
   (data: nscmp; expr: 'j[@l="12"] = j[@w="33"]'; rt: rtBool; b: True),   // #70
   { disjoint nodesets }
   (data: nscmp; expr: 'j[@l="12"] = j[@l="17"]'; rt: rtBool; b: False),  // #71
   { both have one common node }
   (data: nscmp2; expr: 'j[@l="12"] = j[@w="45"]'; rt: rtBool; b: True),  // #72
   { same nodeset - unequal }
   (data: nscmp; expr: 'j[@l="12"] != j[@w="33"]'; rt: rtBool; b: True),  // #73
   { disjoint - unequal }
   (data: nscmp; expr: 'j[@l="12"] != j[@l="17"]'; rt: rtBool; b: True),  // #74
   { one common node - unequal }
   (data: nscmp2; expr: 'j[@l="12"] != j[@w="45"]'; rt: rtBool; b: True), // #75
   { single common node - unequal }
   (data: nscmp2; expr: 'j[@l="16"] != j[@w="78"]'; rt: rtBool; b: False),// #76
   { nodeset vs. string }
   (data: bool58; expr: '/doc/av//*="foo"';       rt: rtBool; b: True),  // #58.1
   (data: bool58; expr: 'not(/doc/av//*!="foo")'; rt: rtBool; b: False), // #58.2
   (data: bool58; expr: '/doc/av//j="foo"';       rt: rtBool; b: True),  // #58.3
   (data: bool58; expr: 'not(/doc/av//j!="foo")'; rt: rtBool; b: True),  // #58.4
   { empty nodeset vs. string. Data differs, but that doesn't matter }
   (data: bool58; expr: '/doc/avj//k="foo"';      rt: rtBool; b: False), // #59.1
   (data: bool58; expr: 'not(/doc/avj//k="foo")'; rt: rtBool; b: True),  // #59.2
   (data: bool58; expr: '/doc/avj//k!="foo"';     rt: rtBool; b: False), // #59.3
   (data: bool58; expr: 'not(/doc/avj//k!="foo")'; rt: rtBool; b: True), // #59.4
   { nodeset vs. number }
   (data: bool84; expr: '/doc/avj/good/*=34';      rt: rtBool; b: True),  // #84.1
   (data: bool84; expr: 'not(/doc/avj/good/*=34)'; rt: rtBool; b: False), // #84.2
   (data: bool84; expr: '/doc/avj/good/*!=34';     rt: rtBool; b: True),  // #84.3
   (data: bool84; expr: 'not(/doc/avj/good/*!=34)'; rt: rtBool; b: False),// #84.4
   { same with reversed order of operands }
   (data: bool84; expr: '34=/doc/avj/good/*';      rt: rtBool; b: True),  // #84.5
   (data: bool84; expr: 'not(34=/doc/avj/good/*)'; rt: rtBool; b: False), // #84.6
   (data: bool84; expr: '34!=/doc/avj/good/*';     rt: rtBool; b: True),  // #84.7
   (data: bool84; expr: 'not(34!=/doc/avj/good/*)'; rt: rtBool; b: False),// #84.8
   { nodeset vs. boolean }
   (data: bool85; expr: '/doc/avj/bool/*=true()';      rt: rtBool; b: True),  // #85.1
   (data: bool85; expr: 'not(/doc/avj/bool/*=true())'; rt: rtBool; b: False), // #85.2
   (data: bool85; expr: '/doc/avj/bool/*!=true()';     rt: rtBool; b: False), // #85.3
   (data: bool85; expr: 'not(/doc/avj/bool/*!=true())'; rt: rtBool; b: True), // #85.4
   { same with reversed order of operands }
   (data: bool85; expr: 'true()=/doc/avj/bool/*';      rt: rtBool; b: True),  // #85.5
   (data: bool85; expr: 'not(true()=/doc/avj/bool/*)'; rt: rtBool; b: False), // #85.6
   (data: bool85; expr: 'true()!=/doc/avj/bool/*';     rt: rtBool; b: False), // #85.7
   (data: bool85; expr: 'not(true()!=/doc/avj/bool/*)'; rt: rtBool; b: True), // #85.8
   { empty nodeset vs. boolean }
   (data: bool85; expr: '/doc/avj/none/*=true()';      rt: rtBool; b: False), // #86.1
   (data: bool85; expr: 'not(/doc/avj/none/*=true())'; rt: rtBool; b: True),  // #86.2
   (data: bool85; expr: '/doc/avj/none/*!=true()';     rt: rtBool; b: True),  // #86.3
   (data: bool85; expr: 'not(/doc/avj/none/*!=true())'; rt: rtBool; b: False),// #86.4
   { same with reversed order of operands }
   (data: bool85; expr: 'true()=/doc/avj/none/*';      rt: rtBool; b: False), // #86.5
   (data: bool85; expr: 'not(true()=/doc/avj/none/*)'; rt: rtBool; b: True),  // #86.6
   (data: bool85; expr: 'true()!=/doc/avj/none/*';     rt: rtBool; b: True),  // #86.7
   (data: bool85; expr: 'not(true()!=/doc/avj/none/*)'; rt: rtBool; b: False) // #86.8

  );

  EqualityTests: array[0..25] of TTestRec = (
    (expr: '1=1';  rt: rtBool; b: True),
    (expr: '1!=1'; rt: rtBool; b: False),
    (expr: '1=0';  rt: rtBool; b: False),
    (expr: '1!=0'; rt: rtBool; b: True),

    (expr: 'true()=true()';   rt: rtBool; b: True),
    (expr: 'true()!=true()';  rt: rtBool; b: False),
    (expr: 'true()=false()';  rt: rtBool; b: False),
    (expr: 'false()!=true()'; rt: rtBool; b: True),

    (expr: '"test"="test"';   rt: rtBool; b: True),
    (expr: '"test"!="test"';  rt: rtBool; b: False),
    (expr: '"test2"="test"';  rt: rtBool; b: False),
    (expr: '"test2"!="test"'; rt: rtBool; b: True),

    (expr: 'false()=0';   rt: rtBool; b: True),
    (expr: 'false()!=0';  rt: rtBool; b: False),
    (expr: 'false()=1';   rt: rtBool; b: False),

    (expr: 'false()!=1';  rt: rtBool; b: True),
    (expr: '0=true()';    rt: rtBool; b: False),
    (expr: '0!=true()';   rt: rtBool; b: True),
    (expr: '1=true()';    rt: rtBool; b: True),
    (expr: '1!=true()';   rt: rtBool; b: False),

    (expr: 'true()="test"';   rt: rtBool; b: True),
    (expr: 'false()="test"';  rt: rtBool; b: False),
    (expr: '"test"!=true()';  rt: rtBool; b: False),
    (expr: '"test"!=false()'; rt: rtBool; b: True),

    (expr: '"a"=0.0'; rt: rtBool; b: False),
    (expr: '"a"!=0.0'; rt: rtBool; b: True)
  );

  math88='<doc>'+
  '<n0>0</n0>'+
  '<n1>1</n1>'+
  '<n2>2</n2>'+
  '<n3>3</n3>'+
  '<n4>4</n4>'+
  '<n5>5</n5>'+
  '<n6>6</n6>'+
  '<n7>2</n7>'+
  '<n8>6</n8>'+
  '<n9>10</n9>'+
  '<n10>3</n10>'+
  '</doc>';

  math85='<doc>'+
  '<n0>0</n0>'+
  '<n1>1</n1>'+
  '<n2>2</n2>'+
  '<n3>3</n3>'+
  '<n4>4</n4>'+
  '<e>five</e>'+
  '</doc>';

  math80='<doc>'+
  '<n1 attrib="10">5</n1>'+
  '<n2 attrib="4">2</n2>'+
  '<div attrib="-5">-5</div>'+
  '<mod attrib="-2">2</mod>'+
  '</doc>';

  math69='<doc>'+
  '<n-1 attrib="9">3</n-1>'+
  '<n-2 attrib="1">7</n-2>'+
  '</doc>';

  FloatTests: array[0..70] of TTestRec = (
    (expr: '1';        rt: rtNumber; n: 1),
    (expr: '123';      rt: rtNumber; n: 123),
    (expr: '1.23';     rt: rtNumber; n: 1.23),
    (expr: '0.123';    rt: rtNumber; n: 0.123),
    (expr: '4.';       rt: rtNumber; n: 4),
    (expr: '.4';       rt: rtNumber; n: 0.4),
    //(expr: '1.23e3';   rt: rtNumber; n: 1230),
    //(expr: '1.23e-3';  rt: rtNumber; n: 0.00123),
    (expr: '1 div 0';  rt: rtNumber; n: Infinity),
    (expr: '-1 div 0'; rt: rtNumber; n: -Infinity),
    (expr: '0 div 0';  rt: rtNumber; n: NaN),
    (expr: '1 div -0'; rt: rtNumber; n: -Infinity),

    (expr: '(1 div 0) > 0';  rt: rtBool; b: True),
    (expr: '(1 div 0) < 0';  rt: rtBool; b: False),
    (expr: '(-1 div 0) > 0'; rt: rtBool; b: False),
    (expr: '(-1 div 0) < 0'; rt: rtBool; b: True),
    (expr: '(0 div 0) > 0';  rt: rtBool; b: False),
    (expr: '(0 div 0) < 0';  rt: rtBool; b: False),
    (expr: '(1 div -0) > 0'; rt: rtBool; b: False),
    (expr: '(1 div -0) < 0'; rt: rtBool; b: True),
    (expr: '0 div 0 = 0 div 0';  rt: rtBool; b: False),
    (expr: '0 div 0 != 0 div 0'; rt: rtBool; b: True),
    (expr: '0 div 0 > 0 div 0';  rt: rtBool; b: False),
    (expr: '0 div 0 < 0 div 0';  rt: rtBool; b: False),
    (expr: '0 div 0 >= 0 div 0'; rt: rtBool; b: False),
    (expr: '0 div 0 <= 0 div 0'; rt: rtBool; b: False),
    (expr: '1 div 0 = -1 div 0'; rt: rtBool; b: False),
    (expr: '1 div 0 != -1 div 0'; rt: rtBool; b: True),
    (expr: '1 div 0 > -1 div 0';  rt: rtBool; b: True),
    (expr: '1 div 0 < -1 div 0';  rt: rtBool; b: False),
    (expr: '1 div 0 >= -1 div 0'; rt: rtBool; b: True),
    (expr: '1 div 0 <= -1 div 0'; rt: rtBool; b: False),
    (expr: '1 div 0 = 1 div 0';   rt: rtBool; b: True),
    (expr: '1 div 0 != 1 div 0';  rt: rtBool; b: False),
    (expr: '1 div 0 > 1 div 0';   rt: rtBool; b: False),
    (expr: '1 div 0 < 1 div 0';   rt: rtBool; b: False),
    (expr: '1 div 0 >= -1 div 0'; rt: rtBool; b: True),
    (expr: '1 div 0 <= -1 div 0'; rt: rtBool; b: False),
    (expr: '-2 div 0 = -1 div 0'; rt: rtBool; b: True),

    (expr: '1 div floor(0.1)';    rt: rtNumber; n: Infinity),
    (expr: '1 div floor(-0.1)';   rt: rtNumber; n: -1),
    (expr: '1 div floor(-0)';     rt: rtNumber; n: -Infinity),
    (expr: '1 div floor(0)';      rt: rtNumber; n: Infinity),
    (expr: '1 div ceiling(0.1)';  rt: rtNumber; n: 1),
    (expr: '1 div ceiling(-0.1)'; rt: rtNumber; n: -Infinity),
    (expr: '1 div ceiling(-0)';   rt: rtNumber; n: -Infinity),
    (expr: '1 div ceiling(0)';    rt: rtNumber; n: Infinity),
    (expr: '1 div round(0.1)';    rt: rtNumber; n: Infinity),
    (expr: '1 div round(-0.1)';   rt: rtNumber; n: -Infinity),
    (expr: '1 div round(-0)';     rt: rtNumber; n: -Infinity),
    (expr: '1 div round(0)';      rt: rtNumber; n: Infinity),
    (expr: '1 div number("f")';   rt: rtNumber; n: NaN),
    (expr: 'number("f") div 1';   rt: rtNumber; n: NaN),
    (expr: '1 div (1 div 0)';     rt: rtNumber; n: 0),
    (expr: '(1 div 0) div 1';     rt: rtNumber; n: Infinity),
    (expr: '-(1 div 0) div 1';    rt: rtNumber; n: -Infinity),

    (expr: '5 mod 2';     rt: rtNumber; n: 1),
    (expr: '5 mod -2';    rt: rtNumber; n: 1),
    (expr: '-5 mod 2';    rt: rtNumber; n: -1),
    (expr: '-5 mod -2';   rt: rtNumber; n: -1),
    (expr: '2 mod number("xxx")'; rt: rtNumber; n: NaN),
    (expr: 'number("xxx") mod 3'; rt: rtNumber; n: NaN),
    (expr: '8 mod 3 = 2'; rt: rtBool; b: True),
    (data: math88; expr: '(n1*n2*n3*n4*n5*n6)div n7 div n8 div n9 div n10'; rt: rtNumber; n: 2),
    (data: math85; expr: '((((((n3+5)*(3)+(((n2)+2)*(n1 - 6)))-(n4 - n2))+(-(4-6)))))'; rt: rtNumber; n: 4),
    (data: math80; expr: 'div mod mod'; rt: rtNumber; n: -1),
    (data: math69; expr: '-(n-2/@attrib) - -(n-1/@attrib)'; rt: rtNumber; n: 8),
    (data: math69; expr: '-n-2/@attrib --n-1/@attrib'; rt: rtNumber; n: 8),
    (data: math69; expr: '-n-2 --n-1'; rt: rtNumber; n: -4),

   // test boolean operator short-circuting; "count(5)" acts as an error
    (expr: '10+30*20 or count(5)';  rt: rtBool; b: True),
    (expr: '75-50-25 and count(5)'; rt: rtBool; b: False),
    (expr: '"1" and "0"';     rt: rtBool; b: True),
    (expr: '0 or ""';         rt: rtBool; b: False)
  );

  math95='<doc>'+
  '<e>1</e>'+
  '<e>2</e>'+
  '<e>3</e>'+
  '<e>4</e>'+
  '<e>five</e>'+
  '</doc>';

  math96='<doc>'+
  '<e>17</e>'+
  '<e>-5</e>'+
  '<e>8</e>'+
  '<e>-37</e>'+
  '</doc>';

  FunctionTests: array[0..45] of TTestRec = (
  // last()
  // position()
  // count()
  // id()
  // local-name()
  // namespace-uri()
  // name()

    (expr: 'boolean(0)';        rt: rtBool; b: False),
    (expr: 'boolean(-0)';       rt: rtBool; b: False),
    (expr: 'boolean(1 div 0)';  rt: rtBool; b: True),
    (expr: 'boolean(-1 div 0)'; rt: rtBool; b: True),
    (expr: 'boolean(0 div 0)';  rt: rtBool; b: False),
    (expr: 'boolean("")';       rt: rtBool; b: False),
    (expr: 'boolean("abc")';    rt: rtBool; b: True),

    (data: simple; expr: 'boolean(/doc)'; rt: rtBool; b: True),   // #40
    (data: simple; expr: 'boolean(foo)'; rt: rtBool; b: False),   // #41

    (expr: 'true()';  rt: rtBool; b: True),
    (expr: 'false()'; rt: rtBool; b: False),
    (expr: 'not(true())';  rt: rtBool; b: False),
    (expr: 'not(false())'; rt: rtBool; b: True),
    (expr: 'not("")';      rt: rtBool; b: True),
    {
     lang() -- involves nodes
    }

    (expr: 'number("1.5")';   rt: rtNumber; n: 1.5),
    (expr: 'number("abc")';   rt: rtNumber; n: NaN),
    (expr: '-number("abc")';  rt: rtNumber; n: NaN),
    (expr: 'number(true())';  rt: rtNumber; n: 1.0),
    (expr: 'number(false())'; rt: rtNumber; n: 0),

    (data: math95; expr: 'sum(e)'; rt: rtNumber; n: NaN),
    (data: math96; expr: 'sum(e)'; rt: rtNumber; n: -17),

    (expr: 'floor(0.1)';     rt: rtNumber; n: 0),
    (expr: 'floor(-0.1)';    rt: rtNumber; n: -1),
    (expr: 'floor(-0)';      rt: rtNumber; n: 0),
    (expr: 'floor(0)';       rt: rtNumber; n: 0),
    (expr: 'floor(5.2)';     rt: rtNumber; n: 5),
    (expr: 'floor(-5.2)';    rt: rtNumber; n: -6),
    (expr: 'floor("NaN")';   rt: rtNumber; n: NaN),

    (expr: 'ceiling(0.1)';   rt: rtNumber; n: 1),
    (expr: 'ceiling(-0.1)';  rt: rtNumber; n: 0),
    (expr: 'ceiling(-0)';    rt: rtNumber; n: 0),
    (expr: 'ceiling(0)';     rt: rtNumber; n: 0),
    (expr: 'ceiling(5.2)';   rt: rtNumber; n: 6),
    (expr: 'ceiling(-5.2)';  rt: rtNumber; n: -5),
    (expr: 'ceiling("NaN")'; rt: rtNumber; n: NaN),

    (expr: 'round(0.1)';     rt: rtNumber; n: 0),
    (expr: 'round(5.2)';     rt: rtNumber; n: 5),
    (expr: 'round(5.5)';     rt: rtNumber; n: 6),
    (expr: 'round(5.6)';     rt: rtNumber; n: 6),
    (expr: 'round(-0.1)';    rt: rtNumber; n: 0),
    (expr: 'round(-5.2)';    rt: rtNumber; n: -5),
    (expr: 'round(-5.5)';    rt: rtNumber; n: -5),
    (expr: 'round(-5.6)';    rt: rtNumber; n: -6),
    (expr: 'round("NaN")';   rt: rtNumber; n: NaN),
    (expr: 'round(1 div 0)'; rt: rtNumber; n: Infinity),
    (expr: 'round(-1 div 0)'; rt: rtNumber; n: -Infinity)
  );

  str14 ='<doc>'#10+
  '  <av>'#10+
  '    <a>'#10+
  '      <b>b</b>'#10+
  '      <c>c</c>'#10+
  '      <d>d</d>'#10+
  '      <e>e</e>'#10+
  '    </a>'#10+
  '    <v>'#10+
  '      <w>w</w>'#10+
  '      <x>x</x>'#10+
  '      <y>y</y>'#10+
  '      <z>z</z>'#10+
  '    </v>'#10+
  '  </av>'#10+
  '</doc>';

  out14 =#10+
'      b'#10+
'      c'#10+
'      d'#10+
'      e'#10+
'    ';

  StringTests: array[0..59] of TTestRec = (
    (expr: 'string(5)';       rt: rtString; s: '5'),
    (expr: 'string(0.5)';     rt: rtString; s: '0.5'),
    (expr: 'string(-0.5)';    rt: rtString; s: '-0.5'),
    (expr: 'string(true())';  rt: rtString; s: 'true'),
    (expr: 'string(false())'; rt: rtString; s: 'false'),
    (expr: 'string(0 div 0)'; rt: rtString; s: 'NaN'),
    (expr: 'string(1 div 0)'; rt: rtString; s: 'Infinity'),
    (expr: 'string(-1 div 0)'; rt: rtString; s: '-Infinity'),
    // maybe other checks for correct numeric formats
    (data: str14; expr: 'string(av//*)'; rt: rtString; s: out14),

    (expr: 'concat("titi","toto")'; rt: rtString; s: 'tititoto'),
    (expr: 'concat("titi","toto","tata")'; rt: rtString; s: 'tititototata'),
    (expr: 'concat("titi",''toto'')'; rt: rtString; s: 'tititoto'),
    (expr: 'concat("titi",''toto'',"tata","last")'; rt: rtString; s: 'tititototatalast'),
    (expr: 'concat("cd", 34)';      rt: rtString; s: 'cd34'),          // #101
    (expr: 'concat(false(), "ly")'; rt: rtString; s: 'falsely'),       // #104

    (expr: 'starts-with("tititoto","titi")'; rt: rtBool; b: True),
    (expr: 'starts-with("tititoto","to")';   rt: rtBool; b: False),
    (expr: 'starts-with("ab", "abc")';       rt: rtBool; b: False),
    (expr: 'starts-with("abc", "")';         rt: rtBool; b: True),     // xalan/string/string48
    (expr: 'starts-with("", "")';            rt: rtBool; b: True),     // #49



    (expr: 'contains("tititototata","titi")'; rt: rtBool; b: True),
    (expr: 'contains("tititototata","toto")'; rt: rtBool; b: True),
    (expr: 'contains("tititototata","tata")'; rt: rtBool; b: True),
    (expr: 'contains("tititototata","tita")'; rt: rtBool; b: False),
    (expr: 'contains("ab", "abc")';           rt: rtBool; b: False),   // #59
    (expr: 'contains("abc", "bcd")';          rt: rtBool; b: False),   // #60
    (expr: 'contains("abc", "")';             rt: rtBool; b: True),    // #61
    (expr: 'contains("", "")';                rt: rtBool; b: True),    // #62
    // 'contains(concat(.,'BC'),concat('A','B','C'))' == true

    (expr: 'substring("12345",2,3)'; rt: rtString; s: '234'),
    (expr: 'substring("12345",2)';   rt: rtString; s: '2345'),
    (expr: 'substring("12345",-4)';  rt: rtString; s: '12345'),
    (expr: 'substring("12345",3.4)'; rt: rtString; s: '345'),
    (expr: 'substring("12345",3.6)'; rt: rtString; s: '45'),

    (expr: 'substring("12345",1.5,2.6)'; rt: rtString; s: '234'),
    (expr: 'substring("12345",2.2,2.2)'; rt: rtString; s: '23'),
    (expr: 'substring("12345",0,3)';     rt: rtString; s: '12'),
    (expr: 'substring("12345",-8,10)';   rt: rtString; s: '1'),
    (expr: 'substring("12345",4,-10)';   rt: rtString; s: ''),

    (expr: 'substring("12345",0 div 0, 3)'; rt: rtString; s: ''),
    (expr: 'substring("12345",1, 0 div 0)'; rt: rtString; s: ''),
    (expr: 'substring("12345",1 div 0, 3)'; rt: rtString; s: ''),
    (expr: 'substring("12345",3,-1 div 0)'; rt: rtString; s: ''),
    (expr: 'substring("12345",-42, 1 div 0)'; rt: rtString; s: '12345'),

    (expr: 'substring("12345",-1 div 0, 1 div 0)'; rt: rtString; s: ''),
    (expr: 'substring("12345",-1 div 0,5)';        rt: rtString; s: ''),

    (expr: 'substring-before("1999/04/01","/")'; rt: rtString; s: '1999'),
    (expr: 'substring-before("1999/04/01","a")'; rt: rtString; s: ''),
    (expr: 'substring-after("1999/04/01","/")'; rt: rtString; s: '04/01'),
    (expr: 'substring-after("1999/04/01","19")'; rt: rtString; s: '99/04/01'),
    (expr: 'substring-after("1999/04/01","a")'; rt: rtString; s: ''),

    (expr: 'string-length("")';     rt: rtNumber; n: 0),
    (expr: 'string-length("titi")'; rt: rtNumber; n: 4),
    (data: simple; expr: 'string-length(.)'; rt: rtNumber; n: 4),    // #02 modified
    (data: str04;  expr: 'string-length(/)'; rt: rtNumber; n:27),    // #04.1 modified
    (data: str04;  expr: 'string-length(/doc/a)'; rt: rtNumber; n: 12), // #04.2
    (data: str04;  expr: 'string-length()';  rt: rtNumber; n: 27),
    (expr: 'normalize-space("'#9#10#13' ab   cd'#10#13#9'ef'#9#10#13'  ")'; rt: rtString; s: 'ab cd ef'),

    (expr: 'translate("bar", "abc", "ABC")'; rt: rtString; s: 'BAr'),
    (expr: 'translate("--aaa--","abc-","ABC")'; rt: rtString; s: 'AAA'),
    (expr: 'translate("ddaaadddd","abcd","ABCxy")'; rt: rtString; s: 'xxAAAxxxx')   // #96
  );
{$warnings on}

var
  FailCount: Integer = 0;  

procedure CheckResult(const t: TTestRec; r: TXPathVariable);
begin
  case t.rt of
    rtBool:
    begin
      if (r is TXPathBooleanVariable) and (r.AsBoolean = t.b) then
        Exit;
      writeln;
      writeln('Failed: ', t.expr);
      writeln('Expected: ', t.b, ' got: ', r.AsBoolean);
    end;

    rtNumber:
    begin
      if (r is TXPathNumberVariable) then
      begin
        if IsNan(t.n) and IsNan(r.AsNumber) then
          Exit;
        if IsInfinite(t.n) and (t.n = r.AsNumber) then
          Exit;
        if SameValue(r.AsNumber, t.n) then
          Exit;
      end;
      writeln;
      writeln('Failed: ', t.expr);
      writeln('Expected: ', t.n, ' got: ', r.AsNumber);
    end;

    rtString:
    begin
      if (r is TXPathStringVariable) and (r.AsText = DOMString(t.s)) then
        Exit;
      writeln;  
      writeln('Failed: ', t.expr);
      writeln('Expected: ', DOMString(t.s), ' got: ', r.AsText);
    end;
  end;
  Inc(FailCount);
end;

function ParseString(const data: string): TXMLDocument;
var
  parser: TDOMParser;
  src: TXMLInputSource;
begin
  parser := TDOMParser.Create;
  try
    parser.Options.PreserveWhitespace := True;
    src := TXMLInputSource.Create(data);
    try
      parser.Parse(src, Result);
    finally
      src.Free;
    end;
  finally
    parser.Free;
  end;
end;

procedure DoSuite(const tests: array of TTestRec);
var
  i: Integer;
  doc: TXMLDocument;
  rslt: TXPathVariable;
begin
  for i := 0 to High(tests) do
  begin
    if tests[i].data <> '' then
      doc := ParseString(tests[i].data)
    else
      doc := TXMLDocument.Create;
    try
      try
        rslt := EvaluateXPathExpression(tests[i].expr, doc.DocumentElement);
        try
          CheckResult(tests[i], rslt);
        finally
          rslt.Free;
        end;
      except
        writeln;
        writeln('Failed: ', tests[i].expr);
        SysUtils.ShowException(ExceptObject, ExceptAddr);
        Inc(FailCount);
      end;
    finally
      doc.Free;
    end;
  end;
end;

begin
  DecimalSeparator := '.';
  DoSuite(BaseTests);
  DoSuite(CompareTests);
  DoSuite(NodesetCompareTests);  
  DoSuite(EqualityTests);
  DoSuite(FloatTests);
  DoSuite(FunctionTests);
  DoSuite(StringTests);

  writeln;
  writeln('Total failed tests: ', FailCount);
end.
