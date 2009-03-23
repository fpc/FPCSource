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
{$mode objfpc}{$h+}

uses
  Classes, SysUtils, Math,
  dom, xmlread, xmlwrite, xpath;

type
  TResultType = (rtString, rtNumber, rtBool, rtNodeset);

  TTestRec = record
    expr: DOMString;
  case rt: TResultType of
    rtString: (s: DOMPChar);   // cannot use DOMString here
    rtNumber: (n: Extended);
    rtBool:   (b: Boolean);
  end;

const
  BaseTests: array[0..4] of TTestRec = (
    (expr: '1';           rt: rtNumber; n: 1),
    (expr: '1+2';         rt: rtNumber; n: 3),
    (expr: '2*3';         rt: rtNumber; n: 6),
    (expr: '1+2*3+4';     rt: rtNumber; n: 11),
    (expr: '(1+2)*(3+4)'; rt: rtNumber; n: 21)
  );

  CompareTests: array[0..45] of TTestRec = (
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

  FloatTests: array[0..60] of TTestRec = (
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
    (expr: '8 mod 3 = 2'; rt: rtBool; b: True),
   // test boolean operator short-circuting; "count(5)" acts as an error
    (expr: '10+30*20 or count(5)';  rt: rtBool; b: True),
    (expr: '75-50-25 and count(5)'; rt: rtBool; b: False)
  );

  FunctionTests: array[0..36] of TTestRec = (
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
    {
     boolean(node-set) -- TODO
    }
    (expr: 'true()';  rt: rtBool; b: True),
    (expr: 'false()'; rt: rtBool; b: False),
    {
     not()
     lang() -- involves nodes
    }

    (expr: 'number("1.5")';   rt: rtNumber; n: 1.5),
    (expr: 'number("abc")';   rt: rtNumber; n: NaN),
    (expr: '-number("abc")';  rt: rtNumber; n: NaN),
    (expr: 'number(true())';  rt: rtNumber; n: 1.0),
    (expr: 'number(false())'; rt: rtNumber; n: 0),
    {
     sum() -- involves nodes
    }

    (expr: 'floor(0.1)';     rt: rtNumber; n: 0),
    (expr: 'floor(-0.1)';    rt: rtNumber; n: -1),
    (expr: 'floor(-0)';      rt: rtNumber; n: 0),
    (expr: 'floor(0)';       rt: rtNumber; n: 0),
    (expr: 'floor(5.2)';     rt: rtNumber; n: 5),
    (expr: 'floor(-5.2)';    rt: rtNumber; n: -6),

    (expr: 'ceiling(0.1)';   rt: rtNumber; n: 1),
    (expr: 'ceiling(-0.1)';  rt: rtNumber; n: 0),
    (expr: 'ceiling(-0)';    rt: rtNumber; n: 0),
    (expr: 'ceiling(0)';     rt: rtNumber; n: 0),
    (expr: 'ceiling(5.2)';   rt: rtNumber; n: 6),
    (expr: 'ceiling(-5.2)';  rt: rtNumber; n: -5),

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

  StringTests: array[0..43] of TTestRec = (
    (expr: 'string(5)';       rt: rtString; s: '5'),
    (expr: 'string(0.5)';     rt: rtString; s: '0.5'),
    (expr: 'string(-0.5)';    rt: rtString; s: '-0.5'),
    (expr: 'string(true())';  rt: rtString; s: 'true'),
    (expr: 'string(false())'; rt: rtString; s: 'false'),
    (expr: 'string(0 div 0)'; rt: rtString; s: 'NaN'),
    (expr: 'string(1 div 0)'; rt: rtString; s: 'Infinity'),
    (expr: 'string(-1 div 0)'; rt: rtString; s: '-Infinity'),
    // maybe other checks for correct numeric formats

    (expr: 'concat("titi","toto")'; rt: rtString; s: 'tititoto'),
    (expr: 'concat("titi","toto","tata")'; rt: rtString; s: 'tititototata'),
    (expr: 'concat("titi",''toto'')'; rt: rtString; s: 'tititoto'),
    (expr: 'concat("titi",''toto'',"tata","last")'; rt: rtString; s: 'tititototatalast'),

    (expr: 'starts-with("tititoto","titi")'; rt: rtBool; b: True),
    (expr: 'starts-with("tititoto","to")';   rt: rtBool; b: False),

    (expr: 'contains("tititototata","titi")'; rt: rtBool; b: True),
    (expr: 'contains("tititototata","toto")'; rt: rtBool; b: True),
    (expr: 'contains("tititototata","tata")'; rt: rtBool; b: True),
    (expr: 'contains("tititototata","tita")'; rt: rtBool; b: False),

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
    {
     normalize-space()
    }
    (expr: 'translate("bar", "abc", "ABC")'; rt: rtString; s: 'BAr'),
    (expr: 'translate("--aaa--","abc-","ABC")'; rt: rtString; s: 'AAA')
  );

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

procedure DoSuite(const tests: array of TTestRec);
var
  i: Integer;
  doc: TXMLDocument;
  rslt: TXPathVariable;
begin
  doc := TXMLDocument.Create;
  try
    for i := 0 to High(tests) do
    begin
      try
        rslt := EvaluateXPathExpression(tests[i].expr, doc);
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
    end;
  finally
    doc.Free;
  end;    
end;

begin
  DecimalSeparator := '.';
  DoSuite(BaseTests);
  DoSuite(CompareTests);
  DoSuite(EqualityTests);
  DoSuite(FloatTests);
  DoSuite(FunctionTests);
  DoSuite(StringTests);

  writeln;
  writeln('Total failed tests: ', FailCount);
end.
