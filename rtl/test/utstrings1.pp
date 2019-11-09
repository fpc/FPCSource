{ Basic test suite for the strings unit }
{$mode objfpc}
unit utstrings1;

interface
  
uses punit, utrtl;

implementation
  
uses
  strings;

Function teststrlen : TTestString;

Const
  P1 : PChar = '';
  P2 : PChar = 'This is a constant pchar string';
begin
  Result:='';
  If not AssertEquals('Empty string',0,strlen(P1)) then exit;
  if not AssertEquals('Non-empty string',31,strlen(P2)) then exit;
end;


function teststrcomp : TTestString;

Const
  P1 : PChar = 'This is the first string.';
  P2 : PCHar = 'This is the second string.';
  P3 : PChar = 'This is the first string.';
begin
  Result:='';
  If Not AssertTrue('Different strings',StrComp (P1,P2)<>0) then exit;
  If Not AssertEquals('Equal strings different pointers',0,StrComp(P1,P3)) then exit;
  If Not AssertTrue('First less than second',StrComp (P1,P2)<=0)  then exit;
  If Not AssertTrue('Second bigger than first',StrComp (P2,P1)>0) then exit;
end;

Function teststrpas : TTestString;

Const
  P1 : PChar = 'This is a PCHAR string';
  P2 : PChar = '';
var
  S : string;
begin
  Result:='';
  S:=StrPas(P1);
  if Not AssertEquals('Test strpas, non-nil','This is a PCHAR string',S) then exit;
  S:=StrPas(P2);
  if Not AssertEquals('Test strpas, nil','',S) then exit;
end;


Function teststrlcomp : TTestString;

Const
   P1 : PChar = 'This is the first string.';
   P2 : PCHar = 'This is the second string.';
   P3 : PChar = 'This is the first string.';
Var
  L : Longint;

begin
  Result:='';
  L:=1;
  While StrLComp(P1,P2,L)=0 do
   inc (L);
  if not AssertEquals('Max 13 chars equal',13,L) then exit;
  if not AssertTrue('Different strings',StrLComp (P1,P2,255)<>0) then exit;
  if not AssertEquals('Equal strings, different pointers',0,StrLComp (P1,P3,100)) then exit;
  if not AssertTrue('P1<P2 negative',StrLComp (P1,P2,65535)<0) then exit;
  if not AssertTrue('P2>P1, positive',StrLComp (P2,P1,12341234)>0) then exit;
end;

Function teststrpcopy : TTestString;

Const
   S1 = 'This is a normal string.';
   S2 = '';

Var
  P : array[0..255] of char;

begin
  Result:='';
  If not AssertEquals('Return value',@P,StrPCopy(P,S1)) then exit;
  If not AssertEquals('Correct copy',0,StrComp(P,S1)) then exit;
  if not AssertEquals('Return value 2',@P,StrPCopy(P,S2)) then
  if not AssertEquals('Correct copy 2',0,StrComp(P,S2)) then exit;
end;


Function teststrend : TTestString;

Const
   P : PChar = 'This is a PCHAR string.';
begin
  Result:='';
  If not AssertEquals('StrEnd, not empty',23,StrEnd(P)-P) then exit;
end;


Function teststrcopy : TTestString;

Const
  P1 : PChar = 'This a test string 012345678901234567890123456789012345678901234567890123456789';
  P2 : PChar = '';

var
  Buf : array[0..255] of char;

begin
  Result:='';
  If not AssertEquals('StrCopy non-empty Result',@Buf,StrCopy(Buf,P1)) then exit;
  If not AssertEquals('StrCopy non-empty Resulting string',0,StrComp(Buf,P1)) then exit;
  If not AssertEquals('StrCopy empty Result',@Buf,StrCopy(Buf,P2)) then exit;
  If not AssertEquals('StrCopy empty Resulting string',0,StrComp(Buf,P2)) then exit;
end;


Function teststrscanstrrscan : TTestString;

Const
  P : PChar = 'This is a PCHAR string.';
  S : Char = 's' ;
begin
  Result:='';
  if Not AssertEquals('Not contained',0, StrComp(StrScan(P,s),'s is a PCHAR string.')) then exit;
  if Not AssertTrue('Contained',StrComp(StrRScan(P,s),'string.')=0) then exit;
end;


Var
  P : Psuite;
begin
  P:=EnsureSuite('Strings');
  AddTest('teststrlen',@teststrlen,P);
  AddTest('teststrcomp',@teststrcomp,P);
  AddTest('teststrlcomp',@teststrlcomp,P);
  AddTest('teststrpas', @teststrpas,P);
  AddTest('teststrcopy', @teststrcopy,P);
  AddTest('teststrpcopy',@teststrpcopy,P);
  AddTest('teststrend', @teststrend,P);
  AddTest('teststrscanstrrscan',@teststrscanstrrscan,P);
end.
