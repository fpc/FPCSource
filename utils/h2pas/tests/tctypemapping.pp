{
  h2pas test suite: mapping of C base types to Pascal and ctypes types.
  Copyright (c) 2026 by Michael Van Canneyt
  See the file COPYING.FPC for details about the copyright.
}
unit tcTypeMapping;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, tcH2PasBase;

type

  { TTestTypeMapping }

  TTestTypeMapping = class(TH2PasTestCase)
  protected
    // Converts "typedef aCType t;" with aOptions and checks that t is aPascalType.
    procedure CheckTypedef(const aCType, aPascalType: string; const aOptions: array of string);
    // Converts "typedef aCType t;" without options and checks that t is aPascalType.
    procedure CheckTypedef(const aCType, aPascalType: string);
  published
    procedure TestInt;
    procedure TestLong;
    procedure TestLongInt;
    procedure TestLongLong;
    procedure TestLongLongInt;
    procedure TestShort;
    procedure TestShortInt;
    procedure TestUnsigned;
    procedure TestUnsignedInt;
    procedure TestUnsignedLong;
    procedure TestUnsignedShort;
    procedure TestUnsignedLongLong;
    procedure TestUnsignedChar;
    procedure TestUnsignedCharPointer;
    procedure TestSignedChar;
    procedure TestSignedCharWithoutAnsiChar;
    procedure TestSigned;
    procedure TestSignedFunction;
    procedure TestLongDouble;
    procedure TestLongDoubleVariable;
    procedure TestDoublePrefix;
    procedure TestSignedInt;
    procedure TestSignedLong;
    procedure TestSignedShort;
    procedure TestChar;
    procedure TestFloat;
    procedure TestDouble;
    procedure TestInt8;
    procedure TestInt16;
    procedure TestInt32;
    procedure TestInt64;
    procedure TestMsInt8;
    procedure TestMsInt16;
    procedure TestMsInt32;
    procedure TestMsInt64;
    procedure TestUnsignedInt8;
    procedure TestUnsignedInt64;
    procedure TestVoidPointer;
    procedure TestNamedType;
    procedure TestConstType;
    procedure TestCharWithoutAnsiChar;
    procedure TestUnsignedCharWithoutAnsiChar;
    procedure TestCTypesInt;
    procedure TestCTypesLong;
    procedure TestCTypesShort;
    procedure TestCTypesChar;
    procedure TestCTypesUnsigned;
    procedure TestCTypesUnsignedInt;
    procedure TestCTypesUnsignedLong;
    procedure TestCTypesUnsignedShort;
    procedure TestCTypesUnsignedChar;
    procedure TestCTypesSignedChar;
    procedure TestCTypesLongLong;
    procedure TestCTypesUnsignedLongLong;
    procedure TestCTypesFloat;
    procedure TestCTypesSigned;
    procedure TestCTypesDouble;
    procedure TestCTypesLongDouble;
    procedure TestCTypesUsesClause;
  end;

implementation


procedure TTestTypeMapping.CheckTypedef(const aCType, aPascalType: string; const aOptions: array of string);

begin
  Convert(['typedef '+aCType+' t;'],aOptions);
  AssertConverted;
  AssertInterface(aCType+' maps to '+aPascalType,['t = '+aPascalType+';']);
end;


procedure TTestTypeMapping.CheckTypedef(const aCType, aPascalType: string);

begin
  CheckTypedef(aCType,aPascalType,[]);
end;


procedure TTestTypeMapping.TestInt;

begin
  CheckTypedef('int','longint');
end;


procedure TTestTypeMapping.TestLong;

begin
  CheckTypedef('long','longint');
end;


procedure TTestTypeMapping.TestLongInt;

begin
  CheckTypedef('long int','longint');
end;


procedure TTestTypeMapping.TestLongLong;

begin
  CheckTypedef('long long','int64');
end;


procedure TTestTypeMapping.TestLongLongInt;

begin
  CheckTypedef('long long int','int64');
end;


procedure TTestTypeMapping.TestShort;

begin
  CheckTypedef('short','smallint');
end;


procedure TTestTypeMapping.TestShortInt;

begin
  CheckTypedef('short int','smallint');
end;


procedure TTestTypeMapping.TestUnsigned;

begin
  CheckTypedef('unsigned','dword');
end;


procedure TTestTypeMapping.TestUnsignedInt;

begin
  CheckTypedef('unsigned int','dword');
end;


procedure TTestTypeMapping.TestUnsignedLong;

begin
  CheckTypedef('unsigned long','dword');
end;


procedure TTestTypeMapping.TestUnsignedShort;

begin
  CheckTypedef('unsigned short','word');
end;


procedure TTestTypeMapping.TestUnsignedLongLong;

begin
  CheckTypedef('unsigned long long','qword');
end;


procedure TTestTypeMapping.TestUnsignedChar;

begin
  CheckTypedef('unsigned char','byte');
end;


procedure TTestTypeMapping.TestUnsignedCharPointer;

begin
  Convert(['void f(unsigned char *buf, unsigned char c);']);
  AssertConverted;
  AssertInterface('unsigned char parameters use byte',['procedure f(buf:Pbyte; c:byte);']);
end;


procedure TTestTypeMapping.TestSignedChar;

begin
  CheckTypedef('signed char','shortint');
end;


procedure TTestTypeMapping.TestSignedCharWithoutAnsiChar;

begin
  CheckTypedef('signed char','shortint',['-a']);
end;


procedure TTestTypeMapping.TestSigned;

begin
  CheckTypedef('signed','longint');
end;


procedure TTestTypeMapping.TestSignedFunction;

begin
  Convert(['signed f(signed a);']);
  AssertConverted;
  AssertInterface('signed alone as result and parameter type',['function f(a:longint):longint;']);
end;


procedure TTestTypeMapping.TestLongDouble;

begin
  CheckTypedef('long double','extended');
end;


procedure TTestTypeMapping.TestLongDoubleVariable;

begin
  Convert(['extern long double ld;','void f(long double *p);'],['-d']);
  AssertConverted;
  AssertInterface('long double variable',['ld : extended;cvar;external;']);
  AssertInterface('pointer to long double',['procedure f(p:Pextended);cdecl;external;']);
  AssertNotOutput('Pextended comes from the system unit','Pextended =');
  AssertCompiles;
end;


procedure TTestTypeMapping.TestDoublePrefix;

begin
  Convert(['typedef double t;'],['-t']);
  AssertConverted;
  AssertInterface('-t does not prefix double',['Tt = double;']);
end;


procedure TTestTypeMapping.TestSignedInt;

begin
  CheckTypedef('signed int','longint');
end;


procedure TTestTypeMapping.TestSignedLong;

begin
  CheckTypedef('signed long','longint');
end;


procedure TTestTypeMapping.TestSignedShort;

begin
  CheckTypedef('signed short','smallint');
end;


procedure TTestTypeMapping.TestChar;

begin
  CheckTypedef('char','ansichar');
end;


procedure TTestTypeMapping.TestFloat;

begin
  CheckTypedef('float','single');
end;


procedure TTestTypeMapping.TestDouble;

begin
  CheckTypedef('double','double');
end;


procedure TTestTypeMapping.TestInt8;

begin
  CheckTypedef('int8','shortint');
end;


procedure TTestTypeMapping.TestInt16;

begin
  CheckTypedef('int16','smallint');
end;


procedure TTestTypeMapping.TestInt32;

begin
  CheckTypedef('int32','longint');
end;


procedure TTestTypeMapping.TestInt64;

begin
  CheckTypedef('int64','int64');
end;


procedure TTestTypeMapping.TestMsInt8;

begin
  CheckTypedef('__int8','shortint');
end;


procedure TTestTypeMapping.TestMsInt16;

begin
  CheckTypedef('__int16','smallint');
end;


procedure TTestTypeMapping.TestMsInt32;

begin
  CheckTypedef('__int32','longint');
end;


procedure TTestTypeMapping.TestMsInt64;

begin
  CheckTypedef('__int64','int64');
end;


procedure TTestTypeMapping.TestUnsignedInt8;

begin
  CheckTypedef('unsigned __int8','byte');
end;


procedure TTestTypeMapping.TestUnsignedInt64;

begin
  CheckTypedef('unsigned __int64','qword');
end;


procedure TTestTypeMapping.TestVoidPointer;

begin
  CheckTypedef('void *','pointer');
end;


procedure TTestTypeMapping.TestNamedType;

begin
  CheckTypedef('mytype','mytype');
end;


procedure TTestTypeMapping.TestConstType;

begin
  CheckTypedef('const int','longint');
end;


procedure TTestTypeMapping.TestCharWithoutAnsiChar;

begin
  CheckTypedef('char','char',['-a']);
end;


procedure TTestTypeMapping.TestUnsignedCharWithoutAnsiChar;

begin
  CheckTypedef('unsigned char','byte',['-a']);
end;


procedure TTestTypeMapping.TestCTypesInt;

begin
  CheckTypedef('int','cint',['-C']);
end;


procedure TTestTypeMapping.TestCTypesLong;

begin
  CheckTypedef('long','clong',['-C']);
end;


procedure TTestTypeMapping.TestCTypesShort;

begin
  CheckTypedef('short','cshort',['-C']);
end;


procedure TTestTypeMapping.TestCTypesChar;

begin
  CheckTypedef('char','cchar',['-C']);
end;


procedure TTestTypeMapping.TestCTypesUnsigned;

begin
  CheckTypedef('unsigned','cunsigned',['-C']);
end;


procedure TTestTypeMapping.TestCTypesUnsignedInt;

begin
  CheckTypedef('unsigned int','cuint',['-C']);
end;


procedure TTestTypeMapping.TestCTypesUnsignedLong;

begin
  CheckTypedef('unsigned long','culong',['-C']);
end;


procedure TTestTypeMapping.TestCTypesUnsignedShort;

begin
  CheckTypedef('unsigned short','cushort',['-C']);
end;


procedure TTestTypeMapping.TestCTypesUnsignedChar;

begin
  CheckTypedef('unsigned char','cuchar',['-C']);
end;


procedure TTestTypeMapping.TestCTypesSignedChar;

begin
  CheckTypedef('signed char','cschar',['-C']);
end;


procedure TTestTypeMapping.TestCTypesLongLong;

begin
  CheckTypedef('long long','clonglong',['-C']);
end;


procedure TTestTypeMapping.TestCTypesUnsignedLongLong;

begin
  CheckTypedef('unsigned long long','culonglong',['-C']);
end;


procedure TTestTypeMapping.TestCTypesFloat;

begin
  CheckTypedef('float','cfloat',['-C']);
end;


procedure TTestTypeMapping.TestCTypesSigned;

begin
  CheckTypedef('signed','csigned',['-C']);
end;


procedure TTestTypeMapping.TestCTypesDouble;

begin
  CheckTypedef('double','cdouble',['-C']);
end;


procedure TTestTypeMapping.TestCTypesLongDouble;

begin
  CheckTypedef('long double','clongdouble',['-C']);
end;


procedure TTestTypeMapping.TestCTypesUsesClause;

begin
  Convert(['typedef int t;'],['-C']);
  AssertConverted;
  AssertOutput('ctypes unit is used',['interface','uses','ctypes;']);
end;


initialization
  RegisterTest('H2Pas',TTestTypeMapping);
end.
