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
    procedure TestStdintTypes;
    procedure TestStdintTypesCTypes;
    procedure TestSizeAndPointerTypes;
    procedure TestSizeAndPointerTypesCTypes;
    procedure TestBoolAndCharTypes;
    procedure TestWideCharWin32;
    procedure TestStandardTypePointers;
    procedure TestStandardTypeInStruct;
    procedure TestStandardTypeCasts;
    procedure TestStandardTypesCompile;
    procedure TestStandardTypesCompileCTypes;
    procedure TestStandardTypesCompilePrefixes;
    procedure TestCTypesPointerToPointer;
    procedure TestOffT;
    procedure TestOffTCTypes;
    procedure TestVaList;
    procedure TestOffTAndVaListCompile;
    procedure TestOffTAndVaListCompileCTypes;
    procedure TestFilePointer;
    procedure TestFilePointerCompiles;
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


const
  StdintFunction = 'int64_t a(uint8_t b, int8_t c, int16_t d, uint16_t e, int32_t f, uint32_t g, uint64_t h, intmax_t i, uintmax_t j);';
  SizeFunction = 'size_t s(ssize_t a, intptr_t b, uintptr_t c, ptrdiff_t d);';
  CharFunction = '_Bool ok(wchar_t w, char16_t c16, char32_t c32, bool b);';
  PointerFunction = 'void p(uint8_t *pb, size_t *ps, wchar_t *pw, uint32_t **pp);';

procedure TTestTypeMapping.TestStdintTypes;

begin
  Convert([StdintFunction],['-d']);
  AssertConverted;
  AssertInterface('stdint types',['function a(b:byte; c:shortint; d:smallint; e:word; f:longint;',
    'g:longword; h:qword; i:int64; j:qword):int64;cdecl;external;']);
end;


procedure TTestTypeMapping.TestStdintTypesCTypes;

begin
  Convert([StdintFunction],['-d','-C']);
  AssertConverted;
  AssertInterface('stdint types with ctypes',
    ['function a(b:cuint8; c:cint8; d:cint16; e:cuint16; f:cint32;',
     'g:cuint32; h:cuint64; i:cint64; j:cuint64):cint64;cdecl;external;']);
end;


procedure TTestTypeMapping.TestSizeAndPointerTypes;

begin
  Convert([SizeFunction],['-d']);
  AssertConverted;
  AssertInterface('size and pointer sized types',['function s(a:SizeInt; b:PtrInt; c:PtrUInt; d:PtrInt):SizeUInt;cdecl;external;']);
end;


procedure TTestTypeMapping.TestSizeAndPointerTypesCTypes;

begin
  Convert([SizeFunction],['-d','-C']);
  AssertConverted;
  AssertInterface('size_t with ctypes',['function s(a:SizeInt; b:PtrInt; c:PtrUInt; d:PtrInt):csize_t;cdecl;external;']);
end;


procedure TTestTypeMapping.TestBoolAndCharTypes;

begin
  Convert([CharFunction],['-d']);
  AssertConverted;
  AssertInterface('bool and character types',['function ok(w:UCS4Char; c16:WideChar; c32:UCS4Char; b:Boolean):Boolean;cdecl;external;']);
end;


procedure TTestTypeMapping.TestWideCharWin32;

begin
  Convert(['int w(wchar_t c);'],['-d','-w']);
  AssertConverted;
  AssertInterface('wchar_t is widechar for Windows headers',['function w(c:widechar):longint;']);
end;


procedure TTestTypeMapping.TestStandardTypePointers;

begin
  Convert([PointerFunction],['-d']);
  AssertConverted;
  AssertInterface('pointers to standard types',['procedure p(pb:Pbyte; ps:PSizeUInt; pw:PUCS4Char; pp:PPlongword);cdecl;external;']);
  AssertNotOutput('the RTL pointer types are not declared','PSizeUInt = ^');
end;


procedure TTestTypeMapping.TestStandardTypeInStruct;

begin
  Convert(['struct s { uint8_t x; size_t *len; _Bool flag; };','typedef uint32_t my_t;']);
  AssertConverted;
  AssertInterface('standard types in a struct',['s = record','x : byte;','len : ^SizeUInt;','flag : Boolean;','end;']);
  AssertInterface('typedef of a standard type',['my_t = longword;']);
end;


procedure TTestTypeMapping.TestStandardTypeCasts;

begin
  Convert(['#define B(x) ((uint8_t)(x))','#define D(q) ((size_t)*(q))','#define P(q) ((uint32_t *)(q))']);
  AssertConverted;
  AssertImplementation('cast to a standard type',['B:=byte(x);']);
  AssertImplementation('cast of a dereference is no product',['D:=SizeUInt(q^);']);
  AssertImplementation('pointer cast to a standard type',['P:=Plongword(q);']);
end;


procedure TTestTypeMapping.TestStandardTypesCompile;

begin
  Convert([StdintFunction,SizeFunction,CharFunction,PointerFunction,'struct rec { uint8_t x; size_t *len; _Bool flag; };',
           '#define B(x) ((uint8_t)(x))'],['-d']);
  AssertConverted;
  AssertCompiles;
end;


procedure TTestTypeMapping.TestStandardTypesCompileCTypes;

begin
  Convert([StdintFunction,SizeFunction,CharFunction,PointerFunction,'struct rec { uint8_t x; size_t *len; _Bool flag; };'],
          ['-d','-C']);
  AssertConverted;
  AssertCompiles;
end;


procedure TTestTypeMapping.TestCTypesPointerToPointer;

begin
  Convert(['void f(unsigned int **pp, int **qq, uint32_t **rr);'],['-d','-C']);
  AssertConverted;
  AssertInterface('pointer to pointer to a ctypes type',['procedure f(pp:Ppcuint; qq:Ppcint; rr:Ppcuint32);']);
  AssertOutput('the pointer types are declared',['Ppcint = ^pcint;','Ppcuint = ^pcuint;','Ppcuint32 = ^pcuint32;']);
  AssertCompiles;
end;


const
  OffTFunction = 'off_t seekit(int fd, off_t pos, off64_t *big, off_t *p);';
  VaListFunction = 'int vlog2(char *fmt, va_list ap, __gnuc_va_list gap, va_list *pap);';

procedure TTestTypeMapping.TestOffT;

begin
  Convert([OffTFunction],['-d']);
  AssertConverted;
  AssertInterface('off_t is pointer sized, off64_t is int64',
    ['function seekit(fd:longint; pos:PtrInt; big:Pint64; p:PPtrInt):PtrInt;cdecl;external;']);
end;


procedure TTestTypeMapping.TestOffTCTypes;

begin
  Convert([OffTFunction],['-d','-C']);
  AssertConverted;
  AssertInterface('off_t with ctypes',['function seekit(fd:cint; pos:coff_t; big:pcint64; p:Pcoff_t):coff_t;cdecl;external;']);
end;


procedure TTestTypeMapping.TestVaList;

begin
  Convert([VaListFunction],['-d']);
  AssertConverted;
  AssertInterface('va_list is a pointer',['function vlog2(fmt:Pansichar; ap:pointer; gap:pointer; pap:Ppointer):longint;cdecl;external;']);
  AssertNotOutput('Ppointer comes from the system unit','Ppointer =');
end;


procedure TTestTypeMapping.TestOffTAndVaListCompile;

begin
  Convert([OffTFunction,VaListFunction],['-d']);
  AssertConverted;
  AssertCompiles;
end;


procedure TTestTypeMapping.TestOffTAndVaListCompileCTypes;

begin
  Convert([OffTFunction,VaListFunction],['-d','-C']);
  AssertConverted;
  AssertCompiles;
end;


procedure TTestTypeMapping.TestFilePointer;

begin
  Convert(['typedef FILE *png_FILE_p;','void init_io(FILE *fp);','FILE **pp(void);','struct s { FILE *f; };'],['-d']);
  AssertConverted;
  AssertInterface('a FILE pointer is a pointer',['png_FILE_p = pointer;','procedure init_io(fp:pointer);cdecl;external;']);
  AssertInterface('a pointer to a FILE pointer',['function pp:Ppointer;cdecl;external;']);
  AssertInterface('a FILE pointer field',['f : pointer;']);
end;


procedure TTestTypeMapping.TestFilePointerCompiles;

begin
  Convert(['typedef FILE *png_FILE_p;','void init_io(FILE *fp);','FILE **pp(void);','struct s { FILE *f; };',
           '#define CASTF(x) ((FILE *)(x))'],['-d']);
  AssertConverted;
  AssertCompiles;
end;


procedure TTestTypeMapping.TestStandardTypesCompilePrefixes;

begin
  Convert([SizeFunction,CharFunction,PointerFunction,'struct rec { uint8_t x; size_t *len; _Bool flag; };'],['-d','-T','-p']);
  AssertConverted;
  AssertInterface('standard types keep their names under -T',['Trec = record','x : byte;','len : PSizeUInt;']);
  AssertCompiles;
end;


initialization
  RegisterTest('H2Pas',TTestTypeMapping);
end.
