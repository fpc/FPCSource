{
  h2pas test suite: command-line options other than the type and pointer prefixes.
  Copyright (c) 2026 by Michael Van Canneyt
  See the file COPYING.FPC for details about the copyright.
}
unit tcOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, tcH2PasBase;

type

  { TTestOptions }

  TTestOptions = class(TH2PasTestCase)
  protected
    // Converts the sample header with aOptions.
    procedure ConvertSample(const aOptions: array of string);
  published
    procedure TestUnitStructure;
    procedure TestCommandLineRecorded;
    procedure TestDefaultOutputName;
    procedure TestDefaultInputExtension;
    procedure TestUnitName;
    procedure TestIncludeFile;
    procedure TestCompact;
    procedure TestNotCompact;
    procedure TestExternal;
    procedure TestExternalName;
    procedure TestExternalNameCompiles;
    procedure TestEnumToConst;
    procedure TestEnumToConstCompiles;
    procedure TestPackRecords;
    procedure TestPackRecordsUnion;
    procedure TestDynLibVariables;
    procedure TestDynLibLoader;
    procedure TestDynLibCompiles;
    procedure TestDynLibWithMacro;
    procedure TestStripComments;
    procedure TestStripInfo;
    procedure TestVarParams;
    procedure TestVarParamsKeepCharPointers;
    procedure TestWin32;
    procedure TestWin32CallingConventions;
    procedure TestWin32WideString;
    procedure TestWin32Packed;
    procedure TestCallingConventionNeedsWin32;
    procedure TestPalmOSSysTrap;
    procedure TestNoAnsiChar;
    procedure TestCTypes;
    procedure TestCTypesCompiles;
  end;

implementation

const
  SampleHeader : array[0..6] of string = (
    '/* comment */',
    'typedef struct _node {',
    '  int value;',
    '  struct _node *next;',
    '} node;',
    'extern int counter;',
    'int getval(node *n, char *name, int *v, void *data);'
  );

  ScalarHeader : array[0..5] of string = (
    '/* scalar */',
    'struct rec { int a; long b; unsigned short c; float d; char name[8]; };',
    'enum kind { ka, kb = 3, kc };',
    'extern int counter;',
    'int add(int a, int b);',
    'void reset(void);'
  );


procedure TTestOptions.ConvertSample(const aOptions: array of string);

begin
  Convert(SampleHeader,aOptions);
  AssertConverted;
end;


procedure TTestOptions.TestUnitStructure;

begin
  ConvertSample([]);
  AssertOutput('unit clause',['unit output;','interface']);
  AssertOutput('C record packing',['{$IFDEF FPC}','{$PACKRECORDS C}','{$ENDIF}']);
  AssertOutput('implementation section',['implementation']);
  AssertRawLine('unit end','end.');
end;


procedure TTestOptions.TestCommandLineRecorded;

begin
  Convert(['int x;'],['-d']);
  AssertConverted;
  AssertOutput('command line is recorded in the unit header',
    ['Automatically converted by H2Pas 0.99.16 from input.h','The following command line parameters were used:',
     '-d','-o','output.pp','input.h','}']);
end;


procedure TTestOptions.TestDefaultOutputName;

begin
  WriteWorkFile('sample.h','int x;');
  RunH2Pas(['sample.h'],'sample.pp');
  AssertConverted;
  AssertOutput('output file and unit name derive from the input name',['unit sample;']);
end;


procedure TTestOptions.TestDefaultInputExtension;

begin
  WriteWorkFile('sample.h','int x;');
  RunH2Pas(['sample'],'sample.pp');
  AssertConverted;
  AssertOutput('.h is appended to an input name without extension',['Automatically converted by H2Pas 0.99.16 from sample.h']);
end;


procedure TTestOptions.TestUnitName;

begin
  ConvertSample(['-u','MyUnit']);
  AssertOutput('-u sets the unit name',['unit MyUnit;']);
end;


procedure TTestOptions.TestIncludeFile;

begin
  ConvertSample(['-i']);
  AssertNotOutput('-i writes no unit clause','unit output;');
  AssertNotOutput('-i writes no interface keyword','interface');
  AssertNotOutput('-i writes no implementation keyword','implementation');
  AssertNotOutput('-i writes no end of unit','end.');
  AssertOutput('-i still writes the declarations',['function getval(n:Pnode; name:Pansichar; v:Plongint; data:pointer):longint;']);
end;


procedure TTestOptions.TestCompact;

begin
  ConvertSample(['-c']);
  AssertRawLine('-c does not indent the type keyword','type');
  AssertRawLine('-c does not indent functions','function getval(n:Pnode; name:Pansichar; v:Plongint; data:pointer):longint;');
end;


procedure TTestOptions.TestNotCompact;

begin
  ConvertSample([]);
  AssertRawLine('type keyword is indented','  type');
  AssertRawLine('functions are indented','  function getval(n:Pnode; name:Pansichar; v:Plongint; data:pointer):longint;');
end;


procedure TTestOptions.TestExternal;

begin
  ConvertSample(['-d']);
  AssertInterface('-d declares functions external',
    ['function getval(n:Pnode; name:Pansichar; v:Plongint; data:pointer):longint;cdecl;external;']);
  AssertNotImplementation('-d writes no stubs','function getval');
end;


procedure TTestOptions.TestExternalName;

begin
  ConvertSample(['-D','-l','mylib']);
  AssertOutput('-D declares the library name constant',['const','External_library=''mylib''; {Setup as you need}']);
  AssertInterface('-D names the imported symbol',
    ['function getval(n:Pnode; name:Pansichar; v:Plongint; data:pointer):longint;cdecl;external External_library name ''getval'';']);
end;


procedure TTestOptions.TestExternalNameCompiles;

begin
  Convert(ScalarHeader,['-D','-l','mylib']);
  AssertConverted;
  AssertCompiles;
end;


procedure TTestOptions.TestEnumToConst;

begin
  Convert(['enum e { A, B = 5, C, D = X, E };'],['-e']);
  AssertConverted;
  AssertInterface('-e writes the enum as longint constants',
    ['e = Longint;','Const','A = 0;','B = 5;','C = 6;','D = X;','E = (X)+1;']);
end;


procedure TTestOptions.TestEnumToConstCompiles;

begin
  Convert(ScalarHeader,['-e','-d']);
  AssertConverted;
  AssertCompiles;
end;


procedure TTestOptions.TestPackRecords;

begin
  ConvertSample(['-pr']);
  AssertInterface('-pr packs records',['_node = packed record']);
  AssertNotOutput('-pr omits C record packing','{$PACKRECORDS C}');
end;


procedure TTestOptions.TestPackRecordsUnion;

begin
  Convert(['union u { int i; float f; };'],['-pr']);
  AssertConverted;
  AssertInterface('-pr packs unions',['u = packed record','case longint of']);
end;


procedure TTestOptions.TestDynLibVariables;

begin
  Convert(['int getval(int a);','void setval(int a);'],['-P']);
  AssertConverted;
  AssertOutput('-P switches to objfpc mode',['{$mode objfpc}','unit output;']);
  AssertInterface('-P declares procedure variables',
    ['var','getval : function(a:longint):longint']);
  AssertInterface('-P declares procedure variables for procedures',['setval : procedure(a:longint)']);
end;


procedure TTestOptions.TestDynLibLoader;

begin
  Convert(['int getval(int a);','void setval(int a);'],['-P']);
  AssertConverted;
  AssertOutput('-P uses dynlibs at the start of the implementation',['implementation','uses','SysUtils, dynlibs;']);
  AssertImplementation('-P declares the library handle',['var','hlib : tlibhandle;']);
  AssertImplementation('-P frees the library',
    ['procedure Freeoutput;','begin','FreeLibrary(hlib);','getval:=nil;','setval:=nil;','end;']);
  AssertImplementation('-P loads the library',
    ['procedure Loadoutput(lib : pchar);','begin','Freeoutput;','hlib:=LoadLibrary(lib);','if hlib=0 then',
     'raise Exception.Create(format(''Could not load library: %s'',[lib]));',
     'pointer(getval):=GetProcAddress(hlib,''getval'');','pointer(setval):=GetProcAddress(hlib,''setval'');','end;']);
  AssertImplementation('-P loads and frees in initialization and finalization',
    ['initialization','Loadoutput(''output'');','finalization','Freeoutput;']);
end;


procedure TTestOptions.TestDynLibCompiles;

begin
  Convert(ScalarHeader,['-P']);
  AssertConverted;
  AssertCompiles;
end;


procedure TTestOptions.TestDynLibWithMacro;

begin
  Convert(['#define SQR(x) ((x)*(x))','int getval(int a);'],['-P']);
  AssertConverted;
  AssertOutput('-P uses clause precedes the macro function bodies',['implementation','uses','SysUtils, dynlibs;']);
  AssertImplementation('macro function body',['function SQR(x : longint) : longint;','begin','SQR:=x*x;','end;']);
  AssertCompiles;
end;


procedure TTestOptions.TestStripComments;

begin
  Convert(['/* a comment */','struct s { const int b; };'],['-s']);
  AssertConverted;
  AssertNotOutput('-s strips comments','a comment');
  AssertOutput('-s keeps info comments',['(* Const before declarator ignored *)']);
end;


procedure TTestOptions.TestStripInfo;

begin
  Convert(['/* a comment */','#ifdef __cplusplus','extern "C" {','#endif',
           'struct s { const int b; };','#pragma once','#define F(a) a'],['-S']);
  AssertConverted;
  AssertNotOutput('-S strips comments','a comment');
  AssertNotOutput('-S strips the const note','Const before declarator ignored');
  AssertNotOutput('-S strips the extern "C" note','C++');
  AssertNotOutput('-S strips unsupported pragmas','pragma');
  AssertNotOutput('-S strips the macro note','was #define');
  AssertInterface('-S keeps the declarations',['function F(a : longint) : longint;']);
end;


procedure TTestOptions.TestVarParams;

begin
  Convert(['void f(int *a, void *c, int **d, int (*cb)(int));'],['-v']);
  AssertConverted;
  AssertInterface('-v turns typed pointer parameters into var parameters',
    ['procedure f(var a:longint; c:pointer; var d:Plongint; cb:f_cb);']);
end;


procedure TTestOptions.TestVarParamsKeepCharPointers;

begin
  ConvertSample(['-v']);
  AssertInterface('-v keeps char pointers and untyped pointers',
    ['function getval(var n:node; name:Pansichar; var v:longint; data:pointer):longint;']);
end;


procedure TTestOptions.TestWin32;

begin
  ConvertSample(['-w']);
  AssertOutput('-w uses kernel32',['External_library=''kernel32''; {Setup as you need}']);
  AssertInterface('-w implies -D and -v',
    ['function getval(var n:node; name:Pansichar; var v:longint; data:pointer):longint;cdecl;external External_library name ''getval'';']);
end;


procedure TTestOptions.TestWin32CallingConventions;

begin
  Convert(['int STDCALL f1(int a);','int WINAPI f2(void);','int CALLBACK f3(void);',
           'int PASCAL f4(void);','int CDECL f5(void);','int WINGDIAPI f6(void);'],['-w']);
  AssertConverted;
  AssertInterface('STDCALL function',['function f1(a:longint):longint;']);
  AssertInterface('WINAPI function',['function f2:longint;']);
  AssertInterface('CALLBACK function',['function f3:longint;']);
  AssertInterface('PASCAL function',['function f4:longint;']);
  AssertInterface('CDECL function is cdecl',['function f5:longint;cdecl;external External_library name ''f5'';']);
  AssertInterface('WINGDIAPI function',['function f6:longint;']);
  AssertNotOutput('only CDECL gives cdecl',':longint;cdecl;external External_library name ''f1''');
end;


procedure TTestOptions.TestWin32WideString;

begin
  Convert(['#define WS L"wide"'],['-w']);
  AssertConverted;
  AssertInterface('-w accepts wide string literals',['WS = ''wide'';']);
end;


procedure TTestOptions.TestWin32Packed;

begin
  Convert(['typedef struct { char c; int i; } PACKED pk;'],['-w']);
  AssertConverted;
  AssertInterface('PACKED switches to byte packing',['{$PACKRECORDS 1}','type','pk = record','c : ansichar;','i : longint;','end;']);
end;


procedure TTestOptions.TestCallingConventionNeedsWin32;

begin
  Convert(['int STDCALL f1(int a);']);
  AssertTrue('without -w STDCALL is an identifier',Pos('syntax error',ToolOutput)>0);
end;


procedure TTestOptions.TestPalmOSSysTrap;

begin
  Convert(['void DmOpen(int a) SYS_TRAP(sysTrapDmOpen);','int DmClose(void) SYS_TRAP(sysTrapDmClose);'],['-x']);
  AssertConverted;
  AssertInterface('-x writes the systrap directive for procedures',['procedure DmOpen(a:longint);systrap sysTrapDmOpen;']);
  AssertInterface('-x writes the systrap directive for functions',['function DmClose:longint;systrap sysTrapDmClose;']);
end;


procedure TTestOptions.TestNoAnsiChar;

begin
  ConvertSample(['-a']);
  AssertInterface('-a uses char',['function getval(n:Pnode; name:Pchar; v:Plongint; data:pointer):longint;']);
  AssertNotOutput('-a does not use ansichar','ansichar');
end;


procedure TTestOptions.TestCTypes;

begin
  ConvertSample(['-C']);
  AssertOutput('-C uses the ctypes unit',['interface','uses','ctypes;']);
  AssertInterface('-C record fields',['value : cint;']);
  AssertInterface('-C variables',['counter : cint;cvar;external;']);
  AssertInterface('-C pointers to ctypes use a lowercase p',
    ['function getval(n:Pnode; name:pcchar; v:pcint; data:pointer):cint;']);
end;


procedure TTestOptions.TestCTypesCompiles;

begin
  Convert(ScalarHeader,['-C','-d']);
  AssertConverted;
  AssertCompiles;
end;


initialization
  RegisterTest('H2Pas',TTestOptions);
end.
