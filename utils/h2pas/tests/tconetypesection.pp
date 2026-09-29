{
  h2pas test suite: all types in one type section (-1).
  Copyright (c) 2026 by Michael Van Canneyt
  See the file COPYING.FPC for details about the copyright.
}
unit tcOneTypeSection;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, tcH2PasBase;

type

  { TTestOneTypeSection }

  TTestOneTypeSection = class(TH2PasTestCase)
  protected
    // Returns the number of lines of the interface that are aLine.
    function CountInterfaceLines(const aLine: string): integer;
  published
    procedure TestOneTypeKeyword;
    procedure TestPointerToStructDeclaredLater;
    procedure TestPointerToStructDeclaredLaterCompiles;
    procedure TestPointerTypedefBeforeStruct;
    procedure TestPlainConstantsBeforeTypes;
    procedure TestConstantsUsingTypesAfterTypes;
    procedure TestConditionalsInEachSection;
    procedure TestConditionalsCompile;
    procedure TestCommentsMoveWithDeclarations;
    procedure TestPackRecordsKept;
    procedure TestEnumToConst;
    procedure TestPointerToUndeclaredType;
    procedure TestWithoutOptionUnchanged;
    procedure TestOneTypeSectionCompiles;
  end;

implementation


function TTestOneTypeSection.CountInterfaceLines(const aLine: string): integer;

var
  lLines: TStringList;
  lLine: string;

begin
  Result:=0;
  lLines:=TStringList.Create;
  try
    lLines.Text:=InterfacePart;
    for lLine in lLines do
      if lLine=aLine then
        inc(Result);
  finally
    lLines.Free;
  end;
end;


const
  FileHeader : array[0..10] of string = (
    '#define IOCAP 1',
    'typedef struct sqlite3_file sqlite3_file;',
    'struct sqlite3_file { const struct sqlite3_io_methods *pMethods; };',
    '#define IOCAP_ATOMIC 2',
    'int sqlite3_file_control(sqlite3_file *p);',
    'typedef struct sqlite3_io_methods sqlite3_io_methods;',
    'struct sqlite3_io_methods { int iVersion; int (*xClose)(sqlite3_file*); };',
    'typedef int myint;',
    'int use(myint a);',
    'typedef myint other;',
    'extern int counter;');

procedure TTestOneTypeSection.TestOneTypeKeyword;

begin
  Convert(FileHeader,['-d','-1']);
  AssertConverted;
  AssertEquals('one type keyword',1,CountInterfaceLines('type'));
  AssertEquals('one const keyword',1,CountInterfaceLines('const'));
end;


procedure TTestOneTypeSection.TestPointerToStructDeclaredLater;

begin
  Convert(FileHeader,['-d','-1']);
  AssertConverted;
  AssertInterface('the pointer types come first',['type','Psqlite3_file = ^sqlite3_file;']);
  AssertInterface('both records are in the type section',
    ['sqlite3_file = record','pMethods : ^sqlite3_io_methods;','end;','sqlite3_io_methods = record','iVersion : longint;',
     'xClose : function (_para1:Psqlite3_file):longint;cdecl;','end;']);
  AssertInterface('the functions follow the types',['function sqlite3_file_control(p:Psqlite3_file):longint;cdecl;external;']);
end;


procedure TTestOneTypeSection.TestPointerToStructDeclaredLaterCompiles;

begin
  Convert(FileHeader,['-d','-1']);
  AssertConverted;
  AssertCompiles;
end;


procedure TTestOneTypeSection.TestPointerTypedefBeforeStruct;

begin
  Convert(['typedef struct gzFile_s *gzFile;','int gzread(gzFile file);','struct gzFile_s { unsigned have; };'],['-d','-1']);
  AssertConverted;
  AssertInterface('the pointer typedef and the record share the type section',
    ['type','gzFile = ^gzFile_s;','gzFile_s = record','have : dword;','end;','function gzread(_file:gzFile):longint;']);
  AssertNotOutput('no empty record','undefined structure');
  AssertCompiles;
end;


procedure TTestOneTypeSection.TestPlainConstantsBeforeTypes;

begin
  Convert(['#define MAXN 4','struct s { char name[MAXN]; };','int f(struct s *p);','#define LATER 5','#define PLAIN (MAXN * 2)'],
          ['-d','-1']);
  AssertConverted;
  AssertInterface('plain constants precede the types',
    ['const','MAXN = 4;','LATER = 5;','PLAIN = MAXN*2;','type','Ps = ^s;','s = record','name : array[0..(MAXN)-1] of ansichar;']);
  AssertCompiles;
end;


procedure TTestOneTypeSection.TestConstantsUsingTypesAfterTypes;

begin
  Convert(['typedef int myint;','enum e { E1, E2 };','#define X E1','#define Y X','#define N 3','int f(myint a);'],
          ['-d','-1']);
  AssertConverted;
  AssertInterface('a plain constant precedes the types',['const','N = 3;','type']);
  AssertInterface('constants of enum members follow the types',['e = (E1,E2);','const','X = E1;','Y = X;']);
  AssertCompiles;
end;


procedure TTestOneTypeSection.TestConditionalsInEachSection;

begin
  Convert(['#ifdef A','typedef int a_t;','#define CA 1','int fa(a_t x);','#else','typedef long a_t;','#endif',
           'typedef a_t b_t;','int fb(b_t y);'],['-d','-1']);
  AssertConverted;
  AssertInterface('the constant keeps its condition',['{$ifdef A}','const','CA = 1;','{$else}','{$endif}']);
  AssertInterface('the types keep their conditions',
    ['type','{$ifdef A}','a_t = longint;','{$else}','a_t = longint;','{$endif}','b_t = a_t;']);
  AssertInterface('the functions keep their conditions',
    ['{$ifdef A}','function fa(x:a_t):longint;cdecl;external;','{$else}','{$endif}','function fb(y:b_t):longint;cdecl;external;']);
end;


procedure TTestOneTypeSection.TestConditionalsCompile;

begin
  Convert(['#ifdef A','typedef int a_t;','#define CA 1','int fa(a_t x);','#else','typedef long a_t;','#endif',
           'typedef a_t b_t;','int fb(b_t y);'],['-d','-1']);
  AssertConverted;
  AssertCompiles;
end;


procedure TTestOneTypeSection.TestCommentsMoveWithDeclarations;

begin
  Convert(['/* doc of f */','int f(void);','/* doc of t */','typedef int t;','/* doc of x */','#define XX 1'],['-d','-1']);
  AssertConverted;
  AssertInterface('the comment of the constant',['const','{ doc of x }','XX = 1;']);
  AssertInterface('the comment of the type',['type','{ doc of t }','t = longint;']);
  AssertInterface('the comment of the function',['{ doc of f }','function f:longint;cdecl;external;']);
end;


procedure TTestOneTypeSection.TestPackRecordsKept;

begin
  Convert(['#pragma pack(push, 1)','struct p { char a; int b; };','#pragma pack(pop)','int g(struct p *q);'],['-d','-1']);
  AssertConverted;
  AssertInterface('the record keeps its alignment',
    ['{$PACKRECORDS 1}','p = record','a : ansichar;','b : longint;','end;','{$PACKRECORDS C}']);
  AssertCompiles;
end;


procedure TTestOneTypeSection.TestEnumToConst;

begin
  Convert(['enum e { E1, E2 = E1 + 3 };','#define X (E2 + 1)','typedef enum e e_t;','int g(e_t v);'],['-d','-1','-e']);
  AssertConverted;
  AssertInterface('the enum constants precede the types',['const','E1 = 0;','E2 = E1+3;','X = E2+1;','type','e = Longint;']);
  AssertCompiles;
end;


procedure TTestOneTypeSection.TestPointerToUndeclaredType;

begin
  Convert(['int f(struct other *p, extern_t *q);'],['-d','-1']);
  AssertConverted;
  AssertInterface('pointers to types of other units are in the type section',['type','Pextern_t = ^extern_t;','Pother = ^other;']);
  AssertNotOutput('no separate pointer list','Type');
end;


procedure TTestOneTypeSection.TestWithoutOptionUnchanged;

begin
  Convert(FileHeader,['-d']);
  AssertConverted;
  AssertTrue('without -1 each declaration has its own section',CountInterfaceLines('type')>1);
end;


procedure TTestOneTypeSection.TestOneTypeSectionCompiles;

begin
  Convert(['#define MAXN 4','/* the file */','typedef struct afile file_t;','struct afile { const struct methods *m; char name[MAXN]; };',
           'int open_file(file_t *f);','#define FLAG_A 1','typedef struct methods methods;',
           'struct methods { int (*close)(file_t *f); int (*read)(file_t *f, void *buf, int n); };',
           'typedef struct vtab vtab;','typedef struct module { int (*create)(vtab **pp); } module;',
           'int create_module(const module *m);','#define LIMIT (MAXN + FLAG_A)',
           'struct vtab { const module *pModule; int nRef; };','extern int counter;'],['-d','-1']);
  AssertConverted;
  AssertEquals('one type keyword',1,CountInterfaceLines('type'));
  AssertCompiles;
end;


initialization
  RegisterTest('H2Pas',TTestOneTypeSection);
end.
