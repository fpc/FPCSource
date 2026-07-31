{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    The clean-code language-surface tests

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit utstLanguageSurface;


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  FpSonar.Types, FpSonar.Resolver, FpSonar.SourceFile, UtstFixtures;

type
  // Whether a staged fixture resolves fully or degrades under the synthetic RTL.
  TLanguageSurfaceResolution = (lsrResolves, lsrDegrades);

  { One analyzable corpus fixture with everything a consumer needs to run it. }
  TLanguageSurfaceEntry = record
    Name: string;                 // file name inside the fixture directory
    Family: string;               // one of the 15 construct-family labels
    Mode: string;                 // compiler mode to hand to Analyze
    Resolution: TLanguageSurfaceResolution;
    Reason: string;               // why it degrades; empty when it resolves
    Diagnostic: string;           // text the resolve diagnostic must contain
    Markers: TFpSonarStringArray; // substrings that must occur in the source
    Path: string;                 // full path, filled in by staging
  end;

  TLanguageSurfaceEntryArray = array of TLanguageSurfaceEntry;

  { Tests fitness: every family parses, resolves as declared, and still holds
    the constructs it exists to cover. }
  TLanguageSurfaceTest = class(TTestCase)
  private
    FFixtures: TTempFixtures;
    FEntries: TLanguageSurfaceEntryArray;
    procedure CheckEntry(const aEntry: TLanguageSurfaceEntry);
    procedure CheckFamily(const aFamily: string);
    function ReadFixture(const aEntry: TLanguageSurfaceEntry): string;
    function ResolveFailure(const aEntry: TLanguageSurfaceEntry): string;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure GenericsFixtureIsClean;
    procedure OperatorsFixtureIsClean;
    procedure HelpersFixtureIsClean;
    procedure RecordsFixtureIsClean;
    procedure StorageFixtureIsClean;
    procedure ClosuresFixtureIsClean;
    procedure AttributesFixtureIsClean;
    procedure ParametersFixtureIsClean;
    procedure PreprocessorFixtureIsClean;
    procedure InterfacesFixtureIsClean;
    procedure ExceptionsFixtureIsClean;
    procedure FlowFixtureIsClean;
    procedure StringsFixtureIsClean;
    procedure ModifiersFixtureIsClean;
    procedure ModulesFixtureIsClean;
    procedure CorpusStagesEveryFamilyNonVacuously;
    procedure CorpusMarkersWitnessEveryConstruct;
    procedure CorpusMarkerMatchingSeesCodeOnly;
    procedure CorpusDegradationsAreJustified;
  end;

// Writes every corpus file into aFixtures and returns one entry per analyzable
// fixture; include payloads are written but never returned.
function StageLanguageSurfaceCorpus(aFixtures: TTempFixtures): TLanguageSurfaceEntryArray;
// The 15 construct-family labels the corpus covers.
function LanguageSurfaceFamilies: TFpSonarStringArray;
// The define set every corpus fixture must be analyzed under
function LanguageSurfaceDefines: TFpSonarStringArray;


implementation

const
  // The suite-wide define set every corpus fixture is analyzed under.
  cSurfaceDefines: array[0..3] of string = ('FPC', 'CPUX86_64', 'UNIX', 'LINUX');

  // The include payload: staged into the fixture directory, never analyzed.
  cIncludeName = 'surfacepreprocessor.inc';

  // What comment text is replaced with before a marker search
  cCommentFill = #1;

  cFamilyLabels: array[0..14] of string = ('generics', 'operators', 'helpers',
    'records', 'storage', 'closures', 'attributes', 'parameters',
    'preprocessor', 'interfaces', 'exceptions', 'flow', 'strings', 'modifiers',
    'modules');

  // How many analyzable files each family of cFamilyLabels stages
  cFamilyFileCounts: array[0..14] of integer = (1, 1, 1,
    1, 1, 2, 1, 1,
    2, 2, 1, 2, 1, 1,
    3);

  cDiagnosticNames: array[TFpSonarDiagnosticKind] of string = ('parse', 'scan',
    'resolve', 'file-not-found');

  // The complete set of fixtures allowed to degrade, pinned as a set rather than a count
  cDegradingFixtures: array[0..2] of string = ('surfaceattributes.pas',
    'surfacedispinterface.pas', 'surfaceflowlabel.pas');

  cAttributesReason = 'identifier not found "TCustomAttribute": the synthetic '
    + 'System has no attribute base class';
  cAttributesDiagnostic = 'identifier not found "TCustomAttribute"';
  cDispInterfaceReason = 'not yet implemented: TPasClassType '
    + '"Kind=dispinterface" - the resolver has no dispinterface support';
  cDispInterfaceDiagnostic = '"Kind=dispinterface"';
  cFlowReason = 'not yet implemented: TPasLabels - the resolver has no label '
    + 'declaration support';
  cFlowDiagnostic = 'TPasLabels';

  // Embedded corpus sources: line i+1 of the staged fixture == element [i].

  cGenericsSource: array[0..98] of string = (
    'unit SurfaceGenerics;',
    '',
    '{ Generic classes, generic routines, constrained parameters and',
    '  specializations, both at type level and inline in an expression. }',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  { A minimal generic container. }',
    '  generic TBox<T> = class(TObject)',
    '  private',
    '    FValue: T;',
    '  public',
    '    // Stores a value in the box.',
    '    procedure Put(const aValue: T);',
    '    // Returns the stored value.',
    '    function Get: T;',
    '  end;',
    '',
    '  // A box holding an integer.',
    '  TIntegerBox = specialize TBox<Integer>;',
    '',
    '  { A generic pair constrained to class instances. }',
    '  generic TOwnedPair<T: TObject> = class(TObject)',
    '  private',
    '    FFirst: T;',
    '    FSecond: T;',
    '  public',
    '    // Drops both references without freeing them.',
    '    procedure Clear;',
    '    // The first member.',
    '    property First: T read FFirst write FFirst;',
    '    // The second member.',
    '    property Second: T read FSecond write FSecond;',
    '  end;',
    '',
    '// Returns the larger of two comparable values.',
    'generic function MaxOf<T>(const aLeft, aRight: T): T;',
    '',
    '// Returns the value a specialized integer box hands back.',
    'function BoxedValue: Integer;',
    '',
    'implementation',
    '',
    'procedure TBox.Put(const aValue: T);',
    '',
    'begin',
    '  FValue := aValue;',
    'end;',
    '',
    '',
    'function TBox.Get: T;',
    '',
    'begin',
    '  Result := FValue;',
    'end;',
    '',
    '',
    'procedure TOwnedPair.Clear;',
    '',
    'begin',
    '  FFirst := nil;',
    '  FSecond := nil;',
    'end;',
    '',
    '',
    'generic function MaxOf<T>(const aLeft, aRight: T): T;',
    '',
    'begin',
    '  if aLeft < aRight then',
    '  begin',
    '    Result := aRight;',
    '  end',
    '  else',
    '  begin',
    '    Result := aLeft;',
    '  end;',
    'end;',
    '',
    '',
    'function BoxedValue: Integer;',
    '',
    'var',
    '  lBox: TIntegerBox;',
    '',
    'begin',
    '  lBox := TIntegerBox.Create;',
    '  try',
    '    lBox.Put(specialize MaxOf<Integer>(1, 2));',
    '    Result := lBox.Get;',
    '  finally',
    '    lBox.Free;',
    '  end;',
    'end;',
    '',
    '',
    'end.');

  // No "class const": the parser rejects it (pparser.pp:8259).
  cOperatorsSource: array[0..102] of string = (
    'unit SurfaceOperators;',
    '',
    '{ Operator overloading and class-level members: a global operator, class',
    '  operators on a record, and a class carrying class variables, a class',
    '  property, a class constructor and destructor and a static method. }',
    '',
    '{$mode objfpc}{$H+}',
    '{$modeswitch advancedrecords}',
    '',
    'interface',
    '',
    'type',
    '  { A value type carrying class-level state and operator overloads. }',
    '  TCounter = record',
    '  private',
    '    class var FInstances: Integer;',
    '  public',
    '    Value: Integer;',
    '    const',
    '      cStep = 1;',
    '    // Resets the shared instance count.',
    '    class constructor Init;',
    '    // Adds two counters componentwise.',
    '    class operator +(const aLeft, aRight: TCounter): TCounter;',
    '    // Reports whether two counters hold the same value.',
    '    class operator =(const aLeft, aRight: TCounter): Boolean;',
    '    // The number of counters handed out.',
    '    class property Instances: Integer read FInstances write FInstances;',
    '  end;',
    '',
    '  { A class carrying the class-level member forms, which a record can only',
    '    show alongside its operators. }',
    '  TTally = class(TObject)',
    '  private',
    '    class var FTotal: Integer;',
    '  public',
    '    // Clears the running total.',
    '    class constructor Setup;',
    '    // Releases the running total.',
    '    class destructor Teardown;',
    '    // Returns aLeft plus aRight without an instance.',
    '    class function Sum(aLeft, aRight: Integer): Integer; static;',
    '    // The running total.',
    '    class property Total: Integer read FTotal write FTotal;',
    '  end;',
    '',
    '// Advances a counter by aSteps single steps.',
    'operator + (const aLeft: TCounter; aSteps: Integer): TCounter;',
    '',
    'implementation',
    '',
    'class constructor TCounter.Init;',
    '',
    'begin',
    '  FInstances := 0;',
    'end;',
    '',
    '',
    'class operator TCounter.+(const aLeft, aRight: TCounter): TCounter;',
    '',
    'begin',
    '  Result.Value := aLeft.Value + aRight.Value;',
    'end;',
    '',
    '',
    'class operator TCounter.=(const aLeft, aRight: TCounter): Boolean;',
    '',
    'begin',
    '  Result := aLeft.Value = aRight.Value;',
    'end;',
    '',
    '',
    'class constructor TTally.Setup;',
    '',
    'begin',
    '  FTotal := 0;',
    'end;',
    '',
    '',
    'class destructor TTally.Teardown;',
    '',
    'begin',
    '  FTotal := 0;',
    'end;',
    '',
    '',
    'class function TTally.Sum(aLeft, aRight: Integer): Integer; static;',
    '',
    'begin',
    '  Result := aLeft + aRight;',
    'end;',
    '',
    '',
    'operator + (const aLeft: TCounter; aSteps: Integer): TCounter;',
    '',
    'begin',
    '  Result.Value := aLeft.Value + aSteps * TCounter.cStep;',
    '  TCounter.Instances := TCounter.Instances + 1;',
    '  TTally.Total := TTally.Sum(TTally.Total, aSteps);',
    'end;',
    '',
    '',
    'end.');

  cHelpersSource: array[0..64] of string = (
    'unit SurfaceHelpers;',
    '',
    '{ The three helper flavours: class helper, record helper and type helper. }',
    '',
    '{$mode objfpc}{$H+}',
    '{$modeswitch advancedrecords}',
    '{$modeswitch typehelpers}',
    '',
    'interface',
    '',
    'type',
    '  { A plain coordinate pair extended by a record helper below. }',
    '  TPoint2D = record',
    '    Horizontal: Integer;',
    '    Vertical: Integer;',
    '  end;',
    '',
    '  { Extends every class instance. }',
    '  TObjectHelper = class helper for TObject',
    '  public',
    '    // Returns the class name in lower case.',
    '    function LowerClassName: string;',
    '  end;',
    '',
    '  { Extends the coordinate pair. }',
    '  TPoint2DHelper = record helper for TPoint2D',
    '  public',
    '    // Returns the sum of both coordinates.',
    '    function Sum: Integer;',
    '  end;',
    '',
    '  { Extends the plain integer type. }',
    '  TIntegerHelper = type helper for Integer',
    '  public',
    '    // Returns the value as decimal text.',
    '    function ToText: string;',
    '  end;',
    '',
    'implementation',
    '',
    'uses',
    '  SysUtils;',
    '',
    'function TObjectHelper.LowerClassName: string;',
    '',
    'begin',
    '  Result := LowerCase(ClassName);',
    'end;',
    '',
    '',
    'function TPoint2DHelper.Sum: Integer;',
    '',
    'begin',
    '  Result := Horizontal + Vertical;',
    'end;',
    '',
    '',
    'function TIntegerHelper.ToText: string;',
    '',
    'begin',
    '  Result := IntToStr(Self);',
    'end;',
    '',
    '',
    'end.');

  cRecordsSource: array[0..67] of string = (
    'unit SurfaceRecords;',
    '',
    '{ Record layouts: a variant record with a case part, a packed record and a',
    '  bitpacked record. }',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  { Selects which payload the shape record carries. }',
    '  TShapeKind = (skCircle, skRectangle);',
    '',
    '  { A variant record whose payload follows its kind. }',
    '  TShape = record',
    '    Caption: ShortString;',
    '    case Kind: TShapeKind of',
    '      skCircle: (Radius: Double);',
    '      skRectangle: (Width: Double; Height: Double);',
    '  end;',
    '',
    '  { A byte-aligned wire header. }',
    '  TPackedHeader = packed record',
    '    Signature: array[0..3] of AnsiChar;',
    '    PayloadSize: Word;',
    '  end;',
    '',
    '  { A bit-aligned flag set. }',
    '  TBitFlags = bitpacked record',
    '    Enabled: Boolean;',
    '    Ready: Boolean;',
    '    Level: 0..15;',
    '  end;',
    '',
    '// Returns the area of aShape.',
    'function ShapeArea(const aShape: TShape): Double;',
    '',
    '// Returns the payload size announced by aHeader when aFlags say it is ready.',
    'function ReadySize(const aHeader: TPackedHeader; const aFlags: TBitFlags): Word;',
    '',
    'implementation',
    '',
    'function ShapeArea(const aShape: TShape): Double;',
    '',
    'begin',
    '  Result := 0;',
    '  case aShape.Kind of',
    '    skCircle: Result := aShape.Radius * aShape.Radius;',
    '    skRectangle: Result := aShape.Width * aShape.Height;',
    '  end;',
    'end;',
    '',
    '',
    'function ReadySize(const aHeader: TPackedHeader; const aFlags: TBitFlags): Word;',
    '',
    'begin',
    '  if aFlags.Enabled and aFlags.Ready and (aFlags.Level > 0) then',
    '  begin',
    '    Result := aHeader.PayloadSize;',
    '  end',
    '  else',
    '  begin',
    '    Result := 0;',
    '  end;',
    'end;',
    '',
    '',
    'end.');

  cStorageSource: array[0..63] of string = (
    'unit SurfaceStorage;',
    '',
    '{ Storage specifiers: resource strings, typed constants, initialized globals,',
    '  a thread variable and an absolute alias. }',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'resourcestring',
    '  rsGreeting = ''Hello'';',
    '  rsFarewell = ''Goodbye'';',
    '',
    'const',
    '  cLimits: array[0..2] of Integer = (1, 2, 3);',
    '  cOrigin: array[0..1] of Double = (0.0, 0.0);',
    '',
    'var',
    '  GStartValue: Integer = 7;',
    '  GStartText: string = ''ready'';',
    '',
    'threadvar',
    '  GPerThreadDepth: Integer;',
    '',
    '// Returns the low byte of the first limit through an absolute alias.',
    'function FirstLimitByte: Byte;',
    '',
    '// Returns the greeting and farewell resource strings joined.',
    'function Greeting: string;',
    '',
    '// Returns the distance of the origin from the start value.',
    'function OriginOffset: Double;',
    '',
    'implementation',
    '',
    'function FirstLimitByte: Byte;',
    '',
    'var',
    '  lValue: Integer;',
    '  lBytes: array[0..3] of Byte absolute lValue;',
    '',
    'begin',
    '  lValue := cLimits[0];',
    '  Result := lBytes[0];',
    'end;',
    '',
    '',
    'function Greeting: string;',
    '',
    'begin',
    '  Inc(GPerThreadDepth);',
    '  Result := rsGreeting + '' '' + rsFarewell + '' '' + GStartText;',
    '  Dec(GPerThreadDepth);',
    'end;',
    '',
    '',
    'function OriginOffset: Double;',
    '',
    'begin',
    '  Result := cOrigin[0] + cOrigin[1] - GStartValue;',
    'end;',
    '',
    '',
    'end.');

  cClosuresDelphiSource: array[0..72] of string = (
    'unit SurfaceClosuresDelphi;',
    '',
    '{ Function references and anonymous methods, the Delphi-mode half of the',
    '  closure surface: one anonymous method as a call argument and one opening',
    '  a statement. }',
    '',
    '{$mode delphi}{$H+}',
    '{$modeswitch anonymousfunctions}',
    '',
    'interface',
    '',
    'type',
    '  // Receives one integer per visited element.',
    '  TIntegerCallback = reference to procedure(aValue: Integer);',
    '  // Maps one integer onto another.',
    '  TIntegerMapper = reference to function(aValue: Integer): Integer;',
    '',
    '// Calls aCallback once for each of the first aCount naturals.',
    'procedure ForEachNatural(aCount: Integer; const aCallback: TIntegerCallback);',
    '',
    '// Returns the sum of the first aCount naturals mapped through aMapper.',
    'function SumMapped(aCount: Integer; const aMapper: TIntegerMapper): Integer;',
    '',
    '// Returns aCount collected by an anonymous method invoked where it stands.',
    'function SumCollected(aCount: Integer): Integer;',
    '',
    'implementation',
    '',
    'procedure ForEachNatural(aCount: Integer; const aCallback: TIntegerCallback);',
    '',
    'var',
    '  lIndex: Integer;',
    '',
    'begin',
    '  for lIndex := 1 to aCount do',
    '  begin',
    '    aCallback(lIndex);',
    '  end;',
    'end;',
    '',
    '',
    'function SumMapped(aCount: Integer; const aMapper: TIntegerMapper): Integer;',
    '',
    'var',
    '  lTotal: Integer;',
    '',
    'begin',
    '  lTotal := 0;',
    '  ForEachNatural(aCount,',
    '    procedure(aMapped: Integer)',
    '    begin',
    '      lTotal := lTotal + aMapper(aMapped);',
    '    end);',
    '  Result := lTotal;',
    'end;',
    '',
    '',
    'function SumCollected(aCount: Integer): Integer;',
    '',
    'var',
    '  lTotal: Integer;',
    '',
    'begin',
    '  lTotal := 0;',
    '  procedure(aCollected: Integer)',
    '  begin',
    '    lTotal := lTotal + aCollected;',
    '  end(aCount);',
    '  Result := lTotal;',
    'end;',
    '',
    '',
    'end.');

  cClosuresNestedSource: array[0..52] of string = (
    'unit SurfaceClosuresNested;',
    '',
    '{ Nested procedural variables, the objfpc half of the closure surface. }',
    '',
    '{$mode objfpc}{$H+}',
    '{$modeswitch nestedprocvars}',
    '',
    'interface',
    '',
    'type',
    '  // A nested routine callable per visited element.',
    '  TNestedVisitor = procedure(aValue: Integer) is nested;',
    '',
    '// Calls aVisitor once for each of the first aCount naturals.',
    'procedure VisitNaturals(aCount: Integer; aVisitor: TNestedVisitor);',
    '',
    '// Returns the sum of the first aCount naturals gathered by a nested routine.',
    'function SumNaturals(aCount: Integer): Integer;',
    '',
    'implementation',
    '',
    'procedure VisitNaturals(aCount: Integer; aVisitor: TNestedVisitor);',
    '',
    'var',
    '  lIndex: Integer;',
    '',
    'begin',
    '  for lIndex := 1 to aCount do',
    '  begin',
    '    aVisitor(lIndex);',
    '  end;',
    'end;',
    '',
    '',
    'function SumNaturals(aCount: Integer): Integer;',
    '',
    'var',
    '  lTotal: Integer;',
    '',
    '  procedure Accumulate(aValue: Integer);',
    '',
    '  begin',
    '    lTotal := lTotal + aValue;',
    '  end;',
    '',
    'begin',
    '  lTotal := 0;',
    '  VisitNaturals(aCount, @Accumulate);',
    '  Result := lTotal;',
    'end;',
    '',
    '',
    'end.');

  // No attribute on a parameter: FPC 3.3.1 rejects it, and the vendored parser
  // accepts it but dies with an EAccessViolation during teardown.
  cAttributesSource: array[0..53] of string = (
    'unit SurfaceAttributes;',
    '',
    '{ Attributes applied to a type and to three kinds of member. }',
    '',
    '{$mode delphi}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  { The attribute this unit applies. }',
    '  DescriptionAttribute = class(TCustomAttribute)',
    '  private',
    '    FText: string;',
    '  public',
    '    // Stores the description text.',
    '    constructor Create(const aText: string);',
    '    // The description text.',
    '    property Text: string read FText;',
    '  end;',
    '',
    '  { A class carrying attributes on itself, on a field, on a method and on a',
    '    property. }',
    '  [Description(''a documented class'')]',
    '  TDocumented = class(TObject)',
    '  private',
    '    [Description(''a documented field'')]',
    '    FValue: Integer;',
    '  public',
    '    [Description(''a documented method'')]',
    '    // Stores the supplied value.',
    '    procedure SetValue(aValue: Integer);',
    '    [Description(''a documented property'')]',
    '    // The value stored last.',
    '    property Value: Integer read FValue;',
    '  end;',
    '',
    'implementation',
    '',
    'constructor DescriptionAttribute.Create(const aText: string);',
    '',
    'begin',
    '  inherited Create;',
    '  FText := aText;',
    'end;',
    '',
    '',
    'procedure TDocumented.SetValue(aValue: Integer);',
    '',
    'begin',
    '  FValue := aValue;',
    'end;',
    '',
    '',
    'end.');

  cParametersSource: array[0..72] of string = (
    'unit SurfaceParameters;',
    '',
    '{ Parameter forms: an array of const, an open array, a varargs external',
    '  routine and the cdecl/stdcall calling conventions. }',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    '// Returns how many variant arguments were supplied.',
    'function CountArguments(const aArgs: array of const): Integer;',
    '',
    '// Returns the sum of an open array of integers.',
    'function SumOpenArray(const aValues: array of Integer): Integer;',
    '',
    '// Writes a formatted message into aBuffer through the C library.',
    'function FormatNative(aBuffer: PAnsiChar; const aFormat: PAnsiChar): Integer;',
    '  cdecl; varargs; external ''c'' name ''sprintf'';',
    '',
    '// Returns the identifier of the current process.',
    'function ProcessId: Integer; cdecl; external ''c'' name ''getpid'';',
    '',
    '// Reports a status code to the host over the stdcall convention.',
    'procedure NotifyHost(aCode: Integer); stdcall;',
    '',
    '// Returns how many arguments the last report carried.',
    'function LastNotifiedCount: Integer;',
    '',
    'implementation',
    '',
    'var',
    '  GLastNotifiedCount: Integer;',
    '',
    'function CountArguments(const aArgs: array of const): Integer;',
    '',
    'begin',
    '  Result := Length(aArgs);',
    'end;',
    '',
    '',
    'function SumOpenArray(const aValues: array of Integer): Integer;',
    '',
    'var',
    '  lIndex: Integer;',
    '',
    'begin',
    '  Result := 0;',
    '  for lIndex := Low(aValues) to High(aValues) do',
    '  begin',
    '    Result := Result + aValues[lIndex];',
    '  end;',
    'end;',
    '',
    '',
    'procedure NotifyHost(aCode: Integer); stdcall;',
    '',
    'var',
    '  lTotal: Integer;',
    '',
    'begin',
    '  lTotal := CountArguments([aCode, ProcessId]);',
    '  GLastNotifiedCount := lTotal;',
    'end;',
    '',
    '',
    'function LastNotifiedCount: Integer;',
    '',
    'begin',
    '  Result := GLastNotifiedCount;',
    'end;',
    '',
    '',
    'end.');

  cPreprocessorSource: array[0..41] of string = (
    'unit SurfacePreprocessor;',
    '',
    '{ Conditional compilation, an included fragment and a macro identifier. }',
    '',
    '{$mode objfpc}{$H+}',
    '{$macro on}',
    '{$define cMaxDepth := 8}',
    '',
    'interface',
    '',
    'const',
    '  cBuildLimit = cMaxDepth;',
    '',
    '// Returns the platform tag chosen at compile time.',
    'function PlatformTag: string;',
    '',
    '// Returns the depth limit combining the macro and the included fragment.',
    'function DepthLimit: Integer;',
    '',
    'implementation',
    '',
    '{$include surfacepreprocessor.inc}',
    '',
    'function PlatformTag: string;',
    '',
    'begin',
    '{$ifdef LINUX}',
    '  Result := ''linux'';',
    '{$else}',
    '  Result := ''other'';',
    '{$endif}',
    'end;',
    '',
    '',
    'function DepthLimit: Integer;',
    '',
    'begin',
    '  Result := cIncludedDepth * cBuildLimit;',
    'end;',
    '',
    '',
    'end.');

  // The second half of the preprocessor family: SURFACE_UNSET is deliberately
  // outside cSurfaceDefines.
  cPreprocessorAltSource: array[0..43] of string = (
    'unit SurfacePreprocessorAlt;',
    '',
    '{ The untaken-branch half of conditional compilation: the tested symbol is',
    '  never defined, so the else arm is the code that reaches the parser. }',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    '// Returns the tag the fallback branch of the conditional selects.',
    'function FallbackTag: string;',
    '',
    '// Returns the depth the fallback branch of the conditional selects.',
    'function FallbackDepth: Integer;',
    '',
    'implementation',
    '',
    '{$ifdef SURFACE_UNSET}',
    'const',
    '  cFallbackDepth = 1;',
    '{$else}',
    'const',
    '  cFallbackDepth = 2;',
    '{$endif}',
    '',
    'function FallbackTag: string;',
    '',
    'begin',
    '{$ifdef SURFACE_UNSET}',
    '  Result := ''defined'';',
    '{$else}',
    '  Result := ''undefined'';',
    '{$endif}',
    'end;',
    '',
    '',
    'function FallbackDepth: Integer;',
    '',
    'begin',
    '  Result := cFallbackDepth;',
    'end;',
    '',
    '',
    'end.');

  cPreprocessorInclude: array[0..3] of string = (
    '{ Included by surfacepreprocessor.pas; never analyzed on its own. }',
    '',
    'const',
    '  cIncludedDepth = 3;');

  cInterfacesSource: array[0..94] of string = (
    'unit SurfaceInterfaces;',
    '',
    '{ A COM interface with a GUID and a CORBA interface with no ancestor. }',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  { A reference-counted interface identified by a GUID. }',
    '  IStorage = interface(IInterface)',
    '    [''{2C6F5C0E-1B7A-4C33-9E1D-2B3A4C5D6E7F}'']',
    '    // Stores a value.',
    '    procedure Put(aValue: Integer);',
    '    // Returns the value stored last.',
    '    function Get: Integer;',
    '  end;',
    '',
    '{$interfaces corba}',
    '',
    '  { A plain, non-reference-counted interface with no ancestor. }',
    '  ILogger = interface',
    '    // Appends one line to the log.',
    '    procedure Log(const aLine: string);',
    '  end;',
    '',
    '{$interfaces com}',
    '',
    '  { An interface with no explicit ancestor again, but declared after the',
    '    restore: under com the compiler supplies IInterface, which is the',
    '    difference the two directives switch between. }',
    '  IResettable = interface',
    '    [''{4D3C2B1A-6E5F-4A3B-9C8D-1F0E9D8C7B6A}'']',
    '    // Clears the stored value.',
    '    procedure Reset;',
    '  end;',
    '',
    '  { A CORBA logger: a plain object, because a CORBA interface carries no',
    '    reference counting of its own. }',
    '  TConsoleLogger = class(TObject, ILogger)',
    '  public',
    '    // Appends one line to the log.',
    '    procedure Log(const aLine: string);',
    '  end;',
    '',
    '  { A storage implementation kept alive by reference counting. }',
    '  TMemoryStorage = class(TInterfacedObject, IStorage)',
    '  private',
    '    FValue: Integer;',
    '  public',
    '    // Stores a value.',
    '    procedure Put(aValue: Integer);',
    '    // Returns the value stored last.',
    '    function Get: Integer;',
    '  end;',
    '',
    '// Returns a logger the caller can append lines to.',
    'function Logger: ILogger;',
    '',
    'implementation',
    '',
    'var',
    '  GLogLength: Integer;',
    '',
    'procedure TConsoleLogger.Log(const aLine: string);',
    '',
    'begin',
    '  GLogLength := GLogLength + Length(aLine);',
    'end;',
    '',
    '',
    'function Logger: ILogger;',
    '',
    'begin',
    '  { A plain object assigned to a CORBA interface: the site the corba half of',
    '    the object-as-interface question is decided on. }',
    '  Result := TConsoleLogger.Create;',
    'end;',
    '',
    '',
    'procedure TMemoryStorage.Put(aValue: Integer);',
    '',
    'begin',
    '  FValue := aValue;',
    'end;',
    '',
    '',
    'function TMemoryStorage.Get: Integer;',
    '',
    'begin',
    '  Result := FValue;',
    'end;',
    '',
    '',
    'end.');

  cDispInterfaceSource: array[0..21] of string = (
    'unit SurfaceDispInterface;',
    '',
    '{ The dispatch-interface flavour, kept in its own module because the resolver',
    '  does not implement it and one unresolved construct degrades a whole file. }',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  { A dispatch interface addressed by dispatch identifier. }',
    '  IAutomation = dispinterface',
    '    [''{9A8B7C6D-5E4F-4A3B-8C1D-0E9F8A7B6C5D}'']',
    '    // Refreshes the automation object.',
    '    procedure Refresh; dispid 1;',
    '    // The caption the automation object presents.',
    '    property Caption: WideString dispid 2;',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  cExceptionsSource: array[0..81] of string = (
    'unit SurfaceExceptions;',
    '',
    '{ Exception handling: typed and bare handlers, an else branch, a re-raise at',
    '  an explicit address and a try..finally nested in a try..except. }',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  SysUtils;',
    '',
    'type',
    '  { Raised when a value falls outside the accepted range. }',
    '  ERangeRejected = class(Exception);',
    '',
    '  { Raised when a value cannot be interpreted at all. }',
    '  EValueUnreadable = class(Exception);',
    '',
    '// Returns aValue when it is in range, or 0 for a rejected or unreadable one.',
    'function GuardedValue(aValue: Integer): Integer;',
    '',
    '// Re-raises aError as if it had been raised at aAddress.',
    'procedure RethrowAt(aError: Exception; aAddress: Pointer);',
    '',
    'implementation',
    '',
    'uses',
    '  Classes;',
    '',
    'const',
    '  cUnhandledCode = -1;',
    '',
    'procedure CheckRange(aValue: Integer);',
    '',
    'begin',
    '  if aValue < 0 then',
    '  begin',
    '    raise ERangeRejected.Create(''negative'');',
    '  end',
    '  else if aValue > 100 then',
    '  begin',
    '    raise EValueUnreadable.Create(''too large'');',
    '  end;',
    'end;',
    '',
    '',
    'function GuardedValue(aValue: Integer): Integer;',
    '',
    'var',
    '  lScratch: TStringList;',
    '',
    'begin',
    '  Result := 0;',
    '  try',
    '    lScratch := TStringList.Create;',
    '    try',
    '      CheckRange(aValue);',
    '      lScratch.Add(IntToStr(aValue));',
    '      Result := aValue;',
    '    finally',
    '      lScratch.Free;',
    '    end;',
    '  except',
    '    on E: ERangeRejected do',
    '      Result := -Length(E.Message);',
    '    on EValueUnreadable do',
    '      Result := 0;',
    '    else',
    '      Result := cUnhandledCode;',
    '  end;',
    'end;',
    '',
    '',
    'procedure RethrowAt(aError: Exception; aAddress: Pointer);',
    '',
    'begin',
    '  raise aError at aAddress;',
    'end;',
    '',
    '',
    'end.');

  // The flow family is two files: the label declaration is unimplemented in the
  // resolver, and one unresolved construct degrades every rule on its file.
  cFlowLabelSource: array[0..36] of string = (
    'unit SurfaceFlowLabel;',
    '',
    '{ Unstructured control flow: a declared label and a goto reaching it. }',
    '',
    '{$mode objfpc}{$H+}',
    '{$goto on}',
    '',
    'interface',
    '',
    '// Returns the first index holding aTarget, or -1 when there is none.',
    'function IndexOfValue(const aValues: array of Integer; aTarget: Integer): Integer;',
    '',
    'implementation',
    '',
    'function IndexOfValue(const aValues: array of Integer; aTarget: Integer): Integer;',
    '',
    'label Found;',
    '',
    'var',
    '  lIndex: Integer;',
    '',
    'begin',
    '  for lIndex := Low(aValues) to High(aValues) do',
    '  begin',
    '    if aValues[lIndex] = aTarget then',
    '    begin',
    '      goto Found;',
    '    end;',
    '  end;',
    '  Result := -1;',
    '  Exit;',
    'Found:',
    '  Result := lIndex;',
    'end;',
    '',
    '',
    'end.');

  cFlowWithSource: array[0..35] of string = (
    'unit SurfaceFlowWith;',
    '',
    '{ Scoped control flow: a with statement opened over several expressions. }',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  { The innermost part of the aggregate visited below. }',
    '  TInner = record',
    '    Amount: Integer;',
    '  end;',
    '',
    '  { An aggregate read through a with statement. }',
    '  TOuter = record',
    '    Inner: TInner;',
    '    Factor: Integer;',
    '  end;',
    '',
    '// Returns the amount of aOuter scaled by its factor.',
    'function ScaledAmount(const aOuter: TOuter): Integer;',
    '',
    'implementation',
    '',
    'function ScaledAmount(const aOuter: TOuter): Integer;',
    '',
    'begin',
    '  with aOuter, aOuter.Inner do',
    '  begin',
    '    Result := Amount * Factor;',
    '  end;',
    'end;',
    '',
    '',
    'end.');

  cStringsSource: array[0..50] of string = (
    'unit SurfaceStrings;',
    '',
    '{ The string-type matrix: short, code-page-tagged ANSI, raw byte, unicode,',
    '  wide and UTF-8 strings. }',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  // An ANSI string tagged with the Windows-1252 code page.',
    '  TWindows1252String = type AnsiString(1252);',
    '',
    '// Returns the combined length of one value of every string flavour.',
    'function TotalLength: Integer;',
    '',
    '// Returns aValue widened to a unicode string.',
    'function Widen(const aValue: ShortString): UnicodeString;',
    '',
    'implementation',
    '',
    'function TotalLength: Integer;',
    '',
    'var',
    '  lShort: ShortString;',
    '  lAnsi: TWindows1252String;',
    '  lRaw: RawByteString;',
    '  lUnicode: UnicodeString;',
    '  lWide: WideString;',
    '  lUtf8: UTF8String;',
    '',
    'begin',
    '  lShort := ''short'';',
    '  lAnsi := ''ansi'';',
    '  lRaw := ''raw'';',
    '  lUnicode := ''unicode'';',
    '  lWide := ''wide'';',
    '  lUtf8 := ''utf8'';',
    '  Result := Length(lShort) + Length(lAnsi) + Length(lRaw) +',
    '    Length(lUnicode) + Length(lWide) + Length(lUtf8);',
    'end;',
    '',
    '',
    'function Widen(const aValue: ShortString): UnicodeString;',
    '',
    'begin',
    '  Result := UnicodeString(aValue);',
    'end;',
    '',
    '',
    'end.');

  cModifiersSource: array[0..126] of string = (
    'unit SurfaceModifiers;',
    '',
    '{ Routine modifiers: inline, the three hint directives, a forward declaration,',
    '  overloads and a reintroduced method. }',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  { The base of the reintroduction pair. }',
    '  TBase = class(TObject)',
    '  public',
    '    // Returns the tag identifying this level.',
    '    function Tag: Integer; virtual;',
    '  end;',
    '',
    '  { Hides the inherited Tag deliberately rather than overriding it. }',
    '  TDerived = class(TBase)',
    '  public',
    '    // Returns the tag identifying this level.',
    '    function Tag: Integer; reintroduce;',
    '  end;',
    '',
    '// Returns twice aValue.',
    'function Doubled(aValue: Integer): Integer; inline;',
    '',
    '// Returns aValue rendered as text.',
    'function Describe(aValue: Integer): string; overload;',
    '',
    '// Returns aText unchanged.',
    'function Describe(const aText: string): string; overload;',
    '',
    '// Returns aValue rendered as text.',
    'function OldDescribe(aValue: Integer): string; deprecated ''use Describe'';',
    '',
    '// Reports the value to the host console.',
    'procedure ReportValue(aValue: Integer); platform;',
    '',
    '// Returns the value reported last.',
    'function LastReported: Integer;',
    '',
    '// Returns a tag whose numbering may still change.',
    'function ProvisionalTag: Integer; experimental;',
    '',
    'implementation',
    '',
    'uses',
    '  SysUtils;',
    '',
    'var',
    '  GLastReported: Integer;',
    '',
    '// Resolved by the implementation further down.',
    'function Combine(aLeft, aRight: Integer): Integer; forward;',
    '',
    'function TBase.Tag: Integer;',
    '',
    'begin',
    '  Result := 1;',
    'end;',
    '',
    '',
    'function TDerived.Tag: Integer;',
    '',
    'begin',
    '  Result := 2;',
    'end;',
    '',
    '',
    'function Doubled(aValue: Integer): Integer; inline;',
    '',
    'begin',
    '  Result := aValue * 2;',
    'end;',
    '',
    '',
    'function Describe(aValue: Integer): string; overload;',
    '',
    'begin',
    '  Result := IntToStr(aValue);',
    'end;',
    '',
    '',
    'function Describe(const aText: string): string; overload;',
    '',
    'begin',
    '  Result := aText;',
    'end;',
    '',
    '',
    'function OldDescribe(aValue: Integer): string;',
    '',
    'begin',
    '  Result := Describe(aValue);',
    'end;',
    '',
    '',
    'procedure ReportValue(aValue: Integer);',
    '',
    'begin',
    '  GLastReported := Combine(aValue, Doubled(aValue));',
    'end;',
    '',
    '',
    'function LastReported: Integer;',
    '',
    'begin',
    '  Result := GLastReported;',
    'end;',
    '',
    '',
    'function ProvisionalTag: Integer;',
    '',
    'begin',
    '  Result := Combine(1, 2);',
    'end;',
    '',
    '',
    'function Combine(aLeft, aRight: Integer): Integer;',
    '',
    'begin',
    '  Result := aLeft + aRight;',
    'end;',
    '',
    '',
    'end.');

  cModuleSource: array[0..32] of string = (
    'unit SurfaceModule;',
    '',
    '{ The unit module form, with an initialization and a finalization section. }',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  Classes;',
    '',
    '// Returns the registry the initialization section created.',
    'function Registry: TStringList;',
    '',
    'implementation',
    '',
    'var',
    '  GRegistry: TStringList;',
    '',
    'function Registry: TStringList;',
    '',
    'begin',
    '  Result := GRegistry;',
    'end;',
    '',
    '',
    'initialization',
    '  GRegistry := TStringList.Create;',
    '',
    'finalization',
    '  GRegistry.Free;',
    '',
    'end.');

  cLibrarySource: array[0..27] of string = (
    'library SurfaceLibrary;',
    '',
    '{ The library module form, with an exports clause. }',
    '',
    '{$mode objfpc}{$H+}',
    '',
    '// Returns the ABI version this library implements.',
    'function AbiVersion: Integer; cdecl;',
    '',
    'begin',
    '  Result := 1;',
    'end;',
    '',
    '',
    '// Returns the number of slots the library offers.',
    'function SlotCount: Integer; cdecl;',
    '',
    'begin',
    '  Result := AbiVersion * 8;',
    'end;',
    '',
    '',
    'exports',
    '  AbiVersion name ''abi_version'',',
    '  SlotCount name ''slot_count'';',
    '',
    'begin',
    'end.');

  cProgramSource: array[0..26] of string = (
    'program SurfaceProgram;',
    '',
    '{ The program module form, with a main statement block. }',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'uses',
    '  SysUtils;',
    '',
    'var',
    '  GBanner: string;',
    '',
    '// Returns the banner this program announces itself with.',
    'function Banner: string;',
    '',
    'begin',
    '  Result := ''surface '' + IntToStr(1);',
    'end;',
    '',
    '',
    'begin',
    '  GBanner := Banner;',
    '  if Length(GBanner) = 0 then',
    '  begin',
    '    GBanner := ''surface'';',
    '  end;',
    'end.');

// Returns how many analyzable files aFamily must stage, or -1 when aFamily is
// not one of the family labels.
function FamilyFileCount(const aFamily: string): integer;

var
  lIndex: integer;

begin
  Result := -1;
  for lIndex := Low(cFamilyLabels) to High(cFamilyLabels) do
    if cFamilyLabels[lIndex] = aFamily then
      Exit(cFamilyFileCounts[lIndex]);
end;


// Returns the name of the published test method that must drive aFamily
function FamilyMethodName(const aFamily: string): string;

begin
  Result := UpperCase(Copy(aFamily, 1, 1)) + Copy(aFamily, 2, MaxInt) +
    'FixtureIsClean';
end;


// Returns whether aDefine is in the suite-wide define set every fixture is analyzed under.
function DefinePresent(const aDefine: string): boolean;

var
  lDefines: TFpSonarStringArray;
  lIndex: integer;

begin
  Result := False;
  lDefines := LanguageSurfaceDefines;
  for lIndex := Low(lDefines) to High(lDefines) do
    if lDefines[lIndex] = aDefine then
      Exit(True);
end;


// Returns the mode named by the first {$mode X} directive of aSource, or an empty string when it declares none
function DeclaredMode(const aSource: string): string;

var
  lStart, lEnd: integer;

begin
  Result := '';
  lStart := Pos('{$mode ', LowerCase(aSource));
  if lStart = 0 then
    Exit;
  Inc(lStart, Length('{$mode '));
  lEnd := lStart;
  while (lEnd <= Length(aSource)) and (aSource[lEnd] <> '}') do
    Inc(lEnd);
  Result := Trim(Copy(aSource, lStart, lEnd - lStart));
end;


// Returns aSource with the text of every comment replaced by cCommentFill
function CodeOnly(const aSource: string): string;

type
  TSpan = (spCode, spBrace, spParen, spDirectiveBrace, spDirectiveParen);

var
  lPos, lLen: integer;
  lInString: boolean;
  lSpan: TSpan;

begin
  Result := aSource;
  lLen := Length(aSource);
  lPos := 1;
  lInString := False;
  lSpan := spCode;
  while lPos <= lLen do
    case lSpan of
      spCode:
        // A string literal cannot cross a line break
        if aSource[lPos] in [#10, #13] then
        begin
          lInString := False;
          Inc(lPos);
        end
        else if lInString then
        begin
          lInString := aSource[lPos] <> '''';
          Inc(lPos);
        end
        else if aSource[lPos] = '''' then
        begin
          lInString := True;
          Inc(lPos);
        end
        else if (aSource[lPos] = '/') and (lPos < lLen)
             and (aSource[lPos + 1] = '/') then
          while (lPos <= lLen) and not (aSource[lPos] in [#10, #13]) do
          begin
            Result[lPos] := cCommentFill;
            Inc(lPos);
          end
        else if aSource[lPos] = '{' then
        begin
          // A directive body is not Pascal...
          if (lPos < lLen) and (aSource[lPos + 1] = '$') then
            lSpan := spDirectiveBrace
          else
          begin
            Result[lPos] := cCommentFill;
            lSpan := spBrace;
          end;
          Inc(lPos);
        end
        else if (aSource[lPos] = '(') and (lPos < lLen)
             and (aSource[lPos + 1] = '*') then
        begin
          if (lPos + 1 < lLen) and (aSource[lPos + 2] = '$') then
            lSpan := spDirectiveParen
          else
          begin
            Result[lPos] := cCommentFill;
            Result[lPos + 1] := cCommentFill;
            lSpan := spParen;
          end;
          Inc(lPos, 2);
        end
        else
          Inc(lPos);
      spDirectiveBrace:
        begin
          if aSource[lPos] = '}' then
            lSpan := spCode;
          Inc(lPos);
        end;
      spDirectiveParen:
        if (aSource[lPos] = '*') and (lPos < lLen)
           and (aSource[lPos + 1] = ')') then
        begin
          lSpan := spCode;
          Inc(lPos, 2);
        end
        else
          Inc(lPos);
      spBrace:
        begin
          if aSource[lPos] = '}' then
            lSpan := spCode;
          if not (aSource[lPos] in [#10, #13]) then
            Result[lPos] := cCommentFill;
          Inc(lPos);
        end;
      spParen:
        if (aSource[lPos] = '*') and (lPos < lLen)
           and (aSource[lPos + 1] = ')') then
        begin
          Result[lPos] := cCommentFill;
          Result[lPos + 1] := cCommentFill;
          lSpan := spCode;
          Inc(lPos, 2);
        end
        else
        begin
          if not (aSource[lPos] in [#10, #13]) then
            Result[lPos] := cCommentFill;
          Inc(lPos);
        end;
    end;
  // An unterminated directive body is preserved rather than blanked
  if lSpan in [spDirectiveBrace, spDirectiveParen] then
    raise Exception.Create('CodeOnly: the source ends inside an unterminated ' +
      'compiler directive');
  if lSpan in [spBrace, spParen] then
    raise Exception.Create('CodeOnly: the source ends inside an unterminated ' +
      'comment');
  // String state ends with its line
  if lInString then
    raise Exception.Create('CodeOnly: the last line of the source ends inside ' +
      'a string literal');
end;


// Returns aText with every run of whitespace collapsed to a single space and no leading or trailing space
function CollapseSpace(const aText: string): string;

var
  lPos, lOut: integer;
  lPending: boolean;

begin
  SetLength(Result, Length(aText));
  lOut := 0;
  lPending := False;
  for lPos := 1 to Length(aText) do
    // Form feed and vertical tab are whitespace to the scanner too
    if aText[lPos] in [' ', #9, #10, #11, #12, #13] then
      lPending := True
    else
    begin
      if lPending and (lOut > 0) then
      begin
        Inc(lOut);
        Result[lOut] := ' ';
      end;
      lPending := False;
      Inc(lOut);
      Result[lOut] := aText[lPos];
    end;
  SetLength(Result, lOut);
end;


// Returns whether aMarker occurs in the code of aSource
function MarkerOccursInCode(const aSource, aMarker: string): boolean;

begin
  Result := Pos(CollapseSpace(aMarker), CollapseSpace(CodeOnly(aSource))) > 0;
end;


function LanguageSurfaceFamilies: TFpSonarStringArray;

var
  lIndex: integer;

begin
  SetLength(Result, Length(cFamilyLabels));
  for lIndex := Low(cFamilyLabels) to High(cFamilyLabels) do
    Result[lIndex] := cFamilyLabels[lIndex];
end;


function LanguageSurfaceDefines: TFpSonarStringArray;

var
  lIndex: integer;

begin
  SetLength(Result, Length(cSurfaceDefines));
  for lIndex := Low(cSurfaceDefines) to High(cSurfaceDefines) do
    Result[lIndex - Low(cSurfaceDefines)] := cSurfaceDefines[lIndex];
end;


function StageLanguageSurfaceCorpus(aFixtures: TTempFixtures): TLanguageSurfaceEntryArray;

var
  lEntries: TLanguageSurfaceEntryArray;

  procedure Stage(const aName, aFamily, aMode: string;
    aResolution: TLanguageSurfaceResolution; const aReason, aDiagnostic: string;
    const aSource, aMarkers: array of string);

  var
    lIndex, lMarker, lScan: integer;

  begin
    // TTempFixtures.Add overwrites silently
    if SameText(aName, cIncludeName) then
      raise Exception.Create('StageLanguageSurfaceCorpus: fixture "' + aName +
        '" collides with the include payload');
    for lScan := 0 to High(lEntries) do
      if SameText(lEntries[lScan].Name, aName) then
        raise Exception.Create('StageLanguageSurfaceCorpus: duplicate fixture ' +
          'name "' + aName + '"');
    lIndex := Length(lEntries);
    SetLength(lEntries, lIndex + 1);
    lEntries[lIndex].Name := aName;
    lEntries[lIndex].Family := aFamily;
    lEntries[lIndex].Mode := aMode;
    lEntries[lIndex].Resolution := aResolution;
    lEntries[lIndex].Reason := aReason;
    lEntries[lIndex].Diagnostic := aDiagnostic;
    SetLength(lEntries[lIndex].Markers, Length(aMarkers));
    for lMarker := Low(aMarkers) to High(aMarkers) do
      lEntries[lIndex].Markers[lMarker] := aMarkers[lMarker];
    lEntries[lIndex].Path := aFixtures.Add(aName, aSource);
  end;

begin
  if aFixtures = nil then
    raise Exception.Create('StageLanguageSurfaceCorpus: aFixtures is nil');
  SetLength(lEntries, 0);

  // The include payload first: the preprocessor fixture needs it on disk, and
  // it is never handed back as an analyzable entry.
  aFixtures.Add(cIncludeName, cPreprocessorInclude);

  Stage('surfacegenerics.pas', 'generics', 'OBJFPC', lsrResolves, '', '',
    cGenericsSource,
    ['generic TBox<T> = class',
     'generic TOwnedPair<T: TObject> = class',
     'TIntegerBox = specialize TBox<Integer>;',
     'generic function MaxOf<T>(const aLeft, aRight: T): T;',
     'specialize MaxOf<Integer>(1, 2)']);

  Stage('surfaceoperators.pas', 'operators', 'OBJFPC', lsrResolves, '', '',
    cOperatorsSource,
    ['operator + (const aLeft: TCounter; aSteps: Integer): TCounter;',
     'class operator +(const aLeft, aRight: TCounter): TCounter;',
     'class operator =(const aLeft, aRight: TCounter): Boolean;',
     'class var FInstances: Integer;',
     'class property Instances: Integer read FInstances write FInstances;',
     'class constructor Init;',
     'cStep = 1;',
     'class var FTotal: Integer;',
     'class constructor Setup;',
     'class destructor Teardown;',
     'class function Sum(aLeft, aRight: Integer): Integer; static;',
     'class property Total: Integer read FTotal write FTotal;']);

  Stage('surfacehelpers.pas', 'helpers', 'OBJFPC', lsrResolves, '', '',
    cHelpersSource,
    ['TObjectHelper = class helper for TObject',
     'TPoint2DHelper = record helper for TPoint2D',
     'TIntegerHelper = type helper for Integer']);

  Stage('surfacerecords.pas', 'records', 'OBJFPC', lsrResolves, '', '',
    cRecordsSource,
    ['case Kind: TShapeKind of',
     'skRectangle: (Width: Double; Height: Double);',
     'TPackedHeader = packed record',
     'TBitFlags = bitpacked record']);

  Stage('surfacestorage.pas', 'storage', 'OBJFPC', lsrResolves, '', '',
    cStorageSource,
    ['resourcestring',
     'rsGreeting = ''Hello'';',
     'threadvar',
     'GPerThreadDepth: Integer;',
     'lBytes: array[0..3] of Byte absolute lValue;',
     'cLimits: array[0..2] of Integer = (1, 2, 3);',
     'GStartValue: Integer = 7;']);

  Stage('surfaceclosuresdelphi.pas', 'closures', 'DELPHI', lsrResolves, '', '',
    cClosuresDelphiSource,
    ['{$modeswitch anonymousfunctions}',
     'TIntegerCallback = reference to procedure(aValue: Integer);',
     'TIntegerMapper = reference to function(aValue: Integer): Integer;',
     'procedure(aMapped: Integer)',
     'procedure(aCollected: Integer)']);

  Stage('surfaceclosuresnested.pas', 'closures', 'OBJFPC', lsrResolves, '', '',
    cClosuresNestedSource,
    ['{$modeswitch nestedprocvars}',
     'TNestedVisitor = procedure(aValue: Integer) is nested;',
     'VisitNaturals(aCount, @Accumulate);']);

  Stage('surfaceattributes.pas', 'attributes', 'DELPHI', lsrDegrades,
    cAttributesReason, cAttributesDiagnostic, cAttributesSource,
    ['DescriptionAttribute = class(TCustomAttribute)',
     '[Description(''a documented class'')]',
     '[Description(''a documented field'')]',
     '[Description(''a documented method'')]',
     '[Description(''a documented property'')]']);

  Stage('surfaceparameters.pas', 'parameters', 'OBJFPC', lsrResolves, '', '',
    cParametersSource,
    ['const aArgs: array of const',
     'const aValues: array of Integer',
     'cdecl; varargs; external ''c'' name ''sprintf'';',
     'cdecl; external ''c'' name ''getpid'';',
     'procedure NotifyHost(aCode: Integer); stdcall;']);

  Stage('surfacepreprocessor.pas', 'preprocessor', 'OBJFPC', lsrResolves, '',
    '', cPreprocessorSource,
    ['{$macro on}',
     '{$define cMaxDepth := 8}',
     '{$include surfacepreprocessor.inc}',
     '{$ifdef LINUX}',
     'Result := ''linux'';']);

  Stage('surfacepreprocessoralt.pas', 'preprocessor', 'OBJFPC', lsrResolves, '',
    '', cPreprocessorAltSource,
    ['{$ifdef SURFACE_UNSET}',
     '{$else}',
     'cFallbackDepth = 2;',
     'Result := ''undefined'';']);

  Stage('surfaceinterfaces.pas', 'interfaces', 'OBJFPC', lsrResolves, '', '',
    cInterfacesSource,
    ['IStorage = interface(IInterface)',
     '[''{2C6F5C0E-1B7A-4C33-9E1D-2B3A4C5D6E7F}'']',
     '{$interfaces corba}',
     'ILogger = interface',
     '{$interfaces com}',
     'IResettable = interface',
     'TMemoryStorage = class(TInterfacedObject, IStorage)',
     'TConsoleLogger = class(TObject, ILogger)',
     'Result := TConsoleLogger.Create;']);

  Stage('surfacedispinterface.pas', 'interfaces', 'OBJFPC', lsrDegrades,
    cDispInterfaceReason, cDispInterfaceDiagnostic, cDispInterfaceSource,
    ['IAutomation = dispinterface',
     'procedure Refresh; dispid 1;',
     'property Caption: WideString dispid 2;']);

  Stage('surfaceexceptions.pas', 'exceptions', 'OBJFPC', lsrResolves, '', '',
    cExceptionsSource,
    ['on E: ERangeRejected do',
     'on EValueUnreadable do',
     'else Result := cUnhandledCode;',
     'finally lScratch.Free; end; except',
     'raise aError at aAddress;']);

  Stage('surfaceflowlabel.pas', 'flow', 'OBJFPC', lsrDegrades, cFlowReason,
    cFlowDiagnostic, cFlowLabelSource,
    ['{$goto on}',
     'label Found;',
     'goto Found;']);

  Stage('surfaceflowwith.pas', 'flow', 'OBJFPC', lsrResolves, '', '',
    cFlowWithSource,
    ['with aOuter, aOuter.Inner do',
     'Result := Amount * Factor;']);

  Stage('surfacestrings.pas', 'strings', 'OBJFPC', lsrResolves, '', '',
    cStringsSource,
    ['lShort: ShortString;',
     'TWindows1252String = type AnsiString(1252);',
     'lRaw: RawByteString;',
     'lUnicode: UnicodeString;',
     'lWide: WideString;',
     'lUtf8: UTF8String;']);

  Stage('surfacemodifiers.pas', 'modifiers', 'OBJFPC', lsrResolves, '', '',
    cModifiersSource,
    ['function Doubled(aValue: Integer): Integer; inline;',
     'deprecated ''use Describe''',
     'procedure ReportValue(aValue: Integer); platform;',
     'function ProvisionalTag: Integer; experimental;',
     'function Combine(aLeft, aRight: Integer): Integer; forward;',
     'function Describe(aValue: Integer): string; overload;',
     'function Tag: Integer; reintroduce;']);

  Stage('surfacemodule.pas', 'modules', 'OBJFPC', lsrResolves, '', '',
    cModuleSource,
    ['initialization GRegistry := TStringList.Create;',
     'finalization GRegistry.Free;']);

  Stage('surfacelibrary.lpr', 'modules', 'OBJFPC', lsrResolves, '', '',
    cLibrarySource,
    ['library SurfaceLibrary;',
     'exports AbiVersion name ''abi_version'',',
     'SlotCount name ''slot_count'';']);

  Stage('surfaceprogram.lpr', 'modules', 'OBJFPC', lsrResolves, '', '',
    cProgramSource,
    ['program SurfaceProgram;',
     'GBanner := Banner;']);

  Result := lEntries;
end;


{ TLanguageSurfaceTest }

procedure TLanguageSurfaceTest.SetUp;

begin
  inherited SetUp;
  FFixtures := TTempFixtures.Create;
  try
    FEntries := StageLanguageSurfaceCorpus(FFixtures);
  except
    // fpcunit skips TearDown when SetUp raises
    FreeAndNil(FFixtures);
    raise;
  end;
end;


procedure TLanguageSurfaceTest.TearDown;

begin
  SetLength(FEntries, 0);
  FreeAndNil(FFixtures);
  inherited TearDown;
end;


function TLanguageSurfaceTest.ReadFixture(
  const aEntry: TLanguageSurfaceEntry): string;

var
  lText: TStringList;

begin
  if not FileExists(aEntry.Path) then
    Fail(aEntry.Name + ': the staged fixture is missing from ' +
      ExtractFilePath(aEntry.Path));
  lText := TStringList.Create;
  try
    lText.LoadFromFile(aEntry.Path);
    Result := lText.Text;
  finally
    lText.Free;
  end;
end;


function TLanguageSurfaceTest.ResolveFailure(
  const aEntry: TLanguageSurfaceEntry): string;

var
  lResolver: TFpSonarResolver;
  lDiag: TFpSonarDiagnostic;

begin
  // TFpSonarSourceFile drops the resolver's diagnostic, so rebuild once
  Result := '(this second, independent resolve of the file succeeded)';
  try
    lResolver := TFpSonarResolver.Create;
    try
      lResolver.DependencyInterfaceOnly := True;
      lResolver.IntrinsicConstEval := True;
      lResolver.CondDirectiveEval := True;
      if not lResolver.BuildFor(aEntry.Path, aEntry.Mode, LanguageSurfaceDefines, [],
        [], lDiag) then
      begin
        if lDiag.Message = '' then
          lDiag.Message := '(the resolver failed without a message)';
        Result := Format('%s(%d,%d) %s: %s', [ExtractFileName(lDiag.FileName),
          lDiag.Row, lDiag.Col, cDiagnosticNames[lDiag.Kind], lDiag.Message]);
      end;
    finally
      lResolver.Free;
    end;
  except
    on E: Exception do
      Result := Format('re-resolving %s raised %s: %s', [aEntry.Name,
        E.ClassName, E.Message]);
  end;
end;


procedure TLanguageSurfaceTest.CheckEntry(const aEntry: TLanguageSurfaceEntry);

var
  lSource: TFpSonarSourceFile;
  lDiag: TFpSonarDiagnostic;
  lIndex: integer;
  lFailure: string;

begin
  lSource := TFpSonarSourceFile.Create;
  try
    lSource.Analyze(aEntry.Path, aEntry.Mode, LanguageSurfaceDefines);

    for lIndex := 0 to High(lSource.Diagnostics) do
    begin
      lDiag := lSource.Diagnostics[lIndex];
      if lDiag.Kind in [dkParseError, dkScanError, dkFileNotFound] then
        Fail(Format('%s: %s(%d,%d): %s diagnostic: %s', [aEntry.Name,
          ExtractFileName(lDiag.FileName), lDiag.Row, lDiag.Col,
          cDiagnosticNames[lDiag.Kind], lDiag.Message]));
    end;

    AssertTrue(aEntry.Name + ': parse succeeded', lSource.ParseSucceeded);
    AssertTrue(aEntry.Name + ': module built', lSource.Module <> nil);
    AssertTrue(aEntry.Name + ': resolver built', lSource.Resolver <> nil);

    if aEntry.Resolution = lsrResolves then
    begin
      AssertEquals(aEntry.Name + ': no diagnostic', 0,
        Length(lSource.Diagnostics));
      if not lSource.Resolver.Succeeded then
        Fail(Format('%s: declared lsrResolves but degraded - %s',
          [aEntry.Name, ResolveFailure(aEntry)]));
    end
    else
    begin
      if lSource.Resolver.Succeeded then
        Fail(Format('%s: declared lsrDegrades (%s) but now resolves cleanly ' +
          '- re-declare it lsrResolves', [aEntry.Name, aEntry.Reason]));
      lFailure := ResolveFailure(aEntry);
      if Pos(aEntry.Diagnostic, lFailure) = 0 then
        Fail(Format('%s: declared lsrDegrades on "%s", but the resolver now ' +
          'reports "%s" - the recorded reason no longer describes the fixture',
          [aEntry.Name, aEntry.Diagnostic, lFailure]));
    end;
  finally
    lSource.Free;
  end;
end;


procedure TLanguageSurfaceTest.CheckFamily(const aFamily: string);

var
  lIndex, lSeen: integer;

begin
  AssertTrue('"' + aFamily + '" is one of the 15 construct-family labels',
    FamilyFileCount(aFamily) > 0);
  AssertEquals('the family a test drives is the family it is named for',
    FamilyMethodName(aFamily), TestName);

  lSeen := 0;
  for lIndex := 0 to High(FEntries) do
    if FEntries[lIndex].Family = aFamily then
    begin
      Inc(lSeen);
      CheckEntry(FEntries[lIndex]);
    end;
  AssertEquals('family "' + aFamily + '" stages every file it declares',
    FamilyFileCount(aFamily), lSeen);
end;


procedure TLanguageSurfaceTest.GenericsFixtureIsClean;

begin
  CheckFamily('generics');
end;


procedure TLanguageSurfaceTest.OperatorsFixtureIsClean;

begin
  CheckFamily('operators');
end;


procedure TLanguageSurfaceTest.HelpersFixtureIsClean;

begin
  CheckFamily('helpers');
end;


procedure TLanguageSurfaceTest.RecordsFixtureIsClean;

begin
  CheckFamily('records');
end;


procedure TLanguageSurfaceTest.StorageFixtureIsClean;

begin
  CheckFamily('storage');
end;


procedure TLanguageSurfaceTest.ClosuresFixtureIsClean;

begin
  CheckFamily('closures');
end;


procedure TLanguageSurfaceTest.AttributesFixtureIsClean;

begin
  CheckFamily('attributes');
end;


procedure TLanguageSurfaceTest.ParametersFixtureIsClean;

begin
  CheckFamily('parameters');
end;


procedure TLanguageSurfaceTest.PreprocessorFixtureIsClean;

begin
  CheckFamily('preprocessor');
end;


procedure TLanguageSurfaceTest.InterfacesFixtureIsClean;

begin
  CheckFamily('interfaces');
end;


procedure TLanguageSurfaceTest.ExceptionsFixtureIsClean;

begin
  CheckFamily('exceptions');
end;


procedure TLanguageSurfaceTest.FlowFixtureIsClean;

begin
  CheckFamily('flow');
end;


procedure TLanguageSurfaceTest.StringsFixtureIsClean;

begin
  CheckFamily('strings');
end;


procedure TLanguageSurfaceTest.ModifiersFixtureIsClean;

begin
  CheckFamily('modifiers');
end;


procedure TLanguageSurfaceTest.ModulesFixtureIsClean;

begin
  CheckFamily('modules');
end;


procedure TLanguageSurfaceTest.CorpusStagesEveryFamilyNonVacuously;

var
  lFamilies: TFpSonarStringArray;
  lNames: TStringList;
  lIndex, lOther, lSeen: integer;
  lKnown: boolean;
  lExt, lMethod, lDeclared: string;

begin
  lFamilies := LanguageSurfaceFamilies;
  AssertEquals('the corpus covers 15 construct families', 15,
    Length(lFamilies));
  AssertEquals('every family label has a declared file count',
    Length(lFamilies), Length(cFamilyFileCounts));

  for lIndex := 0 to High(lFamilies) do
  begin
    for lOther := 0 to lIndex - 1 do
      AssertFalse('family label "' + lFamilies[lIndex] + '" is listed twice',
        lFamilies[lOther] = lFamilies[lIndex]);

    lMethod := FamilyMethodName(lFamilies[lIndex]);
    AssertTrue('family "' + lFamilies[lIndex] + '" has a published ' +
      lMethod, MethodAddress(lMethod) <> nil);
  end;

  // Which {$ifdef} arm of the preprocessor family is analyzed ?
  AssertTrue('LINUX is defined, so surfacepreprocessor.pas covers the taken ' +
    '{$ifdef} arm', DefinePresent('LINUX'));
  AssertFalse('SURFACE_UNSET stays undefined, so surfacepreprocessoralt.pas ' +
    'covers the taken {$else} arm', DefinePresent('SURFACE_UNSET'));

  for lIndex := 0 to High(lFamilies) do
  begin
    // A family declaring no file at all would make its own test pass by checking
    // nothing.
    AssertTrue('family "' + lFamilies[lIndex] + '" declares at least one file',
      FamilyFileCount(lFamilies[lIndex]) > 0);
    lSeen := 0;
    for lOther := 0 to High(FEntries) do
      if FEntries[lOther].Family = lFamilies[lIndex] then
        Inc(lSeen);
    AssertEquals('family "' + lFamilies[lIndex] +
      '" stages every file it declares', FamilyFileCount(lFamilies[lIndex]),
      lSeen);
  end;

  lNames := TStringList.Create;
  try
    lNames.CaseSensitive := False;
    lNames.Add(cIncludeName);
    for lIndex := 0 to High(FEntries) do
    begin
      lKnown := False;
      for lOther := 0 to High(lFamilies) do
        lKnown := lKnown or (FEntries[lIndex].Family = lFamilies[lOther]);
      AssertTrue(FEntries[lIndex].Name + ': family "' +
        FEntries[lIndex].Family + '" is one of the 15 labels', lKnown);

      AssertEquals(FEntries[lIndex].Name + ': staged under its own name', -1,
        lNames.IndexOf(FEntries[lIndex].Name));
      lNames.Add(FEntries[lIndex].Name);

      AssertTrue(FEntries[lIndex].Name + ': staging filled in a path',
        FEntries[lIndex].Path <> '');
      AssertTrue(FEntries[lIndex].Name + ': the staged file exists',
        FileExists(FEntries[lIndex].Path));
      AssertTrue(FEntries[lIndex].Name + ': the staged file holds source and ' +
        'not only line breaks',
        Length(Trim(ReadFixture(FEntries[lIndex]))) > 0);
      AssertTrue(FEntries[lIndex].Name + ': declares at least one marker',
        Length(FEntries[lIndex].Markers) > 0);
      for lOther := 0 to High(FEntries[lIndex].Markers) do
        AssertTrue(FEntries[lIndex].Name + ': marker ' + IntToStr(lOther) +
          ' is not blank - a blank marker matches anywhere',
          Trim(FEntries[lIndex].Markers[lOther]) <> '');

      lExt := LowerCase(ExtractFileExt(FEntries[lIndex].Name));
      AssertTrue(FEntries[lIndex].Name + ': is handed back with an analyzable '
        + 'extension, so no include payload can reach a consumer',
        (lExt = '.pas') or (lExt = '.lpr'));

      // Check Entry.Mode
      lDeclared := DeclaredMode(ReadFixture(FEntries[lIndex]));
      if lDeclared <> '' then
        AssertEquals(FEntries[lIndex].Name + ': the declared Mode is the mode ' +
          'its own source directive selects', LowerCase(FEntries[lIndex].Mode),
          LowerCase(lDeclared));
    end;
  finally
    lNames.Free;
  end;

  AssertTrue('the include payload is staged but not returned',
    FileExists(IncludeTrailingPathDelimiter(FFixtures.Dir) + cIncludeName));
end;


procedure TLanguageSurfaceTest.CorpusMarkersWitnessEveryConstruct;

var
  lIndex, lMarker: integer;
  lSource: string;

begin
  for lIndex := 0 to High(FEntries) do
  begin
    lSource := ReadFixture(FEntries[lIndex]);
    for lMarker := 0 to High(FEntries[lIndex].Markers) do
      AssertTrue(Format('%s: construct marker "%s" no longer occurs in the ' +
        'code of the staged fixture', [FEntries[lIndex].Name,
        FEntries[lIndex].Markers[lMarker]]),
        MarkerOccursInCode(lSource, FEntries[lIndex].Markers[lMarker]));
  end;
end;


procedure TLanguageSurfaceTest.CorpusMarkerMatchingSeesCodeOnly;

const
  // A unit naming "initialization" in all three comment forms, carrying an
  // apostrophe in a directive body, and holding no initialization section.
  cProbe = 'unit Probe;'#10
    + '{ Header: with an initialization section. }'#10
    + '// initialization'#10
    + '(* initialization *)'#10
    + '{$warning don''t rely on section order}'#10
    + '{$mode objfpc}'#10
    + 'implementation'#10
    + 'var'#10
    + '  GProbe: Integer;'#10;

var
  lRaised: boolean;

begin
  AssertFalse('a construct named only in comments does not satisfy its marker',
    MarkerOccursInCode(cProbe, 'initialization GProbe := 1;'));
  AssertFalse('a bare keyword in comments does not satisfy its marker',
    MarkerOccursInCode(cProbe, 'initialization'));
  AssertTrue('a compiler directive is code and is searched as written',
    MarkerOccursInCode(cProbe, '{$mode objfpc}'));
  AssertTrue('an apostrophe inside a directive does not open a string literal, '
    + 'which would leave every later comment unblanked',
    MarkerOccursInCode(cProbe + 'initialization'#10'  GProbe := 1;'#10'end.'#10,
      'initialization GProbe := 1;'));
  AssertTrue('a marker may span the line break the construct is written across',
    MarkerOccursInCode(cProbe, 'var GProbe: Integer;'));
  AssertFalse('a marker absent from the code fails',
    MarkerOccursInCode(cProbe, 'finalization GProbe := 0;'));

  // A brace inside a string literal is not a comment.
  AssertTrue('a brace inside a string literal does not open a comment',
    MarkerOccursInCode('const cGuid = ''{2C6F5C0E}'';'#10,
      'cGuid = ''{2C6F5C0E}'';'));

  // The (*$ *) directive form has its own scanner branch
  AssertTrue('a paren-form directive is code and is searched as written',
    MarkerOccursInCode('(*$warning don''t rely on order*)'#10'var GX: Integer;'#10,
      '(*$warning don''t rely on order*)'));
  AssertTrue('code after a paren-form directive is still code',
    MarkerOccursInCode('(*$warning don''t rely on order*)'#10'var GX: Integer;'#10,
      'var GX: Integer;'));
  AssertFalse('a paren comment is still blanked',
    MarkerOccursInCode('(* var GY: Integer; *)'#10, 'var GY: Integer;'));

  // Comment text is not blanked to spaces
  AssertFalse('a marker may not span a comment sitting between two fragments',
    MarkerOccursInCode('begin'#10'  A := 1; { note } B := 2;'#10'end;'#10,
      'A := 1; B := 2;'));
  AssertTrue('the same two fragments with nothing between them satisfy it',
    MarkerOccursInCode('begin'#10'  A := 1; B := 2;'#10'end;'#10,
      'A := 1; B := 2;'));

  // An unterminated span leaves the whole remainder of the file unblanked
  lRaised := False;
  try
    MarkerOccursInCode('unit P;'#10'{$mode objfpc'#10, '{$mode objfpc');
  except
    on E: Exception do
      lRaised := True;
  end;
  AssertTrue('an unterminated directive body is reported, not scanned as code',
    lRaised);
end;


procedure TLanguageSurfaceTest.CorpusDegradationsAreJustified;

var
  lIndex, lOther, lSeen: integer;

begin
  for lOther := Low(cDegradingFixtures) to High(cDegradingFixtures) do
  begin
    lSeen := 0;
    for lIndex := 0 to High(FEntries) do
      if SameText(FEntries[lIndex].Name, cDegradingFixtures[lOther])
         and (FEntries[lIndex].Resolution = lsrDegrades) then
        Inc(lSeen);
    AssertEquals(cDegradingFixtures[lOther] + ': is staged and still declared ' +
      'lsrDegrades', 1, lSeen);
  end;

  for lIndex := 0 to High(FEntries) do
    if FEntries[lIndex].Resolution = lsrDegrades then
    begin
      lSeen := 0;
      for lOther := Low(cDegradingFixtures) to High(cDegradingFixtures) do
        if SameText(cDegradingFixtures[lOther], FEntries[lIndex].Name) then
          Inc(lSeen);
      AssertEquals(FEntries[lIndex].Name + ': degrades, so it must be one of ' +
        'the sanctioned degradations - a fixture may not be re-declared ' +
        'lsrDegrades to make a resolve regression green', 1, lSeen);
      AssertTrue(FEntries[lIndex].Name + ': declared lsrDegrades without a ' +
        'reason', Trim(FEntries[lIndex].Reason) <> '');
      AssertTrue(FEntries[lIndex].Name + ': declared lsrDegrades without the ' +
        'resolver diagnostic that reason claims',
        Trim(FEntries[lIndex].Diagnostic) <> '');
      // Only Diagnostic is checked against the resolver
      AssertTrue(FEntries[lIndex].Name + ': the recorded reason does not ' +
        'contain the diagnostic text the test enforces, so the two may drift',
        Pos(FEntries[lIndex].Diagnostic, FEntries[lIndex].Reason) > 0);
    end
    else
    begin
      AssertEquals(FEntries[lIndex].Name + ': declared lsrResolves, so it ' +
        'carries no degradation reason', '', FEntries[lIndex].Reason);
      AssertEquals(FEntries[lIndex].Name + ': declared lsrResolves, so it ' +
        'carries no degradation diagnostic', '', FEntries[lIndex].Diagnostic);
    end;
end;


initialization
  RegisterTest(TLanguageSurfaceTest);
end.
