{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for use expansion, reference cycles and cloning.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit tcsvguse;

{$mode objfpc}{$H+}
{$IFDEF UNICODERTL}
{$modeswitch unicodestrings}
{$ENDIF}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils, System.Classes, FpcUnit.Test, FpcUnit.Registry,
     svggoldens, fpsvg.types, fpsvg.dom, fpsvg.read, fpsvg.style;
{$ELSE FPC_DOTTEDUNITS}
uses sysutils, classes, fpcunit, testregistry, svggoldens, fpsvg.types,
     fpsvg.dom, fpsvg.read, fpsvg.style;
{$ENDIF FPC_DOTTEDUNITS}

type
  TTestSVGClone = class(TTestCase)
  private
    FDocument: TSVGDocument;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestCloneCopiesAttributes;
    procedure TestCloneCopiesChildren;
    procedure TestCloneKeepsTheElementClass;
    procedure TestCloneIsDetached;
    procedure TestCloneIsIndependent;
    procedure TestCloneCopiesText;
  end;

  TTestSVGUseExpansion = class(TTestCase)
  private
    FDocument: TSVGDocument;
    FExpander: TSVGUseExpander;
    // Expands the use with the given id and checks the expected outcome.
    function ExpandID(const aID: String;
      aExpected: TSVGUseResult): TSVGElement;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestSimpleUseBecomesAGroup;
    procedure TestOffsetBecomesATransform;
    procedure TestNoOffsetLeavesNoTransform;
    procedure TestOffsetAppliesInsideTheOwnTransform;
    procedure TestPropertiesAreCopiedToTheGroup;
    procedure TestGeometryAttributesAreNotCopied;
    procedure TestNestedUsesAreExpanded;
    procedure TestChainedUseIsExpanded;
    procedure TestMutualCycleIsRefused;
    procedure TestSelfReferenceIsRefused;
    procedure TestDanglingReferenceIsRefused;
    procedure TestNonUseIsRefused;
    procedure TestDepthLimitIsEnforced;
    procedure TestRenderedElementPredicate;
    procedure TestExpansionGolden;
  end;

implementation

{ TTestSVGClone }

procedure TTestSVGClone.SetUp;

begin
  inherited SetUp;
  FDocument := ReadSVGFile(DataDir + 'uses.svg');
end;


procedure TTestSVGClone.TearDown;

begin
  FreeAndNil(FDocument);
  inherited TearDown;
end;


procedure TTestSVGClone.TestCloneCopiesAttributes;

var
  lCopy: TSVGElement;

begin
  lCopy := TSVGElement(FDocument.ElementByID('box').Clone);
  try
    AssertEquals('the clone keeps the width', '10', lCopy.Attributes['width']);
    AssertEquals('the clone keeps the id', 'box', lCopy.ID);
  finally
    lCopy.Free;
  end;
end;


procedure TTestSVGClone.TestCloneCopiesChildren;

var
  lCopy: TSVGElement;

begin
  lCopy := TSVGElement(FDocument.ElementByID('pair').Clone);
  try
    AssertEquals('the clone keeps both children', 2, lCopy.ChildCount);
    AssertTrue('the cloned child is a use', lCopy[0] is TSVGUseElement);
  finally
    lCopy.Free;
  end;
end;


procedure TTestSVGClone.TestCloneKeepsTheElementClass;

var
  lCopy: TSVGNode;

begin
  lCopy := FDocument.ElementByID('box').Clone;
  try
    AssertTrue('a cloned rect is still a rect element',
      lCopy is TSVGRectElement);
  finally
    lCopy.Free;
  end;
end;


procedure TTestSVGClone.TestCloneIsDetached;

var
  lCopy: TSVGNode;

begin
  lCopy := FDocument.ElementByID('box').Clone;
  try
    AssertNull('the clone has no parent', lCopy.Parent);
    AssertNull('the clone belongs to no document', lCopy.Document);
  finally
    lCopy.Free;
  end;
end;


procedure TTestSVGClone.TestCloneIsIndependent;

var
  lCopy: TSVGElement;

begin
  lCopy := TSVGElement(FDocument.ElementByID('box').Clone);
  try
    lCopy.Attributes['width'] := '99';
    AssertEquals('changing the clone leaves the original alone', '10',
      FDocument.ElementByID('box').Attributes['width']);
  finally
    lCopy.Free;
  end;
end;


procedure TTestSVGClone.TestCloneCopiesText;

var
  lSource, lCopy: TSVGElement;

begin
  lSource := CreateSVGElement('text');
  try
    lSource.AppendChild(TSVGTextNode.Create('hello'));
    lCopy := TSVGElement(lSource.Clone);
    try
      AssertEquals('the clone keeps the character data', 'hello',
        lCopy.TextContent);
    finally
      lCopy.Free;
    end;
  finally
    lSource.Free;
  end;
end;


{ TTestSVGUseExpansion }

procedure TTestSVGUseExpansion.SetUp;

begin
  inherited SetUp;
  FDocument := ReadSVGFile(DataDir + 'uses.svg');
  FExpander := TSVGUseExpander.Create(FDocument);
end;


procedure TTestSVGUseExpansion.TearDown;

begin
  FreeAndNil(FExpander);
  FreeAndNil(FDocument);
  inherited TearDown;
end;


function TTestSVGUseExpansion.ExpandID(const aID: String;
  aExpected: TSVGUseResult): TSVGElement;

var
  lUse: TSVGElement;
  lResult: TSVGUseResult;

begin
  lUse := FDocument.ElementByID(aID);
  AssertNotNull('the document holds an element with id ' + aID, lUse);
  lResult := FExpander.Expand(lUse, Result);
  AssertTrue(Format('expanding %s gives the expected outcome (%d, got %d)',
    [aID, Ord(aExpected), Ord(lResult)]), lResult = aExpected);
end;


procedure TTestSVGUseExpansion.TestSimpleUseBecomesAGroup;

var
  lGroup: TSVGElement;

begin
  lGroup := ExpandID('simple', urOK);
  try
    AssertTrue('the expansion is a group', lGroup is TSVGGroupElement);
    AssertEquals('the group holds the referenced element', 1,
      lGroup.ChildCount);
    AssertTrue('the referenced rect is copied in',
      lGroup[0] is TSVGRectElement);
  finally
    lGroup.Free;
  end;
end;


procedure TTestSVGUseExpansion.TestOffsetBecomesATransform;

var
  lGroup: TSVGElement;

begin
  lGroup := ExpandID('simple', urOK);
  try
    AssertEquals('x and y become a translation', 'translate(3,4)',
      lGroup.Attributes['transform']);
  finally
    lGroup.Free;
  end;
end;


procedure TTestSVGUseExpansion.TestOffsetAppliesInsideTheOwnTransform;

var
  lGroup: TSVGElement;

begin
  lGroup := ExpandID('turned', urOK);
  try
    AssertEquals('the offset follows the transform of the use',
      'scale(2) translate(3,4)', lGroup.Attributes['transform']);
  finally
    lGroup.Free;
  end;
end;


procedure TTestSVGUseExpansion.TestNoOffsetLeavesNoTransform;

var
  lGroup: TSVGElement;

begin
  lGroup := ExpandID('plainuse', urOK);
  try
    AssertFalse('a use without an offset adds no transform',
      lGroup.HasAttribute('transform'));
  finally
    lGroup.Free;
  end;
end;


procedure TTestSVGUseExpansion.TestPropertiesAreCopiedToTheGroup;

var
  lGroup: TSVGElement;

begin
  lGroup := ExpandID('simple', urOK);
  try
    AssertEquals('the fill of the use reaches the group', 'red',
      lGroup.Attributes['fill']);
  finally
    lGroup.Free;
  end;
end;


procedure TTestSVGUseExpansion.TestGeometryAttributesAreNotCopied;

var
  lGroup: TSVGElement;

begin
  lGroup := ExpandID('simple', urOK);
  try
    AssertFalse('x is consumed by the translation', lGroup.HasAttribute('x'));
    AssertFalse('the reference is not copied on',
      lGroup.HasAttribute('href'));
    AssertFalse('the id of the use is not duplicated',
      lGroup.HasAttribute('id'));
  finally
    lGroup.Free;
  end;
end;


procedure TTestSVGUseExpansion.TestNestedUsesAreExpanded;

var
  lGroup, lPair: TSVGElement;

begin
  lGroup := ExpandID('deep', urOK);
  try
    lPair := TSVGElement(lGroup[0]);
    AssertTrue('the chained use expands to a group', lPair is TSVGGroupElement);
    AssertTrue('no use element survives the expansion',
      lPair.FindChildElement('use') = nil);
  finally
    lGroup.Free;
  end;
end;


procedure TTestSVGUseExpansion.TestChainedUseIsExpanded;

var
  lGroup: TSVGElement;
  lLines: TStringList;

begin
  lGroup := ExpandID('deep', urOK);
  lLines := TStringList.Create;
  try
    lGroup.DumpTo(lLines, 0);
    AssertTrue('the chain reaches the referenced rect',
      Pos('<rect', lLines.Text) > 0);
    AssertEquals('no use element is left anywhere', 0,
      Pos('<use', lLines.Text));
  finally
    lLines.Free;
    lGroup.Free;
  end;
end;


procedure TTestSVGUseExpansion.TestMutualCycleIsRefused;

begin
  AssertNull('a mutual reference cycle yields nothing',
    ExpandID('cyclic', urCycle));
end;


procedure TTestSVGUseExpansion.TestSelfReferenceIsRefused;

begin
  AssertNull('a self reference yields nothing',
    ExpandID('direct', urCycle));
end;


procedure TTestSVGUseExpansion.TestDanglingReferenceIsRefused;

begin
  AssertNull('a reference to a missing id yields nothing',
    ExpandID('dangling', urNoReference));
end;


procedure TTestSVGUseExpansion.TestNonUseIsRefused;

var
  lResult: TSVGElement;

begin
  AssertTrue('a rect is not a use element',
    FExpander.Expand(FDocument.ElementByID('notause'), lResult) = urNotAUse);
  AssertNull('nothing is produced for a non-use element', lResult);
end;


procedure TTestSVGUseExpansion.TestDepthLimitIsEnforced;

var
  lGroup: TSVGElement;

begin
  FExpander.MaxDepth := 0;
  lGroup := ExpandID('simple', urOK);
  lGroup.Free;
  AssertNull('a depth limit of zero refuses a nested use',
    ExpandID('deep', urTooDeep));
end;


procedure TTestSVGUseExpansion.TestRenderedElementPredicate;

begin
  AssertTrue('a rect is rendered in place',
    SVGIsRenderedElement(FDocument.ElementByID('notause')));
  AssertFalse('a defs element is not rendered',
    SVGIsRenderedElement(FDocument.Root.FindChildElement('defs')));
  AssertFalse('nothing is not rendered', SVGIsRenderedElement(nil));
end;


procedure TTestSVGUseExpansion.TestExpansionGolden;

const
  Ids: array[0..2] of String = ('simple', 'plainuse', 'deep');

var
  lLines: TStringList;
  lGroup: TSVGElement;
  I: Integer;

begin
  lLines := TStringList.Create;
  try
    for I := Low(Ids) to High(Ids) do
      begin
      lLines.Add('# ' + Ids[I]);
      lGroup := ExpandID(Ids[I], urOK);
      try
        lGroup.DumpTo(lLines, 1);
      finally
        lGroup.Free;
      end;
      end;
    AssertGolden(Self, 'use-expansion', lLines);
  finally
    lLines.Free;
  end;
end;


initialization
  RegisterTest('use', TTestSVGClone);
  RegisterTest('use', TTestSVGUseExpansion);
end.
