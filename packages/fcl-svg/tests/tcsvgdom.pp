{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for the fpsvg.dom tree and the XML to tree conversion.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit tcsvgdom;

{$mode objfpc}{$H+}
{$IFDEF UNICODERTL}
{$modeswitch unicodestrings}
{$ENDIF}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils, System.Classes, FpcUnit.Test, FpcUnit.Registry,
     svggoldens, fpsvg.types, fpsvg.dom, fpsvg.read;
{$ELSE FPC_DOTTEDUNITS}
uses sysutils, classes, fpcunit, testregistry, svggoldens, fpsvg.types,
     fpsvg.dom, fpsvg.read;
{$ENDIF FPC_DOTTEDUNITS}

type
  TTestSVGElementRegistry = class(TTestCase)
  published
    procedure TestKnownTagsMapToClasses;
    procedure TestCaseSensitiveTagNames;
    procedure TestUnknownTagFallsBack;
    procedure TestContainerFlag;
    procedure TestFilterTagsMapToClasses;
    procedure TestAFilterHoldsItsPrimitives;
    procedure TestAPrimitiveHoldsNothingButAMerge;
  end;

  TTestSVGTree = class(TTestCase)
  private
    FDocument: TSVGDocument;
    procedure AppendToForeignParent;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAppendSetsParentAndDocument;
    procedure TestNodeIndexAndDepth;
    procedure TestElementSiblingsSkipText;
    procedure TestIDIsIndexed;
    procedure TestIDSetAfterAppendIsIndexed;
    procedure TestRemoveChildDropsID;
    procedure TestReparentingIsRejected;
    procedure TestClassAttributeSplits;
    procedure TestTextContentConcatenatesDescendants;
    procedure TestAbsentAttributeHasDefault;
  end;

  TTestSVGReference = class(TTestCase)
  published
    procedure TestBareID;
    procedure TestHashID;
    procedure TestUrlForm;
    procedure TestQuotedUrlForm;
    procedure TestResolveFindsElement;
    procedure TestResolveMissingIsNil;
  end;

  { The base that a reference on an element resolves against. An xml:base
    moves it away from the location the document was read from. }
  TTestSVGElementBase = class(TTestCase)
  private
    FDocument: TSVGDocument;
    // The base of the element with the given id, in a document read from
    // "here/doc.svg" and holding the given body.
    function BaseOf(const aBody, aID: String): String;
  protected
    procedure TearDown; override;
  published
    procedure TestWithoutOneTheDocumentBaseStands;
    procedure TestOneOnTheElementMovesIt;
    procedure TestOneOnAnAncestorMovesIt;
    procedure TestTheInnerOneResolvesAgainstTheOuter;
    procedure TestAnAbsoluteOneReplacesWhatIsAboveIt;
  end;

  TTestSVGDocumentReading = class(TTestCase)
  private
    FDocument: TSVGDocument;
    procedure ReadRootless;
  protected
    procedure TearDown; override;
  published
    procedure TestRootIsSVGElement;
    procedure TestAttributesAreCopied;
    procedure TestXLinkHrefLosesItsPrefix;
    procedure TestNamespaceDeclarationsAreDropped;
    procedure TestWhitespaceOnlyTextIsDropped;
    procedure TestEntityReferenceBecomesItsMarkup;
    procedure TestEntityReferenceInAnAttributeIsExpanded;
    procedure TestExternalEntityIsRefused;
    procedure TestExternalDoctypeSubsetIsNotRefused;
    procedure TestExternalEntityCanBeAllowed;
    procedure TestTextIsKept;
    procedure TestIDsAreIndexedAfterReading;
    procedure TestForeignElementsAreDroppedByDefault;
    procedure TestForeignElementsKeptOnRequest;
    procedure TestUnknownSVGTagIsKept;
    procedure TestMissingRootIsRejected;
    procedure TestBasicTreeGolden;
    procedure TestForeignTreeGolden;
  end;

  TTestSVGAttributeAccess = class(TTestCase)
  private
    FDocument: TSVGDocument;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestLengthAttribute;
    procedure TestAbsentLengthFallsBack;
    procedure TestMalformedLengthFallsBack;
    procedure TestViewBoxAttribute;
    procedure TestAbsentViewBox;
    procedure TestPreserveAspectRatioAttribute;
    procedure TestTransformAttribute;
    procedure TestAbsentTransformIsIdentity;
    procedure TestHRefFromXLink;
    procedure TestAnAttributeKeepsItsBytes;
    procedure TestTextKeepsItsBytes;
    procedure TestADocumentKeepsTheBytesOfItsText;
  end;

implementation

{ TTestSVGElementRegistry }

procedure TTestSVGElementRegistry.TestKnownTagsMapToClasses;

begin
  AssertTrue('svg maps to the svg element class',
    SVGElementClass('svg') = TSVGSVGElement);
  AssertTrue('g maps to the group element class',
    SVGElementClass('g') = TSVGGroupElement);
  AssertTrue('linearGradient maps to the gradient element class',
    SVGElementClass('linearGradient') = TSVGLinearGradientElement);
end;


procedure TTestSVGElementRegistry.TestFilterTagsMapToClasses;

begin
  AssertTrue('filter maps to the filter element class',
    SVGElementClass('filter') = TSVGFilterElement);
  AssertTrue('feGaussianBlur maps to its own class',
    SVGElementClass('feGaussianBlur') = TSVGFeGaussianBlurElement);
  AssertTrue('feMergeNode maps to its own class',
    SVGElementClass('feMergeNode') = TSVGFeMergeNodeElement);
  AssertTrue('feFuncR maps to its own class',
    SVGElementClass('feFuncR') = TSVGFeFuncRElement);
  AssertTrue('feSpotLight maps to its own class',
    SVGElementClass('feSpotLight') = TSVGFeSpotLightElement);
end;


procedure TTestSVGElementRegistry.TestAFilterHoldsItsPrimitives;

begin
  AssertTrue('a filter holds the primitives of its chain',
    TSVGFilterElement.IsContainer);
end;


procedure TTestSVGElementRegistry.TestAPrimitiveHoldsNothingButAMerge;

begin
  // A primitive is a leaf, bar the four that name their inputs or their
  // parts in children of their own.
  AssertTrue('feMerge holds its nodes', TSVGFeMergeElement.IsContainer);
  AssertTrue('feComponentTransfer holds its functions',
    TSVGFeComponentTransferElement.IsContainer);
  AssertTrue('feDiffuseLighting holds its light',
    TSVGFeDiffuseLightingElement.IsContainer);
  AssertFalse('a blur holds nothing',
    TSVGFeGaussianBlurElement.IsContainer);
  AssertFalse('nor does a flood', TSVGFeFloodElement.IsContainer);
end;


procedure TTestSVGElementRegistry.TestCaseSensitiveTagNames;

begin
  AssertTrue('SVG tag names are case sensitive',
    SVGElementClass('lineargradient') = TSVGUnknownElement);
  AssertTrue('clipPath keeps its capital',
    SVGElementClass('clipPath') = TSVGClipPathElement);
end;


procedure TTestSVGElementRegistry.TestUnknownTagFallsBack;

var
  lElement: TSVGElement;

begin
  lElement := CreateSVGElement('bogus');
  try
    AssertTrue('an unregistered tag builds an unknown element',
      lElement is TSVGUnknownElement);
    AssertEquals('the unknown element keeps the source tag', 'bogus',
      lElement.TagName);
  finally
    lElement.Free;
  end;
end;


procedure TTestSVGElementRegistry.TestContainerFlag;

begin
  AssertTrue('a group holds rendered children', TSVGGroupElement.IsContainer);
  AssertFalse('a rect holds no rendered children', TSVGRectElement.IsContainer);
end;


{ TTestSVGTree }

procedure TTestSVGTree.SetUp;

begin
  inherited SetUp;
  FDocument := TSVGDocument.Create;
  FDocument.Root := CreateSVGElement('svg');
end;


procedure TTestSVGTree.TearDown;

begin
  FreeAndNil(FDocument);
  inherited TearDown;
end;


procedure TTestSVGTree.AppendToForeignParent;

var
  lChild: TSVGElement;

begin
  lChild := TSVGElement(FDocument.Root[0]);
  FDocument.Root.AppendChild(lChild);
end;


procedure TTestSVGTree.TestAppendSetsParentAndDocument;

var
  lChild: TSVGElement;

begin
  lChild := CreateSVGElement('rect');
  FDocument.Root.AppendChild(lChild);
  AssertSame('the child points at its parent', FDocument.Root, lChild.Parent);
  AssertSame('the child joins the document', FDocument, lChild.Document);
  AssertEquals('the parent counts the child', 1, FDocument.Root.ChildCount);
end;


procedure TTestSVGTree.TestNodeIndexAndDepth;

var
  lGroup, lRect: TSVGElement;

begin
  FDocument.Root.AppendChild(CreateSVGElement('rect'));
  lGroup := CreateSVGElement('g');
  FDocument.Root.AppendChild(lGroup);
  lRect := CreateSVGElement('rect');
  lGroup.AppendChild(lRect);
  AssertEquals('the second child reports index one', 1, lGroup.NodeIndex);
  AssertEquals('the root has no index', -1, FDocument.Root.NodeIndex);
  AssertEquals('the root sits at depth zero', 0, FDocument.Root.Depth);
  AssertEquals('a grandchild sits at depth two', 2, lRect.Depth);
end;


procedure TTestSVGTree.TestElementSiblingsSkipText;

var
  lFirst, lSecond: TSVGElement;

begin
  lFirst := CreateSVGElement('rect');
  lSecond := CreateSVGElement('circle');
  FDocument.Root.AppendChild(lFirst);
  FDocument.Root.AppendChild(TSVGTextNode.Create('between'));
  FDocument.Root.AppendChild(lSecond);
  AssertSame('the next element sibling skips the text node', lSecond,
    lFirst.NextElementSibling);
  AssertSame('the previous element sibling skips the text node', lFirst,
    lSecond.PreviousElementSibling);
  AssertNull('the last element has no next sibling',
    lSecond.NextElementSibling);
end;


procedure TTestSVGTree.TestIDIsIndexed;

var
  lRect: TSVGElement;

begin
  lRect := CreateSVGElement('rect');
  lRect.Attributes['id'] := 'box';
  FDocument.Root.AppendChild(lRect);
  AssertSame('an element with an id is found by it', lRect,
    FDocument.ElementByID('box'));
end;


procedure TTestSVGTree.TestIDSetAfterAppendIsIndexed;

var
  lRect: TSVGElement;

begin
  lRect := CreateSVGElement('rect');
  FDocument.Root.AppendChild(lRect);
  lRect.Attributes['id'] := 'later';
  AssertSame('an id set after appending is indexed', lRect,
    FDocument.ElementByID('later'));
end;


procedure TTestSVGTree.TestRemoveChildDropsID;

var
  lRect: TSVGElement;

begin
  lRect := CreateSVGElement('rect');
  lRect.Attributes['id'] := 'box';
  FDocument.Root.AppendChild(lRect);
  FDocument.Root.RemoveChild(lRect);
  AssertNull('a removed element is no longer found by its id',
    FDocument.ElementByID('box'));
  AssertEquals('the parent no longer counts the child', 0,
    FDocument.Root.ChildCount);
end;


procedure TTestSVGTree.TestReparentingIsRejected;

begin
  FDocument.Root.AppendChild(CreateSVGElement('rect'));
  AssertException('appending a node that already has a parent is rejected',
    ESVGDOM, @AppendToForeignParent);
end;


procedure TTestSVGTree.TestClassAttributeSplits;

var
  lRect: TSVGElement;

begin
  lRect := CreateSVGElement('rect');
  FDocument.Root.AppendChild(lRect);
  lRect.Attributes['class'] := 'alpha beta';
  AssertEquals('both class names are recorded', 2, lRect.ClassCount);
  AssertTrue('the first class is found', lRect.HasClass('alpha'));
  AssertTrue('the second class is found', lRect.HasClass('beta'));
  AssertFalse('an absent class is not found', lRect.HasClass('gamma'));
end;


procedure TTestSVGTree.TestTextContentConcatenatesDescendants;

var
  lText, lSpan: TSVGElement;

begin
  lText := CreateSVGElement('text');
  FDocument.Root.AppendChild(lText);
  lText.AppendChild(TSVGTextNode.Create('Hello '));
  lSpan := CreateSVGElement('tspan');
  lText.AppendChild(lSpan);
  lSpan.AppendChild(TSVGTextNode.Create('world'));
  AssertEquals('text content spans nested elements', 'Hello world',
    lText.TextContent);
end;


procedure TTestSVGTree.TestAbsentAttributeHasDefault;

var
  lRect: TSVGElement;

begin
  lRect := CreateSVGElement('rect');
  AssertFalse('an unset attribute is absent', lRect.HasAttribute('x'));
  AssertEquals('an absent attribute falls back to the default', '0',
    lRect.AttributeDef('x', '0'));
  lRect.Attributes['x'] := '';
  AssertTrue('an empty attribute is still present', lRect.HasAttribute('x'));
  AssertEquals('an empty attribute overrides the default', '',
    lRect.AttributeDef('x', '0'));
  lRect.Free;
end;


{ TTestSVGReference }

procedure TTestSVGReference.TestBareID;

begin
  AssertEquals('a bare id is returned unchanged', 'box',
    SVGReferenceToID('box'));
end;


procedure TTestSVGReference.TestHashID;

begin
  AssertEquals('a leading hash is stripped', 'box', SVGReferenceToID('#box'));
end;


procedure TTestSVGReference.TestUrlForm;

begin
  AssertEquals('the url wrapper is stripped', 'grad',
    SVGReferenceToID('url(#grad)'));
  AssertEquals('whitespace inside url is ignored', 'grad',
    SVGReferenceToID('  url( #grad )  '));
end;


procedure TTestSVGReference.TestQuotedUrlForm;

begin
  AssertEquals('single quotes are stripped', 'grad',
    SVGReferenceToID('url(''#grad'')'));
  AssertEquals('double quotes are stripped', 'grad',
    SVGReferenceToID('url("#grad")'));
end;


procedure TTestSVGReference.TestResolveFindsElement;

var
  lDocument: TSVGDocument;
  lRect: TSVGElement;

begin
  lDocument := TSVGDocument.Create;
  try
    lDocument.Root := CreateSVGElement('svg');
    lRect := CreateSVGElement('rect');
    lRect.Attributes['id'] := 'box';
    lDocument.Root.AppendChild(lRect);
    AssertSame('a hash reference resolves', lRect,
      lDocument.ResolveReference('#box'));
    AssertSame('a url reference resolves', lRect,
      lDocument.ResolveReference('url(#box)'));
  finally
    lDocument.Free;
  end;
end;


procedure TTestSVGReference.TestResolveMissingIsNil;

var
  lDocument: TSVGDocument;

begin
  lDocument := TSVGDocument.Create;
  try
    lDocument.Root := CreateSVGElement('svg');
    AssertNull('an unknown id resolves to nothing',
      lDocument.ResolveReference('#missing'));
    AssertNull('an empty reference resolves to nothing',
      lDocument.ResolveReference(''));
  finally
    lDocument.Free;
  end;
end;


{ TTestSVGDocumentReading }

procedure TTestSVGDocumentReading.TestExternalEntityIsRefused;

begin
  try
    FDocument := ReadSVGString(
      '<?xml version="1.0"?>' + LineEnding
      + '<!DOCTYPE svg [<!ENTITY leak SYSTEM "file:///etc/hostname">]>'
      + LineEnding
      + '<svg xmlns="http://www.w3.org/2000/svg" width="10" height="10">'
      + '<text>&leak;</text></svg>');
    Fail('an entity that reads another file should be refused');
  except
    on E: ESVGRead do
      AssertTrue('the message reports what was refused',
        Pos('reads another file', E.Message) > 0);
  end;
end;


procedure TTestSVGDocumentReading.TestExternalDoctypeSubsetIsNotRefused;

begin
  FDocument := ReadSVGString(
    '<?xml version="1.0"?>' + LineEnding
    + '<!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" '
    + '"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd">' + LineEnding
    + '<svg xmlns="http://www.w3.org/2000/svg" width="10" height="10">'
    + '<rect id="r" width="4" height="4"/></svg>');
  AssertNotNull('a doctype with an external grammar is read as usual',
    FDocument.ElementByID('r'));
end;


procedure TTestSVGDocumentReading.TestExternalEntityCanBeAllowed;

var
  lReader: TSVGReader;
  lName: String;
  lFile: TStringList;

begin
  lName := GetTempFileName;
  lFile := TStringList.Create;
  try
    lFile.Add('allowed');
    lFile.SaveToFile(lName);
    lReader := TSVGReader.Create;
    try
      lReader.Options := [roExternalEntities];
      FDocument := lReader.ReadFromString(
        '<?xml version="1.0"?>' + LineEnding
        + '<!DOCTYPE svg [<!ENTITY body SYSTEM "file://' + lName + '">]>'
        + LineEnding
        + '<svg xmlns="http://www.w3.org/2000/svg" width="10" height="10">'
        + '<text id="t">&body;</text></svg>');
      AssertTrue('the option lets the entity through',
        Pos('allowed', FDocument.ElementByID('t').TextContent) > 0);
    finally
      lReader.Free;
    end;
  finally
    lFile.Free;
    DeleteFile(lName);
  end;
end;


procedure TTestSVGDocumentReading.TestEntityReferenceBecomesItsMarkup;

var
  lGroup: TSVGElement;

begin
  FDocument := ReadSVGString(
    '<?xml version="1.0"?>' + LineEnding
    + '<!DOCTYPE svg [<!ENTITY box "<rect id=''r'' width=''4'' '
    + 'height=''4''/>">]>' + LineEnding
    + '<svg xmlns="http://www.w3.org/2000/svg" width="10" height="10">'
    + '<g id="holder">&box;</g></svg>');
  lGroup := FDocument.ElementByID('holder');
  AssertNotNull('the group was read', lGroup);
  AssertEquals('the entity put its element in the group', 1,
    lGroup.ChildCount);
  AssertTrue('and it is the rectangle of the entity',
    lGroup[0] is TSVGRectElement);
end;


procedure TTestSVGDocumentReading.TestEntityReferenceInAnAttributeIsExpanded;

var
  lRect: TSVGElement;

begin
  FDocument := ReadSVGString(
    '<?xml version="1.0"?>' + LineEnding
    + '<!DOCTYPE svg [<!ENTITY wide "40">]>' + LineEnding
    + '<svg xmlns="http://www.w3.org/2000/svg" width="100" height="10">'
    + '<rect id="r" width="&wide;" height="4"/></svg>');
  lRect := FDocument.ElementByID('r');
  AssertNotNull('the rectangle was read', lRect);
  AssertEquals('the entity was expanded in the attribute', '40',
    lRect.Attributes['width']);
end;


procedure TTestSVGDocumentReading.TearDown;

begin
  FreeAndNil(FDocument);
  inherited TearDown;
end;


procedure TTestSVGDocumentReading.ReadRootless;

begin
  FDocument := ReadSVGString('<html xmlns="http://www.w3.org/1999/xhtml"/>');
end;


procedure TTestSVGDocumentReading.TestRootIsSVGElement;

begin
  FDocument := ReadSVGString('<svg xmlns="http://www.w3.org/2000/svg"/>');
  AssertTrue('the root builds an svg element class',
    FDocument.Root is TSVGSVGElement);
  AssertEquals('the root keeps its tag name', 'svg', FDocument.Root.TagName);
end;


procedure TTestSVGDocumentReading.TestAttributesAreCopied;

begin
  FDocument := ReadSVGString('<svg xmlns="http://www.w3.org/2000/svg" '
    + 'width="100" viewBox="0 0 10 10"/>');
  AssertEquals('the width attribute is copied', '100',
    FDocument.Root.Attributes['width']);
  AssertEquals('the viewBox attribute is copied', '0 0 10 10',
    FDocument.Root.Attributes['viewBox']);
end;


procedure TTestSVGDocumentReading.TestXLinkHrefLosesItsPrefix;

begin
  FDocument := ReadSVGString('<svg xmlns="http://www.w3.org/2000/svg" '
    + 'xmlns:xlink="http://www.w3.org/1999/xlink">'
    + '<use xlink:href="#a"/></svg>');
  AssertEquals('an xlink href is stored under its local name', '#a',
    TSVGElement(FDocument.Root[0]).Attributes['href']);
end;


procedure TTestSVGDocumentReading.TestNamespaceDeclarationsAreDropped;

begin
  FDocument := ReadSVGString('<svg xmlns="http://www.w3.org/2000/svg" '
    + 'xmlns:xlink="http://www.w3.org/1999/xlink" width="1"/>');
  AssertFalse('the default namespace declaration is not an attribute',
    FDocument.Root.HasAttribute('xmlns'));
  AssertFalse('a prefixed namespace declaration is not an attribute',
    FDocument.Root.HasAttribute('xmlns:xlink'));
  AssertEquals('only the real attribute is kept', 1,
    FDocument.Root.AttributeCount);
end;


procedure TTestSVGDocumentReading.TestWhitespaceOnlyTextIsDropped;

begin
  FDocument := ReadSVGString('<svg xmlns="http://www.w3.org/2000/svg">'
    + '  '#10'  <rect/>'#10'</svg>');
  AssertEquals('indentation does not become a text node', 1,
    FDocument.Root.ChildCount);
end;


procedure TTestSVGDocumentReading.TestTextIsKept;

begin
  FDocument := ReadSVGString('<svg xmlns="http://www.w3.org/2000/svg">'
    + '<text>Hello</text></svg>');
  AssertEquals('character data survives reading', 'Hello',
    FDocument.Root.TextContent);
end;


procedure TTestSVGDocumentReading.TestIDsAreIndexedAfterReading;

begin
  FDocument := ReadSVGString('<svg xmlns="http://www.w3.org/2000/svg">'
    + '<defs><rect id="box"/></defs></svg>');
  AssertNotNull('an id deep in the tree is indexed',
    FDocument.ElementByID('box'));
  AssertTrue('the indexed element is the rect',
    FDocument.ElementByID('box') is TSVGRectElement);
end;


procedure TTestSVGDocumentReading.TestForeignElementsAreDroppedByDefault;

begin
  FDocument := ReadSVGString('<svg xmlns="http://www.w3.org/2000/svg" '
    + 'xmlns:x="http://example.invalid/ns"><x:thing/><rect/></svg>');
  AssertEquals('the foreign element is dropped', 1,
    FDocument.Root.ChildCount);
  AssertTrue('the SVG element survives',
    FDocument.Root[0] is TSVGRectElement);
end;


procedure TTestSVGDocumentReading.TestForeignElementsKeptOnRequest;

var
  lReader: TSVGReader;

begin
  lReader := TSVGReader.Create;
  try
    lReader.Options := [roKeepForeign];
    FDocument := lReader.ReadFromString('<svg xmlns="http://www.w3.org/2000/svg" '
      + 'xmlns:x="http://example.invalid/ns"><x:thing/></svg>');
  finally
    lReader.Free;
  end;
  AssertEquals('the foreign element is kept', 1, FDocument.Root.ChildCount);
  AssertTrue('it builds a foreign element',
    FDocument.Root[0] is TSVGForeignElement);
  AssertEquals('it remembers its namespace', 'http://example.invalid/ns',
    TSVGForeignElement(FDocument.Root[0]).Namespace);
end;


procedure TTestSVGDocumentReading.TestUnknownSVGTagIsKept;

begin
  FDocument := ReadSVGString('<svg xmlns="http://www.w3.org/2000/svg">'
    + '<bogus/></svg>');
  AssertEquals('an unknown SVG tag is still a node', 1,
    FDocument.Root.ChildCount);
  AssertTrue('it builds an unknown element',
    FDocument.Root[0] is TSVGUnknownElement);
end;


procedure TTestSVGDocumentReading.TestMissingRootIsRejected;

begin
  AssertException('a non-SVG root is rejected', ESVGRead, @ReadRootless);
end;


procedure TTestSVGDocumentReading.TestBasicTreeGolden;

begin
  FDocument := ReadSVGFile(DataDir + 'basic.svg');
  AssertGoldenText(Self, 'tree-basic', FDocument.DumpTree);
end;


procedure TTestSVGDocumentReading.TestForeignTreeGolden;

begin
  FDocument := ReadSVGFile(DataDir + 'foreign.svg');
  AssertGoldenText(Self, 'tree-foreign', FDocument.DumpTree);
end;


{ TTestSVGAttributeAccess }

// The three characters of the tests below, as the bytes UTF-8 writes
// them: e acute, the ideograph for sun, and the euro sign. They are built
// as bytes rather than written out, a literal being read in whatever code
// page the compiler takes the source for.
function AccentedBytes: RawByteString;

begin
  Result := #$C3#$A9 + ' ' + #$E6#$97#$A5 + ' ' + #$E2#$82#$AC;
end;


procedure TTestSVGAttributeAccess.TestAnAttributeKeepsItsBytes;

var
  lElement: TSVGElement;
  lValue: TSVGString;

begin
  lValue := AccentedBytes;
  lElement := FDocument.Root;
  lElement.Attributes['aria-label'] := lValue;
  AssertEquals('an attribute reads back as it was set', lValue,
    lElement.Attributes['aria-label']);
  AssertEquals('and its length is counted in bytes', 10,
    Length(lElement.Attributes['aria-label']));
end;


procedure TTestSVGAttributeAccess.TestTextKeepsItsBytes;

var
  lElement: TSVGElement;
  lValue: TSVGString;

begin
  lValue := AccentedBytes;
  lElement := TSVGElement.Create('text');
  FDocument.Root.AppendChild(lElement);
  lElement.AppendChild(TSVGTextNode.Create(lValue));
  AssertEquals('the text of an element reads back as it was set', lValue,
    lElement.TextContent);
end;


procedure TTestSVGAttributeAccess.TestADocumentKeepsTheBytesOfItsText;

var
  lSource: RawByteString;
  lDocument: TSVGDocument;
  lText: TSVGElement;

begin
  lSource := '<svg xmlns="http://www.w3.org/2000/svg" width="10" '
    + 'height="10"><text id="' + AccentedBytes + '">' + AccentedBytes
    + '</text></svg>';
  lDocument := ReadSVGString(lSource);
  try
    AssertNotNull('the document is read', lDocument);
    lText := lDocument.Root.FindChildElement('text');
    AssertNotNull('and holds the text element', lText);
    AssertEquals('its text is the bytes of the source',
      TSVGString(AccentedBytes), lText.TextContent);
    AssertEquals('and so is the id it was given',
      TSVGString(AccentedBytes), lText.ID);
    AssertTrue('which the id index answers to',
      lDocument.ElementByID(AccentedBytes) = lText);
  finally
    lDocument.Free;
  end;
end;


procedure TTestSVGAttributeAccess.SetUp;

begin
  inherited SetUp;
  FDocument := ReadSVGFile(DataDir + 'basic.svg');
end;


procedure TTestSVGAttributeAccess.TearDown;

begin
  FreeAndNil(FDocument);
  inherited TearDown;
end;


procedure TTestSVGAttributeAccess.TestLengthAttribute;

var
  lLength: TSVGLength;

begin
  lLength := TSVGLength.Zero;
  lLength.ReadAttribute(FDocument.Root, 'width');
  AssertEquals('the width attribute is read as a length', 200,
    lLength.Value, 1e-9);
  AssertTrue('a bare width is in user units', lLength.LengthUnit = luNumber);
end;


procedure TTestSVGAttributeAccess.TestAbsentLengthFallsBack;

var
  lLength: TSVGLength;

begin
  lLength := TSVGLength.Create(7, luPx);
  lLength.ReadAttribute(FDocument.Root, 'rx');
  AssertEquals('an absent attribute leaves the value alone', 7,
    lLength.Value, 1e-9);
  AssertTrue('an absent attribute leaves the unit alone',
    lLength.LengthUnit = luPx);
end;


procedure TTestSVGAttributeAccess.TestMalformedLengthFallsBack;

var
  lLength: TSVGLength;

begin
  FDocument.Root.Attributes['width'] := 'wide';
  lLength := TSVGLength.Create(3, luNumber);
  lLength.ReadAttribute(FDocument.Root, 'width');
  AssertEquals('a malformed length leaves the value alone', 3,
    lLength.Value, 1e-9);
end;


procedure TTestSVGAttributeAccess.TestViewBoxAttribute;

var
  lRect: TSVGRect;

begin
  AssertTrue('the viewBox is read', lRect.ReadViewBoxAttribute(FDocument.Root));
  AssertEquals('the viewBox width is read', 400, lRect.Width, 1e-9);
  AssertEquals('the viewBox height is read', 200, lRect.Height, 1e-9);
end;


procedure TTestSVGAttributeAccess.TestAbsentViewBox;

var
  lRect: TSVGRect;

begin
  AssertFalse('an element without a viewBox reports none',
    lRect.ReadViewBoxAttribute(FDocument.ElementByID('layer')));
  AssertTrue('the rectangle is left empty', lRect.IsEmpty);
end;


procedure TTestSVGAttributeAccess.TestPreserveAspectRatioAttribute;

var
  lAspect: TSVGPreserveAspectRatio;

begin
  lAspect.ReadAttribute(FDocument.Root);
  AssertTrue('the alignment is read', lAspect.Align = paXMidYMid);
  AssertTrue('the mode is read', lAspect.MeetOrSlice = msMeet);
  lAspect.ReadAttribute(FDocument.ElementByID('layer'));
  AssertTrue('an absent attribute falls back to the initial value',
    lAspect.Align = paXMidYMid);
end;


procedure TTestSVGAttributeAccess.TestTransformAttribute;

var
  lMatrix: TSVGMatrix;
  lPoint: TSVGPoint;

begin
  lMatrix.ReadAttribute(FDocument.ElementByID('layer'));
  lPoint := lMatrix.Transform(TSVGPoint.Create(1, 1));
  AssertEquals('the transform list is composed left to right (x)', 12,
    lPoint.X, 1e-9);
  AssertEquals('the transform list is composed left to right (y)', 22,
    lPoint.Y, 1e-9);
end;


procedure TTestSVGAttributeAccess.TestAbsentTransformIsIdentity;

var
  lMatrix: TSVGMatrix;

begin
  lMatrix.ReadAttribute(FDocument.Root);
  AssertTrue('an element without a transform yields the identity',
    lMatrix.IsIdentity);
end;


procedure TTestSVGAttributeAccess.TestHRefFromXLink;

var
  lUse: TSVGElement;

begin
  lUse := FDocument.ElementByID('layer').FindChildElement('use');
  AssertNotNull('the use element is found', lUse);
  AssertEquals('an xlink href reads back as href', '#box', SVGHRefOf(lUse));
  AssertSame('the reference resolves to the defined rect',
    FDocument.ElementByID('box'), FDocument.ResolveReference(SVGHRefOf(lUse)));
end;


{ TTestSVGElementBase }

procedure TTestSVGElementBase.TearDown;

begin
  FreeAndNil(FDocument);
  inherited TearDown;
end;


function TTestSVGElementBase.BaseOf(const aBody, aID: String): String;

var
  lElement: TSVGElement;

begin
  FreeAndNil(FDocument);
  FDocument := ReadSVGString(
    '<svg xmlns="http://www.w3.org/2000/svg" width="100" height="100">'
    + aBody + '</svg>');
  FDocument.BaseURI := 'here/doc.svg';
  lElement := FDocument.ElementByID(aID);
  AssertNotNull('the document holds an element with id ' + aID, lElement);
  Result := SVGElementBase(lElement, FDocument.BaseURI);
end;


procedure TTestSVGElementBase.TestWithoutOneTheDocumentBaseStands;

begin
  AssertEquals('a reference resolves against the location the document was read from',
    'here/doc.svg', BaseOf('<image id="x"/>', 'x'));
end;


procedure TTestSVGElementBase.TestOneOnTheElementMovesIt;

begin
  AssertEquals('the base of the element is the one its xml:base gives',
    'here/pictures/', BaseOf('<image id="x" xml:base="pictures/"/>', 'x'));
end;


procedure TTestSVGElementBase.TestOneOnAnAncestorMovesIt;

begin
  AssertEquals('a base above the element reaches it', 'here/pictures/',
    BaseOf('<g xml:base="pictures/"><image id="x"/></g>', 'x'));
end;


procedure TTestSVGElementBase.TestTheInnerOneResolvesAgainstTheOuter;

begin
  AssertEquals('each base resolves against the one above it',
    'here/pictures/small/',
    BaseOf('<g xml:base="pictures/"><image id="x" xml:base="small/"/></g>',
      'x'));
end;


procedure TTestSVGElementBase.TestAnAbsoluteOneReplacesWhatIsAboveIt;

begin
  AssertEquals('a base of its own takes the place of the one above',
    '/pictures/',
    BaseOf('<g xml:base="ignored/"><image id="x" xml:base="/pictures/"/></g>',
      'x'));
end;


initialization
  RegisterTest('dom', TTestSVGElementBase);
  RegisterTest('dom', TTestSVGElementRegistry);
  RegisterTest('dom', TTestSVGTree);
  RegisterTest('dom', TTestSVGReference);
  RegisterTest('dom', TTestSVGDocumentReading);
  RegisterTest('dom', TTestSVGAttributeAccess);
end.
