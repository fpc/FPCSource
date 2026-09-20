{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt

    Operation-log golden tests for shape elements converted to paths.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit tcsvgshapes;

{$mode objfpc}{$H+}
{$IFDEF UNICODERTL}
{$modeswitch unicodestrings}
{$ENDIF}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils, FpcUnit.Test, FpcUnit.Registry, svggoldens,
     fpsvg.types, fpsvg.backend, fpsvg.trace, fpsvg.dom, fpsvg.path,
     fpsvg.read;
{$ELSE FPC_DOTTEDUNITS}
uses sysutils, fpcunit, testregistry, svggoldens, fpsvg.types,
     fpsvg.backend, fpsvg.trace, fpsvg.dom, fpsvg.path, fpsvg.read;
{$ENDIF FPC_DOTTEDUNITS}

type
  TTestSVGShapeElements = class(TTestCase)
  private
    FDocument: TSVGDocument;
    FBackend: TSVGTraceBackend;
    FPath: TSVGPath;
    FContext: TSVGLengthContext;
    // Converts the element with the given id, then fills it through the
    // trace backend.
    procedure TraceShape(const aID: String);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestShapeRecognition;
    procedure TestGroupIsNotAShape;
    procedure TestPercentageResolvesAgainstTheContext;
    procedure TestMalformedPathDataIsReported;
    procedure TestAllShapesGolden;
  end;

implementation

procedure TTestSVGShapeElements.SetUp;

begin
  inherited SetUp;
  FDocument := ReadSVGFile(DataDir + 'shapes.svg');
  FBackend := TSVGTraceBackend.Create;
  FPath := TSVGPath.Create;
  FContext := TSVGLengthContext.Create(TSVGRect.CreateSize(0, 0, 200, 200));
end;


procedure TTestSVGShapeElements.TearDown;

begin
  FreeAndNil(FPath);
  FreeAndNil(FBackend);
  FreeAndNil(FDocument);
  inherited TearDown;
end;


procedure TTestSVGShapeElements.TraceShape(const aID: String);

var
  lElement: TSVGElement;

begin
  lElement := FDocument.ElementByID(aID);
  AssertNotNull('the document holds an element with id ' + aID, lElement);
  AssertTrue('the shape ' + aID + ' converts to a path',
    BuildSVGShapePath(lElement, FPath, FContext));
  FBackend.Log.Add('# ' + aID);
  FBackend.FillPath(FPath, TSVGMatrix.Identity,
    TSVGPaint.CreateColor(TSVGColor.FromBytes(0, 0, 0, 255)), frNonZero, 1);
end;


procedure TTestSVGShapeElements.TestShapeRecognition;

begin
  AssertTrue('a rect is a shape',
    IsSVGShapeElement(FDocument.ElementByID('plain')));
  AssertTrue('a path is a shape',
    IsSVGShapeElement(FDocument.ElementByID('curve')));
  AssertTrue('a polyline is a shape',
    IsSVGShapeElement(FDocument.ElementByID('open')));
end;


procedure TTestSVGShapeElements.TestGroupIsNotAShape;

begin
  AssertFalse('the root svg element is not a shape',
    IsSVGShapeElement(FDocument.Root));
  AssertFalse('a non-shape does not convert',
    BuildSVGShapePath(FDocument.Root, FPath, FContext));
  AssertFalse('a nil element does not convert',
    BuildSVGShapePath(nil, FPath, FContext));
end;


procedure TTestSVGShapeElements.TestPercentageResolvesAgainstTheContext;

begin
  AssertTrue('the percentage rect converts',
    BuildSVGShapePath(FDocument.ElementByID('percent'), FPath, FContext));
  AssertEquals('half of a 200 unit viewport is 100', 100,
    FPath.ControlBounds.Width, 1e-9);
  AssertEquals('a quarter of a 200 unit viewport is 50', 50,
    FPath.ControlBounds.Height, 1e-9);
end;


procedure TTestSVGShapeElements.TestMalformedPathDataIsReported;

var
  lElement: TSVGElement;

begin
  lElement := FDocument.ElementByID('curve');
  lElement.Attributes['d'] := 'M0,0 L10,10 L20';
  AssertFalse('malformed path data is reported',
    BuildSVGShapePath(lElement, FPath, FContext));
  AssertEquals('the segments before the error survive', 2,
    FPath.SegmentCount);
end;


procedure TTestSVGShapeElements.TestAllShapesGolden;

begin
  FBackend.BeginFrame(200, 200);
  TraceShape('plain');
  TraceShape('rounded');
  TraceShape('onlyrx');
  TraceShape('percent');
  TraceShape('dot');
  TraceShape('oval');
  TraceShape('edge');
  TraceShape('open');
  TraceShape('closed');
  TraceShape('curve');
  TraceShape('arcs');
  TraceShape('quads');
  FBackend.EndFrame;
  AssertGolden(Self, 'shapes', FBackend.Log);
end;


initialization
  RegisterTest('shapes', TTestSVGShapeElements);
end.
