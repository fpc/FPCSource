{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for the SMIL timeline: clocks, interpolation and seeking.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit tcsvganim;

{$mode objfpc}{$H+}
{$IFDEF UNICODERTL}
{$modeswitch unicodestrings}
{$ENDIF}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils, System.Classes, FpcUnit.Test, FpcUnit.Registry,
     svggoldens, fpsvg.types, fpsvg.dom, fpsvg.read, fpsvg.anim,
     fpsvg.trace, fpsvg.render;
{$ELSE FPC_DOTTEDUNITS}
uses sysutils, classes, fpcunit, testregistry, svggoldens, fpsvg.types,
     fpsvg.dom, fpsvg.read, fpsvg.anim, fpsvg.trace, fpsvg.render;
{$ENDIF FPC_DOTTEDUNITS}

type
  TTestSVGClock = class(TTestCase)
  private
    // The seconds a clock value reads as. Fails when it is not one.
    function Clock(const aText: String): Double;
  published
    procedure TestBareNumberIsSeconds;
    procedure TestSecondsSuffix;
    procedure TestMilliseconds;
    procedure TestMinutes;
    procedure TestHours;
    procedure TestPartialClockValue;
    procedure TestFullClockValue;
    procedure TestNegativeOffset;
    procedure TestWhatIsNotAClock;
  end;

  { The entries of a begin or an end list. }
  TTestSVGTimeEntry = class(TTestCase)
  private
    // The entry a text reads as. Fails when it is not one.
    function Entry(const aText: String): TSVGTimeEntry;
  published
    procedure TestAClockIsATimeOfItsOwn;
    procedure TestTheBeginOfAnotherAnimation;
    procedure TestTheEndOfAnotherAnimation;
    procedure TestAnOffsetAfterWhatItNames;
    procedure TestAnOffsetBeforeWhatItNames;
    procedure TestAnIdOfDigitsAndUnderscores;
    procedure TestARunOfAnotherAnimation;
    procedure TestAMomentOfTheWallClock;
    procedure TestWhatIsNotATimeEntry;
  end;

  TTestSVGInterpolation = class(TTestCase)
  published
    procedure TestNumberInterpolates;
    procedure TestLengthKeepsItsUnit;
    procedure TestLengthsOfDifferentUnitsStep;
    procedure TestLengthListInterpolates;
    procedure TestLengthListsOfUnlikeShapeStep;
    procedure TestColourInterpolates;
    procedure TestColourNameInterpolates;
    procedure TestNumberListInterpolates;
    procedure TestPathInterpolates;
    procedure TestPathsOfUnlikeCommandsStep;
    procedure TestPathRepeatedArgumentsAreCommandsOfTheirOwn;
    procedure TestPathArcFlagsComeFromTheNearerEnd;
    procedure TestWhatCannotBeReadStepsHalfWay;
    procedure TestEndsAreGivenBack;
    procedure TestValueKindOfAnAttribute;
  end;

  { One animation of a document, and the values it gives over time. }
  TTestSVGAnimation = class(TTestCase)
  private
    FDocument: TSVGDocument;
    FTimeline: TSVGTimeline;
    FSpare: TSVGAnimation;
    // Reads a document of one rectangle holding the given markup.
    procedure Build(const aBody: String);
    // The value the first animation gives, or "(none)" when it gives
    // none.
    function At(aSeconds: Double): TSVGString;
    // The same value read as a number. Fails when it is not one.
    function AtNumber(aSeconds: Double): Double;
    procedure IndexOutOfRange;
  protected
    procedure TearDown; override;
  published
    procedure TestValueAtTheStartIsTheFirst;
    procedure TestValueAtTheEndIsTheLast;
    procedure TestValueHalfWayInterpolates;
    procedure TestBeforeBeginNoValueIsGiven;
    procedure TestAfterTheEndTheValueIsRemoved;
    procedure TestFillFreezeKeepsTheLastValue;
    procedure TestRepeatCountRunsTheValuesAgain;
    procedure TestRepeatCountEndsAfterItsRuns;
    procedure TestAFractionalRepeatFreezesPartWay;
    procedure TestIndefiniteRepeatNeverEnds;
    procedure TestSetHoldsItsValueOn;
    procedure TestToAloneStartsFromTheBase;
    procedure TestByAloneAddsToTheBase;
    procedure TestFromAndByAddUp;
    procedure TestDiscreteCalcModeSteps;
    procedure TestKeyTimesPlaceTheValues;
    procedure TestKeyTimesOfTheWrongLengthAreIgnored;
    procedure TestEndsAtIsTheEndOfTheActiveInterval;
    procedure TestEndsAtOfAnIndefiniteAnimationIsNegative;
    procedure TestAnEventBeginNeverRuns;
    procedure TestAnimationIndexOutOfRangeRaises;
    procedure TestTranslateWritesTheFunction;
    procedure TestOneArgumentTranslateGainsAZero;
    procedure TestScaleRepeatsItsOneArgument;
    procedure TestRotateGainsACentreOfNothing;
    procedure TestRotateKeepsTheCentreItIsGiven;
    procedure TestSkewTakesOneArgument;
    procedure TestATypeThatIsNotGivenIsTranslate;
    procedure TestAValuesListOfTransforms;
    procedure TestATransformValueOfTheWrongLengthIsDropped;
    procedure TestAccumulateAddsTheRunsBefore;
    procedure TestWithoutAccumulateEveryRunIsTheSame;
    procedure TestATransformAccumulatesByItsArguments;
    procedure TestAToAnimationDoesNotAccumulate;
    procedure TestAnimateColorInterpolatesTheColour;
    procedure TestAnimateColorReadsEveryValueAsAColour;
    procedure TestPacedSpacesTheValuesByDistance;
    procedure TestPacedStandsInForTheKeyTimesGiven;
    procedure TestPacedOfAValueWithoutDistanceSpacesEvenly;
    procedure TestPacedOfATransformMeasuresItsArguments;
    procedure TestThePointsOfAShapeInterpolate;
    procedure TestTheDOfAPathInterpolates;
    procedure TestTheDOfAPathAccumulates;
    procedure TestAnXLinkAttributeNameDropsItsPrefix;
    procedure TestCurrentColorIsTheColourOfTheTarget;
    procedure TestCurrentColorIsInheritedFromAnAncestor;
    procedure TestCurrentColorFollowsAnAnimatedColour;
    procedure TestCurrentColorOfADocumentSettingNoneIsBlack;
    procedure TestTheNumberOfAFilterPrimitiveInterpolates;
    procedure TestThePositionListOfATextInterpolates;
    procedure TestTheGlyphAnglesOfATextInterpolate;
    procedure TestSplineWithAStraightCurveRunsAsLinear;
    procedure TestSplineReachesTheValueEarly;
    procedure TestSplineEndsAreTheValuesThemselves;
    procedure TestSplineWithoutKeySplinesRunsAsLinear;
    procedure TestMotionAlongAPathWritesATranslate;
    procedure TestMotionOverItsValuesInterpolatesThePoints;
    procedure TestMotionFromAndTo;
    procedure TestMotionByStartsFromNoOffset;
    procedure TestMotionIsPacedUnlessItSaysOtherwise;
    procedure TestMotionRotateAutoTurnsWithThePath;
    procedure TestMotionRotateAutoReverseAddsHalfATurn;
    procedure TestMotionRotateTakesAFixedAngle;
    procedure TestKeyPointsIndexThePathByLength;
    procedure TestKeyPointsOverValuesMakeThemAPath;
    procedure TestAnMPathNamesThePathToRunAlong;
    procedure TestAMotionWithoutAPathOrValuesIsDropped;
    procedure TestAMotionValueOfTheWrongLengthIsDropped;
  end;

  { Writing the values into the document, and putting them back. }
  TTestSVGTimeline = class(TTestCase)
  private
    FDocument: TSVGDocument;
    FTimeline: TSVGTimeline;
    // Reads a document and builds a timeline over it.
    procedure Build(const aBody: String);
    // The same, with the moment the document clock starts at.
    procedure BuildAt(const aBody: String; aNow: TDateTime);
    // The attribute of the element with that id.
    function Attribute(const aID, aName: String): String;
    // True when the element with that id has the attribute.
    function Has(const aID, aName: String): Boolean;
  protected
    procedure TearDown; override;
  published
    procedure TestSeekWritesTheAttribute;
    procedure TestResetPutsTheAttributeBack;
    procedure TestSeekingBackwardsGivesTheSameValue;
    procedure TestSeekAddsAnAttributeTheDocumentHadNot;
    procedure TestResetRemovesAnAttributeTheDocumentHadNot;
    procedure TestSeekBeforeBeginRemovesItAgain;
    procedure TestALaterAnimationOfTheSameAttributeWins;
    procedure TestAFrozenAnimationIsNotOverwritten;
    procedure TestAnHRefNamesTheTarget;
    procedure TestCountIsEveryAnimationInDocumentOrder;
    procedure TestADocumentWithoutAnimationIsNotAnimated;
    procedure TestDurationIsWhereTheLastAnimationEnds;
    procedure TestDurationIsNegativeWhenOneNeverEnds;
    procedure TestChangeTimesHoldTheStepsOfADiscreteAnimation;
    procedure TestChangeTimesOfAStillDocumentAreEmpty;
    procedure TestAnAdditiveTransformRunsOverTheBase;
    procedure TestATransformThatReplacesDropsTheBase;
    procedure TestTwoAdditiveTransformsRunInDocumentOrder;
    procedure TestAnAdditiveNumberAddsToTheBase;
    procedure TestAnAdditiveColourAddsToTheBase;
    procedure TestAdditiveOfAKindThatCannotAddReplaces;
    procedure TestASetTakesNeitherAdditiveNorAccumulate;
    procedure TestAByTransformStartsAtRestAndAdds;
    procedure TestAnAdditiveAnimationRunsOverAFrozenOne;
    procedure TestASyncBaseTakesTheBeginOfAnother;
    procedure TestASyncBaseTakesTheEndOfAnother;
    procedure TestASyncBaseIsResolvedWhicheverOrderTheyAreWrittenIn;
    procedure TestASyncBaseOfAnAnimationThatNeverEndsNeverBegins;
    procedure TestASyncBaseNamingNothingNeverBegins;
    procedure TestACycleOfSyncBasesKeepsItsOwnTimes;
    procedure TestAnEndAttributeCutsTheIntervalShort;
    procedure TestAnEndAfterTheDurationChangesNothing;
    procedure TestASyncBaseInAnEndAttribute;
    procedure TestSeveralBeginsRunTheAnimationAgain;
    procedure TestAnEndListPairsWithTheBegins;
    procedure TestChangeTimesHoldEveryBegin;
    procedure TestRepeatDurRunsTheAnimationForThatLong;
    procedure TestRepeatCountAndRepeatDurTakeTheShorter;
    procedure TestAnIndefiniteRepeatDurNeverEnds;
    procedure TestMaxCutsTheActiveDuration;
    procedure TestMinStretchesTheActiveDuration;
    procedure TestAMinAboveAMaxLeavesBothUnread;
    procedure TestRestartNeverKeepsTheFirstBeginOnly;
    procedure TestRestartWhenNotActiveDropsABeginInsideTheRun;
    procedure TestRestartAlwaysTakesEveryBegin;
    procedure TestASyncOnARunOfAnother;
    procedure TestASyncOnARunItNeverReachesNeverBegins;
    procedure TestAWallClockWithoutAClockToMeasureNeverBegins;
    procedure TestAWallClockAgainstTheClockGiven;
    procedure TestAWallClockAlreadyPastAppliesFromTheStart;
    procedure TestAMotionKeepsTheTransformUnderIt;
    procedure TestAMotionStandsOutsideATransformThatReplaces;
    procedure TestAMotionAndTwoTransformsOfOneElement;
    procedure TestResetTakesAwayTheMotionTransform;
  end;

  { The documents at three times each, through the tracing backend. }
  TTestSVGAnimGoldens = class(TTestCase)
  private
    FDocument: TSVGDocument;
    FTimeline: TSVGTimeline;
    FTrace: TSVGTraceBackend;
    FRenderer: TSVGRenderer;
    // Reads one of the test documents and builds a timeline over it.
    procedure Load(const aFileName: String);
    // Seeks to a time and renders, comparing against the golden log.
    procedure CheckAt(aSeconds: Double; const aName: String);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestGoldenAtZero;
    procedure TestGoldenAtHalfASecond;
    procedure TestGoldenAtTwoSeconds;
    procedure TestTransformGoldenAtZero;
    procedure TestTransformGoldenAtHalfASecond;
    procedure TestTransformGoldenAtTwoSeconds;
    procedure TestMotionGoldenAtZero;
    procedure TestMotionGoldenAtHalfASecond;
    procedure TestMotionGoldenAtTwoSeconds;
    procedure TestChainGoldenAtZero;
    procedure TestChainGoldenAtOneAndAHalfSeconds;
    procedure TestChainGoldenAtTwoSeconds;
  end;

implementation

const
  NotRunning = '(none)';

// Wraps markup in a root element of a hundred by a hundred.
function Doc(const aBody: String): String;

begin
  Result := '<svg xmlns="http://www.w3.org/2000/svg"'
    + ' xmlns:xlink="http://www.w3.org/1999/xlink"'
    + ' width="100" height="100">' + aBody + '</svg>';
end;


{ TTestSVGClock }

function TTestSVGClock.Clock(const aText: String): Double;

begin
  AssertTrue(Format('"%s" reads as a clock value', [aText]),
    TryStrToSVGClock(aText, Result));
end;


procedure TTestSVGClock.TestBareNumberIsSeconds;

begin
  AssertEquals('a bare number is a count of seconds', 3.0, Clock('3'), 1e-9);
end;


procedure TTestSVGClock.TestSecondsSuffix;

begin
  AssertEquals('the s suffix is seconds', 3.0, Clock('3s'), 1e-9);
  AssertEquals('a fraction of a second is kept', 1.5, Clock('1.5s'), 1e-9);
end;


procedure TTestSVGClock.TestMilliseconds;

begin
  AssertEquals('the ms suffix is thousandths', 0.25, Clock('250ms'), 1e-9);
end;


procedure TTestSVGClock.TestMinutes;

begin
  AssertEquals('the min suffix is sixties', 90.0, Clock('1.5min'), 1e-9);
end;


procedure TTestSVGClock.TestHours;

begin
  AssertEquals('the h suffix is hours', 5400.0, Clock('1.5h'), 1e-9);
end;


procedure TTestSVGClock.TestPartialClockValue;

begin
  AssertEquals('minutes and seconds', 90.0, Clock('01:30'), 1e-9);
  AssertEquals('a fraction on the seconds', 90.25, Clock('01:30.25'), 1e-9);
end;


procedure TTestSVGClock.TestFullClockValue;

begin
  AssertEquals('hours, minutes and seconds', 3723.0, Clock('01:02:03'), 1e-9);
end;


procedure TTestSVGClock.TestNegativeOffset;

begin
  AssertEquals('an offset may be negative', -2.0, Clock('-2s'), 1e-9);
end;


procedure TTestSVGClock.TestWhatIsNotAClock;

var
  lSeconds: Double;

begin
  AssertFalse('an empty text is not a clock value',
    TryStrToSVGClock('', lSeconds));
  AssertFalse('indefinite is not a clock value',
    TryStrToSVGClock('indefinite', lSeconds));
  AssertFalse('a word is not a clock value',
    TryStrToSVGClock('mouseover', lSeconds));
  AssertFalse('a metric this does not know is not a clock value',
    TryStrToSVGClock('3sec', lSeconds));
  AssertFalse('a space before the metric is not allowed',
    TryStrToSVGClock('3 s', lSeconds));
  AssertFalse('four fields are not a clock value',
    TryStrToSVGClock('1:2:3:4', lSeconds));
  AssertFalse('a fraction on the minutes is not allowed',
    TryStrToSVGClock('1.5:30', lSeconds));
end;


{ TTestSVGTimeEntry }

function TTestSVGTimeEntry.Entry(const aText: String): TSVGTimeEntry;

begin
  AssertTrue(Format('"%s" reads as an entry of a begin list', [aText]),
    TryStrToSVGTimeEntry(aText, Result));
end;


procedure TTestSVGTimeEntry.TestAClockIsATimeOfItsOwn;

var
  lEntry: TSVGTimeEntry;

begin
  lEntry := Entry('2.5s');
  AssertTrue('a clock value names no other animation',
    lEntry.Kind = teOffset);
  AssertEquals('and is the time itself', 2.5, lEntry.Offset, 1e-9);
end;


procedure TTestSVGTimeEntry.TestTheBeginOfAnotherAnimation;

var
  lEntry: TSVGTimeEntry;

begin
  lEntry := Entry('logo.begin');
  AssertTrue('the begin of another animation', lEntry.Kind = teSyncBegin);
  AssertEquals('the id it names', 'logo', lEntry.Base);
  AssertEquals('and no offset from it', 0.0, lEntry.Offset, 1e-9);
end;


procedure TTestSVGTimeEntry.TestTheEndOfAnotherAnimation;

var
  lEntry: TSVGTimeEntry;

begin
  lEntry := Entry('logo.end');
  AssertTrue('the end of another animation', lEntry.Kind = teSyncEnd);
  AssertEquals('the id it names', 'logo', lEntry.Base);
end;


procedure TTestSVGTimeEntry.TestAnOffsetAfterWhatItNames;

var
  lEntry: TSVGTimeEntry;

begin
  lEntry := Entry('logo.end + 2s');
  AssertTrue('the end of another animation', lEntry.Kind = teSyncEnd);
  AssertEquals('two seconds after it', 2.0, lEntry.Offset, 1e-9);
  AssertEquals('and the spaces about the sign are read past', 2.0,
    Entry('logo.end+2s').Offset, 1e-9);
end;


procedure TTestSVGTimeEntry.TestAnOffsetBeforeWhatItNames;

begin
  AssertEquals('an offset may take it back before what it names', -2.0,
    Entry('logo.end - 2s').Offset, 1e-9);
end;


procedure TTestSVGTimeEntry.TestAnIdOfDigitsAndUnderscores;

var
  lEntry: TSVGTimeEntry;

begin
  lEntry := Entry('secondSet3_1.end+1s');
  AssertEquals('the whole of the id is read', 'secondSet3_1', lEntry.Base);
  AssertEquals('and the offset after it', 1.0, lEntry.Offset, 1e-9);
end;


procedure TTestSVGTimeEntry.TestARunOfAnotherAnimation;

var
  lEntry: TSVGTimeEntry;

begin
  lEntry := Entry('logo.repeat(3)');
  AssertTrue('a run of another animation', lEntry.Kind = teSyncRepeat);
  AssertEquals('the id it names', 'logo', lEntry.Base);
  AssertEquals('the run it waits for', 3.0, lEntry.Count, 1e-9);
  AssertEquals('and an offset may follow it', 2.0,
    Entry('logo.repeat(3) + 2s').Offset, 1e-9);
end;


procedure TTestSVGTimeEntry.TestAMomentOfTheWallClock;

var
  lEntry: TSVGTimeEntry;

begin
  lEntry := Entry('wallclock(2000-06-10T12:00:00Z)');
  AssertTrue('a moment of the wall clock', lEntry.Kind = teWallClock);
  AssertEquals('the date and the time it names',
    EncodeDate(2000, 6, 10) + EncodeTime(12, 0, 0, 0), lEntry.When, 1e-9);
  AssertEquals('a date on its own is midnight of it',
    EncodeDate(2000, 6, 10), Entry('wallclock(2000-06-10)').When, 1e-9);
  AssertEquals('a zone of hours and minutes is taken off the time',
    EncodeDate(2000, 6, 10) + EncodeTime(10, 0, 0, 0),
    Entry('wallclock(2000-06-10T12:00+02:00)').When, 1e-9);
end;


procedure TTestSVGTimeEntry.TestWhatIsNotATimeEntry;

var
  lEntry: TSVGTimeEntry;

begin
  AssertFalse('an empty entry', TryStrToSVGTimeEntry('', lEntry));
  AssertFalse('an event on the document',
    TryStrToSVGTimeEntry('mouseover', lEntry));
  AssertFalse('an event on an element',
    TryStrToSVGTimeEntry('button.click', lEntry));
  AssertFalse('a begin that waits to be started',
    TryStrToSVGTimeEntry('indefinite', lEntry));
  AssertFalse('a key press', TryStrToSVGTimeEntry('accessKey(a)', lEntry));
  AssertFalse('a repeat with no run named',
    TryStrToSVGTimeEntry('logo.repeat', lEntry));
  AssertFalse('a repeat of no run at all',
    TryStrToSVGTimeEntry('logo.repeat(0)', lEntry));
  AssertFalse('a wall clock moment that is not a date',
    TryStrToSVGTimeEntry('wallclock(yesterday)', lEntry));
  AssertFalse('an offset with no sign before it',
    TryStrToSVGTimeEntry('logo.end 2s', lEntry));
  AssertFalse('a sync with no id before it',
    TryStrToSVGTimeEntry('.end', lEntry));
end;


{ TTestSVGInterpolation }

procedure TTestSVGInterpolation.TestNumberInterpolates;

begin
  AssertEquals('half way between two numbers', '15',
    SVGInterpolate(vkNumber, '10', '20', 0.5));
  AssertEquals('a quarter of the way', '2.5',
    SVGInterpolate(vkNumber, '0', '10', 0.25));
end;


procedure TTestSVGInterpolation.TestLengthKeepsItsUnit;

begin
  AssertEquals('the unit of the two ends is kept', '15px',
    SVGInterpolate(vkLength, '10px', '20px', 0.5));
  AssertEquals('a percentage interpolates as a percentage', '50%',
    SVGInterpolate(vkLength, '0%', '100%', 0.5));
end;


procedure TTestSVGInterpolation.TestLengthsOfDifferentUnitsStep;

begin
  AssertEquals('two units that do not agree step instead', '10px',
    SVGInterpolate(vkLength, '10px', '20em', 0.25));
  AssertEquals('and step at the half way point', '20em',
    SVGInterpolate(vkLength, '10px', '20em', 0.75));
end;


procedure TTestSVGInterpolation.TestLengthListInterpolates;

begin
  AssertEquals('every length of the list interpolates', '20 30 40',
    SVGInterpolate(vkLength, '10 20 30', '30 40 50', 0.5));
  AssertEquals('each keeps the unit it is written in', '2em 3em',
    SVGInterpolate(vkLength, '1em 2em', '3em 4em', 0.5));
end;


procedure TTestSVGInterpolation.TestLengthListsOfUnlikeShapeStep;

begin
  AssertEquals('lists of different lengths step instead', '10 20',
    SVGInterpolate(vkLength, '10 20', '30 40 50', 0.25));
  AssertEquals('a length of another unit than its counterpart steps as well',
    '10 20', SVGInterpolate(vkLength, '10 20', '30px 40px', 0.25));
end;


procedure TTestSVGInterpolation.TestColourInterpolates;

begin
  AssertEquals('a fifth of the way from black to green', '#003300ff',
    SVGInterpolate(vkColour, '#000000', '#00ff00', 0.2));
  AssertEquals('the middle of black and white', '#808080ff',
    SVGInterpolate(vkColour, '#000000', '#ffffff', 0.5));
end;


procedure TTestSVGInterpolation.TestColourNameInterpolates;

begin
  AssertEquals('a colour keyword is read as a colour', '#800000ff',
    SVGInterpolate(vkColour, 'black', 'red', 0.5));
end;


procedure TTestSVGInterpolation.TestNumberListInterpolates;

begin
  AssertEquals('every number of the list interpolates', '5 15',
    SVGInterpolate(vkNumberList, '0 10', '10 20', 0.5));
  AssertEquals('lists of different lengths step instead', '0 10',
    SVGInterpolate(vkNumberList, '0 10', '10 20 30', 0.25));
end;


procedure TTestSVGInterpolation.TestPathInterpolates;

begin
  AssertEquals('every coordinate of the path interpolates',
    'M 15 15 L 30 30', SVGInterpolate(vkPath, 'M 10 10 L 20 20',
    'M 20 20 L 40 40', 0.5));
  AssertEquals('a closed path keeps the commands it holds',
    'M 0 0 C 5 0 10 5 10 10 Z', SVGInterpolate(vkPath,
    'M 0 0 C 0 0 0 0 0 0 Z', 'M 0 0 C 10 0 20 10 20 20 Z', 0.5));
end;


procedure TTestSVGInterpolation.TestPathsOfUnlikeCommandsStep;

begin
  AssertEquals('paths of different commands step instead',
    'M 0 0 L 10 10', SVGInterpolate(vkPath, 'M 0 0 L 10 10',
    'M 0 0 C 0 0 5 5 10 10', 0.25));
  AssertEquals('a relative command is not the absolute one',
    'M 0 0 L 10 10', SVGInterpolate(vkPath, 'M 0 0 L 10 10',
    'M 0 0 l 20 20', 0.25));
  AssertEquals('paths of different lengths step as well',
    'M 0 0 L 10 10', SVGInterpolate(vkPath, 'M 0 0 L 10 10',
    'M 0 0 L 10 10 L 20 20', 0.25));
end;


procedure TTestSVGInterpolation.TestPathRepeatedArgumentsAreCommandsOfTheirOwn;

begin
  AssertEquals('the numbers after a moveto draw lines',
    'M 0 0 L 15 15', SVGInterpolate(vkPath, 'M 0 0 10 10',
    'M 0 0 L 20 20', 0.5));
  AssertEquals('a repeated lineto is a lineto of its own',
    'M 0 0 L 5 5 L 15 15', SVGInterpolate(vkPath, 'M 0 0 L 0 0 10 10',
    'M 0 0 L 10 10 20 20', 0.5));
end;


procedure TTestSVGInterpolation.TestPathArcFlagsComeFromTheNearerEnd;

begin
  AssertEquals('before the half way point the flags of the first stand',
    'M 0 0 A 12.5 12.5 0 0 1 25 25', SVGInterpolate(vkPath,
    'M 0 0 A 10 10 0 0 1 20 20', 'M 0 0 A 20 20 0 1 0 40 40', 0.25));
  AssertEquals('from the half way point the flags of the second do',
    'M 0 0 A 17.5 17.5 0 1 0 35 35', SVGInterpolate(vkPath,
    'M 0 0 A 10 10 0 0 1 20 20', 'M 0 0 A 20 20 0 1 0 40 40', 0.75));
end;


procedure TTestSVGInterpolation.TestWhatCannotBeReadStepsHalfWay;

begin
  AssertEquals('before the half way point the first value stands', 'hidden',
    SVGInterpolate(vkDiscrete, 'hidden', 'visible', 0.4));
  AssertEquals('from the half way point the second does', 'visible',
    SVGInterpolate(vkDiscrete, 'hidden', 'visible', 0.5));
  AssertEquals('a number that cannot be read steps as well', 'thin',
    SVGInterpolate(vkNumber, 'thin', 'thick', 0.25));
end;


procedure TTestSVGInterpolation.TestEndsAreGivenBack;

begin
  AssertEquals('at nothing the first value is given back', '10px',
    SVGInterpolate(vkLength, '10px', '20px', 0));
  AssertEquals('at one the second value is given back', '20px',
    SVGInterpolate(vkLength, '10px', '20px', 1));
end;


procedure TTestSVGInterpolation.TestValueKindOfAnAttribute;

begin
  AssertTrue('fill holds a colour', SVGValueKindOf('fill') = vkColour);
  AssertTrue('stroke-width holds a length',
    SVGValueKindOf('stroke-width') = vkLength);
  AssertTrue('x holds a length', SVGValueKindOf('x') = vkLength);
  AssertTrue('fill-opacity holds a number',
    SVGValueKindOf('fill-opacity') = vkNumber);
  AssertTrue('stroke-dasharray holds a list of numbers',
    SVGValueKindOf('stroke-dasharray') = vkNumberList);
  AssertTrue('the limitingConeAngle of a spot light holds a number',
    SVGValueKindOf('limitingConeAngle') = vkNumber);
  AssertTrue('the surfaceScale of a lighting primitive holds one too',
    SVGValueKindOf('surfaceScale') = vkNumber);
  AssertTrue('the stdDeviation of a blur holds one number or two',
    SVGValueKindOf('stdDeviation') = vkNumberList);
  AssertTrue('the markerWidth of a marker holds a length',
    SVGValueKindOf('markerWidth') = vkLength);
  AssertTrue('the numOctaves of a turbulence is an integer and steps',
    SVGValueKindOf('numOctaves') = vkDiscrete);
  AssertTrue('the rotate of a text holds a list of numbers',
    SVGValueKindOf('rotate') = vkNumberList);
  AssertTrue('the d of a path holds a path',
    SVGValueKindOf('d') = vkPath);
  AssertTrue('visibility is not interpolated',
    SVGValueKindOf('visibility') = vkDiscrete);
end;


{ TTestSVGAnimation }

procedure TTestSVGAnimation.Build(const aBody: String);

begin
  FreeAndNil(FTimeline);
  FreeAndNil(FDocument);
  FDocument := ReadSVGString(Doc(aBody));
  FTimeline := TSVGTimeline.Create(FDocument);
end;


procedure TTestSVGAnimation.TearDown;

begin
  FreeAndNil(FTimeline);
  FreeAndNil(FDocument);
  inherited TearDown;
end;


function TTestSVGAnimation.At(aSeconds: Double): TSVGString;

begin
  AssertTrue('the document declares an animation', FTimeline.Count > 0);
  if not FTimeline.Animations[0].ValueAt(aSeconds, Result) then
    Result := NotRunning;
end;


function TTestSVGAnimation.AtNumber(aSeconds: Double): Double;

var
  lText: String;

begin
  lText := At(aSeconds);
  AssertTrue(Format('the animation gives a number at %s seconds, not "%s"',
    [SVGFormatFloat(aSeconds), lText]), TryStrToSVGNumber(lText, Result));
end;


procedure TTestSVGAnimation.IndexOutOfRange;

begin
  FSpare := FTimeline.Animations[FTimeline.Count];
end;


procedure TTestSVGAnimation.TestValueAtTheStartIsTheFirst;

begin
  Build('<rect x="10" width="5" height="5">'
    + '<animate attributeName="x" from="10" to="50" dur="2s"/></rect>');
  AssertEquals('at the start the animation gives the first value', '10',
    At(0));
end;


procedure TTestSVGAnimation.TestValueAtTheEndIsTheLast;

begin
  Build('<rect x="10" width="5" height="5">'
    + '<animate attributeName="x" from="10" to="50" dur="2s" fill="freeze"/>'
    + '</rect>');
  AssertEquals('at the end the animation gives the last value', '50', At(2));
end;


procedure TTestSVGAnimation.TestValueHalfWayInterpolates;

begin
  Build('<rect x="10" width="5" height="5">'
    + '<animate attributeName="x" from="10" to="50" dur="2s"/></rect>');
  AssertEquals('half way through the run the value is half way', '30', At(1));
end;


procedure TTestSVGAnimation.TestBeforeBeginNoValueIsGiven;

begin
  Build('<rect x="10" width="5" height="5">'
    + '<animate attributeName="x" from="10" to="50" dur="2s" begin="1s"/>'
    + '</rect>');
  AssertEquals('before it begins the animation gives no value', NotRunning,
    At(0.5));
  AssertEquals('at its begin it gives the first value', '10', At(1));
end;


procedure TTestSVGAnimation.TestAfterTheEndTheValueIsRemoved;

begin
  Build('<rect x="10" width="5" height="5">'
    + '<animate attributeName="x" from="10" to="50" dur="2s"/></rect>');
  AssertEquals('without freeze the animation gives no value after its end',
    NotRunning, At(2.5));
end;


procedure TTestSVGAnimation.TestFillFreezeKeepsTheLastValue;

begin
  Build('<rect x="10" width="5" height="5">'
    + '<animate attributeName="x" from="10" to="50" dur="2s" fill="freeze"/>'
    + '</rect>');
  AssertEquals('a frozen animation keeps giving its last value', '50',
    At(100));
end;


procedure TTestSVGAnimation.TestRepeatCountRunsTheValuesAgain;

begin
  Build('<rect x="10" width="5" height="5">'
    + '<animate attributeName="x" from="10" to="50" dur="1s"'
    + ' repeatCount="3"/></rect>');
  AssertEquals('the second run starts from the first value again', '10',
    At(1));
  AssertEquals('half way into the second run the value is half way', '30',
    At(1.5));
end;


procedure TTestSVGAnimation.TestRepeatCountEndsAfterItsRuns;

begin
  Build('<rect x="10" width="5" height="5">'
    + '<animate attributeName="x" from="10" to="50" dur="1s"'
    + ' repeatCount="3"/></rect>');
  AssertEquals('after the last run the animation gives no value', NotRunning,
    At(3));
end;


procedure TTestSVGAnimation.TestAFractionalRepeatFreezesPartWay;

begin
  Build('<rect x="10" width="5" height="5">'
    + '<animate attributeName="x" from="10" to="50" dur="1s"'
    + ' repeatCount="2.5" fill="freeze"/></rect>');
  AssertEquals('a frozen part run holds the value it reached', '30', At(4));
end;


procedure TTestSVGAnimation.TestIndefiniteRepeatNeverEnds;

begin
  Build('<rect x="10" width="5" height="5">'
    + '<animate attributeName="x" from="10" to="50" dur="1s"'
    + ' repeatCount="indefinite"/></rect>');
  AssertEquals('an indefinite repeat is still running much later', '30',
    At(100.5));
  AssertTrue('an indefinite repeat never ends',
    FTimeline.Animations[0].EndsAt < 0);
end;


procedure TTestSVGAnimation.TestSetHoldsItsValueOn;

begin
  Build('<rect x="10" width="5" height="5">'
    + '<set attributeName="fill" to="green" begin="1s"/></rect>');
  AssertEquals('before its begin a set gives no value', NotRunning, At(0.5));
  AssertEquals('at its begin a set gives its value', 'green', At(1));
  AssertEquals('a set without a duration holds its value on', 'green',
    At(1000));
end;


procedure TTestSVGAnimation.TestToAloneStartsFromTheBase;

begin
  Build('<rect x="10" width="5" height="5">'
    + '<animate attributeName="x" to="30" dur="2s"/></rect>');
  AssertEquals('a to on its own starts from the value the file was read with',
    '10', At(0));
  AssertEquals('and runs to the value it names', '20', At(1));
end;


procedure TTestSVGAnimation.TestByAloneAddsToTheBase;

begin
  Build('<rect x="10" width="5" height="5">'
    + '<animate attributeName="x" by="8" dur="2s" fill="freeze"/></rect>');
  AssertEquals('a by on its own adds to the base value', '18', At(2));
  AssertEquals('and starts from the base value', '14', At(1));
end;


procedure TTestSVGAnimation.TestFromAndByAddUp;

begin
  Build('<rect x="10" width="5" height="5">'
    + '<animate attributeName="x" from="20" by="10" dur="2s" fill="freeze"/>'
    + '</rect>');
  AssertEquals('a from and a by run to their sum', '30', At(2));
end;


procedure TTestSVGAnimation.TestDiscreteCalcModeSteps;

begin
  Build('<rect x="10" width="5" height="5">'
    + '<animate attributeName="visibility"'
    + ' values="visible;hidden;visible" dur="3s" calcMode="discrete"/>'
    + '</rect>');
  AssertEquals('the first third is the first value', 'visible', At(0.5));
  AssertEquals('the second third is the second value', 'hidden', At(1.5));
  AssertEquals('the last third is the last value', 'visible', At(2.5));
end;


procedure TTestSVGAnimation.TestKeyTimesPlaceTheValues;

begin
  Build('<rect x="10" width="5" height="5">'
    + '<animate attributeName="x" values="0;10;20" keyTimes="0;0.75;1"'
    + ' dur="4s" fill="freeze"/></rect>');
  AssertEquals('the second value is reached at its key time', '10', At(3));
  AssertEquals('half way to it the value is half way', '5', At(1.5));
  AssertEquals('the last interval is the quarter that is left', '15',
    At(3.5));
end;


procedure TTestSVGAnimation.TestKeyTimesOfTheWrongLengthAreIgnored;

begin
  Build('<rect x="10" width="5" height="5">'
    + '<animate attributeName="x" values="0;10;20" keyTimes="0;1"'
    + ' dur="4s"/></rect>');
  AssertEquals('a key time list of the wrong length is dropped, so the '
    + 'values are spread evenly', '10', At(2));
end;


procedure TTestSVGAnimation.TestEndsAtIsTheEndOfTheActiveInterval;

begin
  Build('<rect x="10" width="5" height="5">'
    + '<animate attributeName="x" from="10" to="50" dur="2s" begin="1s"'
    + ' repeatCount="3"/></rect>');
  AssertEquals('the active interval ends after every run of it', 7.0,
    FTimeline.Animations[0].EndsAt, 1e-9);
end;


procedure TTestSVGAnimation.TestEndsAtOfAnIndefiniteAnimationIsNegative;

begin
  Build('<rect x="10" width="5" height="5">'
    + '<set attributeName="fill" to="green"/></rect>');
  AssertTrue('an animation without a duration never ends',
    FTimeline.Animations[0].EndsAt < 0);
end;


procedure TTestSVGAnimation.TestAnEventBeginNeverRuns;

begin
  Build('<rect x="10" width="5" height="5">'
    + '<set attributeName="fill" to="green" begin="mouseover"/></rect>');
  AssertEquals('an animation timed on an event is not run at all', 0,
    FTimeline.Count);
  AssertFalse('and the document counts as still', FTimeline.IsAnimated);
end;


procedure TTestSVGAnimation.TestAnimationIndexOutOfRangeRaises;

begin
  Build('<rect x="10" width="5" height="5">'
    + '<set attributeName="fill" to="green"/></rect>');
  AssertException('an index past the last animation is rejected',
    ESVGAnim, @IndexOutOfRange);
end;


procedure TTestSVGAnimation.TestTranslateWritesTheFunction;

begin
  Build('<rect width="5" height="5">'
    + '<animateTransform attributeName="transform" type="translate"'
    + ' from="0 0" to="100 50" dur="2s"/></rect>');
  AssertEquals('a transform animation writes the function it names',
    'translate(50 25)', At(1));
end;


procedure TTestSVGAnimation.TestOneArgumentTranslateGainsAZero;

begin
  Build('<rect width="5" height="5">'
    + '<animateTransform attributeName="transform" type="translate"'
    + ' from="0" to="30" dur="2s"/></rect>');
  AssertEquals('a translate of one argument moves along x alone',
    'translate(15 0)', At(1));
end;


procedure TTestSVGAnimation.TestScaleRepeatsItsOneArgument;

begin
  Build('<rect width="5" height="5">'
    + '<animateTransform attributeName="transform" type="scale"'
    + ' from="1" to="3" dur="2s"/></rect>');
  AssertEquals('a scale of one argument scales both axes by it',
    'scale(2 2)', At(1));
end;


procedure TTestSVGAnimation.TestRotateGainsACentreOfNothing;

begin
  Build('<rect width="5" height="5">'
    + '<animateTransform attributeName="transform" type="rotate"'
    + ' from="0" to="90" dur="2s"/></rect>');
  AssertEquals('a rotate of one argument turns about the origin',
    'rotate(45 0 0)', At(1));
end;


procedure TTestSVGAnimation.TestRotateKeepsTheCentreItIsGiven;

begin
  Build('<rect width="5" height="5">'
    + '<animateTransform attributeName="transform" type="rotate"'
    + ' values="0 50 50;90 50 50" dur="2s"/></rect>');
  AssertEquals('the centre of a rotate is interpolated with the angle',
    'rotate(45 50 50)', At(1));
end;


procedure TTestSVGAnimation.TestSkewTakesOneArgument;

begin
  Build('<rect width="5" height="5">'
    + '<animateTransform attributeName="transform" type="skewX"'
    + ' from="0" to="30" dur="2s"/></rect>');
  AssertEquals('a skew is written with the one angle it takes',
    'skewX(15)', At(1));
end;


procedure TTestSVGAnimation.TestATypeThatIsNotGivenIsTranslate;

begin
  Build('<rect width="5" height="5">'
    + '<animateTransform attributeName="transform" from="0 0" to="10 10"'
    + ' dur="2s"/></rect>');
  AssertEquals('an animateTransform without a type translates',
    'translate(5 5)', At(1));
end;


procedure TTestSVGAnimation.TestAValuesListOfTransforms;

begin
  Build('<rect width="5" height="5">'
    + '<animateTransform attributeName="transform" type="rotate"'
    + ' values="0;360;180;360" dur="3s"/></rect>');
  AssertEquals('the list is run in even thirds', 'rotate(180 0 0)', At(0.5));
  AssertEquals('and each value is reached at its own time',
    'rotate(360 0 0)', At(1));
end;


procedure TTestSVGAnimation.TestATransformValueOfTheWrongLengthIsDropped;

begin
  Build('<rect width="5" height="5">'
    + '<animateTransform attributeName="transform" type="rotate"'
    + ' from="0 1" to="90" dur="2s"/></rect>');
  AssertEquals('a rotate of two arguments is not a rotate at all, so the '
    + 'animation is dropped', 0, FTimeline.Count);
end;


procedure TTestSVGAnimation.TestAccumulateAddsTheRunsBefore;

begin
  Build('<rect x="0" width="5" height="5">'
    + '<animate attributeName="x" from="0" to="10" dur="1s"'
    + ' repeatCount="3" accumulate="sum" fill="freeze"/></rect>');
  AssertEquals('the first run is the values as they are', '5', At(0.5));
  AssertEquals('the second run builds on the end of the first', '15',
    At(1.5));
  AssertEquals('and the third on the end of the second', '25', At(2.5));
  AssertEquals('the frozen value is where the last run ended', '30', At(4));
end;


procedure TTestSVGAnimation.TestWithoutAccumulateEveryRunIsTheSame;

begin
  Build('<rect x="0" width="5" height="5">'
    + '<animate attributeName="x" from="0" to="10" dur="1s"'
    + ' repeatCount="3"/></rect>');
  AssertEquals('a repeat that does not accumulate runs the same values',
    '5', At(1.5));
end;


procedure TTestSVGAnimation.TestATransformAccumulatesByItsArguments;

begin
  Build('<rect width="5" height="5">'
    + '<animateTransform attributeName="transform" type="scale"'
    + ' from="0 0" to="2 2" dur="2.5s" repeatCount="2" accumulate="sum"'
    + ' fill="freeze"/></rect>');
  AssertEquals('the second run starts where the first ended',
    'scale(2 2)', At(2.5));
  AssertEquals('and its arguments add to it as it runs', 'scale(3 3)',
    At(3.75));
  AssertEquals('so that the two runs end at twice the last value',
    'scale(4 4)', At(5));
end;


procedure TTestSVGAnimation.TestAToAnimationDoesNotAccumulate;

begin
  Build('<rect x="10" width="5" height="5">'
    + '<animate attributeName="x" to="20" dur="1s" repeatCount="2"'
    + ' accumulate="sum"/></rect>');
  AssertEquals('a to animation runs from the base value again instead of '
    + 'accumulating', '15', At(1.5));
end;


procedure TTestSVGAnimation.TestAnimateColorInterpolatesTheColour;

begin
  Build('<rect width="5" height="5" fill="#000000">'
    + '<animateColor attributeName="fill" from="#000000" to="#00ff00"'
    + ' dur="1s"/></rect>');
  AssertEquals('animateColor interpolates the same way animate does',
    '#003300ff', At(0.2));
end;


procedure TTestSVGAnimation.TestAnimateColorReadsEveryValueAsAColour;

begin
  Build('<rect x="10" width="5" height="5">'
    + '<animateColor attributeName="x" from="10" to="50" dur="2s"/></rect>');
  AssertEquals('a value that is not a colour steps, whatever attribute it '
    + 'is written on', '10', At(0.5));
  AssertEquals('and steps at the half way point', '50', At(1.5));
end;


procedure TTestSVGAnimation.TestPacedSpacesTheValuesByDistance;

begin
  Build('<rect x="0" width="5" height="5">'
    + '<animate attributeName="x" values="0;10;30" dur="3s"'
    + ' calcMode="paced"/></rect>');
  AssertEquals('the second value is reached a third of the way in, where '
    + 'a third of the distance is covered', '10', At(1));
  AssertEquals('and the run to it is at that same speed', '5', At(0.5));
end;


procedure TTestSVGAnimation.TestPacedStandsInForTheKeyTimesGiven;

begin
  Build('<rect x="0" width="5" height="5">'
    + '<animate attributeName="x" values="0;10;30" keyTimes="0;0.9;1"'
    + ' dur="3s" calcMode="paced"/></rect>');
  AssertEquals('pacing by distance leaves the key times of the element '
    + 'unused', '10', At(1));
end;


procedure TTestSVGAnimation.TestPacedOfAValueWithoutDistanceSpacesEvenly;

begin
  Build('<rect width="5" height="5">'
    + '<animate attributeName="visibility" values="visible;hidden;visible"'
    + ' dur="3s" calcMode="paced"/></rect>');
  AssertEquals('a value with no distance to measure is spread evenly '
    + 'instead', 'hidden', At(1.5));
end;


procedure TTestSVGAnimation.TestPacedOfATransformMeasuresItsArguments;

begin
  Build('<rect width="5" height="5">'
    + '<animateTransform attributeName="transform" type="translate"'
    + ' values="0 0;3 4;3 14" dur="3s" calcMode="paced"/></rect>');
  AssertEquals('the distance of a transform is measured over its '
    + 'arguments, so five of fifteen are covered in a third of the time',
    'translate(3 4)', At(1));
end;


procedure TTestSVGAnimation.TestThePointsOfAShapeInterpolate;

begin
  Build('<polygon points="10,10 40,10 40,40 10,40">'
    + '<animate attributeName="points" dur="2s"'
    + ' values="10,10 40,10 40,40 10,40;10,50 40,50 40,80 10,80"/>'
    + '</polygon>');
  AssertEquals('half way through, every coordinate of the shape is half '
    + 'way', '10 30 40 30 40 60 10 60', At(1));
end;


procedure TTestSVGAnimation.TestTheDOfAPathInterpolates;

begin
  Build('<path d="M 10 10 L 20 20 Z">'
    + '<animate attributeName="d" dur="2s" from="M 10 10 L 20 20 Z"'
    + ' to="M 30 30 L 60 60 Z"/>'
    + '</path>');
  AssertEquals('half way through, every coordinate of the path is half way',
    'M 20 20 L 40 40 Z', At(1));
end;


procedure TTestSVGAnimation.TestTheDOfAPathAccumulates;

begin
  Build('<path d="M 0 0 L 10 10">'
    + '<animate attributeName="d" dur="2s" repeatCount="2"'
    + ' accumulate="sum" values="M 0 0 L 0 0;M 10 10 L 20 20"/>'
    + '</path>');
  AssertEquals('the second run starts from where the first ended',
    'M 10 10 L 20 20', At(2));
  AssertEquals('and every coordinate of it goes on from there',
    'M 15 15 L 30 30', At(3));
end;


procedure TTestSVGAnimation.TestCurrentColorIsTheColourOfTheTarget;

begin
  Build('<rect id="r" color="#00ff00" fill="#000000">'
    + '<animate attributeName="fill" from="#000000" to="currentColor"'
    + ' dur="2s"/>'
    + '</rect>');
  AssertEquals('half way to the colour the element states', '#008000ff',
    At(1));
end;


procedure TTestSVGAnimation.TestCurrentColorIsInheritedFromAnAncestor;

begin
  Build('<g color="#00ff00"><g><rect id="r" fill="#000000">'
    + '<animate attributeName="fill" from="currentColor" to="#000000"'
    + ' dur="2s"/>'
    + '</rect></g></g>');
  AssertEquals('the nearest ancestor stating a colour gives it', '#008000ff',
    At(1));
end;


procedure TTestSVGAnimation.TestCurrentColorFollowsAnAnimatedColour;

begin
  Build('<rect id="r" color="#000000" fill="#000000">'
    + '<animate attributeName="fill" to="currentColor" dur="4s"'
    + ' fill="freeze"/>'
    + '<animate attributeName="color" from="#000000" to="#00ff00"'
    + ' dur="4s" fill="freeze"/>'
    + '</rect>');
  FTimeline.Seek(4);
  AssertEquals('the colour reached the end of its own animation',
    '#00ff00', FDocument.ElementByID('r').Attributes['color']);
  AssertEquals('and the fill was given that colour, not the one the '
    + 'document was read with', '#00ff00ff',
    FDocument.ElementByID('r').Attributes['fill']);
end;


procedure TTestSVGAnimation.TestCurrentColorOfADocumentSettingNoneIsBlack;

begin
  Build('<rect id="r" fill="#ffffff">'
    + '<animate attributeName="fill" from="#ffffff" to="currentColor"'
    + ' dur="2s" fill="freeze"/>'
    + '</rect>');
  AssertEquals('a document stating no colour leaves the keyword at black',
    '#000000ff', At(2));
end;


procedure TTestSVGAnimation.TestAnXLinkAttributeNameDropsItsPrefix;

begin
  Build('<image id="i" x="0" y="0" width="10" height="10"'
    + ' xlink:href="one.png">'
    + '<set attributeName="xlink:href" to="two.png" begin="1s" dur="1s"/>'
    + '</image>');
  AssertEquals('the name is the one the reader kept the attribute under',
    'href', FTimeline.Animations[0].AttributeName);
  AssertEquals('the base is the href the document was read with', 'one.png',
    FTimeline.Animations[0].Base);
  FTimeline.Seek(1.5);
  AssertEquals('and the value reaches the attribute a renderer reads',
    'two.png', FDocument.ElementByID('i').Attributes['href']);
end;


procedure TTestSVGAnimation.TestTheNumberOfAFilterPrimitiveInterpolates;

begin
  Build('<filter id="f"><feDiffuseLighting surfaceScale="10">'
    + '<feSpotLight limitingConeAngle="0">'
    + '<animate attributeName="limitingConeAngle" from="0" to="50"'
    + ' dur="10s" fill="freeze"/>'
    + '</feSpotLight></feDiffuseLighting></filter>');
  AssertEquals('a fifth of the way through the cone is a fifth open', '10',
    At(2));
  AssertEquals('and half way through it is half open', '25', At(5));
end;


procedure TTestSVGAnimation.TestThePositionListOfATextInterpolates;

begin
  Build('<text x="10 20 30" y="50">abc'
    + '<animate attributeName="x" dur="2s" from="10 20 30"'
    + ' to="30 40 50"/>'
    + '</text>');
  AssertEquals('every position of the list is half way', '20 30 40', At(1));
end;


procedure TTestSVGAnimation.TestTheGlyphAnglesOfATextInterpolate;

begin
  Build('<text x="10" y="50" rotate="0 0 0">abc'
    + '<animate attributeName="rotate" dur="2s" from="0 0 0"'
    + ' to="10 20 30"/>'
    + '</text>');
  AssertEquals('every angle of the list is half way', '5 10 15', At(1));
end;


procedure TTestSVGAnimation.TestSplineWithAStraightCurveRunsAsLinear;

begin
  Build('<rect x="0" width="5" height="5">'
    + '<animate attributeName="x" values="0;100" dur="2s"'
    + ' calcMode="spline" keySplines="0 0 1 1"/></rect>');
  AssertEquals('the curve through the two corners eases nothing', 50.0,
    AtNumber(1), 1e-6);
end;


procedure TTestSVGAnimation.TestSplineReachesTheValueEarly;

var
  lValue: Double;

begin
  Build('<rect x="0" width="5" height="5">'
    + '<animate attributeName="x" values="0;100" dur="2s"'
    + ' calcMode="spline" keySplines="0 1 0 1"/></rect>');
  lValue := AtNumber(1);
  AssertTrue(Format('a curve rising at once is most of the way there half '
    + 'way through, and %s is not', [SVGFormatFloat(lValue)]),
    (lValue > 90) and (lValue < 100));
end;


procedure TTestSVGAnimation.TestSplineEndsAreTheValuesThemselves;

begin
  Build('<rect x="0" width="5" height="5">'
    + '<animate attributeName="x" values="0;100" dur="2s"'
    + ' calcMode="spline" keySplines="0 1 0 1" fill="freeze"/></rect>');
  AssertEquals('the start of an eased interval is its first value', '0',
    At(0));
  AssertEquals('and the end is its last', '100', At(2));
end;


procedure TTestSVGAnimation.TestSplineWithoutKeySplinesRunsAsLinear;

begin
  Build('<rect x="0" width="5" height="5">'
    + '<animate attributeName="x" values="0;100" dur="2s"'
    + ' calcMode="spline"/></rect>');
  AssertEquals('an eased mode with no curve to ease by runs straight',
    50.0, AtNumber(1), 1e-6);
end;


procedure TTestSVGAnimation.TestMotionAlongAPathWritesATranslate;

begin
  Build('<rect width="5" height="5">'
    + '<animateMotion path="M 0 0 L 100 0" dur="2s"/></rect>');
  AssertEquals('a motion writes where along the path it has reached',
    'translate(50 0)', At(1));
end;


procedure TTestSVGAnimation.TestMotionOverItsValuesInterpolatesThePoints;

begin
  Build('<rect width="5" height="5">'
    + '<animateMotion values="0,0;10,20" dur="2s" calcMode="linear"/>'
    + '</rect>');
  AssertEquals('a motion over points interpolates them', 'translate(5 10)',
    At(1));
end;


procedure TTestSVGAnimation.TestMotionFromAndTo;

begin
  Build('<rect width="5" height="5">'
    + '<animateMotion from="0,0" to="10,0" dur="2s"/></rect>');
  AssertEquals('a from and a to name the two ends of the move',
    'translate(5 0)', At(1));
end;


procedure TTestSVGAnimation.TestMotionByStartsFromNoOffset;

begin
  Build('<rect width="5" height="5">'
    + '<animateMotion by="10,0" dur="2s"/></rect>');
  AssertEquals('a by on its own moves from where the element stands',
    'translate(5 0)', At(1));
end;


procedure TTestSVGAnimation.TestMotionIsPacedUnlessItSaysOtherwise;

begin
  Build('<rect width="5" height="5">'
    + '<animateMotion values="0,0;0,40;0,120" dur="6s"/></rect>');
  AssertEquals('a motion without a calcMode is paced, so a third of the '
    + 'time covers a third of the way', 'translate(0 40)', At(2));
end;


procedure TTestSVGAnimation.TestMotionRotateAutoTurnsWithThePath;

begin
  Build('<rect width="5" height="5">'
    + '<animateMotion path="M 0 0 L 0 100" rotate="auto" dur="2s"/></rect>');
  AssertEquals('rotate=auto turns the element to the direction of the path',
    'translate(0 50) rotate(90)', At(1));
end;


procedure TTestSVGAnimation.TestMotionRotateAutoReverseAddsHalfATurn;

begin
  Build('<rect width="5" height="5">'
    + '<animateMotion path="M 0 0 L 0 100" rotate="auto-reverse"'
    + ' dur="2s"/></rect>');
  AssertEquals('rotate=auto-reverse turns it the other way about',
    'translate(0 50) rotate(270)', At(1));
end;


procedure TTestSVGAnimation.TestMotionRotateTakesAFixedAngle;

begin
  Build('<rect width="5" height="5">'
    + '<animateMotion path="M 0 0 L 0 100" rotate="45" dur="2s"/></rect>');
  AssertEquals('an angle turns the element by it the whole way along',
    'translate(0 50) rotate(45)', At(1));
end;


procedure TTestSVGAnimation.TestKeyPointsIndexThePathByLength;

begin
  Build('<rect width="5" height="5">'
    + '<animateMotion path="M 0 0 L 100 0" keyPoints="0;0.5;1"'
    + ' keyTimes="0;0.9;1" calcMode="linear" dur="10s"/></rect>');
  AssertEquals('a key point is a fraction of the length of the path',
    'translate(50 0)', At(9));
  AssertEquals('and the run to it takes the time its key time gives',
    'translate(25 0)', At(4.5));
end;


procedure TTestSVGAnimation.TestKeyPointsOverValuesMakeThemAPath;

begin
  Build('<rect width="5" height="5">'
    + '<animateMotion values="-150 0; 150 0" keyPoints="0;1"'
    + ' keyTimes="0;1" calcMode="linear" dur="4s"/></rect>');
  AssertEquals('with key points the values are the path they are measured '
    + 'along', 'translate(0 0)', At(2));
end;


procedure TTestSVGAnimation.TestAnMPathNamesThePathToRunAlong;

begin
  Build('<defs><path id="p" d="M 0 0 L 100 0"/></defs>'
    + '<rect width="5" height="5">'
    + '<animateMotion dur="2s"><mpath xlink:href="#p"/></animateMotion>'
    + '</rect>');
  AssertEquals('an mpath child names the path to run along',
    'translate(50 0)', At(1));
end;


procedure TTestSVGAnimation.TestAMotionWithoutAPathOrValuesIsDropped;

begin
  Build('<rect width="5" height="5">'
    + '<animateMotion dur="2s"/></rect>');
  AssertEquals('a motion that names nothing to run along is dropped', 0,
    FTimeline.Count);
end;


procedure TTestSVGAnimation.TestAMotionValueOfTheWrongLengthIsDropped;

begin
  Build('<rect width="5" height="5">'
    + '<animateMotion values="0,0,0;10,10" dur="2s"/></rect>');
  AssertEquals('a motion value that is not a pair of coordinates is not a '
    + 'place, so the animation is dropped', 0, FTimeline.Count);
end;


{ TTestSVGTimeline }

procedure TTestSVGTimeline.Build(const aBody: String);

begin
  FreeAndNil(FTimeline);
  FreeAndNil(FDocument);
  FDocument := ReadSVGString(Doc(aBody));
  FTimeline := TSVGTimeline.Create(FDocument);
end;


procedure TTestSVGTimeline.BuildAt(const aBody: String; aNow: TDateTime);

begin
  FreeAndNil(FTimeline);
  FreeAndNil(FDocument);
  FDocument := ReadSVGString(Doc(aBody));
  FTimeline := TSVGTimeline.Create(FDocument, aNow);
end;


procedure TTestSVGTimeline.TearDown;

begin
  FreeAndNil(FTimeline);
  FreeAndNil(FDocument);
  inherited TearDown;
end;


function TTestSVGTimeline.Attribute(const aID, aName: String): String;

var
  lElement: TSVGElement;

begin
  lElement := FDocument.ElementByID(aID);
  AssertNotNull(Format('the document holds an element with the id %s', [aID]),
    lElement);
  Result := lElement.Attributes[aName];
end;


function TTestSVGTimeline.Has(const aID, aName: String): Boolean;

var
  lElement: TSVGElement;

begin
  lElement := FDocument.ElementByID(aID);
  AssertNotNull(Format('the document holds an element with the id %s', [aID]),
    lElement);
  Result := lElement.HasAttribute(aName);
end;


procedure TTestSVGTimeline.TestSeekWritesTheAttribute;

begin
  Build('<rect id="r" x="10" width="5" height="5">'
    + '<animate attributeName="x" from="10" to="50" dur="2s"/></rect>');
  FTimeline.Seek(1);
  AssertEquals('a seek writes the value into the attribute', '30',
    Attribute('r', 'x'));
end;


procedure TTestSVGTimeline.TestResetPutsTheAttributeBack;

begin
  Build('<rect id="r" x="10" width="5" height="5">'
    + '<animate attributeName="x" from="10" to="50" dur="2s"/></rect>');
  FTimeline.Seek(1);
  FTimeline.Reset;
  AssertEquals('a reset writes back the value the file was read with', '10',
    Attribute('r', 'x'));
end;


procedure TTestSVGTimeline.TestSeekingBackwardsGivesTheSameValue;

begin
  Build('<rect id="r" x="10" width="5" height="5">'
    + '<animate attributeName="x" from="10" to="50" dur="2s"/></rect>');
  FTimeline.Seek(1.5);
  FTimeline.Seek(1);
  AssertEquals('seeking backwards gives what seeking forwards does', '30',
    Attribute('r', 'x'));
end;


procedure TTestSVGTimeline.TestSeekAddsAnAttributeTheDocumentHadNot;

begin
  Build('<rect id="r" x="10" width="5" height="5">'
    + '<set attributeName="fill" to="green" begin="1s"/></rect>');
  AssertFalse('the rectangle is read without a fill', Has('r', 'fill'));
  FTimeline.Seek(2);
  AssertEquals('a set adds the attribute the document had not', 'green',
    Attribute('r', 'fill'));
end;


procedure TTestSVGTimeline.TestResetRemovesAnAttributeTheDocumentHadNot;

begin
  Build('<rect id="r" x="10" width="5" height="5">'
    + '<set attributeName="fill" to="green" begin="1s"/></rect>');
  FTimeline.Seek(2);
  FTimeline.Reset;
  AssertFalse('a reset takes away an attribute the document had not',
    Has('r', 'fill'));
end;


procedure TTestSVGTimeline.TestSeekBeforeBeginRemovesItAgain;

begin
  Build('<rect id="r" x="10" width="5" height="5">'
    + '<set attributeName="fill" to="green" begin="1s"/></rect>');
  FTimeline.Seek(2);
  FTimeline.Seek(0);
  AssertFalse('seeking before the begin takes the attribute away again',
    Has('r', 'fill'));
end;


procedure TTestSVGTimeline.TestALaterAnimationOfTheSameAttributeWins;

begin
  Build('<rect id="r" x="10" width="5" height="5">'
    + '<animate attributeName="x" from="10" to="50" dur="4s"/>'
    + '<set attributeName="x" to="90" begin="1s"/></rect>');
  FTimeline.Seek(2);
  AssertEquals('the last animation running on an attribute gives its value',
    '90', Attribute('r', 'x'));
end;


procedure TTestSVGTimeline.TestAFrozenAnimationIsNotOverwritten;

begin
  Build('<rect id="r" x="10" width="5" height="5">'
    + '<animate attributeName="x" from="10" to="50" dur="1s" fill="freeze"/>'
    + '<set attributeName="x" to="90" begin="5s"/></rect>');
  FTimeline.Seek(2);
  AssertEquals('an animation that has not begun does not undo a frozen one',
    '50', Attribute('r', 'x'));
end;


procedure TTestSVGTimeline.TestAnHRefNamesTheTarget;

begin
  Build('<rect id="r" x="10" width="5" height="5"/>'
    + '<animate xlink:href="#r" attributeName="x" from="10" to="50"'
    + ' dur="2s"/>');
  FTimeline.Seek(1);
  AssertEquals('an animation writes the element its href names', '30',
    Attribute('r', 'x'));
end;


procedure TTestSVGTimeline.TestCountIsEveryAnimationInDocumentOrder;

begin
  Build('<rect id="r" x="10" width="5" height="5">'
    + '<animate attributeName="x" from="10" to="50" dur="2s"/>'
    + '<set attributeName="fill" to="green"/></rect>');
  AssertEquals('both animations are in the timeline', 2, FTimeline.Count);
  AssertEquals('the first is the one written first', 'x',
    FTimeline.Animations[0].AttributeName);
  AssertEquals('the second is the one written second', 'fill',
    FTimeline.Animations[1].AttributeName);
end;


procedure TTestSVGTimeline.TestADocumentWithoutAnimationIsNotAnimated;

begin
  Build('<rect id="r" x="10" width="5" height="5"/>');
  AssertFalse('a document declaring no animation is not animated',
    FTimeline.IsAnimated);
  AssertEquals('and its duration is nothing', 0.0, FTimeline.Duration, 1e-9);
end;


procedure TTestSVGTimeline.TestDurationIsWhereTheLastAnimationEnds;

begin
  Build('<rect id="r" x="10" width="5" height="5">'
    + '<animate attributeName="x" from="10" to="50" dur="2s"/>'
    + '<animate attributeName="width" from="5" to="9" dur="1s" begin="3s"/>'
    + '</rect>');
  AssertEquals('the duration is where the last animation ends', 4.0,
    FTimeline.Duration, 1e-9);
end;


procedure TTestSVGTimeline.TestDurationIsNegativeWhenOneNeverEnds;

begin
  Build('<rect id="r" x="10" width="5" height="5">'
    + '<animate attributeName="x" from="10" to="50" dur="2s"/>'
    + '<set attributeName="fill" to="green" begin="1s"/></rect>');
  AssertTrue('a document with an animation that never ends has no duration',
    FTimeline.Duration < 0);
end;


procedure TTestSVGTimeline.TestChangeTimesHoldTheStepsOfADiscreteAnimation;

var
  lTimes: TSVGDoubleArray;

begin
  Build('<rect id="r" x="10" width="5" height="5">'
    + '<animate attributeName="visibility" values="visible;hidden"'
    + ' dur="2s" calcMode="discrete"/></rect>');
  lTimes := FTimeline.ChangeTimes;
  AssertEquals('the begin, the step and the end are reported', 3,
    Length(lTimes));
  AssertEquals('the first is the begin', 0.0, lTimes[0], 1e-9);
  AssertEquals('the second is where the value steps', 1.0, lTimes[1], 1e-9);
  AssertEquals('the last is the end of the animation', 2.0, lTimes[2], 1e-9);
end;


procedure TTestSVGTimeline.TestChangeTimesOfAStillDocumentAreEmpty;

begin
  Build('<rect id="r" x="10" width="5" height="5"/>');
  AssertEquals('a still document has no change times', 0,
    Length(FTimeline.ChangeTimes));
end;


procedure TTestSVGTimeline.TestAnAdditiveTransformRunsOverTheBase;

begin
  Build('<rect id="r" width="5" height="5" transform="skewX(30)">'
    + '<animateTransform attributeName="transform" type="rotate" from="0"'
    + ' to="90" dur="2s" additive="sum" fill="freeze"/></rect>');
  FTimeline.Seek(2);
  AssertEquals('an additive transform is concatenated after the one the '
    + 'file was read with', 'skewX(30) rotate(90 0 0)',
    Attribute('r', 'transform'));
end;


procedure TTestSVGTimeline.TestATransformThatReplacesDropsTheBase;

begin
  Build('<rect id="r" width="5" height="5" transform="skewX(30)">'
    + '<animateTransform attributeName="transform" type="rotate" from="0"'
    + ' to="90" dur="2s" fill="freeze"/></rect>');
  FTimeline.Seek(2);
  AssertEquals('a transform that replaces writes its function alone',
    'rotate(90 0 0)', Attribute('r', 'transform'));
end;


procedure TTestSVGTimeline.TestTwoAdditiveTransformsRunInDocumentOrder;

begin
  Build('<rect id="r" width="5" height="5" transform="skewX(30)">'
    + '<animateTransform attributeName="transform" type="rotate" from="0"'
    + ' to="90" dur="2s" additive="sum" fill="freeze"/>'
    + '<animateTransform attributeName="transform" type="scale" from="1 1"'
    + ' to="2 2" dur="2s" additive="sum" fill="freeze"/></rect>');
  FTimeline.Seek(2);
  AssertEquals('both additive transforms are concatenated, in the order '
    + 'the file writes them', 'skewX(30) rotate(90 0 0) scale(2 2)',
    Attribute('r', 'transform'));
end;


procedure TTestSVGTimeline.TestAnAdditiveNumberAddsToTheBase;

begin
  Build('<rect id="r" x="10" width="5" height="5">'
    + '<animate attributeName="x" from="200" to="20" dur="2s"'
    + ' additive="sum" fill="freeze"/></rect>');
  FTimeline.Seek(0);
  AssertEquals('an additive number starts at the base plus the first value',
    '210', Attribute('r', 'x'));
  FTimeline.Seek(2);
  AssertEquals('and ends at the base plus the last', '30',
    Attribute('r', 'x'));
end;


procedure TTestSVGTimeline.TestAnAdditiveColourAddsToTheBase;

begin
  Build('<rect id="r" width="5" height="5" fill="#100000">'
    + '<animate attributeName="fill" from="#000000" to="#00ff00" dur="2s"'
    + ' additive="sum" fill="freeze"/></rect>');
  FTimeline.Seek(2);
  AssertEquals('the channels of the two colours are added',
    '#10ff00ff', Attribute('r', 'fill'));
end;


procedure TTestSVGTimeline.TestAdditiveOfAKindThatCannotAddReplaces;

begin
  Build('<rect id="r" width="5" height="5" visibility="visible">'
    + '<animate attributeName="visibility" from="visible" to="hidden"'
    + ' dur="2s" additive="sum" fill="freeze"/></rect>');
  FTimeline.Seek(2);
  AssertEquals('a value that cannot be added replaces what is under it',
    'hidden', Attribute('r', 'visibility'));
end;


procedure TTestSVGTimeline.TestASetTakesNeitherAdditiveNorAccumulate;

begin
  Build('<rect id="r" x="10" width="5" height="5">'
    + '<set attributeName="x" to="5" additive="sum" accumulate="sum"/>'
    + '</rect>');
  FTimeline.Seek(1);
  AssertEquals('a set writes the value it names, whatever it is written '
    + 'with', '5', Attribute('r', 'x'));
end;


procedure TTestSVGTimeline.TestAByTransformStartsAtRestAndAdds;

begin
  Build('<rect id="r" width="5" height="5" transform="translate(10 10)">'
    + '<animateTransform attributeName="transform" type="scale" by="1"'
    + ' dur="2s" fill="freeze"/></rect>');
  FTimeline.Seek(0);
  AssertEquals('a by animation of a transform starts from the function at '
    + 'rest, over the transform the file was read with',
    'translate(10 10) scale(0 0)', Attribute('r', 'transform'));
  FTimeline.Seek(2);
  AssertEquals('and runs to the value it names', 'translate(10 10) scale(1 1)',
    Attribute('r', 'transform'));
end;


procedure TTestSVGTimeline.TestASyncBaseTakesTheBeginOfAnother;

begin
  Build('<rect id="r" width="5" height="5">'
    + '<set id="first" attributeName="fill" to="red" begin="1s"/>'
    + '<set attributeName="stroke" to="blue" begin="first.begin + 2s"/>'
    + '</rect>');
  AssertEquals('the second begins two seconds after the first does', 3.0,
    FTimeline.Animations[1].StartsAt, 1e-9);
  FTimeline.Seek(2.9);
  AssertFalse('so it has not begun just before that', Has('r', 'stroke'));
  FTimeline.Seek(3);
  AssertEquals('and gives its value from then on', 'blue',
    Attribute('r', 'stroke'));
end;


procedure TTestSVGTimeline.TestASyncBaseTakesTheEndOfAnother;

begin
  Build('<rect id="r" width="5" height="5">'
    + '<set id="base" attributeName="fill" to="red" begin="7s" dur="1s"/>'
    + '<set attributeName="stroke" to="blue" begin="base.end - 2s"/>'
    + '</rect>');
  AssertEquals('the end of the first is at eight seconds, so two before it '
    + 'is six', 6.0, FTimeline.Animations[1].StartsAt, 1e-9);
end;


procedure TTestSVGTimeline.TestASyncBaseIsResolvedWhicheverOrderTheyAreWrittenIn;

begin
  Build('<rect id="r" width="5" height="5">'
    + '<set attributeName="stroke" to="blue" begin="base.end + 1s"/>'
    + '<set id="base" attributeName="fill" to="red" begin="1s" dur="2s"/>'
    + '</rect>');
  AssertEquals('an animation may name one written after it', 4.0,
    FTimeline.Animations[0].StartsAt, 1e-9);
end;


procedure TTestSVGTimeline.TestASyncBaseOfAnAnimationThatNeverEndsNeverBegins;

begin
  Build('<rect id="r" width="5" height="5">'
    + '<set id="base" attributeName="fill" to="red" begin="1s"/>'
    + '<set attributeName="stroke" to="blue" begin="base.end"/>'
    + '</rect>');
  AssertEquals('an animation waiting on an end that never comes is left '
    + 'out', 1, FTimeline.Count);
  AssertEquals('and the one it waited on is what is left', 'fill',
    FTimeline.Animations[0].AttributeName);
end;


procedure TTestSVGTimeline.TestASyncBaseNamingNothingNeverBegins;

begin
  Build('<rect id="r" width="5" height="5">'
    + '<set attributeName="fill" to="red" begin="ghost.begin"/></rect>');
  AssertEquals('an animation naming an id the document has not is left out',
    0, FTimeline.Count);
end;


procedure TTestSVGTimeline.TestACycleOfSyncBasesKeepsItsOwnTimes;

begin
  Build('<rect id="r" width="5" height="5">'
    + '<set id="a" attributeName="fill" to="red" begin="0s;b.end + 1s"'
    + ' dur="1s"/>'
    + '<set id="b" attributeName="stroke" to="blue" begin="a.end + 1s"'
    + ' dur="1s"/></rect>');
  AssertEquals('both are timed', 2, FTimeline.Count);
  AssertEquals('the first begins at the offset it gives of its own', 0.0,
    FTimeline.Animations[0].StartsAt, 1e-9);
  AssertEquals('the second a second after the first run of it ends', 2.0,
    FTimeline.Animations[1].StartsAt, 1e-9);
  AssertEquals('and the first runs again a second after that one ends',
    4.0, FTimeline.Animations[0].Begins[1], 1e-9);
end;


procedure TTestSVGTimeline.TestAnEndAttributeCutsTheIntervalShort;

begin
  Build('<rect id="r" x="0" width="5" height="5">'
    + '<animate attributeName="x" from="0" to="100" dur="10s" end="2s"'
    + ' fill="freeze"/></rect>');
  FTimeline.Seek(1);
  AssertEquals('the animation runs as it would without an end', '10',
    Attribute('r', 'x'));
  FTimeline.Seek(5);
  AssertEquals('and freezes where the end cut it off', '20',
    Attribute('r', 'x'));
  AssertEquals('the end of the interval is where the attribute says', 2.0,
    FTimeline.Duration, 1e-9);
end;


procedure TTestSVGTimeline.TestAnEndAfterTheDurationChangesNothing;

begin
  Build('<rect id="r" x="0" width="5" height="5">'
    + '<animate attributeName="x" from="0" to="100" dur="2s" end="10s"'
    + ' fill="freeze"/></rect>');
  FTimeline.Seek(5);
  AssertEquals('an end past the runs of the animation leaves them alone',
    '100', Attribute('r', 'x'));
end;


procedure TTestSVGTimeline.TestASyncBaseInAnEndAttribute;

begin
  Build('<rect id="r" x="0" width="5" height="5">'
    + '<set id="base" attributeName="fill" to="red" begin="1s" dur="1s"/>'
    + '<animate attributeName="x" from="0" to="100" dur="10s"'
    + ' end="base.end" fill="freeze"/></rect>');
  FTimeline.Seek(5);
  AssertEquals('the animation freezes where the end it names falls, at two '
    + 'seconds of ten', '20', Attribute('r', 'x'));
end;


procedure TTestSVGTimeline.TestSeveralBeginsRunTheAnimationAgain;

begin
  Build('<rect id="r" x="0" width="5" height="5">'
    + '<animate attributeName="x" from="0" to="10" dur="1s"'
    + ' begin="0s;5s"/></rect>');
  FTimeline.Seek(0.5);
  AssertEquals('the first run gives its value', '5', Attribute('r', 'x'));
  FTimeline.Seek(2);
  AssertEquals('between the two runs the base value stands', '0',
    Attribute('r', 'x'));
  FTimeline.Seek(5.5);
  AssertEquals('and the second run gives it again', '5',
    Attribute('r', 'x'));
  AssertEquals('the duration is where the last of them ends', 6.0,
    FTimeline.Duration, 1e-9);
end;


procedure TTestSVGTimeline.TestAnEndListPairsWithTheBegins;

begin
  Build('<rect id="r" x="0" width="5" height="5">'
    + '<animate attributeName="x" from="0" to="100" dur="10s"'
    + ' begin="0s;5s" end="2s;7s" fill="freeze"/></rect>');
  FTimeline.Seek(3);
  AssertEquals('the first end cuts the first run', '20',
    Attribute('r', 'x'));
  FTimeline.Seek(6);
  AssertEquals('the second run starts over at its begin', '10',
    Attribute('r', 'x'));
  FTimeline.Seek(9);
  AssertEquals('and the second end cuts it two seconds in', '20',
    Attribute('r', 'x'));
end;


procedure TTestSVGTimeline.TestChangeTimesHoldEveryBegin;

var
  lTimes: TSVGDoubleArray;

begin
  Build('<rect id="r" x="0" width="5" height="5">'
    + '<animate attributeName="x" from="0" to="10" dur="1s"'
    + ' begin="0s;5s"/></rect>');
  lTimes := FTimeline.ChangeTimes;
  AssertEquals('both begins and both ends are reported', 4, Length(lTimes));
  AssertEquals('the first begin', 0.0, lTimes[0], 1e-9);
  AssertEquals('the end of the first run', 1.0, lTimes[1], 1e-9);
  AssertEquals('the second begin', 5.0, lTimes[2], 1e-9);
  AssertEquals('and the end of the second', 6.0, lTimes[3], 1e-9);
end;


procedure TTestSVGTimeline.TestRepeatDurRunsTheAnimationForThatLong;

begin
  Build('<rect id="r" x="0" width="5" height="5">'
    + '<animate attributeName="x" from="0" to="10" dur="1s"'
    + ' repeatDur="2.5s" fill="freeze"/></rect>');
  FTimeline.Seek(1.5);
  AssertEquals('the runs go on for as long as the repeat duration', '5',
    Attribute('r', 'x'));
  FTimeline.Seek(4);
  AssertEquals('and freeze part way through the run it cuts off', '5',
    Attribute('r', 'x'));
  AssertEquals('the animation ends where the repeat duration does', 2.5,
    FTimeline.Duration, 1e-9);
end;


procedure TTestSVGTimeline.TestRepeatCountAndRepeatDurTakeTheShorter;

begin
  Build('<rect id="r" x="0" width="5" height="5">'
    + '<animate attributeName="x" from="0" to="10" dur="1s"'
    + ' repeatCount="5" repeatDur="2s"/></rect>');
  AssertEquals('the shorter of the two bounds the runs', 2.0,
    FTimeline.Duration, 1e-9);
end;


procedure TTestSVGTimeline.TestAnIndefiniteRepeatDurNeverEnds;

begin
  Build('<rect id="r" x="0" width="5" height="5">'
    + '<animate attributeName="x" from="0" to="10" dur="1s"'
    + ' repeatDur="indefinite"/></rect>');
  AssertTrue('runs that go on for ever never end',
    FTimeline.Duration < 0);
  FTimeline.Seek(100.5);
  AssertEquals('and the animation is still running much later', '5',
    Attribute('r', 'x'));
end;


procedure TTestSVGTimeline.TestMaxCutsTheActiveDuration;

begin
  Build('<rect id="r" x="0" width="5" height="5">'
    + '<animate attributeName="x" from="0" to="100" dur="10s" max="2s"'
    + ' fill="freeze"/></rect>');
  FTimeline.Seek(5);
  AssertEquals('a max cuts the interval where an end would', '20',
    Attribute('r', 'x'));
  AssertEquals('and the animation ends there', 2.0, FTimeline.Duration,
    1e-9);
end;


procedure TTestSVGTimeline.TestMinStretchesTheActiveDuration;

begin
  Build('<rect id="r" x="0" width="5" height="5">'
    + '<animate attributeName="x" from="0" to="10" dur="1s"'
    + ' min="3s"/></rect>');
  AssertEquals('a min holds the animation active for that long', 3.0,
    FTimeline.Duration, 1e-9);
  FTimeline.Seek(2);
  AssertEquals('past the run of it the last value is held', '10',
    Attribute('r', 'x'));
  FTimeline.Seek(3.5);
  AssertEquals('and once the interval is over the base value stands', '0',
    Attribute('r', 'x'));
end;


procedure TTestSVGTimeline.TestAMinAboveAMaxLeavesBothUnread;

begin
  Build('<rect id="r" x="0" width="5" height="5">'
    + '<animate attributeName="x" from="0" to="100" dur="10s" min="5s"'
    + ' max="2s"/></rect>');
  AssertEquals('a min above a max is as if neither were written', 10.0,
    FTimeline.Duration, 1e-9);
end;


procedure TTestSVGTimeline.TestRestartNeverKeepsTheFirstBeginOnly;

begin
  Build('<rect id="r" x="0" width="5" height="5">'
    + '<animate attributeName="x" from="0" to="10" dur="1s"'
    + ' begin="0s;5s" restart="never"/></rect>');
  AssertEquals('only the first begin is kept', 1,
    Length(FTimeline.Animations[0].Begins));
  FTimeline.Seek(5.5);
  AssertEquals('so the animation does not run again', '0',
    Attribute('r', 'x'));
end;


procedure TTestSVGTimeline.TestRestartWhenNotActiveDropsABeginInsideTheRun;

begin
  Build('<rect id="r" x="0" width="5" height="5">'
    + '<animate attributeName="x" from="0" to="10" dur="1s"'
    + ' begin="0s;0.5s;5s" restart="whenNotActive"/></rect>');
  AssertEquals('the begin falling due while it runs is dropped, and the '
    + 'one after the run is kept', 2,
    Length(FTimeline.Animations[0].Begins));
  AssertEquals('the one that is kept is the later of them', 5.0,
    FTimeline.Animations[0].Begins[1], 1e-9);
end;


procedure TTestSVGTimeline.TestRestartAlwaysTakesEveryBegin;

begin
  Build('<rect id="r" x="0" width="5" height="5">'
    + '<animate attributeName="x" from="0" to="10" dur="1s"'
    + ' begin="0s;0.5s;5s"/></rect>');
  AssertEquals('without a restart of its own every begin starts it over',
    3, Length(FTimeline.Animations[0].Begins));
  FTimeline.Seek(1);
  AssertEquals('so the run that began at a half is what is going at one',
    '5', Attribute('r', 'x'));
end;


procedure TTestSVGTimeline.TestASyncOnARunOfAnother;

begin
  Build('<rect id="r" width="5" height="5">'
    + '<animate id="base" attributeName="x" from="0" to="10" dur="1s"'
    + ' repeatCount="4"/>'
    + '<set attributeName="fill" to="green" begin="base.repeat(2)"/>'
    + '</rect>');
  AssertEquals('the third run of the first starts two seconds in', 2.0,
    FTimeline.Animations[1].StartsAt, 1e-9);
end;


procedure TTestSVGTimeline.TestASyncOnARunItNeverReachesNeverBegins;

begin
  Build('<rect id="r" width="5" height="5">'
    + '<animate id="base" attributeName="x" from="0" to="10" dur="1s"'
    + ' repeatCount="2"/>'
    + '<set attributeName="fill" to="green" begin="base.repeat(5)"/>'
    + '</rect>');
  AssertEquals('a run the animation never reaches never comes, so what '
    + 'waits on it is left out', 1, FTimeline.Count);
end;


procedure TTestSVGTimeline.TestAWallClockWithoutAClockToMeasureNeverBegins;

begin
  Build('<rect id="r" width="5" height="5">'
    + '<set attributeName="fill" to="green"'
    + ' begin="wallclock(2000-06-10T12:00:00Z)"/></rect>');
  AssertEquals('a timeline built without a wall clock has nothing to '
    + 'measure such a begin against', 0, FTimeline.Count);
end;


procedure TTestSVGTimeline.TestAWallClockAgainstTheClockGiven;

begin
  BuildAt('<rect id="r" width="5" height="5">'
    + '<set attributeName="fill" to="green"'
    + ' begin="wallclock(2000-06-10T12:00:00Z)"/></rect>',
    EncodeDate(2000, 6, 10) + EncodeTime(11, 59, 58, 0));
  AssertEquals('the begin falls where the moment it names does on the '
    + 'document clock', 2.0, FTimeline.Animations[0].StartsAt, 1e-6);
end;


procedure TTestSVGTimeline.TestAWallClockAlreadyPastAppliesFromTheStart;

begin
  BuildAt('<rect id="r" width="5" height="5">'
    + '<set attributeName="fill" to="green"'
    + ' begin="wallclock(2000-06-10T12:34:56Z)" dur="indefinite"/></rect>',
    EncodeDate(2020, 1, 1));
  AssertTrue('a moment long past begins before the document clock does',
    FTimeline.Animations[0].StartsAt < 0);
  FTimeline.Seek(0);
  AssertEquals('so the value stands from the first frame', 'green',
    Attribute('r', 'fill'));
end;


procedure TTestSVGTimeline.TestAMotionKeepsTheTransformUnderIt;

begin
  Build('<rect id="r" width="5" height="5" transform="skewX(30)">'
    + '<animateMotion path="M 0 0 L 100 0" dur="2s"/></rect>');
  FTimeline.Seek(1);
  AssertEquals('a motion is written before the transform the element was '
    + 'read with', 'translate(50 0) skewX(30)',
    Attribute('r', 'transform'));
end;


procedure TTestSVGTimeline.TestAMotionStandsOutsideATransformThatReplaces;

begin
  Build('<rect id="r" width="5" height="5" transform="rotate(-30)">'
    + '<animateMotion path="M 0 0 L 100 0" dur="2s"/>'
    + '<animateTransform attributeName="transform" type="rotate" from="-30"'
    + ' to="0" dur="2s"/></rect>');
  FTimeline.Seek(1);
  AssertEquals('an animation of the transform attribute replaces that '
    + 'attribute and not the motion outside it',
    'translate(50 0) rotate(-15 0 0)', Attribute('r', 'transform'));
end;


procedure TTestSVGTimeline.TestAMotionAndTwoTransformsOfOneElement;

begin
  Build('<text id="t" transform="rotate(-30)">It''s alive!'
    + '<animateMotion path="M 0 0 L 50 180" dur="6s" fill="freeze"/>'
    + '<animateTransform attributeName="transform" type="rotate" from="-30"'
    + ' to="0" dur="6s" fill="freeze"/>'
    + '<animateTransform attributeName="transform" type="scale" from="1"'
    + ' to="3" additive="sum" dur="6s" fill="freeze"/></text>');
  FTimeline.Seek(6);
  AssertEquals('the motion moves the element, the rotate replaces the '
    + 'transform it was read with, and the scale adds to that',
    'translate(50 180) rotate(0 0 0) scale(3 3)',
    Attribute('t', 'transform'));
end;


procedure TTestSVGTimeline.TestResetTakesAwayTheMotionTransform;

begin
  Build('<rect id="r" width="5" height="5">'
    + '<animateMotion path="M 0 0 L 100 0" dur="2s"/></rect>');
  FTimeline.Seek(1);
  AssertTrue('a motion gives the element a transform it had not',
    Has('r', 'transform'));
  FTimeline.Reset;
  AssertFalse('and a reset takes it away again', Has('r', 'transform'));
end;


procedure TTestSVGTimeline.TestAnAdditiveAnimationRunsOverAFrozenOne;

begin
  Build('<rect id="r" x="10" width="5" height="5">'
    + '<animate attributeName="x" from="0" to="20" dur="1s" fill="freeze"/>'
    + '<animate attributeName="x" from="0" to="5" dur="4s" additive="sum"/>'
    + '</rect>');
  FTimeline.Seek(2);
  AssertEquals('an additive animation adds to the frozen value under it '
    + 'rather than to the base', '22.5', Attribute('r', 'x'));
end;


{ TTestSVGAnimGoldens }

procedure TTestSVGAnimGoldens.SetUp;

begin
  inherited SetUp;
  FRenderer := TSVGRenderer.Create;
end;


procedure TTestSVGAnimGoldens.Load(const aFileName: String);

begin
  FreeAndNil(FTimeline);
  FreeAndNil(FDocument);
  FDocument := ReadSVGFile(DataDir + aFileName);
  FTimeline := TSVGTimeline.Create(FDocument);
end;


procedure TTestSVGAnimGoldens.TearDown;

begin
  FreeAndNil(FTrace);
  FreeAndNil(FTimeline);
  FreeAndNil(FDocument);
  FreeAndNil(FRenderer);
  inherited TearDown;
end;


procedure TTestSVGAnimGoldens.CheckAt(aSeconds: Double; const aName: String);

begin
  FreeAndNil(FTrace);
  FTrace := TSVGTraceBackend.Create;
  FTimeline.Seek(aSeconds);
  FRenderer.Render(FDocument, FTrace);
  AssertGolden(Self, aName, FTrace.Log);
end;


procedure TTestSVGAnimGoldens.TestGoldenAtZero;

begin
  Load('anim.svg');
  CheckAt(0, 'anim-zero');
end;


procedure TTestSVGAnimGoldens.TestGoldenAtHalfASecond;

begin
  Load('anim.svg');
  CheckAt(0.5, 'anim-half');
end;


procedure TTestSVGAnimGoldens.TestGoldenAtTwoSeconds;

begin
  Load('anim.svg');
  CheckAt(2, 'anim-two');
end;


procedure TTestSVGAnimGoldens.TestTransformGoldenAtZero;

begin
  Load('animtransform.svg');
  CheckAt(0, 'anim-transform-zero');
end;


procedure TTestSVGAnimGoldens.TestTransformGoldenAtHalfASecond;

begin
  Load('animtransform.svg');
  CheckAt(0.5, 'anim-transform-half');
end;


procedure TTestSVGAnimGoldens.TestTransformGoldenAtTwoSeconds;

begin
  Load('animtransform.svg');
  CheckAt(2, 'anim-transform-two');
end;


procedure TTestSVGAnimGoldens.TestMotionGoldenAtZero;

begin
  Load('animmotion.svg');
  CheckAt(0, 'anim-motion-zero');
end;


procedure TTestSVGAnimGoldens.TestMotionGoldenAtHalfASecond;

begin
  Load('animmotion.svg');
  CheckAt(0.5, 'anim-motion-half');
end;


procedure TTestSVGAnimGoldens.TestMotionGoldenAtTwoSeconds;

begin
  Load('animmotion.svg');
  CheckAt(2, 'anim-motion-two');
end;


procedure TTestSVGAnimGoldens.TestChainGoldenAtZero;

begin
  Load('animchain.svg');
  CheckAt(0, 'anim-chain-zero');
end;


procedure TTestSVGAnimGoldens.TestChainGoldenAtOneAndAHalfSeconds;

begin
  Load('animchain.svg');
  CheckAt(1.5, 'anim-chain-half');
end;


procedure TTestSVGAnimGoldens.TestChainGoldenAtTwoSeconds;

begin
  Load('animchain.svg');
  CheckAt(2, 'anim-chain-two');
end;


initialization
  RegisterTest('anim', TTestSVGClock);
  RegisterTest('anim', TTestSVGTimeEntry);
  RegisterTest('anim', TTestSVGInterpolation);
  RegisterTest('anim', TTestSVGAnimation);
  RegisterTest('anim', TTestSVGTimeline);
  RegisterTest('anim', TTestSVGAnimGoldens);
end.
