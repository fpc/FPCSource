{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Tests for the class-hygiene (AST) rules

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit utstRulesClasses;


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry,
  FpSonar.Types, FpSonar.Issues, FpSonar.RuleFramework,
  FpSonar.Rules.Classes, FpSonar.Rules.Structure, UtstFixtures;

type
  { AST-tier class-hygiene-rule position + registration tests. }
  TRulesClassesTest = class(TTestCase)
  private
    // Runs aRule over aFixture, collecting issues into aCollector.
    procedure RunRule(aRule: TRuleBase; const aFixture: string;
      const aCollector: TFpSonarIssueCollector); overload;
    // aWithhold runs the degraded pass: real-RTL chain, no unit paths.
    procedure RunRule(aRule: TRuleBase; const aFixture: string;
      aWithhold: boolean; const aCollector: TFpSonarIssueCollector); overload;
    // How often aRule fires on aSource, staged as its own fixture.
    function RuleCount(aRule: TRuleBase; aWithhold: boolean;
      const aSource: array of string): Integer;
    // Asserts aRule is silent on aSource while aSibling fires on it.
    procedure CheckSilentWithLiveSibling(aRule, aSibling: TRuleBase;
      const aId, aSiblingId: string; const aSource: array of string);
    function CountById(const aCollector: TFpSonarIssueCollector;
      const aId: string): Integer;
    function FirstById(const aCollector: TFpSonarIssueCollector;
      const aId: string): Integer;
    // Asserts the rule fires once at aDeclLine, column 1, with message args
    // [aArg]; and zero on the compliant fixture. Fixtures supplied inline and
    // materialised to a temp dir.
    procedure CheckClassRuleSrc(aRule, aCompliantRule: TRuleBase;
      const aId: string; aDeclLine: Integer; const aArg: string;
      const aNoncompliant, aCompliant: array of string);
    // Fresh, separately-owned instances of each rule.
    function NewVisibilityAscendingOrder: TRuleBase;
    function NewDeclarationsFollowVisibilityOrder: TRuleBase;
    function NewFieldsNotPublic: TRuleBase;
    function NewFileNotTooManyClasses: TRuleBase;
    function NewInterfaceNotEmpty: TRuleBase;
    function NewInterfaceUniqueGuid: TRuleBase;
    function NewConstructorInherited: TRuleBase;
    function NewDestructorInherited: TRuleBase;
    function NewTopLevelClassInheritsTObject: TRuleBase;
    function NewMethodHidesVirtualWithoutOverride: TRuleBase;
    function NewOverrideChangesDefaultParameterValue: TRuleBase;
    function NewAbstractMethodCalledDirectly: TRuleBase;
    function NewInstantiatesClassWithAbstractMethods: TRuleBase;
    function NewInterfaceWithoutGuidUsedDynamically: TRuleBase;
    function NewSupportsResultIgnored: TRuleBase;
    function NewClassHelperHidesAncestorMethod: TRuleBase;
    function NewAssignedOnNonReference: TRuleBase;
    function NewPublicFieldAndPropertyForSameStorage: TRuleBase;
    function NewPropertyAccessorVisibilityWiderThanProperty: TRuleBase;
    function NewPropertyGetterWithSideEffect: TRuleBase;
    function NewConstructorNotVirtualInPolymorphicHierarchy: TRuleBase;
    function NewInheritedCreateNotFirstStatement: TRuleBase;
    function NewInheritedDestroyNotLastStatement: TRuleBase;
    function NewComparingClassReferencesWithEquals: TRuleBase;
    function NewNilCheckViaAssigned: TRuleBase;
  published
    procedure VisibilityAscendingOrderPositions;
    procedure DeclarationsFollowVisibilityOrderPositions;
    procedure FieldsNotPublicPositions;
    procedure FileNotTooManyClassesPositions;
    procedure InterfaceNotEmptyPositions;
    procedure InterfaceUniqueGuidPositions;
    procedure InterfaceUniqueGuidDuplicateFlagsBoth;
    procedure ConstructorInheritedPositions;
    procedure DestructorInheritedPositions;
    procedure TopLevelClassInheritsTObjectPositions;
    procedure MethodHidesVirtualWithoutOverridePositions;
    procedure MethodHidesVirtualWithoutOverrideDegradesWithoutResolver;
    procedure MethodHidesVirtualWithoutOverrideSilentOnUnresolvedOperand;
    procedure MethodHidesVirtualWithoutOverrideFlagsDynamicAncestor;
    procedure MethodHidesVirtualWithoutOverrideAllowsDeclaredHides;
    procedure OverrideChangesDefaultParameterValuePositions;
    procedure OverrideChangesDefaultParameterValueDegradesWithoutResolver;
    procedure OverrideChangesDefaultParameterValueSilentOnUnresolvedOperand;
    procedure OverrideChangesDefaultParameterValueFlagsDroppedDefault;
    procedure OverrideChangesDefaultParameterValueFlagsAddedDefault;
    procedure OverrideChangesDefaultParameterValueAllowsEqualNumericDefault;
    procedure AbstractMethodCalledDirectlyPositions;
    procedure AbstractMethodCalledDirectlyDegradesOnParseFailure;
    procedure AbstractMethodCalledDirectlyFlagsBareInherited;
    procedure AbstractMethodCalledDirectlyFlagsEmbeddedInherited;
    procedure AbstractMethodCalledDirectlyStopsAtNearestAncestor;
    procedure AbstractMethodCalledDirectlySilentOnAmbiguousOverload;
    procedure AbstractMethodCalledDirectlySilentOnSplitOverload;
    procedure AbstractMethodCalledDirectlySilentOnNestedShadow;
    procedure InstantiatesClassWithAbstractMethodsPositions;
    procedure InstantiatesClassWithAbstractMethodsDegradesWithoutResolver;
    procedure InstantiatesClassWithAbstractMethodsSilentOnUnresolvedOperand;
    procedure InterfaceWithoutGuidUsedDynamicallyPositions;
    procedure InterfaceWithoutGuidUsedDynamicallyDegradesOnParseFailure;
    procedure InterfaceWithoutGuidUsedDynamicallyFlagsQueryCalls;
    procedure InterfaceWithoutGuidUsedDynamicallyAllowsCorbaInterface;
    procedure InterfaceWithoutGuidUsedDynamicallySilentOnForeignInterface;
    procedure InterfaceWithoutGuidUsedDynamicallySilentOnQualifiedTarget;
    procedure InterfaceWithoutGuidUsedDynamicallyDoesNotOverlapUniqueGuid;
    procedure SupportsResultIgnoredPositions;
    procedure SupportsResultIgnoredDegradesOnParseFailure;
    procedure ClassHelperHidesAncestorMethodPositions;
    procedure ClassHelperHidesAncestorMethodDegradesWithoutResolver;
    procedure ClassHelperHidesAncestorMethodSilentOnUnresolvedOperand;
    procedure ClassHelperHidesAncestorMethodFlagsInheritedName;
    procedure ClassHelperHidesAncestorMethodAllowsOverload;
    procedure ClassHelperHidesAncestorMethodFlagsSameUnitPrivate;
    procedure AssignedOnNonReferencePositions;
    procedure AssignedOnNonReferenceDegradesOnParseFailure;
    procedure AssignedOnNonReferenceFlagsValueKinds;
    procedure AssignedOnNonReferenceSilentOnUnknownType;
    procedure AssignedOnNonReferenceSilentOnNestedRoutineScope;
    procedure PublicFieldAndPropertyForSameStoragePositions;
    procedure PublicFieldAndPropertyForSameStorageDegradesWithoutResolver;
    procedure PublicFieldAndPropertyForSameStorageSilentOnUnresolvedOperand;
    procedure PublicFieldAndPropertyForSameStorageAllowsPrivateBackingField;
    procedure PublicFieldAndPropertyForSameStorageSilentOnRoutineAccessor;
    procedure PublicFieldAndPropertyForSameStorageDoesNotOverlapFieldsNotPublic;
    procedure PublicFieldAndPropertyForSameStorageFlagsPublishedField;
    procedure PublicFieldAndPropertyForSameStorageFlagsFieldWriteAccessor;
    procedure PublicFieldAndPropertyForSameStorageSilentOnRecord;
    procedure PropertyAccessorVisibilityWiderThanPropertyPositions;
    procedure PropertyAccessorVisibilityWiderThanPropertyDegradesWithoutResolver;
    procedure PropertyAccessorVisibilityWiderThanPropertySilentOnUnresolvedOperand;
    procedure PropertyAccessorVisibilityWiderThanPropertyFlagsWiderSetter;
    procedure PropertyAccessorVisibilityWiderThanPropertyAllowsEqualVisibility;
    procedure PropertyAccessorVisibilityWiderThanPropertySilentOnFieldAccessor;
    procedure PropertyAccessorVisibilityWiderThanPropertyAllowsPublishedAccessor;
    procedure PropertyGetterWithSideEffectPositions;
    procedure PropertyGetterWithSideEffectDegradesWithoutResolver;
    procedure PropertyGetterWithSideEffectSilentOnUnresolvedOperand;
    procedure PropertyGetterWithSideEffectAllowsSideEffectFreeGetter;
    procedure PropertyGetterWithSideEffectFlagsWriteThroughSelf;
    procedure PropertyGetterWithSideEffectAllowsLocalVariableWrite;
    procedure PropertyGetterWithSideEffectAllowsOtherInstanceWrite;
    procedure PropertyGetterWithSideEffectAllowsWithScopedWrite;
    procedure ConstructorNotVirtualInPolymorphicHierarchyPositions;
    procedure ConstructorNotVirtualInPolymorphicHierarchyDegradesWithoutResolver;
    procedure ConstructorNotVirtualInPolymorphicHierarchySilentOnUnresolvedOperand;
    procedure ConstructorNotVirtualInPolymorphicHierarchyAllowsVirtualConstructor;
    procedure ConstructorNotVirtualInPolymorphicHierarchySilentWithoutVirtualMethods;
    procedure ConstructorNotVirtualInPolymorphicHierarchySilentWithoutDescendant;
    procedure ConstructorNotVirtualInPolymorphicHierarchyAllowsOverloadedDescendant;
    procedure ConstructorNotVirtualInPolymorphicHierarchyDoesNotOverlapMethodHidesVirtualWithoutOverride;
    procedure InheritedCreateNotFirstStatementPositions;
    procedure InheritedCreateNotFirstStatementDegradesOnParseFailure;
    procedure InheritedCreateNotFirstStatementAllowsBareInheritedFirst;
    procedure InheritedCreateNotFirstStatementAllowsNamedAncestorConstructor;
    procedure InheritedCreateNotFirstStatementAllowsLeadingEmptyStatement;
    procedure InheritedCreateNotFirstStatementSilentWhenInheritedAbsent;
    procedure InheritedCreateNotFirstStatementIgnoresClassConstructor;
    procedure InheritedDestroyNotLastStatementPositions;
    procedure InheritedDestroyNotLastStatementDegradesOnParseFailure;
    procedure InheritedDestroyNotLastStatementAllowsBareInheritedLast;
    procedure InheritedDestroyNotLastStatementAllowsTrailingEmptyStatement;
    procedure InheritedDestroyNotLastStatementSilentWhenInheritedAbsent;
    procedure InheritedDestroyNotLastStatementIgnoresClassDestructor;
    procedure ComparingClassReferencesWithEqualsPositions;
    procedure ComparingClassReferencesWithEqualsDegradesWithoutResolver;
    procedure ComparingClassReferencesWithEqualsSilentOnUnresolvedOperand;
    procedure ComparingClassReferencesWithEqualsDegradesOnParseFailure;
    procedure ComparingClassReferencesWithEqualsReportsInequalityOperator;
    procedure ComparingClassReferencesWithEqualsReportsRelatedClassOperands;
    procedure ComparingClassReferencesWithEqualsAllowsNilComparison;
    procedure ComparingClassReferencesWithEqualsAllowsInterfaceOperands;
    procedure ComparingClassReferencesWithEqualsAllowsClassReferenceOperands;
    procedure ComparingClassReferencesWithEqualsAllowsNonClassOperands;
    procedure ComparingClassReferencesWithEqualsDoesNotOverlapNilCheckViaAssigned;
    procedure RulesSelfRegisterGlobally;
  end;


implementation

const
  cMode = 'OBJFPC';
  cVisibilityAscendingOrderId = 'VisibilityAscendingOrder';
  cDeclarationsFollowVisibilityOrderId = 'DeclarationsFollowVisibilityOrder';
  cFieldsNotPublicId = 'FieldsNotPublic';
  cFileNotTooManyClassesId = 'FileNotTooManyClasses';
  cInterfaceNotEmptyId = 'InterfaceNotEmpty';
  cInterfaceUniqueGuidId = 'InterfaceUniqueGuid';
  cConstructorInheritedId = 'ConstructorInherited';
  cDestructorInheritedId = 'DestructorInherited';
  cTopLevelClassInheritsTObjectId = 'TopLevelClassInheritsTObject';
  cMethodHidesVirtualWithoutOverrideId = 'MethodHidesVirtualWithoutOverride';
  cOverrideChangesDefaultParameterValueId =
    'OverrideChangesDefaultParameterValue';
  cAbstractMethodCalledDirectlyId = 'AbstractMethodCalledDirectly';
  cInstantiatesClassWithAbstractMethodsId =
    'InstantiatesClassWithAbstractMethods';
  cInterfaceWithoutGuidUsedDynamicallyId =
    'InterfaceWithoutGuidUsedDynamically';
  cSupportsResultIgnoredId = 'SupportsResultIgnored';
  cClassHelperHidesAncestorMethodId = 'ClassHelperHidesAncestorMethod';
  cAssignedOnNonReferenceId = 'AssignedOnNonReference';
  cPublicFieldAndPropertyForSameStorageId =
    'PublicFieldAndPropertyForSameStorage';
  cPropertyAccessorVisibilityWiderThanPropertyId =
    'PropertyAccessorVisibilityWiderThanProperty';
  cPropertyGetterWithSideEffectId = 'PropertyGetterWithSideEffect';
  cConstructorNotVirtualInPolymorphicHierarchyId =
    'ConstructorNotVirtualInPolymorphicHierarchy';
  cInheritedCreateNotFirstStatementId = 'InheritedCreateNotFirstStatement';
  cInheritedDestroyNotLastStatementId = 'InheritedDestroyNotLastStatement';
  cComparingClassReferencesWithEqualsId =
    'ComparingClassReferencesWithEquals';
  cNilCheckViaAssignedId = 'NilCheckViaAssigned';

  cDefines: array[0..3] of string = ('FPC', 'CPUX86_64', 'UNIX', 'LINUX');

  // Embedded class-hygiene-rule fixtures: line i+1 == [i].

  cVisibilityAscendingOrderNoncompliant: array[0..21] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TWidget = class',
    '  public',
    '    procedure DoPublic;',
    '  private',
    '    FName: string;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure TWidget.DoPublic;',
    '',
    'begin',
    'end;',
    '',
    'end.');

  cVisibilityAscendingOrderCompliant: array[0..21] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TWidget = class',
    '  private',
    '    FName: string;',
    '  public',
    '    procedure DoPublic;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure TWidget.DoPublic;',
    '',
    'begin',
    'end;',
    '',
    'end.');

  cDeclarationsFollowVisibilityOrderNoncompliant: array[0..15] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TWidget = class',
    '  private',
    '    property Name: string;',
    '    FName: string;',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  cDeclarationsFollowVisibilityOrderCompliant: array[0..22] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TWidget = class',
    '  private',
    '    FName: string;',
    '    procedure SetName(const aValue: string);',
    '    property Name: string read FName write SetName;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure TWidget.SetName(const aValue: string);',
    '',
    'begin',
    '  FName := aValue;',
    'end;',
    '',
    'end.');

  cFieldsNotPublicNoncompliant: array[0..14] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TWidget = class',
    '  public',
    '    FName: string;',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  cFieldsNotPublicCompliant: array[0..16] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TWidget = class',
    '  private',
    '    FName: string;',
    '  public',
    '    property Name: string read FName;',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  cFileNotTooManyClassesNoncompliant: array[0..16] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TC1 = class(TObject) end;',
    '  TC2 = class(TObject) end;',
    '  TC3 = class(TObject) end;',
    '  TC4 = class(TObject) end;',
    '  TC5 = class(TObject) end;',
    '  TC6 = class(TObject) end;',
    '',
    'implementation',
    '',
    'end.');

  cFileNotTooManyClassesCompliant: array[0..15] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TC1 = class(TObject) end;',
    '  TC2 = class(TObject) end;',
    '  TC3 = class(TObject) end;',
    '  TC4 = class(TObject) end;',
    '  TC5 = class(TObject) end;',
    '',
    'implementation',
    '',
    'end.');

  cInterfaceNotEmptyNoncompliant: array[0..12] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  IEmpty = interface',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  cInterfaceNotEmptyCompliant: array[0..13] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  IThing = interface',
    '    procedure DoIt;',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  cInterfaceUniqueGuidNoncompliant: array[0..13] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  IService = interface',
    '    procedure Run;',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  cInterfaceUniqueGuidCompliant: array[0..20] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  {$interfaces corba}',
    '  ICorba = interface',
    '    procedure A;',
    '  end;',
    '',
    '  {$interfaces com}',
    '  IComWithGuid = interface',
    '    [''{12345678-1234-1234-1234-123456789ABC}'']',
    '    procedure B;',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  cConstructorInheritedNoncompliant: array[0..19] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TWidget = class(TObject)',
    '  public',
    '    constructor Create;',
    '  end;',
    '',
    'implementation',
    '',
    'constructor TWidget.Create;',
    'begin',
    '  // no inherited call',
    'end;',
    '',
    'end.');

  cConstructorInheritedCompliant: array[0..25] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TWidget = class(TObject)',
    '  public',
    '    constructor Create;',
    '    class constructor InitClass;',
    '  end;',
    '',
    'implementation',
    '',
    'constructor TWidget.Create;',
    'begin',
    '  inherited Create;',
    'end;',
    '',
    'class constructor TWidget.InitClass;',
    'begin',
    '  // a class constructor never chains to inherited; must not be flagged',
    'end;',
    '',
    'end.');

  cDestructorInheritedNoncompliant: array[0..19] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TWidget = class(TObject)',
    '  public',
    '    destructor Destroy; override;',
    '  end;',
    '',
    'implementation',
    '',
    'destructor TWidget.Destroy;',
    'begin',
    '  // no inherited call',
    'end;',
    '',
    'end.');

  cDestructorInheritedCompliant: array[0..25] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TWidget = class(TObject)',
    '  public',
    '    destructor Destroy; override;',
    '    class destructor DoneClass;',
    '  end;',
    '',
    'implementation',
    '',
    'destructor TWidget.Destroy;',
    'begin',
    '  inherited Destroy;',
    'end;',
    '',
    'class destructor TWidget.DoneClass;',
    'begin',
    '  // a class destructor never chains to inherited; must not be flagged',
    'end;',
    '',
    'end.');

  cTopLevelClassInheritsTObjectNoncompliant: array[0..12] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TFoo = class',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  cTopLevelClassInheritsTObjectCompliant: array[0..16] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TBar = class(TObject)',
    '  public',
    '    type',
    '      TInner = class',
    '      end;',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  cInterfaceUniqueGuidDuplicate: array[0..19] of string = (
    'unit Duplicate;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  IAlpha = interface',
    '    [''{11111111-1111-1111-1111-111111111111}'']',
    '    procedure A;',
    '  end;',
    '',
    '  IBeta = interface',
    '    [''{11111111-1111-1111-1111-111111111111}'']',
    '    procedure B;',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  cMethodHidesVirtualNoncompliant: array[0..27] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TBase = class(TObject)',
    '  public',
    '    procedure Foo; virtual;',
    '  end;',
    '',
    '  TDerived = class(TBase)',
    '  public',
    '    procedure Foo;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure TBase.Foo;',
    'begin',
    'end;',
    '',
    'procedure TDerived.Foo;',
    'begin',
    'end;',
    '',
    'end.');

  cMethodHidesVirtualCompliant: array[0..27] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TBase = class(TObject)',
    '  public',
    '    procedure Foo; virtual;',
    '  end;',
    '',
    '  TDerived = class(TBase)',
    '  public',
    '    procedure Foo; override;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure TBase.Foo;',
    'begin',
    'end;',
    '',
    'procedure TDerived.Foo;',
    'begin',
    'end;',
    '',
    'end.');

  cMethodHidesVirtualDynamic: array[0..27] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TBase = class(TObject)',
    '  public',
    '    procedure Foo; dynamic;',
    '  end;',
    '',
    '  TDerived = class(TBase)',
    '  public',
    '    procedure Foo;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure TBase.Foo;',
    'begin',
    'end;',
    '',
    'procedure TDerived.Foo;',
    'begin',
    'end;',
    '',
    'end.');

  // Baz is the only undeclared hide; Foo and Bar spell their hide out.
  cMethodHidesVirtualDeclaredHides: array[0..47] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TBase = class(TObject)',
    '  public',
    '    procedure Foo; virtual;',
    '    procedure Bar; virtual;',
    '    procedure Baz; virtual;',
    '  end;',
    '',
    '  TDerived = class(TBase)',
    '  public',
    '    procedure Foo; reintroduce;',
    '    procedure Bar; overload;',
    '    procedure Baz;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure TBase.Foo;',
    'begin',
    'end;',
    '',
    'procedure TBase.Bar;',
    'begin',
    'end;',
    '',
    'procedure TBase.Baz;',
    'begin',
    'end;',
    '',
    'procedure TDerived.Foo;',
    'begin',
    'end;',
    '',
    'procedure TDerived.Bar;',
    'begin',
    'end;',
    '',
    'procedure TDerived.Baz;',
    'begin',
    'end;',
    '',
    'end.');

  // TFPList.Clear is declared plain in the Classes stub AND in the real RTL
  // (rtl/objpas/classes/classesh.inc:530).
  cMethodHidesVirtualUnresolved: array[0..33] of string = (
    'unit Unresolved;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses Classes;',
    '',
    'type',
    '  TFoo = class(TFPList)',
    '  public',
    '    procedure Clear;',
    '  end;',
    '',
    '  TAbstract = class(TObject)',
    '  public',
    '    procedure Run; virtual; abstract;',
    '  end;',
    '',
    '// Returns a fresh instance.',
    'function Make: TAbstract;',
    '',
    'implementation',
    '',
    'procedure TFoo.Clear;',
    'begin',
    'end;',
    '',
    'function Make: TAbstract;',
    'begin',
    '  Result := TAbstract.Create;',
    'end;',
    '',
    'end.');

  cOverrideDefaultParamNoncompliant: array[0..27] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TBase = class(TObject)',
    '  public',
    '    procedure P(aValue: Integer = 1); virtual;',
    '  end;',
    '',
    '  TDerived = class(TBase)',
    '  public',
    '    procedure P(aValue: Integer = 2); override;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure TBase.P(aValue: Integer);',
    'begin',
    'end;',
    '',
    'procedure TDerived.P(aValue: Integer);',
    'begin',
    'end;',
    '',
    'end.');

  cOverrideDefaultParamCompliant: array[0..27] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TBase = class(TObject)',
    '  public',
    '    procedure P(aValue: Integer = 1); virtual;',
    '  end;',
    '',
    '  TDerived = class(TBase)',
    '  public',
    '    procedure P(aValue: Integer = 1); override;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure TBase.P(aValue: Integer);',
    'begin',
    'end;',
    '',
    'procedure TDerived.P(aValue: Integer);',
    'begin',
    'end;',
    '',
    'end.');

  cOverrideDefaultParamDropped: array[0..27] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TBase = class(TObject)',
    '  public',
    '    procedure P(aValue: Integer = 1); virtual;',
    '  end;',
    '',
    '  TDerived = class(TBase)',
    '  public',
    '    procedure P(aValue: Integer); override;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure TBase.P(aValue: Integer);',
    'begin',
    'end;',
    '',
    'procedure TDerived.P(aValue: Integer);',
    'begin',
    'end;',
    '',
    'end.');

  cOverrideDefaultParamAdded: array[0..27] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TBase = class(TObject)',
    '  public',
    '    procedure P(aValue: Integer); virtual;',
    '  end;',
    '',
    '  TDerived = class(TBase)',
    '  public',
    '    procedure P(aValue: Integer = 2); override;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure TBase.P(aValue: Integer);',
    'begin',
    'end;',
    '',
    'procedure TDerived.P(aValue: Integer);',
    'begin',
    'end;',
    '',
    'end.');

  // The two defaults fold to different value kinds but the same number.
  cOverrideDefaultParamEquivalent: array[0..27] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TBase = class(TObject)',
    '  public',
    '    procedure P(aValue: Double = 1); virtual;',
    '  end;',
    '',
    '  TDerived = class(TBase)',
    '  public',
    '    procedure P(aValue: Double = 1.0); override;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure TBase.P(aValue: Double);',
    'begin',
    'end;',
    '',
    'procedure TDerived.P(aValue: Double);',
    'begin',
    'end;',
    '',
    'end.');

  cOverrideDefaultParamUnfoldable: array[0..40] of string = (
    'unit Unfoldable;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TBase = class(TObject)',
    '  public',
    '    procedure P(aClass: TClass = TObject); virtual;',
    '  end;',
    '',
    '  TDerived = class(TBase)',
    '  public',
    '    procedure P(aClass: TClass = TInterfacedObject); override;',
    '  end;',
    '',
    '  TAbstract = class(TObject)',
    '  public',
    '    procedure Run; virtual; abstract;',
    '  end;',
    '',
    '// Returns a fresh instance.',
    'function Make: TAbstract;',
    '',
    'implementation',
    '',
    'procedure TBase.P(aClass: TClass);',
    'begin',
    'end;',
    '',
    'procedure TDerived.P(aClass: TClass);',
    'begin',
    'end;',
    '',
    'function Make: TAbstract;',
    'begin',
    '  Result := TAbstract.Create;',
    'end;',
    '',
    'end.');

  cAbstractMethodCalledNoncompliant: array[0..24] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TBase = class(TObject)',
    '  public',
    '    procedure Foo; virtual; abstract;',
    '  end;',
    '',
    '  TDerived = class(TBase)',
    '  public',
    '    procedure Foo; override;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure TDerived.Foo;',
    'begin',
    '  inherited Foo;',
    'end;',
    '',
    'end.');

  cAbstractMethodCalledCompliant: array[0..28] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TBase = class(TObject)',
    '  public',
    '    procedure Foo; virtual;',
    '  end;',
    '',
    '  TDerived = class(TBase)',
    '  public',
    '    procedure Foo; override;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure TBase.Foo;',
    'begin',
    'end;',
    '',
    'procedure TDerived.Foo;',
    'begin',
    '  inherited Foo;',
    'end;',
    '',
    'end.');

  cAbstractMethodCalledBare: array[0..24] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TBase = class(TObject)',
    '  public',
    '    procedure Foo; virtual; abstract;',
    '  end;',
    '',
    '  TDerived = class(TBase)',
    '  public',
    '    procedure Foo; override;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure TDerived.Foo;',
    'begin',
    '  inherited;',
    'end;',
    '',
    'end.');

  cAbstractMethodCalledEmbedded: array[0..26] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TBase = class(TObject)',
    '  public',
    '    function Foo: Boolean; virtual; abstract;',
    '  end;',
    '',
    '  TDerived = class(TBase)',
    '  public',
    '    function Foo: Boolean; override;',
    '  end;',
    '',
    'implementation',
    '',
    'function TDerived.Foo: Boolean;',
    'begin',
    '  Result := False;',
    '  if inherited Foo then',
    '    Result := True;',
    'end;',
    '',
    'end.');

  // TLeaf.Foo binds to TMid.Foo, which has a body: the abstract TBase.Foo two
  // levels up is not what the call reaches.
  cAbstractMethodCalledNearestAncestor: array[0..33] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TBase = class(TObject)',
    '  public',
    '    procedure Foo; virtual; abstract;',
    '  end;',
    '',
    '  TMid = class(TBase)',
    '  public',
    '    procedure Foo; override;',
    '  end;',
    '',
    '  TLeaf = class(TMid)',
    '  public',
    '    procedure Foo; override;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure TMid.Foo;',
    'begin',
    'end;',
    '',
    'procedure TLeaf.Foo;',
    'begin',
    '  inherited Foo;',
    'end;',
    '',
    'end.');

  cAbstractMethodCalledAmbiguous: array[0..29] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TBase = class(TObject)',
    '  public',
    '    procedure Foo; virtual; abstract; overload;',
    '    procedure Foo(aValue: Integer); virtual; overload;',
    '  end;',
    '',
    '  TDerived = class(TBase)',
    '  public',
    '    procedure Foo(aValue: Integer); override;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure TBase.Foo(aValue: Integer);',
    'begin',
    'end;',
    '',
    'procedure TDerived.Foo(aValue: Integer);',
    'begin',
    '  inherited Foo(aValue);',
    'end;',
    '',
    'end.');

  // TLeaf's `inherited Foo` binds to the concrete TBase.Foo, not to the abstract
  // one-argument TMid.Foo; only TImpl.Bar is a real direct abstract call.
  cAbstractMethodCalledSplitOverload: array[0..53] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TBase = class(TObject)',
    '  public',
    '    procedure Foo; virtual; overload;',
    '  end;',
    '',
    '  TMid = class(TBase)',
    '  public',
    '    procedure Foo(aValue: Integer); virtual; abstract; overload;',
    '  end;',
    '',
    '  TLeaf = class(TMid)',
    '  public',
    '    procedure Foo(aValue: Integer); override;',
    '    procedure Run;',
    '  end;',
    '',
    '  TAbs = class(TObject)',
    '  public',
    '    procedure Bar; virtual; abstract;',
    '  end;',
    '',
    '  TImpl = class(TAbs)',
    '  public',
    '    procedure Bar; override;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure TBase.Foo;',
    'begin',
    'end;',
    '',
    'procedure TLeaf.Foo(aValue: Integer);',
    'begin',
    'end;',
    '',
    'procedure TLeaf.Run;',
    'begin',
    '  inherited Foo;',
    'end;',
    '',
    'procedure TImpl.Bar;',
    'begin',
    '  inherited Bar;',
    'end;',
    '',
    'end.');

  // TOuter.TBase shadows the top-level TBase by name only; TDerived ascends to
  // the top-level one, whose Foo has a body.
  cAbstractMethodCalledNestedShadow: array[0..52] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TOuter = class(TObject)',
    '  public',
    '    type',
    '      TBase = class(TObject)',
    '      public',
    '        procedure Foo; virtual; abstract;',
    '      end;',
    '  end;',
    '',
    '  TBase = class(TObject)',
    '  public',
    '    procedure Foo; virtual;',
    '  end;',
    '',
    '  TDerived = class(TBase)',
    '  public',
    '    procedure Foo; override;',
    '  end;',
    '',
    '  TAbs = class(TObject)',
    '  public',
    '    procedure Bar; virtual; abstract;',
    '  end;',
    '',
    '  TImpl = class(TAbs)',
    '  public',
    '    procedure Bar; override;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure TBase.Foo;',
    'begin',
    'end;',
    '',
    'procedure TDerived.Foo;',
    'begin',
    '  inherited Foo;',
    'end;',
    '',
    'procedure TImpl.Bar;',
    'begin',
    '  inherited Bar;',
    'end;',
    '',
    'end.');

  // cAbstractMethodCalledNoncompliant with an unclosed ancestor list on line 13:
  // the reported shape survives, only the parse does not.
  cAbstractMethodCalledUnparseable: array[0..24] of string = (
    'unit Broken;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TBase = class(TObject)',
    '  public',
    '    procedure Foo; virtual; abstract;',
    '  end;',
    '',
    '  TDerived = class(TBase',
    '  public',
    '    procedure Foo; override;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure TDerived.Foo;',
    'begin',
    '  inherited Foo;',
    'end;',
    '',
    'end.');

  cInstantiatesAbstractNoncompliant: array[0..22] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TAbstract = class(TObject)',
    '  public',
    '    procedure Run; virtual; abstract;',
    '  end;',
    '',
    '// Returns a fresh instance.',
    'function Make: TAbstract;',
    '',
    'implementation',
    '',
    'function Make: TAbstract;',
    'begin',
    '  Result := TAbstract.Create;',
    'end;',
    '',
    'end.');

  cInstantiatesAbstractCompliant: array[0..31] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TAbstract = class(TObject)',
    '  public',
    '    procedure Run; virtual; abstract;',
    '  end;',
    '',
    '  TConcrete = class(TAbstract)',
    '  public',
    '    procedure Run; override;',
    '  end;',
    '',
    '// Returns a fresh instance.',
    'function Make: TAbstract;',
    '',
    'implementation',
    '',
    'procedure TConcrete.Run;',
    'begin',
    'end;',
    '',
    'function Make: TAbstract;',
    'begin',
    '  Result := TConcrete.Create;',
    'end;',
    '',
    'end.');

  // The constructed class arrives through a class reference.
  cInstantiatesAbstractUnresolved: array[0..41] of string = (
    'unit Unresolved;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TAbstract = class(TObject)',
    '  public',
    '    procedure Run; virtual; abstract;',
    '  end;',
    '  TAbstractClass = class of TAbstract;',
    '',
    '  TBase = class(TObject)',
    '  public',
    '    procedure Foo; virtual;',
    '  end;',
    '',
    '  TDerived = class(TBase)',
    '  public',
    '    procedure Foo;',
    '  end;',
    '',
    '// Returns a fresh instance.',
    'function Make(aClass: TAbstractClass): TAbstract;',
    '',
    'implementation',
    '',
    'procedure TBase.Foo;',
    'begin',
    'end;',
    '',
    'procedure TDerived.Foo;',
    'begin',
    'end;',
    '',
    'function Make(aClass: TAbstractClass): TAbstract;',
    'begin',
    '  Result := aClass.Create;',
    'end;',
    '',
    'end.');

  // AST tier: the rule reads no resolver fact.
  cGuidlessInterfaceAsCast: array[0..23] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  IFoo = interface',
    '    // Runs the action.',
    '    procedure Run;',
    '  end;',
    '',
    '// Returns the item as IFoo.',
    'function AsFoo(aItem: TObject): IFoo;',
    '',
    'implementation',
    '',
    'function AsFoo(aItem: TObject): IFoo;',
    '',
    'begin',
    '  Result := aItem as IFoo;',
    'end;',
    '',
    'end.');

  cGuidlessInterfaceCompliant: array[0..24] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  IFoo = interface',
    '    [''{7C9A1E5B-3D2F-4A61-8B0C-5E7D9F1A2B34}'']',
    '    // Runs the action.',
    '    procedure Run;',
    '  end;',
    '',
    '// Returns the item as IFoo.',
    'function AsFoo(aItem: TObject): IFoo;',
    '',
    'implementation',
    '',
    'function AsFoo(aItem: TObject): IFoo;',
    '',
    'begin',
    '  Result := aItem as IFoo;',
    'end;',
    '',
    'end.');

  cGuidlessInterfaceCorba: array[0..24] of string = (
    'unit Corba;',
    '',
    '{$mode objfpc}{$H+}',
    '{$interfaces corba}',
    '',
    'interface',
    '',
    'type',
    '  IFoo = interface',
    '    // Runs the action.',
    '    procedure Run;',
    '  end;',
    '',
    '// Returns the item as IFoo.',
    'function AsFoo(aItem: TObject): IFoo;',
    '',
    'implementation',
    '',
    'function AsFoo(aItem: TObject): IFoo;',
    '',
    'begin',
    '  Result := aItem as IFoo;',
    'end;',
    '',
    'end.');

  cGuidlessInterfaceQueries: array[0..31] of string = (
    'unit Queries;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  IFoo = interface',
    '    // Runs the action.',
    '    procedure Run;',
    '  end;',
    '',
    '// Runs the action when the item exposes it.',
    'procedure RunIt(aItem: TObject);',
    '',
    'implementation',
    '',
    'uses',
    '  SysUtils;',
    '',
    'procedure RunIt(aItem: TObject);',
    '',
    'var',
    '  lFoo: IFoo;',
    '',
    'begin',
    '  if Supports(aItem, IFoo, lFoo) then',
    '    lFoo.Run;',
    '  aItem.QueryInterface(IFoo, lFoo);',
    'end;',
    '',
    'end.');

  // The cast target is written qualified, which rule 1 does not bind.
  cGuidlessInterfaceQualified: array[0..23] of string = (
    'unit Qualified;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  IFoo = interface',
    '    // Runs the action.',
    '    procedure Run;',
    '  end;',
    '',
    '// Returns the item as IFoo.',
    'function AsFoo(aItem: TObject): IFoo;',
    '',
    'implementation',
    '',
    'function AsFoo(aItem: TObject): IFoo;',
    '',
    'begin',
    '  Result := aItem as Qualified.IFoo;',
    'end;',
    '',
    'end.');

  // IInterface is declared by the RTL rather than here.
  cGuidlessInterfaceForeign: array[0..17] of string = (
    'unit Foreign;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    '// Returns the item as IInterface.',
    'function AsIface(aItem: TObject): IInterface;',
    '',
    'implementation',
    '',
    'function AsIface(aItem: TObject): IInterface;',
    '',
    'begin',
    '  Result := aItem as IInterface;',
    'end;',
    '',
    'end.');

  cGuidlessInterfaceDeclaredOnly: array[0..14] of string = (
    'unit DeclaredOnly;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  IFoo = interface',
    '    // Runs the action.',
    '    procedure Run;',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  cGuidlessInterfaceUnparseable: array[0..23] of string = (
    'unit Broken;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  IFoo = interface',
    '    // Runs the action.',
    '    procedure Run;',
    '  end;',
    '',
    '// Returns the item as IFoo.',
    'function AsFoo(: IFoo;',
    '',
    'implementation',
    '',
    'function AsFoo(aItem: TObject): IFoo;',
    '',
    'begin',
    '  Result := aItem as IFoo;',
    'end;',
    '',
    'end.');

  // AST tier: the rule reads no resolver fact.
  cSupportsIgnoredNoncompliant: array[0..31] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  IFoo = interface',
    '    [''{7C9A1E5B-3D2F-4A61-8B0C-5E7D9F1A2B34}'']',
    '    // Runs the action.',
    '    procedure Run;',
    '  end;',
    '',
    '// Runs the action when the item exposes it.',
    'procedure RunIt(aItem: TObject);',
    '',
    'implementation',
    '',
    'uses',
    '  SysUtils;',
    '',
    'procedure RunIt(aItem: TObject);',
    '',
    'var',
    '  lFoo: IFoo;',
    '',
    'begin',
    '  Supports(aItem, IFoo, lFoo);',
    '  lFoo.Run;',
    'end;',
    '',
    'end.');

  cSupportsIgnoredCompliant: array[0..35] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  IFoo = interface',
    '    [''{7C9A1E5B-3D2F-4A61-8B0C-5E7D9F1A2B34}'']',
    '    // Runs the action.',
    '    procedure Run;',
    '  end;',
    '',
    '// Runs the action when the item exposes it.',
    'procedure RunIt(aItem: TObject);',
    '',
    'implementation',
    '',
    'uses',
    '  SysUtils;',
    '',
    'procedure RunIt(aItem: TObject);',
    '',
    'var',
    '  lFoo: IFoo;',
    '  lOk: Boolean;',
    '',
    'begin',
    '  lOk := Supports(aItem, IFoo, lFoo);',
    '  if Supports(aItem, IFoo, lFoo) then',
    '    lFoo.Run;',
    '  if lOk then',
    '    lFoo.Run;',
    'end;',
    '',
    'end.');

  cSupportsIgnoredUnparseable: array[0..31] of string = (
    'unit Broken;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  IFoo = interface',
    '    [''{7C9A1E5B-3D2F-4A61-8B0C-5E7D9F1A2B34}'']',
    '    // Runs the action.',
    '    procedure Run;',
    '  end;',
    '',
    '// Runs the action when the item exposes it.',
    'procedure RunIt(: TObject;',
    '',
    'implementation',
    '',
    'uses',
    '  SysUtils;',
    '',
    'procedure RunIt(aItem: TObject);',
    '',
    'var',
    '  lFoo: IFoo;',
    '',
    'begin',
    '  Supports(aItem, IFoo, lFoo);',
    '  lFoo.Run;',
    'end;',
    '',
    'end.');

  cClassHelperHidesNoncompliant: array[0..31] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TBase = class(TObject)',
    '  public',
    '    // Runs the action.',
    '    procedure Run;',
    '  end;',
    '',
    '  TBaseHelper = class helper for TBase',
    '  public',
    '    // Runs the action differently.',
    '    procedure Run;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure TBase.Run;',
    '',
    'begin',
    'end;',
    '',
    'procedure TBaseHelper.Run;',
    '',
    'begin',
    'end;',
    '',
    'end.');

  cClassHelperHidesCompliant: array[0..31] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TBase = class(TObject)',
    '  public',
    '    // Runs the action.',
    '    procedure Run;',
    '  end;',
    '',
    '  TBaseHelper = class helper for TBase',
    '  public',
    '    // Describes the action.',
    '    procedure Describe;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure TBase.Run;',
    '',
    'begin',
    'end;',
    '',
    'procedure TBaseHelper.Describe;',
    '',
    'begin',
    'end;',
    '',
    'end.');

  // A private method of the extended type is hidden all the same when the
  // helper is declared in the same unit, where that method can be named.
  cClassHelperHidesPrivate: array[0..31] of string = (
    'unit SameUnitPrivate;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TBase = class(TObject)',
    '  private',
    '    // Runs the action.',
    '    procedure Run;',
    '  end;',
    '',
    '  TBaseHelper = class helper for TBase',
    '  public',
    '    // Runs the action differently.',
    '    procedure Run;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure TBase.Run;',
    '',
    'begin',
    'end;',
    '',
    'procedure TBaseHelper.Run;',
    '',
    'begin',
    'end;',
    '',
    'end.');

  cClassHelperHidesOverload: array[0..45] of string = (
    'unit Overloading;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TBase = class(TObject)',
    '  public',
    '    // Runs the action.',
    '    procedure Run;',
    '    // Describes the action.',
    '    procedure Describe; overload;',
    '  end;',
    '',
    '  TBaseHelper = class helper for TBase',
    '  public',
    '    // Describes the action a given number of times.',
    '    procedure Describe(aCount: Integer); overload;',
    '    // Runs the action differently.',
    '    procedure Run;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure TBase.Run;',
    '',
    'begin',
    'end;',
    '',
    'procedure TBase.Describe;',
    '',
    'begin',
    'end;',
    '',
    'procedure TBaseHelper.Describe(aCount: Integer);',
    '',
    'begin',
    'end;',
    '',
    'procedure TBaseHelper.Run;',
    '',
    'begin',
    'end;',
    '',
    'end.');

  cClassHelperHidesInherited: array[0..31] of string = (
    'unit Ancestor;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TBase = class(TObject)',
    '  public',
    '    // Runs the action.',
    '    procedure Run;',
    '  end;',
    '',
    '  TBaseHelper = class helper for TBase',
    '  public',
    '    // Releases the instance.',
    '    procedure Free;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure TBase.Run;',
    '',
    'begin',
    'end;',
    '',
    'procedure TBaseHelper.Free;',
    '',
    'begin',
    'end;',
    '',
    'end.');

  // A record helper shadowing a method of the record it extends is the same
  // defect shape, but the extended type carries no class chain to walk.
  cClassHelperHidesUnresolved: array[0..55] of string = (
    'unit Unresolved;',
    '',
    '{$mode objfpc}{$H+}',
    '{$modeswitch advancedrecords}',
    '',
    'interface',
    '',
    'type',
    '  TPoint2D = record',
    '    // Returns the sum of both coordinates.',
    '    function Sum: Integer;',
    '  end;',
    '',
    '  TPoint2DHelper = record helper for TPoint2D',
    '  public',
    '    // Returns the sum of both coordinates.',
    '    function Sum: Integer;',
    '  end;',
    '',
    '  TBase = class(TObject)',
    '  public',
    '    // Runs the action.',
    '    procedure Run; virtual;',
    '  end;',
    '',
    '  TDerived = class(TBase)',
    '  public',
    '    // Runs the action.',
    '    procedure Run;',
    '  end;',
    '',
    'implementation',
    '',
    'function TPoint2D.Sum: Integer;',
    '',
    'begin',
    '  Result := 0;',
    'end;',
    '',
    'function TPoint2DHelper.Sum: Integer;',
    '',
    'begin',
    '  Result := 1;',
    'end;',
    '',
    'procedure TBase.Run;',
    '',
    'begin',
    'end;',
    '',
    'procedure TDerived.Run;',
    '',
    'begin',
    'end;',
    '',
    'end.');

  // AST tier: the rule reads no resolver fact.
  cAssignedNonReferenceNoncompliant: array[0..26] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TPoint2D = record',
    '    Horizontal: Integer;',
    '    Vertical: Integer;',
    '  end;',
    '',
    '// Reports whether a point was produced.',
    'function HasPoint: Boolean;',
    '',
    'implementation',
    '',
    'function HasPoint: Boolean;',
    '',
    'var',
    '  lPoint: TPoint2D;',
    '',
    'begin',
    '  Result := Assigned(lPoint);',
    'end;',
    '',
    'end.');

  cAssignedNonReferenceCompliant: array[0..53] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TPoint2D = record',
    '    Horizontal: Integer;',
    '  end;',
    '  PPoint2D = ^TPoint2D;',
    '  TItem = class(TObject)',
    '  end;',
    '  TItemClass = class of TItem;',
    '  TItems = array of TItem;',
    '  THandler = procedure(aItem: TItem);',
    '  IFoo = interface',
    '    [''{7C9A1E5B-3D2F-4A61-8B0C-5E7D9F1A2B34}'']',
    '    // Runs the action.',
    '    procedure Run;',
    '  end;',
    '',
    '// Counts the operands that carry a value.',
    'function Present: Integer;',
    '',
    'implementation',
    '',
    'function Present: Integer;',
    '',
    'var',
    '  lPtr: PPoint2D;',
    '  lItem: TItem;',
    '  lClass: TItemClass;',
    '  lItems: TItems;',
    '  lHandler: THandler;',
    '  lFoo: IFoo;',
    '',
    'begin',
    '  Result := 0;',
    '  if Assigned(lPtr) then',
    '    Inc(Result);',
    '  if Assigned(lItem) then',
    '    Inc(Result);',
    '  if Assigned(lClass) then',
    '    Inc(Result);',
    '  if Assigned(lItems) then',
    '    Inc(Result);',
    '  if Assigned(lHandler) then',
    '    Inc(Result);',
    '  if Assigned(lFoo) then',
    '    Inc(Result);',
    'end;',
    '',
    'end.');

  cAssignedNonReferenceValueKinds: array[0..33] of string = (
    'unit ValueKinds;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TGrid = array[0..3] of Integer;',
    '  TColour = (clRed, clGreen, clBlue);',
    '  TDigit = 0..9;',
    '',
    '// Counts the operands that carry a value.',
    'function Present: Integer;',
    '',
    'implementation',
    '',
    'function Present: Integer;',
    '',
    'var',
    '  lGrid: TGrid;',
    '  lColour: TColour;',
    '  lDigit: TDigit;',
    '',
    'begin',
    '  Result := 0;',
    '  if Assigned(lGrid) then',
    '    Inc(Result);',
    '  if Assigned(lColour) then',
    '    Inc(Result);',
    '  if Assigned(lDigit) then',
    '    Inc(Result);',
    'end;',
    '',
    'end.');

  cAssignedNonReferenceNestedScope: array[0..37] of string = (
    'unit NestedScope;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TPoint2D = record',
    '    Horizontal: Integer;',
    '  end;',
    '',
    '  TPalette = class(TObject)',
    '  public',
    '    Current: TPoint2D;',
    '  end;',
    '',
    '// Reports whether the outer operand carries a value.',
    'function OuterHas: Boolean;',
    '',
    'implementation',
    '',
    'function OuterHas: Boolean;',
    '',
    'var',
    '  Current: TObject;',
    '',
    '  function Inner: Boolean;',
    '',
    '  begin',
    '    Result := Assigned(Current);',
    '  end;',
    '',
    'begin',
    '  Current := nil;',
    '  Result := Inner;',
    'end;',
    '',
    'end.');

  // An alias is not folded and Integer is declared by the RTL.
  cAssignedNonReferenceUnknown: array[0..31] of string = (
    'unit Unknowns;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TPoint2D = record',
    '    Horizontal: Integer;',
    '  end;',
    '  TAlias = TPoint2D;',
    '',
    '// Counts the operands that carry a value.',
    'function Present: Integer;',
    '',
    'implementation',
    '',
    'function Present: Integer;',
    '',
    'var',
    '  lAliased: TAlias;',
    '  lCount: Integer;',
    '',
    'begin',
    '  Result := 0;',
    '  if Assigned(lAliased) then',
    '    Inc(Result);',
    '  if Assigned(lCount) then',
    '    Inc(Result);',
    'end;',
    '',
    'end.');

  cAssignedNonReferenceUnparseable: array[0..26] of string = (
    'unit Broken;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TPoint2D = record',
    '    Horizontal: Integer;',
    '    Vertical: Integer;',
    '  end;',
    '',
    '// Reports whether a point was produced.',
    'function HasPoint(: Boolean;',
    '',
    'implementation',
    '',
    'function HasPoint: Boolean;',
    '',
    'var',
    '  lPoint: TPoint2D;',
    '',
    'begin',
    '  Result := Assigned(lPoint);',
    'end;',
    '',
    'end.');

  cPublicFieldPropertyNoncompliant: array[0..16] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TWidget = class(TObject)',
    '  public',
    '    FName: string;',
    '    // The widget name.',
    '    property Name: string read FName;',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  cPublicFieldPropertyCompliant: array[0..17] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TWidget = class(TObject)',
    '  private',
    '    FName: string;',
    '  public',
    '    // The widget name.',
    '    property Name: string read FName;',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  cPublicFieldPropertyRoutineAccessor: array[0..24] of string = (
    'unit RoutineAccessor;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TWidget = class(TObject)',
    '  public',
    '    FName: string;',
    '    // Returns the widget name.',
    '    function GetName: string;',
    '    // The widget name.',
    '    property Name: string read GetName;',
    '  end;',
    '',
    'implementation',
    '',
    'function TWidget.GetName: string;',
    '',
    'begin',
    '  Result := FName;',
    'end;',
    '',
    'end.');

  cPublicFieldPropertyPublishedField: array[0..16] of string = (
    'unit PublishedField;',
    '',
    '{$mode objfpc}{$H+}{$M+}',
    '',
    'interface',
    '',
    'type',
    '  TWidget = class(TObject)',
    '  published',
    '    FOwner: TObject;',
    '    // The widget owner.',
    '    property Owner: TObject read FOwner;',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  cPublicFieldPropertySetterField: array[0..25] of string = (
    'unit SetterField;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TWidget = class(TObject)',
    '  private',
    '    // Returns the size.',
    '    function GetSize: Integer;',
    '  public',
    '    FSize: Integer;',
    '    // The widget size.',
    '    property Size: Integer read GetSize write FSize;',
    '  end;',
    '',
    'implementation',
    '',
    'function TWidget.GetSize: Integer;',
    '',
    'begin',
    '  Result := FSize;',
    'end;',
    '',
    'end.');

  cPublicFieldPropertyRecord: array[0..16] of string = (
    'unit RecordStorage;',
    '',
    '{$mode objfpc}{$H+}{$modeswitch advancedrecords}',
    '',
    'interface',
    '',
    'type',
    '  TPoint2D = record',
    '  public',
    '    FX: Integer;',
    '    // The x coordinate.',
    '    property X: Integer read FX;',
    '  end;',
    '',
    'implementation',
    '',
    'end.');

  { The resolver binds a property accessor as the member list is read, so every
    accessor here is declared ahead of the property that names it. }
  cWiderAccessorNoncompliant: array[0..33] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TGadget = class(TObject)',
    '  public',
    '    // Returns the value.',
    '    function GetValue: Integer;',
    '    // Stores the value.',
    '    procedure SetValue(aValue: Integer);',
    '  private',
    '    FValue: Integer;',
    '    // The value.',
    '    property Value: Integer read GetValue write SetValue;',
    '  end;',
    '',
    'implementation',
    '',
    'function TGadget.GetValue: Integer;',
    '',
    'begin',
    '  Result := FValue;',
    'end;',
    '',
    'procedure TGadget.SetValue(aValue: Integer);',
    '',
    'begin',
    '  FValue := aValue;',
    'end;',
    '',
    'end.');

  cWiderAccessorCompliant: array[0..25] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TGadget = class(TObject)',
    '  private',
    '    FValue: Integer;',
    '    // Returns the value.',
    '    function GetValue: Integer;',
    '  public',
    '    // The value.',
    '    property Value: Integer read GetValue;',
    '  end;',
    '',
    'implementation',
    '',
    'function TGadget.GetValue: Integer;',
    '',
    'begin',
    '  Result := FValue;',
    'end;',
    '',
    'end.');

  cWiderAccessorSetter: array[0..25] of string = (
    'unit WiderSetter;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TDial = class(TObject)',
    '  public',
    '    // Stores the level.',
    '    procedure SetLevel(aValue: Integer);',
    '  protected',
    '    FLevel: Integer;',
    '    // The level.',
    '    property Level: Integer write SetLevel;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure TDial.SetLevel(aValue: Integer);',
    '',
    'begin',
    '  FLevel := aValue;',
    'end;',
    '',
    'end.');

  cWiderAccessorEqual: array[0..25] of string = (
    'unit EqualVisibility;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TGadget = class(TObject)',
    '  private',
    '    FValue: Integer;',
    '  public',
    '    // Returns the value.',
    '    function GetValue: Integer;',
    '    // The value.',
    '    property Value: Integer read GetValue;',
    '  end;',
    '',
    'implementation',
    '',
    'function TGadget.GetValue: Integer;',
    '',
    'begin',
    '  Result := FValue;',
    'end;',
    '',
    'end.');

  // The getter sits in the implicit section, which $M+ makes published.
  cWiderAccessorPublished: array[0..25] of string = (
    'unit PublishedAccessor;',
    '',
    '{$mode objfpc}{$H+}{$M+}',
    '',
    'interface',
    '',
    'type',
    '  TGadget = class(TObject)',
    '    // Returns the value.',
    '    function GetValue: Integer;',
    '  private',
    '    FValue: Integer;',
    '  public',
    '    // The value.',
    '    property Value: Integer read GetValue;',
    '  end;',
    '',
    'implementation',
    '',
    'function TGadget.GetValue: Integer;',
    '',
    'begin',
    '  Result := FValue;',
    'end;',
    '',
    'end.');

  cGetterSideEffectNoncompliant: array[0..29] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TGadget = class(TObject)',
    '  private',
    '    FCount: Integer;',
    '    FLast: Integer;',
    '    FValue: Integer;',
    '    // Returns the value.',
    '    function GetValue: Integer;',
    '  public',
    '    // The value.',
    '    property Value: Integer read GetValue;',
    '  end;',
    '',
    'implementation',
    '',
    'function TGadget.GetValue: Integer;',
    '',
    'begin',
    '  Inc(FCount);',
    '  FLast := FValue;',
    '  Result := FValue;',
    'end;',
    '',
    'end.');

  // The second property reads a field directly, so it has no body to judge.
  cGetterSideEffectCompliant: array[0..27] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TGadget = class(TObject)',
    '  private',
    '    FValue: Integer;',
    '    // Returns the value.',
    '    function GetValue: Integer;',
    '  public',
    '    // The value.',
    '    property Value: Integer read GetValue;',
    '    // The stored value.',
    '    property Raw: Integer read FValue;',
    '  end;',
    '',
    'implementation',
    '',
    'function TGadget.GetValue: Integer;',
    '',
    'begin',
    '  Result := FValue;',
    'end;',
    '',
    'end.');

  cGetterWritesThroughSelf: array[0..27] of string = (
    'unit WriteThroughSelf;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TGadget = class(TObject)',
    '  private',
    '    FCount: Integer;',
    '    FValue: Integer;',
    '    // Returns the value.',
    '    function GetValue: Integer;',
    '  public',
    '    // The value.',
    '    property Value: Integer read GetValue;',
    '  end;',
    '',
    'implementation',
    '',
    'function TGadget.GetValue: Integer;',
    '',
    'begin',
    '  Self.FCount := 0;',
    '  Result := FValue;',
    'end;',
    '',
    'end.');

  cGetterWritesLocal: array[0..30] of string = (
    'unit LocalWrite;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TGadget = class(TObject)',
    '  private',
    '    FValue: Integer;',
    '    // Returns the value, one higher.',
    '    function GetValue: Integer;',
    '  public',
    '    // The value.',
    '    property Value: Integer read GetValue;',
    '  end;',
    '',
    'implementation',
    '',
    'function TGadget.GetValue: Integer;',
    '',
    'var',
    '  lTotal: Integer;',
    '',
    'begin',
    '  lTotal := FValue;',
    '  Inc(lTotal);',
    '  Result := lTotal;',
    'end;',
    '',
    'end.');

  cGetterWritesOtherInstance: array[0..28] of string = (
    'unit OtherInstanceWrite;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TGadget = class(TObject)',
    '  private',
    '    FNext: TGadget;',
    '    FValue: Integer;',
    '    // Returns the value.',
    '    function GetValue: Integer;',
    '  public',
    '    // The value.',
    '    property Value: Integer read GetValue;',
    '  end;',
    '',
    'implementation',
    '',
    'function TGadget.GetValue: Integer;',
    '',
    'begin',
    '  if FNext <> nil then',
    '    FNext.FValue := 0;',
    '  Result := FValue;',
    'end;',
    '',
    'end.');

  cGetterWritesWithScoped: array[0..28] of string = (
    'unit WithScopedWrite;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TGadget = class(TObject)',
    '  private',
    '    FPeer: TGadget;',
    '    FValue: Integer;',
    '    // Returns the value.',
    '    function GetValue: Integer;',
    '  public',
    '    // The value.',
    '    property Value: Integer read GetValue;',
    '  end;',
    '',
    'implementation',
    '',
    'function TGadget.GetValue: Integer;',
    '',
    'begin',
    '  with FPeer do',
    '    FValue := 0;',
    '  Result := FValue;',
    'end;',
    '',
    'end.');

  { An interface property: its accessor is a routine the analysed module never
    implements, so no field, no wider section and no body is reachable behind
    it. The class pair keeps the resolver demonstrably live. }
  cPropertyAccessorUnresolved: array[0..39] of string = (
    'unit Unresolved;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  IStore = interface',
    '    [''{9C4D9C4E-1B2A-4C3D-9E5F-0A1B2C3D4E5F}'']',
    '    // Returns the stored value.',
    '    function GetValue: Integer;',
    '    // The stored value.',
    '    property Value: Integer read GetValue;',
    '  end;',
    '',
    '  TBase = class(TObject)',
    '  public',
    '    // Runs the action.',
    '    procedure Run; virtual;',
    '  end;',
    '',
    '  TDerived = class(TBase)',
    '  public',
    '    // Runs the action.',
    '    procedure Run;',
    '  end;',
    '',
    'implementation',
    '',
    'procedure TBase.Run;',
    '',
    'begin',
    'end;',
    '',
    'procedure TDerived.Run;',
    '',
    'begin',
    'end;',
    '',
    'end.');

  cHiddenConstructorNoncompliant: array[0..40] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TBase = class(TObject)',
    '  public',
    '    // Runs the action.',
    '    procedure Run; virtual;',
    '    // Creates the instance.',
    '    constructor Create;',
    '  end;',
    '',
    '  TDerived = class(TBase)',
    '  public',
    '    // Creates the instance.',
    '    constructor Create;',
    '  end;',
    '',
    'implementation',
    '',
    'constructor TBase.Create;',
    '',
    'begin',
    '  inherited Create;',
    'end;',
    '',
    'procedure TBase.Run;',
    '',
    'begin',
    'end;',
    '',
    'constructor TDerived.Create;',
    '',
    'begin',
    '  inherited Create;',
    'end;',
    '',
    'end.');

  cHiddenConstructorCompliant: array[0..40] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TBase = class(TObject)',
    '  public',
    '    // Runs the action.',
    '    procedure Run; virtual;',
    '    // Creates the instance.',
    '    constructor Create; virtual;',
    '  end;',
    '',
    '  TDerived = class(TBase)',
    '  public',
    '    // Creates the instance.',
    '    constructor Create; override;',
    '  end;',
    '',
    'implementation',
    '',
    'constructor TBase.Create;',
    '',
    'begin',
    '  inherited Create;',
    'end;',
    '',
    'procedure TBase.Run;',
    '',
    'begin',
    'end;',
    '',
    'constructor TDerived.Create;',
    '',
    'begin',
    '  inherited Create;',
    'end;',
    '',
    'end.');

  cHiddenConstructorNoVirtual: array[0..40] of string = (
    'unit Probe;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TBase = class(TObject)',
    '  public',
    '    // Runs the action.',
    '    procedure Run;',
    '    // Creates the instance.',
    '    constructor Create;',
    '  end;',
    '',
    '  TDerived = class(TBase)',
    '  public',
    '    // Creates the instance.',
    '    constructor Create;',
    '  end;',
    '',
    'implementation',
    '',
    'constructor TBase.Create;',
    '',
    'begin',
    '  inherited Create;',
    'end;',
    '',
    'procedure TBase.Run;',
    '',
    'begin',
    'end;',
    '',
    'constructor TDerived.Create;',
    '',
    'begin',
    '  inherited Create;',
    'end;',
    '',
    'end.');

  cHiddenConstructorNoDescendant: array[0..28] of string = (
    'unit Probe;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TBase = class(TObject)',
    '  public',
    '    // Runs the action.',
    '    procedure Run; virtual;',
    '    // Creates the instance.',
    '    constructor Create;',
    '  end;',
    '',
    'implementation',
    '',
    'constructor TBase.Create;',
    '',
    'begin',
    '  inherited Create;',
    'end;',
    '',
    'procedure TBase.Run;',
    '',
    'begin',
    'end;',
    '',
    'end.');

  cHiddenConstructorOverloadedDescendant: array[0..40] of string = (
    'unit Probe;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TBase = class(TObject)',
    '  public',
    '    // Runs the action.',
    '    procedure Run; virtual;',
    '    // Creates the instance.',
    '    constructor Create; overload;',
    '  end;',
    '',
    '  TDerived = class(TBase)',
    '  public',
    '    // Creates the instance from a size.',
    '    constructor Create(aSize: Integer); overload;',
    '  end;',
    '',
    'implementation',
    '',
    'constructor TBase.Create;',
    '',
    'begin',
    '  inherited Create;',
    'end;',
    '',
    'procedure TBase.Run;',
    '',
    'begin',
    'end;',
    '',
    'constructor TDerived.Create(aSize: Integer);',
    '',
    'begin',
    '  inherited Create;',
    'end;',
    '',
    'end.');

  cHiddenConstructorUnresolved: array[0..46] of string = (
    'unit Unresolved;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'uses',
    '  SysUtils;',
    '',
    'type',
    '  EStoreError = class(Exception)',
    '  public',
    '    // Creates the error.',
    '    constructor Create;',
    '  end;',
    '',
    '  TBase = class(TObject)',
    '  public',
    '    // Runs the action.',
    '    procedure Run; virtual;',
    '  end;',
    '',
    '  TDerived = class(TBase)',
    '  public',
    '    // Runs the action.',
    '    procedure Run;',
    '  end;',
    '',
    'implementation',
    '',
    'constructor EStoreError.Create;',
    '',
    'begin',
    '  inherited Create(''store'');',
    'end;',
    '',
    'procedure TBase.Run;',
    '',
    'begin',
    'end;',
    '',
    'procedure TDerived.Run;',
    '',
    'begin',
    'end;',
    '',
    'end.');

  cInheritedCreateNotFirstNoncompliant: array[0..24] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TWidget = class(TObject)',
    '  private',
    '    FSize: Integer;',
    '  public',
    '    // Creates the widget.',
    '    constructor Create;',
    '  end;',
    '',
    'implementation',
    '',
    'constructor TWidget.Create;',
    '',
    'begin',
    '  FSize := 1;',
    '  inherited Create;',
    'end;',
    '',
    'end.');

  cInheritedCreateNotFirstCompliant: array[0..24] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TWidget = class(TObject)',
    '  private',
    '    FSize: Integer;',
    '  public',
    '    // Creates the widget.',
    '    constructor Create;',
    '  end;',
    '',
    'implementation',
    '',
    'constructor TWidget.Create;',
    '',
    'begin',
    '  inherited Create;',
    '  FSize := 1;',
    'end;',
    '',
    'end.');

  cInheritedCreateBareFirst: array[0..24] of string = (
    'unit Probe;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TWidget = class(TObject)',
    '  private',
    '    FSize: Integer;',
    '  public',
    '    // Creates the widget.',
    '    constructor Create;',
    '  end;',
    '',
    'implementation',
    '',
    'constructor TWidget.Create;',
    '',
    'begin',
    '  inherited;',
    '  FSize := 1;',
    'end;',
    '',
    'end.');

  cInheritedCreateNamedAncestor: array[0..35] of string = (
    'unit Probe;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TBase = class(TObject)',
    '  public',
    '    // Creates the instance from a code.',
    '    constructor CreateFromCode(aCode: Integer);',
    '  end;',
    '',
    '  TWidget = class(TBase)',
    '  private',
    '    FSize: Integer;',
    '  public',
    '    // Creates the widget.',
    '    constructor Create;',
    '  end;',
    '',
    'implementation',
    '',
    'constructor TBase.CreateFromCode(aCode: Integer);',
    '',
    'begin',
    'end;',
    '',
    'constructor TWidget.Create;',
    '',
    'begin',
    '  inherited CreateFromCode(1);',
    '  FSize := 1;',
    'end;',
    '',
    'end.');

  cInheritedCreateLeadingEmpty: array[0..25] of string = (
    'unit Probe;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TWidget = class(TObject)',
    '  private',
    '    FSize: Integer;',
    '  public',
    '    // Creates the widget.',
    '    constructor Create;',
    '  end;',
    '',
    'implementation',
    '',
    'constructor TWidget.Create;',
    '',
    'begin',
    '  ;',
    '  inherited Create;',
    '  FSize := 1;',
    'end;',
    '',
    'end.');

  cInheritedCreateUnparseable: array[0..23] of string = (
    'unit Broken;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TWidget = class(TObject',
    '  private',
    '    FSize: Integer;',
    '  public',
    '    constructor Create;',
    '  end;',
    '',
    'implementation',
    '',
    'constructor TWidget.Create;',
    '',
    'begin',
    '  FSize := 1;',
    '  inherited Create;',
    'end;',
    '',
    'end.');

  cClassConstructorChains: array[0..34] of string = (
    'unit Probe;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TWidget = class(TObject)',
    '  public',
    '    // Prepares the class.',
    '    class constructor InitClass;',
    '    // Releases the class.',
    '    class destructor DoneClass;',
    '  end;',
    '',
    'var',
    '  GSize: Integer;',
    '',
    'implementation',
    '',
    'class constructor TWidget.InitClass;',
    '',
    'begin',
    '  GSize := 1;',
    '  inherited Create;',
    'end;',
    '',
    'class destructor TWidget.DoneClass;',
    '',
    'begin',
    '  inherited Destroy;',
    '  GSize := 0;',
    'end;',
    '',
    'end.');

  cInheritedDestroyNotLastNoncompliant: array[0..24] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TWidget = class(TObject)',
    '  private',
    '    FSize: Integer;',
    '  public',
    '    // Destroys the widget.',
    '    destructor Destroy; override;',
    '  end;',
    '',
    'implementation',
    '',
    'destructor TWidget.Destroy;',
    '',
    'begin',
    '  inherited Destroy;',
    '  FSize := 0;',
    'end;',
    '',
    'end.');

  cInheritedDestroyNotLastCompliant: array[0..24] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TWidget = class(TObject)',
    '  private',
    '    FSize: Integer;',
    '  public',
    '    // Destroys the widget.',
    '    destructor Destroy; override;',
    '  end;',
    '',
    'implementation',
    '',
    'destructor TWidget.Destroy;',
    '',
    'begin',
    '  FSize := 0;',
    '  inherited Destroy;',
    'end;',
    '',
    'end.');

  cInheritedDestroyBareLast: array[0..24] of string = (
    'unit Probe;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TWidget = class(TObject)',
    '  private',
    '    FSize: Integer;',
    '  public',
    '    // Destroys the widget.',
    '    destructor Destroy; override;',
    '  end;',
    '',
    'implementation',
    '',
    'destructor TWidget.Destroy;',
    '',
    'begin',
    '  FSize := 0;',
    '  inherited;',
    'end;',
    '',
    'end.');

  cInheritedDestroyTrailingEmpty: array[0..25] of string = (
    'unit Probe;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TWidget = class(TObject)',
    '  private',
    '    FSize: Integer;',
    '  public',
    '    // Destroys the widget.',
    '    destructor Destroy; override;',
    '  end;',
    '',
    'implementation',
    '',
    'destructor TWidget.Destroy;',
    '',
    'begin',
    '  FSize := 0;',
    '  inherited Destroy;',
    '  ;',
    'end;',
    '',
    'end.');

  cInheritedDestroyUnparseable: array[0..23] of string = (
    'unit Broken;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TWidget = class(TObject',
    '  private',
    '    FSize: Integer;',
    '  public',
    '    destructor Destroy; override;',
    '  end;',
    '',
    'implementation',
    '',
    'destructor TWidget.Destroy;',
    '',
    'begin',
    '  inherited Destroy;',
    '  FSize := 0;',
    'end;',
    '',
    'end.');

  cClassIdentityNoncompliant: array[0..28] of string = (
    'unit NonCompliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TWidget = class(TObject)',
    '  private',
    '    FSize: Integer;',
    '  public',
    '    // The widget size.',
    '    property Size: Integer read FSize;',
    '  end;',
    '',
    '// Reports whether the two widgets match.',
    'function Match(aLeft, aRight: TWidget): boolean;',
    '',
    'implementation',
    '',
    'function Match(aLeft, aRight: TWidget): boolean;',
    '',
    'begin',
    '  Result := False;',
    '  if aLeft = aRight then',
    '    Result := True;',
    'end;',
    '',
    'end.');

  cClassIdentityCompliant: array[0..26] of string = (
    'unit Compliant;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TWidget = class(TObject)',
    '  private',
    '    FSize: Integer;',
    '  public',
    '    // The widget size.',
    '    property Size: Integer read FSize;',
    '  end;',
    '',
    '// Reports whether the left widget is the smaller one.',
    'function Smaller(aLeft, aRight: TWidget): boolean;',
    '',
    'implementation',
    '',
    'function Smaller(aLeft, aRight: TWidget): boolean;',
    '',
    'begin',
    '  Result := aLeft.Size < aRight.Size;',
    'end;',
    '',
    'end.');

  cClassIdentityInequality: array[0..28] of string = (
    'unit Probe;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TWidget = class(TObject)',
    '  private',
    '    FSize: Integer;',
    '  public',
    '    // The widget size.',
    '    property Size: Integer read FSize;',
    '  end;',
    '',
    '// Reports whether the two widgets differ.',
    'function Differs(aLeft, aRight: TWidget): boolean;',
    '',
    'implementation',
    '',
    'function Differs(aLeft, aRight: TWidget): boolean;',
    '',
    'begin',
    '  Result := False;',
    '  if aLeft <> aRight then',
    '    Result := True;',
    'end;',
    '',
    'end.');

  cClassIdentityRelatedClasses: array[0..24] of string = (
    'unit Probe;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TBase = class(TObject)',
    '  end;',
    '',
    '  TDerived = class(TBase)',
    '  end;',
    '',
    '// Reports whether the two operands are the same object.',
    'function Match(aBase: TBase; aDerived: TDerived): boolean;',
    '',
    'implementation',
    '',
    'function Match(aBase: TBase; aDerived: TDerived): boolean;',
    '',
    'begin',
    '  Result := aBase = aDerived;',
    'end;',
    '',
    'end.');

  cClassIdentityNilComparison: array[0..25] of string = (
    'unit Probe;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TWidget = class(TObject)',
    '  end;',
    '',
    '// Reports whether the widget is present.',
    'function Present(aWidget: TWidget): boolean;',
    '',
    'implementation',
    '',
    'function Present(aWidget: TWidget): boolean;',
    '',
    'begin',
    '  Result := False;',
    '  if aWidget = nil then',
    '    Exit;',
    '  if aWidget <> nil then',
    '    Result := True;',
    'end;',
    '',
    'end.');

  cClassIdentityInterfaceOperands: array[0..24] of string = (
    'unit Probe;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  IStore = interface',
    '    [''{9C4D9C4E-1B2A-4C3D-9E5F-0A1B2C3D4E5F}'']',
    '    // Returns the stored value.',
    '    function GetValue: Integer;',
    '  end;',
    '',
    '// Reports whether the two stores are the same reference.',
    'function Match(const aLeft, aRight: IStore): boolean;',
    '',
    'implementation',
    '',
    'function Match(const aLeft, aRight: IStore): boolean;',
    '',
    'begin',
    '  Result := aLeft = aRight;',
    'end;',
    '',
    'end.');

  cClassIdentityClassReferenceOperands: array[0..22] of string = (
    'unit Probe;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TWidget = class(TObject)',
    '  end;',
    '  TWidgetClass = class of TWidget;',
    '',
    '// Reports whether the widget is exactly of the given class.',
    'function IsExactly(aWidget: TWidget; aClass: TWidgetClass): boolean;',
    '',
    'implementation',
    '',
    'function IsExactly(aWidget: TWidget; aClass: TWidgetClass): boolean;',
    '',
    'begin',
    '  Result := aWidget.ClassType = aClass;',
    'end;',
    '',
    'end.');

  cClassIdentityNonClassOperands: array[0..35] of string = (
    'unit Probe;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TPoint = record',
    '    X: Integer;',
    '  end;',
    '',
    '  TColor = (clRed, clBlue);',
    '',
    'operator = (const aLeft, aRight: TPoint): boolean;',
    '',
    '// Reports whether every operand pair matches.',
    'function AllMatch(const aFirst, aSecond: string; aLow, aHigh: Integer;',
    '  const aOne, aTwo: TPoint; aHue, aTint: TColor): boolean;',
    '',
    'implementation',
    '',
    'operator = (const aLeft, aRight: TPoint): boolean;',
    '',
    'begin',
    '  Result := aLeft.X = aRight.X;',
    'end;',
    '',
    'function AllMatch(const aFirst, aSecond: string; aLow, aHigh: Integer;',
    '  const aOne, aTwo: TPoint; aHue, aTint: TColor): boolean;',
    '',
    'begin',
    '  Result := (aFirst = aSecond) and (aLow = aHigh) and (aOne = aTwo)',
    '    and (aHue = aTint);',
    'end;',
    '',
    'end.');

  cClassIdentityUnresolvedOperand: array[0..40] of string = (
    'unit Probe;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TBase = class(TObject)',
    '  public',
    '    // Runs the action.',
    '    procedure Run; virtual;',
    '  end;',
    '',
    '  TDerived = class(TBase)',
    '  public',
    '    // Runs the action.',
    '    procedure Run;',
    '  end;',
    '',
    '// Reports whether the two values are the same.',
    'generic function Same<T>(const aLeft, aRight: T): boolean;',
    '',
    'implementation',
    '',
    'generic function Same<T>(const aLeft, aRight: T): boolean;',
    '',
    'begin',
    '  Result := aLeft = aRight;',
    'end;',
    '',
    'procedure TBase.Run;',
    '',
    'begin',
    'end;',
    '',
    'procedure TDerived.Run;',
    '',
    'begin',
    'end;',
    '',
    'end.');

  cClassIdentityUnparseable: array[0..21] of string = (
    'unit Broken;',
    '',
    '{$mode objfpc}{$H+}',
    '',
    'interface',
    '',
    'type',
    '  TWidget = class(TObject',
    '  end;',
    '',
    '// Reports whether the two widgets match.',
    'function Match(aLeft, aRight: TWidget): boolean;',
    '',
    'implementation',
    '',
    'function Match(aLeft, aRight: TWidget): boolean;',
    '',
    'begin',
    '  Result := aLeft = aRight;',
    'end;',
    '',
    'end.');

procedure TRulesClassesTest.RunRule(aRule: TRuleBase; const aFixture: string;
  const aCollector: TFpSonarIssueCollector);

var
  lReg: TRuleRegistry;
  lEngine: TFpSonarRuleEngine;

begin
  lReg := TRuleRegistry.Create;
  lEngine := TFpSonarRuleEngine.CreateWith(lReg);
  try
    lReg.Register(aRule);
    lEngine.Analyze(aFixture, cMode, cDefines, aCollector);
  finally
    lEngine.Free;
    lReg.Free;
  end;
end;


procedure TRulesClassesTest.RunRule(aRule: TRuleBase; const aFixture: string;
  aWithhold: boolean; const aCollector: TFpSonarIssueCollector);

var
  lReg: TRuleRegistry;
  lEngine: TFpSonarRuleEngine;

begin
  lReg := TRuleRegistry.Create;
  lEngine := TFpSonarRuleEngine.CreateWith(lReg);
  try
    lReg.Register(aRule);
    if aWithhold then
      lEngine.Analyze(aFixture, cMode, cDefines, [], [], True, SizeOf(Pointer),
        aCollector)
    else
      lEngine.Analyze(aFixture, cMode, cDefines, aCollector);
  finally
    lEngine.Free;
    lReg.Free;
  end;
end;


function TRulesClassesTest.RuleCount(aRule: TRuleBase; aWithhold: boolean;
  const aSource: array of string): Integer;

var
  lFix: TTempFixtures;
  lc: TFpSonarIssueCollector;
  lId: string;

begin
  // The registry RunRule builds owns and frees aRule, so read the id first.
  lId := aRule.Metadata.RuleId;
  lFix := TTempFixtures.Create;
  try
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(aRule, lFix.Add('probe.pas', aSource), aWithhold, lc);
      Result := CountById(lc, lId);
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesClassesTest.CheckSilentWithLiveSibling(aRule,
  aSibling: TRuleBase; const aId, aSiblingId: string;
  const aSource: array of string);

var
  lFix: TTempFixtures;
  lc: TFpSonarIssueCollector;
  lPath: string;

begin
  lFix := TTempFixtures.Create;
  try
    lPath := lFix.Add('operand.pas', aSource);
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(aRule, lPath, False, lc);
      AssertEquals('an unresolved fact is silent', 0, CountById(lc, aId));
    finally
      lc.Free;
    end;
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(aSibling, lPath, False, lc);
      AssertEquals('the resolver was live', 1, CountById(lc, aSiblingId));
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


function TRulesClassesTest.CountById(
  const aCollector: TFpSonarIssueCollector; const aId: string): Integer;

var
  i: Integer;

begin
  Result := 0;
  for i := 0 to aCollector.Count - 1 do
    if aCollector.Issues[i].RuleId = aId then
      Inc(Result);
end;


function TRulesClassesTest.FirstById(
  const aCollector: TFpSonarIssueCollector; const aId: string): Integer;

var
  i: Integer;

begin
  Result := -1;
  for i := 0 to aCollector.Count - 1 do
    if aCollector.Issues[i].RuleId = aId then
      begin
        Result := i;
        Exit;
      end;
end;


function TRulesClassesTest.NewVisibilityAscendingOrder: TRuleBase;

begin
  Result := TRuleVisibilityAscendingOrder.Create(TRuleMetadata.Make(
    cVisibilityAscendingOrderId, rtAst, rfAst, sevMinor, itCodeSmell, cfHigh,
    True, ''));
end;


function TRulesClassesTest.NewDeclarationsFollowVisibilityOrder: TRuleBase;

begin
  Result := TRuleDeclarationsFollowVisibilityOrder.Create(TRuleMetadata.Make(
    cDeclarationsFollowVisibilityOrderId, rtAst, rfAst, sevMinor, itCodeSmell,
    cfHigh, True, ''));
end;


function TRulesClassesTest.NewFieldsNotPublic: TRuleBase;

begin
  Result := TRuleFieldsNotPublic.Create(TRuleMetadata.Make(cFieldsNotPublicId, rtAst,
    rfAst, sevMajor, itCodeSmell, cfHigh, True, ''));
end;


function TRulesClassesTest.NewFileNotTooManyClasses: TRuleBase;

begin
  Result := TRuleFileNotTooManyClasses.Create(TRuleMetadata.Make(
    cFileNotTooManyClassesId, rtAst, rfAst, sevMinor, itCodeSmell, cfHigh,
    True, ''));
end;


function TRulesClassesTest.NewInterfaceNotEmpty: TRuleBase;

begin
  Result := TRuleInterfaceNotEmpty.Create(TRuleMetadata.Make(cInterfaceNotEmptyId,
    rtAst, rfAst, sevMinor, itCodeSmell, cfHigh, True, ''));
end;


function TRulesClassesTest.NewInterfaceUniqueGuid: TRuleBase;

begin
  Result := TRuleInterfaceUniqueGuid.Create(TRuleMetadata.Make(cInterfaceUniqueGuidId,
    rtAst, rfAst, sevMajor, itBug, cfHigh, True, ''));
end;


function TRulesClassesTest.NewConstructorInherited: TRuleBase;

begin
  Result := TRuleConstructorInherited.Create(TRuleMetadata.Make(
    cConstructorInheritedId, rtAst, rfAst, sevMajor, itBug, cfHigh, True, ''));
end;


function TRulesClassesTest.NewDestructorInherited: TRuleBase;

begin
  Result := TRuleDestructorInherited.Create(TRuleMetadata.Make(cDestructorInheritedId,
    rtAst, rfAst, sevMajor, itBug, cfHigh, True, ''));
end;


function TRulesClassesTest.NewTopLevelClassInheritsTObject: TRuleBase;

begin
  Result := TRuleTopLevelClassInheritsTObject.Create(TRuleMetadata.Make(
    cTopLevelClassInheritsTObjectId, rtAst, rfAst, sevMinor, itCodeSmell,
    cfHigh, True, ''));
end;


function TRulesClassesTest.NewMethodHidesVirtualWithoutOverride: TRuleBase;

begin
  Result := TRuleMethodHidesVirtualWithoutOverride.Create(TRuleMetadata.Make(
    cMethodHidesVirtualWithoutOverrideId, rtSem, rfResolver, sevMajor, itBug,
    cfHigh, True, ''));
end;


function TRulesClassesTest.NewOverrideChangesDefaultParameterValue: TRuleBase;

begin
  Result := TRuleOverrideChangesDefaultParameterValue.Create(TRuleMetadata.Make(
    cOverrideChangesDefaultParameterValueId, rtSem, rfResolver, sevMajor, itBug,
    cfHigh, True, ''));
end;


function TRulesClassesTest.NewAbstractMethodCalledDirectly: TRuleBase;

begin
  Result := TRuleAbstractMethodCalledDirectly.Create(TRuleMetadata.Make(
    cAbstractMethodCalledDirectlyId, rtAst, rfAst, sevMajor, itBug, cfMedium,
    True, ''));
end;


function TRulesClassesTest.NewInstantiatesClassWithAbstractMethods: TRuleBase;

begin
  Result := TRuleInstantiatesClassWithAbstractMethods.Create(TRuleMetadata.Make(
    cInstantiatesClassWithAbstractMethodsId, rtSem, rfResolver, sevMajor, itBug,
    cfHigh, True, ''));
end;


function TRulesClassesTest.NewInterfaceWithoutGuidUsedDynamically: TRuleBase;

begin
  Result := TRuleInterfaceWithoutGuidUsedDynamically.Create(TRuleMetadata.Make(
    cInterfaceWithoutGuidUsedDynamicallyId, rtAst, rfAst, sevMajor, itBug,
    cfMedium, True, ''));
end;


function TRulesClassesTest.NewSupportsResultIgnored: TRuleBase;

begin
  Result := TRuleSupportsResultIgnored.Create(TRuleMetadata.Make(
    cSupportsResultIgnoredId, rtAst, rfAst, sevMajor, itBug, cfMedium,
    True, ''));
end;


function TRulesClassesTest.NewClassHelperHidesAncestorMethod: TRuleBase;

begin
  Result := TRuleClassHelperHidesAncestorMethod.Create(TRuleMetadata.Make(
    cClassHelperHidesAncestorMethodId, rtSem, rfResolver, sevMajor,
    itCodeSmell, cfHigh, True, ''));
end;


function TRulesClassesTest.NewAssignedOnNonReference: TRuleBase;

begin
  Result := TRuleAssignedOnNonReference.Create(TRuleMetadata.Make(
    cAssignedOnNonReferenceId, rtAst, rfAst, sevMajor, itBug, cfMedium,
    True, ''));
end;


function TRulesClassesTest.NewPublicFieldAndPropertyForSameStorage: TRuleBase;

begin
  Result := TRulePublicFieldAndPropertyForSameStorage.Create(
    TRuleMetadata.Make(cPublicFieldAndPropertyForSameStorageId, rtSem,
    rfResolver, sevMajor, itCodeSmell, cfHigh, True, ''));
end;


function TRulesClassesTest.NewPropertyAccessorVisibilityWiderThanProperty: TRuleBase;

begin
  Result := TRulePropertyAccessorVisibilityWiderThanProperty.Create(
    TRuleMetadata.Make(cPropertyAccessorVisibilityWiderThanPropertyId, rtSem,
    rfResolver, sevMajor, itCodeSmell, cfHigh, True, ''));
end;


function TRulesClassesTest.NewPropertyGetterWithSideEffect: TRuleBase;

begin
  Result := TRulePropertyGetterWithSideEffect.Create(TRuleMetadata.Make(
    cPropertyGetterWithSideEffectId, rtSem, rfResolver, sevMajor, itCodeSmell,
    cfLow, True, ''));
end;


function TRulesClassesTest.NewConstructorNotVirtualInPolymorphicHierarchy: TRuleBase;

begin
  Result := TRuleConstructorNotVirtualInPolymorphicHierarchy.Create(
    TRuleMetadata.Make(cConstructorNotVirtualInPolymorphicHierarchyId, rtSem,
    rfResolver, sevMajor, itCodeSmell, cfMedium, True, ''));
end;


function TRulesClassesTest.NewInheritedCreateNotFirstStatement: TRuleBase;

begin
  Result := TRuleInheritedCreateNotFirstStatement.Create(TRuleMetadata.Make(
    cInheritedCreateNotFirstStatementId, rtAst, rfAst, sevMajor, itBug,
    cfMedium, True, ''));
end;


function TRulesClassesTest.NewInheritedDestroyNotLastStatement: TRuleBase;

begin
  Result := TRuleInheritedDestroyNotLastStatement.Create(TRuleMetadata.Make(
    cInheritedDestroyNotLastStatementId, rtAst, rfAst, sevMajor, itBug,
    cfMedium, True, ''));
end;


function TRulesClassesTest.NewComparingClassReferencesWithEquals: TRuleBase;

begin
  Result := TRuleComparingClassReferencesWithEquals.Create(TRuleMetadata.Make(
    cComparingClassReferencesWithEqualsId, rtSem, rfResolver, sevMinor,
    itCodeSmell, cfLow, True, ''));
end;


function TRulesClassesTest.NewNilCheckViaAssigned: TRuleBase;

begin
  Result := TRuleNilCheckViaAssigned.Create(TRuleMetadata.Make(
    cNilCheckViaAssignedId, rtAst, rfAst, sevMinor, itCodeSmell, cfHigh,
    True, ''));
end;


procedure TRulesClassesTest.CheckClassRuleSrc(aRule, aCompliantRule: TRuleBase;
  const aId: string; aDeclLine: Integer; const aArg: string;
  const aNoncompliant, aCompliant: array of string);

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  k: Integer;

begin
  lFix := TTempFixtures.Create;
  try
    // Noncompliant: one issue at the offending member's line, column 1,
    // carrying [aArg] as the single message arg.
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(aRule, lFix.Add('noncompliant.pas', aNoncompliant), lc);
      AssertEquals('one issue for ' + aId, 1, CountById(lc, aId));
      k := FirstById(lc, aId);
      AssertEquals('start line', aDeclLine, lc.Issues[k].StartLine);
      AssertEquals('start col', 1, lc.Issues[k].StartCol);
      AssertEquals('end line', aDeclLine, lc.Issues[k].EndLine);
      AssertEquals('end col', 1, lc.Issues[k].EndCol);
      AssertEquals('key is the dotted rule key', 'rule.' + aId + '.message',
        lc.Issues[k].MessageKey);
      AssertEquals('one message arg', 1, Length(lc.Issues[k].MessageArgs));
      AssertEquals('arg 0 is the offending name/keyword', aArg,
        lc.Issues[k].MessageArgs[0]);
    finally
      lc.Free;
    end;

    // Compliant: a conforming class => nothing flagged.
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(aCompliantRule, lFix.Add('compliant.pas', aCompliant), lc);
      AssertEquals('compliant => zero', 0, CountById(lc, aId));
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesClassesTest.VisibilityAscendingOrderPositions;

begin
  // Noncompliant: a 'private' section follows 'public'; the first member of the
  // out-of-order section (FName, line 12) is flagged, arg the section keyword.
  CheckClassRuleSrc(NewVisibilityAscendingOrder, NewVisibilityAscendingOrder,
    cVisibilityAscendingOrderId, 12, 'private',
    cVisibilityAscendingOrderNoncompliant, cVisibilityAscendingOrderCompliant);
end;


procedure TRulesClassesTest.DeclarationsFollowVisibilityOrderPositions;

begin
  // Noncompliant: a field (FName, line 11) follows a property in one section;
  // the out-of-phase member is flagged, arg its Name.
  CheckClassRuleSrc(NewDeclarationsFollowVisibilityOrder,
    NewDeclarationsFollowVisibilityOrder,
    cDeclarationsFollowVisibilityOrderId, 11, 'FName',
    cDeclarationsFollowVisibilityOrderNoncompliant, cDeclarationsFollowVisibilityOrderCompliant);
end;


procedure TRulesClassesTest.FieldsNotPublicPositions;

begin
  // Noncompliant: a public field (FName, line 10) is flagged, arg its Name.
  CheckClassRuleSrc(NewFieldsNotPublic, NewFieldsNotPublic, cFieldsNotPublicId, 10,
    'FName',
    cFieldsNotPublicNoncompliant, cFieldsNotPublicCompliant);
end;


procedure TRulesClassesTest.FileNotTooManyClassesPositions;

begin
  // Noncompliant: 6 classes (> 5) flagged once at the unit line (line 1),
  // arg the class count "6"; compliant (5 classes) flags nothing.
  CheckClassRuleSrc(NewFileNotTooManyClasses, NewFileNotTooManyClasses,
    cFileNotTooManyClassesId, 1, '6',
    cFileNotTooManyClassesNoncompliant, cFileNotTooManyClassesCompliant);
end;


procedure TRulesClassesTest.InterfaceNotEmptyPositions;

begin
  // Noncompliant: an empty interface (IEmpty, line 8) is flagged, arg its name.
  CheckClassRuleSrc(NewInterfaceNotEmpty, NewInterfaceNotEmpty,
    cInterfaceNotEmptyId, 8, 'IEmpty',
    cInterfaceNotEmptyNoncompliant, cInterfaceNotEmptyCompliant);
end;


procedure TRulesClassesTest.InterfaceUniqueGuidPositions;

begin
  // Noncompliant: a COM interface with no GUID (IService, line 8) is flagged;
  // compliant pairs a CORBA interface with no GUID (skipped) and a COM
  // interface with a distinct GUID — both yield nothing.
  CheckClassRuleSrc(NewInterfaceUniqueGuid, NewInterfaceUniqueGuid,
    cInterfaceUniqueGuidId, 8, 'IService',
    cInterfaceUniqueGuidNoncompliant, cInterfaceUniqueGuidCompliant);
end;


procedure TRulesClassesTest.InterfaceUniqueGuidDuplicateFlagsBoth;

var
  lc: TFpSonarIssueCollector;
  lFix: TTempFixtures;
  i, lAlpha, lBeta: Integer;

begin
  // The duplicate-GUID half of InterfaceUniqueGuid: two COM interfaces sharing
  // one GUID each emit one issue, in declaration order, arg = interface name.
  lAlpha := -1;
  lBeta := -1;
  lFix := TTempFixtures.Create;
  try
  lc := TFpSonarIssueCollector.Create;
  try
    RunRule(NewInterfaceUniqueGuid,
      lFix.Add('duplicate.pas', cInterfaceUniqueGuidDuplicate), lc);
    AssertEquals('two duplicate-GUID issues', 2,
      CountById(lc, cInterfaceUniqueGuidId));
    for i := 0 to lc.Count - 1 do
      if lc.Issues[i].RuleId = cInterfaceUniqueGuidId then
        begin
          if lc.Issues[i].MessageArgs[0] = 'IAlpha' then
            lAlpha := i
          else if lc.Issues[i].MessageArgs[0] = 'IBeta' then
            lBeta := i;
        end;
    AssertTrue('IAlpha flagged', lAlpha >= 0);
    AssertTrue('IBeta flagged', lBeta >= 0);
    AssertEquals('IAlpha at its declaration line', 8,
      lc.Issues[lAlpha].StartLine);
    AssertEquals('IAlpha col 1', 1, lc.Issues[lAlpha].StartCol);
    AssertEquals('IBeta at its declaration line', 13,
      lc.Issues[lBeta].StartLine);
    AssertEquals('IBeta col 1', 1, lc.Issues[lBeta].StartCol);
  finally
    lc.Free;
  end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesClassesTest.ConstructorInheritedPositions;

begin
  // Noncompliant: a constructor implementation (line 15) with no inherited call
  // is flagged, arg its qualified name; compliant calls inherited (and its
  // class constructor is excluded) => nothing.
  CheckClassRuleSrc(NewConstructorInherited, NewConstructorInherited,
    cConstructorInheritedId, 15, 'TWidget.Create',
    cConstructorInheritedNoncompliant, cConstructorInheritedCompliant);
end;


procedure TRulesClassesTest.DestructorInheritedPositions;

begin
  // Noncompliant: a destructor implementation (line 15) with no inherited call
  // is flagged; compliant calls inherited (and its class destructor is
  // excluded) => nothing.
  CheckClassRuleSrc(NewDestructorInherited, NewDestructorInherited,
    cDestructorInheritedId, 15, 'TWidget.Destroy',
    cDestructorInheritedNoncompliant, cDestructorInheritedCompliant);
end;


procedure TRulesClassesTest.TopLevelClassInheritsTObjectPositions;

begin
  // Noncompliant: a top-level ancestor-less class (TFoo = class, line 8) is
  // flagged; compliant has an explicit ancestor and a nested ancestor-less
  // class (not top-level) => nothing.
  CheckClassRuleSrc(NewTopLevelClassInheritsTObject,
    NewTopLevelClassInheritsTObject, cTopLevelClassInheritsTObjectId, 8, 'TFoo',
    cTopLevelClassInheritsTObjectNoncompliant, cTopLevelClassInheritsTObjectCompliant);
end;


procedure TRulesClassesTest.MethodHidesVirtualWithoutOverridePositions;

begin
  // Noncompliant: TDerived.Foo (line 15) repeats a virtual ancestor signature
  // with no modifier; compliant declares it override.
  CheckClassRuleSrc(NewMethodHidesVirtualWithoutOverride,
    NewMethodHidesVirtualWithoutOverride,
    cMethodHidesVirtualWithoutOverrideId, 15, 'Foo',
    cMethodHidesVirtualNoncompliant, cMethodHidesVirtualCompliant);
end;


procedure TRulesClassesTest.MethodHidesVirtualWithoutOverrideDegradesWithoutResolver;

begin
  AssertEquals('withheld resolution => silent', 0,
    RuleCount(NewMethodHidesVirtualWithoutOverride, True,
    cMethodHidesVirtualNoncompliant));
end;


procedure TRulesClassesTest.MethodHidesVirtualWithoutOverrideSilentOnUnresolvedOperand;

begin
  // TFoo.Clear repeats the signature of an ancestor method declared outside the
  // analysed file; the resolver reports it plain.
  CheckSilentWithLiveSibling(NewMethodHidesVirtualWithoutOverride,
    NewInstantiatesClassWithAbstractMethods,
    cMethodHidesVirtualWithoutOverrideId,
    cInstantiatesClassWithAbstractMethodsId, cMethodHidesVirtualUnresolved);
end;


procedure TRulesClassesTest.MethodHidesVirtualWithoutOverrideFlagsDynamicAncestor;

begin
  // A dynamic ancestor method is virtual through the DMT.
  CheckClassRuleSrc(NewMethodHidesVirtualWithoutOverride,
    NewMethodHidesVirtualWithoutOverride,
    cMethodHidesVirtualWithoutOverrideId, 15, 'Foo',
    cMethodHidesVirtualDynamic, cMethodHidesVirtualCompliant);
end;


procedure TRulesClassesTest.MethodHidesVirtualWithoutOverrideAllowsDeclaredHides;

begin
  // reintroduce and overload declare the hide.
  CheckClassRuleSrc(NewMethodHidesVirtualWithoutOverride,
    NewMethodHidesVirtualWithoutOverride,
    cMethodHidesVirtualWithoutOverrideId, 19, 'Baz',
    cMethodHidesVirtualDeclaredHides, cMethodHidesVirtualCompliant);
end;


procedure TRulesClassesTest.OverrideChangesDefaultParameterValuePositions;

begin
  // Noncompliant: TDerived.P (line 15) overrides with = 2 over an inherited
  // = 1; compliant repeats the inherited value.
  CheckClassRuleSrc(NewOverrideChangesDefaultParameterValue,
    NewOverrideChangesDefaultParameterValue,
    cOverrideChangesDefaultParameterValueId, 15, 'P',
    cOverrideDefaultParamNoncompliant, cOverrideDefaultParamCompliant);
end;


procedure TRulesClassesTest.OverrideChangesDefaultParameterValueDegradesWithoutResolver;

begin
  AssertEquals('withheld resolution => silent', 0,
    RuleCount(NewOverrideChangesDefaultParameterValue, True,
    cOverrideDefaultParamNoncompliant));
end;


procedure TRulesClassesTest.OverrideChangesDefaultParameterValueSilentOnUnresolvedOperand;

begin
  // Both defaults are class references, which the evaluator does not fold.
  CheckSilentWithLiveSibling(NewOverrideChangesDefaultParameterValue,
    NewInstantiatesClassWithAbstractMethods,
    cOverrideChangesDefaultParameterValueId,
    cInstantiatesClassWithAbstractMethodsId, cOverrideDefaultParamUnfoldable);
end;


procedure TRulesClassesTest.OverrideChangesDefaultParameterValueFlagsDroppedDefault;

begin
  // TDerived.P (line 15) drops the inherited default: every polymorphic call
  // through TBase still supplies 1, every call through TDerived must pass one.
  CheckClassRuleSrc(NewOverrideChangesDefaultParameterValue,
    NewOverrideChangesDefaultParameterValue,
    cOverrideChangesDefaultParameterValueId, 15, 'P',
    cOverrideDefaultParamDropped, cOverrideDefaultParamCompliant);
end;


procedure TRulesClassesTest.OverrideChangesDefaultParameterValueFlagsAddedDefault;

begin
  // TDerived.P (line 15) adds a default its ancestor does not declare.
  CheckClassRuleSrc(NewOverrideChangesDefaultParameterValue,
    NewOverrideChangesDefaultParameterValue,
    cOverrideChangesDefaultParameterValueId, 15, 'P',
    cOverrideDefaultParamAdded, cOverrideDefaultParamCompliant);
end;


procedure TRulesClassesTest.OverrideChangesDefaultParameterValueAllowsEqualNumericDefault;

begin
  // 1 folds to an integer and 1.0 to a float, but the effective default of the
  // Double parameter is the same number.
  AssertEquals('same number => silent', 0,
    RuleCount(NewOverrideChangesDefaultParameterValue, False,
    cOverrideDefaultParamEquivalent));
end;


procedure TRulesClassesTest.AbstractMethodCalledDirectlyPositions;

begin
  // Noncompliant: 'inherited Foo;' (line 22) binds to an abstract TBase.Foo;
  // compliant has the same call over a virtual ancestor with a body.
  CheckClassRuleSrc(NewAbstractMethodCalledDirectly,
    NewAbstractMethodCalledDirectly, cAbstractMethodCalledDirectlyId, 22, 'Foo',
    cAbstractMethodCalledNoncompliant, cAbstractMethodCalledCompliant);
end;


procedure TRulesClassesTest.AbstractMethodCalledDirectlyDegradesOnParseFailure;

begin
  // The AST tier is what this rule reads, so a failed parse is its degradation.
  AssertEquals('no module => silent', 0,
    RuleCount(NewAbstractMethodCalledDirectly, False,
    cAbstractMethodCalledUnparseable));
end;


procedure TRulesClassesTest.AbstractMethodCalledDirectlyFlagsBareInherited;

begin
  // The bare 'inherited;' (line 22) names the enclosing method, TDerived.Foo,
  // whose ancestor declaration is abstract.
  CheckClassRuleSrc(NewAbstractMethodCalledDirectly,
    NewAbstractMethodCalledDirectly, cAbstractMethodCalledDirectlyId, 22, 'Foo',
    cAbstractMethodCalledBare, cAbstractMethodCalledCompliant);
end;


procedure TRulesClassesTest.AbstractMethodCalledDirectlyFlagsEmbeddedInherited;

begin
  // 'inherited Foo' (line 23) sits in an if condition rather than in a bare
  // statement or an assignment right-hand side.
  CheckClassRuleSrc(NewAbstractMethodCalledDirectly,
    NewAbstractMethodCalledDirectly, cAbstractMethodCalledDirectlyId, 23, 'Foo',
    cAbstractMethodCalledEmbedded, cAbstractMethodCalledCompliant);
end;


procedure TRulesClassesTest.AbstractMethodCalledDirectlyStopsAtNearestAncestor;

begin
  // TLeaf.Foo's inherited call binds to TMid.Foo, which has a body; the
  // abstract TBase.Foo above it is not what the call reaches.
  AssertEquals('nearest declaration has a body => silent', 0,
    RuleCount(NewAbstractMethodCalledDirectly, False,
    cAbstractMethodCalledNearestAncestor));
end;


procedure TRulesClassesTest.AbstractMethodCalledDirectlySilentOnAmbiguousOverload;

begin
  // TBase declares Foo twice, so which declaration the inherited call binds to
  // is not an AST-tier fact.
  AssertEquals('overloaded ancestor => silent', 0,
    RuleCount(NewAbstractMethodCalledDirectly, False,
    cAbstractMethodCalledAmbiguous));
end;


procedure TRulesClassesTest.AbstractMethodCalledDirectlySilentOnSplitOverload;

begin
  // The abstract Foo and the concrete one it overloads sit in different
  // ancestors.
  CheckClassRuleSrc(NewAbstractMethodCalledDirectly,
    NewAbstractMethodCalledDirectly, cAbstractMethodCalledDirectlyId, 51, 'Bar',
    cAbstractMethodCalledSplitOverload, cAbstractMethodCalledCompliant);
end;


procedure TRulesClassesTest.AbstractMethodCalledDirectlySilentOnNestedShadow;

begin
  // A member type sharing a top-level class's name is not the ancestor.
  CheckClassRuleSrc(NewAbstractMethodCalledDirectly,
    NewAbstractMethodCalledDirectly, cAbstractMethodCalledDirectlyId, 50, 'Bar',
    cAbstractMethodCalledNestedShadow, cAbstractMethodCalledCompliant);
end;


procedure TRulesClassesTest.InstantiatesClassWithAbstractMethodsPositions;

begin
  // Noncompliant: TAbstract.Create (line 20) names a class whose Run is still
  // abstract; compliant constructs the descendant that implements it.
  CheckClassRuleSrc(NewInstantiatesClassWithAbstractMethods,
    NewInstantiatesClassWithAbstractMethods,
    cInstantiatesClassWithAbstractMethodsId, 20, 'TAbstract',
    cInstantiatesAbstractNoncompliant, cInstantiatesAbstractCompliant);
end;


procedure TRulesClassesTest.InstantiatesClassWithAbstractMethodsDegradesWithoutResolver;

begin
  AssertEquals('withheld resolution => silent', 0,
    RuleCount(NewInstantiatesClassWithAbstractMethods, True,
    cInstantiatesAbstractNoncompliant));
end;


procedure TRulesClassesTest.InstantiatesClassWithAbstractMethodsSilentOnUnresolvedOperand;

begin
  // The construction goes through a class reference.
  CheckSilentWithLiveSibling(NewInstantiatesClassWithAbstractMethods,
    NewMethodHidesVirtualWithoutOverride,
    cInstantiatesClassWithAbstractMethodsId,
    cMethodHidesVirtualWithoutOverrideId, cInstantiatesAbstractUnresolved);
end;


procedure TRulesClassesTest.InterfaceWithoutGuidUsedDynamicallyPositions;

begin
  // Noncompliant: 'aItem as IFoo' (line 21) queries a COM interface the file
  // declares without a GUID; compliant declares the same interface with one.
  CheckClassRuleSrc(NewInterfaceWithoutGuidUsedDynamically,
    NewInterfaceWithoutGuidUsedDynamically,
    cInterfaceWithoutGuidUsedDynamicallyId, 21, 'IFoo',
    cGuidlessInterfaceAsCast, cGuidlessInterfaceCompliant);
end;


procedure TRulesClassesTest.InterfaceWithoutGuidUsedDynamicallyDegradesOnParseFailure;

begin
  // The AST tier is what this rule reads, so a failed parse is its degradation.
  AssertEquals('no module => silent', 0,
    RuleCount(NewInterfaceWithoutGuidUsedDynamically, False,
    cGuidlessInterfaceUnparseable));
end;


procedure TRulesClassesTest.InterfaceWithoutGuidUsedDynamicallyFlagsQueryCalls;

begin
  // Supports (line 27) and QueryInterface (line 29) name the same GUID-less
  // interface as the as-cast does.
  AssertEquals('both query calls => two issues', 2,
    RuleCount(NewInterfaceWithoutGuidUsedDynamically, False,
    cGuidlessInterfaceQueries));
end;


procedure TRulesClassesTest.InterfaceWithoutGuidUsedDynamicallyAllowsCorbaInterface;

begin
  // A CORBA interface is queried by name rather than by GUID.
  AssertEquals('corba => silent', 0,
    RuleCount(NewInterfaceWithoutGuidUsedDynamically, False,
    cGuidlessInterfaceCorba));
end;


procedure TRulesClassesTest.InterfaceWithoutGuidUsedDynamicallySilentOnForeignInterface;

begin
  // The queried interface is declared outside the analysed file, which is the
  // honest limit of a module-local name binding.
  AssertEquals('interface not declared here => silent', 0,
    RuleCount(NewInterfaceWithoutGuidUsedDynamically, False,
    cGuidlessInterfaceForeign));
end;


procedure TRulesClassesTest.InterfaceWithoutGuidUsedDynamicallySilentOnQualifiedTarget;

begin
  // A qualified target names a type through a scope the rule does not bind,
  // even when the qualifier is this unit.
  AssertEquals('qualified target => silent', 0,
    RuleCount(NewInterfaceWithoutGuidUsedDynamically, False,
    cGuidlessInterfaceQualified));
end;


procedure TRulesClassesTest.InterfaceWithoutGuidUsedDynamicallyDoesNotOverlapUniqueGuid;

var
  lFix: TTempFixtures;
  lc: TFpSonarIssueCollector;
  lPath: string;

begin
  lFix := TTempFixtures.Create;
  try
    // Declared and never queried: only the declaration-hygiene rule reports.
    lPath := lFix.Add('declonly.pas', cGuidlessInterfaceDeclaredOnly);
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewInterfaceWithoutGuidUsedDynamically, lPath, False, lc);
      AssertEquals('never queried => silent', 0,
        CountById(lc, cInterfaceWithoutGuidUsedDynamicallyId));
    finally
      lc.Free;
    end;
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewInterfaceUniqueGuid, lPath, False, lc);
      AssertEquals('the declaration is reported', 1,
        CountById(lc, cInterfaceUniqueGuidId));
      AssertEquals('at the interface declaration', 8,
        lc.Issues[FirstById(lc, cInterfaceUniqueGuidId)].StartLine);
    finally
      lc.Free;
    end;
    // Queried as well: the two ids report different lines, 8 and 21.
    lPath := lFix.Add('queried.pas', cGuidlessInterfaceAsCast);
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewInterfaceUniqueGuid, lPath, False, lc);
      AssertEquals('the declaration line', 8,
        lc.Issues[FirstById(lc, cInterfaceUniqueGuidId)].StartLine);
    finally
      lc.Free;
    end;
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewInterfaceWithoutGuidUsedDynamically, lPath, False, lc);
      AssertEquals('the query line', 21,
        lc.Issues[FirstById(lc,
        cInterfaceWithoutGuidUsedDynamicallyId)].StartLine);
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesClassesTest.SupportsResultIgnoredPositions;

begin
  // Noncompliant: 'Supports(aItem, IFoo, lFoo);' (line 28) is a statement.
  CheckClassRuleSrc(NewSupportsResultIgnored, NewSupportsResultIgnored,
    cSupportsResultIgnoredId, 28, 'Supports',
    cSupportsIgnoredNoncompliant, cSupportsIgnoredCompliant);
end;


procedure TRulesClassesTest.SupportsResultIgnoredDegradesOnParseFailure;

begin
  // The AST tier is what this rule reads, so a failed parse is its degradation.
  AssertEquals('no module => silent', 0,
    RuleCount(NewSupportsResultIgnored, False, cSupportsIgnoredUnparseable));
end;


procedure TRulesClassesTest.ClassHelperHidesAncestorMethodPositions;

begin
  // Noncompliant: TBaseHelper.Run (line 17) repeats a method name TBase
  // declares; compliant adds a name the extended chain does not carry.
  CheckClassRuleSrc(NewClassHelperHidesAncestorMethod,
    NewClassHelperHidesAncestorMethod, cClassHelperHidesAncestorMethodId,
    17, 'Run', cClassHelperHidesNoncompliant, cClassHelperHidesCompliant);
end;


procedure TRulesClassesTest.ClassHelperHidesAncestorMethodDegradesWithoutResolver;

begin
  AssertEquals('withheld resolution => silent', 0,
    RuleCount(NewClassHelperHidesAncestorMethod, True,
    cClassHelperHidesNoncompliant));
end;


procedure TRulesClassesTest.ClassHelperHidesAncestorMethodSilentOnUnresolvedOperand;

begin
  // TPoint2DHelper.Sum shadows TPoint2D.Sum, but a record helper is not a class
  // helper.
  CheckSilentWithLiveSibling(NewClassHelperHidesAncestorMethod,
    NewMethodHidesVirtualWithoutOverride, cClassHelperHidesAncestorMethodId,
    cMethodHidesVirtualWithoutOverrideId, cClassHelperHidesUnresolved);
end;


procedure TRulesClassesTest.ClassHelperHidesAncestorMethodFlagsInheritedName;

begin
  // TBaseHelper.Free (line 17) shadows a method TObject declares, one link up
  // the extended type's ancestor chain.
  CheckClassRuleSrc(NewClassHelperHidesAncestorMethod,
    NewClassHelperHidesAncestorMethod, cClassHelperHidesAncestorMethodId,
    17, 'Free', cClassHelperHidesInherited, cClassHelperHidesCompliant);
end;


procedure TRulesClassesTest.ClassHelperHidesAncestorMethodAllowsOverload;

begin
  // TBaseHelper.Describe (line 19) is declared overload.
  CheckClassRuleSrc(NewClassHelperHidesAncestorMethod,
    NewClassHelperHidesAncestorMethod, cClassHelperHidesAncestorMethodId,
    21, 'Run', cClassHelperHidesOverload, cClassHelperHidesCompliant);
end;


procedure TRulesClassesTest.ClassHelperHidesAncestorMethodFlagsSameUnitPrivate;

begin
  // TBaseHelper.Run (line 17) hides TBase.Run although it is private: the
  // helper is in the same unit.
  CheckClassRuleSrc(NewClassHelperHidesAncestorMethod,
    NewClassHelperHidesAncestorMethod, cClassHelperHidesAncestorMethodId,
    17, 'Run', cClassHelperHidesPrivate, cClassHelperHidesCompliant);
end;


procedure TRulesClassesTest.AssignedOnNonReferencePositions;

begin
  // Noncompliant: 'Assigned(lPoint)' (line 24) tests a record; compliant tests
  // a pointer, a class, a class reference, a dynamic array, a procedure
  // variable and an interface.
  CheckClassRuleSrc(NewAssignedOnNonReference, NewAssignedOnNonReference,
    cAssignedOnNonReferenceId, 24, 'lPoint',
    cAssignedNonReferenceNoncompliant, cAssignedNonReferenceCompliant);
end;


procedure TRulesClassesTest.AssignedOnNonReferenceDegradesOnParseFailure;

begin
  // The AST tier is what this rule reads, so a failed parse is its degradation.
  AssertEquals('no module => silent', 0,
    RuleCount(NewAssignedOnNonReference, False,
    cAssignedNonReferenceUnparseable));
end;


procedure TRulesClassesTest.AssignedOnNonReferenceFlagsValueKinds;

begin
  // A static array, an enumeration and a subrange are all value types.
  AssertEquals('three value operands', 3,
    RuleCount(NewAssignedOnNonReference, False,
    cAssignedNonReferenceValueKinds));
end;


procedure TRulesClassesTest.AssignedOnNonReferenceSilentOnUnknownType;

begin
  // An alias is not folded and Integer is declared outside the file.
  AssertEquals('unprovable type => silent', 0,
    RuleCount(NewAssignedOnNonReference, False,
    cAssignedNonReferenceUnknown));
end;


procedure TRulesClassesTest.AssignedOnNonReferenceSilentOnNestedRoutineScope;

begin
  // Inner's operand is OuterHas's TObject local, not the same-named record
  // field TPalette declares; a nested routine cannot see the enclosing locals
  // from here.
  AssertEquals('enclosing-routine local => silent', 0,
    RuleCount(NewAssignedOnNonReference, False,
    cAssignedNonReferenceNestedScope));
end;


procedure TRulesClassesTest.PublicFieldAndPropertyForSameStoragePositions;

begin
  // Noncompliant: property Name (line 12) reads the public field FName;
  // compliant keeps the same field private.
  CheckClassRuleSrc(NewPublicFieldAndPropertyForSameStorage,
    NewPublicFieldAndPropertyForSameStorage,
    cPublicFieldAndPropertyForSameStorageId, 12, 'FName',
    cPublicFieldPropertyNoncompliant, cPublicFieldPropertyCompliant);
end;


procedure TRulesClassesTest.PublicFieldAndPropertyForSameStorageDegradesWithoutResolver;

begin
  AssertEquals('withheld resolution => silent', 0,
    RuleCount(NewPublicFieldAndPropertyForSameStorage, True,
    cPublicFieldPropertyNoncompliant));
end;


procedure TRulesClassesTest.PublicFieldAndPropertyForSameStorageSilentOnUnresolvedOperand;

begin
  // IStore.Value binds an accessor the analysed module never implements.
  CheckSilentWithLiveSibling(NewPublicFieldAndPropertyForSameStorage,
    NewMethodHidesVirtualWithoutOverride,
    cPublicFieldAndPropertyForSameStorageId,
    cMethodHidesVirtualWithoutOverrideId, cPropertyAccessorUnresolved);
end;


procedure TRulesClassesTest.PublicFieldAndPropertyForSameStorageAllowsPrivateBackingField;

begin
  // The correct shape: one private field, one property over it.
  AssertEquals('private backing field => silent', 0,
    RuleCount(NewPublicFieldAndPropertyForSameStorage, False,
    cPublicFieldPropertyCompliant));
end;


procedure TRulesClassesTest.PublicFieldAndPropertyForSameStorageSilentOnRoutineAccessor;

begin
  // FName is public, but the property reads it through GetName.
  AssertEquals('routine accessor => silent', 0,
    RuleCount(NewPublicFieldAndPropertyForSameStorage, False,
    cPublicFieldPropertyRoutineAccessor));
end;


procedure TRulesClassesTest.PublicFieldAndPropertyForSameStorageDoesNotOverlapFieldsNotPublic;

var
  lFix: TTempFixtures;
  lc: TFpSonarIssueCollector;
  lPath: string;

begin
  lFix := TTempFixtures.Create;
  try
    lPath := lFix.Add('overlap.pas', cPublicFieldPropertyNoncompliant);
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewFieldsNotPublic, lPath, False, lc);
      AssertEquals('the field is reported once', 1,
        CountById(lc, cFieldsNotPublicId));
      AssertEquals('at the field declaration', 10,
        lc.Issues[FirstById(lc, cFieldsNotPublicId)].StartLine);
    finally
      lc.Free;
    end;
    lc := TFpSonarIssueCollector.Create;
    try
      RunRule(NewPublicFieldAndPropertyForSameStorage, lPath, False, lc);
      AssertEquals('the property is reported once', 1,
        CountById(lc, cPublicFieldAndPropertyForSameStorageId));
      AssertEquals('at the property declaration', 12,
        lc.Issues[FirstById(lc,
        cPublicFieldAndPropertyForSameStorageId)].StartLine);
    finally
      lc.Free;
    end;
  finally
    lFix.Free;
  end;
end;


procedure TRulesClassesTest.PublicFieldAndPropertyForSameStorageFlagsPublishedField;

begin
  // Noncompliant: property Owner (line 12) reads the published field FOwner;
  // compliant keeps the same field private.
  CheckClassRuleSrc(NewPublicFieldAndPropertyForSameStorage,
    NewPublicFieldAndPropertyForSameStorage,
    cPublicFieldAndPropertyForSameStorageId, 12, 'FOwner',
    cPublicFieldPropertyPublishedField, cPublicFieldPropertyCompliant);
end;


procedure TRulesClassesTest.PublicFieldAndPropertyForSameStorageFlagsFieldWriteAccessor;

begin
  // Noncompliant: property Size (line 15) reads through GetSize but writes the
  // public field FSize.
  CheckClassRuleSrc(NewPublicFieldAndPropertyForSameStorage,
    NewPublicFieldAndPropertyForSameStorage,
    cPublicFieldAndPropertyForSameStorageId, 15, 'FSize',
    cPublicFieldPropertySetterField, cPublicFieldPropertyCompliant);
end;


procedure TRulesClassesTest.PublicFieldAndPropertyForSameStorageSilentOnRecord;

begin
  // An advanced record with a public field and a property over it: a record
  // does not encapsulate its fields.
  AssertEquals('record storage => silent', 0,
    RuleCount(NewPublicFieldAndPropertyForSameStorage, False,
    cPublicFieldPropertyRecord));
end;


procedure TRulesClassesTest.PropertyAccessorVisibilityWiderThanPropertyPositions;

begin
  // Noncompliant: the private property Value (line 17) has a public getter and
  // a public setter, and is reported once rather than once per accessor;
  // compliant keeps the getter private and the property public.
  CheckClassRuleSrc(NewPropertyAccessorVisibilityWiderThanProperty,
    NewPropertyAccessorVisibilityWiderThanProperty,
    cPropertyAccessorVisibilityWiderThanPropertyId, 17, 'Value',
    cWiderAccessorNoncompliant, cWiderAccessorCompliant);
end;


procedure TRulesClassesTest.PropertyAccessorVisibilityWiderThanPropertyDegradesWithoutResolver;

begin
  AssertEquals('withheld resolution => silent', 0,
    RuleCount(NewPropertyAccessorVisibilityWiderThanProperty, True,
    cWiderAccessorNoncompliant));
end;


procedure TRulesClassesTest.PropertyAccessorVisibilityWiderThanPropertySilentOnUnresolvedOperand;

begin
  // IStore.Value and its accessor share the visibility the resolver gives an
  // interface member.
  CheckSilentWithLiveSibling(NewPropertyAccessorVisibilityWiderThanProperty,
    NewMethodHidesVirtualWithoutOverride,
    cPropertyAccessorVisibilityWiderThanPropertyId,
    cMethodHidesVirtualWithoutOverrideId, cPropertyAccessorUnresolved);
end;


procedure TRulesClassesTest.PropertyAccessorVisibilityWiderThanPropertyFlagsWiderSetter;

begin
  // A write-only protected property whose setter is public.
  AssertEquals('wider setter => one issue', 1,
    RuleCount(NewPropertyAccessorVisibilityWiderThanProperty, False,
    cWiderAccessorSetter));
end;


procedure TRulesClassesTest.PropertyAccessorVisibilityWiderThanPropertyAllowsEqualVisibility;

begin
  // Getter and property are both public, so neither widens the other.
  AssertEquals('equal visibility => silent', 0,
    RuleCount(NewPropertyAccessorVisibilityWiderThanProperty, False,
    cWiderAccessorEqual));
end;


procedure TRulesClassesTest.PropertyAccessorVisibilityWiderThanPropertySilentOnFieldAccessor;

begin
  // The public field FName is wider than nothing here: a field accessor is
  // PublicFieldAndPropertyForSameStorage's concern, not this rule's.
  AssertEquals('field accessor => silent', 0,
    RuleCount(NewPropertyAccessorVisibilityWiderThanProperty, False,
    cPublicFieldPropertyNoncompliant));
end;


procedure TRulesClassesTest.PropertyAccessorVisibilityWiderThanPropertyAllowsPublishedAccessor;

begin
  // The getter is published and the property public, and published grants
  // exactly public's access.
  AssertEquals('published accessor => silent', 0,
    RuleCount(NewPropertyAccessorVisibilityWiderThanProperty, False,
    cWiderAccessorPublished));
end;


procedure TRulesClassesTest.PropertyGetterWithSideEffectPositions;

begin
  // Noncompliant: GetValue increments FCount and assigns FLast, and property
  // Value (line 17) is reported once for the two writes; compliant reads only.
  CheckClassRuleSrc(NewPropertyGetterWithSideEffect,
    NewPropertyGetterWithSideEffect, cPropertyGetterWithSideEffectId,
    17, 'Value', cGetterSideEffectNoncompliant, cGetterSideEffectCompliant);
end;


procedure TRulesClassesTest.PropertyGetterWithSideEffectDegradesWithoutResolver;

begin
  AssertEquals('withheld resolution => silent', 0,
    RuleCount(NewPropertyGetterWithSideEffect, True,
    cGetterSideEffectNoncompliant));
end;


procedure TRulesClassesTest.PropertyGetterWithSideEffectSilentOnUnresolvedOperand;

begin
  // IStore.GetValue is declared but never implemented.
  CheckSilentWithLiveSibling(NewPropertyGetterWithSideEffect,
    NewMethodHidesVirtualWithoutOverride, cPropertyGetterWithSideEffectId,
    cMethodHidesVirtualWithoutOverrideId, cPropertyAccessorUnresolved);
end;


procedure TRulesClassesTest.PropertyGetterWithSideEffectAllowsSideEffectFreeGetter;

begin
  // One getter that only reads, and one property whose accessor is a field.
  AssertEquals('read-only getter => silent', 0,
    RuleCount(NewPropertyGetterWithSideEffect, False,
    cGetterSideEffectCompliant));
end;


procedure TRulesClassesTest.PropertyGetterWithSideEffectFlagsWriteThroughSelf;

begin
  // Self.FCount := 0 is the same write as FCount := 0.
  AssertEquals('qualified write => one issue', 1,
    RuleCount(NewPropertyGetterWithSideEffect, False,
    cGetterWritesThroughSelf));
end;


procedure TRulesClassesTest.PropertyGetterWithSideEffectAllowsLocalVariableWrite;

begin
  // Only the local lTotal and Result are written; the field is read.
  AssertEquals('local write => silent', 0,
    RuleCount(NewPropertyGetterWithSideEffect, False, cGetterWritesLocal));
end;


procedure TRulesClassesTest.PropertyGetterWithSideEffectAllowsOtherInstanceWrite;

begin
  // FNext.FValue := 0 writes a field of another instance, not of this one.
  AssertEquals('other-instance write => silent', 0,
    RuleCount(NewPropertyGetterWithSideEffect, False,
    cGetterWritesOtherInstance));
end;


procedure TRulesClassesTest.PropertyGetterWithSideEffectAllowsWithScopedWrite;

begin
  // The write is scoped by with FPeer do, so it targets the peer instance.
  AssertEquals('with-scoped write => silent', 0,
    RuleCount(NewPropertyGetterWithSideEffect, False, cGetterWritesWithScoped));
end;


procedure TRulesClassesTest.ConstructorNotVirtualInPolymorphicHierarchyPositions;

begin
  // Noncompliant: TBase declares a virtual Run and a non-virtual Create
  // (line 13) that TDerived redeclares; compliant makes the pair
  // virtual/override.
  CheckClassRuleSrc(NewConstructorNotVirtualInPolymorphicHierarchy,
    NewConstructorNotVirtualInPolymorphicHierarchy,
    cConstructorNotVirtualInPolymorphicHierarchyId, 13, 'TBase.Create',
    cHiddenConstructorNoncompliant, cHiddenConstructorCompliant);
end;


procedure TRulesClassesTest.ConstructorNotVirtualInPolymorphicHierarchyDegradesWithoutResolver;

begin
  AssertEquals('withheld resolution => silent', 0,
    RuleCount(NewConstructorNotVirtualInPolymorphicHierarchy, True,
    cHiddenConstructorNoncompliant));
end;


procedure TRulesClassesTest.ConstructorNotVirtualInPolymorphicHierarchySilentOnUnresolvedOperand;

begin
  // EStoreError redeclares Exception.Create over a polymorphic base, but that
  // base is declared outside the analysed module.
  CheckSilentWithLiveSibling(NewConstructorNotVirtualInPolymorphicHierarchy,
    NewMethodHidesVirtualWithoutOverride,
    cConstructorNotVirtualInPolymorphicHierarchyId,
    cMethodHidesVirtualWithoutOverrideId, cHiddenConstructorUnresolved);
end;


procedure TRulesClassesTest.ConstructorNotVirtualInPolymorphicHierarchyAllowsVirtualConstructor;

begin
  // A virtual base constructor with an overriding descendant dispatches
  // correctly.
  AssertEquals('virtual constructor => silent', 0,
    RuleCount(NewConstructorNotVirtualInPolymorphicHierarchy, False,
    cHiddenConstructorCompliant));
end;


procedure TRulesClassesTest.ConstructorNotVirtualInPolymorphicHierarchySilentWithoutVirtualMethods;

begin
  // TBase declares no virtual or dynamic method, the query's polymorphism
  // precondition.
  AssertEquals('no virtual method => silent', 0,
    RuleCount(NewConstructorNotVirtualInPolymorphicHierarchy, False,
    cHiddenConstructorNoVirtual));
end;


procedure TRulesClassesTest.ConstructorNotVirtualInPolymorphicHierarchySilentWithoutDescendant;

begin
  // Nothing in the module descends TBase, so its constructor is never hidden.
  AssertEquals('no descendant => silent', 0,
    RuleCount(NewConstructorNotVirtualInPolymorphicHierarchy, False,
    cHiddenConstructorNoDescendant));
end;


procedure TRulesClassesTest.ConstructorNotVirtualInPolymorphicHierarchyAllowsOverloadedDescendant;

begin
  // An overloaded descendant constructor leaves the base one visible.
  AssertEquals('overloaded descendant => silent', 0,
    RuleCount(NewConstructorNotVirtualInPolymorphicHierarchy, False,
    cHiddenConstructorOverloadedDescendant));
end;


procedure TRulesClassesTest.ConstructorNotVirtualInPolymorphicHierarchyDoesNotOverlapMethodHidesVirtualWithoutOverride;

begin
  // TDerived.Create hides a base constructor that is not overridable, which is
  // this rule's premise and the other rule's exclusion.
  AssertEquals('the hidden constructor is reported once', 1,
    RuleCount(NewConstructorNotVirtualInPolymorphicHierarchy, False,
    cHiddenConstructorNoncompliant));
  AssertEquals('a non-virtual ancestor is not a hidden virtual', 0,
    RuleCount(NewMethodHidesVirtualWithoutOverride, False,
    cHiddenConstructorNoncompliant));
end;


procedure TRulesClassesTest.InheritedCreateNotFirstStatementPositions;

begin
  // Noncompliant: TWidget.Create assigns FSize before chaining; the
  // implementation header (line 18) carries the issue.
  CheckClassRuleSrc(NewInheritedCreateNotFirstStatement,
    NewInheritedCreateNotFirstStatement, cInheritedCreateNotFirstStatementId,
    18, 'TWidget.Create',
    cInheritedCreateNotFirstNoncompliant, cInheritedCreateNotFirstCompliant);
end;


procedure TRulesClassesTest.InheritedCreateNotFirstStatementDegradesOnParseFailure;

begin
  // The AST tier is what this rule reads, so a failed parse is its degradation.
  AssertEquals('no module => silent', 0,
    RuleCount(NewInheritedCreateNotFirstStatement, False,
    cInheritedCreateUnparseable));
end;


procedure TRulesClassesTest.InheritedCreateNotFirstStatementAllowsBareInheritedFirst;

begin
  // A bare 'inherited;' chains to the ancestor constructor by name.
  AssertEquals('bare inherited first => silent', 0,
    RuleCount(NewInheritedCreateNotFirstStatement, False,
    cInheritedCreateBareFirst));
end;


procedure TRulesClassesTest.InheritedCreateNotFirstStatementAllowsNamedAncestorConstructor;

begin
  // An ancestor constructor need not be called Create to be the chain.
  AssertEquals('named ancestor constructor first => silent', 0,
    RuleCount(NewInheritedCreateNotFirstStatement, False,
    cInheritedCreateNamedAncestor));
end;


procedure TRulesClassesTest.InheritedCreateNotFirstStatementAllowsLeadingEmptyStatement;

begin
  // A stray ';' contributes no statement, so the chain is still first.
  AssertEquals('leading empty statement => silent', 0,
    RuleCount(NewInheritedCreateNotFirstStatement, False,
    cInheritedCreateLeadingEmpty));
end;


procedure TRulesClassesTest.InheritedCreateNotFirstStatementSilentWhenInheritedAbsent;

begin
  // The two ids partition the constructors: no inherited at all belongs to
  // ConstructorInherited, an inherited that is not first belongs here.
  AssertEquals('no inherited => silent here', 0,
    RuleCount(NewInheritedCreateNotFirstStatement, False,
    cConstructorInheritedNoncompliant));
  AssertEquals('no inherited => ConstructorInherited reports it', 1,
    RuleCount(NewConstructorInherited, False,
    cConstructorInheritedNoncompliant));
  AssertEquals('inherited second => reported here', 1,
    RuleCount(NewInheritedCreateNotFirstStatement, False,
    cInheritedCreateNotFirstNoncompliant));
  AssertEquals('inherited second => ConstructorInherited silent', 0,
    RuleCount(NewConstructorInherited, False,
    cInheritedCreateNotFirstNoncompliant));
end;


procedure TRulesClassesTest.InheritedCreateNotFirstStatementIgnoresClassConstructor;

begin
  // A class constructor has no instance ancestor to chain to.
  AssertEquals('class constructor => silent', 0,
    RuleCount(NewInheritedCreateNotFirstStatement, False,
    cClassConstructorChains));
end;


procedure TRulesClassesTest.InheritedDestroyNotLastStatementPositions;

begin
  // Noncompliant: TWidget.Destroy clears FSize after chaining; the
  // implementation header (line 18) carries the issue.
  CheckClassRuleSrc(NewInheritedDestroyNotLastStatement,
    NewInheritedDestroyNotLastStatement, cInheritedDestroyNotLastStatementId,
    18, 'TWidget.Destroy',
    cInheritedDestroyNotLastNoncompliant, cInheritedDestroyNotLastCompliant);
end;


procedure TRulesClassesTest.InheritedDestroyNotLastStatementDegradesOnParseFailure;

begin
  // The AST tier is what this rule reads, so a failed parse is its degradation.
  AssertEquals('no module => silent', 0,
    RuleCount(NewInheritedDestroyNotLastStatement, False,
    cInheritedDestroyUnparseable));
end;


procedure TRulesClassesTest.InheritedDestroyNotLastStatementAllowsBareInheritedLast;

begin
  // A bare 'inherited;' chains to the ancestor destructor by name.
  AssertEquals('bare inherited last => silent', 0,
    RuleCount(NewInheritedDestroyNotLastStatement, False,
    cInheritedDestroyBareLast));
end;


procedure TRulesClassesTest.InheritedDestroyNotLastStatementAllowsTrailingEmptyStatement;

begin
  // A stray ';' contributes no statement, so the chain is still last.
  AssertEquals('trailing empty statement => silent', 0,
    RuleCount(NewInheritedDestroyNotLastStatement, False,
    cInheritedDestroyTrailingEmpty));
end;


procedure TRulesClassesTest.InheritedDestroyNotLastStatementSilentWhenInheritedAbsent;

begin
  // The two ids partition the destructors: no inherited at all belongs to
  // DestructorInherited, an inherited that is not last belongs here.
  AssertEquals('no inherited => silent here', 0,
    RuleCount(NewInheritedDestroyNotLastStatement, False,
    cDestructorInheritedNoncompliant));
  AssertEquals('no inherited => DestructorInherited reports it', 1,
    RuleCount(NewDestructorInherited, False, cDestructorInheritedNoncompliant));
  AssertEquals('inherited first => reported here', 1,
    RuleCount(NewInheritedDestroyNotLastStatement, False,
    cInheritedDestroyNotLastNoncompliant));
  AssertEquals('inherited first => DestructorInherited silent', 0,
    RuleCount(NewDestructorInherited, False,
    cInheritedDestroyNotLastNoncompliant));
end;


procedure TRulesClassesTest.InheritedDestroyNotLastStatementIgnoresClassDestructor;

begin
  // A class destructor has no instance ancestor to chain to.
  AssertEquals('class destructor => silent', 0,
    RuleCount(NewInheritedDestroyNotLastStatement, False,
    cClassConstructorChains));
end;


procedure TRulesClassesTest.ComparingClassReferencesWithEqualsPositions;

begin
  // Noncompliant: 'if aLeft = aRight then' (line 25) compares two TWidget
  // references; compliant compares the Size property of each.
  CheckClassRuleSrc(NewComparingClassReferencesWithEquals,
    NewComparingClassReferencesWithEquals,
    cComparingClassReferencesWithEqualsId, 25, 'TWidget',
    cClassIdentityNoncompliant, cClassIdentityCompliant);
end;


procedure TRulesClassesTest.ComparingClassReferencesWithEqualsDegradesWithoutResolver;

begin
  AssertEquals('withheld resolution => silent', 0,
    RuleCount(NewComparingClassReferencesWithEquals, True,
    cClassIdentityNoncompliant));
end;


procedure TRulesClassesTest.ComparingClassReferencesWithEqualsSilentOnUnresolvedOperand;

begin
  // The operands are typed by the generic parameter T, which has no class
  // behind it until the template is specialized.
  CheckSilentWithLiveSibling(NewComparingClassReferencesWithEquals,
    NewMethodHidesVirtualWithoutOverride,
    cComparingClassReferencesWithEqualsId,
    cMethodHidesVirtualWithoutOverrideId, cClassIdentityUnresolvedOperand);
end;


procedure TRulesClassesTest.ComparingClassReferencesWithEqualsDegradesOnParseFailure;

begin
  AssertEquals('no module => silent', 0,
    RuleCount(NewComparingClassReferencesWithEquals, False,
    cClassIdentityUnparseable));
end;


procedure TRulesClassesTest.ComparingClassReferencesWithEqualsReportsInequalityOperator;

begin
  // <> tests the same reference identity that = does.
  AssertEquals('<> => one issue', 1,
    RuleCount(NewComparingClassReferencesWithEquals, False,
    cClassIdentityInequality));
end;


procedure TRulesClassesTest.ComparingClassReferencesWithEqualsReportsRelatedClassOperands;

begin
  // A base and a descendant are two class instances like any other pair.
  AssertEquals('related classes => one issue', 1,
    RuleCount(NewComparingClassReferencesWithEquals, False,
    cClassIdentityRelatedClasses));
end;


procedure TRulesClassesTest.ComparingClassReferencesWithEqualsAllowsNilComparison;

begin
  // nil is the one reference comparison that has no other meaning.
  AssertEquals('nil operand => silent', 0,
    RuleCount(NewComparingClassReferencesWithEquals, False,
    cClassIdentityNilComparison));
end;


procedure TRulesClassesTest.ComparingClassReferencesWithEqualsAllowsInterfaceOperands;

begin
  // Comparing two interface references is the language's identity test for
  // them, and an interface is not a class instance.
  AssertEquals('interface operands => silent', 0,
    RuleCount(NewComparingClassReferencesWithEquals, False,
    cClassIdentityInterfaceOperands));
end;


procedure TRulesClassesTest.ComparingClassReferencesWithEqualsAllowsClassReferenceOperands;

begin
  // A metaclass comparison names types, not instances.
  AssertEquals('class-reference operands => silent', 0,
    RuleCount(NewComparingClassReferencesWithEquals, False,
    cClassIdentityClassReferenceOperands));
end;


procedure TRulesClassesTest.ComparingClassReferencesWithEqualsAllowsNonClassOperands;

begin
  // Strings, integers, an operator-overloaded record and an enumeration all
  // compare by value.
  AssertEquals('value operands => silent', 0,
    RuleCount(NewComparingClassReferencesWithEquals, False,
    cClassIdentityNonClassOperands));
end;


procedure TRulesClassesTest.ComparingClassReferencesWithEqualsDoesNotOverlapNilCheckViaAssigned;

begin
  // The two ids never report one comparison: a nil operand never classifies as
  // a class instance, and this rule needs both operands to.
  AssertEquals('nil comparisons => NilCheckViaAssigned reports both', 2,
    RuleCount(NewNilCheckViaAssigned, False, cClassIdentityNilComparison));
  AssertEquals('nil comparisons => silent here', 0,
    RuleCount(NewComparingClassReferencesWithEquals, False,
    cClassIdentityNilComparison));
  AssertEquals('class comparison => NilCheckViaAssigned silent', 0,
    RuleCount(NewNilCheckViaAssigned, False, cClassIdentityNoncompliant));
end;


procedure TRulesClassesTest.RulesSelfRegisterGlobally;

begin
  // The production initialization registered all twenty-four class-hygiene
  // rules into the global registry.
  AssertTrue('VisibilityAscendingOrder registered',
    RuleRegistry.FindById(cVisibilityAscendingOrderId) <> nil);
  AssertTrue('DeclarationsFollowVisibilityOrder registered',
    RuleRegistry.FindById(cDeclarationsFollowVisibilityOrderId) <> nil);
  AssertTrue('FieldsNotPublic registered',
    RuleRegistry.FindById(cFieldsNotPublicId) <> nil);
  AssertTrue('FileNotTooManyClasses registered',
    RuleRegistry.FindById(cFileNotTooManyClassesId) <> nil);
  AssertTrue('InterfaceNotEmpty registered',
    RuleRegistry.FindById(cInterfaceNotEmptyId) <> nil);
  AssertTrue('InterfaceUniqueGuid registered',
    RuleRegistry.FindById(cInterfaceUniqueGuidId) <> nil);
  AssertTrue('ConstructorInherited registered',
    RuleRegistry.FindById(cConstructorInheritedId) <> nil);
  AssertTrue('DestructorInherited registered',
    RuleRegistry.FindById(cDestructorInheritedId) <> nil);
  AssertTrue('TopLevelClassInheritsTObject registered',
    RuleRegistry.FindById(cTopLevelClassInheritsTObjectId) <> nil);
  AssertTrue('MethodHidesVirtualWithoutOverride registered',
    RuleRegistry.FindById(cMethodHidesVirtualWithoutOverrideId) <> nil);
  AssertTrue('OverrideChangesDefaultParameterValue registered',
    RuleRegistry.FindById(cOverrideChangesDefaultParameterValueId) <> nil);
  AssertTrue('AbstractMethodCalledDirectly registered',
    RuleRegistry.FindById(cAbstractMethodCalledDirectlyId) <> nil);
  AssertTrue('InstantiatesClassWithAbstractMethods registered',
    RuleRegistry.FindById(cInstantiatesClassWithAbstractMethodsId) <> nil);
  AssertTrue('InterfaceWithoutGuidUsedDynamically registered',
    RuleRegistry.FindById(cInterfaceWithoutGuidUsedDynamicallyId) <> nil);
  AssertTrue('SupportsResultIgnored registered',
    RuleRegistry.FindById(cSupportsResultIgnoredId) <> nil);
  AssertTrue('ClassHelperHidesAncestorMethod registered',
    RuleRegistry.FindById(cClassHelperHidesAncestorMethodId) <> nil);
  AssertTrue('AssignedOnNonReference registered',
    RuleRegistry.FindById(cAssignedOnNonReferenceId) <> nil);
  AssertTrue('PublicFieldAndPropertyForSameStorage registered',
    RuleRegistry.FindById(cPublicFieldAndPropertyForSameStorageId) <> nil);
  AssertTrue('PropertyAccessorVisibilityWiderThanProperty registered',
    RuleRegistry.FindById(cPropertyAccessorVisibilityWiderThanPropertyId) <> nil);
  AssertTrue('PropertyGetterWithSideEffect registered',
    RuleRegistry.FindById(cPropertyGetterWithSideEffectId) <> nil);
  AssertTrue('ConstructorNotVirtualInPolymorphicHierarchy registered',
    RuleRegistry.FindById(cConstructorNotVirtualInPolymorphicHierarchyId) <> nil);
  AssertTrue('InheritedCreateNotFirstStatement registered',
    RuleRegistry.FindById(cInheritedCreateNotFirstStatementId) <> nil);
  AssertTrue('InheritedDestroyNotLastStatement registered',
    RuleRegistry.FindById(cInheritedDestroyNotLastStatementId) <> nil);
  AssertTrue('ComparingClassReferencesWithEquals registered',
    RuleRegistry.FindById(cComparingClassReferencesWithEqualsId) <> nil);
end;


initialization
  RegisterTest(TRulesClassesTest);

end.
