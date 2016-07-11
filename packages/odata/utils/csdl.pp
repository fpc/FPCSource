{
This unit has been produced by ws_helper.
  Input unit name : "CSDL".
  This unit name  : "CSDL".
  Date            : "12-5-16 15:37:59".
}
unit CSDL;
{$IFDEF FPC}
  {$mode objfpc} {$H+}
{$ENDIF}
{$DEFINE WST_RECORD_RTTI}
interface

uses SysUtils, Classes, TypInfo, base_service_intf, service_intf, cgs, ras{,
     System_Data_Resources_CodeGenerationSchema, System_Data_Resources_AnnotationSchema};

const
  sNAME_SPACE = 'http://schemas.microsoft.com/ado/2009/11/edm';
  sUNIT_NAME = 'CSDL';

type

  GSchemaBodyElements_UsingArray = class;
  GSchemaBodyElements_AssociationArray = class;
  GSchemaBodyElements_ComplexTypeArray = class;
  GSchemaBodyElements_EntityTypeArray = class;
  GSchemaBodyElements_EnumTypeArray = class;
  GSchemaBodyElements_ValueTermArray = class;
  GSchemaBodyElements__FunctionArray = class;
  GSchemaBodyElements_AnnotationsArray = class;
  GSchemaBodyElements = class;
  TSchema = class;
  TDocumentation = class;
  TText = class;
  TXmlOrText = class;
  GEmptyElementExtensibility = class;
  TUsing = class;
  TAssociation__EndArray = class;
  TAssociation = class;
  TTypeAttributes = class;
  TComplexType__PropertyArray = class;
  TComplexType_ValueAnnotationArray = class;
  TComplexType_TypeAnnotationArray = class;
  TComplexType = class;
  TConstraint = class;
  TReferentialConstraintRoleElement_PropertyRefArray = class;
  TReferentialConstraintRoleElement = class;
  TNavigationProperty_ValueAnnotationArray = class;
  TNavigationProperty_TypeAnnotationArray = class;
  TNavigationProperty = class;
  TDerivableTypeAttributes = class;
  TEntityType__PropertyArray = class;
  TEntityType_NavigationPropertyArray = class;
  TEntityType_ValueAnnotationArray = class;
  TEntityType_TypeAnnotationArray = class;
  TEntityType = class;
  TEnumTypeMember = class;
  TEnumType_MemberArray = class;
  TEnumType_ValueAnnotationArray = class;
  TEnumType_TypeAnnotationArray = class;
  TEnumType = class;
  TFacetAttributes = class;
  TFunction_ParameterArray = class;
  TFunction_DefiningExpressionArray = class;
  TFunction_ReturnTypeArray = class;
  TFunction_ValueAnnotationArray = class;
  TFunction_TypeAnnotationArray = class;
  TFunction = class;
  TFunctionParameter_ValueAnnotationArray = class;
  TFunctionParameter_TypeAnnotationArray = class;
  TFunctionParameter = class;
  TCollectionType = class;
  TTypeRef = class;
  TReferenceType = class;
  TRowType = class;
  TRowProperty = class;
  TFunctionReturnType = class;
  TFunctionImportReturnType = class;
  TEntityKeyElement = class;
  TPropertyRef = class;
  TAnnotations_ValueAnnotationArray = class;
  TAnnotations_TypeAnnotationArray = class;
  TAnnotations = class;
  GExpression = class;
  GInlineExpressions = class;
  TValueAnnotation = class;
  TTypeAnnotation_PropertyValueArray = class;
  TTypeAnnotation = class;
  TStringConstantExpression = class;
  TBinaryConstantExpression = class;
  TIntConstantExpression = class;
  TFloatConstantExpression = class;
  TGuidConstantExpression = class;
  TDecimalConstantExpression = class;
  TBoolConstantExpression = class;
  TTimeConstantExpression = class;
  TDateTimeConstantExpression = class;
  TDateTimeOffsetConstantExpression = class;
  TEnumMemberReferenceExpression = class;
  TNullExpression = class;
  TPathExpression = class;
  TIfExpression = class;
  TRecordExpression_PropertyValueArray = class;
  TRecordExpression = class;
  TPropertyValue = class;
  TCollectionExpression = class;
  TAssertTypeExpression = class;
  TIsTypeExpression = class;
  TFunctionReferenceExpression_Parameter_Type = class;
  TFunctionReferenceExpression_ParameterArray = class;
  TFunctionReferenceExpression = class;
  TEntitySetReferenceExpression = class;
  TParameterReferenceExpression = class;
  TApplyExpression = class;
  TPropertyReferenceExpression = class;
  TValueTermReferenceExpression = class;
  TLabeledElement = class;
  TLabeledElementReferenceExpression = class;
  TOperations = class;
  TAssociationEnd = class;
  TOnAction = class;
  TCommonPropertyAttributes = class;
  TEntityProperty_DocumentationArray = class;
  TEntityProperty_ValueAnnotationArray = class;
  TEntityProperty_TypeAnnotationArray = class;
  TEntityProperty = class;
  TComplexTypeProperty_DocumentationArray = class;
  TComplexTypeProperty_ValueAnnotationArray = class;
  TComplexTypeProperty_TypeAnnotationArray = class;
  TComplexTypeProperty = class;
  TValueTerm = class;
  TFunctionImportParameterAttributes = class;
  TFunctionImportParameter_ValueAnnotationArray = class;
  TFunctionImportParameter_TypeAnnotationArray = class;
  TFunctionImportParameter = class;
  TFunctionImportAttributes = class;
  TEntitySetAttributes = class;
  EntityContainer_FunctionImport_Type_ReturnTypeArray = class;
  EntityContainer_FunctionImport_Type_ParameterArray = class;
  EntityContainer_FunctionImport_Type_ValueAnnotationArray = class;
  EntityContainer_FunctionImport_Type_TypeAnnotationArray = class;
  EntityContainer_FunctionImport_Type = class;
  EntityContainer_EntitySet_Type_ValueAnnotationArray = class;
  EntityContainer_EntitySet_Type_TypeAnnotationArray = class;
  EntityContainer_EntitySet_Type = class;
  EntityContainer_AssociationSet_Type_End_Type = class;
  EntityContainer_AssociationSet_Type__EndArray = class;
  EntityContainer_AssociationSet_Type = class;
  EntityContainer_FunctionImportArray = class;
  EntityContainer_EntitySetArray = class;
  EntityContainer_AssociationSetArray = class;
  EntityContainer_ValueAnnotationArray = class;
  EntityContainer_TypeAnnotationArray = class;
  EntityContainer = class;

  EDMSimpleType = ( 
    Binary
    ,EDMSimpleType_Boolean
    ,EDMSimpleType_Byte
    ,DateTime
    ,DateTimeOffset
    ,Time
    ,Decimal
    ,EDMSimpleType_Double
    ,EDMSimpleType_Single
    ,Geography
    ,GeographyPoint
    ,GeographyLineString
    ,GeographyPolygon
    ,GeographyMultiPoint
    ,GeographyMultiLineString
    ,GeographyMultiPolygon
    ,GeographyCollection
    ,Geometry
    ,GeometryPoint
    ,GeometryLineString
    ,GeometryPolygon
    ,GeometryMultiPoint
    ,GeometryMultiLineString
    ,GeometryMultiPolygon
    ,GeometryCollection
    ,Guid
    ,Int16
    ,Int32
    ,EDMSimpleType_Int64
    ,EDMSimpleType_String
    ,SByte
    ,Stream
  );

  TMax = ( 
    Max
  );

  TVariable = ( 
    Variable
  );

  TParameterMode = ( 
    TParameterMode_In
    ,TParameterMode_Out
    ,InOut
  );

  TAction = ( 
    Cascade
    ,None
  );

  TMultiplicity = ( 
    TMultiplicity__0_1
    ,TMultiplicity__1
    ,TMultiplicity__
  );

  TConcurrencyMode = ( 
    TConcurrencyMode_None
    ,Fixed
  );

  Schema = TSchema;

  TMaxLengthFacet = type UnicodeString;

  TIsFixedLengthFacet = type boolean;

  TPrecisionFacet = type nonNegativeInteger;

  TScaleFacet = type nonNegativeInteger;

  TIsUnicodeFacet = type boolean;

  TCollationFacet = type UnicodeString;

  TSridFacet = type UnicodeString;

  TGuidLiteral = type UnicodeString;

  TQualifiedName = type UnicodeString;

  TNamespaceName = type TQualifiedName;

  TPath = type UnicodeString;

  TSimpleIdentifier = type UnicodeString;

  TPropertyType = type UnicodeString;

  TCommandText = type UnicodeString;

  TFunctionImportParameterAndReturnType = type UnicodeString;

  TWrappedFunctionType = type UnicodeString;

  TUnwrappedFunctionType = type UnicodeString;

  GSchemaBodyElements = class(TBaseComplexRemotable)
  private
    FUsing : GSchemaBodyElements_UsingArray;
    FAssociation : GSchemaBodyElements_AssociationArray;
    FComplexType : GSchemaBodyElements_ComplexTypeArray;
    FEntityType : GSchemaBodyElements_EntityTypeArray;
    FEnumType : GSchemaBodyElements_EnumTypeArray;
    FValueTerm : GSchemaBodyElements_ValueTermArray;
    F_Function : GSchemaBodyElements__FunctionArray;
    FAnnotations : GSchemaBodyElements_AnnotationsArray;
    FEntityContainer : CSDL.EntityContainer;
  private
    function wstHas_Using() : Boolean;
    function wstHas_Association() : Boolean;
    function wstHas_ComplexType() : Boolean;
    function wstHas_EntityType() : Boolean;
    function wstHas_EnumType() : Boolean;
    function wstHas_ValueTerm() : Boolean;
    function wstHas__Function() : Boolean;
    function wstHas_Annotations() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Using : GSchemaBodyElements_UsingArray read FUsing write FUsing stored wstHas_Using;
    property Association : GSchemaBodyElements_AssociationArray read FAssociation write FAssociation stored wstHas_Association;
    property ComplexType : GSchemaBodyElements_ComplexTypeArray read FComplexType write FComplexType stored wstHas_ComplexType;
    property EntityType : GSchemaBodyElements_EntityTypeArray read FEntityType write FEntityType stored wstHas_EntityType;
    property EnumType : GSchemaBodyElements_EnumTypeArray read FEnumType write FEnumType stored wstHas_EnumType;
    property ValueTerm : GSchemaBodyElements_ValueTermArray read FValueTerm write FValueTerm stored wstHas_ValueTerm;
    property _Function : GSchemaBodyElements__FunctionArray read F_Function write F_Function stored wstHas__Function;
    property Annotations : GSchemaBodyElements_AnnotationsArray read FAnnotations write FAnnotations stored wstHas_Annotations;
    property EntityContainer : CSDL.EntityContainer read FEntityContainer write FEntityContainer;
  end;

  TSchema = class(TBaseComplexRemotable)
  private
    FNamespace : TNamespaceName;
    FAlias : TSimpleIdentifier;
    FUsing : GSchemaBodyElements_UsingArray;
    FAssociation : GSchemaBodyElements_AssociationArray;
    FComplexType : GSchemaBodyElements_ComplexTypeArray;
    FEntityType : GSchemaBodyElements_EntityTypeArray;
    FEnumType : GSchemaBodyElements_EnumTypeArray;
    FValueTerm : GSchemaBodyElements_ValueTermArray;
    F_Function : GSchemaBodyElements__FunctionArray;
    FAnnotations : GSchemaBodyElements_AnnotationsArray;
    FEntityContainer : CSDL.EntityContainer;
  private
    function wstHas_Namespace() : Boolean;
    function wstHas_Alias() : Boolean;
    function wstHas_Using() : Boolean;
    function wstHas_Association() : Boolean;
    function wstHas_ComplexType() : Boolean;
    function wstHas_EntityType() : Boolean;
    function wstHas_EnumType() : Boolean;
    function wstHas_ValueTerm() : Boolean;
    function wstHas__Function() : Boolean;
    function wstHas_Annotations() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Namespace : TNamespaceName read FNamespace write FNamespace stored wstHas_Namespace;
    property Alias : TSimpleIdentifier read FAlias write FAlias stored wstHas_Alias;
    property Using : GSchemaBodyElements_UsingArray read FUsing write FUsing stored wstHas_Using;
    property Association : GSchemaBodyElements_AssociationArray read FAssociation write FAssociation stored wstHas_Association;
    property ComplexType : GSchemaBodyElements_ComplexTypeArray read FComplexType write FComplexType stored wstHas_ComplexType;
    property EntityType : GSchemaBodyElements_EntityTypeArray read FEntityType write FEntityType stored wstHas_EntityType;
    property EnumType : GSchemaBodyElements_EnumTypeArray read FEnumType write FEnumType stored wstHas_EnumType;
    property ValueTerm : GSchemaBodyElements_ValueTermArray read FValueTerm write FValueTerm stored wstHas_ValueTerm;
    property _Function : GSchemaBodyElements__FunctionArray read F_Function write F_Function stored wstHas__Function;
    property Annotations : GSchemaBodyElements_AnnotationsArray read FAnnotations write FAnnotations stored wstHas_Annotations;
    property EntityContainer : CSDL.EntityContainer read FEntityContainer write FEntityContainer;
  end;

  TDocumentation = class(TBaseComplexRemotable)
  private
    FSummary : TText;
    FLongDescription : TText;
  private
    function wstHas_Summary() : Boolean;
    function wstHas_LongDescription() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Summary : TText read FSummary write FSummary stored wstHas_Summary;
    property LongDescription : TText read FLongDescription write FLongDescription stored wstHas_LongDescription;
  end;

  TText = class(TStringBufferRemotable)
  end;

  TXmlOrText = class(TStringBufferRemotable)
  end;

  GEmptyElementExtensibility = class(TBaseComplexRemotable)
  private
    FDocumentation : TDocumentation;
  private
    function wstHas_Documentation() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Documentation : TDocumentation read FDocumentation write FDocumentation stored wstHas_Documentation;
  end;

  TUsing = class(TBaseComplexRemotable)
  private
    FNamespace : TNamespaceName;
    FAlias : TSimpleIdentifier;
    FDocumentation : TDocumentation;
  private
    function wstHas_Documentation() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Namespace : TNamespaceName read FNamespace write FNamespace;
    property Alias : TSimpleIdentifier read FAlias write FAlias;
    property Documentation : TDocumentation read FDocumentation write FDocumentation stored wstHas_Documentation;
  end;

  TAssociation = class(TBaseComplexRemotable)
  private
    FDocumentation : TDocumentation;
    F_End : TAssociation__EndArray;
    FReferentialConstraint : TConstraint;
    FName : TSimpleIdentifier;
  private
    function wstHas_Documentation() : Boolean;
    function wstHas_ReferentialConstraint() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Documentation : TDocumentation read FDocumentation write FDocumentation stored wstHas_Documentation;
    property _End : TAssociation__EndArray read F_End write F_End;
    property ReferentialConstraint : TConstraint read FReferentialConstraint write FReferentialConstraint stored wstHas_ReferentialConstraint;
    property Name : TSimpleIdentifier read FName write FName;
  end;

  TTypeAttributes = class(TBaseComplexRemotable)
  private
    FName : TSimpleIdentifier;
  published
    property Name : TSimpleIdentifier read FName write FName;
  end;

  TComplexType = class(TBaseComplexRemotable)
  private
    FDocumentation : TDocumentation;
    F_Property : TComplexType__PropertyArray;
    FValueAnnotation : TComplexType_ValueAnnotationArray;
    FTypeAnnotation : TComplexType_TypeAnnotationArray;
    FTypeAccess : TypeAccess_Type;
    FName : TSimpleIdentifier;
  private
    function wstHas_Documentation() : Boolean;
    function wstHas__Property() : Boolean;
    function wstHas_ValueAnnotation() : Boolean;
    function wstHas_TypeAnnotation() : Boolean;
    function wstHas_TypeAccess() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Documentation : TDocumentation read FDocumentation write FDocumentation stored wstHas_Documentation;
    property _Property : TComplexType__PropertyArray read F_Property write F_Property stored wstHas__Property;
    property ValueAnnotation : TComplexType_ValueAnnotationArray read FValueAnnotation write FValueAnnotation stored wstHas_ValueAnnotation;
    property TypeAnnotation : TComplexType_TypeAnnotationArray read FTypeAnnotation write FTypeAnnotation stored wstHas_TypeAnnotation;
    property TypeAccess : TypeAccess_Type read FTypeAccess write FTypeAccess stored wstHas_TypeAccess;
    property Name : TSimpleIdentifier read FName write FName;
  end;

  TConstraint = class(TBaseComplexRemotable)
  private
    FDocumentation : TDocumentation;
    FPrincipal : TReferentialConstraintRoleElement;
    FDependent : TReferentialConstraintRoleElement;
  private
    function wstHas_Documentation() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Documentation : TDocumentation read FDocumentation write FDocumentation stored wstHas_Documentation;
    property Principal : TReferentialConstraintRoleElement read FPrincipal write FPrincipal;
    property Dependent : TReferentialConstraintRoleElement read FDependent write FDependent;
  end;

  TReferentialConstraintRoleElement = class(TBaseComplexRemotable)
  private
    FPropertyRef : TReferentialConstraintRoleElement_PropertyRefArray;
    FRole : TSimpleIdentifier;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property PropertyRef : TReferentialConstraintRoleElement_PropertyRefArray read FPropertyRef write FPropertyRef;
    property Role : TSimpleIdentifier read FRole write FRole;
  end;

  TNavigationProperty = class(TBaseComplexRemotable)
  private
    FDocumentation : TDocumentation;
    FValueAnnotation : TNavigationProperty_ValueAnnotationArray;
    FTypeAnnotation : TNavigationProperty_TypeAnnotationArray;
    FName : TSimpleIdentifier;
    FRelationship : TQualifiedName;
    FToRole : TSimpleIdentifier;
    FFromRole : TSimpleIdentifier;
    FContainsTarget : boolean;
    FGetterAccess : GetterAccess_Type;
    FSetterAccess : SetterAccess_Type;
  private
    function wstHas_Documentation() : Boolean;
    function wstHas_ValueAnnotation() : Boolean;
    function wstHas_TypeAnnotation() : Boolean;
    function wstHas_ContainsTarget() : Boolean;
    function wstHas_GetterAccess() : Boolean;
    function wstHas_SetterAccess() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Documentation : TDocumentation read FDocumentation write FDocumentation stored wstHas_Documentation;
    property ValueAnnotation : TNavigationProperty_ValueAnnotationArray read FValueAnnotation write FValueAnnotation stored wstHas_ValueAnnotation;
    property TypeAnnotation : TNavigationProperty_TypeAnnotationArray read FTypeAnnotation write FTypeAnnotation stored wstHas_TypeAnnotation;
    property Name : TSimpleIdentifier read FName write FName;
    property Relationship : TQualifiedName read FRelationship write FRelationship;
    property ToRole : TSimpleIdentifier read FToRole write FToRole;
    property FromRole : TSimpleIdentifier read FFromRole write FFromRole;
    property ContainsTarget : boolean read FContainsTarget write FContainsTarget stored wstHas_ContainsTarget;
    property GetterAccess : GetterAccess_Type read FGetterAccess write FGetterAccess stored wstHas_GetterAccess;
    property SetterAccess : SetterAccess_Type read FSetterAccess write FSetterAccess stored wstHas_SetterAccess;
  end;

  TDerivableTypeAttributes = class(TBaseComplexRemotable)
  private
    FBaseType : TQualifiedName;
    F_Abstract : boolean;
    FName : TSimpleIdentifier;
  private
    function wstHas_BaseType() : Boolean;
    function wstHas__Abstract() : Boolean;
  published
    property BaseType : TQualifiedName read FBaseType write FBaseType stored wstHas_BaseType;
    property _Abstract : boolean read F_Abstract write F_Abstract stored wstHas__Abstract;
    property Name : TSimpleIdentifier read FName write FName;
  end;

  TEntityType = class(TBaseComplexRemotable)
  private
    FDocumentation : TDocumentation;
    FKey : TEntityKeyElement;
    F_Property : TEntityType__PropertyArray;
    FNavigationProperty : TEntityType_NavigationPropertyArray;
    FValueAnnotation : TEntityType_ValueAnnotationArray;
    FTypeAnnotation : TEntityType_TypeAnnotationArray;
    FOpenType : boolean;
    FTypeAccess : TypeAccess_Type;
    FBaseType : TQualifiedName;
    F_Abstract : boolean;
    FName : TSimpleIdentifier;
  private
    function wstHas_Documentation() : Boolean;
    function wstHas_Key() : Boolean;
    function wstHas__Property() : Boolean;
    function wstHas_NavigationProperty() : Boolean;
    function wstHas_ValueAnnotation() : Boolean;
    function wstHas_TypeAnnotation() : Boolean;
    function wstHas_OpenType() : Boolean;
    function wstHas_TypeAccess() : Boolean;
    function wstHas_BaseType() : Boolean;
    function wstHas__Abstract() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Documentation : TDocumentation read FDocumentation write FDocumentation stored wstHas_Documentation;
    property Key : TEntityKeyElement read FKey write FKey stored wstHas_Key;
    property _Property : TEntityType__PropertyArray read F_Property write F_Property stored wstHas__Property;
    property NavigationProperty : TEntityType_NavigationPropertyArray read FNavigationProperty write FNavigationProperty stored wstHas_NavigationProperty;
    property ValueAnnotation : TEntityType_ValueAnnotationArray read FValueAnnotation write FValueAnnotation stored wstHas_ValueAnnotation;
    property TypeAnnotation : TEntityType_TypeAnnotationArray read FTypeAnnotation write FTypeAnnotation stored wstHas_TypeAnnotation;
    property OpenType : boolean read FOpenType write FOpenType stored wstHas_OpenType;
    property TypeAccess : TypeAccess_Type read FTypeAccess write FTypeAccess stored wstHas_TypeAccess;
    property BaseType : TQualifiedName read FBaseType write FBaseType stored wstHas_BaseType;
    property _Abstract : boolean read F_Abstract write F_Abstract stored wstHas__Abstract;
    property Name : TSimpleIdentifier read FName write FName;
  end;

  TEnumTypeMember = class(TBaseComplexRemotable)
  private
    FName : TSimpleIdentifier;
    FValue : Int64;
    FDocumentation : TDocumentation;
  private
    function wstHas_Value() : Boolean;
    function wstHas_Documentation() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Name : TSimpleIdentifier read FName write FName;
    property Value : Int64 read FValue write FValue stored wstHas_Value;
    property Documentation : TDocumentation read FDocumentation write FDocumentation stored wstHas_Documentation;
  end;

  TEnumType = class(TBaseComplexRemotable)
  private
    FDocumentation : TDocumentation;
    FMember : TEnumType_MemberArray;
    FValueAnnotation : TEnumType_ValueAnnotationArray;
    FTypeAnnotation : TEnumType_TypeAnnotationArray;
    FIsFlags : boolean;
    FUnderlyingType : TPropertyType;
    FTypeAccess : TypeAccess_Type;
    FName : TSimpleIdentifier;
  private
    function wstHas_Documentation() : Boolean;
    function wstHas_Member() : Boolean;
    function wstHas_ValueAnnotation() : Boolean;
    function wstHas_TypeAnnotation() : Boolean;
    function wstHas_IsFlags() : Boolean;
    function wstHas_UnderlyingType() : Boolean;
    function wstHas_TypeAccess() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Documentation : TDocumentation read FDocumentation write FDocumentation stored wstHas_Documentation;
    property Member : TEnumType_MemberArray read FMember write FMember stored wstHas_Member;
    property ValueAnnotation : TEnumType_ValueAnnotationArray read FValueAnnotation write FValueAnnotation stored wstHas_ValueAnnotation;
    property TypeAnnotation : TEnumType_TypeAnnotationArray read FTypeAnnotation write FTypeAnnotation stored wstHas_TypeAnnotation;
    property IsFlags : boolean read FIsFlags write FIsFlags stored wstHas_IsFlags;
    property UnderlyingType : TPropertyType read FUnderlyingType write FUnderlyingType stored wstHas_UnderlyingType;
    property TypeAccess : TypeAccess_Type read FTypeAccess write FTypeAccess stored wstHas_TypeAccess;
    property Name : TSimpleIdentifier read FName write FName;
  end;

  TFacetAttributes = class(TBaseComplexRemotable)
  private
    FNullable : boolean;
    FDefaultValue : UnicodeString;
    FMaxLength : TMaxLengthFacet;
    FFixedLength : TIsFixedLengthFacet;
    FPrecision : TPrecisionFacet;
    FScale : TScaleFacet;
    FUnicode : TIsUnicodeFacet;
    FCollation : TCollationFacet;
    FSRID : TSridFacet;
  private
    function wstHas_Nullable() : Boolean;
    function wstHas_DefaultValue() : Boolean;
    function wstHas_MaxLength() : Boolean;
    function wstHas_FixedLength() : Boolean;
    function wstHas_Precision() : Boolean;
    function wstHas_Scale() : Boolean;
    function wstHas_Unicode() : Boolean;
    function wstHas_Collation() : Boolean;
    function wstHas_SRID() : Boolean;
  published
    property Nullable : boolean read FNullable write FNullable stored wstHas_Nullable;
    property DefaultValue : UnicodeString read FDefaultValue write FDefaultValue stored wstHas_DefaultValue;
    property MaxLength : TMaxLengthFacet read FMaxLength write FMaxLength stored wstHas_MaxLength;
    property FixedLength : TIsFixedLengthFacet read FFixedLength write FFixedLength stored wstHas_FixedLength;
    property Precision : TPrecisionFacet read FPrecision write FPrecision stored wstHas_Precision;
    property Scale : TScaleFacet read FScale write FScale stored wstHas_Scale;
    property Unicode : TIsUnicodeFacet read FUnicode write FUnicode stored wstHas_Unicode;
    property Collation : TCollationFacet read FCollation write FCollation stored wstHas_Collation;
    property SRID : TSridFacet read FSRID write FSRID stored wstHas_SRID;
  end;

  TFunction = class(TBaseComplexRemotable)
  private
    FDocumentation : TDocumentation;
    FParameter : TFunction_ParameterArray;
    FDefiningExpression : TFunction_DefiningExpressionArray;
    FReturnType : TFunction_ReturnTypeArray;
    FValueAnnotation : TFunction_ValueAnnotationArray;
    FTypeAnnotation : TFunction_TypeAnnotationArray;
    FName : TSimpleIdentifier;
    FReturnTypeAtt : TWrappedFunctionType;
    FNullable : boolean;
    FDefaultValue : UnicodeString;
    FMaxLength : TMaxLengthFacet;
    FFixedLength : TIsFixedLengthFacet;
    FPrecision : TPrecisionFacet;
    FScale : TScaleFacet;
    FUnicode : TIsUnicodeFacet;
    FCollation : TCollationFacet;
    FSRID : TSridFacet;
  private
    function wstHas_Documentation() : Boolean;
    function wstHas_Parameter() : Boolean;
    function wstHas_DefiningExpression() : Boolean;
    function wstHas_ReturnType() : Boolean;
    function wstHas_ValueAnnotation() : Boolean;
    function wstHas_TypeAnnotation() : Boolean;
    function wstHas_ReturnTypeAtt() : Boolean;
    function wstHas_Nullable() : Boolean;
    function wstHas_DefaultValue() : Boolean;
    function wstHas_MaxLength() : Boolean;
    function wstHas_FixedLength() : Boolean;
    function wstHas_Precision() : Boolean;
    function wstHas_Scale() : Boolean;
    function wstHas_Unicode() : Boolean;
    function wstHas_Collation() : Boolean;
    function wstHas_SRID() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Documentation : TDocumentation read FDocumentation write FDocumentation stored wstHas_Documentation;
    property Parameter : TFunction_ParameterArray read FParameter write FParameter stored wstHas_Parameter;
    property DefiningExpression : TFunction_DefiningExpressionArray read FDefiningExpression write FDefiningExpression stored wstHas_DefiningExpression;
    property ReturnType : TFunction_ReturnTypeArray read FReturnType write FReturnType stored wstHas_ReturnType;
    property ValueAnnotation : TFunction_ValueAnnotationArray read FValueAnnotation write FValueAnnotation stored wstHas_ValueAnnotation;
    property TypeAnnotation : TFunction_TypeAnnotationArray read FTypeAnnotation write FTypeAnnotation stored wstHas_TypeAnnotation;
    property Name : TSimpleIdentifier read FName write FName;
    property ReturnTypeAtt : TWrappedFunctionType read FReturnTypeAtt write FReturnTypeAtt stored wstHas_ReturnTypeAtt;
    property Nullable : boolean read FNullable write FNullable stored wstHas_Nullable;
    property DefaultValue : UnicodeString read FDefaultValue write FDefaultValue stored wstHas_DefaultValue;
    property MaxLength : TMaxLengthFacet read FMaxLength write FMaxLength stored wstHas_MaxLength;
    property FixedLength : TIsFixedLengthFacet read FFixedLength write FFixedLength stored wstHas_FixedLength;
    property Precision : TPrecisionFacet read FPrecision write FPrecision stored wstHas_Precision;
    property Scale : TScaleFacet read FScale write FScale stored wstHas_Scale;
    property Unicode : TIsUnicodeFacet read FUnicode write FUnicode stored wstHas_Unicode;
    property Collation : TCollationFacet read FCollation write FCollation stored wstHas_Collation;
    property SRID : TSridFacet read FSRID write FSRID stored wstHas_SRID;
  end;

  TFunctionParameter = class(TBaseComplexRemotable)
  private
    FCollectionType : TCollectionType;
    FReferenceType : TReferenceType;
    FRowType : TRowType;
    FValueAnnotation : TFunctionParameter_ValueAnnotationArray;
    FTypeAnnotation : TFunctionParameter_TypeAnnotationArray;
    FName : TSimpleIdentifier;
    F_Type : TWrappedFunctionType;
    FNullable : boolean;
    FDefaultValue : UnicodeString;
    FMaxLength : TMaxLengthFacet;
    FFixedLength : TIsFixedLengthFacet;
    FPrecision : TPrecisionFacet;
    FScale : TScaleFacet;
    FUnicode : TIsUnicodeFacet;
    FCollation : TCollationFacet;
    FSRID : TSridFacet;
  private
    function wstHas_CollectionType() : Boolean;
    function wstHas_ReferenceType() : Boolean;
    function wstHas_RowType() : Boolean;
    function wstHas_ValueAnnotation() : Boolean;
    function wstHas_TypeAnnotation() : Boolean;
    function wstHas__Type() : Boolean;
    function wstHas_Nullable() : Boolean;
    function wstHas_DefaultValue() : Boolean;
    function wstHas_MaxLength() : Boolean;
    function wstHas_FixedLength() : Boolean;
    function wstHas_Precision() : Boolean;
    function wstHas_Scale() : Boolean;
    function wstHas_Unicode() : Boolean;
    function wstHas_Collation() : Boolean;
    function wstHas_SRID() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property CollectionType : TCollectionType read FCollectionType write FCollectionType stored wstHas_CollectionType;
    property ReferenceType : TReferenceType read FReferenceType write FReferenceType stored wstHas_ReferenceType;
    property RowType : TRowType read FRowType write FRowType stored wstHas_RowType;
    property ValueAnnotation : TFunctionParameter_ValueAnnotationArray read FValueAnnotation write FValueAnnotation stored wstHas_ValueAnnotation;
    property TypeAnnotation : TFunctionParameter_TypeAnnotationArray read FTypeAnnotation write FTypeAnnotation stored wstHas_TypeAnnotation;
    property Name : TSimpleIdentifier read FName write FName;
    property _Type : TWrappedFunctionType read F_Type write F_Type stored wstHas__Type;
    property Nullable : boolean read FNullable write FNullable stored wstHas_Nullable;
    property DefaultValue : UnicodeString read FDefaultValue write FDefaultValue stored wstHas_DefaultValue;
    property MaxLength : TMaxLengthFacet read FMaxLength write FMaxLength stored wstHas_MaxLength;
    property FixedLength : TIsFixedLengthFacet read FFixedLength write FFixedLength stored wstHas_FixedLength;
    property Precision : TPrecisionFacet read FPrecision write FPrecision stored wstHas_Precision;
    property Scale : TScaleFacet read FScale write FScale stored wstHas_Scale;
    property Unicode : TIsUnicodeFacet read FUnicode write FUnicode stored wstHas_Unicode;
    property Collation : TCollationFacet read FCollation write FCollation stored wstHas_Collation;
    property SRID : TSridFacet read FSRID write FSRID stored wstHas_SRID;
  end;

  TCollectionType = class(TBaseComplexRemotable)
  private
    FCollectionType : TCollectionType;
    FReferenceType : TReferenceType;
    FRowType : TRowType;
    FTypeRef : TTypeRef;
    FElementType : TUnwrappedFunctionType;
    FNullable : boolean;
    FDefaultValue : UnicodeString;
    FMaxLength : TMaxLengthFacet;
    FFixedLength : TIsFixedLengthFacet;
    FPrecision : TPrecisionFacet;
    FScale : TScaleFacet;
    FUnicode : TIsUnicodeFacet;
    FCollation : TCollationFacet;
    FSRID : TSridFacet;
  private
    function wstHas_CollectionType() : Boolean;
    function wstHas_ReferenceType() : Boolean;
    function wstHas_RowType() : Boolean;
    function wstHas_TypeRef() : Boolean;
    function wstHas_ElementType() : Boolean;
    function wstHas_Nullable() : Boolean;
    function wstHas_DefaultValue() : Boolean;
    function wstHas_MaxLength() : Boolean;
    function wstHas_FixedLength() : Boolean;
    function wstHas_Precision() : Boolean;
    function wstHas_Scale() : Boolean;
    function wstHas_Unicode() : Boolean;
    function wstHas_Collation() : Boolean;
    function wstHas_SRID() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property CollectionType : TCollectionType read FCollectionType write FCollectionType stored wstHas_CollectionType;
    property ReferenceType : TReferenceType read FReferenceType write FReferenceType stored wstHas_ReferenceType;
    property RowType : TRowType read FRowType write FRowType stored wstHas_RowType;
    property TypeRef : TTypeRef read FTypeRef write FTypeRef stored wstHas_TypeRef;
    property ElementType : TUnwrappedFunctionType read FElementType write FElementType stored wstHas_ElementType;
    property Nullable : boolean read FNullable write FNullable stored wstHas_Nullable;
    property DefaultValue : UnicodeString read FDefaultValue write FDefaultValue stored wstHas_DefaultValue;
    property MaxLength : TMaxLengthFacet read FMaxLength write FMaxLength stored wstHas_MaxLength;
    property FixedLength : TIsFixedLengthFacet read FFixedLength write FFixedLength stored wstHas_FixedLength;
    property Precision : TPrecisionFacet read FPrecision write FPrecision stored wstHas_Precision;
    property Scale : TScaleFacet read FScale write FScale stored wstHas_Scale;
    property Unicode : TIsUnicodeFacet read FUnicode write FUnicode stored wstHas_Unicode;
    property Collation : TCollationFacet read FCollation write FCollation stored wstHas_Collation;
    property SRID : TSridFacet read FSRID write FSRID stored wstHas_SRID;
  end;

  TTypeRef = class(TBaseComplexRemotable)
  private
    F_Type : TUnwrappedFunctionType;
    FDocumentation : TDocumentation;
    FNullable : boolean;
    FDefaultValue : UnicodeString;
    FMaxLength : TMaxLengthFacet;
    FFixedLength : TIsFixedLengthFacet;
    FPrecision : TPrecisionFacet;
    FScale : TScaleFacet;
    FUnicode : TIsUnicodeFacet;
    FCollation : TCollationFacet;
    FSRID : TSridFacet;
  private
    function wstHas_Documentation() : Boolean;
    function wstHas_Nullable() : Boolean;
    function wstHas_DefaultValue() : Boolean;
    function wstHas_MaxLength() : Boolean;
    function wstHas_FixedLength() : Boolean;
    function wstHas_Precision() : Boolean;
    function wstHas_Scale() : Boolean;
    function wstHas_Unicode() : Boolean;
    function wstHas_Collation() : Boolean;
    function wstHas_SRID() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property _Type : TUnwrappedFunctionType read F_Type write F_Type;
    property Documentation : TDocumentation read FDocumentation write FDocumentation stored wstHas_Documentation;
    property Nullable : boolean read FNullable write FNullable stored wstHas_Nullable;
    property DefaultValue : UnicodeString read FDefaultValue write FDefaultValue stored wstHas_DefaultValue;
    property MaxLength : TMaxLengthFacet read FMaxLength write FMaxLength stored wstHas_MaxLength;
    property FixedLength : TIsFixedLengthFacet read FFixedLength write FFixedLength stored wstHas_FixedLength;
    property Precision : TPrecisionFacet read FPrecision write FPrecision stored wstHas_Precision;
    property Scale : TScaleFacet read FScale write FScale stored wstHas_Scale;
    property Unicode : TIsUnicodeFacet read FUnicode write FUnicode stored wstHas_Unicode;
    property Collation : TCollationFacet read FCollation write FCollation stored wstHas_Collation;
    property SRID : TSridFacet read FSRID write FSRID stored wstHas_SRID;
  end;

  TReferenceType = class(TBaseComplexRemotable)
  private
    F_Type : TUnwrappedFunctionType;
    FDocumentation : TDocumentation;
  private
    function wstHas_Documentation() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property _Type : TUnwrappedFunctionType read F_Type write F_Type;
    property Documentation : TDocumentation read FDocumentation write FDocumentation stored wstHas_Documentation;
  end;

  TRowProperty = class(TBaseComplexRemotable)
  private
    FCollectionType : TCollectionType;
    FReferenceType : TReferenceType;
    FRowType : TRowType;
    FName : TSimpleIdentifier;
    F_Type : TWrappedFunctionType;
    FNullable : boolean;
    FDefaultValue : UnicodeString;
    FMaxLength : TMaxLengthFacet;
    FFixedLength : TIsFixedLengthFacet;
    FPrecision : TPrecisionFacet;
    FScale : TScaleFacet;
    FUnicode : TIsUnicodeFacet;
    FCollation : TCollationFacet;
    FSRID : TSridFacet;
  private
    function wstHas_CollectionType() : Boolean;
    function wstHas_ReferenceType() : Boolean;
    function wstHas_RowType() : Boolean;
    function wstHas__Type() : Boolean;
    function wstHas_Nullable() : Boolean;
    function wstHas_DefaultValue() : Boolean;
    function wstHas_MaxLength() : Boolean;
    function wstHas_FixedLength() : Boolean;
    function wstHas_Precision() : Boolean;
    function wstHas_Scale() : Boolean;
    function wstHas_Unicode() : Boolean;
    function wstHas_Collation() : Boolean;
    function wstHas_SRID() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property CollectionType : TCollectionType read FCollectionType write FCollectionType stored wstHas_CollectionType;
    property ReferenceType : TReferenceType read FReferenceType write FReferenceType stored wstHas_ReferenceType;
    property RowType : TRowType read FRowType write FRowType stored wstHas_RowType;
    property Name : TSimpleIdentifier read FName write FName;
    property _Type : TWrappedFunctionType read F_Type write F_Type stored wstHas__Type;
    property Nullable : boolean read FNullable write FNullable stored wstHas_Nullable;
    property DefaultValue : UnicodeString read FDefaultValue write FDefaultValue stored wstHas_DefaultValue;
    property MaxLength : TMaxLengthFacet read FMaxLength write FMaxLength stored wstHas_MaxLength;
    property FixedLength : TIsFixedLengthFacet read FFixedLength write FFixedLength stored wstHas_FixedLength;
    property Precision : TPrecisionFacet read FPrecision write FPrecision stored wstHas_Precision;
    property Scale : TScaleFacet read FScale write FScale stored wstHas_Scale;
    property Unicode : TIsUnicodeFacet read FUnicode write FUnicode stored wstHas_Unicode;
    property Collation : TCollationFacet read FCollation write FCollation stored wstHas_Collation;
    property SRID : TSridFacet read FSRID write FSRID stored wstHas_SRID;
  end;

  TFunctionReturnType = class(TBaseComplexRemotable)
  private
    FCollectionType : TCollectionType;
    FReferenceType : TReferenceType;
    FRowType : TRowType;
    F_Type : TFunctionImportParameterAndReturnType;
    FNullable : boolean;
    FDefaultValue : UnicodeString;
    FMaxLength : TMaxLengthFacet;
    FFixedLength : TIsFixedLengthFacet;
    FPrecision : TPrecisionFacet;
    FScale : TScaleFacet;
    FUnicode : TIsUnicodeFacet;
    FCollation : TCollationFacet;
    FSRID : TSridFacet;
  private
    function wstHas_CollectionType() : Boolean;
    function wstHas_ReferenceType() : Boolean;
    function wstHas_RowType() : Boolean;
    function wstHas__Type() : Boolean;
    function wstHas_Nullable() : Boolean;
    function wstHas_DefaultValue() : Boolean;
    function wstHas_MaxLength() : Boolean;
    function wstHas_FixedLength() : Boolean;
    function wstHas_Precision() : Boolean;
    function wstHas_Scale() : Boolean;
    function wstHas_Unicode() : Boolean;
    function wstHas_Collation() : Boolean;
    function wstHas_SRID() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property CollectionType : TCollectionType read FCollectionType write FCollectionType stored wstHas_CollectionType;
    property ReferenceType : TReferenceType read FReferenceType write FReferenceType stored wstHas_ReferenceType;
    property RowType : TRowType read FRowType write FRowType stored wstHas_RowType;
    property _Type : TFunctionImportParameterAndReturnType read F_Type write F_Type stored wstHas__Type;
    property Nullable : boolean read FNullable write FNullable stored wstHas_Nullable;
    property DefaultValue : UnicodeString read FDefaultValue write FDefaultValue stored wstHas_DefaultValue;
    property MaxLength : TMaxLengthFacet read FMaxLength write FMaxLength stored wstHas_MaxLength;
    property FixedLength : TIsFixedLengthFacet read FFixedLength write FFixedLength stored wstHas_FixedLength;
    property Precision : TPrecisionFacet read FPrecision write FPrecision stored wstHas_Precision;
    property Scale : TScaleFacet read FScale write FScale stored wstHas_Scale;
    property Unicode : TIsUnicodeFacet read FUnicode write FUnicode stored wstHas_Unicode;
    property Collation : TCollationFacet read FCollation write FCollation stored wstHas_Collation;
    property SRID : TSridFacet read FSRID write FSRID stored wstHas_SRID;
  end;

  TFunctionImportReturnType = class(TBaseComplexRemotable)
  private
    F_Type : TFunctionImportParameterAndReturnType;
    FEntitySet : TSimpleIdentifier;
    FEntitySetPath : TPath;
  private
    function wstHas__Type() : Boolean;
    function wstHas_EntitySet() : Boolean;
    function wstHas_EntitySetPath() : Boolean;
  published
    property _Type : TFunctionImportParameterAndReturnType read F_Type write F_Type stored wstHas__Type;
    property EntitySet : TSimpleIdentifier read FEntitySet write FEntitySet stored wstHas_EntitySet;
    property EntitySetPath : TPath read FEntitySetPath write FEntitySetPath stored wstHas_EntitySetPath;
  end;

  TPropertyRef = class(TBaseComplexRemotable)
  private
    FName : TSimpleIdentifier;
  published
    property Name : TSimpleIdentifier read FName write FName;
  end;

  TAnnotations = class(TBaseComplexRemotable)
  private
    FValueAnnotation : TAnnotations_ValueAnnotationArray;
    FTypeAnnotation : TAnnotations_TypeAnnotationArray;
    FTarget : TPath;
    FQualifier : TSimpleIdentifier;
  private
    function wstHas_ValueAnnotation() : Boolean;
    function wstHas_TypeAnnotation() : Boolean;
    function wstHas_Qualifier() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property ValueAnnotation : TAnnotations_ValueAnnotationArray read FValueAnnotation write FValueAnnotation stored wstHas_ValueAnnotation;
    property TypeAnnotation : TAnnotations_TypeAnnotationArray read FTypeAnnotation write FTypeAnnotation stored wstHas_TypeAnnotation;
    property Target : TPath read FTarget write FTarget;
    property Qualifier : TSimpleIdentifier read FQualifier write FQualifier stored wstHas_Qualifier;
  end;

  GExpression = class(TBaseComplexRemotable)
  end;

  GInlineExpressions = class(TBaseComplexRemotable)
  private
    F_String : UnicodeString;
    FBinary : TBase16StringRemotable;
    FInt : integer;
    FFloat : Double;
    FGuid : TGuidLiteral;
    FDecimal : Currency;
    FBool : boolean;
    FTime : TTimeRemotable;
    FDateTime : TDateTimeRemotable;
    FDateTimeOffset : TDateTimeRemotable;
    FPath : TPath;
  private
    function wstHas__String() : Boolean;
    function wstHas_Binary() : Boolean;
    function wstHas_Int() : Boolean;
    function wstHas_Float() : Boolean;
    function wstHas_Guid() : Boolean;
    function wstHas_Decimal() : Boolean;
    function wstHas_Bool() : Boolean;
    function wstHas_Time() : Boolean;
    function wstHas_DateTime() : Boolean;
    function wstHas_DateTimeOffset() : Boolean;
    function wstHas_Path() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property _String : UnicodeString read F_String write F_String stored wstHas__String;
    property Binary : TBase16StringRemotable read FBinary write FBinary stored wstHas_Binary;
    property Int : integer read FInt write FInt stored wstHas_Int;
    property Float : Double read FFloat write FFloat stored wstHas_Float;
    property Guid : TGuidLiteral read FGuid write FGuid stored wstHas_Guid;
    property Decimal : Currency read FDecimal write FDecimal stored wstHas_Decimal;
    property Bool : boolean read FBool write FBool stored wstHas_Bool;
    property Time : TTimeRemotable read FTime write FTime stored wstHas_Time;
    property DateTime : TDateTimeRemotable read FDateTime write FDateTime stored wstHas_DateTime;
    property DateTimeOffset : TDateTimeRemotable read FDateTimeOffset write FDateTimeOffset stored wstHas_DateTimeOffset;
    property Path : TPath read FPath write FPath stored wstHas_Path;
  end;

  TValueAnnotation = class(TBaseComplexRemotable)
  private
    FDocumentation : TDocumentation;
    FTerm : TQualifiedName;
    FQualifier : TSimpleIdentifier;
    F_String : UnicodeString;
    FBinary : TBase16StringRemotable;
    FInt : integer;
    FFloat : Double;
    FGuid : TGuidLiteral;
    FDecimal : Currency;
    FBool : boolean;
    FTime : TTimeRemotable;
    FDateTime : TDateTimeRemotable;
    FDateTimeOffset : TDateTimeRemotable;
    FPath : TPath;
  private
    function wstHas_Documentation() : Boolean;
    function wstHas_Qualifier() : Boolean;
    function wstHas__String() : Boolean;
    function wstHas_Binary() : Boolean;
    function wstHas_Int() : Boolean;
    function wstHas_Float() : Boolean;
    function wstHas_Guid() : Boolean;
    function wstHas_Decimal() : Boolean;
    function wstHas_Bool() : Boolean;
    function wstHas_Time() : Boolean;
    function wstHas_DateTime() : Boolean;
    function wstHas_DateTimeOffset() : Boolean;
    function wstHas_Path() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Documentation : TDocumentation read FDocumentation write FDocumentation stored wstHas_Documentation;
    property Term : TQualifiedName read FTerm write FTerm;
    property Qualifier : TSimpleIdentifier read FQualifier write FQualifier stored wstHas_Qualifier;
    property _String : UnicodeString read F_String write F_String stored wstHas__String;
    property Binary : TBase16StringRemotable read FBinary write FBinary stored wstHas_Binary;
    property Int : integer read FInt write FInt stored wstHas_Int;
    property Float : Double read FFloat write FFloat stored wstHas_Float;
    property Guid : TGuidLiteral read FGuid write FGuid stored wstHas_Guid;
    property Decimal : Currency read FDecimal write FDecimal stored wstHas_Decimal;
    property Bool : boolean read FBool write FBool stored wstHas_Bool;
    property Time : TTimeRemotable read FTime write FTime stored wstHas_Time;
    property DateTime : TDateTimeRemotable read FDateTime write FDateTime stored wstHas_DateTime;
    property DateTimeOffset : TDateTimeRemotable read FDateTimeOffset write FDateTimeOffset stored wstHas_DateTimeOffset;
    property Path : TPath read FPath write FPath stored wstHas_Path;
  end;

  TTypeAnnotation = class(TBaseComplexRemotable)
  private
    FDocumentation : TDocumentation;
    FPropertyValue : TTypeAnnotation_PropertyValueArray;
    FTerm : TQualifiedName;
    FQualifier : TSimpleIdentifier;
    F_String : UnicodeString;
    FBinary : TBase16StringRemotable;
    FInt : integer;
    FFloat : Double;
    FGuid : TGuidLiteral;
    FDecimal : Currency;
    FBool : boolean;
    FTime : TTimeRemotable;
    FDateTime : TDateTimeRemotable;
    FDateTimeOffset : TDateTimeRemotable;
    FPath : TPath;
  private
    function wstHas_Documentation() : Boolean;
    function wstHas_PropertyValue() : Boolean;
    function wstHas_Qualifier() : Boolean;
    function wstHas__String() : Boolean;
    function wstHas_Binary() : Boolean;
    function wstHas_Int() : Boolean;
    function wstHas_Float() : Boolean;
    function wstHas_Guid() : Boolean;
    function wstHas_Decimal() : Boolean;
    function wstHas_Bool() : Boolean;
    function wstHas_Time() : Boolean;
    function wstHas_DateTime() : Boolean;
    function wstHas_DateTimeOffset() : Boolean;
    function wstHas_Path() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Documentation : TDocumentation read FDocumentation write FDocumentation stored wstHas_Documentation;
    property PropertyValue : TTypeAnnotation_PropertyValueArray read FPropertyValue write FPropertyValue stored wstHas_PropertyValue;
    property Term : TQualifiedName read FTerm write FTerm;
    property Qualifier : TSimpleIdentifier read FQualifier write FQualifier stored wstHas_Qualifier;
    property _String : UnicodeString read F_String write F_String stored wstHas__String;
    property Binary : TBase16StringRemotable read FBinary write FBinary stored wstHas_Binary;
    property Int : integer read FInt write FInt stored wstHas_Int;
    property Float : Double read FFloat write FFloat stored wstHas_Float;
    property Guid : TGuidLiteral read FGuid write FGuid stored wstHas_Guid;
    property Decimal : Currency read FDecimal write FDecimal stored wstHas_Decimal;
    property Bool : boolean read FBool write FBool stored wstHas_Bool;
    property Time : TTimeRemotable read FTime write FTime stored wstHas_Time;
    property DateTime : TDateTimeRemotable read FDateTime write FDateTime stored wstHas_DateTime;
    property DateTimeOffset : TDateTimeRemotable read FDateTimeOffset write FDateTimeOffset stored wstHas_DateTimeOffset;
    property Path : TPath read FPath write FPath stored wstHas_Path;
  end;

  TStringConstantExpression = class(TComplexUnicodeStringContentRemotable)
  end;

  TBinaryConstantExpression = class(TBase16StringExtRemotable)
  end;

  TIntConstantExpression = class(TComplexInt32SContentRemotable)
  end;

  TFloatConstantExpression = class(TComplexFloatDoubleContentRemotable)
  end;

  TGuidConstantExpression = class(TComplexUnicodeStringContentRemotable)
  end;

  TDecimalConstantExpression = class(TComplexCurrencyContentRemotable)
  end;

  TBoolConstantExpression = class(TComplexBooleanContentRemotable)
  end;

  TTimeConstantExpression = class(TTimeRemotable)
  end;

  TDateTimeConstantExpression = class(TDateTimeRemotable)
  end;

  TDateTimeOffsetConstantExpression = class(TDateTimeRemotable)
  end;

  TEnumMemberReferenceExpression = class(TComplexUnicodeStringContentRemotable)
  end;

  TNullExpression = class(TBaseComplexRemotable)
  private
    FDocumentation : TDocumentation;
  private
    function wstHas_Documentation() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Documentation : TDocumentation read FDocumentation write FDocumentation stored wstHas_Documentation;
  end;

  TPathExpression = class(TComplexUnicodeStringContentRemotable)
  end;

  TIfExpression = class(TBaseComplexRemotable)
  private
    FDocumentation : TDocumentation;
  private
    function wstHas_Documentation() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Documentation : TDocumentation read FDocumentation write FDocumentation stored wstHas_Documentation;
  end;

  TRecordExpression = class(TBaseComplexRemotable)
  private
    FDocumentation : TDocumentation;
    FPropertyValue : TRecordExpression_PropertyValueArray;
    F_Type : TUnwrappedFunctionType;
  private
    function wstHas_Documentation() : Boolean;
    function wstHas_PropertyValue() : Boolean;
    function wstHas__Type() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Documentation : TDocumentation read FDocumentation write FDocumentation stored wstHas_Documentation;
    property PropertyValue : TRecordExpression_PropertyValueArray read FPropertyValue write FPropertyValue stored wstHas_PropertyValue;
    property _Type : TUnwrappedFunctionType read F_Type write F_Type stored wstHas__Type;
  end;

  TPropertyValue = class(TBaseComplexRemotable)
  private
    FDocumentation : TDocumentation;
    F_Property : TSimpleIdentifier;
    F_String : UnicodeString;
    FBinary : TBase16StringRemotable;
    FInt : integer;
    FFloat : Double;
    FGuid : TGuidLiteral;
    FDecimal : Currency;
    FBool : boolean;
    FTime : TTimeRemotable;
    FDateTime : TDateTimeRemotable;
    FDateTimeOffset : TDateTimeRemotable;
    FPath : TPath;
  private
    function wstHas_Documentation() : Boolean;
    function wstHas__String() : Boolean;
    function wstHas_Binary() : Boolean;
    function wstHas_Int() : Boolean;
    function wstHas_Float() : Boolean;
    function wstHas_Guid() : Boolean;
    function wstHas_Decimal() : Boolean;
    function wstHas_Bool() : Boolean;
    function wstHas_Time() : Boolean;
    function wstHas_DateTime() : Boolean;
    function wstHas_DateTimeOffset() : Boolean;
    function wstHas_Path() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Documentation : TDocumentation read FDocumentation write FDocumentation stored wstHas_Documentation;
    property _Property : TSimpleIdentifier read F_Property write F_Property;
    property _String : UnicodeString read F_String write F_String stored wstHas__String;
    property Binary : TBase16StringRemotable read FBinary write FBinary stored wstHas_Binary;
    property Int : integer read FInt write FInt stored wstHas_Int;
    property Float : Double read FFloat write FFloat stored wstHas_Float;
    property Guid : TGuidLiteral read FGuid write FGuid stored wstHas_Guid;
    property Decimal : Currency read FDecimal write FDecimal stored wstHas_Decimal;
    property Bool : boolean read FBool write FBool stored wstHas_Bool;
    property Time : TTimeRemotable read FTime write FTime stored wstHas_Time;
    property DateTime : TDateTimeRemotable read FDateTime write FDateTime stored wstHas_DateTime;
    property DateTimeOffset : TDateTimeRemotable read FDateTimeOffset write FDateTimeOffset stored wstHas_DateTimeOffset;
    property Path : TPath read FPath write FPath stored wstHas_Path;
  end;

  TCollectionExpression = class(TBaseComplexRemotable)
  private
    FDocumentation : TDocumentation;
  private
    function wstHas_Documentation() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Documentation : TDocumentation read FDocumentation write FDocumentation stored wstHas_Documentation;
  end;

  TAssertTypeExpression = class(TBaseComplexRemotable)
  private
    FDocumentation : TDocumentation;
    FCollectionType : TCollectionType;
    FReferenceType : TReferenceType;
    FRowType : TRowType;
    F_Type : TWrappedFunctionType;
    FNullable : boolean;
    FDefaultValue : UnicodeString;
    FMaxLength : TMaxLengthFacet;
    FFixedLength : TIsFixedLengthFacet;
    FPrecision : TPrecisionFacet;
    FScale : TScaleFacet;
    FUnicode : TIsUnicodeFacet;
    FCollation : TCollationFacet;
    FSRID : TSridFacet;
  private
    function wstHas_Documentation() : Boolean;
    function wstHas_CollectionType() : Boolean;
    function wstHas_ReferenceType() : Boolean;
    function wstHas_RowType() : Boolean;
    function wstHas__Type() : Boolean;
    function wstHas_Nullable() : Boolean;
    function wstHas_DefaultValue() : Boolean;
    function wstHas_MaxLength() : Boolean;
    function wstHas_FixedLength() : Boolean;
    function wstHas_Precision() : Boolean;
    function wstHas_Scale() : Boolean;
    function wstHas_Unicode() : Boolean;
    function wstHas_Collation() : Boolean;
    function wstHas_SRID() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Documentation : TDocumentation read FDocumentation write FDocumentation stored wstHas_Documentation;
    property CollectionType : TCollectionType read FCollectionType write FCollectionType stored wstHas_CollectionType;
    property ReferenceType : TReferenceType read FReferenceType write FReferenceType stored wstHas_ReferenceType;
    property RowType : TRowType read FRowType write FRowType stored wstHas_RowType;
    property _Type : TWrappedFunctionType read F_Type write F_Type stored wstHas__Type;
    property Nullable : boolean read FNullable write FNullable stored wstHas_Nullable;
    property DefaultValue : UnicodeString read FDefaultValue write FDefaultValue stored wstHas_DefaultValue;
    property MaxLength : TMaxLengthFacet read FMaxLength write FMaxLength stored wstHas_MaxLength;
    property FixedLength : TIsFixedLengthFacet read FFixedLength write FFixedLength stored wstHas_FixedLength;
    property Precision : TPrecisionFacet read FPrecision write FPrecision stored wstHas_Precision;
    property Scale : TScaleFacet read FScale write FScale stored wstHas_Scale;
    property Unicode : TIsUnicodeFacet read FUnicode write FUnicode stored wstHas_Unicode;
    property Collation : TCollationFacet read FCollation write FCollation stored wstHas_Collation;
    property SRID : TSridFacet read FSRID write FSRID stored wstHas_SRID;
  end;

  TIsTypeExpression = class(TBaseComplexRemotable)
  private
    FDocumentation : TDocumentation;
    FCollectionType : TCollectionType;
    FReferenceType : TReferenceType;
    FRowType : TRowType;
    F_Type : TWrappedFunctionType;
    FNullable : boolean;
    FDefaultValue : UnicodeString;
    FMaxLength : TMaxLengthFacet;
    FFixedLength : TIsFixedLengthFacet;
    FPrecision : TPrecisionFacet;
    FScale : TScaleFacet;
    FUnicode : TIsUnicodeFacet;
    FCollation : TCollationFacet;
    FSRID : TSridFacet;
  private
    function wstHas_Documentation() : Boolean;
    function wstHas_CollectionType() : Boolean;
    function wstHas_ReferenceType() : Boolean;
    function wstHas_RowType() : Boolean;
    function wstHas__Type() : Boolean;
    function wstHas_Nullable() : Boolean;
    function wstHas_DefaultValue() : Boolean;
    function wstHas_MaxLength() : Boolean;
    function wstHas_FixedLength() : Boolean;
    function wstHas_Precision() : Boolean;
    function wstHas_Scale() : Boolean;
    function wstHas_Unicode() : Boolean;
    function wstHas_Collation() : Boolean;
    function wstHas_SRID() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Documentation : TDocumentation read FDocumentation write FDocumentation stored wstHas_Documentation;
    property CollectionType : TCollectionType read FCollectionType write FCollectionType stored wstHas_CollectionType;
    property ReferenceType : TReferenceType read FReferenceType write FReferenceType stored wstHas_ReferenceType;
    property RowType : TRowType read FRowType write FRowType stored wstHas_RowType;
    property _Type : TWrappedFunctionType read F_Type write F_Type stored wstHas__Type;
    property Nullable : boolean read FNullable write FNullable stored wstHas_Nullable;
    property DefaultValue : UnicodeString read FDefaultValue write FDefaultValue stored wstHas_DefaultValue;
    property MaxLength : TMaxLengthFacet read FMaxLength write FMaxLength stored wstHas_MaxLength;
    property FixedLength : TIsFixedLengthFacet read FFixedLength write FFixedLength stored wstHas_FixedLength;
    property Precision : TPrecisionFacet read FPrecision write FPrecision stored wstHas_Precision;
    property Scale : TScaleFacet read FScale write FScale stored wstHas_Scale;
    property Unicode : TIsUnicodeFacet read FUnicode write FUnicode stored wstHas_Unicode;
    property Collation : TCollationFacet read FCollation write FCollation stored wstHas_Collation;
    property SRID : TSridFacet read FSRID write FSRID stored wstHas_SRID;
  end;

  TFunctionReferenceExpression_Parameter_Type = class(TBaseComplexRemotable)
  private
    FCollectionType : TCollectionType;
    FReferenceType : TReferenceType;
    FRowType : TRowType;
    F_Type : TWrappedFunctionType;
  private
    function wstHas_CollectionType() : Boolean;
    function wstHas_ReferenceType() : Boolean;
    function wstHas_RowType() : Boolean;
    function wstHas__Type() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property CollectionType : TCollectionType read FCollectionType write FCollectionType stored wstHas_CollectionType;
    property ReferenceType : TReferenceType read FReferenceType write FReferenceType stored wstHas_ReferenceType;
    property RowType : TRowType read FRowType write FRowType stored wstHas_RowType;
    property _Type : TWrappedFunctionType read F_Type write F_Type stored wstHas__Type;
  end;

  TFunctionReferenceExpression = class(TBaseComplexRemotable)
  private
    FDocumentation : TDocumentation;
    FParameter : TFunctionReferenceExpression_ParameterArray;
    F_Function : TQualifiedName;
  private
    function wstHas_Documentation() : Boolean;
    function wstHas_Parameter() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Documentation : TDocumentation read FDocumentation write FDocumentation stored wstHas_Documentation;
    property Parameter : TFunctionReferenceExpression_ParameterArray read FParameter write FParameter stored wstHas_Parameter;
    property _Function : TQualifiedName read F_Function write F_Function;
  end;

  TEntitySetReferenceExpression = class(TComplexUnicodeStringContentRemotable)
  end;

  TParameterReferenceExpression = class(TComplexUnicodeStringContentRemotable)
  end;

  TApplyExpression = class(TBaseComplexRemotable)
  private
    FDocumentation : TDocumentation;
    F_Function : TQualifiedName;
  private
    function wstHas_Documentation() : Boolean;
    function wstHas__Function() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Documentation : TDocumentation read FDocumentation write FDocumentation stored wstHas_Documentation;
    property _Function : TQualifiedName read F_Function write F_Function stored wstHas__Function;
  end;

  TPropertyReferenceExpression = class(TBaseComplexRemotable)
  private
    FDocumentation : TDocumentation;
    F_Property : TSimpleIdentifier;
  private
    function wstHas_Documentation() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Documentation : TDocumentation read FDocumentation write FDocumentation stored wstHas_Documentation;
    property _Property : TSimpleIdentifier read F_Property write F_Property;
  end;

  TValueTermReferenceExpression = class(TBaseComplexRemotable)
  private
    FDocumentation : TDocumentation;
    FTerm : TQualifiedName;
    FQualifier : TSimpleIdentifier;
  private
    function wstHas_Documentation() : Boolean;
    function wstHas_Qualifier() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Documentation : TDocumentation read FDocumentation write FDocumentation stored wstHas_Documentation;
    property Term : TQualifiedName read FTerm write FTerm;
    property Qualifier : TSimpleIdentifier read FQualifier write FQualifier stored wstHas_Qualifier;
  end;

  TLabeledElement = class(TBaseComplexRemotable)
  private
    FDocumentation : TDocumentation;
    FName : TSimpleIdentifier;
  private
    function wstHas_Documentation() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Documentation : TDocumentation read FDocumentation write FDocumentation stored wstHas_Documentation;
    property Name : TSimpleIdentifier read FName write FName;
  end;

  TLabeledElementReferenceExpression = class(TComplexUnicodeStringContentRemotable)
  end;

  TOperations = class(TBaseComplexRemotable)
  private
    FOnDelete : TOnAction;
  private
    function wstHas_OnDelete() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property OnDelete : TOnAction read FOnDelete write FOnDelete stored wstHas_OnDelete;
  end;

  TAssociationEnd = class(TBaseComplexRemotable)
  private
    FDocumentation : TDocumentation;
    F_Type : TQualifiedName;
    FRole : TSimpleIdentifier;
    FMultiplicity : TMultiplicity;
    FOnDelete : TOnAction;
  private
    function wstHas_Documentation() : Boolean;
    function wstHas_Role() : Boolean;
    function wstHas_Multiplicity() : Boolean;
    function wstHas_OnDelete() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Documentation : TDocumentation read FDocumentation write FDocumentation stored wstHas_Documentation;
    property _Type : TQualifiedName read F_Type write F_Type;
    property Role : TSimpleIdentifier read FRole write FRole stored wstHas_Role;
    property Multiplicity : TMultiplicity read FMultiplicity write FMultiplicity stored wstHas_Multiplicity;
    property OnDelete : TOnAction read FOnDelete write FOnDelete stored wstHas_OnDelete;
  end;

  TOnAction = class(TBaseComplexRemotable)
  private
    FAction : TAction;
    FDocumentation : TDocumentation;
  private
    function wstHas_Documentation() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Action : TAction read FAction write FAction;
    property Documentation : TDocumentation read FDocumentation write FDocumentation stored wstHas_Documentation;
  end;

  TCommonPropertyAttributes = class(TBaseComplexRemotable)
  private
    FName : TSimpleIdentifier;
    F_Type : TPropertyType;
    FNullable : boolean;
    FDefaultValue : UnicodeString;
    FMaxLength : TMaxLengthFacet;
    FFixedLength : TIsFixedLengthFacet;
    FPrecision : TPrecisionFacet;
    FScale : TScaleFacet;
    FUnicode : TIsUnicodeFacet;
    FCollation : TCollationFacet;
    FSRID : TSridFacet;
    FConcurrencyMode : TConcurrencyMode;
    FSetterAccess : SetterAccess_Type;
    FGetterAccess : GetterAccess_Type;
  private
    function wstHas_Nullable() : Boolean;
    function wstHas_DefaultValue() : Boolean;
    function wstHas_MaxLength() : Boolean;
    function wstHas_FixedLength() : Boolean;
    function wstHas_Precision() : Boolean;
    function wstHas_Scale() : Boolean;
    function wstHas_Unicode() : Boolean;
    function wstHas_Collation() : Boolean;
    function wstHas_SRID() : Boolean;
    function wstHas_ConcurrencyMode() : Boolean;
    function wstHas_SetterAccess() : Boolean;
    function wstHas_GetterAccess() : Boolean;
  published
    property Name : TSimpleIdentifier read FName write FName;
    property _Type : TPropertyType read F_Type write F_Type;
    property Nullable : boolean read FNullable write FNullable stored wstHas_Nullable;
    property DefaultValue : UnicodeString read FDefaultValue write FDefaultValue stored wstHas_DefaultValue;
    property MaxLength : TMaxLengthFacet read FMaxLength write FMaxLength stored wstHas_MaxLength;
    property FixedLength : TIsFixedLengthFacet read FFixedLength write FFixedLength stored wstHas_FixedLength;
    property Precision : TPrecisionFacet read FPrecision write FPrecision stored wstHas_Precision;
    property Scale : TScaleFacet read FScale write FScale stored wstHas_Scale;
    property Unicode : TIsUnicodeFacet read FUnicode write FUnicode stored wstHas_Unicode;
    property Collation : TCollationFacet read FCollation write FCollation stored wstHas_Collation;
    property SRID : TSridFacet read FSRID write FSRID stored wstHas_SRID;
    property ConcurrencyMode : TConcurrencyMode read FConcurrencyMode write FConcurrencyMode stored wstHas_ConcurrencyMode;
    property SetterAccess : SetterAccess_Type read FSetterAccess write FSetterAccess stored wstHas_SetterAccess;
    property GetterAccess : GetterAccess_Type read FGetterAccess write FGetterAccess stored wstHas_GetterAccess;
  end;

  TEntityProperty = class(TBaseComplexRemotable)
  private
    FDocumentation : TEntityProperty_DocumentationArray;
    FValueAnnotation : TEntityProperty_ValueAnnotationArray;
    FTypeAnnotation : TEntityProperty_TypeAnnotationArray;
    FStoreGeneratedPattern : StoreGeneratedPattern_Type;
    FName : TSimpleIdentifier;
    F_Type : TPropertyType;
    FNullable : boolean;
    FDefaultValue : UnicodeString;
    FMaxLength : TMaxLengthFacet;
    FFixedLength : TIsFixedLengthFacet;
    FPrecision : TPrecisionFacet;
    FScale : TScaleFacet;
    FUnicode : TIsUnicodeFacet;
    FCollation : TCollationFacet;
    FSRID : TSridFacet;
    FConcurrencyMode : TConcurrencyMode;
    FSetterAccess : SetterAccess_Type;
    FGetterAccess : GetterAccess_Type;
  private
    function wstHas_Documentation() : Boolean;
    function wstHas_ValueAnnotation() : Boolean;
    function wstHas_TypeAnnotation() : Boolean;
    function wstHas_StoreGeneratedPattern() : Boolean;
    function wstHas_Nullable() : Boolean;
    function wstHas_DefaultValue() : Boolean;
    function wstHas_MaxLength() : Boolean;
    function wstHas_FixedLength() : Boolean;
    function wstHas_Precision() : Boolean;
    function wstHas_Scale() : Boolean;
    function wstHas_Unicode() : Boolean;
    function wstHas_Collation() : Boolean;
    function wstHas_SRID() : Boolean;
    function wstHas_ConcurrencyMode() : Boolean;
    function wstHas_SetterAccess() : Boolean;
    function wstHas_GetterAccess() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Documentation : TEntityProperty_DocumentationArray read FDocumentation write FDocumentation stored wstHas_Documentation;
    property ValueAnnotation : TEntityProperty_ValueAnnotationArray read FValueAnnotation write FValueAnnotation stored wstHas_ValueAnnotation;
    property TypeAnnotation : TEntityProperty_TypeAnnotationArray read FTypeAnnotation write FTypeAnnotation stored wstHas_TypeAnnotation;
    property StoreGeneratedPattern : StoreGeneratedPattern_Type read FStoreGeneratedPattern write FStoreGeneratedPattern stored wstHas_StoreGeneratedPattern;
    property Name : TSimpleIdentifier read FName write FName;
    property _Type : TPropertyType read F_Type write F_Type;
    property Nullable : boolean read FNullable write FNullable stored wstHas_Nullable;
    property DefaultValue : UnicodeString read FDefaultValue write FDefaultValue stored wstHas_DefaultValue;
    property MaxLength : TMaxLengthFacet read FMaxLength write FMaxLength stored wstHas_MaxLength;
    property FixedLength : TIsFixedLengthFacet read FFixedLength write FFixedLength stored wstHas_FixedLength;
    property Precision : TPrecisionFacet read FPrecision write FPrecision stored wstHas_Precision;
    property Scale : TScaleFacet read FScale write FScale stored wstHas_Scale;
    property Unicode : TIsUnicodeFacet read FUnicode write FUnicode stored wstHas_Unicode;
    property Collation : TCollationFacet read FCollation write FCollation stored wstHas_Collation;
    property SRID : TSridFacet read FSRID write FSRID stored wstHas_SRID;
    property ConcurrencyMode : TConcurrencyMode read FConcurrencyMode write FConcurrencyMode stored wstHas_ConcurrencyMode;
    property SetterAccess : SetterAccess_Type read FSetterAccess write FSetterAccess stored wstHas_SetterAccess;
    property GetterAccess : GetterAccess_Type read FGetterAccess write FGetterAccess stored wstHas_GetterAccess;
  end;

  TComplexTypeProperty = class(TBaseComplexRemotable)
  private
    FDocumentation : TComplexTypeProperty_DocumentationArray;
    FValueAnnotation : TComplexTypeProperty_ValueAnnotationArray;
    FTypeAnnotation : TComplexTypeProperty_TypeAnnotationArray;
    FName : TSimpleIdentifier;
    F_Type : TPropertyType;
    FNullable : boolean;
    FDefaultValue : UnicodeString;
    FMaxLength : TMaxLengthFacet;
    FFixedLength : TIsFixedLengthFacet;
    FPrecision : TPrecisionFacet;
    FScale : TScaleFacet;
    FUnicode : TIsUnicodeFacet;
    FCollation : TCollationFacet;
    FSRID : TSridFacet;
    FConcurrencyMode : TConcurrencyMode;
    FSetterAccess : SetterAccess_Type;
    FGetterAccess : GetterAccess_Type;
  private
    function wstHas_Documentation() : Boolean;
    function wstHas_ValueAnnotation() : Boolean;
    function wstHas_TypeAnnotation() : Boolean;
    function wstHas_Nullable() : Boolean;
    function wstHas_DefaultValue() : Boolean;
    function wstHas_MaxLength() : Boolean;
    function wstHas_FixedLength() : Boolean;
    function wstHas_Precision() : Boolean;
    function wstHas_Scale() : Boolean;
    function wstHas_Unicode() : Boolean;
    function wstHas_Collation() : Boolean;
    function wstHas_SRID() : Boolean;
    function wstHas_ConcurrencyMode() : Boolean;
    function wstHas_SetterAccess() : Boolean;
    function wstHas_GetterAccess() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Documentation : TComplexTypeProperty_DocumentationArray read FDocumentation write FDocumentation stored wstHas_Documentation;
    property ValueAnnotation : TComplexTypeProperty_ValueAnnotationArray read FValueAnnotation write FValueAnnotation stored wstHas_ValueAnnotation;
    property TypeAnnotation : TComplexTypeProperty_TypeAnnotationArray read FTypeAnnotation write FTypeAnnotation stored wstHas_TypeAnnotation;
    property Name : TSimpleIdentifier read FName write FName;
    property _Type : TPropertyType read F_Type write F_Type;
    property Nullable : boolean read FNullable write FNullable stored wstHas_Nullable;
    property DefaultValue : UnicodeString read FDefaultValue write FDefaultValue stored wstHas_DefaultValue;
    property MaxLength : TMaxLengthFacet read FMaxLength write FMaxLength stored wstHas_MaxLength;
    property FixedLength : TIsFixedLengthFacet read FFixedLength write FFixedLength stored wstHas_FixedLength;
    property Precision : TPrecisionFacet read FPrecision write FPrecision stored wstHas_Precision;
    property Scale : TScaleFacet read FScale write FScale stored wstHas_Scale;
    property Unicode : TIsUnicodeFacet read FUnicode write FUnicode stored wstHas_Unicode;
    property Collation : TCollationFacet read FCollation write FCollation stored wstHas_Collation;
    property SRID : TSridFacet read FSRID write FSRID stored wstHas_SRID;
    property ConcurrencyMode : TConcurrencyMode read FConcurrencyMode write FConcurrencyMode stored wstHas_ConcurrencyMode;
    property SetterAccess : SetterAccess_Type read FSetterAccess write FSetterAccess stored wstHas_SetterAccess;
    property GetterAccess : GetterAccess_Type read FGetterAccess write FGetterAccess stored wstHas_GetterAccess;
  end;

  TValueTerm = class(TBaseComplexRemotable)
  private
    FDocumentation : TDocumentation;
    FCollectionType : TCollectionType;
    FReferenceType : TReferenceType;
    FRowType : TRowType;
    FName : TSimpleIdentifier;
    F_Type : TWrappedFunctionType;
    FNullable : boolean;
    FDefaultValue : UnicodeString;
    FMaxLength : TMaxLengthFacet;
    FFixedLength : TIsFixedLengthFacet;
    FPrecision : TPrecisionFacet;
    FScale : TScaleFacet;
    FUnicode : TIsUnicodeFacet;
    FCollation : TCollationFacet;
    FSRID : TSridFacet;
  private
    function wstHas_Documentation() : Boolean;
    function wstHas_CollectionType() : Boolean;
    function wstHas_ReferenceType() : Boolean;
    function wstHas_RowType() : Boolean;
    function wstHas__Type() : Boolean;
    function wstHas_Nullable() : Boolean;
    function wstHas_DefaultValue() : Boolean;
    function wstHas_MaxLength() : Boolean;
    function wstHas_FixedLength() : Boolean;
    function wstHas_Precision() : Boolean;
    function wstHas_Scale() : Boolean;
    function wstHas_Unicode() : Boolean;
    function wstHas_Collation() : Boolean;
    function wstHas_SRID() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Documentation : TDocumentation read FDocumentation write FDocumentation stored wstHas_Documentation;
    property CollectionType : TCollectionType read FCollectionType write FCollectionType stored wstHas_CollectionType;
    property ReferenceType : TReferenceType read FReferenceType write FReferenceType stored wstHas_ReferenceType;
    property RowType : TRowType read FRowType write FRowType stored wstHas_RowType;
    property Name : TSimpleIdentifier read FName write FName;
    property _Type : TWrappedFunctionType read F_Type write F_Type stored wstHas__Type;
    property Nullable : boolean read FNullable write FNullable stored wstHas_Nullable;
    property DefaultValue : UnicodeString read FDefaultValue write FDefaultValue stored wstHas_DefaultValue;
    property MaxLength : TMaxLengthFacet read FMaxLength write FMaxLength stored wstHas_MaxLength;
    property FixedLength : TIsFixedLengthFacet read FFixedLength write FFixedLength stored wstHas_FixedLength;
    property Precision : TPrecisionFacet read FPrecision write FPrecision stored wstHas_Precision;
    property Scale : TScaleFacet read FScale write FScale stored wstHas_Scale;
    property Unicode : TIsUnicodeFacet read FUnicode write FUnicode stored wstHas_Unicode;
    property Collation : TCollationFacet read FCollation write FCollation stored wstHas_Collation;
    property SRID : TSridFacet read FSRID write FSRID stored wstHas_SRID;
  end;

  TFunctionImportParameterAttributes = class(TBaseComplexRemotable)
  private
    FName : TSimpleIdentifier;
    F_Type : TFunctionImportParameterAndReturnType;
    FMode : TParameterMode;
    FNullable : boolean;
    FMaxLength : TMaxLengthFacet;
    FPrecision : TPrecisionFacet;
    FScale : TScaleFacet;
    FSRID : TSridFacet;
  private
    function wstHas_Mode() : Boolean;
    function wstHas_Nullable() : Boolean;
    function wstHas_MaxLength() : Boolean;
    function wstHas_Precision() : Boolean;
    function wstHas_Scale() : Boolean;
    function wstHas_SRID() : Boolean;
  published
    property Name : TSimpleIdentifier read FName write FName;
    property _Type : TFunctionImportParameterAndReturnType read F_Type write F_Type;
    property Mode : TParameterMode read FMode write FMode stored wstHas_Mode;
    property Nullable : boolean read FNullable write FNullable stored wstHas_Nullable;
    property MaxLength : TMaxLengthFacet read FMaxLength write FMaxLength stored wstHas_MaxLength;
    property Precision : TPrecisionFacet read FPrecision write FPrecision stored wstHas_Precision;
    property Scale : TScaleFacet read FScale write FScale stored wstHas_Scale;
    property SRID : TSridFacet read FSRID write FSRID stored wstHas_SRID;
  end;

  TFunctionImportParameter = class(TBaseComplexRemotable)
  private
    FDocumentation : TDocumentation;
    FValueAnnotation : TFunctionImportParameter_ValueAnnotationArray;
    FTypeAnnotation : TFunctionImportParameter_TypeAnnotationArray;
    FName : TSimpleIdentifier;
    F_Type : TFunctionImportParameterAndReturnType;
    FMode : TParameterMode;
    FNullable : boolean;
    FMaxLength : TMaxLengthFacet;
    FPrecision : TPrecisionFacet;
    FScale : TScaleFacet;
    FSRID : TSridFacet;
  private
    function wstHas_Documentation() : Boolean;
    function wstHas_ValueAnnotation() : Boolean;
    function wstHas_TypeAnnotation() : Boolean;
    function wstHas_Mode() : Boolean;
    function wstHas_Nullable() : Boolean;
    function wstHas_MaxLength() : Boolean;
    function wstHas_Precision() : Boolean;
    function wstHas_Scale() : Boolean;
    function wstHas_SRID() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Documentation : TDocumentation read FDocumentation write FDocumentation stored wstHas_Documentation;
    property ValueAnnotation : TFunctionImportParameter_ValueAnnotationArray read FValueAnnotation write FValueAnnotation stored wstHas_ValueAnnotation;
    property TypeAnnotation : TFunctionImportParameter_TypeAnnotationArray read FTypeAnnotation write FTypeAnnotation stored wstHas_TypeAnnotation;
    property Name : TSimpleIdentifier read FName write FName;
    property _Type : TFunctionImportParameterAndReturnType read F_Type write F_Type;
    property Mode : TParameterMode read FMode write FMode stored wstHas_Mode;
    property Nullable : boolean read FNullable write FNullable stored wstHas_Nullable;
    property MaxLength : TMaxLengthFacet read FMaxLength write FMaxLength stored wstHas_MaxLength;
    property Precision : TPrecisionFacet read FPrecision write FPrecision stored wstHas_Precision;
    property Scale : TScaleFacet read FScale write FScale stored wstHas_Scale;
    property SRID : TSridFacet read FSRID write FSRID stored wstHas_SRID;
  end;

  TFunctionImportAttributes = class(TBaseComplexRemotable)
  private
    FName : TSimpleIdentifier;
    FReturnType : TFunctionImportParameterAndReturnType;
    FEntitySet : TSimpleIdentifier;
    FEntitySetPath : UnicodeString;
    FIsComposable : boolean;
    FIsSideEffecting : boolean;
    FIsBindable : boolean;
    FMethodAccess : MethodAccess_Type;
  private
    function wstHas_ReturnType() : Boolean;
    function wstHas_EntitySet() : Boolean;
    function wstHas_EntitySetPath() : Boolean;
    function wstHas_IsComposable() : Boolean;
    function wstHas_IsSideEffecting() : Boolean;
    function wstHas_IsBindable() : Boolean;
    function wstHas_MethodAccess() : Boolean;
  published
    property Name : TSimpleIdentifier read FName write FName;
    property ReturnType : TFunctionImportParameterAndReturnType read FReturnType write FReturnType stored wstHas_ReturnType;
    property EntitySet : TSimpleIdentifier read FEntitySet write FEntitySet stored wstHas_EntitySet;
    property EntitySetPath : UnicodeString read FEntitySetPath write FEntitySetPath stored wstHas_EntitySetPath;
    property IsComposable : boolean read FIsComposable write FIsComposable stored wstHas_IsComposable;
    property IsSideEffecting : boolean read FIsSideEffecting write FIsSideEffecting stored wstHas_IsSideEffecting;
    property IsBindable : boolean read FIsBindable write FIsBindable stored wstHas_IsBindable;
    property MethodAccess : MethodAccess_Type read FMethodAccess write FMethodAccess stored wstHas_MethodAccess;
  end;

  TEntitySetAttributes = class(TBaseComplexRemotable)
  private
    FName : TSimpleIdentifier;
    FEntityType : TQualifiedName;
    FGetterAccess : GetterAccess_Type;
  private
    function wstHas_GetterAccess() : Boolean;
  published
    property Name : TSimpleIdentifier read FName write FName;
    property EntityType : TQualifiedName read FEntityType write FEntityType;
    property GetterAccess : GetterAccess_Type read FGetterAccess write FGetterAccess stored wstHas_GetterAccess;
  end;

  EntityContainer_FunctionImport_Type = class(TBaseComplexRemotable)
  private
    FDocumentation : TDocumentation;
    FReturnType : EntityContainer_FunctionImport_Type_ReturnTypeArray;
    FParameter : EntityContainer_FunctionImport_Type_ParameterArray;
    FValueAnnotation : EntityContainer_FunctionImport_Type_ValueAnnotationArray;
    FTypeAnnotation : EntityContainer_FunctionImport_Type_TypeAnnotationArray;
    FName : TSimpleIdentifier;
    FReturnTypeAtt : TFunctionImportParameterAndReturnType;
    FEntitySet : TSimpleIdentifier;
    FEntitySetPath : UnicodeString;
    FIsComposable : boolean;
    FIsSideEffecting : boolean;
    FIsBindable : boolean;
    FMethodAccess : MethodAccess_Type;
  private
    function wstHas_Documentation() : Boolean;
    function wstHas_ReturnType() : Boolean;
    function wstHas_Parameter() : Boolean;
    function wstHas_ValueAnnotation() : Boolean;
    function wstHas_TypeAnnotation() : Boolean;
    function wstHas_ReturnTypeAtt() : Boolean;
    function wstHas_EntitySet() : Boolean;
    function wstHas_EntitySetPath() : Boolean;
    function wstHas_IsComposable() : Boolean;
    function wstHas_IsSideEffecting() : Boolean;
    function wstHas_IsBindable() : Boolean;
    function wstHas_MethodAccess() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Documentation : TDocumentation read FDocumentation write FDocumentation stored wstHas_Documentation;
    property ReturnType : EntityContainer_FunctionImport_Type_ReturnTypeArray read FReturnType write FReturnType stored wstHas_ReturnType;
    property Parameter : EntityContainer_FunctionImport_Type_ParameterArray read FParameter write FParameter stored wstHas_Parameter;
    property ValueAnnotation : EntityContainer_FunctionImport_Type_ValueAnnotationArray read FValueAnnotation write FValueAnnotation stored wstHas_ValueAnnotation;
    property TypeAnnotation : EntityContainer_FunctionImport_Type_TypeAnnotationArray read FTypeAnnotation write FTypeAnnotation stored wstHas_TypeAnnotation;
    property Name : TSimpleIdentifier read FName write FName;
    property ReturnTypeAtt : TFunctionImportParameterAndReturnType read FReturnTypeAtt write FReturnTypeAtt stored wstHas_ReturnTypeAtt;
    property EntitySet : TSimpleIdentifier read FEntitySet write FEntitySet stored wstHas_EntitySet;
    property EntitySetPath : UnicodeString read FEntitySetPath write FEntitySetPath stored wstHas_EntitySetPath;
    property IsComposable : boolean read FIsComposable write FIsComposable stored wstHas_IsComposable;
    property IsSideEffecting : boolean read FIsSideEffecting write FIsSideEffecting stored wstHas_IsSideEffecting;
    property IsBindable : boolean read FIsBindable write FIsBindable stored wstHas_IsBindable;
    property MethodAccess : MethodAccess_Type read FMethodAccess write FMethodAccess stored wstHas_MethodAccess;
  end;

  EntityContainer_EntitySet_Type = class(TBaseComplexRemotable)
  private
    FDocumentation : TDocumentation;
    FValueAnnotation : EntityContainer_EntitySet_Type_ValueAnnotationArray;
    FTypeAnnotation : EntityContainer_EntitySet_Type_TypeAnnotationArray;
    FName : TSimpleIdentifier;
    FEntityType : TQualifiedName;
    FGetterAccess : GetterAccess_Type;
  private
    function wstHas_Documentation() : Boolean;
    function wstHas_ValueAnnotation() : Boolean;
    function wstHas_TypeAnnotation() : Boolean;
    function wstHas_GetterAccess() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Documentation : TDocumentation read FDocumentation write FDocumentation stored wstHas_Documentation;
    property ValueAnnotation : EntityContainer_EntitySet_Type_ValueAnnotationArray read FValueAnnotation write FValueAnnotation stored wstHas_ValueAnnotation;
    property TypeAnnotation : EntityContainer_EntitySet_Type_TypeAnnotationArray read FTypeAnnotation write FTypeAnnotation stored wstHas_TypeAnnotation;
    property Name : TSimpleIdentifier read FName write FName;
    property EntityType : TQualifiedName read FEntityType write FEntityType;
    property GetterAccess : GetterAccess_Type read FGetterAccess write FGetterAccess stored wstHas_GetterAccess;
  end;

  EntityContainer_AssociationSet_Type_End_Type = class(TBaseComplexRemotable)
  private
    FRole : TSimpleIdentifier;
    FEntitySet : TSimpleIdentifier;
    FDocumentation : TDocumentation;
  private
    function wstHas_Role() : Boolean;
    function wstHas_Documentation() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Role : TSimpleIdentifier read FRole write FRole stored wstHas_Role;
    property EntitySet : TSimpleIdentifier read FEntitySet write FEntitySet;
    property Documentation : TDocumentation read FDocumentation write FDocumentation stored wstHas_Documentation;
  end;

  EntityContainer_AssociationSet_Type = class(TBaseComplexRemotable)
  private
    FDocumentation : TDocumentation;
    F_End : EntityContainer_AssociationSet_Type__EndArray;
    FName : TSimpleIdentifier;
    FAssociation : TQualifiedName;
  private
    function wstHas_Documentation() : Boolean;
    function wstHas__End() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Documentation : TDocumentation read FDocumentation write FDocumentation stored wstHas_Documentation;
    property _End : EntityContainer_AssociationSet_Type__EndArray read F_End write F_End stored wstHas__End;
    property Name : TSimpleIdentifier read FName write FName;
    property Association : TQualifiedName read FAssociation write FAssociation;
  end;

  EntityContainer = class(TBaseComplexRemotable)
  private
    FDocumentation : TDocumentation;
    FFunctionImport : EntityContainer_FunctionImportArray;
    FEntitySet : EntityContainer_EntitySetArray;
    FAssociationSet : EntityContainer_AssociationSetArray;
    FValueAnnotation : EntityContainer_ValueAnnotationArray;
    FTypeAnnotation : EntityContainer_TypeAnnotationArray;
    FName : TSimpleIdentifier;
    FExtends : TSimpleIdentifier;
    FTypeAccess : TypeAccess_Type;
    FLazyLoadingEnabled : LazyLoadingEnabled_Type;
  private
    function wstHas_Documentation() : Boolean;
    function wstHas_FunctionImport() : Boolean;
    function wstHas_EntitySet() : Boolean;
    function wstHas_AssociationSet() : Boolean;
    function wstHas_ValueAnnotation() : Boolean;
    function wstHas_TypeAnnotation() : Boolean;
    function wstHas_Extends() : Boolean;
    function wstHas_TypeAccess() : Boolean;
    function wstHas_LazyLoadingEnabled() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Documentation : TDocumentation read FDocumentation write FDocumentation stored wstHas_Documentation;
    property FunctionImport : EntityContainer_FunctionImportArray read FFunctionImport write FFunctionImport stored wstHas_FunctionImport;
    property EntitySet : EntityContainer_EntitySetArray read FEntitySet write FEntitySet stored wstHas_EntitySet;
    property AssociationSet : EntityContainer_AssociationSetArray read FAssociationSet write FAssociationSet stored wstHas_AssociationSet;
    property ValueAnnotation : EntityContainer_ValueAnnotationArray read FValueAnnotation write FValueAnnotation stored wstHas_ValueAnnotation;
    property TypeAnnotation : EntityContainer_TypeAnnotationArray read FTypeAnnotation write FTypeAnnotation stored wstHas_TypeAnnotation;
    property Name : TSimpleIdentifier read FName write FName;
    property Extends : TSimpleIdentifier read FExtends write FExtends stored wstHas_Extends;
    property TypeAccess : TypeAccess_Type read FTypeAccess write FTypeAccess stored wstHas_TypeAccess;
    property LazyLoadingEnabled : LazyLoadingEnabled_Type read FLazyLoadingEnabled write FLazyLoadingEnabled stored wstHas_LazyLoadingEnabled;
  end;

  GSchemaBodyElements_UsingArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TUsing;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TUsing Read GetItem;Default;
  end;

  GSchemaBodyElements_AssociationArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TAssociation;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TAssociation Read GetItem;Default;
  end;

  GSchemaBodyElements_ComplexTypeArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TComplexType;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TComplexType Read GetItem;Default;
  end;

  GSchemaBodyElements_EntityTypeArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TEntityType;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TEntityType Read GetItem;Default;
  end;

  GSchemaBodyElements_EnumTypeArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TEnumType;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TEnumType Read GetItem;Default;
  end;

  GSchemaBodyElements_ValueTermArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TValueTerm;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TValueTerm Read GetItem;Default;
  end;

  GSchemaBodyElements__FunctionArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TFunction;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TFunction Read GetItem;Default;
  end;

  GSchemaBodyElements_AnnotationsArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TAnnotations;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TAnnotations Read GetItem;Default;
  end;

  TAssociation__EndArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TAssociationEnd;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TAssociationEnd Read GetItem;Default;
  end;

  TComplexType__PropertyArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TComplexTypeProperty;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TComplexTypeProperty Read GetItem;Default;
  end;

  TComplexType_ValueAnnotationArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TValueAnnotation;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TValueAnnotation Read GetItem;Default;
  end;

  TComplexType_TypeAnnotationArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TTypeAnnotation;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TTypeAnnotation Read GetItem;Default;
  end;

  TReferentialConstraintRoleElement_PropertyRefArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TPropertyRef;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TPropertyRef Read GetItem;Default;
  end;

  TNavigationProperty_ValueAnnotationArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TValueAnnotation;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TValueAnnotation Read GetItem;Default;
  end;

  TNavigationProperty_TypeAnnotationArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TTypeAnnotation;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TTypeAnnotation Read GetItem;Default;
  end;

  TEntityType__PropertyArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TEntityProperty;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TEntityProperty Read GetItem;Default;
  end;

  TEntityType_NavigationPropertyArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TNavigationProperty;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TNavigationProperty Read GetItem;Default;
  end;

  TEntityType_ValueAnnotationArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TValueAnnotation;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TValueAnnotation Read GetItem;Default;
  end;

  TEntityType_TypeAnnotationArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TTypeAnnotation;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TTypeAnnotation Read GetItem;Default;
  end;

  TEnumType_MemberArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TEnumTypeMember;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TEnumTypeMember Read GetItem;Default;
  end;

  TEnumType_ValueAnnotationArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TValueAnnotation;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TValueAnnotation Read GetItem;Default;
  end;

  TEnumType_TypeAnnotationArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TTypeAnnotation;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TTypeAnnotation Read GetItem;Default;
  end;

  TFunction_ParameterArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TFunctionParameter;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TFunctionParameter Read GetItem;Default;
  end;

  TFunction_DefiningExpressionArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of TCommandText;
  private
    function GetItem(AIndex: Integer): TCommandText;
    procedure SetItem(AIndex: Integer; const AValue: TCommandText);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    procedure Assign(Source: TPersistent); override;
    property Item[AIndex:Integer] : TCommandText read GetItem write SetItem; default;
  end;

  TFunction_ReturnTypeArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TFunctionReturnType;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TFunctionReturnType Read GetItem;Default;
  end;

  TFunction_ValueAnnotationArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TValueAnnotation;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TValueAnnotation Read GetItem;Default;
  end;

  TFunction_TypeAnnotationArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TTypeAnnotation;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TTypeAnnotation Read GetItem;Default;
  end;

  TFunctionParameter_ValueAnnotationArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TValueAnnotation;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TValueAnnotation Read GetItem;Default;
  end;

  TFunctionParameter_TypeAnnotationArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TTypeAnnotation;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TTypeAnnotation Read GetItem;Default;
  end;

  TRowType = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TRowProperty;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TRowProperty Read GetItem;Default;
  end;

  TEntityKeyElement = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TPropertyRef;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TPropertyRef Read GetItem;Default;
  end;

  TAnnotations_ValueAnnotationArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TValueAnnotation;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TValueAnnotation Read GetItem;Default;
  end;

  TAnnotations_TypeAnnotationArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TTypeAnnotation;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TTypeAnnotation Read GetItem;Default;
  end;

  TTypeAnnotation_PropertyValueArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TPropertyValue;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TPropertyValue Read GetItem;Default;
  end;

  TRecordExpression_PropertyValueArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TPropertyValue;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TPropertyValue Read GetItem;Default;
  end;

  TFunctionReferenceExpression_ParameterArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TFunctionReferenceExpression_Parameter_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TFunctionReferenceExpression_Parameter_Type Read GetItem;Default;
  end;

  TEntityProperty_DocumentationArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TDocumentation;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TDocumentation Read GetItem;Default;
  end;

  TEntityProperty_ValueAnnotationArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TValueAnnotation;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TValueAnnotation Read GetItem;Default;
  end;

  TEntityProperty_TypeAnnotationArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TTypeAnnotation;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TTypeAnnotation Read GetItem;Default;
  end;

  TComplexTypeProperty_DocumentationArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TDocumentation;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TDocumentation Read GetItem;Default;
  end;

  TComplexTypeProperty_ValueAnnotationArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TValueAnnotation;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TValueAnnotation Read GetItem;Default;
  end;

  TComplexTypeProperty_TypeAnnotationArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TTypeAnnotation;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TTypeAnnotation Read GetItem;Default;
  end;

  TFunctionImportParameter_ValueAnnotationArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TValueAnnotation;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TValueAnnotation Read GetItem;Default;
  end;

  TFunctionImportParameter_TypeAnnotationArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TTypeAnnotation;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TTypeAnnotation Read GetItem;Default;
  end;

  EntityContainer_FunctionImport_Type_ReturnTypeArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TFunctionImportReturnType;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TFunctionImportReturnType Read GetItem;Default;
  end;

  EntityContainer_FunctionImport_Type_ParameterArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TFunctionImportParameter;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TFunctionImportParameter Read GetItem;Default;
  end;

  EntityContainer_FunctionImport_Type_ValueAnnotationArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TValueAnnotation;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TValueAnnotation Read GetItem;Default;
  end;

  EntityContainer_FunctionImport_Type_TypeAnnotationArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TTypeAnnotation;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TTypeAnnotation Read GetItem;Default;
  end;

  EntityContainer_EntitySet_Type_ValueAnnotationArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TValueAnnotation;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TValueAnnotation Read GetItem;Default;
  end;

  EntityContainer_EntitySet_Type_TypeAnnotationArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TTypeAnnotation;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TTypeAnnotation Read GetItem;Default;
  end;

  EntityContainer_AssociationSet_Type__EndArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): EntityContainer_AssociationSet_Type_End_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : EntityContainer_AssociationSet_Type_End_Type Read GetItem;Default;
  end;

  EntityContainer_FunctionImportArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): EntityContainer_FunctionImport_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : EntityContainer_FunctionImport_Type Read GetItem;Default;
  end;

  EntityContainer_EntitySetArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): EntityContainer_EntitySet_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : EntityContainer_EntitySet_Type Read GetItem;Default;
  end;

  EntityContainer_AssociationSetArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): EntityContainer_AssociationSet_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : EntityContainer_AssociationSet_Type Read GetItem;Default;
  end;

  EntityContainer_ValueAnnotationArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TValueAnnotation;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TValueAnnotation Read GetItem;Default;
  end;

  EntityContainer_TypeAnnotationArray = class(TBaseObjectArrayRemotable)
  private
    function GetItem(AIndex: Integer): TTypeAnnotation;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    property Item[AIndex:Integer] : TTypeAnnotation Read GetItem;Default;
  end;

Implementation

uses metadata_repository, record_rtti;

{ GSchemaBodyElements }

constructor GSchemaBodyElements.Create();
begin
  inherited Create();
  FUsing := GSchemaBodyElements_UsingArray.Create();
  FAssociation := GSchemaBodyElements_AssociationArray.Create();
  FComplexType := GSchemaBodyElements_ComplexTypeArray.Create();
  FEntityType := GSchemaBodyElements_EntityTypeArray.Create();
  FEnumType := GSchemaBodyElements_EnumTypeArray.Create();
  FValueTerm := GSchemaBodyElements_ValueTermArray.Create();
  F_Function := GSchemaBodyElements__FunctionArray.Create();
  FAnnotations := GSchemaBodyElements_AnnotationsArray.Create();
  FEntityContainer := CSDL.EntityContainer.Create();
end;

procedure GSchemaBodyElements.FreeObjectProperties();
begin
  if Assigned(FUsing) then
    FreeAndNil(FUsing);
  if Assigned(FAssociation) then
    FreeAndNil(FAssociation);
  if Assigned(FComplexType) then
    FreeAndNil(FComplexType);
  if Assigned(FEntityType) then
    FreeAndNil(FEntityType);
  if Assigned(FEnumType) then
    FreeAndNil(FEnumType);
  if Assigned(FValueTerm) then
    FreeAndNil(FValueTerm);
  if Assigned(F_Function) then
    FreeAndNil(F_Function);
  if Assigned(FAnnotations) then
    FreeAndNil(FAnnotations);
  if Assigned(FEntityContainer) then
    FreeAndNil(FEntityContainer);
  inherited FreeObjectProperties();
end;

function GSchemaBodyElements.wstHas_Using() : Boolean;
begin
  Result := ( FUsing <> GSchemaBodyElements_UsingArray(0) );
end;

function GSchemaBodyElements.wstHas_Association() : Boolean;
begin
  Result := ( FAssociation <> GSchemaBodyElements_AssociationArray(0) );
end;

function GSchemaBodyElements.wstHas_ComplexType() : Boolean;
begin
  Result := ( FComplexType <> GSchemaBodyElements_ComplexTypeArray(0) );
end;

function GSchemaBodyElements.wstHas_EntityType() : Boolean;
begin
  Result := ( FEntityType <> GSchemaBodyElements_EntityTypeArray(0) );
end;

function GSchemaBodyElements.wstHas_EnumType() : Boolean;
begin
  Result := ( FEnumType <> GSchemaBodyElements_EnumTypeArray(0) );
end;

function GSchemaBodyElements.wstHas_ValueTerm() : Boolean;
begin
  Result := ( FValueTerm <> GSchemaBodyElements_ValueTermArray(0) );
end;

function GSchemaBodyElements.wstHas__Function() : Boolean;
begin
  Result := ( F_Function <> GSchemaBodyElements__FunctionArray(0) );
end;

function GSchemaBodyElements.wstHas_Annotations() : Boolean;
begin
  Result := ( FAnnotations <> GSchemaBodyElements_AnnotationsArray(0) );
end;

{ TSchema }

constructor TSchema.Create();
begin
  inherited Create();
  FUsing := GSchemaBodyElements_UsingArray.Create();
  FAssociation := GSchemaBodyElements_AssociationArray.Create();
  FComplexType := GSchemaBodyElements_ComplexTypeArray.Create();
  FEntityType := GSchemaBodyElements_EntityTypeArray.Create();
  FEnumType := GSchemaBodyElements_EnumTypeArray.Create();
  FValueTerm := GSchemaBodyElements_ValueTermArray.Create();
  F_Function := GSchemaBodyElements__FunctionArray.Create();
  FAnnotations := GSchemaBodyElements_AnnotationsArray.Create();
  FEntityContainer := CSDL.EntityContainer.Create();
end;

procedure TSchema.FreeObjectProperties();
begin
  if Assigned(FUsing) then
    FreeAndNil(FUsing);
  if Assigned(FAssociation) then
    FreeAndNil(FAssociation);
  if Assigned(FComplexType) then
    FreeAndNil(FComplexType);
  if Assigned(FEntityType) then
    FreeAndNil(FEntityType);
  if Assigned(FEnumType) then
    FreeAndNil(FEnumType);
  if Assigned(FValueTerm) then
    FreeAndNil(FValueTerm);
  if Assigned(F_Function) then
    FreeAndNil(F_Function);
  if Assigned(FAnnotations) then
    FreeAndNil(FAnnotations);
  if Assigned(FEntityContainer) then
    FreeAndNil(FEntityContainer);
  inherited FreeObjectProperties();
end;

function TSchema.wstHas_Namespace() : Boolean;
begin
  Result := ( FNamespace <> '' );
end;

function TSchema.wstHas_Alias() : Boolean;
begin
  Result := ( FAlias <> '' );
end;

function TSchema.wstHas_Using() : Boolean;
begin
  Result := ( FUsing <> GSchemaBodyElements_UsingArray(0) );
end;

function TSchema.wstHas_Association() : Boolean;
begin
  Result := ( FAssociation <> GSchemaBodyElements_AssociationArray(0) );
end;

function TSchema.wstHas_ComplexType() : Boolean;
begin
  Result := ( FComplexType <> GSchemaBodyElements_ComplexTypeArray(0) );
end;

function TSchema.wstHas_EntityType() : Boolean;
begin
  Result := ( FEntityType <> GSchemaBodyElements_EntityTypeArray(0) );
end;

function TSchema.wstHas_EnumType() : Boolean;
begin
  Result := ( FEnumType <> GSchemaBodyElements_EnumTypeArray(0) );
end;

function TSchema.wstHas_ValueTerm() : Boolean;
begin
  Result := ( FValueTerm <> GSchemaBodyElements_ValueTermArray(0) );
end;

function TSchema.wstHas__Function() : Boolean;
begin
  Result := ( F_Function <> GSchemaBodyElements__FunctionArray(0) );
end;

function TSchema.wstHas_Annotations() : Boolean;
begin
  Result := ( FAnnotations <> GSchemaBodyElements_AnnotationsArray(0) );
end;

{ TDocumentation }

constructor TDocumentation.Create();
begin
  inherited Create();
  FSummary := TText.Create();
  FLongDescription := TText.Create();
end;

procedure TDocumentation.FreeObjectProperties();
begin
  if Assigned(FSummary) then
    FreeAndNil(FSummary);
  if Assigned(FLongDescription) then
    FreeAndNil(FLongDescription);
  inherited FreeObjectProperties();
end;

function TDocumentation.wstHas_Summary() : Boolean;
begin
  Result := ( FSummary <> nil );
end;

function TDocumentation.wstHas_LongDescription() : Boolean;
begin
  Result := ( FLongDescription <> nil );
end;

{ GEmptyElementExtensibility }

constructor GEmptyElementExtensibility.Create();
begin
  inherited Create();
  FDocumentation := TDocumentation.Create();
end;

procedure GEmptyElementExtensibility.FreeObjectProperties();
begin
  if Assigned(FDocumentation) then
    FreeAndNil(FDocumentation);
  inherited FreeObjectProperties();
end;

function GEmptyElementExtensibility.wstHas_Documentation() : Boolean;
begin
  Result := ( FDocumentation <> nil );
end;

{ TUsing }

constructor TUsing.Create();
begin
  inherited Create();
  FDocumentation := TDocumentation.Create();
end;

procedure TUsing.FreeObjectProperties();
begin
  if Assigned(FDocumentation) then
    FreeAndNil(FDocumentation);
  inherited FreeObjectProperties();
end;

function TUsing.wstHas_Documentation() : Boolean;
begin
  Result := ( FDocumentation <> nil );
end;

{ TAssociation }

constructor TAssociation.Create();
begin
  inherited Create();
  FDocumentation := TDocumentation.Create();
  F_End := TAssociation__EndArray.Create();
  FReferentialConstraint := TConstraint.Create();
end;

procedure TAssociation.FreeObjectProperties();
begin
  if Assigned(FDocumentation) then
    FreeAndNil(FDocumentation);
  if Assigned(F_End) then
    FreeAndNil(F_End);
  if Assigned(FReferentialConstraint) then
    FreeAndNil(FReferentialConstraint);
  inherited FreeObjectProperties();
end;

function TAssociation.wstHas_Documentation() : Boolean;
begin
  Result := ( FDocumentation <> nil );
end;

function TAssociation.wstHas_ReferentialConstraint() : Boolean;
begin
  Result := ( FReferentialConstraint <> nil );
end;

{ TComplexType }

constructor TComplexType.Create();
begin
  inherited Create();
  FDocumentation := TDocumentation.Create();
  F_Property := TComplexType__PropertyArray.Create();
  FValueAnnotation := TComplexType_ValueAnnotationArray.Create();
  FTypeAnnotation := TComplexType_TypeAnnotationArray.Create();
end;

procedure TComplexType.FreeObjectProperties();
begin
  if Assigned(FDocumentation) then
    FreeAndNil(FDocumentation);
  if Assigned(F_Property) then
    FreeAndNil(F_Property);
  if Assigned(FValueAnnotation) then
    FreeAndNil(FValueAnnotation);
  if Assigned(FTypeAnnotation) then
    FreeAndNil(FTypeAnnotation);
  inherited FreeObjectProperties();
end;

function TComplexType.wstHas_Documentation() : Boolean;
begin
  Result := ( FDocumentation <> nil );
end;

function TComplexType.wstHas__Property() : Boolean;
begin
  Result := ( F_Property <> TComplexType__PropertyArray(0) );
end;

function TComplexType.wstHas_ValueAnnotation() : Boolean;
begin
  Result := ( FValueAnnotation <> TComplexType_ValueAnnotationArray(0) );
end;

function TComplexType.wstHas_TypeAnnotation() : Boolean;
begin
  Result := ( FTypeAnnotation <> TComplexType_TypeAnnotationArray(0) );
end;

function TComplexType.wstHas_TypeAccess() : Boolean;
begin
  Result := True;
end;

{ TConstraint }

constructor TConstraint.Create();
begin
  inherited Create();
  FDocumentation := TDocumentation.Create();
  FPrincipal := TReferentialConstraintRoleElement.Create();
  FDependent := TReferentialConstraintRoleElement.Create();
end;

procedure TConstraint.FreeObjectProperties();
begin
  if Assigned(FDocumentation) then
    FreeAndNil(FDocumentation);
  if Assigned(FPrincipal) then
    FreeAndNil(FPrincipal);
  if Assigned(FDependent) then
    FreeAndNil(FDependent);
  inherited FreeObjectProperties();
end;

function TConstraint.wstHas_Documentation() : Boolean;
begin
  Result := ( FDocumentation <> nil );
end;

{ TReferentialConstraintRoleElement }

constructor TReferentialConstraintRoleElement.Create();
begin
  inherited Create();
  FPropertyRef := TReferentialConstraintRoleElement_PropertyRefArray.Create();
end;

procedure TReferentialConstraintRoleElement.FreeObjectProperties();
begin
  if Assigned(FPropertyRef) then
    FreeAndNil(FPropertyRef);
  inherited FreeObjectProperties();
end;

{ TNavigationProperty }

constructor TNavigationProperty.Create();
begin
  inherited Create();
  FDocumentation := TDocumentation.Create();
  FValueAnnotation := TNavigationProperty_ValueAnnotationArray.Create();
  FTypeAnnotation := TNavigationProperty_TypeAnnotationArray.Create();
end;

procedure TNavigationProperty.FreeObjectProperties();
begin
  if Assigned(FDocumentation) then
    FreeAndNil(FDocumentation);
  if Assigned(FValueAnnotation) then
    FreeAndNil(FValueAnnotation);
  if Assigned(FTypeAnnotation) then
    FreeAndNil(FTypeAnnotation);
  inherited FreeObjectProperties();
end;

function TNavigationProperty.wstHas_Documentation() : Boolean;
begin
  Result := ( FDocumentation <> nil );
end;

function TNavigationProperty.wstHas_ValueAnnotation() : Boolean;
begin
  Result := ( FValueAnnotation <> TNavigationProperty_ValueAnnotationArray(0) );
end;

function TNavigationProperty.wstHas_TypeAnnotation() : Boolean;
begin
  Result := ( FTypeAnnotation <> TNavigationProperty_TypeAnnotationArray(0) );
end;

function TNavigationProperty.wstHas_ContainsTarget() : Boolean;
begin
  Result := ( FContainsTarget <> boolean(0) );
end;

function TNavigationProperty.wstHas_GetterAccess() : Boolean;
begin
  Result := True;
end;

function TNavigationProperty.wstHas_SetterAccess() : Boolean;
begin
  Result := True;
end;

function TDerivableTypeAttributes.wstHas_BaseType() : Boolean;
begin
  Result := ( FBaseType <> '' );
end;

function TDerivableTypeAttributes.wstHas__Abstract() : Boolean;
begin
  Result := ( F_Abstract <> boolean(0) );
end;

{ TEntityType }

constructor TEntityType.Create();
begin
  inherited Create();
  FDocumentation := TDocumentation.Create();
  FKey := TEntityKeyElement.Create();
  F_Property := TEntityType__PropertyArray.Create();
  FNavigationProperty := TEntityType_NavigationPropertyArray.Create();
  FValueAnnotation := TEntityType_ValueAnnotationArray.Create();
  FTypeAnnotation := TEntityType_TypeAnnotationArray.Create();
end;

procedure TEntityType.FreeObjectProperties();
begin
  if Assigned(FDocumentation) then
    FreeAndNil(FDocumentation);
  if Assigned(FKey) then
    FreeAndNil(FKey);
  if Assigned(F_Property) then
    FreeAndNil(F_Property);
  if Assigned(FNavigationProperty) then
    FreeAndNil(FNavigationProperty);
  if Assigned(FValueAnnotation) then
    FreeAndNil(FValueAnnotation);
  if Assigned(FTypeAnnotation) then
    FreeAndNil(FTypeAnnotation);
  inherited FreeObjectProperties();
end;

function TEntityType.wstHas_Documentation() : Boolean;
begin
  Result := ( FDocumentation <> nil );
end;

function TEntityType.wstHas_Key() : Boolean;
begin
  Result := ( FKey <> TEntityKeyElement(0) );
end;

function TEntityType.wstHas__Property() : Boolean;
begin
  Result := ( F_Property <> TEntityType__PropertyArray(0) );
end;

function TEntityType.wstHas_NavigationProperty() : Boolean;
begin
  Result := ( FNavigationProperty <> TEntityType_NavigationPropertyArray(0) );
end;

function TEntityType.wstHas_ValueAnnotation() : Boolean;
begin
  Result := ( FValueAnnotation <> TEntityType_ValueAnnotationArray(0) );
end;

function TEntityType.wstHas_TypeAnnotation() : Boolean;
begin
  Result := ( FTypeAnnotation <> TEntityType_TypeAnnotationArray(0) );
end;

function TEntityType.wstHas_OpenType() : Boolean;
begin
  Result := ( FOpenType <> boolean(0) );
end;

function TEntityType.wstHas_TypeAccess() : Boolean;
begin
  Result := True;
end;

function TEntityType.wstHas_BaseType() : Boolean;
begin
  Result := ( FBaseType <> '' );
end;

function TEntityType.wstHas__Abstract() : Boolean;
begin
  Result := ( F_Abstract <> boolean(0) );
end;

{ TEnumTypeMember }

constructor TEnumTypeMember.Create();
begin
  inherited Create();
  FDocumentation := TDocumentation.Create();
end;

procedure TEnumTypeMember.FreeObjectProperties();
begin
  if Assigned(FDocumentation) then
    FreeAndNil(FDocumentation);
  inherited FreeObjectProperties();
end;

function TEnumTypeMember.wstHas_Value() : Boolean;
begin
  Result := ( FValue <> Int64(0) );
end;

function TEnumTypeMember.wstHas_Documentation() : Boolean;
begin
  Result := ( FDocumentation <> nil );
end;

{ TEnumType }

constructor TEnumType.Create();
begin
  inherited Create();
  FDocumentation := TDocumentation.Create();
  FMember := TEnumType_MemberArray.Create();
  FValueAnnotation := TEnumType_ValueAnnotationArray.Create();
  FTypeAnnotation := TEnumType_TypeAnnotationArray.Create();
end;

procedure TEnumType.FreeObjectProperties();
begin
  if Assigned(FDocumentation) then
    FreeAndNil(FDocumentation);
  if Assigned(FMember) then
    FreeAndNil(FMember);
  if Assigned(FValueAnnotation) then
    FreeAndNil(FValueAnnotation);
  if Assigned(FTypeAnnotation) then
    FreeAndNil(FTypeAnnotation);
  inherited FreeObjectProperties();
end;

function TEnumType.wstHas_Documentation() : Boolean;
begin
  Result := ( FDocumentation <> nil );
end;

function TEnumType.wstHas_Member() : Boolean;
begin
  Result := ( FMember <> TEnumType_MemberArray(0) );
end;

function TEnumType.wstHas_ValueAnnotation() : Boolean;
begin
  Result := ( FValueAnnotation <> TEnumType_ValueAnnotationArray(0) );
end;

function TEnumType.wstHas_TypeAnnotation() : Boolean;
begin
  Result := ( FTypeAnnotation <> TEnumType_TypeAnnotationArray(0) );
end;

function TEnumType.wstHas_IsFlags() : Boolean;
begin
  Result := ( FIsFlags <> boolean(0) );
end;

function TEnumType.wstHas_UnderlyingType() : Boolean;
begin
  Result := ( FUnderlyingType <> '' );
end;

function TEnumType.wstHas_TypeAccess() : Boolean;
begin
  Result := True;
end;

function TFacetAttributes.wstHas_Nullable() : Boolean;
begin
  Result := ( FNullable <> boolean(0) );
end;

function TFacetAttributes.wstHas_DefaultValue() : Boolean;
begin
  Result := ( FDefaultValue <> '' );
end;

function TFacetAttributes.wstHas_MaxLength() : Boolean;
begin
  Result := ( FMaxLength <> '' );
end;

function TFacetAttributes.wstHas_FixedLength() : Boolean;
begin
  Result := ( FFixedLength <> TIsFixedLengthFacet(0) );
end;

function TFacetAttributes.wstHas_Precision() : Boolean;
begin
  Result := ( FPrecision <> TPrecisionFacet(0) );
end;

function TFacetAttributes.wstHas_Scale() : Boolean;
begin
  Result := ( FScale <> TScaleFacet(0) );
end;

function TFacetAttributes.wstHas_Unicode() : Boolean;
begin
  Result := ( FUnicode <> TIsUnicodeFacet(0) );
end;

function TFacetAttributes.wstHas_Collation() : Boolean;
begin
  Result := ( FCollation <> '' );
end;

function TFacetAttributes.wstHas_SRID() : Boolean;
begin
  Result := ( FSRID <> '' );
end;

{ TFunction }

constructor TFunction.Create();
begin
  inherited Create();
  FDocumentation := TDocumentation.Create();
  FParameter := TFunction_ParameterArray.Create();
  FDefiningExpression := TFunction_DefiningExpressionArray.Create();
  FReturnType := TFunction_ReturnTypeArray.Create();
  FValueAnnotation := TFunction_ValueAnnotationArray.Create();
  FTypeAnnotation := TFunction_TypeAnnotationArray.Create();
end;

procedure TFunction.FreeObjectProperties();
begin
  if Assigned(FDocumentation) then
    FreeAndNil(FDocumentation);
  if Assigned(FParameter) then
    FreeAndNil(FParameter);
  if Assigned(FDefiningExpression) then
    FreeAndNil(FDefiningExpression);
  if Assigned(FReturnType) then
    FreeAndNil(FReturnType);
  if Assigned(FValueAnnotation) then
    FreeAndNil(FValueAnnotation);
  if Assigned(FTypeAnnotation) then
    FreeAndNil(FTypeAnnotation);
  inherited FreeObjectProperties();
end;

function TFunction.wstHas_Documentation() : Boolean;
begin
  Result := ( FDocumentation <> nil );
end;

function TFunction.wstHas_Parameter() : Boolean;
begin
  Result := ( FParameter <> TFunction_ParameterArray(0) );
end;

function TFunction.wstHas_DefiningExpression() : Boolean;
begin
  Result := ( FDefiningExpression <> TFunction_DefiningExpressionArray(0) );
end;

function TFunction.wstHas_ReturnType() : Boolean;
begin
  Result := ( FReturnType <> TFunction_ReturnTypeArray(0) );
end;

function TFunction.wstHas_ValueAnnotation() : Boolean;
begin
  Result := ( FValueAnnotation <> TFunction_ValueAnnotationArray(0) );
end;

function TFunction.wstHas_TypeAnnotation() : Boolean;
begin
  Result := ( FTypeAnnotation <> TFunction_TypeAnnotationArray(0) );
end;

function TFunction.wstHas_ReturnTypeAtt() : Boolean;
begin
  Result := ( FReturnTypeAtt <> '' );
end;

function TFunction.wstHas_Nullable() : Boolean;
begin
  Result := ( FNullable <> boolean(0) );
end;

function TFunction.wstHas_DefaultValue() : Boolean;
begin
  Result := ( FDefaultValue <> '' );
end;

function TFunction.wstHas_MaxLength() : Boolean;
begin
  Result := ( FMaxLength <> '' );
end;

function TFunction.wstHas_FixedLength() : Boolean;
begin
  Result := ( FFixedLength <> TIsFixedLengthFacet(0) );
end;

function TFunction.wstHas_Precision() : Boolean;
begin
  Result := ( FPrecision <> TPrecisionFacet(0) );
end;

function TFunction.wstHas_Scale() : Boolean;
begin
  Result := ( FScale <> TScaleFacet(0) );
end;

function TFunction.wstHas_Unicode() : Boolean;
begin
  Result := ( FUnicode <> TIsUnicodeFacet(0) );
end;

function TFunction.wstHas_Collation() : Boolean;
begin
  Result := ( FCollation <> '' );
end;

function TFunction.wstHas_SRID() : Boolean;
begin
  Result := ( FSRID <> '' );
end;

{ TFunctionParameter }

constructor TFunctionParameter.Create();
begin
  inherited Create();
  FCollectionType := TCollectionType.Create();
  FReferenceType := TReferenceType.Create();
  FRowType := TRowType.Create();
  FValueAnnotation := TFunctionParameter_ValueAnnotationArray.Create();
  FTypeAnnotation := TFunctionParameter_TypeAnnotationArray.Create();
end;

procedure TFunctionParameter.FreeObjectProperties();
begin
  if Assigned(FCollectionType) then
    FreeAndNil(FCollectionType);
  if Assigned(FReferenceType) then
    FreeAndNil(FReferenceType);
  if Assigned(FRowType) then
    FreeAndNil(FRowType);
  if Assigned(FValueAnnotation) then
    FreeAndNil(FValueAnnotation);
  if Assigned(FTypeAnnotation) then
    FreeAndNil(FTypeAnnotation);
  inherited FreeObjectProperties();
end;

function TFunctionParameter.wstHas_CollectionType() : Boolean;
begin
  Result := ( FCollectionType <> nil );
end;

function TFunctionParameter.wstHas_ReferenceType() : Boolean;
begin
  Result := ( FReferenceType <> nil );
end;

function TFunctionParameter.wstHas_RowType() : Boolean;
begin
  Result := ( FRowType <> TRowType(0) );
end;

function TFunctionParameter.wstHas_ValueAnnotation() : Boolean;
begin
  Result := ( FValueAnnotation <> TFunctionParameter_ValueAnnotationArray(0) );
end;

function TFunctionParameter.wstHas_TypeAnnotation() : Boolean;
begin
  Result := ( FTypeAnnotation <> TFunctionParameter_TypeAnnotationArray(0) );
end;

function TFunctionParameter.wstHas__Type() : Boolean;
begin
  Result := ( F_Type <> '' );
end;

function TFunctionParameter.wstHas_Nullable() : Boolean;
begin
  Result := ( FNullable <> boolean(0) );
end;

function TFunctionParameter.wstHas_DefaultValue() : Boolean;
begin
  Result := ( FDefaultValue <> '' );
end;

function TFunctionParameter.wstHas_MaxLength() : Boolean;
begin
  Result := ( FMaxLength <> '' );
end;

function TFunctionParameter.wstHas_FixedLength() : Boolean;
begin
  Result := ( FFixedLength <> TIsFixedLengthFacet(0) );
end;

function TFunctionParameter.wstHas_Precision() : Boolean;
begin
  Result := ( FPrecision <> TPrecisionFacet(0) );
end;

function TFunctionParameter.wstHas_Scale() : Boolean;
begin
  Result := ( FScale <> TScaleFacet(0) );
end;

function TFunctionParameter.wstHas_Unicode() : Boolean;
begin
  Result := ( FUnicode <> TIsUnicodeFacet(0) );
end;

function TFunctionParameter.wstHas_Collation() : Boolean;
begin
  Result := ( FCollation <> '' );
end;

function TFunctionParameter.wstHas_SRID() : Boolean;
begin
  Result := ( FSRID <> '' );
end;

{ TCollectionType }

constructor TCollectionType.Create();
begin
  inherited Create();
  FReferenceType := TReferenceType.Create();
  FRowType := TRowType.Create();
  FTypeRef := TTypeRef.Create();
end;

procedure TCollectionType.FreeObjectProperties();
begin
  if Assigned(FCollectionType) then
    FreeAndNil(FCollectionType);
  if Assigned(FReferenceType) then
    FreeAndNil(FReferenceType);
  if Assigned(FRowType) then
    FreeAndNil(FRowType);
  if Assigned(FTypeRef) then
    FreeAndNil(FTypeRef);
  inherited FreeObjectProperties();
end;

function TCollectionType.wstHas_CollectionType() : Boolean;
begin
  Result := ( FCollectionType <> nil );
end;

function TCollectionType.wstHas_ReferenceType() : Boolean;
begin
  Result := ( FReferenceType <> nil );
end;

function TCollectionType.wstHas_RowType() : Boolean;
begin
  Result := ( FRowType <> TRowType(0) );
end;

function TCollectionType.wstHas_TypeRef() : Boolean;
begin
  Result := ( FTypeRef <> nil );
end;

function TCollectionType.wstHas_ElementType() : Boolean;
begin
  Result := ( FElementType <> '' );
end;

function TCollectionType.wstHas_Nullable() : Boolean;
begin
  Result := ( FNullable <> boolean(0) );
end;

function TCollectionType.wstHas_DefaultValue() : Boolean;
begin
  Result := ( FDefaultValue <> '' );
end;

function TCollectionType.wstHas_MaxLength() : Boolean;
begin
  Result := ( FMaxLength <> '' );
end;

function TCollectionType.wstHas_FixedLength() : Boolean;
begin
  Result := ( FFixedLength <> TIsFixedLengthFacet(0) );
end;

function TCollectionType.wstHas_Precision() : Boolean;
begin
  Result := ( FPrecision <> TPrecisionFacet(0) );
end;

function TCollectionType.wstHas_Scale() : Boolean;
begin
  Result := ( FScale <> TScaleFacet(0) );
end;

function TCollectionType.wstHas_Unicode() : Boolean;
begin
  Result := ( FUnicode <> TIsUnicodeFacet(0) );
end;

function TCollectionType.wstHas_Collation() : Boolean;
begin
  Result := ( FCollation <> '' );
end;

function TCollectionType.wstHas_SRID() : Boolean;
begin
  Result := ( FSRID <> '' );
end;

{ TTypeRef }

constructor TTypeRef.Create();
begin
  inherited Create();
  FDocumentation := TDocumentation.Create();
end;

procedure TTypeRef.FreeObjectProperties();
begin
  if Assigned(FDocumentation) then
    FreeAndNil(FDocumentation);
  inherited FreeObjectProperties();
end;

function TTypeRef.wstHas_Documentation() : Boolean;
begin
  Result := ( FDocumentation <> nil );
end;

function TTypeRef.wstHas_Nullable() : Boolean;
begin
  Result := ( FNullable <> boolean(0) );
end;

function TTypeRef.wstHas_DefaultValue() : Boolean;
begin
  Result := ( FDefaultValue <> '' );
end;

function TTypeRef.wstHas_MaxLength() : Boolean;
begin
  Result := ( FMaxLength <> '' );
end;

function TTypeRef.wstHas_FixedLength() : Boolean;
begin
  Result := ( FFixedLength <> TIsFixedLengthFacet(0) );
end;

function TTypeRef.wstHas_Precision() : Boolean;
begin
  Result := ( FPrecision <> TPrecisionFacet(0) );
end;

function TTypeRef.wstHas_Scale() : Boolean;
begin
  Result := ( FScale <> TScaleFacet(0) );
end;

function TTypeRef.wstHas_Unicode() : Boolean;
begin
  Result := ( FUnicode <> TIsUnicodeFacet(0) );
end;

function TTypeRef.wstHas_Collation() : Boolean;
begin
  Result := ( FCollation <> '' );
end;

function TTypeRef.wstHas_SRID() : Boolean;
begin
  Result := ( FSRID <> '' );
end;

{ TReferenceType }

constructor TReferenceType.Create();
begin
  inherited Create();
  FDocumentation := TDocumentation.Create();
end;

procedure TReferenceType.FreeObjectProperties();
begin
  if Assigned(FDocumentation) then
    FreeAndNil(FDocumentation);
  inherited FreeObjectProperties();
end;

function TReferenceType.wstHas_Documentation() : Boolean;
begin
  Result := ( FDocumentation <> nil );
end;

{ TRowProperty }

constructor TRowProperty.Create();
begin
  inherited Create();
  FCollectionType := TCollectionType.Create();
  FReferenceType := TReferenceType.Create();
  FRowType := TRowType.Create();
end;

procedure TRowProperty.FreeObjectProperties();
begin
  if Assigned(FCollectionType) then
    FreeAndNil(FCollectionType);
  if Assigned(FReferenceType) then
    FreeAndNil(FReferenceType);
  if Assigned(FRowType) then
    FreeAndNil(FRowType);
  inherited FreeObjectProperties();
end;

function TRowProperty.wstHas_CollectionType() : Boolean;
begin
  Result := ( FCollectionType <> nil );
end;

function TRowProperty.wstHas_ReferenceType() : Boolean;
begin
  Result := ( FReferenceType <> nil );
end;

function TRowProperty.wstHas_RowType() : Boolean;
begin
  Result := ( FRowType <> TRowType(0) );
end;

function TRowProperty.wstHas__Type() : Boolean;
begin
  Result := ( F_Type <> '' );
end;

function TRowProperty.wstHas_Nullable() : Boolean;
begin
  Result := ( FNullable <> boolean(0) );
end;

function TRowProperty.wstHas_DefaultValue() : Boolean;
begin
  Result := ( FDefaultValue <> '' );
end;

function TRowProperty.wstHas_MaxLength() : Boolean;
begin
  Result := ( FMaxLength <> '' );
end;

function TRowProperty.wstHas_FixedLength() : Boolean;
begin
  Result := ( FFixedLength <> TIsFixedLengthFacet(0) );
end;

function TRowProperty.wstHas_Precision() : Boolean;
begin
  Result := ( FPrecision <> TPrecisionFacet(0) );
end;

function TRowProperty.wstHas_Scale() : Boolean;
begin
  Result := ( FScale <> TScaleFacet(0) );
end;

function TRowProperty.wstHas_Unicode() : Boolean;
begin
  Result := ( FUnicode <> TIsUnicodeFacet(0) );
end;

function TRowProperty.wstHas_Collation() : Boolean;
begin
  Result := ( FCollation <> '' );
end;

function TRowProperty.wstHas_SRID() : Boolean;
begin
  Result := ( FSRID <> '' );
end;

{ TFunctionReturnType }

constructor TFunctionReturnType.Create();
begin
  inherited Create();
  FCollectionType := TCollectionType.Create();
  FReferenceType := TReferenceType.Create();
  FRowType := TRowType.Create();
end;

procedure TFunctionReturnType.FreeObjectProperties();
begin
  if Assigned(FCollectionType) then
    FreeAndNil(FCollectionType);
  if Assigned(FReferenceType) then
    FreeAndNil(FReferenceType);
  if Assigned(FRowType) then
    FreeAndNil(FRowType);
  inherited FreeObjectProperties();
end;

function TFunctionReturnType.wstHas_CollectionType() : Boolean;
begin
  Result := ( FCollectionType <> nil );
end;

function TFunctionReturnType.wstHas_ReferenceType() : Boolean;
begin
  Result := ( FReferenceType <> nil );
end;

function TFunctionReturnType.wstHas_RowType() : Boolean;
begin
  Result := ( FRowType <> TRowType(0) );
end;

function TFunctionReturnType.wstHas__Type() : Boolean;
begin
  Result := ( F_Type <> '' );
end;

function TFunctionReturnType.wstHas_Nullable() : Boolean;
begin
  Result := ( FNullable <> boolean(0) );
end;

function TFunctionReturnType.wstHas_DefaultValue() : Boolean;
begin
  Result := ( FDefaultValue <> '' );
end;

function TFunctionReturnType.wstHas_MaxLength() : Boolean;
begin
  Result := ( FMaxLength <> '' );
end;

function TFunctionReturnType.wstHas_FixedLength() : Boolean;
begin
  Result := ( FFixedLength <> TIsFixedLengthFacet(0) );
end;

function TFunctionReturnType.wstHas_Precision() : Boolean;
begin
  Result := ( FPrecision <> TPrecisionFacet(0) );
end;

function TFunctionReturnType.wstHas_Scale() : Boolean;
begin
  Result := ( FScale <> TScaleFacet(0) );
end;

function TFunctionReturnType.wstHas_Unicode() : Boolean;
begin
  Result := ( FUnicode <> TIsUnicodeFacet(0) );
end;

function TFunctionReturnType.wstHas_Collation() : Boolean;
begin
  Result := ( FCollation <> '' );
end;

function TFunctionReturnType.wstHas_SRID() : Boolean;
begin
  Result := ( FSRID <> '' );
end;

function TFunctionImportReturnType.wstHas__Type() : Boolean;
begin
  Result := ( F_Type <> '' );
end;

function TFunctionImportReturnType.wstHas_EntitySet() : Boolean;
begin
  Result := ( FEntitySet <> '' );
end;

function TFunctionImportReturnType.wstHas_EntitySetPath() : Boolean;
begin
  Result := ( FEntitySetPath <> '' );
end;

{ TAnnotations }

constructor TAnnotations.Create();
begin
  inherited Create();
  FValueAnnotation := TAnnotations_ValueAnnotationArray.Create();
  FTypeAnnotation := TAnnotations_TypeAnnotationArray.Create();
end;

procedure TAnnotations.FreeObjectProperties();
begin
  if Assigned(FValueAnnotation) then
    FreeAndNil(FValueAnnotation);
  if Assigned(FTypeAnnotation) then
    FreeAndNil(FTypeAnnotation);
  inherited FreeObjectProperties();
end;

function TAnnotations.wstHas_ValueAnnotation() : Boolean;
begin
  Result := ( FValueAnnotation <> TAnnotations_ValueAnnotationArray(0) );
end;

function TAnnotations.wstHas_TypeAnnotation() : Boolean;
begin
  Result := ( FTypeAnnotation <> TAnnotations_TypeAnnotationArray(0) );
end;

function TAnnotations.wstHas_Qualifier() : Boolean;
begin
  Result := ( FQualifier <> '' );
end;

{ GInlineExpressions }

constructor GInlineExpressions.Create();
begin
  inherited Create();
  FBinary := TBase16StringRemotable.Create();
  FTime := TTimeRemotable.Create();
  FDateTime := TDateTimeRemotable.Create();
  FDateTimeOffset := TDateTimeRemotable.Create();
end;

procedure GInlineExpressions.FreeObjectProperties();
begin
  if Assigned(FBinary) then
    FreeAndNil(FBinary);
  if Assigned(FTime) then
    FreeAndNil(FTime);
  if Assigned(FDateTime) then
    FreeAndNil(FDateTime);
  if Assigned(FDateTimeOffset) then
    FreeAndNil(FDateTimeOffset);
  inherited FreeObjectProperties();
end;

function GInlineExpressions.wstHas__String() : Boolean;
begin
  Result := ( F_String <> '' );
end;

function GInlineExpressions.wstHas_Binary() : Boolean;
begin
  Result := ( FBinary <> nil );
end;

function GInlineExpressions.wstHas_Int() : Boolean;
begin
  Result := ( FInt <> integer(0) );
end;

function GInlineExpressions.wstHas_Float() : Boolean;
begin
  Result := ( FFloat <> 0 );
end;

function GInlineExpressions.wstHas_Guid() : Boolean;
begin
  Result := ( FGuid <> '' );
end;

function GInlineExpressions.wstHas_Decimal() : Boolean;
begin
  Result := ( FDecimal <> 0 );
end;

function GInlineExpressions.wstHas_Bool() : Boolean;
begin
  Result := ( FBool <> boolean(0) );
end;

function GInlineExpressions.wstHas_Time() : Boolean;
begin
  Result := ( FTime <> nil );
end;

function GInlineExpressions.wstHas_DateTime() : Boolean;
begin
  Result := ( FDateTime <> nil );
end;

function GInlineExpressions.wstHas_DateTimeOffset() : Boolean;
begin
  Result := ( FDateTimeOffset <> nil );
end;

function GInlineExpressions.wstHas_Path() : Boolean;
begin
  Result := ( FPath <> '' );
end;

{ TValueAnnotation }

constructor TValueAnnotation.Create();
begin
  inherited Create();
  FDocumentation := TDocumentation.Create();
  FBinary := TBase16StringRemotable.Create();
  FTime := TTimeRemotable.Create();
  FDateTime := TDateTimeRemotable.Create();
  FDateTimeOffset := TDateTimeRemotable.Create();
end;

procedure TValueAnnotation.FreeObjectProperties();
begin
  if Assigned(FDocumentation) then
    FreeAndNil(FDocumentation);
  if Assigned(FBinary) then
    FreeAndNil(FBinary);
  if Assigned(FTime) then
    FreeAndNil(FTime);
  if Assigned(FDateTime) then
    FreeAndNil(FDateTime);
  if Assigned(FDateTimeOffset) then
    FreeAndNil(FDateTimeOffset);
  inherited FreeObjectProperties();
end;

function TValueAnnotation.wstHas_Documentation() : Boolean;
begin
  Result := ( FDocumentation <> nil );
end;

function TValueAnnotation.wstHas_Qualifier() : Boolean;
begin
  Result := ( FQualifier <> '' );
end;

function TValueAnnotation.wstHas__String() : Boolean;
begin
  Result := ( F_String <> '' );
end;

function TValueAnnotation.wstHas_Binary() : Boolean;
begin
  Result := ( FBinary <> nil );
end;

function TValueAnnotation.wstHas_Int() : Boolean;
begin
  Result := ( FInt <> integer(0) );
end;

function TValueAnnotation.wstHas_Float() : Boolean;
begin
  Result := ( FFloat <> 0 );
end;

function TValueAnnotation.wstHas_Guid() : Boolean;
begin
  Result := ( FGuid <> '' );
end;

function TValueAnnotation.wstHas_Decimal() : Boolean;
begin
  Result := ( FDecimal <> 0 );
end;

function TValueAnnotation.wstHas_Bool() : Boolean;
begin
  Result := ( FBool <> boolean(0) );
end;

function TValueAnnotation.wstHas_Time() : Boolean;
begin
  Result := ( FTime <> nil );
end;

function TValueAnnotation.wstHas_DateTime() : Boolean;
begin
  Result := ( FDateTime <> nil );
end;

function TValueAnnotation.wstHas_DateTimeOffset() : Boolean;
begin
  Result := ( FDateTimeOffset <> nil );
end;

function TValueAnnotation.wstHas_Path() : Boolean;
begin
  Result := ( FPath <> '' );
end;

{ TTypeAnnotation }

constructor TTypeAnnotation.Create();
begin
  inherited Create();
  FDocumentation := TDocumentation.Create();
  FPropertyValue := TTypeAnnotation_PropertyValueArray.Create();
  FBinary := TBase16StringRemotable.Create();
  FTime := TTimeRemotable.Create();
  FDateTime := TDateTimeRemotable.Create();
  FDateTimeOffset := TDateTimeRemotable.Create();
end;

procedure TTypeAnnotation.FreeObjectProperties();
begin
  if Assigned(FDocumentation) then
    FreeAndNil(FDocumentation);
  if Assigned(FPropertyValue) then
    FreeAndNil(FPropertyValue);
  if Assigned(FBinary) then
    FreeAndNil(FBinary);
  if Assigned(FTime) then
    FreeAndNil(FTime);
  if Assigned(FDateTime) then
    FreeAndNil(FDateTime);
  if Assigned(FDateTimeOffset) then
    FreeAndNil(FDateTimeOffset);
  inherited FreeObjectProperties();
end;

function TTypeAnnotation.wstHas_Documentation() : Boolean;
begin
  Result := ( FDocumentation <> nil );
end;

function TTypeAnnotation.wstHas_PropertyValue() : Boolean;
begin
  Result := ( FPropertyValue <> TTypeAnnotation_PropertyValueArray(0) );
end;

function TTypeAnnotation.wstHas_Qualifier() : Boolean;
begin
  Result := ( FQualifier <> '' );
end;

function TTypeAnnotation.wstHas__String() : Boolean;
begin
  Result := ( F_String <> '' );
end;

function TTypeAnnotation.wstHas_Binary() : Boolean;
begin
  Result := ( FBinary <> nil );
end;

function TTypeAnnotation.wstHas_Int() : Boolean;
begin
  Result := ( FInt <> integer(0) );
end;

function TTypeAnnotation.wstHas_Float() : Boolean;
begin
  Result := ( FFloat <> 0 );
end;

function TTypeAnnotation.wstHas_Guid() : Boolean;
begin
  Result := ( FGuid <> '' );
end;

function TTypeAnnotation.wstHas_Decimal() : Boolean;
begin
  Result := ( FDecimal <> 0 );
end;

function TTypeAnnotation.wstHas_Bool() : Boolean;
begin
  Result := ( FBool <> boolean(0) );
end;

function TTypeAnnotation.wstHas_Time() : Boolean;
begin
  Result := ( FTime <> nil );
end;

function TTypeAnnotation.wstHas_DateTime() : Boolean;
begin
  Result := ( FDateTime <> nil );
end;

function TTypeAnnotation.wstHas_DateTimeOffset() : Boolean;
begin
  Result := ( FDateTimeOffset <> nil );
end;

function TTypeAnnotation.wstHas_Path() : Boolean;
begin
  Result := ( FPath <> '' );
end;

{ TNullExpression }

constructor TNullExpression.Create();
begin
  inherited Create();
  FDocumentation := TDocumentation.Create();
end;

procedure TNullExpression.FreeObjectProperties();
begin
  if Assigned(FDocumentation) then
    FreeAndNil(FDocumentation);
  inherited FreeObjectProperties();
end;

function TNullExpression.wstHas_Documentation() : Boolean;
begin
  Result := ( FDocumentation <> nil );
end;

{ TIfExpression }

constructor TIfExpression.Create();
begin
  inherited Create();
  FDocumentation := TDocumentation.Create();
end;

procedure TIfExpression.FreeObjectProperties();
begin
  if Assigned(FDocumentation) then
    FreeAndNil(FDocumentation);
  inherited FreeObjectProperties();
end;

function TIfExpression.wstHas_Documentation() : Boolean;
begin
  Result := ( FDocumentation <> nil );
end;

{ TRecordExpression }

constructor TRecordExpression.Create();
begin
  inherited Create();
  FDocumentation := TDocumentation.Create();
  FPropertyValue := TRecordExpression_PropertyValueArray.Create();
end;

procedure TRecordExpression.FreeObjectProperties();
begin
  if Assigned(FDocumentation) then
    FreeAndNil(FDocumentation);
  if Assigned(FPropertyValue) then
    FreeAndNil(FPropertyValue);
  inherited FreeObjectProperties();
end;

function TRecordExpression.wstHas_Documentation() : Boolean;
begin
  Result := ( FDocumentation <> nil );
end;

function TRecordExpression.wstHas_PropertyValue() : Boolean;
begin
  Result := ( FPropertyValue <> TRecordExpression_PropertyValueArray(0) );
end;

function TRecordExpression.wstHas__Type() : Boolean;
begin
  Result := ( F_Type <> '' );
end;

{ TPropertyValue }

constructor TPropertyValue.Create();
begin
  inherited Create();
  FDocumentation := TDocumentation.Create();
  FBinary := TBase16StringRemotable.Create();
  FTime := TTimeRemotable.Create();
  FDateTime := TDateTimeRemotable.Create();
  FDateTimeOffset := TDateTimeRemotable.Create();
end;

procedure TPropertyValue.FreeObjectProperties();
begin
  if Assigned(FDocumentation) then
    FreeAndNil(FDocumentation);
  if Assigned(FBinary) then
    FreeAndNil(FBinary);
  if Assigned(FTime) then
    FreeAndNil(FTime);
  if Assigned(FDateTime) then
    FreeAndNil(FDateTime);
  if Assigned(FDateTimeOffset) then
    FreeAndNil(FDateTimeOffset);
  inherited FreeObjectProperties();
end;

function TPropertyValue.wstHas_Documentation() : Boolean;
begin
  Result := ( FDocumentation <> nil );
end;

function TPropertyValue.wstHas__String() : Boolean;
begin
  Result := ( F_String <> '' );
end;

function TPropertyValue.wstHas_Binary() : Boolean;
begin
  Result := ( FBinary <> nil );
end;

function TPropertyValue.wstHas_Int() : Boolean;
begin
  Result := ( FInt <> integer(0) );
end;

function TPropertyValue.wstHas_Float() : Boolean;
begin
  Result := ( FFloat <> 0 );
end;

function TPropertyValue.wstHas_Guid() : Boolean;
begin
  Result := ( FGuid <> '' );
end;

function TPropertyValue.wstHas_Decimal() : Boolean;
begin
  Result := ( FDecimal <> 0 );
end;

function TPropertyValue.wstHas_Bool() : Boolean;
begin
  Result := ( FBool <> boolean(0) );
end;

function TPropertyValue.wstHas_Time() : Boolean;
begin
  Result := ( FTime <> nil );
end;

function TPropertyValue.wstHas_DateTime() : Boolean;
begin
  Result := ( FDateTime <> nil );
end;

function TPropertyValue.wstHas_DateTimeOffset() : Boolean;
begin
  Result := ( FDateTimeOffset <> nil );
end;

function TPropertyValue.wstHas_Path() : Boolean;
begin
  Result := ( FPath <> '' );
end;

{ TCollectionExpression }

constructor TCollectionExpression.Create();
begin
  inherited Create();
  FDocumentation := TDocumentation.Create();
end;

procedure TCollectionExpression.FreeObjectProperties();
begin
  if Assigned(FDocumentation) then
    FreeAndNil(FDocumentation);
  inherited FreeObjectProperties();
end;

function TCollectionExpression.wstHas_Documentation() : Boolean;
begin
  Result := ( FDocumentation <> nil );
end;

{ TAssertTypeExpression }

constructor TAssertTypeExpression.Create();
begin
  inherited Create();
  FDocumentation := TDocumentation.Create();
  FCollectionType := TCollectionType.Create();
  FReferenceType := TReferenceType.Create();
  FRowType := TRowType.Create();
end;

procedure TAssertTypeExpression.FreeObjectProperties();
begin
  if Assigned(FDocumentation) then
    FreeAndNil(FDocumentation);
  if Assigned(FCollectionType) then
    FreeAndNil(FCollectionType);
  if Assigned(FReferenceType) then
    FreeAndNil(FReferenceType);
  if Assigned(FRowType) then
    FreeAndNil(FRowType);
  inherited FreeObjectProperties();
end;

function TAssertTypeExpression.wstHas_Documentation() : Boolean;
begin
  Result := ( FDocumentation <> nil );
end;

function TAssertTypeExpression.wstHas_CollectionType() : Boolean;
begin
  Result := ( FCollectionType <> nil );
end;

function TAssertTypeExpression.wstHas_ReferenceType() : Boolean;
begin
  Result := ( FReferenceType <> nil );
end;

function TAssertTypeExpression.wstHas_RowType() : Boolean;
begin
  Result := ( FRowType <> TRowType(0) );
end;

function TAssertTypeExpression.wstHas__Type() : Boolean;
begin
  Result := ( F_Type <> '' );
end;

function TAssertTypeExpression.wstHas_Nullable() : Boolean;
begin
  Result := ( FNullable <> boolean(0) );
end;

function TAssertTypeExpression.wstHas_DefaultValue() : Boolean;
begin
  Result := ( FDefaultValue <> '' );
end;

function TAssertTypeExpression.wstHas_MaxLength() : Boolean;
begin
  Result := ( FMaxLength <> '' );
end;

function TAssertTypeExpression.wstHas_FixedLength() : Boolean;
begin
  Result := ( FFixedLength <> TIsFixedLengthFacet(0) );
end;

function TAssertTypeExpression.wstHas_Precision() : Boolean;
begin
  Result := ( FPrecision <> TPrecisionFacet(0) );
end;

function TAssertTypeExpression.wstHas_Scale() : Boolean;
begin
  Result := ( FScale <> TScaleFacet(0) );
end;

function TAssertTypeExpression.wstHas_Unicode() : Boolean;
begin
  Result := ( FUnicode <> TIsUnicodeFacet(0) );
end;

function TAssertTypeExpression.wstHas_Collation() : Boolean;
begin
  Result := ( FCollation <> '' );
end;

function TAssertTypeExpression.wstHas_SRID() : Boolean;
begin
  Result := ( FSRID <> '' );
end;

{ TIsTypeExpression }

constructor TIsTypeExpression.Create();
begin
  inherited Create();
  FDocumentation := TDocumentation.Create();
  FCollectionType := TCollectionType.Create();
  FReferenceType := TReferenceType.Create();
  FRowType := TRowType.Create();
end;

procedure TIsTypeExpression.FreeObjectProperties();
begin
  if Assigned(FDocumentation) then
    FreeAndNil(FDocumentation);
  if Assigned(FCollectionType) then
    FreeAndNil(FCollectionType);
  if Assigned(FReferenceType) then
    FreeAndNil(FReferenceType);
  if Assigned(FRowType) then
    FreeAndNil(FRowType);
  inherited FreeObjectProperties();
end;

function TIsTypeExpression.wstHas_Documentation() : Boolean;
begin
  Result := ( FDocumentation <> nil );
end;

function TIsTypeExpression.wstHas_CollectionType() : Boolean;
begin
  Result := ( FCollectionType <> nil );
end;

function TIsTypeExpression.wstHas_ReferenceType() : Boolean;
begin
  Result := ( FReferenceType <> nil );
end;

function TIsTypeExpression.wstHas_RowType() : Boolean;
begin
  Result := ( FRowType <> TRowType(0) );
end;

function TIsTypeExpression.wstHas__Type() : Boolean;
begin
  Result := ( F_Type <> '' );
end;

function TIsTypeExpression.wstHas_Nullable() : Boolean;
begin
  Result := ( FNullable <> boolean(0) );
end;

function TIsTypeExpression.wstHas_DefaultValue() : Boolean;
begin
  Result := ( FDefaultValue <> '' );
end;

function TIsTypeExpression.wstHas_MaxLength() : Boolean;
begin
  Result := ( FMaxLength <> '' );
end;

function TIsTypeExpression.wstHas_FixedLength() : Boolean;
begin
  Result := ( FFixedLength <> TIsFixedLengthFacet(0) );
end;

function TIsTypeExpression.wstHas_Precision() : Boolean;
begin
  Result := ( FPrecision <> TPrecisionFacet(0) );
end;

function TIsTypeExpression.wstHas_Scale() : Boolean;
begin
  Result := ( FScale <> TScaleFacet(0) );
end;

function TIsTypeExpression.wstHas_Unicode() : Boolean;
begin
  Result := ( FUnicode <> TIsUnicodeFacet(0) );
end;

function TIsTypeExpression.wstHas_Collation() : Boolean;
begin
  Result := ( FCollation <> '' );
end;

function TIsTypeExpression.wstHas_SRID() : Boolean;
begin
  Result := ( FSRID <> '' );
end;

{ TFunctionReferenceExpression_Parameter_Type }

constructor TFunctionReferenceExpression_Parameter_Type.Create();
begin
  inherited Create();
  FCollectionType := TCollectionType.Create();
  FReferenceType := TReferenceType.Create();
  FRowType := TRowType.Create();
end;

procedure TFunctionReferenceExpression_Parameter_Type.FreeObjectProperties();
begin
  if Assigned(FCollectionType) then
    FreeAndNil(FCollectionType);
  if Assigned(FReferenceType) then
    FreeAndNil(FReferenceType);
  if Assigned(FRowType) then
    FreeAndNil(FRowType);
  inherited FreeObjectProperties();
end;

function TFunctionReferenceExpression_Parameter_Type.wstHas_CollectionType() : Boolean;
begin
  Result := ( FCollectionType <> nil );
end;

function TFunctionReferenceExpression_Parameter_Type.wstHas_ReferenceType() : Boolean;
begin
  Result := ( FReferenceType <> nil );
end;

function TFunctionReferenceExpression_Parameter_Type.wstHas_RowType() : Boolean;
begin
  Result := ( FRowType <> TRowType(0) );
end;

function TFunctionReferenceExpression_Parameter_Type.wstHas__Type() : Boolean;
begin
  Result := ( F_Type <> '' );
end;

{ TFunctionReferenceExpression }

constructor TFunctionReferenceExpression.Create();
begin
  inherited Create();
  FDocumentation := TDocumentation.Create();
  FParameter := TFunctionReferenceExpression_ParameterArray.Create();
end;

procedure TFunctionReferenceExpression.FreeObjectProperties();
begin
  if Assigned(FDocumentation) then
    FreeAndNil(FDocumentation);
  if Assigned(FParameter) then
    FreeAndNil(FParameter);
  inherited FreeObjectProperties();
end;

function TFunctionReferenceExpression.wstHas_Documentation() : Boolean;
begin
  Result := ( FDocumentation <> nil );
end;

function TFunctionReferenceExpression.wstHas_Parameter() : Boolean;
begin
  Result := ( FParameter <> TFunctionReferenceExpression_ParameterArray(0) );
end;

{ TApplyExpression }

constructor TApplyExpression.Create();
begin
  inherited Create();
  FDocumentation := TDocumentation.Create();
end;

procedure TApplyExpression.FreeObjectProperties();
begin
  if Assigned(FDocumentation) then
    FreeAndNil(FDocumentation);
  inherited FreeObjectProperties();
end;

function TApplyExpression.wstHas_Documentation() : Boolean;
begin
  Result := ( FDocumentation <> nil );
end;

function TApplyExpression.wstHas__Function() : Boolean;
begin
  Result := ( F_Function <> '' );
end;

{ TPropertyReferenceExpression }

constructor TPropertyReferenceExpression.Create();
begin
  inherited Create();
  FDocumentation := TDocumentation.Create();
end;

procedure TPropertyReferenceExpression.FreeObjectProperties();
begin
  if Assigned(FDocumentation) then
    FreeAndNil(FDocumentation);
  inherited FreeObjectProperties();
end;

function TPropertyReferenceExpression.wstHas_Documentation() : Boolean;
begin
  Result := ( FDocumentation <> nil );
end;

{ TValueTermReferenceExpression }

constructor TValueTermReferenceExpression.Create();
begin
  inherited Create();
  FDocumentation := TDocumentation.Create();
end;

procedure TValueTermReferenceExpression.FreeObjectProperties();
begin
  if Assigned(FDocumentation) then
    FreeAndNil(FDocumentation);
  inherited FreeObjectProperties();
end;

function TValueTermReferenceExpression.wstHas_Documentation() : Boolean;
begin
  Result := ( FDocumentation <> nil );
end;

function TValueTermReferenceExpression.wstHas_Qualifier() : Boolean;
begin
  Result := ( FQualifier <> '' );
end;

{ TLabeledElement }

constructor TLabeledElement.Create();
begin
  inherited Create();
  FDocumentation := TDocumentation.Create();
end;

procedure TLabeledElement.FreeObjectProperties();
begin
  if Assigned(FDocumentation) then
    FreeAndNil(FDocumentation);
  inherited FreeObjectProperties();
end;

function TLabeledElement.wstHas_Documentation() : Boolean;
begin
  Result := ( FDocumentation <> nil );
end;

{ TOperations }

constructor TOperations.Create();
begin
  inherited Create();
  FOnDelete := TOnAction.Create();
end;

procedure TOperations.FreeObjectProperties();
begin
  if Assigned(FOnDelete) then
    FreeAndNil(FOnDelete);
  inherited FreeObjectProperties();
end;

function TOperations.wstHas_OnDelete() : Boolean;
begin
  Result := ( FOnDelete <> nil );
end;

{ TAssociationEnd }

constructor TAssociationEnd.Create();
begin
  inherited Create();
  FDocumentation := TDocumentation.Create();
  FOnDelete := TOnAction.Create();
end;

procedure TAssociationEnd.FreeObjectProperties();
begin
  if Assigned(FDocumentation) then
    FreeAndNil(FDocumentation);
  if Assigned(FOnDelete) then
    FreeAndNil(FOnDelete);
  inherited FreeObjectProperties();
end;

function TAssociationEnd.wstHas_Documentation() : Boolean;
begin
  Result := ( FDocumentation <> nil );
end;

function TAssociationEnd.wstHas_Role() : Boolean;
begin
  Result := ( FRole <> '' );
end;

function TAssociationEnd.wstHas_Multiplicity() : Boolean;
begin
  Result := True;
end;

function TAssociationEnd.wstHas_OnDelete() : Boolean;
begin
  Result := ( FOnDelete <> nil );
end;

{ TOnAction }

constructor TOnAction.Create();
begin
  inherited Create();
  FDocumentation := TDocumentation.Create();
end;

procedure TOnAction.FreeObjectProperties();
begin
  if Assigned(FDocumentation) then
    FreeAndNil(FDocumentation);
  inherited FreeObjectProperties();
end;

function TOnAction.wstHas_Documentation() : Boolean;
begin
  Result := ( FDocumentation <> nil );
end;

function TCommonPropertyAttributes.wstHas_Nullable() : Boolean;
begin
  Result := ( FNullable <> boolean(0) );
end;

function TCommonPropertyAttributes.wstHas_DefaultValue() : Boolean;
begin
  Result := ( FDefaultValue <> '' );
end;

function TCommonPropertyAttributes.wstHas_MaxLength() : Boolean;
begin
  Result := ( FMaxLength <> '' );
end;

function TCommonPropertyAttributes.wstHas_FixedLength() : Boolean;
begin
  Result := ( FFixedLength <> TIsFixedLengthFacet(0) );
end;

function TCommonPropertyAttributes.wstHas_Precision() : Boolean;
begin
  Result := ( FPrecision <> TPrecisionFacet(0) );
end;

function TCommonPropertyAttributes.wstHas_Scale() : Boolean;
begin
  Result := ( FScale <> TScaleFacet(0) );
end;

function TCommonPropertyAttributes.wstHas_Unicode() : Boolean;
begin
  Result := ( FUnicode <> TIsUnicodeFacet(0) );
end;

function TCommonPropertyAttributes.wstHas_Collation() : Boolean;
begin
  Result := ( FCollation <> '' );
end;

function TCommonPropertyAttributes.wstHas_SRID() : Boolean;
begin
  Result := ( FSRID <> '' );
end;

function TCommonPropertyAttributes.wstHas_ConcurrencyMode() : Boolean;
begin
  Result := True;
end;

function TCommonPropertyAttributes.wstHas_SetterAccess() : Boolean;
begin
  Result := True;
end;

function TCommonPropertyAttributes.wstHas_GetterAccess() : Boolean;
begin
  Result := True;
end;

{ TEntityProperty }

constructor TEntityProperty.Create();
begin
  inherited Create();
  FDocumentation := TEntityProperty_DocumentationArray.Create();
  FValueAnnotation := TEntityProperty_ValueAnnotationArray.Create();
  FTypeAnnotation := TEntityProperty_TypeAnnotationArray.Create();
end;

procedure TEntityProperty.FreeObjectProperties();
begin
  if Assigned(FDocumentation) then
    FreeAndNil(FDocumentation);
  if Assigned(FValueAnnotation) then
    FreeAndNil(FValueAnnotation);
  if Assigned(FTypeAnnotation) then
    FreeAndNil(FTypeAnnotation);
  inherited FreeObjectProperties();
end;

function TEntityProperty.wstHas_Documentation() : Boolean;
begin
  Result := ( FDocumentation <> TEntityProperty_DocumentationArray(0) );
end;

function TEntityProperty.wstHas_ValueAnnotation() : Boolean;
begin
  Result := ( FValueAnnotation <> TEntityProperty_ValueAnnotationArray(0) );
end;

function TEntityProperty.wstHas_TypeAnnotation() : Boolean;
begin
  Result := ( FTypeAnnotation <> TEntityProperty_TypeAnnotationArray(0) );
end;

function TEntityProperty.wstHas_StoreGeneratedPattern() : Boolean;
begin
  Result := True;
end;

function TEntityProperty.wstHas_Nullable() : Boolean;
begin
  Result := ( FNullable <> boolean(0) );
end;

function TEntityProperty.wstHas_DefaultValue() : Boolean;
begin
  Result := ( FDefaultValue <> '' );
end;

function TEntityProperty.wstHas_MaxLength() : Boolean;
begin
  Result := ( FMaxLength <> '' );
end;

function TEntityProperty.wstHas_FixedLength() : Boolean;
begin
  Result := ( FFixedLength <> TIsFixedLengthFacet(0) );
end;

function TEntityProperty.wstHas_Precision() : Boolean;
begin
  Result := ( FPrecision <> TPrecisionFacet(0) );
end;

function TEntityProperty.wstHas_Scale() : Boolean;
begin
  Result := ( FScale <> TScaleFacet(0) );
end;

function TEntityProperty.wstHas_Unicode() : Boolean;
begin
  Result := ( FUnicode <> TIsUnicodeFacet(0) );
end;

function TEntityProperty.wstHas_Collation() : Boolean;
begin
  Result := ( FCollation <> '' );
end;

function TEntityProperty.wstHas_SRID() : Boolean;
begin
  Result := ( FSRID <> '' );
end;

function TEntityProperty.wstHas_ConcurrencyMode() : Boolean;
begin
  Result := True;
end;

function TEntityProperty.wstHas_SetterAccess() : Boolean;
begin
  Result := True;
end;

function TEntityProperty.wstHas_GetterAccess() : Boolean;
begin
  Result := True;
end;

{ TComplexTypeProperty }

constructor TComplexTypeProperty.Create();
begin
  inherited Create();
  FDocumentation := TComplexTypeProperty_DocumentationArray.Create();
  FValueAnnotation := TComplexTypeProperty_ValueAnnotationArray.Create();
  FTypeAnnotation := TComplexTypeProperty_TypeAnnotationArray.Create();
end;

procedure TComplexTypeProperty.FreeObjectProperties();
begin
  if Assigned(FDocumentation) then
    FreeAndNil(FDocumentation);
  if Assigned(FValueAnnotation) then
    FreeAndNil(FValueAnnotation);
  if Assigned(FTypeAnnotation) then
    FreeAndNil(FTypeAnnotation);
  inherited FreeObjectProperties();
end;

function TComplexTypeProperty.wstHas_Documentation() : Boolean;
begin
  Result := ( FDocumentation <> TComplexTypeProperty_DocumentationArray(0) );
end;

function TComplexTypeProperty.wstHas_ValueAnnotation() : Boolean;
begin
  Result := ( FValueAnnotation <> TComplexTypeProperty_ValueAnnotationArray(0) );
end;

function TComplexTypeProperty.wstHas_TypeAnnotation() : Boolean;
begin
  Result := ( FTypeAnnotation <> TComplexTypeProperty_TypeAnnotationArray(0) );
end;

function TComplexTypeProperty.wstHas_Nullable() : Boolean;
begin
  Result := ( FNullable <> boolean(0) );
end;

function TComplexTypeProperty.wstHas_DefaultValue() : Boolean;
begin
  Result := ( FDefaultValue <> '' );
end;

function TComplexTypeProperty.wstHas_MaxLength() : Boolean;
begin
  Result := ( FMaxLength <> '' );
end;

function TComplexTypeProperty.wstHas_FixedLength() : Boolean;
begin
  Result := ( FFixedLength <> TIsFixedLengthFacet(0) );
end;

function TComplexTypeProperty.wstHas_Precision() : Boolean;
begin
  Result := ( FPrecision <> TPrecisionFacet(0) );
end;

function TComplexTypeProperty.wstHas_Scale() : Boolean;
begin
  Result := ( FScale <> TScaleFacet(0) );
end;

function TComplexTypeProperty.wstHas_Unicode() : Boolean;
begin
  Result := ( FUnicode <> TIsUnicodeFacet(0) );
end;

function TComplexTypeProperty.wstHas_Collation() : Boolean;
begin
  Result := ( FCollation <> '' );
end;

function TComplexTypeProperty.wstHas_SRID() : Boolean;
begin
  Result := ( FSRID <> '' );
end;

function TComplexTypeProperty.wstHas_ConcurrencyMode() : Boolean;
begin
  Result := True;
end;

function TComplexTypeProperty.wstHas_SetterAccess() : Boolean;
begin
  Result := True;
end;

function TComplexTypeProperty.wstHas_GetterAccess() : Boolean;
begin
  Result := True;
end;

{ TValueTerm }

constructor TValueTerm.Create();
begin
  inherited Create();
  FDocumentation := TDocumentation.Create();
  FCollectionType := TCollectionType.Create();
  FReferenceType := TReferenceType.Create();
  FRowType := TRowType.Create();
end;

procedure TValueTerm.FreeObjectProperties();
begin
  if Assigned(FDocumentation) then
    FreeAndNil(FDocumentation);
  if Assigned(FCollectionType) then
    FreeAndNil(FCollectionType);
  if Assigned(FReferenceType) then
    FreeAndNil(FReferenceType);
  if Assigned(FRowType) then
    FreeAndNil(FRowType);
  inherited FreeObjectProperties();
end;

function TValueTerm.wstHas_Documentation() : Boolean;
begin
  Result := ( FDocumentation <> nil );
end;

function TValueTerm.wstHas_CollectionType() : Boolean;
begin
  Result := ( FCollectionType <> nil );
end;

function TValueTerm.wstHas_ReferenceType() : Boolean;
begin
  Result := ( FReferenceType <> nil );
end;

function TValueTerm.wstHas_RowType() : Boolean;
begin
  Result := ( FRowType <> TRowType(0) );
end;

function TValueTerm.wstHas__Type() : Boolean;
begin
  Result := ( F_Type <> '' );
end;

function TValueTerm.wstHas_Nullable() : Boolean;
begin
  Result := ( FNullable <> boolean(0) );
end;

function TValueTerm.wstHas_DefaultValue() : Boolean;
begin
  Result := ( FDefaultValue <> '' );
end;

function TValueTerm.wstHas_MaxLength() : Boolean;
begin
  Result := ( FMaxLength <> '' );
end;

function TValueTerm.wstHas_FixedLength() : Boolean;
begin
  Result := ( FFixedLength <> TIsFixedLengthFacet(0) );
end;

function TValueTerm.wstHas_Precision() : Boolean;
begin
  Result := ( FPrecision <> TPrecisionFacet(0) );
end;

function TValueTerm.wstHas_Scale() : Boolean;
begin
  Result := ( FScale <> TScaleFacet(0) );
end;

function TValueTerm.wstHas_Unicode() : Boolean;
begin
  Result := ( FUnicode <> TIsUnicodeFacet(0) );
end;

function TValueTerm.wstHas_Collation() : Boolean;
begin
  Result := ( FCollation <> '' );
end;

function TValueTerm.wstHas_SRID() : Boolean;
begin
  Result := ( FSRID <> '' );
end;

function TFunctionImportParameterAttributes.wstHas_Mode() : Boolean;
begin
  Result := True;
end;

function TFunctionImportParameterAttributes.wstHas_Nullable() : Boolean;
begin
  Result := ( FNullable <> boolean(0) );
end;

function TFunctionImportParameterAttributes.wstHas_MaxLength() : Boolean;
begin
  Result := ( FMaxLength <> '' );
end;

function TFunctionImportParameterAttributes.wstHas_Precision() : Boolean;
begin
  Result := ( FPrecision <> TPrecisionFacet(0) );
end;

function TFunctionImportParameterAttributes.wstHas_Scale() : Boolean;
begin
  Result := ( FScale <> TScaleFacet(0) );
end;

function TFunctionImportParameterAttributes.wstHas_SRID() : Boolean;
begin
  Result := ( FSRID <> '' );
end;

{ TFunctionImportParameter }

constructor TFunctionImportParameter.Create();
begin
  inherited Create();
  FDocumentation := TDocumentation.Create();
  FValueAnnotation := TFunctionImportParameter_ValueAnnotationArray.Create();
  FTypeAnnotation := TFunctionImportParameter_TypeAnnotationArray.Create();
end;

procedure TFunctionImportParameter.FreeObjectProperties();
begin
  if Assigned(FDocumentation) then
    FreeAndNil(FDocumentation);
  if Assigned(FValueAnnotation) then
    FreeAndNil(FValueAnnotation);
  if Assigned(FTypeAnnotation) then
    FreeAndNil(FTypeAnnotation);
  inherited FreeObjectProperties();
end;

function TFunctionImportParameter.wstHas_Documentation() : Boolean;
begin
  Result := ( FDocumentation <> nil );
end;

function TFunctionImportParameter.wstHas_ValueAnnotation() : Boolean;
begin
  Result := ( FValueAnnotation <> TFunctionImportParameter_ValueAnnotationArray(0) );
end;

function TFunctionImportParameter.wstHas_TypeAnnotation() : Boolean;
begin
  Result := ( FTypeAnnotation <> TFunctionImportParameter_TypeAnnotationArray(0) );
end;

function TFunctionImportParameter.wstHas_Mode() : Boolean;
begin
  Result := True;
end;

function TFunctionImportParameter.wstHas_Nullable() : Boolean;
begin
  Result := ( FNullable <> boolean(0) );
end;

function TFunctionImportParameter.wstHas_MaxLength() : Boolean;
begin
  Result := ( FMaxLength <> '' );
end;

function TFunctionImportParameter.wstHas_Precision() : Boolean;
begin
  Result := ( FPrecision <> TPrecisionFacet(0) );
end;

function TFunctionImportParameter.wstHas_Scale() : Boolean;
begin
  Result := ( FScale <> TScaleFacet(0) );
end;

function TFunctionImportParameter.wstHas_SRID() : Boolean;
begin
  Result := ( FSRID <> '' );
end;

function TFunctionImportAttributes.wstHas_ReturnType() : Boolean;
begin
  Result := ( FReturnType <> '' );
end;

function TFunctionImportAttributes.wstHas_EntitySet() : Boolean;
begin
  Result := ( FEntitySet <> '' );
end;

function TFunctionImportAttributes.wstHas_EntitySetPath() : Boolean;
begin
  Result := ( FEntitySetPath <> '' );
end;

function TFunctionImportAttributes.wstHas_IsComposable() : Boolean;
begin
  Result := ( FIsComposable <> boolean(0) );
end;

function TFunctionImportAttributes.wstHas_IsSideEffecting() : Boolean;
begin
  Result := ( FIsSideEffecting <> boolean(0) );
end;

function TFunctionImportAttributes.wstHas_IsBindable() : Boolean;
begin
  Result := ( FIsBindable <> boolean(0) );
end;

function TFunctionImportAttributes.wstHas_MethodAccess() : Boolean;
begin
  Result := True;
end;

function TEntitySetAttributes.wstHas_GetterAccess() : Boolean;
begin
  Result := True;
end;

{ EntityContainer_FunctionImport_Type }

constructor EntityContainer_FunctionImport_Type.Create();
begin
  inherited Create();
  FDocumentation := TDocumentation.Create();
  FReturnType := EntityContainer_FunctionImport_Type_ReturnTypeArray.Create();
  FParameter := EntityContainer_FunctionImport_Type_ParameterArray.Create();
  FValueAnnotation := EntityContainer_FunctionImport_Type_ValueAnnotationArray.Create();
  FTypeAnnotation := EntityContainer_FunctionImport_Type_TypeAnnotationArray.Create();
end;

procedure EntityContainer_FunctionImport_Type.FreeObjectProperties();
begin
  if Assigned(FDocumentation) then
    FreeAndNil(FDocumentation);
  if Assigned(FReturnType) then
    FreeAndNil(FReturnType);
  if Assigned(FParameter) then
    FreeAndNil(FParameter);
  if Assigned(FValueAnnotation) then
    FreeAndNil(FValueAnnotation);
  if Assigned(FTypeAnnotation) then
    FreeAndNil(FTypeAnnotation);
  inherited FreeObjectProperties();
end;

function EntityContainer_FunctionImport_Type.wstHas_Documentation() : Boolean;
begin
  Result := ( FDocumentation <> nil );
end;

function EntityContainer_FunctionImport_Type.wstHas_ReturnType() : Boolean;
begin
  Result := ( FReturnType <> EntityContainer_FunctionImport_Type_ReturnTypeArray(0) );
end;

function EntityContainer_FunctionImport_Type.wstHas_Parameter() : Boolean;
begin
  Result := ( FParameter <> EntityContainer_FunctionImport_Type_ParameterArray(0) );
end;

function EntityContainer_FunctionImport_Type.wstHas_ValueAnnotation() : Boolean;
begin
  Result := ( FValueAnnotation <> EntityContainer_FunctionImport_Type_ValueAnnotationArray(0) );
end;

function EntityContainer_FunctionImport_Type.wstHas_TypeAnnotation() : Boolean;
begin
  Result := ( FTypeAnnotation <> EntityContainer_FunctionImport_Type_TypeAnnotationArray(0) );
end;

function EntityContainer_FunctionImport_Type.wstHas_ReturnTypeAtt() : Boolean;
begin
  Result := ( FReturnTypeAtt <> '' );
end;

function EntityContainer_FunctionImport_Type.wstHas_EntitySet() : Boolean;
begin
  Result := ( FEntitySet <> '' );
end;

function EntityContainer_FunctionImport_Type.wstHas_EntitySetPath() : Boolean;
begin
  Result := ( FEntitySetPath <> '' );
end;

function EntityContainer_FunctionImport_Type.wstHas_IsComposable() : Boolean;
begin
  Result := ( FIsComposable <> boolean(0) );
end;

function EntityContainer_FunctionImport_Type.wstHas_IsSideEffecting() : Boolean;
begin
  Result := ( FIsSideEffecting <> boolean(0) );
end;

function EntityContainer_FunctionImport_Type.wstHas_IsBindable() : Boolean;
begin
  Result := ( FIsBindable <> boolean(0) );
end;

function EntityContainer_FunctionImport_Type.wstHas_MethodAccess() : Boolean;
begin
  Result := True;
end;

{ EntityContainer_EntitySet_Type }

constructor EntityContainer_EntitySet_Type.Create();
begin
  inherited Create();
  FDocumentation := TDocumentation.Create();
  FValueAnnotation := EntityContainer_EntitySet_Type_ValueAnnotationArray.Create();
  FTypeAnnotation := EntityContainer_EntitySet_Type_TypeAnnotationArray.Create();
end;

procedure EntityContainer_EntitySet_Type.FreeObjectProperties();
begin
  if Assigned(FDocumentation) then
    FreeAndNil(FDocumentation);
  if Assigned(FValueAnnotation) then
    FreeAndNil(FValueAnnotation);
  if Assigned(FTypeAnnotation) then
    FreeAndNil(FTypeAnnotation);
  inherited FreeObjectProperties();
end;

function EntityContainer_EntitySet_Type.wstHas_Documentation() : Boolean;
begin
  Result := ( FDocumentation <> nil );
end;

function EntityContainer_EntitySet_Type.wstHas_ValueAnnotation() : Boolean;
begin
  Result := ( FValueAnnotation <> EntityContainer_EntitySet_Type_ValueAnnotationArray(0) );
end;

function EntityContainer_EntitySet_Type.wstHas_TypeAnnotation() : Boolean;
begin
  Result := ( FTypeAnnotation <> EntityContainer_EntitySet_Type_TypeAnnotationArray(0) );
end;

function EntityContainer_EntitySet_Type.wstHas_GetterAccess() : Boolean;
begin
  Result := True;
end;

{ EntityContainer_AssociationSet_Type_End_Type }

constructor EntityContainer_AssociationSet_Type_End_Type.Create();
begin
  inherited Create();
  FDocumentation := TDocumentation.Create();
end;

procedure EntityContainer_AssociationSet_Type_End_Type.FreeObjectProperties();
begin
  if Assigned(FDocumentation) then
    FreeAndNil(FDocumentation);
  inherited FreeObjectProperties();
end;

function EntityContainer_AssociationSet_Type_End_Type.wstHas_Role() : Boolean;
begin
  Result := ( FRole <> '' );
end;

function EntityContainer_AssociationSet_Type_End_Type.wstHas_Documentation() : Boolean;
begin
  Result := ( FDocumentation <> nil );
end;

{ EntityContainer_AssociationSet_Type }

constructor EntityContainer_AssociationSet_Type.Create();
begin
  inherited Create();
  FDocumentation := TDocumentation.Create();
  F_End := EntityContainer_AssociationSet_Type__EndArray.Create();
end;

procedure EntityContainer_AssociationSet_Type.FreeObjectProperties();
begin
  if Assigned(FDocumentation) then
    FreeAndNil(FDocumentation);
  if Assigned(F_End) then
    FreeAndNil(F_End);
  inherited FreeObjectProperties();
end;

function EntityContainer_AssociationSet_Type.wstHas_Documentation() : Boolean;
begin
  Result := ( FDocumentation <> nil );
end;

function EntityContainer_AssociationSet_Type.wstHas__End() : Boolean;
begin
  Result := ( F_End <> EntityContainer_AssociationSet_Type__EndArray(0) );
end;

{ EntityContainer }

constructor EntityContainer.Create();
begin
  inherited Create();
  FDocumentation := TDocumentation.Create();
  FFunctionImport := EntityContainer_FunctionImportArray.Create();
  FEntitySet := EntityContainer_EntitySetArray.Create();
  FAssociationSet := EntityContainer_AssociationSetArray.Create();
  FValueAnnotation := EntityContainer_ValueAnnotationArray.Create();
  FTypeAnnotation := EntityContainer_TypeAnnotationArray.Create();
end;

procedure EntityContainer.FreeObjectProperties();
begin
  if Assigned(FDocumentation) then
    FreeAndNil(FDocumentation);
  if Assigned(FFunctionImport) then
    FreeAndNil(FFunctionImport);
  if Assigned(FEntitySet) then
    FreeAndNil(FEntitySet);
  if Assigned(FAssociationSet) then
    FreeAndNil(FAssociationSet);
  if Assigned(FValueAnnotation) then
    FreeAndNil(FValueAnnotation);
  if Assigned(FTypeAnnotation) then
    FreeAndNil(FTypeAnnotation);
  inherited FreeObjectProperties();
end;

function EntityContainer.wstHas_Documentation() : Boolean;
begin
  Result := ( FDocumentation <> nil );
end;

function EntityContainer.wstHas_FunctionImport() : Boolean;
begin
  Result := ( FFunctionImport <> EntityContainer_FunctionImportArray(0) );
end;

function EntityContainer.wstHas_EntitySet() : Boolean;
begin
  Result := ( FEntitySet <> EntityContainer_EntitySetArray(0) );
end;

function EntityContainer.wstHas_AssociationSet() : Boolean;
begin
  Result := ( FAssociationSet <> EntityContainer_AssociationSetArray(0) );
end;

function EntityContainer.wstHas_ValueAnnotation() : Boolean;
begin
  Result := ( FValueAnnotation <> EntityContainer_ValueAnnotationArray(0) );
end;

function EntityContainer.wstHas_TypeAnnotation() : Boolean;
begin
  Result := ( FTypeAnnotation <> EntityContainer_TypeAnnotationArray(0) );
end;

function EntityContainer.wstHas_Extends() : Boolean;
begin
  Result := ( FExtends <> '' );
end;

function EntityContainer.wstHas_TypeAccess() : Boolean;
begin
  Result := True;
end;

function EntityContainer.wstHas_LazyLoadingEnabled() : Boolean;
begin
  Result := ( FLazyLoadingEnabled <> LazyLoadingEnabled_Type(0) );
end;

{ GSchemaBodyElements_UsingArray }

function GSchemaBodyElements_UsingArray.GetItem(AIndex: Integer): TUsing;
begin
  Result := TUsing(Inherited GetItem(AIndex));
end;

class function GSchemaBodyElements_UsingArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TUsing;
end;

{ GSchemaBodyElements_AssociationArray }

function GSchemaBodyElements_AssociationArray.GetItem(AIndex: Integer): TAssociation;
begin
  Result := TAssociation(Inherited GetItem(AIndex));
end;

class function GSchemaBodyElements_AssociationArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TAssociation;
end;

{ GSchemaBodyElements_ComplexTypeArray }

function GSchemaBodyElements_ComplexTypeArray.GetItem(AIndex: Integer): TComplexType;
begin
  Result := TComplexType(Inherited GetItem(AIndex));
end;

class function GSchemaBodyElements_ComplexTypeArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TComplexType;
end;

{ GSchemaBodyElements_EntityTypeArray }

function GSchemaBodyElements_EntityTypeArray.GetItem(AIndex: Integer): TEntityType;
begin
  Result := TEntityType(Inherited GetItem(AIndex));
end;

class function GSchemaBodyElements_EntityTypeArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TEntityType;
end;

{ GSchemaBodyElements_EnumTypeArray }

function GSchemaBodyElements_EnumTypeArray.GetItem(AIndex: Integer): TEnumType;
begin
  Result := TEnumType(Inherited GetItem(AIndex));
end;

class function GSchemaBodyElements_EnumTypeArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TEnumType;
end;

{ GSchemaBodyElements_ValueTermArray }

function GSchemaBodyElements_ValueTermArray.GetItem(AIndex: Integer): TValueTerm;
begin
  Result := TValueTerm(Inherited GetItem(AIndex));
end;

class function GSchemaBodyElements_ValueTermArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TValueTerm;
end;

{ GSchemaBodyElements__FunctionArray }

function GSchemaBodyElements__FunctionArray.GetItem(AIndex: Integer): TFunction;
begin
  Result := TFunction(Inherited GetItem(AIndex));
end;

class function GSchemaBodyElements__FunctionArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TFunction;
end;

{ GSchemaBodyElements_AnnotationsArray }

function GSchemaBodyElements_AnnotationsArray.GetItem(AIndex: Integer): TAnnotations;
begin
  Result := TAnnotations(Inherited GetItem(AIndex));
end;

class function GSchemaBodyElements_AnnotationsArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TAnnotations;
end;

{ TAssociation__EndArray }

function TAssociation__EndArray.GetItem(AIndex: Integer): TAssociationEnd;
begin
  Result := TAssociationEnd(Inherited GetItem(AIndex));
end;

class function TAssociation__EndArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TAssociationEnd;
end;

{ TComplexType__PropertyArray }

function TComplexType__PropertyArray.GetItem(AIndex: Integer): TComplexTypeProperty;
begin
  Result := TComplexTypeProperty(Inherited GetItem(AIndex));
end;

class function TComplexType__PropertyArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TComplexTypeProperty;
end;

{ TComplexType_ValueAnnotationArray }

function TComplexType_ValueAnnotationArray.GetItem(AIndex: Integer): TValueAnnotation;
begin
  Result := TValueAnnotation(Inherited GetItem(AIndex));
end;

class function TComplexType_ValueAnnotationArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TValueAnnotation;
end;

{ TComplexType_TypeAnnotationArray }

function TComplexType_TypeAnnotationArray.GetItem(AIndex: Integer): TTypeAnnotation;
begin
  Result := TTypeAnnotation(Inherited GetItem(AIndex));
end;

class function TComplexType_TypeAnnotationArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TTypeAnnotation;
end;

{ TReferentialConstraintRoleElement_PropertyRefArray }

function TReferentialConstraintRoleElement_PropertyRefArray.GetItem(AIndex: Integer): TPropertyRef;
begin
  Result := TPropertyRef(Inherited GetItem(AIndex));
end;

class function TReferentialConstraintRoleElement_PropertyRefArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TPropertyRef;
end;

{ TNavigationProperty_ValueAnnotationArray }

function TNavigationProperty_ValueAnnotationArray.GetItem(AIndex: Integer): TValueAnnotation;
begin
  Result := TValueAnnotation(Inherited GetItem(AIndex));
end;

class function TNavigationProperty_ValueAnnotationArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TValueAnnotation;
end;

{ TNavigationProperty_TypeAnnotationArray }

function TNavigationProperty_TypeAnnotationArray.GetItem(AIndex: Integer): TTypeAnnotation;
begin
  Result := TTypeAnnotation(Inherited GetItem(AIndex));
end;

class function TNavigationProperty_TypeAnnotationArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TTypeAnnotation;
end;

{ TEntityType__PropertyArray }

function TEntityType__PropertyArray.GetItem(AIndex: Integer): TEntityProperty;
begin
  Result := TEntityProperty(Inherited GetItem(AIndex));
end;

class function TEntityType__PropertyArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TEntityProperty;
end;

{ TEntityType_NavigationPropertyArray }

function TEntityType_NavigationPropertyArray.GetItem(AIndex: Integer): TNavigationProperty;
begin
  Result := TNavigationProperty(Inherited GetItem(AIndex));
end;

class function TEntityType_NavigationPropertyArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TNavigationProperty;
end;

{ TEntityType_ValueAnnotationArray }

function TEntityType_ValueAnnotationArray.GetItem(AIndex: Integer): TValueAnnotation;
begin
  Result := TValueAnnotation(Inherited GetItem(AIndex));
end;

class function TEntityType_ValueAnnotationArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TValueAnnotation;
end;

{ TEntityType_TypeAnnotationArray }

function TEntityType_TypeAnnotationArray.GetItem(AIndex: Integer): TTypeAnnotation;
begin
  Result := TTypeAnnotation(Inherited GetItem(AIndex));
end;

class function TEntityType_TypeAnnotationArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TTypeAnnotation;
end;

{ TEnumType_MemberArray }

function TEnumType_MemberArray.GetItem(AIndex: Integer): TEnumTypeMember;
begin
  Result := TEnumTypeMember(Inherited GetItem(AIndex));
end;

class function TEnumType_MemberArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TEnumTypeMember;
end;

{ TEnumType_ValueAnnotationArray }

function TEnumType_ValueAnnotationArray.GetItem(AIndex: Integer): TValueAnnotation;
begin
  Result := TValueAnnotation(Inherited GetItem(AIndex));
end;

class function TEnumType_ValueAnnotationArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TValueAnnotation;
end;

{ TEnumType_TypeAnnotationArray }

function TEnumType_TypeAnnotationArray.GetItem(AIndex: Integer): TTypeAnnotation;
begin
  Result := TTypeAnnotation(Inherited GetItem(AIndex));
end;

class function TEnumType_TypeAnnotationArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TTypeAnnotation;
end;

{ TFunction_ParameterArray }

function TFunction_ParameterArray.GetItem(AIndex: Integer): TFunctionParameter;
begin
  Result := TFunctionParameter(Inherited GetItem(AIndex));
end;

class function TFunction_ParameterArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TFunctionParameter;
end;

{ TFunction_DefiningExpressionArray }

function TFunction_DefiningExpressionArray.GetItem(AIndex: Integer): TCommandText;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure TFunction_DefiningExpressionArray.SetItem(AIndex: Integer;const AValue: TCommandText);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function TFunction_DefiningExpressionArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure TFunction_DefiningExpressionArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('DefiningExpression',TypeInfo(TCommandText),FData[AIndex]);
end;

procedure TFunction_DefiningExpressionArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'DefiningExpression';
  AStore.Get(TypeInfo(TCommandText),sName,FData[AIndex]);
end;

class function TFunction_DefiningExpressionArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(TCommandText);
end;

procedure TFunction_DefiningExpressionArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

procedure TFunction_DefiningExpressionArray.Assign(Source: TPersistent);
var
  src : TFunction_DefiningExpressionArray;
  i, c : Integer;
begin
  if Assigned(Source) and Source.InheritsFrom(TFunction_DefiningExpressionArray) then begin
    src := TFunction_DefiningExpressionArray(Source);
    c := src.Length;
    Self.SetLength(c);
    if ( c > 0 ) then begin
      for i := 0 to Pred(c) do begin
        Self[i] := src[i];
      end;
    end;
  end else begin
    inherited Assign(Source);
  end;
end;

{ TFunction_ReturnTypeArray }

function TFunction_ReturnTypeArray.GetItem(AIndex: Integer): TFunctionReturnType;
begin
  Result := TFunctionReturnType(Inherited GetItem(AIndex));
end;

class function TFunction_ReturnTypeArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TFunctionReturnType;
end;

{ TFunction_ValueAnnotationArray }

function TFunction_ValueAnnotationArray.GetItem(AIndex: Integer): TValueAnnotation;
begin
  Result := TValueAnnotation(Inherited GetItem(AIndex));
end;

class function TFunction_ValueAnnotationArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TValueAnnotation;
end;

{ TFunction_TypeAnnotationArray }

function TFunction_TypeAnnotationArray.GetItem(AIndex: Integer): TTypeAnnotation;
begin
  Result := TTypeAnnotation(Inherited GetItem(AIndex));
end;

class function TFunction_TypeAnnotationArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TTypeAnnotation;
end;

{ TFunctionParameter_ValueAnnotationArray }

function TFunctionParameter_ValueAnnotationArray.GetItem(AIndex: Integer): TValueAnnotation;
begin
  Result := TValueAnnotation(Inherited GetItem(AIndex));
end;

class function TFunctionParameter_ValueAnnotationArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TValueAnnotation;
end;

{ TFunctionParameter_TypeAnnotationArray }

function TFunctionParameter_TypeAnnotationArray.GetItem(AIndex: Integer): TTypeAnnotation;
begin
  Result := TTypeAnnotation(Inherited GetItem(AIndex));
end;

class function TFunctionParameter_TypeAnnotationArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TTypeAnnotation;
end;

{ TRowType }

function TRowType.GetItem(AIndex: Integer): TRowProperty;
begin
  Result := TRowProperty(Inherited GetItem(AIndex));
end;

class function TRowType.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TRowProperty;
end;

{ TEntityKeyElement }

function TEntityKeyElement.GetItem(AIndex: Integer): TPropertyRef;
begin
  Result := TPropertyRef(Inherited GetItem(AIndex));
end;

class function TEntityKeyElement.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TPropertyRef;
end;

{ TAnnotations_ValueAnnotationArray }

function TAnnotations_ValueAnnotationArray.GetItem(AIndex: Integer): TValueAnnotation;
begin
  Result := TValueAnnotation(Inherited GetItem(AIndex));
end;

class function TAnnotations_ValueAnnotationArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TValueAnnotation;
end;

{ TAnnotations_TypeAnnotationArray }

function TAnnotations_TypeAnnotationArray.GetItem(AIndex: Integer): TTypeAnnotation;
begin
  Result := TTypeAnnotation(Inherited GetItem(AIndex));
end;

class function TAnnotations_TypeAnnotationArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TTypeAnnotation;
end;

{ TTypeAnnotation_PropertyValueArray }

function TTypeAnnotation_PropertyValueArray.GetItem(AIndex: Integer): TPropertyValue;
begin
  Result := TPropertyValue(Inherited GetItem(AIndex));
end;

class function TTypeAnnotation_PropertyValueArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TPropertyValue;
end;

{ TRecordExpression_PropertyValueArray }

function TRecordExpression_PropertyValueArray.GetItem(AIndex: Integer): TPropertyValue;
begin
  Result := TPropertyValue(Inherited GetItem(AIndex));
end;

class function TRecordExpression_PropertyValueArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TPropertyValue;
end;

{ TFunctionReferenceExpression_ParameterArray }

function TFunctionReferenceExpression_ParameterArray.GetItem(AIndex: Integer): TFunctionReferenceExpression_Parameter_Type;
begin
  Result := TFunctionReferenceExpression_Parameter_Type(Inherited GetItem(AIndex));
end;

class function TFunctionReferenceExpression_ParameterArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TFunctionReferenceExpression_Parameter_Type;
end;

{ TEntityProperty_DocumentationArray }

function TEntityProperty_DocumentationArray.GetItem(AIndex: Integer): TDocumentation;
begin
  Result := TDocumentation(Inherited GetItem(AIndex));
end;

class function TEntityProperty_DocumentationArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TDocumentation;
end;

{ TEntityProperty_ValueAnnotationArray }

function TEntityProperty_ValueAnnotationArray.GetItem(AIndex: Integer): TValueAnnotation;
begin
  Result := TValueAnnotation(Inherited GetItem(AIndex));
end;

class function TEntityProperty_ValueAnnotationArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TValueAnnotation;
end;

{ TEntityProperty_TypeAnnotationArray }

function TEntityProperty_TypeAnnotationArray.GetItem(AIndex: Integer): TTypeAnnotation;
begin
  Result := TTypeAnnotation(Inherited GetItem(AIndex));
end;

class function TEntityProperty_TypeAnnotationArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TTypeAnnotation;
end;

{ TComplexTypeProperty_DocumentationArray }

function TComplexTypeProperty_DocumentationArray.GetItem(AIndex: Integer): TDocumentation;
begin
  Result := TDocumentation(Inherited GetItem(AIndex));
end;

class function TComplexTypeProperty_DocumentationArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TDocumentation;
end;

{ TComplexTypeProperty_ValueAnnotationArray }

function TComplexTypeProperty_ValueAnnotationArray.GetItem(AIndex: Integer): TValueAnnotation;
begin
  Result := TValueAnnotation(Inherited GetItem(AIndex));
end;

class function TComplexTypeProperty_ValueAnnotationArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TValueAnnotation;
end;

{ TComplexTypeProperty_TypeAnnotationArray }

function TComplexTypeProperty_TypeAnnotationArray.GetItem(AIndex: Integer): TTypeAnnotation;
begin
  Result := TTypeAnnotation(Inherited GetItem(AIndex));
end;

class function TComplexTypeProperty_TypeAnnotationArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TTypeAnnotation;
end;

{ TFunctionImportParameter_ValueAnnotationArray }

function TFunctionImportParameter_ValueAnnotationArray.GetItem(AIndex: Integer): TValueAnnotation;
begin
  Result := TValueAnnotation(Inherited GetItem(AIndex));
end;

class function TFunctionImportParameter_ValueAnnotationArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TValueAnnotation;
end;

{ TFunctionImportParameter_TypeAnnotationArray }

function TFunctionImportParameter_TypeAnnotationArray.GetItem(AIndex: Integer): TTypeAnnotation;
begin
  Result := TTypeAnnotation(Inherited GetItem(AIndex));
end;

class function TFunctionImportParameter_TypeAnnotationArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TTypeAnnotation;
end;

{ EntityContainer_FunctionImport_Type_ReturnTypeArray }

function EntityContainer_FunctionImport_Type_ReturnTypeArray.GetItem(AIndex: Integer): TFunctionImportReturnType;
begin
  Result := TFunctionImportReturnType(Inherited GetItem(AIndex));
end;

class function EntityContainer_FunctionImport_Type_ReturnTypeArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TFunctionImportReturnType;
end;

{ EntityContainer_FunctionImport_Type_ParameterArray }

function EntityContainer_FunctionImport_Type_ParameterArray.GetItem(AIndex: Integer): TFunctionImportParameter;
begin
  Result := TFunctionImportParameter(Inherited GetItem(AIndex));
end;

class function EntityContainer_FunctionImport_Type_ParameterArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TFunctionImportParameter;
end;

{ EntityContainer_FunctionImport_Type_ValueAnnotationArray }

function EntityContainer_FunctionImport_Type_ValueAnnotationArray.GetItem(AIndex: Integer): TValueAnnotation;
begin
  Result := TValueAnnotation(Inherited GetItem(AIndex));
end;

class function EntityContainer_FunctionImport_Type_ValueAnnotationArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TValueAnnotation;
end;

{ EntityContainer_FunctionImport_Type_TypeAnnotationArray }

function EntityContainer_FunctionImport_Type_TypeAnnotationArray.GetItem(AIndex: Integer): TTypeAnnotation;
begin
  Result := TTypeAnnotation(Inherited GetItem(AIndex));
end;

class function EntityContainer_FunctionImport_Type_TypeAnnotationArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TTypeAnnotation;
end;

{ EntityContainer_EntitySet_Type_ValueAnnotationArray }

function EntityContainer_EntitySet_Type_ValueAnnotationArray.GetItem(AIndex: Integer): TValueAnnotation;
begin
  Result := TValueAnnotation(Inherited GetItem(AIndex));
end;

class function EntityContainer_EntitySet_Type_ValueAnnotationArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TValueAnnotation;
end;

{ EntityContainer_EntitySet_Type_TypeAnnotationArray }

function EntityContainer_EntitySet_Type_TypeAnnotationArray.GetItem(AIndex: Integer): TTypeAnnotation;
begin
  Result := TTypeAnnotation(Inherited GetItem(AIndex));
end;

class function EntityContainer_EntitySet_Type_TypeAnnotationArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TTypeAnnotation;
end;

{ EntityContainer_AssociationSet_Type__EndArray }

function EntityContainer_AssociationSet_Type__EndArray.GetItem(AIndex: Integer): EntityContainer_AssociationSet_Type_End_Type;
begin
  Result := EntityContainer_AssociationSet_Type_End_Type(Inherited GetItem(AIndex));
end;

class function EntityContainer_AssociationSet_Type__EndArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= EntityContainer_AssociationSet_Type_End_Type;
end;

{ EntityContainer_FunctionImportArray }

function EntityContainer_FunctionImportArray.GetItem(AIndex: Integer): EntityContainer_FunctionImport_Type;
begin
  Result := EntityContainer_FunctionImport_Type(Inherited GetItem(AIndex));
end;

class function EntityContainer_FunctionImportArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= EntityContainer_FunctionImport_Type;
end;

{ EntityContainer_EntitySetArray }

function EntityContainer_EntitySetArray.GetItem(AIndex: Integer): EntityContainer_EntitySet_Type;
begin
  Result := EntityContainer_EntitySet_Type(Inherited GetItem(AIndex));
end;

class function EntityContainer_EntitySetArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= EntityContainer_EntitySet_Type;
end;

{ EntityContainer_AssociationSetArray }

function EntityContainer_AssociationSetArray.GetItem(AIndex: Integer): EntityContainer_AssociationSet_Type;
begin
  Result := EntityContainer_AssociationSet_Type(Inherited GetItem(AIndex));
end;

class function EntityContainer_AssociationSetArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= EntityContainer_AssociationSet_Type;
end;

{ EntityContainer_ValueAnnotationArray }

function EntityContainer_ValueAnnotationArray.GetItem(AIndex: Integer): TValueAnnotation;
begin
  Result := TValueAnnotation(Inherited GetItem(AIndex));
end;

class function EntityContainer_ValueAnnotationArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TValueAnnotation;
end;

{ EntityContainer_TypeAnnotationArray }

function EntityContainer_TypeAnnotationArray.GetItem(AIndex: Integer): TTypeAnnotation;
begin
  Result := TTypeAnnotation(Inherited GetItem(AIndex));
end;

class function EntityContainer_TypeAnnotationArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TTypeAnnotation;
end;


var
  typeRegistryInstance : TTypeRegistry = nil;
initialization
  typeRegistryInstance := GetTypeRegistry();
  TSchema.RegisterAttributeProperty('Namespace');
  TSchema.RegisterAttributeProperty('Alias');
  TUsing.RegisterAttributeProperty('Namespace');
  TUsing.RegisterAttributeProperty('Alias');
  TAssociation.RegisterAttributeProperty('Name');
  TTypeAttributes.RegisterAttributeProperty('Name');
  TComplexType.RegisterAttributeProperty('TypeAccess');
  TComplexType.RegisterAttributeProperty('Name');
  TReferentialConstraintRoleElement.RegisterAttributeProperty('Role');
  TNavigationProperty.RegisterAttributeProperty('Name');
  TNavigationProperty.RegisterAttributeProperty('Relationship');
  TNavigationProperty.RegisterAttributeProperty('ToRole');
  TNavigationProperty.RegisterAttributeProperty('FromRole');
  TNavigationProperty.RegisterAttributeProperty('ContainsTarget');
  TNavigationProperty.RegisterAttributeProperty('GetterAccess');
  TNavigationProperty.RegisterAttributeProperty('SetterAccess');
  TDerivableTypeAttributes.RegisterAttributeProperty('BaseType');
  TDerivableTypeAttributes.RegisterAttributeProperty('_Abstract');
  TDerivableTypeAttributes.RegisterAttributeProperty('Name');
  TEntityType.RegisterAttributeProperty('OpenType');
  TEntityType.RegisterAttributeProperty('TypeAccess');
  TEntityType.RegisterAttributeProperty('BaseType');
  TEntityType.RegisterAttributeProperty('_Abstract');
  TEntityType.RegisterAttributeProperty('Name');
  TEnumTypeMember.RegisterAttributeProperty('Name');
  TEnumTypeMember.RegisterAttributeProperty('Value');
  TEnumType.RegisterAttributeProperty('IsFlags');
  TEnumType.RegisterAttributeProperty('UnderlyingType');
  TEnumType.RegisterAttributeProperty('TypeAccess');
  TEnumType.RegisterAttributeProperty('Name');
  TFacetAttributes.RegisterAttributeProperty('Nullable');
  TFacetAttributes.RegisterAttributeProperty('DefaultValue');
  TFacetAttributes.RegisterAttributeProperty('MaxLength');
  TFacetAttributes.RegisterAttributeProperty('FixedLength');
  TFacetAttributes.RegisterAttributeProperty('Precision');
  TFacetAttributes.RegisterAttributeProperty('Scale');
  TFacetAttributes.RegisterAttributeProperty('Unicode');
  TFacetAttributes.RegisterAttributeProperty('Collation');
  TFacetAttributes.RegisterAttributeProperty('SRID');
  TFunction.RegisterAttributeProperty('Name');
  TFunction.RegisterAttributeProperty('ReturnTypeAtt');
  TFunction.RegisterAttributeProperty('Nullable');
  TFunction.RegisterAttributeProperty('DefaultValue');
  TFunction.RegisterAttributeProperty('MaxLength');
  TFunction.RegisterAttributeProperty('FixedLength');
  TFunction.RegisterAttributeProperty('Precision');
  TFunction.RegisterAttributeProperty('Scale');
  TFunction.RegisterAttributeProperty('Unicode');
  TFunction.RegisterAttributeProperty('Collation');
  TFunction.RegisterAttributeProperty('SRID');
  TFunctionParameter.RegisterAttributeProperty('Name');
  TFunctionParameter.RegisterAttributeProperty('_Type');
  TFunctionParameter.RegisterAttributeProperty('Nullable');
  TFunctionParameter.RegisterAttributeProperty('DefaultValue');
  TFunctionParameter.RegisterAttributeProperty('MaxLength');
  TFunctionParameter.RegisterAttributeProperty('FixedLength');
  TFunctionParameter.RegisterAttributeProperty('Precision');
  TFunctionParameter.RegisterAttributeProperty('Scale');
  TFunctionParameter.RegisterAttributeProperty('Unicode');
  TFunctionParameter.RegisterAttributeProperty('Collation');
  TFunctionParameter.RegisterAttributeProperty('SRID');
  TCollectionType.RegisterAttributeProperty('ElementType');
  TCollectionType.RegisterAttributeProperty('Nullable');
  TCollectionType.RegisterAttributeProperty('DefaultValue');
  TCollectionType.RegisterAttributeProperty('MaxLength');
  TCollectionType.RegisterAttributeProperty('FixedLength');
  TCollectionType.RegisterAttributeProperty('Precision');
  TCollectionType.RegisterAttributeProperty('Scale');
  TCollectionType.RegisterAttributeProperty('Unicode');
  TCollectionType.RegisterAttributeProperty('Collation');
  TCollectionType.RegisterAttributeProperty('SRID');
  TTypeRef.RegisterAttributeProperty('_Type');
  TTypeRef.RegisterAttributeProperty('Nullable');
  TTypeRef.RegisterAttributeProperty('DefaultValue');
  TTypeRef.RegisterAttributeProperty('MaxLength');
  TTypeRef.RegisterAttributeProperty('FixedLength');
  TTypeRef.RegisterAttributeProperty('Precision');
  TTypeRef.RegisterAttributeProperty('Scale');
  TTypeRef.RegisterAttributeProperty('Unicode');
  TTypeRef.RegisterAttributeProperty('Collation');
  TTypeRef.RegisterAttributeProperty('SRID');
  TReferenceType.RegisterAttributeProperty('_Type');
  TRowProperty.RegisterAttributeProperty('Name');
  TRowProperty.RegisterAttributeProperty('_Type');
  TRowProperty.RegisterAttributeProperty('Nullable');
  TRowProperty.RegisterAttributeProperty('DefaultValue');
  TRowProperty.RegisterAttributeProperty('MaxLength');
  TRowProperty.RegisterAttributeProperty('FixedLength');
  TRowProperty.RegisterAttributeProperty('Precision');
  TRowProperty.RegisterAttributeProperty('Scale');
  TRowProperty.RegisterAttributeProperty('Unicode');
  TRowProperty.RegisterAttributeProperty('Collation');
  TRowProperty.RegisterAttributeProperty('SRID');
  TFunctionReturnType.RegisterAttributeProperty('_Type');
  TFunctionReturnType.RegisterAttributeProperty('Nullable');
  TFunctionReturnType.RegisterAttributeProperty('DefaultValue');
  TFunctionReturnType.RegisterAttributeProperty('MaxLength');
  TFunctionReturnType.RegisterAttributeProperty('FixedLength');
  TFunctionReturnType.RegisterAttributeProperty('Precision');
  TFunctionReturnType.RegisterAttributeProperty('Scale');
  TFunctionReturnType.RegisterAttributeProperty('Unicode');
  TFunctionReturnType.RegisterAttributeProperty('Collation');
  TFunctionReturnType.RegisterAttributeProperty('SRID');
  TFunctionImportReturnType.RegisterAttributeProperty('_Type');
  TFunctionImportReturnType.RegisterAttributeProperty('EntitySet');
  TFunctionImportReturnType.RegisterAttributeProperty('EntitySetPath');
  TPropertyRef.RegisterAttributeProperty('Name');
  TAnnotations.RegisterAttributeProperty('Target');
  TAnnotations.RegisterAttributeProperty('Qualifier');
  GInlineExpressions.RegisterAttributeProperty('_String');
  GInlineExpressions.RegisterAttributeProperty('Binary');
  GInlineExpressions.RegisterAttributeProperty('Int');
  GInlineExpressions.RegisterAttributeProperty('Float');
  GInlineExpressions.RegisterAttributeProperty('Guid');
  GInlineExpressions.RegisterAttributeProperty('Decimal');
  GInlineExpressions.RegisterAttributeProperty('Bool');
  GInlineExpressions.RegisterAttributeProperty('Time');
  GInlineExpressions.RegisterAttributeProperty('DateTime');
  GInlineExpressions.RegisterAttributeProperty('DateTimeOffset');
  GInlineExpressions.RegisterAttributeProperty('Path');
  TValueAnnotation.RegisterAttributeProperty('Term');
  TValueAnnotation.RegisterAttributeProperty('Qualifier');
  TValueAnnotation.RegisterAttributeProperty('_String');
  TValueAnnotation.RegisterAttributeProperty('Binary');
  TValueAnnotation.RegisterAttributeProperty('Int');
  TValueAnnotation.RegisterAttributeProperty('Float');
  TValueAnnotation.RegisterAttributeProperty('Guid');
  TValueAnnotation.RegisterAttributeProperty('Decimal');
  TValueAnnotation.RegisterAttributeProperty('Bool');
  TValueAnnotation.RegisterAttributeProperty('Time');
  TValueAnnotation.RegisterAttributeProperty('DateTime');
  TValueAnnotation.RegisterAttributeProperty('DateTimeOffset');
  TValueAnnotation.RegisterAttributeProperty('Path');
  TTypeAnnotation.RegisterAttributeProperty('Term');
  TTypeAnnotation.RegisterAttributeProperty('Qualifier');
  TTypeAnnotation.RegisterAttributeProperty('_String');
  TTypeAnnotation.RegisterAttributeProperty('Binary');
  TTypeAnnotation.RegisterAttributeProperty('Int');
  TTypeAnnotation.RegisterAttributeProperty('Float');
  TTypeAnnotation.RegisterAttributeProperty('Guid');
  TTypeAnnotation.RegisterAttributeProperty('Decimal');
  TTypeAnnotation.RegisterAttributeProperty('Bool');
  TTypeAnnotation.RegisterAttributeProperty('Time');
  TTypeAnnotation.RegisterAttributeProperty('DateTime');
  TTypeAnnotation.RegisterAttributeProperty('DateTimeOffset');
  TTypeAnnotation.RegisterAttributeProperty('Path');
  TRecordExpression.RegisterAttributeProperty('_Type');
  TPropertyValue.RegisterAttributeProperty('_Property');
  TPropertyValue.RegisterAttributeProperty('_String');
  TPropertyValue.RegisterAttributeProperty('Binary');
  TPropertyValue.RegisterAttributeProperty('Int');
  TPropertyValue.RegisterAttributeProperty('Float');
  TPropertyValue.RegisterAttributeProperty('Guid');
  TPropertyValue.RegisterAttributeProperty('Decimal');
  TPropertyValue.RegisterAttributeProperty('Bool');
  TPropertyValue.RegisterAttributeProperty('Time');
  TPropertyValue.RegisterAttributeProperty('DateTime');
  TPropertyValue.RegisterAttributeProperty('DateTimeOffset');
  TPropertyValue.RegisterAttributeProperty('Path');
  TAssertTypeExpression.RegisterAttributeProperty('_Type');
  TAssertTypeExpression.RegisterAttributeProperty('Nullable');
  TAssertTypeExpression.RegisterAttributeProperty('DefaultValue');
  TAssertTypeExpression.RegisterAttributeProperty('MaxLength');
  TAssertTypeExpression.RegisterAttributeProperty('FixedLength');
  TAssertTypeExpression.RegisterAttributeProperty('Precision');
  TAssertTypeExpression.RegisterAttributeProperty('Scale');
  TAssertTypeExpression.RegisterAttributeProperty('Unicode');
  TAssertTypeExpression.RegisterAttributeProperty('Collation');
  TAssertTypeExpression.RegisterAttributeProperty('SRID');
  TIsTypeExpression.RegisterAttributeProperty('_Type');
  TIsTypeExpression.RegisterAttributeProperty('Nullable');
  TIsTypeExpression.RegisterAttributeProperty('DefaultValue');
  TIsTypeExpression.RegisterAttributeProperty('MaxLength');
  TIsTypeExpression.RegisterAttributeProperty('FixedLength');
  TIsTypeExpression.RegisterAttributeProperty('Precision');
  TIsTypeExpression.RegisterAttributeProperty('Scale');
  TIsTypeExpression.RegisterAttributeProperty('Unicode');
  TIsTypeExpression.RegisterAttributeProperty('Collation');
  TIsTypeExpression.RegisterAttributeProperty('SRID');
  TFunctionReferenceExpression_Parameter_Type.RegisterAttributeProperty('_Type');
  TFunctionReferenceExpression.RegisterAttributeProperty('_Function');
  TApplyExpression.RegisterAttributeProperty('_Function');
  TPropertyReferenceExpression.RegisterAttributeProperty('_Property');
  TValueTermReferenceExpression.RegisterAttributeProperty('Term');
  TValueTermReferenceExpression.RegisterAttributeProperty('Qualifier');
  TLabeledElement.RegisterAttributeProperty('Name');
  TAssociationEnd.RegisterAttributeProperty('_Type');
  TAssociationEnd.RegisterAttributeProperty('Role');
  TAssociationEnd.RegisterAttributeProperty('Multiplicity');
  TOnAction.RegisterAttributeProperty('Action');
  TCommonPropertyAttributes.RegisterAttributeProperty('Name');
  TCommonPropertyAttributes.RegisterAttributeProperty('_Type');
  TCommonPropertyAttributes.RegisterAttributeProperty('Nullable');
  TCommonPropertyAttributes.RegisterAttributeProperty('DefaultValue');
  TCommonPropertyAttributes.RegisterAttributeProperty('MaxLength');
  TCommonPropertyAttributes.RegisterAttributeProperty('FixedLength');
  TCommonPropertyAttributes.RegisterAttributeProperty('Precision');
  TCommonPropertyAttributes.RegisterAttributeProperty('Scale');
  TCommonPropertyAttributes.RegisterAttributeProperty('Unicode');
  TCommonPropertyAttributes.RegisterAttributeProperty('Collation');
  TCommonPropertyAttributes.RegisterAttributeProperty('SRID');
  TCommonPropertyAttributes.RegisterAttributeProperty('ConcurrencyMode');
  TCommonPropertyAttributes.RegisterAttributeProperty('SetterAccess');
  TCommonPropertyAttributes.RegisterAttributeProperty('GetterAccess');
  TEntityProperty.RegisterAttributeProperty('StoreGeneratedPattern');
  TEntityProperty.RegisterAttributeProperty('Name');
  TEntityProperty.RegisterAttributeProperty('_Type');
  TEntityProperty.RegisterAttributeProperty('Nullable');
  TEntityProperty.RegisterAttributeProperty('DefaultValue');
  TEntityProperty.RegisterAttributeProperty('MaxLength');
  TEntityProperty.RegisterAttributeProperty('FixedLength');
  TEntityProperty.RegisterAttributeProperty('Precision');
  TEntityProperty.RegisterAttributeProperty('Scale');
  TEntityProperty.RegisterAttributeProperty('Unicode');
  TEntityProperty.RegisterAttributeProperty('Collation');
  TEntityProperty.RegisterAttributeProperty('SRID');
  TEntityProperty.RegisterAttributeProperty('ConcurrencyMode');
  TEntityProperty.RegisterAttributeProperty('SetterAccess');
  TEntityProperty.RegisterAttributeProperty('GetterAccess');
  TComplexTypeProperty.RegisterAttributeProperty('Name');
  TComplexTypeProperty.RegisterAttributeProperty('_Type');
  TComplexTypeProperty.RegisterAttributeProperty('Nullable');
  TComplexTypeProperty.RegisterAttributeProperty('DefaultValue');
  TComplexTypeProperty.RegisterAttributeProperty('MaxLength');
  TComplexTypeProperty.RegisterAttributeProperty('FixedLength');
  TComplexTypeProperty.RegisterAttributeProperty('Precision');
  TComplexTypeProperty.RegisterAttributeProperty('Scale');
  TComplexTypeProperty.RegisterAttributeProperty('Unicode');
  TComplexTypeProperty.RegisterAttributeProperty('Collation');
  TComplexTypeProperty.RegisterAttributeProperty('SRID');
  TComplexTypeProperty.RegisterAttributeProperty('ConcurrencyMode');
  TComplexTypeProperty.RegisterAttributeProperty('SetterAccess');
  TComplexTypeProperty.RegisterAttributeProperty('GetterAccess');
  TValueTerm.RegisterAttributeProperty('Name');
  TValueTerm.RegisterAttributeProperty('_Type');
  TValueTerm.RegisterAttributeProperty('Nullable');
  TValueTerm.RegisterAttributeProperty('DefaultValue');
  TValueTerm.RegisterAttributeProperty('MaxLength');
  TValueTerm.RegisterAttributeProperty('FixedLength');
  TValueTerm.RegisterAttributeProperty('Precision');
  TValueTerm.RegisterAttributeProperty('Scale');
  TValueTerm.RegisterAttributeProperty('Unicode');
  TValueTerm.RegisterAttributeProperty('Collation');
  TValueTerm.RegisterAttributeProperty('SRID');
  TFunctionImportParameterAttributes.RegisterAttributeProperty('Name');
  TFunctionImportParameterAttributes.RegisterAttributeProperty('_Type');
  TFunctionImportParameterAttributes.RegisterAttributeProperty('Mode');
  TFunctionImportParameterAttributes.RegisterAttributeProperty('Nullable');
  TFunctionImportParameterAttributes.RegisterAttributeProperty('MaxLength');
  TFunctionImportParameterAttributes.RegisterAttributeProperty('Precision');
  TFunctionImportParameterAttributes.RegisterAttributeProperty('Scale');
  TFunctionImportParameterAttributes.RegisterAttributeProperty('SRID');
  TFunctionImportParameter.RegisterAttributeProperty('Name');
  TFunctionImportParameter.RegisterAttributeProperty('_Type');
  TFunctionImportParameter.RegisterAttributeProperty('Mode');
  TFunctionImportParameter.RegisterAttributeProperty('Nullable');
  TFunctionImportParameter.RegisterAttributeProperty('MaxLength');
  TFunctionImportParameter.RegisterAttributeProperty('Precision');
  TFunctionImportParameter.RegisterAttributeProperty('Scale');
  TFunctionImportParameter.RegisterAttributeProperty('SRID');
  TFunctionImportAttributes.RegisterAttributeProperty('Name');
  TFunctionImportAttributes.RegisterAttributeProperty('ReturnType');
  TFunctionImportAttributes.RegisterAttributeProperty('EntitySet');
  TFunctionImportAttributes.RegisterAttributeProperty('EntitySetPath');
  TFunctionImportAttributes.RegisterAttributeProperty('IsComposable');
  TFunctionImportAttributes.RegisterAttributeProperty('IsSideEffecting');
  TFunctionImportAttributes.RegisterAttributeProperty('IsBindable');
  TFunctionImportAttributes.RegisterAttributeProperty('MethodAccess');
  TEntitySetAttributes.RegisterAttributeProperty('Name');
  TEntitySetAttributes.RegisterAttributeProperty('EntityType');
  TEntitySetAttributes.RegisterAttributeProperty('GetterAccess');
  EntityContainer_FunctionImport_Type.RegisterAttributeProperty('Name');
  EntityContainer_FunctionImport_Type.RegisterAttributeProperty('ReturnTypeAtt');
  EntityContainer_FunctionImport_Type.RegisterAttributeProperty('EntitySet');
  EntityContainer_FunctionImport_Type.RegisterAttributeProperty('EntitySetPath');
  EntityContainer_FunctionImport_Type.RegisterAttributeProperty('IsComposable');
  EntityContainer_FunctionImport_Type.RegisterAttributeProperty('IsSideEffecting');
  EntityContainer_FunctionImport_Type.RegisterAttributeProperty('IsBindable');
  EntityContainer_FunctionImport_Type.RegisterAttributeProperty('MethodAccess');
  EntityContainer_EntitySet_Type.RegisterAttributeProperty('Name');
  EntityContainer_EntitySet_Type.RegisterAttributeProperty('EntityType');
  EntityContainer_EntitySet_Type.RegisterAttributeProperty('GetterAccess');
  EntityContainer_AssociationSet_Type_End_Type.RegisterAttributeProperty('Role');
  EntityContainer_AssociationSet_Type_End_Type.RegisterAttributeProperty('EntitySet');
  EntityContainer_AssociationSet_Type.RegisterAttributeProperty('Name');
  EntityContainer_AssociationSet_Type.RegisterAttributeProperty('Association');
  EntityContainer.RegisterAttributeProperty('Name');
  EntityContainer.RegisterAttributeProperty('Extends');
  EntityContainer.RegisterAttributeProperty('TypeAccess');
  EntityContainer.RegisterAttributeProperty('LazyLoadingEnabled');

  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(EDMSimpleType),'EDMSimpleType');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(EDMSimpleType)].RegisterExternalPropertyName('EDMSimpleType_Boolean','Boolean');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(EDMSimpleType)].RegisterExternalPropertyName('EDMSimpleType_Byte','Byte');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(EDMSimpleType)].RegisterExternalPropertyName('EDMSimpleType_Double','Double');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(EDMSimpleType)].RegisterExternalPropertyName('EDMSimpleType_Single','Single');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(EDMSimpleType)].RegisterExternalPropertyName('EDMSimpleType_Int64','Int64');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(EDMSimpleType)].RegisterExternalPropertyName('EDMSimpleType_String','String');
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TMax),'TMax');
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TVariable),'TVariable');
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TParameterMode),'TParameterMode');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TParameterMode)].RegisterExternalPropertyName('TParameterMode_In','In');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TParameterMode)].RegisterExternalPropertyName('TParameterMode_Out','Out');
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TAction),'TAction');
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TMultiplicity),'TMultiplicity');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TMultiplicity)].RegisterExternalPropertyName('TMultiplicity__0_1','0..1');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TMultiplicity)].RegisterExternalPropertyName('TMultiplicity__1','1');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TMultiplicity)].RegisterExternalPropertyName('TMultiplicity__','*');
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TConcurrencyMode),'TConcurrencyMode');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TConcurrencyMode)].RegisterExternalPropertyName('TConcurrencyMode_None','None');
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(GSchemaBodyElements),'GSchemaBodyElements',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TSchema),'TSchema',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TDocumentation),'TDocumentation',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TText),'TText',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TXmlOrText),'TXmlOrText',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(GEmptyElementExtensibility),'GEmptyElementExtensibility',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TUsing),'TUsing',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TAssociation),'TAssociation',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TTypeAttributes),'TTypeAttributes',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TComplexType),'TComplexType',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TConstraint),'TConstraint',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TReferentialConstraintRoleElement),'TReferentialConstraintRoleElement',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TNavigationProperty),'TNavigationProperty',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TDerivableTypeAttributes),'TDerivableTypeAttributes',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TEntityType),'TEntityType',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TEnumTypeMember),'TEnumTypeMember',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TEnumType),'TEnumType',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TFacetAttributes),'TFacetAttributes',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TFunction),'TFunction',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TFunctionParameter),'TFunctionParameter',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TCollectionType),'TCollectionType',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TTypeRef),'TTypeRef',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TReferenceType),'TReferenceType',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TRowProperty),'TRowProperty',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TFunctionReturnType),'TFunctionReturnType',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TFunctionImportReturnType),'TFunctionImportReturnType',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TPropertyRef),'TPropertyRef',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TAnnotations),'TAnnotations',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(GExpression),'GExpression',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(GInlineExpressions),'GInlineExpressions',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TValueAnnotation),'TValueAnnotation',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TTypeAnnotation),'TTypeAnnotation',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TStringConstantExpression),'TStringConstantExpression',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TBinaryConstantExpression),'TBinaryConstantExpression',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TIntConstantExpression),'TIntConstantExpression',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TFloatConstantExpression),'TFloatConstantExpression',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TGuidConstantExpression),'TGuidConstantExpression',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TDecimalConstantExpression),'TDecimalConstantExpression',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TBoolConstantExpression),'TBoolConstantExpression',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TTimeConstantExpression),'TTimeConstantExpression',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TDateTimeConstantExpression),'TDateTimeConstantExpression',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TDateTimeOffsetConstantExpression),'TDateTimeOffsetConstantExpression',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TEnumMemberReferenceExpression),'TEnumMemberReferenceExpression',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TNullExpression),'TNullExpression',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TPathExpression),'TPathExpression',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TIfExpression),'TIfExpression',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TRecordExpression),'TRecordExpression',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TPropertyValue),'TPropertyValue',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TCollectionExpression),'TCollectionExpression',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TAssertTypeExpression),'TAssertTypeExpression',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TIsTypeExpression),'TIsTypeExpression',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TFunctionReferenceExpression_Parameter_Type),'TFunctionReferenceExpression_Parameter_Type',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TFunctionReferenceExpression),'TFunctionReferenceExpression',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TEntitySetReferenceExpression),'TEntitySetReferenceExpression',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TParameterReferenceExpression),'TParameterReferenceExpression',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TApplyExpression),'TApplyExpression',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TPropertyReferenceExpression),'TPropertyReferenceExpression',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TValueTermReferenceExpression),'TValueTermReferenceExpression',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TLabeledElement),'TLabeledElement',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TLabeledElementReferenceExpression),'TLabeledElementReferenceExpression',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TOperations),'TOperations',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TAssociationEnd),'TAssociationEnd',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TOnAction),'TOnAction',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TCommonPropertyAttributes),'TCommonPropertyAttributes',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TEntityProperty),'TEntityProperty',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TComplexTypeProperty),'TComplexTypeProperty',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TValueTerm),'TValueTerm',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TFunctionImportParameterAttributes),'TFunctionImportParameterAttributes',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TFunctionImportParameter),'TFunctionImportParameter',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TFunctionImportAttributes),'TFunctionImportAttributes',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TEntitySetAttributes),'TEntitySetAttributes',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(EntityContainer_FunctionImport_Type),'EntityContainer_FunctionImport_Type',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(EntityContainer_EntitySet_Type),'EntityContainer_EntitySet_Type',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(EntityContainer_AssociationSet_Type_End_Type),'EntityContainer_AssociationSet_Type_End_Type',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(EntityContainer_AssociationSet_Type),'EntityContainer_AssociationSet_Type',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(EntityContainer),'EntityContainer',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(GSchemaBodyElements_UsingArray),'GSchemaBodyElements_UsingArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(GSchemaBodyElements_UsingArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(GSchemaBodyElements_AssociationArray),'GSchemaBodyElements_AssociationArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(GSchemaBodyElements_AssociationArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(GSchemaBodyElements_ComplexTypeArray),'GSchemaBodyElements_ComplexTypeArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(GSchemaBodyElements_ComplexTypeArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(GSchemaBodyElements_EntityTypeArray),'GSchemaBodyElements_EntityTypeArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(GSchemaBodyElements_EntityTypeArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(GSchemaBodyElements_EnumTypeArray),'GSchemaBodyElements_EnumTypeArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(GSchemaBodyElements_EnumTypeArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(GSchemaBodyElements_ValueTermArray),'GSchemaBodyElements_ValueTermArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(GSchemaBodyElements_ValueTermArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(GSchemaBodyElements__FunctionArray),'GSchemaBodyElements__FunctionArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(GSchemaBodyElements__FunctionArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(GSchemaBodyElements_AnnotationsArray),'GSchemaBodyElements_AnnotationsArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(GSchemaBodyElements_AnnotationsArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TAssociation__EndArray),'TAssociation__EndArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TAssociation__EndArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TComplexType__PropertyArray),'TComplexType__PropertyArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TComplexType__PropertyArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TComplexType_ValueAnnotationArray),'TComplexType_ValueAnnotationArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TComplexType_ValueAnnotationArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TComplexType_TypeAnnotationArray),'TComplexType_TypeAnnotationArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TComplexType_TypeAnnotationArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TReferentialConstraintRoleElement_PropertyRefArray),'TReferentialConstraintRoleElement_PropertyRefArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TReferentialConstraintRoleElement_PropertyRefArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TNavigationProperty_ValueAnnotationArray),'TNavigationProperty_ValueAnnotationArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TNavigationProperty_ValueAnnotationArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TNavigationProperty_TypeAnnotationArray),'TNavigationProperty_TypeAnnotationArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TNavigationProperty_TypeAnnotationArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TEntityType__PropertyArray),'TEntityType__PropertyArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TEntityType__PropertyArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TEntityType_NavigationPropertyArray),'TEntityType_NavigationPropertyArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TEntityType_NavigationPropertyArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TEntityType_ValueAnnotationArray),'TEntityType_ValueAnnotationArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TEntityType_ValueAnnotationArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TEntityType_TypeAnnotationArray),'TEntityType_TypeAnnotationArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TEntityType_TypeAnnotationArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TEnumType_MemberArray),'TEnumType_MemberArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TEnumType_MemberArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TEnumType_ValueAnnotationArray),'TEnumType_ValueAnnotationArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TEnumType_ValueAnnotationArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TEnumType_TypeAnnotationArray),'TEnumType_TypeAnnotationArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TEnumType_TypeAnnotationArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TFunction_ParameterArray),'TFunction_ParameterArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TFunction_ParameterArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TFunction_DefiningExpressionArray),'TFunction_DefiningExpressionArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TFunction_DefiningExpressionArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TFunction_ReturnTypeArray),'TFunction_ReturnTypeArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TFunction_ReturnTypeArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TFunction_ValueAnnotationArray),'TFunction_ValueAnnotationArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TFunction_ValueAnnotationArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TFunction_TypeAnnotationArray),'TFunction_TypeAnnotationArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TFunction_TypeAnnotationArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TFunctionParameter_ValueAnnotationArray),'TFunctionParameter_ValueAnnotationArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TFunctionParameter_ValueAnnotationArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TFunctionParameter_TypeAnnotationArray),'TFunctionParameter_TypeAnnotationArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TFunctionParameter_TypeAnnotationArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TRowType),'TRowType');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TRowType)].RegisterExternalPropertyName(sARRAY_ITEM,'Property');
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TEntityKeyElement),'TEntityKeyElement');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TEntityKeyElement)].RegisterExternalPropertyName(sARRAY_ITEM,'PropertyRef');
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TAnnotations_ValueAnnotationArray),'TAnnotations_ValueAnnotationArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TAnnotations_ValueAnnotationArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TAnnotations_TypeAnnotationArray),'TAnnotations_TypeAnnotationArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TAnnotations_TypeAnnotationArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TTypeAnnotation_PropertyValueArray),'TTypeAnnotation_PropertyValueArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TTypeAnnotation_PropertyValueArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TRecordExpression_PropertyValueArray),'TRecordExpression_PropertyValueArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TRecordExpression_PropertyValueArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TFunctionReferenceExpression_ParameterArray),'TFunctionReferenceExpression_ParameterArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TFunctionReferenceExpression_ParameterArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TEntityProperty_DocumentationArray),'TEntityProperty_DocumentationArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TEntityProperty_DocumentationArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TEntityProperty_ValueAnnotationArray),'TEntityProperty_ValueAnnotationArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TEntityProperty_ValueAnnotationArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TEntityProperty_TypeAnnotationArray),'TEntityProperty_TypeAnnotationArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TEntityProperty_TypeAnnotationArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TComplexTypeProperty_DocumentationArray),'TComplexTypeProperty_DocumentationArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TComplexTypeProperty_DocumentationArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TComplexTypeProperty_ValueAnnotationArray),'TComplexTypeProperty_ValueAnnotationArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TComplexTypeProperty_ValueAnnotationArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TComplexTypeProperty_TypeAnnotationArray),'TComplexTypeProperty_TypeAnnotationArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TComplexTypeProperty_TypeAnnotationArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TFunctionImportParameter_ValueAnnotationArray),'TFunctionImportParameter_ValueAnnotationArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TFunctionImportParameter_ValueAnnotationArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TFunctionImportParameter_TypeAnnotationArray),'TFunctionImportParameter_TypeAnnotationArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TFunctionImportParameter_TypeAnnotationArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(EntityContainer_FunctionImport_Type_ReturnTypeArray),'EntityContainer_FunctionImport_Type_ReturnTypeArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(EntityContainer_FunctionImport_Type_ReturnTypeArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(EntityContainer_FunctionImport_Type_ParameterArray),'EntityContainer_FunctionImport_Type_ParameterArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(EntityContainer_FunctionImport_Type_ParameterArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(EntityContainer_FunctionImport_Type_ValueAnnotationArray),'EntityContainer_FunctionImport_Type_ValueAnnotationArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(EntityContainer_FunctionImport_Type_ValueAnnotationArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(EntityContainer_FunctionImport_Type_TypeAnnotationArray),'EntityContainer_FunctionImport_Type_TypeAnnotationArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(EntityContainer_FunctionImport_Type_TypeAnnotationArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(EntityContainer_EntitySet_Type_ValueAnnotationArray),'EntityContainer_EntitySet_Type_ValueAnnotationArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(EntityContainer_EntitySet_Type_ValueAnnotationArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(EntityContainer_EntitySet_Type_TypeAnnotationArray),'EntityContainer_EntitySet_Type_TypeAnnotationArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(EntityContainer_EntitySet_Type_TypeAnnotationArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(EntityContainer_AssociationSet_Type__EndArray),'EntityContainer_AssociationSet_Type__EndArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(EntityContainer_AssociationSet_Type__EndArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(EntityContainer_FunctionImportArray),'EntityContainer_FunctionImportArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(EntityContainer_FunctionImportArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(EntityContainer_EntitySetArray),'EntityContainer_EntitySetArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(EntityContainer_EntitySetArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(EntityContainer_AssociationSetArray),'EntityContainer_AssociationSetArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(EntityContainer_AssociationSetArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(EntityContainer_ValueAnnotationArray),'EntityContainer_ValueAnnotationArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(EntityContainer_ValueAnnotationArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(EntityContainer_TypeAnnotationArray),'EntityContainer_TypeAnnotationArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(EntityContainer_TypeAnnotationArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);

  typeRegistryInstance.ItemByTypeInfo[TypeInfo(GSchemaBodyElements)].RegisterExternalPropertyName('_Function','Function');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TSchema)].RegisterExternalPropertyName('_Function','Function');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TAssociation)].RegisterExternalPropertyName('_End','End');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TComplexType)].RegisterExternalPropertyName('_Property','Property');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TDerivableTypeAttributes)].RegisterExternalPropertyName('_Abstract','Abstract');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TEntityType)].RegisterExternalPropertyName('_Property','Property');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TEntityType)].RegisterExternalPropertyName('_Abstract','Abstract');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TFunction)].RegisterExternalPropertyName('ReturnTypeAtt','ReturnType');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TFunctionParameter)].RegisterExternalPropertyName('_Type','Type');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TTypeRef)].RegisterExternalPropertyName('_Type','Type');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TReferenceType)].RegisterExternalPropertyName('_Type','Type');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TRowProperty)].RegisterExternalPropertyName('_Type','Type');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TFunctionReturnType)].RegisterExternalPropertyName('_Type','Type');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TFunctionImportReturnType)].RegisterExternalPropertyName('_Type','Type');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(GInlineExpressions)].RegisterExternalPropertyName('_String','String');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TValueAnnotation)].RegisterExternalPropertyName('_String','String');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TTypeAnnotation)].RegisterExternalPropertyName('_String','String');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TRecordExpression)].RegisterExternalPropertyName('_Type','Type');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TPropertyValue)].RegisterExternalPropertyName('_Property','Property');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TPropertyValue)].RegisterExternalPropertyName('_String','String');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TAssertTypeExpression)].RegisterExternalPropertyName('_Type','Type');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TIsTypeExpression)].RegisterExternalPropertyName('_Type','Type');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TFunctionReferenceExpression_Parameter_Type)].RegisterExternalPropertyName('_Type','Type');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TFunctionReferenceExpression)].RegisterExternalPropertyName('_Function','Function');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TApplyExpression)].RegisterExternalPropertyName('_Function','Function');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TPropertyReferenceExpression)].RegisterExternalPropertyName('_Property','Property');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TAssociationEnd)].RegisterExternalPropertyName('_Type','Type');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TCommonPropertyAttributes)].RegisterExternalPropertyName('_Type','Type');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TEntityProperty)].RegisterExternalPropertyName('_Type','Type');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TComplexTypeProperty)].RegisterExternalPropertyName('_Type','Type');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TValueTerm)].RegisterExternalPropertyName('_Type','Type');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TFunctionImportParameterAttributes)].RegisterExternalPropertyName('_Type','Type');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TFunctionImportParameter)].RegisterExternalPropertyName('_Type','Type');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(EntityContainer_FunctionImport_Type)].RegisterExternalPropertyName('ReturnTypeAtt','ReturnType');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(EntityContainer_AssociationSet_Type)].RegisterExternalPropertyName('_End','End');


End.
