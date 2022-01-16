{
This unit has been produced by ws_helper.
  Input unit name : "edm".
  This unit name  : "edm".
  Date            : "27-5-16 21:52:37".
}
unit edm;
{$IFDEF FPC}
  {$mode objfpc} {$H+}
{$ENDIF}
{$DEFINE WST_RECORD_RTTI}
interface

uses SysUtils, Classes, TypInfo, base_service_intf, service_intf;

const
  sNAME_SPACE = 'http://docs.oasis-open.org/odata/ns/edm';
  sUNIT_NAME = 'edm';

type

  Schema_ComplexTypeArray = class;
  Schema_EntityTypeArray = class;
  Schema_TypeDefinitionArray = class;
  Schema_EnumTypeArray = class;
  Schema_ActionArray = class;
  Schema__FunctionArray = class;
  Schema_TermArray = class;
  Schema_AnnotationsArray = class;
  Schema_EntityContainerArray = class;
  Schema_AnnotationArray = class;
  Schema = class;
  TTypeAttributes = class;
  TDerivableTypeAttributes = class;
  TEntityType_KeyArray = class;
  TEntityType__PropertyArray = class;
  TEntityType_NavigationPropertyArray = class;
  TEntityType_AnnotationArray = class;
  TEntityType = class;
  TEntityKeyElement = class;
  TPropertyRef = class;
  TComplexType__PropertyArray = class;
  TComplexType_NavigationPropertyArray = class;
  TComplexType_AnnotationArray = class;
  TComplexType = class;
  TFacetAttributes = class;
  TPropertyFacetAttributes = class;
  TCommonPropertyAttributes = class;
  TProperty_AnnotationArray = class;
  TProperty = class;
  TTypeDefinition_AnnotationArray = class;
  TTypeDefinition = class;
  TNavigationProperty_ReferentialConstraintArray = class;
  TNavigationProperty_OnDeleteArray = class;
  TNavigationProperty_AnnotationArray = class;
  TNavigationProperty = class;
  TReferentialConstraint_AnnotationArray = class;
  TReferentialConstraint = class;
  TOnDelete_AnnotationArray = class;
  TOnDelete = class;
  TEnumType_MemberArray = class;
  TEnumType_AnnotationArray = class;
  TEnumType = class;
  TEnumTypeMember_AnnotationArray = class;
  TEnumTypeMember = class;
  TActionFunctionReturnType_AnnotationArray = class;
  TActionFunctionReturnType = class;
  TActionAttributes = class;
  TAction_ParameterArray = class;
  TAction_AnnotationArray = class;
  TAction = class;
  TFunctionAttributes = class;
  TFunction_ParameterArray = class;
  TFunction_AnnotationArray = class;
  TFunction = class;
  TActionFunctionParameterAttributes = class;
  TActionFunctionParameter_AnnotationArray = class;
  TActionFunctionParameter = class;
  TTerm_AnnotationArray = class;
  TTerm = class;
  TAnnotations_AnnotationArray = class;
  TAnnotations = class;
  GExpression = class;
  GInlineExpressions = class;
  Annotation_AnnotationArray = class;
  Annotation_Type = class;
  TBinaryConstantExpression = class;
  TBoolConstantExpression = class;
  TDateConstantExpression = class;
  TDateTimeOffsetConstantExpression = class;
  TDecimalConstantExpression = class;
  TDurationConstantExpression = class;
  TFloatConstantExpression = class;
  TGuidConstantExpression = class;
  TIntConstantExpression = class;
  TStringConstantExpression = class;
  TTimeOfDayConstantExpression = class;
  TApplyExpression_AnnotationArray = class;
  TApplyExpression = class;
  TCastOrIsOfExpression_AnnotationArray = class;
  TCastOrIsOfExpression = class;
  TCollectionExpression = class;
  TIfExpression_AnnotationArray = class;
  TIfExpression = class;
  TOneChildExpression_AnnotationArray = class;
  TOneChildExpression = class;
  TTwoChildrenExpression_AnnotationArray = class;
  TTwoChildrenExpression = class;
  TLabeledElementExpression_AnnotationArray = class;
  TLabeledElementExpression = class;
  TLabeledElementReferenceExpression = class;
  TNullExpression = class;
  TPathExpression = class;
  TRecordExpression_PropertyValueArray = class;
  TRecordExpression_AnnotationArray = class;
  TRecordExpression = class;
  TPropertyValue_AnnotationArray = class;
  TPropertyValue = class;
  TEntityContainer_EntitySetArray = class;
  TEntityContainer_ActionImportArray = class;
  TEntityContainer_FunctionImportArray = class;
  TEntityContainer_SingletonArray = class;
  TEntityContainer_AnnotationArray = class;
  TEntityContainer = class;
  TEntitySetAttributes = class;
  TEntitySet_NavigationPropertyBindingArray = class;
  TEntitySet_AnnotationArray = class;
  TEntitySet = class;
  TNavigationPropertyBinding = class;
  TSingleton_NavigationPropertyBindingArray = class;
  TSingleton_AnnotationArray = class;
  TSingleton = class;
  TActionFunctionImportAttributes = class;
  TActionImport_AnnotationArray = class;
  TActionImport = class;
  TFunctionImport_AnnotationArray = class;
  TFunctionImport = class;

  TAbstractType = ( 
    TAbstractType_Edm_ComplexType
    ,TAbstractType_Edm_EntityType
    ,TAbstractType_Edm_PrimitiveType
    ,TAbstractType_Edm_Geography
    ,TAbstractType_Edm_Geometry
    ,TAbstractType_Edm_AnnotationPath
    ,TAbstractType_Edm_NavigationPropertyPath
    ,TAbstractType_Edm_PropertyPath
    ,TAbstractType_Collection_Edm_ComplexType
    ,TAbstractType_Collection_Edm_EntityType
    ,TAbstractType_Collection_Edm_PrimitiveType
    ,TAbstractType_Collection_Edm_Geography
    ,TAbstractType_Collection_Edm_Geometry
    ,TAbstractType_Collection_Edm_AnnotationPath
    ,TAbstractType_Collection_Edm_NavigationPropertyPath
    ,TAbstractType_Collection_Edm_PropertyPath
  );

  TMax = ( 
    max
  );

  TVariable = ( 
    variable
  );

  TOnDeleteAction = ( 
    Cascade
    ,None
    ,SetDefault
    ,SetNull
  );

  TGuidLiteral = type UnicodeString;

  TSimpleIdentifier = type NCName;

  TNamespaceName = type NCName;

  TQualifiedName = type NCName;

  TEnumMemberList = type UnicodeString;

  TTypeName = type UnicodeString;

  TPath = type UnicodeString;

  TPathWithTermSegments = type UnicodeString;

  TClientFunction = type UnicodeString;

  TPrimitiveType = type UnicodeString;

  TAppliesTo = type UnicodeString;

  TMaxLengthFacet = type UnicodeString;

  TPrecisionFacet = type nonNegativeInteger;

  TScaleFacet = type UnicodeString;

  TSridFacet = type UnicodeString;

  TUnicodeFacet = type boolean;

  binary = type UnicodeString;

  _boolean = type boolean;

  date = type TDateRemotable;

  time = type TTimeRemotable;

  dateTimeStamp = type TDateTimeRemotable;

  dayTimeDuration = type TDurationRemotable;

  Schema = class(TBaseComplexRemotable)
  private
    FComplexType : Schema_ComplexTypeArray;
    FEntityType : Schema_EntityTypeArray;
    FTypeDefinition : Schema_TypeDefinitionArray;
    FEnumType : Schema_EnumTypeArray;
    FAction : Schema_ActionArray;
    F_Function : Schema__FunctionArray;
    FTerm : Schema_TermArray;
    FAnnotations : Schema_AnnotationsArray;
    FEntityContainer : Schema_EntityContainerArray;
    FAnnotation : Schema_AnnotationArray;
    FNamespace : TNamespaceName;
    FAlias : TSimpleIdentifier;
  private
    function wstHas_ComplexType() : Boolean;
    function wstHas_EntityType() : Boolean;
    function wstHas_TypeDefinition() : Boolean;
    function wstHas_EnumType() : Boolean;
    function wstHas_Action() : Boolean;
    function wstHas__Function() : Boolean;
    function wstHas_Term() : Boolean;
    function wstHas_Annotations() : Boolean;
    function wstHas_EntityContainer() : Boolean;
    function wstHas_Annotation() : Boolean;
    function wstHas_Alias() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property ComplexType : Schema_ComplexTypeArray read FComplexType write FComplexType stored wstHas_ComplexType;
    property EntityType : Schema_EntityTypeArray read FEntityType write FEntityType stored wstHas_EntityType;
    property TypeDefinition : Schema_TypeDefinitionArray read FTypeDefinition write FTypeDefinition stored wstHas_TypeDefinition;
    property EnumType : Schema_EnumTypeArray read FEnumType write FEnumType stored wstHas_EnumType;
    property Action : Schema_ActionArray read FAction write FAction stored wstHas_Action;
    property _Function : Schema__FunctionArray read F_Function write F_Function stored wstHas__Function;
    property Term : Schema_TermArray read FTerm write FTerm stored wstHas_Term;
    property Annotations : Schema_AnnotationsArray read FAnnotations write FAnnotations stored wstHas_Annotations;
    property EntityContainer : Schema_EntityContainerArray read FEntityContainer write FEntityContainer stored wstHas_EntityContainer;
    property Annotation : Schema_AnnotationArray read FAnnotation write FAnnotation stored wstHas_Annotation;
    property Namespace : TNamespaceName read FNamespace write FNamespace;
    property Alias : TSimpleIdentifier read FAlias write FAlias stored wstHas_Alias;
  end;

  TTypeAttributes = class(TBaseComplexRemotable)
  private
    FName : TSimpleIdentifier;
  published
    property Name : TSimpleIdentifier read FName write FName;
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
    FKey : TEntityType_KeyArray;
    F_Property : TEntityType__PropertyArray;
    FNavigationProperty : TEntityType_NavigationPropertyArray;
    FAnnotation : TEntityType_AnnotationArray;
    FOpenType : boolean;
    FHasStream : boolean;
    FBaseType : TQualifiedName;
    F_Abstract : boolean;
    FName : TSimpleIdentifier;
  private
    function wstHas_Key() : Boolean;
    function wstHas__Property() : Boolean;
    function wstHas_NavigationProperty() : Boolean;
    function wstHas_Annotation() : Boolean;
    function wstHas_OpenType() : Boolean;
    function wstHas_HasStream() : Boolean;
    function wstHas_BaseType() : Boolean;
    function wstHas__Abstract() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Key : TEntityType_KeyArray read FKey write FKey stored wstHas_Key;
    property _Property : TEntityType__PropertyArray read F_Property write F_Property stored wstHas__Property;
    property NavigationProperty : TEntityType_NavigationPropertyArray read FNavigationProperty write FNavigationProperty stored wstHas_NavigationProperty;
    property Annotation : TEntityType_AnnotationArray read FAnnotation write FAnnotation stored wstHas_Annotation;
    property OpenType : boolean read FOpenType write FOpenType stored wstHas_OpenType;
    property HasStream : boolean read FHasStream write FHasStream stored wstHas_HasStream;
    property BaseType : TQualifiedName read FBaseType write FBaseType stored wstHas_BaseType;
    property _Abstract : boolean read F_Abstract write F_Abstract stored wstHas__Abstract;
    property Name : TSimpleIdentifier read FName write FName;
  end;

  TPropertyRef = class(TBaseComplexRemotable)
  private
    FName : TPath;
    FAlias : TSimpleIdentifier;
  private
    function wstHas_Alias() : Boolean;
  published
    property Name : TPath read FName write FName;
    property Alias : TSimpleIdentifier read FAlias write FAlias stored wstHas_Alias;
  end;

  TComplexType = class(TBaseComplexRemotable)
  private
    F_Property : TComplexType__PropertyArray;
    FNavigationProperty : TComplexType_NavigationPropertyArray;
    FAnnotation : TComplexType_AnnotationArray;
    FOpenType : boolean;
    FBaseType : TQualifiedName;
    F_Abstract : boolean;
    FName : TSimpleIdentifier;
  private
    function wstHas__Property() : Boolean;
    function wstHas_NavigationProperty() : Boolean;
    function wstHas_Annotation() : Boolean;
    function wstHas_OpenType() : Boolean;
    function wstHas_BaseType() : Boolean;
    function wstHas__Abstract() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property _Property : TComplexType__PropertyArray read F_Property write F_Property stored wstHas__Property;
    property NavigationProperty : TComplexType_NavigationPropertyArray read FNavigationProperty write FNavigationProperty stored wstHas_NavigationProperty;
    property Annotation : TComplexType_AnnotationArray read FAnnotation write FAnnotation stored wstHas_Annotation;
    property OpenType : boolean read FOpenType write FOpenType stored wstHas_OpenType;
    property BaseType : TQualifiedName read FBaseType write FBaseType stored wstHas_BaseType;
    property _Abstract : boolean read F_Abstract write F_Abstract stored wstHas__Abstract;
    property Name : TSimpleIdentifier read FName write FName;
  end;

  TFacetAttributes = class(TBaseComplexRemotable)
  private
    FMaxLength : TMaxLengthFacet;
    FPrecision : TPrecisionFacet;
    FScale : TScaleFacet;
    FSRID : TSridFacet;
  private
    function wstHas_MaxLength() : Boolean;
    function wstHas_Precision() : Boolean;
    function wstHas_Scale() : Boolean;
    function wstHas_SRID() : Boolean;
  published
    property MaxLength : TMaxLengthFacet read FMaxLength write FMaxLength stored wstHas_MaxLength;
    property Precision : TPrecisionFacet read FPrecision write FPrecision stored wstHas_Precision;
    property Scale : TScaleFacet read FScale write FScale stored wstHas_Scale;
    property SRID : TSridFacet read FSRID write FSRID stored wstHas_SRID;
  end;

  TPropertyFacetAttributes = class(TBaseComplexRemotable)
  private
    FUnicode : TUnicodeFacet;
  private
    function wstHas_Unicode() : Boolean;
  published
    property Unicode : TUnicodeFacet read FUnicode write FUnicode stored wstHas_Unicode;
  end;

  TCommonPropertyAttributes = class(TBaseComplexRemotable)
  private
    FName : TSimpleIdentifier;
    F_Type : TTypeName;
    FNullable : boolean;
    FDefaultValue : UnicodeString;
    FMaxLength : TMaxLengthFacet;
    FPrecision : TPrecisionFacet;
    FScale : TScaleFacet;
    FSRID : TSridFacet;
    FUnicode : TUnicodeFacet;
  private
    function wstHas_Nullable() : Boolean;
    function wstHas_DefaultValue() : Boolean;
    function wstHas_MaxLength() : Boolean;
    function wstHas_Precision() : Boolean;
    function wstHas_Scale() : Boolean;
    function wstHas_SRID() : Boolean;
    function wstHas_Unicode() : Boolean;
  published
    property Name : TSimpleIdentifier read FName write FName;
    property _Type : TTypeName read F_Type write F_Type;
    property Nullable : boolean read FNullable write FNullable stored wstHas_Nullable;
    property DefaultValue : UnicodeString read FDefaultValue write FDefaultValue stored wstHas_DefaultValue;
    property MaxLength : TMaxLengthFacet read FMaxLength write FMaxLength stored wstHas_MaxLength;
    property Precision : TPrecisionFacet read FPrecision write FPrecision stored wstHas_Precision;
    property Scale : TScaleFacet read FScale write FScale stored wstHas_Scale;
    property SRID : TSridFacet read FSRID write FSRID stored wstHas_SRID;
    property Unicode : TUnicodeFacet read FUnicode write FUnicode stored wstHas_Unicode;
  end;

  TProperty = class(TBaseComplexRemotable)
  private
    FAnnotation : TProperty_AnnotationArray;
    FName : TSimpleIdentifier;
    F_Type : TTypeName;
    FNullable : boolean;
    FDefaultValue : UnicodeString;
    FMaxLength : TMaxLengthFacet;
    FPrecision : TPrecisionFacet;
    FScale : TScaleFacet;
    FSRID : TSridFacet;
    FUnicode : TUnicodeFacet;
  private
    function wstHas_Annotation() : Boolean;
    function wstHas_Nullable() : Boolean;
    function wstHas_DefaultValue() : Boolean;
    function wstHas_MaxLength() : Boolean;
    function wstHas_Precision() : Boolean;
    function wstHas_Scale() : Boolean;
    function wstHas_SRID() : Boolean;
    function wstHas_Unicode() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Annotation : TProperty_AnnotationArray read FAnnotation write FAnnotation stored wstHas_Annotation;
    property Name : TSimpleIdentifier read FName write FName;
    property _Type : TTypeName read F_Type write F_Type;
    property Nullable : boolean read FNullable write FNullable stored wstHas_Nullable;
    property DefaultValue : UnicodeString read FDefaultValue write FDefaultValue stored wstHas_DefaultValue;
    property MaxLength : TMaxLengthFacet read FMaxLength write FMaxLength stored wstHas_MaxLength;
    property Precision : TPrecisionFacet read FPrecision write FPrecision stored wstHas_Precision;
    property Scale : TScaleFacet read FScale write FScale stored wstHas_Scale;
    property SRID : TSridFacet read FSRID write FSRID stored wstHas_SRID;
    property Unicode : TUnicodeFacet read FUnicode write FUnicode stored wstHas_Unicode;
  end;

  TTypeDefinition = class(TBaseComplexRemotable)
  private
    FAnnotation : TTypeDefinition_AnnotationArray;
    FName : TSimpleIdentifier;
    FUnderlyingType : TPrimitiveType;
    FMaxLength : TMaxLengthFacet;
    FPrecision : TPrecisionFacet;
    FScale : TScaleFacet;
    FSRID : TSridFacet;
    FUnicode : TUnicodeFacet;
  private
    function wstHas_Annotation() : Boolean;
    function wstHas_MaxLength() : Boolean;
    function wstHas_Precision() : Boolean;
    function wstHas_Scale() : Boolean;
    function wstHas_SRID() : Boolean;
    function wstHas_Unicode() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Annotation : TTypeDefinition_AnnotationArray read FAnnotation write FAnnotation stored wstHas_Annotation;
    property Name : TSimpleIdentifier read FName write FName;
    property UnderlyingType : TPrimitiveType read FUnderlyingType write FUnderlyingType;
    property MaxLength : TMaxLengthFacet read FMaxLength write FMaxLength stored wstHas_MaxLength;
    property Precision : TPrecisionFacet read FPrecision write FPrecision stored wstHas_Precision;
    property Scale : TScaleFacet read FScale write FScale stored wstHas_Scale;
    property SRID : TSridFacet read FSRID write FSRID stored wstHas_SRID;
    property Unicode : TUnicodeFacet read FUnicode write FUnicode stored wstHas_Unicode;
  end;

  TNavigationProperty = class(TBaseComplexRemotable)
  private
    FReferentialConstraint : TNavigationProperty_ReferentialConstraintArray;
    FOnDelete : TNavigationProperty_OnDeleteArray;
    FAnnotation : TNavigationProperty_AnnotationArray;
    FName : TSimpleIdentifier;
    F_Type : TTypeName;
    FNullable : boolean;
    FPartner : TPath;
    FContainsTarget : boolean;
  private
    function wstHas_ReferentialConstraint() : Boolean;
    function wstHas_OnDelete() : Boolean;
    function wstHas_Annotation() : Boolean;
    function wstHas_Nullable() : Boolean;
    function wstHas_Partner() : Boolean;
    function wstHas_ContainsTarget() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property ReferentialConstraint : TNavigationProperty_ReferentialConstraintArray read FReferentialConstraint write FReferentialConstraint stored wstHas_ReferentialConstraint;
    property OnDelete : TNavigationProperty_OnDeleteArray read FOnDelete write FOnDelete stored wstHas_OnDelete;
    property Annotation : TNavigationProperty_AnnotationArray read FAnnotation write FAnnotation stored wstHas_Annotation;
    property Name : TSimpleIdentifier read FName write FName;
    property _Type : TTypeName read F_Type write F_Type;
    property Nullable : boolean read FNullable write FNullable stored wstHas_Nullable;
    property Partner : TPath read FPartner write FPartner stored wstHas_Partner;
    property ContainsTarget : boolean read FContainsTarget write FContainsTarget stored wstHas_ContainsTarget;
  end;

  TReferentialConstraint = class(TBaseComplexRemotable)
  private
    FAnnotation : TReferentialConstraint_AnnotationArray;
    F_Property : TPath;
    FReferencedProperty : TPath;
  private
    function wstHas_Annotation() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Annotation : TReferentialConstraint_AnnotationArray read FAnnotation write FAnnotation stored wstHas_Annotation;
    property _Property : TPath read F_Property write F_Property;
    property ReferencedProperty : TPath read FReferencedProperty write FReferencedProperty;
  end;

  TOnDelete = class(TBaseComplexRemotable)
  private
    FAnnotation : TOnDelete_AnnotationArray;
    FAction : TOnDeleteAction;
  private
    function wstHas_Annotation() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Annotation : TOnDelete_AnnotationArray read FAnnotation write FAnnotation stored wstHas_Annotation;
    property Action : TOnDeleteAction read FAction write FAction;
  end;

  TEnumType = class(TBaseComplexRemotable)
  private
    FMember : TEnumType_MemberArray;
    FAnnotation : TEnumType_AnnotationArray;
    FIsFlags : boolean;
    FUnderlyingType : TTypeName;
    FName : TSimpleIdentifier;
  private
    function wstHas_Member() : Boolean;
    function wstHas_Annotation() : Boolean;
    function wstHas_IsFlags() : Boolean;
    function wstHas_UnderlyingType() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Member : TEnumType_MemberArray read FMember write FMember stored wstHas_Member;
    property Annotation : TEnumType_AnnotationArray read FAnnotation write FAnnotation stored wstHas_Annotation;
    property IsFlags : boolean read FIsFlags write FIsFlags stored wstHas_IsFlags;
    property UnderlyingType : TTypeName read FUnderlyingType write FUnderlyingType stored wstHas_UnderlyingType;
    property Name : TSimpleIdentifier read FName write FName;
  end;

  TEnumTypeMember = class(TBaseComplexRemotable)
  private
    FAnnotation : TEnumTypeMember_AnnotationArray;
    FName : TSimpleIdentifier;
    FValue : Int64;
  private
    function wstHas_Annotation() : Boolean;
    function wstHas_Value() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Annotation : TEnumTypeMember_AnnotationArray read FAnnotation write FAnnotation stored wstHas_Annotation;
    property Name : TSimpleIdentifier read FName write FName;
    property Value : Int64 read FValue write FValue stored wstHas_Value;
  end;

  TActionFunctionReturnType = class(TBaseComplexRemotable)
  private
    FAnnotation : TActionFunctionReturnType_AnnotationArray;
    F_Type : TTypeName;
    FNullable : boolean;
    FMaxLength : TMaxLengthFacet;
    FPrecision : TPrecisionFacet;
    FScale : TScaleFacet;
    FSRID : TSridFacet;
  private
    function wstHas_Annotation() : Boolean;
    function wstHas_Nullable() : Boolean;
    function wstHas_MaxLength() : Boolean;
    function wstHas_Precision() : Boolean;
    function wstHas_Scale() : Boolean;
    function wstHas_SRID() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Annotation : TActionFunctionReturnType_AnnotationArray read FAnnotation write FAnnotation stored wstHas_Annotation;
    property _Type : TTypeName read F_Type write F_Type;
    property Nullable : boolean read FNullable write FNullable stored wstHas_Nullable;
    property MaxLength : TMaxLengthFacet read FMaxLength write FMaxLength stored wstHas_MaxLength;
    property Precision : TPrecisionFacet read FPrecision write FPrecision stored wstHas_Precision;
    property Scale : TScaleFacet read FScale write FScale stored wstHas_Scale;
    property SRID : TSridFacet read FSRID write FSRID stored wstHas_SRID;
  end;

  TActionAttributes = class(TBaseComplexRemotable)
  private
    FName : TSimpleIdentifier;
    FEntitySetPath : TPath;
    FIsBound : boolean;
  private
    function wstHas_EntitySetPath() : Boolean;
    function wstHas_IsBound() : Boolean;
  published
    property Name : TSimpleIdentifier read FName write FName;
    property EntitySetPath : TPath read FEntitySetPath write FEntitySetPath stored wstHas_EntitySetPath;
    property IsBound : boolean read FIsBound write FIsBound stored wstHas_IsBound;
  end;

  TAction = class(TBaseComplexRemotable)
  private
    FParameter : TAction_ParameterArray;
    FAnnotation : TAction_AnnotationArray;
    FReturnType : TActionFunctionReturnType;
    FName : TSimpleIdentifier;
    FEntitySetPath : TPath;
    FIsBound : boolean;
  private
    function wstHas_Parameter() : Boolean;
    function wstHas_Annotation() : Boolean;
    function wstHas_ReturnType() : Boolean;
    function wstHas_EntitySetPath() : Boolean;
    function wstHas_IsBound() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Parameter : TAction_ParameterArray read FParameter write FParameter stored wstHas_Parameter;
    property Annotation : TAction_AnnotationArray read FAnnotation write FAnnotation stored wstHas_Annotation;
    property ReturnType : TActionFunctionReturnType read FReturnType write FReturnType stored wstHas_ReturnType;
    property Name : TSimpleIdentifier read FName write FName;
    property EntitySetPath : TPath read FEntitySetPath write FEntitySetPath stored wstHas_EntitySetPath;
    property IsBound : boolean read FIsBound write FIsBound stored wstHas_IsBound;
  end;

  TFunctionAttributes = class(TBaseComplexRemotable)
  private
    FName : TSimpleIdentifier;
    FEntitySetPath : TPath;
    FIsBound : boolean;
    FIsComposable : boolean;
  private
    function wstHas_EntitySetPath() : Boolean;
    function wstHas_IsBound() : Boolean;
    function wstHas_IsComposable() : Boolean;
  published
    property Name : TSimpleIdentifier read FName write FName;
    property EntitySetPath : TPath read FEntitySetPath write FEntitySetPath stored wstHas_EntitySetPath;
    property IsBound : boolean read FIsBound write FIsBound stored wstHas_IsBound;
    property IsComposable : boolean read FIsComposable write FIsComposable stored wstHas_IsComposable;
  end;

  TFunction = class(TBaseComplexRemotable)
  private
    FParameter : TFunction_ParameterArray;
    FAnnotation : TFunction_AnnotationArray;
    FReturnType : TActionFunctionReturnType;
    FName : TSimpleIdentifier;
    FEntitySetPath : TPath;
    FIsBound : boolean;
    FIsComposable : boolean;
  private
    function wstHas_Parameter() : Boolean;
    function wstHas_Annotation() : Boolean;
    function wstHas_EntitySetPath() : Boolean;
    function wstHas_IsBound() : Boolean;
    function wstHas_IsComposable() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Parameter : TFunction_ParameterArray read FParameter write FParameter stored wstHas_Parameter;
    property Annotation : TFunction_AnnotationArray read FAnnotation write FAnnotation stored wstHas_Annotation;
    property ReturnType : TActionFunctionReturnType read FReturnType write FReturnType;
    property Name : TSimpleIdentifier read FName write FName;
    property EntitySetPath : TPath read FEntitySetPath write FEntitySetPath stored wstHas_EntitySetPath;
    property IsBound : boolean read FIsBound write FIsBound stored wstHas_IsBound;
    property IsComposable : boolean read FIsComposable write FIsComposable stored wstHas_IsComposable;
  end;

  TActionFunctionParameterAttributes = class(TBaseComplexRemotable)
  private
    FName : TSimpleIdentifier;
    F_Type : TTypeName;
    FNullable : boolean;
    FMaxLength : TMaxLengthFacet;
    FPrecision : TPrecisionFacet;
    FScale : TScaleFacet;
    FSRID : TSridFacet;
  private
    function wstHas_Nullable() : Boolean;
    function wstHas_MaxLength() : Boolean;
    function wstHas_Precision() : Boolean;
    function wstHas_Scale() : Boolean;
    function wstHas_SRID() : Boolean;
  published
    property Name : TSimpleIdentifier read FName write FName;
    property _Type : TTypeName read F_Type write F_Type;
    property Nullable : boolean read FNullable write FNullable stored wstHas_Nullable;
    property MaxLength : TMaxLengthFacet read FMaxLength write FMaxLength stored wstHas_MaxLength;
    property Precision : TPrecisionFacet read FPrecision write FPrecision stored wstHas_Precision;
    property Scale : TScaleFacet read FScale write FScale stored wstHas_Scale;
    property SRID : TSridFacet read FSRID write FSRID stored wstHas_SRID;
  end;

  TActionFunctionParameter = class(TBaseComplexRemotable)
  private
    FAnnotation : TActionFunctionParameter_AnnotationArray;
    FName : TSimpleIdentifier;
    F_Type : TTypeName;
    FNullable : boolean;
    FMaxLength : TMaxLengthFacet;
    FPrecision : TPrecisionFacet;
    FScale : TScaleFacet;
    FSRID : TSridFacet;
  private
    function wstHas_Annotation() : Boolean;
    function wstHas_Nullable() : Boolean;
    function wstHas_MaxLength() : Boolean;
    function wstHas_Precision() : Boolean;
    function wstHas_Scale() : Boolean;
    function wstHas_SRID() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Annotation : TActionFunctionParameter_AnnotationArray read FAnnotation write FAnnotation stored wstHas_Annotation;
    property Name : TSimpleIdentifier read FName write FName;
    property _Type : TTypeName read F_Type write F_Type;
    property Nullable : boolean read FNullable write FNullable stored wstHas_Nullable;
    property MaxLength : TMaxLengthFacet read FMaxLength write FMaxLength stored wstHas_MaxLength;
    property Precision : TPrecisionFacet read FPrecision write FPrecision stored wstHas_Precision;
    property Scale : TScaleFacet read FScale write FScale stored wstHas_Scale;
    property SRID : TSridFacet read FSRID write FSRID stored wstHas_SRID;
  end;

  TTerm = class(TBaseComplexRemotable)
  private
    FAnnotation : TTerm_AnnotationArray;
    FName : TSimpleIdentifier;
    F_Type : TTypeName;
    FBaseTerm : TQualifiedName;
    FNullable : boolean;
    FDefaultValue : UnicodeString;
    FAppliesTo : TAppliesTo;
    FMaxLength : TMaxLengthFacet;
    FPrecision : TPrecisionFacet;
    FScale : TScaleFacet;
    FSRID : TSridFacet;
  private
    function wstHas_Annotation() : Boolean;
    function wstHas_BaseTerm() : Boolean;
    function wstHas_Nullable() : Boolean;
    function wstHas_DefaultValue() : Boolean;
    function wstHas_AppliesTo() : Boolean;
    function wstHas_MaxLength() : Boolean;
    function wstHas_Precision() : Boolean;
    function wstHas_Scale() : Boolean;
    function wstHas_SRID() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Annotation : TTerm_AnnotationArray read FAnnotation write FAnnotation stored wstHas_Annotation;
    property Name : TSimpleIdentifier read FName write FName;
    property _Type : TTypeName read F_Type write F_Type;
    property BaseTerm : TQualifiedName read FBaseTerm write FBaseTerm stored wstHas_BaseTerm;
    property Nullable : boolean read FNullable write FNullable stored wstHas_Nullable;
    property DefaultValue : UnicodeString read FDefaultValue write FDefaultValue stored wstHas_DefaultValue;
    property AppliesTo : TAppliesTo read FAppliesTo write FAppliesTo stored wstHas_AppliesTo;
    property MaxLength : TMaxLengthFacet read FMaxLength write FMaxLength stored wstHas_MaxLength;
    property Precision : TPrecisionFacet read FPrecision write FPrecision stored wstHas_Precision;
    property Scale : TScaleFacet read FScale write FScale stored wstHas_Scale;
    property SRID : TSridFacet read FSRID write FSRID stored wstHas_SRID;
  end;

  TAnnotations = class(TBaseComplexRemotable)
  private
    FAnnotation : TAnnotations_AnnotationArray;
    FTarget : TPath;
    FQualifier : TSimpleIdentifier;
  private
    function wstHas_Qualifier() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Annotation : TAnnotations_AnnotationArray read FAnnotation write FAnnotation;
    property Target : TPath read FTarget write FTarget;
    property Qualifier : TSimpleIdentifier read FQualifier write FQualifier stored wstHas_Qualifier;
  end;

  GExpression = class(TBaseComplexRemotable)
  private
    FBinary : TBinaryConstantExpression;
    FBool : TBoolConstantExpression;
    FDate : TDateConstantExpression;
    FDateTimeOffset : TDateTimeOffsetConstantExpression;
    FDecimal : TDecimalConstantExpression;
    FDuration : TDurationConstantExpression;
    FEnumMember : TEnumMemberList;
    FFloat : TFloatConstantExpression;
    FGuid : TGuidConstantExpression;
    FInt : TIntConstantExpression;
    F_String : TStringConstantExpression;
    FTimeOfDay : TTimeOfDayConstantExpression;
    FAnnotationPath : TPathExpression;
    FApply : TApplyExpression;
    FCast : TCastOrIsOfExpression;
    FCollection : TCollectionExpression;
    F_If : TIfExpression;
    FEq : TTwoChildrenExpression;
    FNe : TTwoChildrenExpression;
    FGe : TTwoChildrenExpression;
    FGt : TTwoChildrenExpression;
    FLe : TTwoChildrenExpression;
    FLt : TTwoChildrenExpression;
    F_And : TTwoChildrenExpression;
    F_Or : TTwoChildrenExpression;
    F_Not : TOneChildExpression;
    FIsOf : TCastOrIsOfExpression;
    FLabeledElement : TLabeledElementExpression;
    FLabeledElementReference : TLabeledElementReferenceExpression;
    FNull : TNullExpression;
    FNavigationPropertyPath : TPathExpression;
    FPath : TPathExpression;
    FPropertyPath : TPathExpression;
    F_Record : TRecordExpression;
    FUrlRef : TOneChildExpression;
  private
    function wstHas_Binary() : Boolean;
    function wstHas_Bool() : Boolean;
    function wstHas_Date() : Boolean;
    function wstHas_DateTimeOffset() : Boolean;
    function wstHas_Decimal() : Boolean;
    function wstHas_Duration() : Boolean;
    function wstHas_EnumMember() : Boolean;
    function wstHas_Float() : Boolean;
    function wstHas_Guid() : Boolean;
    function wstHas_Int() : Boolean;
    function wstHas__String() : Boolean;
    function wstHas_TimeOfDay() : Boolean;
    function wstHas_AnnotationPath() : Boolean;
    function wstHas_Apply() : Boolean;
    function wstHas_Cast() : Boolean;
    function wstHas_Collection() : Boolean;
    function wstHas__If() : Boolean;
    function wstHas_Eq() : Boolean;
    function wstHas_Ne() : Boolean;
    function wstHas_Ge() : Boolean;
    function wstHas_Gt() : Boolean;
    function wstHas_Le() : Boolean;
    function wstHas_Lt() : Boolean;
    function wstHas__And() : Boolean;
    function wstHas__Or() : Boolean;
    function wstHas__Not() : Boolean;
    function wstHas_IsOf() : Boolean;
    function wstHas_LabeledElement() : Boolean;
    function wstHas_LabeledElementReference() : Boolean;
    function wstHas_Null() : Boolean;
    function wstHas_NavigationPropertyPath() : Boolean;
    function wstHas_Path() : Boolean;
    function wstHas_PropertyPath() : Boolean;
    function wstHas__Record() : Boolean;
    function wstHas_UrlRef() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Binary : TBinaryConstantExpression read FBinary write FBinary stored wstHas_Binary;
    property Bool : TBoolConstantExpression read FBool write FBool stored wstHas_Bool;
    property Date : TDateConstantExpression read FDate write FDate stored wstHas_Date;
    property DateTimeOffset : TDateTimeOffsetConstantExpression read FDateTimeOffset write FDateTimeOffset stored wstHas_DateTimeOffset;
    property Decimal : TDecimalConstantExpression read FDecimal write FDecimal stored wstHas_Decimal;
    property Duration : TDurationConstantExpression read FDuration write FDuration stored wstHas_Duration;
    property EnumMember : TEnumMemberList read FEnumMember write FEnumMember stored wstHas_EnumMember;
    property Float : TFloatConstantExpression read FFloat write FFloat stored wstHas_Float;
    property Guid : TGuidConstantExpression read FGuid write FGuid stored wstHas_Guid;
    property Int : TIntConstantExpression read FInt write FInt stored wstHas_Int;
    property _String : TStringConstantExpression read F_String write F_String stored wstHas__String;
    property TimeOfDay : TTimeOfDayConstantExpression read FTimeOfDay write FTimeOfDay stored wstHas_TimeOfDay;
    property AnnotationPath : TPathExpression read FAnnotationPath write FAnnotationPath stored wstHas_AnnotationPath;
    property Apply : TApplyExpression read FApply write FApply stored wstHas_Apply;
    property Cast : TCastOrIsOfExpression read FCast write FCast stored wstHas_Cast;
    property Collection : TCollectionExpression read FCollection write FCollection stored wstHas_Collection;
    property _If : TIfExpression read F_If write F_If stored wstHas__If;
    property Eq : TTwoChildrenExpression read FEq write FEq stored wstHas_Eq;
    property Ne : TTwoChildrenExpression read FNe write FNe stored wstHas_Ne;
    property Ge : TTwoChildrenExpression read FGe write FGe stored wstHas_Ge;
    property Gt : TTwoChildrenExpression read FGt write FGt stored wstHas_Gt;
    property Le : TTwoChildrenExpression read FLe write FLe stored wstHas_Le;
    property Lt : TTwoChildrenExpression read FLt write FLt stored wstHas_Lt;
    property _And : TTwoChildrenExpression read F_And write F_And stored wstHas__And;
    property _Or : TTwoChildrenExpression read F_Or write F_Or stored wstHas__Or;
    property _Not : TOneChildExpression read F_Not write F_Not stored wstHas__Not;
    property IsOf : TCastOrIsOfExpression read FIsOf write FIsOf stored wstHas_IsOf;
    property LabeledElement : TLabeledElementExpression read FLabeledElement write FLabeledElement stored wstHas_LabeledElement;
    property LabeledElementReference : TLabeledElementReferenceExpression read FLabeledElementReference write FLabeledElementReference stored wstHas_LabeledElementReference;
    property Null : TNullExpression read FNull write FNull stored wstHas_Null;
    property NavigationPropertyPath : TPathExpression read FNavigationPropertyPath write FNavigationPropertyPath stored wstHas_NavigationPropertyPath;
    property Path : TPathExpression read FPath write FPath stored wstHas_Path;
    property PropertyPath : TPathExpression read FPropertyPath write FPropertyPath stored wstHas_PropertyPath;
    property _Record : TRecordExpression read F_Record write F_Record stored wstHas__Record;
    property UrlRef : TOneChildExpression read FUrlRef write FUrlRef stored wstHas_UrlRef;
  end;

  GInlineExpressions = class(TBaseComplexRemotable)
  private
    FBinary : edm.binary;
    FBool : boolean;
    FDate : TDateRemotable;
    FDateTimeOffset : dateTimeStamp;
    FDecimal : Currency;
    FDuration : dayTimeDuration;
    FEnumMember : TEnumMemberList;
    FFloat : Double;
    FGuid : TGuidLiteral;
    FInt : integer;
    F_String : UnicodeString;
    FTimeOfDay : TTimeRemotable;
    FAnnotationPath : TPathWithTermSegments;
    FNavigationPropertyPath : TPathWithTermSegments;
    FPath : TPathWithTermSegments;
    FPropertyPath : TPathWithTermSegments;
    FUrlRef : anyURI;
  private
    function wstHas_Binary() : Boolean;
    function wstHas_Bool() : Boolean;
    function wstHas_Date() : Boolean;
    function wstHas_DateTimeOffset() : Boolean;
    function wstHas_Decimal() : Boolean;
    function wstHas_Duration() : Boolean;
    function wstHas_EnumMember() : Boolean;
    function wstHas_Float() : Boolean;
    function wstHas_Guid() : Boolean;
    function wstHas_Int() : Boolean;
    function wstHas__String() : Boolean;
    function wstHas_TimeOfDay() : Boolean;
    function wstHas_AnnotationPath() : Boolean;
    function wstHas_NavigationPropertyPath() : Boolean;
    function wstHas_Path() : Boolean;
    function wstHas_PropertyPath() : Boolean;
    function wstHas_UrlRef() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Binary : edm.binary read FBinary write FBinary stored wstHas_Binary;
    property Bool : boolean read FBool write FBool stored wstHas_Bool;
    property Date : TDateRemotable read FDate write FDate stored wstHas_Date;
    property DateTimeOffset : dateTimeStamp read FDateTimeOffset write FDateTimeOffset stored wstHas_DateTimeOffset;
    property Decimal : Currency read FDecimal write FDecimal stored wstHas_Decimal;
    property Duration : dayTimeDuration read FDuration write FDuration stored wstHas_Duration;
    property EnumMember : TEnumMemberList read FEnumMember write FEnumMember stored wstHas_EnumMember;
    property Float : Double read FFloat write FFloat stored wstHas_Float;
    property Guid : TGuidLiteral read FGuid write FGuid stored wstHas_Guid;
    property Int : integer read FInt write FInt stored wstHas_Int;
    property _String : UnicodeString read F_String write F_String stored wstHas__String;
    property TimeOfDay : TTimeRemotable read FTimeOfDay write FTimeOfDay stored wstHas_TimeOfDay;
    property AnnotationPath : TPathWithTermSegments read FAnnotationPath write FAnnotationPath stored wstHas_AnnotationPath;
    property NavigationPropertyPath : TPathWithTermSegments read FNavigationPropertyPath write FNavigationPropertyPath stored wstHas_NavigationPropertyPath;
    property Path : TPathWithTermSegments read FPath write FPath stored wstHas_Path;
    property PropertyPath : TPathWithTermSegments read FPropertyPath write FPropertyPath stored wstHas_PropertyPath;
    property UrlRef : anyURI read FUrlRef write FUrlRef stored wstHas_UrlRef;
  end;

  Annotation_Type = class(TBaseComplexRemotable)
  private
    FAnnotation : Annotation_AnnotationArray;
    FTerm : TQualifiedName;
    FQualifier : TSimpleIdentifier;
    FBinary : TBinaryConstantExpression;
    FBool : TBoolConstantExpression;
    FDate : TDateConstantExpression;
    FDateTimeOffset : TDateTimeOffsetConstantExpression;
    FDecimal : TDecimalConstantExpression;
    FDuration : TDurationConstantExpression;
    FEnumMember : TEnumMemberList;
    FFloat : TFloatConstantExpression;
    FGuid : TGuidConstantExpression;
    FInt : TIntConstantExpression;
    F_String : TStringConstantExpression;
    FTimeOfDay : TTimeOfDayConstantExpression;
    FAnnotationPath : TPathExpression;
    FApply : TApplyExpression;
    FCast : TCastOrIsOfExpression;
    FCollection : TCollectionExpression;
    F_If : TIfExpression;
    FEq : TTwoChildrenExpression;
    FNe : TTwoChildrenExpression;
    FGe : TTwoChildrenExpression;
    FGt : TTwoChildrenExpression;
    FLe : TTwoChildrenExpression;
    FLt : TTwoChildrenExpression;
    F_And : TTwoChildrenExpression;
    F_Or : TTwoChildrenExpression;
    F_Not : TOneChildExpression;
    FIsOf : TCastOrIsOfExpression;
    FLabeledElement : TLabeledElementExpression;
    FLabeledElementReference : TLabeledElementReferenceExpression;
    FNull : TNullExpression;
    FNavigationPropertyPath : TPathExpression;
    FPath : TPathExpression;
    FPropertyPath : TPathExpression;
    F_Record : TRecordExpression;
    FUrlRef : TOneChildExpression;
    FBinaryAtt : binary;
    FBoolAtt : boolean;
    FDateAtt : TDateRemotable;
    FDateTimeOffsetAtt : dateTimeStamp;
    FDecimalAtt : Currency;
    FDurationAtt : dayTimeDuration;
    FEnumMemberAtt : TEnumMemberList;
    FFloatAtt : Double;
    FGuidAtt : TGuidLiteral;
    FIntAtt : integer;
    F_StringAtt : UnicodeString;
    FTimeOfDayAtt : TTimeRemotable;
    FAnnotationPathAtt : TPathWithTermSegments;
    FNavigationPropertyPathAtt : TPathWithTermSegments;
    FPathAtt : TPathWithTermSegments;
    FPropertyPathAtt : TPathWithTermSegments;
    FUrlRefAtt : anyURI;
  private
    function wstHas_Annotation() : Boolean;
    function wstHas_Qualifier() : Boolean;
    function wstHas_Binary() : Boolean;
    function wstHas_Bool() : Boolean;
    function wstHas_Date() : Boolean;
    function wstHas_DateTimeOffset() : Boolean;
    function wstHas_Decimal() : Boolean;
    function wstHas_Duration() : Boolean;
    function wstHas_EnumMember() : Boolean;
    function wstHas_Float() : Boolean;
    function wstHas_Guid() : Boolean;
    function wstHas_Int() : Boolean;
    function wstHas__String() : Boolean;
    function wstHas_TimeOfDay() : Boolean;
    function wstHas_AnnotationPath() : Boolean;
    function wstHas_Apply() : Boolean;
    function wstHas_Cast() : Boolean;
    function wstHas_Collection() : Boolean;
    function wstHas__If() : Boolean;
    function wstHas_Eq() : Boolean;
    function wstHas_Ne() : Boolean;
    function wstHas_Ge() : Boolean;
    function wstHas_Gt() : Boolean;
    function wstHas_Le() : Boolean;
    function wstHas_Lt() : Boolean;
    function wstHas__And() : Boolean;
    function wstHas__Or() : Boolean;
    function wstHas__Not() : Boolean;
    function wstHas_IsOf() : Boolean;
    function wstHas_LabeledElement() : Boolean;
    function wstHas_LabeledElementReference() : Boolean;
    function wstHas_Null() : Boolean;
    function wstHas_NavigationPropertyPath() : Boolean;
    function wstHas_Path() : Boolean;
    function wstHas_PropertyPath() : Boolean;
    function wstHas__Record() : Boolean;
    function wstHas_UrlRef() : Boolean;
    function wstHas_BinaryAtt() : Boolean;
    function wstHas_BoolAtt() : Boolean;
    function wstHas_DateAtt() : Boolean;
    function wstHas_DateTimeOffsetAtt() : Boolean;
    function wstHas_DecimalAtt() : Boolean;
    function wstHas_DurationAtt() : Boolean;
    function wstHas_EnumMemberAtt() : Boolean;
    function wstHas_FloatAtt() : Boolean;
    function wstHas_GuidAtt() : Boolean;
    function wstHas_IntAtt() : Boolean;
    function wstHas__StringAtt() : Boolean;
    function wstHas_TimeOfDayAtt() : Boolean;
    function wstHas_AnnotationPathAtt() : Boolean;
    function wstHas_NavigationPropertyPathAtt() : Boolean;
    function wstHas_PathAtt() : Boolean;
    function wstHas_PropertyPathAtt() : Boolean;
    function wstHas_UrlRefAtt() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Annotation : Annotation_AnnotationArray read FAnnotation write FAnnotation stored wstHas_Annotation;
    property Term : TQualifiedName read FTerm write FTerm;
    property Qualifier : TSimpleIdentifier read FQualifier write FQualifier stored wstHas_Qualifier;
    property Binary : TBinaryConstantExpression read FBinary write FBinary stored wstHas_Binary;
    property Bool : TBoolConstantExpression read FBool write FBool stored wstHas_Bool;
    property Date : TDateConstantExpression read FDate write FDate stored wstHas_Date;
    property DateTimeOffset : TDateTimeOffsetConstantExpression read FDateTimeOffset write FDateTimeOffset stored wstHas_DateTimeOffset;
    property Decimal : TDecimalConstantExpression read FDecimal write FDecimal stored wstHas_Decimal;
    property Duration : TDurationConstantExpression read FDuration write FDuration stored wstHas_Duration;
    property EnumMember : TEnumMemberList read FEnumMember write FEnumMember stored wstHas_EnumMember;
    property Float : TFloatConstantExpression read FFloat write FFloat stored wstHas_Float;
    property Guid : TGuidConstantExpression read FGuid write FGuid stored wstHas_Guid;
    property Int : TIntConstantExpression read FInt write FInt stored wstHas_Int;
    property _String : TStringConstantExpression read F_String write F_String stored wstHas__String;
    property TimeOfDay : TTimeOfDayConstantExpression read FTimeOfDay write FTimeOfDay stored wstHas_TimeOfDay;
    property AnnotationPath : TPathExpression read FAnnotationPath write FAnnotationPath stored wstHas_AnnotationPath;
    property Apply : TApplyExpression read FApply write FApply stored wstHas_Apply;
    property Cast : TCastOrIsOfExpression read FCast write FCast stored wstHas_Cast;
    property Collection : TCollectionExpression read FCollection write FCollection stored wstHas_Collection;
    property _If : TIfExpression read F_If write F_If stored wstHas__If;
    property Eq : TTwoChildrenExpression read FEq write FEq stored wstHas_Eq;
    property Ne : TTwoChildrenExpression read FNe write FNe stored wstHas_Ne;
    property Ge : TTwoChildrenExpression read FGe write FGe stored wstHas_Ge;
    property Gt : TTwoChildrenExpression read FGt write FGt stored wstHas_Gt;
    property Le : TTwoChildrenExpression read FLe write FLe stored wstHas_Le;
    property Lt : TTwoChildrenExpression read FLt write FLt stored wstHas_Lt;
    property _And : TTwoChildrenExpression read F_And write F_And stored wstHas__And;
    property _Or : TTwoChildrenExpression read F_Or write F_Or stored wstHas__Or;
    property _Not : TOneChildExpression read F_Not write F_Not stored wstHas__Not;
    property IsOf : TCastOrIsOfExpression read FIsOf write FIsOf stored wstHas_IsOf;
    property LabeledElement : TLabeledElementExpression read FLabeledElement write FLabeledElement stored wstHas_LabeledElement;
    property LabeledElementReference : TLabeledElementReferenceExpression read FLabeledElementReference write FLabeledElementReference stored wstHas_LabeledElementReference;
    property Null : TNullExpression read FNull write FNull stored wstHas_Null;
    property NavigationPropertyPath : TPathExpression read FNavigationPropertyPath write FNavigationPropertyPath stored wstHas_NavigationPropertyPath;
    property Path : TPathExpression read FPath write FPath stored wstHas_Path;
    property PropertyPath : TPathExpression read FPropertyPath write FPropertyPath stored wstHas_PropertyPath;
    property _Record : TRecordExpression read F_Record write F_Record stored wstHas__Record;
    property UrlRef : TOneChildExpression read FUrlRef write FUrlRef stored wstHas_UrlRef;
    property BinaryAtt : binary read FBinaryAtt write FBinaryAtt stored wstHas_BinaryAtt;
    property BoolAtt : boolean read FBoolAtt write FBoolAtt stored wstHas_BoolAtt;
    property DateAtt : TDateRemotable read FDateAtt write FDateAtt stored wstHas_DateAtt;
    property DateTimeOffsetAtt : dateTimeStamp read FDateTimeOffsetAtt write FDateTimeOffsetAtt stored wstHas_DateTimeOffsetAtt;
    property DecimalAtt : Currency read FDecimalAtt write FDecimalAtt stored wstHas_DecimalAtt;
    property DurationAtt : dayTimeDuration read FDurationAtt write FDurationAtt stored wstHas_DurationAtt;
    property EnumMemberAtt : TEnumMemberList read FEnumMemberAtt write FEnumMemberAtt stored wstHas_EnumMemberAtt;
    property FloatAtt : Double read FFloatAtt write FFloatAtt stored wstHas_FloatAtt;
    property GuidAtt : TGuidLiteral read FGuidAtt write FGuidAtt stored wstHas_GuidAtt;
    property IntAtt : integer read FIntAtt write FIntAtt stored wstHas_IntAtt;
    property _StringAtt : UnicodeString read F_StringAtt write F_StringAtt stored wstHas__StringAtt;
    property TimeOfDayAtt : TTimeRemotable read FTimeOfDayAtt write FTimeOfDayAtt stored wstHas_TimeOfDayAtt;
    property AnnotationPathAtt : TPathWithTermSegments read FAnnotationPathAtt write FAnnotationPathAtt stored wstHas_AnnotationPathAtt;
    property NavigationPropertyPathAtt : TPathWithTermSegments read FNavigationPropertyPathAtt write FNavigationPropertyPathAtt stored wstHas_NavigationPropertyPathAtt;
    property PathAtt : TPathWithTermSegments read FPathAtt write FPathAtt stored wstHas_PathAtt;
    property PropertyPathAtt : TPathWithTermSegments read FPropertyPathAtt write FPropertyPathAtt stored wstHas_PropertyPathAtt;
    property UrlRefAtt : anyURI read FUrlRefAtt write FUrlRefAtt stored wstHas_UrlRefAtt;
  end;

  TBinaryConstantExpression = class(TComplexUnicodeStringContentRemotable)
  end;

  TBoolConstantExpression = class(TComplexBooleanContentRemotable)
  end;

  TDateConstantExpression = class(date)
  end;

  TDateTimeOffsetConstantExpression = class(dateTimeStamp)
  end;

  TDecimalConstantExpression = class(TComplexCurrencyContentRemotable)
  end;

  TDurationConstantExpression = class(dayTimeDuration)
  end;

  TFloatConstantExpression = class(TComplexFloatDoubleContentRemotable)
  end;

  TGuidConstantExpression = class(TComplexUnicodeStringContentRemotable)
  end;

  TIntConstantExpression = class(TComplexInt32SContentRemotable)
  end;

  TStringConstantExpression = class(TComplexUnicodeStringContentRemotable)
  end;

  TTimeOfDayConstantExpression = class(time)
  end;

  TApplyExpression = class(TBaseComplexRemotable)
  private
    FAnnotation : TApplyExpression_AnnotationArray;
    F_Function : TClientFunction;
    FBinary : TBinaryConstantExpression;
    FBool : TBoolConstantExpression;
    FDate : TDateConstantExpression;
    FDateTimeOffset : TDateTimeOffsetConstantExpression;
    FDecimal : TDecimalConstantExpression;
    FDuration : TDurationConstantExpression;
    FEnumMember : TEnumMemberList;
    FFloat : TFloatConstantExpression;
    FGuid : TGuidConstantExpression;
    FInt : TIntConstantExpression;
    F_String : TStringConstantExpression;
    FTimeOfDay : TTimeOfDayConstantExpression;
    FAnnotationPath : TPathExpression;
    FApply : TApplyExpression;
    FCast : TCastOrIsOfExpression;
    FCollection : TCollectionExpression;
    F_If : TIfExpression;
    FEq : TTwoChildrenExpression;
    FNe : TTwoChildrenExpression;
    FGe : TTwoChildrenExpression;
    FGt : TTwoChildrenExpression;
    FLe : TTwoChildrenExpression;
    FLt : TTwoChildrenExpression;
    F_And : TTwoChildrenExpression;
    F_Or : TTwoChildrenExpression;
    F_Not : TOneChildExpression;
    FIsOf : TCastOrIsOfExpression;
    FLabeledElement : TLabeledElementExpression;
    FLabeledElementReference : TLabeledElementReferenceExpression;
    FNull : TNullExpression;
    FNavigationPropertyPath : TPathExpression;
    FPath : TPathExpression;
    FPropertyPath : TPathExpression;
    F_Record : TRecordExpression;
    FUrlRef : TOneChildExpression;
  private
    function wstHas_Annotation() : Boolean;
    function wstHas__Function() : Boolean;
    function wstHas_Binary() : Boolean;
    function wstHas_Bool() : Boolean;
    function wstHas_Date() : Boolean;
    function wstHas_DateTimeOffset() : Boolean;
    function wstHas_Decimal() : Boolean;
    function wstHas_Duration() : Boolean;
    function wstHas_EnumMember() : Boolean;
    function wstHas_Float() : Boolean;
    function wstHas_Guid() : Boolean;
    function wstHas_Int() : Boolean;
    function wstHas__String() : Boolean;
    function wstHas_TimeOfDay() : Boolean;
    function wstHas_AnnotationPath() : Boolean;
    function wstHas_Apply() : Boolean;
    function wstHas_Cast() : Boolean;
    function wstHas_Collection() : Boolean;
    function wstHas__If() : Boolean;
    function wstHas_Eq() : Boolean;
    function wstHas_Ne() : Boolean;
    function wstHas_Ge() : Boolean;
    function wstHas_Gt() : Boolean;
    function wstHas_Le() : Boolean;
    function wstHas_Lt() : Boolean;
    function wstHas__And() : Boolean;
    function wstHas__Or() : Boolean;
    function wstHas__Not() : Boolean;
    function wstHas_IsOf() : Boolean;
    function wstHas_LabeledElement() : Boolean;
    function wstHas_LabeledElementReference() : Boolean;
    function wstHas_Null() : Boolean;
    function wstHas_NavigationPropertyPath() : Boolean;
    function wstHas_Path() : Boolean;
    function wstHas_PropertyPath() : Boolean;
    function wstHas__Record() : Boolean;
    function wstHas_UrlRef() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Annotation : TApplyExpression_AnnotationArray read FAnnotation write FAnnotation stored wstHas_Annotation;
    property _Function : TClientFunction read F_Function write F_Function stored wstHas__Function;
    property Binary : TBinaryConstantExpression read FBinary write FBinary stored wstHas_Binary;
    property Bool : TBoolConstantExpression read FBool write FBool stored wstHas_Bool;
    property Date : TDateConstantExpression read FDate write FDate stored wstHas_Date;
    property DateTimeOffset : TDateTimeOffsetConstantExpression read FDateTimeOffset write FDateTimeOffset stored wstHas_DateTimeOffset;
    property Decimal : TDecimalConstantExpression read FDecimal write FDecimal stored wstHas_Decimal;
    property Duration : TDurationConstantExpression read FDuration write FDuration stored wstHas_Duration;
    property EnumMember : TEnumMemberList read FEnumMember write FEnumMember stored wstHas_EnumMember;
    property Float : TFloatConstantExpression read FFloat write FFloat stored wstHas_Float;
    property Guid : TGuidConstantExpression read FGuid write FGuid stored wstHas_Guid;
    property Int : TIntConstantExpression read FInt write FInt stored wstHas_Int;
    property _String : TStringConstantExpression read F_String write F_String stored wstHas__String;
    property TimeOfDay : TTimeOfDayConstantExpression read FTimeOfDay write FTimeOfDay stored wstHas_TimeOfDay;
    property AnnotationPath : TPathExpression read FAnnotationPath write FAnnotationPath stored wstHas_AnnotationPath;
    property Apply : TApplyExpression read FApply write FApply stored wstHas_Apply;
    property Cast : TCastOrIsOfExpression read FCast write FCast stored wstHas_Cast;
    property Collection : TCollectionExpression read FCollection write FCollection stored wstHas_Collection;
    property _If : TIfExpression read F_If write F_If stored wstHas__If;
    property Eq : TTwoChildrenExpression read FEq write FEq stored wstHas_Eq;
    property Ne : TTwoChildrenExpression read FNe write FNe stored wstHas_Ne;
    property Ge : TTwoChildrenExpression read FGe write FGe stored wstHas_Ge;
    property Gt : TTwoChildrenExpression read FGt write FGt stored wstHas_Gt;
    property Le : TTwoChildrenExpression read FLe write FLe stored wstHas_Le;
    property Lt : TTwoChildrenExpression read FLt write FLt stored wstHas_Lt;
    property _And : TTwoChildrenExpression read F_And write F_And stored wstHas__And;
    property _Or : TTwoChildrenExpression read F_Or write F_Or stored wstHas__Or;
    property _Not : TOneChildExpression read F_Not write F_Not stored wstHas__Not;
    property IsOf : TCastOrIsOfExpression read FIsOf write FIsOf stored wstHas_IsOf;
    property LabeledElement : TLabeledElementExpression read FLabeledElement write FLabeledElement stored wstHas_LabeledElement;
    property LabeledElementReference : TLabeledElementReferenceExpression read FLabeledElementReference write FLabeledElementReference stored wstHas_LabeledElementReference;
    property Null : TNullExpression read FNull write FNull stored wstHas_Null;
    property NavigationPropertyPath : TPathExpression read FNavigationPropertyPath write FNavigationPropertyPath stored wstHas_NavigationPropertyPath;
    property Path : TPathExpression read FPath write FPath stored wstHas_Path;
    property PropertyPath : TPathExpression read FPropertyPath write FPropertyPath stored wstHas_PropertyPath;
    property _Record : TRecordExpression read F_Record write F_Record stored wstHas__Record;
    property UrlRef : TOneChildExpression read FUrlRef write FUrlRef stored wstHas_UrlRef;
  end;

  TCastOrIsOfExpression = class(TBaseComplexRemotable)
  private
    FAnnotation : TCastOrIsOfExpression_AnnotationArray;
    F_Type : TTypeName;
    FBinary : TBinaryConstantExpression;
    FBool : TBoolConstantExpression;
    FDate : TDateConstantExpression;
    FDateTimeOffset : TDateTimeOffsetConstantExpression;
    FDecimal : TDecimalConstantExpression;
    FDuration : TDurationConstantExpression;
    FEnumMember : TEnumMemberList;
    FFloat : TFloatConstantExpression;
    FGuid : TGuidConstantExpression;
    FInt : TIntConstantExpression;
    F_String : TStringConstantExpression;
    FTimeOfDay : TTimeOfDayConstantExpression;
    FAnnotationPath : TPathExpression;
    FApply : TApplyExpression;
    FCast : TCastOrIsOfExpression;
    FCollection : TCollectionExpression;
    F_If : TIfExpression;
    FEq : TTwoChildrenExpression;
    FNe : TTwoChildrenExpression;
    FGe : TTwoChildrenExpression;
    FGt : TTwoChildrenExpression;
    FLe : TTwoChildrenExpression;
    FLt : TTwoChildrenExpression;
    F_And : TTwoChildrenExpression;
    F_Or : TTwoChildrenExpression;
    F_Not : TOneChildExpression;
    FIsOf : TCastOrIsOfExpression;
    FLabeledElement : TLabeledElementExpression;
    FLabeledElementReference : TLabeledElementReferenceExpression;
    FNull : TNullExpression;
    FNavigationPropertyPath : TPathExpression;
    FPath : TPathExpression;
    FPropertyPath : TPathExpression;
    F_Record : TRecordExpression;
    FUrlRef : TOneChildExpression;
    FMaxLength : TMaxLengthFacet;
    FPrecision : TPrecisionFacet;
    FScale : TScaleFacet;
    FSRID : TSridFacet;
  private
    function wstHas_Annotation() : Boolean;
    function wstHas__Type() : Boolean;
    function wstHas_Binary() : Boolean;
    function wstHas_Bool() : Boolean;
    function wstHas_Date() : Boolean;
    function wstHas_DateTimeOffset() : Boolean;
    function wstHas_Decimal() : Boolean;
    function wstHas_Duration() : Boolean;
    function wstHas_EnumMember() : Boolean;
    function wstHas_Float() : Boolean;
    function wstHas_Guid() : Boolean;
    function wstHas_Int() : Boolean;
    function wstHas__String() : Boolean;
    function wstHas_TimeOfDay() : Boolean;
    function wstHas_AnnotationPath() : Boolean;
    function wstHas_Apply() : Boolean;
    function wstHas_Cast() : Boolean;
    function wstHas_Collection() : Boolean;
    function wstHas__If() : Boolean;
    function wstHas_Eq() : Boolean;
    function wstHas_Ne() : Boolean;
    function wstHas_Ge() : Boolean;
    function wstHas_Gt() : Boolean;
    function wstHas_Le() : Boolean;
    function wstHas_Lt() : Boolean;
    function wstHas__And() : Boolean;
    function wstHas__Or() : Boolean;
    function wstHas__Not() : Boolean;
    function wstHas_IsOf() : Boolean;
    function wstHas_LabeledElement() : Boolean;
    function wstHas_LabeledElementReference() : Boolean;
    function wstHas_Null() : Boolean;
    function wstHas_NavigationPropertyPath() : Boolean;
    function wstHas_Path() : Boolean;
    function wstHas_PropertyPath() : Boolean;
    function wstHas__Record() : Boolean;
    function wstHas_UrlRef() : Boolean;
    function wstHas_MaxLength() : Boolean;
    function wstHas_Precision() : Boolean;
    function wstHas_Scale() : Boolean;
    function wstHas_SRID() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Annotation : TCastOrIsOfExpression_AnnotationArray read FAnnotation write FAnnotation stored wstHas_Annotation;
    property _Type : TTypeName read F_Type write F_Type stored wstHas__Type;
    property Binary : TBinaryConstantExpression read FBinary write FBinary stored wstHas_Binary;
    property Bool : TBoolConstantExpression read FBool write FBool stored wstHas_Bool;
    property Date : TDateConstantExpression read FDate write FDate stored wstHas_Date;
    property DateTimeOffset : TDateTimeOffsetConstantExpression read FDateTimeOffset write FDateTimeOffset stored wstHas_DateTimeOffset;
    property Decimal : TDecimalConstantExpression read FDecimal write FDecimal stored wstHas_Decimal;
    property Duration : TDurationConstantExpression read FDuration write FDuration stored wstHas_Duration;
    property EnumMember : TEnumMemberList read FEnumMember write FEnumMember stored wstHas_EnumMember;
    property Float : TFloatConstantExpression read FFloat write FFloat stored wstHas_Float;
    property Guid : TGuidConstantExpression read FGuid write FGuid stored wstHas_Guid;
    property Int : TIntConstantExpression read FInt write FInt stored wstHas_Int;
    property _String : TStringConstantExpression read F_String write F_String stored wstHas__String;
    property TimeOfDay : TTimeOfDayConstantExpression read FTimeOfDay write FTimeOfDay stored wstHas_TimeOfDay;
    property AnnotationPath : TPathExpression read FAnnotationPath write FAnnotationPath stored wstHas_AnnotationPath;
    property Apply : TApplyExpression read FApply write FApply stored wstHas_Apply;
    property Cast : TCastOrIsOfExpression read FCast write FCast stored wstHas_Cast;
    property Collection : TCollectionExpression read FCollection write FCollection stored wstHas_Collection;
    property _If : TIfExpression read F_If write F_If stored wstHas__If;
    property Eq : TTwoChildrenExpression read FEq write FEq stored wstHas_Eq;
    property Ne : TTwoChildrenExpression read FNe write FNe stored wstHas_Ne;
    property Ge : TTwoChildrenExpression read FGe write FGe stored wstHas_Ge;
    property Gt : TTwoChildrenExpression read FGt write FGt stored wstHas_Gt;
    property Le : TTwoChildrenExpression read FLe write FLe stored wstHas_Le;
    property Lt : TTwoChildrenExpression read FLt write FLt stored wstHas_Lt;
    property _And : TTwoChildrenExpression read F_And write F_And stored wstHas__And;
    property _Or : TTwoChildrenExpression read F_Or write F_Or stored wstHas__Or;
    property _Not : TOneChildExpression read F_Not write F_Not stored wstHas__Not;
    property IsOf : TCastOrIsOfExpression read FIsOf write FIsOf stored wstHas_IsOf;
    property LabeledElement : TLabeledElementExpression read FLabeledElement write FLabeledElement stored wstHas_LabeledElement;
    property LabeledElementReference : TLabeledElementReferenceExpression read FLabeledElementReference write FLabeledElementReference stored wstHas_LabeledElementReference;
    property Null : TNullExpression read FNull write FNull stored wstHas_Null;
    property NavigationPropertyPath : TPathExpression read FNavigationPropertyPath write FNavigationPropertyPath stored wstHas_NavigationPropertyPath;
    property Path : TPathExpression read FPath write FPath stored wstHas_Path;
    property PropertyPath : TPathExpression read FPropertyPath write FPropertyPath stored wstHas_PropertyPath;
    property _Record : TRecordExpression read F_Record write F_Record stored wstHas__Record;
    property UrlRef : TOneChildExpression read FUrlRef write FUrlRef stored wstHas_UrlRef;
    property MaxLength : TMaxLengthFacet read FMaxLength write FMaxLength stored wstHas_MaxLength;
    property Precision : TPrecisionFacet read FPrecision write FPrecision stored wstHas_Precision;
    property Scale : TScaleFacet read FScale write FScale stored wstHas_Scale;
    property SRID : TSridFacet read FSRID write FSRID stored wstHas_SRID;
  end;

  TCollectionExpression = class(TBaseComplexRemotable)
  private
    FBinary : TBinaryConstantExpression;
    FBool : TBoolConstantExpression;
    FDate : TDateConstantExpression;
    FDateTimeOffset : TDateTimeOffsetConstantExpression;
    FDecimal : TDecimalConstantExpression;
    FDuration : TDurationConstantExpression;
    FEnumMember : TEnumMemberList;
    FFloat : TFloatConstantExpression;
    FGuid : TGuidConstantExpression;
    FInt : TIntConstantExpression;
    F_String : TStringConstantExpression;
    FTimeOfDay : TTimeOfDayConstantExpression;
    FAnnotationPath : TPathExpression;
    FApply : TApplyExpression;
    FCast : TCastOrIsOfExpression;
    FCollection : TCollectionExpression;
    F_If : TIfExpression;
    FEq : TTwoChildrenExpression;
    FNe : TTwoChildrenExpression;
    FGe : TTwoChildrenExpression;
    FGt : TTwoChildrenExpression;
    FLe : TTwoChildrenExpression;
    FLt : TTwoChildrenExpression;
    F_And : TTwoChildrenExpression;
    F_Or : TTwoChildrenExpression;
    F_Not : TOneChildExpression;
    FIsOf : TCastOrIsOfExpression;
    FLabeledElement : TLabeledElementExpression;
    FLabeledElementReference : TLabeledElementReferenceExpression;
    FNull : TNullExpression;
    FNavigationPropertyPath : TPathExpression;
    FPath : TPathExpression;
    FPropertyPath : TPathExpression;
    F_Record : TRecordExpression;
    FUrlRef : TOneChildExpression;
  private
    function wstHas_Binary() : Boolean;
    function wstHas_Bool() : Boolean;
    function wstHas_Date() : Boolean;
    function wstHas_DateTimeOffset() : Boolean;
    function wstHas_Decimal() : Boolean;
    function wstHas_Duration() : Boolean;
    function wstHas_EnumMember() : Boolean;
    function wstHas_Float() : Boolean;
    function wstHas_Guid() : Boolean;
    function wstHas_Int() : Boolean;
    function wstHas__String() : Boolean;
    function wstHas_TimeOfDay() : Boolean;
    function wstHas_AnnotationPath() : Boolean;
    function wstHas_Apply() : Boolean;
    function wstHas_Cast() : Boolean;
    function wstHas_Collection() : Boolean;
    function wstHas__If() : Boolean;
    function wstHas_Eq() : Boolean;
    function wstHas_Ne() : Boolean;
    function wstHas_Ge() : Boolean;
    function wstHas_Gt() : Boolean;
    function wstHas_Le() : Boolean;
    function wstHas_Lt() : Boolean;
    function wstHas__And() : Boolean;
    function wstHas__Or() : Boolean;
    function wstHas__Not() : Boolean;
    function wstHas_IsOf() : Boolean;
    function wstHas_LabeledElement() : Boolean;
    function wstHas_LabeledElementReference() : Boolean;
    function wstHas_Null() : Boolean;
    function wstHas_NavigationPropertyPath() : Boolean;
    function wstHas_Path() : Boolean;
    function wstHas_PropertyPath() : Boolean;
    function wstHas__Record() : Boolean;
    function wstHas_UrlRef() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Binary : TBinaryConstantExpression read FBinary write FBinary stored wstHas_Binary;
    property Bool : TBoolConstantExpression read FBool write FBool stored wstHas_Bool;
    property Date : TDateConstantExpression read FDate write FDate stored wstHas_Date;
    property DateTimeOffset : TDateTimeOffsetConstantExpression read FDateTimeOffset write FDateTimeOffset stored wstHas_DateTimeOffset;
    property Decimal : TDecimalConstantExpression read FDecimal write FDecimal stored wstHas_Decimal;
    property Duration : TDurationConstantExpression read FDuration write FDuration stored wstHas_Duration;
    property EnumMember : TEnumMemberList read FEnumMember write FEnumMember stored wstHas_EnumMember;
    property Float : TFloatConstantExpression read FFloat write FFloat stored wstHas_Float;
    property Guid : TGuidConstantExpression read FGuid write FGuid stored wstHas_Guid;
    property Int : TIntConstantExpression read FInt write FInt stored wstHas_Int;
    property _String : TStringConstantExpression read F_String write F_String stored wstHas__String;
    property TimeOfDay : TTimeOfDayConstantExpression read FTimeOfDay write FTimeOfDay stored wstHas_TimeOfDay;
    property AnnotationPath : TPathExpression read FAnnotationPath write FAnnotationPath stored wstHas_AnnotationPath;
    property Apply : TApplyExpression read FApply write FApply stored wstHas_Apply;
    property Cast : TCastOrIsOfExpression read FCast write FCast stored wstHas_Cast;
    property Collection : TCollectionExpression read FCollection write FCollection stored wstHas_Collection;
    property _If : TIfExpression read F_If write F_If stored wstHas__If;
    property Eq : TTwoChildrenExpression read FEq write FEq stored wstHas_Eq;
    property Ne : TTwoChildrenExpression read FNe write FNe stored wstHas_Ne;
    property Ge : TTwoChildrenExpression read FGe write FGe stored wstHas_Ge;
    property Gt : TTwoChildrenExpression read FGt write FGt stored wstHas_Gt;
    property Le : TTwoChildrenExpression read FLe write FLe stored wstHas_Le;
    property Lt : TTwoChildrenExpression read FLt write FLt stored wstHas_Lt;
    property _And : TTwoChildrenExpression read F_And write F_And stored wstHas__And;
    property _Or : TTwoChildrenExpression read F_Or write F_Or stored wstHas__Or;
    property _Not : TOneChildExpression read F_Not write F_Not stored wstHas__Not;
    property IsOf : TCastOrIsOfExpression read FIsOf write FIsOf stored wstHas_IsOf;
    property LabeledElement : TLabeledElementExpression read FLabeledElement write FLabeledElement stored wstHas_LabeledElement;
    property LabeledElementReference : TLabeledElementReferenceExpression read FLabeledElementReference write FLabeledElementReference stored wstHas_LabeledElementReference;
    property Null : TNullExpression read FNull write FNull stored wstHas_Null;
    property NavigationPropertyPath : TPathExpression read FNavigationPropertyPath write FNavigationPropertyPath stored wstHas_NavigationPropertyPath;
    property Path : TPathExpression read FPath write FPath stored wstHas_Path;
    property PropertyPath : TPathExpression read FPropertyPath write FPropertyPath stored wstHas_PropertyPath;
    property _Record : TRecordExpression read F_Record write F_Record stored wstHas__Record;
    property UrlRef : TOneChildExpression read FUrlRef write FUrlRef stored wstHas_UrlRef;
  end;

  TIfExpression = class(TBaseComplexRemotable)
  private
    FAnnotation : TIfExpression_AnnotationArray;
    FBinary : TBinaryConstantExpression;
    FBool : TBoolConstantExpression;
    FDate : TDateConstantExpression;
    FDateTimeOffset : TDateTimeOffsetConstantExpression;
    FDecimal : TDecimalConstantExpression;
    FDuration : TDurationConstantExpression;
    FEnumMember : TEnumMemberList;
    FFloat : TFloatConstantExpression;
    FGuid : TGuidConstantExpression;
    FInt : TIntConstantExpression;
    F_String : TStringConstantExpression;
    FTimeOfDay : TTimeOfDayConstantExpression;
    FAnnotationPath : TPathExpression;
    FApply : TApplyExpression;
    FCast : TCastOrIsOfExpression;
    FCollection : TCollectionExpression;
    F_If : TIfExpression;
    FEq : TTwoChildrenExpression;
    FNe : TTwoChildrenExpression;
    FGe : TTwoChildrenExpression;
    FGt : TTwoChildrenExpression;
    FLe : TTwoChildrenExpression;
    FLt : TTwoChildrenExpression;
    F_And : TTwoChildrenExpression;
    F_Or : TTwoChildrenExpression;
    F_Not : TOneChildExpression;
    FIsOf : TCastOrIsOfExpression;
    FLabeledElement : TLabeledElementExpression;
    FLabeledElementReference : TLabeledElementReferenceExpression;
    FNull : TNullExpression;
    FNavigationPropertyPath : TPathExpression;
    FPath : TPathExpression;
    FPropertyPath : TPathExpression;
    F_Record : TRecordExpression;
    FUrlRef : TOneChildExpression;
  private
    function wstHas_Annotation() : Boolean;
    function wstHas_Binary() : Boolean;
    function wstHas_Bool() : Boolean;
    function wstHas_Date() : Boolean;
    function wstHas_DateTimeOffset() : Boolean;
    function wstHas_Decimal() : Boolean;
    function wstHas_Duration() : Boolean;
    function wstHas_EnumMember() : Boolean;
    function wstHas_Float() : Boolean;
    function wstHas_Guid() : Boolean;
    function wstHas_Int() : Boolean;
    function wstHas__String() : Boolean;
    function wstHas_TimeOfDay() : Boolean;
    function wstHas_AnnotationPath() : Boolean;
    function wstHas_Apply() : Boolean;
    function wstHas_Cast() : Boolean;
    function wstHas_Collection() : Boolean;
    function wstHas__If() : Boolean;
    function wstHas_Eq() : Boolean;
    function wstHas_Ne() : Boolean;
    function wstHas_Ge() : Boolean;
    function wstHas_Gt() : Boolean;
    function wstHas_Le() : Boolean;
    function wstHas_Lt() : Boolean;
    function wstHas__And() : Boolean;
    function wstHas__Or() : Boolean;
    function wstHas__Not() : Boolean;
    function wstHas_IsOf() : Boolean;
    function wstHas_LabeledElement() : Boolean;
    function wstHas_LabeledElementReference() : Boolean;
    function wstHas_Null() : Boolean;
    function wstHas_NavigationPropertyPath() : Boolean;
    function wstHas_Path() : Boolean;
    function wstHas_PropertyPath() : Boolean;
    function wstHas__Record() : Boolean;
    function wstHas_UrlRef() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Annotation : TIfExpression_AnnotationArray read FAnnotation write FAnnotation stored wstHas_Annotation;
    property Binary : TBinaryConstantExpression read FBinary write FBinary stored wstHas_Binary;
    property Bool : TBoolConstantExpression read FBool write FBool stored wstHas_Bool;
    property Date : TDateConstantExpression read FDate write FDate stored wstHas_Date;
    property DateTimeOffset : TDateTimeOffsetConstantExpression read FDateTimeOffset write FDateTimeOffset stored wstHas_DateTimeOffset;
    property Decimal : TDecimalConstantExpression read FDecimal write FDecimal stored wstHas_Decimal;
    property Duration : TDurationConstantExpression read FDuration write FDuration stored wstHas_Duration;
    property EnumMember : TEnumMemberList read FEnumMember write FEnumMember stored wstHas_EnumMember;
    property Float : TFloatConstantExpression read FFloat write FFloat stored wstHas_Float;
    property Guid : TGuidConstantExpression read FGuid write FGuid stored wstHas_Guid;
    property Int : TIntConstantExpression read FInt write FInt stored wstHas_Int;
    property _String : TStringConstantExpression read F_String write F_String stored wstHas__String;
    property TimeOfDay : TTimeOfDayConstantExpression read FTimeOfDay write FTimeOfDay stored wstHas_TimeOfDay;
    property AnnotationPath : TPathExpression read FAnnotationPath write FAnnotationPath stored wstHas_AnnotationPath;
    property Apply : TApplyExpression read FApply write FApply stored wstHas_Apply;
    property Cast : TCastOrIsOfExpression read FCast write FCast stored wstHas_Cast;
    property Collection : TCollectionExpression read FCollection write FCollection stored wstHas_Collection;
    property _If : TIfExpression read F_If write F_If stored wstHas__If;
    property Eq : TTwoChildrenExpression read FEq write FEq stored wstHas_Eq;
    property Ne : TTwoChildrenExpression read FNe write FNe stored wstHas_Ne;
    property Ge : TTwoChildrenExpression read FGe write FGe stored wstHas_Ge;
    property Gt : TTwoChildrenExpression read FGt write FGt stored wstHas_Gt;
    property Le : TTwoChildrenExpression read FLe write FLe stored wstHas_Le;
    property Lt : TTwoChildrenExpression read FLt write FLt stored wstHas_Lt;
    property _And : TTwoChildrenExpression read F_And write F_And stored wstHas__And;
    property _Or : TTwoChildrenExpression read F_Or write F_Or stored wstHas__Or;
    property _Not : TOneChildExpression read F_Not write F_Not stored wstHas__Not;
    property IsOf : TCastOrIsOfExpression read FIsOf write FIsOf stored wstHas_IsOf;
    property LabeledElement : TLabeledElementExpression read FLabeledElement write FLabeledElement stored wstHas_LabeledElement;
    property LabeledElementReference : TLabeledElementReferenceExpression read FLabeledElementReference write FLabeledElementReference stored wstHas_LabeledElementReference;
    property Null : TNullExpression read FNull write FNull stored wstHas_Null;
    property NavigationPropertyPath : TPathExpression read FNavigationPropertyPath write FNavigationPropertyPath stored wstHas_NavigationPropertyPath;
    property Path : TPathExpression read FPath write FPath stored wstHas_Path;
    property PropertyPath : TPathExpression read FPropertyPath write FPropertyPath stored wstHas_PropertyPath;
    property _Record : TRecordExpression read F_Record write F_Record stored wstHas__Record;
    property UrlRef : TOneChildExpression read FUrlRef write FUrlRef stored wstHas_UrlRef;
  end;

  TOneChildExpression = class(TBaseComplexRemotable)
  private
    FAnnotation : TOneChildExpression_AnnotationArray;
    FBinary : TBinaryConstantExpression;
    FBool : TBoolConstantExpression;
    FDate : TDateConstantExpression;
    FDateTimeOffset : TDateTimeOffsetConstantExpression;
    FDecimal : TDecimalConstantExpression;
    FDuration : TDurationConstantExpression;
    FEnumMember : TEnumMemberList;
    FFloat : TFloatConstantExpression;
    FGuid : TGuidConstantExpression;
    FInt : TIntConstantExpression;
    F_String : TStringConstantExpression;
    FTimeOfDay : TTimeOfDayConstantExpression;
    FAnnotationPath : TPathExpression;
    FApply : TApplyExpression;
    FCast : TCastOrIsOfExpression;
    FCollection : TCollectionExpression;
    F_If : TIfExpression;
    FEq : TTwoChildrenExpression;
    FNe : TTwoChildrenExpression;
    FGe : TTwoChildrenExpression;
    FGt : TTwoChildrenExpression;
    FLe : TTwoChildrenExpression;
    FLt : TTwoChildrenExpression;
    F_And : TTwoChildrenExpression;
    F_Or : TTwoChildrenExpression;
    F_Not : TOneChildExpression;
    FIsOf : TCastOrIsOfExpression;
    FLabeledElement : TLabeledElementExpression;
    FLabeledElementReference : TLabeledElementReferenceExpression;
    FNull : TNullExpression;
    FNavigationPropertyPath : TPathExpression;
    FPath : TPathExpression;
    FPropertyPath : TPathExpression;
    F_Record : TRecordExpression;
    FUrlRef : TOneChildExpression;
  private
    function wstHas_Annotation() : Boolean;
    function wstHas_Binary() : Boolean;
    function wstHas_Bool() : Boolean;
    function wstHas_Date() : Boolean;
    function wstHas_DateTimeOffset() : Boolean;
    function wstHas_Decimal() : Boolean;
    function wstHas_Duration() : Boolean;
    function wstHas_EnumMember() : Boolean;
    function wstHas_Float() : Boolean;
    function wstHas_Guid() : Boolean;
    function wstHas_Int() : Boolean;
    function wstHas__String() : Boolean;
    function wstHas_TimeOfDay() : Boolean;
    function wstHas_AnnotationPath() : Boolean;
    function wstHas_Apply() : Boolean;
    function wstHas_Cast() : Boolean;
    function wstHas_Collection() : Boolean;
    function wstHas__If() : Boolean;
    function wstHas_Eq() : Boolean;
    function wstHas_Ne() : Boolean;
    function wstHas_Ge() : Boolean;
    function wstHas_Gt() : Boolean;
    function wstHas_Le() : Boolean;
    function wstHas_Lt() : Boolean;
    function wstHas__And() : Boolean;
    function wstHas__Or() : Boolean;
    function wstHas__Not() : Boolean;
    function wstHas_IsOf() : Boolean;
    function wstHas_LabeledElement() : Boolean;
    function wstHas_LabeledElementReference() : Boolean;
    function wstHas_Null() : Boolean;
    function wstHas_NavigationPropertyPath() : Boolean;
    function wstHas_Path() : Boolean;
    function wstHas_PropertyPath() : Boolean;
    function wstHas__Record() : Boolean;
    function wstHas_UrlRef() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Annotation : TOneChildExpression_AnnotationArray read FAnnotation write FAnnotation stored wstHas_Annotation;
    property Binary : TBinaryConstantExpression read FBinary write FBinary stored wstHas_Binary;
    property Bool : TBoolConstantExpression read FBool write FBool stored wstHas_Bool;
    property Date : TDateConstantExpression read FDate write FDate stored wstHas_Date;
    property DateTimeOffset : TDateTimeOffsetConstantExpression read FDateTimeOffset write FDateTimeOffset stored wstHas_DateTimeOffset;
    property Decimal : TDecimalConstantExpression read FDecimal write FDecimal stored wstHas_Decimal;
    property Duration : TDurationConstantExpression read FDuration write FDuration stored wstHas_Duration;
    property EnumMember : TEnumMemberList read FEnumMember write FEnumMember stored wstHas_EnumMember;
    property Float : TFloatConstantExpression read FFloat write FFloat stored wstHas_Float;
    property Guid : TGuidConstantExpression read FGuid write FGuid stored wstHas_Guid;
    property Int : TIntConstantExpression read FInt write FInt stored wstHas_Int;
    property _String : TStringConstantExpression read F_String write F_String stored wstHas__String;
    property TimeOfDay : TTimeOfDayConstantExpression read FTimeOfDay write FTimeOfDay stored wstHas_TimeOfDay;
    property AnnotationPath : TPathExpression read FAnnotationPath write FAnnotationPath stored wstHas_AnnotationPath;
    property Apply : TApplyExpression read FApply write FApply stored wstHas_Apply;
    property Cast : TCastOrIsOfExpression read FCast write FCast stored wstHas_Cast;
    property Collection : TCollectionExpression read FCollection write FCollection stored wstHas_Collection;
    property _If : TIfExpression read F_If write F_If stored wstHas__If;
    property Eq : TTwoChildrenExpression read FEq write FEq stored wstHas_Eq;
    property Ne : TTwoChildrenExpression read FNe write FNe stored wstHas_Ne;
    property Ge : TTwoChildrenExpression read FGe write FGe stored wstHas_Ge;
    property Gt : TTwoChildrenExpression read FGt write FGt stored wstHas_Gt;
    property Le : TTwoChildrenExpression read FLe write FLe stored wstHas_Le;
    property Lt : TTwoChildrenExpression read FLt write FLt stored wstHas_Lt;
    property _And : TTwoChildrenExpression read F_And write F_And stored wstHas__And;
    property _Or : TTwoChildrenExpression read F_Or write F_Or stored wstHas__Or;
    property _Not : TOneChildExpression read F_Not write F_Not stored wstHas__Not;
    property IsOf : TCastOrIsOfExpression read FIsOf write FIsOf stored wstHas_IsOf;
    property LabeledElement : TLabeledElementExpression read FLabeledElement write FLabeledElement stored wstHas_LabeledElement;
    property LabeledElementReference : TLabeledElementReferenceExpression read FLabeledElementReference write FLabeledElementReference stored wstHas_LabeledElementReference;
    property Null : TNullExpression read FNull write FNull stored wstHas_Null;
    property NavigationPropertyPath : TPathExpression read FNavigationPropertyPath write FNavigationPropertyPath stored wstHas_NavigationPropertyPath;
    property Path : TPathExpression read FPath write FPath stored wstHas_Path;
    property PropertyPath : TPathExpression read FPropertyPath write FPropertyPath stored wstHas_PropertyPath;
    property _Record : TRecordExpression read F_Record write F_Record stored wstHas__Record;
    property UrlRef : TOneChildExpression read FUrlRef write FUrlRef stored wstHas_UrlRef;
  end;

  TTwoChildrenExpression = class(TBaseComplexRemotable)
  private
    FAnnotation : TTwoChildrenExpression_AnnotationArray;
    FBinary : TBinaryConstantExpression;
    FBool : TBoolConstantExpression;
    FDate : TDateConstantExpression;
    FDateTimeOffset : TDateTimeOffsetConstantExpression;
    FDecimal : TDecimalConstantExpression;
    FDuration : TDurationConstantExpression;
    FEnumMember : TEnumMemberList;
    FFloat : TFloatConstantExpression;
    FGuid : TGuidConstantExpression;
    FInt : TIntConstantExpression;
    F_String : TStringConstantExpression;
    FTimeOfDay : TTimeOfDayConstantExpression;
    FAnnotationPath : TPathExpression;
    FApply : TApplyExpression;
    FCast : TCastOrIsOfExpression;
    FCollection : TCollectionExpression;
    F_If : TIfExpression;
    FEq : TTwoChildrenExpression;
    FNe : TTwoChildrenExpression;
    FGe : TTwoChildrenExpression;
    FGt : TTwoChildrenExpression;
    FLe : TTwoChildrenExpression;
    FLt : TTwoChildrenExpression;
    F_And : TTwoChildrenExpression;
    F_Or : TTwoChildrenExpression;
    F_Not : TOneChildExpression;
    FIsOf : TCastOrIsOfExpression;
    FLabeledElement : TLabeledElementExpression;
    FLabeledElementReference : TLabeledElementReferenceExpression;
    FNull : TNullExpression;
    FNavigationPropertyPath : TPathExpression;
    FPath : TPathExpression;
    FPropertyPath : TPathExpression;
    F_Record : TRecordExpression;
    FUrlRef : TOneChildExpression;
  private
    function wstHas_Annotation() : Boolean;
    function wstHas_Binary() : Boolean;
    function wstHas_Bool() : Boolean;
    function wstHas_Date() : Boolean;
    function wstHas_DateTimeOffset() : Boolean;
    function wstHas_Decimal() : Boolean;
    function wstHas_Duration() : Boolean;
    function wstHas_EnumMember() : Boolean;
    function wstHas_Float() : Boolean;
    function wstHas_Guid() : Boolean;
    function wstHas_Int() : Boolean;
    function wstHas__String() : Boolean;
    function wstHas_TimeOfDay() : Boolean;
    function wstHas_AnnotationPath() : Boolean;
    function wstHas_Apply() : Boolean;
    function wstHas_Cast() : Boolean;
    function wstHas_Collection() : Boolean;
    function wstHas__If() : Boolean;
    function wstHas_Eq() : Boolean;
    function wstHas_Ne() : Boolean;
    function wstHas_Ge() : Boolean;
    function wstHas_Gt() : Boolean;
    function wstHas_Le() : Boolean;
    function wstHas_Lt() : Boolean;
    function wstHas__And() : Boolean;
    function wstHas__Or() : Boolean;
    function wstHas__Not() : Boolean;
    function wstHas_IsOf() : Boolean;
    function wstHas_LabeledElement() : Boolean;
    function wstHas_LabeledElementReference() : Boolean;
    function wstHas_Null() : Boolean;
    function wstHas_NavigationPropertyPath() : Boolean;
    function wstHas_Path() : Boolean;
    function wstHas_PropertyPath() : Boolean;
    function wstHas__Record() : Boolean;
    function wstHas_UrlRef() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Annotation : TTwoChildrenExpression_AnnotationArray read FAnnotation write FAnnotation stored wstHas_Annotation;
    property Binary : TBinaryConstantExpression read FBinary write FBinary stored wstHas_Binary;
    property Bool : TBoolConstantExpression read FBool write FBool stored wstHas_Bool;
    property Date : TDateConstantExpression read FDate write FDate stored wstHas_Date;
    property DateTimeOffset : TDateTimeOffsetConstantExpression read FDateTimeOffset write FDateTimeOffset stored wstHas_DateTimeOffset;
    property Decimal : TDecimalConstantExpression read FDecimal write FDecimal stored wstHas_Decimal;
    property Duration : TDurationConstantExpression read FDuration write FDuration stored wstHas_Duration;
    property EnumMember : TEnumMemberList read FEnumMember write FEnumMember stored wstHas_EnumMember;
    property Float : TFloatConstantExpression read FFloat write FFloat stored wstHas_Float;
    property Guid : TGuidConstantExpression read FGuid write FGuid stored wstHas_Guid;
    property Int : TIntConstantExpression read FInt write FInt stored wstHas_Int;
    property _String : TStringConstantExpression read F_String write F_String stored wstHas__String;
    property TimeOfDay : TTimeOfDayConstantExpression read FTimeOfDay write FTimeOfDay stored wstHas_TimeOfDay;
    property AnnotationPath : TPathExpression read FAnnotationPath write FAnnotationPath stored wstHas_AnnotationPath;
    property Apply : TApplyExpression read FApply write FApply stored wstHas_Apply;
    property Cast : TCastOrIsOfExpression read FCast write FCast stored wstHas_Cast;
    property Collection : TCollectionExpression read FCollection write FCollection stored wstHas_Collection;
    property _If : TIfExpression read F_If write F_If stored wstHas__If;
    property Eq : TTwoChildrenExpression read FEq write FEq stored wstHas_Eq;
    property Ne : TTwoChildrenExpression read FNe write FNe stored wstHas_Ne;
    property Ge : TTwoChildrenExpression read FGe write FGe stored wstHas_Ge;
    property Gt : TTwoChildrenExpression read FGt write FGt stored wstHas_Gt;
    property Le : TTwoChildrenExpression read FLe write FLe stored wstHas_Le;
    property Lt : TTwoChildrenExpression read FLt write FLt stored wstHas_Lt;
    property _And : TTwoChildrenExpression read F_And write F_And stored wstHas__And;
    property _Or : TTwoChildrenExpression read F_Or write F_Or stored wstHas__Or;
    property _Not : TOneChildExpression read F_Not write F_Not stored wstHas__Not;
    property IsOf : TCastOrIsOfExpression read FIsOf write FIsOf stored wstHas_IsOf;
    property LabeledElement : TLabeledElementExpression read FLabeledElement write FLabeledElement stored wstHas_LabeledElement;
    property LabeledElementReference : TLabeledElementReferenceExpression read FLabeledElementReference write FLabeledElementReference stored wstHas_LabeledElementReference;
    property Null : TNullExpression read FNull write FNull stored wstHas_Null;
    property NavigationPropertyPath : TPathExpression read FNavigationPropertyPath write FNavigationPropertyPath stored wstHas_NavigationPropertyPath;
    property Path : TPathExpression read FPath write FPath stored wstHas_Path;
    property PropertyPath : TPathExpression read FPropertyPath write FPropertyPath stored wstHas_PropertyPath;
    property _Record : TRecordExpression read F_Record write F_Record stored wstHas__Record;
    property UrlRef : TOneChildExpression read FUrlRef write FUrlRef stored wstHas_UrlRef;
  end;

  TLabeledElementExpression = class(TBaseComplexRemotable)
  private
    FAnnotation : TLabeledElementExpression_AnnotationArray;
    FName : TSimpleIdentifier;
    FBinary : TBinaryConstantExpression;
    FBool : TBoolConstantExpression;
    FDate : TDateConstantExpression;
    FDateTimeOffset : TDateTimeOffsetConstantExpression;
    FDecimal : TDecimalConstantExpression;
    FDuration : TDurationConstantExpression;
    FEnumMember : TEnumMemberList;
    FFloat : TFloatConstantExpression;
    FGuid : TGuidConstantExpression;
    FInt : TIntConstantExpression;
    F_String : TStringConstantExpression;
    FTimeOfDay : TTimeOfDayConstantExpression;
    FAnnotationPath : TPathExpression;
    FApply : TApplyExpression;
    FCast : TCastOrIsOfExpression;
    FCollection : TCollectionExpression;
    F_If : TIfExpression;
    FEq : TTwoChildrenExpression;
    FNe : TTwoChildrenExpression;
    FGe : TTwoChildrenExpression;
    FGt : TTwoChildrenExpression;
    FLe : TTwoChildrenExpression;
    FLt : TTwoChildrenExpression;
    F_And : TTwoChildrenExpression;
    F_Or : TTwoChildrenExpression;
    F_Not : TOneChildExpression;
    FIsOf : TCastOrIsOfExpression;
    FLabeledElement : TLabeledElementExpression;
    FLabeledElementReference : TLabeledElementReferenceExpression;
    FNull : TNullExpression;
    FNavigationPropertyPath : TPathExpression;
    FPath : TPathExpression;
    FPropertyPath : TPathExpression;
    F_Record : TRecordExpression;
    FUrlRef : TOneChildExpression;
    FBinaryAtt : binary;
    FBoolAtt : boolean;
    FDateAtt : TDateRemotable;
    FDateTimeOffsetAtt : dateTimeStamp;
    FDecimalAtt : Currency;
    FDurationAtt : dayTimeDuration;
    FEnumMemberAtt : TEnumMemberList;
    FFloatAtt : Double;
    FGuidAtt : TGuidLiteral;
    FIntAtt : integer;
    F_StringAtt : UnicodeString;
    FTimeOfDayAtt : TTimeRemotable;
    FAnnotationPathAtt : TPathWithTermSegments;
    FNavigationPropertyPathAtt : TPathWithTermSegments;
    FPathAtt : TPathWithTermSegments;
    FPropertyPathAtt : TPathWithTermSegments;
    FUrlRefAtt : anyURI;
  private
    function wstHas_Annotation() : Boolean;
    function wstHas_Binary() : Boolean;
    function wstHas_Bool() : Boolean;
    function wstHas_Date() : Boolean;
    function wstHas_DateTimeOffset() : Boolean;
    function wstHas_Decimal() : Boolean;
    function wstHas_Duration() : Boolean;
    function wstHas_EnumMember() : Boolean;
    function wstHas_Float() : Boolean;
    function wstHas_Guid() : Boolean;
    function wstHas_Int() : Boolean;
    function wstHas__String() : Boolean;
    function wstHas_TimeOfDay() : Boolean;
    function wstHas_AnnotationPath() : Boolean;
    function wstHas_Apply() : Boolean;
    function wstHas_Cast() : Boolean;
    function wstHas_Collection() : Boolean;
    function wstHas__If() : Boolean;
    function wstHas_Eq() : Boolean;
    function wstHas_Ne() : Boolean;
    function wstHas_Ge() : Boolean;
    function wstHas_Gt() : Boolean;
    function wstHas_Le() : Boolean;
    function wstHas_Lt() : Boolean;
    function wstHas__And() : Boolean;
    function wstHas__Or() : Boolean;
    function wstHas__Not() : Boolean;
    function wstHas_IsOf() : Boolean;
    function wstHas_LabeledElement() : Boolean;
    function wstHas_LabeledElementReference() : Boolean;
    function wstHas_Null() : Boolean;
    function wstHas_NavigationPropertyPath() : Boolean;
    function wstHas_Path() : Boolean;
    function wstHas_PropertyPath() : Boolean;
    function wstHas__Record() : Boolean;
    function wstHas_UrlRef() : Boolean;
    function wstHas_BinaryAtt() : Boolean;
    function wstHas_BoolAtt() : Boolean;
    function wstHas_DateAtt() : Boolean;
    function wstHas_DateTimeOffsetAtt() : Boolean;
    function wstHas_DecimalAtt() : Boolean;
    function wstHas_DurationAtt() : Boolean;
    function wstHas_EnumMemberAtt() : Boolean;
    function wstHas_FloatAtt() : Boolean;
    function wstHas_GuidAtt() : Boolean;
    function wstHas_IntAtt() : Boolean;
    function wstHas__StringAtt() : Boolean;
    function wstHas_TimeOfDayAtt() : Boolean;
    function wstHas_AnnotationPathAtt() : Boolean;
    function wstHas_NavigationPropertyPathAtt() : Boolean;
    function wstHas_PathAtt() : Boolean;
    function wstHas_PropertyPathAtt() : Boolean;
    function wstHas_UrlRefAtt() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Annotation : TLabeledElementExpression_AnnotationArray read FAnnotation write FAnnotation stored wstHas_Annotation;
    property Name : TSimpleIdentifier read FName write FName;
    property Binary : TBinaryConstantExpression read FBinary write FBinary stored wstHas_Binary;
    property Bool : TBoolConstantExpression read FBool write FBool stored wstHas_Bool;
    property Date : TDateConstantExpression read FDate write FDate stored wstHas_Date;
    property DateTimeOffset : TDateTimeOffsetConstantExpression read FDateTimeOffset write FDateTimeOffset stored wstHas_DateTimeOffset;
    property Decimal : TDecimalConstantExpression read FDecimal write FDecimal stored wstHas_Decimal;
    property Duration : TDurationConstantExpression read FDuration write FDuration stored wstHas_Duration;
    property EnumMember : TEnumMemberList read FEnumMember write FEnumMember stored wstHas_EnumMember;
    property Float : TFloatConstantExpression read FFloat write FFloat stored wstHas_Float;
    property Guid : TGuidConstantExpression read FGuid write FGuid stored wstHas_Guid;
    property Int : TIntConstantExpression read FInt write FInt stored wstHas_Int;
    property _String : TStringConstantExpression read F_String write F_String stored wstHas__String;
    property TimeOfDay : TTimeOfDayConstantExpression read FTimeOfDay write FTimeOfDay stored wstHas_TimeOfDay;
    property AnnotationPath : TPathExpression read FAnnotationPath write FAnnotationPath stored wstHas_AnnotationPath;
    property Apply : TApplyExpression read FApply write FApply stored wstHas_Apply;
    property Cast : TCastOrIsOfExpression read FCast write FCast stored wstHas_Cast;
    property Collection : TCollectionExpression read FCollection write FCollection stored wstHas_Collection;
    property _If : TIfExpression read F_If write F_If stored wstHas__If;
    property Eq : TTwoChildrenExpression read FEq write FEq stored wstHas_Eq;
    property Ne : TTwoChildrenExpression read FNe write FNe stored wstHas_Ne;
    property Ge : TTwoChildrenExpression read FGe write FGe stored wstHas_Ge;
    property Gt : TTwoChildrenExpression read FGt write FGt stored wstHas_Gt;
    property Le : TTwoChildrenExpression read FLe write FLe stored wstHas_Le;
    property Lt : TTwoChildrenExpression read FLt write FLt stored wstHas_Lt;
    property _And : TTwoChildrenExpression read F_And write F_And stored wstHas__And;
    property _Or : TTwoChildrenExpression read F_Or write F_Or stored wstHas__Or;
    property _Not : TOneChildExpression read F_Not write F_Not stored wstHas__Not;
    property IsOf : TCastOrIsOfExpression read FIsOf write FIsOf stored wstHas_IsOf;
    property LabeledElement : TLabeledElementExpression read FLabeledElement write FLabeledElement stored wstHas_LabeledElement;
    property LabeledElementReference : TLabeledElementReferenceExpression read FLabeledElementReference write FLabeledElementReference stored wstHas_LabeledElementReference;
    property Null : TNullExpression read FNull write FNull stored wstHas_Null;
    property NavigationPropertyPath : TPathExpression read FNavigationPropertyPath write FNavigationPropertyPath stored wstHas_NavigationPropertyPath;
    property Path : TPathExpression read FPath write FPath stored wstHas_Path;
    property PropertyPath : TPathExpression read FPropertyPath write FPropertyPath stored wstHas_PropertyPath;
    property _Record : TRecordExpression read F_Record write F_Record stored wstHas__Record;
    property UrlRef : TOneChildExpression read FUrlRef write FUrlRef stored wstHas_UrlRef;
    property BinaryAtt : binary read FBinaryAtt write FBinaryAtt stored wstHas_BinaryAtt;
    property BoolAtt : boolean read FBoolAtt write FBoolAtt stored wstHas_BoolAtt;
    property DateAtt : TDateRemotable read FDateAtt write FDateAtt stored wstHas_DateAtt;
    property DateTimeOffsetAtt : dateTimeStamp read FDateTimeOffsetAtt write FDateTimeOffsetAtt stored wstHas_DateTimeOffsetAtt;
    property DecimalAtt : Currency read FDecimalAtt write FDecimalAtt stored wstHas_DecimalAtt;
    property DurationAtt : dayTimeDuration read FDurationAtt write FDurationAtt stored wstHas_DurationAtt;
    property EnumMemberAtt : TEnumMemberList read FEnumMemberAtt write FEnumMemberAtt stored wstHas_EnumMemberAtt;
    property FloatAtt : Double read FFloatAtt write FFloatAtt stored wstHas_FloatAtt;
    property GuidAtt : TGuidLiteral read FGuidAtt write FGuidAtt stored wstHas_GuidAtt;
    property IntAtt : integer read FIntAtt write FIntAtt stored wstHas_IntAtt;
    property _StringAtt : UnicodeString read F_StringAtt write F_StringAtt stored wstHas__StringAtt;
    property TimeOfDayAtt : TTimeRemotable read FTimeOfDayAtt write FTimeOfDayAtt stored wstHas_TimeOfDayAtt;
    property AnnotationPathAtt : TPathWithTermSegments read FAnnotationPathAtt write FAnnotationPathAtt stored wstHas_AnnotationPathAtt;
    property NavigationPropertyPathAtt : TPathWithTermSegments read FNavigationPropertyPathAtt write FNavigationPropertyPathAtt stored wstHas_NavigationPropertyPathAtt;
    property PathAtt : TPathWithTermSegments read FPathAtt write FPathAtt stored wstHas_PathAtt;
    property PropertyPathAtt : TPathWithTermSegments read FPropertyPathAtt write FPropertyPathAtt stored wstHas_PropertyPathAtt;
    property UrlRefAtt : anyURI read FUrlRefAtt write FUrlRefAtt stored wstHas_UrlRefAtt;
  end;

  TLabeledElementReferenceExpression = class(TComplexUnicodeStringContentRemotable)
  end;

  TPathExpression = class(TComplexUnicodeStringContentRemotable)
  end;

  TRecordExpression = class(TBaseComplexRemotable)
  private
    FPropertyValue : TRecordExpression_PropertyValueArray;
    FAnnotation : TRecordExpression_AnnotationArray;
    F_Type : TQualifiedName;
  private
    function wstHas_PropertyValue() : Boolean;
    function wstHas_Annotation() : Boolean;
    function wstHas__Type() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property PropertyValue : TRecordExpression_PropertyValueArray read FPropertyValue write FPropertyValue stored wstHas_PropertyValue;
    property Annotation : TRecordExpression_AnnotationArray read FAnnotation write FAnnotation stored wstHas_Annotation;
    property _Type : TQualifiedName read F_Type write F_Type stored wstHas__Type;
  end;

  TPropertyValue = class(TBaseComplexRemotable)
  private
    FAnnotation : TPropertyValue_AnnotationArray;
    F_Property : TSimpleIdentifier;
    FBinary : TBinaryConstantExpression;
    FBool : TBoolConstantExpression;
    FDate : TDateConstantExpression;
    FDateTimeOffset : TDateTimeOffsetConstantExpression;
    FDecimal : TDecimalConstantExpression;
    FDuration : TDurationConstantExpression;
    FEnumMember : TEnumMemberList;
    FFloat : TFloatConstantExpression;
    FGuid : TGuidConstantExpression;
    FInt : TIntConstantExpression;
    F_String : TStringConstantExpression;
    FTimeOfDay : TTimeOfDayConstantExpression;
    FAnnotationPath : TPathExpression;
    FApply : TApplyExpression;
    FCast : TCastOrIsOfExpression;
    FCollection : TCollectionExpression;
    F_If : TIfExpression;
    FEq : TTwoChildrenExpression;
    FNe : TTwoChildrenExpression;
    FGe : TTwoChildrenExpression;
    FGt : TTwoChildrenExpression;
    FLe : TTwoChildrenExpression;
    FLt : TTwoChildrenExpression;
    F_And : TTwoChildrenExpression;
    F_Or : TTwoChildrenExpression;
    F_Not : TOneChildExpression;
    FIsOf : TCastOrIsOfExpression;
    FLabeledElement : TLabeledElementExpression;
    FLabeledElementReference : TLabeledElementReferenceExpression;
    FNull : TNullExpression;
    FNavigationPropertyPath : TPathExpression;
    FPath : TPathExpression;
    FPropertyPath : TPathExpression;
    F_Record : TRecordExpression;
    FUrlRef : TOneChildExpression;
    FBinaryAtt : binary;
    FBoolAtt : boolean;
    FDateAtt : TDateRemotable;
    FDateTimeOffsetAtt : dateTimeStamp;
    FDecimalAtt : Currency;
    FDurationAtt : dayTimeDuration;
    FEnumMemberAtt : TEnumMemberList;
    FFloatAtt : Double;
    FGuidAtt : TGuidLiteral;
    FIntAtt : integer;
    F_StringAtt : UnicodeString;
    FTimeOfDayAtt : TTimeRemotable;
    FAnnotationPathAtt : TPathWithTermSegments;
    FNavigationPropertyPathAtt : TPathWithTermSegments;
    FPathAtt : TPathWithTermSegments;
    FPropertyPathAtt : TPathWithTermSegments;
    FUrlRefAtt : anyURI;
  private
    function wstHas_Annotation() : Boolean;
    function wstHas_Binary() : Boolean;
    function wstHas_Bool() : Boolean;
    function wstHas_Date() : Boolean;
    function wstHas_DateTimeOffset() : Boolean;
    function wstHas_Decimal() : Boolean;
    function wstHas_Duration() : Boolean;
    function wstHas_EnumMember() : Boolean;
    function wstHas_Float() : Boolean;
    function wstHas_Guid() : Boolean;
    function wstHas_Int() : Boolean;
    function wstHas__String() : Boolean;
    function wstHas_TimeOfDay() : Boolean;
    function wstHas_AnnotationPath() : Boolean;
    function wstHas_Apply() : Boolean;
    function wstHas_Cast() : Boolean;
    function wstHas_Collection() : Boolean;
    function wstHas__If() : Boolean;
    function wstHas_Eq() : Boolean;
    function wstHas_Ne() : Boolean;
    function wstHas_Ge() : Boolean;
    function wstHas_Gt() : Boolean;
    function wstHas_Le() : Boolean;
    function wstHas_Lt() : Boolean;
    function wstHas__And() : Boolean;
    function wstHas__Or() : Boolean;
    function wstHas__Not() : Boolean;
    function wstHas_IsOf() : Boolean;
    function wstHas_LabeledElement() : Boolean;
    function wstHas_LabeledElementReference() : Boolean;
    function wstHas_Null() : Boolean;
    function wstHas_NavigationPropertyPath() : Boolean;
    function wstHas_Path() : Boolean;
    function wstHas_PropertyPath() : Boolean;
    function wstHas__Record() : Boolean;
    function wstHas_UrlRef() : Boolean;
    function wstHas_BinaryAtt() : Boolean;
    function wstHas_BoolAtt() : Boolean;
    function wstHas_DateAtt() : Boolean;
    function wstHas_DateTimeOffsetAtt() : Boolean;
    function wstHas_DecimalAtt() : Boolean;
    function wstHas_DurationAtt() : Boolean;
    function wstHas_EnumMemberAtt() : Boolean;
    function wstHas_FloatAtt() : Boolean;
    function wstHas_GuidAtt() : Boolean;
    function wstHas_IntAtt() : Boolean;
    function wstHas__StringAtt() : Boolean;
    function wstHas_TimeOfDayAtt() : Boolean;
    function wstHas_AnnotationPathAtt() : Boolean;
    function wstHas_NavigationPropertyPathAtt() : Boolean;
    function wstHas_PathAtt() : Boolean;
    function wstHas_PropertyPathAtt() : Boolean;
    function wstHas_UrlRefAtt() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Annotation : TPropertyValue_AnnotationArray read FAnnotation write FAnnotation stored wstHas_Annotation;
    property _Property : TSimpleIdentifier read F_Property write F_Property;
    property Binary : TBinaryConstantExpression read FBinary write FBinary stored wstHas_Binary;
    property Bool : TBoolConstantExpression read FBool write FBool stored wstHas_Bool;
    property Date : TDateConstantExpression read FDate write FDate stored wstHas_Date;
    property DateTimeOffset : TDateTimeOffsetConstantExpression read FDateTimeOffset write FDateTimeOffset stored wstHas_DateTimeOffset;
    property Decimal : TDecimalConstantExpression read FDecimal write FDecimal stored wstHas_Decimal;
    property Duration : TDurationConstantExpression read FDuration write FDuration stored wstHas_Duration;
    property EnumMember : TEnumMemberList read FEnumMember write FEnumMember stored wstHas_EnumMember;
    property Float : TFloatConstantExpression read FFloat write FFloat stored wstHas_Float;
    property Guid : TGuidConstantExpression read FGuid write FGuid stored wstHas_Guid;
    property Int : TIntConstantExpression read FInt write FInt stored wstHas_Int;
    property _String : TStringConstantExpression read F_String write F_String stored wstHas__String;
    property TimeOfDay : TTimeOfDayConstantExpression read FTimeOfDay write FTimeOfDay stored wstHas_TimeOfDay;
    property AnnotationPath : TPathExpression read FAnnotationPath write FAnnotationPath stored wstHas_AnnotationPath;
    property Apply : TApplyExpression read FApply write FApply stored wstHas_Apply;
    property Cast : TCastOrIsOfExpression read FCast write FCast stored wstHas_Cast;
    property Collection : TCollectionExpression read FCollection write FCollection stored wstHas_Collection;
    property _If : TIfExpression read F_If write F_If stored wstHas__If;
    property Eq : TTwoChildrenExpression read FEq write FEq stored wstHas_Eq;
    property Ne : TTwoChildrenExpression read FNe write FNe stored wstHas_Ne;
    property Ge : TTwoChildrenExpression read FGe write FGe stored wstHas_Ge;
    property Gt : TTwoChildrenExpression read FGt write FGt stored wstHas_Gt;
    property Le : TTwoChildrenExpression read FLe write FLe stored wstHas_Le;
    property Lt : TTwoChildrenExpression read FLt write FLt stored wstHas_Lt;
    property _And : TTwoChildrenExpression read F_And write F_And stored wstHas__And;
    property _Or : TTwoChildrenExpression read F_Or write F_Or stored wstHas__Or;
    property _Not : TOneChildExpression read F_Not write F_Not stored wstHas__Not;
    property IsOf : TCastOrIsOfExpression read FIsOf write FIsOf stored wstHas_IsOf;
    property LabeledElement : TLabeledElementExpression read FLabeledElement write FLabeledElement stored wstHas_LabeledElement;
    property LabeledElementReference : TLabeledElementReferenceExpression read FLabeledElementReference write FLabeledElementReference stored wstHas_LabeledElementReference;
    property Null : TNullExpression read FNull write FNull stored wstHas_Null;
    property NavigationPropertyPath : TPathExpression read FNavigationPropertyPath write FNavigationPropertyPath stored wstHas_NavigationPropertyPath;
    property Path : TPathExpression read FPath write FPath stored wstHas_Path;
    property PropertyPath : TPathExpression read FPropertyPath write FPropertyPath stored wstHas_PropertyPath;
    property _Record : TRecordExpression read F_Record write F_Record stored wstHas__Record;
    property UrlRef : TOneChildExpression read FUrlRef write FUrlRef stored wstHas_UrlRef;
    property BinaryAtt : binary read FBinaryAtt write FBinaryAtt stored wstHas_BinaryAtt;
    property BoolAtt : boolean read FBoolAtt write FBoolAtt stored wstHas_BoolAtt;
    property DateAtt : TDateRemotable read FDateAtt write FDateAtt stored wstHas_DateAtt;
    property DateTimeOffsetAtt : dateTimeStamp read FDateTimeOffsetAtt write FDateTimeOffsetAtt stored wstHas_DateTimeOffsetAtt;
    property DecimalAtt : Currency read FDecimalAtt write FDecimalAtt stored wstHas_DecimalAtt;
    property DurationAtt : dayTimeDuration read FDurationAtt write FDurationAtt stored wstHas_DurationAtt;
    property EnumMemberAtt : TEnumMemberList read FEnumMemberAtt write FEnumMemberAtt stored wstHas_EnumMemberAtt;
    property FloatAtt : Double read FFloatAtt write FFloatAtt stored wstHas_FloatAtt;
    property GuidAtt : TGuidLiteral read FGuidAtt write FGuidAtt stored wstHas_GuidAtt;
    property IntAtt : integer read FIntAtt write FIntAtt stored wstHas_IntAtt;
    property _StringAtt : UnicodeString read F_StringAtt write F_StringAtt stored wstHas__StringAtt;
    property TimeOfDayAtt : TTimeRemotable read FTimeOfDayAtt write FTimeOfDayAtt stored wstHas_TimeOfDayAtt;
    property AnnotationPathAtt : TPathWithTermSegments read FAnnotationPathAtt write FAnnotationPathAtt stored wstHas_AnnotationPathAtt;
    property NavigationPropertyPathAtt : TPathWithTermSegments read FNavigationPropertyPathAtt write FNavigationPropertyPathAtt stored wstHas_NavigationPropertyPathAtt;
    property PathAtt : TPathWithTermSegments read FPathAtt write FPathAtt stored wstHas_PathAtt;
    property PropertyPathAtt : TPathWithTermSegments read FPropertyPathAtt write FPropertyPathAtt stored wstHas_PropertyPathAtt;
    property UrlRefAtt : anyURI read FUrlRefAtt write FUrlRefAtt stored wstHas_UrlRefAtt;
  end;

  TEntityContainer = class(TBaseComplexRemotable)
  private
    FEntitySet : TEntityContainer_EntitySetArray;
    FActionImport : TEntityContainer_ActionImportArray;
    FFunctionImport : TEntityContainer_FunctionImportArray;
    FSingleton : TEntityContainer_SingletonArray;
    FAnnotation : TEntityContainer_AnnotationArray;
    FName : TSimpleIdentifier;
    FExtends : TQualifiedName;
  private
    function wstHas_EntitySet() : Boolean;
    function wstHas_ActionImport() : Boolean;
    function wstHas_FunctionImport() : Boolean;
    function wstHas_Singleton() : Boolean;
    function wstHas_Annotation() : Boolean;
    function wstHas_Extends() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property EntitySet : TEntityContainer_EntitySetArray read FEntitySet write FEntitySet stored wstHas_EntitySet;
    property ActionImport : TEntityContainer_ActionImportArray read FActionImport write FActionImport stored wstHas_ActionImport;
    property FunctionImport : TEntityContainer_FunctionImportArray read FFunctionImport write FFunctionImport stored wstHas_FunctionImport;
    property Singleton : TEntityContainer_SingletonArray read FSingleton write FSingleton stored wstHas_Singleton;
    property Annotation : TEntityContainer_AnnotationArray read FAnnotation write FAnnotation stored wstHas_Annotation;
    property Name : TSimpleIdentifier read FName write FName;
    property Extends : TQualifiedName read FExtends write FExtends stored wstHas_Extends;
  end;

  TEntitySetAttributes = class(TBaseComplexRemotable)
  private
    FName : TSimpleIdentifier;
    FEntityType : TQualifiedName;
    FIncludeInServiceDocument : boolean;
  private
    function wstHas_IncludeInServiceDocument() : Boolean;
  published
    property Name : TSimpleIdentifier read FName write FName;
    property EntityType : TQualifiedName read FEntityType write FEntityType;
    property IncludeInServiceDocument : boolean read FIncludeInServiceDocument write FIncludeInServiceDocument stored wstHas_IncludeInServiceDocument;
  end;

  TEntitySet = class(TBaseComplexRemotable)
  private
    FNavigationPropertyBinding : TEntitySet_NavigationPropertyBindingArray;
    FAnnotation : TEntitySet_AnnotationArray;
    FName : TSimpleIdentifier;
    FEntityType : TQualifiedName;
    FIncludeInServiceDocument : boolean;
  private
    function wstHas_NavigationPropertyBinding() : Boolean;
    function wstHas_Annotation() : Boolean;
    function wstHas_IncludeInServiceDocument() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property NavigationPropertyBinding : TEntitySet_NavigationPropertyBindingArray read FNavigationPropertyBinding write FNavigationPropertyBinding stored wstHas_NavigationPropertyBinding;
    property Annotation : TEntitySet_AnnotationArray read FAnnotation write FAnnotation stored wstHas_Annotation;
    property Name : TSimpleIdentifier read FName write FName;
    property EntityType : TQualifiedName read FEntityType write FEntityType;
    property IncludeInServiceDocument : boolean read FIncludeInServiceDocument write FIncludeInServiceDocument stored wstHas_IncludeInServiceDocument;
  end;

  TNavigationPropertyBinding = class(TBaseComplexRemotable)
  private
    FPath : TPath;
    FTarget : TPath;
  published
    property Path : TPath read FPath write FPath;
    property Target : TPath read FTarget write FTarget;
  end;

  TSingleton = class(TBaseComplexRemotable)
  private
    FNavigationPropertyBinding : TSingleton_NavigationPropertyBindingArray;
    FAnnotation : TSingleton_AnnotationArray;
    FName : TSimpleIdentifier;
    F_Type : TQualifiedName;
  private
    function wstHas_NavigationPropertyBinding() : Boolean;
    function wstHas_Annotation() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property NavigationPropertyBinding : TSingleton_NavigationPropertyBindingArray read FNavigationPropertyBinding write FNavigationPropertyBinding stored wstHas_NavigationPropertyBinding;
    property Annotation : TSingleton_AnnotationArray read FAnnotation write FAnnotation stored wstHas_Annotation;
    property Name : TSimpleIdentifier read FName write FName;
    property _Type : TQualifiedName read F_Type write F_Type;
  end;

  TActionFunctionImportAttributes = class(TBaseComplexRemotable)
  private
    FName : TSimpleIdentifier;
    FEntitySet : TPath;
    FIncludeInServiceDocument : boolean;
  private
    function wstHas_EntitySet() : Boolean;
    function wstHas_IncludeInServiceDocument() : Boolean;
  published
    property Name : TSimpleIdentifier read FName write FName;
    property EntitySet : TPath read FEntitySet write FEntitySet stored wstHas_EntitySet;
    property IncludeInServiceDocument : boolean read FIncludeInServiceDocument write FIncludeInServiceDocument stored wstHas_IncludeInServiceDocument;
  end;

  TActionImport = class(TBaseComplexRemotable)
  private
    FAnnotation : TActionImport_AnnotationArray;
    FAction : TQualifiedName;
    FName : TSimpleIdentifier;
    FEntitySet : TPath;
    FIncludeInServiceDocument : boolean;
  private
    function wstHas_Annotation() : Boolean;
    function wstHas_EntitySet() : Boolean;
    function wstHas_IncludeInServiceDocument() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Annotation : TActionImport_AnnotationArray read FAnnotation write FAnnotation stored wstHas_Annotation;
    property Action : TQualifiedName read FAction write FAction;
    property Name : TSimpleIdentifier read FName write FName;
    property EntitySet : TPath read FEntitySet write FEntitySet stored wstHas_EntitySet;
    property IncludeInServiceDocument : boolean read FIncludeInServiceDocument write FIncludeInServiceDocument stored wstHas_IncludeInServiceDocument;
  end;

  TFunctionImport = class(TBaseComplexRemotable)
  private
    FAnnotation : TFunctionImport_AnnotationArray;
    F_Function : TQualifiedName;
    FName : TSimpleIdentifier;
    FEntitySet : TPath;
    FIncludeInServiceDocument : boolean;
  private
    function wstHas_Annotation() : Boolean;
    function wstHas_EntitySet() : Boolean;
    function wstHas_IncludeInServiceDocument() : Boolean;
  public
    constructor Create();override;
    procedure FreeObjectProperties();override;
  published
    property Annotation : TFunctionImport_AnnotationArray read FAnnotation write FAnnotation stored wstHas_Annotation;
    property _Function : TQualifiedName read F_Function write F_Function;
    property Name : TSimpleIdentifier read FName write FName;
    property EntitySet : TPath read FEntitySet write FEntitySet stored wstHas_EntitySet;
    property IncludeInServiceDocument : boolean read FIncludeInServiceDocument write FIncludeInServiceDocument stored wstHas_IncludeInServiceDocument;
  end;

  Schema_ComplexTypeArray = class(TObjectCollectionRemotable)
  private
    function GetItem(AIndex: Integer): TComplexType;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    function Add(): TComplexType; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function AddAt(const APosition : Integer) : TComplexType; {$IFDEF USE_INLINE}inline;{$ENDIF}
    property Item[AIndex:Integer] : TComplexType Read GetItem;Default;
  end;

  Schema_EntityTypeArray = class(TObjectCollectionRemotable)
  private
    function GetItem(AIndex: Integer): TEntityType;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    function Add(): TEntityType; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function AddAt(const APosition : Integer) : TEntityType; {$IFDEF USE_INLINE}inline;{$ENDIF}
    property Item[AIndex:Integer] : TEntityType Read GetItem;Default;
  end;

  Schema_TypeDefinitionArray = class(TObjectCollectionRemotable)
  private
    function GetItem(AIndex: Integer): TTypeDefinition;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    function Add(): TTypeDefinition; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function AddAt(const APosition : Integer) : TTypeDefinition; {$IFDEF USE_INLINE}inline;{$ENDIF}
    property Item[AIndex:Integer] : TTypeDefinition Read GetItem;Default;
  end;

  Schema_EnumTypeArray = class(TObjectCollectionRemotable)
  private
    function GetItem(AIndex: Integer): TEnumType;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    function Add(): TEnumType; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function AddAt(const APosition : Integer) : TEnumType; {$IFDEF USE_INLINE}inline;{$ENDIF}
    property Item[AIndex:Integer] : TEnumType Read GetItem;Default;
  end;

  Schema_ActionArray = class(TObjectCollectionRemotable)
  private
    function GetItem(AIndex: Integer): TAction;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    function Add(): TAction; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function AddAt(const APosition : Integer) : TAction; {$IFDEF USE_INLINE}inline;{$ENDIF}
    property Item[AIndex:Integer] : TAction Read GetItem;Default;
  end;

  Schema__FunctionArray = class(TObjectCollectionRemotable)
  private
    function GetItem(AIndex: Integer): TFunction;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    function Add(): TFunction; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function AddAt(const APosition : Integer) : TFunction; {$IFDEF USE_INLINE}inline;{$ENDIF}
    property Item[AIndex:Integer] : TFunction Read GetItem;Default;
  end;

  Schema_TermArray = class(TObjectCollectionRemotable)
  private
    function GetItem(AIndex: Integer): TTerm;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    function Add(): TTerm; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function AddAt(const APosition : Integer) : TTerm; {$IFDEF USE_INLINE}inline;{$ENDIF}
    property Item[AIndex:Integer] : TTerm Read GetItem;Default;
  end;

  Schema_AnnotationsArray = class(TObjectCollectionRemotable)
  private
    function GetItem(AIndex: Integer): TAnnotations;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    function Add(): TAnnotations; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function AddAt(const APosition : Integer) : TAnnotations; {$IFDEF USE_INLINE}inline;{$ENDIF}
    property Item[AIndex:Integer] : TAnnotations Read GetItem;Default;
  end;

  Schema_EntityContainerArray = class(TObjectCollectionRemotable)
  private
    function GetItem(AIndex: Integer): TEntityContainer;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    function Add(): TEntityContainer; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function AddAt(const APosition : Integer) : TEntityContainer; {$IFDEF USE_INLINE}inline;{$ENDIF}
    property Item[AIndex:Integer] : TEntityContainer Read GetItem;Default;
  end;

  Schema_AnnotationArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of Annotation_Type;
  private
    function GetItem(AIndex: Integer): Annotation_Type;
    procedure SetItem(AIndex: Integer; const AValue: Annotation_Type);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    procedure Assign(Source: TPersistent); override;
    property Item[AIndex:Integer] : Annotation_Type read GetItem write SetItem; default;
  end;

  TEntityType_KeyArray = class(TObjectCollectionRemotable)
  private
    function GetItem(AIndex: Integer): TEntityKeyElement;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    function Add(): TEntityKeyElement; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function AddAt(const APosition : Integer) : TEntityKeyElement; {$IFDEF USE_INLINE}inline;{$ENDIF}
    property Item[AIndex:Integer] : TEntityKeyElement Read GetItem;Default;
  end;

  TEntityType__PropertyArray = class(TObjectCollectionRemotable)
  private
    function GetItem(AIndex: Integer): TProperty;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    function Add(): TProperty; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function AddAt(const APosition : Integer) : TProperty; {$IFDEF USE_INLINE}inline;{$ENDIF}
    property Item[AIndex:Integer] : TProperty Read GetItem;Default;
  end;

  TEntityType_NavigationPropertyArray = class(TObjectCollectionRemotable)
  private
    function GetItem(AIndex: Integer): TNavigationProperty;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    function Add(): TNavigationProperty; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function AddAt(const APosition : Integer) : TNavigationProperty; {$IFDEF USE_INLINE}inline;{$ENDIF}
    property Item[AIndex:Integer] : TNavigationProperty Read GetItem;Default;
  end;

  TEntityType_AnnotationArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of Annotation_Type;
  private
    function GetItem(AIndex: Integer): Annotation_Type;
    procedure SetItem(AIndex: Integer; const AValue: Annotation_Type);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    procedure Assign(Source: TPersistent); override;
    property Item[AIndex:Integer] : Annotation_Type read GetItem write SetItem; default;
  end;

  TEntityKeyElement = class(TObjectCollectionRemotable)
  private
    function GetItem(AIndex: Integer): TPropertyRef;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    function Add(): TPropertyRef; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function AddAt(const APosition : Integer) : TPropertyRef; {$IFDEF USE_INLINE}inline;{$ENDIF}
    property Item[AIndex:Integer] : TPropertyRef Read GetItem;Default;
  end;

  TComplexType__PropertyArray = class(TObjectCollectionRemotable)
  private
    function GetItem(AIndex: Integer): TProperty;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    function Add(): TProperty; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function AddAt(const APosition : Integer) : TProperty; {$IFDEF USE_INLINE}inline;{$ENDIF}
    property Item[AIndex:Integer] : TProperty Read GetItem;Default;
  end;

  TComplexType_NavigationPropertyArray = class(TObjectCollectionRemotable)
  private
    function GetItem(AIndex: Integer): TNavigationProperty;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    function Add(): TNavigationProperty; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function AddAt(const APosition : Integer) : TNavigationProperty; {$IFDEF USE_INLINE}inline;{$ENDIF}
    property Item[AIndex:Integer] : TNavigationProperty Read GetItem;Default;
  end;

  TComplexType_AnnotationArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of Annotation_Type;
  private
    function GetItem(AIndex: Integer): Annotation_Type;
    procedure SetItem(AIndex: Integer; const AValue: Annotation_Type);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    procedure Assign(Source: TPersistent); override;
    property Item[AIndex:Integer] : Annotation_Type read GetItem write SetItem; default;
  end;

  TProperty_AnnotationArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of Annotation_Type;
  private
    function GetItem(AIndex: Integer): Annotation_Type;
    procedure SetItem(AIndex: Integer; const AValue: Annotation_Type);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    procedure Assign(Source: TPersistent); override;
    property Item[AIndex:Integer] : Annotation_Type read GetItem write SetItem; default;
  end;

  TTypeDefinition_AnnotationArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of Annotation_Type;
  private
    function GetItem(AIndex: Integer): Annotation_Type;
    procedure SetItem(AIndex: Integer; const AValue: Annotation_Type);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    procedure Assign(Source: TPersistent); override;
    property Item[AIndex:Integer] : Annotation_Type read GetItem write SetItem; default;
  end;

  TNavigationProperty_ReferentialConstraintArray = class(TObjectCollectionRemotable)
  private
    function GetItem(AIndex: Integer): TReferentialConstraint;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    function Add(): TReferentialConstraint; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function AddAt(const APosition : Integer) : TReferentialConstraint; {$IFDEF USE_INLINE}inline;{$ENDIF}
    property Item[AIndex:Integer] : TReferentialConstraint Read GetItem;Default;
  end;

  TNavigationProperty_OnDeleteArray = class(TObjectCollectionRemotable)
  private
    function GetItem(AIndex: Integer): TOnDelete;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    function Add(): TOnDelete; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function AddAt(const APosition : Integer) : TOnDelete; {$IFDEF USE_INLINE}inline;{$ENDIF}
    property Item[AIndex:Integer] : TOnDelete Read GetItem;Default;
  end;

  TNavigationProperty_AnnotationArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of Annotation_Type;
  private
    function GetItem(AIndex: Integer): Annotation_Type;
    procedure SetItem(AIndex: Integer; const AValue: Annotation_Type);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    procedure Assign(Source: TPersistent); override;
    property Item[AIndex:Integer] : Annotation_Type read GetItem write SetItem; default;
  end;

  TReferentialConstraint_AnnotationArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of Annotation_Type;
  private
    function GetItem(AIndex: Integer): Annotation_Type;
    procedure SetItem(AIndex: Integer; const AValue: Annotation_Type);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    procedure Assign(Source: TPersistent); override;
    property Item[AIndex:Integer] : Annotation_Type read GetItem write SetItem; default;
  end;

  TOnDelete_AnnotationArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of Annotation_Type;
  private
    function GetItem(AIndex: Integer): Annotation_Type;
    procedure SetItem(AIndex: Integer; const AValue: Annotation_Type);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    procedure Assign(Source: TPersistent); override;
    property Item[AIndex:Integer] : Annotation_Type read GetItem write SetItem; default;
  end;

  TEnumType_MemberArray = class(TObjectCollectionRemotable)
  private
    function GetItem(AIndex: Integer): TEnumTypeMember;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    function Add(): TEnumTypeMember; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function AddAt(const APosition : Integer) : TEnumTypeMember; {$IFDEF USE_INLINE}inline;{$ENDIF}
    property Item[AIndex:Integer] : TEnumTypeMember Read GetItem;Default;
  end;

  TEnumType_AnnotationArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of Annotation_Type;
  private
    function GetItem(AIndex: Integer): Annotation_Type;
    procedure SetItem(AIndex: Integer; const AValue: Annotation_Type);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    procedure Assign(Source: TPersistent); override;
    property Item[AIndex:Integer] : Annotation_Type read GetItem write SetItem; default;
  end;

  TEnumTypeMember_AnnotationArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of Annotation_Type;
  private
    function GetItem(AIndex: Integer): Annotation_Type;
    procedure SetItem(AIndex: Integer; const AValue: Annotation_Type);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    procedure Assign(Source: TPersistent); override;
    property Item[AIndex:Integer] : Annotation_Type read GetItem write SetItem; default;
  end;

  TActionFunctionReturnType_AnnotationArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of Annotation_Type;
  private
    function GetItem(AIndex: Integer): Annotation_Type;
    procedure SetItem(AIndex: Integer; const AValue: Annotation_Type);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    procedure Assign(Source: TPersistent); override;
    property Item[AIndex:Integer] : Annotation_Type read GetItem write SetItem; default;
  end;

  TAction_ParameterArray = class(TObjectCollectionRemotable)
  private
    function GetItem(AIndex: Integer): TActionFunctionParameter;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    function Add(): TActionFunctionParameter; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function AddAt(const APosition : Integer) : TActionFunctionParameter; {$IFDEF USE_INLINE}inline;{$ENDIF}
    property Item[AIndex:Integer] : TActionFunctionParameter Read GetItem;Default;
  end;

  TAction_AnnotationArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of Annotation_Type;
  private
    function GetItem(AIndex: Integer): Annotation_Type;
    procedure SetItem(AIndex: Integer; const AValue: Annotation_Type);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    procedure Assign(Source: TPersistent); override;
    property Item[AIndex:Integer] : Annotation_Type read GetItem write SetItem; default;
  end;

  TFunction_ParameterArray = class(TObjectCollectionRemotable)
  private
    function GetItem(AIndex: Integer): TActionFunctionParameter;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    function Add(): TActionFunctionParameter; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function AddAt(const APosition : Integer) : TActionFunctionParameter; {$IFDEF USE_INLINE}inline;{$ENDIF}
    property Item[AIndex:Integer] : TActionFunctionParameter Read GetItem;Default;
  end;

  TFunction_AnnotationArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of Annotation_Type;
  private
    function GetItem(AIndex: Integer): Annotation_Type;
    procedure SetItem(AIndex: Integer; const AValue: Annotation_Type);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    procedure Assign(Source: TPersistent); override;
    property Item[AIndex:Integer] : Annotation_Type read GetItem write SetItem; default;
  end;

  TActionFunctionParameter_AnnotationArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of Annotation_Type;
  private
    function GetItem(AIndex: Integer): Annotation_Type;
    procedure SetItem(AIndex: Integer; const AValue: Annotation_Type);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    procedure Assign(Source: TPersistent); override;
    property Item[AIndex:Integer] : Annotation_Type read GetItem write SetItem; default;
  end;

  TTerm_AnnotationArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of Annotation_Type;
  private
    function GetItem(AIndex: Integer): Annotation_Type;
    procedure SetItem(AIndex: Integer; const AValue: Annotation_Type);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    procedure Assign(Source: TPersistent); override;
    property Item[AIndex:Integer] : Annotation_Type read GetItem write SetItem; default;
  end;

  TAnnotations_AnnotationArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of Annotation_Type;
  private
    function GetItem(AIndex: Integer): Annotation_Type;
    procedure SetItem(AIndex: Integer; const AValue: Annotation_Type);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    procedure Assign(Source: TPersistent); override;
    property Item[AIndex:Integer] : Annotation_Type read GetItem write SetItem; default;
  end;

  Annotation_AnnotationArray = class(TBaseSimpleTypeArrayRemotable)
  private
    FData : array of Annotation_Type;
  private
    function GetItem(AIndex: Integer): Annotation_Type;
    procedure SetItem(AIndex: Integer; const AValue: Annotation_Type);
  protected
    function GetLength():Integer;override;
    procedure SaveItem(AStore : IFormatterBase;const AName : String;const AIndex : Integer);override;
    procedure LoadItem(AStore : IFormatterBase;const AIndex : Integer);override;
  public
    class function GetItemTypeInfo():PTypeInfo;override;
    procedure SetLength(const ANewSize : Integer);override;
    procedure Assign(Source: TPersistent); override;
    property Item[AIndex:Integer] : Annotation_Type read GetItem write SetItem; default;
  end;

  TApplyExpression_AnnotationArray = class(TObjectCollectionRemotable)
  private
    function GetItem(AIndex: Integer): Annotation_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    function Add(): Annotation_Type; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function AddAt(const APosition : Integer) : Annotation_Type; {$IFDEF USE_INLINE}inline;{$ENDIF}
    property Item[AIndex:Integer] : Annotation_Type Read GetItem;Default;
  end;

  TCastOrIsOfExpression_AnnotationArray = class(TObjectCollectionRemotable)
  private
    function GetItem(AIndex: Integer): Annotation_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    function Add(): Annotation_Type; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function AddAt(const APosition : Integer) : Annotation_Type; {$IFDEF USE_INLINE}inline;{$ENDIF}
    property Item[AIndex:Integer] : Annotation_Type Read GetItem;Default;
  end;

  TIfExpression_AnnotationArray = class(TObjectCollectionRemotable)
  private
    function GetItem(AIndex: Integer): Annotation_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    function Add(): Annotation_Type; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function AddAt(const APosition : Integer) : Annotation_Type; {$IFDEF USE_INLINE}inline;{$ENDIF}
    property Item[AIndex:Integer] : Annotation_Type Read GetItem;Default;
  end;

  TOneChildExpression_AnnotationArray = class(TObjectCollectionRemotable)
  private
    function GetItem(AIndex: Integer): Annotation_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    function Add(): Annotation_Type; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function AddAt(const APosition : Integer) : Annotation_Type; {$IFDEF USE_INLINE}inline;{$ENDIF}
    property Item[AIndex:Integer] : Annotation_Type Read GetItem;Default;
  end;

  TTwoChildrenExpression_AnnotationArray = class(TObjectCollectionRemotable)
  private
    function GetItem(AIndex: Integer): Annotation_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    function Add(): Annotation_Type; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function AddAt(const APosition : Integer) : Annotation_Type; {$IFDEF USE_INLINE}inline;{$ENDIF}
    property Item[AIndex:Integer] : Annotation_Type Read GetItem;Default;
  end;

  TLabeledElementExpression_AnnotationArray = class(TObjectCollectionRemotable)
  private
    function GetItem(AIndex: Integer): Annotation_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    function Add(): Annotation_Type; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function AddAt(const APosition : Integer) : Annotation_Type; {$IFDEF USE_INLINE}inline;{$ENDIF}
    property Item[AIndex:Integer] : Annotation_Type Read GetItem;Default;
  end;

  TNullExpression = class(TObjectCollectionRemotable)
  private
    function GetItem(AIndex: Integer): Annotation_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    function Add(): Annotation_Type; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function AddAt(const APosition : Integer) : Annotation_Type; {$IFDEF USE_INLINE}inline;{$ENDIF}
    property Item[AIndex:Integer] : Annotation_Type Read GetItem;Default;
  end;

  TRecordExpression_PropertyValueArray = class(TObjectCollectionRemotable)
  private
    function GetItem(AIndex: Integer): TPropertyValue;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    function Add(): TPropertyValue; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function AddAt(const APosition : Integer) : TPropertyValue; {$IFDEF USE_INLINE}inline;{$ENDIF}
    property Item[AIndex:Integer] : TPropertyValue Read GetItem;Default;
  end;

  TRecordExpression_AnnotationArray = class(TObjectCollectionRemotable)
  private
    function GetItem(AIndex: Integer): Annotation_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    function Add(): Annotation_Type; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function AddAt(const APosition : Integer) : Annotation_Type; {$IFDEF USE_INLINE}inline;{$ENDIF}
    property Item[AIndex:Integer] : Annotation_Type Read GetItem;Default;
  end;

  TPropertyValue_AnnotationArray = class(TObjectCollectionRemotable)
  private
    function GetItem(AIndex: Integer): Annotation_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    function Add(): Annotation_Type; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function AddAt(const APosition : Integer) : Annotation_Type; {$IFDEF USE_INLINE}inline;{$ENDIF}
    property Item[AIndex:Integer] : Annotation_Type Read GetItem;Default;
  end;

  TEntityContainer_EntitySetArray = class(TObjectCollectionRemotable)
  private
    function GetItem(AIndex: Integer): TEntitySet;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    function Add(): TEntitySet; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function AddAt(const APosition : Integer) : TEntitySet; {$IFDEF USE_INLINE}inline;{$ENDIF}
    property Item[AIndex:Integer] : TEntitySet Read GetItem;Default;
  end;

  TEntityContainer_ActionImportArray = class(TObjectCollectionRemotable)
  private
    function GetItem(AIndex: Integer): TActionImport;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    function Add(): TActionImport; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function AddAt(const APosition : Integer) : TActionImport; {$IFDEF USE_INLINE}inline;{$ENDIF}
    property Item[AIndex:Integer] : TActionImport Read GetItem;Default;
  end;

  TEntityContainer_FunctionImportArray = class(TObjectCollectionRemotable)
  private
    function GetItem(AIndex: Integer): TFunctionImport;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    function Add(): TFunctionImport; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function AddAt(const APosition : Integer) : TFunctionImport; {$IFDEF USE_INLINE}inline;{$ENDIF}
    property Item[AIndex:Integer] : TFunctionImport Read GetItem;Default;
  end;

  TEntityContainer_SingletonArray = class(TObjectCollectionRemotable)
  private
    function GetItem(AIndex: Integer): TSingleton;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    function Add(): TSingleton; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function AddAt(const APosition : Integer) : TSingleton; {$IFDEF USE_INLINE}inline;{$ENDIF}
    property Item[AIndex:Integer] : TSingleton Read GetItem;Default;
  end;

  TEntityContainer_AnnotationArray = class(TObjectCollectionRemotable)
  private
    function GetItem(AIndex: Integer): Annotation_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    function Add(): Annotation_Type; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function AddAt(const APosition : Integer) : Annotation_Type; {$IFDEF USE_INLINE}inline;{$ENDIF}
    property Item[AIndex:Integer] : Annotation_Type Read GetItem;Default;
  end;

  TEntitySet_NavigationPropertyBindingArray = class(TObjectCollectionRemotable)
  private
    function GetItem(AIndex: Integer): TNavigationPropertyBinding;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    function Add(): TNavigationPropertyBinding; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function AddAt(const APosition : Integer) : TNavigationPropertyBinding; {$IFDEF USE_INLINE}inline;{$ENDIF}
    property Item[AIndex:Integer] : TNavigationPropertyBinding Read GetItem;Default;
  end;

  TEntitySet_AnnotationArray = class(TObjectCollectionRemotable)
  private
    function GetItem(AIndex: Integer): Annotation_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    function Add(): Annotation_Type; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function AddAt(const APosition : Integer) : Annotation_Type; {$IFDEF USE_INLINE}inline;{$ENDIF}
    property Item[AIndex:Integer] : Annotation_Type Read GetItem;Default;
  end;

  TSingleton_NavigationPropertyBindingArray = class(TObjectCollectionRemotable)
  private
    function GetItem(AIndex: Integer): TNavigationPropertyBinding;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    function Add(): TNavigationPropertyBinding; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function AddAt(const APosition : Integer) : TNavigationPropertyBinding; {$IFDEF USE_INLINE}inline;{$ENDIF}
    property Item[AIndex:Integer] : TNavigationPropertyBinding Read GetItem;Default;
  end;

  TSingleton_AnnotationArray = class(TObjectCollectionRemotable)
  private
    function GetItem(AIndex: Integer): Annotation_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    function Add(): Annotation_Type; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function AddAt(const APosition : Integer) : Annotation_Type; {$IFDEF USE_INLINE}inline;{$ENDIF}
    property Item[AIndex:Integer] : Annotation_Type Read GetItem;Default;
  end;

  TActionImport_AnnotationArray = class(TObjectCollectionRemotable)
  private
    function GetItem(AIndex: Integer): Annotation_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    function Add(): Annotation_Type; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function AddAt(const APosition : Integer) : Annotation_Type; {$IFDEF USE_INLINE}inline;{$ENDIF}
    property Item[AIndex:Integer] : Annotation_Type Read GetItem;Default;
  end;

  TFunctionImport_AnnotationArray = class(TObjectCollectionRemotable)
  private
    function GetItem(AIndex: Integer): Annotation_Type;
  public
    class function GetItemClass():TBaseRemotableClass;override;
    function Add(): Annotation_Type; {$IFDEF USE_INLINE}inline;{$ENDIF}
    function AddAt(const APosition : Integer) : Annotation_Type; {$IFDEF USE_INLINE}inline;{$ENDIF}
    property Item[AIndex:Integer] : Annotation_Type Read GetItem;Default;
  end;

Implementation
uses metadata_repository, record_rtti, wst_types;

{ Schema }

constructor Schema.Create();
begin
  inherited Create();
  FComplexType := Schema_ComplexTypeArray.Create();
  FEntityType := Schema_EntityTypeArray.Create();
  FTypeDefinition := Schema_TypeDefinitionArray.Create();
  FEnumType := Schema_EnumTypeArray.Create();
  FAction := Schema_ActionArray.Create();
  F_Function := Schema__FunctionArray.Create();
  FTerm := Schema_TermArray.Create();
  FAnnotations := Schema_AnnotationsArray.Create();
  FEntityContainer := Schema_EntityContainerArray.Create();
  FAnnotation := Schema_AnnotationArray.Create();
end;

procedure Schema.FreeObjectProperties();
begin
  if Assigned(FComplexType) then
    FreeAndNil(FComplexType);
  if Assigned(FEntityType) then
    FreeAndNil(FEntityType);
  if Assigned(FTypeDefinition) then
    FreeAndNil(FTypeDefinition);
  if Assigned(FEnumType) then
    FreeAndNil(FEnumType);
  if Assigned(FAction) then
    FreeAndNil(FAction);
  if Assigned(F_Function) then
    FreeAndNil(F_Function);
  if Assigned(FTerm) then
    FreeAndNil(FTerm);
  if Assigned(FAnnotations) then
    FreeAndNil(FAnnotations);
  if Assigned(FEntityContainer) then
    FreeAndNil(FEntityContainer);
  if Assigned(FAnnotation) then
    FreeAndNil(FAnnotation);
  inherited FreeObjectProperties();
end;

function Schema.wstHas_ComplexType() : Boolean;
begin
  Result := ( FComplexType <> Schema_ComplexTypeArray(0) );
end;

function Schema.wstHas_EntityType() : Boolean;
begin
  Result := ( FEntityType <> Schema_EntityTypeArray(0) );
end;

function Schema.wstHas_TypeDefinition() : Boolean;
begin
  Result := ( FTypeDefinition <> Schema_TypeDefinitionArray(0) );
end;

function Schema.wstHas_EnumType() : Boolean;
begin
  Result := ( FEnumType <> Schema_EnumTypeArray(0) );
end;

function Schema.wstHas_Action() : Boolean;
begin
  Result := ( FAction <> Schema_ActionArray(0) );
end;

function Schema.wstHas__Function() : Boolean;
begin
  Result := ( F_Function <> Schema__FunctionArray(0) );
end;

function Schema.wstHas_Term() : Boolean;
begin
  Result := ( FTerm <> Schema_TermArray(0) );
end;

function Schema.wstHas_Annotations() : Boolean;
begin
  Result := ( FAnnotations <> Schema_AnnotationsArray(0) );
end;

function Schema.wstHas_EntityContainer() : Boolean;
begin
  Result := ( FEntityContainer <> Schema_EntityContainerArray(0) );
end;

function Schema.wstHas_Annotation() : Boolean;
begin
  Result := ( FAnnotation <> Schema_AnnotationArray(0) );
end;

function Schema.wstHas_Alias() : Boolean;
begin
  Result := ( FAlias <> '' );
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
  FKey := TEntityType_KeyArray.Create();
  F_Property := TEntityType__PropertyArray.Create();
  FNavigationProperty := TEntityType_NavigationPropertyArray.Create();
  FAnnotation := TEntityType_AnnotationArray.Create();
end;

procedure TEntityType.FreeObjectProperties();
begin
  if Assigned(FKey) then
    FreeAndNil(FKey);
  if Assigned(F_Property) then
    FreeAndNil(F_Property);
  if Assigned(FNavigationProperty) then
    FreeAndNil(FNavigationProperty);
  if Assigned(FAnnotation) then
    FreeAndNil(FAnnotation);
  inherited FreeObjectProperties();
end;

function TEntityType.wstHas_Key() : Boolean;
begin
  Result := ( FKey <> TEntityType_KeyArray(0) );
end;

function TEntityType.wstHas__Property() : Boolean;
begin
  Result := ( F_Property <> TEntityType__PropertyArray(0) );
end;

function TEntityType.wstHas_NavigationProperty() : Boolean;
begin
  Result := ( FNavigationProperty <> TEntityType_NavigationPropertyArray(0) );
end;

function TEntityType.wstHas_Annotation() : Boolean;
begin
  Result := ( FAnnotation <> TEntityType_AnnotationArray(0) );
end;

function TEntityType.wstHas_OpenType() : Boolean;
begin
  Result := ( FOpenType <> boolean(0) );
end;

function TEntityType.wstHas_HasStream() : Boolean;
begin
  Result := ( FHasStream <> boolean(0) );
end;

function TEntityType.wstHas_BaseType() : Boolean;
begin
  Result := ( FBaseType <> '' );
end;

function TEntityType.wstHas__Abstract() : Boolean;
begin
  Result := ( F_Abstract <> boolean(0) );
end;

function TPropertyRef.wstHas_Alias() : Boolean;
begin
  Result := ( FAlias <> '' );
end;

{ TComplexType }

constructor TComplexType.Create();
begin
  inherited Create();
  F_Property := TComplexType__PropertyArray.Create();
  FNavigationProperty := TComplexType_NavigationPropertyArray.Create();
  FAnnotation := TComplexType_AnnotationArray.Create();
end;

procedure TComplexType.FreeObjectProperties();
begin
  if Assigned(F_Property) then
    FreeAndNil(F_Property);
  if Assigned(FNavigationProperty) then
    FreeAndNil(FNavigationProperty);
  if Assigned(FAnnotation) then
    FreeAndNil(FAnnotation);
  inherited FreeObjectProperties();
end;

function TComplexType.wstHas__Property() : Boolean;
begin
  Result := ( F_Property <> TComplexType__PropertyArray(0) );
end;

function TComplexType.wstHas_NavigationProperty() : Boolean;
begin
  Result := ( FNavigationProperty <> TComplexType_NavigationPropertyArray(0) );
end;

function TComplexType.wstHas_Annotation() : Boolean;
begin
  Result := ( FAnnotation <> TComplexType_AnnotationArray(0) );
end;

function TComplexType.wstHas_OpenType() : Boolean;
begin
  Result := ( FOpenType <> boolean(0) );
end;

function TComplexType.wstHas_BaseType() : Boolean;
begin
  Result := ( FBaseType <> '' );
end;

function TComplexType.wstHas__Abstract() : Boolean;
begin
  Result := ( F_Abstract <> boolean(0) );
end;

function TFacetAttributes.wstHas_MaxLength() : Boolean;
begin
  Result := ( FMaxLength <> '' );
end;

function TFacetAttributes.wstHas_Precision() : Boolean;
begin
  Result := ( FPrecision <> TPrecisionFacet(0) );
end;

function TFacetAttributes.wstHas_Scale() : Boolean;
begin
  Result := ( FScale <> '' );
end;

function TFacetAttributes.wstHas_SRID() : Boolean;
begin
  Result := ( FSRID <> '' );
end;

function TPropertyFacetAttributes.wstHas_Unicode() : Boolean;
begin
  Result := ( FUnicode <> TUnicodeFacet(0) );
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

function TCommonPropertyAttributes.wstHas_Precision() : Boolean;
begin
  Result := ( FPrecision <> TPrecisionFacet(0) );
end;

function TCommonPropertyAttributes.wstHas_Scale() : Boolean;
begin
  Result := ( FScale <> '' );
end;

function TCommonPropertyAttributes.wstHas_SRID() : Boolean;
begin
  Result := ( FSRID <> '' );
end;

function TCommonPropertyAttributes.wstHas_Unicode() : Boolean;
begin
  Result := ( FUnicode <> TUnicodeFacet(0) );
end;

{ TProperty }

constructor TProperty.Create();
begin
  inherited Create();
  FAnnotation := TProperty_AnnotationArray.Create();
end;

procedure TProperty.FreeObjectProperties();
begin
  if Assigned(FAnnotation) then
    FreeAndNil(FAnnotation);
  inherited FreeObjectProperties();
end;

function TProperty.wstHas_Annotation() : Boolean;
begin
  Result := ( FAnnotation <> TProperty_AnnotationArray(0) );
end;

function TProperty.wstHas_Nullable() : Boolean;
begin
  Result := ( FNullable <> boolean(0) );
end;

function TProperty.wstHas_DefaultValue() : Boolean;
begin
  Result := ( FDefaultValue <> '' );
end;

function TProperty.wstHas_MaxLength() : Boolean;
begin
  Result := ( FMaxLength <> '' );
end;

function TProperty.wstHas_Precision() : Boolean;
begin
  Result := ( FPrecision <> TPrecisionFacet(0) );
end;

function TProperty.wstHas_Scale() : Boolean;
begin
  Result := ( FScale <> '' );
end;

function TProperty.wstHas_SRID() : Boolean;
begin
  Result := ( FSRID <> '' );
end;

function TProperty.wstHas_Unicode() : Boolean;
begin
  Result := ( FUnicode <> TUnicodeFacet(0) );
end;

{ TTypeDefinition }

constructor TTypeDefinition.Create();
begin
  inherited Create();
  FAnnotation := TTypeDefinition_AnnotationArray.Create();
end;

procedure TTypeDefinition.FreeObjectProperties();
begin
  if Assigned(FAnnotation) then
    FreeAndNil(FAnnotation);
  inherited FreeObjectProperties();
end;

function TTypeDefinition.wstHas_Annotation() : Boolean;
begin
  Result := ( FAnnotation <> TTypeDefinition_AnnotationArray(0) );
end;

function TTypeDefinition.wstHas_MaxLength() : Boolean;
begin
  Result := ( FMaxLength <> '' );
end;

function TTypeDefinition.wstHas_Precision() : Boolean;
begin
  Result := ( FPrecision <> TPrecisionFacet(0) );
end;

function TTypeDefinition.wstHas_Scale() : Boolean;
begin
  Result := ( FScale <> '' );
end;

function TTypeDefinition.wstHas_SRID() : Boolean;
begin
  Result := ( FSRID <> '' );
end;

function TTypeDefinition.wstHas_Unicode() : Boolean;
begin
  Result := ( FUnicode <> TUnicodeFacet(0) );
end;

{ TNavigationProperty }

constructor TNavigationProperty.Create();
begin
  inherited Create();
  FReferentialConstraint := TNavigationProperty_ReferentialConstraintArray.Create();
  FOnDelete := TNavigationProperty_OnDeleteArray.Create();
  FAnnotation := TNavigationProperty_AnnotationArray.Create();
end;

procedure TNavigationProperty.FreeObjectProperties();
begin
  if Assigned(FReferentialConstraint) then
    FreeAndNil(FReferentialConstraint);
  if Assigned(FOnDelete) then
    FreeAndNil(FOnDelete);
  if Assigned(FAnnotation) then
    FreeAndNil(FAnnotation);
  inherited FreeObjectProperties();
end;

function TNavigationProperty.wstHas_ReferentialConstraint() : Boolean;
begin
  Result := ( FReferentialConstraint <> TNavigationProperty_ReferentialConstraintArray(0) );
end;

function TNavigationProperty.wstHas_OnDelete() : Boolean;
begin
  Result := ( FOnDelete <> TNavigationProperty_OnDeleteArray(0) );
end;

function TNavigationProperty.wstHas_Annotation() : Boolean;
begin
  Result := ( FAnnotation <> TNavigationProperty_AnnotationArray(0) );
end;

function TNavigationProperty.wstHas_Nullable() : Boolean;
begin
  Result := ( FNullable <> boolean(0) );
end;

function TNavigationProperty.wstHas_Partner() : Boolean;
begin
  Result := ( FPartner <> '' );
end;

function TNavigationProperty.wstHas_ContainsTarget() : Boolean;
begin
  Result := ( FContainsTarget <> boolean(0) );
end;

{ TReferentialConstraint }

constructor TReferentialConstraint.Create();
begin
  inherited Create();
  FAnnotation := TReferentialConstraint_AnnotationArray.Create();
end;

procedure TReferentialConstraint.FreeObjectProperties();
begin
  if Assigned(FAnnotation) then
    FreeAndNil(FAnnotation);
  inherited FreeObjectProperties();
end;

function TReferentialConstraint.wstHas_Annotation() : Boolean;
begin
  Result := ( FAnnotation <> TReferentialConstraint_AnnotationArray(0) );
end;

{ TOnDelete }

constructor TOnDelete.Create();
begin
  inherited Create();
  FAnnotation := TOnDelete_AnnotationArray.Create();
end;

procedure TOnDelete.FreeObjectProperties();
begin
  if Assigned(FAnnotation) then
    FreeAndNil(FAnnotation);
  inherited FreeObjectProperties();
end;

function TOnDelete.wstHas_Annotation() : Boolean;
begin
  Result := ( FAnnotation <> TOnDelete_AnnotationArray(0) );
end;

{ TEnumType }

constructor TEnumType.Create();
begin
  inherited Create();
  FMember := TEnumType_MemberArray.Create();
  FAnnotation := TEnumType_AnnotationArray.Create();
end;

procedure TEnumType.FreeObjectProperties();
begin
  if Assigned(FMember) then
    FreeAndNil(FMember);
  if Assigned(FAnnotation) then
    FreeAndNil(FAnnotation);
  inherited FreeObjectProperties();
end;

function TEnumType.wstHas_Member() : Boolean;
begin
  Result := ( FMember <> TEnumType_MemberArray(0) );
end;

function TEnumType.wstHas_Annotation() : Boolean;
begin
  Result := ( FAnnotation <> TEnumType_AnnotationArray(0) );
end;

function TEnumType.wstHas_IsFlags() : Boolean;
begin
  Result := ( FIsFlags <> boolean(0) );
end;

function TEnumType.wstHas_UnderlyingType() : Boolean;
begin
  Result := ( FUnderlyingType <> '' );
end;

{ TEnumTypeMember }

constructor TEnumTypeMember.Create();
begin
  inherited Create();
  FAnnotation := TEnumTypeMember_AnnotationArray.Create();
end;

procedure TEnumTypeMember.FreeObjectProperties();
begin
  if Assigned(FAnnotation) then
    FreeAndNil(FAnnotation);
  inherited FreeObjectProperties();
end;

function TEnumTypeMember.wstHas_Annotation() : Boolean;
begin
  Result := ( FAnnotation <> TEnumTypeMember_AnnotationArray(0) );
end;

function TEnumTypeMember.wstHas_Value() : Boolean;
begin
  Result := ( FValue <> Int64(0) );
end;

{ TActionFunctionReturnType }

constructor TActionFunctionReturnType.Create();
begin
  inherited Create();
  FAnnotation := TActionFunctionReturnType_AnnotationArray.Create();
end;

procedure TActionFunctionReturnType.FreeObjectProperties();
begin
  if Assigned(FAnnotation) then
    FreeAndNil(FAnnotation);
  inherited FreeObjectProperties();
end;

function TActionFunctionReturnType.wstHas_Annotation() : Boolean;
begin
  Result := ( FAnnotation <> TActionFunctionReturnType_AnnotationArray(0) );
end;

function TActionFunctionReturnType.wstHas_Nullable() : Boolean;
begin
  Result := ( FNullable <> boolean(0) );
end;

function TActionFunctionReturnType.wstHas_MaxLength() : Boolean;
begin
  Result := ( FMaxLength <> '' );
end;

function TActionFunctionReturnType.wstHas_Precision() : Boolean;
begin
  Result := ( FPrecision <> TPrecisionFacet(0) );
end;

function TActionFunctionReturnType.wstHas_Scale() : Boolean;
begin
  Result := ( FScale <> '' );
end;

function TActionFunctionReturnType.wstHas_SRID() : Boolean;
begin
  Result := ( FSRID <> '' );
end;

function TActionAttributes.wstHas_EntitySetPath() : Boolean;
begin
  Result := ( FEntitySetPath <> '' );
end;

function TActionAttributes.wstHas_IsBound() : Boolean;
begin
  Result := ( FIsBound <> boolean(0) );
end;

{ TAction }

constructor TAction.Create();
begin
  inherited Create();
  FParameter := TAction_ParameterArray.Create();
  FAnnotation := TAction_AnnotationArray.Create();
  FReturnType := TActionFunctionReturnType.Create();
end;

procedure TAction.FreeObjectProperties();
begin
  if Assigned(FParameter) then
    FreeAndNil(FParameter);
  if Assigned(FAnnotation) then
    FreeAndNil(FAnnotation);
  if Assigned(FReturnType) then
    FreeAndNil(FReturnType);
  inherited FreeObjectProperties();
end;

function TAction.wstHas_Parameter() : Boolean;
begin
  Result := ( FParameter <> TAction_ParameterArray(0) );
end;

function TAction.wstHas_Annotation() : Boolean;
begin
  Result := ( FAnnotation <> TAction_AnnotationArray(0) );
end;

function TAction.wstHas_ReturnType() : Boolean;
begin
  Result := ( FReturnType <> nil );
end;

function TAction.wstHas_EntitySetPath() : Boolean;
begin
  Result := ( FEntitySetPath <> '' );
end;

function TAction.wstHas_IsBound() : Boolean;
begin
  Result := ( FIsBound <> boolean(0) );
end;

function TFunctionAttributes.wstHas_EntitySetPath() : Boolean;
begin
  Result := ( FEntitySetPath <> '' );
end;

function TFunctionAttributes.wstHas_IsBound() : Boolean;
begin
  Result := ( FIsBound <> boolean(0) );
end;

function TFunctionAttributes.wstHas_IsComposable() : Boolean;
begin
  Result := ( FIsComposable <> boolean(0) );
end;

{ TFunction }

constructor TFunction.Create();
begin
  inherited Create();
  FParameter := TFunction_ParameterArray.Create();
  FAnnotation := TFunction_AnnotationArray.Create();
  FReturnType := TActionFunctionReturnType.Create();
end;

procedure TFunction.FreeObjectProperties();
begin
  if Assigned(FParameter) then
    FreeAndNil(FParameter);
  if Assigned(FAnnotation) then
    FreeAndNil(FAnnotation);
  if Assigned(FReturnType) then
    FreeAndNil(FReturnType);
  inherited FreeObjectProperties();
end;

function TFunction.wstHas_Parameter() : Boolean;
begin
  Result := ( FParameter <> TFunction_ParameterArray(0) );
end;

function TFunction.wstHas_Annotation() : Boolean;
begin
  Result := ( FAnnotation <> TFunction_AnnotationArray(0) );
end;

function TFunction.wstHas_EntitySetPath() : Boolean;
begin
  Result := ( FEntitySetPath <> '' );
end;

function TFunction.wstHas_IsBound() : Boolean;
begin
  Result := ( FIsBound <> boolean(0) );
end;

function TFunction.wstHas_IsComposable() : Boolean;
begin
  Result := ( FIsComposable <> boolean(0) );
end;

function TActionFunctionParameterAttributes.wstHas_Nullable() : Boolean;
begin
  Result := ( FNullable <> boolean(0) );
end;

function TActionFunctionParameterAttributes.wstHas_MaxLength() : Boolean;
begin
  Result := ( FMaxLength <> '' );
end;

function TActionFunctionParameterAttributes.wstHas_Precision() : Boolean;
begin
  Result := ( FPrecision <> TPrecisionFacet(0) );
end;

function TActionFunctionParameterAttributes.wstHas_Scale() : Boolean;
begin
  Result := ( FScale <> '' );
end;

function TActionFunctionParameterAttributes.wstHas_SRID() : Boolean;
begin
  Result := ( FSRID <> '' );
end;

{ TActionFunctionParameter }

constructor TActionFunctionParameter.Create();
begin
  inherited Create();
  FAnnotation := TActionFunctionParameter_AnnotationArray.Create();
end;

procedure TActionFunctionParameter.FreeObjectProperties();
begin
  if Assigned(FAnnotation) then
    FreeAndNil(FAnnotation);
  inherited FreeObjectProperties();
end;

function TActionFunctionParameter.wstHas_Annotation() : Boolean;
begin
  Result := ( FAnnotation <> TActionFunctionParameter_AnnotationArray(0) );
end;

function TActionFunctionParameter.wstHas_Nullable() : Boolean;
begin
  Result := ( FNullable <> boolean(0) );
end;

function TActionFunctionParameter.wstHas_MaxLength() : Boolean;
begin
  Result := ( FMaxLength <> '' );
end;

function TActionFunctionParameter.wstHas_Precision() : Boolean;
begin
  Result := ( FPrecision <> TPrecisionFacet(0) );
end;

function TActionFunctionParameter.wstHas_Scale() : Boolean;
begin
  Result := ( FScale <> '' );
end;

function TActionFunctionParameter.wstHas_SRID() : Boolean;
begin
  Result := ( FSRID <> '' );
end;

{ TTerm }

constructor TTerm.Create();
begin
  inherited Create();
  FAnnotation := TTerm_AnnotationArray.Create();
end;

procedure TTerm.FreeObjectProperties();
begin
  if Assigned(FAnnotation) then
    FreeAndNil(FAnnotation);
  inherited FreeObjectProperties();
end;

function TTerm.wstHas_Annotation() : Boolean;
begin
  Result := ( FAnnotation <> TTerm_AnnotationArray(0) );
end;

function TTerm.wstHas_BaseTerm() : Boolean;
begin
  Result := ( FBaseTerm <> '' );
end;

function TTerm.wstHas_Nullable() : Boolean;
begin
  Result := ( FNullable <> boolean(0) );
end;

function TTerm.wstHas_DefaultValue() : Boolean;
begin
  Result := ( FDefaultValue <> '' );
end;

function TTerm.wstHas_AppliesTo() : Boolean;
begin
  Result := ( FAppliesTo <> '' );
end;

function TTerm.wstHas_MaxLength() : Boolean;
begin
  Result := ( FMaxLength <> '' );
end;

function TTerm.wstHas_Precision() : Boolean;
begin
  Result := ( FPrecision <> TPrecisionFacet(0) );
end;

function TTerm.wstHas_Scale() : Boolean;
begin
  Result := ( FScale <> '' );
end;

function TTerm.wstHas_SRID() : Boolean;
begin
  Result := ( FSRID <> '' );
end;

{ TAnnotations }

constructor TAnnotations.Create();
begin
  inherited Create();
  FAnnotation := TAnnotations_AnnotationArray.Create();
end;

procedure TAnnotations.FreeObjectProperties();
begin
  if Assigned(FAnnotation) then
    FreeAndNil(FAnnotation);
  inherited FreeObjectProperties();
end;

function TAnnotations.wstHas_Qualifier() : Boolean;
begin
  Result := ( FQualifier <> '' );
end;

{ GExpression }

constructor GExpression.Create();
begin
  inherited Create();
  FNull := TNullExpression.Create();
end;

procedure GExpression.FreeObjectProperties();
begin
  if Assigned(FBinary) then
    FreeAndNil(FBinary);
  if Assigned(FBool) then
    FreeAndNil(FBool);
  if Assigned(FDate) then
    FreeAndNil(FDate);
  if Assigned(FDateTimeOffset) then
    FreeAndNil(FDateTimeOffset);
  if Assigned(FDecimal) then
    FreeAndNil(FDecimal);
  if Assigned(FDuration) then
    FreeAndNil(FDuration);
  if Assigned(FFloat) then
    FreeAndNil(FFloat);
  if Assigned(FGuid) then
    FreeAndNil(FGuid);
  if Assigned(FInt) then
    FreeAndNil(FInt);
  if Assigned(F_String) then
    FreeAndNil(F_String);
  if Assigned(FTimeOfDay) then
    FreeAndNil(FTimeOfDay);
  if Assigned(FAnnotationPath) then
    FreeAndNil(FAnnotationPath);
  if Assigned(FApply) then
    FreeAndNil(FApply);
  if Assigned(FCast) then
    FreeAndNil(FCast);
  if Assigned(FCollection) then
    FreeAndNil(FCollection);
  if Assigned(F_If) then
    FreeAndNil(F_If);
  if Assigned(FEq) then
    FreeAndNil(FEq);
  if Assigned(FNe) then
    FreeAndNil(FNe);
  if Assigned(FGe) then
    FreeAndNil(FGe);
  if Assigned(FGt) then
    FreeAndNil(FGt);
  if Assigned(FLe) then
    FreeAndNil(FLe);
  if Assigned(FLt) then
    FreeAndNil(FLt);
  if Assigned(F_And) then
    FreeAndNil(F_And);
  if Assigned(F_Or) then
    FreeAndNil(F_Or);
  if Assigned(F_Not) then
    FreeAndNil(F_Not);
  if Assigned(FIsOf) then
    FreeAndNil(FIsOf);
  if Assigned(FLabeledElement) then
    FreeAndNil(FLabeledElement);
  if Assigned(FLabeledElementReference) then
    FreeAndNil(FLabeledElementReference);
  if Assigned(FNull) then
    FreeAndNil(FNull);
  if Assigned(FNavigationPropertyPath) then
    FreeAndNil(FNavigationPropertyPath);
  if Assigned(FPath) then
    FreeAndNil(FPath);
  if Assigned(FPropertyPath) then
    FreeAndNil(FPropertyPath);
  if Assigned(F_Record) then
    FreeAndNil(F_Record);
  if Assigned(FUrlRef) then
    FreeAndNil(FUrlRef);
  inherited FreeObjectProperties();
end;

function GExpression.wstHas_Binary() : Boolean;
begin
  Result := ( FBinary <> nil );
end;

function GExpression.wstHas_Bool() : Boolean;
begin
  Result := ( FBool <> nil );
end;

function GExpression.wstHas_Date() : Boolean;
begin
  Result := ( FDate <> nil );
end;

function GExpression.wstHas_DateTimeOffset() : Boolean;
begin
  Result := ( FDateTimeOffset <> nil );
end;

function GExpression.wstHas_Decimal() : Boolean;
begin
  Result := ( FDecimal <> nil );
end;

function GExpression.wstHas_Duration() : Boolean;
begin
  Result := ( FDuration <> nil );
end;

function GExpression.wstHas_EnumMember() : Boolean;
begin
  Result := ( FEnumMember <> '' );
end;

function GExpression.wstHas_Float() : Boolean;
begin
  Result := ( FFloat <> nil );
end;

function GExpression.wstHas_Guid() : Boolean;
begin
  Result := ( FGuid <> nil );
end;

function GExpression.wstHas_Int() : Boolean;
begin
  Result := ( FInt <> nil );
end;

function GExpression.wstHas__String() : Boolean;
begin
  Result := ( F_String <> nil );
end;

function GExpression.wstHas_TimeOfDay() : Boolean;
begin
  Result := ( FTimeOfDay <> nil );
end;

function GExpression.wstHas_AnnotationPath() : Boolean;
begin
  Result := ( FAnnotationPath <> nil );
end;

function GExpression.wstHas_Apply() : Boolean;
begin
  Result := ( FApply <> nil );
end;

function GExpression.wstHas_Cast() : Boolean;
begin
  Result := ( FCast <> nil );
end;

function GExpression.wstHas_Collection() : Boolean;
begin
  Result := ( FCollection <> nil );
end;

function GExpression.wstHas__If() : Boolean;
begin
  Result := ( F_If <> nil );
end;

function GExpression.wstHas_Eq() : Boolean;
begin
  Result := ( FEq <> nil );
end;

function GExpression.wstHas_Ne() : Boolean;
begin
  Result := ( FNe <> nil );
end;

function GExpression.wstHas_Ge() : Boolean;
begin
  Result := ( FGe <> nil );
end;

function GExpression.wstHas_Gt() : Boolean;
begin
  Result := ( FGt <> nil );
end;

function GExpression.wstHas_Le() : Boolean;
begin
  Result := ( FLe <> nil );
end;

function GExpression.wstHas_Lt() : Boolean;
begin
  Result := ( FLt <> nil );
end;

function GExpression.wstHas__And() : Boolean;
begin
  Result := ( F_And <> nil );
end;

function GExpression.wstHas__Or() : Boolean;
begin
  Result := ( F_Or <> nil );
end;

function GExpression.wstHas__Not() : Boolean;
begin
  Result := ( F_Not <> nil );
end;

function GExpression.wstHas_IsOf() : Boolean;
begin
  Result := ( FIsOf <> nil );
end;

function GExpression.wstHas_LabeledElement() : Boolean;
begin
  Result := ( FLabeledElement <> nil );
end;

function GExpression.wstHas_LabeledElementReference() : Boolean;
begin
  Result := ( FLabeledElementReference <> nil );
end;

function GExpression.wstHas_Null() : Boolean;
begin
  Result := ( FNull <> TNullExpression(0) );
end;

function GExpression.wstHas_NavigationPropertyPath() : Boolean;
begin
  Result := ( FNavigationPropertyPath <> nil );
end;

function GExpression.wstHas_Path() : Boolean;
begin
  Result := ( FPath <> nil );
end;

function GExpression.wstHas_PropertyPath() : Boolean;
begin
  Result := ( FPropertyPath <> nil );
end;

function GExpression.wstHas__Record() : Boolean;
begin
  Result := ( F_Record <> nil );
end;

function GExpression.wstHas_UrlRef() : Boolean;
begin
  Result := ( FUrlRef <> nil );
end;

{ GInlineExpressions }

constructor GInlineExpressions.Create();
begin
  inherited Create();
  FDate := TDateRemotable.Create();
  FDateTimeOffset := dateTimeStamp.Create();
  FDuration := dayTimeDuration.Create();
  FTimeOfDay := TTimeRemotable.Create();
end;

procedure GInlineExpressions.FreeObjectProperties();
begin
  if Assigned(FDate) then
    FreeAndNil(FDate);
  if Assigned(FDateTimeOffset) then
    FreeAndNil(FDateTimeOffset);
  if Assigned(FDuration) then
    FreeAndNil(FDuration);
  if Assigned(FTimeOfDay) then
    FreeAndNil(FTimeOfDay);
  inherited FreeObjectProperties();
end;

function GInlineExpressions.wstHas_Binary() : Boolean;
begin
  Result := ( FBinary <> '' );
end;

function GInlineExpressions.wstHas_Bool() : Boolean;
begin
  Result := ( FBool <> boolean(0) );
end;

function GInlineExpressions.wstHas_Date() : Boolean;
begin
  Result := ( FDate <> nil );
end;

function GInlineExpressions.wstHas_DateTimeOffset() : Boolean;
begin
  Result := ( FDateTimeOffset <> nil );
end;

function GInlineExpressions.wstHas_Decimal() : Boolean;
begin
  Result := ( FDecimal <> 0 );
end;

function GInlineExpressions.wstHas_Duration() : Boolean;
begin
  Result := ( FDuration <> nil );
end;

function GInlineExpressions.wstHas_EnumMember() : Boolean;
begin
  Result := ( FEnumMember <> '' );
end;

function GInlineExpressions.wstHas_Float() : Boolean;
begin
  Result := ( FFloat <> 0 );
end;

function GInlineExpressions.wstHas_Guid() : Boolean;
begin
  Result := ( FGuid <> '' );
end;

function GInlineExpressions.wstHas_Int() : Boolean;
begin
  Result := ( FInt <> integer(0) );
end;

function GInlineExpressions.wstHas__String() : Boolean;
begin
  Result := ( F_String <> '' );
end;

function GInlineExpressions.wstHas_TimeOfDay() : Boolean;
begin
  Result := ( FTimeOfDay <> nil );
end;

function GInlineExpressions.wstHas_AnnotationPath() : Boolean;
begin
  Result := ( FAnnotationPath <> '' );
end;

function GInlineExpressions.wstHas_NavigationPropertyPath() : Boolean;
begin
  Result := ( FNavigationPropertyPath <> '' );
end;

function GInlineExpressions.wstHas_Path() : Boolean;
begin
  Result := ( FPath <> '' );
end;

function GInlineExpressions.wstHas_PropertyPath() : Boolean;
begin
  Result := ( FPropertyPath <> '' );
end;

function GInlineExpressions.wstHas_UrlRef() : Boolean;
begin
  Result := ( FUrlRef <> '' );
end;

{ Annotation_Type }

constructor Annotation_Type.Create();
begin
  inherited Create();
  FAnnotation := Annotation_AnnotationArray.Create();
  FNull := TNullExpression.Create();
  FDateAtt := TDateRemotable.Create();
  FDateTimeOffsetAtt := dateTimeStamp.Create();
  FDurationAtt := dayTimeDuration.Create();
  FTimeOfDayAtt := TTimeRemotable.Create();
end;

procedure Annotation_Type.FreeObjectProperties();
begin
  if Assigned(FAnnotation) then
    FreeAndNil(FAnnotation);
  if Assigned(FBinary) then
    FreeAndNil(FBinary);
  if Assigned(FBool) then
    FreeAndNil(FBool);
  if Assigned(FDate) then
    FreeAndNil(FDate);
  if Assigned(FDateTimeOffset) then
    FreeAndNil(FDateTimeOffset);
  if Assigned(FDecimal) then
    FreeAndNil(FDecimal);
  if Assigned(FDuration) then
    FreeAndNil(FDuration);
  if Assigned(FFloat) then
    FreeAndNil(FFloat);
  if Assigned(FGuid) then
    FreeAndNil(FGuid);
  if Assigned(FInt) then
    FreeAndNil(FInt);
  if Assigned(F_String) then
    FreeAndNil(F_String);
  if Assigned(FTimeOfDay) then
    FreeAndNil(FTimeOfDay);
  if Assigned(FAnnotationPath) then
    FreeAndNil(FAnnotationPath);
  if Assigned(FApply) then
    FreeAndNil(FApply);
  if Assigned(FCast) then
    FreeAndNil(FCast);
  if Assigned(FCollection) then
    FreeAndNil(FCollection);
  if Assigned(F_If) then
    FreeAndNil(F_If);
  if Assigned(FEq) then
    FreeAndNil(FEq);
  if Assigned(FNe) then
    FreeAndNil(FNe);
  if Assigned(FGe) then
    FreeAndNil(FGe);
  if Assigned(FGt) then
    FreeAndNil(FGt);
  if Assigned(FLe) then
    FreeAndNil(FLe);
  if Assigned(FLt) then
    FreeAndNil(FLt);
  if Assigned(F_And) then
    FreeAndNil(F_And);
  if Assigned(F_Or) then
    FreeAndNil(F_Or);
  if Assigned(F_Not) then
    FreeAndNil(F_Not);
  if Assigned(FIsOf) then
    FreeAndNil(FIsOf);
  if Assigned(FLabeledElement) then
    FreeAndNil(FLabeledElement);
  if Assigned(FLabeledElementReference) then
    FreeAndNil(FLabeledElementReference);
  if Assigned(FNull) then
    FreeAndNil(FNull);
  if Assigned(FNavigationPropertyPath) then
    FreeAndNil(FNavigationPropertyPath);
  if Assigned(FPath) then
    FreeAndNil(FPath);
  if Assigned(FPropertyPath) then
    FreeAndNil(FPropertyPath);
  if Assigned(F_Record) then
    FreeAndNil(F_Record);
  if Assigned(FUrlRef) then
    FreeAndNil(FUrlRef);
  if Assigned(FDateAtt) then
    FreeAndNil(FDateAtt);
  if Assigned(FDateTimeOffsetAtt) then
    FreeAndNil(FDateTimeOffsetAtt);
  if Assigned(FDurationAtt) then
    FreeAndNil(FDurationAtt);
  if Assigned(FTimeOfDayAtt) then
    FreeAndNil(FTimeOfDayAtt);
  inherited FreeObjectProperties();
end;

function Annotation_Type.wstHas_Annotation() : Boolean;
begin
  Result := ( FAnnotation <> Annotation_AnnotationArray(0) );
end;

function Annotation_Type.wstHas_Qualifier() : Boolean;
begin
  Result := ( FQualifier <> '' );
end;

function Annotation_Type.wstHas_Binary() : Boolean;
begin
  Result := ( FBinary <> nil );
end;

function Annotation_Type.wstHas_Bool() : Boolean;
begin
  Result := ( FBool <> nil );
end;

function Annotation_Type.wstHas_Date() : Boolean;
begin
  Result := ( FDate <> nil );
end;

function Annotation_Type.wstHas_DateTimeOffset() : Boolean;
begin
  Result := ( FDateTimeOffset <> nil );
end;

function Annotation_Type.wstHas_Decimal() : Boolean;
begin
  Result := ( FDecimal <> nil );
end;

function Annotation_Type.wstHas_Duration() : Boolean;
begin
  Result := ( FDuration <> nil );
end;

function Annotation_Type.wstHas_EnumMember() : Boolean;
begin
  Result := ( FEnumMember <> '' );
end;

function Annotation_Type.wstHas_Float() : Boolean;
begin
  Result := ( FFloat <> nil );
end;

function Annotation_Type.wstHas_Guid() : Boolean;
begin
  Result := ( FGuid <> nil );
end;

function Annotation_Type.wstHas_Int() : Boolean;
begin
  Result := ( FInt <> nil );
end;

function Annotation_Type.wstHas__String() : Boolean;
begin
  Result := ( F_String <> nil );
end;

function Annotation_Type.wstHas_TimeOfDay() : Boolean;
begin
  Result := ( FTimeOfDay <> nil );
end;

function Annotation_Type.wstHas_AnnotationPath() : Boolean;
begin
  Result := ( FAnnotationPath <> nil );
end;

function Annotation_Type.wstHas_Apply() : Boolean;
begin
  Result := ( FApply <> nil );
end;

function Annotation_Type.wstHas_Cast() : Boolean;
begin
  Result := ( FCast <> nil );
end;

function Annotation_Type.wstHas_Collection() : Boolean;
begin
  Result := ( FCollection <> nil );
end;

function Annotation_Type.wstHas__If() : Boolean;
begin
  Result := ( F_If <> nil );
end;

function Annotation_Type.wstHas_Eq() : Boolean;
begin
  Result := ( FEq <> nil );
end;

function Annotation_Type.wstHas_Ne() : Boolean;
begin
  Result := ( FNe <> nil );
end;

function Annotation_Type.wstHas_Ge() : Boolean;
begin
  Result := ( FGe <> nil );
end;

function Annotation_Type.wstHas_Gt() : Boolean;
begin
  Result := ( FGt <> nil );
end;

function Annotation_Type.wstHas_Le() : Boolean;
begin
  Result := ( FLe <> nil );
end;

function Annotation_Type.wstHas_Lt() : Boolean;
begin
  Result := ( FLt <> nil );
end;

function Annotation_Type.wstHas__And() : Boolean;
begin
  Result := ( F_And <> nil );
end;

function Annotation_Type.wstHas__Or() : Boolean;
begin
  Result := ( F_Or <> nil );
end;

function Annotation_Type.wstHas__Not() : Boolean;
begin
  Result := ( F_Not <> nil );
end;

function Annotation_Type.wstHas_IsOf() : Boolean;
begin
  Result := ( FIsOf <> nil );
end;

function Annotation_Type.wstHas_LabeledElement() : Boolean;
begin
  Result := ( FLabeledElement <> nil );
end;

function Annotation_Type.wstHas_LabeledElementReference() : Boolean;
begin
  Result := ( FLabeledElementReference <> nil );
end;

function Annotation_Type.wstHas_Null() : Boolean;
begin
  Result := ( FNull <> TNullExpression(0) );
end;

function Annotation_Type.wstHas_NavigationPropertyPath() : Boolean;
begin
  Result := ( FNavigationPropertyPath <> nil );
end;

function Annotation_Type.wstHas_Path() : Boolean;
begin
  Result := ( FPath <> nil );
end;

function Annotation_Type.wstHas_PropertyPath() : Boolean;
begin
  Result := ( FPropertyPath <> nil );
end;

function Annotation_Type.wstHas__Record() : Boolean;
begin
  Result := ( F_Record <> nil );
end;

function Annotation_Type.wstHas_UrlRef() : Boolean;
begin
  Result := ( FUrlRef <> nil );
end;

function Annotation_Type.wstHas_BinaryAtt() : Boolean;
begin
  Result := ( FBinaryAtt <> '' );
end;

function Annotation_Type.wstHas_BoolAtt() : Boolean;
begin
  Result := ( FBoolAtt <> boolean(0) );
end;

function Annotation_Type.wstHas_DateAtt() : Boolean;
begin
  Result := ( FDateAtt <> nil );
end;

function Annotation_Type.wstHas_DateTimeOffsetAtt() : Boolean;
begin
  Result := ( FDateTimeOffsetAtt <> nil );
end;

function Annotation_Type.wstHas_DecimalAtt() : Boolean;
begin
  Result := ( FDecimalAtt <> 0 );
end;

function Annotation_Type.wstHas_DurationAtt() : Boolean;
begin
  Result := ( FDurationAtt <> nil );
end;

function Annotation_Type.wstHas_EnumMemberAtt() : Boolean;
begin
  Result := ( FEnumMemberAtt <> '' );
end;

function Annotation_Type.wstHas_FloatAtt() : Boolean;
begin
  Result := ( FFloatAtt <> 0 );
end;

function Annotation_Type.wstHas_GuidAtt() : Boolean;
begin
  Result := ( FGuidAtt <> '' );
end;

function Annotation_Type.wstHas_IntAtt() : Boolean;
begin
  Result := ( FIntAtt <> integer(0) );
end;

function Annotation_Type.wstHas__StringAtt() : Boolean;
begin
  Result := ( F_StringAtt <> '' );
end;

function Annotation_Type.wstHas_TimeOfDayAtt() : Boolean;
begin
  Result := ( FTimeOfDayAtt <> nil );
end;

function Annotation_Type.wstHas_AnnotationPathAtt() : Boolean;
begin
  Result := ( FAnnotationPathAtt <> '' );
end;

function Annotation_Type.wstHas_NavigationPropertyPathAtt() : Boolean;
begin
  Result := ( FNavigationPropertyPathAtt <> '' );
end;

function Annotation_Type.wstHas_PathAtt() : Boolean;
begin
  Result := ( FPathAtt <> '' );
end;

function Annotation_Type.wstHas_PropertyPathAtt() : Boolean;
begin
  Result := ( FPropertyPathAtt <> '' );
end;

function Annotation_Type.wstHas_UrlRefAtt() : Boolean;
begin
  Result := ( FUrlRefAtt <> '' );
end;

{ TApplyExpression }

constructor TApplyExpression.Create();
begin
  inherited Create();
  FAnnotation := TApplyExpression_AnnotationArray.Create();
  FNull := TNullExpression.Create();
end;

procedure TApplyExpression.FreeObjectProperties();
begin
  if Assigned(FAnnotation) then
    FreeAndNil(FAnnotation);
  if Assigned(FBinary) then
    FreeAndNil(FBinary);
  if Assigned(FBool) then
    FreeAndNil(FBool);
  if Assigned(FDate) then
    FreeAndNil(FDate);
  if Assigned(FDateTimeOffset) then
    FreeAndNil(FDateTimeOffset);
  if Assigned(FDecimal) then
    FreeAndNil(FDecimal);
  if Assigned(FDuration) then
    FreeAndNil(FDuration);
  if Assigned(FFloat) then
    FreeAndNil(FFloat);
  if Assigned(FGuid) then
    FreeAndNil(FGuid);
  if Assigned(FInt) then
    FreeAndNil(FInt);
  if Assigned(F_String) then
    FreeAndNil(F_String);
  if Assigned(FTimeOfDay) then
    FreeAndNil(FTimeOfDay);
  if Assigned(FAnnotationPath) then
    FreeAndNil(FAnnotationPath);
  if Assigned(FApply) then
    FreeAndNil(FApply);
  if Assigned(FCast) then
    FreeAndNil(FCast);
  if Assigned(FCollection) then
    FreeAndNil(FCollection);
  if Assigned(F_If) then
    FreeAndNil(F_If);
  if Assigned(FEq) then
    FreeAndNil(FEq);
  if Assigned(FNe) then
    FreeAndNil(FNe);
  if Assigned(FGe) then
    FreeAndNil(FGe);
  if Assigned(FGt) then
    FreeAndNil(FGt);
  if Assigned(FLe) then
    FreeAndNil(FLe);
  if Assigned(FLt) then
    FreeAndNil(FLt);
  if Assigned(F_And) then
    FreeAndNil(F_And);
  if Assigned(F_Or) then
    FreeAndNil(F_Or);
  if Assigned(F_Not) then
    FreeAndNil(F_Not);
  if Assigned(FIsOf) then
    FreeAndNil(FIsOf);
  if Assigned(FLabeledElement) then
    FreeAndNil(FLabeledElement);
  if Assigned(FLabeledElementReference) then
    FreeAndNil(FLabeledElementReference);
  if Assigned(FNull) then
    FreeAndNil(FNull);
  if Assigned(FNavigationPropertyPath) then
    FreeAndNil(FNavigationPropertyPath);
  if Assigned(FPath) then
    FreeAndNil(FPath);
  if Assigned(FPropertyPath) then
    FreeAndNil(FPropertyPath);
  if Assigned(F_Record) then
    FreeAndNil(F_Record);
  if Assigned(FUrlRef) then
    FreeAndNil(FUrlRef);
  inherited FreeObjectProperties();
end;

function TApplyExpression.wstHas_Annotation() : Boolean;
begin
  Result := ( FAnnotation <> TApplyExpression_AnnotationArray(0) );
end;

function TApplyExpression.wstHas__Function() : Boolean;
begin
  Result := ( F_Function <> '' );
end;

function TApplyExpression.wstHas_Binary() : Boolean;
begin
  Result := ( FBinary <> nil );
end;

function TApplyExpression.wstHas_Bool() : Boolean;
begin
  Result := ( FBool <> nil );
end;

function TApplyExpression.wstHas_Date() : Boolean;
begin
  Result := ( FDate <> nil );
end;

function TApplyExpression.wstHas_DateTimeOffset() : Boolean;
begin
  Result := ( FDateTimeOffset <> nil );
end;

function TApplyExpression.wstHas_Decimal() : Boolean;
begin
  Result := ( FDecimal <> nil );
end;

function TApplyExpression.wstHas_Duration() : Boolean;
begin
  Result := ( FDuration <> nil );
end;

function TApplyExpression.wstHas_EnumMember() : Boolean;
begin
  Result := ( FEnumMember <> '' );
end;

function TApplyExpression.wstHas_Float() : Boolean;
begin
  Result := ( FFloat <> nil );
end;

function TApplyExpression.wstHas_Guid() : Boolean;
begin
  Result := ( FGuid <> nil );
end;

function TApplyExpression.wstHas_Int() : Boolean;
begin
  Result := ( FInt <> nil );
end;

function TApplyExpression.wstHas__String() : Boolean;
begin
  Result := ( F_String <> nil );
end;

function TApplyExpression.wstHas_TimeOfDay() : Boolean;
begin
  Result := ( FTimeOfDay <> nil );
end;

function TApplyExpression.wstHas_AnnotationPath() : Boolean;
begin
  Result := ( FAnnotationPath <> nil );
end;

function TApplyExpression.wstHas_Apply() : Boolean;
begin
  Result := ( FApply <> nil );
end;

function TApplyExpression.wstHas_Cast() : Boolean;
begin
  Result := ( FCast <> nil );
end;

function TApplyExpression.wstHas_Collection() : Boolean;
begin
  Result := ( FCollection <> nil );
end;

function TApplyExpression.wstHas__If() : Boolean;
begin
  Result := ( F_If <> nil );
end;

function TApplyExpression.wstHas_Eq() : Boolean;
begin
  Result := ( FEq <> nil );
end;

function TApplyExpression.wstHas_Ne() : Boolean;
begin
  Result := ( FNe <> nil );
end;

function TApplyExpression.wstHas_Ge() : Boolean;
begin
  Result := ( FGe <> nil );
end;

function TApplyExpression.wstHas_Gt() : Boolean;
begin
  Result := ( FGt <> nil );
end;

function TApplyExpression.wstHas_Le() : Boolean;
begin
  Result := ( FLe <> nil );
end;

function TApplyExpression.wstHas_Lt() : Boolean;
begin
  Result := ( FLt <> nil );
end;

function TApplyExpression.wstHas__And() : Boolean;
begin
  Result := ( F_And <> nil );
end;

function TApplyExpression.wstHas__Or() : Boolean;
begin
  Result := ( F_Or <> nil );
end;

function TApplyExpression.wstHas__Not() : Boolean;
begin
  Result := ( F_Not <> nil );
end;

function TApplyExpression.wstHas_IsOf() : Boolean;
begin
  Result := ( FIsOf <> nil );
end;

function TApplyExpression.wstHas_LabeledElement() : Boolean;
begin
  Result := ( FLabeledElement <> nil );
end;

function TApplyExpression.wstHas_LabeledElementReference() : Boolean;
begin
  Result := ( FLabeledElementReference <> nil );
end;

function TApplyExpression.wstHas_Null() : Boolean;
begin
  Result := ( FNull <> TNullExpression(0) );
end;

function TApplyExpression.wstHas_NavigationPropertyPath() : Boolean;
begin
  Result := ( FNavigationPropertyPath <> nil );
end;

function TApplyExpression.wstHas_Path() : Boolean;
begin
  Result := ( FPath <> nil );
end;

function TApplyExpression.wstHas_PropertyPath() : Boolean;
begin
  Result := ( FPropertyPath <> nil );
end;

function TApplyExpression.wstHas__Record() : Boolean;
begin
  Result := ( F_Record <> nil );
end;

function TApplyExpression.wstHas_UrlRef() : Boolean;
begin
  Result := ( FUrlRef <> nil );
end;

{ TCastOrIsOfExpression }

constructor TCastOrIsOfExpression.Create();
begin
  inherited Create();
  FAnnotation := TCastOrIsOfExpression_AnnotationArray.Create();
  FNull := TNullExpression.Create();
end;

procedure TCastOrIsOfExpression.FreeObjectProperties();
begin
  if Assigned(FAnnotation) then
    FreeAndNil(FAnnotation);
  if Assigned(FBinary) then
    FreeAndNil(FBinary);
  if Assigned(FBool) then
    FreeAndNil(FBool);
  if Assigned(FDate) then
    FreeAndNil(FDate);
  if Assigned(FDateTimeOffset) then
    FreeAndNil(FDateTimeOffset);
  if Assigned(FDecimal) then
    FreeAndNil(FDecimal);
  if Assigned(FDuration) then
    FreeAndNil(FDuration);
  if Assigned(FFloat) then
    FreeAndNil(FFloat);
  if Assigned(FGuid) then
    FreeAndNil(FGuid);
  if Assigned(FInt) then
    FreeAndNil(FInt);
  if Assigned(F_String) then
    FreeAndNil(F_String);
  if Assigned(FTimeOfDay) then
    FreeAndNil(FTimeOfDay);
  if Assigned(FAnnotationPath) then
    FreeAndNil(FAnnotationPath);
  if Assigned(FApply) then
    FreeAndNil(FApply);
  if Assigned(FCast) then
    FreeAndNil(FCast);
  if Assigned(FCollection) then
    FreeAndNil(FCollection);
  if Assigned(F_If) then
    FreeAndNil(F_If);
  if Assigned(FEq) then
    FreeAndNil(FEq);
  if Assigned(FNe) then
    FreeAndNil(FNe);
  if Assigned(FGe) then
    FreeAndNil(FGe);
  if Assigned(FGt) then
    FreeAndNil(FGt);
  if Assigned(FLe) then
    FreeAndNil(FLe);
  if Assigned(FLt) then
    FreeAndNil(FLt);
  if Assigned(F_And) then
    FreeAndNil(F_And);
  if Assigned(F_Or) then
    FreeAndNil(F_Or);
  if Assigned(F_Not) then
    FreeAndNil(F_Not);
  if Assigned(FIsOf) then
    FreeAndNil(FIsOf);
  if Assigned(FLabeledElement) then
    FreeAndNil(FLabeledElement);
  if Assigned(FLabeledElementReference) then
    FreeAndNil(FLabeledElementReference);
  if Assigned(FNull) then
    FreeAndNil(FNull);
  if Assigned(FNavigationPropertyPath) then
    FreeAndNil(FNavigationPropertyPath);
  if Assigned(FPath) then
    FreeAndNil(FPath);
  if Assigned(FPropertyPath) then
    FreeAndNil(FPropertyPath);
  if Assigned(F_Record) then
    FreeAndNil(F_Record);
  if Assigned(FUrlRef) then
    FreeAndNil(FUrlRef);
  inherited FreeObjectProperties();
end;

function TCastOrIsOfExpression.wstHas_Annotation() : Boolean;
begin
  Result := ( FAnnotation <> TCastOrIsOfExpression_AnnotationArray(0) );
end;

function TCastOrIsOfExpression.wstHas__Type() : Boolean;
begin
  Result := ( F_Type <> '' );
end;

function TCastOrIsOfExpression.wstHas_Binary() : Boolean;
begin
  Result := ( FBinary <> nil );
end;

function TCastOrIsOfExpression.wstHas_Bool() : Boolean;
begin
  Result := ( FBool <> nil );
end;

function TCastOrIsOfExpression.wstHas_Date() : Boolean;
begin
  Result := ( FDate <> nil );
end;

function TCastOrIsOfExpression.wstHas_DateTimeOffset() : Boolean;
begin
  Result := ( FDateTimeOffset <> nil );
end;

function TCastOrIsOfExpression.wstHas_Decimal() : Boolean;
begin
  Result := ( FDecimal <> nil );
end;

function TCastOrIsOfExpression.wstHas_Duration() : Boolean;
begin
  Result := ( FDuration <> nil );
end;

function TCastOrIsOfExpression.wstHas_EnumMember() : Boolean;
begin
  Result := ( FEnumMember <> '' );
end;

function TCastOrIsOfExpression.wstHas_Float() : Boolean;
begin
  Result := ( FFloat <> nil );
end;

function TCastOrIsOfExpression.wstHas_Guid() : Boolean;
begin
  Result := ( FGuid <> nil );
end;

function TCastOrIsOfExpression.wstHas_Int() : Boolean;
begin
  Result := ( FInt <> nil );
end;

function TCastOrIsOfExpression.wstHas__String() : Boolean;
begin
  Result := ( F_String <> nil );
end;

function TCastOrIsOfExpression.wstHas_TimeOfDay() : Boolean;
begin
  Result := ( FTimeOfDay <> nil );
end;

function TCastOrIsOfExpression.wstHas_AnnotationPath() : Boolean;
begin
  Result := ( FAnnotationPath <> nil );
end;

function TCastOrIsOfExpression.wstHas_Apply() : Boolean;
begin
  Result := ( FApply <> nil );
end;

function TCastOrIsOfExpression.wstHas_Cast() : Boolean;
begin
  Result := ( FCast <> nil );
end;

function TCastOrIsOfExpression.wstHas_Collection() : Boolean;
begin
  Result := ( FCollection <> nil );
end;

function TCastOrIsOfExpression.wstHas__If() : Boolean;
begin
  Result := ( F_If <> nil );
end;

function TCastOrIsOfExpression.wstHas_Eq() : Boolean;
begin
  Result := ( FEq <> nil );
end;

function TCastOrIsOfExpression.wstHas_Ne() : Boolean;
begin
  Result := ( FNe <> nil );
end;

function TCastOrIsOfExpression.wstHas_Ge() : Boolean;
begin
  Result := ( FGe <> nil );
end;

function TCastOrIsOfExpression.wstHas_Gt() : Boolean;
begin
  Result := ( FGt <> nil );
end;

function TCastOrIsOfExpression.wstHas_Le() : Boolean;
begin
  Result := ( FLe <> nil );
end;

function TCastOrIsOfExpression.wstHas_Lt() : Boolean;
begin
  Result := ( FLt <> nil );
end;

function TCastOrIsOfExpression.wstHas__And() : Boolean;
begin
  Result := ( F_And <> nil );
end;

function TCastOrIsOfExpression.wstHas__Or() : Boolean;
begin
  Result := ( F_Or <> nil );
end;

function TCastOrIsOfExpression.wstHas__Not() : Boolean;
begin
  Result := ( F_Not <> nil );
end;

function TCastOrIsOfExpression.wstHas_IsOf() : Boolean;
begin
  Result := ( FIsOf <> nil );
end;

function TCastOrIsOfExpression.wstHas_LabeledElement() : Boolean;
begin
  Result := ( FLabeledElement <> nil );
end;

function TCastOrIsOfExpression.wstHas_LabeledElementReference() : Boolean;
begin
  Result := ( FLabeledElementReference <> nil );
end;

function TCastOrIsOfExpression.wstHas_Null() : Boolean;
begin
  Result := ( FNull <> TNullExpression(0) );
end;

function TCastOrIsOfExpression.wstHas_NavigationPropertyPath() : Boolean;
begin
  Result := ( FNavigationPropertyPath <> nil );
end;

function TCastOrIsOfExpression.wstHas_Path() : Boolean;
begin
  Result := ( FPath <> nil );
end;

function TCastOrIsOfExpression.wstHas_PropertyPath() : Boolean;
begin
  Result := ( FPropertyPath <> nil );
end;

function TCastOrIsOfExpression.wstHas__Record() : Boolean;
begin
  Result := ( F_Record <> nil );
end;

function TCastOrIsOfExpression.wstHas_UrlRef() : Boolean;
begin
  Result := ( FUrlRef <> nil );
end;

function TCastOrIsOfExpression.wstHas_MaxLength() : Boolean;
begin
  Result := ( FMaxLength <> '' );
end;

function TCastOrIsOfExpression.wstHas_Precision() : Boolean;
begin
  Result := ( FPrecision <> TPrecisionFacet(0) );
end;

function TCastOrIsOfExpression.wstHas_Scale() : Boolean;
begin
  Result := ( FScale <> '' );
end;

function TCastOrIsOfExpression.wstHas_SRID() : Boolean;
begin
  Result := ( FSRID <> '' );
end;

{ TCollectionExpression }

constructor TCollectionExpression.Create();
begin
  inherited Create();
  FNull := TNullExpression.Create();
end;

procedure TCollectionExpression.FreeObjectProperties();
begin
  if Assigned(FBinary) then
    FreeAndNil(FBinary);
  if Assigned(FBool) then
    FreeAndNil(FBool);
  if Assigned(FDate) then
    FreeAndNil(FDate);
  if Assigned(FDateTimeOffset) then
    FreeAndNil(FDateTimeOffset);
  if Assigned(FDecimal) then
    FreeAndNil(FDecimal);
  if Assigned(FDuration) then
    FreeAndNil(FDuration);
  if Assigned(FFloat) then
    FreeAndNil(FFloat);
  if Assigned(FGuid) then
    FreeAndNil(FGuid);
  if Assigned(FInt) then
    FreeAndNil(FInt);
  if Assigned(F_String) then
    FreeAndNil(F_String);
  if Assigned(FTimeOfDay) then
    FreeAndNil(FTimeOfDay);
  if Assigned(FAnnotationPath) then
    FreeAndNil(FAnnotationPath);
  if Assigned(FApply) then
    FreeAndNil(FApply);
  if Assigned(FCast) then
    FreeAndNil(FCast);
  if Assigned(FCollection) then
    FreeAndNil(FCollection);
  if Assigned(F_If) then
    FreeAndNil(F_If);
  if Assigned(FEq) then
    FreeAndNil(FEq);
  if Assigned(FNe) then
    FreeAndNil(FNe);
  if Assigned(FGe) then
    FreeAndNil(FGe);
  if Assigned(FGt) then
    FreeAndNil(FGt);
  if Assigned(FLe) then
    FreeAndNil(FLe);
  if Assigned(FLt) then
    FreeAndNil(FLt);
  if Assigned(F_And) then
    FreeAndNil(F_And);
  if Assigned(F_Or) then
    FreeAndNil(F_Or);
  if Assigned(F_Not) then
    FreeAndNil(F_Not);
  if Assigned(FIsOf) then
    FreeAndNil(FIsOf);
  if Assigned(FLabeledElement) then
    FreeAndNil(FLabeledElement);
  if Assigned(FLabeledElementReference) then
    FreeAndNil(FLabeledElementReference);
  if Assigned(FNull) then
    FreeAndNil(FNull);
  if Assigned(FNavigationPropertyPath) then
    FreeAndNil(FNavigationPropertyPath);
  if Assigned(FPath) then
    FreeAndNil(FPath);
  if Assigned(FPropertyPath) then
    FreeAndNil(FPropertyPath);
  if Assigned(F_Record) then
    FreeAndNil(F_Record);
  if Assigned(FUrlRef) then
    FreeAndNil(FUrlRef);
  inherited FreeObjectProperties();
end;

function TCollectionExpression.wstHas_Binary() : Boolean;
begin
  Result := ( FBinary <> nil );
end;

function TCollectionExpression.wstHas_Bool() : Boolean;
begin
  Result := ( FBool <> nil );
end;

function TCollectionExpression.wstHas_Date() : Boolean;
begin
  Result := ( FDate <> nil );
end;

function TCollectionExpression.wstHas_DateTimeOffset() : Boolean;
begin
  Result := ( FDateTimeOffset <> nil );
end;

function TCollectionExpression.wstHas_Decimal() : Boolean;
begin
  Result := ( FDecimal <> nil );
end;

function TCollectionExpression.wstHas_Duration() : Boolean;
begin
  Result := ( FDuration <> nil );
end;

function TCollectionExpression.wstHas_EnumMember() : Boolean;
begin
  Result := ( FEnumMember <> '' );
end;

function TCollectionExpression.wstHas_Float() : Boolean;
begin
  Result := ( FFloat <> nil );
end;

function TCollectionExpression.wstHas_Guid() : Boolean;
begin
  Result := ( FGuid <> nil );
end;

function TCollectionExpression.wstHas_Int() : Boolean;
begin
  Result := ( FInt <> nil );
end;

function TCollectionExpression.wstHas__String() : Boolean;
begin
  Result := ( F_String <> nil );
end;

function TCollectionExpression.wstHas_TimeOfDay() : Boolean;
begin
  Result := ( FTimeOfDay <> nil );
end;

function TCollectionExpression.wstHas_AnnotationPath() : Boolean;
begin
  Result := ( FAnnotationPath <> nil );
end;

function TCollectionExpression.wstHas_Apply() : Boolean;
begin
  Result := ( FApply <> nil );
end;

function TCollectionExpression.wstHas_Cast() : Boolean;
begin
  Result := ( FCast <> nil );
end;

function TCollectionExpression.wstHas_Collection() : Boolean;
begin
  Result := ( FCollection <> nil );
end;

function TCollectionExpression.wstHas__If() : Boolean;
begin
  Result := ( F_If <> nil );
end;

function TCollectionExpression.wstHas_Eq() : Boolean;
begin
  Result := ( FEq <> nil );
end;

function TCollectionExpression.wstHas_Ne() : Boolean;
begin
  Result := ( FNe <> nil );
end;

function TCollectionExpression.wstHas_Ge() : Boolean;
begin
  Result := ( FGe <> nil );
end;

function TCollectionExpression.wstHas_Gt() : Boolean;
begin
  Result := ( FGt <> nil );
end;

function TCollectionExpression.wstHas_Le() : Boolean;
begin
  Result := ( FLe <> nil );
end;

function TCollectionExpression.wstHas_Lt() : Boolean;
begin
  Result := ( FLt <> nil );
end;

function TCollectionExpression.wstHas__And() : Boolean;
begin
  Result := ( F_And <> nil );
end;

function TCollectionExpression.wstHas__Or() : Boolean;
begin
  Result := ( F_Or <> nil );
end;

function TCollectionExpression.wstHas__Not() : Boolean;
begin
  Result := ( F_Not <> nil );
end;

function TCollectionExpression.wstHas_IsOf() : Boolean;
begin
  Result := ( FIsOf <> nil );
end;

function TCollectionExpression.wstHas_LabeledElement() : Boolean;
begin
  Result := ( FLabeledElement <> nil );
end;

function TCollectionExpression.wstHas_LabeledElementReference() : Boolean;
begin
  Result := ( FLabeledElementReference <> nil );
end;

function TCollectionExpression.wstHas_Null() : Boolean;
begin
  Result := ( FNull <> TNullExpression(0) );
end;

function TCollectionExpression.wstHas_NavigationPropertyPath() : Boolean;
begin
  Result := ( FNavigationPropertyPath <> nil );
end;

function TCollectionExpression.wstHas_Path() : Boolean;
begin
  Result := ( FPath <> nil );
end;

function TCollectionExpression.wstHas_PropertyPath() : Boolean;
begin
  Result := ( FPropertyPath <> nil );
end;

function TCollectionExpression.wstHas__Record() : Boolean;
begin
  Result := ( F_Record <> nil );
end;

function TCollectionExpression.wstHas_UrlRef() : Boolean;
begin
  Result := ( FUrlRef <> nil );
end;

{ TIfExpression }

constructor TIfExpression.Create();
begin
  inherited Create();
  FAnnotation := TIfExpression_AnnotationArray.Create();
  FNull := TNullExpression.Create();
end;

procedure TIfExpression.FreeObjectProperties();
begin
  if Assigned(FAnnotation) then
    FreeAndNil(FAnnotation);
  if Assigned(FBinary) then
    FreeAndNil(FBinary);
  if Assigned(FBool) then
    FreeAndNil(FBool);
  if Assigned(FDate) then
    FreeAndNil(FDate);
  if Assigned(FDateTimeOffset) then
    FreeAndNil(FDateTimeOffset);
  if Assigned(FDecimal) then
    FreeAndNil(FDecimal);
  if Assigned(FDuration) then
    FreeAndNil(FDuration);
  if Assigned(FFloat) then
    FreeAndNil(FFloat);
  if Assigned(FGuid) then
    FreeAndNil(FGuid);
  if Assigned(FInt) then
    FreeAndNil(FInt);
  if Assigned(F_String) then
    FreeAndNil(F_String);
  if Assigned(FTimeOfDay) then
    FreeAndNil(FTimeOfDay);
  if Assigned(FAnnotationPath) then
    FreeAndNil(FAnnotationPath);
  if Assigned(FApply) then
    FreeAndNil(FApply);
  if Assigned(FCast) then
    FreeAndNil(FCast);
  if Assigned(FCollection) then
    FreeAndNil(FCollection);
  if Assigned(F_If) then
    FreeAndNil(F_If);
  if Assigned(FEq) then
    FreeAndNil(FEq);
  if Assigned(FNe) then
    FreeAndNil(FNe);
  if Assigned(FGe) then
    FreeAndNil(FGe);
  if Assigned(FGt) then
    FreeAndNil(FGt);
  if Assigned(FLe) then
    FreeAndNil(FLe);
  if Assigned(FLt) then
    FreeAndNil(FLt);
  if Assigned(F_And) then
    FreeAndNil(F_And);
  if Assigned(F_Or) then
    FreeAndNil(F_Or);
  if Assigned(F_Not) then
    FreeAndNil(F_Not);
  if Assigned(FIsOf) then
    FreeAndNil(FIsOf);
  if Assigned(FLabeledElement) then
    FreeAndNil(FLabeledElement);
  if Assigned(FLabeledElementReference) then
    FreeAndNil(FLabeledElementReference);
  if Assigned(FNull) then
    FreeAndNil(FNull);
  if Assigned(FNavigationPropertyPath) then
    FreeAndNil(FNavigationPropertyPath);
  if Assigned(FPath) then
    FreeAndNil(FPath);
  if Assigned(FPropertyPath) then
    FreeAndNil(FPropertyPath);
  if Assigned(F_Record) then
    FreeAndNil(F_Record);
  if Assigned(FUrlRef) then
    FreeAndNil(FUrlRef);
  inherited FreeObjectProperties();
end;

function TIfExpression.wstHas_Annotation() : Boolean;
begin
  Result := ( FAnnotation <> TIfExpression_AnnotationArray(0) );
end;

function TIfExpression.wstHas_Binary() : Boolean;
begin
  Result := ( FBinary <> nil );
end;

function TIfExpression.wstHas_Bool() : Boolean;
begin
  Result := ( FBool <> nil );
end;

function TIfExpression.wstHas_Date() : Boolean;
begin
  Result := ( FDate <> nil );
end;

function TIfExpression.wstHas_DateTimeOffset() : Boolean;
begin
  Result := ( FDateTimeOffset <> nil );
end;

function TIfExpression.wstHas_Decimal() : Boolean;
begin
  Result := ( FDecimal <> nil );
end;

function TIfExpression.wstHas_Duration() : Boolean;
begin
  Result := ( FDuration <> nil );
end;

function TIfExpression.wstHas_EnumMember() : Boolean;
begin
  Result := ( FEnumMember <> '' );
end;

function TIfExpression.wstHas_Float() : Boolean;
begin
  Result := ( FFloat <> nil );
end;

function TIfExpression.wstHas_Guid() : Boolean;
begin
  Result := ( FGuid <> nil );
end;

function TIfExpression.wstHas_Int() : Boolean;
begin
  Result := ( FInt <> nil );
end;

function TIfExpression.wstHas__String() : Boolean;
begin
  Result := ( F_String <> nil );
end;

function TIfExpression.wstHas_TimeOfDay() : Boolean;
begin
  Result := ( FTimeOfDay <> nil );
end;

function TIfExpression.wstHas_AnnotationPath() : Boolean;
begin
  Result := ( FAnnotationPath <> nil );
end;

function TIfExpression.wstHas_Apply() : Boolean;
begin
  Result := ( FApply <> nil );
end;

function TIfExpression.wstHas_Cast() : Boolean;
begin
  Result := ( FCast <> nil );
end;

function TIfExpression.wstHas_Collection() : Boolean;
begin
  Result := ( FCollection <> nil );
end;

function TIfExpression.wstHas__If() : Boolean;
begin
  Result := ( F_If <> nil );
end;

function TIfExpression.wstHas_Eq() : Boolean;
begin
  Result := ( FEq <> nil );
end;

function TIfExpression.wstHas_Ne() : Boolean;
begin
  Result := ( FNe <> nil );
end;

function TIfExpression.wstHas_Ge() : Boolean;
begin
  Result := ( FGe <> nil );
end;

function TIfExpression.wstHas_Gt() : Boolean;
begin
  Result := ( FGt <> nil );
end;

function TIfExpression.wstHas_Le() : Boolean;
begin
  Result := ( FLe <> nil );
end;

function TIfExpression.wstHas_Lt() : Boolean;
begin
  Result := ( FLt <> nil );
end;

function TIfExpression.wstHas__And() : Boolean;
begin
  Result := ( F_And <> nil );
end;

function TIfExpression.wstHas__Or() : Boolean;
begin
  Result := ( F_Or <> nil );
end;

function TIfExpression.wstHas__Not() : Boolean;
begin
  Result := ( F_Not <> nil );
end;

function TIfExpression.wstHas_IsOf() : Boolean;
begin
  Result := ( FIsOf <> nil );
end;

function TIfExpression.wstHas_LabeledElement() : Boolean;
begin
  Result := ( FLabeledElement <> nil );
end;

function TIfExpression.wstHas_LabeledElementReference() : Boolean;
begin
  Result := ( FLabeledElementReference <> nil );
end;

function TIfExpression.wstHas_Null() : Boolean;
begin
  Result := ( FNull <> TNullExpression(0) );
end;

function TIfExpression.wstHas_NavigationPropertyPath() : Boolean;
begin
  Result := ( FNavigationPropertyPath <> nil );
end;

function TIfExpression.wstHas_Path() : Boolean;
begin
  Result := ( FPath <> nil );
end;

function TIfExpression.wstHas_PropertyPath() : Boolean;
begin
  Result := ( FPropertyPath <> nil );
end;

function TIfExpression.wstHas__Record() : Boolean;
begin
  Result := ( F_Record <> nil );
end;

function TIfExpression.wstHas_UrlRef() : Boolean;
begin
  Result := ( FUrlRef <> nil );
end;

{ TOneChildExpression }

constructor TOneChildExpression.Create();
begin
  inherited Create();
  FAnnotation := TOneChildExpression_AnnotationArray.Create();
  FNull := TNullExpression.Create();
end;

procedure TOneChildExpression.FreeObjectProperties();
begin
  if Assigned(FAnnotation) then
    FreeAndNil(FAnnotation);
  if Assigned(FBinary) then
    FreeAndNil(FBinary);
  if Assigned(FBool) then
    FreeAndNil(FBool);
  if Assigned(FDate) then
    FreeAndNil(FDate);
  if Assigned(FDateTimeOffset) then
    FreeAndNil(FDateTimeOffset);
  if Assigned(FDecimal) then
    FreeAndNil(FDecimal);
  if Assigned(FDuration) then
    FreeAndNil(FDuration);
  if Assigned(FFloat) then
    FreeAndNil(FFloat);
  if Assigned(FGuid) then
    FreeAndNil(FGuid);
  if Assigned(FInt) then
    FreeAndNil(FInt);
  if Assigned(F_String) then
    FreeAndNil(F_String);
  if Assigned(FTimeOfDay) then
    FreeAndNil(FTimeOfDay);
  if Assigned(FAnnotationPath) then
    FreeAndNil(FAnnotationPath);
  if Assigned(FApply) then
    FreeAndNil(FApply);
  if Assigned(FCast) then
    FreeAndNil(FCast);
  if Assigned(FCollection) then
    FreeAndNil(FCollection);
  if Assigned(F_If) then
    FreeAndNil(F_If);
  if Assigned(FEq) then
    FreeAndNil(FEq);
  if Assigned(FNe) then
    FreeAndNil(FNe);
  if Assigned(FGe) then
    FreeAndNil(FGe);
  if Assigned(FGt) then
    FreeAndNil(FGt);
  if Assigned(FLe) then
    FreeAndNil(FLe);
  if Assigned(FLt) then
    FreeAndNil(FLt);
  if Assigned(F_And) then
    FreeAndNil(F_And);
  if Assigned(F_Or) then
    FreeAndNil(F_Or);
  if Assigned(F_Not) then
    FreeAndNil(F_Not);
  if Assigned(FIsOf) then
    FreeAndNil(FIsOf);
  if Assigned(FLabeledElement) then
    FreeAndNil(FLabeledElement);
  if Assigned(FLabeledElementReference) then
    FreeAndNil(FLabeledElementReference);
  if Assigned(FNull) then
    FreeAndNil(FNull);
  if Assigned(FNavigationPropertyPath) then
    FreeAndNil(FNavigationPropertyPath);
  if Assigned(FPath) then
    FreeAndNil(FPath);
  if Assigned(FPropertyPath) then
    FreeAndNil(FPropertyPath);
  if Assigned(F_Record) then
    FreeAndNil(F_Record);
  if Assigned(FUrlRef) then
    FreeAndNil(FUrlRef);
  inherited FreeObjectProperties();
end;

function TOneChildExpression.wstHas_Annotation() : Boolean;
begin
  Result := ( FAnnotation <> TOneChildExpression_AnnotationArray(0) );
end;

function TOneChildExpression.wstHas_Binary() : Boolean;
begin
  Result := ( FBinary <> nil );
end;

function TOneChildExpression.wstHas_Bool() : Boolean;
begin
  Result := ( FBool <> nil );
end;

function TOneChildExpression.wstHas_Date() : Boolean;
begin
  Result := ( FDate <> nil );
end;

function TOneChildExpression.wstHas_DateTimeOffset() : Boolean;
begin
  Result := ( FDateTimeOffset <> nil );
end;

function TOneChildExpression.wstHas_Decimal() : Boolean;
begin
  Result := ( FDecimal <> nil );
end;

function TOneChildExpression.wstHas_Duration() : Boolean;
begin
  Result := ( FDuration <> nil );
end;

function TOneChildExpression.wstHas_EnumMember() : Boolean;
begin
  Result := ( FEnumMember <> '' );
end;

function TOneChildExpression.wstHas_Float() : Boolean;
begin
  Result := ( FFloat <> nil );
end;

function TOneChildExpression.wstHas_Guid() : Boolean;
begin
  Result := ( FGuid <> nil );
end;

function TOneChildExpression.wstHas_Int() : Boolean;
begin
  Result := ( FInt <> nil );
end;

function TOneChildExpression.wstHas__String() : Boolean;
begin
  Result := ( F_String <> nil );
end;

function TOneChildExpression.wstHas_TimeOfDay() : Boolean;
begin
  Result := ( FTimeOfDay <> nil );
end;

function TOneChildExpression.wstHas_AnnotationPath() : Boolean;
begin
  Result := ( FAnnotationPath <> nil );
end;

function TOneChildExpression.wstHas_Apply() : Boolean;
begin
  Result := ( FApply <> nil );
end;

function TOneChildExpression.wstHas_Cast() : Boolean;
begin
  Result := ( FCast <> nil );
end;

function TOneChildExpression.wstHas_Collection() : Boolean;
begin
  Result := ( FCollection <> nil );
end;

function TOneChildExpression.wstHas__If() : Boolean;
begin
  Result := ( F_If <> nil );
end;

function TOneChildExpression.wstHas_Eq() : Boolean;
begin
  Result := ( FEq <> nil );
end;

function TOneChildExpression.wstHas_Ne() : Boolean;
begin
  Result := ( FNe <> nil );
end;

function TOneChildExpression.wstHas_Ge() : Boolean;
begin
  Result := ( FGe <> nil );
end;

function TOneChildExpression.wstHas_Gt() : Boolean;
begin
  Result := ( FGt <> nil );
end;

function TOneChildExpression.wstHas_Le() : Boolean;
begin
  Result := ( FLe <> nil );
end;

function TOneChildExpression.wstHas_Lt() : Boolean;
begin
  Result := ( FLt <> nil );
end;

function TOneChildExpression.wstHas__And() : Boolean;
begin
  Result := ( F_And <> nil );
end;

function TOneChildExpression.wstHas__Or() : Boolean;
begin
  Result := ( F_Or <> nil );
end;

function TOneChildExpression.wstHas__Not() : Boolean;
begin
  Result := ( F_Not <> nil );
end;

function TOneChildExpression.wstHas_IsOf() : Boolean;
begin
  Result := ( FIsOf <> nil );
end;

function TOneChildExpression.wstHas_LabeledElement() : Boolean;
begin
  Result := ( FLabeledElement <> nil );
end;

function TOneChildExpression.wstHas_LabeledElementReference() : Boolean;
begin
  Result := ( FLabeledElementReference <> nil );
end;

function TOneChildExpression.wstHas_Null() : Boolean;
begin
  Result := ( FNull <> TNullExpression(0) );
end;

function TOneChildExpression.wstHas_NavigationPropertyPath() : Boolean;
begin
  Result := ( FNavigationPropertyPath <> nil );
end;

function TOneChildExpression.wstHas_Path() : Boolean;
begin
  Result := ( FPath <> nil );
end;

function TOneChildExpression.wstHas_PropertyPath() : Boolean;
begin
  Result := ( FPropertyPath <> nil );
end;

function TOneChildExpression.wstHas__Record() : Boolean;
begin
  Result := ( F_Record <> nil );
end;

function TOneChildExpression.wstHas_UrlRef() : Boolean;
begin
  Result := ( FUrlRef <> nil );
end;

{ TTwoChildrenExpression }

constructor TTwoChildrenExpression.Create();
begin
  inherited Create();
  FAnnotation := TTwoChildrenExpression_AnnotationArray.Create();
  FNull := TNullExpression.Create();
end;

procedure TTwoChildrenExpression.FreeObjectProperties();
begin
  if Assigned(FAnnotation) then
    FreeAndNil(FAnnotation);
  if Assigned(FBinary) then
    FreeAndNil(FBinary);
  if Assigned(FBool) then
    FreeAndNil(FBool);
  if Assigned(FDate) then
    FreeAndNil(FDate);
  if Assigned(FDateTimeOffset) then
    FreeAndNil(FDateTimeOffset);
  if Assigned(FDecimal) then
    FreeAndNil(FDecimal);
  if Assigned(FDuration) then
    FreeAndNil(FDuration);
  if Assigned(FFloat) then
    FreeAndNil(FFloat);
  if Assigned(FGuid) then
    FreeAndNil(FGuid);
  if Assigned(FInt) then
    FreeAndNil(FInt);
  if Assigned(F_String) then
    FreeAndNil(F_String);
  if Assigned(FTimeOfDay) then
    FreeAndNil(FTimeOfDay);
  if Assigned(FAnnotationPath) then
    FreeAndNil(FAnnotationPath);
  if Assigned(FApply) then
    FreeAndNil(FApply);
  if Assigned(FCast) then
    FreeAndNil(FCast);
  if Assigned(FCollection) then
    FreeAndNil(FCollection);
  if Assigned(F_If) then
    FreeAndNil(F_If);
  if Assigned(FEq) then
    FreeAndNil(FEq);
  if Assigned(FNe) then
    FreeAndNil(FNe);
  if Assigned(FGe) then
    FreeAndNil(FGe);
  if Assigned(FGt) then
    FreeAndNil(FGt);
  if Assigned(FLe) then
    FreeAndNil(FLe);
  if Assigned(FLt) then
    FreeAndNil(FLt);
  if Assigned(F_And) then
    FreeAndNil(F_And);
  if Assigned(F_Or) then
    FreeAndNil(F_Or);
  if Assigned(F_Not) then
    FreeAndNil(F_Not);
  if Assigned(FIsOf) then
    FreeAndNil(FIsOf);
  if Assigned(FLabeledElement) then
    FreeAndNil(FLabeledElement);
  if Assigned(FLabeledElementReference) then
    FreeAndNil(FLabeledElementReference);
  if Assigned(FNull) then
    FreeAndNil(FNull);
  if Assigned(FNavigationPropertyPath) then
    FreeAndNil(FNavigationPropertyPath);
  if Assigned(FPath) then
    FreeAndNil(FPath);
  if Assigned(FPropertyPath) then
    FreeAndNil(FPropertyPath);
  if Assigned(F_Record) then
    FreeAndNil(F_Record);
  if Assigned(FUrlRef) then
    FreeAndNil(FUrlRef);
  inherited FreeObjectProperties();
end;

function TTwoChildrenExpression.wstHas_Annotation() : Boolean;
begin
  Result := ( FAnnotation <> TTwoChildrenExpression_AnnotationArray(0) );
end;

function TTwoChildrenExpression.wstHas_Binary() : Boolean;
begin
  Result := ( FBinary <> nil );
end;

function TTwoChildrenExpression.wstHas_Bool() : Boolean;
begin
  Result := ( FBool <> nil );
end;

function TTwoChildrenExpression.wstHas_Date() : Boolean;
begin
  Result := ( FDate <> nil );
end;

function TTwoChildrenExpression.wstHas_DateTimeOffset() : Boolean;
begin
  Result := ( FDateTimeOffset <> nil );
end;

function TTwoChildrenExpression.wstHas_Decimal() : Boolean;
begin
  Result := ( FDecimal <> nil );
end;

function TTwoChildrenExpression.wstHas_Duration() : Boolean;
begin
  Result := ( FDuration <> nil );
end;

function TTwoChildrenExpression.wstHas_EnumMember() : Boolean;
begin
  Result := ( FEnumMember <> '' );
end;

function TTwoChildrenExpression.wstHas_Float() : Boolean;
begin
  Result := ( FFloat <> nil );
end;

function TTwoChildrenExpression.wstHas_Guid() : Boolean;
begin
  Result := ( FGuid <> nil );
end;

function TTwoChildrenExpression.wstHas_Int() : Boolean;
begin
  Result := ( FInt <> nil );
end;

function TTwoChildrenExpression.wstHas__String() : Boolean;
begin
  Result := ( F_String <> nil );
end;

function TTwoChildrenExpression.wstHas_TimeOfDay() : Boolean;
begin
  Result := ( FTimeOfDay <> nil );
end;

function TTwoChildrenExpression.wstHas_AnnotationPath() : Boolean;
begin
  Result := ( FAnnotationPath <> nil );
end;

function TTwoChildrenExpression.wstHas_Apply() : Boolean;
begin
  Result := ( FApply <> nil );
end;

function TTwoChildrenExpression.wstHas_Cast() : Boolean;
begin
  Result := ( FCast <> nil );
end;

function TTwoChildrenExpression.wstHas_Collection() : Boolean;
begin
  Result := ( FCollection <> nil );
end;

function TTwoChildrenExpression.wstHas__If() : Boolean;
begin
  Result := ( F_If <> nil );
end;

function TTwoChildrenExpression.wstHas_Eq() : Boolean;
begin
  Result := ( FEq <> nil );
end;

function TTwoChildrenExpression.wstHas_Ne() : Boolean;
begin
  Result := ( FNe <> nil );
end;

function TTwoChildrenExpression.wstHas_Ge() : Boolean;
begin
  Result := ( FGe <> nil );
end;

function TTwoChildrenExpression.wstHas_Gt() : Boolean;
begin
  Result := ( FGt <> nil );
end;

function TTwoChildrenExpression.wstHas_Le() : Boolean;
begin
  Result := ( FLe <> nil );
end;

function TTwoChildrenExpression.wstHas_Lt() : Boolean;
begin
  Result := ( FLt <> nil );
end;

function TTwoChildrenExpression.wstHas__And() : Boolean;
begin
  Result := ( F_And <> nil );
end;

function TTwoChildrenExpression.wstHas__Or() : Boolean;
begin
  Result := ( F_Or <> nil );
end;

function TTwoChildrenExpression.wstHas__Not() : Boolean;
begin
  Result := ( F_Not <> nil );
end;

function TTwoChildrenExpression.wstHas_IsOf() : Boolean;
begin
  Result := ( FIsOf <> nil );
end;

function TTwoChildrenExpression.wstHas_LabeledElement() : Boolean;
begin
  Result := ( FLabeledElement <> nil );
end;

function TTwoChildrenExpression.wstHas_LabeledElementReference() : Boolean;
begin
  Result := ( FLabeledElementReference <> nil );
end;

function TTwoChildrenExpression.wstHas_Null() : Boolean;
begin
  Result := ( FNull <> TNullExpression(0) );
end;

function TTwoChildrenExpression.wstHas_NavigationPropertyPath() : Boolean;
begin
  Result := ( FNavigationPropertyPath <> nil );
end;

function TTwoChildrenExpression.wstHas_Path() : Boolean;
begin
  Result := ( FPath <> nil );
end;

function TTwoChildrenExpression.wstHas_PropertyPath() : Boolean;
begin
  Result := ( FPropertyPath <> nil );
end;

function TTwoChildrenExpression.wstHas__Record() : Boolean;
begin
  Result := ( F_Record <> nil );
end;

function TTwoChildrenExpression.wstHas_UrlRef() : Boolean;
begin
  Result := ( FUrlRef <> nil );
end;

{ TLabeledElementExpression }

constructor TLabeledElementExpression.Create();
begin
  inherited Create();
  FAnnotation := TLabeledElementExpression_AnnotationArray.Create();
  FNull := TNullExpression.Create();
  FDateAtt := TDateRemotable.Create();
  FDateTimeOffsetAtt := dateTimeStamp.Create();
  FDurationAtt := dayTimeDuration.Create();
  FTimeOfDayAtt := TTimeRemotable.Create();
end;

procedure TLabeledElementExpression.FreeObjectProperties();
begin
  if Assigned(FAnnotation) then
    FreeAndNil(FAnnotation);
  if Assigned(FBinary) then
    FreeAndNil(FBinary);
  if Assigned(FBool) then
    FreeAndNil(FBool);
  if Assigned(FDate) then
    FreeAndNil(FDate);
  if Assigned(FDateTimeOffset) then
    FreeAndNil(FDateTimeOffset);
  if Assigned(FDecimal) then
    FreeAndNil(FDecimal);
  if Assigned(FDuration) then
    FreeAndNil(FDuration);
  if Assigned(FFloat) then
    FreeAndNil(FFloat);
  if Assigned(FGuid) then
    FreeAndNil(FGuid);
  if Assigned(FInt) then
    FreeAndNil(FInt);
  if Assigned(F_String) then
    FreeAndNil(F_String);
  if Assigned(FTimeOfDay) then
    FreeAndNil(FTimeOfDay);
  if Assigned(FAnnotationPath) then
    FreeAndNil(FAnnotationPath);
  if Assigned(FApply) then
    FreeAndNil(FApply);
  if Assigned(FCast) then
    FreeAndNil(FCast);
  if Assigned(FCollection) then
    FreeAndNil(FCollection);
  if Assigned(F_If) then
    FreeAndNil(F_If);
  if Assigned(FEq) then
    FreeAndNil(FEq);
  if Assigned(FNe) then
    FreeAndNil(FNe);
  if Assigned(FGe) then
    FreeAndNil(FGe);
  if Assigned(FGt) then
    FreeAndNil(FGt);
  if Assigned(FLe) then
    FreeAndNil(FLe);
  if Assigned(FLt) then
    FreeAndNil(FLt);
  if Assigned(F_And) then
    FreeAndNil(F_And);
  if Assigned(F_Or) then
    FreeAndNil(F_Or);
  if Assigned(F_Not) then
    FreeAndNil(F_Not);
  if Assigned(FIsOf) then
    FreeAndNil(FIsOf);
  if Assigned(FLabeledElement) then
    FreeAndNil(FLabeledElement);
  if Assigned(FLabeledElementReference) then
    FreeAndNil(FLabeledElementReference);
  if Assigned(FNull) then
    FreeAndNil(FNull);
  if Assigned(FNavigationPropertyPath) then
    FreeAndNil(FNavigationPropertyPath);
  if Assigned(FPath) then
    FreeAndNil(FPath);
  if Assigned(FPropertyPath) then
    FreeAndNil(FPropertyPath);
  if Assigned(F_Record) then
    FreeAndNil(F_Record);
  if Assigned(FUrlRef) then
    FreeAndNil(FUrlRef);
  if Assigned(FDateAtt) then
    FreeAndNil(FDateAtt);
  if Assigned(FDateTimeOffsetAtt) then
    FreeAndNil(FDateTimeOffsetAtt);
  if Assigned(FDurationAtt) then
    FreeAndNil(FDurationAtt);
  if Assigned(FTimeOfDayAtt) then
    FreeAndNil(FTimeOfDayAtt);
  inherited FreeObjectProperties();
end;

function TLabeledElementExpression.wstHas_Annotation() : Boolean;
begin
  Result := ( FAnnotation <> TLabeledElementExpression_AnnotationArray(0) );
end;

function TLabeledElementExpression.wstHas_Binary() : Boolean;
begin
  Result := ( FBinary <> nil );
end;

function TLabeledElementExpression.wstHas_Bool() : Boolean;
begin
  Result := ( FBool <> nil );
end;

function TLabeledElementExpression.wstHas_Date() : Boolean;
begin
  Result := ( FDate <> nil );
end;

function TLabeledElementExpression.wstHas_DateTimeOffset() : Boolean;
begin
  Result := ( FDateTimeOffset <> nil );
end;

function TLabeledElementExpression.wstHas_Decimal() : Boolean;
begin
  Result := ( FDecimal <> nil );
end;

function TLabeledElementExpression.wstHas_Duration() : Boolean;
begin
  Result := ( FDuration <> nil );
end;

function TLabeledElementExpression.wstHas_EnumMember() : Boolean;
begin
  Result := ( FEnumMember <> '' );
end;

function TLabeledElementExpression.wstHas_Float() : Boolean;
begin
  Result := ( FFloat <> nil );
end;

function TLabeledElementExpression.wstHas_Guid() : Boolean;
begin
  Result := ( FGuid <> nil );
end;

function TLabeledElementExpression.wstHas_Int() : Boolean;
begin
  Result := ( FInt <> nil );
end;

function TLabeledElementExpression.wstHas__String() : Boolean;
begin
  Result := ( F_String <> nil );
end;

function TLabeledElementExpression.wstHas_TimeOfDay() : Boolean;
begin
  Result := ( FTimeOfDay <> nil );
end;

function TLabeledElementExpression.wstHas_AnnotationPath() : Boolean;
begin
  Result := ( FAnnotationPath <> nil );
end;

function TLabeledElementExpression.wstHas_Apply() : Boolean;
begin
  Result := ( FApply <> nil );
end;

function TLabeledElementExpression.wstHas_Cast() : Boolean;
begin
  Result := ( FCast <> nil );
end;

function TLabeledElementExpression.wstHas_Collection() : Boolean;
begin
  Result := ( FCollection <> nil );
end;

function TLabeledElementExpression.wstHas__If() : Boolean;
begin
  Result := ( F_If <> nil );
end;

function TLabeledElementExpression.wstHas_Eq() : Boolean;
begin
  Result := ( FEq <> nil );
end;

function TLabeledElementExpression.wstHas_Ne() : Boolean;
begin
  Result := ( FNe <> nil );
end;

function TLabeledElementExpression.wstHas_Ge() : Boolean;
begin
  Result := ( FGe <> nil );
end;

function TLabeledElementExpression.wstHas_Gt() : Boolean;
begin
  Result := ( FGt <> nil );
end;

function TLabeledElementExpression.wstHas_Le() : Boolean;
begin
  Result := ( FLe <> nil );
end;

function TLabeledElementExpression.wstHas_Lt() : Boolean;
begin
  Result := ( FLt <> nil );
end;

function TLabeledElementExpression.wstHas__And() : Boolean;
begin
  Result := ( F_And <> nil );
end;

function TLabeledElementExpression.wstHas__Or() : Boolean;
begin
  Result := ( F_Or <> nil );
end;

function TLabeledElementExpression.wstHas__Not() : Boolean;
begin
  Result := ( F_Not <> nil );
end;

function TLabeledElementExpression.wstHas_IsOf() : Boolean;
begin
  Result := ( FIsOf <> nil );
end;

function TLabeledElementExpression.wstHas_LabeledElement() : Boolean;
begin
  Result := ( FLabeledElement <> nil );
end;

function TLabeledElementExpression.wstHas_LabeledElementReference() : Boolean;
begin
  Result := ( FLabeledElementReference <> nil );
end;

function TLabeledElementExpression.wstHas_Null() : Boolean;
begin
  Result := ( FNull <> TNullExpression(0) );
end;

function TLabeledElementExpression.wstHas_NavigationPropertyPath() : Boolean;
begin
  Result := ( FNavigationPropertyPath <> nil );
end;

function TLabeledElementExpression.wstHas_Path() : Boolean;
begin
  Result := ( FPath <> nil );
end;

function TLabeledElementExpression.wstHas_PropertyPath() : Boolean;
begin
  Result := ( FPropertyPath <> nil );
end;

function TLabeledElementExpression.wstHas__Record() : Boolean;
begin
  Result := ( F_Record <> nil );
end;

function TLabeledElementExpression.wstHas_UrlRef() : Boolean;
begin
  Result := ( FUrlRef <> nil );
end;

function TLabeledElementExpression.wstHas_BinaryAtt() : Boolean;
begin
  Result := ( FBinaryAtt <> '' );
end;

function TLabeledElementExpression.wstHas_BoolAtt() : Boolean;
begin
  Result := ( FBoolAtt <> boolean(0) );
end;

function TLabeledElementExpression.wstHas_DateAtt() : Boolean;
begin
  Result := ( FDateAtt <> nil );
end;

function TLabeledElementExpression.wstHas_DateTimeOffsetAtt() : Boolean;
begin
  Result := ( FDateTimeOffsetAtt <> nil );
end;

function TLabeledElementExpression.wstHas_DecimalAtt() : Boolean;
begin
  Result := ( FDecimalAtt <> 0 );
end;

function TLabeledElementExpression.wstHas_DurationAtt() : Boolean;
begin
  Result := ( FDurationAtt <> nil );
end;

function TLabeledElementExpression.wstHas_EnumMemberAtt() : Boolean;
begin
  Result := ( FEnumMemberAtt <> '' );
end;

function TLabeledElementExpression.wstHas_FloatAtt() : Boolean;
begin
  Result := ( FFloatAtt <> 0 );
end;

function TLabeledElementExpression.wstHas_GuidAtt() : Boolean;
begin
  Result := ( FGuidAtt <> '' );
end;

function TLabeledElementExpression.wstHas_IntAtt() : Boolean;
begin
  Result := ( FIntAtt <> integer(0) );
end;

function TLabeledElementExpression.wstHas__StringAtt() : Boolean;
begin
  Result := ( F_StringAtt <> '' );
end;

function TLabeledElementExpression.wstHas_TimeOfDayAtt() : Boolean;
begin
  Result := ( FTimeOfDayAtt <> nil );
end;

function TLabeledElementExpression.wstHas_AnnotationPathAtt() : Boolean;
begin
  Result := ( FAnnotationPathAtt <> '' );
end;

function TLabeledElementExpression.wstHas_NavigationPropertyPathAtt() : Boolean;
begin
  Result := ( FNavigationPropertyPathAtt <> '' );
end;

function TLabeledElementExpression.wstHas_PathAtt() : Boolean;
begin
  Result := ( FPathAtt <> '' );
end;

function TLabeledElementExpression.wstHas_PropertyPathAtt() : Boolean;
begin
  Result := ( FPropertyPathAtt <> '' );
end;

function TLabeledElementExpression.wstHas_UrlRefAtt() : Boolean;
begin
  Result := ( FUrlRefAtt <> '' );
end;

{ TRecordExpression }

constructor TRecordExpression.Create();
begin
  inherited Create();
  FPropertyValue := TRecordExpression_PropertyValueArray.Create();
  FAnnotation := TRecordExpression_AnnotationArray.Create();
end;

procedure TRecordExpression.FreeObjectProperties();
begin
  if Assigned(FPropertyValue) then
    FreeAndNil(FPropertyValue);
  if Assigned(FAnnotation) then
    FreeAndNil(FAnnotation);
  inherited FreeObjectProperties();
end;

function TRecordExpression.wstHas_PropertyValue() : Boolean;
begin
  Result := ( FPropertyValue <> TRecordExpression_PropertyValueArray(0) );
end;

function TRecordExpression.wstHas_Annotation() : Boolean;
begin
  Result := ( FAnnotation <> TRecordExpression_AnnotationArray(0) );
end;

function TRecordExpression.wstHas__Type() : Boolean;
begin
  Result := ( F_Type <> '' );
end;

{ TPropertyValue }

constructor TPropertyValue.Create();
begin
  inherited Create();
  FAnnotation := TPropertyValue_AnnotationArray.Create();
  FNull := TNullExpression.Create();
  FDateAtt := TDateRemotable.Create();
  FDateTimeOffsetAtt := dateTimeStamp.Create();
  FDurationAtt := dayTimeDuration.Create();
  FTimeOfDayAtt := TTimeRemotable.Create();
end;

procedure TPropertyValue.FreeObjectProperties();
begin
  if Assigned(FAnnotation) then
    FreeAndNil(FAnnotation);
  if Assigned(FBinary) then
    FreeAndNil(FBinary);
  if Assigned(FBool) then
    FreeAndNil(FBool);
  if Assigned(FDate) then
    FreeAndNil(FDate);
  if Assigned(FDateTimeOffset) then
    FreeAndNil(FDateTimeOffset);
  if Assigned(FDecimal) then
    FreeAndNil(FDecimal);
  if Assigned(FDuration) then
    FreeAndNil(FDuration);
  if Assigned(FFloat) then
    FreeAndNil(FFloat);
  if Assigned(FGuid) then
    FreeAndNil(FGuid);
  if Assigned(FInt) then
    FreeAndNil(FInt);
  if Assigned(F_String) then
    FreeAndNil(F_String);
  if Assigned(FTimeOfDay) then
    FreeAndNil(FTimeOfDay);
  if Assigned(FAnnotationPath) then
    FreeAndNil(FAnnotationPath);
  if Assigned(FApply) then
    FreeAndNil(FApply);
  if Assigned(FCast) then
    FreeAndNil(FCast);
  if Assigned(FCollection) then
    FreeAndNil(FCollection);
  if Assigned(F_If) then
    FreeAndNil(F_If);
  if Assigned(FEq) then
    FreeAndNil(FEq);
  if Assigned(FNe) then
    FreeAndNil(FNe);
  if Assigned(FGe) then
    FreeAndNil(FGe);
  if Assigned(FGt) then
    FreeAndNil(FGt);
  if Assigned(FLe) then
    FreeAndNil(FLe);
  if Assigned(FLt) then
    FreeAndNil(FLt);
  if Assigned(F_And) then
    FreeAndNil(F_And);
  if Assigned(F_Or) then
    FreeAndNil(F_Or);
  if Assigned(F_Not) then
    FreeAndNil(F_Not);
  if Assigned(FIsOf) then
    FreeAndNil(FIsOf);
  if Assigned(FLabeledElement) then
    FreeAndNil(FLabeledElement);
  if Assigned(FLabeledElementReference) then
    FreeAndNil(FLabeledElementReference);
  if Assigned(FNull) then
    FreeAndNil(FNull);
  if Assigned(FNavigationPropertyPath) then
    FreeAndNil(FNavigationPropertyPath);
  if Assigned(FPath) then
    FreeAndNil(FPath);
  if Assigned(FPropertyPath) then
    FreeAndNil(FPropertyPath);
  if Assigned(F_Record) then
    FreeAndNil(F_Record);
  if Assigned(FUrlRef) then
    FreeAndNil(FUrlRef);
  if Assigned(FDateAtt) then
    FreeAndNil(FDateAtt);
  if Assigned(FDateTimeOffsetAtt) then
    FreeAndNil(FDateTimeOffsetAtt);
  if Assigned(FDurationAtt) then
    FreeAndNil(FDurationAtt);
  if Assigned(FTimeOfDayAtt) then
    FreeAndNil(FTimeOfDayAtt);
  inherited FreeObjectProperties();
end;

function TPropertyValue.wstHas_Annotation() : Boolean;
begin
  Result := ( FAnnotation <> TPropertyValue_AnnotationArray(0) );
end;

function TPropertyValue.wstHas_Binary() : Boolean;
begin
  Result := ( FBinary <> nil );
end;

function TPropertyValue.wstHas_Bool() : Boolean;
begin
  Result := ( FBool <> nil );
end;

function TPropertyValue.wstHas_Date() : Boolean;
begin
  Result := ( FDate <> nil );
end;

function TPropertyValue.wstHas_DateTimeOffset() : Boolean;
begin
  Result := ( FDateTimeOffset <> nil );
end;

function TPropertyValue.wstHas_Decimal() : Boolean;
begin
  Result := ( FDecimal <> nil );
end;

function TPropertyValue.wstHas_Duration() : Boolean;
begin
  Result := ( FDuration <> nil );
end;

function TPropertyValue.wstHas_EnumMember() : Boolean;
begin
  Result := ( FEnumMember <> '' );
end;

function TPropertyValue.wstHas_Float() : Boolean;
begin
  Result := ( FFloat <> nil );
end;

function TPropertyValue.wstHas_Guid() : Boolean;
begin
  Result := ( FGuid <> nil );
end;

function TPropertyValue.wstHas_Int() : Boolean;
begin
  Result := ( FInt <> nil );
end;

function TPropertyValue.wstHas__String() : Boolean;
begin
  Result := ( F_String <> nil );
end;

function TPropertyValue.wstHas_TimeOfDay() : Boolean;
begin
  Result := ( FTimeOfDay <> nil );
end;

function TPropertyValue.wstHas_AnnotationPath() : Boolean;
begin
  Result := ( FAnnotationPath <> nil );
end;

function TPropertyValue.wstHas_Apply() : Boolean;
begin
  Result := ( FApply <> nil );
end;

function TPropertyValue.wstHas_Cast() : Boolean;
begin
  Result := ( FCast <> nil );
end;

function TPropertyValue.wstHas_Collection() : Boolean;
begin
  Result := ( FCollection <> nil );
end;

function TPropertyValue.wstHas__If() : Boolean;
begin
  Result := ( F_If <> nil );
end;

function TPropertyValue.wstHas_Eq() : Boolean;
begin
  Result := ( FEq <> nil );
end;

function TPropertyValue.wstHas_Ne() : Boolean;
begin
  Result := ( FNe <> nil );
end;

function TPropertyValue.wstHas_Ge() : Boolean;
begin
  Result := ( FGe <> nil );
end;

function TPropertyValue.wstHas_Gt() : Boolean;
begin
  Result := ( FGt <> nil );
end;

function TPropertyValue.wstHas_Le() : Boolean;
begin
  Result := ( FLe <> nil );
end;

function TPropertyValue.wstHas_Lt() : Boolean;
begin
  Result := ( FLt <> nil );
end;

function TPropertyValue.wstHas__And() : Boolean;
begin
  Result := ( F_And <> nil );
end;

function TPropertyValue.wstHas__Or() : Boolean;
begin
  Result := ( F_Or <> nil );
end;

function TPropertyValue.wstHas__Not() : Boolean;
begin
  Result := ( F_Not <> nil );
end;

function TPropertyValue.wstHas_IsOf() : Boolean;
begin
  Result := ( FIsOf <> nil );
end;

function TPropertyValue.wstHas_LabeledElement() : Boolean;
begin
  Result := ( FLabeledElement <> nil );
end;

function TPropertyValue.wstHas_LabeledElementReference() : Boolean;
begin
  Result := ( FLabeledElementReference <> nil );
end;

function TPropertyValue.wstHas_Null() : Boolean;
begin
  Result := ( FNull <> TNullExpression(0) );
end;

function TPropertyValue.wstHas_NavigationPropertyPath() : Boolean;
begin
  Result := ( FNavigationPropertyPath <> nil );
end;

function TPropertyValue.wstHas_Path() : Boolean;
begin
  Result := ( FPath <> nil );
end;

function TPropertyValue.wstHas_PropertyPath() : Boolean;
begin
  Result := ( FPropertyPath <> nil );
end;

function TPropertyValue.wstHas__Record() : Boolean;
begin
  Result := ( F_Record <> nil );
end;

function TPropertyValue.wstHas_UrlRef() : Boolean;
begin
  Result := ( FUrlRef <> nil );
end;

function TPropertyValue.wstHas_BinaryAtt() : Boolean;
begin
  Result := ( FBinaryAtt <> '' );
end;

function TPropertyValue.wstHas_BoolAtt() : Boolean;
begin
  Result := ( FBoolAtt <> boolean(0) );
end;

function TPropertyValue.wstHas_DateAtt() : Boolean;
begin
  Result := ( FDateAtt <> nil );
end;

function TPropertyValue.wstHas_DateTimeOffsetAtt() : Boolean;
begin
  Result := ( FDateTimeOffsetAtt <> nil );
end;

function TPropertyValue.wstHas_DecimalAtt() : Boolean;
begin
  Result := ( FDecimalAtt <> 0 );
end;

function TPropertyValue.wstHas_DurationAtt() : Boolean;
begin
  Result := ( FDurationAtt <> nil );
end;

function TPropertyValue.wstHas_EnumMemberAtt() : Boolean;
begin
  Result := ( FEnumMemberAtt <> '' );
end;

function TPropertyValue.wstHas_FloatAtt() : Boolean;
begin
  Result := ( FFloatAtt <> 0 );
end;

function TPropertyValue.wstHas_GuidAtt() : Boolean;
begin
  Result := ( FGuidAtt <> '' );
end;

function TPropertyValue.wstHas_IntAtt() : Boolean;
begin
  Result := ( FIntAtt <> integer(0) );
end;

function TPropertyValue.wstHas__StringAtt() : Boolean;
begin
  Result := ( F_StringAtt <> '' );
end;

function TPropertyValue.wstHas_TimeOfDayAtt() : Boolean;
begin
  Result := ( FTimeOfDayAtt <> nil );
end;

function TPropertyValue.wstHas_AnnotationPathAtt() : Boolean;
begin
  Result := ( FAnnotationPathAtt <> '' );
end;

function TPropertyValue.wstHas_NavigationPropertyPathAtt() : Boolean;
begin
  Result := ( FNavigationPropertyPathAtt <> '' );
end;

function TPropertyValue.wstHas_PathAtt() : Boolean;
begin
  Result := ( FPathAtt <> '' );
end;

function TPropertyValue.wstHas_PropertyPathAtt() : Boolean;
begin
  Result := ( FPropertyPathAtt <> '' );
end;

function TPropertyValue.wstHas_UrlRefAtt() : Boolean;
begin
  Result := ( FUrlRefAtt <> '' );
end;

{ TEntityContainer }

constructor TEntityContainer.Create();
begin
  inherited Create();
  FEntitySet := TEntityContainer_EntitySetArray.Create();
  FActionImport := TEntityContainer_ActionImportArray.Create();
  FFunctionImport := TEntityContainer_FunctionImportArray.Create();
  FSingleton := TEntityContainer_SingletonArray.Create();
  FAnnotation := TEntityContainer_AnnotationArray.Create();
end;

procedure TEntityContainer.FreeObjectProperties();
begin
  if Assigned(FEntitySet) then
    FreeAndNil(FEntitySet);
  if Assigned(FActionImport) then
    FreeAndNil(FActionImport);
  if Assigned(FFunctionImport) then
    FreeAndNil(FFunctionImport);
  if Assigned(FSingleton) then
    FreeAndNil(FSingleton);
  if Assigned(FAnnotation) then
    FreeAndNil(FAnnotation);
  inherited FreeObjectProperties();
end;

function TEntityContainer.wstHas_EntitySet() : Boolean;
begin
  Result := ( FEntitySet <> TEntityContainer_EntitySetArray(0) );
end;

function TEntityContainer.wstHas_ActionImport() : Boolean;
begin
  Result := ( FActionImport <> TEntityContainer_ActionImportArray(0) );
end;

function TEntityContainer.wstHas_FunctionImport() : Boolean;
begin
  Result := ( FFunctionImport <> TEntityContainer_FunctionImportArray(0) );
end;

function TEntityContainer.wstHas_Singleton() : Boolean;
begin
  Result := ( FSingleton <> TEntityContainer_SingletonArray(0) );
end;

function TEntityContainer.wstHas_Annotation() : Boolean;
begin
  Result := ( FAnnotation <> TEntityContainer_AnnotationArray(0) );
end;

function TEntityContainer.wstHas_Extends() : Boolean;
begin
  Result := ( FExtends <> '' );
end;

function TEntitySetAttributes.wstHas_IncludeInServiceDocument() : Boolean;
begin
  Result := ( FIncludeInServiceDocument <> boolean(0) );
end;

{ TEntitySet }

constructor TEntitySet.Create();
begin
  inherited Create();
  FNavigationPropertyBinding := TEntitySet_NavigationPropertyBindingArray.Create();
  FAnnotation := TEntitySet_AnnotationArray.Create();
end;

procedure TEntitySet.FreeObjectProperties();
begin
  if Assigned(FNavigationPropertyBinding) then
    FreeAndNil(FNavigationPropertyBinding);
  if Assigned(FAnnotation) then
    FreeAndNil(FAnnotation);
  inherited FreeObjectProperties();
end;

function TEntitySet.wstHas_NavigationPropertyBinding() : Boolean;
begin
  Result := ( FNavigationPropertyBinding <> TEntitySet_NavigationPropertyBindingArray(0) );
end;

function TEntitySet.wstHas_Annotation() : Boolean;
begin
  Result := ( FAnnotation <> TEntitySet_AnnotationArray(0) );
end;

function TEntitySet.wstHas_IncludeInServiceDocument() : Boolean;
begin
  Result := ( FIncludeInServiceDocument <> boolean(0) );
end;

{ TSingleton }

constructor TSingleton.Create();
begin
  inherited Create();
  FNavigationPropertyBinding := TSingleton_NavigationPropertyBindingArray.Create();
  FAnnotation := TSingleton_AnnotationArray.Create();
end;

procedure TSingleton.FreeObjectProperties();
begin
  if Assigned(FNavigationPropertyBinding) then
    FreeAndNil(FNavigationPropertyBinding);
  if Assigned(FAnnotation) then
    FreeAndNil(FAnnotation);
  inherited FreeObjectProperties();
end;

function TSingleton.wstHas_NavigationPropertyBinding() : Boolean;
begin
  Result := ( FNavigationPropertyBinding <> TSingleton_NavigationPropertyBindingArray(0) );
end;

function TSingleton.wstHas_Annotation() : Boolean;
begin
  Result := ( FAnnotation <> TSingleton_AnnotationArray(0) );
end;

function TActionFunctionImportAttributes.wstHas_EntitySet() : Boolean;
begin
  Result := ( FEntitySet <> '' );
end;

function TActionFunctionImportAttributes.wstHas_IncludeInServiceDocument() : Boolean;
begin
  Result := ( FIncludeInServiceDocument <> boolean(0) );
end;

{ TActionImport }

constructor TActionImport.Create();
begin
  inherited Create();
  FAnnotation := TActionImport_AnnotationArray.Create();
end;

procedure TActionImport.FreeObjectProperties();
begin
  if Assigned(FAnnotation) then
    FreeAndNil(FAnnotation);
  inherited FreeObjectProperties();
end;

function TActionImport.wstHas_Annotation() : Boolean;
begin
  Result := ( FAnnotation <> TActionImport_AnnotationArray(0) );
end;

function TActionImport.wstHas_EntitySet() : Boolean;
begin
  Result := ( FEntitySet <> '' );
end;

function TActionImport.wstHas_IncludeInServiceDocument() : Boolean;
begin
  Result := ( FIncludeInServiceDocument <> boolean(0) );
end;

{ TFunctionImport }

constructor TFunctionImport.Create();
begin
  inherited Create();
  FAnnotation := TFunctionImport_AnnotationArray.Create();
end;

procedure TFunctionImport.FreeObjectProperties();
begin
  if Assigned(FAnnotation) then
    FreeAndNil(FAnnotation);
  inherited FreeObjectProperties();
end;

function TFunctionImport.wstHas_Annotation() : Boolean;
begin
  Result := ( FAnnotation <> TFunctionImport_AnnotationArray(0) );
end;

function TFunctionImport.wstHas_EntitySet() : Boolean;
begin
  Result := ( FEntitySet <> '' );
end;

function TFunctionImport.wstHas_IncludeInServiceDocument() : Boolean;
begin
  Result := ( FIncludeInServiceDocument <> boolean(0) );
end;

{ Schema_ComplexTypeArray }

function Schema_ComplexTypeArray.GetItem(AIndex: Integer): TComplexType;
begin
  Result := TComplexType(Inherited GetItem(AIndex));
end;

class function Schema_ComplexTypeArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TComplexType;
end;

function Schema_ComplexTypeArray.Add() : TComplexType;
begin
  Result := TComplexType(inherited Add());
end;

function Schema_ComplexTypeArray.AddAt(const APosition : Integer) : TComplexType;
begin
  Result := TComplexType(inherited AddAt(APosition));
end;

{ Schema_EntityTypeArray }

function Schema_EntityTypeArray.GetItem(AIndex: Integer): TEntityType;
begin
  Result := TEntityType(Inherited GetItem(AIndex));
end;

class function Schema_EntityTypeArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TEntityType;
end;

function Schema_EntityTypeArray.Add() : TEntityType;
begin
  Result := TEntityType(inherited Add());
end;

function Schema_EntityTypeArray.AddAt(const APosition : Integer) : TEntityType;
begin
  Result := TEntityType(inherited AddAt(APosition));
end;

{ Schema_TypeDefinitionArray }

function Schema_TypeDefinitionArray.GetItem(AIndex: Integer): TTypeDefinition;
begin
  Result := TTypeDefinition(Inherited GetItem(AIndex));
end;

class function Schema_TypeDefinitionArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TTypeDefinition;
end;

function Schema_TypeDefinitionArray.Add() : TTypeDefinition;
begin
  Result := TTypeDefinition(inherited Add());
end;

function Schema_TypeDefinitionArray.AddAt(const APosition : Integer) : TTypeDefinition;
begin
  Result := TTypeDefinition(inherited AddAt(APosition));
end;

{ Schema_EnumTypeArray }

function Schema_EnumTypeArray.GetItem(AIndex: Integer): TEnumType;
begin
  Result := TEnumType(Inherited GetItem(AIndex));
end;

class function Schema_EnumTypeArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TEnumType;
end;

function Schema_EnumTypeArray.Add() : TEnumType;
begin
  Result := TEnumType(inherited Add());
end;

function Schema_EnumTypeArray.AddAt(const APosition : Integer) : TEnumType;
begin
  Result := TEnumType(inherited AddAt(APosition));
end;

{ Schema_ActionArray }

function Schema_ActionArray.GetItem(AIndex: Integer): TAction;
begin
  Result := TAction(Inherited GetItem(AIndex));
end;

class function Schema_ActionArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TAction;
end;

function Schema_ActionArray.Add() : TAction;
begin
  Result := TAction(inherited Add());
end;

function Schema_ActionArray.AddAt(const APosition : Integer) : TAction;
begin
  Result := TAction(inherited AddAt(APosition));
end;

{ Schema__FunctionArray }

function Schema__FunctionArray.GetItem(AIndex: Integer): TFunction;
begin
  Result := TFunction(Inherited GetItem(AIndex));
end;

class function Schema__FunctionArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TFunction;
end;

function Schema__FunctionArray.Add() : TFunction;
begin
  Result := TFunction(inherited Add());
end;

function Schema__FunctionArray.AddAt(const APosition : Integer) : TFunction;
begin
  Result := TFunction(inherited AddAt(APosition));
end;

{ Schema_TermArray }

function Schema_TermArray.GetItem(AIndex: Integer): TTerm;
begin
  Result := TTerm(Inherited GetItem(AIndex));
end;

class function Schema_TermArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TTerm;
end;

function Schema_TermArray.Add() : TTerm;
begin
  Result := TTerm(inherited Add());
end;

function Schema_TermArray.AddAt(const APosition : Integer) : TTerm;
begin
  Result := TTerm(inherited AddAt(APosition));
end;

{ Schema_AnnotationsArray }

function Schema_AnnotationsArray.GetItem(AIndex: Integer): TAnnotations;
begin
  Result := TAnnotations(Inherited GetItem(AIndex));
end;

class function Schema_AnnotationsArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TAnnotations;
end;

function Schema_AnnotationsArray.Add() : TAnnotations;
begin
  Result := TAnnotations(inherited Add());
end;

function Schema_AnnotationsArray.AddAt(const APosition : Integer) : TAnnotations;
begin
  Result := TAnnotations(inherited AddAt(APosition));
end;

{ Schema_EntityContainerArray }

function Schema_EntityContainerArray.GetItem(AIndex: Integer): TEntityContainer;
begin
  Result := TEntityContainer(Inherited GetItem(AIndex));
end;

class function Schema_EntityContainerArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TEntityContainer;
end;

function Schema_EntityContainerArray.Add() : TEntityContainer;
begin
  Result := TEntityContainer(inherited Add());
end;

function Schema_EntityContainerArray.AddAt(const APosition : Integer) : TEntityContainer;
begin
  Result := TEntityContainer(inherited AddAt(APosition));
end;

{ Schema_AnnotationArray }

function Schema_AnnotationArray.GetItem(AIndex: Integer): Annotation_Type;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure Schema_AnnotationArray.SetItem(AIndex: Integer;const AValue: Annotation_Type);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function Schema_AnnotationArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure Schema_AnnotationArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('Annotation',TypeInfo(Annotation_Type),FData[AIndex]);
end;

procedure Schema_AnnotationArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'Annotation';
  AStore.Get(TypeInfo(Annotation_Type),sName,FData[AIndex]);
end;

class function Schema_AnnotationArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(Annotation_Type);
end;

procedure Schema_AnnotationArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

procedure Schema_AnnotationArray.Assign(Source: TPersistent);
var
  src : Schema_AnnotationArray;
  i, c : Integer;
begin
  if Assigned(Source) and Source.InheritsFrom(Schema_AnnotationArray) then begin
    src := Schema_AnnotationArray(Source);
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

{ TEntityType_KeyArray }

function TEntityType_KeyArray.GetItem(AIndex: Integer): TEntityKeyElement;
begin
  Result := TEntityKeyElement(Inherited GetItem(AIndex));
end;

class function TEntityType_KeyArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TEntityKeyElement;
end;

function TEntityType_KeyArray.Add() : TEntityKeyElement;
begin
  Result := TEntityKeyElement(inherited Add());
end;

function TEntityType_KeyArray.AddAt(const APosition : Integer) : TEntityKeyElement;
begin
  Result := TEntityKeyElement(inherited AddAt(APosition));
end;

{ TEntityType__PropertyArray }

function TEntityType__PropertyArray.GetItem(AIndex: Integer): TProperty;
begin
  Result := TProperty(Inherited GetItem(AIndex));
end;

class function TEntityType__PropertyArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TProperty;
end;

function TEntityType__PropertyArray.Add() : TProperty;
begin
  Result := TProperty(inherited Add());
end;

function TEntityType__PropertyArray.AddAt(const APosition : Integer) : TProperty;
begin
  Result := TProperty(inherited AddAt(APosition));
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

function TEntityType_NavigationPropertyArray.Add() : TNavigationProperty;
begin
  Result := TNavigationProperty(inherited Add());
end;

function TEntityType_NavigationPropertyArray.AddAt(const APosition : Integer) : TNavigationProperty;
begin
  Result := TNavigationProperty(inherited AddAt(APosition));
end;

{ TEntityType_AnnotationArray }

function TEntityType_AnnotationArray.GetItem(AIndex: Integer): Annotation_Type;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure TEntityType_AnnotationArray.SetItem(AIndex: Integer;const AValue: Annotation_Type);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function TEntityType_AnnotationArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure TEntityType_AnnotationArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('Annotation',TypeInfo(Annotation_Type),FData[AIndex]);
end;

procedure TEntityType_AnnotationArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'Annotation';
  AStore.Get(TypeInfo(Annotation_Type),sName,FData[AIndex]);
end;

class function TEntityType_AnnotationArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(Annotation_Type);
end;

procedure TEntityType_AnnotationArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

procedure TEntityType_AnnotationArray.Assign(Source: TPersistent);
var
  src : TEntityType_AnnotationArray;
  i, c : Integer;
begin
  if Assigned(Source) and Source.InheritsFrom(TEntityType_AnnotationArray) then begin
    src := TEntityType_AnnotationArray(Source);
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

{ TEntityKeyElement }

function TEntityKeyElement.GetItem(AIndex: Integer): TPropertyRef;
begin
  Result := TPropertyRef(Inherited GetItem(AIndex));
end;

class function TEntityKeyElement.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TPropertyRef;
end;

function TEntityKeyElement.Add() : TPropertyRef;
begin
  Result := TPropertyRef(inherited Add());
end;

function TEntityKeyElement.AddAt(const APosition : Integer) : TPropertyRef;
begin
  Result := TPropertyRef(inherited AddAt(APosition));
end;

{ TComplexType__PropertyArray }

function TComplexType__PropertyArray.GetItem(AIndex: Integer): TProperty;
begin
  Result := TProperty(Inherited GetItem(AIndex));
end;

class function TComplexType__PropertyArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TProperty;
end;

function TComplexType__PropertyArray.Add() : TProperty;
begin
  Result := TProperty(inherited Add());
end;

function TComplexType__PropertyArray.AddAt(const APosition : Integer) : TProperty;
begin
  Result := TProperty(inherited AddAt(APosition));
end;

{ TComplexType_NavigationPropertyArray }

function TComplexType_NavigationPropertyArray.GetItem(AIndex: Integer): TNavigationProperty;
begin
  Result := TNavigationProperty(Inherited GetItem(AIndex));
end;

class function TComplexType_NavigationPropertyArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TNavigationProperty;
end;

function TComplexType_NavigationPropertyArray.Add() : TNavigationProperty;
begin
  Result := TNavigationProperty(inherited Add());
end;

function TComplexType_NavigationPropertyArray.AddAt(const APosition : Integer) : TNavigationProperty;
begin
  Result := TNavigationProperty(inherited AddAt(APosition));
end;

{ TComplexType_AnnotationArray }

function TComplexType_AnnotationArray.GetItem(AIndex: Integer): Annotation_Type;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure TComplexType_AnnotationArray.SetItem(AIndex: Integer;const AValue: Annotation_Type);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function TComplexType_AnnotationArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure TComplexType_AnnotationArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('Annotation',TypeInfo(Annotation_Type),FData[AIndex]);
end;

procedure TComplexType_AnnotationArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'Annotation';
  AStore.Get(TypeInfo(Annotation_Type),sName,FData[AIndex]);
end;

class function TComplexType_AnnotationArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(Annotation_Type);
end;

procedure TComplexType_AnnotationArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

procedure TComplexType_AnnotationArray.Assign(Source: TPersistent);
var
  src : TComplexType_AnnotationArray;
  i, c : Integer;
begin
  if Assigned(Source) and Source.InheritsFrom(TComplexType_AnnotationArray) then begin
    src := TComplexType_AnnotationArray(Source);
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

{ TProperty_AnnotationArray }

function TProperty_AnnotationArray.GetItem(AIndex: Integer): Annotation_Type;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure TProperty_AnnotationArray.SetItem(AIndex: Integer;const AValue: Annotation_Type);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function TProperty_AnnotationArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure TProperty_AnnotationArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('Annotation',TypeInfo(Annotation_Type),FData[AIndex]);
end;

procedure TProperty_AnnotationArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'Annotation';
  AStore.Get(TypeInfo(Annotation_Type),sName,FData[AIndex]);
end;

class function TProperty_AnnotationArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(Annotation_Type);
end;

procedure TProperty_AnnotationArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

procedure TProperty_AnnotationArray.Assign(Source: TPersistent);
var
  src : TProperty_AnnotationArray;
  i, c : Integer;
begin
  if Assigned(Source) and Source.InheritsFrom(TProperty_AnnotationArray) then begin
    src := TProperty_AnnotationArray(Source);
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

{ TTypeDefinition_AnnotationArray }

function TTypeDefinition_AnnotationArray.GetItem(AIndex: Integer): Annotation_Type;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure TTypeDefinition_AnnotationArray.SetItem(AIndex: Integer;const AValue: Annotation_Type);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function TTypeDefinition_AnnotationArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure TTypeDefinition_AnnotationArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('Annotation',TypeInfo(Annotation_Type),FData[AIndex]);
end;

procedure TTypeDefinition_AnnotationArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'Annotation';
  AStore.Get(TypeInfo(Annotation_Type),sName,FData[AIndex]);
end;

class function TTypeDefinition_AnnotationArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(Annotation_Type);
end;

procedure TTypeDefinition_AnnotationArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

procedure TTypeDefinition_AnnotationArray.Assign(Source: TPersistent);
var
  src : TTypeDefinition_AnnotationArray;
  i, c : Integer;
begin
  if Assigned(Source) and Source.InheritsFrom(TTypeDefinition_AnnotationArray) then begin
    src := TTypeDefinition_AnnotationArray(Source);
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

{ TNavigationProperty_ReferentialConstraintArray }

function TNavigationProperty_ReferentialConstraintArray.GetItem(AIndex: Integer): TReferentialConstraint;
begin
  Result := TReferentialConstraint(Inherited GetItem(AIndex));
end;

class function TNavigationProperty_ReferentialConstraintArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TReferentialConstraint;
end;

function TNavigationProperty_ReferentialConstraintArray.Add() : TReferentialConstraint;
begin
  Result := TReferentialConstraint(inherited Add());
end;

function TNavigationProperty_ReferentialConstraintArray.AddAt(const APosition : Integer) : TReferentialConstraint;
begin
  Result := TReferentialConstraint(inherited AddAt(APosition));
end;

{ TNavigationProperty_OnDeleteArray }

function TNavigationProperty_OnDeleteArray.GetItem(AIndex: Integer): TOnDelete;
begin
  Result := TOnDelete(Inherited GetItem(AIndex));
end;

class function TNavigationProperty_OnDeleteArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TOnDelete;
end;

function TNavigationProperty_OnDeleteArray.Add() : TOnDelete;
begin
  Result := TOnDelete(inherited Add());
end;

function TNavigationProperty_OnDeleteArray.AddAt(const APosition : Integer) : TOnDelete;
begin
  Result := TOnDelete(inherited AddAt(APosition));
end;

{ TNavigationProperty_AnnotationArray }

function TNavigationProperty_AnnotationArray.GetItem(AIndex: Integer): Annotation_Type;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure TNavigationProperty_AnnotationArray.SetItem(AIndex: Integer;const AValue: Annotation_Type);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function TNavigationProperty_AnnotationArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure TNavigationProperty_AnnotationArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('Annotation',TypeInfo(Annotation_Type),FData[AIndex]);
end;

procedure TNavigationProperty_AnnotationArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'Annotation';
  AStore.Get(TypeInfo(Annotation_Type),sName,FData[AIndex]);
end;

class function TNavigationProperty_AnnotationArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(Annotation_Type);
end;

procedure TNavigationProperty_AnnotationArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

procedure TNavigationProperty_AnnotationArray.Assign(Source: TPersistent);
var
  src : TNavigationProperty_AnnotationArray;
  i, c : Integer;
begin
  if Assigned(Source) and Source.InheritsFrom(TNavigationProperty_AnnotationArray) then begin
    src := TNavigationProperty_AnnotationArray(Source);
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

{ TReferentialConstraint_AnnotationArray }

function TReferentialConstraint_AnnotationArray.GetItem(AIndex: Integer): Annotation_Type;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure TReferentialConstraint_AnnotationArray.SetItem(AIndex: Integer;const AValue: Annotation_Type);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function TReferentialConstraint_AnnotationArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure TReferentialConstraint_AnnotationArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('Annotation',TypeInfo(Annotation_Type),FData[AIndex]);
end;

procedure TReferentialConstraint_AnnotationArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'Annotation';
  AStore.Get(TypeInfo(Annotation_Type),sName,FData[AIndex]);
end;

class function TReferentialConstraint_AnnotationArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(Annotation_Type);
end;

procedure TReferentialConstraint_AnnotationArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

procedure TReferentialConstraint_AnnotationArray.Assign(Source: TPersistent);
var
  src : TReferentialConstraint_AnnotationArray;
  i, c : Integer;
begin
  if Assigned(Source) and Source.InheritsFrom(TReferentialConstraint_AnnotationArray) then begin
    src := TReferentialConstraint_AnnotationArray(Source);
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

{ TOnDelete_AnnotationArray }

function TOnDelete_AnnotationArray.GetItem(AIndex: Integer): Annotation_Type;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure TOnDelete_AnnotationArray.SetItem(AIndex: Integer;const AValue: Annotation_Type);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function TOnDelete_AnnotationArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure TOnDelete_AnnotationArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('Annotation',TypeInfo(Annotation_Type),FData[AIndex]);
end;

procedure TOnDelete_AnnotationArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'Annotation';
  AStore.Get(TypeInfo(Annotation_Type),sName,FData[AIndex]);
end;

class function TOnDelete_AnnotationArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(Annotation_Type);
end;

procedure TOnDelete_AnnotationArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

procedure TOnDelete_AnnotationArray.Assign(Source: TPersistent);
var
  src : TOnDelete_AnnotationArray;
  i, c : Integer;
begin
  if Assigned(Source) and Source.InheritsFrom(TOnDelete_AnnotationArray) then begin
    src := TOnDelete_AnnotationArray(Source);
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

{ TEnumType_MemberArray }

function TEnumType_MemberArray.GetItem(AIndex: Integer): TEnumTypeMember;
begin
  Result := TEnumTypeMember(Inherited GetItem(AIndex));
end;

class function TEnumType_MemberArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TEnumTypeMember;
end;

function TEnumType_MemberArray.Add() : TEnumTypeMember;
begin
  Result := TEnumTypeMember(inherited Add());
end;

function TEnumType_MemberArray.AddAt(const APosition : Integer) : TEnumTypeMember;
begin
  Result := TEnumTypeMember(inherited AddAt(APosition));
end;

{ TEnumType_AnnotationArray }

function TEnumType_AnnotationArray.GetItem(AIndex: Integer): Annotation_Type;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure TEnumType_AnnotationArray.SetItem(AIndex: Integer;const AValue: Annotation_Type);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function TEnumType_AnnotationArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure TEnumType_AnnotationArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('Annotation',TypeInfo(Annotation_Type),FData[AIndex]);
end;

procedure TEnumType_AnnotationArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'Annotation';
  AStore.Get(TypeInfo(Annotation_Type),sName,FData[AIndex]);
end;

class function TEnumType_AnnotationArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(Annotation_Type);
end;

procedure TEnumType_AnnotationArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

procedure TEnumType_AnnotationArray.Assign(Source: TPersistent);
var
  src : TEnumType_AnnotationArray;
  i, c : Integer;
begin
  if Assigned(Source) and Source.InheritsFrom(TEnumType_AnnotationArray) then begin
    src := TEnumType_AnnotationArray(Source);
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

{ TEnumTypeMember_AnnotationArray }

function TEnumTypeMember_AnnotationArray.GetItem(AIndex: Integer): Annotation_Type;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure TEnumTypeMember_AnnotationArray.SetItem(AIndex: Integer;const AValue: Annotation_Type);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function TEnumTypeMember_AnnotationArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure TEnumTypeMember_AnnotationArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('Annotation',TypeInfo(Annotation_Type),FData[AIndex]);
end;

procedure TEnumTypeMember_AnnotationArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'Annotation';
  AStore.Get(TypeInfo(Annotation_Type),sName,FData[AIndex]);
end;

class function TEnumTypeMember_AnnotationArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(Annotation_Type);
end;

procedure TEnumTypeMember_AnnotationArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

procedure TEnumTypeMember_AnnotationArray.Assign(Source: TPersistent);
var
  src : TEnumTypeMember_AnnotationArray;
  i, c : Integer;
begin
  if Assigned(Source) and Source.InheritsFrom(TEnumTypeMember_AnnotationArray) then begin
    src := TEnumTypeMember_AnnotationArray(Source);
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

{ TActionFunctionReturnType_AnnotationArray }

function TActionFunctionReturnType_AnnotationArray.GetItem(AIndex: Integer): Annotation_Type;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure TActionFunctionReturnType_AnnotationArray.SetItem(AIndex: Integer;const AValue: Annotation_Type);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function TActionFunctionReturnType_AnnotationArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure TActionFunctionReturnType_AnnotationArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('Annotation',TypeInfo(Annotation_Type),FData[AIndex]);
end;

procedure TActionFunctionReturnType_AnnotationArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'Annotation';
  AStore.Get(TypeInfo(Annotation_Type),sName,FData[AIndex]);
end;

class function TActionFunctionReturnType_AnnotationArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(Annotation_Type);
end;

procedure TActionFunctionReturnType_AnnotationArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

procedure TActionFunctionReturnType_AnnotationArray.Assign(Source: TPersistent);
var
  src : TActionFunctionReturnType_AnnotationArray;
  i, c : Integer;
begin
  if Assigned(Source) and Source.InheritsFrom(TActionFunctionReturnType_AnnotationArray) then begin
    src := TActionFunctionReturnType_AnnotationArray(Source);
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

{ TAction_ParameterArray }

function TAction_ParameterArray.GetItem(AIndex: Integer): TActionFunctionParameter;
begin
  Result := TActionFunctionParameter(Inherited GetItem(AIndex));
end;

class function TAction_ParameterArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TActionFunctionParameter;
end;

function TAction_ParameterArray.Add() : TActionFunctionParameter;
begin
  Result := TActionFunctionParameter(inherited Add());
end;

function TAction_ParameterArray.AddAt(const APosition : Integer) : TActionFunctionParameter;
begin
  Result := TActionFunctionParameter(inherited AddAt(APosition));
end;

{ TAction_AnnotationArray }

function TAction_AnnotationArray.GetItem(AIndex: Integer): Annotation_Type;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure TAction_AnnotationArray.SetItem(AIndex: Integer;const AValue: Annotation_Type);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function TAction_AnnotationArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure TAction_AnnotationArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('Annotation',TypeInfo(Annotation_Type),FData[AIndex]);
end;

procedure TAction_AnnotationArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'Annotation';
  AStore.Get(TypeInfo(Annotation_Type),sName,FData[AIndex]);
end;

class function TAction_AnnotationArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(Annotation_Type);
end;

procedure TAction_AnnotationArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

procedure TAction_AnnotationArray.Assign(Source: TPersistent);
var
  src : TAction_AnnotationArray;
  i, c : Integer;
begin
  if Assigned(Source) and Source.InheritsFrom(TAction_AnnotationArray) then begin
    src := TAction_AnnotationArray(Source);
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

{ TFunction_ParameterArray }

function TFunction_ParameterArray.GetItem(AIndex: Integer): TActionFunctionParameter;
begin
  Result := TActionFunctionParameter(Inherited GetItem(AIndex));
end;

class function TFunction_ParameterArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TActionFunctionParameter;
end;

function TFunction_ParameterArray.Add() : TActionFunctionParameter;
begin
  Result := TActionFunctionParameter(inherited Add());
end;

function TFunction_ParameterArray.AddAt(const APosition : Integer) : TActionFunctionParameter;
begin
  Result := TActionFunctionParameter(inherited AddAt(APosition));
end;

{ TFunction_AnnotationArray }

function TFunction_AnnotationArray.GetItem(AIndex: Integer): Annotation_Type;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure TFunction_AnnotationArray.SetItem(AIndex: Integer;const AValue: Annotation_Type);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function TFunction_AnnotationArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure TFunction_AnnotationArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('Annotation',TypeInfo(Annotation_Type),FData[AIndex]);
end;

procedure TFunction_AnnotationArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'Annotation';
  AStore.Get(TypeInfo(Annotation_Type),sName,FData[AIndex]);
end;

class function TFunction_AnnotationArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(Annotation_Type);
end;

procedure TFunction_AnnotationArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

procedure TFunction_AnnotationArray.Assign(Source: TPersistent);
var
  src : TFunction_AnnotationArray;
  i, c : Integer;
begin
  if Assigned(Source) and Source.InheritsFrom(TFunction_AnnotationArray) then begin
    src := TFunction_AnnotationArray(Source);
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

{ TActionFunctionParameter_AnnotationArray }

function TActionFunctionParameter_AnnotationArray.GetItem(AIndex: Integer): Annotation_Type;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure TActionFunctionParameter_AnnotationArray.SetItem(AIndex: Integer;const AValue: Annotation_Type);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function TActionFunctionParameter_AnnotationArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure TActionFunctionParameter_AnnotationArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('Annotation',TypeInfo(Annotation_Type),FData[AIndex]);
end;

procedure TActionFunctionParameter_AnnotationArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'Annotation';
  AStore.Get(TypeInfo(Annotation_Type),sName,FData[AIndex]);
end;

class function TActionFunctionParameter_AnnotationArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(Annotation_Type);
end;

procedure TActionFunctionParameter_AnnotationArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

procedure TActionFunctionParameter_AnnotationArray.Assign(Source: TPersistent);
var
  src : TActionFunctionParameter_AnnotationArray;
  i, c : Integer;
begin
  if Assigned(Source) and Source.InheritsFrom(TActionFunctionParameter_AnnotationArray) then begin
    src := TActionFunctionParameter_AnnotationArray(Source);
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

{ TTerm_AnnotationArray }

function TTerm_AnnotationArray.GetItem(AIndex: Integer): Annotation_Type;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure TTerm_AnnotationArray.SetItem(AIndex: Integer;const AValue: Annotation_Type);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function TTerm_AnnotationArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure TTerm_AnnotationArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('Annotation',TypeInfo(Annotation_Type),FData[AIndex]);
end;

procedure TTerm_AnnotationArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'Annotation';
  AStore.Get(TypeInfo(Annotation_Type),sName,FData[AIndex]);
end;

class function TTerm_AnnotationArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(Annotation_Type);
end;

procedure TTerm_AnnotationArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

procedure TTerm_AnnotationArray.Assign(Source: TPersistent);
var
  src : TTerm_AnnotationArray;
  i, c : Integer;
begin
  if Assigned(Source) and Source.InheritsFrom(TTerm_AnnotationArray) then begin
    src := TTerm_AnnotationArray(Source);
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

{ TAnnotations_AnnotationArray }

function TAnnotations_AnnotationArray.GetItem(AIndex: Integer): Annotation_Type;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure TAnnotations_AnnotationArray.SetItem(AIndex: Integer;const AValue: Annotation_Type);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function TAnnotations_AnnotationArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure TAnnotations_AnnotationArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('Annotation',TypeInfo(Annotation_Type),FData[AIndex]);
end;

procedure TAnnotations_AnnotationArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'Annotation';
  AStore.Get(TypeInfo(Annotation_Type),sName,FData[AIndex]);
end;

class function TAnnotations_AnnotationArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(Annotation_Type);
end;

procedure TAnnotations_AnnotationArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

procedure TAnnotations_AnnotationArray.Assign(Source: TPersistent);
var
  src : TAnnotations_AnnotationArray;
  i, c : Integer;
begin
  if Assigned(Source) and Source.InheritsFrom(TAnnotations_AnnotationArray) then begin
    src := TAnnotations_AnnotationArray(Source);
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

{ Annotation_AnnotationArray }

function Annotation_AnnotationArray.GetItem(AIndex: Integer): Annotation_Type;
begin
  CheckIndex(AIndex);
  Result := FData[AIndex];
end;

procedure Annotation_AnnotationArray.SetItem(AIndex: Integer;const AValue: Annotation_Type);
begin
  CheckIndex(AIndex);
  FData[AIndex] := AValue;
end;

function Annotation_AnnotationArray.GetLength(): Integer;
begin
  Result := System.Length(FData);
end;

procedure Annotation_AnnotationArray.SaveItem(AStore: IFormatterBase;const AName: String; const AIndex: Integer);
begin
  AStore.Put('Annotation',TypeInfo(Annotation_Type),FData[AIndex]);
end;

procedure Annotation_AnnotationArray.LoadItem(AStore: IFormatterBase;const AIndex: Integer);
var
  sName : string;
begin
  sName := 'Annotation';
  AStore.Get(TypeInfo(Annotation_Type),sName,FData[AIndex]);
end;

class function Annotation_AnnotationArray.GetItemTypeInfo(): PTypeInfo;
begin
  Result := TypeInfo(Annotation_Type);
end;

procedure Annotation_AnnotationArray.SetLength(const ANewSize: Integer);
var
  i : Integer;
begin
  if ( ANewSize < 0 ) then
    i := 0
  else
    i := ANewSize;
  System.SetLength(FData,i);
end;

procedure Annotation_AnnotationArray.Assign(Source: TPersistent);
var
  src : Annotation_AnnotationArray;
  i, c : Integer;
begin
  if Assigned(Source) and Source.InheritsFrom(Annotation_AnnotationArray) then begin
    src := Annotation_AnnotationArray(Source);
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

{ TApplyExpression_AnnotationArray }

function TApplyExpression_AnnotationArray.GetItem(AIndex: Integer): Annotation_Type;
begin
  Result := Annotation_Type(Inherited GetItem(AIndex));
end;

class function TApplyExpression_AnnotationArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= Annotation_Type;
end;

function TApplyExpression_AnnotationArray.Add() : Annotation_Type;
begin
  Result := Annotation_Type(inherited Add());
end;

function TApplyExpression_AnnotationArray.AddAt(const APosition : Integer) : Annotation_Type;
begin
  Result := Annotation_Type(inherited AddAt(APosition));
end;

{ TCastOrIsOfExpression_AnnotationArray }

function TCastOrIsOfExpression_AnnotationArray.GetItem(AIndex: Integer): Annotation_Type;
begin
  Result := Annotation_Type(Inherited GetItem(AIndex));
end;

class function TCastOrIsOfExpression_AnnotationArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= Annotation_Type;
end;

function TCastOrIsOfExpression_AnnotationArray.Add() : Annotation_Type;
begin
  Result := Annotation_Type(inherited Add());
end;

function TCastOrIsOfExpression_AnnotationArray.AddAt(const APosition : Integer) : Annotation_Type;
begin
  Result := Annotation_Type(inherited AddAt(APosition));
end;

{ TIfExpression_AnnotationArray }

function TIfExpression_AnnotationArray.GetItem(AIndex: Integer): Annotation_Type;
begin
  Result := Annotation_Type(Inherited GetItem(AIndex));
end;

class function TIfExpression_AnnotationArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= Annotation_Type;
end;

function TIfExpression_AnnotationArray.Add() : Annotation_Type;
begin
  Result := Annotation_Type(inherited Add());
end;

function TIfExpression_AnnotationArray.AddAt(const APosition : Integer) : Annotation_Type;
begin
  Result := Annotation_Type(inherited AddAt(APosition));
end;

{ TOneChildExpression_AnnotationArray }

function TOneChildExpression_AnnotationArray.GetItem(AIndex: Integer): Annotation_Type;
begin
  Result := Annotation_Type(Inherited GetItem(AIndex));
end;

class function TOneChildExpression_AnnotationArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= Annotation_Type;
end;

function TOneChildExpression_AnnotationArray.Add() : Annotation_Type;
begin
  Result := Annotation_Type(inherited Add());
end;

function TOneChildExpression_AnnotationArray.AddAt(const APosition : Integer) : Annotation_Type;
begin
  Result := Annotation_Type(inherited AddAt(APosition));
end;

{ TTwoChildrenExpression_AnnotationArray }

function TTwoChildrenExpression_AnnotationArray.GetItem(AIndex: Integer): Annotation_Type;
begin
  Result := Annotation_Type(Inherited GetItem(AIndex));
end;

class function TTwoChildrenExpression_AnnotationArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= Annotation_Type;
end;

function TTwoChildrenExpression_AnnotationArray.Add() : Annotation_Type;
begin
  Result := Annotation_Type(inherited Add());
end;

function TTwoChildrenExpression_AnnotationArray.AddAt(const APosition : Integer) : Annotation_Type;
begin
  Result := Annotation_Type(inherited AddAt(APosition));
end;

{ TLabeledElementExpression_AnnotationArray }

function TLabeledElementExpression_AnnotationArray.GetItem(AIndex: Integer): Annotation_Type;
begin
  Result := Annotation_Type(Inherited GetItem(AIndex));
end;

class function TLabeledElementExpression_AnnotationArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= Annotation_Type;
end;

function TLabeledElementExpression_AnnotationArray.Add() : Annotation_Type;
begin
  Result := Annotation_Type(inherited Add());
end;

function TLabeledElementExpression_AnnotationArray.AddAt(const APosition : Integer) : Annotation_Type;
begin
  Result := Annotation_Type(inherited AddAt(APosition));
end;

{ TNullExpression }

function TNullExpression.GetItem(AIndex: Integer): Annotation_Type;
begin
  Result := Annotation_Type(Inherited GetItem(AIndex));
end;

class function TNullExpression.GetItemClass(): TBaseRemotableClass;
begin
  Result:= Annotation_Type;
end;

function TNullExpression.Add() : Annotation_Type;
begin
  Result := Annotation_Type(inherited Add());
end;

function TNullExpression.AddAt(const APosition : Integer) : Annotation_Type;
begin
  Result := Annotation_Type(inherited AddAt(APosition));
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

function TRecordExpression_PropertyValueArray.Add() : TPropertyValue;
begin
  Result := TPropertyValue(inherited Add());
end;

function TRecordExpression_PropertyValueArray.AddAt(const APosition : Integer) : TPropertyValue;
begin
  Result := TPropertyValue(inherited AddAt(APosition));
end;

{ TRecordExpression_AnnotationArray }

function TRecordExpression_AnnotationArray.GetItem(AIndex: Integer): Annotation_Type;
begin
  Result := Annotation_Type(Inherited GetItem(AIndex));
end;

class function TRecordExpression_AnnotationArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= Annotation_Type;
end;

function TRecordExpression_AnnotationArray.Add() : Annotation_Type;
begin
  Result := Annotation_Type(inherited Add());
end;

function TRecordExpression_AnnotationArray.AddAt(const APosition : Integer) : Annotation_Type;
begin
  Result := Annotation_Type(inherited AddAt(APosition));
end;

{ TPropertyValue_AnnotationArray }

function TPropertyValue_AnnotationArray.GetItem(AIndex: Integer): Annotation_Type;
begin
  Result := Annotation_Type(Inherited GetItem(AIndex));
end;

class function TPropertyValue_AnnotationArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= Annotation_Type;
end;

function TPropertyValue_AnnotationArray.Add() : Annotation_Type;
begin
  Result := Annotation_Type(inherited Add());
end;

function TPropertyValue_AnnotationArray.AddAt(const APosition : Integer) : Annotation_Type;
begin
  Result := Annotation_Type(inherited AddAt(APosition));
end;

{ TEntityContainer_EntitySetArray }

function TEntityContainer_EntitySetArray.GetItem(AIndex: Integer): TEntitySet;
begin
  Result := TEntitySet(Inherited GetItem(AIndex));
end;

class function TEntityContainer_EntitySetArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TEntitySet;
end;

function TEntityContainer_EntitySetArray.Add() : TEntitySet;
begin
  Result := TEntitySet(inherited Add());
end;

function TEntityContainer_EntitySetArray.AddAt(const APosition : Integer) : TEntitySet;
begin
  Result := TEntitySet(inherited AddAt(APosition));
end;

{ TEntityContainer_ActionImportArray }

function TEntityContainer_ActionImportArray.GetItem(AIndex: Integer): TActionImport;
begin
  Result := TActionImport(Inherited GetItem(AIndex));
end;

class function TEntityContainer_ActionImportArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TActionImport;
end;

function TEntityContainer_ActionImportArray.Add() : TActionImport;
begin
  Result := TActionImport(inherited Add());
end;

function TEntityContainer_ActionImportArray.AddAt(const APosition : Integer) : TActionImport;
begin
  Result := TActionImport(inherited AddAt(APosition));
end;

{ TEntityContainer_FunctionImportArray }

function TEntityContainer_FunctionImportArray.GetItem(AIndex: Integer): TFunctionImport;
begin
  Result := TFunctionImport(Inherited GetItem(AIndex));
end;

class function TEntityContainer_FunctionImportArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TFunctionImport;
end;

function TEntityContainer_FunctionImportArray.Add() : TFunctionImport;
begin
  Result := TFunctionImport(inherited Add());
end;

function TEntityContainer_FunctionImportArray.AddAt(const APosition : Integer) : TFunctionImport;
begin
  Result := TFunctionImport(inherited AddAt(APosition));
end;

{ TEntityContainer_SingletonArray }

function TEntityContainer_SingletonArray.GetItem(AIndex: Integer): TSingleton;
begin
  Result := TSingleton(Inherited GetItem(AIndex));
end;

class function TEntityContainer_SingletonArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TSingleton;
end;

function TEntityContainer_SingletonArray.Add() : TSingleton;
begin
  Result := TSingleton(inherited Add());
end;

function TEntityContainer_SingletonArray.AddAt(const APosition : Integer) : TSingleton;
begin
  Result := TSingleton(inherited AddAt(APosition));
end;

{ TEntityContainer_AnnotationArray }

function TEntityContainer_AnnotationArray.GetItem(AIndex: Integer): Annotation_Type;
begin
  Result := Annotation_Type(Inherited GetItem(AIndex));
end;

class function TEntityContainer_AnnotationArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= Annotation_Type;
end;

function TEntityContainer_AnnotationArray.Add() : Annotation_Type;
begin
  Result := Annotation_Type(inherited Add());
end;

function TEntityContainer_AnnotationArray.AddAt(const APosition : Integer) : Annotation_Type;
begin
  Result := Annotation_Type(inherited AddAt(APosition));
end;

{ TEntitySet_NavigationPropertyBindingArray }

function TEntitySet_NavigationPropertyBindingArray.GetItem(AIndex: Integer): TNavigationPropertyBinding;
begin
  Result := TNavigationPropertyBinding(Inherited GetItem(AIndex));
end;

class function TEntitySet_NavigationPropertyBindingArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TNavigationPropertyBinding;
end;

function TEntitySet_NavigationPropertyBindingArray.Add() : TNavigationPropertyBinding;
begin
  Result := TNavigationPropertyBinding(inherited Add());
end;

function TEntitySet_NavigationPropertyBindingArray.AddAt(const APosition : Integer) : TNavigationPropertyBinding;
begin
  Result := TNavigationPropertyBinding(inherited AddAt(APosition));
end;

{ TEntitySet_AnnotationArray }

function TEntitySet_AnnotationArray.GetItem(AIndex: Integer): Annotation_Type;
begin
  Result := Annotation_Type(Inherited GetItem(AIndex));
end;

class function TEntitySet_AnnotationArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= Annotation_Type;
end;

function TEntitySet_AnnotationArray.Add() : Annotation_Type;
begin
  Result := Annotation_Type(inherited Add());
end;

function TEntitySet_AnnotationArray.AddAt(const APosition : Integer) : Annotation_Type;
begin
  Result := Annotation_Type(inherited AddAt(APosition));
end;

{ TSingleton_NavigationPropertyBindingArray }

function TSingleton_NavigationPropertyBindingArray.GetItem(AIndex: Integer): TNavigationPropertyBinding;
begin
  Result := TNavigationPropertyBinding(Inherited GetItem(AIndex));
end;

class function TSingleton_NavigationPropertyBindingArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= TNavigationPropertyBinding;
end;

function TSingleton_NavigationPropertyBindingArray.Add() : TNavigationPropertyBinding;
begin
  Result := TNavigationPropertyBinding(inherited Add());
end;

function TSingleton_NavigationPropertyBindingArray.AddAt(const APosition : Integer) : TNavigationPropertyBinding;
begin
  Result := TNavigationPropertyBinding(inherited AddAt(APosition));
end;

{ TSingleton_AnnotationArray }

function TSingleton_AnnotationArray.GetItem(AIndex: Integer): Annotation_Type;
begin
  Result := Annotation_Type(Inherited GetItem(AIndex));
end;

class function TSingleton_AnnotationArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= Annotation_Type;
end;

function TSingleton_AnnotationArray.Add() : Annotation_Type;
begin
  Result := Annotation_Type(inherited Add());
end;

function TSingleton_AnnotationArray.AddAt(const APosition : Integer) : Annotation_Type;
begin
  Result := Annotation_Type(inherited AddAt(APosition));
end;

{ TActionImport_AnnotationArray }

function TActionImport_AnnotationArray.GetItem(AIndex: Integer): Annotation_Type;
begin
  Result := Annotation_Type(Inherited GetItem(AIndex));
end;

class function TActionImport_AnnotationArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= Annotation_Type;
end;

function TActionImport_AnnotationArray.Add() : Annotation_Type;
begin
  Result := Annotation_Type(inherited Add());
end;

function TActionImport_AnnotationArray.AddAt(const APosition : Integer) : Annotation_Type;
begin
  Result := Annotation_Type(inherited AddAt(APosition));
end;

{ TFunctionImport_AnnotationArray }

function TFunctionImport_AnnotationArray.GetItem(AIndex: Integer): Annotation_Type;
begin
  Result := Annotation_Type(Inherited GetItem(AIndex));
end;

class function TFunctionImport_AnnotationArray.GetItemClass(): TBaseRemotableClass;
begin
  Result:= Annotation_Type;
end;

function TFunctionImport_AnnotationArray.Add() : Annotation_Type;
begin
  Result := Annotation_Type(inherited Add());
end;

function TFunctionImport_AnnotationArray.AddAt(const APosition : Integer) : Annotation_Type;
begin
  Result := Annotation_Type(inherited AddAt(APosition));
end;


var
  typeRegistryInstance : TTypeRegistry = nil;
initialization
  typeRegistryInstance := GetTypeRegistry();
  Schema.RegisterAttributeProperty('Namespace');
  Schema.RegisterAttributeProperty('Alias');
  TTypeAttributes.RegisterAttributeProperty('Name');
  TDerivableTypeAttributes.RegisterAttributeProperty('BaseType');
  TDerivableTypeAttributes.RegisterAttributeProperty('_Abstract');
  TDerivableTypeAttributes.RegisterAttributeProperty('Name');
  TEntityType.RegisterAttributeProperty('OpenType');
  TEntityType.RegisterAttributeProperty('HasStream');
  TEntityType.RegisterAttributeProperty('BaseType');
  TEntityType.RegisterAttributeProperty('_Abstract');
  TEntityType.RegisterAttributeProperty('Name');
  TPropertyRef.RegisterAttributeProperty('Name');
  TPropertyRef.RegisterAttributeProperty('Alias');
  TComplexType.RegisterAttributeProperty('OpenType');
  TComplexType.RegisterAttributeProperty('BaseType');
  TComplexType.RegisterAttributeProperty('_Abstract');
  TComplexType.RegisterAttributeProperty('Name');
  TFacetAttributes.RegisterAttributeProperty('MaxLength');
  TFacetAttributes.RegisterAttributeProperty('Precision');
  TFacetAttributes.RegisterAttributeProperty('Scale');
  TFacetAttributes.RegisterAttributeProperty('SRID');
  TPropertyFacetAttributes.RegisterAttributeProperty('Unicode');
  TCommonPropertyAttributes.RegisterAttributeProperty('Name');
  TCommonPropertyAttributes.RegisterAttributeProperty('_Type');
  TCommonPropertyAttributes.RegisterAttributeProperty('Nullable');
  TCommonPropertyAttributes.RegisterAttributeProperty('DefaultValue');
  TCommonPropertyAttributes.RegisterAttributeProperty('MaxLength');
  TCommonPropertyAttributes.RegisterAttributeProperty('Precision');
  TCommonPropertyAttributes.RegisterAttributeProperty('Scale');
  TCommonPropertyAttributes.RegisterAttributeProperty('SRID');
  TCommonPropertyAttributes.RegisterAttributeProperty('Unicode');
  TProperty.RegisterAttributeProperty('Name');
  TProperty.RegisterAttributeProperty('_Type');
  TProperty.RegisterAttributeProperty('Nullable');
  TProperty.RegisterAttributeProperty('DefaultValue');
  TProperty.RegisterAttributeProperty('MaxLength');
  TProperty.RegisterAttributeProperty('Precision');
  TProperty.RegisterAttributeProperty('Scale');
  TProperty.RegisterAttributeProperty('SRID');
  TProperty.RegisterAttributeProperty('Unicode');
  TTypeDefinition.RegisterAttributeProperty('Name');
  TTypeDefinition.RegisterAttributeProperty('UnderlyingType');
  TTypeDefinition.RegisterAttributeProperty('MaxLength');
  TTypeDefinition.RegisterAttributeProperty('Precision');
  TTypeDefinition.RegisterAttributeProperty('Scale');
  TTypeDefinition.RegisterAttributeProperty('SRID');
  TTypeDefinition.RegisterAttributeProperty('Unicode');
  TNavigationProperty.RegisterAttributeProperty('Name');
  TNavigationProperty.RegisterAttributeProperty('_Type');
  TNavigationProperty.RegisterAttributeProperty('Nullable');
  TNavigationProperty.RegisterAttributeProperty('Partner');
  TNavigationProperty.RegisterAttributeProperty('ContainsTarget');
  TReferentialConstraint.RegisterAttributeProperty('_Property');
  TReferentialConstraint.RegisterAttributeProperty('ReferencedProperty');
  TOnDelete.RegisterAttributeProperty('Action');
  TEnumType.RegisterAttributeProperty('IsFlags');
  TEnumType.RegisterAttributeProperty('UnderlyingType');
  TEnumType.RegisterAttributeProperty('Name');
  TEnumTypeMember.RegisterAttributeProperty('Name');
  TEnumTypeMember.RegisterAttributeProperty('Value');
  TActionFunctionReturnType.RegisterAttributeProperty('_Type');
  TActionFunctionReturnType.RegisterAttributeProperty('Nullable');
  TActionFunctionReturnType.RegisterAttributeProperty('MaxLength');
  TActionFunctionReturnType.RegisterAttributeProperty('Precision');
  TActionFunctionReturnType.RegisterAttributeProperty('Scale');
  TActionFunctionReturnType.RegisterAttributeProperty('SRID');
  TActionAttributes.RegisterAttributeProperty('Name');
  TActionAttributes.RegisterAttributeProperty('EntitySetPath');
  TActionAttributes.RegisterAttributeProperty('IsBound');
  TAction.RegisterAttributeProperty('Name');
  TAction.RegisterAttributeProperty('EntitySetPath');
  TAction.RegisterAttributeProperty('IsBound');
  TFunctionAttributes.RegisterAttributeProperty('Name');
  TFunctionAttributes.RegisterAttributeProperty('EntitySetPath');
  TFunctionAttributes.RegisterAttributeProperty('IsBound');
  TFunctionAttributes.RegisterAttributeProperty('IsComposable');
  TFunction.RegisterAttributeProperty('Name');
  TFunction.RegisterAttributeProperty('EntitySetPath');
  TFunction.RegisterAttributeProperty('IsBound');
  TFunction.RegisterAttributeProperty('IsComposable');
  TActionFunctionParameterAttributes.RegisterAttributeProperty('Name');
  TActionFunctionParameterAttributes.RegisterAttributeProperty('_Type');
  TActionFunctionParameterAttributes.RegisterAttributeProperty('Nullable');
  TActionFunctionParameterAttributes.RegisterAttributeProperty('MaxLength');
  TActionFunctionParameterAttributes.RegisterAttributeProperty('Precision');
  TActionFunctionParameterAttributes.RegisterAttributeProperty('Scale');
  TActionFunctionParameterAttributes.RegisterAttributeProperty('SRID');
  TActionFunctionParameter.RegisterAttributeProperty('Name');
  TActionFunctionParameter.RegisterAttributeProperty('_Type');
  TActionFunctionParameter.RegisterAttributeProperty('Nullable');
  TActionFunctionParameter.RegisterAttributeProperty('MaxLength');
  TActionFunctionParameter.RegisterAttributeProperty('Precision');
  TActionFunctionParameter.RegisterAttributeProperty('Scale');
  TActionFunctionParameter.RegisterAttributeProperty('SRID');
  TTerm.RegisterAttributeProperty('Name');
  TTerm.RegisterAttributeProperty('_Type');
  TTerm.RegisterAttributeProperty('BaseTerm');
  TTerm.RegisterAttributeProperty('Nullable');
  TTerm.RegisterAttributeProperty('DefaultValue');
  TTerm.RegisterAttributeProperty('AppliesTo');
  TTerm.RegisterAttributeProperty('MaxLength');
  TTerm.RegisterAttributeProperty('Precision');
  TTerm.RegisterAttributeProperty('Scale');
  TTerm.RegisterAttributeProperty('SRID');
  TAnnotations.RegisterAttributeProperty('Target');
  TAnnotations.RegisterAttributeProperty('Qualifier');
  GInlineExpressions.RegisterAttributeProperty('Binary');
  GInlineExpressions.RegisterAttributeProperty('Bool');
  GInlineExpressions.RegisterAttributeProperty('Date');
  GInlineExpressions.RegisterAttributeProperty('DateTimeOffset');
  GInlineExpressions.RegisterAttributeProperty('Decimal');
  GInlineExpressions.RegisterAttributeProperty('Duration');
  GInlineExpressions.RegisterAttributeProperty('EnumMember');
  GInlineExpressions.RegisterAttributeProperty('Float');
  GInlineExpressions.RegisterAttributeProperty('Guid');
  GInlineExpressions.RegisterAttributeProperty('Int');
  GInlineExpressions.RegisterAttributeProperty('_String');
  GInlineExpressions.RegisterAttributeProperty('TimeOfDay');
  GInlineExpressions.RegisterAttributeProperty('AnnotationPath');
  GInlineExpressions.RegisterAttributeProperty('NavigationPropertyPath');
  GInlineExpressions.RegisterAttributeProperty('Path');
  GInlineExpressions.RegisterAttributeProperty('PropertyPath');
  GInlineExpressions.RegisterAttributeProperty('UrlRef');
  Annotation_Type.RegisterAttributeProperty('Term');
  Annotation_Type.RegisterAttributeProperty('Qualifier');
  Annotation_Type.RegisterAttributeProperty('BinaryAtt');
  Annotation_Type.RegisterAttributeProperty('BoolAtt');
  Annotation_Type.RegisterAttributeProperty('DateAtt');
  Annotation_Type.RegisterAttributeProperty('DateTimeOffsetAtt');
  Annotation_Type.RegisterAttributeProperty('DecimalAtt');
  Annotation_Type.RegisterAttributeProperty('DurationAtt');
  Annotation_Type.RegisterAttributeProperty('EnumMemberAtt');
  Annotation_Type.RegisterAttributeProperty('FloatAtt');
  Annotation_Type.RegisterAttributeProperty('GuidAtt');
  Annotation_Type.RegisterAttributeProperty('IntAtt');
  Annotation_Type.RegisterAttributeProperty('_StringAtt');
  Annotation_Type.RegisterAttributeProperty('TimeOfDayAtt');
  Annotation_Type.RegisterAttributeProperty('AnnotationPathAtt');
  Annotation_Type.RegisterAttributeProperty('NavigationPropertyPathAtt');
  Annotation_Type.RegisterAttributeProperty('PathAtt');
  Annotation_Type.RegisterAttributeProperty('PropertyPathAtt');
  Annotation_Type.RegisterAttributeProperty('UrlRefAtt');
  TApplyExpression.RegisterAttributeProperty('_Function');
  TCastOrIsOfExpression.RegisterAttributeProperty('_Type');
  TCastOrIsOfExpression.RegisterAttributeProperty('MaxLength');
  TCastOrIsOfExpression.RegisterAttributeProperty('Precision');
  TCastOrIsOfExpression.RegisterAttributeProperty('Scale');
  TCastOrIsOfExpression.RegisterAttributeProperty('SRID');
  TLabeledElementExpression.RegisterAttributeProperty('Name');
  TLabeledElementExpression.RegisterAttributeProperty('BinaryAtt');
  TLabeledElementExpression.RegisterAttributeProperty('BoolAtt');
  TLabeledElementExpression.RegisterAttributeProperty('DateAtt');
  TLabeledElementExpression.RegisterAttributeProperty('DateTimeOffsetAtt');
  TLabeledElementExpression.RegisterAttributeProperty('DecimalAtt');
  TLabeledElementExpression.RegisterAttributeProperty('DurationAtt');
  TLabeledElementExpression.RegisterAttributeProperty('EnumMemberAtt');
  TLabeledElementExpression.RegisterAttributeProperty('FloatAtt');
  TLabeledElementExpression.RegisterAttributeProperty('GuidAtt');
  TLabeledElementExpression.RegisterAttributeProperty('IntAtt');
  TLabeledElementExpression.RegisterAttributeProperty('_StringAtt');
  TLabeledElementExpression.RegisterAttributeProperty('TimeOfDayAtt');
  TLabeledElementExpression.RegisterAttributeProperty('AnnotationPathAtt');
  TLabeledElementExpression.RegisterAttributeProperty('NavigationPropertyPathAtt');
  TLabeledElementExpression.RegisterAttributeProperty('PathAtt');
  TLabeledElementExpression.RegisterAttributeProperty('PropertyPathAtt');
  TLabeledElementExpression.RegisterAttributeProperty('UrlRefAtt');
  TRecordExpression.RegisterAttributeProperty('_Type');
  TPropertyValue.RegisterAttributeProperty('_Property');
  TPropertyValue.RegisterAttributeProperty('BinaryAtt');
  TPropertyValue.RegisterAttributeProperty('BoolAtt');
  TPropertyValue.RegisterAttributeProperty('DateAtt');
  TPropertyValue.RegisterAttributeProperty('DateTimeOffsetAtt');
  TPropertyValue.RegisterAttributeProperty('DecimalAtt');
  TPropertyValue.RegisterAttributeProperty('DurationAtt');
  TPropertyValue.RegisterAttributeProperty('EnumMemberAtt');
  TPropertyValue.RegisterAttributeProperty('FloatAtt');
  TPropertyValue.RegisterAttributeProperty('GuidAtt');
  TPropertyValue.RegisterAttributeProperty('IntAtt');
  TPropertyValue.RegisterAttributeProperty('_StringAtt');
  TPropertyValue.RegisterAttributeProperty('TimeOfDayAtt');
  TPropertyValue.RegisterAttributeProperty('AnnotationPathAtt');
  TPropertyValue.RegisterAttributeProperty('NavigationPropertyPathAtt');
  TPropertyValue.RegisterAttributeProperty('PathAtt');
  TPropertyValue.RegisterAttributeProperty('PropertyPathAtt');
  TPropertyValue.RegisterAttributeProperty('UrlRefAtt');
  TEntityContainer.RegisterAttributeProperty('Name');
  TEntityContainer.RegisterAttributeProperty('Extends');
  TEntitySetAttributes.RegisterAttributeProperty('Name');
  TEntitySetAttributes.RegisterAttributeProperty('EntityType');
  TEntitySetAttributes.RegisterAttributeProperty('IncludeInServiceDocument');
  TEntitySet.RegisterAttributeProperty('Name');
  TEntitySet.RegisterAttributeProperty('EntityType');
  TEntitySet.RegisterAttributeProperty('IncludeInServiceDocument');
  TNavigationPropertyBinding.RegisterAttributeProperty('Path');
  TNavigationPropertyBinding.RegisterAttributeProperty('Target');
  TSingleton.RegisterAttributeProperty('Name');
  TSingleton.RegisterAttributeProperty('_Type');
  TActionFunctionImportAttributes.RegisterAttributeProperty('Name');
  TActionFunctionImportAttributes.RegisterAttributeProperty('EntitySet');
  TActionFunctionImportAttributes.RegisterAttributeProperty('IncludeInServiceDocument');
  TActionImport.RegisterAttributeProperty('Action');
  TActionImport.RegisterAttributeProperty('Name');
  TActionImport.RegisterAttributeProperty('EntitySet');
  TActionImport.RegisterAttributeProperty('IncludeInServiceDocument');
  TFunctionImport.RegisterAttributeProperty('_Function');
  TFunctionImport.RegisterAttributeProperty('Name');
  TFunctionImport.RegisterAttributeProperty('EntitySet');
  TFunctionImport.RegisterAttributeProperty('IncludeInServiceDocument');

  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TAbstractType),'TAbstractType');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TAbstractType)].RegisterExternalPropertyName('TAbstractType_Edm_ComplexType','Edm.ComplexType');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TAbstractType)].RegisterExternalPropertyName('TAbstractType_Edm_EntityType','Edm.EntityType');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TAbstractType)].RegisterExternalPropertyName('TAbstractType_Edm_PrimitiveType','Edm.PrimitiveType');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TAbstractType)].RegisterExternalPropertyName('TAbstractType_Edm_Geography','Edm.Geography');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TAbstractType)].RegisterExternalPropertyName('TAbstractType_Edm_Geometry','Edm.Geometry');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TAbstractType)].RegisterExternalPropertyName('TAbstractType_Edm_AnnotationPath','Edm.AnnotationPath');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TAbstractType)].RegisterExternalPropertyName('TAbstractType_Edm_NavigationPropertyPath','Edm.NavigationPropertyPath');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TAbstractType)].RegisterExternalPropertyName('TAbstractType_Edm_PropertyPath','Edm.PropertyPath');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TAbstractType)].RegisterExternalPropertyName('TAbstractType_Collection_Edm_ComplexType','Collection(Edm.ComplexType)');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TAbstractType)].RegisterExternalPropertyName('TAbstractType_Collection_Edm_EntityType','Collection(Edm.EntityType)');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TAbstractType)].RegisterExternalPropertyName('TAbstractType_Collection_Edm_PrimitiveType','Collection(Edm.PrimitiveType)');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TAbstractType)].RegisterExternalPropertyName('TAbstractType_Collection_Edm_Geography','Collection(Edm.Geography)');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TAbstractType)].RegisterExternalPropertyName('TAbstractType_Collection_Edm_Geometry','Collection(Edm.Geometry)');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TAbstractType)].RegisterExternalPropertyName('TAbstractType_Collection_Edm_AnnotationPath','Collection(Edm.AnnotationPath)');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TAbstractType)].RegisterExternalPropertyName('TAbstractType_Collection_Edm_NavigationPropertyPath','Collection(Edm.NavigationPropertyPath)');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TAbstractType)].RegisterExternalPropertyName('TAbstractType_Collection_Edm_PropertyPath','Collection(Edm.PropertyPath)');
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TMax),'TMax');
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TVariable),'TVariable');
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TOnDeleteAction),'TOnDeleteAction');
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(Schema),'Schema',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TTypeAttributes),'TTypeAttributes',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TDerivableTypeAttributes),'TDerivableTypeAttributes',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TEntityType),'TEntityType',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TPropertyRef),'TPropertyRef',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TComplexType),'TComplexType',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TFacetAttributes),'TFacetAttributes',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TPropertyFacetAttributes),'TPropertyFacetAttributes',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TCommonPropertyAttributes),'TCommonPropertyAttributes',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TProperty),'TProperty',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TTypeDefinition),'TTypeDefinition',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TNavigationProperty),'TNavigationProperty',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TReferentialConstraint),'TReferentialConstraint',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TOnDelete),'TOnDelete',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TEnumType),'TEnumType',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TEnumTypeMember),'TEnumTypeMember',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TActionFunctionReturnType),'TActionFunctionReturnType',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TActionAttributes),'TActionAttributes',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TAction),'TAction',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TFunctionAttributes),'TFunctionAttributes',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TFunction),'TFunction',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TActionFunctionParameterAttributes),'TActionFunctionParameterAttributes',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TActionFunctionParameter),'TActionFunctionParameter',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TTerm),'TTerm',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TAnnotations),'TAnnotations',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(GExpression),'GExpression',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(GInlineExpressions),'GInlineExpressions',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(Annotation_Type),'Annotation',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TBinaryConstantExpression),'TBinaryConstantExpression',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TBoolConstantExpression),'TBoolConstantExpression',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TDateConstantExpression),'TDateConstantExpression',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TDateTimeOffsetConstantExpression),'TDateTimeOffsetConstantExpression',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TDecimalConstantExpression),'TDecimalConstantExpression',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TDurationConstantExpression),'TDurationConstantExpression',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TFloatConstantExpression),'TFloatConstantExpression',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TGuidConstantExpression),'TGuidConstantExpression',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TIntConstantExpression),'TIntConstantExpression',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TStringConstantExpression),'TStringConstantExpression',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TTimeOfDayConstantExpression),'TTimeOfDayConstantExpression',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TApplyExpression),'TApplyExpression',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TCastOrIsOfExpression),'TCastOrIsOfExpression',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TCollectionExpression),'TCollectionExpression',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TIfExpression),'TIfExpression',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TOneChildExpression),'TOneChildExpression',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TTwoChildrenExpression),'TTwoChildrenExpression',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TLabeledElementExpression),'TLabeledElementExpression',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TLabeledElementReferenceExpression),'TLabeledElementReferenceExpression',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TPathExpression),'TPathExpression',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TRecordExpression),'TRecordExpression',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TPropertyValue),'TPropertyValue',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TEntityContainer),'TEntityContainer',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TEntitySetAttributes),'TEntitySetAttributes',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TEntitySet),'TEntitySet',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TNavigationPropertyBinding),'TNavigationPropertyBinding',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TSingleton),'TSingleton',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TActionFunctionImportAttributes),'TActionFunctionImportAttributes',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TActionImport),'TActionImport',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TFunctionImport),'TFunctionImport',[trioqualifiedElement, triounqualifiedAttribute]);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(Schema_ComplexTypeArray),'Schema_ComplexTypeArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(Schema_ComplexTypeArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(Schema_EntityTypeArray),'Schema_EntityTypeArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(Schema_EntityTypeArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(Schema_TypeDefinitionArray),'Schema_TypeDefinitionArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(Schema_TypeDefinitionArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(Schema_EnumTypeArray),'Schema_EnumTypeArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(Schema_EnumTypeArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(Schema_ActionArray),'Schema_ActionArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(Schema_ActionArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(Schema__FunctionArray),'Schema__FunctionArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(Schema__FunctionArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(Schema_TermArray),'Schema_TermArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(Schema_TermArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(Schema_AnnotationsArray),'Schema_AnnotationsArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(Schema_AnnotationsArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(Schema_EntityContainerArray),'Schema_EntityContainerArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(Schema_EntityContainerArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(Schema_AnnotationArray),'Schema_AnnotationArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(Schema_AnnotationArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TEntityType_KeyArray),'TEntityType_KeyArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TEntityType_KeyArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TEntityType__PropertyArray),'TEntityType__PropertyArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TEntityType__PropertyArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TEntityType_NavigationPropertyArray),'TEntityType_NavigationPropertyArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TEntityType_NavigationPropertyArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TEntityType_AnnotationArray),'TEntityType_AnnotationArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TEntityType_AnnotationArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TEntityKeyElement),'TEntityKeyElement');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TEntityKeyElement)].RegisterExternalPropertyName(sARRAY_ITEM,'PropertyRef');
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TComplexType__PropertyArray),'TComplexType__PropertyArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TComplexType__PropertyArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TComplexType_NavigationPropertyArray),'TComplexType_NavigationPropertyArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TComplexType_NavigationPropertyArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TComplexType_AnnotationArray),'TComplexType_AnnotationArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TComplexType_AnnotationArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TProperty_AnnotationArray),'TProperty_AnnotationArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TProperty_AnnotationArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TTypeDefinition_AnnotationArray),'TTypeDefinition_AnnotationArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TTypeDefinition_AnnotationArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TNavigationProperty_ReferentialConstraintArray),'TNavigationProperty_ReferentialConstraintArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TNavigationProperty_ReferentialConstraintArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TNavigationProperty_OnDeleteArray),'TNavigationProperty_OnDeleteArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TNavigationProperty_OnDeleteArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TNavigationProperty_AnnotationArray),'TNavigationProperty_AnnotationArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TNavigationProperty_AnnotationArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TReferentialConstraint_AnnotationArray),'TReferentialConstraint_AnnotationArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TReferentialConstraint_AnnotationArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TOnDelete_AnnotationArray),'TOnDelete_AnnotationArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TOnDelete_AnnotationArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TEnumType_MemberArray),'TEnumType_MemberArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TEnumType_MemberArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TEnumType_AnnotationArray),'TEnumType_AnnotationArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TEnumType_AnnotationArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TEnumTypeMember_AnnotationArray),'TEnumTypeMember_AnnotationArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TEnumTypeMember_AnnotationArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TActionFunctionReturnType_AnnotationArray),'TActionFunctionReturnType_AnnotationArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TActionFunctionReturnType_AnnotationArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TAction_ParameterArray),'TAction_ParameterArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TAction_ParameterArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TAction_AnnotationArray),'TAction_AnnotationArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TAction_AnnotationArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TFunction_ParameterArray),'TFunction_ParameterArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TFunction_ParameterArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TFunction_AnnotationArray),'TFunction_AnnotationArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TFunction_AnnotationArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TActionFunctionParameter_AnnotationArray),'TActionFunctionParameter_AnnotationArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TActionFunctionParameter_AnnotationArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TTerm_AnnotationArray),'TTerm_AnnotationArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TTerm_AnnotationArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TAnnotations_AnnotationArray),'TAnnotations_AnnotationArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TAnnotations_AnnotationArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(Annotation_AnnotationArray),'Annotation_AnnotationArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(Annotation_AnnotationArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TApplyExpression_AnnotationArray),'TApplyExpression_AnnotationArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TApplyExpression_AnnotationArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TCastOrIsOfExpression_AnnotationArray),'TCastOrIsOfExpression_AnnotationArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TCastOrIsOfExpression_AnnotationArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TIfExpression_AnnotationArray),'TIfExpression_AnnotationArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TIfExpression_AnnotationArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TOneChildExpression_AnnotationArray),'TOneChildExpression_AnnotationArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TOneChildExpression_AnnotationArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TTwoChildrenExpression_AnnotationArray),'TTwoChildrenExpression_AnnotationArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TTwoChildrenExpression_AnnotationArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TLabeledElementExpression_AnnotationArray),'TLabeledElementExpression_AnnotationArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TLabeledElementExpression_AnnotationArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TNullExpression),'TNullExpression');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TNullExpression)].RegisterExternalPropertyName(sARRAY_ITEM,'Annotation');
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TRecordExpression_PropertyValueArray),'TRecordExpression_PropertyValueArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TRecordExpression_PropertyValueArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TRecordExpression_AnnotationArray),'TRecordExpression_AnnotationArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TRecordExpression_AnnotationArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TPropertyValue_AnnotationArray),'TPropertyValue_AnnotationArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TPropertyValue_AnnotationArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TEntityContainer_EntitySetArray),'TEntityContainer_EntitySetArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TEntityContainer_EntitySetArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TEntityContainer_ActionImportArray),'TEntityContainer_ActionImportArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TEntityContainer_ActionImportArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TEntityContainer_FunctionImportArray),'TEntityContainer_FunctionImportArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TEntityContainer_FunctionImportArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TEntityContainer_SingletonArray),'TEntityContainer_SingletonArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TEntityContainer_SingletonArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TEntityContainer_AnnotationArray),'TEntityContainer_AnnotationArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TEntityContainer_AnnotationArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TEntitySet_NavigationPropertyBindingArray),'TEntitySet_NavigationPropertyBindingArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TEntitySet_NavigationPropertyBindingArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TEntitySet_AnnotationArray),'TEntitySet_AnnotationArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TEntitySet_AnnotationArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TSingleton_NavigationPropertyBindingArray),'TSingleton_NavigationPropertyBindingArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TSingleton_NavigationPropertyBindingArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TSingleton_AnnotationArray),'TSingleton_AnnotationArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TSingleton_AnnotationArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TActionImport_AnnotationArray),'TActionImport_AnnotationArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TActionImport_AnnotationArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);
  typeRegistryInstance.Register(sNAME_SPACE,TypeInfo(TFunctionImport_AnnotationArray),'TFunctionImport_AnnotationArray');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TFunctionImport_AnnotationArray)].RegisterExternalPropertyName(sARRAY_STYLE,sEmbedded);

  typeRegistryInstance.ItemByTypeInfo[TypeInfo(Schema)].RegisterExternalPropertyName('_Function','Function');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TDerivableTypeAttributes)].RegisterExternalPropertyName('_Abstract','Abstract');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TEntityType)].RegisterExternalPropertyName('_Property','Property');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TEntityType)].RegisterExternalPropertyName('_Abstract','Abstract');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TComplexType)].RegisterExternalPropertyName('_Property','Property');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TComplexType)].RegisterExternalPropertyName('_Abstract','Abstract');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TCommonPropertyAttributes)].RegisterExternalPropertyName('_Type','Type');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TProperty)].RegisterExternalPropertyName('_Type','Type');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TNavigationProperty)].RegisterExternalPropertyName('_Type','Type');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TReferentialConstraint)].RegisterExternalPropertyName('_Property','Property');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TActionFunctionReturnType)].RegisterExternalPropertyName('_Type','Type');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TActionFunctionParameterAttributes)].RegisterExternalPropertyName('_Type','Type');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TActionFunctionParameter)].RegisterExternalPropertyName('_Type','Type');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TTerm)].RegisterExternalPropertyName('_Type','Type');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(GExpression)].RegisterExternalPropertyName('_String','String');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(GExpression)].RegisterExternalPropertyName('_If','If');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(GExpression)].RegisterExternalPropertyName('_And','And');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(GExpression)].RegisterExternalPropertyName('_Or','Or');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(GExpression)].RegisterExternalPropertyName('_Not','Not');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(GExpression)].RegisterExternalPropertyName('_Record','Record');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(GInlineExpressions)].RegisterExternalPropertyName('_String','String');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(Annotation_Type)].RegisterExternalPropertyName('_String','String');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(Annotation_Type)].RegisterExternalPropertyName('_If','If');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(Annotation_Type)].RegisterExternalPropertyName('_And','And');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(Annotation_Type)].RegisterExternalPropertyName('_Or','Or');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(Annotation_Type)].RegisterExternalPropertyName('_Not','Not');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(Annotation_Type)].RegisterExternalPropertyName('_Record','Record');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(Annotation_Type)].RegisterExternalPropertyName('BinaryAtt','Binary');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(Annotation_Type)].RegisterExternalPropertyName('BoolAtt','Bool');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(Annotation_Type)].RegisterExternalPropertyName('DateAtt','Date');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(Annotation_Type)].RegisterExternalPropertyName('DateTimeOffsetAtt','DateTimeOffset');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(Annotation_Type)].RegisterExternalPropertyName('DecimalAtt','Decimal');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(Annotation_Type)].RegisterExternalPropertyName('DurationAtt','Duration');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(Annotation_Type)].RegisterExternalPropertyName('EnumMemberAtt','EnumMember');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(Annotation_Type)].RegisterExternalPropertyName('FloatAtt','Float');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(Annotation_Type)].RegisterExternalPropertyName('GuidAtt','Guid');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(Annotation_Type)].RegisterExternalPropertyName('IntAtt','Int');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(Annotation_Type)].RegisterExternalPropertyName('_StringAtt','String');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(Annotation_Type)].RegisterExternalPropertyName('TimeOfDayAtt','TimeOfDay');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(Annotation_Type)].RegisterExternalPropertyName('AnnotationPathAtt','AnnotationPath');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(Annotation_Type)].RegisterExternalPropertyName('NavigationPropertyPathAtt','NavigationPropertyPath');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(Annotation_Type)].RegisterExternalPropertyName('PathAtt','Path');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(Annotation_Type)].RegisterExternalPropertyName('PropertyPathAtt','PropertyPath');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(Annotation_Type)].RegisterExternalPropertyName('UrlRefAtt','UrlRef');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TApplyExpression)].RegisterExternalPropertyName('_Function','Function');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TApplyExpression)].RegisterExternalPropertyName('_String','String');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TApplyExpression)].RegisterExternalPropertyName('_If','If');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TApplyExpression)].RegisterExternalPropertyName('_And','And');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TApplyExpression)].RegisterExternalPropertyName('_Or','Or');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TApplyExpression)].RegisterExternalPropertyName('_Not','Not');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TApplyExpression)].RegisterExternalPropertyName('_Record','Record');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TCastOrIsOfExpression)].RegisterExternalPropertyName('_Type','Type');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TCastOrIsOfExpression)].RegisterExternalPropertyName('_String','String');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TCastOrIsOfExpression)].RegisterExternalPropertyName('_If','If');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TCastOrIsOfExpression)].RegisterExternalPropertyName('_And','And');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TCastOrIsOfExpression)].RegisterExternalPropertyName('_Or','Or');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TCastOrIsOfExpression)].RegisterExternalPropertyName('_Not','Not');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TCastOrIsOfExpression)].RegisterExternalPropertyName('_Record','Record');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TCollectionExpression)].RegisterExternalPropertyName('_String','String');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TCollectionExpression)].RegisterExternalPropertyName('_If','If');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TCollectionExpression)].RegisterExternalPropertyName('_And','And');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TCollectionExpression)].RegisterExternalPropertyName('_Or','Or');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TCollectionExpression)].RegisterExternalPropertyName('_Not','Not');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TCollectionExpression)].RegisterExternalPropertyName('_Record','Record');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TIfExpression)].RegisterExternalPropertyName('_String','String');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TIfExpression)].RegisterExternalPropertyName('_If','If');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TIfExpression)].RegisterExternalPropertyName('_And','And');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TIfExpression)].RegisterExternalPropertyName('_Or','Or');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TIfExpression)].RegisterExternalPropertyName('_Not','Not');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TIfExpression)].RegisterExternalPropertyName('_Record','Record');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TOneChildExpression)].RegisterExternalPropertyName('_String','String');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TOneChildExpression)].RegisterExternalPropertyName('_If','If');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TOneChildExpression)].RegisterExternalPropertyName('_And','And');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TOneChildExpression)].RegisterExternalPropertyName('_Or','Or');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TOneChildExpression)].RegisterExternalPropertyName('_Not','Not');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TOneChildExpression)].RegisterExternalPropertyName('_Record','Record');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TTwoChildrenExpression)].RegisterExternalPropertyName('_String','String');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TTwoChildrenExpression)].RegisterExternalPropertyName('_If','If');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TTwoChildrenExpression)].RegisterExternalPropertyName('_And','And');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TTwoChildrenExpression)].RegisterExternalPropertyName('_Or','Or');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TTwoChildrenExpression)].RegisterExternalPropertyName('_Not','Not');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TTwoChildrenExpression)].RegisterExternalPropertyName('_Record','Record');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TLabeledElementExpression)].RegisterExternalPropertyName('_String','String');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TLabeledElementExpression)].RegisterExternalPropertyName('_If','If');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TLabeledElementExpression)].RegisterExternalPropertyName('_And','And');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TLabeledElementExpression)].RegisterExternalPropertyName('_Or','Or');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TLabeledElementExpression)].RegisterExternalPropertyName('_Not','Not');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TLabeledElementExpression)].RegisterExternalPropertyName('_Record','Record');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TLabeledElementExpression)].RegisterExternalPropertyName('BinaryAtt','Binary');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TLabeledElementExpression)].RegisterExternalPropertyName('BoolAtt','Bool');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TLabeledElementExpression)].RegisterExternalPropertyName('DateAtt','Date');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TLabeledElementExpression)].RegisterExternalPropertyName('DateTimeOffsetAtt','DateTimeOffset');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TLabeledElementExpression)].RegisterExternalPropertyName('DecimalAtt','Decimal');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TLabeledElementExpression)].RegisterExternalPropertyName('DurationAtt','Duration');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TLabeledElementExpression)].RegisterExternalPropertyName('EnumMemberAtt','EnumMember');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TLabeledElementExpression)].RegisterExternalPropertyName('FloatAtt','Float');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TLabeledElementExpression)].RegisterExternalPropertyName('GuidAtt','Guid');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TLabeledElementExpression)].RegisterExternalPropertyName('IntAtt','Int');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TLabeledElementExpression)].RegisterExternalPropertyName('_StringAtt','String');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TLabeledElementExpression)].RegisterExternalPropertyName('TimeOfDayAtt','TimeOfDay');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TLabeledElementExpression)].RegisterExternalPropertyName('AnnotationPathAtt','AnnotationPath');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TLabeledElementExpression)].RegisterExternalPropertyName('NavigationPropertyPathAtt','NavigationPropertyPath');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TLabeledElementExpression)].RegisterExternalPropertyName('PathAtt','Path');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TLabeledElementExpression)].RegisterExternalPropertyName('PropertyPathAtt','PropertyPath');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TLabeledElementExpression)].RegisterExternalPropertyName('UrlRefAtt','UrlRef');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TRecordExpression)].RegisterExternalPropertyName('_Type','Type');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TPropertyValue)].RegisterExternalPropertyName('_Property','Property');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TPropertyValue)].RegisterExternalPropertyName('_String','String');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TPropertyValue)].RegisterExternalPropertyName('_If','If');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TPropertyValue)].RegisterExternalPropertyName('_And','And');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TPropertyValue)].RegisterExternalPropertyName('_Or','Or');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TPropertyValue)].RegisterExternalPropertyName('_Not','Not');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TPropertyValue)].RegisterExternalPropertyName('_Record','Record');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TPropertyValue)].RegisterExternalPropertyName('BinaryAtt','Binary');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TPropertyValue)].RegisterExternalPropertyName('BoolAtt','Bool');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TPropertyValue)].RegisterExternalPropertyName('DateAtt','Date');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TPropertyValue)].RegisterExternalPropertyName('DateTimeOffsetAtt','DateTimeOffset');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TPropertyValue)].RegisterExternalPropertyName('DecimalAtt','Decimal');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TPropertyValue)].RegisterExternalPropertyName('DurationAtt','Duration');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TPropertyValue)].RegisterExternalPropertyName('EnumMemberAtt','EnumMember');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TPropertyValue)].RegisterExternalPropertyName('FloatAtt','Float');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TPropertyValue)].RegisterExternalPropertyName('GuidAtt','Guid');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TPropertyValue)].RegisterExternalPropertyName('IntAtt','Int');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TPropertyValue)].RegisterExternalPropertyName('_StringAtt','String');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TPropertyValue)].RegisterExternalPropertyName('TimeOfDayAtt','TimeOfDay');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TPropertyValue)].RegisterExternalPropertyName('AnnotationPathAtt','AnnotationPath');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TPropertyValue)].RegisterExternalPropertyName('NavigationPropertyPathAtt','NavigationPropertyPath');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TPropertyValue)].RegisterExternalPropertyName('PathAtt','Path');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TPropertyValue)].RegisterExternalPropertyName('PropertyPathAtt','PropertyPath');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TPropertyValue)].RegisterExternalPropertyName('UrlRefAtt','UrlRef');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TSingleton)].RegisterExternalPropertyName('_Type','Type');
  typeRegistryInstance.ItemByTypeInfo[TypeInfo(TFunctionImport)].RegisterExternalPropertyName('_Function','Function');


End.
