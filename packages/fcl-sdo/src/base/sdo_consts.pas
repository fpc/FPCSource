{
    This file is part of the Free Pascal Class Library SDO Implementation
    Copyright (c) 2012 by Inoussa OUEDRAOGO
    Free Pascal development team

    This unit implements sdo constants

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit sdo_consts;

interface

const
  s_any           = 'any';
  s_anyAttribute  = 'anyAttribute';
  s_append        = 'append';
  s_change        = 'change';
  s_changeSummary = 'changeSummary';
  s_changeSummaryListObject = 'changeSummaryListObject';
  s_create        = 'create';
  s_datagraph     = 'datagraph';
  s_dataValues    = 'dataValues';
  s_delete        = 'delete';
  s_index         = 'index';
  s_insert        = 'insert';
  s_item          = 'item';
  s_kind          = 'kind';
  s_listChanges   = 'listChanges';
  s_nil           = 'nil';
  s_propertyType  = 'propertyType';
  s_ref           = 'ref';
  s_schema        = 'schema';
  s_sdo           = 'sdo';
  s_unset         = 'unset';
  s_xml           = 'xml';
  s_xml_ns        = 'xmlns';
  s_xsd           = 'xsd';
  s_xsd_namespace = 'http://www.w3.org/2001/XMLSchema';
  s_xsi_namespace = 'http://www.w3.org/2001/XMLSchema-instance';

  // Database related constants
  s_db_namespace     = 'sdo.db';
  s_DataRowType      = 'DataRowType';
  s_DataSetType      = 'DatasetType';
  s_Row              = 'Row';
  s_RowCount         = 'RowCount';

resourcestring
  SMSG_FileNotFound = 'File not found : "%s"';

  SERR_BaseTypeNotSpecfifiedForSimpleType = 'Base type is not specified for the simple type, parsing : "%s".';
  SERR_CannotMakeInternalSymbolName  ='Unable to make an internal symbol Name from "%s".';
  SERR_CannotResolveNamespace        = 'Unable to resolve this namespace : "%s".';
  SERR_DataFilterNotFound            = 'Data Filter not found : "%s".';
  SERR_DuplicateBindingName          = 'Duplicated binding : "%s".';
  SERR_ExpectedButFound              = '%s expected but %s found.';
  SERR_ExpectedTypeDefinition        = '"%s" was expected to be a type definition.';
  SERR_ExpectingRemotableObjectClass = 'Expecting remotable object class but found "%s".';
  SERR_FailedTransportRequest        = '%s Request to %s failed.';
  SERR_HeaderNotUnderstood         = 'Header "%s" not Understood.';
  SERR_IllegalChar                 = 'Illegal character for that encoding : "%s".';
  SERR_IndexOutOfBound             = 'Index out of bound : %d.';
  SERR_IncompleteParamTypeRegistration  = 'Incomplete type registration for the type of this parameter : "%s".';
  SERR_InnerScopeMustBeSimpleType       = 'Inner Scope value must be a "simple type" value.';
  SERR_InvalidArrayBounds          = 'Invalid array bounds.';
  SERR_InvalidArrayItemType        = 'Invalid array type definition, invalid item type definition : "%s".';
  SERR_InvalidArrayLength          = 'Invalid array length : %d.';
  SERR_InvalidAttributeDef_EmptyName = 'Invalid Attribute definition : empty "name".';
  SERR_InvalidAttributeDef_EmptyType = 'Invalid Attribute definition : empty "type".';
  SERR_InvalidAttributeDef_EmptyUse = 'Invalid Attribute definition : empty "use".';
  SERR_InvalidAttributeDef_InvalidUse = 'Invalid Attribute definition : invalid "use" value "%s".';
  SERR_InvalidAttributeDef_MissingName = 'Invalid Attribute definition : missing "name" attribute.';
  SERR_InvalidAttributeDef_MissingType = 'Invalid Attribute definition : missing "type" attribute.';     
  SERR_InvalidCollectionLength     = 'Invalid collection length : %d.';
  SERR_InvalidComplexSimpleTypeDef_NoRestOrExt = 'Invalid "complexeType.simpleType" definition : restriction/extension not found.';
  SERR_InvalidDataTypeInContext    = 'Invalid data type in this context : "%s".';
  SERR_InvalidDomDocument          = 'Invalid DOM document.';
  SERR_InvalidElementDef_MissingNameOrRef = 'Invalid <element> definition : missing "name" or "ref" attribute.';
  SERR_InvalidElementDef_EmptyName = 'Invalid <element> definition : empty "name".';
  SERR_InvalidElementDef_EmptyType = 'Invalid <element> definition : empty "type".';
  SERR_InvalidElementDef_Type      = 'Invalid <element> definition : unable to determine the type. Type name : "%s"; Element name :"%s".';
  SERR_InvalidEncodedData          = 'Invalid encoded data.';
  SERR_InvalidEnumItemNode_NoValueAttribute = 'Invalid "enum" item node : no value attribute, type = "%s".';
  SERR_InvalidHourOffetValue       = '"%d" is not a valid hour offset value.';
  SERR_InvalidMaxOccursValue       = 'Invalid "maxOccurs" value : "%s.%s".';
  SERR_InvalidMinOccursValue       = 'Invalid "minOccurs" value : "%s.%s".';
  SERR_InvalidMinuteOffetValue     = '"%d" is not a valid minute offset value.';
  SERR_InvalidEmbeddedScopeOperation    = 'Invalid opération on scope, their are no embedded scope.';
  SERR_InvalidParameter            = 'Invalid parameter : "%s".';
  SERR_InvalidPropertyValue        = 'Invalid property ("%s") value : "%s".';
  SERR_InvalidParameterProc        = 'Invalid parameter : "%s"; Procedure = "%s".';
  SERR_InvalidParameters           = 'Invalid parameters.';
  SERR_InvalidPoolParametersArgs   = 'Invalid pool arguments Min = %d; Max = %d .';
  SERR_InvalidSchemaDoc_NamespaceNotFound = 'Invalid Schema document, namespace not found : %s.';
  SERR_InvalidSchemaDoc_NamespaceNotFoundShort = 'Invalid Schema document, namespace not found ( short names ) : %s.';
  SERR_InvalidSchemaNode           = 'Invalid schema node.';
  SERR_InvalidSymbolTable          = 'Invalid Symbol table.';
  SERR_InvalidTypeDef_AttributeNotFound = 'Invalid type definition, attributes not found : "%s".';
  SERR_InvalidTypeDef_BaseAttributeNotFound = 'Invalid extension/restriction of type "%s" : "base" attribute not found.';
  SERR_InvalidTypeDef_NamedAttributeNotFound = 'Invalid type definition, unable to find the "%s" attribute : "%s".';
  SERR_InvalidTypeDef_NoChild      = 'Invalid type definition, this element must have children.';
  SERR_InvalidTypeName             = 'Invalid type/element name( the name is empty ).';
  SERR_IsNotAFieldOf               = '"%s" is not a field of "%s".';
  SERR_NodeNotFoundByID            = 'Node not found with this ID in the document : "%s".';
  SERR_NoHandlerForThatVerb        = 'No handler for that verb : "%s".';
  SERR_NoReaderProc                = 'No reader proc for that type, Prop : "(%s : %s)".';
  SERR_NoScope                     = 'There is no scope.';  
  SERR_NoSerializerFoThisType      = 'No serializer for this type : "%s".';
  SERRE_ObjectCreationTimeOut      = 'Unable to create the object : Timeout expired.';
  SERR_OperationNotAllowedOnActivePool = 'Operation not allowed on an active pool.';
  SERR_ParamaterNotFound           = 'Parameter non found : "%s".';
  SERR_Parsing                     = 'Parsing "%s" ...';
  SERR_RecordExtendedRttiNotFound  = 'Record extended RTTI informations not found in type registry : "%s".';
  SERR_RootObjectCannotBeNIL       = 'The root object cannot be NIL.';
  SERR_SchemaNodeRequiredAttribute = 'The Schema node must have at least the "%s" attribute.';
  SERR_SerializerInitializationException = 'Unable to initialize the serializer of that type : "%s".';
  SERR_ServiceNotFound                   = 'Service not found : "%s".';
  SERR_ScopeNotFound                     = 'Scope not found : "%s".';
  SERR_TypeDefinitionNotFound            = 'Type definition not found %s : "%s"';
  SERR_TypeNodeFoundButUnableToParseIt   = 'Type node found but unable to parse it : "%s"';
  SERR_TypeNotRegistered                 = 'Type not registered : "%s".';
  SERR_TypeStyleNotSupported             = 'This type style is not supported : "%s".';
  SERR_UnableToFindNameTagInNode         = 'Unable to find the <name> tag in the type/element node attributes.';
  SERR_UnableToResolveNamespace          = 'Unable to resolve namespace, short name = "%s".';
  SERR_UnexpectedEndOfData               = 'Unexpected end of data.';
  SERR_UnknownProperty                   = 'Unknown property : "%s".';
  SERR_UnsupportedOperation        = 'Unsupported operation : "%s".';

implementation

end.
