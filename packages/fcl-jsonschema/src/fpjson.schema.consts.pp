{
    This file is part of the Free Component Library

    JSON Schema constants
    Copyright (c) 2024 by Michael Van Canneyt michael@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit FpJson.Schema.Consts;

{$mode ObjFPC}
{$H+}
{$modeswitch typehelpers}

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils;
  {$ELSE}
  SysUtils;
  {$ENDIF}

Const
  // Keywords

  // Core
  SJKWUnknown = '';
  SJKWId = '$id';
  SJKWOldId = 'id';
  SJKWSchema = '$schema';
  SJKWDefs = '$defs';
  SJKWRef = '$ref';
  SJKWAnchor = '$anchor';
  SJKWVocabulary = '$vocabulary';
  SJKWComment = '$comment';
  SJKWDynamicRef = '$dynamicRef';
  SJKWDynamicAnchor = '$dynamicAnchor';
  // Applicator
  SJKWAllOf = 'allOf';
  SJKWAnyOf = 'anyOf';
  SJKWOneOf = 'oneOf';
  SJKWNot = 'not';
  SJKWIf = 'if';
  SJKWThen = 'then';
  SJKWElse = 'else';
  SJKWProperties = 'properties';
  SJKWPatternProperties = 'patternProperties';
  SJKWAdditionalProperties = 'additionalProperties';
  SJKWPropertyNames = 'propertyNames';
  SJKWDependentSchemas = 'dependentSchemas';
  SJKWDependentRequired = 'dependentRequired';
  SJKWPrefixItems = 'prefixItems';
  SJKWItems = 'items';
  SJKWContains = 'contains';

  // Metadata
  SJKWTitle = 'title';
  SJKWDescription = 'description';
  SJKWDefault = 'default';
  SJKWDeprecated = 'deprecated';
  SJKWExamples = 'examples';
  SJKWReadOnly = 'readOnly';
  SJKWWriteOnly = 'writeOnly';

  // Validation
  SJKWMultipleOf = 'multipleOf';
  SJKWMaximum = 'maximum';
  SJKWExclusiveMaximum = 'exclusiveMaximum';
  SJKWMinimum = 'minimum';
  SJKWExclusiveMinimum = 'exclusiveMinimum';
  SJKWMaxLength = 'maxLength';
  SJKWMinLength = 'minLength';
  SJKWPattern = 'pattern';
  SJKWAdditionalItems = 'additionalItems';
  SJKWMaxItems = 'maxItems';
  SJKWMinItems = 'minItems';
  SJKWUniqueItems = 'uniqueItems';
  SJKWMaxProperties = 'maxProperties';
  SJKWMinProperties = 'minProperties';
  SJKWMaxContains = 'maxContains';
  SJKWMinContains = 'minContains';
  SJKWRequired = 'required';
  SJKWDefinitions = 'definitions';
  SJKWEnum = 'enum';
  SJKWType = 'type';
  SJKWFormat = 'format';
  SJKWConst = 'const';
  SJKWUnevaluatedItems = 'unevaluatedItems';
  SJKWUnevaluatedProperties = 'unevaluatedProperties';

  SJKWContentEncoding = 'contentEncoding';
  SJKWContentMediaType =  'contentMediaType';
  SJKWContentSchema = 'contentSchema';

  // Types
  STNone    = '';
  STNull    = 'null';
  STBoolean = 'boolean';
  STInteger = 'integer';
  STNumber  = 'number';
  STString  = 'string';
  STArray   = 'array';
  STObject  = 'object';
  STAny     = 'any';

  SFmtDatetime = 'date-time';
  SFmtDate = 'date';
  SFmtTime = 'time';
  SFmtDuration = 'duration';
  SFmtEmail = 'email';
  SFmtIdnEmail = 'idn-email';
  SFmtHostname = 'hostname';
  SFmtIdnHostname = 'idn-hostname';
  SFmtIPV4 = 'ipv4';
  SFmtIPV6 = 'ipv6';
  SFmtURI = 'uri';
  SFmtURIReference = 'uri-reference';
  SFmtIRI = 'iri';
  SFmtIRIReference = 'iri-reference';
  SFmtUUID = 'uuid';
  SFmtURITemplate = 'uri-template';
  SFmtJSONPointer = 'json-pointer';
  SFmtRelativeJSONPointer = 'relative-json-pointer';
  SFmtRegex = 'regex';

Resourcestring
  // Types
  SErrOnlySimpleValues = 'Only simple values can be stored as Schema value';

  // Reader
  SErrInvalidNumber = 'Invalid number : %s';
  SErrInvalidToken = 'Invalid token at %s: "%s"';
  SErrUnexpectedToken = 'Invalid token at %s, expected: "%s", got: "%s"';
  SErrUnexpectedTokenNotInSet = 'Invalid token, expected one of: [%s], got: "%s"';
  SErrNumberIsNotAnInteger = 'Number is not an integer at %s: %s';
  SErrIntegerIsNegative = 'Integer is negative %s: %d';
  SErrUnexpectedType = 'Invalid JSON type at <<%s>>, expected: "%s", got: "%s"';
  SErrUnexpectedTypeNotInSet = 'Invalid JSON type at <<%s>>, expected one of: [%s], got: "%s"';
  SErrInvalidType = 'Invalid JSON type %s at <<%s>>';

  // Writer
  SErrNoObjectsOnStack = 'No objects created on stack';
  SPropertyNameAlreadySet = 'Cannot set property name to "%s", it is already set to "%s"';
  SErrNotAtStructuredValue = 'Current value is not a structured value';
  SErrCannotPop = 'Cannot pop, stack empty';
  SErrNoPushOnSimpleValue = 'Cannot push on top of non-structured value';
  SErrNoPropertyNameForPush = 'Cannot push to object without property name';

  // Validator
  SSchemaInfo = 'Schema info: "%s" : %s';
  SErrNoFalseMatch = '"false" schema does not match any JSON';
  SErrTypeMismatch = 'JSON type "%s" does not match one of %s';
  SNotNumericalData = '%s is not numerical data, cannot check %s';
  SNotStringData = '%s is not string data, cannot check %s';
  SNotArrayData = '%s is not array data, cannot check %s';
  SNotObjectData = '%s is not object data, cannot check %s';
  SViolatesNumericalCondition = '%g violates numerical condition %s: %g';
  SViolatesArrayCondition = '%s violates array condition %s: %s';
  SViolatesObjectCondition = '%s violates object condition %s: %s';
  SViolatesStringCondition = '"%s" violates string condition %s: %s';
  SNotImplementedInValidator = 'Not implemented in validator %s';
  SErrListCountMismatch = 'Data fails to comply to correct amount of schemas in "%s" list: %d (list has %d items)';
  SIfResult = 'If() condition at path "%s" result: %s';
  SErrMissingRequiredDependent = 'Missing required dependent "%s"';
  SErrNotEqual = 'Not equal to JSON value %s';
  SErrSchemaMatchesNot = 'JSON data Matches "not" schema';
  SErrNotInList = 'Not in list of JSON values %s';

implementation

end.

