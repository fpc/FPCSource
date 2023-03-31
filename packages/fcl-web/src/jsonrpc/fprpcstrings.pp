unit fprpcstrings;

{$mode ObjFPC}{$H+}

interface

Resourcestring
  SErrInvalidPath = 'Invalid path';
  SErrUnknownServiceName = 'Unknown service name : "%s"';
  SErrUnknownServiceGUID = 'Unknown service GUID : "%s"';
  SErrSupportedServiceName = 'Interface does not support service: "%s"';
  SErrExpectedReturnButNoServerReturn = 'Method "%s" expects return values, but no result was returned';
  SErrInvalidServerResponse = 'Invalid server response';
  SErrDuplicateParam  = 'Duplicate JSON-RPC Parameter name';
  SErrUnknownParamDef = 'Unknown parameter definition: "%s"';
  SErrParams = 'Error checking JSON-RPC parameters: "%s"';
  SErrParamsMustBeArrayorObject = 'Parameters must be passed in an object or an array.';
  SErrParamsMustBeObject = 'Parameters must be passed in an object.';
  SErrParamsMustBeArray  = 'Parameters must be passed in an array.';
  SErrParamsRequiredParamNotFound = 'Required parameter "%s" not found.';
  SErrParamsRequiredParams = '%d parameter(s) required, but no parameters found in request.';
  SErrParamsDataTypeMismatch = 'Expected parameter "%s" having type "%s", got "%s".';
  SErrParamsNotAllowd = 'Parameter "%s" is not allowed.';
  SErrParamsOnlyObjectsInArray = 'Array elements must be objects, got %s at position %d.';
  SErrRequestMustBeObject = 'JSON-RPC Request must be an object.';
  SErrNoIDProperty = 'No "id" property found in request.';
  SErrInvalidIDProperty = 'Type of "id" property is not correct.';
  SErrNoJSONRPCProperty = 'No "jsonrpc" property in request.';
  SErrInvalidJSONRPCProperty = 'Type or value of "jsonrpc" property is not correct.';
  SErrNoMethodName = 'Cannot determine method: No "%s" property found in request.';
  SErrNoClassName  = 'Cannot determine class: No "%s" property found in request.';
  SErrNoParams = 'Cannot determine parameters: No "%s" property found in request.';
  SErrInvalidMethodType = 'Type of "%s" property in request is not correct.';
  SErrInvalidClassNameType = 'Type of "%s" property in request is not correct.';
  SErrJSON2NotAllowed = 'JSON RPC 2 calls are not allowed.';
  SErrJSON1NotAllowed = 'JSON RPC 1 calls are not allowed.';
  SErrNoResponse = 'No response received from non-notification method "%s".';
  SErrResponseFromNotification = 'A response was received from a notification method "%s".';
  SErrInvalidMethodName = 'No method "%s" was found.';
  SErrInvalidClassMethodName = 'No class "%s" with method "%s" was found.';
  SErrDuplicateJSONRPCClassHandlerName = 'Duplicate JSON-RPC handler for class "%s" with method "%s".';
  SErrDuplicateJSONRPCHandlerName = 'Duplicate JSON-RPC handler for method "%s".';
  SErrUnknownJSONRPCClassMethodHandler = 'Unknown JSON-RPC handler for class "%s", method "%s".';
  SErrUnknownJSONRPCMethodHandler = 'Unknown JSON-RPC handler for method "%s".';
  SErrDuplicateRPCCLassMethodHandler = 'Duplicate JSON-RPC handler for class "%s", method "%s".';
  SErrDuplicateRPCMethodHandler = 'Duplicate JSON-RPC handler for method "%s".';
  SErrNoDispatcher = 'No method dispatcher available to handle request.';
  SErrUnknownMethodForClass = 'unknown method name for class %s: %s';
  SErrCreatorDoesNotSupportInterface = 'Creator does not support interface %s';

implementation

end.

