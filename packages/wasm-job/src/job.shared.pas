{
  JOB - JS Object Bridge for Webassembly

  These types and constants are shared between pas2js and webassembly.
}
{$IFNDEF FPC_DOTTEDUNITS}
unit JOB.Shared;
{$ENDIF}

interface

type
  TJOBObjectID = NativeInt;
  TJOBObjectIDArray = array of TJOBObjectID;

// invoke results
type
  TJOBResult = longint;
const
  JOBResult_None = 0;
  JOBResult_Success = 1;
  JOBResult_UnknownObjId = 2;
  JOBResult_NotAFunction = 3;
  JOBResult_WrongArgs = 4;
  JOBResult_Undefined = 5;
  JOBResult_Null = 6;
  JOBResult_Boolean = 7;
  JOBResult_Double = 8;
  JOBResult_String = 9;
  JOBResult_Function = 10;
  JOBResult_Object = 11;
  JOBResult_BigInt = 12;
  JOBResult_Symbol = 13;
  JOBResult_ArrayOfString = 14;

  JOBResultLast = 14;

  JOBResult_Names: array[0..JOBResultLast] of string = (
    'None',
    'Success',
    'UnknownObjId',
    'NotAFunction',
    'WrongArgs',
    'Undefined',
    'Null',
    'Boolean',
    'Double',
    'String',
    'Function',
    'Object',
    'BigInt',
    'Symbol',
    'ArrayOfString'
    );

  JOBExportName = 'job';
  JOBFn_GetGlobal = 'get_registered';
  JOBFn_InvokeNoResult = 'invoke_noresult';
  JOBFn_InvokeBooleanResult = 'invoke_boolresult';
  JOBFn_InvokeDoubleResult = 'invoke_doubleresult';
  JOBFn_InvokeStringResult = 'invoke_stringresult';
  JOBFn_GetStringResult = 'get_stringresult';
  JOBFn_InvokeArrayStringResult = 'invoke_arraystringresult';
  JOBFn_ReleaseStringResult = 'release_stringresult';
  JOBFn_InvokeObjectResult = 'invoke_objectresult';
  JOBFn_ReleaseObject = 'release_object';
  JOBFn_InvokeJSValueResult = 'invoke_jsvalueresult';
  JOBFn_CallbackHandler = 'JOBCallback';

  JOBArgUndefined = 0;
  JOBArgLongint = 1;
  JOBArgDouble = 2;
  JOBArgTrue = 3;
  JOBArgFalse = 4;
  JOBArgChar = 5; // followed by a word
  JOBArgString = 6; // followed by length and UTF-16 data
  JOBArgUnicodeString = 7; // followed by length and pointer
  JOBArgNil = 8;
  JOBArgPointer = 9;
  JOBArgObject = 10; // followed by ObjectID
  JOBArgMethod = 11; // followed by Callback, Data, Code
  JOBArgDictionary = 12; // followed by count and pairs
  JOBArgArrayOfJSValue = 13; // followed by count and values
  JOBArgArrayOfDouble = 14; // followed by count and pointer

  JOBArgNames: array[0..14] of string = (
    'Undefined',
    'Longint',
    'Double',
    'True',
    'False',
    'Char',
    'UTF8String',
    'UnicodeString',
    'Nil',
    'Pointer',
    'Object',
    'Method',
    'Dictionary',
    'ArrayOfJSValue',
    'ArrayOfDouble'
    );

  JOBInvokeCall = 0; // call function
  JOBInvokeGet = 1; // read property
  JOBInvokeGetTypeOf = 2; // read property and typeof
  JOBInvokeSet = 3; // set property
  JOBInvokeNew = 4; // new operator

  JOBInvokeNames: array[0..4] of string = (
    'Call',
    'Get',
    'GetTypeOf',
    'Set',
    'New'
    );

implementation

end.
