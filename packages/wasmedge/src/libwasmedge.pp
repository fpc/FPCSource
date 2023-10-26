
{$mode objfpc}
{$IFNDEF FPC_DOTTEDUNITS}
unit libwasmedge;
{$ENDIF}

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.CTypes;
{$ELSE}  
  ctypes;
{$ENDIF}
{
  Automatically converted by H2Pas 0.99.16 from libwasmedge4.h
  The following command line parameters were used:
    -l
    libwasmedge.so
    -o
    libwasmedge.pp
    -p
    -T
    -S
    -P
    -C
    -c
    libwasmedge4.h
}

const
  {$IFDEF UNIX}
  {$IFNDEF DARWIN}
  LibWasmName = 'libwasmedge.so';
  {$ELSE}
  LibWasmName = 'libwasmedge.dylib';
  {$ENDIF}
  {$ELSE}
  {$IFDEF WINDOWS}
  LibWasmName = 'libwasmedge.dll';
  {$ENDIF}
  {$ENDIF}

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

Type
 Puint64_t = ^PQWord; 
 Puint8_t = ^PByte; 
 uint128_t = record
  Low : QWord;
  High : QWord;
 end;
 tuint128_t = uint128_t;

 int128_t = record
  low: qword;
  high : int64;
 end;
 tint128_t = int128_t;

 tcint = cint; 
 tuint32_t = uint32;
 tint32_t = int32;
 tint64_t = int64;
 tuint64_t = qword;
 tcfloat = single;
 tdouble = double;
 tbool = cbool;
 Ppcchar = ^PAnsichar;
    
 (*
libwasmedge.pp(68,1) Error: Forward type not resolved "Tuint64_t"
libwasmedge.pp(68,1) Error: Forward type not resolved "Tuint8_t"
libwasmedge.pp(68,1) Error: Forward type not resolved "TWasmEdge_ASTModuleContext"
libwasmedge.pp(68,1) Error: Forward type not resolved "TWasmEdge_Async"
libwasmedge.pp(68,1) Error: Forward type not resolved "TWasmEdge_CallingFrameContext"
libwasmedge.pp(68,1) Error: Forward type not resolved "TWasmEdge_CompilerContext"
libwasmedge.pp(68,1) Error: Forward type not resolved "TWasmEdge_CompilerOptimizationLevel"
libwasmedge.pp(68,1) Error: Forward type not resolved "TWasmEdge_CompilerOutputFormat"
libwasmedge.pp(68,1) Error: Forward type not resolved "TWasmEdge_ConfigureContext"
libwasmedge.pp(68,1) Error: Forward type not resolved "TWasmEdge_ErrCategory"
libwasmedge.pp(68,1) Error: Forward type not resolved "TWasmEdge_ErrCode"
libwasmedge.pp(68,1) Error: Forward type not resolved "TWasmEdge_ExecutorContext"
libwasmedge.pp(68,1) Error: Forward type not resolved "TWasmEdge_ExportTypeContext"
libwasmedge.pp(68,1) Error: Forward type not resolved "TWasmEdge_ExternalType"
libwasmedge.pp(68,1) Error: Forward type not resolved "TWasmEdge_FunctionInstanceContext"
libwasmedge.pp(68,1) Error: Forward type not resolved "TWasmEdge_FunctionTypeContext"
libwasmedge.pp(68,1) Error: Forward type not resolved "TWasmEdge_GlobalInstanceContext"
libwasmedge.pp(68,1) Error: Forward type not resolved "TWasmEdge_GlobalTypeContext"
libwasmedge.pp(68,1) Error: Forward type not resolved "TWasmEdge_HostRegistration"
libwasmedge.pp(68,1) Error: Forward type not resolved "TWasmEdge_ImportTypeContext"
libwasmedge.pp(68,1) Error: Forward type not resolved "TWasmEdge_LoaderContext"
libwasmedge.pp(68,1) Error: Forward type not resolved "TWasmEdge_MemoryInstanceContext"
libwasmedge.pp(68,1) Error: Forward type not resolved "TWasmEdge_MemoryTypeContext"
libwasmedge.pp(68,1) Error: Forward type not resolved "TWasmEdge_ModuleInstanceContext"
libwasmedge.pp(68,1) Error: Forward type not resolved "TWasmEdge_Mutability"
libwasmedge.pp(68,1) Error: Forward type not resolved "TWasmEdge_NumType"
libwasmedge.pp(68,1) Error: Forward type not resolved "TWasmEdge_PluginContext"
libwasmedge.pp(68,1) Error: Forward type not resolved "TWasmEdge_Proposal"
libwasmedge.pp(68,1) Error: Forward type not resolved "TWasmEdge_RefType"
libwasmedge.pp(68,1) Error: Forward type not resolved "TWasmEdge_StatisticsContext"
libwasmedge.pp(68,1) Error: Forward type not resolved "TWasmEdge_StoreContext"
libwasmedge.pp(68,1) Error: Forward type not resolved "TWasmEdge_TableInstanceContext"
libwasmedge.pp(68,1) Error: Forward type not resolved "TWasmEdge_TableTypeContext"
libwasmedge.pp(68,1) Error: Forward type not resolved "TWasmEdge_ValidatorContext"
libwasmedge.pp(68,1) Error: Forward type not resolved "TWasmEdge_ValType"
libwasmedge.pp(68,1) Error: Forward type not resolved "TWasmEdge_VMContext 
 *)

TWasmEdge_ConfigureContext= record end;
TWasmEdge_StatisticsContext= record end;
TWasmEdge_ASTModuleContext= record end;
TWasmEdge_FunctionTypeContext= record end;
TWasmEdge_MemoryTypeContext= record end;
TWasmEdge_TableTypeContext= record end;
TWasmEdge_GlobalTypeContext= record end;
TWasmEdge_ImportTypeContext= record end;
TWasmEdge_ExportTypeContext= record end;
TWasmEdge_CompilerContext= record end;
TWasmEdge_LoaderContext= record end;
TWasmEdge_ValidatorContext= record end;
TWasmEdge_ExecutorContext= record end;
TWasmEdge_StoreContext= record end;
TWasmEdge_ModuleInstanceContext= record end;
TWasmEdge_FunctionInstanceContext = record end;
TWasmEdge_TableInstanceContext= record end;
TWasmEdge_MemoryInstanceContext= record end;
TWasmEdge_GlobalInstanceContext= record end;
TWasmEdge_CallingFrameContext= record end;
TWasmEdge_Async= record end;
TWasmEdge_VMContext= record end;
TWasmEdge_PluginContext= record end;

PWasmEdge_ASTModuleContext = ^TWasmEdge_ASTModuleContext;
PWasmEdge_Async = ^TWasmEdge_Async;
PWasmEdge_CallingFrameContext = ^TWasmEdge_CallingFrameContext;
PWasmEdge_CompilerContext = ^TWasmEdge_CompilerContext;
PWasmEdge_CompilerOptimizationLevel = ^TWasmEdge_CompilerOptimizationLevel;
PWasmEdge_CompilerOutputFormat = ^TWasmEdge_CompilerOutputFormat;
PWasmEdge_ConfigureContext = ^TWasmEdge_ConfigureContext;
PWasmEdge_ErrCategory = ^TWasmEdge_ErrCategory;
PWasmEdge_ErrCode = ^TWasmEdge_ErrCode;
PWasmEdge_ExecutorContext = ^TWasmEdge_ExecutorContext;
PWasmEdge_ExportTypeContext = ^TWasmEdge_ExportTypeContext;
PWasmEdge_ExternalType = ^TWasmEdge_ExternalType;
PWasmEdge_FunctionInstanceContext = ^TWasmEdge_FunctionInstanceContext;
PWasmEdge_FunctionTypeContext = ^TWasmEdge_FunctionTypeContext;
PWasmEdge_GlobalInstanceContext = ^TWasmEdge_GlobalInstanceContext;
PWasmEdge_GlobalTypeContext = ^TWasmEdge_GlobalTypeContext;
PWasmEdge_HostRegistration = ^TWasmEdge_HostRegistration;
PWasmEdge_ImportTypeContext = ^TWasmEdge_ImportTypeContext;
PWasmEdge_LoaderContext = ^TWasmEdge_LoaderContext;
PWasmEdge_MemoryInstanceContext = ^TWasmEdge_MemoryInstanceContext;
PWasmEdge_MemoryTypeContext = ^TWasmEdge_MemoryTypeContext;
PWasmEdge_ModuleInstanceContext = ^TWasmEdge_ModuleInstanceContext;
PWasmEdge_Mutability = ^TWasmEdge_Mutability;
PWasmEdge_NumType = ^TWasmEdge_NumType;
PWasmEdge_PluginContext = ^TWasmEdge_PluginContext;
PWasmEdge_Proposal = ^TWasmEdge_Proposal;
PWasmEdge_RefType = ^TWasmEdge_RefType;
PWasmEdge_StatisticsContext = ^TWasmEdge_StatisticsContext;
PWasmEdge_StoreContext = ^TWasmEdge_StoreContext;
PWasmEdge_TableInstanceContext = ^TWasmEdge_TableInstanceContext;
PWasmEdge_TableTypeContext = ^TWasmEdge_TableTypeContext;
PWasmEdge_ValidatorContext = ^TWasmEdge_ValidatorContext;
PWasmEdge_ValType = ^TWasmEdge_ValType;
PWasmEdge_VMContext = ^TWasmEdge_VMContext;

PPWasmEdge_ImportTypeContext = ^PWasmEdge_ImportTypeContext;
PPWasmEdge_ExportTypeContext =  ^PWasmEdge_ExportTypeContext;
PPWasmEdge_ASTModuleContext = ^PWasmEdge_ASTModuleContext;
PPWasmEdge_ModuleInstanceContext = ^PWasmEdge_ModuleInstanceContext ;
PPWasmEdge_FunctionTypeContext =  ^PWasmEdge_FunctionTypeContext;


  TWasmEdge_Proposal = (WasmEdge_Proposal_ImportExportMutGlobals,
    WasmEdge_Proposal_NonTrapFloatToIntConversions,
    WasmEdge_Proposal_SignExtensionOperators,
    WasmEdge_Proposal_MultiValue,WasmEdge_Proposal_BulkMemoryOperations,
    WasmEdge_Proposal_ReferenceTypes,WasmEdge_Proposal_SIMD,
    WasmEdge_Proposal_TailCall,WasmEdge_Proposal_MultiMemories,
    WasmEdge_Proposal_Annotations,WasmEdge_Proposal_Memory64,
    WasmEdge_Proposal_ExceptionHandling,WasmEdge_Proposal_ExtendedConst,
    WasmEdge_Proposal_Threads,WasmEdge_Proposal_FunctionReferences
    );

  TWasmEdge_HostRegistration = (WasmEdge_HostRegistration_Wasi);

  TWasmEdge_CompilerOptimizationLevel = (
    WasmEdge_CompilerOptimizationLevel_O0 := 0,
    WasmEdge_CompilerOptimizationLevel_O1,
    WasmEdge_CompilerOptimizationLevel_O2,
    WasmEdge_CompilerOptimizationLevel_O3,
    WasmEdge_CompilerOptimizationLevel_Os,
    WasmEdge_CompilerOptimizationLevel_Oz
    );

  TWasmEdge_CompilerOutputFormat = (WasmEdge_CompilerOutputFormat_Native := 0,
    WasmEdge_CompilerOutputFormat_Wasm);

  TWasmEdge_ErrCategory = (WasmEdge_ErrCategory_WASM := $00,WasmEdge_ErrCategory_UserLevelError := $01
    );

  TWasmEdge_ErrCode = (WasmEdge_ErrCode_Success := $00,WasmEdge_ErrCode_Terminated := $01,
    WasmEdge_ErrCode_RuntimeError := $02,WasmEdge_ErrCode_CostLimitExceeded := $03,
    WasmEdge_ErrCode_WrongVMWorkflow := $04,
    WasmEdge_ErrCode_FuncNotFound := $05,WasmEdge_ErrCode_AOTDisabled := $06,
    WasmEdge_ErrCode_Interrupted := $07,WasmEdge_ErrCode_NotValidated := $08,
    WasmEdge_ErrCode_UserDefError := $09,WasmEdge_ErrCode_IllegalPath := $20,
    WasmEdge_ErrCode_ReadError := $21,WasmEdge_ErrCode_UnexpectedEnd := $22,
    WasmEdge_ErrCode_MalformedMagic := $23,
    WasmEdge_ErrCode_MalformedVersion := $24,
    WasmEdge_ErrCode_MalformedSection := $25,
    WasmEdge_ErrCode_SectionSizeMismatch := $26,
    WasmEdge_ErrCode_LengthOutOfBounds := $27,
    WasmEdge_ErrCode_JunkSection := $28,WasmEdge_ErrCode_IncompatibleFuncCode := $29,
    WasmEdge_ErrCode_IncompatibleDataCount := $2A,
    WasmEdge_ErrCode_DataCountRequired := $2B,
    WasmEdge_ErrCode_MalformedImportKind := $2C,
    WasmEdge_ErrCode_MalformedExportKind := $2D,
    WasmEdge_ErrCode_ExpectedZeroByte := $2E,
    WasmEdge_ErrCode_InvalidMut := $2F,WasmEdge_ErrCode_TooManyLocals := $30,
    WasmEdge_ErrCode_MalformedValType := $31,
    WasmEdge_ErrCode_MalformedElemType := $32,
    WasmEdge_ErrCode_MalformedRefType := $33,
    WasmEdge_ErrCode_MalformedUTF8 := $34,WasmEdge_ErrCode_IntegerTooLarge := $35,
    WasmEdge_ErrCode_IntegerTooLong := $36,
    WasmEdge_ErrCode_IllegalOpCode := $37,WasmEdge_ErrCode_ENDCodeExpected := $38,
    WasmEdge_ErrCode_IllegalGrammar := $39,
    WasmEdge_ErrCode_SharedMemoryNoMax := $3A,
    WasmEdge_ErrCode_IntrinsicsTableNotFound := $3B,
    WasmEdge_ErrCode_InvalidAlignment := $40,
    WasmEdge_ErrCode_TypeCheckFailed := $41,
    WasmEdge_ErrCode_InvalidLabelIdx := $42,
    WasmEdge_ErrCode_InvalidLocalIdx := $43,
    WasmEdge_ErrCode_InvalidFuncTypeIdx := $44,
    WasmEdge_ErrCode_InvalidFuncIdx := $45,
    WasmEdge_ErrCode_InvalidTableIdx := $46,
    WasmEdge_ErrCode_InvalidMemoryIdx := $47,
    WasmEdge_ErrCode_InvalidGlobalIdx := $48,
    WasmEdge_ErrCode_InvalidElemIdx := $49,
    WasmEdge_ErrCode_InvalidDataIdx := $4A,
    WasmEdge_ErrCode_InvalidRefIdx := $4B,WasmEdge_ErrCode_ConstExprRequired := $4C,
    WasmEdge_ErrCode_DupExportName := $4D,WasmEdge_ErrCode_ImmutableGlobal := $4E,
    WasmEdge_ErrCode_InvalidResultArity := $4F,
    WasmEdge_ErrCode_MultiTables := $50,WasmEdge_ErrCode_MultiMemories := $51,
    WasmEdge_ErrCode_InvalidLimit := $52,WasmEdge_ErrCode_InvalidMemPages := $53,
    WasmEdge_ErrCode_InvalidStartFunc := $54,
    WasmEdge_ErrCode_InvalidLaneIdx := $55,
    WasmEdge_ErrCode_ModuleNameConflict := $60,
    WasmEdge_ErrCode_IncompatibleImportType := $61,
    WasmEdge_ErrCode_UnknownImport := $62,WasmEdge_ErrCode_DataSegDoesNotFit := $63,
    WasmEdge_ErrCode_ElemSegDoesNotFit := $64,
    WasmEdge_ErrCode_WrongInstanceAddress := $80,
    WasmEdge_ErrCode_WrongInstanceIndex := $81,
    WasmEdge_ErrCode_InstrTypeMismatch := $82,
    WasmEdge_ErrCode_FuncSigMismatch := $83,
    WasmEdge_ErrCode_DivideByZero := $84,WasmEdge_ErrCode_IntegerOverflow := $85,
    WasmEdge_ErrCode_InvalidConvToInt := $86,
    WasmEdge_ErrCode_TableOutOfBounds := $87,
    WasmEdge_ErrCode_MemoryOutOfBounds := $88,
    WasmEdge_ErrCode_Unreachable := $89,WasmEdge_ErrCode_UninitializedElement := $8A,
    WasmEdge_ErrCode_UndefinedElement := $8B,
    WasmEdge_ErrCode_IndirectCallTypeMismatch := $8C,
    WasmEdge_ErrCode_HostFuncError := $8D,WasmEdge_ErrCode_RefTypeMismatch := $8E,
    WasmEdge_ErrCode_UnalignedAtomicAccess := $8F,
    WasmEdge_ErrCode_ExpectSharedMemory := $90
    );

  TWasmEdge_ValType = (WasmEdge_ValType_I32 := $7F,WasmEdge_ValType_I64 := $7E,
    WasmEdge_ValType_F32 := $7D,WasmEdge_ValType_F64 := $7C,
    WasmEdge_ValType_V128 := $7B,WasmEdge_ValType_FuncRef := $70,
    WasmEdge_ValType_ExternRef := $6F);

  TWasmEdge_NumType = (WasmEdge_NumType_I32 := $7F,WasmEdge_NumType_I64 := $7E,
    WasmEdge_NumType_F32 := $7D,WasmEdge_NumType_F64 := $7C,
    WasmEdge_NumType_V128 := $7B);

  TWasmEdge_RefType = (WasmEdge_RefType_FuncRef := $70,WasmEdge_RefType_ExternRef := $6F
    );

  TWasmEdge_Mutability = (WasmEdge_Mutability_Const := $00,WasmEdge_Mutability_Var := $01
    );

  TWasmEdge_ExternalType = (WasmEdge_ExternalType_Function := $00,WasmEdge_ExternalType_Table := $01,
    WasmEdge_ExternalType_Memory := $02,WasmEdge_ExternalType_Global := $03
    );


  PWasmEdge_Value = ^TWasmEdge_Value;
  TWasmEdge_Value = record
      Value : Tuint128_t;
      _Type : TWasmEdge_ValType;
    end;

  PWasmEdge_String = ^TWasmEdge_String;
  TWasmEdge_String = record
      Length : Tuint32_t;
      Buf : pcchar;
    end;

  PWasmEdge_Result = ^TWasmEdge_Result;
  TWasmEdge_Result = record
      Code : Tuint32_t;
    end;

  PWasmEdge_Limit = ^TWasmEdge_Limit;
  TWasmEdge_Limit = record
      HasMax : TBool;
      Shared : TBool;
      Min : Tuint32_t;
      Max : Tuint32_t;
    end;

  PWasmEdge_ProgramOptionType = ^TWasmEdge_ProgramOptionType;
  TWasmEdge_ProgramOptionType = (WasmEdge_ProgramOptionType_None,WasmEdge_ProgramOptionType_Toggle,
    WasmEdge_ProgramOptionType_Int8,WasmEdge_ProgramOptionType_Int16,
    WasmEdge_ProgramOptionType_Int32,WasmEdge_ProgramOptionType_Int64,
    WasmEdge_ProgramOptionType_UInt8,WasmEdge_ProgramOptionType_UInt16,
    WasmEdge_ProgramOptionType_UInt32,WasmEdge_ProgramOptionType_UInt64,
    WasmEdge_ProgramOptionType_Float,WasmEdge_ProgramOptionType_Double,
    WasmEdge_ProgramOptionType_String);

  PWasmEdge_ProgramOption = ^TWasmEdge_ProgramOption;
  TWasmEdge_ProgramOption = record
      Name : pcchar;
      Description : pcchar;
      _Type : TWasmEdge_ProgramOptionType;
      Storage : pointer;
      DefaultValue : pointer;
    end;

  PWasmEdge_ModuleDescriptor = ^TWasmEdge_ModuleDescriptor;
  TWasmEdge_ModuleDescriptor = record
      Name : pcchar;
      Description : pcchar;
      Create : function (para1:PWasmEdge_ModuleDescriptor):PWasmEdge_ModuleInstanceContext;cdecl;
    end;

  PWasmEdge_PluginVersionData = ^TWasmEdge_PluginVersionData;
  TWasmEdge_PluginVersionData = record
      Major : Tuint32_t;
      Minor : Tuint32_t;
      Patch : Tuint32_t;
      Build : Tuint32_t;
    end;

  PWasmEdge_PluginDescriptor = ^TWasmEdge_PluginDescriptor;
  TWasmEdge_PluginDescriptor = record
      Name : pcchar;
      Description : pcchar;
      APIVersion : Tuint32_t;
      Version : TWasmEdge_PluginVersionData;
      ModuleCount : Tuint32_t;
      ProgramOptionCount : Tuint32_t;
      ModuleDescriptions : PWasmEdge_ModuleDescriptor;
      ProgramOptions : PWasmEdge_ProgramOption;
    end;
var
WasmEdge_VersionGet : function:pcchar;cdecl;
WasmEdge_VersionGetMajor : function:Tuint32_t;cdecl;
WasmEdge_VersionGetMinor : function:Tuint32_t;cdecl;
WasmEdge_VersionGetPatch : function:Tuint32_t;cdecl;
WasmEdge_LogSetErrorLevel : procedure;cdecl;
WasmEdge_LogSetDebugLevel : procedure;cdecl;
WasmEdge_LogOff : procedure;cdecl;
WasmEdge_ValueGenI32 : function(Val:Tint32_t):TWasmEdge_Value;cdecl;
WasmEdge_ValueGenI64 : function(Val:Tint64_t):TWasmEdge_Value;cdecl;
WasmEdge_ValueGenF32 : function(Val:Tcfloat):TWasmEdge_Value;cdecl;
WasmEdge_ValueGenF64 : function(Val:Tdouble):TWasmEdge_Value;cdecl;
WasmEdge_ValueGenV128 : function(Val:Tint128_t):TWasmEdge_Value;cdecl;
WasmEdge_ValueGenNullRef : function(T:TWasmEdge_RefType):TWasmEdge_Value;cdecl;
WasmEdge_ValueGenFuncRef : function(Cxt:PWasmEdge_FunctionInstanceContext):TWasmEdge_Value;cdecl;
WasmEdge_ValueGenExternRef : function(Ref:pointer):TWasmEdge_Value;cdecl;
WasmEdge_ValueGetI32 : function(Val:TWasmEdge_Value):Tint32_t;cdecl;
WasmEdge_ValueGetI64 : function(Val:TWasmEdge_Value):Tint64_t;cdecl;
WasmEdge_ValueGetF32 : function(Val:TWasmEdge_Value):Tcfloat;cdecl;
WasmEdge_ValueGetF64 : function(Val:TWasmEdge_Value):Tdouble;cdecl;
WasmEdge_ValueGetV128 : function(Val:TWasmEdge_Value):Tint128_t;cdecl;
WasmEdge_ValueIsNullRef : function(Val:TWasmEdge_Value):TBool;cdecl;
WasmEdge_ValueGetFuncRef : function(Val:TWasmEdge_Value):PWasmEdge_FunctionInstanceContext;cdecl;
WasmEdge_ValueGetExternRef : function(Val:TWasmEdge_Value):pointer;cdecl;
WasmEdge_StringCreateByCString : function(Str:pcchar):TWasmEdge_String;cdecl;
WasmEdge_StringCreateByBuffer : function(Buf:pcchar; Len:Tuint32_t):TWasmEdge_String;cdecl;
WasmEdge_StringWrap : function(Buf:pcchar; Len:Tuint32_t):TWasmEdge_String;cdecl;
WasmEdge_StringIsEqual : function(Str1:TWasmEdge_String; Str2:TWasmEdge_String):TBool;cdecl;
WasmEdge_StringCopy : function(Str:TWasmEdge_String; Buf:pcchar; Len:Tuint32_t):Tuint32_t;cdecl;
WasmEdge_StringDelete : procedure(Str:TWasmEdge_String);cdecl;
WasmEdge_ResultOK : function(Res:TWasmEdge_Result):TBool;cdecl;
WasmEdge_ResultGen : function(Category:TWasmEdge_ErrCategory; Code:Tuint32_t):TWasmEdge_Result;cdecl;
WasmEdge_ResultGetCode : function(Res:TWasmEdge_Result):Tuint32_t;cdecl;
WasmEdge_ResultGetCategory : function(Res:TWasmEdge_Result):TWasmEdge_ErrCategory;cdecl;
WasmEdge_ResultGetMessage : function(Res:TWasmEdge_Result):pcchar;cdecl;
WasmEdge_LimitIsEqual : function(Lim1:TWasmEdge_Limit; Lim2:TWasmEdge_Limit):TBool;cdecl;
WasmEdge_ConfigureCreate : function:PWasmEdge_ConfigureContext;cdecl;
WasmEdge_ConfigureAddProposal : procedure(Cxt:PWasmEdge_ConfigureContext; Prop:TWasmEdge_Proposal);cdecl;
WasmEdge_ConfigureRemoveProposal : procedure(Cxt:PWasmEdge_ConfigureContext; Prop:TWasmEdge_Proposal);cdecl;
WasmEdge_ConfigureHasProposal : function(Cxt:PWasmEdge_ConfigureContext; Prop:TWasmEdge_Proposal):TBool;cdecl;
WasmEdge_ConfigureAddHostRegistration : procedure(Cxt:PWasmEdge_ConfigureContext; Host:TWasmEdge_HostRegistration);cdecl;
WasmEdge_ConfigureRemoveHostRegistration : procedure(Cxt:PWasmEdge_ConfigureContext; Host:TWasmEdge_HostRegistration);cdecl;
WasmEdge_ConfigureHasHostRegistration : function(Cxt:PWasmEdge_ConfigureContext; Host:TWasmEdge_HostRegistration):TBool;cdecl;
WasmEdge_ConfigureSetMaxMemoryPage : procedure(Cxt:PWasmEdge_ConfigureContext; Page:Tuint32_t);cdecl;
WasmEdge_ConfigureGetMaxMemoryPage : function(Cxt:PWasmEdge_ConfigureContext):Tuint32_t;cdecl;
WasmEdge_ConfigureSetForceInterpreter : procedure(Cxt:PWasmEdge_ConfigureContext; IsForceInterpreter:TBool);cdecl;
WasmEdge_ConfigureIsForceInterpreter : function(Cxt:PWasmEdge_ConfigureContext):TBool;cdecl;
WasmEdge_ConfigureSetAllowAFUNIX : procedure(Cxt:PWasmEdge_ConfigureContext; EnableAFUNIX:TBool);cdecl;
WasmEdge_ConfigureIsAllowAFUNIX : function(Cxt:PWasmEdge_ConfigureContext):TBool;cdecl;
WasmEdge_ConfigureCompilerSetOptimizationLevel : procedure(Cxt:PWasmEdge_ConfigureContext; Level:TWasmEdge_CompilerOptimizationLevel);cdecl;
WasmEdge_ConfigureCompilerGetOptimizationLevel : function(Cxt:PWasmEdge_ConfigureContext):TWasmEdge_CompilerOptimizationLevel;cdecl;
WasmEdge_ConfigureCompilerSetOutputFormat : procedure(Cxt:PWasmEdge_ConfigureContext; Format:TWasmEdge_CompilerOutputFormat);cdecl;
WasmEdge_ConfigureCompilerGetOutputFormat : function(Cxt:PWasmEdge_ConfigureContext):TWasmEdge_CompilerOutputFormat;cdecl;
WasmEdge_ConfigureCompilerSetDumpIR : procedure(Cxt:PWasmEdge_ConfigureContext; IsDump:TBool);cdecl;
WasmEdge_ConfigureCompilerIsDumpIR : function(Cxt:PWasmEdge_ConfigureContext):TBool;cdecl;
WasmEdge_ConfigureCompilerSetGenericBinary : procedure(Cxt:PWasmEdge_ConfigureContext; IsGeneric:TBool);cdecl;
WasmEdge_ConfigureCompilerIsGenericBinary : function(Cxt:PWasmEdge_ConfigureContext):TBool;cdecl;
WasmEdge_ConfigureCompilerSetInterruptible : procedure(Cxt:PWasmEdge_ConfigureContext; IsInterruptible:TBool);cdecl;
WasmEdge_ConfigureCompilerIsInterruptible : function(Cxt:PWasmEdge_ConfigureContext):TBool;cdecl;
WasmEdge_ConfigureStatisticsSetInstructionCounting : procedure(Cxt:PWasmEdge_ConfigureContext; IsCount:TBool);cdecl;
WasmEdge_ConfigureStatisticsIsInstructionCounting : function(Cxt:PWasmEdge_ConfigureContext):TBool;cdecl;
WasmEdge_ConfigureStatisticsSetCostMeasuring : procedure(Cxt:PWasmEdge_ConfigureContext; IsMeasure:TBool);cdecl;
WasmEdge_ConfigureStatisticsIsCostMeasuring : function(Cxt:PWasmEdge_ConfigureContext):TBool;cdecl;
WasmEdge_ConfigureStatisticsSetTimeMeasuring : procedure(Cxt:PWasmEdge_ConfigureContext; IsMeasure:TBool);cdecl;
WasmEdge_ConfigureStatisticsIsTimeMeasuring : function(Cxt:PWasmEdge_ConfigureContext):TBool;cdecl;
WasmEdge_ConfigureDelete : procedure(Cxt:PWasmEdge_ConfigureContext);cdecl;
WasmEdge_StatisticsCreate : function:PWasmEdge_StatisticsContext;cdecl;
WasmEdge_StatisticsGetInstrCount : function(Cxt:PWasmEdge_StatisticsContext):Tuint64_t;cdecl;
WasmEdge_StatisticsGetInstrPerSecond : function(Cxt:PWasmEdge_StatisticsContext):Tdouble;cdecl;
WasmEdge_StatisticsGetTotalCost : function(Cxt:PWasmEdge_StatisticsContext):Tuint64_t;cdecl;
WasmEdge_StatisticsSetCostTable : procedure(Cxt:PWasmEdge_StatisticsContext; CostArr:Puint64_t; Len:Tuint32_t);cdecl;
WasmEdge_StatisticsSetCostLimit : procedure(Cxt:PWasmEdge_StatisticsContext; Limit:Tuint64_t);cdecl;
WasmEdge_StatisticsClear : procedure(Cxt:PWasmEdge_StatisticsContext);cdecl;
WasmEdge_StatisticsDelete : procedure(Cxt:PWasmEdge_StatisticsContext);cdecl;
WasmEdge_ASTModuleListImportsLength : function(Cxt:PWasmEdge_ASTModuleContext):Tuint32_t;cdecl;
WasmEdge_ASTModuleListImports : function(Cxt:PWasmEdge_ASTModuleContext; Imports:PPWasmEdge_ImportTypeContext; Len:Tuint32_t):Tuint32_t;cdecl;
WasmEdge_ASTModuleListExportsLength : function(Cxt:PWasmEdge_ASTModuleContext):Tuint32_t;cdecl;
WasmEdge_ASTModuleListExports : function(Cxt:PWasmEdge_ASTModuleContext; Exports_:PPWasmEdge_ExportTypeContext; Len:Tuint32_t):Tuint32_t;cdecl;
WasmEdge_ASTModuleDelete : procedure(Cxt:PWasmEdge_ASTModuleContext);cdecl;
WasmEdge_FunctionTypeCreate : function(ParamList:PWasmEdge_ValType; ParamLen:Tuint32_t; ReturnList:PWasmEdge_ValType; ReturnLen:Tuint32_t):PWasmEdge_FunctionTypeContext;cdecl;
WasmEdge_FunctionTypeGetParametersLength : function(Cxt:PWasmEdge_FunctionTypeContext):Tuint32_t;cdecl;
WasmEdge_FunctionTypeGetParameters : function(Cxt:PWasmEdge_FunctionTypeContext; List:PWasmEdge_ValType; Len:Tuint32_t):Tuint32_t;cdecl;
WasmEdge_FunctionTypeGetReturnsLength : function(Cxt:PWasmEdge_FunctionTypeContext):Tuint32_t;cdecl;
WasmEdge_FunctionTypeGetReturns : function(Cxt:PWasmEdge_FunctionTypeContext; List:PWasmEdge_ValType; Len:Tuint32_t):Tuint32_t;cdecl;
WasmEdge_FunctionTypeDelete : procedure(Cxt:PWasmEdge_FunctionTypeContext);cdecl;
WasmEdge_TableTypeCreate : function(RefType:TWasmEdge_RefType; Limit:TWasmEdge_Limit):PWasmEdge_TableTypeContext;cdecl;
WasmEdge_TableTypeGetRefType : function(Cxt:PWasmEdge_TableTypeContext):TWasmEdge_RefType;cdecl;
WasmEdge_TableTypeGetLimit : function(Cxt:PWasmEdge_TableTypeContext):TWasmEdge_Limit;cdecl;
WasmEdge_TableTypeDelete : procedure(Cxt:PWasmEdge_TableTypeContext);cdecl;
WasmEdge_MemoryTypeCreate : function(Limit:TWasmEdge_Limit):PWasmEdge_MemoryTypeContext;cdecl;
WasmEdge_MemoryTypeGetLimit : function(Cxt:PWasmEdge_MemoryTypeContext):TWasmEdge_Limit;cdecl;
WasmEdge_MemoryTypeDelete : procedure(Cxt:PWasmEdge_MemoryTypeContext);cdecl;
WasmEdge_GlobalTypeCreate : function(ValType:TWasmEdge_ValType; Mut:TWasmEdge_Mutability):PWasmEdge_GlobalTypeContext;cdecl;
WasmEdge_GlobalTypeGetValType : function(Cxt:PWasmEdge_GlobalTypeContext):TWasmEdge_ValType;cdecl;
WasmEdge_GlobalTypeGetMutability : function(Cxt:PWasmEdge_GlobalTypeContext):TWasmEdge_Mutability;cdecl;
WasmEdge_GlobalTypeDelete : procedure(Cxt:PWasmEdge_GlobalTypeContext);cdecl;
WasmEdge_ImportTypeGetExternalType : function(Cxt:PWasmEdge_ImportTypeContext):TWasmEdge_ExternalType;cdecl;
WasmEdge_ImportTypeGetModuleName : function(Cxt:PWasmEdge_ImportTypeContext):TWasmEdge_String;cdecl;
WasmEdge_ImportTypeGetExternalName : function(Cxt:PWasmEdge_ImportTypeContext):TWasmEdge_String;cdecl;
WasmEdge_ImportTypeGetFunctionType : function(ASTCxt:PWasmEdge_ASTModuleContext; Cxt:PWasmEdge_ImportTypeContext):PWasmEdge_FunctionTypeContext;cdecl;
WasmEdge_ImportTypeGetTableType : function(ASTCxt:PWasmEdge_ASTModuleContext; Cxt:PWasmEdge_ImportTypeContext):PWasmEdge_TableTypeContext;cdecl;
WasmEdge_ImportTypeGetMemoryType : function(ASTCxt:PWasmEdge_ASTModuleContext; Cxt:PWasmEdge_ImportTypeContext):PWasmEdge_MemoryTypeContext;cdecl;
WasmEdge_ImportTypeGetGlobalType : function(ASTCxt:PWasmEdge_ASTModuleContext; Cxt:PWasmEdge_ImportTypeContext):PWasmEdge_GlobalTypeContext;cdecl;
WasmEdge_ExportTypeGetExternalType : function(Cxt:PWasmEdge_ExportTypeContext):TWasmEdge_ExternalType;cdecl;
WasmEdge_ExportTypeGetExternalName : function(Cxt:PWasmEdge_ExportTypeContext):TWasmEdge_String;cdecl;
WasmEdge_ExportTypeGetFunctionType : function(ASTCxt:PWasmEdge_ASTModuleContext; Cxt:PWasmEdge_ExportTypeContext):PWasmEdge_FunctionTypeContext;cdecl;
WasmEdge_ExportTypeGetTableType : function(ASTCxt:PWasmEdge_ASTModuleContext; Cxt:PWasmEdge_ExportTypeContext):PWasmEdge_TableTypeContext;cdecl;
WasmEdge_ExportTypeGetMemoryType : function(ASTCxt:PWasmEdge_ASTModuleContext; Cxt:PWasmEdge_ExportTypeContext):PWasmEdge_MemoryTypeContext;cdecl;
WasmEdge_ExportTypeGetGlobalType : function(ASTCxt:PWasmEdge_ASTModuleContext; Cxt:PWasmEdge_ExportTypeContext):PWasmEdge_GlobalTypeContext;cdecl;
WasmEdge_CompilerCreate : function(ConfCxt:PWasmEdge_ConfigureContext):PWasmEdge_CompilerContext;cdecl;
WasmEdge_CompilerCompile : function(Cxt:PWasmEdge_CompilerContext; InPath:pcchar; OutPath:pcchar):TWasmEdge_Result;cdecl;
WasmEdge_CompilerCompileFromBuffer : function(Cxt:PWasmEdge_CompilerContext; InBuffer:Puint8_t; InBufferLen:Tuint64_t; OutPath:pcchar):TWasmEdge_Result;cdecl;
WasmEdge_CompilerDelete : procedure(Cxt:PWasmEdge_CompilerContext);cdecl;
WasmEdge_LoaderCreate : function(ConfCxt:PWasmEdge_ConfigureContext):PWasmEdge_LoaderContext;cdecl;
WasmEdge_LoaderParseFromFile : function(Cxt:PWasmEdge_LoaderContext; Module:PPWasmEdge_ASTModuleContext; Path:pcchar):TWasmEdge_Result;cdecl;
WasmEdge_LoaderParseFromBuffer : function(Cxt:PWasmEdge_LoaderContext; Module:PPWasmEdge_ASTModuleContext; Buf:Puint8_t; BufLen:Tuint32_t):TWasmEdge_Result;cdecl;
WasmEdge_LoaderDelete : procedure(Cxt:PWasmEdge_LoaderContext);cdecl;
WasmEdge_ValidatorCreate : function(ConfCxt:PWasmEdge_ConfigureContext):PWasmEdge_ValidatorContext;cdecl;
WasmEdge_ValidatorValidate : function(Cxt:PWasmEdge_ValidatorContext; ASTCxt:PWasmEdge_ASTModuleContext):TWasmEdge_Result;cdecl;
WasmEdge_ValidatorDelete : procedure(Cxt:PWasmEdge_ValidatorContext);cdecl;
WasmEdge_ExecutorCreate : function(ConfCxt:PWasmEdge_ConfigureContext; StatCxt:PWasmEdge_StatisticsContext):PWasmEdge_ExecutorContext;cdecl;
WasmEdge_ExecutorInstantiate : function(Cxt:PWasmEdge_ExecutorContext; ModuleCxt:PPWasmEdge_ModuleInstanceContext; StoreCxt:PWasmEdge_StoreContext; ASTCxt:PWasmEdge_ASTModuleContext):TWasmEdge_Result;cdecl;
WasmEdge_ExecutorRegister : function(Cxt:PWasmEdge_ExecutorContext; ModuleCxt:PPWasmEdge_ModuleInstanceContext; StoreCxt:PWasmEdge_StoreContext; ASTCxt:PWasmEdge_ASTModuleContext; ModuleName:TWasmEdge_String):TWasmEdge_Result;cdecl;
WasmEdge_ExecutorRegisterImport : function(Cxt:PWasmEdge_ExecutorContext; StoreCxt:PWasmEdge_StoreContext; ImportCxt:PWasmEdge_ModuleInstanceContext):TWasmEdge_Result;cdecl;
WasmEdge_ExecutorInvoke : function(Cxt:PWasmEdge_ExecutorContext; FuncCxt:PWasmEdge_FunctionInstanceContext; Params:PWasmEdge_Value; ParamLen:Tuint32_t; Returns:PWasmEdge_Value; 
    ReturnLen:Tuint32_t):TWasmEdge_Result;cdecl;
WasmEdge_ExecutorAsyncInvoke : function(Cxt:PWasmEdge_ExecutorContext; FuncCxt:PWasmEdge_FunctionInstanceContext; Params:PWasmEdge_Value; ParamLen:Tuint32_t):PWasmEdge_Async;cdecl;
WasmEdge_ExecutorDelete : procedure(Cxt:PWasmEdge_ExecutorContext);cdecl;
WasmEdge_StoreCreate : function:PWasmEdge_StoreContext;cdecl;
WasmEdge_StoreFindModule : function(Cxt:PWasmEdge_StoreContext; Name:TWasmEdge_String):PWasmEdge_ModuleInstanceContext;cdecl;
WasmEdge_StoreListModuleLength : function(Cxt:PWasmEdge_StoreContext):Tuint32_t;cdecl;
WasmEdge_StoreListModule : function(Cxt:PWasmEdge_StoreContext; Names:PWasmEdge_String; Len:Tuint32_t):Tuint32_t;cdecl;
WasmEdge_StoreDelete : procedure(Cxt:PWasmEdge_StoreContext);cdecl;
WasmEdge_ModuleInstanceCreate : function(ModuleName:TWasmEdge_String):PWasmEdge_ModuleInstanceContext;cdecl;
Type
  tfinalizer = procedure (para1:pointer); cdecl;
var
WasmEdge_ModuleInstanceCreateWithData : function(ModuleName:TWasmEdge_String; HostData:pointer; Finalizer: tfinalizer):PWasmEdge_ModuleInstanceContext;cdecl;
WasmEdge_ModuleInstanceCreateWASI : function(Args:Ppcchar; ArgLen:Tuint32_t; Envs:Ppcchar; EnvLen:Tuint32_t; Preopens:Ppcchar; 
    PreopenLen:Tuint32_t):PWasmEdge_ModuleInstanceContext;cdecl;
WasmEdge_ModuleInstanceInitWASI : procedure(Cxt:PWasmEdge_ModuleInstanceContext; Args:Ppcchar; ArgLen:Tuint32_t; Envs:Ppcchar; EnvLen:Tuint32_t; 
    Preopens:Ppcchar; PreopenLen:Tuint32_t);cdecl;
WasmEdge_ModuleInstanceWASIGetExitCode : function(Cxt:PWasmEdge_ModuleInstanceContext):Tuint32_t;cdecl;
WasmEdge_ModuleInstanceWASIGetNativeHandler : function(Cxt:PWasmEdge_ModuleInstanceContext; Fd:Tint32_t; NativeHandler:Puint64_t):Tuint32_t;cdecl;
WasmEdge_ModuleInstanceInitWasmEdgeProcess : procedure(AllowedCmds:Ppcchar; CmdsLen:Tuint32_t; AllowAll:TBool);cdecl;
WasmEdge_ModuleInstanceGetModuleName : function(Cxt:PWasmEdge_ModuleInstanceContext):TWasmEdge_String;cdecl;
WasmEdge_ModuleInstanceGetHostData : function(Cxt:PWasmEdge_ModuleInstanceContext):pointer;cdecl;
WasmEdge_ModuleInstanceFindFunction : function(Cxt:PWasmEdge_ModuleInstanceContext; Name:TWasmEdge_String):PWasmEdge_FunctionInstanceContext;cdecl;
WasmEdge_ModuleInstanceFindTable : function(Cxt:PWasmEdge_ModuleInstanceContext; Name:TWasmEdge_String):PWasmEdge_TableInstanceContext;cdecl;
WasmEdge_ModuleInstanceFindMemory : function(Cxt:PWasmEdge_ModuleInstanceContext; Name:TWasmEdge_String):PWasmEdge_MemoryInstanceContext;cdecl;
WasmEdge_ModuleInstanceFindGlobal : function(Cxt:PWasmEdge_ModuleInstanceContext; Name:TWasmEdge_String):PWasmEdge_GlobalInstanceContext;cdecl;
WasmEdge_ModuleInstanceListFunctionLength : function(Cxt:PWasmEdge_ModuleInstanceContext):Tuint32_t;cdecl;
WasmEdge_ModuleInstanceListFunction : function(Cxt:PWasmEdge_ModuleInstanceContext; Names:PWasmEdge_String; Len:Tuint32_t):Tuint32_t;cdecl;
WasmEdge_ModuleInstanceListTableLength : function(Cxt:PWasmEdge_ModuleInstanceContext):Tuint32_t;cdecl;
WasmEdge_ModuleInstanceListTable : function(Cxt:PWasmEdge_ModuleInstanceContext; Names:PWasmEdge_String; Len:Tuint32_t):Tuint32_t;cdecl;
WasmEdge_ModuleInstanceListMemoryLength : function(Cxt:PWasmEdge_ModuleInstanceContext):Tuint32_t;cdecl;
WasmEdge_ModuleInstanceListMemory : function(Cxt:PWasmEdge_ModuleInstanceContext; Names:PWasmEdge_String; Len:Tuint32_t):Tuint32_t;cdecl;
WasmEdge_ModuleInstanceListGlobalLength : function(Cxt:PWasmEdge_ModuleInstanceContext):Tuint32_t;cdecl;
WasmEdge_ModuleInstanceListGlobal : function(Cxt:PWasmEdge_ModuleInstanceContext; Names:PWasmEdge_String; Len:Tuint32_t):Tuint32_t;cdecl;
WasmEdge_ModuleInstanceAddFunction : procedure(Cxt:PWasmEdge_ModuleInstanceContext; Name:TWasmEdge_String; FuncCxt:PWasmEdge_FunctionInstanceContext);cdecl;
WasmEdge_ModuleInstanceAddTable : procedure(Cxt:PWasmEdge_ModuleInstanceContext; Name:TWasmEdge_String; TableCxt:PWasmEdge_TableInstanceContext);cdecl;
WasmEdge_ModuleInstanceAddMemory : procedure(Cxt:PWasmEdge_ModuleInstanceContext; Name:TWasmEdge_String; MemoryCxt:PWasmEdge_MemoryInstanceContext);cdecl;
WasmEdge_ModuleInstanceAddGlobal : procedure(Cxt:PWasmEdge_ModuleInstanceContext; Name:TWasmEdge_String; GlobalCxt:PWasmEdge_GlobalInstanceContext);cdecl;
WasmEdge_ModuleInstanceDelete : procedure(Cxt:PWasmEdge_ModuleInstanceContext);cdecl;
type

  TWasmEdge_HostFunc_t = function (Data:pointer; CallFrameCxt:PWasmEdge_CallingFrameContext; Params:PWasmEdge_Value; Returns:PWasmEdge_Value):TWasmEdge_Result;cdecl;
var
WasmEdge_FunctionInstanceCreate : function(_Type:PWasmEdge_FunctionTypeContext; HostFunc:TWasmEdge_HostFunc_t; Data:pointer; Cost:Tuint64_t):PWasmEdge_FunctionInstanceContext;cdecl;
type

  TWasmEdge_WrapFunc_t = function (This:pointer; Data:pointer; CallFrameCxt:PWasmEdge_CallingFrameContext; Params:PWasmEdge_Value; ParamLen:Tuint32_t; 
               Returns:PWasmEdge_Value; ReturnLen:Tuint32_t):TWasmEdge_Result;cdecl;
var
WasmEdge_FunctionInstanceCreateBinding : function(_Type:PWasmEdge_FunctionTypeContext; WrapFunc:TWasmEdge_WrapFunc_t; Binding:pointer; Data:pointer; Cost:Tuint64_t):PWasmEdge_FunctionInstanceContext;cdecl;
WasmEdge_FunctionInstanceGetFunctionType : function(Cxt:PWasmEdge_FunctionInstanceContext):PWasmEdge_FunctionTypeContext;cdecl;
WasmEdge_FunctionInstanceDelete : procedure(Cxt:PWasmEdge_FunctionInstanceContext);cdecl;
WasmEdge_TableInstanceCreate : function(TabType:PWasmEdge_TableTypeContext):PWasmEdge_TableInstanceContext;cdecl;
WasmEdge_TableInstanceGetTableType : function(Cxt:PWasmEdge_TableInstanceContext):PWasmEdge_TableTypeContext;cdecl;
WasmEdge_TableInstanceGetData : function(Cxt:PWasmEdge_TableInstanceContext; Data:PWasmEdge_Value; Offset:Tuint32_t):TWasmEdge_Result;cdecl;
WasmEdge_TableInstanceSetData : function(Cxt:PWasmEdge_TableInstanceContext; Data:TWasmEdge_Value; Offset:Tuint32_t):TWasmEdge_Result;cdecl;
WasmEdge_TableInstanceGetSize : function(Cxt:PWasmEdge_TableInstanceContext):Tuint32_t;cdecl;
WasmEdge_TableInstanceGrow : function(Cxt:PWasmEdge_TableInstanceContext; Size:Tuint32_t):TWasmEdge_Result;cdecl;
WasmEdge_TableInstanceDelete : procedure(Cxt:PWasmEdge_TableInstanceContext);cdecl;
WasmEdge_MemoryInstanceCreate : function(MemType:PWasmEdge_MemoryTypeContext):PWasmEdge_MemoryInstanceContext;cdecl;
WasmEdge_MemoryInstanceGetMemoryType : function(Cxt:PWasmEdge_MemoryInstanceContext):PWasmEdge_MemoryTypeContext;cdecl;
WasmEdge_MemoryInstanceGetData : function(Cxt:PWasmEdge_MemoryInstanceContext; Data:Puint8_t; Offset:Tuint32_t; Length:Tuint32_t):TWasmEdge_Result;cdecl;
WasmEdge_MemoryInstanceSetData : function(Cxt:PWasmEdge_MemoryInstanceContext; Data:Puint8_t; Offset:Tuint32_t; Length:Tuint32_t):TWasmEdge_Result;cdecl;
WasmEdge_MemoryInstanceGetPointer : function(Cxt:PWasmEdge_MemoryInstanceContext; Offset:Tuint32_t; Length:Tuint32_t):Puint8_t;cdecl;
WasmEdge_MemoryInstanceGetPointerConst : function(Cxt:PWasmEdge_MemoryInstanceContext; Offset:Tuint32_t; Length:Tuint32_t):Puint8_t;cdecl;
WasmEdge_MemoryInstanceGetPageSize : function(Cxt:PWasmEdge_MemoryInstanceContext):Tuint32_t;cdecl;
WasmEdge_MemoryInstanceGrowPage : function(Cxt:PWasmEdge_MemoryInstanceContext; Page:Tuint32_t):TWasmEdge_Result;cdecl;
WasmEdge_MemoryInstanceDelete : procedure(Cxt:PWasmEdge_MemoryInstanceContext);cdecl;
WasmEdge_GlobalInstanceCreate : function(GlobType:PWasmEdge_GlobalTypeContext; Value:TWasmEdge_Value):PWasmEdge_GlobalInstanceContext;cdecl;
WasmEdge_GlobalInstanceGetGlobalType : function(Cxt:PWasmEdge_GlobalInstanceContext):PWasmEdge_GlobalTypeContext;cdecl;
WasmEdge_GlobalInstanceGetValue : function(Cxt:PWasmEdge_GlobalInstanceContext):TWasmEdge_Value;cdecl;
WasmEdge_GlobalInstanceSetValue : procedure(Cxt:PWasmEdge_GlobalInstanceContext; Value:TWasmEdge_Value);cdecl;
WasmEdge_GlobalInstanceDelete : procedure(Cxt:PWasmEdge_GlobalInstanceContext);cdecl;
WasmEdge_CallingFrameGetExecutor : function(Cxt:PWasmEdge_CallingFrameContext):PWasmEdge_ExecutorContext;cdecl;
WasmEdge_CallingFrameGetModuleInstance : function(Cxt:PWasmEdge_CallingFrameContext):PWasmEdge_ModuleInstanceContext;cdecl;
WasmEdge_CallingFrameGetMemoryInstance : function(Cxt:PWasmEdge_CallingFrameContext; Idx:Tuint32_t):PWasmEdge_MemoryInstanceContext;cdecl;
WasmEdge_AsyncWait : procedure(Cxt:PWasmEdge_Async);
WasmEdge_AsyncWaitFor : function(Cxt:PWasmEdge_Async; Milliseconds:Tuint64_t):TBool;
WasmEdge_AsyncCancel : procedure(Cxt:PWasmEdge_Async);
WasmEdge_AsyncGetReturnsLength : function(Cxt:PWasmEdge_Async):Tuint32_t;
WasmEdge_AsyncGet : function(Cxt:PWasmEdge_Async; Returns:PWasmEdge_Value; ReturnLen:Tuint32_t):TWasmEdge_Result;
WasmEdge_AsyncDelete : procedure(Cxt:PWasmEdge_Async);
WasmEdge_VMCreate : function(ConfCxt:PWasmEdge_ConfigureContext; StoreCxt:PWasmEdge_StoreContext):PWasmEdge_VMContext;cdecl;
WasmEdge_VMRegisterModuleFromFile : function(Cxt:PWasmEdge_VMContext; ModuleName:TWasmEdge_String; Path:pcchar):TWasmEdge_Result;cdecl;
WasmEdge_VMRegisterModuleFromBuffer : function(Cxt:PWasmEdge_VMContext; ModuleName:TWasmEdge_String; Buf:Puint8_t; BufLen:Tuint32_t):TWasmEdge_Result;cdecl;
WasmEdge_VMRegisterModuleFromASTModule : function(Cxt:PWasmEdge_VMContext; ModuleName:TWasmEdge_String; ASTCxt:PWasmEdge_ASTModuleContext):TWasmEdge_Result;cdecl;
WasmEdge_VMRegisterModuleFromImport : function(Cxt:PWasmEdge_VMContext; ImportCxt:PWasmEdge_ModuleInstanceContext):TWasmEdge_Result;cdecl;
WasmEdge_VMRunWasmFromFile : function(Cxt:PWasmEdge_VMContext; Path:pcchar; FuncName:TWasmEdge_String; Params:PWasmEdge_Value; ParamLen:Tuint32_t; 
    Returns:PWasmEdge_Value; ReturnLen:Tuint32_t):TWasmEdge_Result;cdecl;
WasmEdge_VMRunWasmFromBuffer : function(Cxt:PWasmEdge_VMContext; Buf:Puint8_t; BufLen:Tuint32_t; FuncName:TWasmEdge_String; Params:PWasmEdge_Value; 
    ParamLen:Tuint32_t; Returns:PWasmEdge_Value; ReturnLen:Tuint32_t):TWasmEdge_Result;cdecl;
WasmEdge_VMRunWasmFromASTModule : function(Cxt:PWasmEdge_VMContext; ASTCxt:PWasmEdge_ASTModuleContext; FuncName:TWasmEdge_String; Params:PWasmEdge_Value; ParamLen:Tuint32_t; 
    Returns:PWasmEdge_Value; ReturnLen:Tuint32_t):TWasmEdge_Result;cdecl;
WasmEdge_VMAsyncRunWasmFromFile : function(Cxt:PWasmEdge_VMContext; Path:pcchar; FuncName:TWasmEdge_String; Params:PWasmEdge_Value; ParamLen:Tuint32_t):PWasmEdge_Async;cdecl;
WasmEdge_VMAsyncRunWasmFromBuffer : function(Cxt:PWasmEdge_VMContext; Buf:Puint8_t; BufLen:Tuint32_t; FuncName:TWasmEdge_String; Params:PWasmEdge_Value; 
    ParamLen:Tuint32_t):PWasmEdge_Async;cdecl;
WasmEdge_VMAsyncRunWasmFromASTModule : function(Cxt:PWasmEdge_VMContext; ASTCxt:PWasmEdge_ASTModuleContext; FuncName:TWasmEdge_String; Params:PWasmEdge_Value; ParamLen:Tuint32_t):PWasmEdge_Async;cdecl;
WasmEdge_VMLoadWasmFromFile : function(Cxt:PWasmEdge_VMContext; Path:pcchar):TWasmEdge_Result;cdecl;
WasmEdge_VMLoadWasmFromBuffer : function(Cxt:PWasmEdge_VMContext; Buf:Puint8_t; BufLen:Tuint32_t):TWasmEdge_Result;cdecl;
WasmEdge_VMLoadWasmFromASTModule : function(Cxt:PWasmEdge_VMContext; ASTCxt:PWasmEdge_ASTModuleContext):TWasmEdge_Result;cdecl;
WasmEdge_VMValidate : function(Cxt:PWasmEdge_VMContext):TWasmEdge_Result;cdecl;
WasmEdge_VMInstantiate : function(Cxt:PWasmEdge_VMContext):TWasmEdge_Result;cdecl;
WasmEdge_VMExecute : function(Cxt:PWasmEdge_VMContext; FuncName:TWasmEdge_String; Params:PWasmEdge_Value; ParamLen:Tuint32_t; Returns:PWasmEdge_Value; 
    ReturnLen:Tuint32_t):TWasmEdge_Result;cdecl;
WasmEdge_VMExecuteRegistered : function(Cxt:PWasmEdge_VMContext; ModuleName:TWasmEdge_String; FuncName:TWasmEdge_String; Params:PWasmEdge_Value; ParamLen:Tuint32_t; 
    Returns:PWasmEdge_Value; ReturnLen:Tuint32_t):TWasmEdge_Result;cdecl;
WasmEdge_VMAsyncExecute : function(Cxt:PWasmEdge_VMContext; FuncName:TWasmEdge_String; Params:PWasmEdge_Value; ParamLen:Tuint32_t):PWasmEdge_Async;cdecl;
WasmEdge_VMAsyncExecuteRegistered : function(Cxt:PWasmEdge_VMContext; ModuleName:TWasmEdge_String; FuncName:TWasmEdge_String; Params:PWasmEdge_Value; ParamLen:Tuint32_t):PWasmEdge_Async;cdecl;
WasmEdge_VMGetFunctionType : function(Cxt:PWasmEdge_VMContext; FuncName:TWasmEdge_String):PWasmEdge_FunctionTypeContext;cdecl;
WasmEdge_VMGetFunctionTypeRegistered : function(Cxt:PWasmEdge_VMContext; ModuleName:TWasmEdge_String; FuncName:TWasmEdge_String):PWasmEdge_FunctionTypeContext;cdecl;
WasmEdge_VMCleanup : procedure(Cxt:PWasmEdge_VMContext);cdecl;
WasmEdge_VMGetFunctionListLength : function(Cxt:PWasmEdge_VMContext):Tuint32_t;cdecl;
WasmEdge_VMGetFunctionList : function(Cxt:PWasmEdge_VMContext; Names:PWasmEdge_String; FuncTypes:PPWasmEdge_FunctionTypeContext; Len:Tuint32_t):Tuint32_t;cdecl;
WasmEdge_VMGetImportModuleContext : function(Cxt:PWasmEdge_VMContext; Reg:TWasmEdge_HostRegistration):PWasmEdge_ModuleInstanceContext;cdecl;
WasmEdge_VMGetActiveModule : function(Cxt:PWasmEdge_VMContext):PWasmEdge_ModuleInstanceContext;cdecl;
WasmEdge_VMGetRegisteredModule : function(Cxt:PWasmEdge_VMContext; ModuleName:TWasmEdge_String):PWasmEdge_ModuleInstanceContext;cdecl;
WasmEdge_VMListRegisteredModuleLength : function(Cxt:PWasmEdge_VMContext):Tuint32_t;cdecl;
WasmEdge_VMListRegisteredModule : function(Cxt:PWasmEdge_VMContext; Names:PWasmEdge_String; Len:Tuint32_t):Tuint32_t;cdecl;
WasmEdge_VMGetStoreContext : function(Cxt:PWasmEdge_VMContext):PWasmEdge_StoreContext;cdecl;
WasmEdge_VMGetLoaderContext : function(Cxt:PWasmEdge_VMContext):PWasmEdge_LoaderContext;cdecl;
WasmEdge_VMGetValidatorContext : function(Cxt:PWasmEdge_VMContext):PWasmEdge_ValidatorContext;cdecl;
WasmEdge_VMGetExecutorContext : function(Cxt:PWasmEdge_VMContext):PWasmEdge_ExecutorContext;cdecl;
WasmEdge_VMGetStatisticsContext : function(Cxt:PWasmEdge_VMContext):PWasmEdge_StatisticsContext;cdecl;
WasmEdge_VMDelete : procedure(Cxt:PWasmEdge_VMContext);cdecl;
WasmEdge_Driver_Compiler : function(Argc:Tcint; Argv:Ppcchar):Tcint;cdecl;
WasmEdge_Driver_Tool : function(Argc:Tcint; Argv:Ppcchar):Tcint;cdecl;
WasmEdge_Driver_UniTool : function(Argc:Tcint; Argv:Ppcchar):Tcint;cdecl;
WasmEdge_PluginLoadWithDefaultPaths : procedure;cdecl;
WasmEdge_PluginLoadFromPath : procedure(Path:pcchar);cdecl;
WasmEdge_PluginListPluginsLength : function:Tuint32_t;cdecl;
WasmEdge_PluginListPlugins : function(Names:PWasmEdge_String; Len:Tuint32_t):Tuint32_t;cdecl;
WasmEdge_PluginFind : function(Name:TWasmEdge_String):PWasmEdge_PluginContext;cdecl;
WasmEdge_PluginGetPluginName : function(Cxt:PWasmEdge_PluginContext):TWasmEdge_String;cdecl;
WasmEdge_PluginListModuleLength : function(Cxt:PWasmEdge_PluginContext):Tuint32_t;cdecl;
WasmEdge_PluginListModule : function(Cxt:PWasmEdge_PluginContext; Names:PWasmEdge_String; Len:Tuint32_t):Tuint32_t;cdecl;
WasmEdge_PluginCreateModule : function(Cxt:PWasmEdge_PluginContext; ModuleName:TWasmEdge_String):PWasmEdge_ModuleInstanceContext;cdecl;
WasmEdge_Plugin_GetDescriptor : function:PWasmEdge_PluginDescriptor;cdecl;

WasmEdge_ExecutorExperimentalRegisterPreHostFunction : procedure(Cxt:PWasmEdge_ExecutorContext; Data:pointer; Func:TFinalizer);cdecl;
WasmEdge_ExecutorExperimentalRegisterPostHostFunction : procedure(Cxt:PWasmEdge_ExecutorContext; Data:pointer; Func:TFinalizer);cdecl;

procedure Loadlibwasmedge(const lib : string = LibWasmName);
procedure FreeLibWasmEdge;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.SysUtils, System.DynLibs;
{$ELSE}
uses
   SysUtils, dynlibs;
{$ENDIF}
  var
    hlib : tlibhandle;


  procedure Freelibwasmedge;
    begin
      if hLib=NilHandle then
        exit;
      FreeLibrary(hlib);
      WasmEdge_VersionGet:=nil;
      WasmEdge_VersionGetMajor:=nil;
      WasmEdge_VersionGetMinor:=nil;
      WasmEdge_VersionGetPatch:=nil;
      WasmEdge_LogSetErrorLevel:=nil;
      WasmEdge_LogSetDebugLevel:=nil;
      WasmEdge_LogOff:=nil;
      WasmEdge_ValueGenI32:=nil;
      WasmEdge_ValueGenI64:=nil;
      WasmEdge_ValueGenF32:=nil;
      WasmEdge_ValueGenF64:=nil;
      WasmEdge_ValueGenV128:=nil;
      WasmEdge_ValueGenNullRef:=nil;
      WasmEdge_ValueGenFuncRef:=nil;
      WasmEdge_ValueGenExternRef:=nil;
      WasmEdge_ValueGetI32:=nil;
      WasmEdge_ValueGetI64:=nil;
      WasmEdge_ValueGetF32:=nil;
      WasmEdge_ValueGetF64:=nil;
      WasmEdge_ValueGetV128:=nil;
      WasmEdge_ValueIsNullRef:=nil;
      WasmEdge_ValueGetFuncRef:=nil;
      WasmEdge_ValueGetExternRef:=nil;
      WasmEdge_StringCreateByCString:=nil;
      WasmEdge_StringCreateByBuffer:=nil;
      WasmEdge_StringWrap:=nil;
      WasmEdge_StringIsEqual:=nil;
      WasmEdge_StringCopy:=nil;
      WasmEdge_StringDelete:=nil;
      WasmEdge_ResultOK:=nil;
      WasmEdge_ResultGen:=nil;
      WasmEdge_ResultGetCode:=nil;
      WasmEdge_ResultGetCategory:=nil;
      WasmEdge_ResultGetMessage:=nil;
      WasmEdge_LimitIsEqual:=nil;
      WasmEdge_ConfigureCreate:=nil;
      WasmEdge_ConfigureAddProposal:=nil;
      WasmEdge_ConfigureRemoveProposal:=nil;
      WasmEdge_ConfigureHasProposal:=nil;
      WasmEdge_ConfigureAddHostRegistration:=nil;
      WasmEdge_ConfigureRemoveHostRegistration:=nil;
      WasmEdge_ConfigureHasHostRegistration:=nil;
      WasmEdge_ConfigureSetMaxMemoryPage:=nil;
      WasmEdge_ConfigureGetMaxMemoryPage:=nil;
      WasmEdge_ConfigureSetForceInterpreter:=nil;
      WasmEdge_ConfigureIsForceInterpreter:=nil;
      WasmEdge_ConfigureSetAllowAFUNIX:=nil;
      WasmEdge_ConfigureIsAllowAFUNIX:=nil;
      WasmEdge_ConfigureCompilerSetOptimizationLevel:=nil;
      WasmEdge_ConfigureCompilerGetOptimizationLevel:=nil;
      WasmEdge_ConfigureCompilerSetOutputFormat:=nil;
      WasmEdge_ConfigureCompilerGetOutputFormat:=nil;
      WasmEdge_ConfigureCompilerSetDumpIR:=nil;
      WasmEdge_ConfigureCompilerIsDumpIR:=nil;
      WasmEdge_ConfigureCompilerSetGenericBinary:=nil;
      WasmEdge_ConfigureCompilerIsGenericBinary:=nil;
      WasmEdge_ConfigureCompilerSetInterruptible:=nil;
      WasmEdge_ConfigureCompilerIsInterruptible:=nil;
      WasmEdge_ConfigureStatisticsSetInstructionCounting:=nil;
      WasmEdge_ConfigureStatisticsIsInstructionCounting:=nil;
      WasmEdge_ConfigureStatisticsSetCostMeasuring:=nil;
      WasmEdge_ConfigureStatisticsIsCostMeasuring:=nil;
      WasmEdge_ConfigureStatisticsSetTimeMeasuring:=nil;
      WasmEdge_ConfigureStatisticsIsTimeMeasuring:=nil;
      WasmEdge_ConfigureDelete:=nil;
      WasmEdge_StatisticsCreate:=nil;
      WasmEdge_StatisticsGetInstrCount:=nil;
      WasmEdge_StatisticsGetInstrPerSecond:=nil;
      WasmEdge_StatisticsGetTotalCost:=nil;
      WasmEdge_StatisticsSetCostTable:=nil;
      WasmEdge_StatisticsSetCostLimit:=nil;
      WasmEdge_StatisticsClear:=nil;
      WasmEdge_StatisticsDelete:=nil;
      WasmEdge_ASTModuleListImportsLength:=nil;
      WasmEdge_ASTModuleListImports:=nil;
      WasmEdge_ASTModuleListExportsLength:=nil;
      WasmEdge_ASTModuleListExports:=nil;
      WasmEdge_ASTModuleDelete:=nil;
      WasmEdge_FunctionTypeCreate:=nil;
      WasmEdge_FunctionTypeGetParametersLength:=nil;
      WasmEdge_FunctionTypeGetParameters:=nil;
      WasmEdge_FunctionTypeGetReturnsLength:=nil;
      WasmEdge_FunctionTypeGetReturns:=nil;
      WasmEdge_FunctionTypeDelete:=nil;
      WasmEdge_TableTypeCreate:=nil;
      WasmEdge_TableTypeGetRefType:=nil;
      WasmEdge_TableTypeGetLimit:=nil;
      WasmEdge_TableTypeDelete:=nil;
      WasmEdge_MemoryTypeCreate:=nil;
      WasmEdge_MemoryTypeGetLimit:=nil;
      WasmEdge_MemoryTypeDelete:=nil;
      WasmEdge_GlobalTypeCreate:=nil;
      WasmEdge_GlobalTypeGetValType:=nil;
      WasmEdge_GlobalTypeGetMutability:=nil;
      WasmEdge_GlobalTypeDelete:=nil;
      WasmEdge_ImportTypeGetExternalType:=nil;
      WasmEdge_ImportTypeGetModuleName:=nil;
      WasmEdge_ImportTypeGetExternalName:=nil;
      WasmEdge_ImportTypeGetFunctionType:=nil;
      WasmEdge_ImportTypeGetTableType:=nil;
      WasmEdge_ImportTypeGetMemoryType:=nil;
      WasmEdge_ImportTypeGetGlobalType:=nil;
      WasmEdge_ExportTypeGetExternalType:=nil;
      WasmEdge_ExportTypeGetExternalName:=nil;
      WasmEdge_ExportTypeGetFunctionType:=nil;
      WasmEdge_ExportTypeGetTableType:=nil;
      WasmEdge_ExportTypeGetMemoryType:=nil;
      WasmEdge_ExportTypeGetGlobalType:=nil;
      WasmEdge_CompilerCreate:=nil;
      WasmEdge_CompilerCompile:=nil;
      WasmEdge_CompilerCompileFromBuffer:=nil;
      WasmEdge_CompilerDelete:=nil;
      WasmEdge_LoaderCreate:=nil;
      WasmEdge_LoaderParseFromFile:=nil;
      WasmEdge_LoaderParseFromBuffer:=nil;
      WasmEdge_LoaderDelete:=nil;
      WasmEdge_ValidatorCreate:=nil;
      WasmEdge_ValidatorValidate:=nil;
      WasmEdge_ValidatorDelete:=nil;
      WasmEdge_ExecutorCreate:=nil;
      WasmEdge_ExecutorInstantiate:=nil;
      WasmEdge_ExecutorRegister:=nil;
      WasmEdge_ExecutorRegisterImport:=nil;
      WasmEdge_ExecutorInvoke:=nil;
      WasmEdge_ExecutorAsyncInvoke:=nil;
      WasmEdge_ExecutorDelete:=nil;
      WasmEdge_StoreCreate:=nil;
      WasmEdge_StoreFindModule:=nil;
      WasmEdge_StoreListModuleLength:=nil;
      WasmEdge_StoreListModule:=nil;
      WasmEdge_StoreDelete:=nil;
      WasmEdge_ModuleInstanceCreate:=nil;
      WasmEdge_ModuleInstanceCreateWithData:=nil;
      WasmEdge_ModuleInstanceCreateWASI:=nil;
      WasmEdge_ModuleInstanceInitWASI:=nil;
      WasmEdge_ModuleInstanceWASIGetExitCode:=nil;
      WasmEdge_ModuleInstanceWASIGetNativeHandler:=nil;
      WasmEdge_ModuleInstanceInitWasmEdgeProcess:=nil;
      WasmEdge_ModuleInstanceGetModuleName:=nil;
      WasmEdge_ModuleInstanceGetHostData:=nil;
      WasmEdge_ModuleInstanceFindFunction:=nil;
      WasmEdge_ModuleInstanceFindTable:=nil;
      WasmEdge_ModuleInstanceFindMemory:=nil;
      WasmEdge_ModuleInstanceFindGlobal:=nil;
      WasmEdge_ModuleInstanceListFunctionLength:=nil;
      WasmEdge_ModuleInstanceListFunction:=nil;
      WasmEdge_ModuleInstanceListTableLength:=nil;
      WasmEdge_ModuleInstanceListTable:=nil;
      WasmEdge_ModuleInstanceListMemoryLength:=nil;
      WasmEdge_ModuleInstanceListMemory:=nil;
      WasmEdge_ModuleInstanceListGlobalLength:=nil;
      WasmEdge_ModuleInstanceListGlobal:=nil;
      WasmEdge_ModuleInstanceAddFunction:=nil;
      WasmEdge_ModuleInstanceAddTable:=nil;
      WasmEdge_ModuleInstanceAddMemory:=nil;
      WasmEdge_ModuleInstanceAddGlobal:=nil;
      WasmEdge_ModuleInstanceDelete:=nil;
      WasmEdge_FunctionInstanceCreate:=nil;
      WasmEdge_FunctionInstanceCreateBinding:=nil;
      WasmEdge_FunctionInstanceGetFunctionType:=nil;
      WasmEdge_FunctionInstanceDelete:=nil;
      WasmEdge_TableInstanceCreate:=nil;
      WasmEdge_TableInstanceGetTableType:=nil;
      WasmEdge_TableInstanceGetData:=nil;
      WasmEdge_TableInstanceSetData:=nil;
      WasmEdge_TableInstanceGetSize:=nil;
      WasmEdge_TableInstanceGrow:=nil;
      WasmEdge_TableInstanceDelete:=nil;
      WasmEdge_MemoryInstanceCreate:=nil;
      WasmEdge_MemoryInstanceGetMemoryType:=nil;
      WasmEdge_MemoryInstanceGetData:=nil;
      WasmEdge_MemoryInstanceSetData:=nil;
      WasmEdge_MemoryInstanceGetPointer:=nil;
      WasmEdge_MemoryInstanceGetPointerConst:=nil;
      WasmEdge_MemoryInstanceGetPageSize:=nil;
      WasmEdge_MemoryInstanceGrowPage:=nil;
      WasmEdge_MemoryInstanceDelete:=nil;
      WasmEdge_GlobalInstanceCreate:=nil;
      WasmEdge_GlobalInstanceGetGlobalType:=nil;
      WasmEdge_GlobalInstanceGetValue:=nil;
      WasmEdge_GlobalInstanceSetValue:=nil;
      WasmEdge_GlobalInstanceDelete:=nil;
      WasmEdge_CallingFrameGetExecutor:=nil;
      WasmEdge_CallingFrameGetModuleInstance:=nil;
      WasmEdge_CallingFrameGetMemoryInstance:=nil;
      WasmEdge_AsyncWait:=nil;
      WasmEdge_AsyncWaitFor:=nil;
      WasmEdge_AsyncCancel:=nil;
      WasmEdge_AsyncGetReturnsLength:=nil;
      WasmEdge_AsyncGet:=nil;
      WasmEdge_AsyncDelete:=nil;
      WasmEdge_VMCreate:=nil;
      WasmEdge_VMRegisterModuleFromFile:=nil;
      WasmEdge_VMRegisterModuleFromBuffer:=nil;
      WasmEdge_VMRegisterModuleFromASTModule:=nil;
      WasmEdge_VMRegisterModuleFromImport:=nil;
      WasmEdge_VMRunWasmFromFile:=nil;
      WasmEdge_VMRunWasmFromBuffer:=nil;
      WasmEdge_VMRunWasmFromASTModule:=nil;
      WasmEdge_VMAsyncRunWasmFromFile:=nil;
      WasmEdge_VMAsyncRunWasmFromBuffer:=nil;
      WasmEdge_VMAsyncRunWasmFromASTModule:=nil;
      WasmEdge_VMLoadWasmFromFile:=nil;
      WasmEdge_VMLoadWasmFromBuffer:=nil;
      WasmEdge_VMLoadWasmFromASTModule:=nil;
      WasmEdge_VMValidate:=nil;
      WasmEdge_VMInstantiate:=nil;
      WasmEdge_VMExecute:=nil;
      WasmEdge_VMExecuteRegistered:=nil;
      WasmEdge_VMAsyncExecute:=nil;
      WasmEdge_VMAsyncExecuteRegistered:=nil;
      WasmEdge_VMGetFunctionType:=nil;
      WasmEdge_VMGetFunctionTypeRegistered:=nil;
      WasmEdge_VMCleanup:=nil;
      WasmEdge_VMGetFunctionListLength:=nil;
      WasmEdge_VMGetFunctionList:=nil;
      WasmEdge_VMGetImportModuleContext:=nil;
      WasmEdge_VMGetActiveModule:=nil;
      WasmEdge_VMGetRegisteredModule:=nil;
      WasmEdge_VMListRegisteredModuleLength:=nil;
      WasmEdge_VMListRegisteredModule:=nil;
      WasmEdge_VMGetStoreContext:=nil;
      WasmEdge_VMGetLoaderContext:=nil;
      WasmEdge_VMGetValidatorContext:=nil;
      WasmEdge_VMGetExecutorContext:=nil;
      WasmEdge_VMGetStatisticsContext:=nil;
      WasmEdge_VMDelete:=nil;
      WasmEdge_Driver_Compiler:=nil;
      WasmEdge_Driver_Tool:=nil;
      WasmEdge_Driver_UniTool:=nil;
      WasmEdge_PluginLoadWithDefaultPaths:=nil;
      WasmEdge_PluginLoadFromPath:=nil;
      WasmEdge_PluginListPluginsLength:=nil;
      WasmEdge_PluginListPlugins:=nil;
      WasmEdge_PluginFind:=nil;
      WasmEdge_PluginGetPluginName:=nil;
      WasmEdge_PluginListModuleLength:=nil;
      WasmEdge_PluginListModule:=nil;
      WasmEdge_PluginCreateModule:=nil;
      WasmEdge_Plugin_GetDescriptor:=nil;
      WasmEdge_ExecutorExperimentalRegisterPreHostFunction:=nil;
      WasmEdge_ExecutorExperimentalRegisterPostHostFunction:=nil;
    end;


  procedure Loadlibwasmedge(const lib : string);
    begin
      Freelibwasmedge;
      hlib:=LoadLibrary(lib);
      if (hlib=0) then
        raise Exception.Create(format('Could not load library: %s',[lib]));

      pointer(WasmEdge_VersionGet):=GetProcAddress(hlib,'WasmEdge_VersionGet');
      pointer(WasmEdge_VersionGetMajor):=GetProcAddress(hlib,'WasmEdge_VersionGetMajor');
      pointer(WasmEdge_VersionGetMinor):=GetProcAddress(hlib,'WasmEdge_VersionGetMinor');
      pointer(WasmEdge_VersionGetPatch):=GetProcAddress(hlib,'WasmEdge_VersionGetPatch');
      pointer(WasmEdge_LogSetErrorLevel):=GetProcAddress(hlib,'WasmEdge_LogSetErrorLevel');
      pointer(WasmEdge_LogSetDebugLevel):=GetProcAddress(hlib,'WasmEdge_LogSetDebugLevel');
      pointer(WasmEdge_LogOff):=GetProcAddress(hlib,'WasmEdge_LogOff');
      pointer(WasmEdge_ValueGenI32):=GetProcAddress(hlib,'WasmEdge_ValueGenI32');
      pointer(WasmEdge_ValueGenI64):=GetProcAddress(hlib,'WasmEdge_ValueGenI64');
      pointer(WasmEdge_ValueGenF32):=GetProcAddress(hlib,'WasmEdge_ValueGenF32');
      pointer(WasmEdge_ValueGenF64):=GetProcAddress(hlib,'WasmEdge_ValueGenF64');
      pointer(WasmEdge_ValueGenV128):=GetProcAddress(hlib,'WasmEdge_ValueGenV128');
      pointer(WasmEdge_ValueGenNullRef):=GetProcAddress(hlib,'WasmEdge_ValueGenNullRef');
      pointer(WasmEdge_ValueGenFuncRef):=GetProcAddress(hlib,'WasmEdge_ValueGenFuncRef');
      pointer(WasmEdge_ValueGenExternRef):=GetProcAddress(hlib,'WasmEdge_ValueGenExternRef');
      pointer(WasmEdge_ValueGetI32):=GetProcAddress(hlib,'WasmEdge_ValueGetI32');
      pointer(WasmEdge_ValueGetI64):=GetProcAddress(hlib,'WasmEdge_ValueGetI64');
      pointer(WasmEdge_ValueGetF32):=GetProcAddress(hlib,'WasmEdge_ValueGetF32');
      pointer(WasmEdge_ValueGetF64):=GetProcAddress(hlib,'WasmEdge_ValueGetF64');
      pointer(WasmEdge_ValueGetV128):=GetProcAddress(hlib,'WasmEdge_ValueGetV128');
      pointer(WasmEdge_ValueIsNullRef):=GetProcAddress(hlib,'WasmEdge_ValueIsNullRef');
      pointer(WasmEdge_ValueGetFuncRef):=GetProcAddress(hlib,'WasmEdge_ValueGetFuncRef');
      pointer(WasmEdge_ValueGetExternRef):=GetProcAddress(hlib,'WasmEdge_ValueGetExternRef');
      pointer(WasmEdge_StringCreateByCString):=GetProcAddress(hlib,'WasmEdge_StringCreateByCString');
      pointer(WasmEdge_StringCreateByBuffer):=GetProcAddress(hlib,'WasmEdge_StringCreateByBuffer');
      pointer(WasmEdge_StringWrap):=GetProcAddress(hlib,'WasmEdge_StringWrap');
      pointer(WasmEdge_StringIsEqual):=GetProcAddress(hlib,'WasmEdge_StringIsEqual');
      pointer(WasmEdge_StringCopy):=GetProcAddress(hlib,'WasmEdge_StringCopy');
      pointer(WasmEdge_StringDelete):=GetProcAddress(hlib,'WasmEdge_StringDelete');
      pointer(WasmEdge_ResultOK):=GetProcAddress(hlib,'WasmEdge_ResultOK');
      pointer(WasmEdge_ResultGen):=GetProcAddress(hlib,'WasmEdge_ResultGen');
      pointer(WasmEdge_ResultGetCode):=GetProcAddress(hlib,'WasmEdge_ResultGetCode');
      pointer(WasmEdge_ResultGetCategory):=GetProcAddress(hlib,'WasmEdge_ResultGetCategory');
      pointer(WasmEdge_ResultGetMessage):=GetProcAddress(hlib,'WasmEdge_ResultGetMessage');
      pointer(WasmEdge_LimitIsEqual):=GetProcAddress(hlib,'WasmEdge_LimitIsEqual');
      pointer(WasmEdge_ConfigureCreate):=GetProcAddress(hlib,'WasmEdge_ConfigureCreate');
      pointer(WasmEdge_ConfigureAddProposal):=GetProcAddress(hlib,'WasmEdge_ConfigureAddProposal');
      pointer(WasmEdge_ConfigureRemoveProposal):=GetProcAddress(hlib,'WasmEdge_ConfigureRemoveProposal');
      pointer(WasmEdge_ConfigureHasProposal):=GetProcAddress(hlib,'WasmEdge_ConfigureHasProposal');
      pointer(WasmEdge_ConfigureAddHostRegistration):=GetProcAddress(hlib,'WasmEdge_ConfigureAddHostRegistration');
      pointer(WasmEdge_ConfigureRemoveHostRegistration):=GetProcAddress(hlib,'WasmEdge_ConfigureRemoveHostRegistration');
      pointer(WasmEdge_ConfigureHasHostRegistration):=GetProcAddress(hlib,'WasmEdge_ConfigureHasHostRegistration');
      pointer(WasmEdge_ConfigureSetMaxMemoryPage):=GetProcAddress(hlib,'WasmEdge_ConfigureSetMaxMemoryPage');
      pointer(WasmEdge_ConfigureGetMaxMemoryPage):=GetProcAddress(hlib,'WasmEdge_ConfigureGetMaxMemoryPage');
      pointer(WasmEdge_ConfigureSetForceInterpreter):=GetProcAddress(hlib,'WasmEdge_ConfigureSetForceInterpreter');
      pointer(WasmEdge_ConfigureIsForceInterpreter):=GetProcAddress(hlib,'WasmEdge_ConfigureIsForceInterpreter');
      pointer(WasmEdge_ConfigureSetAllowAFUNIX):=GetProcAddress(hlib,'WasmEdge_ConfigureSetAllowAFUNIX');
      pointer(WasmEdge_ConfigureIsAllowAFUNIX):=GetProcAddress(hlib,'WasmEdge_ConfigureIsAllowAFUNIX');
      pointer(WasmEdge_ConfigureCompilerSetOptimizationLevel):=GetProcAddress(hlib,'WasmEdge_ConfigureCompilerSetOptimizationLevel');
      pointer(WasmEdge_ConfigureCompilerGetOptimizationLevel):=GetProcAddress(hlib,'WasmEdge_ConfigureCompilerGetOptimizationLevel');
      pointer(WasmEdge_ConfigureCompilerSetOutputFormat):=GetProcAddress(hlib,'WasmEdge_ConfigureCompilerSetOutputFormat');
      pointer(WasmEdge_ConfigureCompilerGetOutputFormat):=GetProcAddress(hlib,'WasmEdge_ConfigureCompilerGetOutputFormat');
      pointer(WasmEdge_ConfigureCompilerSetDumpIR):=GetProcAddress(hlib,'WasmEdge_ConfigureCompilerSetDumpIR');
      pointer(WasmEdge_ConfigureCompilerIsDumpIR):=GetProcAddress(hlib,'WasmEdge_ConfigureCompilerIsDumpIR');
      pointer(WasmEdge_ConfigureCompilerSetGenericBinary):=GetProcAddress(hlib,'WasmEdge_ConfigureCompilerSetGenericBinary');
      pointer(WasmEdge_ConfigureCompilerIsGenericBinary):=GetProcAddress(hlib,'WasmEdge_ConfigureCompilerIsGenericBinary');
      pointer(WasmEdge_ConfigureCompilerSetInterruptible):=GetProcAddress(hlib,'WasmEdge_ConfigureCompilerSetInterruptible');
      pointer(WasmEdge_ConfigureCompilerIsInterruptible):=GetProcAddress(hlib,'WasmEdge_ConfigureCompilerIsInterruptible');
      pointer(WasmEdge_ConfigureStatisticsSetInstructionCounting):=GetProcAddress(hlib,'WasmEdge_ConfigureStatisticsSetInstructionCounting');
      pointer(WasmEdge_ConfigureStatisticsIsInstructionCounting):=GetProcAddress(hlib,'WasmEdge_ConfigureStatisticsIsInstructionCounting');
      pointer(WasmEdge_ConfigureStatisticsSetCostMeasuring):=GetProcAddress(hlib,'WasmEdge_ConfigureStatisticsSetCostMeasuring');
      pointer(WasmEdge_ConfigureStatisticsIsCostMeasuring):=GetProcAddress(hlib,'WasmEdge_ConfigureStatisticsIsCostMeasuring');
      pointer(WasmEdge_ConfigureStatisticsSetTimeMeasuring):=GetProcAddress(hlib,'WasmEdge_ConfigureStatisticsSetTimeMeasuring');
      pointer(WasmEdge_ConfigureStatisticsIsTimeMeasuring):=GetProcAddress(hlib,'WasmEdge_ConfigureStatisticsIsTimeMeasuring');
      pointer(WasmEdge_ConfigureDelete):=GetProcAddress(hlib,'WasmEdge_ConfigureDelete');
      pointer(WasmEdge_StatisticsCreate):=GetProcAddress(hlib,'WasmEdge_StatisticsCreate');
      pointer(WasmEdge_StatisticsGetInstrCount):=GetProcAddress(hlib,'WasmEdge_StatisticsGetInstrCount');
      pointer(WasmEdge_StatisticsGetInstrPerSecond):=GetProcAddress(hlib,'WasmEdge_StatisticsGetInstrPerSecond');
      pointer(WasmEdge_StatisticsGetTotalCost):=GetProcAddress(hlib,'WasmEdge_StatisticsGetTotalCost');
      pointer(WasmEdge_StatisticsSetCostTable):=GetProcAddress(hlib,'WasmEdge_StatisticsSetCostTable');
      pointer(WasmEdge_StatisticsSetCostLimit):=GetProcAddress(hlib,'WasmEdge_StatisticsSetCostLimit');
      pointer(WasmEdge_StatisticsClear):=GetProcAddress(hlib,'WasmEdge_StatisticsClear');
      pointer(WasmEdge_StatisticsDelete):=GetProcAddress(hlib,'WasmEdge_StatisticsDelete');
      pointer(WasmEdge_ASTModuleListImportsLength):=GetProcAddress(hlib,'WasmEdge_ASTModuleListImportsLength');
      pointer(WasmEdge_ASTModuleListImports):=GetProcAddress(hlib,'WasmEdge_ASTModuleListImports');
      pointer(WasmEdge_ASTModuleListExportsLength):=GetProcAddress(hlib,'WasmEdge_ASTModuleListExportsLength');
      pointer(WasmEdge_ASTModuleListExports):=GetProcAddress(hlib,'WasmEdge_ASTModuleListExports');
      pointer(WasmEdge_ASTModuleDelete):=GetProcAddress(hlib,'WasmEdge_ASTModuleDelete');
      pointer(WasmEdge_FunctionTypeCreate):=GetProcAddress(hlib,'WasmEdge_FunctionTypeCreate');
      pointer(WasmEdge_FunctionTypeGetParametersLength):=GetProcAddress(hlib,'WasmEdge_FunctionTypeGetParametersLength');
      pointer(WasmEdge_FunctionTypeGetParameters):=GetProcAddress(hlib,'WasmEdge_FunctionTypeGetParameters');
      pointer(WasmEdge_FunctionTypeGetReturnsLength):=GetProcAddress(hlib,'WasmEdge_FunctionTypeGetReturnsLength');
      pointer(WasmEdge_FunctionTypeGetReturns):=GetProcAddress(hlib,'WasmEdge_FunctionTypeGetReturns');
      pointer(WasmEdge_FunctionTypeDelete):=GetProcAddress(hlib,'WasmEdge_FunctionTypeDelete');
      pointer(WasmEdge_TableTypeCreate):=GetProcAddress(hlib,'WasmEdge_TableTypeCreate');
      pointer(WasmEdge_TableTypeGetRefType):=GetProcAddress(hlib,'WasmEdge_TableTypeGetRefType');
      pointer(WasmEdge_TableTypeGetLimit):=GetProcAddress(hlib,'WasmEdge_TableTypeGetLimit');
      pointer(WasmEdge_TableTypeDelete):=GetProcAddress(hlib,'WasmEdge_TableTypeDelete');
      pointer(WasmEdge_MemoryTypeCreate):=GetProcAddress(hlib,'WasmEdge_MemoryTypeCreate');
      pointer(WasmEdge_MemoryTypeGetLimit):=GetProcAddress(hlib,'WasmEdge_MemoryTypeGetLimit');
      pointer(WasmEdge_MemoryTypeDelete):=GetProcAddress(hlib,'WasmEdge_MemoryTypeDelete');
      pointer(WasmEdge_GlobalTypeCreate):=GetProcAddress(hlib,'WasmEdge_GlobalTypeCreate');
      pointer(WasmEdge_GlobalTypeGetValType):=GetProcAddress(hlib,'WasmEdge_GlobalTypeGetValType');
      pointer(WasmEdge_GlobalTypeGetMutability):=GetProcAddress(hlib,'WasmEdge_GlobalTypeGetMutability');
      pointer(WasmEdge_GlobalTypeDelete):=GetProcAddress(hlib,'WasmEdge_GlobalTypeDelete');
      pointer(WasmEdge_ImportTypeGetExternalType):=GetProcAddress(hlib,'WasmEdge_ImportTypeGetExternalType');
      pointer(WasmEdge_ImportTypeGetModuleName):=GetProcAddress(hlib,'WasmEdge_ImportTypeGetModuleName');
      pointer(WasmEdge_ImportTypeGetExternalName):=GetProcAddress(hlib,'WasmEdge_ImportTypeGetExternalName');
      pointer(WasmEdge_ImportTypeGetFunctionType):=GetProcAddress(hlib,'WasmEdge_ImportTypeGetFunctionType');
      pointer(WasmEdge_ImportTypeGetTableType):=GetProcAddress(hlib,'WasmEdge_ImportTypeGetTableType');
      pointer(WasmEdge_ImportTypeGetMemoryType):=GetProcAddress(hlib,'WasmEdge_ImportTypeGetMemoryType');
      pointer(WasmEdge_ImportTypeGetGlobalType):=GetProcAddress(hlib,'WasmEdge_ImportTypeGetGlobalType');
      pointer(WasmEdge_ExportTypeGetExternalType):=GetProcAddress(hlib,'WasmEdge_ExportTypeGetExternalType');
      pointer(WasmEdge_ExportTypeGetExternalName):=GetProcAddress(hlib,'WasmEdge_ExportTypeGetExternalName');
      pointer(WasmEdge_ExportTypeGetFunctionType):=GetProcAddress(hlib,'WasmEdge_ExportTypeGetFunctionType');
      pointer(WasmEdge_ExportTypeGetTableType):=GetProcAddress(hlib,'WasmEdge_ExportTypeGetTableType');
      pointer(WasmEdge_ExportTypeGetMemoryType):=GetProcAddress(hlib,'WasmEdge_ExportTypeGetMemoryType');
      pointer(WasmEdge_ExportTypeGetGlobalType):=GetProcAddress(hlib,'WasmEdge_ExportTypeGetGlobalType');
      pointer(WasmEdge_CompilerCreate):=GetProcAddress(hlib,'WasmEdge_CompilerCreate');
      pointer(WasmEdge_CompilerCompile):=GetProcAddress(hlib,'WasmEdge_CompilerCompile');
      pointer(WasmEdge_CompilerCompileFromBuffer):=GetProcAddress(hlib,'WasmEdge_CompilerCompileFromBuffer');
      pointer(WasmEdge_CompilerDelete):=GetProcAddress(hlib,'WasmEdge_CompilerDelete');
      pointer(WasmEdge_LoaderCreate):=GetProcAddress(hlib,'WasmEdge_LoaderCreate');
      pointer(WasmEdge_LoaderParseFromFile):=GetProcAddress(hlib,'WasmEdge_LoaderParseFromFile');
      pointer(WasmEdge_LoaderParseFromBuffer):=GetProcAddress(hlib,'WasmEdge_LoaderParseFromBuffer');
      pointer(WasmEdge_LoaderDelete):=GetProcAddress(hlib,'WasmEdge_LoaderDelete');
      pointer(WasmEdge_ValidatorCreate):=GetProcAddress(hlib,'WasmEdge_ValidatorCreate');
      pointer(WasmEdge_ValidatorValidate):=GetProcAddress(hlib,'WasmEdge_ValidatorValidate');
      pointer(WasmEdge_ValidatorDelete):=GetProcAddress(hlib,'WasmEdge_ValidatorDelete');
      pointer(WasmEdge_ExecutorCreate):=GetProcAddress(hlib,'WasmEdge_ExecutorCreate');
      pointer(WasmEdge_ExecutorInstantiate):=GetProcAddress(hlib,'WasmEdge_ExecutorInstantiate');
      pointer(WasmEdge_ExecutorRegister):=GetProcAddress(hlib,'WasmEdge_ExecutorRegister');
      pointer(WasmEdge_ExecutorRegisterImport):=GetProcAddress(hlib,'WasmEdge_ExecutorRegisterImport');
      pointer(WasmEdge_ExecutorInvoke):=GetProcAddress(hlib,'WasmEdge_ExecutorInvoke');
      pointer(WasmEdge_ExecutorAsyncInvoke):=GetProcAddress(hlib,'WasmEdge_ExecutorAsyncInvoke');
      pointer(WasmEdge_ExecutorDelete):=GetProcAddress(hlib,'WasmEdge_ExecutorDelete');
      pointer(WasmEdge_StoreCreate):=GetProcAddress(hlib,'WasmEdge_StoreCreate');
      pointer(WasmEdge_StoreFindModule):=GetProcAddress(hlib,'WasmEdge_StoreFindModule');
      pointer(WasmEdge_StoreListModuleLength):=GetProcAddress(hlib,'WasmEdge_StoreListModuleLength');
      pointer(WasmEdge_StoreListModule):=GetProcAddress(hlib,'WasmEdge_StoreListModule');
      pointer(WasmEdge_StoreDelete):=GetProcAddress(hlib,'WasmEdge_StoreDelete');
      pointer(WasmEdge_ModuleInstanceCreate):=GetProcAddress(hlib,'WasmEdge_ModuleInstanceCreate');
      pointer(WasmEdge_ModuleInstanceCreateWithData):=GetProcAddress(hlib,'WasmEdge_ModuleInstanceCreateWithData');
      pointer(WasmEdge_ModuleInstanceCreateWASI):=GetProcAddress(hlib,'WasmEdge_ModuleInstanceCreateWASI');
      pointer(WasmEdge_ModuleInstanceInitWASI):=GetProcAddress(hlib,'WasmEdge_ModuleInstanceInitWASI');
      pointer(WasmEdge_ModuleInstanceWASIGetExitCode):=GetProcAddress(hlib,'WasmEdge_ModuleInstanceWASIGetExitCode');
      pointer(WasmEdge_ModuleInstanceWASIGetNativeHandler):=GetProcAddress(hlib,'WasmEdge_ModuleInstanceWASIGetNativeHandler');
      pointer(WasmEdge_ModuleInstanceInitWasmEdgeProcess):=GetProcAddress(hlib,'WasmEdge_ModuleInstanceInitWasmEdgeProcess');
      pointer(WasmEdge_ModuleInstanceGetModuleName):=GetProcAddress(hlib,'WasmEdge_ModuleInstanceGetModuleName');
      pointer(WasmEdge_ModuleInstanceGetHostData):=GetProcAddress(hlib,'WasmEdge_ModuleInstanceGetHostData');
      pointer(WasmEdge_ModuleInstanceFindFunction):=GetProcAddress(hlib,'WasmEdge_ModuleInstanceFindFunction');
      pointer(WasmEdge_ModuleInstanceFindTable):=GetProcAddress(hlib,'WasmEdge_ModuleInstanceFindTable');
      pointer(WasmEdge_ModuleInstanceFindMemory):=GetProcAddress(hlib,'WasmEdge_ModuleInstanceFindMemory');
      pointer(WasmEdge_ModuleInstanceFindGlobal):=GetProcAddress(hlib,'WasmEdge_ModuleInstanceFindGlobal');
      pointer(WasmEdge_ModuleInstanceListFunctionLength):=GetProcAddress(hlib,'WasmEdge_ModuleInstanceListFunctionLength');
      pointer(WasmEdge_ModuleInstanceListFunction):=GetProcAddress(hlib,'WasmEdge_ModuleInstanceListFunction');
      pointer(WasmEdge_ModuleInstanceListTableLength):=GetProcAddress(hlib,'WasmEdge_ModuleInstanceListTableLength');
      pointer(WasmEdge_ModuleInstanceListTable):=GetProcAddress(hlib,'WasmEdge_ModuleInstanceListTable');
      pointer(WasmEdge_ModuleInstanceListMemoryLength):=GetProcAddress(hlib,'WasmEdge_ModuleInstanceListMemoryLength');
      pointer(WasmEdge_ModuleInstanceListMemory):=GetProcAddress(hlib,'WasmEdge_ModuleInstanceListMemory');
      pointer(WasmEdge_ModuleInstanceListGlobalLength):=GetProcAddress(hlib,'WasmEdge_ModuleInstanceListGlobalLength');
      pointer(WasmEdge_ModuleInstanceListGlobal):=GetProcAddress(hlib,'WasmEdge_ModuleInstanceListGlobal');
      pointer(WasmEdge_ModuleInstanceAddFunction):=GetProcAddress(hlib,'WasmEdge_ModuleInstanceAddFunction');
      pointer(WasmEdge_ModuleInstanceAddTable):=GetProcAddress(hlib,'WasmEdge_ModuleInstanceAddTable');
      pointer(WasmEdge_ModuleInstanceAddMemory):=GetProcAddress(hlib,'WasmEdge_ModuleInstanceAddMemory');
      pointer(WasmEdge_ModuleInstanceAddGlobal):=GetProcAddress(hlib,'WasmEdge_ModuleInstanceAddGlobal');
      pointer(WasmEdge_ModuleInstanceDelete):=GetProcAddress(hlib,'WasmEdge_ModuleInstanceDelete');
      pointer(WasmEdge_FunctionInstanceCreate):=GetProcAddress(hlib,'WasmEdge_FunctionInstanceCreate');
      pointer(WasmEdge_FunctionInstanceCreateBinding):=GetProcAddress(hlib,'WasmEdge_FunctionInstanceCreateBinding');
      pointer(WasmEdge_FunctionInstanceGetFunctionType):=GetProcAddress(hlib,'WasmEdge_FunctionInstanceGetFunctionType');
      pointer(WasmEdge_FunctionInstanceDelete):=GetProcAddress(hlib,'WasmEdge_FunctionInstanceDelete');
      pointer(WasmEdge_TableInstanceCreate):=GetProcAddress(hlib,'WasmEdge_TableInstanceCreate');
      pointer(WasmEdge_TableInstanceGetTableType):=GetProcAddress(hlib,'WasmEdge_TableInstanceGetTableType');
      pointer(WasmEdge_TableInstanceGetData):=GetProcAddress(hlib,'WasmEdge_TableInstanceGetData');
      pointer(WasmEdge_TableInstanceSetData):=GetProcAddress(hlib,'WasmEdge_TableInstanceSetData');
      pointer(WasmEdge_TableInstanceGetSize):=GetProcAddress(hlib,'WasmEdge_TableInstanceGetSize');
      pointer(WasmEdge_TableInstanceGrow):=GetProcAddress(hlib,'WasmEdge_TableInstanceGrow');
      pointer(WasmEdge_TableInstanceDelete):=GetProcAddress(hlib,'WasmEdge_TableInstanceDelete');
      pointer(WasmEdge_MemoryInstanceCreate):=GetProcAddress(hlib,'WasmEdge_MemoryInstanceCreate');
      pointer(WasmEdge_MemoryInstanceGetMemoryType):=GetProcAddress(hlib,'WasmEdge_MemoryInstanceGetMemoryType');
      pointer(WasmEdge_MemoryInstanceGetData):=GetProcAddress(hlib,'WasmEdge_MemoryInstanceGetData');
      pointer(WasmEdge_MemoryInstanceSetData):=GetProcAddress(hlib,'WasmEdge_MemoryInstanceSetData');
      pointer(WasmEdge_MemoryInstanceGetPointer):=GetProcAddress(hlib,'WasmEdge_MemoryInstanceGetPointer');
      pointer(WasmEdge_MemoryInstanceGetPointerConst):=GetProcAddress(hlib,'WasmEdge_MemoryInstanceGetPointerConst');
      pointer(WasmEdge_MemoryInstanceGetPageSize):=GetProcAddress(hlib,'WasmEdge_MemoryInstanceGetPageSize');
      pointer(WasmEdge_MemoryInstanceGrowPage):=GetProcAddress(hlib,'WasmEdge_MemoryInstanceGrowPage');
      pointer(WasmEdge_MemoryInstanceDelete):=GetProcAddress(hlib,'WasmEdge_MemoryInstanceDelete');
      pointer(WasmEdge_GlobalInstanceCreate):=GetProcAddress(hlib,'WasmEdge_GlobalInstanceCreate');
      pointer(WasmEdge_GlobalInstanceGetGlobalType):=GetProcAddress(hlib,'WasmEdge_GlobalInstanceGetGlobalType');
      pointer(WasmEdge_GlobalInstanceGetValue):=GetProcAddress(hlib,'WasmEdge_GlobalInstanceGetValue');
      pointer(WasmEdge_GlobalInstanceSetValue):=GetProcAddress(hlib,'WasmEdge_GlobalInstanceSetValue');
      pointer(WasmEdge_GlobalInstanceDelete):=GetProcAddress(hlib,'WasmEdge_GlobalInstanceDelete');
      pointer(WasmEdge_CallingFrameGetExecutor):=GetProcAddress(hlib,'WasmEdge_CallingFrameGetExecutor');
      pointer(WasmEdge_CallingFrameGetModuleInstance):=GetProcAddress(hlib,'WasmEdge_CallingFrameGetModuleInstance');
      pointer(WasmEdge_CallingFrameGetMemoryInstance):=GetProcAddress(hlib,'WasmEdge_CallingFrameGetMemoryInstance');
      pointer(WasmEdge_AsyncWait):=GetProcAddress(hlib,'WasmEdge_AsyncWait');
      pointer(WasmEdge_AsyncWaitFor):=GetProcAddress(hlib,'WasmEdge_AsyncWaitFor');
      pointer(WasmEdge_AsyncCancel):=GetProcAddress(hlib,'WasmEdge_AsyncCancel');
      pointer(WasmEdge_AsyncGetReturnsLength):=GetProcAddress(hlib,'WasmEdge_AsyncGetReturnsLength');
      pointer(WasmEdge_AsyncGet):=GetProcAddress(hlib,'WasmEdge_AsyncGet');
      pointer(WasmEdge_AsyncDelete):=GetProcAddress(hlib,'WasmEdge_AsyncDelete');
      pointer(WasmEdge_VMCreate):=GetProcAddress(hlib,'WasmEdge_VMCreate');
      pointer(WasmEdge_VMRegisterModuleFromFile):=GetProcAddress(hlib,'WasmEdge_VMRegisterModuleFromFile');
      pointer(WasmEdge_VMRegisterModuleFromBuffer):=GetProcAddress(hlib,'WasmEdge_VMRegisterModuleFromBuffer');
      pointer(WasmEdge_VMRegisterModuleFromASTModule):=GetProcAddress(hlib,'WasmEdge_VMRegisterModuleFromASTModule');
      pointer(WasmEdge_VMRegisterModuleFromImport):=GetProcAddress(hlib,'WasmEdge_VMRegisterModuleFromImport');
      pointer(WasmEdge_VMRunWasmFromFile):=GetProcAddress(hlib,'WasmEdge_VMRunWasmFromFile');
      pointer(WasmEdge_VMRunWasmFromBuffer):=GetProcAddress(hlib,'WasmEdge_VMRunWasmFromBuffer');
      pointer(WasmEdge_VMRunWasmFromASTModule):=GetProcAddress(hlib,'WasmEdge_VMRunWasmFromASTModule');
      pointer(WasmEdge_VMAsyncRunWasmFromFile):=GetProcAddress(hlib,'WasmEdge_VMAsyncRunWasmFromFile');
      pointer(WasmEdge_VMAsyncRunWasmFromBuffer):=GetProcAddress(hlib,'WasmEdge_VMAsyncRunWasmFromBuffer');
      pointer(WasmEdge_VMAsyncRunWasmFromASTModule):=GetProcAddress(hlib,'WasmEdge_VMAsyncRunWasmFromASTModule');
      pointer(WasmEdge_VMLoadWasmFromFile):=GetProcAddress(hlib,'WasmEdge_VMLoadWasmFromFile');
      pointer(WasmEdge_VMLoadWasmFromBuffer):=GetProcAddress(hlib,'WasmEdge_VMLoadWasmFromBuffer');
      pointer(WasmEdge_VMLoadWasmFromASTModule):=GetProcAddress(hlib,'WasmEdge_VMLoadWasmFromASTModule');
      pointer(WasmEdge_VMValidate):=GetProcAddress(hlib,'WasmEdge_VMValidate');
      pointer(WasmEdge_VMInstantiate):=GetProcAddress(hlib,'WasmEdge_VMInstantiate');
      pointer(WasmEdge_VMExecute):=GetProcAddress(hlib,'WasmEdge_VMExecute');
      pointer(WasmEdge_VMExecuteRegistered):=GetProcAddress(hlib,'WasmEdge_VMExecuteRegistered');
      pointer(WasmEdge_VMAsyncExecute):=GetProcAddress(hlib,'WasmEdge_VMAsyncExecute');
      pointer(WasmEdge_VMAsyncExecuteRegistered):=GetProcAddress(hlib,'WasmEdge_VMAsyncExecuteRegistered');
      pointer(WasmEdge_VMGetFunctionType):=GetProcAddress(hlib,'WasmEdge_VMGetFunctionType');
      pointer(WasmEdge_VMGetFunctionTypeRegistered):=GetProcAddress(hlib,'WasmEdge_VMGetFunctionTypeRegistered');
      pointer(WasmEdge_VMCleanup):=GetProcAddress(hlib,'WasmEdge_VMCleanup');
      pointer(WasmEdge_VMGetFunctionListLength):=GetProcAddress(hlib,'WasmEdge_VMGetFunctionListLength');
      pointer(WasmEdge_VMGetFunctionList):=GetProcAddress(hlib,'WasmEdge_VMGetFunctionList');
      pointer(WasmEdge_VMGetImportModuleContext):=GetProcAddress(hlib,'WasmEdge_VMGetImportModuleContext');
      pointer(WasmEdge_VMGetActiveModule):=GetProcAddress(hlib,'WasmEdge_VMGetActiveModule');
      pointer(WasmEdge_VMGetRegisteredModule):=GetProcAddress(hlib,'WasmEdge_VMGetRegisteredModule');
      pointer(WasmEdge_VMListRegisteredModuleLength):=GetProcAddress(hlib,'WasmEdge_VMListRegisteredModuleLength');
      pointer(WasmEdge_VMListRegisteredModule):=GetProcAddress(hlib,'WasmEdge_VMListRegisteredModule');
      pointer(WasmEdge_VMGetStoreContext):=GetProcAddress(hlib,'WasmEdge_VMGetStoreContext');
      pointer(WasmEdge_VMGetLoaderContext):=GetProcAddress(hlib,'WasmEdge_VMGetLoaderContext');
      pointer(WasmEdge_VMGetValidatorContext):=GetProcAddress(hlib,'WasmEdge_VMGetValidatorContext');
      pointer(WasmEdge_VMGetExecutorContext):=GetProcAddress(hlib,'WasmEdge_VMGetExecutorContext');
      pointer(WasmEdge_VMGetStatisticsContext):=GetProcAddress(hlib,'WasmEdge_VMGetStatisticsContext');
      pointer(WasmEdge_VMDelete):=GetProcAddress(hlib,'WasmEdge_VMDelete');
      pointer(WasmEdge_Driver_Compiler):=GetProcAddress(hlib,'WasmEdge_Driver_Compiler');
      pointer(WasmEdge_Driver_Tool):=GetProcAddress(hlib,'WasmEdge_Driver_Tool');
      pointer(WasmEdge_Driver_UniTool):=GetProcAddress(hlib,'WasmEdge_Driver_UniTool');
      pointer(WasmEdge_PluginLoadWithDefaultPaths):=GetProcAddress(hlib,'WasmEdge_PluginLoadWithDefaultPaths');
      pointer(WasmEdge_PluginLoadFromPath):=GetProcAddress(hlib,'WasmEdge_PluginLoadFromPath');
      pointer(WasmEdge_PluginListPluginsLength):=GetProcAddress(hlib,'WasmEdge_PluginListPluginsLength');
      pointer(WasmEdge_PluginListPlugins):=GetProcAddress(hlib,'WasmEdge_PluginListPlugins');
      pointer(WasmEdge_PluginFind):=GetProcAddress(hlib,'WasmEdge_PluginFind');
      pointer(WasmEdge_PluginGetPluginName):=GetProcAddress(hlib,'WasmEdge_PluginGetPluginName');
      pointer(WasmEdge_PluginListModuleLength):=GetProcAddress(hlib,'WasmEdge_PluginListModuleLength');
      pointer(WasmEdge_PluginListModule):=GetProcAddress(hlib,'WasmEdge_PluginListModule');
      pointer(WasmEdge_PluginCreateModule):=GetProcAddress(hlib,'WasmEdge_PluginCreateModule');
      pointer(WasmEdge_Plugin_GetDescriptor):=GetProcAddress(hlib,'WasmEdge_Plugin_GetDescriptor');
      pointer(WasmEdge_ExecutorExperimentalRegisterPreHostFunction):=GetProcAddress(hlib,'WasmEdge_ExecutorExperimentalRegisterPreHostFunction');
      pointer(WasmEdge_ExecutorExperimentalRegisterPostHostFunction):=GetProcAddress(hlib,'WasmEdge_ExecutorExperimentalRegisterPostHostFunction');
    end;


finalization
  Freelibwasmedge;

end.
