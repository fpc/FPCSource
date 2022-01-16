unit googledatastore;
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TValue = Class;
  TReadOptions = Class;
  TPropertyOrder = Class;
  TCommitRequest = Class;
  TRollbackRequest = Class;
  TQuery = Class;
  TEntityResult = Class;
  TGqlQueryParameter = Class;
  TBeginTransactionResponse = Class;
  TFilter = Class;
  TArrayValue = Class;
  TPartitionId = Class;
  TQueryResultBatch = Class;
  TAllocateIdsRequest = Class;
  TPropertyFilter = Class;
  TKindExpression = Class;
  TPathElement = Class;
  TRollbackResponse = Class;
  TPropertyReference = Class;
  TProjection = Class;
  TAllocateIdsResponse = Class;
  TMutationResult = Class;
  TLookupResponse = Class;
  TBeginTransactionRequest = Class;
  TKey = Class;
  TRunQueryResponse = Class;
  TEntity = Class;
  TGqlQuery = Class;
  TCommitResponse = Class;
  TMutation = Class;
  TRunQueryRequest = Class;
  TCompositeFilter = Class;
  TLatLng = Class;
  TLookupRequest = Class;
  TValueArray = Array of TValue;
  TReadOptionsArray = Array of TReadOptions;
  TPropertyOrderArray = Array of TPropertyOrder;
  TCommitRequestArray = Array of TCommitRequest;
  TRollbackRequestArray = Array of TRollbackRequest;
  TQueryArray = Array of TQuery;
  TEntityResultArray = Array of TEntityResult;
  TGqlQueryParameterArray = Array of TGqlQueryParameter;
  TBeginTransactionResponseArray = Array of TBeginTransactionResponse;
  TFilterArray = Array of TFilter;
  TArrayValueArray = Array of TArrayValue;
  TPartitionIdArray = Array of TPartitionId;
  TQueryResultBatchArray = Array of TQueryResultBatch;
  TAllocateIdsRequestArray = Array of TAllocateIdsRequest;
  TPropertyFilterArray = Array of TPropertyFilter;
  TKindExpressionArray = Array of TKindExpression;
  TPathElementArray = Array of TPathElement;
  TRollbackResponseArray = Array of TRollbackResponse;
  TPropertyReferenceArray = Array of TPropertyReference;
  TProjectionArray = Array of TProjection;
  TAllocateIdsResponseArray = Array of TAllocateIdsResponse;
  TMutationResultArray = Array of TMutationResult;
  TLookupResponseArray = Array of TLookupResponse;
  TBeginTransactionRequestArray = Array of TBeginTransactionRequest;
  TKeyArray = Array of TKey;
  TRunQueryResponseArray = Array of TRunQueryResponse;
  TEntityArray = Array of TEntity;
  TGqlQueryArray = Array of TGqlQuery;
  TCommitResponseArray = Array of TCommitResponse;
  TMutationArray = Array of TMutation;
  TRunQueryRequestArray = Array of TRunQueryRequest;
  TCompositeFilterArray = Array of TCompositeFilter;
  TLatLngArray = Array of TLatLng;
  TLookupRequestArray = Array of TLookupRequest;
  //Anonymous types, using auto-generated names
  TEntityTypeproperties = Class;
  TGqlQueryTypenamedBindings = Class;
  TCommitRequestTypemutationsArray = Array of TMutation;
  TQueryTypedistinctOnArray = Array of TPropertyReference;
  TQueryTypeprojectionArray = Array of TProjection;
  TQueryTypeorderArray = Array of TPropertyOrder;
  TQueryTypekindArray = Array of TKindExpression;
  TArrayValueTypevaluesArray = Array of TValue;
  TQueryResultBatchTypeentityResultsArray = Array of TEntityResult;
  TAllocateIdsRequestTypekeysArray = Array of TKey;
  TAllocateIdsResponseTypekeysArray = Array of TKey;
  TLookupResponseTypefoundArray = Array of TEntityResult;
  TLookupResponseTypedeferredArray = Array of TKey;
  TLookupResponseTypemissingArray = Array of TEntityResult;
  TKeyTypepathArray = Array of TPathElement;
  TGqlQueryTypepositionalBindingsArray = Array of TGqlQueryParameter;
  TCommitResponseTypemutationResultsArray = Array of TMutationResult;
  TCompositeFilterTypefiltersArray = Array of TFilter;
  TLookupRequestTypekeysArray = Array of TKey;
  
  { --------------------------------------------------------------------
    TValue
    --------------------------------------------------------------------}
  
  TValue = Class(TGoogleBaseObject)
  Private
    FstringValue : String;
    FarrayValue : TArrayValue;
    FentityValue : TEntity;
    Fmeaning : integer;
    FintegerValue : String;
    FdoubleValue : double;
    FgeoPointValue : TLatLng;
    FblobValue : String;
    FnullValue : String;
    FkeyValue : TKey;
    FbooleanValue : boolean;
    FexcludeFromIndexes : boolean;
    FtimestampValue : String;
  Protected
    //Property setters
    Procedure SetstringValue(AIndex : Integer; const AValue : String); virtual;
    Procedure SetarrayValue(AIndex : Integer; const AValue : TArrayValue); virtual;
    Procedure SetentityValue(AIndex : Integer; const AValue : TEntity); virtual;
    Procedure Setmeaning(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetintegerValue(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdoubleValue(AIndex : Integer; const AValue : double); virtual;
    Procedure SetgeoPointValue(AIndex : Integer; const AValue : TLatLng); virtual;
    Procedure SetblobValue(AIndex : Integer; const AValue : String); virtual;
    Procedure SetnullValue(AIndex : Integer; const AValue : String); virtual;
    Procedure SetkeyValue(AIndex : Integer; const AValue : TKey); virtual;
    Procedure SetbooleanValue(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetexcludeFromIndexes(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SettimestampValue(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property stringValue : String Index 0 Read FstringValue Write SetstringValue;
    Property arrayValue : TArrayValue Index 8 Read FarrayValue Write SetarrayValue;
    Property entityValue : TEntity Index 16 Read FentityValue Write SetentityValue;
    Property meaning : integer Index 24 Read Fmeaning Write Setmeaning;
    Property integerValue : String Index 32 Read FintegerValue Write SetintegerValue;
    Property doubleValue : double Index 40 Read FdoubleValue Write SetdoubleValue;
    Property geoPointValue : TLatLng Index 48 Read FgeoPointValue Write SetgeoPointValue;
    Property blobValue : String Index 56 Read FblobValue Write SetblobValue;
    Property nullValue : String Index 64 Read FnullValue Write SetnullValue;
    Property keyValue : TKey Index 72 Read FkeyValue Write SetkeyValue;
    Property booleanValue : boolean Index 80 Read FbooleanValue Write SetbooleanValue;
    Property excludeFromIndexes : boolean Index 88 Read FexcludeFromIndexes Write SetexcludeFromIndexes;
    Property timestampValue : String Index 96 Read FtimestampValue Write SettimestampValue;
  end;
  TValueClass = Class of TValue;
  
  { --------------------------------------------------------------------
    TReadOptions
    --------------------------------------------------------------------}
  
  TReadOptions = Class(TGoogleBaseObject)
  Private
    Ftransaction : String;
    FreadConsistency : String;
  Protected
    //Property setters
    Procedure Settransaction(AIndex : Integer; const AValue : String); virtual;
    Procedure SetreadConsistency(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property transaction : String Index 0 Read Ftransaction Write Settransaction;
    Property readConsistency : String Index 8 Read FreadConsistency Write SetreadConsistency;
  end;
  TReadOptionsClass = Class of TReadOptions;
  
  { --------------------------------------------------------------------
    TPropertyOrder
    --------------------------------------------------------------------}
  
  TPropertyOrder = Class(TGoogleBaseObject)
  Private
    Fdirection : String;
    F_property : TPropertyReference;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setdirection(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_property(AIndex : Integer; const AValue : TPropertyReference); virtual;
  Public
  Published
    Property direction : String Index 0 Read Fdirection Write Setdirection;
    Property _property : TPropertyReference Index 8 Read F_property Write Set_property;
  end;
  TPropertyOrderClass = Class of TPropertyOrder;
  
  { --------------------------------------------------------------------
    TCommitRequest
    --------------------------------------------------------------------}
  
  TCommitRequest = Class(TGoogleBaseObject)
  Private
    Ftransaction : String;
    Fmutations : TCommitRequestTypemutationsArray;
    Fmode : String;
  Protected
    //Property setters
    Procedure Settransaction(AIndex : Integer; const AValue : String); virtual;
    Procedure Setmutations(AIndex : Integer; const AValue : TCommitRequestTypemutationsArray); virtual;
    Procedure Setmode(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property transaction : String Index 0 Read Ftransaction Write Settransaction;
    Property mutations : TCommitRequestTypemutationsArray Index 8 Read Fmutations Write Setmutations;
    Property mode : String Index 16 Read Fmode Write Setmode;
  end;
  TCommitRequestClass = Class of TCommitRequest;
  
  { --------------------------------------------------------------------
    TRollbackRequest
    --------------------------------------------------------------------}
  
  TRollbackRequest = Class(TGoogleBaseObject)
  Private
    Ftransaction : String;
  Protected
    //Property setters
    Procedure Settransaction(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property transaction : String Index 0 Read Ftransaction Write Settransaction;
  end;
  TRollbackRequestClass = Class of TRollbackRequest;
  
  { --------------------------------------------------------------------
    TQuery
    --------------------------------------------------------------------}
  
  TQuery = Class(TGoogleBaseObject)
  Private
    Flimit : integer;
    Ffilter : TFilter;
    FendCursor : String;
    FdistinctOn : TQueryTypedistinctOnArray;
    Foffset : integer;
    Fprojection : TQueryTypeprojectionArray;
    FstartCursor : String;
    Forder : TQueryTypeorderArray;
    Fkind : TQueryTypekindArray;
  Protected
    //Property setters
    Procedure Setlimit(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setfilter(AIndex : Integer; const AValue : TFilter); virtual;
    Procedure SetendCursor(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdistinctOn(AIndex : Integer; const AValue : TQueryTypedistinctOnArray); virtual;
    Procedure Setoffset(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setprojection(AIndex : Integer; const AValue : TQueryTypeprojectionArray); virtual;
    Procedure SetstartCursor(AIndex : Integer; const AValue : String); virtual;
    Procedure Setorder(AIndex : Integer; const AValue : TQueryTypeorderArray); virtual;
    Procedure Setkind(AIndex : Integer; const AValue : TQueryTypekindArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property limit : integer Index 0 Read Flimit Write Setlimit;
    Property filter : TFilter Index 8 Read Ffilter Write Setfilter;
    Property endCursor : String Index 16 Read FendCursor Write SetendCursor;
    Property distinctOn : TQueryTypedistinctOnArray Index 24 Read FdistinctOn Write SetdistinctOn;
    Property offset : integer Index 32 Read Foffset Write Setoffset;
    Property projection : TQueryTypeprojectionArray Index 40 Read Fprojection Write Setprojection;
    Property startCursor : String Index 48 Read FstartCursor Write SetstartCursor;
    Property order : TQueryTypeorderArray Index 56 Read Forder Write Setorder;
    Property kind : TQueryTypekindArray Index 64 Read Fkind Write Setkind;
  end;
  TQueryClass = Class of TQuery;
  
  { --------------------------------------------------------------------
    TEntityResult
    --------------------------------------------------------------------}
  
  TEntityResult = Class(TGoogleBaseObject)
  Private
    Fcursor : String;
    Fentity : TEntity;
  Protected
    //Property setters
    Procedure Setcursor(AIndex : Integer; const AValue : String); virtual;
    Procedure Setentity(AIndex : Integer; const AValue : TEntity); virtual;
  Public
  Published
    Property cursor : String Index 0 Read Fcursor Write Setcursor;
    Property entity : TEntity Index 8 Read Fentity Write Setentity;
  end;
  TEntityResultClass = Class of TEntityResult;
  
  { --------------------------------------------------------------------
    TGqlQueryParameter
    --------------------------------------------------------------------}
  
  TGqlQueryParameter = Class(TGoogleBaseObject)
  Private
    Fcursor : String;
    Fvalue : TValue;
  Protected
    //Property setters
    Procedure Setcursor(AIndex : Integer; const AValue : String); virtual;
    Procedure Setvalue(AIndex : Integer; const AValue : TValue); virtual;
  Public
  Published
    Property cursor : String Index 0 Read Fcursor Write Setcursor;
    Property value : TValue Index 8 Read Fvalue Write Setvalue;
  end;
  TGqlQueryParameterClass = Class of TGqlQueryParameter;
  
  { --------------------------------------------------------------------
    TBeginTransactionResponse
    --------------------------------------------------------------------}
  
  TBeginTransactionResponse = Class(TGoogleBaseObject)
  Private
    Ftransaction : String;
  Protected
    //Property setters
    Procedure Settransaction(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property transaction : String Index 0 Read Ftransaction Write Settransaction;
  end;
  TBeginTransactionResponseClass = Class of TBeginTransactionResponse;
  
  { --------------------------------------------------------------------
    TFilter
    --------------------------------------------------------------------}
  
  TFilter = Class(TGoogleBaseObject)
  Private
    FpropertyFilter : TPropertyFilter;
    FcompositeFilter : TCompositeFilter;
  Protected
    //Property setters
    Procedure SetpropertyFilter(AIndex : Integer; const AValue : TPropertyFilter); virtual;
    Procedure SetcompositeFilter(AIndex : Integer; const AValue : TCompositeFilter); virtual;
  Public
  Published
    Property propertyFilter : TPropertyFilter Index 0 Read FpropertyFilter Write SetpropertyFilter;
    Property compositeFilter : TCompositeFilter Index 8 Read FcompositeFilter Write SetcompositeFilter;
  end;
  TFilterClass = Class of TFilter;
  
  { --------------------------------------------------------------------
    TArrayValue
    --------------------------------------------------------------------}
  
  TArrayValue = Class(TGoogleBaseObject)
  Private
    Fvalues : TArrayValueTypevaluesArray;
  Protected
    //Property setters
    Procedure Setvalues(AIndex : Integer; const AValue : TArrayValueTypevaluesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property values : TArrayValueTypevaluesArray Index 0 Read Fvalues Write Setvalues;
  end;
  TArrayValueClass = Class of TArrayValue;
  
  { --------------------------------------------------------------------
    TPartitionId
    --------------------------------------------------------------------}
  
  TPartitionId = Class(TGoogleBaseObject)
  Private
    FnamespaceId : String;
    FprojectId : String;
  Protected
    //Property setters
    Procedure SetnamespaceId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetprojectId(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property namespaceId : String Index 0 Read FnamespaceId Write SetnamespaceId;
    Property projectId : String Index 8 Read FprojectId Write SetprojectId;
  end;
  TPartitionIdClass = Class of TPartitionId;
  
  { --------------------------------------------------------------------
    TQueryResultBatch
    --------------------------------------------------------------------}
  
  TQueryResultBatch = Class(TGoogleBaseObject)
  Private
    FendCursor : String;
    FskippedCursor : String;
    FentityResultType : String;
    FmoreResults : String;
    FentityResults : TQueryResultBatchTypeentityResultsArray;
    FskippedResults : integer;
  Protected
    //Property setters
    Procedure SetendCursor(AIndex : Integer; const AValue : String); virtual;
    Procedure SetskippedCursor(AIndex : Integer; const AValue : String); virtual;
    Procedure SetentityResultType(AIndex : Integer; const AValue : String); virtual;
    Procedure SetmoreResults(AIndex : Integer; const AValue : String); virtual;
    Procedure SetentityResults(AIndex : Integer; const AValue : TQueryResultBatchTypeentityResultsArray); virtual;
    Procedure SetskippedResults(AIndex : Integer; const AValue : integer); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property endCursor : String Index 0 Read FendCursor Write SetendCursor;
    Property skippedCursor : String Index 8 Read FskippedCursor Write SetskippedCursor;
    Property entityResultType : String Index 16 Read FentityResultType Write SetentityResultType;
    Property moreResults : String Index 24 Read FmoreResults Write SetmoreResults;
    Property entityResults : TQueryResultBatchTypeentityResultsArray Index 32 Read FentityResults Write SetentityResults;
    Property skippedResults : integer Index 40 Read FskippedResults Write SetskippedResults;
  end;
  TQueryResultBatchClass = Class of TQueryResultBatch;
  
  { --------------------------------------------------------------------
    TAllocateIdsRequest
    --------------------------------------------------------------------}
  
  TAllocateIdsRequest = Class(TGoogleBaseObject)
  Private
    Fkeys : TAllocateIdsRequestTypekeysArray;
  Protected
    //Property setters
    Procedure Setkeys(AIndex : Integer; const AValue : TAllocateIdsRequestTypekeysArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property keys : TAllocateIdsRequestTypekeysArray Index 0 Read Fkeys Write Setkeys;
  end;
  TAllocateIdsRequestClass = Class of TAllocateIdsRequest;
  
  { --------------------------------------------------------------------
    TPropertyFilter
    --------------------------------------------------------------------}
  
  TPropertyFilter = Class(TGoogleBaseObject)
  Private
    Fvalue : TValue;
    Fop : String;
    F_property : TPropertyReference;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setvalue(AIndex : Integer; const AValue : TValue); virtual;
    Procedure Setop(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_property(AIndex : Integer; const AValue : TPropertyReference); virtual;
  Public
  Published
    Property value : TValue Index 0 Read Fvalue Write Setvalue;
    Property op : String Index 8 Read Fop Write Setop;
    Property _property : TPropertyReference Index 16 Read F_property Write Set_property;
  end;
  TPropertyFilterClass = Class of TPropertyFilter;
  
  { --------------------------------------------------------------------
    TKindExpression
    --------------------------------------------------------------------}
  
  TKindExpression = Class(TGoogleBaseObject)
  Private
    Fname : String;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property name : String Index 0 Read Fname Write Setname;
  end;
  TKindExpressionClass = Class of TKindExpression;
  
  { --------------------------------------------------------------------
    TPathElement
    --------------------------------------------------------------------}
  
  TPathElement = Class(TGoogleBaseObject)
  Private
    Fkind : String;
    Fname : String;
    Fid : String;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
    Property name : String Index 8 Read Fname Write Setname;
    Property id : String Index 16 Read Fid Write Setid;
  end;
  TPathElementClass = Class of TPathElement;
  
  { --------------------------------------------------------------------
    TRollbackResponse
    --------------------------------------------------------------------}
  
  TRollbackResponse = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TRollbackResponseClass = Class of TRollbackResponse;
  
  { --------------------------------------------------------------------
    TPropertyReference
    --------------------------------------------------------------------}
  
  TPropertyReference = Class(TGoogleBaseObject)
  Private
    Fname : String;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property name : String Index 0 Read Fname Write Setname;
  end;
  TPropertyReferenceClass = Class of TPropertyReference;
  
  { --------------------------------------------------------------------
    TProjection
    --------------------------------------------------------------------}
  
  TProjection = Class(TGoogleBaseObject)
  Private
    F_property : TPropertyReference;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_property(AIndex : Integer; const AValue : TPropertyReference); virtual;
  Public
  Published
    Property _property : TPropertyReference Index 0 Read F_property Write Set_property;
  end;
  TProjectionClass = Class of TProjection;
  
  { --------------------------------------------------------------------
    TAllocateIdsResponse
    --------------------------------------------------------------------}
  
  TAllocateIdsResponse = Class(TGoogleBaseObject)
  Private
    Fkeys : TAllocateIdsResponseTypekeysArray;
  Protected
    //Property setters
    Procedure Setkeys(AIndex : Integer; const AValue : TAllocateIdsResponseTypekeysArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property keys : TAllocateIdsResponseTypekeysArray Index 0 Read Fkeys Write Setkeys;
  end;
  TAllocateIdsResponseClass = Class of TAllocateIdsResponse;
  
  { --------------------------------------------------------------------
    TMutationResult
    --------------------------------------------------------------------}
  
  TMutationResult = Class(TGoogleBaseObject)
  Private
    Fkey : TKey;
  Protected
    //Property setters
    Procedure Setkey(AIndex : Integer; const AValue : TKey); virtual;
  Public
  Published
    Property key : TKey Index 0 Read Fkey Write Setkey;
  end;
  TMutationResultClass = Class of TMutationResult;
  
  { --------------------------------------------------------------------
    TLookupResponse
    --------------------------------------------------------------------}
  
  TLookupResponse = Class(TGoogleBaseObject)
  Private
    Ffound : TLookupResponseTypefoundArray;
    Fdeferred : TLookupResponseTypedeferredArray;
    Fmissing : TLookupResponseTypemissingArray;
  Protected
    //Property setters
    Procedure Setfound(AIndex : Integer; const AValue : TLookupResponseTypefoundArray); virtual;
    Procedure Setdeferred(AIndex : Integer; const AValue : TLookupResponseTypedeferredArray); virtual;
    Procedure Setmissing(AIndex : Integer; const AValue : TLookupResponseTypemissingArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property found : TLookupResponseTypefoundArray Index 0 Read Ffound Write Setfound;
    Property deferred : TLookupResponseTypedeferredArray Index 8 Read Fdeferred Write Setdeferred;
    Property missing : TLookupResponseTypemissingArray Index 16 Read Fmissing Write Setmissing;
  end;
  TLookupResponseClass = Class of TLookupResponse;
  
  { --------------------------------------------------------------------
    TBeginTransactionRequest
    --------------------------------------------------------------------}
  
  TBeginTransactionRequest = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TBeginTransactionRequestClass = Class of TBeginTransactionRequest;
  
  { --------------------------------------------------------------------
    TKey
    --------------------------------------------------------------------}
  
  TKey = Class(TGoogleBaseObject)
  Private
    FpartitionId : TPartitionId;
    Fpath : TKeyTypepathArray;
  Protected
    //Property setters
    Procedure SetpartitionId(AIndex : Integer; const AValue : TPartitionId); virtual;
    Procedure Setpath(AIndex : Integer; const AValue : TKeyTypepathArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property partitionId : TPartitionId Index 0 Read FpartitionId Write SetpartitionId;
    Property path : TKeyTypepathArray Index 8 Read Fpath Write Setpath;
  end;
  TKeyClass = Class of TKey;
  
  { --------------------------------------------------------------------
    TRunQueryResponse
    --------------------------------------------------------------------}
  
  TRunQueryResponse = Class(TGoogleBaseObject)
  Private
    Fbatch : TQueryResultBatch;
    Fquery : TQuery;
  Protected
    //Property setters
    Procedure Setbatch(AIndex : Integer; const AValue : TQueryResultBatch); virtual;
    Procedure Setquery(AIndex : Integer; const AValue : TQuery); virtual;
  Public
  Published
    Property batch : TQueryResultBatch Index 0 Read Fbatch Write Setbatch;
    Property query : TQuery Index 8 Read Fquery Write Setquery;
  end;
  TRunQueryResponseClass = Class of TRunQueryResponse;
  
  { --------------------------------------------------------------------
    TEntityTypeproperties
    --------------------------------------------------------------------}
  
  TEntityTypeproperties = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TEntityTypepropertiesClass = Class of TEntityTypeproperties;
  
  { --------------------------------------------------------------------
    TEntity
    --------------------------------------------------------------------}
  
  TEntity = Class(TGoogleBaseObject)
  Private
    Fproperties : TEntityTypeproperties;
    Fkey : TKey;
  Protected
    //Property setters
    Procedure Setproperties(AIndex : Integer; const AValue : TEntityTypeproperties); virtual;
    Procedure Setkey(AIndex : Integer; const AValue : TKey); virtual;
  Public
  Published
    Property properties : TEntityTypeproperties Index 0 Read Fproperties Write Setproperties;
    Property key : TKey Index 8 Read Fkey Write Setkey;
  end;
  TEntityClass = Class of TEntity;
  
  { --------------------------------------------------------------------
    TGqlQueryTypenamedBindings
    --------------------------------------------------------------------}
  
  TGqlQueryTypenamedBindings = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TGqlQueryTypenamedBindingsClass = Class of TGqlQueryTypenamedBindings;
  
  { --------------------------------------------------------------------
    TGqlQuery
    --------------------------------------------------------------------}
  
  TGqlQuery = Class(TGoogleBaseObject)
  Private
    FnamedBindings : TGqlQueryTypenamedBindings;
    FqueryString : String;
    FallowLiterals : boolean;
    FpositionalBindings : TGqlQueryTypepositionalBindingsArray;
  Protected
    //Property setters
    Procedure SetnamedBindings(AIndex : Integer; const AValue : TGqlQueryTypenamedBindings); virtual;
    Procedure SetqueryString(AIndex : Integer; const AValue : String); virtual;
    Procedure SetallowLiterals(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetpositionalBindings(AIndex : Integer; const AValue : TGqlQueryTypepositionalBindingsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property namedBindings : TGqlQueryTypenamedBindings Index 0 Read FnamedBindings Write SetnamedBindings;
    Property queryString : String Index 8 Read FqueryString Write SetqueryString;
    Property allowLiterals : boolean Index 16 Read FallowLiterals Write SetallowLiterals;
    Property positionalBindings : TGqlQueryTypepositionalBindingsArray Index 24 Read FpositionalBindings Write SetpositionalBindings;
  end;
  TGqlQueryClass = Class of TGqlQuery;
  
  { --------------------------------------------------------------------
    TCommitResponse
    --------------------------------------------------------------------}
  
  TCommitResponse = Class(TGoogleBaseObject)
  Private
    FmutationResults : TCommitResponseTypemutationResultsArray;
    FindexUpdates : integer;
  Protected
    //Property setters
    Procedure SetmutationResults(AIndex : Integer; const AValue : TCommitResponseTypemutationResultsArray); virtual;
    Procedure SetindexUpdates(AIndex : Integer; const AValue : integer); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property mutationResults : TCommitResponseTypemutationResultsArray Index 0 Read FmutationResults Write SetmutationResults;
    Property indexUpdates : integer Index 8 Read FindexUpdates Write SetindexUpdates;
  end;
  TCommitResponseClass = Class of TCommitResponse;
  
  { --------------------------------------------------------------------
    TMutation
    --------------------------------------------------------------------}
  
  TMutation = Class(TGoogleBaseObject)
  Private
    Fupdate : TEntity;
    Finsert : TEntity;
    Fdelete : TKey;
    Fupsert : TEntity;
  Protected
    //Property setters
    Procedure Setupdate(AIndex : Integer; const AValue : TEntity); virtual;
    Procedure Setinsert(AIndex : Integer; const AValue : TEntity); virtual;
    Procedure Setdelete(AIndex : Integer; const AValue : TKey); virtual;
    Procedure Setupsert(AIndex : Integer; const AValue : TEntity); virtual;
  Public
  Published
    Property update : TEntity Index 0 Read Fupdate Write Setupdate;
    Property insert : TEntity Index 8 Read Finsert Write Setinsert;
    Property delete : TKey Index 16 Read Fdelete Write Setdelete;
    Property upsert : TEntity Index 24 Read Fupsert Write Setupsert;
  end;
  TMutationClass = Class of TMutation;
  
  { --------------------------------------------------------------------
    TRunQueryRequest
    --------------------------------------------------------------------}
  
  TRunQueryRequest = Class(TGoogleBaseObject)
  Private
    FpartitionId : TPartitionId;
    FgqlQuery : TGqlQuery;
    Fquery : TQuery;
    FreadOptions : TReadOptions;
  Protected
    //Property setters
    Procedure SetpartitionId(AIndex : Integer; const AValue : TPartitionId); virtual;
    Procedure SetgqlQuery(AIndex : Integer; const AValue : TGqlQuery); virtual;
    Procedure Setquery(AIndex : Integer; const AValue : TQuery); virtual;
    Procedure SetreadOptions(AIndex : Integer; const AValue : TReadOptions); virtual;
  Public
  Published
    Property partitionId : TPartitionId Index 0 Read FpartitionId Write SetpartitionId;
    Property gqlQuery : TGqlQuery Index 8 Read FgqlQuery Write SetgqlQuery;
    Property query : TQuery Index 16 Read Fquery Write Setquery;
    Property readOptions : TReadOptions Index 24 Read FreadOptions Write SetreadOptions;
  end;
  TRunQueryRequestClass = Class of TRunQueryRequest;
  
  { --------------------------------------------------------------------
    TCompositeFilter
    --------------------------------------------------------------------}
  
  TCompositeFilter = Class(TGoogleBaseObject)
  Private
    Fop : String;
    Ffilters : TCompositeFilterTypefiltersArray;
  Protected
    //Property setters
    Procedure Setop(AIndex : Integer; const AValue : String); virtual;
    Procedure Setfilters(AIndex : Integer; const AValue : TCompositeFilterTypefiltersArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property op : String Index 0 Read Fop Write Setop;
    Property filters : TCompositeFilterTypefiltersArray Index 8 Read Ffilters Write Setfilters;
  end;
  TCompositeFilterClass = Class of TCompositeFilter;
  
  { --------------------------------------------------------------------
    TLatLng
    --------------------------------------------------------------------}
  
  TLatLng = Class(TGoogleBaseObject)
  Private
    Flongitude : double;
    Flatitude : double;
  Protected
    //Property setters
    Procedure Setlongitude(AIndex : Integer; const AValue : double); virtual;
    Procedure Setlatitude(AIndex : Integer; const AValue : double); virtual;
  Public
  Published
    Property longitude : double Index 0 Read Flongitude Write Setlongitude;
    Property latitude : double Index 8 Read Flatitude Write Setlatitude;
  end;
  TLatLngClass = Class of TLatLng;
  
  { --------------------------------------------------------------------
    TLookupRequest
    --------------------------------------------------------------------}
  
  TLookupRequest = Class(TGoogleBaseObject)
  Private
    Fkeys : TLookupRequestTypekeysArray;
    FreadOptions : TReadOptions;
  Protected
    //Property setters
    Procedure Setkeys(AIndex : Integer; const AValue : TLookupRequestTypekeysArray); virtual;
    Procedure SetreadOptions(AIndex : Integer; const AValue : TReadOptions); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property keys : TLookupRequestTypekeysArray Index 0 Read Fkeys Write Setkeys;
    Property readOptions : TReadOptions Index 8 Read FreadOptions Write SetreadOptions;
  end;
  TLookupRequestClass = Class of TLookupRequest;
  
  { --------------------------------------------------------------------
    TProjectsResource
    --------------------------------------------------------------------}
  
  TProjectsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function RunQuery(projectId: string; aRunQueryRequest : TRunQueryRequest) : TRunQueryResponse;
    Function BeginTransaction(projectId: string; aBeginTransactionRequest : TBeginTransactionRequest) : TBeginTransactionResponse;
    Function AllocateIds(projectId: string; aAllocateIdsRequest : TAllocateIdsRequest) : TAllocateIdsResponse;
    Function Lookup(projectId: string; aLookupRequest : TLookupRequest) : TLookupResponse;
    Function Commit(projectId: string; aCommitRequest : TCommitRequest) : TCommitResponse;
    Function Rollback(projectId: string; aRollbackRequest : TRollbackRequest) : TRollbackResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TDatastoreAPI
    --------------------------------------------------------------------}
  
  TDatastoreAPI = Class(TGoogleAPI)
  Private
    FProjectsInstance : TProjectsResource;
    Function GetProjectsInstance : TProjectsResource;virtual;
  Public
    //Override class functions with API info
    Class Function APIName : String; override;
    Class Function APIVersion : String; override;
    Class Function APIRevision : String; override;
    Class Function APIID : String; override;
    Class Function APITitle : String; override;
    Class Function APIDescription : String; override;
    Class Function APIOwnerDomain : String; override;
    Class Function APIOwnerName : String; override;
    Class Function APIIcon16 : String; override;
    Class Function APIIcon32 : String; override;
    Class Function APIdocumentationLink : String; override;
    Class Function APIrootUrl : string; override;
    Class Function APIbasePath : string;override;
    Class Function APIbaseURL : String;override;
    Class Function APIProtocol : string;override;
    Class Function APIservicePath : string;override;
    Class Function APIbatchPath : String;override;
    Class Function APIAuthScopes : TScopeInfoArray;override;
    Class Function APINeedsAuth : Boolean;override;
    Class Procedure RegisterAPIResources; override;
    //Add create function for resources
    Function CreateProjectsResource(AOwner : TComponent) : TProjectsResource;virtual;overload;
    Function CreateProjectsResource : TProjectsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property ProjectsResource : TProjectsResource Read GetProjectsInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TValue
  --------------------------------------------------------------------}


Procedure TValue.SetstringValue(AIndex : Integer; const AValue : String); 

begin
  If (FstringValue=AValue) then exit;
  FstringValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TValue.SetarrayValue(AIndex : Integer; const AValue : TArrayValue); 

begin
  If (FarrayValue=AValue) then exit;
  FarrayValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TValue.SetentityValue(AIndex : Integer; const AValue : TEntity); 

begin
  If (FentityValue=AValue) then exit;
  FentityValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TValue.Setmeaning(AIndex : Integer; const AValue : integer); 

begin
  If (Fmeaning=AValue) then exit;
  Fmeaning:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TValue.SetintegerValue(AIndex : Integer; const AValue : String); 

begin
  If (FintegerValue=AValue) then exit;
  FintegerValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TValue.SetdoubleValue(AIndex : Integer; const AValue : double); 

begin
  If (FdoubleValue=AValue) then exit;
  FdoubleValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TValue.SetgeoPointValue(AIndex : Integer; const AValue : TLatLng); 

begin
  If (FgeoPointValue=AValue) then exit;
  FgeoPointValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TValue.SetblobValue(AIndex : Integer; const AValue : String); 

begin
  If (FblobValue=AValue) then exit;
  FblobValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TValue.SetnullValue(AIndex : Integer; const AValue : String); 

begin
  If (FnullValue=AValue) then exit;
  FnullValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TValue.SetkeyValue(AIndex : Integer; const AValue : TKey); 

begin
  If (FkeyValue=AValue) then exit;
  FkeyValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TValue.SetbooleanValue(AIndex : Integer; const AValue : boolean); 

begin
  If (FbooleanValue=AValue) then exit;
  FbooleanValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TValue.SetexcludeFromIndexes(AIndex : Integer; const AValue : boolean); 

begin
  If (FexcludeFromIndexes=AValue) then exit;
  FexcludeFromIndexes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TValue.SettimestampValue(AIndex : Integer; const AValue : String); 

begin
  If (FtimestampValue=AValue) then exit;
  FtimestampValue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReadOptions
  --------------------------------------------------------------------}


Procedure TReadOptions.Settransaction(AIndex : Integer; const AValue : String); 

begin
  If (Ftransaction=AValue) then exit;
  Ftransaction:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadOptions.SetreadConsistency(AIndex : Integer; const AValue : String); 

begin
  If (FreadConsistency=AValue) then exit;
  FreadConsistency:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPropertyOrder
  --------------------------------------------------------------------}


Procedure TPropertyOrder.Setdirection(AIndex : Integer; const AValue : String); 

begin
  If (Fdirection=AValue) then exit;
  Fdirection:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPropertyOrder.Set_property(AIndex : Integer; const AValue : TPropertyReference); 

begin
  If (F_property=AValue) then exit;
  F_property:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TPropertyOrder.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_property' : Result:='property';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TCommitRequest
  --------------------------------------------------------------------}


Procedure TCommitRequest.Settransaction(AIndex : Integer; const AValue : String); 

begin
  If (Ftransaction=AValue) then exit;
  Ftransaction:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommitRequest.Setmutations(AIndex : Integer; const AValue : TCommitRequestTypemutationsArray); 

begin
  If (Fmutations=AValue) then exit;
  Fmutations:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommitRequest.Setmode(AIndex : Integer; const AValue : String); 

begin
  If (Fmode=AValue) then exit;
  Fmode:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TCommitRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'mutations' : SetLength(Fmutations,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TRollbackRequest
  --------------------------------------------------------------------}


Procedure TRollbackRequest.Settransaction(AIndex : Integer; const AValue : String); 

begin
  If (Ftransaction=AValue) then exit;
  Ftransaction:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TQuery
  --------------------------------------------------------------------}


Procedure TQuery.Setlimit(AIndex : Integer; const AValue : integer); 

begin
  If (Flimit=AValue) then exit;
  Flimit:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.Setfilter(AIndex : Integer; const AValue : TFilter); 

begin
  If (Ffilter=AValue) then exit;
  Ffilter:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.SetendCursor(AIndex : Integer; const AValue : String); 

begin
  If (FendCursor=AValue) then exit;
  FendCursor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.SetdistinctOn(AIndex : Integer; const AValue : TQueryTypedistinctOnArray); 

begin
  If (FdistinctOn=AValue) then exit;
  FdistinctOn:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.Setoffset(AIndex : Integer; const AValue : integer); 

begin
  If (Foffset=AValue) then exit;
  Foffset:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.Setprojection(AIndex : Integer; const AValue : TQueryTypeprojectionArray); 

begin
  If (Fprojection=AValue) then exit;
  Fprojection:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.SetstartCursor(AIndex : Integer; const AValue : String); 

begin
  If (FstartCursor=AValue) then exit;
  FstartCursor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.Setorder(AIndex : Integer; const AValue : TQueryTypeorderArray); 

begin
  If (Forder=AValue) then exit;
  Forder:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.Setkind(AIndex : Integer; const AValue : TQueryTypekindArray); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TQuery.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'distincton' : SetLength(FdistinctOn,ALength);
  'projection' : SetLength(Fprojection,ALength);
  'order' : SetLength(Forder,ALength);
  'kind' : SetLength(Fkind,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TEntityResult
  --------------------------------------------------------------------}


Procedure TEntityResult.Setcursor(AIndex : Integer; const AValue : String); 

begin
  If (Fcursor=AValue) then exit;
  Fcursor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntityResult.Setentity(AIndex : Integer; const AValue : TEntity); 

begin
  If (Fentity=AValue) then exit;
  Fentity:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGqlQueryParameter
  --------------------------------------------------------------------}


Procedure TGqlQueryParameter.Setcursor(AIndex : Integer; const AValue : String); 

begin
  If (Fcursor=AValue) then exit;
  Fcursor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGqlQueryParameter.Setvalue(AIndex : Integer; const AValue : TValue); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBeginTransactionResponse
  --------------------------------------------------------------------}


Procedure TBeginTransactionResponse.Settransaction(AIndex : Integer; const AValue : String); 

begin
  If (Ftransaction=AValue) then exit;
  Ftransaction:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFilter
  --------------------------------------------------------------------}


Procedure TFilter.SetpropertyFilter(AIndex : Integer; const AValue : TPropertyFilter); 

begin
  If (FpropertyFilter=AValue) then exit;
  FpropertyFilter:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilter.SetcompositeFilter(AIndex : Integer; const AValue : TCompositeFilter); 

begin
  If (FcompositeFilter=AValue) then exit;
  FcompositeFilter:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TArrayValue
  --------------------------------------------------------------------}


Procedure TArrayValue.Setvalues(AIndex : Integer; const AValue : TArrayValueTypevaluesArray); 

begin
  If (Fvalues=AValue) then exit;
  Fvalues:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TArrayValue.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'values' : SetLength(Fvalues,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TPartitionId
  --------------------------------------------------------------------}


Procedure TPartitionId.SetnamespaceId(AIndex : Integer; const AValue : String); 

begin
  If (FnamespaceId=AValue) then exit;
  FnamespaceId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPartitionId.SetprojectId(AIndex : Integer; const AValue : String); 

begin
  If (FprojectId=AValue) then exit;
  FprojectId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TQueryResultBatch
  --------------------------------------------------------------------}


Procedure TQueryResultBatch.SetendCursor(AIndex : Integer; const AValue : String); 

begin
  If (FendCursor=AValue) then exit;
  FendCursor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQueryResultBatch.SetskippedCursor(AIndex : Integer; const AValue : String); 

begin
  If (FskippedCursor=AValue) then exit;
  FskippedCursor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQueryResultBatch.SetentityResultType(AIndex : Integer; const AValue : String); 

begin
  If (FentityResultType=AValue) then exit;
  FentityResultType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQueryResultBatch.SetmoreResults(AIndex : Integer; const AValue : String); 

begin
  If (FmoreResults=AValue) then exit;
  FmoreResults:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQueryResultBatch.SetentityResults(AIndex : Integer; const AValue : TQueryResultBatchTypeentityResultsArray); 

begin
  If (FentityResults=AValue) then exit;
  FentityResults:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQueryResultBatch.SetskippedResults(AIndex : Integer; const AValue : integer); 

begin
  If (FskippedResults=AValue) then exit;
  FskippedResults:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TQueryResultBatch.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'entityresults' : SetLength(FentityResults,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TAllocateIdsRequest
  --------------------------------------------------------------------}


Procedure TAllocateIdsRequest.Setkeys(AIndex : Integer; const AValue : TAllocateIdsRequestTypekeysArray); 

begin
  If (Fkeys=AValue) then exit;
  Fkeys:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAllocateIdsRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'keys' : SetLength(Fkeys,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TPropertyFilter
  --------------------------------------------------------------------}


Procedure TPropertyFilter.Setvalue(AIndex : Integer; const AValue : TValue); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPropertyFilter.Setop(AIndex : Integer; const AValue : String); 

begin
  If (Fop=AValue) then exit;
  Fop:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPropertyFilter.Set_property(AIndex : Integer; const AValue : TPropertyReference); 

begin
  If (F_property=AValue) then exit;
  F_property:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TPropertyFilter.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_property' : Result:='property';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TKindExpression
  --------------------------------------------------------------------}


Procedure TKindExpression.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPathElement
  --------------------------------------------------------------------}


Procedure TPathElement.Setkind(AIndex : Integer; const AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPathElement.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPathElement.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRollbackResponse
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TPropertyReference
  --------------------------------------------------------------------}


Procedure TPropertyReference.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProjection
  --------------------------------------------------------------------}


Procedure TProjection.Set_property(AIndex : Integer; const AValue : TPropertyReference); 

begin
  If (F_property=AValue) then exit;
  F_property:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TProjection.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_property' : Result:='property';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TAllocateIdsResponse
  --------------------------------------------------------------------}


Procedure TAllocateIdsResponse.Setkeys(AIndex : Integer; const AValue : TAllocateIdsResponseTypekeysArray); 

begin
  If (Fkeys=AValue) then exit;
  Fkeys:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAllocateIdsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'keys' : SetLength(Fkeys,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TMutationResult
  --------------------------------------------------------------------}


Procedure TMutationResult.Setkey(AIndex : Integer; const AValue : TKey); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLookupResponse
  --------------------------------------------------------------------}


Procedure TLookupResponse.Setfound(AIndex : Integer; const AValue : TLookupResponseTypefoundArray); 

begin
  If (Ffound=AValue) then exit;
  Ffound:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLookupResponse.Setdeferred(AIndex : Integer; const AValue : TLookupResponseTypedeferredArray); 

begin
  If (Fdeferred=AValue) then exit;
  Fdeferred:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLookupResponse.Setmissing(AIndex : Integer; const AValue : TLookupResponseTypemissingArray); 

begin
  If (Fmissing=AValue) then exit;
  Fmissing:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TLookupResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'found' : SetLength(Ffound,ALength);
  'deferred' : SetLength(Fdeferred,ALength);
  'missing' : SetLength(Fmissing,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TBeginTransactionRequest
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TKey
  --------------------------------------------------------------------}


Procedure TKey.SetpartitionId(AIndex : Integer; const AValue : TPartitionId); 

begin
  If (FpartitionId=AValue) then exit;
  FpartitionId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TKey.Setpath(AIndex : Integer; const AValue : TKeyTypepathArray); 

begin
  If (Fpath=AValue) then exit;
  Fpath:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TKey.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'path' : SetLength(Fpath,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TRunQueryResponse
  --------------------------------------------------------------------}


Procedure TRunQueryResponse.Setbatch(AIndex : Integer; const AValue : TQueryResultBatch); 

begin
  If (Fbatch=AValue) then exit;
  Fbatch:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRunQueryResponse.Setquery(AIndex : Integer; const AValue : TQuery); 

begin
  If (Fquery=AValue) then exit;
  Fquery:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TEntityTypeproperties
  --------------------------------------------------------------------}


Class Function TEntityTypeproperties.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TEntity
  --------------------------------------------------------------------}


Procedure TEntity.Setproperties(AIndex : Integer; const AValue : TEntityTypeproperties); 

begin
  If (Fproperties=AValue) then exit;
  Fproperties:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntity.Setkey(AIndex : Integer; const AValue : TKey); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGqlQueryTypenamedBindings
  --------------------------------------------------------------------}


Class Function TGqlQueryTypenamedBindings.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TGqlQuery
  --------------------------------------------------------------------}


Procedure TGqlQuery.SetnamedBindings(AIndex : Integer; const AValue : TGqlQueryTypenamedBindings); 

begin
  If (FnamedBindings=AValue) then exit;
  FnamedBindings:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGqlQuery.SetqueryString(AIndex : Integer; const AValue : String); 

begin
  If (FqueryString=AValue) then exit;
  FqueryString:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGqlQuery.SetallowLiterals(AIndex : Integer; const AValue : boolean); 

begin
  If (FallowLiterals=AValue) then exit;
  FallowLiterals:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGqlQuery.SetpositionalBindings(AIndex : Integer; const AValue : TGqlQueryTypepositionalBindingsArray); 

begin
  If (FpositionalBindings=AValue) then exit;
  FpositionalBindings:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TGqlQuery.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'positionalbindings' : SetLength(FpositionalBindings,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TCommitResponse
  --------------------------------------------------------------------}


Procedure TCommitResponse.SetmutationResults(AIndex : Integer; const AValue : TCommitResponseTypemutationResultsArray); 

begin
  If (FmutationResults=AValue) then exit;
  FmutationResults:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommitResponse.SetindexUpdates(AIndex : Integer; const AValue : integer); 

begin
  If (FindexUpdates=AValue) then exit;
  FindexUpdates:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TCommitResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'mutationresults' : SetLength(FmutationResults,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TMutation
  --------------------------------------------------------------------}


Procedure TMutation.Setupdate(AIndex : Integer; const AValue : TEntity); 

begin
  If (Fupdate=AValue) then exit;
  Fupdate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMutation.Setinsert(AIndex : Integer; const AValue : TEntity); 

begin
  If (Finsert=AValue) then exit;
  Finsert:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMutation.Setdelete(AIndex : Integer; const AValue : TKey); 

begin
  If (Fdelete=AValue) then exit;
  Fdelete:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMutation.Setupsert(AIndex : Integer; const AValue : TEntity); 

begin
  If (Fupsert=AValue) then exit;
  Fupsert:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRunQueryRequest
  --------------------------------------------------------------------}


Procedure TRunQueryRequest.SetpartitionId(AIndex : Integer; const AValue : TPartitionId); 

begin
  If (FpartitionId=AValue) then exit;
  FpartitionId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRunQueryRequest.SetgqlQuery(AIndex : Integer; const AValue : TGqlQuery); 

begin
  If (FgqlQuery=AValue) then exit;
  FgqlQuery:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRunQueryRequest.Setquery(AIndex : Integer; const AValue : TQuery); 

begin
  If (Fquery=AValue) then exit;
  Fquery:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRunQueryRequest.SetreadOptions(AIndex : Integer; const AValue : TReadOptions); 

begin
  If (FreadOptions=AValue) then exit;
  FreadOptions:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCompositeFilter
  --------------------------------------------------------------------}


Procedure TCompositeFilter.Setop(AIndex : Integer; const AValue : String); 

begin
  If (Fop=AValue) then exit;
  Fop:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCompositeFilter.Setfilters(AIndex : Integer; const AValue : TCompositeFilterTypefiltersArray); 

begin
  If (Ffilters=AValue) then exit;
  Ffilters:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TCompositeFilter.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'filters' : SetLength(Ffilters,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TLatLng
  --------------------------------------------------------------------}


Procedure TLatLng.Setlongitude(AIndex : Integer; const AValue : double); 

begin
  If (Flongitude=AValue) then exit;
  Flongitude:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLatLng.Setlatitude(AIndex : Integer; const AValue : double); 

begin
  If (Flatitude=AValue) then exit;
  Flatitude:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLookupRequest
  --------------------------------------------------------------------}


Procedure TLookupRequest.Setkeys(AIndex : Integer; const AValue : TLookupRequestTypekeysArray); 

begin
  If (Fkeys=AValue) then exit;
  Fkeys:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLookupRequest.SetreadOptions(AIndex : Integer; const AValue : TReadOptions); 

begin
  If (FreadOptions=AValue) then exit;
  FreadOptions:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TLookupRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'keys' : SetLength(Fkeys,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TProjectsResource
  --------------------------------------------------------------------}


Class Function TProjectsResource.ResourceName : String;

begin
  Result:='projects';
end;

Class Function TProjectsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdatastoreAPI;
end;

Function TProjectsResource.RunQuery(projectId: string; aRunQueryRequest : TRunQueryRequest) : TRunQueryResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1beta3/projects/{projectId}:runQuery';
  _Methodid   = 'datastore.projects.runQuery';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aRunQueryRequest,TRunQueryResponse) as TRunQueryResponse;
end;

Function TProjectsResource.BeginTransaction(projectId: string; aBeginTransactionRequest : TBeginTransactionRequest) : TBeginTransactionResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1beta3/projects/{projectId}:beginTransaction';
  _Methodid   = 'datastore.projects.beginTransaction';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aBeginTransactionRequest,TBeginTransactionResponse) as TBeginTransactionResponse;
end;

Function TProjectsResource.AllocateIds(projectId: string; aAllocateIdsRequest : TAllocateIdsRequest) : TAllocateIdsResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1beta3/projects/{projectId}:allocateIds';
  _Methodid   = 'datastore.projects.allocateIds';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aAllocateIdsRequest,TAllocateIdsResponse) as TAllocateIdsResponse;
end;

Function TProjectsResource.Lookup(projectId: string; aLookupRequest : TLookupRequest) : TLookupResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1beta3/projects/{projectId}:lookup';
  _Methodid   = 'datastore.projects.lookup';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aLookupRequest,TLookupResponse) as TLookupResponse;
end;

Function TProjectsResource.Commit(projectId: string; aCommitRequest : TCommitRequest) : TCommitResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1beta3/projects/{projectId}:commit';
  _Methodid   = 'datastore.projects.commit';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aCommitRequest,TCommitResponse) as TCommitResponse;
end;

Function TProjectsResource.Rollback(projectId: string; aRollbackRequest : TRollbackRequest) : TRollbackResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1beta3/projects/{projectId}:rollback';
  _Methodid   = 'datastore.projects.rollback';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['projectId',projectId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aRollbackRequest,TRollbackResponse) as TRollbackResponse;
end;



{ --------------------------------------------------------------------
  TDatastoreAPI
  --------------------------------------------------------------------}

Class Function TDatastoreAPI.APIName : String;

begin
  Result:='datastore';
end;

Class Function TDatastoreAPI.APIVersion : String;

begin
  Result:='v1beta3';
end;

Class Function TDatastoreAPI.APIRevision : String;

begin
  Result:='20160502';
end;

Class Function TDatastoreAPI.APIID : String;

begin
  Result:='datastore:v1beta3';
end;

Class Function TDatastoreAPI.APITitle : String;

begin
  Result:='Google Cloud Datastore API';
end;

Class Function TDatastoreAPI.APIDescription : String;

begin
  Result:='Accesses the schemaless NoSQL database to provide fully managed, robust, scalable storage for your application.';
end;

Class Function TDatastoreAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TDatastoreAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TDatastoreAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TDatastoreAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TDatastoreAPI.APIdocumentationLink : String;

begin
  Result:='https://cloud.google.com/datastore/';
end;

Class Function TDatastoreAPI.APIrootUrl : string;

begin
  Result:='https://datastore.googleapis.com/';
end;

Class Function TDatastoreAPI.APIbasePath : string;

begin
  Result:='';
end;

Class Function TDatastoreAPI.APIbaseURL : String;

begin
  Result:='https://datastore.googleapis.com/';
end;

Class Function TDatastoreAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TDatastoreAPI.APIservicePath : string;

begin
  Result:='';
end;

Class Function TDatastoreAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TDatastoreAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,2);
  Result[0].Name:='https://www.googleapis.com/auth/cloud-platform';
  Result[0].Description:='View and manage your data across Google Cloud Platform services';
  Result[1].Name:='https://www.googleapis.com/auth/datastore';
  Result[1].Description:='View and manage your Google Cloud Datastore data';
  
end;

Class Function TDatastoreAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TDatastoreAPI.RegisterAPIResources;

begin
  TValue.RegisterObject;
  TReadOptions.RegisterObject;
  TPropertyOrder.RegisterObject;
  TCommitRequest.RegisterObject;
  TRollbackRequest.RegisterObject;
  TQuery.RegisterObject;
  TEntityResult.RegisterObject;
  TGqlQueryParameter.RegisterObject;
  TBeginTransactionResponse.RegisterObject;
  TFilter.RegisterObject;
  TArrayValue.RegisterObject;
  TPartitionId.RegisterObject;
  TQueryResultBatch.RegisterObject;
  TAllocateIdsRequest.RegisterObject;
  TPropertyFilter.RegisterObject;
  TKindExpression.RegisterObject;
  TPathElement.RegisterObject;
  TRollbackResponse.RegisterObject;
  TPropertyReference.RegisterObject;
  TProjection.RegisterObject;
  TAllocateIdsResponse.RegisterObject;
  TMutationResult.RegisterObject;
  TLookupResponse.RegisterObject;
  TBeginTransactionRequest.RegisterObject;
  TKey.RegisterObject;
  TRunQueryResponse.RegisterObject;
  TEntityTypeproperties.RegisterObject;
  TEntity.RegisterObject;
  TGqlQueryTypenamedBindings.RegisterObject;
  TGqlQuery.RegisterObject;
  TCommitResponse.RegisterObject;
  TMutation.RegisterObject;
  TRunQueryRequest.RegisterObject;
  TCompositeFilter.RegisterObject;
  TLatLng.RegisterObject;
  TLookupRequest.RegisterObject;
end;


Function TDatastoreAPI.GetProjectsInstance : TProjectsResource;

begin
  if (FProjectsInstance=Nil) then
    FProjectsInstance:=CreateProjectsResource;
  Result:=FProjectsInstance;
end;

Function TDatastoreAPI.CreateProjectsResource : TProjectsResource;

begin
  Result:=CreateProjectsResource(Self);
end;


Function TDatastoreAPI.CreateProjectsResource(AOwner : TComponent) : TProjectsResource;

begin
  Result:=TProjectsResource.Create(AOwner);
  Result.API:=Self.API;
end;



initialization
  TDatastoreAPI.RegisterAPI;
end.
