unit googledatastore;
{
   **********************************************************************
      This file is part of the Free Component Library (FCL)
      Copyright (c) 2015 The free pascal team.
  
      See the file COPYING.FPC, included in this distribution,
      for details about the copyright.
  
      This program is distributed in the hope that it will be useful,
      but WITHOUT ANY WARRANTY; without even the implied warranty of
      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  
   **********************************************************************
}
//Generated on: 9-5-15 13:22:51
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TAllocateIdsRequest = class;
  TAllocateIdsResponse = class;
  TBeginTransactionRequest = class;
  TBeginTransactionResponse = class;
  TCommitRequest = class;
  TCommitResponse = class;
  TCompositeFilter = class;
  TEntity = class;
  TEntityResult = class;
  TFilter = class;
  TGqlQuery = class;
  TGqlQueryArg = class;
  TKey = class;
  TKeyPathElement = class;
  TKindExpression = class;
  TLookupRequest = class;
  TLookupResponse = class;
  TMutation = class;
  TMutationResult = class;
  TPartitionId = class;
  TProperty = class;
  TPropertyExpression = class;
  TPropertyFilter = class;
  TPropertyOrder = class;
  TPropertyReference = class;
  TQuery = class;
  TQueryResultBatch = class;
  TReadOptions = class;
  TResponseHeader = class;
  TRollbackRequest = class;
  TRollbackResponse = class;
  TRunQueryRequest = class;
  TRunQueryResponse = class;
  TValue = class;
  TAllocateIdsRequestArray = Array of TAllocateIdsRequest;
  TAllocateIdsResponseArray = Array of TAllocateIdsResponse;
  TBeginTransactionRequestArray = Array of TBeginTransactionRequest;
  TBeginTransactionResponseArray = Array of TBeginTransactionResponse;
  TCommitRequestArray = Array of TCommitRequest;
  TCommitResponseArray = Array of TCommitResponse;
  TCompositeFilterArray = Array of TCompositeFilter;
  TEntityArray = Array of TEntity;
  TEntityResultArray = Array of TEntityResult;
  TFilterArray = Array of TFilter;
  TGqlQueryArray = Array of TGqlQuery;
  TGqlQueryArgArray = Array of TGqlQueryArg;
  TKeyArray = Array of TKey;
  TKeyPathElementArray = Array of TKeyPathElement;
  TKindExpressionArray = Array of TKindExpression;
  TLookupRequestArray = Array of TLookupRequest;
  TLookupResponseArray = Array of TLookupResponse;
  TMutationArray = Array of TMutation;
  TMutationResultArray = Array of TMutationResult;
  TPartitionIdArray = Array of TPartitionId;
  TPropertyArray = Array of TProperty;
  TPropertyExpressionArray = Array of TPropertyExpression;
  TPropertyFilterArray = Array of TPropertyFilter;
  TPropertyOrderArray = Array of TPropertyOrder;
  TPropertyReferenceArray = Array of TPropertyReference;
  TQueryArray = Array of TQuery;
  TQueryResultBatchArray = Array of TQueryResultBatch;
  TReadOptionsArray = Array of TReadOptions;
  TResponseHeaderArray = Array of TResponseHeader;
  TRollbackRequestArray = Array of TRollbackRequest;
  TRollbackResponseArray = Array of TRollbackResponse;
  TRunQueryRequestArray = Array of TRunQueryRequest;
  TRunQueryResponseArray = Array of TRunQueryResponse;
  TValueArray = Array of TValue;
  //Anonymous types, using auto-generated names
  TEntityTypeproperties = class;
  TAllocateIdsRequestTypekeysArray = Array of TKey;
  TAllocateIdsResponseTypekeysArray = Array of TKey;
  TCompositeFilterTypefiltersArray = Array of TFilter;
  TGqlQueryTypenameArgsArray = Array of TGqlQueryArg;
  TGqlQueryTypenumberArgsArray = Array of TGqlQueryArg;
  TKeyTypepathArray = Array of TKeyPathElement;
  TLookupRequestTypekeysArray = Array of TKey;
  TLookupResponseTypedeferredArray = Array of TKey;
  TLookupResponseTypefoundArray = Array of TEntityResult;
  TLookupResponseTypemissingArray = Array of TEntityResult;
  TMutationTypedeleteArray = Array of TKey;
  TMutationTypeinsertArray = Array of TEntity;
  TMutationTypeinsertAutoIdArray = Array of TEntity;
  TMutationTypeupdateArray = Array of TEntity;
  TMutationTypeupsertArray = Array of TEntity;
  TMutationResultTypeinsertAutoIdKeysArray = Array of TKey;
  TPropertyTypelistValueArray = Array of TValue;
  TQueryTypegroupByArray = Array of TPropertyReference;
  TQueryTypekindsArray = Array of TKindExpression;
  TQueryTypeorderArray = Array of TPropertyOrder;
  TQueryTypeprojectionArray = Array of TPropertyExpression;
  TQueryResultBatchTypeentityResultsArray = Array of TEntityResult;
  TValueTypelistValueArray = Array of TValue;
  
  { --------------------------------------------------------------------
    TAllocateIdsRequest
    --------------------------------------------------------------------}
  
  TAllocateIdsRequest = Class(TGoogleBaseObject)
  Private
    Fkeys : TAllocateIdsRequestTypekeysArray;
  Protected
    //Property setters
    Procedure Setkeys(AIndex : Integer; AValue : TAllocateIdsRequestTypekeysArray); virtual;
  Public
  Published
    Property keys : TAllocateIdsRequestTypekeysArray Index 0 Read Fkeys Write Setkeys;
  end;
  TAllocateIdsRequestClass = Class of TAllocateIdsRequest;
  
  { --------------------------------------------------------------------
    TAllocateIdsResponse
    --------------------------------------------------------------------}
  
  TAllocateIdsResponse = Class(TGoogleBaseObject)
  Private
    Fheader : TResponseHeader;
    Fkeys : TAllocateIdsResponseTypekeysArray;
  Protected
    //Property setters
    Procedure Setheader(AIndex : Integer; AValue : TResponseHeader); virtual;
    Procedure Setkeys(AIndex : Integer; AValue : TAllocateIdsResponseTypekeysArray); virtual;
  Public
  Published
    Property header : TResponseHeader Index 0 Read Fheader Write Setheader;
    Property keys : TAllocateIdsResponseTypekeysArray Index 8 Read Fkeys Write Setkeys;
  end;
  TAllocateIdsResponseClass = Class of TAllocateIdsResponse;
  
  { --------------------------------------------------------------------
    TBeginTransactionRequest
    --------------------------------------------------------------------}
  
  TBeginTransactionRequest = Class(TGoogleBaseObject)
  Private
    FisolationLevel : String;
  Protected
    //Property setters
    Procedure SetisolationLevel(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property isolationLevel : String Index 0 Read FisolationLevel Write SetisolationLevel;
  end;
  TBeginTransactionRequestClass = Class of TBeginTransactionRequest;
  
  { --------------------------------------------------------------------
    TBeginTransactionResponse
    --------------------------------------------------------------------}
  
  TBeginTransactionResponse = Class(TGoogleBaseObject)
  Private
    Fheader : TResponseHeader;
    Ftransaction : String;
  Protected
    //Property setters
    Procedure Setheader(AIndex : Integer; AValue : TResponseHeader); virtual;
    Procedure Settransaction(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property header : TResponseHeader Index 0 Read Fheader Write Setheader;
    Property transaction : String Index 8 Read Ftransaction Write Settransaction;
  end;
  TBeginTransactionResponseClass = Class of TBeginTransactionResponse;
  
  { --------------------------------------------------------------------
    TCommitRequest
    --------------------------------------------------------------------}
  
  TCommitRequest = Class(TGoogleBaseObject)
  Private
    FignoreReadOnly : boolean;
    Fmode : String;
    Fmutation : TMutation;
    Ftransaction : String;
  Protected
    //Property setters
    Procedure SetignoreReadOnly(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setmode(AIndex : Integer; AValue : String); virtual;
    Procedure Setmutation(AIndex : Integer; AValue : TMutation); virtual;
    Procedure Settransaction(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property ignoreReadOnly : boolean Index 0 Read FignoreReadOnly Write SetignoreReadOnly;
    Property mode : String Index 8 Read Fmode Write Setmode;
    Property mutation : TMutation Index 16 Read Fmutation Write Setmutation;
    Property transaction : String Index 24 Read Ftransaction Write Settransaction;
  end;
  TCommitRequestClass = Class of TCommitRequest;
  
  { --------------------------------------------------------------------
    TCommitResponse
    --------------------------------------------------------------------}
  
  TCommitResponse = Class(TGoogleBaseObject)
  Private
    Fheader : TResponseHeader;
    FmutationResult : TMutationResult;
  Protected
    //Property setters
    Procedure Setheader(AIndex : Integer; AValue : TResponseHeader); virtual;
    Procedure SetmutationResult(AIndex : Integer; AValue : TMutationResult); virtual;
  Public
  Published
    Property header : TResponseHeader Index 0 Read Fheader Write Setheader;
    Property mutationResult : TMutationResult Index 8 Read FmutationResult Write SetmutationResult;
  end;
  TCommitResponseClass = Class of TCommitResponse;
  
  { --------------------------------------------------------------------
    TCompositeFilter
    --------------------------------------------------------------------}
  
  TCompositeFilter = Class(TGoogleBaseObject)
  Private
    Ffilters : TCompositeFilterTypefiltersArray;
    F_operator : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setfilters(AIndex : Integer; AValue : TCompositeFilterTypefiltersArray); virtual;
    Procedure Set_operator(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property filters : TCompositeFilterTypefiltersArray Index 0 Read Ffilters Write Setfilters;
    Property _operator : String Index 8 Read F_operator Write Set_operator;
  end;
  TCompositeFilterClass = Class of TCompositeFilter;
  
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
    Fkey : TKey;
    Fproperties : TEntityTypeproperties;
  Protected
    //Property setters
    Procedure Setkey(AIndex : Integer; AValue : TKey); virtual;
    Procedure Setproperties(AIndex : Integer; AValue : TEntityTypeproperties); virtual;
  Public
  Published
    Property key : TKey Index 0 Read Fkey Write Setkey;
    Property properties : TEntityTypeproperties Index 8 Read Fproperties Write Setproperties;
  end;
  TEntityClass = Class of TEntity;
  
  { --------------------------------------------------------------------
    TEntityResult
    --------------------------------------------------------------------}
  
  TEntityResult = Class(TGoogleBaseObject)
  Private
    Fentity : TEntity;
  Protected
    //Property setters
    Procedure Setentity(AIndex : Integer; AValue : TEntity); virtual;
  Public
  Published
    Property entity : TEntity Index 0 Read Fentity Write Setentity;
  end;
  TEntityResultClass = Class of TEntityResult;
  
  { --------------------------------------------------------------------
    TFilter
    --------------------------------------------------------------------}
  
  TFilter = Class(TGoogleBaseObject)
  Private
    FcompositeFilter : TCompositeFilter;
    FpropertyFilter : TPropertyFilter;
  Protected
    //Property setters
    Procedure SetcompositeFilter(AIndex : Integer; AValue : TCompositeFilter); virtual;
    Procedure SetpropertyFilter(AIndex : Integer; AValue : TPropertyFilter); virtual;
  Public
  Published
    Property compositeFilter : TCompositeFilter Index 0 Read FcompositeFilter Write SetcompositeFilter;
    Property propertyFilter : TPropertyFilter Index 8 Read FpropertyFilter Write SetpropertyFilter;
  end;
  TFilterClass = Class of TFilter;
  
  { --------------------------------------------------------------------
    TGqlQuery
    --------------------------------------------------------------------}
  
  TGqlQuery = Class(TGoogleBaseObject)
  Private
    FallowLiteral : boolean;
    FnameArgs : TGqlQueryTypenameArgsArray;
    FnumberArgs : TGqlQueryTypenumberArgsArray;
    FqueryString : String;
  Protected
    //Property setters
    Procedure SetallowLiteral(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetnameArgs(AIndex : Integer; AValue : TGqlQueryTypenameArgsArray); virtual;
    Procedure SetnumberArgs(AIndex : Integer; AValue : TGqlQueryTypenumberArgsArray); virtual;
    Procedure SetqueryString(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property allowLiteral : boolean Index 0 Read FallowLiteral Write SetallowLiteral;
    Property nameArgs : TGqlQueryTypenameArgsArray Index 8 Read FnameArgs Write SetnameArgs;
    Property numberArgs : TGqlQueryTypenumberArgsArray Index 16 Read FnumberArgs Write SetnumberArgs;
    Property queryString : String Index 24 Read FqueryString Write SetqueryString;
  end;
  TGqlQueryClass = Class of TGqlQuery;
  
  { --------------------------------------------------------------------
    TGqlQueryArg
    --------------------------------------------------------------------}
  
  TGqlQueryArg = Class(TGoogleBaseObject)
  Private
    Fcursor : String;
    Fname : String;
    Fvalue : TValue;
  Protected
    //Property setters
    Procedure Setcursor(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : TValue); virtual;
  Public
  Published
    Property cursor : String Index 0 Read Fcursor Write Setcursor;
    Property name : String Index 8 Read Fname Write Setname;
    Property value : TValue Index 16 Read Fvalue Write Setvalue;
  end;
  TGqlQueryArgClass = Class of TGqlQueryArg;
  
  { --------------------------------------------------------------------
    TKey
    --------------------------------------------------------------------}
  
  TKey = Class(TGoogleBaseObject)
  Private
    FpartitionId : TPartitionId;
    Fpath : TKeyTypepathArray;
  Protected
    //Property setters
    Procedure SetpartitionId(AIndex : Integer; AValue : TPartitionId); virtual;
    Procedure Setpath(AIndex : Integer; AValue : TKeyTypepathArray); virtual;
  Public
  Published
    Property partitionId : TPartitionId Index 0 Read FpartitionId Write SetpartitionId;
    Property path : TKeyTypepathArray Index 8 Read Fpath Write Setpath;
  end;
  TKeyClass = Class of TKey;
  
  { --------------------------------------------------------------------
    TKeyPathElement
    --------------------------------------------------------------------}
  
  TKeyPathElement = Class(TGoogleBaseObject)
  Private
    Fid : String;
    Fkind : String;
    Fname : String;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property kind : String Index 8 Read Fkind Write Setkind;
    Property name : String Index 16 Read Fname Write Setname;
  end;
  TKeyPathElementClass = Class of TKeyPathElement;
  
  { --------------------------------------------------------------------
    TKindExpression
    --------------------------------------------------------------------}
  
  TKindExpression = Class(TGoogleBaseObject)
  Private
    Fname : String;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property name : String Index 0 Read Fname Write Setname;
  end;
  TKindExpressionClass = Class of TKindExpression;
  
  { --------------------------------------------------------------------
    TLookupRequest
    --------------------------------------------------------------------}
  
  TLookupRequest = Class(TGoogleBaseObject)
  Private
    Fkeys : TLookupRequestTypekeysArray;
    FreadOptions : TReadOptions;
  Protected
    //Property setters
    Procedure Setkeys(AIndex : Integer; AValue : TLookupRequestTypekeysArray); virtual;
    Procedure SetreadOptions(AIndex : Integer; AValue : TReadOptions); virtual;
  Public
  Published
    Property keys : TLookupRequestTypekeysArray Index 0 Read Fkeys Write Setkeys;
    Property readOptions : TReadOptions Index 8 Read FreadOptions Write SetreadOptions;
  end;
  TLookupRequestClass = Class of TLookupRequest;
  
  { --------------------------------------------------------------------
    TLookupResponse
    --------------------------------------------------------------------}
  
  TLookupResponse = Class(TGoogleBaseObject)
  Private
    Fdeferred : TLookupResponseTypedeferredArray;
    Ffound : TLookupResponseTypefoundArray;
    Fheader : TResponseHeader;
    Fmissing : TLookupResponseTypemissingArray;
  Protected
    //Property setters
    Procedure Setdeferred(AIndex : Integer; AValue : TLookupResponseTypedeferredArray); virtual;
    Procedure Setfound(AIndex : Integer; AValue : TLookupResponseTypefoundArray); virtual;
    Procedure Setheader(AIndex : Integer; AValue : TResponseHeader); virtual;
    Procedure Setmissing(AIndex : Integer; AValue : TLookupResponseTypemissingArray); virtual;
  Public
  Published
    Property deferred : TLookupResponseTypedeferredArray Index 0 Read Fdeferred Write Setdeferred;
    Property found : TLookupResponseTypefoundArray Index 8 Read Ffound Write Setfound;
    Property header : TResponseHeader Index 16 Read Fheader Write Setheader;
    Property missing : TLookupResponseTypemissingArray Index 24 Read Fmissing Write Setmissing;
  end;
  TLookupResponseClass = Class of TLookupResponse;
  
  { --------------------------------------------------------------------
    TMutation
    --------------------------------------------------------------------}
  
  TMutation = Class(TGoogleBaseObject)
  Private
    Fdelete : TMutationTypedeleteArray;
    Fforce : boolean;
    Finsert : TMutationTypeinsertArray;
    FinsertAutoId : TMutationTypeinsertAutoIdArray;
    Fupdate : TMutationTypeupdateArray;
    Fupsert : TMutationTypeupsertArray;
  Protected
    //Property setters
    Procedure Setdelete(AIndex : Integer; AValue : TMutationTypedeleteArray); virtual;
    Procedure Setforce(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setinsert(AIndex : Integer; AValue : TMutationTypeinsertArray); virtual;
    Procedure SetinsertAutoId(AIndex : Integer; AValue : TMutationTypeinsertAutoIdArray); virtual;
    Procedure Setupdate(AIndex : Integer; AValue : TMutationTypeupdateArray); virtual;
    Procedure Setupsert(AIndex : Integer; AValue : TMutationTypeupsertArray); virtual;
  Public
  Published
    Property delete : TMutationTypedeleteArray Index 0 Read Fdelete Write Setdelete;
    Property force : boolean Index 8 Read Fforce Write Setforce;
    Property insert : TMutationTypeinsertArray Index 16 Read Finsert Write Setinsert;
    Property insertAutoId : TMutationTypeinsertAutoIdArray Index 24 Read FinsertAutoId Write SetinsertAutoId;
    Property update : TMutationTypeupdateArray Index 32 Read Fupdate Write Setupdate;
    Property upsert : TMutationTypeupsertArray Index 40 Read Fupsert Write Setupsert;
  end;
  TMutationClass = Class of TMutation;
  
  { --------------------------------------------------------------------
    TMutationResult
    --------------------------------------------------------------------}
  
  TMutationResult = Class(TGoogleBaseObject)
  Private
    FindexUpdates : integer;
    FinsertAutoIdKeys : TMutationResultTypeinsertAutoIdKeysArray;
  Protected
    //Property setters
    Procedure SetindexUpdates(AIndex : Integer; AValue : integer); virtual;
    Procedure SetinsertAutoIdKeys(AIndex : Integer; AValue : TMutationResultTypeinsertAutoIdKeysArray); virtual;
  Public
  Published
    Property indexUpdates : integer Index 0 Read FindexUpdates Write SetindexUpdates;
    Property insertAutoIdKeys : TMutationResultTypeinsertAutoIdKeysArray Index 8 Read FinsertAutoIdKeys Write SetinsertAutoIdKeys;
  end;
  TMutationResultClass = Class of TMutationResult;
  
  { --------------------------------------------------------------------
    TPartitionId
    --------------------------------------------------------------------}
  
  TPartitionId = Class(TGoogleBaseObject)
  Private
    FdatasetId : String;
    Fnamespace : String;
  Protected
    //Property setters
    Procedure SetdatasetId(AIndex : Integer; AValue : String); virtual;
    Procedure Setnamespace(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property datasetId : String Index 0 Read FdatasetId Write SetdatasetId;
    Property namespace : String Index 8 Read Fnamespace Write Setnamespace;
  end;
  TPartitionIdClass = Class of TPartitionId;
  
  { --------------------------------------------------------------------
    TProperty
    --------------------------------------------------------------------}
  
  TProperty = Class(TGoogleBaseObject)
  Private
    FblobKeyValue : String;
    FblobValue : String;
    FbooleanValue : boolean;
    FdateTimeValue : TDatetime;
    FdoubleValue : double;
    FentityValue : TEntity;
    Findexed : boolean;
    FintegerValue : String;
    FkeyValue : TKey;
    FlistValue : TPropertyTypelistValueArray;
    Fmeaning : integer;
    FstringValue : String;
  Protected
    //Property setters
    Procedure SetblobKeyValue(AIndex : Integer; AValue : String); virtual;
    Procedure SetblobValue(AIndex : Integer; AValue : String); virtual;
    Procedure SetbooleanValue(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetdateTimeValue(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetdoubleValue(AIndex : Integer; AValue : double); virtual;
    Procedure SetentityValue(AIndex : Integer; AValue : TEntity); virtual;
    Procedure Setindexed(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetintegerValue(AIndex : Integer; AValue : String); virtual;
    Procedure SetkeyValue(AIndex : Integer; AValue : TKey); virtual;
    Procedure SetlistValue(AIndex : Integer; AValue : TPropertyTypelistValueArray); virtual;
    Procedure Setmeaning(AIndex : Integer; AValue : integer); virtual;
    Procedure SetstringValue(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property blobKeyValue : String Index 0 Read FblobKeyValue Write SetblobKeyValue;
    Property blobValue : String Index 8 Read FblobValue Write SetblobValue;
    Property booleanValue : boolean Index 16 Read FbooleanValue Write SetbooleanValue;
    Property dateTimeValue : TDatetime Index 24 Read FdateTimeValue Write SetdateTimeValue;
    Property doubleValue : double Index 32 Read FdoubleValue Write SetdoubleValue;
    Property entityValue : TEntity Index 40 Read FentityValue Write SetentityValue;
    Property indexed : boolean Index 48 Read Findexed Write Setindexed;
    Property integerValue : String Index 56 Read FintegerValue Write SetintegerValue;
    Property keyValue : TKey Index 64 Read FkeyValue Write SetkeyValue;
    Property listValue : TPropertyTypelistValueArray Index 72 Read FlistValue Write SetlistValue;
    Property meaning : integer Index 80 Read Fmeaning Write Setmeaning;
    Property stringValue : String Index 88 Read FstringValue Write SetstringValue;
  end;
  TPropertyClass = Class of TProperty;
  
  { --------------------------------------------------------------------
    TPropertyExpression
    --------------------------------------------------------------------}
  
  TPropertyExpression = Class(TGoogleBaseObject)
  Private
    FaggregationFunction : String;
    F_property : TPropertyReference;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetaggregationFunction(AIndex : Integer; AValue : String); virtual;
    Procedure Set_property(AIndex : Integer; AValue : TPropertyReference); virtual;
  Public
  Published
    Property aggregationFunction : String Index 0 Read FaggregationFunction Write SetaggregationFunction;
    Property _property : TPropertyReference Index 8 Read F_property Write Set_property;
  end;
  TPropertyExpressionClass = Class of TPropertyExpression;
  
  { --------------------------------------------------------------------
    TPropertyFilter
    --------------------------------------------------------------------}
  
  TPropertyFilter = Class(TGoogleBaseObject)
  Private
    F_operator : String;
    F_property : TPropertyReference;
    Fvalue : TValue;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_operator(AIndex : Integer; AValue : String); virtual;
    Procedure Set_property(AIndex : Integer; AValue : TPropertyReference); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : TValue); virtual;
  Public
  Published
    Property _operator : String Index 0 Read F_operator Write Set_operator;
    Property _property : TPropertyReference Index 8 Read F_property Write Set_property;
    Property value : TValue Index 16 Read Fvalue Write Setvalue;
  end;
  TPropertyFilterClass = Class of TPropertyFilter;
  
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
    Procedure Setdirection(AIndex : Integer; AValue : String); virtual;
    Procedure Set_property(AIndex : Integer; AValue : TPropertyReference); virtual;
  Public
  Published
    Property direction : String Index 0 Read Fdirection Write Setdirection;
    Property _property : TPropertyReference Index 8 Read F_property Write Set_property;
  end;
  TPropertyOrderClass = Class of TPropertyOrder;
  
  { --------------------------------------------------------------------
    TPropertyReference
    --------------------------------------------------------------------}
  
  TPropertyReference = Class(TGoogleBaseObject)
  Private
    Fname : String;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property name : String Index 0 Read Fname Write Setname;
  end;
  TPropertyReferenceClass = Class of TPropertyReference;
  
  { --------------------------------------------------------------------
    TQuery
    --------------------------------------------------------------------}
  
  TQuery = Class(TGoogleBaseObject)
  Private
    FendCursor : String;
    Ffilter : TFilter;
    FgroupBy : TQueryTypegroupByArray;
    Fkinds : TQueryTypekindsArray;
    Flimit : integer;
    Foffset : integer;
    Forder : TQueryTypeorderArray;
    Fprojection : TQueryTypeprojectionArray;
    FstartCursor : String;
  Protected
    //Property setters
    Procedure SetendCursor(AIndex : Integer; AValue : String); virtual;
    Procedure Setfilter(AIndex : Integer; AValue : TFilter); virtual;
    Procedure SetgroupBy(AIndex : Integer; AValue : TQueryTypegroupByArray); virtual;
    Procedure Setkinds(AIndex : Integer; AValue : TQueryTypekindsArray); virtual;
    Procedure Setlimit(AIndex : Integer; AValue : integer); virtual;
    Procedure Setoffset(AIndex : Integer; AValue : integer); virtual;
    Procedure Setorder(AIndex : Integer; AValue : TQueryTypeorderArray); virtual;
    Procedure Setprojection(AIndex : Integer; AValue : TQueryTypeprojectionArray); virtual;
    Procedure SetstartCursor(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property endCursor : String Index 0 Read FendCursor Write SetendCursor;
    Property filter : TFilter Index 8 Read Ffilter Write Setfilter;
    Property groupBy : TQueryTypegroupByArray Index 16 Read FgroupBy Write SetgroupBy;
    Property kinds : TQueryTypekindsArray Index 24 Read Fkinds Write Setkinds;
    Property limit : integer Index 32 Read Flimit Write Setlimit;
    Property offset : integer Index 40 Read Foffset Write Setoffset;
    Property order : TQueryTypeorderArray Index 48 Read Forder Write Setorder;
    Property projection : TQueryTypeprojectionArray Index 56 Read Fprojection Write Setprojection;
    Property startCursor : String Index 64 Read FstartCursor Write SetstartCursor;
  end;
  TQueryClass = Class of TQuery;
  
  { --------------------------------------------------------------------
    TQueryResultBatch
    --------------------------------------------------------------------}
  
  TQueryResultBatch = Class(TGoogleBaseObject)
  Private
    FendCursor : String;
    FentityResultType : String;
    FentityResults : TQueryResultBatchTypeentityResultsArray;
    FmoreResults : String;
    FskippedResults : integer;
  Protected
    //Property setters
    Procedure SetendCursor(AIndex : Integer; AValue : String); virtual;
    Procedure SetentityResultType(AIndex : Integer; AValue : String); virtual;
    Procedure SetentityResults(AIndex : Integer; AValue : TQueryResultBatchTypeentityResultsArray); virtual;
    Procedure SetmoreResults(AIndex : Integer; AValue : String); virtual;
    Procedure SetskippedResults(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property endCursor : String Index 0 Read FendCursor Write SetendCursor;
    Property entityResultType : String Index 8 Read FentityResultType Write SetentityResultType;
    Property entityResults : TQueryResultBatchTypeentityResultsArray Index 16 Read FentityResults Write SetentityResults;
    Property moreResults : String Index 24 Read FmoreResults Write SetmoreResults;
    Property skippedResults : integer Index 32 Read FskippedResults Write SetskippedResults;
  end;
  TQueryResultBatchClass = Class of TQueryResultBatch;
  
  { --------------------------------------------------------------------
    TReadOptions
    --------------------------------------------------------------------}
  
  TReadOptions = Class(TGoogleBaseObject)
  Private
    FreadConsistency : String;
    Ftransaction : String;
  Protected
    //Property setters
    Procedure SetreadConsistency(AIndex : Integer; AValue : String); virtual;
    Procedure Settransaction(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property readConsistency : String Index 0 Read FreadConsistency Write SetreadConsistency;
    Property transaction : String Index 8 Read Ftransaction Write Settransaction;
  end;
  TReadOptionsClass = Class of TReadOptions;
  
  { --------------------------------------------------------------------
    TResponseHeader
    --------------------------------------------------------------------}
  
  TResponseHeader = Class(TGoogleBaseObject)
  Private
    Fkind : String;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property kind : String Index 0 Read Fkind Write Setkind;
  end;
  TResponseHeaderClass = Class of TResponseHeader;
  
  { --------------------------------------------------------------------
    TRollbackRequest
    --------------------------------------------------------------------}
  
  TRollbackRequest = Class(TGoogleBaseObject)
  Private
    Ftransaction : String;
  Protected
    //Property setters
    Procedure Settransaction(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property transaction : String Index 0 Read Ftransaction Write Settransaction;
  end;
  TRollbackRequestClass = Class of TRollbackRequest;
  
  { --------------------------------------------------------------------
    TRollbackResponse
    --------------------------------------------------------------------}
  
  TRollbackResponse = Class(TGoogleBaseObject)
  Private
    Fheader : TResponseHeader;
  Protected
    //Property setters
    Procedure Setheader(AIndex : Integer; AValue : TResponseHeader); virtual;
  Public
  Published
    Property header : TResponseHeader Index 0 Read Fheader Write Setheader;
  end;
  TRollbackResponseClass = Class of TRollbackResponse;
  
  { --------------------------------------------------------------------
    TRunQueryRequest
    --------------------------------------------------------------------}
  
  TRunQueryRequest = Class(TGoogleBaseObject)
  Private
    FgqlQuery : TGqlQuery;
    FpartitionId : TPartitionId;
    Fquery : TQuery;
    FreadOptions : TReadOptions;
  Protected
    //Property setters
    Procedure SetgqlQuery(AIndex : Integer; AValue : TGqlQuery); virtual;
    Procedure SetpartitionId(AIndex : Integer; AValue : TPartitionId); virtual;
    Procedure Setquery(AIndex : Integer; AValue : TQuery); virtual;
    Procedure SetreadOptions(AIndex : Integer; AValue : TReadOptions); virtual;
  Public
  Published
    Property gqlQuery : TGqlQuery Index 0 Read FgqlQuery Write SetgqlQuery;
    Property partitionId : TPartitionId Index 8 Read FpartitionId Write SetpartitionId;
    Property query : TQuery Index 16 Read Fquery Write Setquery;
    Property readOptions : TReadOptions Index 24 Read FreadOptions Write SetreadOptions;
  end;
  TRunQueryRequestClass = Class of TRunQueryRequest;
  
  { --------------------------------------------------------------------
    TRunQueryResponse
    --------------------------------------------------------------------}
  
  TRunQueryResponse = Class(TGoogleBaseObject)
  Private
    Fbatch : TQueryResultBatch;
    Fheader : TResponseHeader;
  Protected
    //Property setters
    Procedure Setbatch(AIndex : Integer; AValue : TQueryResultBatch); virtual;
    Procedure Setheader(AIndex : Integer; AValue : TResponseHeader); virtual;
  Public
  Published
    Property batch : TQueryResultBatch Index 0 Read Fbatch Write Setbatch;
    Property header : TResponseHeader Index 8 Read Fheader Write Setheader;
  end;
  TRunQueryResponseClass = Class of TRunQueryResponse;
  
  { --------------------------------------------------------------------
    TValue
    --------------------------------------------------------------------}
  
  TValue = Class(TGoogleBaseObject)
  Private
    FblobKeyValue : String;
    FblobValue : String;
    FbooleanValue : boolean;
    FdateTimeValue : TDatetime;
    FdoubleValue : double;
    FentityValue : TEntity;
    Findexed : boolean;
    FintegerValue : String;
    FkeyValue : TKey;
    FlistValue : TValueTypelistValueArray;
    Fmeaning : integer;
    FstringValue : String;
  Protected
    //Property setters
    Procedure SetblobKeyValue(AIndex : Integer; AValue : String); virtual;
    Procedure SetblobValue(AIndex : Integer; AValue : String); virtual;
    Procedure SetbooleanValue(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetdateTimeValue(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetdoubleValue(AIndex : Integer; AValue : double); virtual;
    Procedure SetentityValue(AIndex : Integer; AValue : TEntity); virtual;
    Procedure Setindexed(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetintegerValue(AIndex : Integer; AValue : String); virtual;
    Procedure SetkeyValue(AIndex : Integer; AValue : TKey); virtual;
    Procedure SetlistValue(AIndex : Integer; AValue : TValueTypelistValueArray); virtual;
    Procedure Setmeaning(AIndex : Integer; AValue : integer); virtual;
    Procedure SetstringValue(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property blobKeyValue : String Index 0 Read FblobKeyValue Write SetblobKeyValue;
    Property blobValue : String Index 8 Read FblobValue Write SetblobValue;
    Property booleanValue : boolean Index 16 Read FbooleanValue Write SetbooleanValue;
    Property dateTimeValue : TDatetime Index 24 Read FdateTimeValue Write SetdateTimeValue;
    Property doubleValue : double Index 32 Read FdoubleValue Write SetdoubleValue;
    Property entityValue : TEntity Index 40 Read FentityValue Write SetentityValue;
    Property indexed : boolean Index 48 Read Findexed Write Setindexed;
    Property integerValue : String Index 56 Read FintegerValue Write SetintegerValue;
    Property keyValue : TKey Index 64 Read FkeyValue Write SetkeyValue;
    Property listValue : TValueTypelistValueArray Index 72 Read FlistValue Write SetlistValue;
    Property meaning : integer Index 80 Read Fmeaning Write Setmeaning;
    Property stringValue : String Index 88 Read FstringValue Write SetstringValue;
  end;
  TValueClass = Class of TValue;
  
  { --------------------------------------------------------------------
    TDatasetsResource
    --------------------------------------------------------------------}
  
  TDatasetsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function AllocateIds(datasetId: string; aAllocateIdsRequest : TAllocateIdsRequest) : TAllocateIdsResponse;
    Function BeginTransaction(datasetId: string; aBeginTransactionRequest : TBeginTransactionRequest) : TBeginTransactionResponse;
    Function Commit(datasetId: string; aCommitRequest : TCommitRequest) : TCommitResponse;
    Function Lookup(datasetId: string; aLookupRequest : TLookupRequest) : TLookupResponse;
    Function Rollback(datasetId: string; aRollbackRequest : TRollbackRequest) : TRollbackResponse;
    Function RunQuery(datasetId: string; aRunQueryRequest : TRunQueryRequest) : TRunQueryResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TDatastoreAPI
    --------------------------------------------------------------------}
  
  TDatastoreAPI = Class(TGoogleAPI)
  Private
    FDatasetsInstance : TDatasetsResource;
    Function GetDatasetsInstance : TDatasetsResource;virtual;
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
    Function CreateDatasetsResource(AOwner : TComponent) : TDatasetsResource;virtual;overload;
    Function CreateDatasetsResource : TDatasetsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property DatasetsResource : TDatasetsResource Read GetDatasetsInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TAllocateIdsRequest
  --------------------------------------------------------------------}


Procedure TAllocateIdsRequest.Setkeys(AIndex : Integer; AValue : TAllocateIdsRequestTypekeysArray); 

begin
  If (Fkeys=AValue) then exit;
  Fkeys:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAllocateIdsResponse
  --------------------------------------------------------------------}


Procedure TAllocateIdsResponse.Setheader(AIndex : Integer; AValue : TResponseHeader); 

begin
  If (Fheader=AValue) then exit;
  Fheader:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAllocateIdsResponse.Setkeys(AIndex : Integer; AValue : TAllocateIdsResponseTypekeysArray); 

begin
  If (Fkeys=AValue) then exit;
  Fkeys:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBeginTransactionRequest
  --------------------------------------------------------------------}


Procedure TBeginTransactionRequest.SetisolationLevel(AIndex : Integer; AValue : String); 

begin
  If (FisolationLevel=AValue) then exit;
  FisolationLevel:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBeginTransactionResponse
  --------------------------------------------------------------------}


Procedure TBeginTransactionResponse.Setheader(AIndex : Integer; AValue : TResponseHeader); 

begin
  If (Fheader=AValue) then exit;
  Fheader:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBeginTransactionResponse.Settransaction(AIndex : Integer; AValue : String); 

begin
  If (Ftransaction=AValue) then exit;
  Ftransaction:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCommitRequest
  --------------------------------------------------------------------}


Procedure TCommitRequest.SetignoreReadOnly(AIndex : Integer; AValue : boolean); 

begin
  If (FignoreReadOnly=AValue) then exit;
  FignoreReadOnly:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommitRequest.Setmode(AIndex : Integer; AValue : String); 

begin
  If (Fmode=AValue) then exit;
  Fmode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommitRequest.Setmutation(AIndex : Integer; AValue : TMutation); 

begin
  If (Fmutation=AValue) then exit;
  Fmutation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommitRequest.Settransaction(AIndex : Integer; AValue : String); 

begin
  If (Ftransaction=AValue) then exit;
  Ftransaction:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCommitResponse
  --------------------------------------------------------------------}


Procedure TCommitResponse.Setheader(AIndex : Integer; AValue : TResponseHeader); 

begin
  If (Fheader=AValue) then exit;
  Fheader:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCommitResponse.SetmutationResult(AIndex : Integer; AValue : TMutationResult); 

begin
  If (FmutationResult=AValue) then exit;
  FmutationResult:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCompositeFilter
  --------------------------------------------------------------------}


Procedure TCompositeFilter.Setfilters(AIndex : Integer; AValue : TCompositeFilterTypefiltersArray); 

begin
  If (Ffilters=AValue) then exit;
  Ffilters:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCompositeFilter.Set_operator(AIndex : Integer; AValue : String); 

begin
  If (F_operator=AValue) then exit;
  F_operator:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TCompositeFilter.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_operator' : Result:='operator';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
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


Procedure TEntity.Setkey(AIndex : Integer; AValue : TKey); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntity.Setproperties(AIndex : Integer; AValue : TEntityTypeproperties); 

begin
  If (Fproperties=AValue) then exit;
  Fproperties:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TEntityResult
  --------------------------------------------------------------------}


Procedure TEntityResult.Setentity(AIndex : Integer; AValue : TEntity); 

begin
  If (Fentity=AValue) then exit;
  Fentity:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFilter
  --------------------------------------------------------------------}


Procedure TFilter.SetcompositeFilter(AIndex : Integer; AValue : TCompositeFilter); 

begin
  If (FcompositeFilter=AValue) then exit;
  FcompositeFilter:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFilter.SetpropertyFilter(AIndex : Integer; AValue : TPropertyFilter); 

begin
  If (FpropertyFilter=AValue) then exit;
  FpropertyFilter:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGqlQuery
  --------------------------------------------------------------------}


Procedure TGqlQuery.SetallowLiteral(AIndex : Integer; AValue : boolean); 

begin
  If (FallowLiteral=AValue) then exit;
  FallowLiteral:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGqlQuery.SetnameArgs(AIndex : Integer; AValue : TGqlQueryTypenameArgsArray); 

begin
  If (FnameArgs=AValue) then exit;
  FnameArgs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGqlQuery.SetnumberArgs(AIndex : Integer; AValue : TGqlQueryTypenumberArgsArray); 

begin
  If (FnumberArgs=AValue) then exit;
  FnumberArgs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGqlQuery.SetqueryString(AIndex : Integer; AValue : String); 

begin
  If (FqueryString=AValue) then exit;
  FqueryString:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGqlQueryArg
  --------------------------------------------------------------------}


Procedure TGqlQueryArg.Setcursor(AIndex : Integer; AValue : String); 

begin
  If (Fcursor=AValue) then exit;
  Fcursor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGqlQueryArg.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGqlQueryArg.Setvalue(AIndex : Integer; AValue : TValue); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TKey
  --------------------------------------------------------------------}


Procedure TKey.SetpartitionId(AIndex : Integer; AValue : TPartitionId); 

begin
  If (FpartitionId=AValue) then exit;
  FpartitionId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TKey.Setpath(AIndex : Integer; AValue : TKeyTypepathArray); 

begin
  If (Fpath=AValue) then exit;
  Fpath:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TKeyPathElement
  --------------------------------------------------------------------}


Procedure TKeyPathElement.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TKeyPathElement.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TKeyPathElement.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TKindExpression
  --------------------------------------------------------------------}


Procedure TKindExpression.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLookupRequest
  --------------------------------------------------------------------}


Procedure TLookupRequest.Setkeys(AIndex : Integer; AValue : TLookupRequestTypekeysArray); 

begin
  If (Fkeys=AValue) then exit;
  Fkeys:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLookupRequest.SetreadOptions(AIndex : Integer; AValue : TReadOptions); 

begin
  If (FreadOptions=AValue) then exit;
  FreadOptions:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLookupResponse
  --------------------------------------------------------------------}


Procedure TLookupResponse.Setdeferred(AIndex : Integer; AValue : TLookupResponseTypedeferredArray); 

begin
  If (Fdeferred=AValue) then exit;
  Fdeferred:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLookupResponse.Setfound(AIndex : Integer; AValue : TLookupResponseTypefoundArray); 

begin
  If (Ffound=AValue) then exit;
  Ffound:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLookupResponse.Setheader(AIndex : Integer; AValue : TResponseHeader); 

begin
  If (Fheader=AValue) then exit;
  Fheader:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLookupResponse.Setmissing(AIndex : Integer; AValue : TLookupResponseTypemissingArray); 

begin
  If (Fmissing=AValue) then exit;
  Fmissing:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TMutation
  --------------------------------------------------------------------}


Procedure TMutation.Setdelete(AIndex : Integer; AValue : TMutationTypedeleteArray); 

begin
  If (Fdelete=AValue) then exit;
  Fdelete:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMutation.Setforce(AIndex : Integer; AValue : boolean); 

begin
  If (Fforce=AValue) then exit;
  Fforce:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMutation.Setinsert(AIndex : Integer; AValue : TMutationTypeinsertArray); 

begin
  If (Finsert=AValue) then exit;
  Finsert:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMutation.SetinsertAutoId(AIndex : Integer; AValue : TMutationTypeinsertAutoIdArray); 

begin
  If (FinsertAutoId=AValue) then exit;
  FinsertAutoId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMutation.Setupdate(AIndex : Integer; AValue : TMutationTypeupdateArray); 

begin
  If (Fupdate=AValue) then exit;
  Fupdate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMutation.Setupsert(AIndex : Integer; AValue : TMutationTypeupsertArray); 

begin
  If (Fupsert=AValue) then exit;
  Fupsert:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TMutationResult
  --------------------------------------------------------------------}


Procedure TMutationResult.SetindexUpdates(AIndex : Integer; AValue : integer); 

begin
  If (FindexUpdates=AValue) then exit;
  FindexUpdates:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMutationResult.SetinsertAutoIdKeys(AIndex : Integer; AValue : TMutationResultTypeinsertAutoIdKeysArray); 

begin
  If (FinsertAutoIdKeys=AValue) then exit;
  FinsertAutoIdKeys:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPartitionId
  --------------------------------------------------------------------}


Procedure TPartitionId.SetdatasetId(AIndex : Integer; AValue : String); 

begin
  If (FdatasetId=AValue) then exit;
  FdatasetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPartitionId.Setnamespace(AIndex : Integer; AValue : String); 

begin
  If (Fnamespace=AValue) then exit;
  Fnamespace:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProperty
  --------------------------------------------------------------------}


Procedure TProperty.SetblobKeyValue(AIndex : Integer; AValue : String); 

begin
  If (FblobKeyValue=AValue) then exit;
  FblobKeyValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProperty.SetblobValue(AIndex : Integer; AValue : String); 

begin
  If (FblobValue=AValue) then exit;
  FblobValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProperty.SetbooleanValue(AIndex : Integer; AValue : boolean); 

begin
  If (FbooleanValue=AValue) then exit;
  FbooleanValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProperty.SetdateTimeValue(AIndex : Integer; AValue : TDatetime); 

begin
  If (FdateTimeValue=AValue) then exit;
  FdateTimeValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProperty.SetdoubleValue(AIndex : Integer; AValue : double); 

begin
  If (FdoubleValue=AValue) then exit;
  FdoubleValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProperty.SetentityValue(AIndex : Integer; AValue : TEntity); 

begin
  If (FentityValue=AValue) then exit;
  FentityValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProperty.Setindexed(AIndex : Integer; AValue : boolean); 

begin
  If (Findexed=AValue) then exit;
  Findexed:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProperty.SetintegerValue(AIndex : Integer; AValue : String); 

begin
  If (FintegerValue=AValue) then exit;
  FintegerValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProperty.SetkeyValue(AIndex : Integer; AValue : TKey); 

begin
  If (FkeyValue=AValue) then exit;
  FkeyValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProperty.SetlistValue(AIndex : Integer; AValue : TPropertyTypelistValueArray); 

begin
  If (FlistValue=AValue) then exit;
  FlistValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProperty.Setmeaning(AIndex : Integer; AValue : integer); 

begin
  If (Fmeaning=AValue) then exit;
  Fmeaning:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProperty.SetstringValue(AIndex : Integer; AValue : String); 

begin
  If (FstringValue=AValue) then exit;
  FstringValue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPropertyExpression
  --------------------------------------------------------------------}


Procedure TPropertyExpression.SetaggregationFunction(AIndex : Integer; AValue : String); 

begin
  If (FaggregationFunction=AValue) then exit;
  FaggregationFunction:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPropertyExpression.Set_property(AIndex : Integer; AValue : TPropertyReference); 

begin
  If (F_property=AValue) then exit;
  F_property:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TPropertyExpression.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_property' : Result:='property';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TPropertyFilter
  --------------------------------------------------------------------}


Procedure TPropertyFilter.Set_operator(AIndex : Integer; AValue : String); 

begin
  If (F_operator=AValue) then exit;
  F_operator:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPropertyFilter.Set_property(AIndex : Integer; AValue : TPropertyReference); 

begin
  If (F_property=AValue) then exit;
  F_property:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPropertyFilter.Setvalue(AIndex : Integer; AValue : TValue); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TPropertyFilter.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_operator' : Result:='operator';
  '_property' : Result:='property';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TPropertyOrder
  --------------------------------------------------------------------}


Procedure TPropertyOrder.Setdirection(AIndex : Integer; AValue : String); 

begin
  If (Fdirection=AValue) then exit;
  Fdirection:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPropertyOrder.Set_property(AIndex : Integer; AValue : TPropertyReference); 

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
  TPropertyReference
  --------------------------------------------------------------------}


Procedure TPropertyReference.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TQuery
  --------------------------------------------------------------------}


Procedure TQuery.SetendCursor(AIndex : Integer; AValue : String); 

begin
  If (FendCursor=AValue) then exit;
  FendCursor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.Setfilter(AIndex : Integer; AValue : TFilter); 

begin
  If (Ffilter=AValue) then exit;
  Ffilter:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.SetgroupBy(AIndex : Integer; AValue : TQueryTypegroupByArray); 

begin
  If (FgroupBy=AValue) then exit;
  FgroupBy:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.Setkinds(AIndex : Integer; AValue : TQueryTypekindsArray); 

begin
  If (Fkinds=AValue) then exit;
  Fkinds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.Setlimit(AIndex : Integer; AValue : integer); 

begin
  If (Flimit=AValue) then exit;
  Flimit:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.Setoffset(AIndex : Integer; AValue : integer); 

begin
  If (Foffset=AValue) then exit;
  Foffset:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.Setorder(AIndex : Integer; AValue : TQueryTypeorderArray); 

begin
  If (Forder=AValue) then exit;
  Forder:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.Setprojection(AIndex : Integer; AValue : TQueryTypeprojectionArray); 

begin
  If (Fprojection=AValue) then exit;
  Fprojection:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.SetstartCursor(AIndex : Integer; AValue : String); 

begin
  If (FstartCursor=AValue) then exit;
  FstartCursor:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TQueryResultBatch
  --------------------------------------------------------------------}


Procedure TQueryResultBatch.SetendCursor(AIndex : Integer; AValue : String); 

begin
  If (FendCursor=AValue) then exit;
  FendCursor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQueryResultBatch.SetentityResultType(AIndex : Integer; AValue : String); 

begin
  If (FentityResultType=AValue) then exit;
  FentityResultType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQueryResultBatch.SetentityResults(AIndex : Integer; AValue : TQueryResultBatchTypeentityResultsArray); 

begin
  If (FentityResults=AValue) then exit;
  FentityResults:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQueryResultBatch.SetmoreResults(AIndex : Integer; AValue : String); 

begin
  If (FmoreResults=AValue) then exit;
  FmoreResults:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQueryResultBatch.SetskippedResults(AIndex : Integer; AValue : integer); 

begin
  If (FskippedResults=AValue) then exit;
  FskippedResults:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReadOptions
  --------------------------------------------------------------------}


Procedure TReadOptions.SetreadConsistency(AIndex : Integer; AValue : String); 

begin
  If (FreadConsistency=AValue) then exit;
  FreadConsistency:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadOptions.Settransaction(AIndex : Integer; AValue : String); 

begin
  If (Ftransaction=AValue) then exit;
  Ftransaction:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TResponseHeader
  --------------------------------------------------------------------}


Procedure TResponseHeader.Setkind(AIndex : Integer; AValue : String); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRollbackRequest
  --------------------------------------------------------------------}


Procedure TRollbackRequest.Settransaction(AIndex : Integer; AValue : String); 

begin
  If (Ftransaction=AValue) then exit;
  Ftransaction:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRollbackResponse
  --------------------------------------------------------------------}


Procedure TRollbackResponse.Setheader(AIndex : Integer; AValue : TResponseHeader); 

begin
  If (Fheader=AValue) then exit;
  Fheader:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRunQueryRequest
  --------------------------------------------------------------------}


Procedure TRunQueryRequest.SetgqlQuery(AIndex : Integer; AValue : TGqlQuery); 

begin
  If (FgqlQuery=AValue) then exit;
  FgqlQuery:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRunQueryRequest.SetpartitionId(AIndex : Integer; AValue : TPartitionId); 

begin
  If (FpartitionId=AValue) then exit;
  FpartitionId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRunQueryRequest.Setquery(AIndex : Integer; AValue : TQuery); 

begin
  If (Fquery=AValue) then exit;
  Fquery:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRunQueryRequest.SetreadOptions(AIndex : Integer; AValue : TReadOptions); 

begin
  If (FreadOptions=AValue) then exit;
  FreadOptions:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRunQueryResponse
  --------------------------------------------------------------------}


Procedure TRunQueryResponse.Setbatch(AIndex : Integer; AValue : TQueryResultBatch); 

begin
  If (Fbatch=AValue) then exit;
  Fbatch:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRunQueryResponse.Setheader(AIndex : Integer; AValue : TResponseHeader); 

begin
  If (Fheader=AValue) then exit;
  Fheader:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TValue
  --------------------------------------------------------------------}


Procedure TValue.SetblobKeyValue(AIndex : Integer; AValue : String); 

begin
  If (FblobKeyValue=AValue) then exit;
  FblobKeyValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TValue.SetblobValue(AIndex : Integer; AValue : String); 

begin
  If (FblobValue=AValue) then exit;
  FblobValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TValue.SetbooleanValue(AIndex : Integer; AValue : boolean); 

begin
  If (FbooleanValue=AValue) then exit;
  FbooleanValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TValue.SetdateTimeValue(AIndex : Integer; AValue : TDatetime); 

begin
  If (FdateTimeValue=AValue) then exit;
  FdateTimeValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TValue.SetdoubleValue(AIndex : Integer; AValue : double); 

begin
  If (FdoubleValue=AValue) then exit;
  FdoubleValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TValue.SetentityValue(AIndex : Integer; AValue : TEntity); 

begin
  If (FentityValue=AValue) then exit;
  FentityValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TValue.Setindexed(AIndex : Integer; AValue : boolean); 

begin
  If (Findexed=AValue) then exit;
  Findexed:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TValue.SetintegerValue(AIndex : Integer; AValue : String); 

begin
  If (FintegerValue=AValue) then exit;
  FintegerValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TValue.SetkeyValue(AIndex : Integer; AValue : TKey); 

begin
  If (FkeyValue=AValue) then exit;
  FkeyValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TValue.SetlistValue(AIndex : Integer; AValue : TValueTypelistValueArray); 

begin
  If (FlistValue=AValue) then exit;
  FlistValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TValue.Setmeaning(AIndex : Integer; AValue : integer); 

begin
  If (Fmeaning=AValue) then exit;
  Fmeaning:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TValue.SetstringValue(AIndex : Integer; AValue : String); 

begin
  If (FstringValue=AValue) then exit;
  FstringValue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDatasetsResource
  --------------------------------------------------------------------}


Class Function TDatasetsResource.ResourceName : String;

begin
  Result:='datasets';
end;

Class Function TDatasetsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TdatastoreAPI;
end;

Function TDatasetsResource.AllocateIds(datasetId: string; aAllocateIdsRequest : TAllocateIdsRequest) : TAllocateIdsResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = '{datasetId}/allocateIds';
  _Methodid   = 'datastore.datasets.allocateIds';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['datasetId',datasetId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aAllocateIdsRequest,TAllocateIdsResponse) as TAllocateIdsResponse;
end;

Function TDatasetsResource.BeginTransaction(datasetId: string; aBeginTransactionRequest : TBeginTransactionRequest) : TBeginTransactionResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = '{datasetId}/beginTransaction';
  _Methodid   = 'datastore.datasets.beginTransaction';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['datasetId',datasetId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aBeginTransactionRequest,TBeginTransactionResponse) as TBeginTransactionResponse;
end;

Function TDatasetsResource.Commit(datasetId: string; aCommitRequest : TCommitRequest) : TCommitResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = '{datasetId}/commit';
  _Methodid   = 'datastore.datasets.commit';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['datasetId',datasetId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aCommitRequest,TCommitResponse) as TCommitResponse;
end;

Function TDatasetsResource.Lookup(datasetId: string; aLookupRequest : TLookupRequest) : TLookupResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = '{datasetId}/lookup';
  _Methodid   = 'datastore.datasets.lookup';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['datasetId',datasetId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aLookupRequest,TLookupResponse) as TLookupResponse;
end;

Function TDatasetsResource.Rollback(datasetId: string; aRollbackRequest : TRollbackRequest) : TRollbackResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = '{datasetId}/rollback';
  _Methodid   = 'datastore.datasets.rollback';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['datasetId',datasetId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aRollbackRequest,TRollbackResponse) as TRollbackResponse;
end;

Function TDatasetsResource.RunQuery(datasetId: string; aRunQueryRequest : TRunQueryRequest) : TRunQueryResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = '{datasetId}/runQuery';
  _Methodid   = 'datastore.datasets.runQuery';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['datasetId',datasetId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aRunQueryRequest,TRunQueryResponse) as TRunQueryResponse;
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
  Result:='v1beta2';
end;

Class Function TDatastoreAPI.APIRevision : String;

begin
  Result:='20150402';
end;

Class Function TDatastoreAPI.APIID : String;

begin
  Result:='datastore:v1beta2';
end;

Class Function TDatastoreAPI.APITitle : String;

begin
  Result:='Google Cloud Datastore API';
end;

Class Function TDatastoreAPI.APIDescription : String;

begin
  Result:='API for accessing Google Cloud Datastore.';
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
  Result:='https://developers.google.com/datastore/';
end;

Class Function TDatastoreAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com/';
end;

Class Function TDatastoreAPI.APIbasePath : string;

begin
  Result:='/datastore/v1beta2/datasets/';
end;

Class Function TDatastoreAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/datastore/v1beta2/datasets/';
end;

Class Function TDatastoreAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TDatastoreAPI.APIservicePath : string;

begin
  Result:='datastore/v1beta2/datasets/';
end;

Class Function TDatastoreAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TDatastoreAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,3);
  Result[0].Name:='https://www.googleapis.com/auth/cloud-platform';
  Result[0].Description:='View and manage your data across Google Cloud Platform services';
  Result[1].Name:='https://www.googleapis.com/auth/datastore';
  Result[1].Description:='View and manage your Google Cloud Datastore data';
  Result[2].Name:='https://www.googleapis.com/auth/userinfo.email';
  Result[2].Description:='View your email address';
  
end;

Class Function TDatastoreAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TDatastoreAPI.RegisterAPIResources;

begin
  TAllocateIdsRequest.RegisterObject;
  TAllocateIdsResponse.RegisterObject;
  TBeginTransactionRequest.RegisterObject;
  TBeginTransactionResponse.RegisterObject;
  TCommitRequest.RegisterObject;
  TCommitResponse.RegisterObject;
  TCompositeFilter.RegisterObject;
  TEntityTypeproperties.RegisterObject;
  TEntity.RegisterObject;
  TEntityResult.RegisterObject;
  TFilter.RegisterObject;
  TGqlQuery.RegisterObject;
  TGqlQueryArg.RegisterObject;
  TKey.RegisterObject;
  TKeyPathElement.RegisterObject;
  TKindExpression.RegisterObject;
  TLookupRequest.RegisterObject;
  TLookupResponse.RegisterObject;
  TMutation.RegisterObject;
  TMutationResult.RegisterObject;
  TPartitionId.RegisterObject;
  TProperty.RegisterObject;
  TPropertyExpression.RegisterObject;
  TPropertyFilter.RegisterObject;
  TPropertyOrder.RegisterObject;
  TPropertyReference.RegisterObject;
  TQuery.RegisterObject;
  TQueryResultBatch.RegisterObject;
  TReadOptions.RegisterObject;
  TResponseHeader.RegisterObject;
  TRollbackRequest.RegisterObject;
  TRollbackResponse.RegisterObject;
  TRunQueryRequest.RegisterObject;
  TRunQueryResponse.RegisterObject;
  TValue.RegisterObject;
end;


Function TDatastoreAPI.GetDatasetsInstance : TDatasetsResource;

begin
  if (FDatasetsInstance=Nil) then
    FDatasetsInstance:=CreateDatasetsResource;
  Result:=FDatasetsInstance;
end;

Function TDatastoreAPI.CreateDatasetsResource : TDatasetsResource;

begin
  Result:=CreateDatasetsResource(Self);
end;


Function TDatastoreAPI.CreateDatasetsResource(AOwner : TComponent) : TDatasetsResource;

begin
  Result:=TDatasetsResource.Create(AOwner);
  Result.API:=Self;
end;



initialization
  TDatastoreAPI.RegisterAPI;
end.
