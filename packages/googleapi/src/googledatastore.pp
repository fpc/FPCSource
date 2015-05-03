unit googledatastore;
{
  This is the file COPYING.FPC, it applies to the Free Pascal Run-Time Library 
  (RTL) and packages (packages) distributed by members of the Free Pascal 
  Development Team.
  
  The source code of the Free Pascal Runtime Libraries and packages are 
  distributed under the Library GNU General Public License 
  (see the file COPYING) with the following modification:
  
  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,
  and to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a module
  which is not derived from or based on this library. If you modify this
  library, you may extend this exception to your version of the library, but you are
  not obligated to do so. If you do not wish to do so, delete this exception
  statement from your version.
  
  If you didn't receive a copy of the file COPYING, contact:
        Free Software Foundation
        675 Mass Ave
        Cambridge, MA  02139
        USA
  
}
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  //
  TAllocateIdsRequest = class;
  TAllocateIdsRequestArray = Array of TAllocateIdsRequest;
  TAllocateIdsRequestkeys = class;
  TAllocateIdsRequestkeysArray = Array of TAllocateIdsRequestkeys;
  TAllocateIdsResponse = class;
  TAllocateIdsResponseArray = Array of TAllocateIdsResponse;
  TAllocateIdsResponsekeys = class;
  TAllocateIdsResponsekeysArray = Array of TAllocateIdsResponsekeys;
  TBeginTransactionRequest = class;
  TBeginTransactionRequestArray = Array of TBeginTransactionRequest;
  TBeginTransactionResponse = class;
  TBeginTransactionResponseArray = Array of TBeginTransactionResponse;
  TCommitRequest = class;
  TCommitRequestArray = Array of TCommitRequest;
  TCommitResponse = class;
  TCommitResponseArray = Array of TCommitResponse;
  TCompositeFilter = class;
  TCompositeFilterArray = Array of TCompositeFilter;
  TCompositeFilterfilters = class;
  TCompositeFilterfiltersArray = Array of TCompositeFilterfilters;
  TEntity = class;
  TEntityArray = Array of TEntity;
  TEntityproperties = class;
  TEntitypropertiesArray = Array of TEntityproperties;
  TEntityResult = class;
  TEntityResultArray = Array of TEntityResult;
  TFilter = class;
  TFilterArray = Array of TFilter;
  TGqlQuery = class;
  TGqlQueryArray = Array of TGqlQuery;
  TGqlQuerynameArgs = class;
  TGqlQuerynameArgsArray = Array of TGqlQuerynameArgs;
  TGqlQuerynumberArgs = class;
  TGqlQuerynumberArgsArray = Array of TGqlQuerynumberArgs;
  TGqlQueryArg = class;
  TGqlQueryArgArray = Array of TGqlQueryArg;
  TKey = class;
  TKeyArray = Array of TKey;
  TKeypath = class;
  TKeypathArray = Array of TKeypath;
  TKeyPathElement = class;
  TKeyPathElementArray = Array of TKeyPathElement;
  TKindExpression = class;
  TKindExpressionArray = Array of TKindExpression;
  TLookupRequest = class;
  TLookupRequestArray = Array of TLookupRequest;
  TLookupRequestkeys = class;
  TLookupRequestkeysArray = Array of TLookupRequestkeys;
  TLookupResponse = class;
  TLookupResponseArray = Array of TLookupResponse;
  TLookupResponsedeferred = class;
  TLookupResponsedeferredArray = Array of TLookupResponsedeferred;
  TLookupResponsefound = class;
  TLookupResponsefoundArray = Array of TLookupResponsefound;
  TLookupResponsemissing = class;
  TLookupResponsemissingArray = Array of TLookupResponsemissing;
  TMutation = class;
  TMutationArray = Array of TMutation;
  TMutationdelete = class;
  TMutationdeleteArray = Array of TMutationdelete;
  TMutationinsert = class;
  TMutationinsertArray = Array of TMutationinsert;
  TMutationinsertAutoId = class;
  TMutationinsertAutoIdArray = Array of TMutationinsertAutoId;
  TMutationupdate = class;
  TMutationupdateArray = Array of TMutationupdate;
  TMutationupsert = class;
  TMutationupsertArray = Array of TMutationupsert;
  TMutationResult = class;
  TMutationResultArray = Array of TMutationResult;
  TMutationResultinsertAutoIdKeys = class;
  TMutationResultinsertAutoIdKeysArray = Array of TMutationResultinsertAutoIdKeys;
  TPartitionId = class;
  TPartitionIdArray = Array of TPartitionId;
  TProperty = class;
  TPropertyArray = Array of TProperty;
  TPropertylistValue = class;
  TPropertylistValueArray = Array of TPropertylistValue;
  TPropertyExpression = class;
  TPropertyExpressionArray = Array of TPropertyExpression;
  TPropertyFilter = class;
  TPropertyFilterArray = Array of TPropertyFilter;
  TPropertyOrder = class;
  TPropertyOrderArray = Array of TPropertyOrder;
  TPropertyReference = class;
  TPropertyReferenceArray = Array of TPropertyReference;
  TQuery = class;
  TQueryArray = Array of TQuery;
  TQuerygroupBy = class;
  TQuerygroupByArray = Array of TQuerygroupBy;
  TQuerykinds = class;
  TQuerykindsArray = Array of TQuerykinds;
  TQueryorder = class;
  TQueryorderArray = Array of TQueryorder;
  TQueryprojection = class;
  TQueryprojectionArray = Array of TQueryprojection;
  TQueryResultBatch = class;
  TQueryResultBatchArray = Array of TQueryResultBatch;
  TQueryResultBatchentityResults = class;
  TQueryResultBatchentityResultsArray = Array of TQueryResultBatchentityResults;
  TReadOptions = class;
  TReadOptionsArray = Array of TReadOptions;
  TResponseHeader = class;
  TResponseHeaderArray = Array of TResponseHeader;
  TRollbackRequest = class;
  TRollbackRequestArray = Array of TRollbackRequest;
  TRollbackResponse = class;
  TRollbackResponseArray = Array of TRollbackResponse;
  TRunQueryRequest = class;
  TRunQueryRequestArray = Array of TRunQueryRequest;
  TRunQueryResponse = class;
  TRunQueryResponseArray = Array of TRunQueryResponse;
  TValue = class;
  TValueArray = Array of TValue;
  TValuelistValue = class;
  TValuelistValueArray = Array of TValuelistValue;
  
  { --------------------------------------------------------------------
    TAllocateIdsRequest
    --------------------------------------------------------------------}
  
  TAllocateIdsRequest = Class(TGoogleBaseObject)
  Private
    Fkeys : TAllocateIdsRequestkeys;
  Protected
    //Property setters
    Procedure Setkeys(AIndex : Integer; AValue : TAllocateIdsRequestkeys); virtual;
  Public
  Published
    Property keys : TAllocateIdsRequestkeys Index 0 Read Fkeys Write Setkeys;
  end;
  TAllocateIdsRequestClass = Class of TAllocateIdsRequest;
  
  { --------------------------------------------------------------------
    TAllocateIdsRequestkeys
    --------------------------------------------------------------------}
  
  TAllocateIdsRequestkeys = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAllocateIdsRequestkeysClass = Class of TAllocateIdsRequestkeys;
  
  { --------------------------------------------------------------------
    TAllocateIdsResponse
    --------------------------------------------------------------------}
  
  TAllocateIdsResponse = Class(TGoogleBaseObject)
  Private
    Fheader : TResponseHeader;
    Fkeys : TAllocateIdsResponsekeys;
  Protected
    //Property setters
    Procedure Setheader(AIndex : Integer; AValue : TResponseHeader); virtual;
    Procedure Setkeys(AIndex : Integer; AValue : TAllocateIdsResponsekeys); virtual;
  Public
  Published
    Property header : TResponseHeader Index 0 Read Fheader Write Setheader;
    Property keys : TAllocateIdsResponsekeys Index 8 Read Fkeys Write Setkeys;
  end;
  TAllocateIdsResponseClass = Class of TAllocateIdsResponse;
  
  { --------------------------------------------------------------------
    TAllocateIdsResponsekeys
    --------------------------------------------------------------------}
  
  TAllocateIdsResponsekeys = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAllocateIdsResponsekeysClass = Class of TAllocateIdsResponsekeys;
  
  { --------------------------------------------------------------------
    TBeginTransactionRequest
    --------------------------------------------------------------------}
  
  TBeginTransactionRequest = Class(TGoogleBaseObject)
  Private
    FisolationLevel : string;
  Protected
    //Property setters
    Procedure SetisolationLevel(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property isolationLevel : string Index 0 Read FisolationLevel Write SetisolationLevel;
  end;
  TBeginTransactionRequestClass = Class of TBeginTransactionRequest;
  
  { --------------------------------------------------------------------
    TBeginTransactionResponse
    --------------------------------------------------------------------}
  
  TBeginTransactionResponse = Class(TGoogleBaseObject)
  Private
    Fheader : TResponseHeader;
    Ftransaction : string;
  Protected
    //Property setters
    Procedure Setheader(AIndex : Integer; AValue : TResponseHeader); virtual;
    Procedure Settransaction(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property header : TResponseHeader Index 0 Read Fheader Write Setheader;
    Property transaction : string Index 8 Read Ftransaction Write Settransaction;
  end;
  TBeginTransactionResponseClass = Class of TBeginTransactionResponse;
  
  { --------------------------------------------------------------------
    TCommitRequest
    --------------------------------------------------------------------}
  
  TCommitRequest = Class(TGoogleBaseObject)
  Private
    FignoreReadOnly : boolean;
    Fmode : string;
    Fmutation : TMutation;
    Ftransaction : string;
  Protected
    //Property setters
    Procedure SetignoreReadOnly(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setmode(AIndex : Integer; AValue : string); virtual;
    Procedure Setmutation(AIndex : Integer; AValue : TMutation); virtual;
    Procedure Settransaction(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property ignoreReadOnly : boolean Index 0 Read FignoreReadOnly Write SetignoreReadOnly;
    Property mode : string Index 8 Read Fmode Write Setmode;
    Property mutation : TMutation Index 16 Read Fmutation Write Setmutation;
    Property transaction : string Index 24 Read Ftransaction Write Settransaction;
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
    Ffilters : TCompositeFilterfilters;
    F_operator : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setfilters(AIndex : Integer; AValue : TCompositeFilterfilters); virtual;
    Procedure Set_operator(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property filters : TCompositeFilterfilters Index 0 Read Ffilters Write Setfilters;
    Property _operator : string Index 8 Read F_operator Write Set_operator;
  end;
  TCompositeFilterClass = Class of TCompositeFilter;
  
  { --------------------------------------------------------------------
    TCompositeFilterfilters
    --------------------------------------------------------------------}
  
  TCompositeFilterfilters = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TCompositeFilterfiltersClass = Class of TCompositeFilterfilters;
  
  { --------------------------------------------------------------------
    TEntity
    --------------------------------------------------------------------}
  
  TEntity = Class(TGoogleBaseObject)
  Private
    Fkey : TKey;
    Fproperties : TEntityproperties;
  Protected
    //Property setters
    Procedure Setkey(AIndex : Integer; AValue : TKey); virtual;
    Procedure Setproperties(AIndex : Integer; AValue : TEntityproperties); virtual;
  Public
  Published
    Property key : TKey Index 0 Read Fkey Write Setkey;
    Property properties : TEntityproperties Index 8 Read Fproperties Write Setproperties;
  end;
  TEntityClass = Class of TEntity;
  
  { --------------------------------------------------------------------
    TEntityproperties
    --------------------------------------------------------------------}
  
  TEntityproperties = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TEntitypropertiesClass = Class of TEntityproperties;
  
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
    FnameArgs : TGqlQuerynameArgs;
    FnumberArgs : TGqlQuerynumberArgs;
    FqueryString : string;
  Protected
    //Property setters
    Procedure SetallowLiteral(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetnameArgs(AIndex : Integer; AValue : TGqlQuerynameArgs); virtual;
    Procedure SetnumberArgs(AIndex : Integer; AValue : TGqlQuerynumberArgs); virtual;
    Procedure SetqueryString(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property allowLiteral : boolean Index 0 Read FallowLiteral Write SetallowLiteral;
    Property nameArgs : TGqlQuerynameArgs Index 8 Read FnameArgs Write SetnameArgs;
    Property numberArgs : TGqlQuerynumberArgs Index 16 Read FnumberArgs Write SetnumberArgs;
    Property queryString : string Index 24 Read FqueryString Write SetqueryString;
  end;
  TGqlQueryClass = Class of TGqlQuery;
  
  { --------------------------------------------------------------------
    TGqlQuerynameArgs
    --------------------------------------------------------------------}
  
  TGqlQuerynameArgs = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TGqlQuerynameArgsClass = Class of TGqlQuerynameArgs;
  
  { --------------------------------------------------------------------
    TGqlQuerynumberArgs
    --------------------------------------------------------------------}
  
  TGqlQuerynumberArgs = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TGqlQuerynumberArgsClass = Class of TGqlQuerynumberArgs;
  
  { --------------------------------------------------------------------
    TGqlQueryArg
    --------------------------------------------------------------------}
  
  TGqlQueryArg = Class(TGoogleBaseObject)
  Private
    Fcursor : string;
    Fname : string;
    Fvalue : TValue;
  Protected
    //Property setters
    Procedure Setcursor(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : TValue); virtual;
  Public
  Published
    Property cursor : string Index 0 Read Fcursor Write Setcursor;
    Property name : string Index 8 Read Fname Write Setname;
    Property value : TValue Index 16 Read Fvalue Write Setvalue;
  end;
  TGqlQueryArgClass = Class of TGqlQueryArg;
  
  { --------------------------------------------------------------------
    TKey
    --------------------------------------------------------------------}
  
  TKey = Class(TGoogleBaseObject)
  Private
    FpartitionId : TPartitionId;
    Fpath : TKeypath;
  Protected
    //Property setters
    Procedure SetpartitionId(AIndex : Integer; AValue : TPartitionId); virtual;
    Procedure Setpath(AIndex : Integer; AValue : TKeypath); virtual;
  Public
  Published
    Property partitionId : TPartitionId Index 0 Read FpartitionId Write SetpartitionId;
    Property path : TKeypath Index 8 Read Fpath Write Setpath;
  end;
  TKeyClass = Class of TKey;
  
  { --------------------------------------------------------------------
    TKeypath
    --------------------------------------------------------------------}
  
  TKeypath = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TKeypathClass = Class of TKeypath;
  
  { --------------------------------------------------------------------
    TKeyPathElement
    --------------------------------------------------------------------}
  
  TKeyPathElement = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Fkind : string;
    Fname : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property kind : string Index 8 Read Fkind Write Setkind;
    Property name : string Index 16 Read Fname Write Setname;
  end;
  TKeyPathElementClass = Class of TKeyPathElement;
  
  { --------------------------------------------------------------------
    TKindExpression
    --------------------------------------------------------------------}
  
  TKindExpression = Class(TGoogleBaseObject)
  Private
    Fname : string;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property name : string Index 0 Read Fname Write Setname;
  end;
  TKindExpressionClass = Class of TKindExpression;
  
  { --------------------------------------------------------------------
    TLookupRequest
    --------------------------------------------------------------------}
  
  TLookupRequest = Class(TGoogleBaseObject)
  Private
    Fkeys : TLookupRequestkeys;
    FreadOptions : TReadOptions;
  Protected
    //Property setters
    Procedure Setkeys(AIndex : Integer; AValue : TLookupRequestkeys); virtual;
    Procedure SetreadOptions(AIndex : Integer; AValue : TReadOptions); virtual;
  Public
  Published
    Property keys : TLookupRequestkeys Index 0 Read Fkeys Write Setkeys;
    Property readOptions : TReadOptions Index 8 Read FreadOptions Write SetreadOptions;
  end;
  TLookupRequestClass = Class of TLookupRequest;
  
  { --------------------------------------------------------------------
    TLookupRequestkeys
    --------------------------------------------------------------------}
  
  TLookupRequestkeys = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TLookupRequestkeysClass = Class of TLookupRequestkeys;
  
  { --------------------------------------------------------------------
    TLookupResponse
    --------------------------------------------------------------------}
  
  TLookupResponse = Class(TGoogleBaseObject)
  Private
    Fdeferred : TLookupResponsedeferred;
    Ffound : TLookupResponsefound;
    Fheader : TResponseHeader;
    Fmissing : TLookupResponsemissing;
  Protected
    //Property setters
    Procedure Setdeferred(AIndex : Integer; AValue : TLookupResponsedeferred); virtual;
    Procedure Setfound(AIndex : Integer; AValue : TLookupResponsefound); virtual;
    Procedure Setheader(AIndex : Integer; AValue : TResponseHeader); virtual;
    Procedure Setmissing(AIndex : Integer; AValue : TLookupResponsemissing); virtual;
  Public
  Published
    Property deferred : TLookupResponsedeferred Index 0 Read Fdeferred Write Setdeferred;
    Property found : TLookupResponsefound Index 8 Read Ffound Write Setfound;
    Property header : TResponseHeader Index 16 Read Fheader Write Setheader;
    Property missing : TLookupResponsemissing Index 24 Read Fmissing Write Setmissing;
  end;
  TLookupResponseClass = Class of TLookupResponse;
  
  { --------------------------------------------------------------------
    TLookupResponsedeferred
    --------------------------------------------------------------------}
  
  TLookupResponsedeferred = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TLookupResponsedeferredClass = Class of TLookupResponsedeferred;
  
  { --------------------------------------------------------------------
    TLookupResponsefound
    --------------------------------------------------------------------}
  
  TLookupResponsefound = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TLookupResponsefoundClass = Class of TLookupResponsefound;
  
  { --------------------------------------------------------------------
    TLookupResponsemissing
    --------------------------------------------------------------------}
  
  TLookupResponsemissing = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TLookupResponsemissingClass = Class of TLookupResponsemissing;
  
  { --------------------------------------------------------------------
    TMutation
    --------------------------------------------------------------------}
  
  TMutation = Class(TGoogleBaseObject)
  Private
    Fdelete : TMutationdelete;
    Fforce : boolean;
    Finsert : TMutationinsert;
    FinsertAutoId : TMutationinsertAutoId;
    Fupdate : TMutationupdate;
    Fupsert : TMutationupsert;
  Protected
    //Property setters
    Procedure Setdelete(AIndex : Integer; AValue : TMutationdelete); virtual;
    Procedure Setforce(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setinsert(AIndex : Integer; AValue : TMutationinsert); virtual;
    Procedure SetinsertAutoId(AIndex : Integer; AValue : TMutationinsertAutoId); virtual;
    Procedure Setupdate(AIndex : Integer; AValue : TMutationupdate); virtual;
    Procedure Setupsert(AIndex : Integer; AValue : TMutationupsert); virtual;
  Public
  Published
    Property delete : TMutationdelete Index 0 Read Fdelete Write Setdelete;
    Property force : boolean Index 8 Read Fforce Write Setforce;
    Property insert : TMutationinsert Index 16 Read Finsert Write Setinsert;
    Property insertAutoId : TMutationinsertAutoId Index 24 Read FinsertAutoId Write SetinsertAutoId;
    Property update : TMutationupdate Index 32 Read Fupdate Write Setupdate;
    Property upsert : TMutationupsert Index 40 Read Fupsert Write Setupsert;
  end;
  TMutationClass = Class of TMutation;
  
  { --------------------------------------------------------------------
    TMutationdelete
    --------------------------------------------------------------------}
  
  TMutationdelete = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TMutationdeleteClass = Class of TMutationdelete;
  
  { --------------------------------------------------------------------
    TMutationinsert
    --------------------------------------------------------------------}
  
  TMutationinsert = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TMutationinsertClass = Class of TMutationinsert;
  
  { --------------------------------------------------------------------
    TMutationinsertAutoId
    --------------------------------------------------------------------}
  
  TMutationinsertAutoId = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TMutationinsertAutoIdClass = Class of TMutationinsertAutoId;
  
  { --------------------------------------------------------------------
    TMutationupdate
    --------------------------------------------------------------------}
  
  TMutationupdate = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TMutationupdateClass = Class of TMutationupdate;
  
  { --------------------------------------------------------------------
    TMutationupsert
    --------------------------------------------------------------------}
  
  TMutationupsert = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TMutationupsertClass = Class of TMutationupsert;
  
  { --------------------------------------------------------------------
    TMutationResult
    --------------------------------------------------------------------}
  
  TMutationResult = Class(TGoogleBaseObject)
  Private
    FindexUpdates : integer;
    FinsertAutoIdKeys : TMutationResultinsertAutoIdKeys;
  Protected
    //Property setters
    Procedure SetindexUpdates(AIndex : Integer; AValue : integer); virtual;
    Procedure SetinsertAutoIdKeys(AIndex : Integer; AValue : TMutationResultinsertAutoIdKeys); virtual;
  Public
  Published
    Property indexUpdates : integer Index 0 Read FindexUpdates Write SetindexUpdates;
    Property insertAutoIdKeys : TMutationResultinsertAutoIdKeys Index 8 Read FinsertAutoIdKeys Write SetinsertAutoIdKeys;
  end;
  TMutationResultClass = Class of TMutationResult;
  
  { --------------------------------------------------------------------
    TMutationResultinsertAutoIdKeys
    --------------------------------------------------------------------}
  
  TMutationResultinsertAutoIdKeys = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TMutationResultinsertAutoIdKeysClass = Class of TMutationResultinsertAutoIdKeys;
  
  { --------------------------------------------------------------------
    TPartitionId
    --------------------------------------------------------------------}
  
  TPartitionId = Class(TGoogleBaseObject)
  Private
    FdatasetId : string;
    Fnamespace : string;
  Protected
    //Property setters
    Procedure SetdatasetId(AIndex : Integer; AValue : string); virtual;
    Procedure Setnamespace(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property datasetId : string Index 0 Read FdatasetId Write SetdatasetId;
    Property namespace : string Index 8 Read Fnamespace Write Setnamespace;
  end;
  TPartitionIdClass = Class of TPartitionId;
  
  { --------------------------------------------------------------------
    TProperty
    --------------------------------------------------------------------}
  
  TProperty = Class(TGoogleBaseObject)
  Private
    FblobKeyValue : string;
    FblobValue : string;
    FbooleanValue : boolean;
    FdateTimeValue : TDatetime;
    FdoubleValue : double;
    FentityValue : TEntity;
    Findexed : boolean;
    FintegerValue : string;
    FkeyValue : TKey;
    FlistValue : TPropertylistValue;
    Fmeaning : integer;
    FstringValue : string;
  Protected
    //Property setters
    Procedure SetblobKeyValue(AIndex : Integer; AValue : string); virtual;
    Procedure SetblobValue(AIndex : Integer; AValue : string); virtual;
    Procedure SetbooleanValue(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetdateTimeValue(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetdoubleValue(AIndex : Integer; AValue : double); virtual;
    Procedure SetentityValue(AIndex : Integer; AValue : TEntity); virtual;
    Procedure Setindexed(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetintegerValue(AIndex : Integer; AValue : string); virtual;
    Procedure SetkeyValue(AIndex : Integer; AValue : TKey); virtual;
    Procedure SetlistValue(AIndex : Integer; AValue : TPropertylistValue); virtual;
    Procedure Setmeaning(AIndex : Integer; AValue : integer); virtual;
    Procedure SetstringValue(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property blobKeyValue : string Index 0 Read FblobKeyValue Write SetblobKeyValue;
    Property blobValue : string Index 8 Read FblobValue Write SetblobValue;
    Property booleanValue : boolean Index 16 Read FbooleanValue Write SetbooleanValue;
    Property dateTimeValue : TDatetime Index 24 Read FdateTimeValue Write SetdateTimeValue;
    Property doubleValue : double Index 32 Read FdoubleValue Write SetdoubleValue;
    Property entityValue : TEntity Index 40 Read FentityValue Write SetentityValue;
    Property indexed : boolean Index 48 Read Findexed Write Setindexed;
    Property integerValue : string Index 56 Read FintegerValue Write SetintegerValue;
    Property keyValue : TKey Index 64 Read FkeyValue Write SetkeyValue;
    Property listValue : TPropertylistValue Index 72 Read FlistValue Write SetlistValue;
    Property meaning : integer Index 80 Read Fmeaning Write Setmeaning;
    Property stringValue : string Index 88 Read FstringValue Write SetstringValue;
  end;
  TPropertyClass = Class of TProperty;
  
  { --------------------------------------------------------------------
    TPropertylistValue
    --------------------------------------------------------------------}
  
  TPropertylistValue = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TPropertylistValueClass = Class of TPropertylistValue;
  
  { --------------------------------------------------------------------
    TPropertyExpression
    --------------------------------------------------------------------}
  
  TPropertyExpression = Class(TGoogleBaseObject)
  Private
    FaggregationFunction : string;
    F_property : TPropertyReference;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetaggregationFunction(AIndex : Integer; AValue : string); virtual;
    Procedure Set_property(AIndex : Integer; AValue : TPropertyReference); virtual;
  Public
  Published
    Property aggregationFunction : string Index 0 Read FaggregationFunction Write SetaggregationFunction;
    Property _property : TPropertyReference Index 8 Read F_property Write Set_property;
  end;
  TPropertyExpressionClass = Class of TPropertyExpression;
  
  { --------------------------------------------------------------------
    TPropertyFilter
    --------------------------------------------------------------------}
  
  TPropertyFilter = Class(TGoogleBaseObject)
  Private
    F_operator : string;
    F_property : TPropertyReference;
    Fvalue : TValue;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_operator(AIndex : Integer; AValue : string); virtual;
    Procedure Set_property(AIndex : Integer; AValue : TPropertyReference); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : TValue); virtual;
  Public
  Published
    Property _operator : string Index 0 Read F_operator Write Set_operator;
    Property _property : TPropertyReference Index 8 Read F_property Write Set_property;
    Property value : TValue Index 16 Read Fvalue Write Setvalue;
  end;
  TPropertyFilterClass = Class of TPropertyFilter;
  
  { --------------------------------------------------------------------
    TPropertyOrder
    --------------------------------------------------------------------}
  
  TPropertyOrder = Class(TGoogleBaseObject)
  Private
    Fdirection : string;
    F_property : TPropertyReference;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setdirection(AIndex : Integer; AValue : string); virtual;
    Procedure Set_property(AIndex : Integer; AValue : TPropertyReference); virtual;
  Public
  Published
    Property direction : string Index 0 Read Fdirection Write Setdirection;
    Property _property : TPropertyReference Index 8 Read F_property Write Set_property;
  end;
  TPropertyOrderClass = Class of TPropertyOrder;
  
  { --------------------------------------------------------------------
    TPropertyReference
    --------------------------------------------------------------------}
  
  TPropertyReference = Class(TGoogleBaseObject)
  Private
    Fname : string;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property name : string Index 0 Read Fname Write Setname;
  end;
  TPropertyReferenceClass = Class of TPropertyReference;
  
  { --------------------------------------------------------------------
    TQuery
    --------------------------------------------------------------------}
  
  TQuery = Class(TGoogleBaseObject)
  Private
    FendCursor : string;
    Ffilter : TFilter;
    FgroupBy : TQuerygroupBy;
    Fkinds : TQuerykinds;
    Flimit : integer;
    Foffset : integer;
    Forder : TQueryorder;
    Fprojection : TQueryprojection;
    FstartCursor : string;
  Protected
    //Property setters
    Procedure SetendCursor(AIndex : Integer; AValue : string); virtual;
    Procedure Setfilter(AIndex : Integer; AValue : TFilter); virtual;
    Procedure SetgroupBy(AIndex : Integer; AValue : TQuerygroupBy); virtual;
    Procedure Setkinds(AIndex : Integer; AValue : TQuerykinds); virtual;
    Procedure Setlimit(AIndex : Integer; AValue : integer); virtual;
    Procedure Setoffset(AIndex : Integer; AValue : integer); virtual;
    Procedure Setorder(AIndex : Integer; AValue : TQueryorder); virtual;
    Procedure Setprojection(AIndex : Integer; AValue : TQueryprojection); virtual;
    Procedure SetstartCursor(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property endCursor : string Index 0 Read FendCursor Write SetendCursor;
    Property filter : TFilter Index 8 Read Ffilter Write Setfilter;
    Property groupBy : TQuerygroupBy Index 16 Read FgroupBy Write SetgroupBy;
    Property kinds : TQuerykinds Index 24 Read Fkinds Write Setkinds;
    Property limit : integer Index 32 Read Flimit Write Setlimit;
    Property offset : integer Index 40 Read Foffset Write Setoffset;
    Property order : TQueryorder Index 48 Read Forder Write Setorder;
    Property projection : TQueryprojection Index 56 Read Fprojection Write Setprojection;
    Property startCursor : string Index 64 Read FstartCursor Write SetstartCursor;
  end;
  TQueryClass = Class of TQuery;
  
  { --------------------------------------------------------------------
    TQuerygroupBy
    --------------------------------------------------------------------}
  
  TQuerygroupBy = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TQuerygroupByClass = Class of TQuerygroupBy;
  
  { --------------------------------------------------------------------
    TQuerykinds
    --------------------------------------------------------------------}
  
  TQuerykinds = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TQuerykindsClass = Class of TQuerykinds;
  
  { --------------------------------------------------------------------
    TQueryorder
    --------------------------------------------------------------------}
  
  TQueryorder = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TQueryorderClass = Class of TQueryorder;
  
  { --------------------------------------------------------------------
    TQueryprojection
    --------------------------------------------------------------------}
  
  TQueryprojection = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TQueryprojectionClass = Class of TQueryprojection;
  
  { --------------------------------------------------------------------
    TQueryResultBatch
    --------------------------------------------------------------------}
  
  TQueryResultBatch = Class(TGoogleBaseObject)
  Private
    FendCursor : string;
    FentityResultType : string;
    FentityResults : TQueryResultBatchentityResults;
    FmoreResults : string;
    FskippedResults : integer;
  Protected
    //Property setters
    Procedure SetendCursor(AIndex : Integer; AValue : string); virtual;
    Procedure SetentityResultType(AIndex : Integer; AValue : string); virtual;
    Procedure SetentityResults(AIndex : Integer; AValue : TQueryResultBatchentityResults); virtual;
    Procedure SetmoreResults(AIndex : Integer; AValue : string); virtual;
    Procedure SetskippedResults(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property endCursor : string Index 0 Read FendCursor Write SetendCursor;
    Property entityResultType : string Index 8 Read FentityResultType Write SetentityResultType;
    Property entityResults : TQueryResultBatchentityResults Index 16 Read FentityResults Write SetentityResults;
    Property moreResults : string Index 24 Read FmoreResults Write SetmoreResults;
    Property skippedResults : integer Index 32 Read FskippedResults Write SetskippedResults;
  end;
  TQueryResultBatchClass = Class of TQueryResultBatch;
  
  { --------------------------------------------------------------------
    TQueryResultBatchentityResults
    --------------------------------------------------------------------}
  
  TQueryResultBatchentityResults = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TQueryResultBatchentityResultsClass = Class of TQueryResultBatchentityResults;
  
  { --------------------------------------------------------------------
    TReadOptions
    --------------------------------------------------------------------}
  
  TReadOptions = Class(TGoogleBaseObject)
  Private
    FreadConsistency : string;
    Ftransaction : string;
  Protected
    //Property setters
    Procedure SetreadConsistency(AIndex : Integer; AValue : string); virtual;
    Procedure Settransaction(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property readConsistency : string Index 0 Read FreadConsistency Write SetreadConsistency;
    Property transaction : string Index 8 Read Ftransaction Write Settransaction;
  end;
  TReadOptionsClass = Class of TReadOptions;
  
  { --------------------------------------------------------------------
    TResponseHeader
    --------------------------------------------------------------------}
  
  TResponseHeader = Class(TGoogleBaseObject)
  Private
    Fkind : string;
  Protected
    //Property setters
    Procedure Setkind(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property kind : string Index 0 Read Fkind Write Setkind;
  end;
  TResponseHeaderClass = Class of TResponseHeader;
  
  { --------------------------------------------------------------------
    TRollbackRequest
    --------------------------------------------------------------------}
  
  TRollbackRequest = Class(TGoogleBaseObject)
  Private
    Ftransaction : string;
  Protected
    //Property setters
    Procedure Settransaction(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property transaction : string Index 0 Read Ftransaction Write Settransaction;
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
    FblobKeyValue : string;
    FblobValue : string;
    FbooleanValue : boolean;
    FdateTimeValue : TDatetime;
    FdoubleValue : double;
    FentityValue : TEntity;
    Findexed : boolean;
    FintegerValue : string;
    FkeyValue : TKey;
    FlistValue : TValuelistValue;
    Fmeaning : integer;
    FstringValue : string;
  Protected
    //Property setters
    Procedure SetblobKeyValue(AIndex : Integer; AValue : string); virtual;
    Procedure SetblobValue(AIndex : Integer; AValue : string); virtual;
    Procedure SetbooleanValue(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetdateTimeValue(AIndex : Integer; AValue : TDatetime); virtual;
    Procedure SetdoubleValue(AIndex : Integer; AValue : double); virtual;
    Procedure SetentityValue(AIndex : Integer; AValue : TEntity); virtual;
    Procedure Setindexed(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetintegerValue(AIndex : Integer; AValue : string); virtual;
    Procedure SetkeyValue(AIndex : Integer; AValue : TKey); virtual;
    Procedure SetlistValue(AIndex : Integer; AValue : TValuelistValue); virtual;
    Procedure Setmeaning(AIndex : Integer; AValue : integer); virtual;
    Procedure SetstringValue(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property blobKeyValue : string Index 0 Read FblobKeyValue Write SetblobKeyValue;
    Property blobValue : string Index 8 Read FblobValue Write SetblobValue;
    Property booleanValue : boolean Index 16 Read FbooleanValue Write SetbooleanValue;
    Property dateTimeValue : TDatetime Index 24 Read FdateTimeValue Write SetdateTimeValue;
    Property doubleValue : double Index 32 Read FdoubleValue Write SetdoubleValue;
    Property entityValue : TEntity Index 40 Read FentityValue Write SetentityValue;
    Property indexed : boolean Index 48 Read Findexed Write Setindexed;
    Property integerValue : string Index 56 Read FintegerValue Write SetintegerValue;
    Property keyValue : TKey Index 64 Read FkeyValue Write SetkeyValue;
    Property listValue : TValuelistValue Index 72 Read FlistValue Write SetlistValue;
    Property meaning : integer Index 80 Read Fmeaning Write Setmeaning;
    Property stringValue : string Index 88 Read FstringValue Write SetstringValue;
  end;
  TValueClass = Class of TValue;
  
  { --------------------------------------------------------------------
    TValuelistValue
    --------------------------------------------------------------------}
  
  TValuelistValue = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TValuelistValueClass = Class of TValuelistValue;
  
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


Procedure TAllocateIdsRequest.Setkeys(AIndex : Integer; AValue : TAllocateIdsRequestkeys); 

begin
  If (Fkeys=AValue) then exit;
  Fkeys:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAllocateIdsRequestkeys
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAllocateIdsResponse
  --------------------------------------------------------------------}


Procedure TAllocateIdsResponse.Setheader(AIndex : Integer; AValue : TResponseHeader); 

begin
  If (Fheader=AValue) then exit;
  Fheader:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAllocateIdsResponse.Setkeys(AIndex : Integer; AValue : TAllocateIdsResponsekeys); 

begin
  If (Fkeys=AValue) then exit;
  Fkeys:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAllocateIdsResponsekeys
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TBeginTransactionRequest
  --------------------------------------------------------------------}


Procedure TBeginTransactionRequest.SetisolationLevel(AIndex : Integer; AValue : string); 

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



Procedure TBeginTransactionResponse.Settransaction(AIndex : Integer; AValue : string); 

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



Procedure TCommitRequest.Setmode(AIndex : Integer; AValue : string); 

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



Procedure TCommitRequest.Settransaction(AIndex : Integer; AValue : string); 

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


Procedure TCompositeFilter.Setfilters(AIndex : Integer; AValue : TCompositeFilterfilters); 

begin
  If (Ffilters=AValue) then exit;
  Ffilters:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCompositeFilter.Set_operator(AIndex : Integer; AValue : string); 

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
  TCompositeFilterfilters
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TEntity
  --------------------------------------------------------------------}


Procedure TEntity.Setkey(AIndex : Integer; AValue : TKey); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntity.Setproperties(AIndex : Integer; AValue : TEntityproperties); 

begin
  If (Fproperties=AValue) then exit;
  Fproperties:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TEntityproperties
  --------------------------------------------------------------------}


Class Function TEntityproperties.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
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



Procedure TGqlQuery.SetnameArgs(AIndex : Integer; AValue : TGqlQuerynameArgs); 

begin
  If (FnameArgs=AValue) then exit;
  FnameArgs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGqlQuery.SetnumberArgs(AIndex : Integer; AValue : TGqlQuerynumberArgs); 

begin
  If (FnumberArgs=AValue) then exit;
  FnumberArgs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGqlQuery.SetqueryString(AIndex : Integer; AValue : string); 

begin
  If (FqueryString=AValue) then exit;
  FqueryString:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TGqlQuerynameArgs
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TGqlQuerynumberArgs
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TGqlQueryArg
  --------------------------------------------------------------------}


Procedure TGqlQueryArg.Setcursor(AIndex : Integer; AValue : string); 

begin
  If (Fcursor=AValue) then exit;
  Fcursor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TGqlQueryArg.Setname(AIndex : Integer; AValue : string); 

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



Procedure TKey.Setpath(AIndex : Integer; AValue : TKeypath); 

begin
  If (Fpath=AValue) then exit;
  Fpath:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TKeypath
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TKeyPathElement
  --------------------------------------------------------------------}


Procedure TKeyPathElement.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TKeyPathElement.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TKeyPathElement.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TKindExpression
  --------------------------------------------------------------------}


Procedure TKindExpression.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLookupRequest
  --------------------------------------------------------------------}


Procedure TLookupRequest.Setkeys(AIndex : Integer; AValue : TLookupRequestkeys); 

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
  TLookupRequestkeys
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TLookupResponse
  --------------------------------------------------------------------}


Procedure TLookupResponse.Setdeferred(AIndex : Integer; AValue : TLookupResponsedeferred); 

begin
  If (Fdeferred=AValue) then exit;
  Fdeferred:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLookupResponse.Setfound(AIndex : Integer; AValue : TLookupResponsefound); 

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



Procedure TLookupResponse.Setmissing(AIndex : Integer; AValue : TLookupResponsemissing); 

begin
  If (Fmissing=AValue) then exit;
  Fmissing:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TLookupResponsedeferred
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TLookupResponsefound
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TLookupResponsemissing
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TMutation
  --------------------------------------------------------------------}


Procedure TMutation.Setdelete(AIndex : Integer; AValue : TMutationdelete); 

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



Procedure TMutation.Setinsert(AIndex : Integer; AValue : TMutationinsert); 

begin
  If (Finsert=AValue) then exit;
  Finsert:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMutation.SetinsertAutoId(AIndex : Integer; AValue : TMutationinsertAutoId); 

begin
  If (FinsertAutoId=AValue) then exit;
  FinsertAutoId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMutation.Setupdate(AIndex : Integer; AValue : TMutationupdate); 

begin
  If (Fupdate=AValue) then exit;
  Fupdate:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMutation.Setupsert(AIndex : Integer; AValue : TMutationupsert); 

begin
  If (Fupsert=AValue) then exit;
  Fupsert:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TMutationdelete
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TMutationinsert
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TMutationinsertAutoId
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TMutationupdate
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TMutationupsert
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TMutationResult
  --------------------------------------------------------------------}


Procedure TMutationResult.SetindexUpdates(AIndex : Integer; AValue : integer); 

begin
  If (FindexUpdates=AValue) then exit;
  FindexUpdates:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMutationResult.SetinsertAutoIdKeys(AIndex : Integer; AValue : TMutationResultinsertAutoIdKeys); 

begin
  If (FinsertAutoIdKeys=AValue) then exit;
  FinsertAutoIdKeys:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TMutationResultinsertAutoIdKeys
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TPartitionId
  --------------------------------------------------------------------}


Procedure TPartitionId.SetdatasetId(AIndex : Integer; AValue : string); 

begin
  If (FdatasetId=AValue) then exit;
  FdatasetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPartitionId.Setnamespace(AIndex : Integer; AValue : string); 

begin
  If (Fnamespace=AValue) then exit;
  Fnamespace:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProperty
  --------------------------------------------------------------------}


Procedure TProperty.SetblobKeyValue(AIndex : Integer; AValue : string); 

begin
  If (FblobKeyValue=AValue) then exit;
  FblobKeyValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProperty.SetblobValue(AIndex : Integer; AValue : string); 

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



Procedure TProperty.SetintegerValue(AIndex : Integer; AValue : string); 

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



Procedure TProperty.SetlistValue(AIndex : Integer; AValue : TPropertylistValue); 

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



Procedure TProperty.SetstringValue(AIndex : Integer; AValue : string); 

begin
  If (FstringValue=AValue) then exit;
  FstringValue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPropertylistValue
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TPropertyExpression
  --------------------------------------------------------------------}


Procedure TPropertyExpression.SetaggregationFunction(AIndex : Integer; AValue : string); 

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


Procedure TPropertyFilter.Set_operator(AIndex : Integer; AValue : string); 

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


Procedure TPropertyOrder.Setdirection(AIndex : Integer; AValue : string); 

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


Procedure TPropertyReference.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TQuery
  --------------------------------------------------------------------}


Procedure TQuery.SetendCursor(AIndex : Integer; AValue : string); 

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



Procedure TQuery.SetgroupBy(AIndex : Integer; AValue : TQuerygroupBy); 

begin
  If (FgroupBy=AValue) then exit;
  FgroupBy:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.Setkinds(AIndex : Integer; AValue : TQuerykinds); 

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



Procedure TQuery.Setorder(AIndex : Integer; AValue : TQueryorder); 

begin
  If (Forder=AValue) then exit;
  Forder:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.Setprojection(AIndex : Integer; AValue : TQueryprojection); 

begin
  If (Fprojection=AValue) then exit;
  Fprojection:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQuery.SetstartCursor(AIndex : Integer; AValue : string); 

begin
  If (FstartCursor=AValue) then exit;
  FstartCursor:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TQuerygroupBy
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TQuerykinds
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TQueryorder
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TQueryprojection
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TQueryResultBatch
  --------------------------------------------------------------------}


Procedure TQueryResultBatch.SetendCursor(AIndex : Integer; AValue : string); 

begin
  If (FendCursor=AValue) then exit;
  FendCursor:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQueryResultBatch.SetentityResultType(AIndex : Integer; AValue : string); 

begin
  If (FentityResultType=AValue) then exit;
  FentityResultType:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQueryResultBatch.SetentityResults(AIndex : Integer; AValue : TQueryResultBatchentityResults); 

begin
  If (FentityResults=AValue) then exit;
  FentityResults:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQueryResultBatch.SetmoreResults(AIndex : Integer; AValue : string); 

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
  TQueryResultBatchentityResults
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TReadOptions
  --------------------------------------------------------------------}


Procedure TReadOptions.SetreadConsistency(AIndex : Integer; AValue : string); 

begin
  If (FreadConsistency=AValue) then exit;
  FreadConsistency:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadOptions.Settransaction(AIndex : Integer; AValue : string); 

begin
  If (Ftransaction=AValue) then exit;
  Ftransaction:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TResponseHeader
  --------------------------------------------------------------------}


Procedure TResponseHeader.Setkind(AIndex : Integer; AValue : string); 

begin
  If (Fkind=AValue) then exit;
  Fkind:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRollbackRequest
  --------------------------------------------------------------------}


Procedure TRollbackRequest.Settransaction(AIndex : Integer; AValue : string); 

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


Procedure TValue.SetblobKeyValue(AIndex : Integer; AValue : string); 

begin
  If (FblobKeyValue=AValue) then exit;
  FblobKeyValue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TValue.SetblobValue(AIndex : Integer; AValue : string); 

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



Procedure TValue.SetintegerValue(AIndex : Integer; AValue : string); 

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



Procedure TValue.SetlistValue(AIndex : Integer; AValue : TValuelistValue); 

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



Procedure TValue.SetstringValue(AIndex : Integer; AValue : string); 

begin
  If (FstringValue=AValue) then exit;
  FstringValue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TValuelistValue
  --------------------------------------------------------------------}




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
  TAllocateIdsRequestkeys.RegisterObject;
  TAllocateIdsResponse.RegisterObject;
  TAllocateIdsResponsekeys.RegisterObject;
  TBeginTransactionRequest.RegisterObject;
  TBeginTransactionResponse.RegisterObject;
  TCommitRequest.RegisterObject;
  TCommitResponse.RegisterObject;
  TCompositeFilter.RegisterObject;
  TCompositeFilterfilters.RegisterObject;
  TEntity.RegisterObject;
  TEntityproperties.RegisterObject;
  TEntityResult.RegisterObject;
  TFilter.RegisterObject;
  TGqlQuery.RegisterObject;
  TGqlQuerynameArgs.RegisterObject;
  TGqlQuerynumberArgs.RegisterObject;
  TGqlQueryArg.RegisterObject;
  TKey.RegisterObject;
  TKeypath.RegisterObject;
  TKeyPathElement.RegisterObject;
  TKindExpression.RegisterObject;
  TLookupRequest.RegisterObject;
  TLookupRequestkeys.RegisterObject;
  TLookupResponse.RegisterObject;
  TLookupResponsedeferred.RegisterObject;
  TLookupResponsefound.RegisterObject;
  TLookupResponsemissing.RegisterObject;
  TMutation.RegisterObject;
  TMutationdelete.RegisterObject;
  TMutationinsert.RegisterObject;
  TMutationinsertAutoId.RegisterObject;
  TMutationupdate.RegisterObject;
  TMutationupsert.RegisterObject;
  TMutationResult.RegisterObject;
  TMutationResultinsertAutoIdKeys.RegisterObject;
  TPartitionId.RegisterObject;
  TProperty.RegisterObject;
  TPropertylistValue.RegisterObject;
  TPropertyExpression.RegisterObject;
  TPropertyFilter.RegisterObject;
  TPropertyOrder.RegisterObject;
  TPropertyReference.RegisterObject;
  TQuery.RegisterObject;
  TQuerygroupBy.RegisterObject;
  TQuerykinds.RegisterObject;
  TQueryorder.RegisterObject;
  TQueryprojection.RegisterObject;
  TQueryResultBatch.RegisterObject;
  TQueryResultBatchentityResults.RegisterObject;
  TReadOptions.RegisterObject;
  TResponseHeader.RegisterObject;
  TRollbackRequest.RegisterObject;
  TRollbackResponse.RegisterObject;
  TRunQueryRequest.RegisterObject;
  TRunQueryResponse.RegisterObject;
  TValue.RegisterObject;
  TValuelistValue.RegisterObject;
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
