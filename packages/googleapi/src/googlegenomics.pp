unit googlegenomics;
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TAnnotationSet = Class;
  TEmpty = Class;
  TSearchAnnotationSetsRequest = Class;
  TSearchAnnotationSetsResponse = Class;
  TAnnotation = Class;
  TVariantAnnotation = Class;
  TClinicalCondition = Class;
  TExternalId = Class;
  TTranscript = Class;
  TExon = Class;
  TCodingSequence = Class;
  TBatchCreateAnnotationsRequest = Class;
  TBatchCreateAnnotationsResponse = Class;
  TEntry = Class;
  TStatus = Class;
  TSearchAnnotationsRequest = Class;
  TSearchAnnotationsResponse = Class;
  TListDatasetsResponse = Class;
  TDataset = Class;
  TUndeleteDatasetRequest = Class;
  TSetIamPolicyRequest = Class;
  TPolicy = Class;
  TBinding = Class;
  TGetIamPolicyRequest = Class;
  TTestIamPermissionsRequest = Class;
  TTestIamPermissionsResponse = Class;
  TOperation = Class;
  TListOperationsResponse = Class;
  TCancelOperationRequest = Class;
  TImportReadGroupSetsRequest = Class;
  TExportReadGroupSetRequest = Class;
  TSearchReadGroupSetsRequest = Class;
  TSearchReadGroupSetsResponse = Class;
  TReadGroupSet = Class;
  TReadGroup = Class;
  TExperiment = Class;
  TProgram = Class;
  TListCoverageBucketsResponse = Class;
  TCoverageBucket = Class;
  TRange = Class;
  TSearchReadsRequest = Class;
  TSearchReadsResponse = Class;
  TRead = Class;
  TLinearAlignment = Class;
  TPosition = Class;
  TCigarUnit = Class;
  TStreamReadsRequest = Class;
  TStreamReadsResponse = Class;
  TSearchReferenceSetsRequest = Class;
  TSearchReferenceSetsResponse = Class;
  TReferenceSet = Class;
  TSearchReferencesRequest = Class;
  TSearchReferencesResponse = Class;
  TReference = Class;
  TListBasesResponse = Class;
  TImportVariantsRequest = Class;
  TVariantSet = Class;
  TReferenceBound = Class;
  TVariantSetMetadata = Class;
  TExportVariantSetRequest = Class;
  TSearchVariantSetsRequest = Class;
  TSearchVariantSetsResponse = Class;
  TSearchVariantsRequest = Class;
  TSearchVariantsResponse = Class;
  TVariant = Class;
  TVariantCall = Class;
  TMergeVariantsRequest = Class;
  TSearchCallSetsRequest = Class;
  TSearchCallSetsResponse = Class;
  TCallSet = Class;
  TStreamVariantsRequest = Class;
  TStreamVariantsResponse = Class;
  TImportReadGroupSetsResponse = Class;
  TImportVariantsResponse = Class;
  TOperationMetadata = Class;
  TOperationEvent = Class;
  TAnnotationSetArray = Array of TAnnotationSet;
  TEmptyArray = Array of TEmpty;
  TSearchAnnotationSetsRequestArray = Array of TSearchAnnotationSetsRequest;
  TSearchAnnotationSetsResponseArray = Array of TSearchAnnotationSetsResponse;
  TAnnotationArray = Array of TAnnotation;
  TVariantAnnotationArray = Array of TVariantAnnotation;
  TClinicalConditionArray = Array of TClinicalCondition;
  TExternalIdArray = Array of TExternalId;
  TTranscriptArray = Array of TTranscript;
  TExonArray = Array of TExon;
  TCodingSequenceArray = Array of TCodingSequence;
  TBatchCreateAnnotationsRequestArray = Array of TBatchCreateAnnotationsRequest;
  TBatchCreateAnnotationsResponseArray = Array of TBatchCreateAnnotationsResponse;
  TEntryArray = Array of TEntry;
  TStatusArray = Array of TStatus;
  TSearchAnnotationsRequestArray = Array of TSearchAnnotationsRequest;
  TSearchAnnotationsResponseArray = Array of TSearchAnnotationsResponse;
  TListDatasetsResponseArray = Array of TListDatasetsResponse;
  TDatasetArray = Array of TDataset;
  TUndeleteDatasetRequestArray = Array of TUndeleteDatasetRequest;
  TSetIamPolicyRequestArray = Array of TSetIamPolicyRequest;
  TPolicyArray = Array of TPolicy;
  TBindingArray = Array of TBinding;
  TGetIamPolicyRequestArray = Array of TGetIamPolicyRequest;
  TTestIamPermissionsRequestArray = Array of TTestIamPermissionsRequest;
  TTestIamPermissionsResponseArray = Array of TTestIamPermissionsResponse;
  TOperationArray = Array of TOperation;
  TListOperationsResponseArray = Array of TListOperationsResponse;
  TCancelOperationRequestArray = Array of TCancelOperationRequest;
  TImportReadGroupSetsRequestArray = Array of TImportReadGroupSetsRequest;
  TExportReadGroupSetRequestArray = Array of TExportReadGroupSetRequest;
  TSearchReadGroupSetsRequestArray = Array of TSearchReadGroupSetsRequest;
  TSearchReadGroupSetsResponseArray = Array of TSearchReadGroupSetsResponse;
  TReadGroupSetArray = Array of TReadGroupSet;
  TReadGroupArray = Array of TReadGroup;
  TExperimentArray = Array of TExperiment;
  TProgramArray = Array of TProgram;
  TListCoverageBucketsResponseArray = Array of TListCoverageBucketsResponse;
  TCoverageBucketArray = Array of TCoverageBucket;
  TRangeArray = Array of TRange;
  TSearchReadsRequestArray = Array of TSearchReadsRequest;
  TSearchReadsResponseArray = Array of TSearchReadsResponse;
  TReadArray = Array of TRead;
  TLinearAlignmentArray = Array of TLinearAlignment;
  TPositionArray = Array of TPosition;
  TCigarUnitArray = Array of TCigarUnit;
  TStreamReadsRequestArray = Array of TStreamReadsRequest;
  TStreamReadsResponseArray = Array of TStreamReadsResponse;
  TSearchReferenceSetsRequestArray = Array of TSearchReferenceSetsRequest;
  TSearchReferenceSetsResponseArray = Array of TSearchReferenceSetsResponse;
  TReferenceSetArray = Array of TReferenceSet;
  TSearchReferencesRequestArray = Array of TSearchReferencesRequest;
  TSearchReferencesResponseArray = Array of TSearchReferencesResponse;
  TReferenceArray = Array of TReference;
  TListBasesResponseArray = Array of TListBasesResponse;
  TImportVariantsRequestArray = Array of TImportVariantsRequest;
  TVariantSetArray = Array of TVariantSet;
  TReferenceBoundArray = Array of TReferenceBound;
  TVariantSetMetadataArray = Array of TVariantSetMetadata;
  TExportVariantSetRequestArray = Array of TExportVariantSetRequest;
  TSearchVariantSetsRequestArray = Array of TSearchVariantSetsRequest;
  TSearchVariantSetsResponseArray = Array of TSearchVariantSetsResponse;
  TSearchVariantsRequestArray = Array of TSearchVariantsRequest;
  TSearchVariantsResponseArray = Array of TSearchVariantsResponse;
  TVariantArray = Array of TVariant;
  TVariantCallArray = Array of TVariantCall;
  TMergeVariantsRequestArray = Array of TMergeVariantsRequest;
  TSearchCallSetsRequestArray = Array of TSearchCallSetsRequest;
  TSearchCallSetsResponseArray = Array of TSearchCallSetsResponse;
  TCallSetArray = Array of TCallSet;
  TStreamVariantsRequestArray = Array of TStreamVariantsRequest;
  TStreamVariantsResponseArray = Array of TStreamVariantsResponse;
  TImportReadGroupSetsResponseArray = Array of TImportReadGroupSetsResponse;
  TImportVariantsResponseArray = Array of TImportVariantsResponse;
  TOperationMetadataArray = Array of TOperationMetadata;
  TOperationEventArray = Array of TOperationEvent;
  //Anonymous types, using auto-generated names
  TAnnotationSetTypeinfo = Class;
  TAnnotationTypeinfo = Class;
  TStatusTypedetailsItem = Class;
  TOperationTypemetadata = Class;
  TOperationTyperesponse = Class;
  TReadGroupSetTypeinfo = Class;
  TReadGroupTypeinfo = Class;
  TReadTypeinfo = Class;
  TImportVariantsRequestTypeinfoMergeConfig = Class;
  TVariantSetMetadataTypeinfo = Class;
  TVariantTypeinfo = Class;
  TVariantCallTypeinfo = Class;
  TMergeVariantsRequestTypeinfoMergeConfig = Class;
  TCallSetTypeinfo = Class;
  TOperationMetadataTyperequest = Class;
  TSearchAnnotationSetsResponseTypeannotationSetsArray = Array of TAnnotationSet;
  TVariantAnnotationTypeconditionsArray = Array of TClinicalCondition;
  TClinicalConditionTypeexternalIdsArray = Array of TExternalId;
  TTranscriptTypeexonsArray = Array of TExon;
  TBatchCreateAnnotationsRequestTypeannotationsArray = Array of TAnnotation;
  TBatchCreateAnnotationsResponseTypeentriesArray = Array of TEntry;
  TStatusTypedetailsArray = Array of TStatusTypedetailsItem;
  TSearchAnnotationsResponseTypeannotationsArray = Array of TAnnotation;
  TListDatasetsResponseTypedatasetsArray = Array of TDataset;
  TPolicyTypebindingsArray = Array of TBinding;
  TListOperationsResponseTypeoperationsArray = Array of TOperation;
  TSearchReadGroupSetsResponseTypereadGroupSetsArray = Array of TReadGroupSet;
  TReadGroupSetTypereadGroupsArray = Array of TReadGroup;
  TReadGroupTypeprogramsArray = Array of TProgram;
  TListCoverageBucketsResponseTypecoverageBucketsArray = Array of TCoverageBucket;
  TSearchReadsResponseTypealignmentsArray = Array of TRead;
  TLinearAlignmentTypecigarArray = Array of TCigarUnit;
  TStreamReadsResponseTypealignmentsArray = Array of TRead;
  TSearchReferenceSetsResponseTypereferenceSetsArray = Array of TReferenceSet;
  TSearchReferencesResponseTypereferencesArray = Array of TReference;
  TVariantSetTypereferenceBoundsArray = Array of TReferenceBound;
  TVariantSetTypemetadataArray = Array of TVariantSetMetadata;
  TSearchVariantSetsResponseTypevariantSetsArray = Array of TVariantSet;
  TSearchVariantsResponseTypevariantsArray = Array of TVariant;
  TVariantTypecallsArray = Array of TVariantCall;
  TMergeVariantsRequestTypevariantsArray = Array of TVariant;
  TSearchCallSetsResponseTypecallSetsArray = Array of TCallSet;
  TStreamVariantsResponseTypevariantsArray = Array of TVariant;
  TOperationMetadataTypeeventsArray = Array of TOperationEvent;
  
  { --------------------------------------------------------------------
    TAnnotationSetTypeinfo
    --------------------------------------------------------------------}
  
  TAnnotationSetTypeinfo = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TAnnotationSetTypeinfoClass = Class of TAnnotationSetTypeinfo;
  
  { --------------------------------------------------------------------
    TAnnotationSet
    --------------------------------------------------------------------}
  
  TAnnotationSet = Class(TGoogleBaseObject)
  Private
    Fid : String;
    FdatasetId : String;
    FreferenceSetId : String;
    Fname : String;
    FsourceUri : String;
    F_type : String;
    Finfo : TAnnotationSetTypeinfo;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdatasetId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetreferenceSetId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure SetsourceUri(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
    Procedure Setinfo(AIndex : Integer; const AValue : TAnnotationSetTypeinfo); virtual;
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property datasetId : String Index 8 Read FdatasetId Write SetdatasetId;
    Property referenceSetId : String Index 16 Read FreferenceSetId Write SetreferenceSetId;
    Property name : String Index 24 Read Fname Write Setname;
    Property sourceUri : String Index 32 Read FsourceUri Write SetsourceUri;
    Property _type : String Index 40 Read F_type Write Set_type;
    Property info : TAnnotationSetTypeinfo Index 48 Read Finfo Write Setinfo;
  end;
  TAnnotationSetClass = Class of TAnnotationSet;
  
  { --------------------------------------------------------------------
    TEmpty
    --------------------------------------------------------------------}
  
  TEmpty = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TEmptyClass = Class of TEmpty;
  
  { --------------------------------------------------------------------
    TSearchAnnotationSetsRequest
    --------------------------------------------------------------------}
  
  TSearchAnnotationSetsRequest = Class(TGoogleBaseObject)
  Private
    FdatasetIds : TStringArray;
    FreferenceSetId : String;
    Fname : String;
    Ftypes : TStringArray;
    FpageToken : String;
    FpageSize : integer;
  Protected
    //Property setters
    Procedure SetdatasetIds(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetreferenceSetId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure Settypes(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetpageToken(AIndex : Integer; const AValue : String); virtual;
    Procedure SetpageSize(AIndex : Integer; const AValue : integer); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property datasetIds : TStringArray Index 0 Read FdatasetIds Write SetdatasetIds;
    Property referenceSetId : String Index 8 Read FreferenceSetId Write SetreferenceSetId;
    Property name : String Index 16 Read Fname Write Setname;
    Property types : TStringArray Index 24 Read Ftypes Write Settypes;
    Property pageToken : String Index 32 Read FpageToken Write SetpageToken;
    Property pageSize : integer Index 40 Read FpageSize Write SetpageSize;
  end;
  TSearchAnnotationSetsRequestClass = Class of TSearchAnnotationSetsRequest;
  
  { --------------------------------------------------------------------
    TSearchAnnotationSetsResponse
    --------------------------------------------------------------------}
  
  TSearchAnnotationSetsResponse = Class(TGoogleBaseObject)
  Private
    FannotationSets : TSearchAnnotationSetsResponseTypeannotationSetsArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure SetannotationSets(AIndex : Integer; const AValue : TSearchAnnotationSetsResponseTypeannotationSetsArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property annotationSets : TSearchAnnotationSetsResponseTypeannotationSetsArray Index 0 Read FannotationSets Write SetannotationSets;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TSearchAnnotationSetsResponseClass = Class of TSearchAnnotationSetsResponse;
  
  { --------------------------------------------------------------------
    TAnnotationTypeinfo
    --------------------------------------------------------------------}
  
  TAnnotationTypeinfo = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TAnnotationTypeinfoClass = Class of TAnnotationTypeinfo;
  
  { --------------------------------------------------------------------
    TAnnotation
    --------------------------------------------------------------------}
  
  TAnnotation = Class(TGoogleBaseObject)
  Private
    Fid : String;
    FannotationSetId : String;
    Fname : String;
    FreferenceId : String;
    FreferenceName : String;
    Fstart : String;
    F_end : String;
    FreverseStrand : boolean;
    F_type : String;
    Fvariant : TVariantAnnotation;
    Ftranscript : TTranscript;
    Finfo : TAnnotationTypeinfo;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure SetannotationSetId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure SetreferenceId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetreferenceName(AIndex : Integer; const AValue : String); virtual;
    Procedure Setstart(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_end(AIndex : Integer; const AValue : String); virtual;
    Procedure SetreverseStrand(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
    Procedure Setvariant(AIndex : Integer; const AValue : TVariantAnnotation); virtual;
    Procedure Settranscript(AIndex : Integer; const AValue : TTranscript); virtual;
    Procedure Setinfo(AIndex : Integer; const AValue : TAnnotationTypeinfo); virtual;
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property annotationSetId : String Index 8 Read FannotationSetId Write SetannotationSetId;
    Property name : String Index 16 Read Fname Write Setname;
    Property referenceId : String Index 24 Read FreferenceId Write SetreferenceId;
    Property referenceName : String Index 32 Read FreferenceName Write SetreferenceName;
    Property start : String Index 40 Read Fstart Write Setstart;
    Property _end : String Index 48 Read F_end Write Set_end;
    Property reverseStrand : boolean Index 56 Read FreverseStrand Write SetreverseStrand;
    Property _type : String Index 64 Read F_type Write Set_type;
    Property variant : TVariantAnnotation Index 72 Read Fvariant Write Setvariant;
    Property transcript : TTranscript Index 80 Read Ftranscript Write Settranscript;
    Property info : TAnnotationTypeinfo Index 88 Read Finfo Write Setinfo;
  end;
  TAnnotationClass = Class of TAnnotation;
  
  { --------------------------------------------------------------------
    TVariantAnnotation
    --------------------------------------------------------------------}
  
  TVariantAnnotation = Class(TGoogleBaseObject)
  Private
    F_type : String;
    Feffect : String;
    FalternateBases : String;
    FgeneId : String;
    FtranscriptIds : TStringArray;
    Fconditions : TVariantAnnotationTypeconditionsArray;
    FclinicalSignificance : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
    Procedure Seteffect(AIndex : Integer; const AValue : String); virtual;
    Procedure SetalternateBases(AIndex : Integer; const AValue : String); virtual;
    Procedure SetgeneId(AIndex : Integer; const AValue : String); virtual;
    Procedure SettranscriptIds(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure Setconditions(AIndex : Integer; const AValue : TVariantAnnotationTypeconditionsArray); virtual;
    Procedure SetclinicalSignificance(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property _type : String Index 0 Read F_type Write Set_type;
    Property effect : String Index 8 Read Feffect Write Seteffect;
    Property alternateBases : String Index 16 Read FalternateBases Write SetalternateBases;
    Property geneId : String Index 24 Read FgeneId Write SetgeneId;
    Property transcriptIds : TStringArray Index 32 Read FtranscriptIds Write SettranscriptIds;
    Property conditions : TVariantAnnotationTypeconditionsArray Index 40 Read Fconditions Write Setconditions;
    Property clinicalSignificance : String Index 48 Read FclinicalSignificance Write SetclinicalSignificance;
  end;
  TVariantAnnotationClass = Class of TVariantAnnotation;
  
  { --------------------------------------------------------------------
    TClinicalCondition
    --------------------------------------------------------------------}
  
  TClinicalCondition = Class(TGoogleBaseObject)
  Private
    Fnames : TStringArray;
    FexternalIds : TClinicalConditionTypeexternalIdsArray;
    FconceptId : String;
    FomimId : String;
  Protected
    //Property setters
    Procedure Setnames(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetexternalIds(AIndex : Integer; const AValue : TClinicalConditionTypeexternalIdsArray); virtual;
    Procedure SetconceptId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetomimId(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property names : TStringArray Index 0 Read Fnames Write Setnames;
    Property externalIds : TClinicalConditionTypeexternalIdsArray Index 8 Read FexternalIds Write SetexternalIds;
    Property conceptId : String Index 16 Read FconceptId Write SetconceptId;
    Property omimId : String Index 24 Read FomimId Write SetomimId;
  end;
  TClinicalConditionClass = Class of TClinicalCondition;
  
  { --------------------------------------------------------------------
    TExternalId
    --------------------------------------------------------------------}
  
  TExternalId = Class(TGoogleBaseObject)
  Private
    FsourceName : String;
    Fid : String;
  Protected
    //Property setters
    Procedure SetsourceName(AIndex : Integer; const AValue : String); virtual;
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property sourceName : String Index 0 Read FsourceName Write SetsourceName;
    Property id : String Index 8 Read Fid Write Setid;
  end;
  TExternalIdClass = Class of TExternalId;
  
  { --------------------------------------------------------------------
    TTranscript
    --------------------------------------------------------------------}
  
  TTranscript = Class(TGoogleBaseObject)
  Private
    FgeneId : String;
    Fexons : TTranscriptTypeexonsArray;
    FcodingSequence : TCodingSequence;
  Protected
    //Property setters
    Procedure SetgeneId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setexons(AIndex : Integer; const AValue : TTranscriptTypeexonsArray); virtual;
    Procedure SetcodingSequence(AIndex : Integer; const AValue : TCodingSequence); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property geneId : String Index 0 Read FgeneId Write SetgeneId;
    Property exons : TTranscriptTypeexonsArray Index 8 Read Fexons Write Setexons;
    Property codingSequence : TCodingSequence Index 16 Read FcodingSequence Write SetcodingSequence;
  end;
  TTranscriptClass = Class of TTranscript;
  
  { --------------------------------------------------------------------
    TExon
    --------------------------------------------------------------------}
  
  TExon = Class(TGoogleBaseObject)
  Private
    Fstart : String;
    F_end : String;
    Fframe : integer;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setstart(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_end(AIndex : Integer; const AValue : String); virtual;
    Procedure Setframe(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property start : String Index 0 Read Fstart Write Setstart;
    Property _end : String Index 8 Read F_end Write Set_end;
    Property frame : integer Index 16 Read Fframe Write Setframe;
  end;
  TExonClass = Class of TExon;
  
  { --------------------------------------------------------------------
    TCodingSequence
    --------------------------------------------------------------------}
  
  TCodingSequence = Class(TGoogleBaseObject)
  Private
    Fstart : String;
    F_end : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setstart(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_end(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property start : String Index 0 Read Fstart Write Setstart;
    Property _end : String Index 8 Read F_end Write Set_end;
  end;
  TCodingSequenceClass = Class of TCodingSequence;
  
  { --------------------------------------------------------------------
    TBatchCreateAnnotationsRequest
    --------------------------------------------------------------------}
  
  TBatchCreateAnnotationsRequest = Class(TGoogleBaseObject)
  Private
    Fannotations : TBatchCreateAnnotationsRequestTypeannotationsArray;
  Protected
    //Property setters
    Procedure Setannotations(AIndex : Integer; const AValue : TBatchCreateAnnotationsRequestTypeannotationsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property annotations : TBatchCreateAnnotationsRequestTypeannotationsArray Index 0 Read Fannotations Write Setannotations;
  end;
  TBatchCreateAnnotationsRequestClass = Class of TBatchCreateAnnotationsRequest;
  
  { --------------------------------------------------------------------
    TBatchCreateAnnotationsResponse
    --------------------------------------------------------------------}
  
  TBatchCreateAnnotationsResponse = Class(TGoogleBaseObject)
  Private
    Fentries : TBatchCreateAnnotationsResponseTypeentriesArray;
  Protected
    //Property setters
    Procedure Setentries(AIndex : Integer; const AValue : TBatchCreateAnnotationsResponseTypeentriesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property entries : TBatchCreateAnnotationsResponseTypeentriesArray Index 0 Read Fentries Write Setentries;
  end;
  TBatchCreateAnnotationsResponseClass = Class of TBatchCreateAnnotationsResponse;
  
  { --------------------------------------------------------------------
    TEntry
    --------------------------------------------------------------------}
  
  TEntry = Class(TGoogleBaseObject)
  Private
    Fstatus : TStatus;
    Fannotation : TAnnotation;
  Protected
    //Property setters
    Procedure Setstatus(AIndex : Integer; const AValue : TStatus); virtual;
    Procedure Setannotation(AIndex : Integer; const AValue : TAnnotation); virtual;
  Public
  Published
    Property status : TStatus Index 0 Read Fstatus Write Setstatus;
    Property annotation : TAnnotation Index 8 Read Fannotation Write Setannotation;
  end;
  TEntryClass = Class of TEntry;
  
  { --------------------------------------------------------------------
    TStatusTypedetailsItem
    --------------------------------------------------------------------}
  
  TStatusTypedetailsItem = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TStatusTypedetailsItemClass = Class of TStatusTypedetailsItem;
  
  { --------------------------------------------------------------------
    TStatus
    --------------------------------------------------------------------}
  
  TStatus = Class(TGoogleBaseObject)
  Private
    Fcode : integer;
    Fmessage : String;
    Fdetails : TStatusTypedetailsArray;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setmessage(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdetails(AIndex : Integer; const AValue : TStatusTypedetailsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property code : integer Index 0 Read Fcode Write Setcode;
    Property message : String Index 8 Read Fmessage Write Setmessage;
    Property details : TStatusTypedetailsArray Index 16 Read Fdetails Write Setdetails;
  end;
  TStatusClass = Class of TStatus;
  
  { --------------------------------------------------------------------
    TSearchAnnotationsRequest
    --------------------------------------------------------------------}
  
  TSearchAnnotationsRequest = Class(TGoogleBaseObject)
  Private
    FannotationSetIds : TStringArray;
    FreferenceId : String;
    FreferenceName : String;
    Fstart : String;
    F_end : String;
    FpageToken : String;
    FpageSize : integer;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetannotationSetIds(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetreferenceId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetreferenceName(AIndex : Integer; const AValue : String); virtual;
    Procedure Setstart(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_end(AIndex : Integer; const AValue : String); virtual;
    Procedure SetpageToken(AIndex : Integer; const AValue : String); virtual;
    Procedure SetpageSize(AIndex : Integer; const AValue : integer); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property annotationSetIds : TStringArray Index 0 Read FannotationSetIds Write SetannotationSetIds;
    Property referenceId : String Index 8 Read FreferenceId Write SetreferenceId;
    Property referenceName : String Index 16 Read FreferenceName Write SetreferenceName;
    Property start : String Index 24 Read Fstart Write Setstart;
    Property _end : String Index 32 Read F_end Write Set_end;
    Property pageToken : String Index 40 Read FpageToken Write SetpageToken;
    Property pageSize : integer Index 48 Read FpageSize Write SetpageSize;
  end;
  TSearchAnnotationsRequestClass = Class of TSearchAnnotationsRequest;
  
  { --------------------------------------------------------------------
    TSearchAnnotationsResponse
    --------------------------------------------------------------------}
  
  TSearchAnnotationsResponse = Class(TGoogleBaseObject)
  Private
    Fannotations : TSearchAnnotationsResponseTypeannotationsArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setannotations(AIndex : Integer; const AValue : TSearchAnnotationsResponseTypeannotationsArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property annotations : TSearchAnnotationsResponseTypeannotationsArray Index 0 Read Fannotations Write Setannotations;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TSearchAnnotationsResponseClass = Class of TSearchAnnotationsResponse;
  
  { --------------------------------------------------------------------
    TListDatasetsResponse
    --------------------------------------------------------------------}
  
  TListDatasetsResponse = Class(TGoogleBaseObject)
  Private
    Fdatasets : TListDatasetsResponseTypedatasetsArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setdatasets(AIndex : Integer; const AValue : TListDatasetsResponseTypedatasetsArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property datasets : TListDatasetsResponseTypedatasetsArray Index 0 Read Fdatasets Write Setdatasets;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TListDatasetsResponseClass = Class of TListDatasetsResponse;
  
  { --------------------------------------------------------------------
    TDataset
    --------------------------------------------------------------------}
  
  TDataset = Class(TGoogleBaseObject)
  Private
    Fid : String;
    FprojectId : String;
    Fname : String;
    FcreateTime : String;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure SetprojectId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcreateTime(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property projectId : String Index 8 Read FprojectId Write SetprojectId;
    Property name : String Index 16 Read Fname Write Setname;
    Property createTime : String Index 24 Read FcreateTime Write SetcreateTime;
  end;
  TDatasetClass = Class of TDataset;
  
  { --------------------------------------------------------------------
    TUndeleteDatasetRequest
    --------------------------------------------------------------------}
  
  TUndeleteDatasetRequest = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TUndeleteDatasetRequestClass = Class of TUndeleteDatasetRequest;
  
  { --------------------------------------------------------------------
    TSetIamPolicyRequest
    --------------------------------------------------------------------}
  
  TSetIamPolicyRequest = Class(TGoogleBaseObject)
  Private
    Fpolicy : TPolicy;
  Protected
    //Property setters
    Procedure Setpolicy(AIndex : Integer; const AValue : TPolicy); virtual;
  Public
  Published
    Property policy : TPolicy Index 0 Read Fpolicy Write Setpolicy;
  end;
  TSetIamPolicyRequestClass = Class of TSetIamPolicyRequest;
  
  { --------------------------------------------------------------------
    TPolicy
    --------------------------------------------------------------------}
  
  TPolicy = Class(TGoogleBaseObject)
  Private
    Fversion : integer;
    Fbindings : TPolicyTypebindingsArray;
    Fetag : String;
  Protected
    //Property setters
    Procedure Setversion(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setbindings(AIndex : Integer; const AValue : TPolicyTypebindingsArray); virtual;
    Procedure Setetag(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property version : integer Index 0 Read Fversion Write Setversion;
    Property bindings : TPolicyTypebindingsArray Index 8 Read Fbindings Write Setbindings;
    Property etag : String Index 16 Read Fetag Write Setetag;
  end;
  TPolicyClass = Class of TPolicy;
  
  { --------------------------------------------------------------------
    TBinding
    --------------------------------------------------------------------}
  
  TBinding = Class(TGoogleBaseObject)
  Private
    Frole : String;
    Fmembers : TStringArray;
  Protected
    //Property setters
    Procedure Setrole(AIndex : Integer; const AValue : String); virtual;
    Procedure Setmembers(AIndex : Integer; const AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property role : String Index 0 Read Frole Write Setrole;
    Property members : TStringArray Index 8 Read Fmembers Write Setmembers;
  end;
  TBindingClass = Class of TBinding;
  
  { --------------------------------------------------------------------
    TGetIamPolicyRequest
    --------------------------------------------------------------------}
  
  TGetIamPolicyRequest = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TGetIamPolicyRequestClass = Class of TGetIamPolicyRequest;
  
  { --------------------------------------------------------------------
    TTestIamPermissionsRequest
    --------------------------------------------------------------------}
  
  TTestIamPermissionsRequest = Class(TGoogleBaseObject)
  Private
    Fpermissions : TStringArray;
  Protected
    //Property setters
    Procedure Setpermissions(AIndex : Integer; const AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property permissions : TStringArray Index 0 Read Fpermissions Write Setpermissions;
  end;
  TTestIamPermissionsRequestClass = Class of TTestIamPermissionsRequest;
  
  { --------------------------------------------------------------------
    TTestIamPermissionsResponse
    --------------------------------------------------------------------}
  
  TTestIamPermissionsResponse = Class(TGoogleBaseObject)
  Private
    Fpermissions : TStringArray;
  Protected
    //Property setters
    Procedure Setpermissions(AIndex : Integer; const AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property permissions : TStringArray Index 0 Read Fpermissions Write Setpermissions;
  end;
  TTestIamPermissionsResponseClass = Class of TTestIamPermissionsResponse;
  
  { --------------------------------------------------------------------
    TOperationTypemetadata
    --------------------------------------------------------------------}
  
  TOperationTypemetadata = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TOperationTypemetadataClass = Class of TOperationTypemetadata;
  
  { --------------------------------------------------------------------
    TOperationTyperesponse
    --------------------------------------------------------------------}
  
  TOperationTyperesponse = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TOperationTyperesponseClass = Class of TOperationTyperesponse;
  
  { --------------------------------------------------------------------
    TOperation
    --------------------------------------------------------------------}
  
  TOperation = Class(TGoogleBaseObject)
  Private
    Fname : String;
    Fmetadata : TOperationTypemetadata;
    Fdone : boolean;
    Ferror : TStatus;
    Fresponse : TOperationTyperesponse;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure Setmetadata(AIndex : Integer; const AValue : TOperationTypemetadata); virtual;
    Procedure Setdone(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Seterror(AIndex : Integer; const AValue : TStatus); virtual;
    Procedure Setresponse(AIndex : Integer; const AValue : TOperationTyperesponse); virtual;
  Public
  Published
    Property name : String Index 0 Read Fname Write Setname;
    Property metadata : TOperationTypemetadata Index 8 Read Fmetadata Write Setmetadata;
    Property done : boolean Index 16 Read Fdone Write Setdone;
    Property error : TStatus Index 24 Read Ferror Write Seterror;
    Property response : TOperationTyperesponse Index 32 Read Fresponse Write Setresponse;
  end;
  TOperationClass = Class of TOperation;
  
  { --------------------------------------------------------------------
    TListOperationsResponse
    --------------------------------------------------------------------}
  
  TListOperationsResponse = Class(TGoogleBaseObject)
  Private
    Foperations : TListOperationsResponseTypeoperationsArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setoperations(AIndex : Integer; const AValue : TListOperationsResponseTypeoperationsArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property operations : TListOperationsResponseTypeoperationsArray Index 0 Read Foperations Write Setoperations;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TListOperationsResponseClass = Class of TListOperationsResponse;
  
  { --------------------------------------------------------------------
    TCancelOperationRequest
    --------------------------------------------------------------------}
  
  TCancelOperationRequest = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TCancelOperationRequestClass = Class of TCancelOperationRequest;
  
  { --------------------------------------------------------------------
    TImportReadGroupSetsRequest
    --------------------------------------------------------------------}
  
  TImportReadGroupSetsRequest = Class(TGoogleBaseObject)
  Private
    FdatasetId : String;
    FreferenceSetId : String;
    FsourceUris : TStringArray;
    FpartitionStrategy : String;
  Protected
    //Property setters
    Procedure SetdatasetId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetreferenceSetId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetsourceUris(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetpartitionStrategy(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property datasetId : String Index 0 Read FdatasetId Write SetdatasetId;
    Property referenceSetId : String Index 8 Read FreferenceSetId Write SetreferenceSetId;
    Property sourceUris : TStringArray Index 16 Read FsourceUris Write SetsourceUris;
    Property partitionStrategy : String Index 24 Read FpartitionStrategy Write SetpartitionStrategy;
  end;
  TImportReadGroupSetsRequestClass = Class of TImportReadGroupSetsRequest;
  
  { --------------------------------------------------------------------
    TExportReadGroupSetRequest
    --------------------------------------------------------------------}
  
  TExportReadGroupSetRequest = Class(TGoogleBaseObject)
  Private
    FprojectId : String;
    FexportUri : String;
    FreferenceNames : TStringArray;
  Protected
    //Property setters
    Procedure SetprojectId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetexportUri(AIndex : Integer; const AValue : String); virtual;
    Procedure SetreferenceNames(AIndex : Integer; const AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property projectId : String Index 0 Read FprojectId Write SetprojectId;
    Property exportUri : String Index 8 Read FexportUri Write SetexportUri;
    Property referenceNames : TStringArray Index 16 Read FreferenceNames Write SetreferenceNames;
  end;
  TExportReadGroupSetRequestClass = Class of TExportReadGroupSetRequest;
  
  { --------------------------------------------------------------------
    TSearchReadGroupSetsRequest
    --------------------------------------------------------------------}
  
  TSearchReadGroupSetsRequest = Class(TGoogleBaseObject)
  Private
    FdatasetIds : TStringArray;
    Fname : String;
    FpageToken : String;
    FpageSize : integer;
  Protected
    //Property setters
    Procedure SetdatasetIds(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure SetpageToken(AIndex : Integer; const AValue : String); virtual;
    Procedure SetpageSize(AIndex : Integer; const AValue : integer); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property datasetIds : TStringArray Index 0 Read FdatasetIds Write SetdatasetIds;
    Property name : String Index 8 Read Fname Write Setname;
    Property pageToken : String Index 16 Read FpageToken Write SetpageToken;
    Property pageSize : integer Index 24 Read FpageSize Write SetpageSize;
  end;
  TSearchReadGroupSetsRequestClass = Class of TSearchReadGroupSetsRequest;
  
  { --------------------------------------------------------------------
    TSearchReadGroupSetsResponse
    --------------------------------------------------------------------}
  
  TSearchReadGroupSetsResponse = Class(TGoogleBaseObject)
  Private
    FreadGroupSets : TSearchReadGroupSetsResponseTypereadGroupSetsArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure SetreadGroupSets(AIndex : Integer; const AValue : TSearchReadGroupSetsResponseTypereadGroupSetsArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property readGroupSets : TSearchReadGroupSetsResponseTypereadGroupSetsArray Index 0 Read FreadGroupSets Write SetreadGroupSets;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TSearchReadGroupSetsResponseClass = Class of TSearchReadGroupSetsResponse;
  
  { --------------------------------------------------------------------
    TReadGroupSetTypeinfo
    --------------------------------------------------------------------}
  
  TReadGroupSetTypeinfo = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TReadGroupSetTypeinfoClass = Class of TReadGroupSetTypeinfo;
  
  { --------------------------------------------------------------------
    TReadGroupSet
    --------------------------------------------------------------------}
  
  TReadGroupSet = Class(TGoogleBaseObject)
  Private
    Fid : String;
    FdatasetId : String;
    FreferenceSetId : String;
    Fname : String;
    Ffilename : String;
    FreadGroups : TReadGroupSetTypereadGroupsArray;
    Finfo : TReadGroupSetTypeinfo;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdatasetId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetreferenceSetId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure Setfilename(AIndex : Integer; const AValue : String); virtual;
    Procedure SetreadGroups(AIndex : Integer; const AValue : TReadGroupSetTypereadGroupsArray); virtual;
    Procedure Setinfo(AIndex : Integer; const AValue : TReadGroupSetTypeinfo); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property datasetId : String Index 8 Read FdatasetId Write SetdatasetId;
    Property referenceSetId : String Index 16 Read FreferenceSetId Write SetreferenceSetId;
    Property name : String Index 24 Read Fname Write Setname;
    Property filename : String Index 32 Read Ffilename Write Setfilename;
    Property readGroups : TReadGroupSetTypereadGroupsArray Index 40 Read FreadGroups Write SetreadGroups;
    Property info : TReadGroupSetTypeinfo Index 48 Read Finfo Write Setinfo;
  end;
  TReadGroupSetClass = Class of TReadGroupSet;
  
  { --------------------------------------------------------------------
    TReadGroupTypeinfo
    --------------------------------------------------------------------}
  
  TReadGroupTypeinfo = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TReadGroupTypeinfoClass = Class of TReadGroupTypeinfo;
  
  { --------------------------------------------------------------------
    TReadGroup
    --------------------------------------------------------------------}
  
  TReadGroup = Class(TGoogleBaseObject)
  Private
    Fid : String;
    FdatasetId : String;
    Fname : String;
    Fdescription : String;
    FsampleId : String;
    Fexperiment : TExperiment;
    FpredictedInsertSize : integer;
    Fprograms : TReadGroupTypeprogramsArray;
    FreferenceSetId : String;
    Finfo : TReadGroupTypeinfo;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure SetdatasetId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; const AValue : String); virtual;
    Procedure SetsampleId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setexperiment(AIndex : Integer; const AValue : TExperiment); virtual;
    Procedure SetpredictedInsertSize(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setprograms(AIndex : Integer; const AValue : TReadGroupTypeprogramsArray); virtual;
    Procedure SetreferenceSetId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setinfo(AIndex : Integer; const AValue : TReadGroupTypeinfo); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property datasetId : String Index 8 Read FdatasetId Write SetdatasetId;
    Property name : String Index 16 Read Fname Write Setname;
    Property description : String Index 24 Read Fdescription Write Setdescription;
    Property sampleId : String Index 32 Read FsampleId Write SetsampleId;
    Property experiment : TExperiment Index 40 Read Fexperiment Write Setexperiment;
    Property predictedInsertSize : integer Index 48 Read FpredictedInsertSize Write SetpredictedInsertSize;
    Property programs : TReadGroupTypeprogramsArray Index 56 Read Fprograms Write Setprograms;
    Property referenceSetId : String Index 64 Read FreferenceSetId Write SetreferenceSetId;
    Property info : TReadGroupTypeinfo Index 72 Read Finfo Write Setinfo;
  end;
  TReadGroupClass = Class of TReadGroup;
  
  { --------------------------------------------------------------------
    TExperiment
    --------------------------------------------------------------------}
  
  TExperiment = Class(TGoogleBaseObject)
  Private
    FlibraryId : String;
    FplatformUnit : String;
    FsequencingCenter : String;
    FinstrumentModel : String;
  Protected
    //Property setters
    Procedure SetlibraryId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetplatformUnit(AIndex : Integer; const AValue : String); virtual;
    Procedure SetsequencingCenter(AIndex : Integer; const AValue : String); virtual;
    Procedure SetinstrumentModel(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property libraryId : String Index 0 Read FlibraryId Write SetlibraryId;
    Property platformUnit : String Index 8 Read FplatformUnit Write SetplatformUnit;
    Property sequencingCenter : String Index 16 Read FsequencingCenter Write SetsequencingCenter;
    Property instrumentModel : String Index 24 Read FinstrumentModel Write SetinstrumentModel;
  end;
  TExperimentClass = Class of TExperiment;
  
  { --------------------------------------------------------------------
    TProgram
    --------------------------------------------------------------------}
  
  TProgram = Class(TGoogleBaseObject)
  Private
    FcommandLine : String;
    Fid : String;
    Fname : String;
    FprevProgramId : String;
    Fversion : String;
  Protected
    //Property setters
    Procedure SetcommandLine(AIndex : Integer; const AValue : String); virtual;
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure SetprevProgramId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setversion(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property commandLine : String Index 0 Read FcommandLine Write SetcommandLine;
    Property id : String Index 8 Read Fid Write Setid;
    Property name : String Index 16 Read Fname Write Setname;
    Property prevProgramId : String Index 24 Read FprevProgramId Write SetprevProgramId;
    Property version : String Index 32 Read Fversion Write Setversion;
  end;
  TProgramClass = Class of TProgram;
  
  { --------------------------------------------------------------------
    TListCoverageBucketsResponse
    --------------------------------------------------------------------}
  
  TListCoverageBucketsResponse = Class(TGoogleBaseObject)
  Private
    FbucketWidth : String;
    FcoverageBuckets : TListCoverageBucketsResponseTypecoverageBucketsArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure SetbucketWidth(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcoverageBuckets(AIndex : Integer; const AValue : TListCoverageBucketsResponseTypecoverageBucketsArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property bucketWidth : String Index 0 Read FbucketWidth Write SetbucketWidth;
    Property coverageBuckets : TListCoverageBucketsResponseTypecoverageBucketsArray Index 8 Read FcoverageBuckets Write SetcoverageBuckets;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TListCoverageBucketsResponseClass = Class of TListCoverageBucketsResponse;
  
  { --------------------------------------------------------------------
    TCoverageBucket
    --------------------------------------------------------------------}
  
  TCoverageBucket = Class(TGoogleBaseObject)
  Private
    Frange : TRange;
    FmeanCoverage : integer;
  Protected
    //Property setters
    Procedure Setrange(AIndex : Integer; const AValue : TRange); virtual;
    Procedure SetmeanCoverage(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property range : TRange Index 0 Read Frange Write Setrange;
    Property meanCoverage : integer Index 8 Read FmeanCoverage Write SetmeanCoverage;
  end;
  TCoverageBucketClass = Class of TCoverageBucket;
  
  { --------------------------------------------------------------------
    TRange
    --------------------------------------------------------------------}
  
  TRange = Class(TGoogleBaseObject)
  Private
    FreferenceName : String;
    Fstart : String;
    F_end : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetreferenceName(AIndex : Integer; const AValue : String); virtual;
    Procedure Setstart(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_end(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property referenceName : String Index 0 Read FreferenceName Write SetreferenceName;
    Property start : String Index 8 Read Fstart Write Setstart;
    Property _end : String Index 16 Read F_end Write Set_end;
  end;
  TRangeClass = Class of TRange;
  
  { --------------------------------------------------------------------
    TSearchReadsRequest
    --------------------------------------------------------------------}
  
  TSearchReadsRequest = Class(TGoogleBaseObject)
  Private
    FreadGroupSetIds : TStringArray;
    FreadGroupIds : TStringArray;
    FreferenceName : String;
    Fstart : String;
    F_end : String;
    FpageToken : String;
    FpageSize : integer;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetreadGroupSetIds(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetreadGroupIds(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetreferenceName(AIndex : Integer; const AValue : String); virtual;
    Procedure Setstart(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_end(AIndex : Integer; const AValue : String); virtual;
    Procedure SetpageToken(AIndex : Integer; const AValue : String); virtual;
    Procedure SetpageSize(AIndex : Integer; const AValue : integer); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property readGroupSetIds : TStringArray Index 0 Read FreadGroupSetIds Write SetreadGroupSetIds;
    Property readGroupIds : TStringArray Index 8 Read FreadGroupIds Write SetreadGroupIds;
    Property referenceName : String Index 16 Read FreferenceName Write SetreferenceName;
    Property start : String Index 24 Read Fstart Write Setstart;
    Property _end : String Index 32 Read F_end Write Set_end;
    Property pageToken : String Index 40 Read FpageToken Write SetpageToken;
    Property pageSize : integer Index 48 Read FpageSize Write SetpageSize;
  end;
  TSearchReadsRequestClass = Class of TSearchReadsRequest;
  
  { --------------------------------------------------------------------
    TSearchReadsResponse
    --------------------------------------------------------------------}
  
  TSearchReadsResponse = Class(TGoogleBaseObject)
  Private
    Falignments : TSearchReadsResponseTypealignmentsArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setalignments(AIndex : Integer; const AValue : TSearchReadsResponseTypealignmentsArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property alignments : TSearchReadsResponseTypealignmentsArray Index 0 Read Falignments Write Setalignments;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TSearchReadsResponseClass = Class of TSearchReadsResponse;
  
  { --------------------------------------------------------------------
    TReadTypeinfo
    --------------------------------------------------------------------}
  
  TReadTypeinfo = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TReadTypeinfoClass = Class of TReadTypeinfo;
  
  { --------------------------------------------------------------------
    TRead
    --------------------------------------------------------------------}
  
  TRead = Class(TGoogleBaseObject)
  Private
    Fid : String;
    FreadGroupId : String;
    FreadGroupSetId : String;
    FfragmentName : String;
    FproperPlacement : boolean;
    FduplicateFragment : boolean;
    FfragmentLength : integer;
    FreadNumber : integer;
    FnumberReads : integer;
    FfailedVendorQualityChecks : boolean;
    Falignment : TLinearAlignment;
    FsecondaryAlignment : boolean;
    FsupplementaryAlignment : boolean;
    FalignedSequence : String;
    FalignedQuality : TintegerArray;
    FnextMatePosition : TPosition;
    Finfo : TReadTypeinfo;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure SetreadGroupId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetreadGroupSetId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetfragmentName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetproperPlacement(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetduplicateFragment(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetfragmentLength(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetreadNumber(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetnumberReads(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetfailedVendorQualityChecks(AIndex : Integer; const AValue : boolean); virtual;
    Procedure Setalignment(AIndex : Integer; const AValue : TLinearAlignment); virtual;
    Procedure SetsecondaryAlignment(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetsupplementaryAlignment(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetalignedSequence(AIndex : Integer; const AValue : String); virtual;
    Procedure SetalignedQuality(AIndex : Integer; const AValue : TintegerArray); virtual;
    Procedure SetnextMatePosition(AIndex : Integer; const AValue : TPosition); virtual;
    Procedure Setinfo(AIndex : Integer; const AValue : TReadTypeinfo); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property readGroupId : String Index 8 Read FreadGroupId Write SetreadGroupId;
    Property readGroupSetId : String Index 16 Read FreadGroupSetId Write SetreadGroupSetId;
    Property fragmentName : String Index 24 Read FfragmentName Write SetfragmentName;
    Property properPlacement : boolean Index 32 Read FproperPlacement Write SetproperPlacement;
    Property duplicateFragment : boolean Index 40 Read FduplicateFragment Write SetduplicateFragment;
    Property fragmentLength : integer Index 48 Read FfragmentLength Write SetfragmentLength;
    Property readNumber : integer Index 56 Read FreadNumber Write SetreadNumber;
    Property numberReads : integer Index 64 Read FnumberReads Write SetnumberReads;
    Property failedVendorQualityChecks : boolean Index 72 Read FfailedVendorQualityChecks Write SetfailedVendorQualityChecks;
    Property alignment : TLinearAlignment Index 80 Read Falignment Write Setalignment;
    Property secondaryAlignment : boolean Index 88 Read FsecondaryAlignment Write SetsecondaryAlignment;
    Property supplementaryAlignment : boolean Index 96 Read FsupplementaryAlignment Write SetsupplementaryAlignment;
    Property alignedSequence : String Index 104 Read FalignedSequence Write SetalignedSequence;
    Property alignedQuality : TintegerArray Index 112 Read FalignedQuality Write SetalignedQuality;
    Property nextMatePosition : TPosition Index 120 Read FnextMatePosition Write SetnextMatePosition;
    Property info : TReadTypeinfo Index 128 Read Finfo Write Setinfo;
  end;
  TReadClass = Class of TRead;
  
  { --------------------------------------------------------------------
    TLinearAlignment
    --------------------------------------------------------------------}
  
  TLinearAlignment = Class(TGoogleBaseObject)
  Private
    Fposition : TPosition;
    FmappingQuality : integer;
    Fcigar : TLinearAlignmentTypecigarArray;
  Protected
    //Property setters
    Procedure Setposition(AIndex : Integer; const AValue : TPosition); virtual;
    Procedure SetmappingQuality(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setcigar(AIndex : Integer; const AValue : TLinearAlignmentTypecigarArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property position : TPosition Index 0 Read Fposition Write Setposition;
    Property mappingQuality : integer Index 8 Read FmappingQuality Write SetmappingQuality;
    Property cigar : TLinearAlignmentTypecigarArray Index 16 Read Fcigar Write Setcigar;
  end;
  TLinearAlignmentClass = Class of TLinearAlignment;
  
  { --------------------------------------------------------------------
    TPosition
    --------------------------------------------------------------------}
  
  TPosition = Class(TGoogleBaseObject)
  Private
    FreferenceName : String;
    Fposition : String;
    FreverseStrand : boolean;
  Protected
    //Property setters
    Procedure SetreferenceName(AIndex : Integer; const AValue : String); virtual;
    Procedure Setposition(AIndex : Integer; const AValue : String); virtual;
    Procedure SetreverseStrand(AIndex : Integer; const AValue : boolean); virtual;
  Public
  Published
    Property referenceName : String Index 0 Read FreferenceName Write SetreferenceName;
    Property position : String Index 8 Read Fposition Write Setposition;
    Property reverseStrand : boolean Index 16 Read FreverseStrand Write SetreverseStrand;
  end;
  TPositionClass = Class of TPosition;
  
  { --------------------------------------------------------------------
    TCigarUnit
    --------------------------------------------------------------------}
  
  TCigarUnit = Class(TGoogleBaseObject)
  Private
    Foperation : String;
    FoperationLength : String;
    FreferenceSequence : String;
  Protected
    //Property setters
    Procedure Setoperation(AIndex : Integer; const AValue : String); virtual;
    Procedure SetoperationLength(AIndex : Integer; const AValue : String); virtual;
    Procedure SetreferenceSequence(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property operation : String Index 0 Read Foperation Write Setoperation;
    Property operationLength : String Index 8 Read FoperationLength Write SetoperationLength;
    Property referenceSequence : String Index 16 Read FreferenceSequence Write SetreferenceSequence;
  end;
  TCigarUnitClass = Class of TCigarUnit;
  
  { --------------------------------------------------------------------
    TStreamReadsRequest
    --------------------------------------------------------------------}
  
  TStreamReadsRequest = Class(TGoogleBaseObject)
  Private
    FprojectId : String;
    FreadGroupSetId : String;
    FreferenceName : String;
    Fstart : String;
    F_end : String;
    Fshard : integer;
    FtotalShards : integer;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetprojectId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetreadGroupSetId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetreferenceName(AIndex : Integer; const AValue : String); virtual;
    Procedure Setstart(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_end(AIndex : Integer; const AValue : String); virtual;
    Procedure Setshard(AIndex : Integer; const AValue : integer); virtual;
    Procedure SettotalShards(AIndex : Integer; const AValue : integer); virtual;
  Public
  Published
    Property projectId : String Index 0 Read FprojectId Write SetprojectId;
    Property readGroupSetId : String Index 8 Read FreadGroupSetId Write SetreadGroupSetId;
    Property referenceName : String Index 16 Read FreferenceName Write SetreferenceName;
    Property start : String Index 24 Read Fstart Write Setstart;
    Property _end : String Index 32 Read F_end Write Set_end;
    Property shard : integer Index 40 Read Fshard Write Setshard;
    Property totalShards : integer Index 48 Read FtotalShards Write SettotalShards;
  end;
  TStreamReadsRequestClass = Class of TStreamReadsRequest;
  
  { --------------------------------------------------------------------
    TStreamReadsResponse
    --------------------------------------------------------------------}
  
  TStreamReadsResponse = Class(TGoogleBaseObject)
  Private
    Falignments : TStreamReadsResponseTypealignmentsArray;
  Protected
    //Property setters
    Procedure Setalignments(AIndex : Integer; const AValue : TStreamReadsResponseTypealignmentsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property alignments : TStreamReadsResponseTypealignmentsArray Index 0 Read Falignments Write Setalignments;
  end;
  TStreamReadsResponseClass = Class of TStreamReadsResponse;
  
  { --------------------------------------------------------------------
    TSearchReferenceSetsRequest
    --------------------------------------------------------------------}
  
  TSearchReferenceSetsRequest = Class(TGoogleBaseObject)
  Private
    Fmd5checksums : TStringArray;
    Faccessions : TStringArray;
    FassemblyId : String;
    FpageToken : String;
    FpageSize : integer;
  Protected
    //Property setters
    Procedure Setmd5checksums(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure Setaccessions(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetassemblyId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetpageToken(AIndex : Integer; const AValue : String); virtual;
    Procedure SetpageSize(AIndex : Integer; const AValue : integer); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property md5checksums : TStringArray Index 0 Read Fmd5checksums Write Setmd5checksums;
    Property accessions : TStringArray Index 8 Read Faccessions Write Setaccessions;
    Property assemblyId : String Index 16 Read FassemblyId Write SetassemblyId;
    Property pageToken : String Index 24 Read FpageToken Write SetpageToken;
    Property pageSize : integer Index 32 Read FpageSize Write SetpageSize;
  end;
  TSearchReferenceSetsRequestClass = Class of TSearchReferenceSetsRequest;
  
  { --------------------------------------------------------------------
    TSearchReferenceSetsResponse
    --------------------------------------------------------------------}
  
  TSearchReferenceSetsResponse = Class(TGoogleBaseObject)
  Private
    FreferenceSets : TSearchReferenceSetsResponseTypereferenceSetsArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure SetreferenceSets(AIndex : Integer; const AValue : TSearchReferenceSetsResponseTypereferenceSetsArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property referenceSets : TSearchReferenceSetsResponseTypereferenceSetsArray Index 0 Read FreferenceSets Write SetreferenceSets;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TSearchReferenceSetsResponseClass = Class of TSearchReferenceSetsResponse;
  
  { --------------------------------------------------------------------
    TReferenceSet
    --------------------------------------------------------------------}
  
  TReferenceSet = Class(TGoogleBaseObject)
  Private
    Fid : String;
    FreferenceIds : TStringArray;
    Fmd5checksum : String;
    FncbiTaxonId : integer;
    Fdescription : String;
    FassemblyId : String;
    FsourceUri : String;
    FsourceAccessions : TStringArray;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure SetreferenceIds(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure Setmd5checksum(AIndex : Integer; const AValue : String); virtual;
    Procedure SetncbiTaxonId(AIndex : Integer; const AValue : integer); virtual;
    Procedure Setdescription(AIndex : Integer; const AValue : String); virtual;
    Procedure SetassemblyId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetsourceUri(AIndex : Integer; const AValue : String); virtual;
    Procedure SetsourceAccessions(AIndex : Integer; const AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property referenceIds : TStringArray Index 8 Read FreferenceIds Write SetreferenceIds;
    Property md5checksum : String Index 16 Read Fmd5checksum Write Setmd5checksum;
    Property ncbiTaxonId : integer Index 24 Read FncbiTaxonId Write SetncbiTaxonId;
    Property description : String Index 32 Read Fdescription Write Setdescription;
    Property assemblyId : String Index 40 Read FassemblyId Write SetassemblyId;
    Property sourceUri : String Index 48 Read FsourceUri Write SetsourceUri;
    Property sourceAccessions : TStringArray Index 56 Read FsourceAccessions Write SetsourceAccessions;
  end;
  TReferenceSetClass = Class of TReferenceSet;
  
  { --------------------------------------------------------------------
    TSearchReferencesRequest
    --------------------------------------------------------------------}
  
  TSearchReferencesRequest = Class(TGoogleBaseObject)
  Private
    Fmd5checksums : TStringArray;
    Faccessions : TStringArray;
    FreferenceSetId : String;
    FpageToken : String;
    FpageSize : integer;
  Protected
    //Property setters
    Procedure Setmd5checksums(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure Setaccessions(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetreferenceSetId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetpageToken(AIndex : Integer; const AValue : String); virtual;
    Procedure SetpageSize(AIndex : Integer; const AValue : integer); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property md5checksums : TStringArray Index 0 Read Fmd5checksums Write Setmd5checksums;
    Property accessions : TStringArray Index 8 Read Faccessions Write Setaccessions;
    Property referenceSetId : String Index 16 Read FreferenceSetId Write SetreferenceSetId;
    Property pageToken : String Index 24 Read FpageToken Write SetpageToken;
    Property pageSize : integer Index 32 Read FpageSize Write SetpageSize;
  end;
  TSearchReferencesRequestClass = Class of TSearchReferencesRequest;
  
  { --------------------------------------------------------------------
    TSearchReferencesResponse
    --------------------------------------------------------------------}
  
  TSearchReferencesResponse = Class(TGoogleBaseObject)
  Private
    Freferences : TSearchReferencesResponseTypereferencesArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setreferences(AIndex : Integer; const AValue : TSearchReferencesResponseTypereferencesArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property references : TSearchReferencesResponseTypereferencesArray Index 0 Read Freferences Write Setreferences;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TSearchReferencesResponseClass = Class of TSearchReferencesResponse;
  
  { --------------------------------------------------------------------
    TReference
    --------------------------------------------------------------------}
  
  TReference = Class(TGoogleBaseObject)
  Private
    Fid : String;
    F_length : String;
    Fmd5checksum : String;
    Fname : String;
    FsourceUri : String;
    FsourceAccessions : TStringArray;
    FncbiTaxonId : integer;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_length(AIndex : Integer; const AValue : String); virtual;
    Procedure Setmd5checksum(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure SetsourceUri(AIndex : Integer; const AValue : String); virtual;
    Procedure SetsourceAccessions(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetncbiTaxonId(AIndex : Integer; const AValue : integer); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property _length : String Index 8 Read F_length Write Set_length;
    Property md5checksum : String Index 16 Read Fmd5checksum Write Setmd5checksum;
    Property name : String Index 24 Read Fname Write Setname;
    Property sourceUri : String Index 32 Read FsourceUri Write SetsourceUri;
    Property sourceAccessions : TStringArray Index 40 Read FsourceAccessions Write SetsourceAccessions;
    Property ncbiTaxonId : integer Index 48 Read FncbiTaxonId Write SetncbiTaxonId;
  end;
  TReferenceClass = Class of TReference;
  
  { --------------------------------------------------------------------
    TListBasesResponse
    --------------------------------------------------------------------}
  
  TListBasesResponse = Class(TGoogleBaseObject)
  Private
    Foffset : String;
    Fsequence : String;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setoffset(AIndex : Integer; const AValue : String); virtual;
    Procedure Setsequence(AIndex : Integer; const AValue : String); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property offset : String Index 0 Read Foffset Write Setoffset;
    Property sequence : String Index 8 Read Fsequence Write Setsequence;
    Property nextPageToken : String Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TListBasesResponseClass = Class of TListBasesResponse;
  
  { --------------------------------------------------------------------
    TImportVariantsRequestTypeinfoMergeConfig
    --------------------------------------------------------------------}
  
  TImportVariantsRequestTypeinfoMergeConfig = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TImportVariantsRequestTypeinfoMergeConfigClass = Class of TImportVariantsRequestTypeinfoMergeConfig;
  
  { --------------------------------------------------------------------
    TImportVariantsRequest
    --------------------------------------------------------------------}
  
  TImportVariantsRequest = Class(TGoogleBaseObject)
  Private
    FvariantSetId : String;
    FsourceUris : TStringArray;
    Fformat : String;
    FnormalizeReferenceNames : boolean;
    FinfoMergeConfig : TImportVariantsRequestTypeinfoMergeConfig;
  Protected
    //Property setters
    Procedure SetvariantSetId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetsourceUris(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure Setformat(AIndex : Integer; const AValue : String); virtual;
    Procedure SetnormalizeReferenceNames(AIndex : Integer; const AValue : boolean); virtual;
    Procedure SetinfoMergeConfig(AIndex : Integer; const AValue : TImportVariantsRequestTypeinfoMergeConfig); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property variantSetId : String Index 0 Read FvariantSetId Write SetvariantSetId;
    Property sourceUris : TStringArray Index 8 Read FsourceUris Write SetsourceUris;
    Property format : String Index 16 Read Fformat Write Setformat;
    Property normalizeReferenceNames : boolean Index 24 Read FnormalizeReferenceNames Write SetnormalizeReferenceNames;
    Property infoMergeConfig : TImportVariantsRequestTypeinfoMergeConfig Index 32 Read FinfoMergeConfig Write SetinfoMergeConfig;
  end;
  TImportVariantsRequestClass = Class of TImportVariantsRequest;
  
  { --------------------------------------------------------------------
    TVariantSet
    --------------------------------------------------------------------}
  
  TVariantSet = Class(TGoogleBaseObject)
  Private
    FdatasetId : String;
    Fid : String;
    FreferenceSetId : String;
    FreferenceBounds : TVariantSetTypereferenceBoundsArray;
    Fmetadata : TVariantSetTypemetadataArray;
    Fname : String;
    Fdescription : String;
  Protected
    //Property setters
    Procedure SetdatasetId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure SetreferenceSetId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetreferenceBounds(AIndex : Integer; const AValue : TVariantSetTypereferenceBoundsArray); virtual;
    Procedure Setmetadata(AIndex : Integer; const AValue : TVariantSetTypemetadataArray); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property datasetId : String Index 0 Read FdatasetId Write SetdatasetId;
    Property id : String Index 8 Read Fid Write Setid;
    Property referenceSetId : String Index 16 Read FreferenceSetId Write SetreferenceSetId;
    Property referenceBounds : TVariantSetTypereferenceBoundsArray Index 24 Read FreferenceBounds Write SetreferenceBounds;
    Property metadata : TVariantSetTypemetadataArray Index 32 Read Fmetadata Write Setmetadata;
    Property name : String Index 40 Read Fname Write Setname;
    Property description : String Index 48 Read Fdescription Write Setdescription;
  end;
  TVariantSetClass = Class of TVariantSet;
  
  { --------------------------------------------------------------------
    TReferenceBound
    --------------------------------------------------------------------}
  
  TReferenceBound = Class(TGoogleBaseObject)
  Private
    FreferenceName : String;
    FupperBound : String;
  Protected
    //Property setters
    Procedure SetreferenceName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetupperBound(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property referenceName : String Index 0 Read FreferenceName Write SetreferenceName;
    Property upperBound : String Index 8 Read FupperBound Write SetupperBound;
  end;
  TReferenceBoundClass = Class of TReferenceBound;
  
  { --------------------------------------------------------------------
    TVariantSetMetadataTypeinfo
    --------------------------------------------------------------------}
  
  TVariantSetMetadataTypeinfo = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TVariantSetMetadataTypeinfoClass = Class of TVariantSetMetadataTypeinfo;
  
  { --------------------------------------------------------------------
    TVariantSetMetadata
    --------------------------------------------------------------------}
  
  TVariantSetMetadata = Class(TGoogleBaseObject)
  Private
    Fkey : String;
    Fvalue : String;
    Fid : String;
    F_type : String;
    Fnumber : String;
    Fdescription : String;
    Finfo : TVariantSetMetadataTypeinfo;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setkey(AIndex : Integer; const AValue : String); virtual;
    Procedure Setvalue(AIndex : Integer; const AValue : String); virtual;
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; const AValue : String); virtual;
    Procedure Setnumber(AIndex : Integer; const AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; const AValue : String); virtual;
    Procedure Setinfo(AIndex : Integer; const AValue : TVariantSetMetadataTypeinfo); virtual;
  Public
  Published
    Property key : String Index 0 Read Fkey Write Setkey;
    Property value : String Index 8 Read Fvalue Write Setvalue;
    Property id : String Index 16 Read Fid Write Setid;
    Property _type : String Index 24 Read F_type Write Set_type;
    Property number : String Index 32 Read Fnumber Write Setnumber;
    Property description : String Index 40 Read Fdescription Write Setdescription;
    Property info : TVariantSetMetadataTypeinfo Index 48 Read Finfo Write Setinfo;
  end;
  TVariantSetMetadataClass = Class of TVariantSetMetadata;
  
  { --------------------------------------------------------------------
    TExportVariantSetRequest
    --------------------------------------------------------------------}
  
  TExportVariantSetRequest = Class(TGoogleBaseObject)
  Private
    FcallSetIds : TStringArray;
    FprojectId : String;
    Fformat : String;
    FbigqueryDataset : String;
    FbigqueryTable : String;
  Protected
    //Property setters
    Procedure SetcallSetIds(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetprojectId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setformat(AIndex : Integer; const AValue : String); virtual;
    Procedure SetbigqueryDataset(AIndex : Integer; const AValue : String); virtual;
    Procedure SetbigqueryTable(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property callSetIds : TStringArray Index 0 Read FcallSetIds Write SetcallSetIds;
    Property projectId : String Index 8 Read FprojectId Write SetprojectId;
    Property format : String Index 16 Read Fformat Write Setformat;
    Property bigqueryDataset : String Index 24 Read FbigqueryDataset Write SetbigqueryDataset;
    Property bigqueryTable : String Index 32 Read FbigqueryTable Write SetbigqueryTable;
  end;
  TExportVariantSetRequestClass = Class of TExportVariantSetRequest;
  
  { --------------------------------------------------------------------
    TSearchVariantSetsRequest
    --------------------------------------------------------------------}
  
  TSearchVariantSetsRequest = Class(TGoogleBaseObject)
  Private
    FdatasetIds : TStringArray;
    FpageToken : String;
    FpageSize : integer;
  Protected
    //Property setters
    Procedure SetdatasetIds(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetpageToken(AIndex : Integer; const AValue : String); virtual;
    Procedure SetpageSize(AIndex : Integer; const AValue : integer); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property datasetIds : TStringArray Index 0 Read FdatasetIds Write SetdatasetIds;
    Property pageToken : String Index 8 Read FpageToken Write SetpageToken;
    Property pageSize : integer Index 16 Read FpageSize Write SetpageSize;
  end;
  TSearchVariantSetsRequestClass = Class of TSearchVariantSetsRequest;
  
  { --------------------------------------------------------------------
    TSearchVariantSetsResponse
    --------------------------------------------------------------------}
  
  TSearchVariantSetsResponse = Class(TGoogleBaseObject)
  Private
    FvariantSets : TSearchVariantSetsResponseTypevariantSetsArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure SetvariantSets(AIndex : Integer; const AValue : TSearchVariantSetsResponseTypevariantSetsArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property variantSets : TSearchVariantSetsResponseTypevariantSetsArray Index 0 Read FvariantSets Write SetvariantSets;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TSearchVariantSetsResponseClass = Class of TSearchVariantSetsResponse;
  
  { --------------------------------------------------------------------
    TSearchVariantsRequest
    --------------------------------------------------------------------}
  
  TSearchVariantsRequest = Class(TGoogleBaseObject)
  Private
    FvariantSetIds : TStringArray;
    FvariantName : String;
    FcallSetIds : TStringArray;
    FreferenceName : String;
    Fstart : String;
    F_end : String;
    FpageToken : String;
    FpageSize : integer;
    FmaxCalls : integer;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetvariantSetIds(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetvariantName(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcallSetIds(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetreferenceName(AIndex : Integer; const AValue : String); virtual;
    Procedure Setstart(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_end(AIndex : Integer; const AValue : String); virtual;
    Procedure SetpageToken(AIndex : Integer; const AValue : String); virtual;
    Procedure SetpageSize(AIndex : Integer; const AValue : integer); virtual;
    Procedure SetmaxCalls(AIndex : Integer; const AValue : integer); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property variantSetIds : TStringArray Index 0 Read FvariantSetIds Write SetvariantSetIds;
    Property variantName : String Index 8 Read FvariantName Write SetvariantName;
    Property callSetIds : TStringArray Index 16 Read FcallSetIds Write SetcallSetIds;
    Property referenceName : String Index 24 Read FreferenceName Write SetreferenceName;
    Property start : String Index 32 Read Fstart Write Setstart;
    Property _end : String Index 40 Read F_end Write Set_end;
    Property pageToken : String Index 48 Read FpageToken Write SetpageToken;
    Property pageSize : integer Index 56 Read FpageSize Write SetpageSize;
    Property maxCalls : integer Index 64 Read FmaxCalls Write SetmaxCalls;
  end;
  TSearchVariantsRequestClass = Class of TSearchVariantsRequest;
  
  { --------------------------------------------------------------------
    TSearchVariantsResponse
    --------------------------------------------------------------------}
  
  TSearchVariantsResponse = Class(TGoogleBaseObject)
  Private
    Fvariants : TSearchVariantsResponseTypevariantsArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setvariants(AIndex : Integer; const AValue : TSearchVariantsResponseTypevariantsArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property variants : TSearchVariantsResponseTypevariantsArray Index 0 Read Fvariants Write Setvariants;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TSearchVariantsResponseClass = Class of TSearchVariantsResponse;
  
  { --------------------------------------------------------------------
    TVariantTypeinfo
    --------------------------------------------------------------------}
  
  TVariantTypeinfo = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TVariantTypeinfoClass = Class of TVariantTypeinfo;
  
  { --------------------------------------------------------------------
    TVariant
    --------------------------------------------------------------------}
  
  TVariant = Class(TGoogleBaseObject)
  Private
    FvariantSetId : String;
    Fid : String;
    Fnames : TStringArray;
    Fcreated : String;
    FreferenceName : String;
    Fstart : String;
    F_end : String;
    FreferenceBases : String;
    FalternateBases : TStringArray;
    Fquality : double;
    Ffilter : TStringArray;
    Finfo : TVariantTypeinfo;
    Fcalls : TVariantTypecallsArray;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetvariantSetId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure Setnames(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure Setcreated(AIndex : Integer; const AValue : String); virtual;
    Procedure SetreferenceName(AIndex : Integer; const AValue : String); virtual;
    Procedure Setstart(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_end(AIndex : Integer; const AValue : String); virtual;
    Procedure SetreferenceBases(AIndex : Integer; const AValue : String); virtual;
    Procedure SetalternateBases(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure Setquality(AIndex : Integer; const AValue : double); virtual;
    Procedure Setfilter(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure Setinfo(AIndex : Integer; const AValue : TVariantTypeinfo); virtual;
    Procedure Setcalls(AIndex : Integer; const AValue : TVariantTypecallsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property variantSetId : String Index 0 Read FvariantSetId Write SetvariantSetId;
    Property id : String Index 8 Read Fid Write Setid;
    Property names : TStringArray Index 16 Read Fnames Write Setnames;
    Property created : String Index 24 Read Fcreated Write Setcreated;
    Property referenceName : String Index 32 Read FreferenceName Write SetreferenceName;
    Property start : String Index 40 Read Fstart Write Setstart;
    Property _end : String Index 48 Read F_end Write Set_end;
    Property referenceBases : String Index 56 Read FreferenceBases Write SetreferenceBases;
    Property alternateBases : TStringArray Index 64 Read FalternateBases Write SetalternateBases;
    Property quality : double Index 72 Read Fquality Write Setquality;
    Property filter : TStringArray Index 80 Read Ffilter Write Setfilter;
    Property info : TVariantTypeinfo Index 88 Read Finfo Write Setinfo;
    Property calls : TVariantTypecallsArray Index 96 Read Fcalls Write Setcalls;
  end;
  TVariantClass = Class of TVariant;
  
  { --------------------------------------------------------------------
    TVariantCallTypeinfo
    --------------------------------------------------------------------}
  
  TVariantCallTypeinfo = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TVariantCallTypeinfoClass = Class of TVariantCallTypeinfo;
  
  { --------------------------------------------------------------------
    TVariantCall
    --------------------------------------------------------------------}
  
  TVariantCall = Class(TGoogleBaseObject)
  Private
    FcallSetId : String;
    FcallSetName : String;
    Fgenotype : TintegerArray;
    Fphaseset : String;
    FgenotypeLikelihood : TdoubleArray;
    Finfo : TVariantCallTypeinfo;
  Protected
    //Property setters
    Procedure SetcallSetId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcallSetName(AIndex : Integer; const AValue : String); virtual;
    Procedure Setgenotype(AIndex : Integer; const AValue : TintegerArray); virtual;
    Procedure Setphaseset(AIndex : Integer; const AValue : String); virtual;
    Procedure SetgenotypeLikelihood(AIndex : Integer; const AValue : TdoubleArray); virtual;
    Procedure Setinfo(AIndex : Integer; const AValue : TVariantCallTypeinfo); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property callSetId : String Index 0 Read FcallSetId Write SetcallSetId;
    Property callSetName : String Index 8 Read FcallSetName Write SetcallSetName;
    Property genotype : TintegerArray Index 16 Read Fgenotype Write Setgenotype;
    Property phaseset : String Index 24 Read Fphaseset Write Setphaseset;
    Property genotypeLikelihood : TdoubleArray Index 32 Read FgenotypeLikelihood Write SetgenotypeLikelihood;
    Property info : TVariantCallTypeinfo Index 40 Read Finfo Write Setinfo;
  end;
  TVariantCallClass = Class of TVariantCall;
  
  { --------------------------------------------------------------------
    TMergeVariantsRequestTypeinfoMergeConfig
    --------------------------------------------------------------------}
  
  TMergeVariantsRequestTypeinfoMergeConfig = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TMergeVariantsRequestTypeinfoMergeConfigClass = Class of TMergeVariantsRequestTypeinfoMergeConfig;
  
  { --------------------------------------------------------------------
    TMergeVariantsRequest
    --------------------------------------------------------------------}
  
  TMergeVariantsRequest = Class(TGoogleBaseObject)
  Private
    FvariantSetId : String;
    Fvariants : TMergeVariantsRequestTypevariantsArray;
    FinfoMergeConfig : TMergeVariantsRequestTypeinfoMergeConfig;
  Protected
    //Property setters
    Procedure SetvariantSetId(AIndex : Integer; const AValue : String); virtual;
    Procedure Setvariants(AIndex : Integer; const AValue : TMergeVariantsRequestTypevariantsArray); virtual;
    Procedure SetinfoMergeConfig(AIndex : Integer; const AValue : TMergeVariantsRequestTypeinfoMergeConfig); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property variantSetId : String Index 0 Read FvariantSetId Write SetvariantSetId;
    Property variants : TMergeVariantsRequestTypevariantsArray Index 8 Read Fvariants Write Setvariants;
    Property infoMergeConfig : TMergeVariantsRequestTypeinfoMergeConfig Index 16 Read FinfoMergeConfig Write SetinfoMergeConfig;
  end;
  TMergeVariantsRequestClass = Class of TMergeVariantsRequest;
  
  { --------------------------------------------------------------------
    TSearchCallSetsRequest
    --------------------------------------------------------------------}
  
  TSearchCallSetsRequest = Class(TGoogleBaseObject)
  Private
    FvariantSetIds : TStringArray;
    Fname : String;
    FpageToken : String;
    FpageSize : integer;
  Protected
    //Property setters
    Procedure SetvariantSetIds(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure SetpageToken(AIndex : Integer; const AValue : String); virtual;
    Procedure SetpageSize(AIndex : Integer; const AValue : integer); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property variantSetIds : TStringArray Index 0 Read FvariantSetIds Write SetvariantSetIds;
    Property name : String Index 8 Read Fname Write Setname;
    Property pageToken : String Index 16 Read FpageToken Write SetpageToken;
    Property pageSize : integer Index 24 Read FpageSize Write SetpageSize;
  end;
  TSearchCallSetsRequestClass = Class of TSearchCallSetsRequest;
  
  { --------------------------------------------------------------------
    TSearchCallSetsResponse
    --------------------------------------------------------------------}
  
  TSearchCallSetsResponse = Class(TGoogleBaseObject)
  Private
    FcallSets : TSearchCallSetsResponseTypecallSetsArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure SetcallSets(AIndex : Integer; const AValue : TSearchCallSetsResponseTypecallSetsArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property callSets : TSearchCallSetsResponseTypecallSetsArray Index 0 Read FcallSets Write SetcallSets;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TSearchCallSetsResponseClass = Class of TSearchCallSetsResponse;
  
  { --------------------------------------------------------------------
    TCallSetTypeinfo
    --------------------------------------------------------------------}
  
  TCallSetTypeinfo = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TCallSetTypeinfoClass = Class of TCallSetTypeinfo;
  
  { --------------------------------------------------------------------
    TCallSet
    --------------------------------------------------------------------}
  
  TCallSet = Class(TGoogleBaseObject)
  Private
    Fid : String;
    Fname : String;
    FsampleId : String;
    FvariantSetIds : TStringArray;
    Fcreated : String;
    Finfo : TCallSetTypeinfo;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; const AValue : String); virtual;
    Procedure Setname(AIndex : Integer; const AValue : String); virtual;
    Procedure SetsampleId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetvariantSetIds(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure Setcreated(AIndex : Integer; const AValue : String); virtual;
    Procedure Setinfo(AIndex : Integer; const AValue : TCallSetTypeinfo); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property name : String Index 8 Read Fname Write Setname;
    Property sampleId : String Index 16 Read FsampleId Write SetsampleId;
    Property variantSetIds : TStringArray Index 24 Read FvariantSetIds Write SetvariantSetIds;
    Property created : String Index 32 Read Fcreated Write Setcreated;
    Property info : TCallSetTypeinfo Index 40 Read Finfo Write Setinfo;
  end;
  TCallSetClass = Class of TCallSet;
  
  { --------------------------------------------------------------------
    TStreamVariantsRequest
    --------------------------------------------------------------------}
  
  TStreamVariantsRequest = Class(TGoogleBaseObject)
  Private
    FprojectId : String;
    FvariantSetId : String;
    FcallSetIds : TStringArray;
    FreferenceName : String;
    Fstart : String;
    F_end : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetprojectId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetvariantSetId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcallSetIds(AIndex : Integer; const AValue : TStringArray); virtual;
    Procedure SetreferenceName(AIndex : Integer; const AValue : String); virtual;
    Procedure Setstart(AIndex : Integer; const AValue : String); virtual;
    Procedure Set_end(AIndex : Integer; const AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property projectId : String Index 0 Read FprojectId Write SetprojectId;
    Property variantSetId : String Index 8 Read FvariantSetId Write SetvariantSetId;
    Property callSetIds : TStringArray Index 16 Read FcallSetIds Write SetcallSetIds;
    Property referenceName : String Index 24 Read FreferenceName Write SetreferenceName;
    Property start : String Index 32 Read Fstart Write Setstart;
    Property _end : String Index 40 Read F_end Write Set_end;
  end;
  TStreamVariantsRequestClass = Class of TStreamVariantsRequest;
  
  { --------------------------------------------------------------------
    TStreamVariantsResponse
    --------------------------------------------------------------------}
  
  TStreamVariantsResponse = Class(TGoogleBaseObject)
  Private
    Fvariants : TStreamVariantsResponseTypevariantsArray;
  Protected
    //Property setters
    Procedure Setvariants(AIndex : Integer; const AValue : TStreamVariantsResponseTypevariantsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property variants : TStreamVariantsResponseTypevariantsArray Index 0 Read Fvariants Write Setvariants;
  end;
  TStreamVariantsResponseClass = Class of TStreamVariantsResponse;
  
  { --------------------------------------------------------------------
    TImportReadGroupSetsResponse
    --------------------------------------------------------------------}
  
  TImportReadGroupSetsResponse = Class(TGoogleBaseObject)
  Private
    FreadGroupSetIds : TStringArray;
  Protected
    //Property setters
    Procedure SetreadGroupSetIds(AIndex : Integer; const AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property readGroupSetIds : TStringArray Index 0 Read FreadGroupSetIds Write SetreadGroupSetIds;
  end;
  TImportReadGroupSetsResponseClass = Class of TImportReadGroupSetsResponse;
  
  { --------------------------------------------------------------------
    TImportVariantsResponse
    --------------------------------------------------------------------}
  
  TImportVariantsResponse = Class(TGoogleBaseObject)
  Private
    FcallSetIds : TStringArray;
  Protected
    //Property setters
    Procedure SetcallSetIds(AIndex : Integer; const AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property callSetIds : TStringArray Index 0 Read FcallSetIds Write SetcallSetIds;
  end;
  TImportVariantsResponseClass = Class of TImportVariantsResponse;
  
  { --------------------------------------------------------------------
    TOperationMetadataTyperequest
    --------------------------------------------------------------------}
  
  TOperationMetadataTyperequest = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TOperationMetadataTyperequestClass = Class of TOperationMetadataTyperequest;
  
  { --------------------------------------------------------------------
    TOperationMetadata
    --------------------------------------------------------------------}
  
  TOperationMetadata = Class(TGoogleBaseObject)
  Private
    FprojectId : String;
    FcreateTime : String;
    FendTime : String;
    Frequest : TOperationMetadataTyperequest;
    Fevents : TOperationMetadataTypeeventsArray;
  Protected
    //Property setters
    Procedure SetprojectId(AIndex : Integer; const AValue : String); virtual;
    Procedure SetcreateTime(AIndex : Integer; const AValue : String); virtual;
    Procedure SetendTime(AIndex : Integer; const AValue : String); virtual;
    Procedure Setrequest(AIndex : Integer; const AValue : TOperationMetadataTyperequest); virtual;
    Procedure Setevents(AIndex : Integer; const AValue : TOperationMetadataTypeeventsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property projectId : String Index 0 Read FprojectId Write SetprojectId;
    Property createTime : String Index 8 Read FcreateTime Write SetcreateTime;
    Property endTime : String Index 16 Read FendTime Write SetendTime;
    Property request : TOperationMetadataTyperequest Index 24 Read Frequest Write Setrequest;
    Property events : TOperationMetadataTypeeventsArray Index 32 Read Fevents Write Setevents;
  end;
  TOperationMetadataClass = Class of TOperationMetadata;
  
  { --------------------------------------------------------------------
    TOperationEvent
    --------------------------------------------------------------------}
  
  TOperationEvent = Class(TGoogleBaseObject)
  Private
    Fdescription : String;
  Protected
    //Property setters
    Procedure Setdescription(AIndex : Integer; const AValue : String); virtual;
  Public
  Published
    Property description : String Index 0 Read Fdescription Write Setdescription;
  end;
  TOperationEventClass = Class of TOperationEvent;
  
  { --------------------------------------------------------------------
    TAnnotationsetsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAnnotationsetsResource, method Update
  
  TAnnotationsetsUpdateOptions = Record
    updateMask : String;
  end;
  
  TAnnotationsetsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Create(aAnnotationSet : TAnnotationSet) : TAnnotationSet;overload;
    Function Get(annotationSetId: string) : TAnnotationSet;
    Function Update(annotationSetId: string; aAnnotationSet : TAnnotationSet; AQuery : string  = '') : TAnnotationSet;
    Function Update(annotationSetId: string; aAnnotationSet : TAnnotationSet; AQuery : TAnnotationsetsupdateOptions) : TAnnotationSet;
    Function Delete(annotationSetId: string) : TEmpty;
    Function Search(aSearchAnnotationSetsRequest : TSearchAnnotationSetsRequest) : TSearchAnnotationSetsResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TAnnotationsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TAnnotationsResource, method Update
  
  TAnnotationsUpdateOptions = Record
    updateMask : String;
  end;
  
  TAnnotationsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Create(aAnnotation : TAnnotation) : TAnnotation;overload;
    Function BatchCreate(aBatchCreateAnnotationsRequest : TBatchCreateAnnotationsRequest) : TBatchCreateAnnotationsResponse;
    Function Get(annotationId: string) : TAnnotation;
    Function Update(annotationId: string; aAnnotation : TAnnotation; AQuery : string  = '') : TAnnotation;
    Function Update(annotationId: string; aAnnotation : TAnnotation; AQuery : TAnnotationsupdateOptions) : TAnnotation;
    Function Delete(annotationId: string) : TEmpty;
    Function Search(aSearchAnnotationsRequest : TSearchAnnotationsRequest) : TSearchAnnotationsResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TDatasetsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TDatasetsResource, method List
  
  TDatasetsListOptions = Record
    projectId : String;
    pageSize : integer;
    pageToken : String;
  end;
  
  
  //Optional query Options for TDatasetsResource, method Patch
  
  TDatasetsPatchOptions = Record
    updateMask : String;
  end;
  
  TDatasetsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(AQuery : string  = '') : TListDatasetsResponse;
    Function List(AQuery : TDatasetslistOptions) : TListDatasetsResponse;
    Function Create(aDataset : TDataset) : TDataset;overload;
    Function Get(datasetId: string) : TDataset;
    Function Patch(datasetId: string; aDataset : TDataset; AQuery : string  = '') : TDataset;
    Function Patch(datasetId: string; aDataset : TDataset; AQuery : TDatasetspatchOptions) : TDataset;
    Function Delete(datasetId: string) : TEmpty;
    Function Undelete(datasetId: string; aUndeleteDatasetRequest : TUndeleteDatasetRequest) : TDataset;
    Function SetIamPolicy(resource: string; aSetIamPolicyRequest : TSetIamPolicyRequest) : TPolicy;
    Function GetIamPolicy(resource: string; aGetIamPolicyRequest : TGetIamPolicyRequest) : TPolicy;
    Function TestIamPermissions(resource: string; aTestIamPermissionsRequest : TTestIamPermissionsRequest) : TTestIamPermissionsResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TOperationsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TOperationsResource, method List
  
  TOperationsListOptions = Record
    filter : String;
    pageSize : integer;
    pageToken : String;
  end;
  
  TOperationsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(_name: string) : TOperation;
    Function List(_name: string; AQuery : string  = '') : TListOperationsResponse;
    Function List(_name: string; AQuery : TOperationslistOptions) : TListOperationsResponse;
    Function Cancel(_name: string; aCancelOperationRequest : TCancelOperationRequest) : TEmpty;
  end;
  
  
  { --------------------------------------------------------------------
    TReadgroupsetsCoveragebucketsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TReadgroupsetsCoveragebucketsResource, method List
  
  TReadgroupsetsCoveragebucketsListOptions = Record
    referenceName : String;
    start : int64;
    _end : int64;
    targetBucketWidth : int64;
    pageToken : String;
    pageSize : integer;
  end;
  
  TReadgroupsetsCoveragebucketsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(readGroupSetId: string; AQuery : string  = '') : TListCoverageBucketsResponse;
    Function List(readGroupSetId: string; AQuery : TReadgroupsetsCoveragebucketslistOptions) : TListCoverageBucketsResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TReadgroupsetsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TReadgroupsetsResource, method Patch
  
  TReadgroupsetsPatchOptions = Record
    updateMask : String;
  end;
  
  TReadgroupsetsResource = Class(TGoogleResource)
  Private
    FCoveragebucketsInstance : TReadgroupsetsCoveragebucketsResource;
    Function GetCoveragebucketsInstance : TReadgroupsetsCoveragebucketsResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Import(aImportReadGroupSetsRequest : TImportReadGroupSetsRequest) : TOperation;
    Function Export(readGroupSetId: string; aExportReadGroupSetRequest : TExportReadGroupSetRequest) : TOperation;
    Function Search(aSearchReadGroupSetsRequest : TSearchReadGroupSetsRequest) : TSearchReadGroupSetsResponse;
    Function Patch(readGroupSetId: string; aReadGroupSet : TReadGroupSet; AQuery : string  = '') : TReadGroupSet;
    Function Patch(readGroupSetId: string; aReadGroupSet : TReadGroupSet; AQuery : TReadgroupsetspatchOptions) : TReadGroupSet;
    Function Delete(readGroupSetId: string) : TEmpty;
    Function Get(readGroupSetId: string) : TReadGroupSet;
    Function CreateCoveragebucketsResource(AOwner : TComponent) : TReadgroupsetsCoveragebucketsResource;virtual;overload;
    Function CreateCoveragebucketsResource : TReadgroupsetsCoveragebucketsResource;virtual;overload;
    Property CoveragebucketsResource : TReadgroupsetsCoveragebucketsResource Read GetCoveragebucketsInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TReadsResource
    --------------------------------------------------------------------}
  
  TReadsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Search(aSearchReadsRequest : TSearchReadsRequest) : TSearchReadsResponse;
    Function Stream(aStreamReadsRequest : TStreamReadsRequest) : TStreamReadsResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TReferencesetsResource
    --------------------------------------------------------------------}
  
  TReferencesetsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Search(aSearchReferenceSetsRequest : TSearchReferenceSetsRequest) : TSearchReferenceSetsResponse;
    Function Get(referenceSetId: string) : TReferenceSet;
  end;
  
  
  { --------------------------------------------------------------------
    TReferencesBasesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TReferencesBasesResource, method List
  
  TReferencesBasesListOptions = Record
    start : int64;
    _end : int64;
    pageToken : String;
    pageSize : integer;
  end;
  
  TReferencesBasesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function List(referenceId: string; AQuery : string  = '') : TListBasesResponse;
    Function List(referenceId: string; AQuery : TReferencesBaseslistOptions) : TListBasesResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TReferencesResource
    --------------------------------------------------------------------}
  
  TReferencesResource = Class(TGoogleResource)
  Private
    FBasesInstance : TReferencesBasesResource;
    Function GetBasesInstance : TReferencesBasesResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Search(aSearchReferencesRequest : TSearchReferencesRequest) : TSearchReferencesResponse;
    Function Get(referenceId: string) : TReference;
    Function CreateBasesResource(AOwner : TComponent) : TReferencesBasesResource;virtual;overload;
    Function CreateBasesResource : TReferencesBasesResource;virtual;overload;
    Property BasesResource : TReferencesBasesResource Read GetBasesInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TVariantsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TVariantsResource, method Patch
  
  TVariantsPatchOptions = Record
    updateMask : String;
  end;
  
  TVariantsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Import(aImportVariantsRequest : TImportVariantsRequest) : TOperation;
    Function Search(aSearchVariantsRequest : TSearchVariantsRequest) : TSearchVariantsResponse;
    Function Create(aVariant : TVariant) : TVariant;overload;
    Function Patch(variantId: string; aVariant : TVariant; AQuery : string  = '') : TVariant;
    Function Patch(variantId: string; aVariant : TVariant; AQuery : TVariantspatchOptions) : TVariant;
    Function Delete(variantId: string) : TEmpty;
    Function Get(variantId: string) : TVariant;
    Function Merge(aMergeVariantsRequest : TMergeVariantsRequest) : TEmpty;
    Function Stream(aStreamVariantsRequest : TStreamVariantsRequest) : TStreamVariantsResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TVariantsetsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TVariantsetsResource, method Patch
  
  TVariantsetsPatchOptions = Record
    updateMask : String;
  end;
  
  TVariantsetsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Create(aVariantSet : TVariantSet) : TVariantSet;overload;
    Function Export(variantSetId: string; aExportVariantSetRequest : TExportVariantSetRequest) : TOperation;
    Function Get(variantSetId: string) : TVariantSet;
    Function Search(aSearchVariantSetsRequest : TSearchVariantSetsRequest) : TSearchVariantSetsResponse;
    Function Delete(variantSetId: string) : TEmpty;
    Function Patch(variantSetId: string; aVariantSet : TVariantSet; AQuery : string  = '') : TVariantSet;
    Function Patch(variantSetId: string; aVariantSet : TVariantSet; AQuery : TVariantsetspatchOptions) : TVariantSet;
  end;
  
  
  { --------------------------------------------------------------------
    TCallsetsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TCallsetsResource, method Patch
  
  TCallsetsPatchOptions = Record
    updateMask : String;
  end;
  
  TCallsetsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Search(aSearchCallSetsRequest : TSearchCallSetsRequest) : TSearchCallSetsResponse;
    Function Create(aCallSet : TCallSet) : TCallSet;overload;
    Function Patch(callSetId: string; aCallSet : TCallSet; AQuery : string  = '') : TCallSet;
    Function Patch(callSetId: string; aCallSet : TCallSet; AQuery : TCallsetspatchOptions) : TCallSet;
    Function Delete(callSetId: string) : TEmpty;
    Function Get(callSetId: string) : TCallSet;
  end;
  
  
  { --------------------------------------------------------------------
    TGenomicsAPI
    --------------------------------------------------------------------}
  
  TGenomicsAPI = Class(TGoogleAPI)
  Private
    FAnnotationsetsInstance : TAnnotationsetsResource;
    FAnnotationsInstance : TAnnotationsResource;
    FDatasetsInstance : TDatasetsResource;
    FOperationsInstance : TOperationsResource;
    FReadgroupsetsCoveragebucketsInstance : TReadgroupsetsCoveragebucketsResource;
    FReadgroupsetsInstance : TReadgroupsetsResource;
    FReadsInstance : TReadsResource;
    FReferencesetsInstance : TReferencesetsResource;
    FReferencesBasesInstance : TReferencesBasesResource;
    FReferencesInstance : TReferencesResource;
    FVariantsInstance : TVariantsResource;
    FVariantsetsInstance : TVariantsetsResource;
    FCallsetsInstance : TCallsetsResource;
    Function GetAnnotationsetsInstance : TAnnotationsetsResource;virtual;
    Function GetAnnotationsInstance : TAnnotationsResource;virtual;
    Function GetDatasetsInstance : TDatasetsResource;virtual;
    Function GetOperationsInstance : TOperationsResource;virtual;
    Function GetReadgroupsetsCoveragebucketsInstance : TReadgroupsetsCoveragebucketsResource;virtual;
    Function GetReadgroupsetsInstance : TReadgroupsetsResource;virtual;
    Function GetReadsInstance : TReadsResource;virtual;
    Function GetReferencesetsInstance : TReferencesetsResource;virtual;
    Function GetReferencesBasesInstance : TReferencesBasesResource;virtual;
    Function GetReferencesInstance : TReferencesResource;virtual;
    Function GetVariantsInstance : TVariantsResource;virtual;
    Function GetVariantsetsInstance : TVariantsetsResource;virtual;
    Function GetCallsetsInstance : TCallsetsResource;virtual;
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
    Function CreateAnnotationsetsResource(AOwner : TComponent) : TAnnotationsetsResource;virtual;overload;
    Function CreateAnnotationsetsResource : TAnnotationsetsResource;virtual;overload;
    Function CreateAnnotationsResource(AOwner : TComponent) : TAnnotationsResource;virtual;overload;
    Function CreateAnnotationsResource : TAnnotationsResource;virtual;overload;
    Function CreateDatasetsResource(AOwner : TComponent) : TDatasetsResource;virtual;overload;
    Function CreateDatasetsResource : TDatasetsResource;virtual;overload;
    Function CreateOperationsResource(AOwner : TComponent) : TOperationsResource;virtual;overload;
    Function CreateOperationsResource : TOperationsResource;virtual;overload;
    Function CreateReadgroupsetsCoveragebucketsResource(AOwner : TComponent) : TReadgroupsetsCoveragebucketsResource;virtual;overload;
    Function CreateReadgroupsetsCoveragebucketsResource : TReadgroupsetsCoveragebucketsResource;virtual;overload;
    Function CreateReadgroupsetsResource(AOwner : TComponent) : TReadgroupsetsResource;virtual;overload;
    Function CreateReadgroupsetsResource : TReadgroupsetsResource;virtual;overload;
    Function CreateReadsResource(AOwner : TComponent) : TReadsResource;virtual;overload;
    Function CreateReadsResource : TReadsResource;virtual;overload;
    Function CreateReferencesetsResource(AOwner : TComponent) : TReferencesetsResource;virtual;overload;
    Function CreateReferencesetsResource : TReferencesetsResource;virtual;overload;
    Function CreateReferencesBasesResource(AOwner : TComponent) : TReferencesBasesResource;virtual;overload;
    Function CreateReferencesBasesResource : TReferencesBasesResource;virtual;overload;
    Function CreateReferencesResource(AOwner : TComponent) : TReferencesResource;virtual;overload;
    Function CreateReferencesResource : TReferencesResource;virtual;overload;
    Function CreateVariantsResource(AOwner : TComponent) : TVariantsResource;virtual;overload;
    Function CreateVariantsResource : TVariantsResource;virtual;overload;
    Function CreateVariantsetsResource(AOwner : TComponent) : TVariantsetsResource;virtual;overload;
    Function CreateVariantsetsResource : TVariantsetsResource;virtual;overload;
    Function CreateCallsetsResource(AOwner : TComponent) : TCallsetsResource;virtual;overload;
    Function CreateCallsetsResource : TCallsetsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property AnnotationsetsResource : TAnnotationsetsResource Read GetAnnotationsetsInstance;
    Property AnnotationsResource : TAnnotationsResource Read GetAnnotationsInstance;
    Property DatasetsResource : TDatasetsResource Read GetDatasetsInstance;
    Property OperationsResource : TOperationsResource Read GetOperationsInstance;
    Property ReadgroupsetsCoveragebucketsResource : TReadgroupsetsCoveragebucketsResource Read GetReadgroupsetsCoveragebucketsInstance;
    Property ReadgroupsetsResource : TReadgroupsetsResource Read GetReadgroupsetsInstance;
    Property ReadsResource : TReadsResource Read GetReadsInstance;
    Property ReferencesetsResource : TReferencesetsResource Read GetReferencesetsInstance;
    Property ReferencesBasesResource : TReferencesBasesResource Read GetReferencesBasesInstance;
    Property ReferencesResource : TReferencesResource Read GetReferencesInstance;
    Property VariantsResource : TVariantsResource Read GetVariantsInstance;
    Property VariantsetsResource : TVariantsetsResource Read GetVariantsetsInstance;
    Property CallsetsResource : TCallsetsResource Read GetCallsetsInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TAnnotationSetTypeinfo
  --------------------------------------------------------------------}


Class Function TAnnotationSetTypeinfo.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TAnnotationSet
  --------------------------------------------------------------------}


Procedure TAnnotationSet.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationSet.SetdatasetId(AIndex : Integer; const AValue : String); 

begin
  If (FdatasetId=AValue) then exit;
  FdatasetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationSet.SetreferenceSetId(AIndex : Integer; const AValue : String); 

begin
  If (FreferenceSetId=AValue) then exit;
  FreferenceSetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationSet.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationSet.SetsourceUri(AIndex : Integer; const AValue : String); 

begin
  If (FsourceUri=AValue) then exit;
  FsourceUri:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationSet.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationSet.Setinfo(AIndex : Integer; const AValue : TAnnotationSetTypeinfo); 

begin
  If (Finfo=AValue) then exit;
  Finfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TAnnotationSet.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TEmpty
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TSearchAnnotationSetsRequest
  --------------------------------------------------------------------}


Procedure TSearchAnnotationSetsRequest.SetdatasetIds(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FdatasetIds=AValue) then exit;
  FdatasetIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchAnnotationSetsRequest.SetreferenceSetId(AIndex : Integer; const AValue : String); 

begin
  If (FreferenceSetId=AValue) then exit;
  FreferenceSetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchAnnotationSetsRequest.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchAnnotationSetsRequest.Settypes(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Ftypes=AValue) then exit;
  Ftypes:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchAnnotationSetsRequest.SetpageToken(AIndex : Integer; const AValue : String); 

begin
  If (FpageToken=AValue) then exit;
  FpageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchAnnotationSetsRequest.SetpageSize(AIndex : Integer; const AValue : integer); 

begin
  If (FpageSize=AValue) then exit;
  FpageSize:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSearchAnnotationSetsRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'datasetids' : SetLength(FdatasetIds,ALength);
  'types' : SetLength(Ftypes,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSearchAnnotationSetsResponse
  --------------------------------------------------------------------}


Procedure TSearchAnnotationSetsResponse.SetannotationSets(AIndex : Integer; const AValue : TSearchAnnotationSetsResponseTypeannotationSetsArray); 

begin
  If (FannotationSets=AValue) then exit;
  FannotationSets:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchAnnotationSetsResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSearchAnnotationSetsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'annotationsets' : SetLength(FannotationSets,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TAnnotationTypeinfo
  --------------------------------------------------------------------}


Class Function TAnnotationTypeinfo.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TAnnotation
  --------------------------------------------------------------------}


Procedure TAnnotation.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotation.SetannotationSetId(AIndex : Integer; const AValue : String); 

begin
  If (FannotationSetId=AValue) then exit;
  FannotationSetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotation.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotation.SetreferenceId(AIndex : Integer; const AValue : String); 

begin
  If (FreferenceId=AValue) then exit;
  FreferenceId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotation.SetreferenceName(AIndex : Integer; const AValue : String); 

begin
  If (FreferenceName=AValue) then exit;
  FreferenceName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotation.Setstart(AIndex : Integer; const AValue : String); 

begin
  If (Fstart=AValue) then exit;
  Fstart:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotation.Set_end(AIndex : Integer; const AValue : String); 

begin
  If (F_end=AValue) then exit;
  F_end:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotation.SetreverseStrand(AIndex : Integer; const AValue : boolean); 

begin
  If (FreverseStrand=AValue) then exit;
  FreverseStrand:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotation.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotation.Setvariant(AIndex : Integer; const AValue : TVariantAnnotation); 

begin
  If (Fvariant=AValue) then exit;
  Fvariant:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotation.Settranscript(AIndex : Integer; const AValue : TTranscript); 

begin
  If (Ftranscript=AValue) then exit;
  Ftranscript:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotation.Setinfo(AIndex : Integer; const AValue : TAnnotationTypeinfo); 

begin
  If (Finfo=AValue) then exit;
  Finfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TAnnotation.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_end' : Result:='end';
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TVariantAnnotation
  --------------------------------------------------------------------}


Procedure TVariantAnnotation.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariantAnnotation.Seteffect(AIndex : Integer; const AValue : String); 

begin
  If (Feffect=AValue) then exit;
  Feffect:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariantAnnotation.SetalternateBases(AIndex : Integer; const AValue : String); 

begin
  If (FalternateBases=AValue) then exit;
  FalternateBases:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariantAnnotation.SetgeneId(AIndex : Integer; const AValue : String); 

begin
  If (FgeneId=AValue) then exit;
  FgeneId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariantAnnotation.SettranscriptIds(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FtranscriptIds=AValue) then exit;
  FtranscriptIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariantAnnotation.Setconditions(AIndex : Integer; const AValue : TVariantAnnotationTypeconditionsArray); 

begin
  If (Fconditions=AValue) then exit;
  Fconditions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariantAnnotation.SetclinicalSignificance(AIndex : Integer; const AValue : String); 

begin
  If (FclinicalSignificance=AValue) then exit;
  FclinicalSignificance:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TVariantAnnotation.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;

//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TVariantAnnotation.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'transcriptids' : SetLength(FtranscriptIds,ALength);
  'conditions' : SetLength(Fconditions,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TClinicalCondition
  --------------------------------------------------------------------}


Procedure TClinicalCondition.Setnames(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fnames=AValue) then exit;
  Fnames:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TClinicalCondition.SetexternalIds(AIndex : Integer; const AValue : TClinicalConditionTypeexternalIdsArray); 

begin
  If (FexternalIds=AValue) then exit;
  FexternalIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TClinicalCondition.SetconceptId(AIndex : Integer; const AValue : String); 

begin
  If (FconceptId=AValue) then exit;
  FconceptId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TClinicalCondition.SetomimId(AIndex : Integer; const AValue : String); 

begin
  If (FomimId=AValue) then exit;
  FomimId:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TClinicalCondition.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'names' : SetLength(Fnames,ALength);
  'externalids' : SetLength(FexternalIds,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TExternalId
  --------------------------------------------------------------------}


Procedure TExternalId.SetsourceName(AIndex : Integer; const AValue : String); 

begin
  If (FsourceName=AValue) then exit;
  FsourceName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExternalId.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTranscript
  --------------------------------------------------------------------}


Procedure TTranscript.SetgeneId(AIndex : Integer; const AValue : String); 

begin
  If (FgeneId=AValue) then exit;
  FgeneId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTranscript.Setexons(AIndex : Integer; const AValue : TTranscriptTypeexonsArray); 

begin
  If (Fexons=AValue) then exit;
  Fexons:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTranscript.SetcodingSequence(AIndex : Integer; const AValue : TCodingSequence); 

begin
  If (FcodingSequence=AValue) then exit;
  FcodingSequence:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TTranscript.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'exons' : SetLength(Fexons,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TExon
  --------------------------------------------------------------------}


Procedure TExon.Setstart(AIndex : Integer; const AValue : String); 

begin
  If (Fstart=AValue) then exit;
  Fstart:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExon.Set_end(AIndex : Integer; const AValue : String); 

begin
  If (F_end=AValue) then exit;
  F_end:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExon.Setframe(AIndex : Integer; const AValue : integer); 

begin
  If (Fframe=AValue) then exit;
  Fframe:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TExon.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_end' : Result:='end';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TCodingSequence
  --------------------------------------------------------------------}


Procedure TCodingSequence.Setstart(AIndex : Integer; const AValue : String); 

begin
  If (Fstart=AValue) then exit;
  Fstart:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCodingSequence.Set_end(AIndex : Integer; const AValue : String); 

begin
  If (F_end=AValue) then exit;
  F_end:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TCodingSequence.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_end' : Result:='end';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TBatchCreateAnnotationsRequest
  --------------------------------------------------------------------}


Procedure TBatchCreateAnnotationsRequest.Setannotations(AIndex : Integer; const AValue : TBatchCreateAnnotationsRequestTypeannotationsArray); 

begin
  If (Fannotations=AValue) then exit;
  Fannotations:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TBatchCreateAnnotationsRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'annotations' : SetLength(Fannotations,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TBatchCreateAnnotationsResponse
  --------------------------------------------------------------------}


Procedure TBatchCreateAnnotationsResponse.Setentries(AIndex : Integer; const AValue : TBatchCreateAnnotationsResponseTypeentriesArray); 

begin
  If (Fentries=AValue) then exit;
  Fentries:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TBatchCreateAnnotationsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'entries' : SetLength(Fentries,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TEntry
  --------------------------------------------------------------------}


Procedure TEntry.Setstatus(AIndex : Integer; const AValue : TStatus); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TEntry.Setannotation(AIndex : Integer; const AValue : TAnnotation); 

begin
  If (Fannotation=AValue) then exit;
  Fannotation:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TStatusTypedetailsItem
  --------------------------------------------------------------------}


Class Function TStatusTypedetailsItem.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TStatus
  --------------------------------------------------------------------}


Procedure TStatus.Setcode(AIndex : Integer; const AValue : integer); 

begin
  If (Fcode=AValue) then exit;
  Fcode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStatus.Setmessage(AIndex : Integer; const AValue : String); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStatus.Setdetails(AIndex : Integer; const AValue : TStatusTypedetailsArray); 

begin
  If (Fdetails=AValue) then exit;
  Fdetails:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TStatus.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'details' : SetLength(Fdetails,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSearchAnnotationsRequest
  --------------------------------------------------------------------}


Procedure TSearchAnnotationsRequest.SetannotationSetIds(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FannotationSetIds=AValue) then exit;
  FannotationSetIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchAnnotationsRequest.SetreferenceId(AIndex : Integer; const AValue : String); 

begin
  If (FreferenceId=AValue) then exit;
  FreferenceId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchAnnotationsRequest.SetreferenceName(AIndex : Integer; const AValue : String); 

begin
  If (FreferenceName=AValue) then exit;
  FreferenceName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchAnnotationsRequest.Setstart(AIndex : Integer; const AValue : String); 

begin
  If (Fstart=AValue) then exit;
  Fstart:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchAnnotationsRequest.Set_end(AIndex : Integer; const AValue : String); 

begin
  If (F_end=AValue) then exit;
  F_end:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchAnnotationsRequest.SetpageToken(AIndex : Integer; const AValue : String); 

begin
  If (FpageToken=AValue) then exit;
  FpageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchAnnotationsRequest.SetpageSize(AIndex : Integer; const AValue : integer); 

begin
  If (FpageSize=AValue) then exit;
  FpageSize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TSearchAnnotationsRequest.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_end' : Result:='end';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;

//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSearchAnnotationsRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'annotationsetids' : SetLength(FannotationSetIds,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSearchAnnotationsResponse
  --------------------------------------------------------------------}


Procedure TSearchAnnotationsResponse.Setannotations(AIndex : Integer; const AValue : TSearchAnnotationsResponseTypeannotationsArray); 

begin
  If (Fannotations=AValue) then exit;
  Fannotations:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchAnnotationsResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSearchAnnotationsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'annotations' : SetLength(Fannotations,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TListDatasetsResponse
  --------------------------------------------------------------------}


Procedure TListDatasetsResponse.Setdatasets(AIndex : Integer; const AValue : TListDatasetsResponseTypedatasetsArray); 

begin
  If (Fdatasets=AValue) then exit;
  Fdatasets:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListDatasetsResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListDatasetsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'datasets' : SetLength(Fdatasets,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TDataset
  --------------------------------------------------------------------}


Procedure TDataset.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDataset.SetprojectId(AIndex : Integer; const AValue : String); 

begin
  If (FprojectId=AValue) then exit;
  FprojectId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDataset.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDataset.SetcreateTime(AIndex : Integer; const AValue : String); 

begin
  If (FcreateTime=AValue) then exit;
  FcreateTime:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TUndeleteDatasetRequest
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TSetIamPolicyRequest
  --------------------------------------------------------------------}


Procedure TSetIamPolicyRequest.Setpolicy(AIndex : Integer; const AValue : TPolicy); 

begin
  If (Fpolicy=AValue) then exit;
  Fpolicy:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPolicy
  --------------------------------------------------------------------}


Procedure TPolicy.Setversion(AIndex : Integer; const AValue : integer); 

begin
  If (Fversion=AValue) then exit;
  Fversion:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPolicy.Setbindings(AIndex : Integer; const AValue : TPolicyTypebindingsArray); 

begin
  If (Fbindings=AValue) then exit;
  Fbindings:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPolicy.Setetag(AIndex : Integer; const AValue : String); 

begin
  If (Fetag=AValue) then exit;
  Fetag:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TPolicy.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'bindings' : SetLength(Fbindings,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TBinding
  --------------------------------------------------------------------}


Procedure TBinding.Setrole(AIndex : Integer; const AValue : String); 

begin
  If (Frole=AValue) then exit;
  Frole:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBinding.Setmembers(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fmembers=AValue) then exit;
  Fmembers:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TBinding.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'members' : SetLength(Fmembers,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TGetIamPolicyRequest
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TTestIamPermissionsRequest
  --------------------------------------------------------------------}


Procedure TTestIamPermissionsRequest.Setpermissions(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fpermissions=AValue) then exit;
  Fpermissions:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TTestIamPermissionsRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'permissions' : SetLength(Fpermissions,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TTestIamPermissionsResponse
  --------------------------------------------------------------------}


Procedure TTestIamPermissionsResponse.Setpermissions(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fpermissions=AValue) then exit;
  Fpermissions:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TTestIamPermissionsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'permissions' : SetLength(Fpermissions,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TOperationTypemetadata
  --------------------------------------------------------------------}


Class Function TOperationTypemetadata.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TOperationTyperesponse
  --------------------------------------------------------------------}


Class Function TOperationTyperesponse.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TOperation
  --------------------------------------------------------------------}


Procedure TOperation.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setmetadata(AIndex : Integer; const AValue : TOperationTypemetadata); 

begin
  If (Fmetadata=AValue) then exit;
  Fmetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setdone(AIndex : Integer; const AValue : boolean); 

begin
  If (Fdone=AValue) then exit;
  Fdone:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Seterror(AIndex : Integer; const AValue : TStatus); 

begin
  If (Ferror=AValue) then exit;
  Ferror:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperation.Setresponse(AIndex : Integer; const AValue : TOperationTyperesponse); 

begin
  If (Fresponse=AValue) then exit;
  Fresponse:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListOperationsResponse
  --------------------------------------------------------------------}


Procedure TListOperationsResponse.Setoperations(AIndex : Integer; const AValue : TListOperationsResponseTypeoperationsArray); 

begin
  If (Foperations=AValue) then exit;
  Foperations:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListOperationsResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListOperationsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'operations' : SetLength(Foperations,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TCancelOperationRequest
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TImportReadGroupSetsRequest
  --------------------------------------------------------------------}


Procedure TImportReadGroupSetsRequest.SetdatasetId(AIndex : Integer; const AValue : String); 

begin
  If (FdatasetId=AValue) then exit;
  FdatasetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImportReadGroupSetsRequest.SetreferenceSetId(AIndex : Integer; const AValue : String); 

begin
  If (FreferenceSetId=AValue) then exit;
  FreferenceSetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImportReadGroupSetsRequest.SetsourceUris(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FsourceUris=AValue) then exit;
  FsourceUris:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImportReadGroupSetsRequest.SetpartitionStrategy(AIndex : Integer; const AValue : String); 

begin
  If (FpartitionStrategy=AValue) then exit;
  FpartitionStrategy:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TImportReadGroupSetsRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'sourceuris' : SetLength(FsourceUris,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TExportReadGroupSetRequest
  --------------------------------------------------------------------}


Procedure TExportReadGroupSetRequest.SetprojectId(AIndex : Integer; const AValue : String); 

begin
  If (FprojectId=AValue) then exit;
  FprojectId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExportReadGroupSetRequest.SetexportUri(AIndex : Integer; const AValue : String); 

begin
  If (FexportUri=AValue) then exit;
  FexportUri:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExportReadGroupSetRequest.SetreferenceNames(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FreferenceNames=AValue) then exit;
  FreferenceNames:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TExportReadGroupSetRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'referencenames' : SetLength(FreferenceNames,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSearchReadGroupSetsRequest
  --------------------------------------------------------------------}


Procedure TSearchReadGroupSetsRequest.SetdatasetIds(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FdatasetIds=AValue) then exit;
  FdatasetIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchReadGroupSetsRequest.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchReadGroupSetsRequest.SetpageToken(AIndex : Integer; const AValue : String); 

begin
  If (FpageToken=AValue) then exit;
  FpageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchReadGroupSetsRequest.SetpageSize(AIndex : Integer; const AValue : integer); 

begin
  If (FpageSize=AValue) then exit;
  FpageSize:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSearchReadGroupSetsRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'datasetids' : SetLength(FdatasetIds,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSearchReadGroupSetsResponse
  --------------------------------------------------------------------}


Procedure TSearchReadGroupSetsResponse.SetreadGroupSets(AIndex : Integer; const AValue : TSearchReadGroupSetsResponseTypereadGroupSetsArray); 

begin
  If (FreadGroupSets=AValue) then exit;
  FreadGroupSets:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchReadGroupSetsResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSearchReadGroupSetsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'readgroupsets' : SetLength(FreadGroupSets,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TReadGroupSetTypeinfo
  --------------------------------------------------------------------}


Class Function TReadGroupSetTypeinfo.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TReadGroupSet
  --------------------------------------------------------------------}


Procedure TReadGroupSet.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadGroupSet.SetdatasetId(AIndex : Integer; const AValue : String); 

begin
  If (FdatasetId=AValue) then exit;
  FdatasetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadGroupSet.SetreferenceSetId(AIndex : Integer; const AValue : String); 

begin
  If (FreferenceSetId=AValue) then exit;
  FreferenceSetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadGroupSet.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadGroupSet.Setfilename(AIndex : Integer; const AValue : String); 

begin
  If (Ffilename=AValue) then exit;
  Ffilename:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadGroupSet.SetreadGroups(AIndex : Integer; const AValue : TReadGroupSetTypereadGroupsArray); 

begin
  If (FreadGroups=AValue) then exit;
  FreadGroups:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadGroupSet.Setinfo(AIndex : Integer; const AValue : TReadGroupSetTypeinfo); 

begin
  If (Finfo=AValue) then exit;
  Finfo:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TReadGroupSet.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'readgroups' : SetLength(FreadGroups,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TReadGroupTypeinfo
  --------------------------------------------------------------------}


Class Function TReadGroupTypeinfo.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TReadGroup
  --------------------------------------------------------------------}


Procedure TReadGroup.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadGroup.SetdatasetId(AIndex : Integer; const AValue : String); 

begin
  If (FdatasetId=AValue) then exit;
  FdatasetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadGroup.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadGroup.Setdescription(AIndex : Integer; const AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadGroup.SetsampleId(AIndex : Integer; const AValue : String); 

begin
  If (FsampleId=AValue) then exit;
  FsampleId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadGroup.Setexperiment(AIndex : Integer; const AValue : TExperiment); 

begin
  If (Fexperiment=AValue) then exit;
  Fexperiment:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadGroup.SetpredictedInsertSize(AIndex : Integer; const AValue : integer); 

begin
  If (FpredictedInsertSize=AValue) then exit;
  FpredictedInsertSize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadGroup.Setprograms(AIndex : Integer; const AValue : TReadGroupTypeprogramsArray); 

begin
  If (Fprograms=AValue) then exit;
  Fprograms:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadGroup.SetreferenceSetId(AIndex : Integer; const AValue : String); 

begin
  If (FreferenceSetId=AValue) then exit;
  FreferenceSetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadGroup.Setinfo(AIndex : Integer; const AValue : TReadGroupTypeinfo); 

begin
  If (Finfo=AValue) then exit;
  Finfo:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TReadGroup.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'programs' : SetLength(Fprograms,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TExperiment
  --------------------------------------------------------------------}


Procedure TExperiment.SetlibraryId(AIndex : Integer; const AValue : String); 

begin
  If (FlibraryId=AValue) then exit;
  FlibraryId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperiment.SetplatformUnit(AIndex : Integer; const AValue : String); 

begin
  If (FplatformUnit=AValue) then exit;
  FplatformUnit:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperiment.SetsequencingCenter(AIndex : Integer; const AValue : String); 

begin
  If (FsequencingCenter=AValue) then exit;
  FsequencingCenter:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperiment.SetinstrumentModel(AIndex : Integer; const AValue : String); 

begin
  If (FinstrumentModel=AValue) then exit;
  FinstrumentModel:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TProgram
  --------------------------------------------------------------------}


Procedure TProgram.SetcommandLine(AIndex : Integer; const AValue : String); 

begin
  If (FcommandLine=AValue) then exit;
  FcommandLine:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProgram.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProgram.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProgram.SetprevProgramId(AIndex : Integer; const AValue : String); 

begin
  If (FprevProgramId=AValue) then exit;
  FprevProgramId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TProgram.Setversion(AIndex : Integer; const AValue : String); 

begin
  If (Fversion=AValue) then exit;
  Fversion:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListCoverageBucketsResponse
  --------------------------------------------------------------------}


Procedure TListCoverageBucketsResponse.SetbucketWidth(AIndex : Integer; const AValue : String); 

begin
  If (FbucketWidth=AValue) then exit;
  FbucketWidth:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListCoverageBucketsResponse.SetcoverageBuckets(AIndex : Integer; const AValue : TListCoverageBucketsResponseTypecoverageBucketsArray); 

begin
  If (FcoverageBuckets=AValue) then exit;
  FcoverageBuckets:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListCoverageBucketsResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TListCoverageBucketsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'coveragebuckets' : SetLength(FcoverageBuckets,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TCoverageBucket
  --------------------------------------------------------------------}


Procedure TCoverageBucket.Setrange(AIndex : Integer; const AValue : TRange); 

begin
  If (Frange=AValue) then exit;
  Frange:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCoverageBucket.SetmeanCoverage(AIndex : Integer; const AValue : integer); 

begin
  If (FmeanCoverage=AValue) then exit;
  FmeanCoverage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TRange
  --------------------------------------------------------------------}


Procedure TRange.SetreferenceName(AIndex : Integer; const AValue : String); 

begin
  If (FreferenceName=AValue) then exit;
  FreferenceName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRange.Setstart(AIndex : Integer; const AValue : String); 

begin
  If (Fstart=AValue) then exit;
  Fstart:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRange.Set_end(AIndex : Integer; const AValue : String); 

begin
  If (F_end=AValue) then exit;
  F_end:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TRange.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_end' : Result:='end';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TSearchReadsRequest
  --------------------------------------------------------------------}


Procedure TSearchReadsRequest.SetreadGroupSetIds(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FreadGroupSetIds=AValue) then exit;
  FreadGroupSetIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchReadsRequest.SetreadGroupIds(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FreadGroupIds=AValue) then exit;
  FreadGroupIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchReadsRequest.SetreferenceName(AIndex : Integer; const AValue : String); 

begin
  If (FreferenceName=AValue) then exit;
  FreferenceName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchReadsRequest.Setstart(AIndex : Integer; const AValue : String); 

begin
  If (Fstart=AValue) then exit;
  Fstart:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchReadsRequest.Set_end(AIndex : Integer; const AValue : String); 

begin
  If (F_end=AValue) then exit;
  F_end:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchReadsRequest.SetpageToken(AIndex : Integer; const AValue : String); 

begin
  If (FpageToken=AValue) then exit;
  FpageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchReadsRequest.SetpageSize(AIndex : Integer; const AValue : integer); 

begin
  If (FpageSize=AValue) then exit;
  FpageSize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TSearchReadsRequest.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_end' : Result:='end';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;

//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSearchReadsRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'readgroupsetids' : SetLength(FreadGroupSetIds,ALength);
  'readgroupids' : SetLength(FreadGroupIds,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSearchReadsResponse
  --------------------------------------------------------------------}


Procedure TSearchReadsResponse.Setalignments(AIndex : Integer; const AValue : TSearchReadsResponseTypealignmentsArray); 

begin
  If (Falignments=AValue) then exit;
  Falignments:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchReadsResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSearchReadsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'alignments' : SetLength(Falignments,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TReadTypeinfo
  --------------------------------------------------------------------}


Class Function TReadTypeinfo.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TRead
  --------------------------------------------------------------------}


Procedure TRead.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRead.SetreadGroupId(AIndex : Integer; const AValue : String); 

begin
  If (FreadGroupId=AValue) then exit;
  FreadGroupId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRead.SetreadGroupSetId(AIndex : Integer; const AValue : String); 

begin
  If (FreadGroupSetId=AValue) then exit;
  FreadGroupSetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRead.SetfragmentName(AIndex : Integer; const AValue : String); 

begin
  If (FfragmentName=AValue) then exit;
  FfragmentName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRead.SetproperPlacement(AIndex : Integer; const AValue : boolean); 

begin
  If (FproperPlacement=AValue) then exit;
  FproperPlacement:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRead.SetduplicateFragment(AIndex : Integer; const AValue : boolean); 

begin
  If (FduplicateFragment=AValue) then exit;
  FduplicateFragment:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRead.SetfragmentLength(AIndex : Integer; const AValue : integer); 

begin
  If (FfragmentLength=AValue) then exit;
  FfragmentLength:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRead.SetreadNumber(AIndex : Integer; const AValue : integer); 

begin
  If (FreadNumber=AValue) then exit;
  FreadNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRead.SetnumberReads(AIndex : Integer; const AValue : integer); 

begin
  If (FnumberReads=AValue) then exit;
  FnumberReads:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRead.SetfailedVendorQualityChecks(AIndex : Integer; const AValue : boolean); 

begin
  If (FfailedVendorQualityChecks=AValue) then exit;
  FfailedVendorQualityChecks:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRead.Setalignment(AIndex : Integer; const AValue : TLinearAlignment); 

begin
  If (Falignment=AValue) then exit;
  Falignment:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRead.SetsecondaryAlignment(AIndex : Integer; const AValue : boolean); 

begin
  If (FsecondaryAlignment=AValue) then exit;
  FsecondaryAlignment:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRead.SetsupplementaryAlignment(AIndex : Integer; const AValue : boolean); 

begin
  If (FsupplementaryAlignment=AValue) then exit;
  FsupplementaryAlignment:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRead.SetalignedSequence(AIndex : Integer; const AValue : String); 

begin
  If (FalignedSequence=AValue) then exit;
  FalignedSequence:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRead.SetalignedQuality(AIndex : Integer; const AValue : TintegerArray); 

begin
  If (FalignedQuality=AValue) then exit;
  FalignedQuality:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRead.SetnextMatePosition(AIndex : Integer; const AValue : TPosition); 

begin
  If (FnextMatePosition=AValue) then exit;
  FnextMatePosition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRead.Setinfo(AIndex : Integer; const AValue : TReadTypeinfo); 

begin
  If (Finfo=AValue) then exit;
  Finfo:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TRead.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'alignedquality' : SetLength(FalignedQuality,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TLinearAlignment
  --------------------------------------------------------------------}


Procedure TLinearAlignment.Setposition(AIndex : Integer; const AValue : TPosition); 

begin
  If (Fposition=AValue) then exit;
  Fposition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLinearAlignment.SetmappingQuality(AIndex : Integer; const AValue : integer); 

begin
  If (FmappingQuality=AValue) then exit;
  FmappingQuality:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLinearAlignment.Setcigar(AIndex : Integer; const AValue : TLinearAlignmentTypecigarArray); 

begin
  If (Fcigar=AValue) then exit;
  Fcigar:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TLinearAlignment.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'cigar' : SetLength(Fcigar,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TPosition
  --------------------------------------------------------------------}


Procedure TPosition.SetreferenceName(AIndex : Integer; const AValue : String); 

begin
  If (FreferenceName=AValue) then exit;
  FreferenceName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPosition.Setposition(AIndex : Integer; const AValue : String); 

begin
  If (Fposition=AValue) then exit;
  Fposition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPosition.SetreverseStrand(AIndex : Integer; const AValue : boolean); 

begin
  If (FreverseStrand=AValue) then exit;
  FreverseStrand:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCigarUnit
  --------------------------------------------------------------------}


Procedure TCigarUnit.Setoperation(AIndex : Integer; const AValue : String); 

begin
  If (Foperation=AValue) then exit;
  Foperation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCigarUnit.SetoperationLength(AIndex : Integer; const AValue : String); 

begin
  If (FoperationLength=AValue) then exit;
  FoperationLength:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCigarUnit.SetreferenceSequence(AIndex : Integer; const AValue : String); 

begin
  If (FreferenceSequence=AValue) then exit;
  FreferenceSequence:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TStreamReadsRequest
  --------------------------------------------------------------------}


Procedure TStreamReadsRequest.SetprojectId(AIndex : Integer; const AValue : String); 

begin
  If (FprojectId=AValue) then exit;
  FprojectId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStreamReadsRequest.SetreadGroupSetId(AIndex : Integer; const AValue : String); 

begin
  If (FreadGroupSetId=AValue) then exit;
  FreadGroupSetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStreamReadsRequest.SetreferenceName(AIndex : Integer; const AValue : String); 

begin
  If (FreferenceName=AValue) then exit;
  FreferenceName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStreamReadsRequest.Setstart(AIndex : Integer; const AValue : String); 

begin
  If (Fstart=AValue) then exit;
  Fstart:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStreamReadsRequest.Set_end(AIndex : Integer; const AValue : String); 

begin
  If (F_end=AValue) then exit;
  F_end:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStreamReadsRequest.Setshard(AIndex : Integer; const AValue : integer); 

begin
  If (Fshard=AValue) then exit;
  Fshard:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStreamReadsRequest.SettotalShards(AIndex : Integer; const AValue : integer); 

begin
  If (FtotalShards=AValue) then exit;
  FtotalShards:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TStreamReadsRequest.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_end' : Result:='end';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TStreamReadsResponse
  --------------------------------------------------------------------}


Procedure TStreamReadsResponse.Setalignments(AIndex : Integer; const AValue : TStreamReadsResponseTypealignmentsArray); 

begin
  If (Falignments=AValue) then exit;
  Falignments:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TStreamReadsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'alignments' : SetLength(Falignments,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSearchReferenceSetsRequest
  --------------------------------------------------------------------}


Procedure TSearchReferenceSetsRequest.Setmd5checksums(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fmd5checksums=AValue) then exit;
  Fmd5checksums:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchReferenceSetsRequest.Setaccessions(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Faccessions=AValue) then exit;
  Faccessions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchReferenceSetsRequest.SetassemblyId(AIndex : Integer; const AValue : String); 

begin
  If (FassemblyId=AValue) then exit;
  FassemblyId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchReferenceSetsRequest.SetpageToken(AIndex : Integer; const AValue : String); 

begin
  If (FpageToken=AValue) then exit;
  FpageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchReferenceSetsRequest.SetpageSize(AIndex : Integer; const AValue : integer); 

begin
  If (FpageSize=AValue) then exit;
  FpageSize:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSearchReferenceSetsRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'md5checksums' : SetLength(Fmd5checksums,ALength);
  'accessions' : SetLength(Faccessions,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSearchReferenceSetsResponse
  --------------------------------------------------------------------}


Procedure TSearchReferenceSetsResponse.SetreferenceSets(AIndex : Integer; const AValue : TSearchReferenceSetsResponseTypereferenceSetsArray); 

begin
  If (FreferenceSets=AValue) then exit;
  FreferenceSets:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchReferenceSetsResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSearchReferenceSetsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'referencesets' : SetLength(FreferenceSets,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TReferenceSet
  --------------------------------------------------------------------}


Procedure TReferenceSet.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReferenceSet.SetreferenceIds(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FreferenceIds=AValue) then exit;
  FreferenceIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReferenceSet.Setmd5checksum(AIndex : Integer; const AValue : String); 

begin
  If (Fmd5checksum=AValue) then exit;
  Fmd5checksum:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReferenceSet.SetncbiTaxonId(AIndex : Integer; const AValue : integer); 

begin
  If (FncbiTaxonId=AValue) then exit;
  FncbiTaxonId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReferenceSet.Setdescription(AIndex : Integer; const AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReferenceSet.SetassemblyId(AIndex : Integer; const AValue : String); 

begin
  If (FassemblyId=AValue) then exit;
  FassemblyId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReferenceSet.SetsourceUri(AIndex : Integer; const AValue : String); 

begin
  If (FsourceUri=AValue) then exit;
  FsourceUri:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReferenceSet.SetsourceAccessions(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FsourceAccessions=AValue) then exit;
  FsourceAccessions:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TReferenceSet.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'referenceids' : SetLength(FreferenceIds,ALength);
  'sourceaccessions' : SetLength(FsourceAccessions,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSearchReferencesRequest
  --------------------------------------------------------------------}


Procedure TSearchReferencesRequest.Setmd5checksums(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fmd5checksums=AValue) then exit;
  Fmd5checksums:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchReferencesRequest.Setaccessions(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Faccessions=AValue) then exit;
  Faccessions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchReferencesRequest.SetreferenceSetId(AIndex : Integer; const AValue : String); 

begin
  If (FreferenceSetId=AValue) then exit;
  FreferenceSetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchReferencesRequest.SetpageToken(AIndex : Integer; const AValue : String); 

begin
  If (FpageToken=AValue) then exit;
  FpageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchReferencesRequest.SetpageSize(AIndex : Integer; const AValue : integer); 

begin
  If (FpageSize=AValue) then exit;
  FpageSize:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSearchReferencesRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'md5checksums' : SetLength(Fmd5checksums,ALength);
  'accessions' : SetLength(Faccessions,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSearchReferencesResponse
  --------------------------------------------------------------------}


Procedure TSearchReferencesResponse.Setreferences(AIndex : Integer; const AValue : TSearchReferencesResponseTypereferencesArray); 

begin
  If (Freferences=AValue) then exit;
  Freferences:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchReferencesResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSearchReferencesResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'references' : SetLength(Freferences,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TReference
  --------------------------------------------------------------------}


Procedure TReference.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReference.Set_length(AIndex : Integer; const AValue : String); 

begin
  If (F_length=AValue) then exit;
  F_length:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReference.Setmd5checksum(AIndex : Integer; const AValue : String); 

begin
  If (Fmd5checksum=AValue) then exit;
  Fmd5checksum:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReference.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReference.SetsourceUri(AIndex : Integer; const AValue : String); 

begin
  If (FsourceUri=AValue) then exit;
  FsourceUri:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReference.SetsourceAccessions(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FsourceAccessions=AValue) then exit;
  FsourceAccessions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReference.SetncbiTaxonId(AIndex : Integer; const AValue : integer); 

begin
  If (FncbiTaxonId=AValue) then exit;
  FncbiTaxonId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TReference.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_length' : Result:='length';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;

//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TReference.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'sourceaccessions' : SetLength(FsourceAccessions,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TListBasesResponse
  --------------------------------------------------------------------}


Procedure TListBasesResponse.Setoffset(AIndex : Integer; const AValue : String); 

begin
  If (Foffset=AValue) then exit;
  Foffset:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListBasesResponse.Setsequence(AIndex : Integer; const AValue : String); 

begin
  If (Fsequence=AValue) then exit;
  Fsequence:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListBasesResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TImportVariantsRequestTypeinfoMergeConfig
  --------------------------------------------------------------------}


Class Function TImportVariantsRequestTypeinfoMergeConfig.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TImportVariantsRequest
  --------------------------------------------------------------------}


Procedure TImportVariantsRequest.SetvariantSetId(AIndex : Integer; const AValue : String); 

begin
  If (FvariantSetId=AValue) then exit;
  FvariantSetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImportVariantsRequest.SetsourceUris(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FsourceUris=AValue) then exit;
  FsourceUris:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImportVariantsRequest.Setformat(AIndex : Integer; const AValue : String); 

begin
  If (Fformat=AValue) then exit;
  Fformat:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImportVariantsRequest.SetnormalizeReferenceNames(AIndex : Integer; const AValue : boolean); 

begin
  If (FnormalizeReferenceNames=AValue) then exit;
  FnormalizeReferenceNames:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImportVariantsRequest.SetinfoMergeConfig(AIndex : Integer; const AValue : TImportVariantsRequestTypeinfoMergeConfig); 

begin
  If (FinfoMergeConfig=AValue) then exit;
  FinfoMergeConfig:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TImportVariantsRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'sourceuris' : SetLength(FsourceUris,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TVariantSet
  --------------------------------------------------------------------}


Procedure TVariantSet.SetdatasetId(AIndex : Integer; const AValue : String); 

begin
  If (FdatasetId=AValue) then exit;
  FdatasetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariantSet.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariantSet.SetreferenceSetId(AIndex : Integer; const AValue : String); 

begin
  If (FreferenceSetId=AValue) then exit;
  FreferenceSetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariantSet.SetreferenceBounds(AIndex : Integer; const AValue : TVariantSetTypereferenceBoundsArray); 

begin
  If (FreferenceBounds=AValue) then exit;
  FreferenceBounds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariantSet.Setmetadata(AIndex : Integer; const AValue : TVariantSetTypemetadataArray); 

begin
  If (Fmetadata=AValue) then exit;
  Fmetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariantSet.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariantSet.Setdescription(AIndex : Integer; const AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TVariantSet.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'referencebounds' : SetLength(FreferenceBounds,ALength);
  'metadata' : SetLength(Fmetadata,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TReferenceBound
  --------------------------------------------------------------------}


Procedure TReferenceBound.SetreferenceName(AIndex : Integer; const AValue : String); 

begin
  If (FreferenceName=AValue) then exit;
  FreferenceName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReferenceBound.SetupperBound(AIndex : Integer; const AValue : String); 

begin
  If (FupperBound=AValue) then exit;
  FupperBound:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVariantSetMetadataTypeinfo
  --------------------------------------------------------------------}


Class Function TVariantSetMetadataTypeinfo.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TVariantSetMetadata
  --------------------------------------------------------------------}


Procedure TVariantSetMetadata.Setkey(AIndex : Integer; const AValue : String); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariantSetMetadata.Setvalue(AIndex : Integer; const AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariantSetMetadata.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariantSetMetadata.Set_type(AIndex : Integer; const AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariantSetMetadata.Setnumber(AIndex : Integer; const AValue : String); 

begin
  If (Fnumber=AValue) then exit;
  Fnumber:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariantSetMetadata.Setdescription(AIndex : Integer; const AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariantSetMetadata.Setinfo(AIndex : Integer; const AValue : TVariantSetMetadataTypeinfo); 

begin
  If (Finfo=AValue) then exit;
  Finfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TVariantSetMetadata.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TExportVariantSetRequest
  --------------------------------------------------------------------}


Procedure TExportVariantSetRequest.SetcallSetIds(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FcallSetIds=AValue) then exit;
  FcallSetIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExportVariantSetRequest.SetprojectId(AIndex : Integer; const AValue : String); 

begin
  If (FprojectId=AValue) then exit;
  FprojectId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExportVariantSetRequest.Setformat(AIndex : Integer; const AValue : String); 

begin
  If (Fformat=AValue) then exit;
  Fformat:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExportVariantSetRequest.SetbigqueryDataset(AIndex : Integer; const AValue : String); 

begin
  If (FbigqueryDataset=AValue) then exit;
  FbigqueryDataset:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExportVariantSetRequest.SetbigqueryTable(AIndex : Integer; const AValue : String); 

begin
  If (FbigqueryTable=AValue) then exit;
  FbigqueryTable:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TExportVariantSetRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'callsetids' : SetLength(FcallSetIds,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSearchVariantSetsRequest
  --------------------------------------------------------------------}


Procedure TSearchVariantSetsRequest.SetdatasetIds(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FdatasetIds=AValue) then exit;
  FdatasetIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchVariantSetsRequest.SetpageToken(AIndex : Integer; const AValue : String); 

begin
  If (FpageToken=AValue) then exit;
  FpageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchVariantSetsRequest.SetpageSize(AIndex : Integer; const AValue : integer); 

begin
  If (FpageSize=AValue) then exit;
  FpageSize:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSearchVariantSetsRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'datasetids' : SetLength(FdatasetIds,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSearchVariantSetsResponse
  --------------------------------------------------------------------}


Procedure TSearchVariantSetsResponse.SetvariantSets(AIndex : Integer; const AValue : TSearchVariantSetsResponseTypevariantSetsArray); 

begin
  If (FvariantSets=AValue) then exit;
  FvariantSets:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchVariantSetsResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSearchVariantSetsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'variantsets' : SetLength(FvariantSets,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSearchVariantsRequest
  --------------------------------------------------------------------}


Procedure TSearchVariantsRequest.SetvariantSetIds(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FvariantSetIds=AValue) then exit;
  FvariantSetIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchVariantsRequest.SetvariantName(AIndex : Integer; const AValue : String); 

begin
  If (FvariantName=AValue) then exit;
  FvariantName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchVariantsRequest.SetcallSetIds(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FcallSetIds=AValue) then exit;
  FcallSetIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchVariantsRequest.SetreferenceName(AIndex : Integer; const AValue : String); 

begin
  If (FreferenceName=AValue) then exit;
  FreferenceName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchVariantsRequest.Setstart(AIndex : Integer; const AValue : String); 

begin
  If (Fstart=AValue) then exit;
  Fstart:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchVariantsRequest.Set_end(AIndex : Integer; const AValue : String); 

begin
  If (F_end=AValue) then exit;
  F_end:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchVariantsRequest.SetpageToken(AIndex : Integer; const AValue : String); 

begin
  If (FpageToken=AValue) then exit;
  FpageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchVariantsRequest.SetpageSize(AIndex : Integer; const AValue : integer); 

begin
  If (FpageSize=AValue) then exit;
  FpageSize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchVariantsRequest.SetmaxCalls(AIndex : Integer; const AValue : integer); 

begin
  If (FmaxCalls=AValue) then exit;
  FmaxCalls:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TSearchVariantsRequest.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_end' : Result:='end';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;

//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSearchVariantsRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'variantsetids' : SetLength(FvariantSetIds,ALength);
  'callsetids' : SetLength(FcallSetIds,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSearchVariantsResponse
  --------------------------------------------------------------------}


Procedure TSearchVariantsResponse.Setvariants(AIndex : Integer; const AValue : TSearchVariantsResponseTypevariantsArray); 

begin
  If (Fvariants=AValue) then exit;
  Fvariants:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchVariantsResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSearchVariantsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'variants' : SetLength(Fvariants,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TVariantTypeinfo
  --------------------------------------------------------------------}


Class Function TVariantTypeinfo.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TVariant
  --------------------------------------------------------------------}


Procedure TVariant.SetvariantSetId(AIndex : Integer; const AValue : String); 

begin
  If (FvariantSetId=AValue) then exit;
  FvariantSetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariant.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariant.Setnames(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Fnames=AValue) then exit;
  Fnames:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariant.Setcreated(AIndex : Integer; const AValue : String); 

begin
  If (Fcreated=AValue) then exit;
  Fcreated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariant.SetreferenceName(AIndex : Integer; const AValue : String); 

begin
  If (FreferenceName=AValue) then exit;
  FreferenceName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariant.Setstart(AIndex : Integer; const AValue : String); 

begin
  If (Fstart=AValue) then exit;
  Fstart:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariant.Set_end(AIndex : Integer; const AValue : String); 

begin
  If (F_end=AValue) then exit;
  F_end:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariant.SetreferenceBases(AIndex : Integer; const AValue : String); 

begin
  If (FreferenceBases=AValue) then exit;
  FreferenceBases:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariant.SetalternateBases(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FalternateBases=AValue) then exit;
  FalternateBases:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariant.Setquality(AIndex : Integer; const AValue : double); 

begin
  If (Fquality=AValue) then exit;
  Fquality:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariant.Setfilter(AIndex : Integer; const AValue : TStringArray); 

begin
  If (Ffilter=AValue) then exit;
  Ffilter:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariant.Setinfo(AIndex : Integer; const AValue : TVariantTypeinfo); 

begin
  If (Finfo=AValue) then exit;
  Finfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariant.Setcalls(AIndex : Integer; const AValue : TVariantTypecallsArray); 

begin
  If (Fcalls=AValue) then exit;
  Fcalls:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TVariant.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_end' : Result:='end';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;

//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TVariant.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'names' : SetLength(Fnames,ALength);
  'alternatebases' : SetLength(FalternateBases,ALength);
  'filter' : SetLength(Ffilter,ALength);
  'calls' : SetLength(Fcalls,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TVariantCallTypeinfo
  --------------------------------------------------------------------}


Class Function TVariantCallTypeinfo.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TVariantCall
  --------------------------------------------------------------------}


Procedure TVariantCall.SetcallSetId(AIndex : Integer; const AValue : String); 

begin
  If (FcallSetId=AValue) then exit;
  FcallSetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariantCall.SetcallSetName(AIndex : Integer; const AValue : String); 

begin
  If (FcallSetName=AValue) then exit;
  FcallSetName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariantCall.Setgenotype(AIndex : Integer; const AValue : TintegerArray); 

begin
  If (Fgenotype=AValue) then exit;
  Fgenotype:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariantCall.Setphaseset(AIndex : Integer; const AValue : String); 

begin
  If (Fphaseset=AValue) then exit;
  Fphaseset:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariantCall.SetgenotypeLikelihood(AIndex : Integer; const AValue : TdoubleArray); 

begin
  If (FgenotypeLikelihood=AValue) then exit;
  FgenotypeLikelihood:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariantCall.Setinfo(AIndex : Integer; const AValue : TVariantCallTypeinfo); 

begin
  If (Finfo=AValue) then exit;
  Finfo:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TVariantCall.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'genotype' : SetLength(Fgenotype,ALength);
  'genotypelikelihood' : SetLength(FgenotypeLikelihood,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TMergeVariantsRequestTypeinfoMergeConfig
  --------------------------------------------------------------------}


Class Function TMergeVariantsRequestTypeinfoMergeConfig.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TMergeVariantsRequest
  --------------------------------------------------------------------}


Procedure TMergeVariantsRequest.SetvariantSetId(AIndex : Integer; const AValue : String); 

begin
  If (FvariantSetId=AValue) then exit;
  FvariantSetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMergeVariantsRequest.Setvariants(AIndex : Integer; const AValue : TMergeVariantsRequestTypevariantsArray); 

begin
  If (Fvariants=AValue) then exit;
  Fvariants:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMergeVariantsRequest.SetinfoMergeConfig(AIndex : Integer; const AValue : TMergeVariantsRequestTypeinfoMergeConfig); 

begin
  If (FinfoMergeConfig=AValue) then exit;
  FinfoMergeConfig:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TMergeVariantsRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'variants' : SetLength(Fvariants,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSearchCallSetsRequest
  --------------------------------------------------------------------}


Procedure TSearchCallSetsRequest.SetvariantSetIds(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FvariantSetIds=AValue) then exit;
  FvariantSetIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchCallSetsRequest.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchCallSetsRequest.SetpageToken(AIndex : Integer; const AValue : String); 

begin
  If (FpageToken=AValue) then exit;
  FpageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchCallSetsRequest.SetpageSize(AIndex : Integer; const AValue : integer); 

begin
  If (FpageSize=AValue) then exit;
  FpageSize:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSearchCallSetsRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'variantsetids' : SetLength(FvariantSetIds,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSearchCallSetsResponse
  --------------------------------------------------------------------}


Procedure TSearchCallSetsResponse.SetcallSets(AIndex : Integer; const AValue : TSearchCallSetsResponseTypecallSetsArray); 

begin
  If (FcallSets=AValue) then exit;
  FcallSets:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchCallSetsResponse.SetnextPageToken(AIndex : Integer; const AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSearchCallSetsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'callsets' : SetLength(FcallSets,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TCallSetTypeinfo
  --------------------------------------------------------------------}


Class Function TCallSetTypeinfo.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TCallSet
  --------------------------------------------------------------------}


Procedure TCallSet.Setid(AIndex : Integer; const AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCallSet.Setname(AIndex : Integer; const AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCallSet.SetsampleId(AIndex : Integer; const AValue : String); 

begin
  If (FsampleId=AValue) then exit;
  FsampleId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCallSet.SetvariantSetIds(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FvariantSetIds=AValue) then exit;
  FvariantSetIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCallSet.Setcreated(AIndex : Integer; const AValue : String); 

begin
  If (Fcreated=AValue) then exit;
  Fcreated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCallSet.Setinfo(AIndex : Integer; const AValue : TCallSetTypeinfo); 

begin
  If (Finfo=AValue) then exit;
  Finfo:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TCallSet.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'variantsetids' : SetLength(FvariantSetIds,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TStreamVariantsRequest
  --------------------------------------------------------------------}


Procedure TStreamVariantsRequest.SetprojectId(AIndex : Integer; const AValue : String); 

begin
  If (FprojectId=AValue) then exit;
  FprojectId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStreamVariantsRequest.SetvariantSetId(AIndex : Integer; const AValue : String); 

begin
  If (FvariantSetId=AValue) then exit;
  FvariantSetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStreamVariantsRequest.SetcallSetIds(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FcallSetIds=AValue) then exit;
  FcallSetIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStreamVariantsRequest.SetreferenceName(AIndex : Integer; const AValue : String); 

begin
  If (FreferenceName=AValue) then exit;
  FreferenceName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStreamVariantsRequest.Setstart(AIndex : Integer; const AValue : String); 

begin
  If (Fstart=AValue) then exit;
  Fstart:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStreamVariantsRequest.Set_end(AIndex : Integer; const AValue : String); 

begin
  If (F_end=AValue) then exit;
  F_end:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TStreamVariantsRequest.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_end' : Result:='end';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;

//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TStreamVariantsRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'callsetids' : SetLength(FcallSetIds,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TStreamVariantsResponse
  --------------------------------------------------------------------}


Procedure TStreamVariantsResponse.Setvariants(AIndex : Integer; const AValue : TStreamVariantsResponseTypevariantsArray); 

begin
  If (Fvariants=AValue) then exit;
  Fvariants:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TStreamVariantsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'variants' : SetLength(Fvariants,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TImportReadGroupSetsResponse
  --------------------------------------------------------------------}


Procedure TImportReadGroupSetsResponse.SetreadGroupSetIds(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FreadGroupSetIds=AValue) then exit;
  FreadGroupSetIds:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TImportReadGroupSetsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'readgroupsetids' : SetLength(FreadGroupSetIds,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TImportVariantsResponse
  --------------------------------------------------------------------}


Procedure TImportVariantsResponse.SetcallSetIds(AIndex : Integer; const AValue : TStringArray); 

begin
  If (FcallSetIds=AValue) then exit;
  FcallSetIds:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TImportVariantsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'callsetids' : SetLength(FcallSetIds,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TOperationMetadataTyperequest
  --------------------------------------------------------------------}


Class Function TOperationMetadataTyperequest.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TOperationMetadata
  --------------------------------------------------------------------}


Procedure TOperationMetadata.SetprojectId(AIndex : Integer; const AValue : String); 

begin
  If (FprojectId=AValue) then exit;
  FprojectId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationMetadata.SetcreateTime(AIndex : Integer; const AValue : String); 

begin
  If (FcreateTime=AValue) then exit;
  FcreateTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationMetadata.SetendTime(AIndex : Integer; const AValue : String); 

begin
  If (FendTime=AValue) then exit;
  FendTime:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationMetadata.Setrequest(AIndex : Integer; const AValue : TOperationMetadataTyperequest); 

begin
  If (Frequest=AValue) then exit;
  Frequest:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TOperationMetadata.Setevents(AIndex : Integer; const AValue : TOperationMetadataTypeeventsArray); 

begin
  If (Fevents=AValue) then exit;
  Fevents:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TOperationMetadata.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'events' : SetLength(Fevents,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TOperationEvent
  --------------------------------------------------------------------}


Procedure TOperationEvent.Setdescription(AIndex : Integer; const AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAnnotationsetsResource
  --------------------------------------------------------------------}


Class Function TAnnotationsetsResource.ResourceName : String;

begin
  Result:='annotationsets';
end;

Class Function TAnnotationsetsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TgenomicsAPI;
end;

Function TAnnotationsetsResource.Create(aAnnotationSet : TAnnotationSet) : TAnnotationSet;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/annotationsets';
  _Methodid   = 'genomics.annotationsets.create';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aAnnotationSet,TAnnotationSet) as TAnnotationSet;
end;

Function TAnnotationsetsResource.Get(annotationSetId: string) : TAnnotationSet;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/annotationsets/{annotationSetId}';
  _Methodid   = 'genomics.annotationsets.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['annotationSetId',annotationSetId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TAnnotationSet) as TAnnotationSet;
end;

Function TAnnotationsetsResource.Update(annotationSetId: string; aAnnotationSet : TAnnotationSet; AQuery : string = '') : TAnnotationSet;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'v1/annotationsets/{annotationSetId}';
  _Methodid   = 'genomics.annotationsets.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['annotationSetId',annotationSetId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aAnnotationSet,TAnnotationSet) as TAnnotationSet;
end;


Function TAnnotationsetsResource.Update(annotationSetId: string; aAnnotationSet : TAnnotationSet; AQuery : TAnnotationsetsupdateOptions) : TAnnotationSet;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'updateMask',AQuery.updateMask);
  Result:=Update(annotationSetId,aAnnotationSet,_Q);
end;

Function TAnnotationsetsResource.Delete(annotationSetId: string) : TEmpty;

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'v1/annotationsets/{annotationSetId}';
  _Methodid   = 'genomics.annotationsets.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['annotationSetId',annotationSetId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TEmpty) as TEmpty;
end;

Function TAnnotationsetsResource.Search(aSearchAnnotationSetsRequest : TSearchAnnotationSetsRequest) : TSearchAnnotationSetsResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/annotationsets/search';
  _Methodid   = 'genomics.annotationsets.search';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aSearchAnnotationSetsRequest,TSearchAnnotationSetsResponse) as TSearchAnnotationSetsResponse;
end;



{ --------------------------------------------------------------------
  TAnnotationsResource
  --------------------------------------------------------------------}


Class Function TAnnotationsResource.ResourceName : String;

begin
  Result:='annotations';
end;

Class Function TAnnotationsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TgenomicsAPI;
end;

Function TAnnotationsResource.Create(aAnnotation : TAnnotation) : TAnnotation;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/annotations';
  _Methodid   = 'genomics.annotations.create';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aAnnotation,TAnnotation) as TAnnotation;
end;

Function TAnnotationsResource.BatchCreate(aBatchCreateAnnotationsRequest : TBatchCreateAnnotationsRequest) : TBatchCreateAnnotationsResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/annotations:batchCreate';
  _Methodid   = 'genomics.annotations.batchCreate';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aBatchCreateAnnotationsRequest,TBatchCreateAnnotationsResponse) as TBatchCreateAnnotationsResponse;
end;

Function TAnnotationsResource.Get(annotationId: string) : TAnnotation;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/annotations/{annotationId}';
  _Methodid   = 'genomics.annotations.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['annotationId',annotationId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TAnnotation) as TAnnotation;
end;

Function TAnnotationsResource.Update(annotationId: string; aAnnotation : TAnnotation; AQuery : string = '') : TAnnotation;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'v1/annotations/{annotationId}';
  _Methodid   = 'genomics.annotations.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['annotationId',annotationId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aAnnotation,TAnnotation) as TAnnotation;
end;


Function TAnnotationsResource.Update(annotationId: string; aAnnotation : TAnnotation; AQuery : TAnnotationsupdateOptions) : TAnnotation;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'updateMask',AQuery.updateMask);
  Result:=Update(annotationId,aAnnotation,_Q);
end;

Function TAnnotationsResource.Delete(annotationId: string) : TEmpty;

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'v1/annotations/{annotationId}';
  _Methodid   = 'genomics.annotations.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['annotationId',annotationId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TEmpty) as TEmpty;
end;

Function TAnnotationsResource.Search(aSearchAnnotationsRequest : TSearchAnnotationsRequest) : TSearchAnnotationsResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/annotations/search';
  _Methodid   = 'genomics.annotations.search';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aSearchAnnotationsRequest,TSearchAnnotationsResponse) as TSearchAnnotationsResponse;
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
  Result:=TgenomicsAPI;
end;

Function TDatasetsResource.List(AQuery : string = '') : TListDatasetsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/datasets';
  _Methodid   = 'genomics.datasets.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TListDatasetsResponse) as TListDatasetsResponse;
end;


Function TDatasetsResource.List(AQuery : TDatasetslistOptions) : TListDatasetsResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'projectId',AQuery.projectId);
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(_Q);
end;

Function TDatasetsResource.Create(aDataset : TDataset) : TDataset;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/datasets';
  _Methodid   = 'genomics.datasets.create';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aDataset,TDataset) as TDataset;
end;

Function TDatasetsResource.Get(datasetId: string) : TDataset;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/datasets/{datasetId}';
  _Methodid   = 'genomics.datasets.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['datasetId',datasetId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TDataset) as TDataset;
end;

Function TDatasetsResource.Patch(datasetId: string; aDataset : TDataset; AQuery : string = '') : TDataset;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'v1/datasets/{datasetId}';
  _Methodid   = 'genomics.datasets.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['datasetId',datasetId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aDataset,TDataset) as TDataset;
end;


Function TDatasetsResource.Patch(datasetId: string; aDataset : TDataset; AQuery : TDatasetspatchOptions) : TDataset;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'updateMask',AQuery.updateMask);
  Result:=Patch(datasetId,aDataset,_Q);
end;

Function TDatasetsResource.Delete(datasetId: string) : TEmpty;

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'v1/datasets/{datasetId}';
  _Methodid   = 'genomics.datasets.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['datasetId',datasetId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TEmpty) as TEmpty;
end;

Function TDatasetsResource.Undelete(datasetId: string; aUndeleteDatasetRequest : TUndeleteDatasetRequest) : TDataset;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/datasets/{datasetId}:undelete';
  _Methodid   = 'genomics.datasets.undelete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['datasetId',datasetId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aUndeleteDatasetRequest,TDataset) as TDataset;
end;

Function TDatasetsResource.SetIamPolicy(resource: string; aSetIamPolicyRequest : TSetIamPolicyRequest) : TPolicy;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/{+resource}:setIamPolicy';
  _Methodid   = 'genomics.datasets.setIamPolicy';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['resource',resource]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aSetIamPolicyRequest,TPolicy) as TPolicy;
end;

Function TDatasetsResource.GetIamPolicy(resource: string; aGetIamPolicyRequest : TGetIamPolicyRequest) : TPolicy;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/{+resource}:getIamPolicy';
  _Methodid   = 'genomics.datasets.getIamPolicy';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['resource',resource]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aGetIamPolicyRequest,TPolicy) as TPolicy;
end;

Function TDatasetsResource.TestIamPermissions(resource: string; aTestIamPermissionsRequest : TTestIamPermissionsRequest) : TTestIamPermissionsResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/{+resource}:testIamPermissions';
  _Methodid   = 'genomics.datasets.testIamPermissions';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['resource',resource]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aTestIamPermissionsRequest,TTestIamPermissionsResponse) as TTestIamPermissionsResponse;
end;



{ --------------------------------------------------------------------
  TOperationsResource
  --------------------------------------------------------------------}


Class Function TOperationsResource.ResourceName : String;

begin
  Result:='operations';
end;

Class Function TOperationsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TgenomicsAPI;
end;

Function TOperationsResource.Get(_name: string) : TOperation;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/{+name}';
  _Methodid   = 'genomics.operations.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TOperation) as TOperation;
end;

Function TOperationsResource.List(_name: string; AQuery : string = '') : TListOperationsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/{+name}';
  _Methodid   = 'genomics.operations.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListOperationsResponse) as TListOperationsResponse;
end;


Function TOperationsResource.List(_name: string; AQuery : TOperationslistOptions) : TListOperationsResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'filter',AQuery.filter);
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  Result:=List(_name,_Q);
end;

Function TOperationsResource.Cancel(_name: string; aCancelOperationRequest : TCancelOperationRequest) : TEmpty;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/{+name}:cancel';
  _Methodid   = 'genomics.operations.cancel';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['name',_name]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aCancelOperationRequest,TEmpty) as TEmpty;
end;



{ --------------------------------------------------------------------
  TReadgroupsetsCoveragebucketsResource
  --------------------------------------------------------------------}


Class Function TReadgroupsetsCoveragebucketsResource.ResourceName : String;

begin
  Result:='coveragebuckets';
end;

Class Function TReadgroupsetsCoveragebucketsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TgenomicsAPI;
end;

Function TReadgroupsetsCoveragebucketsResource.List(readGroupSetId: string; AQuery : string = '') : TListCoverageBucketsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/readgroupsets/{readGroupSetId}/coveragebuckets';
  _Methodid   = 'genomics.readgroupsets.coveragebuckets.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['readGroupSetId',readGroupSetId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListCoverageBucketsResponse) as TListCoverageBucketsResponse;
end;


Function TReadgroupsetsCoveragebucketsResource.List(readGroupSetId: string; AQuery : TReadgroupsetsCoveragebucketslistOptions) : TListCoverageBucketsResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'referenceName',AQuery.referenceName);
  AddToQuery(_Q,'start',AQuery.start);
  AddToQuery(_Q,'end',AQuery._end);
  AddToQuery(_Q,'targetBucketWidth',AQuery.targetBucketWidth);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  Result:=List(readGroupSetId,_Q);
end;



{ --------------------------------------------------------------------
  TReadgroupsetsResource
  --------------------------------------------------------------------}


Class Function TReadgroupsetsResource.ResourceName : String;

begin
  Result:='readgroupsets';
end;

Class Function TReadgroupsetsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TgenomicsAPI;
end;

Function TReadgroupsetsResource.Import(aImportReadGroupSetsRequest : TImportReadGroupSetsRequest) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/readgroupsets:import';
  _Methodid   = 'genomics.readgroupsets.import';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aImportReadGroupSetsRequest,TOperation) as TOperation;
end;

Function TReadgroupsetsResource.Export(readGroupSetId: string; aExportReadGroupSetRequest : TExportReadGroupSetRequest) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/readgroupsets/{readGroupSetId}:export';
  _Methodid   = 'genomics.readgroupsets.export';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['readGroupSetId',readGroupSetId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aExportReadGroupSetRequest,TOperation) as TOperation;
end;

Function TReadgroupsetsResource.Search(aSearchReadGroupSetsRequest : TSearchReadGroupSetsRequest) : TSearchReadGroupSetsResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/readgroupsets/search';
  _Methodid   = 'genomics.readgroupsets.search';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aSearchReadGroupSetsRequest,TSearchReadGroupSetsResponse) as TSearchReadGroupSetsResponse;
end;

Function TReadgroupsetsResource.Patch(readGroupSetId: string; aReadGroupSet : TReadGroupSet; AQuery : string = '') : TReadGroupSet;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'v1/readgroupsets/{readGroupSetId}';
  _Methodid   = 'genomics.readgroupsets.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['readGroupSetId',readGroupSetId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aReadGroupSet,TReadGroupSet) as TReadGroupSet;
end;


Function TReadgroupsetsResource.Patch(readGroupSetId: string; aReadGroupSet : TReadGroupSet; AQuery : TReadgroupsetspatchOptions) : TReadGroupSet;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'updateMask',AQuery.updateMask);
  Result:=Patch(readGroupSetId,aReadGroupSet,_Q);
end;

Function TReadgroupsetsResource.Delete(readGroupSetId: string) : TEmpty;

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'v1/readgroupsets/{readGroupSetId}';
  _Methodid   = 'genomics.readgroupsets.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['readGroupSetId',readGroupSetId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TEmpty) as TEmpty;
end;

Function TReadgroupsetsResource.Get(readGroupSetId: string) : TReadGroupSet;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/readgroupsets/{readGroupSetId}';
  _Methodid   = 'genomics.readgroupsets.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['readGroupSetId',readGroupSetId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TReadGroupSet) as TReadGroupSet;
end;



Function TReadgroupsetsResource.GetCoveragebucketsInstance : TReadgroupsetsCoveragebucketsResource;

begin
  if (FCoveragebucketsInstance=Nil) then
    FCoveragebucketsInstance:=CreateCoveragebucketsResource;
  Result:=FCoveragebucketsInstance;
end;

Function TReadgroupsetsResource.CreateCoveragebucketsResource : TReadgroupsetsCoveragebucketsResource;

begin
  Result:=CreateCoveragebucketsResource(Self);
end;


Function TReadgroupsetsResource.CreateCoveragebucketsResource(AOwner : TComponent) : TReadgroupsetsCoveragebucketsResource;

begin
  Result:=TReadgroupsetsCoveragebucketsResource.Create(AOwner);
  Result.API:=Self.API;
end;



{ --------------------------------------------------------------------
  TReadsResource
  --------------------------------------------------------------------}


Class Function TReadsResource.ResourceName : String;

begin
  Result:='reads';
end;

Class Function TReadsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TgenomicsAPI;
end;

Function TReadsResource.Search(aSearchReadsRequest : TSearchReadsRequest) : TSearchReadsResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/reads/search';
  _Methodid   = 'genomics.reads.search';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aSearchReadsRequest,TSearchReadsResponse) as TSearchReadsResponse;
end;

Function TReadsResource.Stream(aStreamReadsRequest : TStreamReadsRequest) : TStreamReadsResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/reads:stream';
  _Methodid   = 'genomics.reads.stream';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aStreamReadsRequest,TStreamReadsResponse) as TStreamReadsResponse;
end;



{ --------------------------------------------------------------------
  TReferencesetsResource
  --------------------------------------------------------------------}


Class Function TReferencesetsResource.ResourceName : String;

begin
  Result:='referencesets';
end;

Class Function TReferencesetsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TgenomicsAPI;
end;

Function TReferencesetsResource.Search(aSearchReferenceSetsRequest : TSearchReferenceSetsRequest) : TSearchReferenceSetsResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/referencesets/search';
  _Methodid   = 'genomics.referencesets.search';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aSearchReferenceSetsRequest,TSearchReferenceSetsResponse) as TSearchReferenceSetsResponse;
end;

Function TReferencesetsResource.Get(referenceSetId: string) : TReferenceSet;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/referencesets/{referenceSetId}';
  _Methodid   = 'genomics.referencesets.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['referenceSetId',referenceSetId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TReferenceSet) as TReferenceSet;
end;



{ --------------------------------------------------------------------
  TReferencesBasesResource
  --------------------------------------------------------------------}


Class Function TReferencesBasesResource.ResourceName : String;

begin
  Result:='bases';
end;

Class Function TReferencesBasesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TgenomicsAPI;
end;

Function TReferencesBasesResource.List(referenceId: string; AQuery : string = '') : TListBasesResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/references/{referenceId}/bases';
  _Methodid   = 'genomics.references.bases.list';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['referenceId',referenceId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,Nil,TListBasesResponse) as TListBasesResponse;
end;


Function TReferencesBasesResource.List(referenceId: string; AQuery : TReferencesBaseslistOptions) : TListBasesResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'start',AQuery.start);
  AddToQuery(_Q,'end',AQuery._end);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  Result:=List(referenceId,_Q);
end;



{ --------------------------------------------------------------------
  TReferencesResource
  --------------------------------------------------------------------}


Class Function TReferencesResource.ResourceName : String;

begin
  Result:='references';
end;

Class Function TReferencesResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TgenomicsAPI;
end;

Function TReferencesResource.Search(aSearchReferencesRequest : TSearchReferencesRequest) : TSearchReferencesResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/references/search';
  _Methodid   = 'genomics.references.search';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aSearchReferencesRequest,TSearchReferencesResponse) as TSearchReferencesResponse;
end;

Function TReferencesResource.Get(referenceId: string) : TReference;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/references/{referenceId}';
  _Methodid   = 'genomics.references.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['referenceId',referenceId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TReference) as TReference;
end;



Function TReferencesResource.GetBasesInstance : TReferencesBasesResource;

begin
  if (FBasesInstance=Nil) then
    FBasesInstance:=CreateBasesResource;
  Result:=FBasesInstance;
end;

Function TReferencesResource.CreateBasesResource : TReferencesBasesResource;

begin
  Result:=CreateBasesResource(Self);
end;


Function TReferencesResource.CreateBasesResource(AOwner : TComponent) : TReferencesBasesResource;

begin
  Result:=TReferencesBasesResource.Create(AOwner);
  Result.API:=Self.API;
end;



{ --------------------------------------------------------------------
  TVariantsResource
  --------------------------------------------------------------------}


Class Function TVariantsResource.ResourceName : String;

begin
  Result:='variants';
end;

Class Function TVariantsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TgenomicsAPI;
end;

Function TVariantsResource.Import(aImportVariantsRequest : TImportVariantsRequest) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/variants:import';
  _Methodid   = 'genomics.variants.import';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aImportVariantsRequest,TOperation) as TOperation;
end;

Function TVariantsResource.Search(aSearchVariantsRequest : TSearchVariantsRequest) : TSearchVariantsResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/variants/search';
  _Methodid   = 'genomics.variants.search';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aSearchVariantsRequest,TSearchVariantsResponse) as TSearchVariantsResponse;
end;

Function TVariantsResource.Create(aVariant : TVariant) : TVariant;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/variants';
  _Methodid   = 'genomics.variants.create';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aVariant,TVariant) as TVariant;
end;

Function TVariantsResource.Patch(variantId: string; aVariant : TVariant; AQuery : string = '') : TVariant;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'v1/variants/{variantId}';
  _Methodid   = 'genomics.variants.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['variantId',variantId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aVariant,TVariant) as TVariant;
end;


Function TVariantsResource.Patch(variantId: string; aVariant : TVariant; AQuery : TVariantspatchOptions) : TVariant;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'updateMask',AQuery.updateMask);
  Result:=Patch(variantId,aVariant,_Q);
end;

Function TVariantsResource.Delete(variantId: string) : TEmpty;

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'v1/variants/{variantId}';
  _Methodid   = 'genomics.variants.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['variantId',variantId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TEmpty) as TEmpty;
end;

Function TVariantsResource.Get(variantId: string) : TVariant;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/variants/{variantId}';
  _Methodid   = 'genomics.variants.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['variantId',variantId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TVariant) as TVariant;
end;

Function TVariantsResource.Merge(aMergeVariantsRequest : TMergeVariantsRequest) : TEmpty;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/variants:merge';
  _Methodid   = 'genomics.variants.merge';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aMergeVariantsRequest,TEmpty) as TEmpty;
end;

Function TVariantsResource.Stream(aStreamVariantsRequest : TStreamVariantsRequest) : TStreamVariantsResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/variants:stream';
  _Methodid   = 'genomics.variants.stream';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aStreamVariantsRequest,TStreamVariantsResponse) as TStreamVariantsResponse;
end;



{ --------------------------------------------------------------------
  TVariantsetsResource
  --------------------------------------------------------------------}


Class Function TVariantsetsResource.ResourceName : String;

begin
  Result:='variantsets';
end;

Class Function TVariantsetsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TgenomicsAPI;
end;

Function TVariantsetsResource.Create(aVariantSet : TVariantSet) : TVariantSet;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/variantsets';
  _Methodid   = 'genomics.variantsets.create';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aVariantSet,TVariantSet) as TVariantSet;
end;

Function TVariantsetsResource.Export(variantSetId: string; aExportVariantSetRequest : TExportVariantSetRequest) : TOperation;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/variantsets/{variantSetId}:export';
  _Methodid   = 'genomics.variantsets.export';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['variantSetId',variantSetId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aExportVariantSetRequest,TOperation) as TOperation;
end;

Function TVariantsetsResource.Get(variantSetId: string) : TVariantSet;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/variantsets/{variantSetId}';
  _Methodid   = 'genomics.variantsets.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['variantSetId',variantSetId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TVariantSet) as TVariantSet;
end;

Function TVariantsetsResource.Search(aSearchVariantSetsRequest : TSearchVariantSetsRequest) : TSearchVariantSetsResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/variantsets/search';
  _Methodid   = 'genomics.variantsets.search';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aSearchVariantSetsRequest,TSearchVariantSetsResponse) as TSearchVariantSetsResponse;
end;

Function TVariantsetsResource.Delete(variantSetId: string) : TEmpty;

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'v1/variantsets/{variantSetId}';
  _Methodid   = 'genomics.variantsets.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['variantSetId',variantSetId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TEmpty) as TEmpty;
end;

Function TVariantsetsResource.Patch(variantSetId: string; aVariantSet : TVariantSet; AQuery : string = '') : TVariantSet;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'v1/variantsets/{variantSetId}';
  _Methodid   = 'genomics.variantsets.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['variantSetId',variantSetId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aVariantSet,TVariantSet) as TVariantSet;
end;


Function TVariantsetsResource.Patch(variantSetId: string; aVariantSet : TVariantSet; AQuery : TVariantsetspatchOptions) : TVariantSet;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'updateMask',AQuery.updateMask);
  Result:=Patch(variantSetId,aVariantSet,_Q);
end;



{ --------------------------------------------------------------------
  TCallsetsResource
  --------------------------------------------------------------------}


Class Function TCallsetsResource.ResourceName : String;

begin
  Result:='callsets';
end;

Class Function TCallsetsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TgenomicsAPI;
end;

Function TCallsetsResource.Search(aSearchCallSetsRequest : TSearchCallSetsRequest) : TSearchCallSetsResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/callsets/search';
  _Methodid   = 'genomics.callsets.search';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aSearchCallSetsRequest,TSearchCallSetsResponse) as TSearchCallSetsResponse;
end;

Function TCallsetsResource.Create(aCallSet : TCallSet) : TCallSet;

Const
  _HTTPMethod = 'POST';
  _Path       = 'v1/callsets';
  _Methodid   = 'genomics.callsets.create';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aCallSet,TCallSet) as TCallSet;
end;

Function TCallsetsResource.Patch(callSetId: string; aCallSet : TCallSet; AQuery : string = '') : TCallSet;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'v1/callsets/{callSetId}';
  _Methodid   = 'genomics.callsets.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['callSetId',callSetId]);
  Result:=ServiceCall(_HTTPMethod,_P,AQuery,aCallSet,TCallSet) as TCallSet;
end;


Function TCallsetsResource.Patch(callSetId: string; aCallSet : TCallSet; AQuery : TCallsetspatchOptions) : TCallSet;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'updateMask',AQuery.updateMask);
  Result:=Patch(callSetId,aCallSet,_Q);
end;

Function TCallsetsResource.Delete(callSetId: string) : TEmpty;

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'v1/callsets/{callSetId}';
  _Methodid   = 'genomics.callsets.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['callSetId',callSetId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TEmpty) as TEmpty;
end;

Function TCallsetsResource.Get(callSetId: string) : TCallSet;

Const
  _HTTPMethod = 'GET';
  _Path       = 'v1/callsets/{callSetId}';
  _Methodid   = 'genomics.callsets.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['callSetId',callSetId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TCallSet) as TCallSet;
end;



{ --------------------------------------------------------------------
  TGenomicsAPI
  --------------------------------------------------------------------}

Class Function TGenomicsAPI.APIName : String;

begin
  Result:='genomics';
end;

Class Function TGenomicsAPI.APIVersion : String;

begin
  Result:='v1';
end;

Class Function TGenomicsAPI.APIRevision : String;

begin
  Result:='20160519';
end;

Class Function TGenomicsAPI.APIID : String;

begin
  Result:='genomics:v1';
end;

Class Function TGenomicsAPI.APITitle : String;

begin
  Result:='Genomics API';
end;

Class Function TGenomicsAPI.APIDescription : String;

begin
  Result:='Stores, processes, explores and shares genomic data. This API implements the Global Alliance for Genomics and Health (GA4GH) v0.5.1 API as well as several extensions.';
end;

Class Function TGenomicsAPI.APIOwnerDomain : String;

begin
  Result:='google.com';
end;

Class Function TGenomicsAPI.APIOwnerName : String;

begin
  Result:='Google';
end;

Class Function TGenomicsAPI.APIIcon16 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-16.gif';
end;

Class Function TGenomicsAPI.APIIcon32 : String;

begin
  Result:='http://www.google.com/images/icons/product/search-32.gif';
end;

Class Function TGenomicsAPI.APIdocumentationLink : String;

begin
  Result:='https://cloud.google.com/genomics/';
end;

Class Function TGenomicsAPI.APIrootUrl : string;

begin
  Result:='https://genomics.googleapis.com/';
end;

Class Function TGenomicsAPI.APIbasePath : string;

begin
  Result:='';
end;

Class Function TGenomicsAPI.APIbaseURL : String;

begin
  Result:='https://genomics.googleapis.com/';
end;

Class Function TGenomicsAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TGenomicsAPI.APIservicePath : string;

begin
  Result:='';
end;

Class Function TGenomicsAPI.APIbatchPath : String;

begin
  Result:='batch';
end;

Class Function TGenomicsAPI.APIAuthScopes : TScopeInfoArray;

begin
  SetLength(Result,5);
  Result[0].Name:='https://www.googleapis.com/auth/bigquery';
  Result[0].Description:='View and manage your data in Google BigQuery';
  Result[1].Name:='https://www.googleapis.com/auth/cloud-platform';
  Result[1].Description:='View and manage your data across Google Cloud Platform services';
  Result[2].Name:='https://www.googleapis.com/auth/devstorage.read_write';
  Result[2].Description:='Manage your data in Google Cloud Storage';
  Result[3].Name:='https://www.googleapis.com/auth/genomics';
  Result[3].Description:='View and manage Genomics data';
  Result[4].Name:='https://www.googleapis.com/auth/genomics.readonly';
  Result[4].Description:='View Genomics data';
  
end;

Class Function TGenomicsAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TGenomicsAPI.RegisterAPIResources;

begin
  TAnnotationSetTypeinfo.RegisterObject;
  TAnnotationSet.RegisterObject;
  TEmpty.RegisterObject;
  TSearchAnnotationSetsRequest.RegisterObject;
  TSearchAnnotationSetsResponse.RegisterObject;
  TAnnotationTypeinfo.RegisterObject;
  TAnnotation.RegisterObject;
  TVariantAnnotation.RegisterObject;
  TClinicalCondition.RegisterObject;
  TExternalId.RegisterObject;
  TTranscript.RegisterObject;
  TExon.RegisterObject;
  TCodingSequence.RegisterObject;
  TBatchCreateAnnotationsRequest.RegisterObject;
  TBatchCreateAnnotationsResponse.RegisterObject;
  TEntry.RegisterObject;
  TStatusTypedetailsItem.RegisterObject;
  TStatus.RegisterObject;
  TSearchAnnotationsRequest.RegisterObject;
  TSearchAnnotationsResponse.RegisterObject;
  TListDatasetsResponse.RegisterObject;
  TDataset.RegisterObject;
  TUndeleteDatasetRequest.RegisterObject;
  TSetIamPolicyRequest.RegisterObject;
  TPolicy.RegisterObject;
  TBinding.RegisterObject;
  TGetIamPolicyRequest.RegisterObject;
  TTestIamPermissionsRequest.RegisterObject;
  TTestIamPermissionsResponse.RegisterObject;
  TOperationTypemetadata.RegisterObject;
  TOperationTyperesponse.RegisterObject;
  TOperation.RegisterObject;
  TListOperationsResponse.RegisterObject;
  TCancelOperationRequest.RegisterObject;
  TImportReadGroupSetsRequest.RegisterObject;
  TExportReadGroupSetRequest.RegisterObject;
  TSearchReadGroupSetsRequest.RegisterObject;
  TSearchReadGroupSetsResponse.RegisterObject;
  TReadGroupSetTypeinfo.RegisterObject;
  TReadGroupSet.RegisterObject;
  TReadGroupTypeinfo.RegisterObject;
  TReadGroup.RegisterObject;
  TExperiment.RegisterObject;
  TProgram.RegisterObject;
  TListCoverageBucketsResponse.RegisterObject;
  TCoverageBucket.RegisterObject;
  TRange.RegisterObject;
  TSearchReadsRequest.RegisterObject;
  TSearchReadsResponse.RegisterObject;
  TReadTypeinfo.RegisterObject;
  TRead.RegisterObject;
  TLinearAlignment.RegisterObject;
  TPosition.RegisterObject;
  TCigarUnit.RegisterObject;
  TStreamReadsRequest.RegisterObject;
  TStreamReadsResponse.RegisterObject;
  TSearchReferenceSetsRequest.RegisterObject;
  TSearchReferenceSetsResponse.RegisterObject;
  TReferenceSet.RegisterObject;
  TSearchReferencesRequest.RegisterObject;
  TSearchReferencesResponse.RegisterObject;
  TReference.RegisterObject;
  TListBasesResponse.RegisterObject;
  TImportVariantsRequestTypeinfoMergeConfig.RegisterObject;
  TImportVariantsRequest.RegisterObject;
  TVariantSet.RegisterObject;
  TReferenceBound.RegisterObject;
  TVariantSetMetadataTypeinfo.RegisterObject;
  TVariantSetMetadata.RegisterObject;
  TExportVariantSetRequest.RegisterObject;
  TSearchVariantSetsRequest.RegisterObject;
  TSearchVariantSetsResponse.RegisterObject;
  TSearchVariantsRequest.RegisterObject;
  TSearchVariantsResponse.RegisterObject;
  TVariantTypeinfo.RegisterObject;
  TVariant.RegisterObject;
  TVariantCallTypeinfo.RegisterObject;
  TVariantCall.RegisterObject;
  TMergeVariantsRequestTypeinfoMergeConfig.RegisterObject;
  TMergeVariantsRequest.RegisterObject;
  TSearchCallSetsRequest.RegisterObject;
  TSearchCallSetsResponse.RegisterObject;
  TCallSetTypeinfo.RegisterObject;
  TCallSet.RegisterObject;
  TStreamVariantsRequest.RegisterObject;
  TStreamVariantsResponse.RegisterObject;
  TImportReadGroupSetsResponse.RegisterObject;
  TImportVariantsResponse.RegisterObject;
  TOperationMetadataTyperequest.RegisterObject;
  TOperationMetadata.RegisterObject;
  TOperationEvent.RegisterObject;
end;


Function TGenomicsAPI.GetAnnotationsetsInstance : TAnnotationsetsResource;

begin
  if (FAnnotationsetsInstance=Nil) then
    FAnnotationsetsInstance:=CreateAnnotationsetsResource;
  Result:=FAnnotationsetsInstance;
end;

Function TGenomicsAPI.CreateAnnotationsetsResource : TAnnotationsetsResource;

begin
  Result:=CreateAnnotationsetsResource(Self);
end;


Function TGenomicsAPI.CreateAnnotationsetsResource(AOwner : TComponent) : TAnnotationsetsResource;

begin
  Result:=TAnnotationsetsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TGenomicsAPI.GetAnnotationsInstance : TAnnotationsResource;

begin
  if (FAnnotationsInstance=Nil) then
    FAnnotationsInstance:=CreateAnnotationsResource;
  Result:=FAnnotationsInstance;
end;

Function TGenomicsAPI.CreateAnnotationsResource : TAnnotationsResource;

begin
  Result:=CreateAnnotationsResource(Self);
end;


Function TGenomicsAPI.CreateAnnotationsResource(AOwner : TComponent) : TAnnotationsResource;

begin
  Result:=TAnnotationsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TGenomicsAPI.GetDatasetsInstance : TDatasetsResource;

begin
  if (FDatasetsInstance=Nil) then
    FDatasetsInstance:=CreateDatasetsResource;
  Result:=FDatasetsInstance;
end;

Function TGenomicsAPI.CreateDatasetsResource : TDatasetsResource;

begin
  Result:=CreateDatasetsResource(Self);
end;


Function TGenomicsAPI.CreateDatasetsResource(AOwner : TComponent) : TDatasetsResource;

begin
  Result:=TDatasetsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TGenomicsAPI.GetOperationsInstance : TOperationsResource;

begin
  if (FOperationsInstance=Nil) then
    FOperationsInstance:=CreateOperationsResource;
  Result:=FOperationsInstance;
end;

Function TGenomicsAPI.CreateOperationsResource : TOperationsResource;

begin
  Result:=CreateOperationsResource(Self);
end;


Function TGenomicsAPI.CreateOperationsResource(AOwner : TComponent) : TOperationsResource;

begin
  Result:=TOperationsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TGenomicsAPI.GetReadgroupsetsCoveragebucketsInstance : TReadgroupsetsCoveragebucketsResource;

begin
  if (FReadgroupsetsCoveragebucketsInstance=Nil) then
    FReadgroupsetsCoveragebucketsInstance:=CreateReadgroupsetsCoveragebucketsResource;
  Result:=FReadgroupsetsCoveragebucketsInstance;
end;

Function TGenomicsAPI.CreateReadgroupsetsCoveragebucketsResource : TReadgroupsetsCoveragebucketsResource;

begin
  Result:=CreateReadgroupsetsCoveragebucketsResource(Self);
end;


Function TGenomicsAPI.CreateReadgroupsetsCoveragebucketsResource(AOwner : TComponent) : TReadgroupsetsCoveragebucketsResource;

begin
  Result:=TReadgroupsetsCoveragebucketsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TGenomicsAPI.GetReadgroupsetsInstance : TReadgroupsetsResource;

begin
  if (FReadgroupsetsInstance=Nil) then
    FReadgroupsetsInstance:=CreateReadgroupsetsResource;
  Result:=FReadgroupsetsInstance;
end;

Function TGenomicsAPI.CreateReadgroupsetsResource : TReadgroupsetsResource;

begin
  Result:=CreateReadgroupsetsResource(Self);
end;


Function TGenomicsAPI.CreateReadgroupsetsResource(AOwner : TComponent) : TReadgroupsetsResource;

begin
  Result:=TReadgroupsetsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TGenomicsAPI.GetReadsInstance : TReadsResource;

begin
  if (FReadsInstance=Nil) then
    FReadsInstance:=CreateReadsResource;
  Result:=FReadsInstance;
end;

Function TGenomicsAPI.CreateReadsResource : TReadsResource;

begin
  Result:=CreateReadsResource(Self);
end;


Function TGenomicsAPI.CreateReadsResource(AOwner : TComponent) : TReadsResource;

begin
  Result:=TReadsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TGenomicsAPI.GetReferencesetsInstance : TReferencesetsResource;

begin
  if (FReferencesetsInstance=Nil) then
    FReferencesetsInstance:=CreateReferencesetsResource;
  Result:=FReferencesetsInstance;
end;

Function TGenomicsAPI.CreateReferencesetsResource : TReferencesetsResource;

begin
  Result:=CreateReferencesetsResource(Self);
end;


Function TGenomicsAPI.CreateReferencesetsResource(AOwner : TComponent) : TReferencesetsResource;

begin
  Result:=TReferencesetsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TGenomicsAPI.GetReferencesBasesInstance : TReferencesBasesResource;

begin
  if (FReferencesBasesInstance=Nil) then
    FReferencesBasesInstance:=CreateReferencesBasesResource;
  Result:=FReferencesBasesInstance;
end;

Function TGenomicsAPI.CreateReferencesBasesResource : TReferencesBasesResource;

begin
  Result:=CreateReferencesBasesResource(Self);
end;


Function TGenomicsAPI.CreateReferencesBasesResource(AOwner : TComponent) : TReferencesBasesResource;

begin
  Result:=TReferencesBasesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TGenomicsAPI.GetReferencesInstance : TReferencesResource;

begin
  if (FReferencesInstance=Nil) then
    FReferencesInstance:=CreateReferencesResource;
  Result:=FReferencesInstance;
end;

Function TGenomicsAPI.CreateReferencesResource : TReferencesResource;

begin
  Result:=CreateReferencesResource(Self);
end;


Function TGenomicsAPI.CreateReferencesResource(AOwner : TComponent) : TReferencesResource;

begin
  Result:=TReferencesResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TGenomicsAPI.GetVariantsInstance : TVariantsResource;

begin
  if (FVariantsInstance=Nil) then
    FVariantsInstance:=CreateVariantsResource;
  Result:=FVariantsInstance;
end;

Function TGenomicsAPI.CreateVariantsResource : TVariantsResource;

begin
  Result:=CreateVariantsResource(Self);
end;


Function TGenomicsAPI.CreateVariantsResource(AOwner : TComponent) : TVariantsResource;

begin
  Result:=TVariantsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TGenomicsAPI.GetVariantsetsInstance : TVariantsetsResource;

begin
  if (FVariantsetsInstance=Nil) then
    FVariantsetsInstance:=CreateVariantsetsResource;
  Result:=FVariantsetsInstance;
end;

Function TGenomicsAPI.CreateVariantsetsResource : TVariantsetsResource;

begin
  Result:=CreateVariantsetsResource(Self);
end;


Function TGenomicsAPI.CreateVariantsetsResource(AOwner : TComponent) : TVariantsetsResource;

begin
  Result:=TVariantsetsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TGenomicsAPI.GetCallsetsInstance : TCallsetsResource;

begin
  if (FCallsetsInstance=Nil) then
    FCallsetsInstance:=CreateCallsetsResource;
  Result:=FCallsetsInstance;
end;

Function TGenomicsAPI.CreateCallsetsResource : TCallsetsResource;

begin
  Result:=CreateCallsetsResource(Self);
end;


Function TGenomicsAPI.CreateCallsetsResource(AOwner : TComponent) : TCallsetsResource;

begin
  Result:=TCallsetsResource.Create(AOwner);
  Result.API:=Self.API;
end;



initialization
  TGenomicsAPI.RegisterAPI;
end.
