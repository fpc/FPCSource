unit googlegenomics;
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
//Generated on: 16-5-15 08:53:04
{$MODE objfpc}
{$H+}

interface

uses sysutils, classes, googleservice, restbase, googlebase;

type
  
  //Top-level schema types
  TAlignReadGroupSetsRequest = Class;
  TAlignReadGroupSetsResponse = Class;
  TAnnotation = Class;
  TAnnotationSet = Class;
  TBatchAnnotationsResponse = Class;
  TBatchAnnotationsResponseEntry = Class;
  TBatchAnnotationsResponseEntryStatus = Class;
  TBatchCreateAnnotationsRequest = Class;
  TCall = Class;
  TCallReadGroupSetsRequest = Class;
  TCallReadGroupSetsResponse = Class;
  TCallSet = Class;
  TCigarUnit = Class;
  TCoverageBucket = Class;
  TDataset = Class;
  TExperimentalCreateJobRequest = Class;
  TExperimentalCreateJobResponse = Class;
  TExportReadGroupSetsRequest = Class;
  TExportReadGroupSetsResponse = Class;
  TExportVariantSetRequest = Class;
  TExportVariantSetResponse = Class;
  TExternalId = Class;
  TFastqMetadata = Class;
  TImportReadGroupSetsRequest = Class;
  TImportReadGroupSetsResponse = Class;
  TImportVariantsRequest = Class;
  TImportVariantsResponse = Class;
  TInt32Value = Class;
  TInterleavedFastqSource = Class;
  TJob = Class;
  TJobRequest = Class;
  TLinearAlignment = Class;
  TListBasesResponse = Class;
  TListCoverageBucketsResponse = Class;
  TListDatasetsResponse = Class;
  TMergeVariantsRequest = Class;
  TMetadata = Class;
  TPairedFastqSource = Class;
  TPosition = Class;
  TQueryRange = Class;
  TRange = Class;
  TRangePosition = Class;
  TRead = Class;
  TReadGroup = Class;
  TReadGroupExperiment = Class;
  TReadGroupProgram = Class;
  TReadGroupSet = Class;
  TReference = Class;
  TReferenceBound = Class;
  TReferenceSet = Class;
  TSearchAnnotationSetsRequest = Class;
  TSearchAnnotationSetsResponse = Class;
  TSearchAnnotationsRequest = Class;
  TSearchAnnotationsResponse = Class;
  TSearchCallSetsRequest = Class;
  TSearchCallSetsResponse = Class;
  TSearchJobsRequest = Class;
  TSearchJobsResponse = Class;
  TSearchReadGroupSetsRequest = Class;
  TSearchReadGroupSetsResponse = Class;
  TSearchReadsRequest = Class;
  TSearchReadsResponse = Class;
  TSearchReferenceSetsRequest = Class;
  TSearchReferenceSetsResponse = Class;
  TSearchReferencesRequest = Class;
  TSearchReferencesResponse = Class;
  TSearchVariantSetsRequest = Class;
  TSearchVariantSetsResponse = Class;
  TSearchVariantsRequest = Class;
  TSearchVariantsResponse = Class;
  TTranscript = Class;
  TTranscriptCodingSequence = Class;
  TTranscriptExon = Class;
  TVariant = Class;
  TVariantAnnotation = Class;
  TVariantAnnotationCondition = Class;
  TVariantSet = Class;
  TAlignReadGroupSetsRequestArray = Array of TAlignReadGroupSetsRequest;
  TAlignReadGroupSetsResponseArray = Array of TAlignReadGroupSetsResponse;
  TAnnotationArray = Array of TAnnotation;
  TAnnotationSetArray = Array of TAnnotationSet;
  TBatchAnnotationsResponseArray = Array of TBatchAnnotationsResponse;
  TBatchAnnotationsResponseEntryArray = Array of TBatchAnnotationsResponseEntry;
  TBatchAnnotationsResponseEntryStatusArray = Array of TBatchAnnotationsResponseEntryStatus;
  TBatchCreateAnnotationsRequestArray = Array of TBatchCreateAnnotationsRequest;
  TCallArray = Array of TCall;
  TCallReadGroupSetsRequestArray = Array of TCallReadGroupSetsRequest;
  TCallReadGroupSetsResponseArray = Array of TCallReadGroupSetsResponse;
  TCallSetArray = Array of TCallSet;
  TCigarUnitArray = Array of TCigarUnit;
  TCoverageBucketArray = Array of TCoverageBucket;
  TDatasetArray = Array of TDataset;
  TExperimentalCreateJobRequestArray = Array of TExperimentalCreateJobRequest;
  TExperimentalCreateJobResponseArray = Array of TExperimentalCreateJobResponse;
  TExportReadGroupSetsRequestArray = Array of TExportReadGroupSetsRequest;
  TExportReadGroupSetsResponseArray = Array of TExportReadGroupSetsResponse;
  TExportVariantSetRequestArray = Array of TExportVariantSetRequest;
  TExportVariantSetResponseArray = Array of TExportVariantSetResponse;
  TExternalIdArray = Array of TExternalId;
  TFastqMetadataArray = Array of TFastqMetadata;
  TImportReadGroupSetsRequestArray = Array of TImportReadGroupSetsRequest;
  TImportReadGroupSetsResponseArray = Array of TImportReadGroupSetsResponse;
  TImportVariantsRequestArray = Array of TImportVariantsRequest;
  TImportVariantsResponseArray = Array of TImportVariantsResponse;
  TInt32ValueArray = Array of TInt32Value;
  TInterleavedFastqSourceArray = Array of TInterleavedFastqSource;
  TJobArray = Array of TJob;
  TJobRequestArray = Array of TJobRequest;
  TLinearAlignmentArray = Array of TLinearAlignment;
  TListBasesResponseArray = Array of TListBasesResponse;
  TListCoverageBucketsResponseArray = Array of TListCoverageBucketsResponse;
  TListDatasetsResponseArray = Array of TListDatasetsResponse;
  TMergeVariantsRequestArray = Array of TMergeVariantsRequest;
  TMetadataArray = Array of TMetadata;
  TPairedFastqSourceArray = Array of TPairedFastqSource;
  TPositionArray = Array of TPosition;
  TQueryRangeArray = Array of TQueryRange;
  TRangeArray = Array of TRange;
  TRangePositionArray = Array of TRangePosition;
  TReadArray = Array of TRead;
  TReadGroupArray = Array of TReadGroup;
  TReadGroupExperimentArray = Array of TReadGroupExperiment;
  TReadGroupProgramArray = Array of TReadGroupProgram;
  TReadGroupSetArray = Array of TReadGroupSet;
  TReferenceArray = Array of TReference;
  TReferenceBoundArray = Array of TReferenceBound;
  TReferenceSetArray = Array of TReferenceSet;
  TSearchAnnotationSetsRequestArray = Array of TSearchAnnotationSetsRequest;
  TSearchAnnotationSetsResponseArray = Array of TSearchAnnotationSetsResponse;
  TSearchAnnotationsRequestArray = Array of TSearchAnnotationsRequest;
  TSearchAnnotationsResponseArray = Array of TSearchAnnotationsResponse;
  TSearchCallSetsRequestArray = Array of TSearchCallSetsRequest;
  TSearchCallSetsResponseArray = Array of TSearchCallSetsResponse;
  TSearchJobsRequestArray = Array of TSearchJobsRequest;
  TSearchJobsResponseArray = Array of TSearchJobsResponse;
  TSearchReadGroupSetsRequestArray = Array of TSearchReadGroupSetsRequest;
  TSearchReadGroupSetsResponseArray = Array of TSearchReadGroupSetsResponse;
  TSearchReadsRequestArray = Array of TSearchReadsRequest;
  TSearchReadsResponseArray = Array of TSearchReadsResponse;
  TSearchReferenceSetsRequestArray = Array of TSearchReferenceSetsRequest;
  TSearchReferenceSetsResponseArray = Array of TSearchReferenceSetsResponse;
  TSearchReferencesRequestArray = Array of TSearchReferencesRequest;
  TSearchReferencesResponseArray = Array of TSearchReferencesResponse;
  TSearchVariantSetsRequestArray = Array of TSearchVariantSetsRequest;
  TSearchVariantSetsResponseArray = Array of TSearchVariantSetsResponse;
  TSearchVariantsRequestArray = Array of TSearchVariantsRequest;
  TSearchVariantsResponseArray = Array of TSearchVariantsResponse;
  TTranscriptArray = Array of TTranscript;
  TTranscriptCodingSequenceArray = Array of TTranscriptCodingSequence;
  TTranscriptExonArray = Array of TTranscriptExon;
  TVariantArray = Array of TVariant;
  TVariantAnnotationArray = Array of TVariantAnnotation;
  TVariantAnnotationConditionArray = Array of TVariantAnnotationCondition;
  TVariantSetArray = Array of TVariantSet;
  //Anonymous types, using auto-generated names
  TAnnotationTypeinfo = Class;
  TAnnotationSetTypeinfo = Class;
  TCallTypeinfo = Class;
  TCallSetTypeinfo = Class;
  TMetadataTypeinfo = Class;
  TReadTypeinfo = Class;
  TReadGroupTypeinfo = Class;
  TReadGroupSetTypeinfo = Class;
  TVariantTypeinfo = Class;
  TBatchAnnotationsResponseTypeentriesArray = Array of TBatchAnnotationsResponseEntry;
  TBatchCreateAnnotationsRequestTypeannotationsArray = Array of TAnnotation;
  TLinearAlignmentTypecigarArray = Array of TCigarUnit;
  TListCoverageBucketsResponseTypecoverageBucketsArray = Array of TCoverageBucket;
  TListDatasetsResponseTypedatasetsArray = Array of TDataset;
  TMergeVariantsRequestTypevariantsArray = Array of TVariant;
  TReadGroupTypeprogramsArray = Array of TReadGroupProgram;
  TReadGroupSetTypereadGroupsArray = Array of TReadGroup;
  TSearchAnnotationSetsResponseTypeannotationSetsArray = Array of TAnnotationSet;
  TSearchAnnotationsResponseTypeannotationsArray = Array of TAnnotation;
  TSearchCallSetsResponseTypecallSetsArray = Array of TCallSet;
  TSearchJobsResponseTypejobsArray = Array of TJob;
  TSearchReadGroupSetsResponseTypereadGroupSetsArray = Array of TReadGroupSet;
  TSearchReadsResponseTypealignmentsArray = Array of TRead;
  TSearchReferenceSetsResponseTypereferenceSetsArray = Array of TReferenceSet;
  TSearchReferencesResponseTypereferencesArray = Array of TReference;
  TSearchVariantSetsResponseTypevariantSetsArray = Array of TVariantSet;
  TSearchVariantsResponseTypevariantsArray = Array of TVariant;
  TTranscriptTypeexonsArray = Array of TTranscriptExon;
  TVariantTypecallsArray = Array of TCall;
  TVariantAnnotationTypeconditionsArray = Array of TVariantAnnotationCondition;
  TVariantAnnotationConditionTypeexternalIdsArray = Array of TExternalId;
  TVariantSetTypemetadataArray = Array of TMetadata;
  TVariantSetTypereferenceBoundsArray = Array of TReferenceBound;
  
  { --------------------------------------------------------------------
    TAlignReadGroupSetsRequest
    --------------------------------------------------------------------}
  
  TAlignReadGroupSetsRequest = Class(TGoogleBaseObject)
  Private
    FbamSourceUris : TStringArray;
    FdatasetId : String;
    FinterleavedFastqSource : TInterleavedFastqSource;
    FpairedFastqSource : TPairedFastqSource;
    FreadGroupSetId : String;
  Protected
    //Property setters
    Procedure SetbamSourceUris(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetdatasetId(AIndex : Integer; AValue : String); virtual;
    Procedure SetinterleavedFastqSource(AIndex : Integer; AValue : TInterleavedFastqSource); virtual;
    Procedure SetpairedFastqSource(AIndex : Integer; AValue : TPairedFastqSource); virtual;
    Procedure SetreadGroupSetId(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property bamSourceUris : TStringArray Index 0 Read FbamSourceUris Write SetbamSourceUris;
    Property datasetId : String Index 8 Read FdatasetId Write SetdatasetId;
    Property interleavedFastqSource : TInterleavedFastqSource Index 16 Read FinterleavedFastqSource Write SetinterleavedFastqSource;
    Property pairedFastqSource : TPairedFastqSource Index 24 Read FpairedFastqSource Write SetpairedFastqSource;
    Property readGroupSetId : String Index 32 Read FreadGroupSetId Write SetreadGroupSetId;
  end;
  TAlignReadGroupSetsRequestClass = Class of TAlignReadGroupSetsRequest;
  
  { --------------------------------------------------------------------
    TAlignReadGroupSetsResponse
    --------------------------------------------------------------------}
  
  TAlignReadGroupSetsResponse = Class(TGoogleBaseObject)
  Private
    FjobId : String;
  Protected
    //Property setters
    Procedure SetjobId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property jobId : String Index 0 Read FjobId Write SetjobId;
  end;
  TAlignReadGroupSetsResponseClass = Class of TAlignReadGroupSetsResponse;
  
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
    FannotationSetId : String;
    Fid : String;
    Finfo : TAnnotationTypeinfo;
    Fname : String;
    Fposition : TRangePosition;
    Ftranscript : TTranscript;
    F_type : String;
    Fvariant : TVariantAnnotation;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetannotationSetId(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setinfo(AIndex : Integer; AValue : TAnnotationTypeinfo); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure Setposition(AIndex : Integer; AValue : TRangePosition); virtual;
    Procedure Settranscript(AIndex : Integer; AValue : TTranscript); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
    Procedure Setvariant(AIndex : Integer; AValue : TVariantAnnotation); virtual;
  Public
  Published
    Property annotationSetId : String Index 0 Read FannotationSetId Write SetannotationSetId;
    Property id : String Index 8 Read Fid Write Setid;
    Property info : TAnnotationTypeinfo Index 16 Read Finfo Write Setinfo;
    Property name : String Index 24 Read Fname Write Setname;
    Property position : TRangePosition Index 32 Read Fposition Write Setposition;
    Property transcript : TTranscript Index 40 Read Ftranscript Write Settranscript;
    Property _type : String Index 48 Read F_type Write Set_type;
    Property variant : TVariantAnnotation Index 56 Read Fvariant Write Setvariant;
  end;
  TAnnotationClass = Class of TAnnotation;
  
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
    FdatasetId : String;
    Fid : String;
    Finfo : TAnnotationSetTypeinfo;
    Fname : String;
    FreferenceSetId : String;
    FsourceUri : String;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetdatasetId(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setinfo(AIndex : Integer; AValue : TAnnotationSetTypeinfo); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetreferenceSetId(AIndex : Integer; AValue : String); virtual;
    Procedure SetsourceUri(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property datasetId : String Index 0 Read FdatasetId Write SetdatasetId;
    Property id : String Index 8 Read Fid Write Setid;
    Property info : TAnnotationSetTypeinfo Index 16 Read Finfo Write Setinfo;
    Property name : String Index 24 Read Fname Write Setname;
    Property referenceSetId : String Index 32 Read FreferenceSetId Write SetreferenceSetId;
    Property sourceUri : String Index 40 Read FsourceUri Write SetsourceUri;
    Property _type : String Index 48 Read F_type Write Set_type;
  end;
  TAnnotationSetClass = Class of TAnnotationSet;
  
  { --------------------------------------------------------------------
    TBatchAnnotationsResponse
    --------------------------------------------------------------------}
  
  TBatchAnnotationsResponse = Class(TGoogleBaseObject)
  Private
    Fentries : TBatchAnnotationsResponseTypeentriesArray;
  Protected
    //Property setters
    Procedure Setentries(AIndex : Integer; AValue : TBatchAnnotationsResponseTypeentriesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property entries : TBatchAnnotationsResponseTypeentriesArray Index 0 Read Fentries Write Setentries;
  end;
  TBatchAnnotationsResponseClass = Class of TBatchAnnotationsResponse;
  
  { --------------------------------------------------------------------
    TBatchAnnotationsResponseEntry
    --------------------------------------------------------------------}
  
  TBatchAnnotationsResponseEntry = Class(TGoogleBaseObject)
  Private
    Fannotation : TAnnotation;
    Fstatus : TBatchAnnotationsResponseEntryStatus;
  Protected
    //Property setters
    Procedure Setannotation(AIndex : Integer; AValue : TAnnotation); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : TBatchAnnotationsResponseEntryStatus); virtual;
  Public
  Published
    Property annotation : TAnnotation Index 0 Read Fannotation Write Setannotation;
    Property status : TBatchAnnotationsResponseEntryStatus Index 8 Read Fstatus Write Setstatus;
  end;
  TBatchAnnotationsResponseEntryClass = Class of TBatchAnnotationsResponseEntry;
  
  { --------------------------------------------------------------------
    TBatchAnnotationsResponseEntryStatus
    --------------------------------------------------------------------}
  
  TBatchAnnotationsResponseEntryStatus = Class(TGoogleBaseObject)
  Private
    Fcode : integer;
    Fmessage : String;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; AValue : integer); virtual;
    Procedure Setmessage(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property code : integer Index 0 Read Fcode Write Setcode;
    Property message : String Index 8 Read Fmessage Write Setmessage;
  end;
  TBatchAnnotationsResponseEntryStatusClass = Class of TBatchAnnotationsResponseEntryStatus;
  
  { --------------------------------------------------------------------
    TBatchCreateAnnotationsRequest
    --------------------------------------------------------------------}
  
  TBatchCreateAnnotationsRequest = Class(TGoogleBaseObject)
  Private
    Fannotations : TBatchCreateAnnotationsRequestTypeannotationsArray;
  Protected
    //Property setters
    Procedure Setannotations(AIndex : Integer; AValue : TBatchCreateAnnotationsRequestTypeannotationsArray); virtual;
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
    TCallTypeinfo
    --------------------------------------------------------------------}
  
  TCallTypeinfo = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TCallTypeinfoClass = Class of TCallTypeinfo;
  
  { --------------------------------------------------------------------
    TCall
    --------------------------------------------------------------------}
  
  TCall = Class(TGoogleBaseObject)
  Private
    FcallSetId : String;
    FcallSetName : String;
    Fgenotype : TintegerArray;
    FgenotypeLikelihood : TdoubleArray;
    Finfo : TCallTypeinfo;
    Fphaseset : String;
  Protected
    //Property setters
    Procedure SetcallSetId(AIndex : Integer; AValue : String); virtual;
    Procedure SetcallSetName(AIndex : Integer; AValue : String); virtual;
    Procedure Setgenotype(AIndex : Integer; AValue : TintegerArray); virtual;
    Procedure SetgenotypeLikelihood(AIndex : Integer; AValue : TdoubleArray); virtual;
    Procedure Setinfo(AIndex : Integer; AValue : TCallTypeinfo); virtual;
    Procedure Setphaseset(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property callSetId : String Index 0 Read FcallSetId Write SetcallSetId;
    Property callSetName : String Index 8 Read FcallSetName Write SetcallSetName;
    Property genotype : TintegerArray Index 16 Read Fgenotype Write Setgenotype;
    Property genotypeLikelihood : TdoubleArray Index 24 Read FgenotypeLikelihood Write SetgenotypeLikelihood;
    Property info : TCallTypeinfo Index 32 Read Finfo Write Setinfo;
    Property phaseset : String Index 40 Read Fphaseset Write Setphaseset;
  end;
  TCallClass = Class of TCall;
  
  { --------------------------------------------------------------------
    TCallReadGroupSetsRequest
    --------------------------------------------------------------------}
  
  TCallReadGroupSetsRequest = Class(TGoogleBaseObject)
  Private
    FdatasetId : String;
    FreadGroupSetId : String;
    FsourceUris : TStringArray;
  Protected
    //Property setters
    Procedure SetdatasetId(AIndex : Integer; AValue : String); virtual;
    Procedure SetreadGroupSetId(AIndex : Integer; AValue : String); virtual;
    Procedure SetsourceUris(AIndex : Integer; AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property datasetId : String Index 0 Read FdatasetId Write SetdatasetId;
    Property readGroupSetId : String Index 8 Read FreadGroupSetId Write SetreadGroupSetId;
    Property sourceUris : TStringArray Index 16 Read FsourceUris Write SetsourceUris;
  end;
  TCallReadGroupSetsRequestClass = Class of TCallReadGroupSetsRequest;
  
  { --------------------------------------------------------------------
    TCallReadGroupSetsResponse
    --------------------------------------------------------------------}
  
  TCallReadGroupSetsResponse = Class(TGoogleBaseObject)
  Private
    FjobId : String;
  Protected
    //Property setters
    Procedure SetjobId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property jobId : String Index 0 Read FjobId Write SetjobId;
  end;
  TCallReadGroupSetsResponseClass = Class of TCallReadGroupSetsResponse;
  
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
    Fcreated : String;
    Fid : String;
    Finfo : TCallSetTypeinfo;
    Fname : String;
    FsampleId : String;
    FvariantSetIds : TStringArray;
  Protected
    //Property setters
    Procedure Setcreated(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setinfo(AIndex : Integer; AValue : TCallSetTypeinfo); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetsampleId(AIndex : Integer; AValue : String); virtual;
    Procedure SetvariantSetIds(AIndex : Integer; AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property created : String Index 0 Read Fcreated Write Setcreated;
    Property id : String Index 8 Read Fid Write Setid;
    Property info : TCallSetTypeinfo Index 16 Read Finfo Write Setinfo;
    Property name : String Index 24 Read Fname Write Setname;
    Property sampleId : String Index 32 Read FsampleId Write SetsampleId;
    Property variantSetIds : TStringArray Index 40 Read FvariantSetIds Write SetvariantSetIds;
  end;
  TCallSetClass = Class of TCallSet;
  
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
    Procedure Setoperation(AIndex : Integer; AValue : String); virtual;
    Procedure SetoperationLength(AIndex : Integer; AValue : String); virtual;
    Procedure SetreferenceSequence(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property operation : String Index 0 Read Foperation Write Setoperation;
    Property operationLength : String Index 8 Read FoperationLength Write SetoperationLength;
    Property referenceSequence : String Index 16 Read FreferenceSequence Write SetreferenceSequence;
  end;
  TCigarUnitClass = Class of TCigarUnit;
  
  { --------------------------------------------------------------------
    TCoverageBucket
    --------------------------------------------------------------------}
  
  TCoverageBucket = Class(TGoogleBaseObject)
  Private
    FmeanCoverage : integer;
    Frange : TRange;
  Protected
    //Property setters
    Procedure SetmeanCoverage(AIndex : Integer; AValue : integer); virtual;
    Procedure Setrange(AIndex : Integer; AValue : TRange); virtual;
  Public
  Published
    Property meanCoverage : integer Index 0 Read FmeanCoverage Write SetmeanCoverage;
    Property range : TRange Index 8 Read Frange Write Setrange;
  end;
  TCoverageBucketClass = Class of TCoverageBucket;
  
  { --------------------------------------------------------------------
    TDataset
    --------------------------------------------------------------------}
  
  TDataset = Class(TGoogleBaseObject)
  Private
    Fid : String;
    FisPublic : boolean;
    Fname : String;
    FprojectNumber : String;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetisPublic(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetprojectNumber(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property isPublic : boolean Index 8 Read FisPublic Write SetisPublic;
    Property name : String Index 16 Read Fname Write Setname;
    Property projectNumber : String Index 24 Read FprojectNumber Write SetprojectNumber;
  end;
  TDatasetClass = Class of TDataset;
  
  { --------------------------------------------------------------------
    TExperimentalCreateJobRequest
    --------------------------------------------------------------------}
  
  TExperimentalCreateJobRequest = Class(TGoogleBaseObject)
  Private
    Falign : boolean;
    FcallVariants : boolean;
    FgcsOutputPath : String;
    FpairedSourceUris : TStringArray;
    FprojectNumber : String;
    FsourceUris : TStringArray;
  Protected
    //Property setters
    Procedure Setalign(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetcallVariants(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetgcsOutputPath(AIndex : Integer; AValue : String); virtual;
    Procedure SetpairedSourceUris(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetprojectNumber(AIndex : Integer; AValue : String); virtual;
    Procedure SetsourceUris(AIndex : Integer; AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property align : boolean Index 0 Read Falign Write Setalign;
    Property callVariants : boolean Index 8 Read FcallVariants Write SetcallVariants;
    Property gcsOutputPath : String Index 16 Read FgcsOutputPath Write SetgcsOutputPath;
    Property pairedSourceUris : TStringArray Index 24 Read FpairedSourceUris Write SetpairedSourceUris;
    Property projectNumber : String Index 32 Read FprojectNumber Write SetprojectNumber;
    Property sourceUris : TStringArray Index 40 Read FsourceUris Write SetsourceUris;
  end;
  TExperimentalCreateJobRequestClass = Class of TExperimentalCreateJobRequest;
  
  { --------------------------------------------------------------------
    TExperimentalCreateJobResponse
    --------------------------------------------------------------------}
  
  TExperimentalCreateJobResponse = Class(TGoogleBaseObject)
  Private
    FjobId : String;
  Protected
    //Property setters
    Procedure SetjobId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property jobId : String Index 0 Read FjobId Write SetjobId;
  end;
  TExperimentalCreateJobResponseClass = Class of TExperimentalCreateJobResponse;
  
  { --------------------------------------------------------------------
    TExportReadGroupSetsRequest
    --------------------------------------------------------------------}
  
  TExportReadGroupSetsRequest = Class(TGoogleBaseObject)
  Private
    FexportUri : String;
    FprojectNumber : String;
    FreadGroupSetIds : TStringArray;
    FreferenceNames : TStringArray;
  Protected
    //Property setters
    Procedure SetexportUri(AIndex : Integer; AValue : String); virtual;
    Procedure SetprojectNumber(AIndex : Integer; AValue : String); virtual;
    Procedure SetreadGroupSetIds(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetreferenceNames(AIndex : Integer; AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property exportUri : String Index 0 Read FexportUri Write SetexportUri;
    Property projectNumber : String Index 8 Read FprojectNumber Write SetprojectNumber;
    Property readGroupSetIds : TStringArray Index 16 Read FreadGroupSetIds Write SetreadGroupSetIds;
    Property referenceNames : TStringArray Index 24 Read FreferenceNames Write SetreferenceNames;
  end;
  TExportReadGroupSetsRequestClass = Class of TExportReadGroupSetsRequest;
  
  { --------------------------------------------------------------------
    TExportReadGroupSetsResponse
    --------------------------------------------------------------------}
  
  TExportReadGroupSetsResponse = Class(TGoogleBaseObject)
  Private
    FjobId : String;
  Protected
    //Property setters
    Procedure SetjobId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property jobId : String Index 0 Read FjobId Write SetjobId;
  end;
  TExportReadGroupSetsResponseClass = Class of TExportReadGroupSetsResponse;
  
  { --------------------------------------------------------------------
    TExportVariantSetRequest
    --------------------------------------------------------------------}
  
  TExportVariantSetRequest = Class(TGoogleBaseObject)
  Private
    FbigqueryDataset : String;
    FbigqueryTable : String;
    FcallSetIds : TStringArray;
    Fformat : String;
    FprojectNumber : String;
  Protected
    //Property setters
    Procedure SetbigqueryDataset(AIndex : Integer; AValue : String); virtual;
    Procedure SetbigqueryTable(AIndex : Integer; AValue : String); virtual;
    Procedure SetcallSetIds(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setformat(AIndex : Integer; AValue : String); virtual;
    Procedure SetprojectNumber(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property bigqueryDataset : String Index 0 Read FbigqueryDataset Write SetbigqueryDataset;
    Property bigqueryTable : String Index 8 Read FbigqueryTable Write SetbigqueryTable;
    Property callSetIds : TStringArray Index 16 Read FcallSetIds Write SetcallSetIds;
    Property format : String Index 24 Read Fformat Write Setformat;
    Property projectNumber : String Index 32 Read FprojectNumber Write SetprojectNumber;
  end;
  TExportVariantSetRequestClass = Class of TExportVariantSetRequest;
  
  { --------------------------------------------------------------------
    TExportVariantSetResponse
    --------------------------------------------------------------------}
  
  TExportVariantSetResponse = Class(TGoogleBaseObject)
  Private
    FjobId : String;
  Protected
    //Property setters
    Procedure SetjobId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property jobId : String Index 0 Read FjobId Write SetjobId;
  end;
  TExportVariantSetResponseClass = Class of TExportVariantSetResponse;
  
  { --------------------------------------------------------------------
    TExternalId
    --------------------------------------------------------------------}
  
  TExternalId = Class(TGoogleBaseObject)
  Private
    Fid : String;
    FsourceName : String;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetsourceName(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property id : String Index 0 Read Fid Write Setid;
    Property sourceName : String Index 8 Read FsourceName Write SetsourceName;
  end;
  TExternalIdClass = Class of TExternalId;
  
  { --------------------------------------------------------------------
    TFastqMetadata
    --------------------------------------------------------------------}
  
  TFastqMetadata = Class(TGoogleBaseObject)
  Private
    FlibraryName : String;
    FplatformName : String;
    FplatformUnit : String;
    FreadGroupName : String;
    FsampleName : String;
  Protected
    //Property setters
    Procedure SetlibraryName(AIndex : Integer; AValue : String); virtual;
    Procedure SetplatformName(AIndex : Integer; AValue : String); virtual;
    Procedure SetplatformUnit(AIndex : Integer; AValue : String); virtual;
    Procedure SetreadGroupName(AIndex : Integer; AValue : String); virtual;
    Procedure SetsampleName(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property libraryName : String Index 0 Read FlibraryName Write SetlibraryName;
    Property platformName : String Index 8 Read FplatformName Write SetplatformName;
    Property platformUnit : String Index 16 Read FplatformUnit Write SetplatformUnit;
    Property readGroupName : String Index 24 Read FreadGroupName Write SetreadGroupName;
    Property sampleName : String Index 32 Read FsampleName Write SetsampleName;
  end;
  TFastqMetadataClass = Class of TFastqMetadata;
  
  { --------------------------------------------------------------------
    TImportReadGroupSetsRequest
    --------------------------------------------------------------------}
  
  TImportReadGroupSetsRequest = Class(TGoogleBaseObject)
  Private
    FdatasetId : String;
    FpartitionStrategy : String;
    FreferenceSetId : String;
    FsourceUris : TStringArray;
  Protected
    //Property setters
    Procedure SetdatasetId(AIndex : Integer; AValue : String); virtual;
    Procedure SetpartitionStrategy(AIndex : Integer; AValue : String); virtual;
    Procedure SetreferenceSetId(AIndex : Integer; AValue : String); virtual;
    Procedure SetsourceUris(AIndex : Integer; AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property datasetId : String Index 0 Read FdatasetId Write SetdatasetId;
    Property partitionStrategy : String Index 8 Read FpartitionStrategy Write SetpartitionStrategy;
    Property referenceSetId : String Index 16 Read FreferenceSetId Write SetreferenceSetId;
    Property sourceUris : TStringArray Index 24 Read FsourceUris Write SetsourceUris;
  end;
  TImportReadGroupSetsRequestClass = Class of TImportReadGroupSetsRequest;
  
  { --------------------------------------------------------------------
    TImportReadGroupSetsResponse
    --------------------------------------------------------------------}
  
  TImportReadGroupSetsResponse = Class(TGoogleBaseObject)
  Private
    FjobId : String;
  Protected
    //Property setters
    Procedure SetjobId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property jobId : String Index 0 Read FjobId Write SetjobId;
  end;
  TImportReadGroupSetsResponseClass = Class of TImportReadGroupSetsResponse;
  
  { --------------------------------------------------------------------
    TImportVariantsRequest
    --------------------------------------------------------------------}
  
  TImportVariantsRequest = Class(TGoogleBaseObject)
  Private
    Fformat : String;
    FsourceUris : TStringArray;
  Protected
    //Property setters
    Procedure Setformat(AIndex : Integer; AValue : String); virtual;
    Procedure SetsourceUris(AIndex : Integer; AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property format : String Index 0 Read Fformat Write Setformat;
    Property sourceUris : TStringArray Index 8 Read FsourceUris Write SetsourceUris;
  end;
  TImportVariantsRequestClass = Class of TImportVariantsRequest;
  
  { --------------------------------------------------------------------
    TImportVariantsResponse
    --------------------------------------------------------------------}
  
  TImportVariantsResponse = Class(TGoogleBaseObject)
  Private
    FjobId : String;
  Protected
    //Property setters
    Procedure SetjobId(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property jobId : String Index 0 Read FjobId Write SetjobId;
  end;
  TImportVariantsResponseClass = Class of TImportVariantsResponse;
  
  { --------------------------------------------------------------------
    TInt32Value
    --------------------------------------------------------------------}
  
  TInt32Value = Class(TGoogleBaseObject)
  Private
    Fvalue : integer;
  Protected
    //Property setters
    Procedure Setvalue(AIndex : Integer; AValue : integer); virtual;
  Public
  Published
    Property value : integer Index 0 Read Fvalue Write Setvalue;
  end;
  TInt32ValueClass = Class of TInt32Value;
  
  { --------------------------------------------------------------------
    TInterleavedFastqSource
    --------------------------------------------------------------------}
  
  TInterleavedFastqSource = Class(TGoogleBaseObject)
  Private
    Fmetadata : TFastqMetadata;
    FsourceUris : TStringArray;
  Protected
    //Property setters
    Procedure Setmetadata(AIndex : Integer; AValue : TFastqMetadata); virtual;
    Procedure SetsourceUris(AIndex : Integer; AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property metadata : TFastqMetadata Index 0 Read Fmetadata Write Setmetadata;
    Property sourceUris : TStringArray Index 8 Read FsourceUris Write SetsourceUris;
  end;
  TInterleavedFastqSourceClass = Class of TInterleavedFastqSource;
  
  { --------------------------------------------------------------------
    TJob
    --------------------------------------------------------------------}
  
  TJob = Class(TGoogleBaseObject)
  Private
    Fcreated : String;
    FdetailedStatus : String;
    Ferrors : TStringArray;
    Fid : String;
    FimportedIds : TStringArray;
    FprojectNumber : String;
    Frequest : TJobRequest;
    Fstatus : String;
    Fwarnings : TStringArray;
  Protected
    //Property setters
    Procedure Setcreated(AIndex : Integer; AValue : String); virtual;
    Procedure SetdetailedStatus(AIndex : Integer; AValue : String); virtual;
    Procedure Seterrors(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure SetimportedIds(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetprojectNumber(AIndex : Integer; AValue : String); virtual;
    Procedure Setrequest(AIndex : Integer; AValue : TJobRequest); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : String); virtual;
    Procedure Setwarnings(AIndex : Integer; AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property created : String Index 0 Read Fcreated Write Setcreated;
    Property detailedStatus : String Index 8 Read FdetailedStatus Write SetdetailedStatus;
    Property errors : TStringArray Index 16 Read Ferrors Write Seterrors;
    Property id : String Index 24 Read Fid Write Setid;
    Property importedIds : TStringArray Index 32 Read FimportedIds Write SetimportedIds;
    Property projectNumber : String Index 40 Read FprojectNumber Write SetprojectNumber;
    Property request : TJobRequest Index 48 Read Frequest Write Setrequest;
    Property status : String Index 56 Read Fstatus Write Setstatus;
    Property warnings : TStringArray Index 64 Read Fwarnings Write Setwarnings;
  end;
  TJobClass = Class of TJob;
  
  { --------------------------------------------------------------------
    TJobRequest
    --------------------------------------------------------------------}
  
  TJobRequest = Class(TGoogleBaseObject)
  Private
    Fdestination : TStringArray;
    Fsource : TStringArray;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setdestination(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setsource(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property destination : TStringArray Index 0 Read Fdestination Write Setdestination;
    Property source : TStringArray Index 8 Read Fsource Write Setsource;
    Property _type : String Index 16 Read F_type Write Set_type;
  end;
  TJobRequestClass = Class of TJobRequest;
  
  { --------------------------------------------------------------------
    TLinearAlignment
    --------------------------------------------------------------------}
  
  TLinearAlignment = Class(TGoogleBaseObject)
  Private
    Fcigar : TLinearAlignmentTypecigarArray;
    FmappingQuality : integer;
    Fposition : TPosition;
  Protected
    //Property setters
    Procedure Setcigar(AIndex : Integer; AValue : TLinearAlignmentTypecigarArray); virtual;
    Procedure SetmappingQuality(AIndex : Integer; AValue : integer); virtual;
    Procedure Setposition(AIndex : Integer; AValue : TPosition); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property cigar : TLinearAlignmentTypecigarArray Index 0 Read Fcigar Write Setcigar;
    Property mappingQuality : integer Index 8 Read FmappingQuality Write SetmappingQuality;
    Property position : TPosition Index 16 Read Fposition Write Setposition;
  end;
  TLinearAlignmentClass = Class of TLinearAlignment;
  
  { --------------------------------------------------------------------
    TListBasesResponse
    --------------------------------------------------------------------}
  
  TListBasesResponse = Class(TGoogleBaseObject)
  Private
    FnextPageToken : String;
    Foffset : String;
    Fsequence : String;
  Protected
    //Property setters
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure Setoffset(AIndex : Integer; AValue : String); virtual;
    Procedure Setsequence(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property nextPageToken : String Index 0 Read FnextPageToken Write SetnextPageToken;
    Property offset : String Index 8 Read Foffset Write Setoffset;
    Property sequence : String Index 16 Read Fsequence Write Setsequence;
  end;
  TListBasesResponseClass = Class of TListBasesResponse;
  
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
    Procedure SetbucketWidth(AIndex : Integer; AValue : String); virtual;
    Procedure SetcoverageBuckets(AIndex : Integer; AValue : TListCoverageBucketsResponseTypecoverageBucketsArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
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
    TListDatasetsResponse
    --------------------------------------------------------------------}
  
  TListDatasetsResponse = Class(TGoogleBaseObject)
  Private
    Fdatasets : TListDatasetsResponseTypedatasetsArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setdatasets(AIndex : Integer; AValue : TListDatasetsResponseTypedatasetsArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
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
    TMergeVariantsRequest
    --------------------------------------------------------------------}
  
  TMergeVariantsRequest = Class(TGoogleBaseObject)
  Private
    Fvariants : TMergeVariantsRequestTypevariantsArray;
  Protected
    //Property setters
    Procedure Setvariants(AIndex : Integer; AValue : TMergeVariantsRequestTypevariantsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property variants : TMergeVariantsRequestTypevariantsArray Index 0 Read Fvariants Write Setvariants;
  end;
  TMergeVariantsRequestClass = Class of TMergeVariantsRequest;
  
  { --------------------------------------------------------------------
    TMetadataTypeinfo
    --------------------------------------------------------------------}
  
  TMetadataTypeinfo = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TMetadataTypeinfoClass = Class of TMetadataTypeinfo;
  
  { --------------------------------------------------------------------
    TMetadata
    --------------------------------------------------------------------}
  
  TMetadata = Class(TGoogleBaseObject)
  Private
    Fdescription : String;
    Fid : String;
    Finfo : TMetadataTypeinfo;
    Fkey : String;
    Fnumber : String;
    F_type : String;
    Fvalue : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setinfo(AIndex : Integer; AValue : TMetadataTypeinfo); virtual;
    Procedure Setkey(AIndex : Integer; AValue : String); virtual;
    Procedure Setnumber(AIndex : Integer; AValue : String); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property description : String Index 0 Read Fdescription Write Setdescription;
    Property id : String Index 8 Read Fid Write Setid;
    Property info : TMetadataTypeinfo Index 16 Read Finfo Write Setinfo;
    Property key : String Index 24 Read Fkey Write Setkey;
    Property number : String Index 32 Read Fnumber Write Setnumber;
    Property _type : String Index 40 Read F_type Write Set_type;
    Property value : String Index 48 Read Fvalue Write Setvalue;
  end;
  TMetadataClass = Class of TMetadata;
  
  { --------------------------------------------------------------------
    TPairedFastqSource
    --------------------------------------------------------------------}
  
  TPairedFastqSource = Class(TGoogleBaseObject)
  Private
    FfirstSourceUris : TStringArray;
    Fmetadata : TFastqMetadata;
    FsecondSourceUris : TStringArray;
  Protected
    //Property setters
    Procedure SetfirstSourceUris(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setmetadata(AIndex : Integer; AValue : TFastqMetadata); virtual;
    Procedure SetsecondSourceUris(AIndex : Integer; AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property firstSourceUris : TStringArray Index 0 Read FfirstSourceUris Write SetfirstSourceUris;
    Property metadata : TFastqMetadata Index 8 Read Fmetadata Write Setmetadata;
    Property secondSourceUris : TStringArray Index 16 Read FsecondSourceUris Write SetsecondSourceUris;
  end;
  TPairedFastqSourceClass = Class of TPairedFastqSource;
  
  { --------------------------------------------------------------------
    TPosition
    --------------------------------------------------------------------}
  
  TPosition = Class(TGoogleBaseObject)
  Private
    Fposition : String;
    FreferenceName : String;
    FreverseStrand : boolean;
  Protected
    //Property setters
    Procedure Setposition(AIndex : Integer; AValue : String); virtual;
    Procedure SetreferenceName(AIndex : Integer; AValue : String); virtual;
    Procedure SetreverseStrand(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property position : String Index 0 Read Fposition Write Setposition;
    Property referenceName : String Index 8 Read FreferenceName Write SetreferenceName;
    Property reverseStrand : boolean Index 16 Read FreverseStrand Write SetreverseStrand;
  end;
  TPositionClass = Class of TPosition;
  
  { --------------------------------------------------------------------
    TQueryRange
    --------------------------------------------------------------------}
  
  TQueryRange = Class(TGoogleBaseObject)
  Private
    F_end : String;
    FreferenceId : String;
    FreferenceName : String;
    Fstart : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_end(AIndex : Integer; AValue : String); virtual;
    Procedure SetreferenceId(AIndex : Integer; AValue : String); virtual;
    Procedure SetreferenceName(AIndex : Integer; AValue : String); virtual;
    Procedure Setstart(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property _end : String Index 0 Read F_end Write Set_end;
    Property referenceId : String Index 8 Read FreferenceId Write SetreferenceId;
    Property referenceName : String Index 16 Read FreferenceName Write SetreferenceName;
    Property start : String Index 24 Read Fstart Write Setstart;
  end;
  TQueryRangeClass = Class of TQueryRange;
  
  { --------------------------------------------------------------------
    TRange
    --------------------------------------------------------------------}
  
  TRange = Class(TGoogleBaseObject)
  Private
    F_end : String;
    FreferenceName : String;
    Fstart : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_end(AIndex : Integer; AValue : String); virtual;
    Procedure SetreferenceName(AIndex : Integer; AValue : String); virtual;
    Procedure Setstart(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property _end : String Index 0 Read F_end Write Set_end;
    Property referenceName : String Index 8 Read FreferenceName Write SetreferenceName;
    Property start : String Index 16 Read Fstart Write Setstart;
  end;
  TRangeClass = Class of TRange;
  
  { --------------------------------------------------------------------
    TRangePosition
    --------------------------------------------------------------------}
  
  TRangePosition = Class(TGoogleBaseObject)
  Private
    F_end : String;
    FreferenceId : String;
    FreferenceName : String;
    FreverseStrand : boolean;
    Fstart : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_end(AIndex : Integer; AValue : String); virtual;
    Procedure SetreferenceId(AIndex : Integer; AValue : String); virtual;
    Procedure SetreferenceName(AIndex : Integer; AValue : String); virtual;
    Procedure SetreverseStrand(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setstart(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property _end : String Index 0 Read F_end Write Set_end;
    Property referenceId : String Index 8 Read FreferenceId Write SetreferenceId;
    Property referenceName : String Index 16 Read FreferenceName Write SetreferenceName;
    Property reverseStrand : boolean Index 24 Read FreverseStrand Write SetreverseStrand;
    Property start : String Index 32 Read Fstart Write Setstart;
  end;
  TRangePositionClass = Class of TRangePosition;
  
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
    FalignedQuality : TintegerArray;
    FalignedSequence : String;
    Falignment : TLinearAlignment;
    FduplicateFragment : boolean;
    FfailedVendorQualityChecks : boolean;
    FfragmentLength : integer;
    FfragmentName : String;
    Fid : String;
    Finfo : TReadTypeinfo;
    FnextMatePosition : TPosition;
    FnumberReads : integer;
    FproperPlacement : boolean;
    FreadGroupId : String;
    FreadGroupSetId : String;
    FreadNumber : integer;
    FsecondaryAlignment : boolean;
    FsupplementaryAlignment : boolean;
  Protected
    //Property setters
    Procedure SetalignedQuality(AIndex : Integer; AValue : TintegerArray); virtual;
    Procedure SetalignedSequence(AIndex : Integer; AValue : String); virtual;
    Procedure Setalignment(AIndex : Integer; AValue : TLinearAlignment); virtual;
    Procedure SetduplicateFragment(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetfailedVendorQualityChecks(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetfragmentLength(AIndex : Integer; AValue : integer); virtual;
    Procedure SetfragmentName(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setinfo(AIndex : Integer; AValue : TReadTypeinfo); virtual;
    Procedure SetnextMatePosition(AIndex : Integer; AValue : TPosition); virtual;
    Procedure SetnumberReads(AIndex : Integer; AValue : integer); virtual;
    Procedure SetproperPlacement(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetreadGroupId(AIndex : Integer; AValue : String); virtual;
    Procedure SetreadGroupSetId(AIndex : Integer; AValue : String); virtual;
    Procedure SetreadNumber(AIndex : Integer; AValue : integer); virtual;
    Procedure SetsecondaryAlignment(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetsupplementaryAlignment(AIndex : Integer; AValue : boolean); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property alignedQuality : TintegerArray Index 0 Read FalignedQuality Write SetalignedQuality;
    Property alignedSequence : String Index 8 Read FalignedSequence Write SetalignedSequence;
    Property alignment : TLinearAlignment Index 16 Read Falignment Write Setalignment;
    Property duplicateFragment : boolean Index 24 Read FduplicateFragment Write SetduplicateFragment;
    Property failedVendorQualityChecks : boolean Index 32 Read FfailedVendorQualityChecks Write SetfailedVendorQualityChecks;
    Property fragmentLength : integer Index 40 Read FfragmentLength Write SetfragmentLength;
    Property fragmentName : String Index 48 Read FfragmentName Write SetfragmentName;
    Property id : String Index 56 Read Fid Write Setid;
    Property info : TReadTypeinfo Index 64 Read Finfo Write Setinfo;
    Property nextMatePosition : TPosition Index 72 Read FnextMatePosition Write SetnextMatePosition;
    Property numberReads : integer Index 80 Read FnumberReads Write SetnumberReads;
    Property properPlacement : boolean Index 88 Read FproperPlacement Write SetproperPlacement;
    Property readGroupId : String Index 96 Read FreadGroupId Write SetreadGroupId;
    Property readGroupSetId : String Index 104 Read FreadGroupSetId Write SetreadGroupSetId;
    Property readNumber : integer Index 112 Read FreadNumber Write SetreadNumber;
    Property secondaryAlignment : boolean Index 120 Read FsecondaryAlignment Write SetsecondaryAlignment;
    Property supplementaryAlignment : boolean Index 128 Read FsupplementaryAlignment Write SetsupplementaryAlignment;
  end;
  TReadClass = Class of TRead;
  
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
    FdatasetId : String;
    Fdescription : String;
    Fexperiment : TReadGroupExperiment;
    Fid : String;
    Finfo : TReadGroupTypeinfo;
    Fname : String;
    FpredictedInsertSize : integer;
    Fprograms : TReadGroupTypeprogramsArray;
    FreferenceSetId : String;
    FsampleId : String;
  Protected
    //Property setters
    Procedure SetdatasetId(AIndex : Integer; AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure Setexperiment(AIndex : Integer; AValue : TReadGroupExperiment); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setinfo(AIndex : Integer; AValue : TReadGroupTypeinfo); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetpredictedInsertSize(AIndex : Integer; AValue : integer); virtual;
    Procedure Setprograms(AIndex : Integer; AValue : TReadGroupTypeprogramsArray); virtual;
    Procedure SetreferenceSetId(AIndex : Integer; AValue : String); virtual;
    Procedure SetsampleId(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property datasetId : String Index 0 Read FdatasetId Write SetdatasetId;
    Property description : String Index 8 Read Fdescription Write Setdescription;
    Property experiment : TReadGroupExperiment Index 16 Read Fexperiment Write Setexperiment;
    Property id : String Index 24 Read Fid Write Setid;
    Property info : TReadGroupTypeinfo Index 32 Read Finfo Write Setinfo;
    Property name : String Index 40 Read Fname Write Setname;
    Property predictedInsertSize : integer Index 48 Read FpredictedInsertSize Write SetpredictedInsertSize;
    Property programs : TReadGroupTypeprogramsArray Index 56 Read Fprograms Write Setprograms;
    Property referenceSetId : String Index 64 Read FreferenceSetId Write SetreferenceSetId;
    Property sampleId : String Index 72 Read FsampleId Write SetsampleId;
  end;
  TReadGroupClass = Class of TReadGroup;
  
  { --------------------------------------------------------------------
    TReadGroupExperiment
    --------------------------------------------------------------------}
  
  TReadGroupExperiment = Class(TGoogleBaseObject)
  Private
    FinstrumentModel : String;
    FlibraryId : String;
    FplatformUnit : String;
    FsequencingCenter : String;
  Protected
    //Property setters
    Procedure SetinstrumentModel(AIndex : Integer; AValue : String); virtual;
    Procedure SetlibraryId(AIndex : Integer; AValue : String); virtual;
    Procedure SetplatformUnit(AIndex : Integer; AValue : String); virtual;
    Procedure SetsequencingCenter(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property instrumentModel : String Index 0 Read FinstrumentModel Write SetinstrumentModel;
    Property libraryId : String Index 8 Read FlibraryId Write SetlibraryId;
    Property platformUnit : String Index 16 Read FplatformUnit Write SetplatformUnit;
    Property sequencingCenter : String Index 24 Read FsequencingCenter Write SetsequencingCenter;
  end;
  TReadGroupExperimentClass = Class of TReadGroupExperiment;
  
  { --------------------------------------------------------------------
    TReadGroupProgram
    --------------------------------------------------------------------}
  
  TReadGroupProgram = Class(TGoogleBaseObject)
  Private
    FcommandLine : String;
    Fid : String;
    Fname : String;
    FprevProgramId : String;
    Fversion : String;
  Protected
    //Property setters
    Procedure SetcommandLine(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetprevProgramId(AIndex : Integer; AValue : String); virtual;
    Procedure Setversion(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property commandLine : String Index 0 Read FcommandLine Write SetcommandLine;
    Property id : String Index 8 Read Fid Write Setid;
    Property name : String Index 16 Read Fname Write Setname;
    Property prevProgramId : String Index 24 Read FprevProgramId Write SetprevProgramId;
    Property version : String Index 32 Read Fversion Write Setversion;
  end;
  TReadGroupProgramClass = Class of TReadGroupProgram;
  
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
    FdatasetId : String;
    Ffilename : String;
    Fid : String;
    Finfo : TReadGroupSetTypeinfo;
    Fname : String;
    FreadGroups : TReadGroupSetTypereadGroupsArray;
    FreferenceSetId : String;
  Protected
    //Property setters
    Procedure SetdatasetId(AIndex : Integer; AValue : String); virtual;
    Procedure Setfilename(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setinfo(AIndex : Integer; AValue : TReadGroupSetTypeinfo); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetreadGroups(AIndex : Integer; AValue : TReadGroupSetTypereadGroupsArray); virtual;
    Procedure SetreferenceSetId(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property datasetId : String Index 0 Read FdatasetId Write SetdatasetId;
    Property filename : String Index 8 Read Ffilename Write Setfilename;
    Property id : String Index 16 Read Fid Write Setid;
    Property info : TReadGroupSetTypeinfo Index 24 Read Finfo Write Setinfo;
    Property name : String Index 32 Read Fname Write Setname;
    Property readGroups : TReadGroupSetTypereadGroupsArray Index 40 Read FreadGroups Write SetreadGroups;
    Property referenceSetId : String Index 48 Read FreferenceSetId Write SetreferenceSetId;
  end;
  TReadGroupSetClass = Class of TReadGroupSet;
  
  { --------------------------------------------------------------------
    TReference
    --------------------------------------------------------------------}
  
  TReference = Class(TGoogleBaseObject)
  Private
    Fid : String;
    F_length : String;
    Fmd5checksum : String;
    Fname : String;
    FncbiTaxonId : integer;
    FsourceAccessions : TStringArray;
    FsourceURI : String;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Set_length(AIndex : Integer; AValue : String); virtual;
    Procedure Setmd5checksum(AIndex : Integer; AValue : String); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetncbiTaxonId(AIndex : Integer; AValue : integer); virtual;
    Procedure SetsourceAccessions(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetsourceURI(AIndex : Integer; AValue : String); virtual;
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
    Property ncbiTaxonId : integer Index 32 Read FncbiTaxonId Write SetncbiTaxonId;
    Property sourceAccessions : TStringArray Index 40 Read FsourceAccessions Write SetsourceAccessions;
    Property sourceURI : String Index 48 Read FsourceURI Write SetsourceURI;
  end;
  TReferenceClass = Class of TReference;
  
  { --------------------------------------------------------------------
    TReferenceBound
    --------------------------------------------------------------------}
  
  TReferenceBound = Class(TGoogleBaseObject)
  Private
    FreferenceName : String;
    FupperBound : String;
  Protected
    //Property setters
    Procedure SetreferenceName(AIndex : Integer; AValue : String); virtual;
    Procedure SetupperBound(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property referenceName : String Index 0 Read FreferenceName Write SetreferenceName;
    Property upperBound : String Index 8 Read FupperBound Write SetupperBound;
  end;
  TReferenceBoundClass = Class of TReferenceBound;
  
  { --------------------------------------------------------------------
    TReferenceSet
    --------------------------------------------------------------------}
  
  TReferenceSet = Class(TGoogleBaseObject)
  Private
    FassemblyId : String;
    Fdescription : String;
    Fid : String;
    Fmd5checksum : String;
    FncbiTaxonId : integer;
    FreferenceIds : TStringArray;
    FsourceAccessions : TStringArray;
    FsourceURI : String;
  Protected
    //Property setters
    Procedure SetassemblyId(AIndex : Integer; AValue : String); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setmd5checksum(AIndex : Integer; AValue : String); virtual;
    Procedure SetncbiTaxonId(AIndex : Integer; AValue : integer); virtual;
    Procedure SetreferenceIds(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetsourceAccessions(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetsourceURI(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property assemblyId : String Index 0 Read FassemblyId Write SetassemblyId;
    Property description : String Index 8 Read Fdescription Write Setdescription;
    Property id : String Index 16 Read Fid Write Setid;
    Property md5checksum : String Index 24 Read Fmd5checksum Write Setmd5checksum;
    Property ncbiTaxonId : integer Index 32 Read FncbiTaxonId Write SetncbiTaxonId;
    Property referenceIds : TStringArray Index 40 Read FreferenceIds Write SetreferenceIds;
    Property sourceAccessions : TStringArray Index 48 Read FsourceAccessions Write SetsourceAccessions;
    Property sourceURI : String Index 56 Read FsourceURI Write SetsourceURI;
  end;
  TReferenceSetClass = Class of TReferenceSet;
  
  { --------------------------------------------------------------------
    TSearchAnnotationSetsRequest
    --------------------------------------------------------------------}
  
  TSearchAnnotationSetsRequest = Class(TGoogleBaseObject)
  Private
    FdatasetIds : TStringArray;
    Fname : String;
    FpageSize : integer;
    FpageToken : String;
    FreferenceSetId : String;
    Ftypes : TStringArray;
  Protected
    //Property setters
    Procedure SetdatasetIds(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetpageSize(AIndex : Integer; AValue : integer); virtual;
    Procedure SetpageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SetreferenceSetId(AIndex : Integer; AValue : String); virtual;
    Procedure Settypes(AIndex : Integer; AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property datasetIds : TStringArray Index 0 Read FdatasetIds Write SetdatasetIds;
    Property name : String Index 8 Read Fname Write Setname;
    Property pageSize : integer Index 16 Read FpageSize Write SetpageSize;
    Property pageToken : String Index 24 Read FpageToken Write SetpageToken;
    Property referenceSetId : String Index 32 Read FreferenceSetId Write SetreferenceSetId;
    Property types : TStringArray Index 40 Read Ftypes Write Settypes;
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
    Procedure SetannotationSets(AIndex : Integer; AValue : TSearchAnnotationSetsResponseTypeannotationSetsArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
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
    TSearchAnnotationsRequest
    --------------------------------------------------------------------}
  
  TSearchAnnotationsRequest = Class(TGoogleBaseObject)
  Private
    FannotationSetIds : TStringArray;
    FpageSize : integer;
    FpageToken : String;
    Frange : TQueryRange;
  Protected
    //Property setters
    Procedure SetannotationSetIds(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetpageSize(AIndex : Integer; AValue : integer); virtual;
    Procedure SetpageToken(AIndex : Integer; AValue : String); virtual;
    Procedure Setrange(AIndex : Integer; AValue : TQueryRange); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property annotationSetIds : TStringArray Index 0 Read FannotationSetIds Write SetannotationSetIds;
    Property pageSize : integer Index 8 Read FpageSize Write SetpageSize;
    Property pageToken : String Index 16 Read FpageToken Write SetpageToken;
    Property range : TQueryRange Index 24 Read Frange Write Setrange;
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
    Procedure Setannotations(AIndex : Integer; AValue : TSearchAnnotationsResponseTypeannotationsArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
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
    TSearchCallSetsRequest
    --------------------------------------------------------------------}
  
  TSearchCallSetsRequest = Class(TGoogleBaseObject)
  Private
    Fname : String;
    FpageSize : integer;
    FpageToken : String;
    FvariantSetIds : TStringArray;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetpageSize(AIndex : Integer; AValue : integer); virtual;
    Procedure SetpageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SetvariantSetIds(AIndex : Integer; AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property name : String Index 0 Read Fname Write Setname;
    Property pageSize : integer Index 8 Read FpageSize Write SetpageSize;
    Property pageToken : String Index 16 Read FpageToken Write SetpageToken;
    Property variantSetIds : TStringArray Index 24 Read FvariantSetIds Write SetvariantSetIds;
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
    Procedure SetcallSets(AIndex : Integer; AValue : TSearchCallSetsResponseTypecallSetsArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
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
    TSearchJobsRequest
    --------------------------------------------------------------------}
  
  TSearchJobsRequest = Class(TGoogleBaseObject)
  Private
    FcreatedAfter : String;
    FcreatedBefore : String;
    FpageSize : integer;
    FpageToken : String;
    FprojectNumber : String;
    Fstatus : TStringArray;
  Protected
    //Property setters
    Procedure SetcreatedAfter(AIndex : Integer; AValue : String); virtual;
    Procedure SetcreatedBefore(AIndex : Integer; AValue : String); virtual;
    Procedure SetpageSize(AIndex : Integer; AValue : integer); virtual;
    Procedure SetpageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SetprojectNumber(AIndex : Integer; AValue : String); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property createdAfter : String Index 0 Read FcreatedAfter Write SetcreatedAfter;
    Property createdBefore : String Index 8 Read FcreatedBefore Write SetcreatedBefore;
    Property pageSize : integer Index 16 Read FpageSize Write SetpageSize;
    Property pageToken : String Index 24 Read FpageToken Write SetpageToken;
    Property projectNumber : String Index 32 Read FprojectNumber Write SetprojectNumber;
    Property status : TStringArray Index 40 Read Fstatus Write Setstatus;
  end;
  TSearchJobsRequestClass = Class of TSearchJobsRequest;
  
  { --------------------------------------------------------------------
    TSearchJobsResponse
    --------------------------------------------------------------------}
  
  TSearchJobsResponse = Class(TGoogleBaseObject)
  Private
    Fjobs : TSearchJobsResponseTypejobsArray;
    FnextPageToken : String;
  Protected
    //Property setters
    Procedure Setjobs(AIndex : Integer; AValue : TSearchJobsResponseTypejobsArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property jobs : TSearchJobsResponseTypejobsArray Index 0 Read Fjobs Write Setjobs;
    Property nextPageToken : String Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TSearchJobsResponseClass = Class of TSearchJobsResponse;
  
  { --------------------------------------------------------------------
    TSearchReadGroupSetsRequest
    --------------------------------------------------------------------}
  
  TSearchReadGroupSetsRequest = Class(TGoogleBaseObject)
  Private
    FdatasetIds : TStringArray;
    Fname : String;
    FpageSize : integer;
    FpageToken : String;
  Protected
    //Property setters
    Procedure SetdatasetIds(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setname(AIndex : Integer; AValue : String); virtual;
    Procedure SetpageSize(AIndex : Integer; AValue : integer); virtual;
    Procedure SetpageToken(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property datasetIds : TStringArray Index 0 Read FdatasetIds Write SetdatasetIds;
    Property name : String Index 8 Read Fname Write Setname;
    Property pageSize : integer Index 16 Read FpageSize Write SetpageSize;
    Property pageToken : String Index 24 Read FpageToken Write SetpageToken;
  end;
  TSearchReadGroupSetsRequestClass = Class of TSearchReadGroupSetsRequest;
  
  { --------------------------------------------------------------------
    TSearchReadGroupSetsResponse
    --------------------------------------------------------------------}
  
  TSearchReadGroupSetsResponse = Class(TGoogleBaseObject)
  Private
    FnextPageToken : String;
    FreadGroupSets : TSearchReadGroupSetsResponseTypereadGroupSetsArray;
  Protected
    //Property setters
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SetreadGroupSets(AIndex : Integer; AValue : TSearchReadGroupSetsResponseTypereadGroupSetsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property nextPageToken : String Index 0 Read FnextPageToken Write SetnextPageToken;
    Property readGroupSets : TSearchReadGroupSetsResponseTypereadGroupSetsArray Index 8 Read FreadGroupSets Write SetreadGroupSets;
  end;
  TSearchReadGroupSetsResponseClass = Class of TSearchReadGroupSetsResponse;
  
  { --------------------------------------------------------------------
    TSearchReadsRequest
    --------------------------------------------------------------------}
  
  TSearchReadsRequest = Class(TGoogleBaseObject)
  Private
    F_end : String;
    FpageSize : integer;
    FpageToken : String;
    FreadGroupIds : TStringArray;
    FreadGroupSetIds : TStringArray;
    FreferenceName : String;
    Fstart : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_end(AIndex : Integer; AValue : String); virtual;
    Procedure SetpageSize(AIndex : Integer; AValue : integer); virtual;
    Procedure SetpageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SetreadGroupIds(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetreadGroupSetIds(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetreferenceName(AIndex : Integer; AValue : String); virtual;
    Procedure Setstart(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property _end : String Index 0 Read F_end Write Set_end;
    Property pageSize : integer Index 8 Read FpageSize Write SetpageSize;
    Property pageToken : String Index 16 Read FpageToken Write SetpageToken;
    Property readGroupIds : TStringArray Index 24 Read FreadGroupIds Write SetreadGroupIds;
    Property readGroupSetIds : TStringArray Index 32 Read FreadGroupSetIds Write SetreadGroupSetIds;
    Property referenceName : String Index 40 Read FreferenceName Write SetreferenceName;
    Property start : String Index 48 Read Fstart Write Setstart;
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
    Procedure Setalignments(AIndex : Integer; AValue : TSearchReadsResponseTypealignmentsArray); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
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
    TSearchReferenceSetsRequest
    --------------------------------------------------------------------}
  
  TSearchReferenceSetsRequest = Class(TGoogleBaseObject)
  Private
    Faccessions : TStringArray;
    FassemblyId : String;
    Fmd5checksums : TStringArray;
    FpageSize : integer;
    FpageToken : String;
  Protected
    //Property setters
    Procedure Setaccessions(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetassemblyId(AIndex : Integer; AValue : String); virtual;
    Procedure Setmd5checksums(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetpageSize(AIndex : Integer; AValue : integer); virtual;
    Procedure SetpageToken(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property accessions : TStringArray Index 0 Read Faccessions Write Setaccessions;
    Property assemblyId : String Index 8 Read FassemblyId Write SetassemblyId;
    Property md5checksums : TStringArray Index 16 Read Fmd5checksums Write Setmd5checksums;
    Property pageSize : integer Index 24 Read FpageSize Write SetpageSize;
    Property pageToken : String Index 32 Read FpageToken Write SetpageToken;
  end;
  TSearchReferenceSetsRequestClass = Class of TSearchReferenceSetsRequest;
  
  { --------------------------------------------------------------------
    TSearchReferenceSetsResponse
    --------------------------------------------------------------------}
  
  TSearchReferenceSetsResponse = Class(TGoogleBaseObject)
  Private
    FnextPageToken : String;
    FreferenceSets : TSearchReferenceSetsResponseTypereferenceSetsArray;
  Protected
    //Property setters
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SetreferenceSets(AIndex : Integer; AValue : TSearchReferenceSetsResponseTypereferenceSetsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property nextPageToken : String Index 0 Read FnextPageToken Write SetnextPageToken;
    Property referenceSets : TSearchReferenceSetsResponseTypereferenceSetsArray Index 8 Read FreferenceSets Write SetreferenceSets;
  end;
  TSearchReferenceSetsResponseClass = Class of TSearchReferenceSetsResponse;
  
  { --------------------------------------------------------------------
    TSearchReferencesRequest
    --------------------------------------------------------------------}
  
  TSearchReferencesRequest = Class(TGoogleBaseObject)
  Private
    Faccessions : TStringArray;
    Fmd5checksums : TStringArray;
    FpageSize : integer;
    FpageToken : String;
    FreferenceSetId : String;
  Protected
    //Property setters
    Procedure Setaccessions(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setmd5checksums(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetpageSize(AIndex : Integer; AValue : integer); virtual;
    Procedure SetpageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SetreferenceSetId(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property accessions : TStringArray Index 0 Read Faccessions Write Setaccessions;
    Property md5checksums : TStringArray Index 8 Read Fmd5checksums Write Setmd5checksums;
    Property pageSize : integer Index 16 Read FpageSize Write SetpageSize;
    Property pageToken : String Index 24 Read FpageToken Write SetpageToken;
    Property referenceSetId : String Index 32 Read FreferenceSetId Write SetreferenceSetId;
  end;
  TSearchReferencesRequestClass = Class of TSearchReferencesRequest;
  
  { --------------------------------------------------------------------
    TSearchReferencesResponse
    --------------------------------------------------------------------}
  
  TSearchReferencesResponse = Class(TGoogleBaseObject)
  Private
    FnextPageToken : String;
    Freferences : TSearchReferencesResponseTypereferencesArray;
  Protected
    //Property setters
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure Setreferences(AIndex : Integer; AValue : TSearchReferencesResponseTypereferencesArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property nextPageToken : String Index 0 Read FnextPageToken Write SetnextPageToken;
    Property references : TSearchReferencesResponseTypereferencesArray Index 8 Read Freferences Write Setreferences;
  end;
  TSearchReferencesResponseClass = Class of TSearchReferencesResponse;
  
  { --------------------------------------------------------------------
    TSearchVariantSetsRequest
    --------------------------------------------------------------------}
  
  TSearchVariantSetsRequest = Class(TGoogleBaseObject)
  Private
    FdatasetIds : TStringArray;
    FpageSize : integer;
    FpageToken : String;
  Protected
    //Property setters
    Procedure SetdatasetIds(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetpageSize(AIndex : Integer; AValue : integer); virtual;
    Procedure SetpageToken(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property datasetIds : TStringArray Index 0 Read FdatasetIds Write SetdatasetIds;
    Property pageSize : integer Index 8 Read FpageSize Write SetpageSize;
    Property pageToken : String Index 16 Read FpageToken Write SetpageToken;
  end;
  TSearchVariantSetsRequestClass = Class of TSearchVariantSetsRequest;
  
  { --------------------------------------------------------------------
    TSearchVariantSetsResponse
    --------------------------------------------------------------------}
  
  TSearchVariantSetsResponse = Class(TGoogleBaseObject)
  Private
    FnextPageToken : String;
    FvariantSets : TSearchVariantSetsResponseTypevariantSetsArray;
  Protected
    //Property setters
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SetvariantSets(AIndex : Integer; AValue : TSearchVariantSetsResponseTypevariantSetsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property nextPageToken : String Index 0 Read FnextPageToken Write SetnextPageToken;
    Property variantSets : TSearchVariantSetsResponseTypevariantSetsArray Index 8 Read FvariantSets Write SetvariantSets;
  end;
  TSearchVariantSetsResponseClass = Class of TSearchVariantSetsResponse;
  
  { --------------------------------------------------------------------
    TSearchVariantsRequest
    --------------------------------------------------------------------}
  
  TSearchVariantsRequest = Class(TGoogleBaseObject)
  Private
    FcallSetIds : TStringArray;
    F_end : String;
    FmaxCalls : integer;
    FpageSize : integer;
    FpageToken : String;
    FreferenceName : String;
    Fstart : String;
    FvariantName : String;
    FvariantSetIds : TStringArray;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetcallSetIds(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Set_end(AIndex : Integer; AValue : String); virtual;
    Procedure SetmaxCalls(AIndex : Integer; AValue : integer); virtual;
    Procedure SetpageSize(AIndex : Integer; AValue : integer); virtual;
    Procedure SetpageToken(AIndex : Integer; AValue : String); virtual;
    Procedure SetreferenceName(AIndex : Integer; AValue : String); virtual;
    Procedure Setstart(AIndex : Integer; AValue : String); virtual;
    Procedure SetvariantName(AIndex : Integer; AValue : String); virtual;
    Procedure SetvariantSetIds(AIndex : Integer; AValue : TStringArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property callSetIds : TStringArray Index 0 Read FcallSetIds Write SetcallSetIds;
    Property _end : String Index 8 Read F_end Write Set_end;
    Property maxCalls : integer Index 16 Read FmaxCalls Write SetmaxCalls;
    Property pageSize : integer Index 24 Read FpageSize Write SetpageSize;
    Property pageToken : String Index 32 Read FpageToken Write SetpageToken;
    Property referenceName : String Index 40 Read FreferenceName Write SetreferenceName;
    Property start : String Index 48 Read Fstart Write Setstart;
    Property variantName : String Index 56 Read FvariantName Write SetvariantName;
    Property variantSetIds : TStringArray Index 64 Read FvariantSetIds Write SetvariantSetIds;
  end;
  TSearchVariantsRequestClass = Class of TSearchVariantsRequest;
  
  { --------------------------------------------------------------------
    TSearchVariantsResponse
    --------------------------------------------------------------------}
  
  TSearchVariantsResponse = Class(TGoogleBaseObject)
  Private
    FnextPageToken : String;
    Fvariants : TSearchVariantsResponseTypevariantsArray;
  Protected
    //Property setters
    Procedure SetnextPageToken(AIndex : Integer; AValue : String); virtual;
    Procedure Setvariants(AIndex : Integer; AValue : TSearchVariantsResponseTypevariantsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property nextPageToken : String Index 0 Read FnextPageToken Write SetnextPageToken;
    Property variants : TSearchVariantsResponseTypevariantsArray Index 8 Read Fvariants Write Setvariants;
  end;
  TSearchVariantsResponseClass = Class of TSearchVariantsResponse;
  
  { --------------------------------------------------------------------
    TTranscript
    --------------------------------------------------------------------}
  
  TTranscript = Class(TGoogleBaseObject)
  Private
    FcodingSequence : TTranscriptCodingSequence;
    Fexons : TTranscriptTypeexonsArray;
    FgeneId : String;
  Protected
    //Property setters
    Procedure SetcodingSequence(AIndex : Integer; AValue : TTranscriptCodingSequence); virtual;
    Procedure Setexons(AIndex : Integer; AValue : TTranscriptTypeexonsArray); virtual;
    Procedure SetgeneId(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property codingSequence : TTranscriptCodingSequence Index 0 Read FcodingSequence Write SetcodingSequence;
    Property exons : TTranscriptTypeexonsArray Index 8 Read Fexons Write Setexons;
    Property geneId : String Index 16 Read FgeneId Write SetgeneId;
  end;
  TTranscriptClass = Class of TTranscript;
  
  { --------------------------------------------------------------------
    TTranscriptCodingSequence
    --------------------------------------------------------------------}
  
  TTranscriptCodingSequence = Class(TGoogleBaseObject)
  Private
    F_end : String;
    Fstart : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_end(AIndex : Integer; AValue : String); virtual;
    Procedure Setstart(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property _end : String Index 0 Read F_end Write Set_end;
    Property start : String Index 8 Read Fstart Write Setstart;
  end;
  TTranscriptCodingSequenceClass = Class of TTranscriptCodingSequence;
  
  { --------------------------------------------------------------------
    TTranscriptExon
    --------------------------------------------------------------------}
  
  TTranscriptExon = Class(TGoogleBaseObject)
  Private
    F_end : String;
    Fframe : TInt32Value;
    Fstart : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_end(AIndex : Integer; AValue : String); virtual;
    Procedure Setframe(AIndex : Integer; AValue : TInt32Value); virtual;
    Procedure Setstart(AIndex : Integer; AValue : String); virtual;
  Public
  Published
    Property _end : String Index 0 Read F_end Write Set_end;
    Property frame : TInt32Value Index 8 Read Fframe Write Setframe;
    Property start : String Index 16 Read Fstart Write Setstart;
  end;
  TTranscriptExonClass = Class of TTranscriptExon;
  
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
    FalternateBases : TStringArray;
    Fcalls : TVariantTypecallsArray;
    Fcreated : String;
    F_end : String;
    Ffilter : TStringArray;
    Fid : String;
    Finfo : TVariantTypeinfo;
    Fnames : TStringArray;
    Fquality : double;
    FreferenceBases : String;
    FreferenceName : String;
    Fstart : String;
    FvariantSetId : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetalternateBases(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setcalls(AIndex : Integer; AValue : TVariantTypecallsArray); virtual;
    Procedure Setcreated(AIndex : Integer; AValue : String); virtual;
    Procedure Set_end(AIndex : Integer; AValue : String); virtual;
    Procedure Setfilter(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setinfo(AIndex : Integer; AValue : TVariantTypeinfo); virtual;
    Procedure Setnames(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Setquality(AIndex : Integer; AValue : double); virtual;
    Procedure SetreferenceBases(AIndex : Integer; AValue : String); virtual;
    Procedure SetreferenceName(AIndex : Integer; AValue : String); virtual;
    Procedure Setstart(AIndex : Integer; AValue : String); virtual;
    Procedure SetvariantSetId(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property alternateBases : TStringArray Index 0 Read FalternateBases Write SetalternateBases;
    Property calls : TVariantTypecallsArray Index 8 Read Fcalls Write Setcalls;
    Property created : String Index 16 Read Fcreated Write Setcreated;
    Property _end : String Index 24 Read F_end Write Set_end;
    Property filter : TStringArray Index 32 Read Ffilter Write Setfilter;
    Property id : String Index 40 Read Fid Write Setid;
    Property info : TVariantTypeinfo Index 48 Read Finfo Write Setinfo;
    Property names : TStringArray Index 56 Read Fnames Write Setnames;
    Property quality : double Index 64 Read Fquality Write Setquality;
    Property referenceBases : String Index 72 Read FreferenceBases Write SetreferenceBases;
    Property referenceName : String Index 80 Read FreferenceName Write SetreferenceName;
    Property start : String Index 88 Read Fstart Write Setstart;
    Property variantSetId : String Index 96 Read FvariantSetId Write SetvariantSetId;
  end;
  TVariantClass = Class of TVariant;
  
  { --------------------------------------------------------------------
    TVariantAnnotation
    --------------------------------------------------------------------}
  
  TVariantAnnotation = Class(TGoogleBaseObject)
  Private
    FalternateBases : String;
    FclinicalSignificance : String;
    Fconditions : TVariantAnnotationTypeconditionsArray;
    Feffect : String;
    FgeneId : String;
    FtranscriptIds : TStringArray;
    F_type : String;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetalternateBases(AIndex : Integer; AValue : String); virtual;
    Procedure SetclinicalSignificance(AIndex : Integer; AValue : String); virtual;
    Procedure Setconditions(AIndex : Integer; AValue : TVariantAnnotationTypeconditionsArray); virtual;
    Procedure Seteffect(AIndex : Integer; AValue : String); virtual;
    Procedure SetgeneId(AIndex : Integer; AValue : String); virtual;
    Procedure SettranscriptIds(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure Set_type(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property alternateBases : String Index 0 Read FalternateBases Write SetalternateBases;
    Property clinicalSignificance : String Index 8 Read FclinicalSignificance Write SetclinicalSignificance;
    Property conditions : TVariantAnnotationTypeconditionsArray Index 16 Read Fconditions Write Setconditions;
    Property effect : String Index 24 Read Feffect Write Seteffect;
    Property geneId : String Index 32 Read FgeneId Write SetgeneId;
    Property transcriptIds : TStringArray Index 40 Read FtranscriptIds Write SettranscriptIds;
    Property _type : String Index 48 Read F_type Write Set_type;
  end;
  TVariantAnnotationClass = Class of TVariantAnnotation;
  
  { --------------------------------------------------------------------
    TVariantAnnotationCondition
    --------------------------------------------------------------------}
  
  TVariantAnnotationCondition = Class(TGoogleBaseObject)
  Private
    FconceptId : String;
    FexternalIds : TVariantAnnotationConditionTypeexternalIdsArray;
    Fnames : TStringArray;
    FomimId : String;
  Protected
    //Property setters
    Procedure SetconceptId(AIndex : Integer; AValue : String); virtual;
    Procedure SetexternalIds(AIndex : Integer; AValue : TVariantAnnotationConditionTypeexternalIdsArray); virtual;
    Procedure Setnames(AIndex : Integer; AValue : TStringArray); virtual;
    Procedure SetomimId(AIndex : Integer; AValue : String); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property conceptId : String Index 0 Read FconceptId Write SetconceptId;
    Property externalIds : TVariantAnnotationConditionTypeexternalIdsArray Index 8 Read FexternalIds Write SetexternalIds;
    Property names : TStringArray Index 16 Read Fnames Write Setnames;
    Property omimId : String Index 24 Read FomimId Write SetomimId;
  end;
  TVariantAnnotationConditionClass = Class of TVariantAnnotationCondition;
  
  { --------------------------------------------------------------------
    TVariantSet
    --------------------------------------------------------------------}
  
  TVariantSet = Class(TGoogleBaseObject)
  Private
    FdatasetId : String;
    Fid : String;
    Fmetadata : TVariantSetTypemetadataArray;
    FreferenceBounds : TVariantSetTypereferenceBoundsArray;
  Protected
    //Property setters
    Procedure SetdatasetId(AIndex : Integer; AValue : String); virtual;
    Procedure Setid(AIndex : Integer; AValue : String); virtual;
    Procedure Setmetadata(AIndex : Integer; AValue : TVariantSetTypemetadataArray); virtual;
    Procedure SetreferenceBounds(AIndex : Integer; AValue : TVariantSetTypereferenceBoundsArray); virtual;
    //2.6.4. bug workaround
    {$IFDEF VER2_6}
    Procedure SetArrayLength(Const AName : String; ALength : Longint); override;
    {$ENDIF VER2_6}
  Public
  Published
    Property datasetId : String Index 0 Read FdatasetId Write SetdatasetId;
    Property id : String Index 8 Read Fid Write Setid;
    Property metadata : TVariantSetTypemetadataArray Index 16 Read Fmetadata Write Setmetadata;
    Property referenceBounds : TVariantSetTypereferenceBoundsArray Index 24 Read FreferenceBounds Write SetreferenceBounds;
  end;
  TVariantSetClass = Class of TVariantSet;
  
  { --------------------------------------------------------------------
    TAnnotationSetsResource
    --------------------------------------------------------------------}
  
  TAnnotationSetsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Create(aAnnotationSet : TAnnotationSet) : TAnnotationSet;overload;
    Procedure Delete(annotationSetId: string);
    Function Get(annotationSetId: string) : TAnnotationSet;
    Function Patch(annotationSetId: string; aAnnotationSet : TAnnotationSet) : TAnnotationSet;
    Function Search(aSearchAnnotationSetsRequest : TSearchAnnotationSetsRequest) : TSearchAnnotationSetsResponse;
    Function Update(annotationSetId: string; aAnnotationSet : TAnnotationSet) : TAnnotationSet;
  end;
  
  
  { --------------------------------------------------------------------
    TAnnotationsResource
    --------------------------------------------------------------------}
  
  TAnnotationsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function BatchCreate(aBatchCreateAnnotationsRequest : TBatchCreateAnnotationsRequest) : TBatchAnnotationsResponse;
    Function Create(aAnnotation : TAnnotation) : TAnnotation;overload;
    Procedure Delete(annotationId: string);
    Function Get(annotationId: string) : TAnnotation;
    Function Patch(annotationId: string; aAnnotation : TAnnotation) : TAnnotation;
    Function Search(aSearchAnnotationsRequest : TSearchAnnotationsRequest) : TSearchAnnotationsResponse;
    Function Update(annotationId: string; aAnnotation : TAnnotation) : TAnnotation;
  end;
  
  
  { --------------------------------------------------------------------
    TCallsetsResource
    --------------------------------------------------------------------}
  
  TCallsetsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Create(aCallSet : TCallSet) : TCallSet;overload;
    Procedure Delete(callSetId: string);
    Function Get(callSetId: string) : TCallSet;
    Function Patch(callSetId: string; aCallSet : TCallSet) : TCallSet;
    Function Search(aSearchCallSetsRequest : TSearchCallSetsRequest) : TSearchCallSetsResponse;
    Function Update(callSetId: string; aCallSet : TCallSet) : TCallSet;
  end;
  
  
  { --------------------------------------------------------------------
    TDatasetsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TDatasetsResource, method List
  
  TDatasetsListOptions = Record
    pageSize : integer;
    pageToken : String;
    projectNumber : int64;
  end;
  
  TDatasetsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Create(aDataset : TDataset) : TDataset;overload;
    Procedure Delete(datasetId: string);
    Function Get(datasetId: string) : TDataset;
    Function List(AQuery : string  = '') : TListDatasetsResponse;
    Function List(AQuery : TDatasetslistOptions) : TListDatasetsResponse;
    Function Patch(datasetId: string; aDataset : TDataset) : TDataset;
    Function Undelete(datasetId: string) : TDataset;
    Function Update(datasetId: string; aDataset : TDataset) : TDataset;
  end;
  
  
  { --------------------------------------------------------------------
    TExperimentalJobsResource
    --------------------------------------------------------------------}
  
  TExperimentalJobsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Create(aExperimentalCreateJobRequest : TExperimentalCreateJobRequest) : TExperimentalCreateJobResponse;overload;
  end;
  
  
  { --------------------------------------------------------------------
    TExperimentalResource
    --------------------------------------------------------------------}
  
  TExperimentalResource = Class(TGoogleResource)
  Private
    FJobsInstance : TExperimentalJobsResource;
    Function GetJobsInstance : TExperimentalJobsResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function CreateJobsResource(AOwner : TComponent) : TExperimentalJobsResource;virtual;overload;
    Function CreateJobsResource : TExperimentalJobsResource;virtual;overload;
    Property JobsResource : TExperimentalJobsResource Read GetJobsInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TJobsResource
    --------------------------------------------------------------------}
  
  TJobsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Cancel(jobId: string);
    Function Get(jobId: string) : TJob;
    Function Search(aSearchJobsRequest : TSearchJobsRequest) : TSearchJobsResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TReadgroupsetsCoveragebucketsResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TReadgroupsetsCoveragebucketsResource, method List
  
  TReadgroupsetsCoveragebucketsListOptions = Record
    pageSize : integer;
    pageToken : String;
    rangeend : int64;
    rangereferenceName : String;
    rangestart : int64;
    targetBucketWidth : int64;
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
  
  TReadgroupsetsResource = Class(TGoogleResource)
  Private
    FCoveragebucketsInstance : TReadgroupsetsCoveragebucketsResource;
    Function GetCoveragebucketsInstance : TReadgroupsetsCoveragebucketsResource;virtual;
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Align(aAlignReadGroupSetsRequest : TAlignReadGroupSetsRequest) : TAlignReadGroupSetsResponse;
    Function Call(aCallReadGroupSetsRequest : TCallReadGroupSetsRequest) : TCallReadGroupSetsResponse;
    Procedure Delete(readGroupSetId: string);
    Function Export(aExportReadGroupSetsRequest : TExportReadGroupSetsRequest) : TExportReadGroupSetsResponse;
    Function Get(readGroupSetId: string) : TReadGroupSet;
    Function Import(aImportReadGroupSetsRequest : TImportReadGroupSetsRequest) : TImportReadGroupSetsResponse;
    Function Patch(readGroupSetId: string; aReadGroupSet : TReadGroupSet) : TReadGroupSet;
    Function Search(aSearchReadGroupSetsRequest : TSearchReadGroupSetsRequest) : TSearchReadGroupSetsResponse;
    Function Update(readGroupSetId: string; aReadGroupSet : TReadGroupSet) : TReadGroupSet;
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
  end;
  
  
  { --------------------------------------------------------------------
    TReferencesBasesResource
    --------------------------------------------------------------------}
  
  
  //Optional query Options for TReferencesBasesResource, method List
  
  TReferencesBasesListOptions = Record
    _end : int64;
    pageSize : integer;
    pageToken : String;
    start : int64;
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
    Function Get(referenceId: string) : TReference;
    Function Search(aSearchReferencesRequest : TSearchReferencesRequest) : TSearchReferencesResponse;
    Function CreateBasesResource(AOwner : TComponent) : TReferencesBasesResource;virtual;overload;
    Function CreateBasesResource : TReferencesBasesResource;virtual;overload;
    Property BasesResource : TReferencesBasesResource Read GetBasesInstance;
  end;
  
  
  { --------------------------------------------------------------------
    TReferencesetsResource
    --------------------------------------------------------------------}
  
  TReferencesetsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(referenceSetId: string) : TReferenceSet;
    Function Search(aSearchReferenceSetsRequest : TSearchReferenceSetsRequest) : TSearchReferenceSetsResponse;
  end;
  
  
  { --------------------------------------------------------------------
    TVariantsResource
    --------------------------------------------------------------------}
  
  TVariantsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Create(aVariant : TVariant) : TVariant;overload;
    Procedure Delete(variantId: string);
    Function Get(variantId: string) : TVariant;
    Function Search(aSearchVariantsRequest : TSearchVariantsRequest) : TSearchVariantsResponse;
    Function Update(variantId: string; aVariant : TVariant) : TVariant;
  end;
  
  
  { --------------------------------------------------------------------
    TVariantsetsResource
    --------------------------------------------------------------------}
  
  TVariantsetsResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Procedure Delete(variantSetId: string);
    Function Export(variantSetId: string; aExportVariantSetRequest : TExportVariantSetRequest) : TExportVariantSetResponse;
    Function Get(variantSetId: string) : TVariantSet;
    Function ImportVariants(variantSetId: string; aImportVariantsRequest : TImportVariantsRequest) : TImportVariantsResponse;
    Procedure MergeVariants(variantSetId: string; aMergeVariantsRequest : TMergeVariantsRequest);
    Function Patch(variantSetId: string; aVariantSet : TVariantSet) : TVariantSet;
    Function Search(aSearchVariantSetsRequest : TSearchVariantSetsRequest) : TSearchVariantSetsResponse;
    Function Update(variantSetId: string; aVariantSet : TVariantSet) : TVariantSet;
  end;
  
  
  { --------------------------------------------------------------------
    TGenomicsAPI
    --------------------------------------------------------------------}
  
  TGenomicsAPI = Class(TGoogleAPI)
  Private
    FAnnotationSetsInstance : TAnnotationSetsResource;
    FAnnotationsInstance : TAnnotationsResource;
    FCallsetsInstance : TCallsetsResource;
    FDatasetsInstance : TDatasetsResource;
    FExperimentalJobsInstance : TExperimentalJobsResource;
    FExperimentalInstance : TExperimentalResource;
    FJobsInstance : TJobsResource;
    FReadgroupsetsCoveragebucketsInstance : TReadgroupsetsCoveragebucketsResource;
    FReadgroupsetsInstance : TReadgroupsetsResource;
    FReadsInstance : TReadsResource;
    FReferencesBasesInstance : TReferencesBasesResource;
    FReferencesInstance : TReferencesResource;
    FReferencesetsInstance : TReferencesetsResource;
    FVariantsInstance : TVariantsResource;
    FVariantsetsInstance : TVariantsetsResource;
    Function GetAnnotationSetsInstance : TAnnotationSetsResource;virtual;
    Function GetAnnotationsInstance : TAnnotationsResource;virtual;
    Function GetCallsetsInstance : TCallsetsResource;virtual;
    Function GetDatasetsInstance : TDatasetsResource;virtual;
    Function GetExperimentalJobsInstance : TExperimentalJobsResource;virtual;
    Function GetExperimentalInstance : TExperimentalResource;virtual;
    Function GetJobsInstance : TJobsResource;virtual;
    Function GetReadgroupsetsCoveragebucketsInstance : TReadgroupsetsCoveragebucketsResource;virtual;
    Function GetReadgroupsetsInstance : TReadgroupsetsResource;virtual;
    Function GetReadsInstance : TReadsResource;virtual;
    Function GetReferencesBasesInstance : TReferencesBasesResource;virtual;
    Function GetReferencesInstance : TReferencesResource;virtual;
    Function GetReferencesetsInstance : TReferencesetsResource;virtual;
    Function GetVariantsInstance : TVariantsResource;virtual;
    Function GetVariantsetsInstance : TVariantsetsResource;virtual;
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
    Function CreateAnnotationSetsResource(AOwner : TComponent) : TAnnotationSetsResource;virtual;overload;
    Function CreateAnnotationSetsResource : TAnnotationSetsResource;virtual;overload;
    Function CreateAnnotationsResource(AOwner : TComponent) : TAnnotationsResource;virtual;overload;
    Function CreateAnnotationsResource : TAnnotationsResource;virtual;overload;
    Function CreateCallsetsResource(AOwner : TComponent) : TCallsetsResource;virtual;overload;
    Function CreateCallsetsResource : TCallsetsResource;virtual;overload;
    Function CreateDatasetsResource(AOwner : TComponent) : TDatasetsResource;virtual;overload;
    Function CreateDatasetsResource : TDatasetsResource;virtual;overload;
    Function CreateExperimentalJobsResource(AOwner : TComponent) : TExperimentalJobsResource;virtual;overload;
    Function CreateExperimentalJobsResource : TExperimentalJobsResource;virtual;overload;
    Function CreateExperimentalResource(AOwner : TComponent) : TExperimentalResource;virtual;overload;
    Function CreateExperimentalResource : TExperimentalResource;virtual;overload;
    Function CreateJobsResource(AOwner : TComponent) : TJobsResource;virtual;overload;
    Function CreateJobsResource : TJobsResource;virtual;overload;
    Function CreateReadgroupsetsCoveragebucketsResource(AOwner : TComponent) : TReadgroupsetsCoveragebucketsResource;virtual;overload;
    Function CreateReadgroupsetsCoveragebucketsResource : TReadgroupsetsCoveragebucketsResource;virtual;overload;
    Function CreateReadgroupsetsResource(AOwner : TComponent) : TReadgroupsetsResource;virtual;overload;
    Function CreateReadgroupsetsResource : TReadgroupsetsResource;virtual;overload;
    Function CreateReadsResource(AOwner : TComponent) : TReadsResource;virtual;overload;
    Function CreateReadsResource : TReadsResource;virtual;overload;
    Function CreateReferencesBasesResource(AOwner : TComponent) : TReferencesBasesResource;virtual;overload;
    Function CreateReferencesBasesResource : TReferencesBasesResource;virtual;overload;
    Function CreateReferencesResource(AOwner : TComponent) : TReferencesResource;virtual;overload;
    Function CreateReferencesResource : TReferencesResource;virtual;overload;
    Function CreateReferencesetsResource(AOwner : TComponent) : TReferencesetsResource;virtual;overload;
    Function CreateReferencesetsResource : TReferencesetsResource;virtual;overload;
    Function CreateVariantsResource(AOwner : TComponent) : TVariantsResource;virtual;overload;
    Function CreateVariantsResource : TVariantsResource;virtual;overload;
    Function CreateVariantsetsResource(AOwner : TComponent) : TVariantsetsResource;virtual;overload;
    Function CreateVariantsetsResource : TVariantsetsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property AnnotationSetsResource : TAnnotationSetsResource Read GetAnnotationSetsInstance;
    Property AnnotationsResource : TAnnotationsResource Read GetAnnotationsInstance;
    Property CallsetsResource : TCallsetsResource Read GetCallsetsInstance;
    Property DatasetsResource : TDatasetsResource Read GetDatasetsInstance;
    Property ExperimentalJobsResource : TExperimentalJobsResource Read GetExperimentalJobsInstance;
    Property ExperimentalResource : TExperimentalResource Read GetExperimentalInstance;
    Property JobsResource : TJobsResource Read GetJobsInstance;
    Property ReadgroupsetsCoveragebucketsResource : TReadgroupsetsCoveragebucketsResource Read GetReadgroupsetsCoveragebucketsInstance;
    Property ReadgroupsetsResource : TReadgroupsetsResource Read GetReadgroupsetsInstance;
    Property ReadsResource : TReadsResource Read GetReadsInstance;
    Property ReferencesBasesResource : TReferencesBasesResource Read GetReferencesBasesInstance;
    Property ReferencesResource : TReferencesResource Read GetReferencesInstance;
    Property ReferencesetsResource : TReferencesetsResource Read GetReferencesetsInstance;
    Property VariantsResource : TVariantsResource Read GetVariantsInstance;
    Property VariantsetsResource : TVariantsetsResource Read GetVariantsetsInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TAlignReadGroupSetsRequest
  --------------------------------------------------------------------}


Procedure TAlignReadGroupSetsRequest.SetbamSourceUris(AIndex : Integer; AValue : TStringArray); 

begin
  If (FbamSourceUris=AValue) then exit;
  FbamSourceUris:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAlignReadGroupSetsRequest.SetdatasetId(AIndex : Integer; AValue : String); 

begin
  If (FdatasetId=AValue) then exit;
  FdatasetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAlignReadGroupSetsRequest.SetinterleavedFastqSource(AIndex : Integer; AValue : TInterleavedFastqSource); 

begin
  If (FinterleavedFastqSource=AValue) then exit;
  FinterleavedFastqSource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAlignReadGroupSetsRequest.SetpairedFastqSource(AIndex : Integer; AValue : TPairedFastqSource); 

begin
  If (FpairedFastqSource=AValue) then exit;
  FpairedFastqSource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAlignReadGroupSetsRequest.SetreadGroupSetId(AIndex : Integer; AValue : String); 

begin
  If (FreadGroupSetId=AValue) then exit;
  FreadGroupSetId:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TAlignReadGroupSetsRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'bamsourceuris' : SetLength(FbamSourceUris,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TAlignReadGroupSetsResponse
  --------------------------------------------------------------------}


Procedure TAlignReadGroupSetsResponse.SetjobId(AIndex : Integer; AValue : String); 

begin
  If (FjobId=AValue) then exit;
  FjobId:=AValue;
  MarkPropertyChanged(AIndex);
end;





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


Procedure TAnnotation.SetannotationSetId(AIndex : Integer; AValue : String); 

begin
  If (FannotationSetId=AValue) then exit;
  FannotationSetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotation.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotation.Setinfo(AIndex : Integer; AValue : TAnnotationTypeinfo); 

begin
  If (Finfo=AValue) then exit;
  Finfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotation.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotation.Setposition(AIndex : Integer; AValue : TRangePosition); 

begin
  If (Fposition=AValue) then exit;
  Fposition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotation.Settranscript(AIndex : Integer; AValue : TTranscript); 

begin
  If (Ftranscript=AValue) then exit;
  Ftranscript:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotation.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotation.Setvariant(AIndex : Integer; AValue : TVariantAnnotation); 

begin
  If (Fvariant=AValue) then exit;
  Fvariant:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TAnnotation.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




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


Procedure TAnnotationSet.SetdatasetId(AIndex : Integer; AValue : String); 

begin
  If (FdatasetId=AValue) then exit;
  FdatasetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationSet.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationSet.Setinfo(AIndex : Integer; AValue : TAnnotationSetTypeinfo); 

begin
  If (Finfo=AValue) then exit;
  Finfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationSet.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationSet.SetreferenceSetId(AIndex : Integer; AValue : String); 

begin
  If (FreferenceSetId=AValue) then exit;
  FreferenceSetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationSet.SetsourceUri(AIndex : Integer; AValue : String); 

begin
  If (FsourceUri=AValue) then exit;
  FsourceUri:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationSet.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
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
  TBatchAnnotationsResponse
  --------------------------------------------------------------------}


Procedure TBatchAnnotationsResponse.Setentries(AIndex : Integer; AValue : TBatchAnnotationsResponseTypeentriesArray); 

begin
  If (Fentries=AValue) then exit;
  Fentries:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TBatchAnnotationsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'entries' : SetLength(Fentries,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TBatchAnnotationsResponseEntry
  --------------------------------------------------------------------}


Procedure TBatchAnnotationsResponseEntry.Setannotation(AIndex : Integer; AValue : TAnnotation); 

begin
  If (Fannotation=AValue) then exit;
  Fannotation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBatchAnnotationsResponseEntry.Setstatus(AIndex : Integer; AValue : TBatchAnnotationsResponseEntryStatus); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBatchAnnotationsResponseEntryStatus
  --------------------------------------------------------------------}


Procedure TBatchAnnotationsResponseEntryStatus.Setcode(AIndex : Integer; AValue : integer); 

begin
  If (Fcode=AValue) then exit;
  Fcode:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TBatchAnnotationsResponseEntryStatus.Setmessage(AIndex : Integer; AValue : String); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBatchCreateAnnotationsRequest
  --------------------------------------------------------------------}


Procedure TBatchCreateAnnotationsRequest.Setannotations(AIndex : Integer; AValue : TBatchCreateAnnotationsRequestTypeannotationsArray); 

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
  TCallTypeinfo
  --------------------------------------------------------------------}


Class Function TCallTypeinfo.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TCall
  --------------------------------------------------------------------}


Procedure TCall.SetcallSetId(AIndex : Integer; AValue : String); 

begin
  If (FcallSetId=AValue) then exit;
  FcallSetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCall.SetcallSetName(AIndex : Integer; AValue : String); 

begin
  If (FcallSetName=AValue) then exit;
  FcallSetName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCall.Setgenotype(AIndex : Integer; AValue : TintegerArray); 

begin
  If (Fgenotype=AValue) then exit;
  Fgenotype:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCall.SetgenotypeLikelihood(AIndex : Integer; AValue : TdoubleArray); 

begin
  If (FgenotypeLikelihood=AValue) then exit;
  FgenotypeLikelihood:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCall.Setinfo(AIndex : Integer; AValue : TCallTypeinfo); 

begin
  If (Finfo=AValue) then exit;
  Finfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCall.Setphaseset(AIndex : Integer; AValue : String); 

begin
  If (Fphaseset=AValue) then exit;
  Fphaseset:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TCall.SetArrayLength(Const AName : String; ALength : Longint); 

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
  TCallReadGroupSetsRequest
  --------------------------------------------------------------------}


Procedure TCallReadGroupSetsRequest.SetdatasetId(AIndex : Integer; AValue : String); 

begin
  If (FdatasetId=AValue) then exit;
  FdatasetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCallReadGroupSetsRequest.SetreadGroupSetId(AIndex : Integer; AValue : String); 

begin
  If (FreadGroupSetId=AValue) then exit;
  FreadGroupSetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCallReadGroupSetsRequest.SetsourceUris(AIndex : Integer; AValue : TStringArray); 

begin
  If (FsourceUris=AValue) then exit;
  FsourceUris:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TCallReadGroupSetsRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'sourceuris' : SetLength(FsourceUris,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TCallReadGroupSetsResponse
  --------------------------------------------------------------------}


Procedure TCallReadGroupSetsResponse.SetjobId(AIndex : Integer; AValue : String); 

begin
  If (FjobId=AValue) then exit;
  FjobId:=AValue;
  MarkPropertyChanged(AIndex);
end;





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


Procedure TCallSet.Setcreated(AIndex : Integer; AValue : String); 

begin
  If (Fcreated=AValue) then exit;
  Fcreated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCallSet.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCallSet.Setinfo(AIndex : Integer; AValue : TCallSetTypeinfo); 

begin
  If (Finfo=AValue) then exit;
  Finfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCallSet.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCallSet.SetsampleId(AIndex : Integer; AValue : String); 

begin
  If (FsampleId=AValue) then exit;
  FsampleId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCallSet.SetvariantSetIds(AIndex : Integer; AValue : TStringArray); 

begin
  If (FvariantSetIds=AValue) then exit;
  FvariantSetIds:=AValue;
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
  TCigarUnit
  --------------------------------------------------------------------}


Procedure TCigarUnit.Setoperation(AIndex : Integer; AValue : String); 

begin
  If (Foperation=AValue) then exit;
  Foperation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCigarUnit.SetoperationLength(AIndex : Integer; AValue : String); 

begin
  If (FoperationLength=AValue) then exit;
  FoperationLength:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCigarUnit.SetreferenceSequence(AIndex : Integer; AValue : String); 

begin
  If (FreferenceSequence=AValue) then exit;
  FreferenceSequence:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCoverageBucket
  --------------------------------------------------------------------}


Procedure TCoverageBucket.SetmeanCoverage(AIndex : Integer; AValue : integer); 

begin
  If (FmeanCoverage=AValue) then exit;
  FmeanCoverage:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCoverageBucket.Setrange(AIndex : Integer; AValue : TRange); 

begin
  If (Frange=AValue) then exit;
  Frange:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TDataset
  --------------------------------------------------------------------}


Procedure TDataset.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDataset.SetisPublic(AIndex : Integer; AValue : boolean); 

begin
  If (FisPublic=AValue) then exit;
  FisPublic:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDataset.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDataset.SetprojectNumber(AIndex : Integer; AValue : String); 

begin
  If (FprojectNumber=AValue) then exit;
  FprojectNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TExperimentalCreateJobRequest
  --------------------------------------------------------------------}


Procedure TExperimentalCreateJobRequest.Setalign(AIndex : Integer; AValue : boolean); 

begin
  If (Falign=AValue) then exit;
  Falign:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperimentalCreateJobRequest.SetcallVariants(AIndex : Integer; AValue : boolean); 

begin
  If (FcallVariants=AValue) then exit;
  FcallVariants:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperimentalCreateJobRequest.SetgcsOutputPath(AIndex : Integer; AValue : String); 

begin
  If (FgcsOutputPath=AValue) then exit;
  FgcsOutputPath:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperimentalCreateJobRequest.SetpairedSourceUris(AIndex : Integer; AValue : TStringArray); 

begin
  If (FpairedSourceUris=AValue) then exit;
  FpairedSourceUris:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperimentalCreateJobRequest.SetprojectNumber(AIndex : Integer; AValue : String); 

begin
  If (FprojectNumber=AValue) then exit;
  FprojectNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperimentalCreateJobRequest.SetsourceUris(AIndex : Integer; AValue : TStringArray); 

begin
  If (FsourceUris=AValue) then exit;
  FsourceUris:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TExperimentalCreateJobRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'pairedsourceuris' : SetLength(FpairedSourceUris,ALength);
  'sourceuris' : SetLength(FsourceUris,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TExperimentalCreateJobResponse
  --------------------------------------------------------------------}


Procedure TExperimentalCreateJobResponse.SetjobId(AIndex : Integer; AValue : String); 

begin
  If (FjobId=AValue) then exit;
  FjobId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TExportReadGroupSetsRequest
  --------------------------------------------------------------------}


Procedure TExportReadGroupSetsRequest.SetexportUri(AIndex : Integer; AValue : String); 

begin
  If (FexportUri=AValue) then exit;
  FexportUri:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExportReadGroupSetsRequest.SetprojectNumber(AIndex : Integer; AValue : String); 

begin
  If (FprojectNumber=AValue) then exit;
  FprojectNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExportReadGroupSetsRequest.SetreadGroupSetIds(AIndex : Integer; AValue : TStringArray); 

begin
  If (FreadGroupSetIds=AValue) then exit;
  FreadGroupSetIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExportReadGroupSetsRequest.SetreferenceNames(AIndex : Integer; AValue : TStringArray); 

begin
  If (FreferenceNames=AValue) then exit;
  FreferenceNames:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TExportReadGroupSetsRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'readgroupsetids' : SetLength(FreadGroupSetIds,ALength);
  'referencenames' : SetLength(FreferenceNames,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TExportReadGroupSetsResponse
  --------------------------------------------------------------------}


Procedure TExportReadGroupSetsResponse.SetjobId(AIndex : Integer; AValue : String); 

begin
  If (FjobId=AValue) then exit;
  FjobId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TExportVariantSetRequest
  --------------------------------------------------------------------}


Procedure TExportVariantSetRequest.SetbigqueryDataset(AIndex : Integer; AValue : String); 

begin
  If (FbigqueryDataset=AValue) then exit;
  FbigqueryDataset:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExportVariantSetRequest.SetbigqueryTable(AIndex : Integer; AValue : String); 

begin
  If (FbigqueryTable=AValue) then exit;
  FbigqueryTable:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExportVariantSetRequest.SetcallSetIds(AIndex : Integer; AValue : TStringArray); 

begin
  If (FcallSetIds=AValue) then exit;
  FcallSetIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExportVariantSetRequest.Setformat(AIndex : Integer; AValue : String); 

begin
  If (Fformat=AValue) then exit;
  Fformat:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExportVariantSetRequest.SetprojectNumber(AIndex : Integer; AValue : String); 

begin
  If (FprojectNumber=AValue) then exit;
  FprojectNumber:=AValue;
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
  TExportVariantSetResponse
  --------------------------------------------------------------------}


Procedure TExportVariantSetResponse.SetjobId(AIndex : Integer; AValue : String); 

begin
  If (FjobId=AValue) then exit;
  FjobId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TExternalId
  --------------------------------------------------------------------}


Procedure TExternalId.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExternalId.SetsourceName(AIndex : Integer; AValue : String); 

begin
  If (FsourceName=AValue) then exit;
  FsourceName:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFastqMetadata
  --------------------------------------------------------------------}


Procedure TFastqMetadata.SetlibraryName(AIndex : Integer; AValue : String); 

begin
  If (FlibraryName=AValue) then exit;
  FlibraryName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFastqMetadata.SetplatformName(AIndex : Integer; AValue : String); 

begin
  If (FplatformName=AValue) then exit;
  FplatformName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFastqMetadata.SetplatformUnit(AIndex : Integer; AValue : String); 

begin
  If (FplatformUnit=AValue) then exit;
  FplatformUnit:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFastqMetadata.SetreadGroupName(AIndex : Integer; AValue : String); 

begin
  If (FreadGroupName=AValue) then exit;
  FreadGroupName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFastqMetadata.SetsampleName(AIndex : Integer; AValue : String); 

begin
  If (FsampleName=AValue) then exit;
  FsampleName:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TImportReadGroupSetsRequest
  --------------------------------------------------------------------}


Procedure TImportReadGroupSetsRequest.SetdatasetId(AIndex : Integer; AValue : String); 

begin
  If (FdatasetId=AValue) then exit;
  FdatasetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImportReadGroupSetsRequest.SetpartitionStrategy(AIndex : Integer; AValue : String); 

begin
  If (FpartitionStrategy=AValue) then exit;
  FpartitionStrategy:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImportReadGroupSetsRequest.SetreferenceSetId(AIndex : Integer; AValue : String); 

begin
  If (FreferenceSetId=AValue) then exit;
  FreferenceSetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImportReadGroupSetsRequest.SetsourceUris(AIndex : Integer; AValue : TStringArray); 

begin
  If (FsourceUris=AValue) then exit;
  FsourceUris:=AValue;
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
  TImportReadGroupSetsResponse
  --------------------------------------------------------------------}


Procedure TImportReadGroupSetsResponse.SetjobId(AIndex : Integer; AValue : String); 

begin
  If (FjobId=AValue) then exit;
  FjobId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TImportVariantsRequest
  --------------------------------------------------------------------}


Procedure TImportVariantsRequest.Setformat(AIndex : Integer; AValue : String); 

begin
  If (Fformat=AValue) then exit;
  Fformat:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImportVariantsRequest.SetsourceUris(AIndex : Integer; AValue : TStringArray); 

begin
  If (FsourceUris=AValue) then exit;
  FsourceUris:=AValue;
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
  TImportVariantsResponse
  --------------------------------------------------------------------}


Procedure TImportVariantsResponse.SetjobId(AIndex : Integer; AValue : String); 

begin
  If (FjobId=AValue) then exit;
  FjobId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInt32Value
  --------------------------------------------------------------------}


Procedure TInt32Value.Setvalue(AIndex : Integer; AValue : integer); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInterleavedFastqSource
  --------------------------------------------------------------------}


Procedure TInterleavedFastqSource.Setmetadata(AIndex : Integer; AValue : TFastqMetadata); 

begin
  If (Fmetadata=AValue) then exit;
  Fmetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TInterleavedFastqSource.SetsourceUris(AIndex : Integer; AValue : TStringArray); 

begin
  If (FsourceUris=AValue) then exit;
  FsourceUris:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TInterleavedFastqSource.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'sourceuris' : SetLength(FsourceUris,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TJob
  --------------------------------------------------------------------}


Procedure TJob.Setcreated(AIndex : Integer; AValue : String); 

begin
  If (Fcreated=AValue) then exit;
  Fcreated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.SetdetailedStatus(AIndex : Integer; AValue : String); 

begin
  If (FdetailedStatus=AValue) then exit;
  FdetailedStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.Seterrors(AIndex : Integer; AValue : TStringArray); 

begin
  If (Ferrors=AValue) then exit;
  Ferrors:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.SetimportedIds(AIndex : Integer; AValue : TStringArray); 

begin
  If (FimportedIds=AValue) then exit;
  FimportedIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.SetprojectNumber(AIndex : Integer; AValue : String); 

begin
  If (FprojectNumber=AValue) then exit;
  FprojectNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.Setrequest(AIndex : Integer; AValue : TJobRequest); 

begin
  If (Frequest=AValue) then exit;
  Frequest:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.Setstatus(AIndex : Integer; AValue : String); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.Setwarnings(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fwarnings=AValue) then exit;
  Fwarnings:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TJob.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'errors' : SetLength(Ferrors,ALength);
  'importedids' : SetLength(FimportedIds,ALength);
  'warnings' : SetLength(Fwarnings,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TJobRequest
  --------------------------------------------------------------------}


Procedure TJobRequest.Setdestination(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fdestination=AValue) then exit;
  Fdestination:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobRequest.Setsource(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fsource=AValue) then exit;
  Fsource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobRequest.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TJobRequest.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;

//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TJobRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'destination' : SetLength(Fdestination,ALength);
  'source' : SetLength(Fsource,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TLinearAlignment
  --------------------------------------------------------------------}


Procedure TLinearAlignment.Setcigar(AIndex : Integer; AValue : TLinearAlignmentTypecigarArray); 

begin
  If (Fcigar=AValue) then exit;
  Fcigar:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLinearAlignment.SetmappingQuality(AIndex : Integer; AValue : integer); 

begin
  If (FmappingQuality=AValue) then exit;
  FmappingQuality:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TLinearAlignment.Setposition(AIndex : Integer; AValue : TPosition); 

begin
  If (Fposition=AValue) then exit;
  Fposition:=AValue;
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
  TListBasesResponse
  --------------------------------------------------------------------}


Procedure TListBasesResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListBasesResponse.Setoffset(AIndex : Integer; AValue : String); 

begin
  If (Foffset=AValue) then exit;
  Foffset:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListBasesResponse.Setsequence(AIndex : Integer; AValue : String); 

begin
  If (Fsequence=AValue) then exit;
  Fsequence:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListCoverageBucketsResponse
  --------------------------------------------------------------------}


Procedure TListCoverageBucketsResponse.SetbucketWidth(AIndex : Integer; AValue : String); 

begin
  If (FbucketWidth=AValue) then exit;
  FbucketWidth:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListCoverageBucketsResponse.SetcoverageBuckets(AIndex : Integer; AValue : TListCoverageBucketsResponseTypecoverageBucketsArray); 

begin
  If (FcoverageBuckets=AValue) then exit;
  FcoverageBuckets:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListCoverageBucketsResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

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
  TListDatasetsResponse
  --------------------------------------------------------------------}


Procedure TListDatasetsResponse.Setdatasets(AIndex : Integer; AValue : TListDatasetsResponseTypedatasetsArray); 

begin
  If (Fdatasets=AValue) then exit;
  Fdatasets:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListDatasetsResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

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
  TMergeVariantsRequest
  --------------------------------------------------------------------}


Procedure TMergeVariantsRequest.Setvariants(AIndex : Integer; AValue : TMergeVariantsRequestTypevariantsArray); 

begin
  If (Fvariants=AValue) then exit;
  Fvariants:=AValue;
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
  TMetadataTypeinfo
  --------------------------------------------------------------------}


Class Function TMetadataTypeinfo.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TMetadata
  --------------------------------------------------------------------}


Procedure TMetadata.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetadata.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetadata.Setinfo(AIndex : Integer; AValue : TMetadataTypeinfo); 

begin
  If (Finfo=AValue) then exit;
  Finfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetadata.Setkey(AIndex : Integer; AValue : String); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetadata.Setnumber(AIndex : Integer; AValue : String); 

begin
  If (Fnumber=AValue) then exit;
  Fnumber:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetadata.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetadata.Setvalue(AIndex : Integer; AValue : String); 

begin
  If (Fvalue=AValue) then exit;
  Fvalue:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TMetadata.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_type' : Result:='type';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TPairedFastqSource
  --------------------------------------------------------------------}


Procedure TPairedFastqSource.SetfirstSourceUris(AIndex : Integer; AValue : TStringArray); 

begin
  If (FfirstSourceUris=AValue) then exit;
  FfirstSourceUris:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPairedFastqSource.Setmetadata(AIndex : Integer; AValue : TFastqMetadata); 

begin
  If (Fmetadata=AValue) then exit;
  Fmetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPairedFastqSource.SetsecondSourceUris(AIndex : Integer; AValue : TStringArray); 

begin
  If (FsecondSourceUris=AValue) then exit;
  FsecondSourceUris:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TPairedFastqSource.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'firstsourceuris' : SetLength(FfirstSourceUris,ALength);
  'secondsourceuris' : SetLength(FsecondSourceUris,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TPosition
  --------------------------------------------------------------------}


Procedure TPosition.Setposition(AIndex : Integer; AValue : String); 

begin
  If (Fposition=AValue) then exit;
  Fposition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPosition.SetreferenceName(AIndex : Integer; AValue : String); 

begin
  If (FreferenceName=AValue) then exit;
  FreferenceName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPosition.SetreverseStrand(AIndex : Integer; AValue : boolean); 

begin
  If (FreverseStrand=AValue) then exit;
  FreverseStrand:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TQueryRange
  --------------------------------------------------------------------}


Procedure TQueryRange.Set_end(AIndex : Integer; AValue : String); 

begin
  If (F_end=AValue) then exit;
  F_end:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQueryRange.SetreferenceId(AIndex : Integer; AValue : String); 

begin
  If (FreferenceId=AValue) then exit;
  FreferenceId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQueryRange.SetreferenceName(AIndex : Integer; AValue : String); 

begin
  If (FreferenceName=AValue) then exit;
  FreferenceName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQueryRange.Setstart(AIndex : Integer; AValue : String); 

begin
  If (Fstart=AValue) then exit;
  Fstart:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TQueryRange.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_end' : Result:='end';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TRange
  --------------------------------------------------------------------}


Procedure TRange.Set_end(AIndex : Integer; AValue : String); 

begin
  If (F_end=AValue) then exit;
  F_end:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRange.SetreferenceName(AIndex : Integer; AValue : String); 

begin
  If (FreferenceName=AValue) then exit;
  FreferenceName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRange.Setstart(AIndex : Integer; AValue : String); 

begin
  If (Fstart=AValue) then exit;
  Fstart:=AValue;
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
  TRangePosition
  --------------------------------------------------------------------}


Procedure TRangePosition.Set_end(AIndex : Integer; AValue : String); 

begin
  If (F_end=AValue) then exit;
  F_end:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRangePosition.SetreferenceId(AIndex : Integer; AValue : String); 

begin
  If (FreferenceId=AValue) then exit;
  FreferenceId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRangePosition.SetreferenceName(AIndex : Integer; AValue : String); 

begin
  If (FreferenceName=AValue) then exit;
  FreferenceName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRangePosition.SetreverseStrand(AIndex : Integer; AValue : boolean); 

begin
  If (FreverseStrand=AValue) then exit;
  FreverseStrand:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRangePosition.Setstart(AIndex : Integer; AValue : String); 

begin
  If (Fstart=AValue) then exit;
  Fstart:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TRangePosition.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_end' : Result:='end';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




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


Procedure TRead.SetalignedQuality(AIndex : Integer; AValue : TintegerArray); 

begin
  If (FalignedQuality=AValue) then exit;
  FalignedQuality:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRead.SetalignedSequence(AIndex : Integer; AValue : String); 

begin
  If (FalignedSequence=AValue) then exit;
  FalignedSequence:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRead.Setalignment(AIndex : Integer; AValue : TLinearAlignment); 

begin
  If (Falignment=AValue) then exit;
  Falignment:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRead.SetduplicateFragment(AIndex : Integer; AValue : boolean); 

begin
  If (FduplicateFragment=AValue) then exit;
  FduplicateFragment:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRead.SetfailedVendorQualityChecks(AIndex : Integer; AValue : boolean); 

begin
  If (FfailedVendorQualityChecks=AValue) then exit;
  FfailedVendorQualityChecks:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRead.SetfragmentLength(AIndex : Integer; AValue : integer); 

begin
  If (FfragmentLength=AValue) then exit;
  FfragmentLength:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRead.SetfragmentName(AIndex : Integer; AValue : String); 

begin
  If (FfragmentName=AValue) then exit;
  FfragmentName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRead.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRead.Setinfo(AIndex : Integer; AValue : TReadTypeinfo); 

begin
  If (Finfo=AValue) then exit;
  Finfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRead.SetnextMatePosition(AIndex : Integer; AValue : TPosition); 

begin
  If (FnextMatePosition=AValue) then exit;
  FnextMatePosition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRead.SetnumberReads(AIndex : Integer; AValue : integer); 

begin
  If (FnumberReads=AValue) then exit;
  FnumberReads:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRead.SetproperPlacement(AIndex : Integer; AValue : boolean); 

begin
  If (FproperPlacement=AValue) then exit;
  FproperPlacement:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRead.SetreadGroupId(AIndex : Integer; AValue : String); 

begin
  If (FreadGroupId=AValue) then exit;
  FreadGroupId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRead.SetreadGroupSetId(AIndex : Integer; AValue : String); 

begin
  If (FreadGroupSetId=AValue) then exit;
  FreadGroupSetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRead.SetreadNumber(AIndex : Integer; AValue : integer); 

begin
  If (FreadNumber=AValue) then exit;
  FreadNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRead.SetsecondaryAlignment(AIndex : Integer; AValue : boolean); 

begin
  If (FsecondaryAlignment=AValue) then exit;
  FsecondaryAlignment:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRead.SetsupplementaryAlignment(AIndex : Integer; AValue : boolean); 

begin
  If (FsupplementaryAlignment=AValue) then exit;
  FsupplementaryAlignment:=AValue;
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
  TReadGroupTypeinfo
  --------------------------------------------------------------------}


Class Function TReadGroupTypeinfo.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TReadGroup
  --------------------------------------------------------------------}


Procedure TReadGroup.SetdatasetId(AIndex : Integer; AValue : String); 

begin
  If (FdatasetId=AValue) then exit;
  FdatasetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadGroup.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadGroup.Setexperiment(AIndex : Integer; AValue : TReadGroupExperiment); 

begin
  If (Fexperiment=AValue) then exit;
  Fexperiment:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadGroup.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadGroup.Setinfo(AIndex : Integer; AValue : TReadGroupTypeinfo); 

begin
  If (Finfo=AValue) then exit;
  Finfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadGroup.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadGroup.SetpredictedInsertSize(AIndex : Integer; AValue : integer); 

begin
  If (FpredictedInsertSize=AValue) then exit;
  FpredictedInsertSize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadGroup.Setprograms(AIndex : Integer; AValue : TReadGroupTypeprogramsArray); 

begin
  If (Fprograms=AValue) then exit;
  Fprograms:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadGroup.SetreferenceSetId(AIndex : Integer; AValue : String); 

begin
  If (FreferenceSetId=AValue) then exit;
  FreferenceSetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadGroup.SetsampleId(AIndex : Integer; AValue : String); 

begin
  If (FsampleId=AValue) then exit;
  FsampleId:=AValue;
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
  TReadGroupExperiment
  --------------------------------------------------------------------}


Procedure TReadGroupExperiment.SetinstrumentModel(AIndex : Integer; AValue : String); 

begin
  If (FinstrumentModel=AValue) then exit;
  FinstrumentModel:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadGroupExperiment.SetlibraryId(AIndex : Integer; AValue : String); 

begin
  If (FlibraryId=AValue) then exit;
  FlibraryId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadGroupExperiment.SetplatformUnit(AIndex : Integer; AValue : String); 

begin
  If (FplatformUnit=AValue) then exit;
  FplatformUnit:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadGroupExperiment.SetsequencingCenter(AIndex : Integer; AValue : String); 

begin
  If (FsequencingCenter=AValue) then exit;
  FsequencingCenter:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReadGroupProgram
  --------------------------------------------------------------------}


Procedure TReadGroupProgram.SetcommandLine(AIndex : Integer; AValue : String); 

begin
  If (FcommandLine=AValue) then exit;
  FcommandLine:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadGroupProgram.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadGroupProgram.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadGroupProgram.SetprevProgramId(AIndex : Integer; AValue : String); 

begin
  If (FprevProgramId=AValue) then exit;
  FprevProgramId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadGroupProgram.Setversion(AIndex : Integer; AValue : String); 

begin
  If (Fversion=AValue) then exit;
  Fversion:=AValue;
  MarkPropertyChanged(AIndex);
end;





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


Procedure TReadGroupSet.SetdatasetId(AIndex : Integer; AValue : String); 

begin
  If (FdatasetId=AValue) then exit;
  FdatasetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadGroupSet.Setfilename(AIndex : Integer; AValue : String); 

begin
  If (Ffilename=AValue) then exit;
  Ffilename:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadGroupSet.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadGroupSet.Setinfo(AIndex : Integer; AValue : TReadGroupSetTypeinfo); 

begin
  If (Finfo=AValue) then exit;
  Finfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadGroupSet.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadGroupSet.SetreadGroups(AIndex : Integer; AValue : TReadGroupSetTypereadGroupsArray); 

begin
  If (FreadGroups=AValue) then exit;
  FreadGroups:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadGroupSet.SetreferenceSetId(AIndex : Integer; AValue : String); 

begin
  If (FreferenceSetId=AValue) then exit;
  FreferenceSetId:=AValue;
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
  TReference
  --------------------------------------------------------------------}


Procedure TReference.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReference.Set_length(AIndex : Integer; AValue : String); 

begin
  If (F_length=AValue) then exit;
  F_length:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReference.Setmd5checksum(AIndex : Integer; AValue : String); 

begin
  If (Fmd5checksum=AValue) then exit;
  Fmd5checksum:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReference.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReference.SetncbiTaxonId(AIndex : Integer; AValue : integer); 

begin
  If (FncbiTaxonId=AValue) then exit;
  FncbiTaxonId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReference.SetsourceAccessions(AIndex : Integer; AValue : TStringArray); 

begin
  If (FsourceAccessions=AValue) then exit;
  FsourceAccessions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReference.SetsourceURI(AIndex : Integer; AValue : String); 

begin
  If (FsourceURI=AValue) then exit;
  FsourceURI:=AValue;
  MarkPropertyChanged(AIndex);
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
  TReferenceBound
  --------------------------------------------------------------------}


Procedure TReferenceBound.SetreferenceName(AIndex : Integer; AValue : String); 

begin
  If (FreferenceName=AValue) then exit;
  FreferenceName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReferenceBound.SetupperBound(AIndex : Integer; AValue : String); 

begin
  If (FupperBound=AValue) then exit;
  FupperBound:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReferenceSet
  --------------------------------------------------------------------}


Procedure TReferenceSet.SetassemblyId(AIndex : Integer; AValue : String); 

begin
  If (FassemblyId=AValue) then exit;
  FassemblyId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReferenceSet.Setdescription(AIndex : Integer; AValue : String); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReferenceSet.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReferenceSet.Setmd5checksum(AIndex : Integer; AValue : String); 

begin
  If (Fmd5checksum=AValue) then exit;
  Fmd5checksum:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReferenceSet.SetncbiTaxonId(AIndex : Integer; AValue : integer); 

begin
  If (FncbiTaxonId=AValue) then exit;
  FncbiTaxonId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReferenceSet.SetreferenceIds(AIndex : Integer; AValue : TStringArray); 

begin
  If (FreferenceIds=AValue) then exit;
  FreferenceIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReferenceSet.SetsourceAccessions(AIndex : Integer; AValue : TStringArray); 

begin
  If (FsourceAccessions=AValue) then exit;
  FsourceAccessions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReferenceSet.SetsourceURI(AIndex : Integer; AValue : String); 

begin
  If (FsourceURI=AValue) then exit;
  FsourceURI:=AValue;
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
  TSearchAnnotationSetsRequest
  --------------------------------------------------------------------}


Procedure TSearchAnnotationSetsRequest.SetdatasetIds(AIndex : Integer; AValue : TStringArray); 

begin
  If (FdatasetIds=AValue) then exit;
  FdatasetIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchAnnotationSetsRequest.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchAnnotationSetsRequest.SetpageSize(AIndex : Integer; AValue : integer); 

begin
  If (FpageSize=AValue) then exit;
  FpageSize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchAnnotationSetsRequest.SetpageToken(AIndex : Integer; AValue : String); 

begin
  If (FpageToken=AValue) then exit;
  FpageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchAnnotationSetsRequest.SetreferenceSetId(AIndex : Integer; AValue : String); 

begin
  If (FreferenceSetId=AValue) then exit;
  FreferenceSetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchAnnotationSetsRequest.Settypes(AIndex : Integer; AValue : TStringArray); 

begin
  If (Ftypes=AValue) then exit;
  Ftypes:=AValue;
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


Procedure TSearchAnnotationSetsResponse.SetannotationSets(AIndex : Integer; AValue : TSearchAnnotationSetsResponseTypeannotationSetsArray); 

begin
  If (FannotationSets=AValue) then exit;
  FannotationSets:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchAnnotationSetsResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

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
  TSearchAnnotationsRequest
  --------------------------------------------------------------------}


Procedure TSearchAnnotationsRequest.SetannotationSetIds(AIndex : Integer; AValue : TStringArray); 

begin
  If (FannotationSetIds=AValue) then exit;
  FannotationSetIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchAnnotationsRequest.SetpageSize(AIndex : Integer; AValue : integer); 

begin
  If (FpageSize=AValue) then exit;
  FpageSize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchAnnotationsRequest.SetpageToken(AIndex : Integer; AValue : String); 

begin
  If (FpageToken=AValue) then exit;
  FpageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchAnnotationsRequest.Setrange(AIndex : Integer; AValue : TQueryRange); 

begin
  If (Frange=AValue) then exit;
  Frange:=AValue;
  MarkPropertyChanged(AIndex);
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


Procedure TSearchAnnotationsResponse.Setannotations(AIndex : Integer; AValue : TSearchAnnotationsResponseTypeannotationsArray); 

begin
  If (Fannotations=AValue) then exit;
  Fannotations:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchAnnotationsResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

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
  TSearchCallSetsRequest
  --------------------------------------------------------------------}


Procedure TSearchCallSetsRequest.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchCallSetsRequest.SetpageSize(AIndex : Integer; AValue : integer); 

begin
  If (FpageSize=AValue) then exit;
  FpageSize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchCallSetsRequest.SetpageToken(AIndex : Integer; AValue : String); 

begin
  If (FpageToken=AValue) then exit;
  FpageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchCallSetsRequest.SetvariantSetIds(AIndex : Integer; AValue : TStringArray); 

begin
  If (FvariantSetIds=AValue) then exit;
  FvariantSetIds:=AValue;
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


Procedure TSearchCallSetsResponse.SetcallSets(AIndex : Integer; AValue : TSearchCallSetsResponseTypecallSetsArray); 

begin
  If (FcallSets=AValue) then exit;
  FcallSets:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchCallSetsResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

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
  TSearchJobsRequest
  --------------------------------------------------------------------}


Procedure TSearchJobsRequest.SetcreatedAfter(AIndex : Integer; AValue : String); 

begin
  If (FcreatedAfter=AValue) then exit;
  FcreatedAfter:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchJobsRequest.SetcreatedBefore(AIndex : Integer; AValue : String); 

begin
  If (FcreatedBefore=AValue) then exit;
  FcreatedBefore:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchJobsRequest.SetpageSize(AIndex : Integer; AValue : integer); 

begin
  If (FpageSize=AValue) then exit;
  FpageSize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchJobsRequest.SetpageToken(AIndex : Integer; AValue : String); 

begin
  If (FpageToken=AValue) then exit;
  FpageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchJobsRequest.SetprojectNumber(AIndex : Integer; AValue : String); 

begin
  If (FprojectNumber=AValue) then exit;
  FprojectNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchJobsRequest.Setstatus(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSearchJobsRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'status' : SetLength(Fstatus,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSearchJobsResponse
  --------------------------------------------------------------------}


Procedure TSearchJobsResponse.Setjobs(AIndex : Integer; AValue : TSearchJobsResponseTypejobsArray); 

begin
  If (Fjobs=AValue) then exit;
  Fjobs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchJobsResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSearchJobsResponse.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'jobs' : SetLength(Fjobs,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSearchReadGroupSetsRequest
  --------------------------------------------------------------------}


Procedure TSearchReadGroupSetsRequest.SetdatasetIds(AIndex : Integer; AValue : TStringArray); 

begin
  If (FdatasetIds=AValue) then exit;
  FdatasetIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchReadGroupSetsRequest.Setname(AIndex : Integer; AValue : String); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchReadGroupSetsRequest.SetpageSize(AIndex : Integer; AValue : integer); 

begin
  If (FpageSize=AValue) then exit;
  FpageSize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchReadGroupSetsRequest.SetpageToken(AIndex : Integer; AValue : String); 

begin
  If (FpageToken=AValue) then exit;
  FpageToken:=AValue;
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


Procedure TSearchReadGroupSetsResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchReadGroupSetsResponse.SetreadGroupSets(AIndex : Integer; AValue : TSearchReadGroupSetsResponseTypereadGroupSetsArray); 

begin
  If (FreadGroupSets=AValue) then exit;
  FreadGroupSets:=AValue;
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
  TSearchReadsRequest
  --------------------------------------------------------------------}


Procedure TSearchReadsRequest.Set_end(AIndex : Integer; AValue : String); 

begin
  If (F_end=AValue) then exit;
  F_end:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchReadsRequest.SetpageSize(AIndex : Integer; AValue : integer); 

begin
  If (FpageSize=AValue) then exit;
  FpageSize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchReadsRequest.SetpageToken(AIndex : Integer; AValue : String); 

begin
  If (FpageToken=AValue) then exit;
  FpageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchReadsRequest.SetreadGroupIds(AIndex : Integer; AValue : TStringArray); 

begin
  If (FreadGroupIds=AValue) then exit;
  FreadGroupIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchReadsRequest.SetreadGroupSetIds(AIndex : Integer; AValue : TStringArray); 

begin
  If (FreadGroupSetIds=AValue) then exit;
  FreadGroupSetIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchReadsRequest.SetreferenceName(AIndex : Integer; AValue : String); 

begin
  If (FreferenceName=AValue) then exit;
  FreferenceName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchReadsRequest.Setstart(AIndex : Integer; AValue : String); 

begin
  If (Fstart=AValue) then exit;
  Fstart:=AValue;
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
  'readgroupids' : SetLength(FreadGroupIds,ALength);
  'readgroupsetids' : SetLength(FreadGroupSetIds,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSearchReadsResponse
  --------------------------------------------------------------------}


Procedure TSearchReadsResponse.Setalignments(AIndex : Integer; AValue : TSearchReadsResponseTypealignmentsArray); 

begin
  If (Falignments=AValue) then exit;
  Falignments:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchReadsResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

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
  TSearchReferenceSetsRequest
  --------------------------------------------------------------------}


Procedure TSearchReferenceSetsRequest.Setaccessions(AIndex : Integer; AValue : TStringArray); 

begin
  If (Faccessions=AValue) then exit;
  Faccessions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchReferenceSetsRequest.SetassemblyId(AIndex : Integer; AValue : String); 

begin
  If (FassemblyId=AValue) then exit;
  FassemblyId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchReferenceSetsRequest.Setmd5checksums(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fmd5checksums=AValue) then exit;
  Fmd5checksums:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchReferenceSetsRequest.SetpageSize(AIndex : Integer; AValue : integer); 

begin
  If (FpageSize=AValue) then exit;
  FpageSize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchReferenceSetsRequest.SetpageToken(AIndex : Integer; AValue : String); 

begin
  If (FpageToken=AValue) then exit;
  FpageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSearchReferenceSetsRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'accessions' : SetLength(Faccessions,ALength);
  'md5checksums' : SetLength(Fmd5checksums,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSearchReferenceSetsResponse
  --------------------------------------------------------------------}


Procedure TSearchReferenceSetsResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchReferenceSetsResponse.SetreferenceSets(AIndex : Integer; AValue : TSearchReferenceSetsResponseTypereferenceSetsArray); 

begin
  If (FreferenceSets=AValue) then exit;
  FreferenceSets:=AValue;
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
  TSearchReferencesRequest
  --------------------------------------------------------------------}


Procedure TSearchReferencesRequest.Setaccessions(AIndex : Integer; AValue : TStringArray); 

begin
  If (Faccessions=AValue) then exit;
  Faccessions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchReferencesRequest.Setmd5checksums(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fmd5checksums=AValue) then exit;
  Fmd5checksums:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchReferencesRequest.SetpageSize(AIndex : Integer; AValue : integer); 

begin
  If (FpageSize=AValue) then exit;
  FpageSize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchReferencesRequest.SetpageToken(AIndex : Integer; AValue : String); 

begin
  If (FpageToken=AValue) then exit;
  FpageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchReferencesRequest.SetreferenceSetId(AIndex : Integer; AValue : String); 

begin
  If (FreferenceSetId=AValue) then exit;
  FreferenceSetId:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TSearchReferencesRequest.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'accessions' : SetLength(Faccessions,ALength);
  'md5checksums' : SetLength(Fmd5checksums,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSearchReferencesResponse
  --------------------------------------------------------------------}


Procedure TSearchReferencesResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchReferencesResponse.Setreferences(AIndex : Integer; AValue : TSearchReferencesResponseTypereferencesArray); 

begin
  If (Freferences=AValue) then exit;
  Freferences:=AValue;
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
  TSearchVariantSetsRequest
  --------------------------------------------------------------------}


Procedure TSearchVariantSetsRequest.SetdatasetIds(AIndex : Integer; AValue : TStringArray); 

begin
  If (FdatasetIds=AValue) then exit;
  FdatasetIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchVariantSetsRequest.SetpageSize(AIndex : Integer; AValue : integer); 

begin
  If (FpageSize=AValue) then exit;
  FpageSize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchVariantSetsRequest.SetpageToken(AIndex : Integer; AValue : String); 

begin
  If (FpageToken=AValue) then exit;
  FpageToken:=AValue;
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


Procedure TSearchVariantSetsResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchVariantSetsResponse.SetvariantSets(AIndex : Integer; AValue : TSearchVariantSetsResponseTypevariantSetsArray); 

begin
  If (FvariantSets=AValue) then exit;
  FvariantSets:=AValue;
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


Procedure TSearchVariantsRequest.SetcallSetIds(AIndex : Integer; AValue : TStringArray); 

begin
  If (FcallSetIds=AValue) then exit;
  FcallSetIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchVariantsRequest.Set_end(AIndex : Integer; AValue : String); 

begin
  If (F_end=AValue) then exit;
  F_end:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchVariantsRequest.SetmaxCalls(AIndex : Integer; AValue : integer); 

begin
  If (FmaxCalls=AValue) then exit;
  FmaxCalls:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchVariantsRequest.SetpageSize(AIndex : Integer; AValue : integer); 

begin
  If (FpageSize=AValue) then exit;
  FpageSize:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchVariantsRequest.SetpageToken(AIndex : Integer; AValue : String); 

begin
  If (FpageToken=AValue) then exit;
  FpageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchVariantsRequest.SetreferenceName(AIndex : Integer; AValue : String); 

begin
  If (FreferenceName=AValue) then exit;
  FreferenceName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchVariantsRequest.Setstart(AIndex : Integer; AValue : String); 

begin
  If (Fstart=AValue) then exit;
  Fstart:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchVariantsRequest.SetvariantName(AIndex : Integer; AValue : String); 

begin
  If (FvariantName=AValue) then exit;
  FvariantName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchVariantsRequest.SetvariantSetIds(AIndex : Integer; AValue : TStringArray); 

begin
  If (FvariantSetIds=AValue) then exit;
  FvariantSetIds:=AValue;
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
  'callsetids' : SetLength(FcallSetIds,ALength);
  'variantsetids' : SetLength(FvariantSetIds,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TSearchVariantsResponse
  --------------------------------------------------------------------}


Procedure TSearchVariantsResponse.SetnextPageToken(AIndex : Integer; AValue : String); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchVariantsResponse.Setvariants(AIndex : Integer; AValue : TSearchVariantsResponseTypevariantsArray); 

begin
  If (Fvariants=AValue) then exit;
  Fvariants:=AValue;
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
  TTranscript
  --------------------------------------------------------------------}


Procedure TTranscript.SetcodingSequence(AIndex : Integer; AValue : TTranscriptCodingSequence); 

begin
  If (FcodingSequence=AValue) then exit;
  FcodingSequence:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTranscript.Setexons(AIndex : Integer; AValue : TTranscriptTypeexonsArray); 

begin
  If (Fexons=AValue) then exit;
  Fexons:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTranscript.SetgeneId(AIndex : Integer; AValue : String); 

begin
  If (FgeneId=AValue) then exit;
  FgeneId:=AValue;
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
  TTranscriptCodingSequence
  --------------------------------------------------------------------}


Procedure TTranscriptCodingSequence.Set_end(AIndex : Integer; AValue : String); 

begin
  If (F_end=AValue) then exit;
  F_end:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTranscriptCodingSequence.Setstart(AIndex : Integer; AValue : String); 

begin
  If (Fstart=AValue) then exit;
  Fstart:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TTranscriptCodingSequence.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_end' : Result:='end';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




{ --------------------------------------------------------------------
  TTranscriptExon
  --------------------------------------------------------------------}


Procedure TTranscriptExon.Set_end(AIndex : Integer; AValue : String); 

begin
  If (F_end=AValue) then exit;
  F_end:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTranscriptExon.Setframe(AIndex : Integer; AValue : TInt32Value); 

begin
  If (Fframe=AValue) then exit;
  Fframe:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTranscriptExon.Setstart(AIndex : Integer; AValue : String); 

begin
  If (Fstart=AValue) then exit;
  Fstart:=AValue;
  MarkPropertyChanged(AIndex);
end;



Class Function TTranscriptExon.ExportPropertyName(Const AName : String) :String;

begin
  Case AName of
  '_end' : Result:='end';
  else
    Result:=Inherited ExportPropertyName(AName);
  end;
end;




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


Procedure TVariant.SetalternateBases(AIndex : Integer; AValue : TStringArray); 

begin
  If (FalternateBases=AValue) then exit;
  FalternateBases:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariant.Setcalls(AIndex : Integer; AValue : TVariantTypecallsArray); 

begin
  If (Fcalls=AValue) then exit;
  Fcalls:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariant.Setcreated(AIndex : Integer; AValue : String); 

begin
  If (Fcreated=AValue) then exit;
  Fcreated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariant.Set_end(AIndex : Integer; AValue : String); 

begin
  If (F_end=AValue) then exit;
  F_end:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariant.Setfilter(AIndex : Integer; AValue : TStringArray); 

begin
  If (Ffilter=AValue) then exit;
  Ffilter:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariant.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariant.Setinfo(AIndex : Integer; AValue : TVariantTypeinfo); 

begin
  If (Finfo=AValue) then exit;
  Finfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariant.Setnames(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fnames=AValue) then exit;
  Fnames:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariant.Setquality(AIndex : Integer; AValue : double); 

begin
  If (Fquality=AValue) then exit;
  Fquality:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariant.SetreferenceBases(AIndex : Integer; AValue : String); 

begin
  If (FreferenceBases=AValue) then exit;
  FreferenceBases:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariant.SetreferenceName(AIndex : Integer; AValue : String); 

begin
  If (FreferenceName=AValue) then exit;
  FreferenceName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariant.Setstart(AIndex : Integer; AValue : String); 

begin
  If (Fstart=AValue) then exit;
  Fstart:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariant.SetvariantSetId(AIndex : Integer; AValue : String); 

begin
  If (FvariantSetId=AValue) then exit;
  FvariantSetId:=AValue;
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
  'alternatebases' : SetLength(FalternateBases,ALength);
  'calls' : SetLength(Fcalls,ALength);
  'filter' : SetLength(Ffilter,ALength);
  'names' : SetLength(Fnames,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TVariantAnnotation
  --------------------------------------------------------------------}


Procedure TVariantAnnotation.SetalternateBases(AIndex : Integer; AValue : String); 

begin
  If (FalternateBases=AValue) then exit;
  FalternateBases:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariantAnnotation.SetclinicalSignificance(AIndex : Integer; AValue : String); 

begin
  If (FclinicalSignificance=AValue) then exit;
  FclinicalSignificance:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariantAnnotation.Setconditions(AIndex : Integer; AValue : TVariantAnnotationTypeconditionsArray); 

begin
  If (Fconditions=AValue) then exit;
  Fconditions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariantAnnotation.Seteffect(AIndex : Integer; AValue : String); 

begin
  If (Feffect=AValue) then exit;
  Feffect:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariantAnnotation.SetgeneId(AIndex : Integer; AValue : String); 

begin
  If (FgeneId=AValue) then exit;
  FgeneId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariantAnnotation.SettranscriptIds(AIndex : Integer; AValue : TStringArray); 

begin
  If (FtranscriptIds=AValue) then exit;
  FtranscriptIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariantAnnotation.Set_type(AIndex : Integer; AValue : String); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
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
  'conditions' : SetLength(Fconditions,ALength);
  'transcriptids' : SetLength(FtranscriptIds,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TVariantAnnotationCondition
  --------------------------------------------------------------------}


Procedure TVariantAnnotationCondition.SetconceptId(AIndex : Integer; AValue : String); 

begin
  If (FconceptId=AValue) then exit;
  FconceptId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariantAnnotationCondition.SetexternalIds(AIndex : Integer; AValue : TVariantAnnotationConditionTypeexternalIdsArray); 

begin
  If (FexternalIds=AValue) then exit;
  FexternalIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariantAnnotationCondition.Setnames(AIndex : Integer; AValue : TStringArray); 

begin
  If (Fnames=AValue) then exit;
  Fnames:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariantAnnotationCondition.SetomimId(AIndex : Integer; AValue : String); 

begin
  If (FomimId=AValue) then exit;
  FomimId:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TVariantAnnotationCondition.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'externalids' : SetLength(FexternalIds,ALength);
  'names' : SetLength(Fnames,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TVariantSet
  --------------------------------------------------------------------}


Procedure TVariantSet.SetdatasetId(AIndex : Integer; AValue : String); 

begin
  If (FdatasetId=AValue) then exit;
  FdatasetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariantSet.Setid(AIndex : Integer; AValue : String); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariantSet.Setmetadata(AIndex : Integer; AValue : TVariantSetTypemetadataArray); 

begin
  If (Fmetadata=AValue) then exit;
  Fmetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariantSet.SetreferenceBounds(AIndex : Integer; AValue : TVariantSetTypereferenceBoundsArray); 

begin
  If (FreferenceBounds=AValue) then exit;
  FreferenceBounds:=AValue;
  MarkPropertyChanged(AIndex);
end;


//2.6.4. bug workaround
{$IFDEF VER2_6}
Procedure TVariantSet.SetArrayLength(Const AName : String; ALength : Longint); 

begin
  Case AName of
  'metadata' : SetLength(Fmetadata,ALength);
  'referencebounds' : SetLength(FreferenceBounds,ALength);
  else
    Inherited SetArrayLength(AName,ALength);
  end;
end;
{$ENDIF VER2_6}




{ --------------------------------------------------------------------
  TAnnotationSetsResource
  --------------------------------------------------------------------}


Class Function TAnnotationSetsResource.ResourceName : String;

begin
  Result:='annotationSets';
end;

Class Function TAnnotationSetsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TgenomicsAPI;
end;

Function TAnnotationSetsResource.Create(aAnnotationSet : TAnnotationSet) : TAnnotationSet;

Const
  _HTTPMethod = 'POST';
  _Path       = 'annotationSets';
  _Methodid   = 'genomics.annotationSets.create';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aAnnotationSet,TAnnotationSet) as TAnnotationSet;
end;

Procedure TAnnotationSetsResource.Delete(annotationSetId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'annotationSets/{annotationSetId}';
  _Methodid   = 'genomics.annotationSets.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['annotationSetId',annotationSetId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TAnnotationSetsResource.Get(annotationSetId: string) : TAnnotationSet;

Const
  _HTTPMethod = 'GET';
  _Path       = 'annotationSets/{annotationSetId}';
  _Methodid   = 'genomics.annotationSets.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['annotationSetId',annotationSetId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TAnnotationSet) as TAnnotationSet;
end;

Function TAnnotationSetsResource.Patch(annotationSetId: string; aAnnotationSet : TAnnotationSet) : TAnnotationSet;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'annotationSets/{annotationSetId}';
  _Methodid   = 'genomics.annotationSets.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['annotationSetId',annotationSetId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aAnnotationSet,TAnnotationSet) as TAnnotationSet;
end;

Function TAnnotationSetsResource.Search(aSearchAnnotationSetsRequest : TSearchAnnotationSetsRequest) : TSearchAnnotationSetsResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'annotationSets/search';
  _Methodid   = 'genomics.annotationSets.search';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aSearchAnnotationSetsRequest,TSearchAnnotationSetsResponse) as TSearchAnnotationSetsResponse;
end;

Function TAnnotationSetsResource.Update(annotationSetId: string; aAnnotationSet : TAnnotationSet) : TAnnotationSet;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'annotationSets/{annotationSetId}';
  _Methodid   = 'genomics.annotationSets.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['annotationSetId',annotationSetId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aAnnotationSet,TAnnotationSet) as TAnnotationSet;
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

Function TAnnotationsResource.BatchCreate(aBatchCreateAnnotationsRequest : TBatchCreateAnnotationsRequest) : TBatchAnnotationsResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'annotations:batchCreate';
  _Methodid   = 'genomics.annotations.batchCreate';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aBatchCreateAnnotationsRequest,TBatchAnnotationsResponse) as TBatchAnnotationsResponse;
end;

Function TAnnotationsResource.Create(aAnnotation : TAnnotation) : TAnnotation;

Const
  _HTTPMethod = 'POST';
  _Path       = 'annotations';
  _Methodid   = 'genomics.annotations.create';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aAnnotation,TAnnotation) as TAnnotation;
end;

Procedure TAnnotationsResource.Delete(annotationId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'annotations/{annotationId}';
  _Methodid   = 'genomics.annotations.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['annotationId',annotationId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TAnnotationsResource.Get(annotationId: string) : TAnnotation;

Const
  _HTTPMethod = 'GET';
  _Path       = 'annotations/{annotationId}';
  _Methodid   = 'genomics.annotations.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['annotationId',annotationId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TAnnotation) as TAnnotation;
end;

Function TAnnotationsResource.Patch(annotationId: string; aAnnotation : TAnnotation) : TAnnotation;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'annotations/{annotationId}';
  _Methodid   = 'genomics.annotations.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['annotationId',annotationId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aAnnotation,TAnnotation) as TAnnotation;
end;

Function TAnnotationsResource.Search(aSearchAnnotationsRequest : TSearchAnnotationsRequest) : TSearchAnnotationsResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'annotations/search';
  _Methodid   = 'genomics.annotations.search';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aSearchAnnotationsRequest,TSearchAnnotationsResponse) as TSearchAnnotationsResponse;
end;

Function TAnnotationsResource.Update(annotationId: string; aAnnotation : TAnnotation) : TAnnotation;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'annotations/{annotationId}';
  _Methodid   = 'genomics.annotations.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['annotationId',annotationId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aAnnotation,TAnnotation) as TAnnotation;
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

Function TCallsetsResource.Create(aCallSet : TCallSet) : TCallSet;

Const
  _HTTPMethod = 'POST';
  _Path       = 'callsets';
  _Methodid   = 'genomics.callsets.create';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aCallSet,TCallSet) as TCallSet;
end;

Procedure TCallsetsResource.Delete(callSetId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'callsets/{callSetId}';
  _Methodid   = 'genomics.callsets.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['callSetId',callSetId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TCallsetsResource.Get(callSetId: string) : TCallSet;

Const
  _HTTPMethod = 'GET';
  _Path       = 'callsets/{callSetId}';
  _Methodid   = 'genomics.callsets.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['callSetId',callSetId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TCallSet) as TCallSet;
end;

Function TCallsetsResource.Patch(callSetId: string; aCallSet : TCallSet) : TCallSet;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'callsets/{callSetId}';
  _Methodid   = 'genomics.callsets.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['callSetId',callSetId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aCallSet,TCallSet) as TCallSet;
end;

Function TCallsetsResource.Search(aSearchCallSetsRequest : TSearchCallSetsRequest) : TSearchCallSetsResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'callsets/search';
  _Methodid   = 'genomics.callsets.search';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aSearchCallSetsRequest,TSearchCallSetsResponse) as TSearchCallSetsResponse;
end;

Function TCallsetsResource.Update(callSetId: string; aCallSet : TCallSet) : TCallSet;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'callsets/{callSetId}';
  _Methodid   = 'genomics.callsets.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['callSetId',callSetId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aCallSet,TCallSet) as TCallSet;
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

Function TDatasetsResource.Create(aDataset : TDataset) : TDataset;

Const
  _HTTPMethod = 'POST';
  _Path       = 'datasets';
  _Methodid   = 'genomics.datasets.create';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aDataset,TDataset) as TDataset;
end;

Procedure TDatasetsResource.Delete(datasetId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'datasets/{datasetId}';
  _Methodid   = 'genomics.datasets.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['datasetId',datasetId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TDatasetsResource.Get(datasetId: string) : TDataset;

Const
  _HTTPMethod = 'GET';
  _Path       = 'datasets/{datasetId}';
  _Methodid   = 'genomics.datasets.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['datasetId',datasetId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TDataset) as TDataset;
end;

Function TDatasetsResource.List(AQuery : string = '') : TListDatasetsResponse;

Const
  _HTTPMethod = 'GET';
  _Path       = 'datasets';
  _Methodid   = 'genomics.datasets.list';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,AQuery,Nil,TListDatasetsResponse) as TListDatasetsResponse;
end;


Function TDatasetsResource.List(AQuery : TDatasetslistOptions) : TListDatasetsResponse;

Var
  _Q : String;

begin
  _Q:='';
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'projectNumber',AQuery.projectNumber);
  Result:=List(_Q);
end;

Function TDatasetsResource.Patch(datasetId: string; aDataset : TDataset) : TDataset;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'datasets/{datasetId}';
  _Methodid   = 'genomics.datasets.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['datasetId',datasetId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aDataset,TDataset) as TDataset;
end;

Function TDatasetsResource.Undelete(datasetId: string) : TDataset;

Const
  _HTTPMethod = 'POST';
  _Path       = 'datasets/{datasetId}/undelete';
  _Methodid   = 'genomics.datasets.undelete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['datasetId',datasetId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TDataset) as TDataset;
end;

Function TDatasetsResource.Update(datasetId: string; aDataset : TDataset) : TDataset;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'datasets/{datasetId}';
  _Methodid   = 'genomics.datasets.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['datasetId',datasetId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aDataset,TDataset) as TDataset;
end;



{ --------------------------------------------------------------------
  TExperimentalJobsResource
  --------------------------------------------------------------------}


Class Function TExperimentalJobsResource.ResourceName : String;

begin
  Result:='jobs';
end;

Class Function TExperimentalJobsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TgenomicsAPI;
end;

Function TExperimentalJobsResource.Create(aExperimentalCreateJobRequest : TExperimentalCreateJobRequest) : TExperimentalCreateJobResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'experimental/jobs/create';
  _Methodid   = 'genomics.experimental.jobs.create';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aExperimentalCreateJobRequest,TExperimentalCreateJobResponse) as TExperimentalCreateJobResponse;
end;



{ --------------------------------------------------------------------
  TExperimentalResource
  --------------------------------------------------------------------}


Class Function TExperimentalResource.ResourceName : String;

begin
  Result:='experimental';
end;

Class Function TExperimentalResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TgenomicsAPI;
end;



Function TExperimentalResource.GetJobsInstance : TExperimentalJobsResource;

begin
  if (FJobsInstance=Nil) then
    FJobsInstance:=CreateJobsResource;
  Result:=FJobsInstance;
end;

Function TExperimentalResource.CreateJobsResource : TExperimentalJobsResource;

begin
  Result:=CreateJobsResource(Self);
end;


Function TExperimentalResource.CreateJobsResource(AOwner : TComponent) : TExperimentalJobsResource;

begin
  Result:=TExperimentalJobsResource.Create(AOwner);
  Result.API:=Self.API;
end;



{ --------------------------------------------------------------------
  TJobsResource
  --------------------------------------------------------------------}


Class Function TJobsResource.ResourceName : String;

begin
  Result:='jobs';
end;

Class Function TJobsResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TgenomicsAPI;
end;

Procedure TJobsResource.Cancel(jobId: string);

Const
  _HTTPMethod = 'POST';
  _Path       = 'jobs/{jobId}/cancel';
  _Methodid   = 'genomics.jobs.cancel';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['jobId',jobId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TJobsResource.Get(jobId: string) : TJob;

Const
  _HTTPMethod = 'GET';
  _Path       = 'jobs/{jobId}';
  _Methodid   = 'genomics.jobs.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['jobId',jobId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TJob) as TJob;
end;

Function TJobsResource.Search(aSearchJobsRequest : TSearchJobsRequest) : TSearchJobsResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'jobs/search';
  _Methodid   = 'genomics.jobs.search';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aSearchJobsRequest,TSearchJobsResponse) as TSearchJobsResponse;
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
  _Path       = 'readgroupsets/{readGroupSetId}/coveragebuckets';
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
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'range.end',AQuery.rangeend);
  AddToQuery(_Q,'range.referenceName',AQuery.rangereferenceName);
  AddToQuery(_Q,'range.start',AQuery.rangestart);
  AddToQuery(_Q,'targetBucketWidth',AQuery.targetBucketWidth);
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

Function TReadgroupsetsResource.Align(aAlignReadGroupSetsRequest : TAlignReadGroupSetsRequest) : TAlignReadGroupSetsResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'readgroupsets/align';
  _Methodid   = 'genomics.readgroupsets.align';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aAlignReadGroupSetsRequest,TAlignReadGroupSetsResponse) as TAlignReadGroupSetsResponse;
end;

Function TReadgroupsetsResource.Call(aCallReadGroupSetsRequest : TCallReadGroupSetsRequest) : TCallReadGroupSetsResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'readgroupsets/call';
  _Methodid   = 'genomics.readgroupsets.call';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aCallReadGroupSetsRequest,TCallReadGroupSetsResponse) as TCallReadGroupSetsResponse;
end;

Procedure TReadgroupsetsResource.Delete(readGroupSetId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'readgroupsets/{readGroupSetId}';
  _Methodid   = 'genomics.readgroupsets.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['readGroupSetId',readGroupSetId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TReadgroupsetsResource.Export(aExportReadGroupSetsRequest : TExportReadGroupSetsRequest) : TExportReadGroupSetsResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'readgroupsets/export';
  _Methodid   = 'genomics.readgroupsets.export';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aExportReadGroupSetsRequest,TExportReadGroupSetsResponse) as TExportReadGroupSetsResponse;
end;

Function TReadgroupsetsResource.Get(readGroupSetId: string) : TReadGroupSet;

Const
  _HTTPMethod = 'GET';
  _Path       = 'readgroupsets/{readGroupSetId}';
  _Methodid   = 'genomics.readgroupsets.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['readGroupSetId',readGroupSetId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TReadGroupSet) as TReadGroupSet;
end;

Function TReadgroupsetsResource.Import(aImportReadGroupSetsRequest : TImportReadGroupSetsRequest) : TImportReadGroupSetsResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'readgroupsets/import';
  _Methodid   = 'genomics.readgroupsets.import';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aImportReadGroupSetsRequest,TImportReadGroupSetsResponse) as TImportReadGroupSetsResponse;
end;

Function TReadgroupsetsResource.Patch(readGroupSetId: string; aReadGroupSet : TReadGroupSet) : TReadGroupSet;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'readgroupsets/{readGroupSetId}';
  _Methodid   = 'genomics.readgroupsets.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['readGroupSetId',readGroupSetId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aReadGroupSet,TReadGroupSet) as TReadGroupSet;
end;

Function TReadgroupsetsResource.Search(aSearchReadGroupSetsRequest : TSearchReadGroupSetsRequest) : TSearchReadGroupSetsResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'readgroupsets/search';
  _Methodid   = 'genomics.readgroupsets.search';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aSearchReadGroupSetsRequest,TSearchReadGroupSetsResponse) as TSearchReadGroupSetsResponse;
end;

Function TReadgroupsetsResource.Update(readGroupSetId: string; aReadGroupSet : TReadGroupSet) : TReadGroupSet;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'readgroupsets/{readGroupSetId}';
  _Methodid   = 'genomics.readgroupsets.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['readGroupSetId',readGroupSetId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aReadGroupSet,TReadGroupSet) as TReadGroupSet;
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
  _Path       = 'reads/search';
  _Methodid   = 'genomics.reads.search';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aSearchReadsRequest,TSearchReadsResponse) as TSearchReadsResponse;
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
  _Path       = 'references/{referenceId}/bases';
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
  AddToQuery(_Q,'end',AQuery._end);
  AddToQuery(_Q,'pageSize',AQuery.pageSize);
  AddToQuery(_Q,'pageToken',AQuery.pageToken);
  AddToQuery(_Q,'start',AQuery.start);
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

Function TReferencesResource.Get(referenceId: string) : TReference;

Const
  _HTTPMethod = 'GET';
  _Path       = 'references/{referenceId}';
  _Methodid   = 'genomics.references.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['referenceId',referenceId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TReference) as TReference;
end;

Function TReferencesResource.Search(aSearchReferencesRequest : TSearchReferencesRequest) : TSearchReferencesResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'references/search';
  _Methodid   = 'genomics.references.search';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aSearchReferencesRequest,TSearchReferencesResponse) as TSearchReferencesResponse;
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

Function TReferencesetsResource.Get(referenceSetId: string) : TReferenceSet;

Const
  _HTTPMethod = 'GET';
  _Path       = 'referencesets/{referenceSetId}';
  _Methodid   = 'genomics.referencesets.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['referenceSetId',referenceSetId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TReferenceSet) as TReferenceSet;
end;

Function TReferencesetsResource.Search(aSearchReferenceSetsRequest : TSearchReferenceSetsRequest) : TSearchReferenceSetsResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'referencesets/search';
  _Methodid   = 'genomics.referencesets.search';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aSearchReferenceSetsRequest,TSearchReferenceSetsResponse) as TSearchReferenceSetsResponse;
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

Function TVariantsResource.Create(aVariant : TVariant) : TVariant;

Const
  _HTTPMethod = 'POST';
  _Path       = 'variants';
  _Methodid   = 'genomics.variants.create';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aVariant,TVariant) as TVariant;
end;

Procedure TVariantsResource.Delete(variantId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'variants/{variantId}';
  _Methodid   = 'genomics.variants.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['variantId',variantId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TVariantsResource.Get(variantId: string) : TVariant;

Const
  _HTTPMethod = 'GET';
  _Path       = 'variants/{variantId}';
  _Methodid   = 'genomics.variants.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['variantId',variantId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TVariant) as TVariant;
end;

Function TVariantsResource.Search(aSearchVariantsRequest : TSearchVariantsRequest) : TSearchVariantsResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'variants/search';
  _Methodid   = 'genomics.variants.search';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aSearchVariantsRequest,TSearchVariantsResponse) as TSearchVariantsResponse;
end;

Function TVariantsResource.Update(variantId: string; aVariant : TVariant) : TVariant;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'variants/{variantId}';
  _Methodid   = 'genomics.variants.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['variantId',variantId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aVariant,TVariant) as TVariant;
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

Procedure TVariantsetsResource.Delete(variantSetId: string);

Const
  _HTTPMethod = 'DELETE';
  _Path       = 'variantsets/{variantSetId}';
  _Methodid   = 'genomics.variantsets.delete';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['variantSetId',variantSetId]);
  ServiceCall(_HTTPMethod,_P,'',Nil,Nil);
end;

Function TVariantsetsResource.Export(variantSetId: string; aExportVariantSetRequest : TExportVariantSetRequest) : TExportVariantSetResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'variantsets/{variantSetId}/export';
  _Methodid   = 'genomics.variantsets.export';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['variantSetId',variantSetId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aExportVariantSetRequest,TExportVariantSetResponse) as TExportVariantSetResponse;
end;

Function TVariantsetsResource.Get(variantSetId: string) : TVariantSet;

Const
  _HTTPMethod = 'GET';
  _Path       = 'variantsets/{variantSetId}';
  _Methodid   = 'genomics.variantsets.get';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['variantSetId',variantSetId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',Nil,TVariantSet) as TVariantSet;
end;

Function TVariantsetsResource.ImportVariants(variantSetId: string; aImportVariantsRequest : TImportVariantsRequest) : TImportVariantsResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'variantsets/{variantSetId}/importVariants';
  _Methodid   = 'genomics.variantsets.importVariants';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['variantSetId',variantSetId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aImportVariantsRequest,TImportVariantsResponse) as TImportVariantsResponse;
end;

Procedure TVariantsetsResource.MergeVariants(variantSetId: string; aMergeVariantsRequest : TMergeVariantsRequest);

Const
  _HTTPMethod = 'POST';
  _Path       = 'variantsets/{variantSetId}/mergeVariants';
  _Methodid   = 'genomics.variantsets.mergeVariants';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['variantSetId',variantSetId]);
  ServiceCall(_HTTPMethod,_P,'',aMergeVariantsRequest,Nil);
end;

Function TVariantsetsResource.Patch(variantSetId: string; aVariantSet : TVariantSet) : TVariantSet;

Const
  _HTTPMethod = 'PATCH';
  _Path       = 'variantsets/{variantSetId}';
  _Methodid   = 'genomics.variantsets.patch';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['variantSetId',variantSetId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aVariantSet,TVariantSet) as TVariantSet;
end;

Function TVariantsetsResource.Search(aSearchVariantSetsRequest : TSearchVariantSetsRequest) : TSearchVariantSetsResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'variantsets/search';
  _Methodid   = 'genomics.variantsets.search';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aSearchVariantSetsRequest,TSearchVariantSetsResponse) as TSearchVariantSetsResponse;
end;

Function TVariantsetsResource.Update(variantSetId: string; aVariantSet : TVariantSet) : TVariantSet;

Const
  _HTTPMethod = 'PUT';
  _Path       = 'variantsets/{variantSetId}';
  _Methodid   = 'genomics.variantsets.update';

Var
  _P : String;

begin
  _P:=SubstitutePath(_Path,['variantSetId',variantSetId]);
  Result:=ServiceCall(_HTTPMethod,_P,'',aVariantSet,TVariantSet) as TVariantSet;
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
  Result:='v1beta2';
end;

Class Function TGenomicsAPI.APIRevision : String;

begin
  Result:='20150326';
end;

Class Function TGenomicsAPI.APIID : String;

begin
  Result:='genomics:v1beta2';
end;

Class Function TGenomicsAPI.APITitle : String;

begin
  Result:='Genomics API';
end;

Class Function TGenomicsAPI.APIDescription : String;

begin
  Result:='Provides access to Genomics data.';
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
  Result:='https://developers.google.com/genomics/v1beta2/reference';
end;

Class Function TGenomicsAPI.APIrootUrl : string;

begin
  Result:='https://www.googleapis.com:443/';
end;

Class Function TGenomicsAPI.APIbasePath : string;

begin
  Result:='/genomics/v1beta2/';
end;

Class Function TGenomicsAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com:443/genomics/v1beta2/';
end;

Class Function TGenomicsAPI.APIProtocol : string;

begin
  Result:='rest';
end;

Class Function TGenomicsAPI.APIservicePath : string;

begin
  Result:='genomics/v1beta2/';
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
  TAlignReadGroupSetsRequest.RegisterObject;
  TAlignReadGroupSetsResponse.RegisterObject;
  TAnnotationTypeinfo.RegisterObject;
  TAnnotation.RegisterObject;
  TAnnotationSetTypeinfo.RegisterObject;
  TAnnotationSet.RegisterObject;
  TBatchAnnotationsResponse.RegisterObject;
  TBatchAnnotationsResponseEntry.RegisterObject;
  TBatchAnnotationsResponseEntryStatus.RegisterObject;
  TBatchCreateAnnotationsRequest.RegisterObject;
  TCallTypeinfo.RegisterObject;
  TCall.RegisterObject;
  TCallReadGroupSetsRequest.RegisterObject;
  TCallReadGroupSetsResponse.RegisterObject;
  TCallSetTypeinfo.RegisterObject;
  TCallSet.RegisterObject;
  TCigarUnit.RegisterObject;
  TCoverageBucket.RegisterObject;
  TDataset.RegisterObject;
  TExperimentalCreateJobRequest.RegisterObject;
  TExperimentalCreateJobResponse.RegisterObject;
  TExportReadGroupSetsRequest.RegisterObject;
  TExportReadGroupSetsResponse.RegisterObject;
  TExportVariantSetRequest.RegisterObject;
  TExportVariantSetResponse.RegisterObject;
  TExternalId.RegisterObject;
  TFastqMetadata.RegisterObject;
  TImportReadGroupSetsRequest.RegisterObject;
  TImportReadGroupSetsResponse.RegisterObject;
  TImportVariantsRequest.RegisterObject;
  TImportVariantsResponse.RegisterObject;
  TInt32Value.RegisterObject;
  TInterleavedFastqSource.RegisterObject;
  TJob.RegisterObject;
  TJobRequest.RegisterObject;
  TLinearAlignment.RegisterObject;
  TListBasesResponse.RegisterObject;
  TListCoverageBucketsResponse.RegisterObject;
  TListDatasetsResponse.RegisterObject;
  TMergeVariantsRequest.RegisterObject;
  TMetadataTypeinfo.RegisterObject;
  TMetadata.RegisterObject;
  TPairedFastqSource.RegisterObject;
  TPosition.RegisterObject;
  TQueryRange.RegisterObject;
  TRange.RegisterObject;
  TRangePosition.RegisterObject;
  TReadTypeinfo.RegisterObject;
  TRead.RegisterObject;
  TReadGroupTypeinfo.RegisterObject;
  TReadGroup.RegisterObject;
  TReadGroupExperiment.RegisterObject;
  TReadGroupProgram.RegisterObject;
  TReadGroupSetTypeinfo.RegisterObject;
  TReadGroupSet.RegisterObject;
  TReference.RegisterObject;
  TReferenceBound.RegisterObject;
  TReferenceSet.RegisterObject;
  TSearchAnnotationSetsRequest.RegisterObject;
  TSearchAnnotationSetsResponse.RegisterObject;
  TSearchAnnotationsRequest.RegisterObject;
  TSearchAnnotationsResponse.RegisterObject;
  TSearchCallSetsRequest.RegisterObject;
  TSearchCallSetsResponse.RegisterObject;
  TSearchJobsRequest.RegisterObject;
  TSearchJobsResponse.RegisterObject;
  TSearchReadGroupSetsRequest.RegisterObject;
  TSearchReadGroupSetsResponse.RegisterObject;
  TSearchReadsRequest.RegisterObject;
  TSearchReadsResponse.RegisterObject;
  TSearchReferenceSetsRequest.RegisterObject;
  TSearchReferenceSetsResponse.RegisterObject;
  TSearchReferencesRequest.RegisterObject;
  TSearchReferencesResponse.RegisterObject;
  TSearchVariantSetsRequest.RegisterObject;
  TSearchVariantSetsResponse.RegisterObject;
  TSearchVariantsRequest.RegisterObject;
  TSearchVariantsResponse.RegisterObject;
  TTranscript.RegisterObject;
  TTranscriptCodingSequence.RegisterObject;
  TTranscriptExon.RegisterObject;
  TVariantTypeinfo.RegisterObject;
  TVariant.RegisterObject;
  TVariantAnnotation.RegisterObject;
  TVariantAnnotationCondition.RegisterObject;
  TVariantSet.RegisterObject;
end;


Function TGenomicsAPI.GetAnnotationSetsInstance : TAnnotationSetsResource;

begin
  if (FAnnotationSetsInstance=Nil) then
    FAnnotationSetsInstance:=CreateAnnotationSetsResource;
  Result:=FAnnotationSetsInstance;
end;

Function TGenomicsAPI.CreateAnnotationSetsResource : TAnnotationSetsResource;

begin
  Result:=CreateAnnotationSetsResource(Self);
end;


Function TGenomicsAPI.CreateAnnotationSetsResource(AOwner : TComponent) : TAnnotationSetsResource;

begin
  Result:=TAnnotationSetsResource.Create(AOwner);
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



Function TGenomicsAPI.GetExperimentalJobsInstance : TExperimentalJobsResource;

begin
  if (FExperimentalJobsInstance=Nil) then
    FExperimentalJobsInstance:=CreateExperimentalJobsResource;
  Result:=FExperimentalJobsInstance;
end;

Function TGenomicsAPI.CreateExperimentalJobsResource : TExperimentalJobsResource;

begin
  Result:=CreateExperimentalJobsResource(Self);
end;


Function TGenomicsAPI.CreateExperimentalJobsResource(AOwner : TComponent) : TExperimentalJobsResource;

begin
  Result:=TExperimentalJobsResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TGenomicsAPI.GetExperimentalInstance : TExperimentalResource;

begin
  if (FExperimentalInstance=Nil) then
    FExperimentalInstance:=CreateExperimentalResource;
  Result:=FExperimentalInstance;
end;

Function TGenomicsAPI.CreateExperimentalResource : TExperimentalResource;

begin
  Result:=CreateExperimentalResource(Self);
end;


Function TGenomicsAPI.CreateExperimentalResource(AOwner : TComponent) : TExperimentalResource;

begin
  Result:=TExperimentalResource.Create(AOwner);
  Result.API:=Self.API;
end;



Function TGenomicsAPI.GetJobsInstance : TJobsResource;

begin
  if (FJobsInstance=Nil) then
    FJobsInstance:=CreateJobsResource;
  Result:=FJobsInstance;
end;

Function TGenomicsAPI.CreateJobsResource : TJobsResource;

begin
  Result:=CreateJobsResource(Self);
end;


Function TGenomicsAPI.CreateJobsResource(AOwner : TComponent) : TJobsResource;

begin
  Result:=TJobsResource.Create(AOwner);
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



initialization
  TGenomicsAPI.RegisterAPI;
end.
