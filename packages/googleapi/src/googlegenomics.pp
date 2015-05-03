unit googlegenomics;
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
  TAlignReadGroupSetsRequest = class;
  TAlignReadGroupSetsRequestArray = Array of TAlignReadGroupSetsRequest;
  TAlignReadGroupSetsRequestbamSourceUris = class;
  TAlignReadGroupSetsRequestbamSourceUrisArray = Array of TAlignReadGroupSetsRequestbamSourceUris;
  TAlignReadGroupSetsResponse = class;
  TAlignReadGroupSetsResponseArray = Array of TAlignReadGroupSetsResponse;
  TAnnotation = class;
  TAnnotationArray = Array of TAnnotation;
  TAnnotationinfo = class;
  TAnnotationinfoArray = Array of TAnnotationinfo;
  TAnnotationSet = class;
  TAnnotationSetArray = Array of TAnnotationSet;
  TAnnotationSetinfo = class;
  TAnnotationSetinfoArray = Array of TAnnotationSetinfo;
  TBatchAnnotationsResponse = class;
  TBatchAnnotationsResponseArray = Array of TBatchAnnotationsResponse;
  TBatchAnnotationsResponseentries = class;
  TBatchAnnotationsResponseentriesArray = Array of TBatchAnnotationsResponseentries;
  TBatchAnnotationsResponseEntry = class;
  TBatchAnnotationsResponseEntryArray = Array of TBatchAnnotationsResponseEntry;
  TBatchAnnotationsResponseEntryStatus = class;
  TBatchAnnotationsResponseEntryStatusArray = Array of TBatchAnnotationsResponseEntryStatus;
  TBatchCreateAnnotationsRequest = class;
  TBatchCreateAnnotationsRequestArray = Array of TBatchCreateAnnotationsRequest;
  TBatchCreateAnnotationsRequestannotations = class;
  TBatchCreateAnnotationsRequestannotationsArray = Array of TBatchCreateAnnotationsRequestannotations;
  TCall = class;
  TCallArray = Array of TCall;
  TCallgenotype = class;
  TCallgenotypeArray = Array of TCallgenotype;
  TCallgenotypeLikelihood = class;
  TCallgenotypeLikelihoodArray = Array of TCallgenotypeLikelihood;
  TCallinfo = class;
  TCallinfoArray = Array of TCallinfo;
  TCallReadGroupSetsRequest = class;
  TCallReadGroupSetsRequestArray = Array of TCallReadGroupSetsRequest;
  TCallReadGroupSetsRequestsourceUris = class;
  TCallReadGroupSetsRequestsourceUrisArray = Array of TCallReadGroupSetsRequestsourceUris;
  TCallReadGroupSetsResponse = class;
  TCallReadGroupSetsResponseArray = Array of TCallReadGroupSetsResponse;
  TCallSet = class;
  TCallSetArray = Array of TCallSet;
  TCallSetinfo = class;
  TCallSetinfoArray = Array of TCallSetinfo;
  TCallSetvariantSetIds = class;
  TCallSetvariantSetIdsArray = Array of TCallSetvariantSetIds;
  TCigarUnit = class;
  TCigarUnitArray = Array of TCigarUnit;
  TCoverageBucket = class;
  TCoverageBucketArray = Array of TCoverageBucket;
  TDataset = class;
  TDatasetArray = Array of TDataset;
  TExperimentalCreateJobRequest = class;
  TExperimentalCreateJobRequestArray = Array of TExperimentalCreateJobRequest;
  TExperimentalCreateJobRequestpairedSourceUris = class;
  TExperimentalCreateJobRequestpairedSourceUrisArray = Array of TExperimentalCreateJobRequestpairedSourceUris;
  TExperimentalCreateJobRequestsourceUris = class;
  TExperimentalCreateJobRequestsourceUrisArray = Array of TExperimentalCreateJobRequestsourceUris;
  TExperimentalCreateJobResponse = class;
  TExperimentalCreateJobResponseArray = Array of TExperimentalCreateJobResponse;
  TExportReadGroupSetsRequest = class;
  TExportReadGroupSetsRequestArray = Array of TExportReadGroupSetsRequest;
  TExportReadGroupSetsRequestreadGroupSetIds = class;
  TExportReadGroupSetsRequestreadGroupSetIdsArray = Array of TExportReadGroupSetsRequestreadGroupSetIds;
  TExportReadGroupSetsRequestreferenceNames = class;
  TExportReadGroupSetsRequestreferenceNamesArray = Array of TExportReadGroupSetsRequestreferenceNames;
  TExportReadGroupSetsResponse = class;
  TExportReadGroupSetsResponseArray = Array of TExportReadGroupSetsResponse;
  TExportVariantSetRequest = class;
  TExportVariantSetRequestArray = Array of TExportVariantSetRequest;
  TExportVariantSetRequestcallSetIds = class;
  TExportVariantSetRequestcallSetIdsArray = Array of TExportVariantSetRequestcallSetIds;
  TExportVariantSetResponse = class;
  TExportVariantSetResponseArray = Array of TExportVariantSetResponse;
  TExternalId = class;
  TExternalIdArray = Array of TExternalId;
  TFastqMetadata = class;
  TFastqMetadataArray = Array of TFastqMetadata;
  TImportReadGroupSetsRequest = class;
  TImportReadGroupSetsRequestArray = Array of TImportReadGroupSetsRequest;
  TImportReadGroupSetsRequestsourceUris = class;
  TImportReadGroupSetsRequestsourceUrisArray = Array of TImportReadGroupSetsRequestsourceUris;
  TImportReadGroupSetsResponse = class;
  TImportReadGroupSetsResponseArray = Array of TImportReadGroupSetsResponse;
  TImportVariantsRequest = class;
  TImportVariantsRequestArray = Array of TImportVariantsRequest;
  TImportVariantsRequestsourceUris = class;
  TImportVariantsRequestsourceUrisArray = Array of TImportVariantsRequestsourceUris;
  TImportVariantsResponse = class;
  TImportVariantsResponseArray = Array of TImportVariantsResponse;
  TInt32Value = class;
  TInt32ValueArray = Array of TInt32Value;
  TInterleavedFastqSource = class;
  TInterleavedFastqSourceArray = Array of TInterleavedFastqSource;
  TInterleavedFastqSourcesourceUris = class;
  TInterleavedFastqSourcesourceUrisArray = Array of TInterleavedFastqSourcesourceUris;
  TJob = class;
  TJobArray = Array of TJob;
  TJoberrors = class;
  TJoberrorsArray = Array of TJoberrors;
  TJobimportedIds = class;
  TJobimportedIdsArray = Array of TJobimportedIds;
  TJobwarnings = class;
  TJobwarningsArray = Array of TJobwarnings;
  TJobRequest = class;
  TJobRequestArray = Array of TJobRequest;
  TJobRequestdestination = class;
  TJobRequestdestinationArray = Array of TJobRequestdestination;
  TJobRequestsource = class;
  TJobRequestsourceArray = Array of TJobRequestsource;
  TLinearAlignment = class;
  TLinearAlignmentArray = Array of TLinearAlignment;
  TLinearAlignmentcigar = class;
  TLinearAlignmentcigarArray = Array of TLinearAlignmentcigar;
  TListBasesResponse = class;
  TListBasesResponseArray = Array of TListBasesResponse;
  TListCoverageBucketsResponse = class;
  TListCoverageBucketsResponseArray = Array of TListCoverageBucketsResponse;
  TListCoverageBucketsResponsecoverageBuckets = class;
  TListCoverageBucketsResponsecoverageBucketsArray = Array of TListCoverageBucketsResponsecoverageBuckets;
  TListDatasetsResponse = class;
  TListDatasetsResponseArray = Array of TListDatasetsResponse;
  TListDatasetsResponsedatasets = class;
  TListDatasetsResponsedatasetsArray = Array of TListDatasetsResponsedatasets;
  TMergeVariantsRequest = class;
  TMergeVariantsRequestArray = Array of TMergeVariantsRequest;
  TMergeVariantsRequestvariants = class;
  TMergeVariantsRequestvariantsArray = Array of TMergeVariantsRequestvariants;
  TMetadata = class;
  TMetadataArray = Array of TMetadata;
  TMetadatainfo = class;
  TMetadatainfoArray = Array of TMetadatainfo;
  TPairedFastqSource = class;
  TPairedFastqSourceArray = Array of TPairedFastqSource;
  TPairedFastqSourcefirstSourceUris = class;
  TPairedFastqSourcefirstSourceUrisArray = Array of TPairedFastqSourcefirstSourceUris;
  TPairedFastqSourcesecondSourceUris = class;
  TPairedFastqSourcesecondSourceUrisArray = Array of TPairedFastqSourcesecondSourceUris;
  TPosition = class;
  TPositionArray = Array of TPosition;
  TQueryRange = class;
  TQueryRangeArray = Array of TQueryRange;
  TRange = class;
  TRangeArray = Array of TRange;
  TRangePosition = class;
  TRangePositionArray = Array of TRangePosition;
  TRead = class;
  TReadArray = Array of TRead;
  TReadalignedQuality = class;
  TReadalignedQualityArray = Array of TReadalignedQuality;
  TReadinfo = class;
  TReadinfoArray = Array of TReadinfo;
  TReadGroup = class;
  TReadGroupArray = Array of TReadGroup;
  TReadGroupinfo = class;
  TReadGroupinfoArray = Array of TReadGroupinfo;
  TReadGroupprograms = class;
  TReadGroupprogramsArray = Array of TReadGroupprograms;
  TReadGroupExperiment = class;
  TReadGroupExperimentArray = Array of TReadGroupExperiment;
  TReadGroupProgram = class;
  TReadGroupProgramArray = Array of TReadGroupProgram;
  TReadGroupSet = class;
  TReadGroupSetArray = Array of TReadGroupSet;
  TReadGroupSetinfo = class;
  TReadGroupSetinfoArray = Array of TReadGroupSetinfo;
  TReadGroupSetreadGroups = class;
  TReadGroupSetreadGroupsArray = Array of TReadGroupSetreadGroups;
  TReference = class;
  TReferenceArray = Array of TReference;
  TReferencesourceAccessions = class;
  TReferencesourceAccessionsArray = Array of TReferencesourceAccessions;
  TReferenceBound = class;
  TReferenceBoundArray = Array of TReferenceBound;
  TReferenceSet = class;
  TReferenceSetArray = Array of TReferenceSet;
  TReferenceSetreferenceIds = class;
  TReferenceSetreferenceIdsArray = Array of TReferenceSetreferenceIds;
  TReferenceSetsourceAccessions = class;
  TReferenceSetsourceAccessionsArray = Array of TReferenceSetsourceAccessions;
  TSearchAnnotationSetsRequest = class;
  TSearchAnnotationSetsRequestArray = Array of TSearchAnnotationSetsRequest;
  TSearchAnnotationSetsRequestdatasetIds = class;
  TSearchAnnotationSetsRequestdatasetIdsArray = Array of TSearchAnnotationSetsRequestdatasetIds;
  TSearchAnnotationSetsRequesttypes = class;
  TSearchAnnotationSetsRequesttypesArray = Array of TSearchAnnotationSetsRequesttypes;
  TSearchAnnotationSetsResponse = class;
  TSearchAnnotationSetsResponseArray = Array of TSearchAnnotationSetsResponse;
  TSearchAnnotationSetsResponseannotationSets = class;
  TSearchAnnotationSetsResponseannotationSetsArray = Array of TSearchAnnotationSetsResponseannotationSets;
  TSearchAnnotationsRequest = class;
  TSearchAnnotationsRequestArray = Array of TSearchAnnotationsRequest;
  TSearchAnnotationsRequestannotationSetIds = class;
  TSearchAnnotationsRequestannotationSetIdsArray = Array of TSearchAnnotationsRequestannotationSetIds;
  TSearchAnnotationsResponse = class;
  TSearchAnnotationsResponseArray = Array of TSearchAnnotationsResponse;
  TSearchAnnotationsResponseannotations = class;
  TSearchAnnotationsResponseannotationsArray = Array of TSearchAnnotationsResponseannotations;
  TSearchCallSetsRequest = class;
  TSearchCallSetsRequestArray = Array of TSearchCallSetsRequest;
  TSearchCallSetsRequestvariantSetIds = class;
  TSearchCallSetsRequestvariantSetIdsArray = Array of TSearchCallSetsRequestvariantSetIds;
  TSearchCallSetsResponse = class;
  TSearchCallSetsResponseArray = Array of TSearchCallSetsResponse;
  TSearchCallSetsResponsecallSets = class;
  TSearchCallSetsResponsecallSetsArray = Array of TSearchCallSetsResponsecallSets;
  TSearchJobsRequest = class;
  TSearchJobsRequestArray = Array of TSearchJobsRequest;
  TSearchJobsRequeststatus = class;
  TSearchJobsRequeststatusArray = Array of TSearchJobsRequeststatus;
  TSearchJobsResponse = class;
  TSearchJobsResponseArray = Array of TSearchJobsResponse;
  TSearchJobsResponsejobs = class;
  TSearchJobsResponsejobsArray = Array of TSearchJobsResponsejobs;
  TSearchReadGroupSetsRequest = class;
  TSearchReadGroupSetsRequestArray = Array of TSearchReadGroupSetsRequest;
  TSearchReadGroupSetsRequestdatasetIds = class;
  TSearchReadGroupSetsRequestdatasetIdsArray = Array of TSearchReadGroupSetsRequestdatasetIds;
  TSearchReadGroupSetsResponse = class;
  TSearchReadGroupSetsResponseArray = Array of TSearchReadGroupSetsResponse;
  TSearchReadGroupSetsResponsereadGroupSets = class;
  TSearchReadGroupSetsResponsereadGroupSetsArray = Array of TSearchReadGroupSetsResponsereadGroupSets;
  TSearchReadsRequest = class;
  TSearchReadsRequestArray = Array of TSearchReadsRequest;
  TSearchReadsRequestreadGroupIds = class;
  TSearchReadsRequestreadGroupIdsArray = Array of TSearchReadsRequestreadGroupIds;
  TSearchReadsRequestreadGroupSetIds = class;
  TSearchReadsRequestreadGroupSetIdsArray = Array of TSearchReadsRequestreadGroupSetIds;
  TSearchReadsResponse = class;
  TSearchReadsResponseArray = Array of TSearchReadsResponse;
  TSearchReadsResponsealignments = class;
  TSearchReadsResponsealignmentsArray = Array of TSearchReadsResponsealignments;
  TSearchReferenceSetsRequest = class;
  TSearchReferenceSetsRequestArray = Array of TSearchReferenceSetsRequest;
  TSearchReferenceSetsRequestaccessions = class;
  TSearchReferenceSetsRequestaccessionsArray = Array of TSearchReferenceSetsRequestaccessions;
  TSearchReferenceSetsRequestmd5checksums = class;
  TSearchReferenceSetsRequestmd5checksumsArray = Array of TSearchReferenceSetsRequestmd5checksums;
  TSearchReferenceSetsResponse = class;
  TSearchReferenceSetsResponseArray = Array of TSearchReferenceSetsResponse;
  TSearchReferenceSetsResponsereferenceSets = class;
  TSearchReferenceSetsResponsereferenceSetsArray = Array of TSearchReferenceSetsResponsereferenceSets;
  TSearchReferencesRequest = class;
  TSearchReferencesRequestArray = Array of TSearchReferencesRequest;
  TSearchReferencesRequestaccessions = class;
  TSearchReferencesRequestaccessionsArray = Array of TSearchReferencesRequestaccessions;
  TSearchReferencesRequestmd5checksums = class;
  TSearchReferencesRequestmd5checksumsArray = Array of TSearchReferencesRequestmd5checksums;
  TSearchReferencesResponse = class;
  TSearchReferencesResponseArray = Array of TSearchReferencesResponse;
  TSearchReferencesResponsereferences = class;
  TSearchReferencesResponsereferencesArray = Array of TSearchReferencesResponsereferences;
  TSearchVariantSetsRequest = class;
  TSearchVariantSetsRequestArray = Array of TSearchVariantSetsRequest;
  TSearchVariantSetsRequestdatasetIds = class;
  TSearchVariantSetsRequestdatasetIdsArray = Array of TSearchVariantSetsRequestdatasetIds;
  TSearchVariantSetsResponse = class;
  TSearchVariantSetsResponseArray = Array of TSearchVariantSetsResponse;
  TSearchVariantSetsResponsevariantSets = class;
  TSearchVariantSetsResponsevariantSetsArray = Array of TSearchVariantSetsResponsevariantSets;
  TSearchVariantsRequest = class;
  TSearchVariantsRequestArray = Array of TSearchVariantsRequest;
  TSearchVariantsRequestcallSetIds = class;
  TSearchVariantsRequestcallSetIdsArray = Array of TSearchVariantsRequestcallSetIds;
  TSearchVariantsRequestvariantSetIds = class;
  TSearchVariantsRequestvariantSetIdsArray = Array of TSearchVariantsRequestvariantSetIds;
  TSearchVariantsResponse = class;
  TSearchVariantsResponseArray = Array of TSearchVariantsResponse;
  TSearchVariantsResponsevariants = class;
  TSearchVariantsResponsevariantsArray = Array of TSearchVariantsResponsevariants;
  TStreamReadsRequest = class;
  TStreamReadsRequestArray = Array of TStreamReadsRequest;
  TStreamReadsRequestreadGroupSetIds = class;
  TStreamReadsRequestreadGroupSetIdsArray = Array of TStreamReadsRequestreadGroupSetIds;
  TStreamReadsResponse = class;
  TStreamReadsResponseArray = Array of TStreamReadsResponse;
  TStreamReadsResponsealignments = class;
  TStreamReadsResponsealignmentsArray = Array of TStreamReadsResponsealignments;
  TTranscript = class;
  TTranscriptArray = Array of TTranscript;
  TTranscriptexons = class;
  TTranscriptexonsArray = Array of TTranscriptexons;
  TTranscriptCodingSequence = class;
  TTranscriptCodingSequenceArray = Array of TTranscriptCodingSequence;
  TTranscriptExon = class;
  TTranscriptExonArray = Array of TTranscriptExon;
  TVariant = class;
  TVariantArray = Array of TVariant;
  TVariantalternateBases = class;
  TVariantalternateBasesArray = Array of TVariantalternateBases;
  TVariantcalls = class;
  TVariantcallsArray = Array of TVariantcalls;
  TVariantfilter = class;
  TVariantfilterArray = Array of TVariantfilter;
  TVariantinfo = class;
  TVariantinfoArray = Array of TVariantinfo;
  TVariantnames = class;
  TVariantnamesArray = Array of TVariantnames;
  TVariantAnnotation = class;
  TVariantAnnotationArray = Array of TVariantAnnotation;
  TVariantAnnotationconditions = class;
  TVariantAnnotationconditionsArray = Array of TVariantAnnotationconditions;
  TVariantAnnotationtranscriptIds = class;
  TVariantAnnotationtranscriptIdsArray = Array of TVariantAnnotationtranscriptIds;
  TVariantAnnotationCondition = class;
  TVariantAnnotationConditionArray = Array of TVariantAnnotationCondition;
  TVariantAnnotationConditionexternalIds = class;
  TVariantAnnotationConditionexternalIdsArray = Array of TVariantAnnotationConditionexternalIds;
  TVariantAnnotationConditionnames = class;
  TVariantAnnotationConditionnamesArray = Array of TVariantAnnotationConditionnames;
  TVariantSet = class;
  TVariantSetArray = Array of TVariantSet;
  TVariantSetmetadata = class;
  TVariantSetmetadataArray = Array of TVariantSetmetadata;
  TVariantSetreferenceBounds = class;
  TVariantSetreferenceBoundsArray = Array of TVariantSetreferenceBounds;
  
  { --------------------------------------------------------------------
    TAlignReadGroupSetsRequest
    --------------------------------------------------------------------}
  
  TAlignReadGroupSetsRequest = Class(TGoogleBaseObject)
  Private
    FbamSourceUris : TAlignReadGroupSetsRequestbamSourceUris;
    FdatasetId : string;
    FinterleavedFastqSource : TInterleavedFastqSource;
    FpairedFastqSource : TPairedFastqSource;
    FreadGroupSetId : string;
  Protected
    //Property setters
    Procedure SetbamSourceUris(AIndex : Integer; AValue : TAlignReadGroupSetsRequestbamSourceUris); virtual;
    Procedure SetdatasetId(AIndex : Integer; AValue : string); virtual;
    Procedure SetinterleavedFastqSource(AIndex : Integer; AValue : TInterleavedFastqSource); virtual;
    Procedure SetpairedFastqSource(AIndex : Integer; AValue : TPairedFastqSource); virtual;
    Procedure SetreadGroupSetId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property bamSourceUris : TAlignReadGroupSetsRequestbamSourceUris Index 0 Read FbamSourceUris Write SetbamSourceUris;
    Property datasetId : string Index 8 Read FdatasetId Write SetdatasetId;
    Property interleavedFastqSource : TInterleavedFastqSource Index 16 Read FinterleavedFastqSource Write SetinterleavedFastqSource;
    Property pairedFastqSource : TPairedFastqSource Index 24 Read FpairedFastqSource Write SetpairedFastqSource;
    Property readGroupSetId : string Index 32 Read FreadGroupSetId Write SetreadGroupSetId;
  end;
  TAlignReadGroupSetsRequestClass = Class of TAlignReadGroupSetsRequest;
  
  { --------------------------------------------------------------------
    TAlignReadGroupSetsRequestbamSourceUris
    --------------------------------------------------------------------}
  
  TAlignReadGroupSetsRequestbamSourceUris = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TAlignReadGroupSetsRequestbamSourceUrisClass = Class of TAlignReadGroupSetsRequestbamSourceUris;
  
  { --------------------------------------------------------------------
    TAlignReadGroupSetsResponse
    --------------------------------------------------------------------}
  
  TAlignReadGroupSetsResponse = Class(TGoogleBaseObject)
  Private
    FjobId : string;
  Protected
    //Property setters
    Procedure SetjobId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property jobId : string Index 0 Read FjobId Write SetjobId;
  end;
  TAlignReadGroupSetsResponseClass = Class of TAlignReadGroupSetsResponse;
  
  { --------------------------------------------------------------------
    TAnnotation
    --------------------------------------------------------------------}
  
  TAnnotation = Class(TGoogleBaseObject)
  Private
    FannotationSetId : string;
    Fid : string;
    Finfo : TAnnotationinfo;
    Fname : string;
    Fposition : TRangePosition;
    Ftranscript : TTranscript;
    F_type : string;
    Fvariant : TVariantAnnotation;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetannotationSetId(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setinfo(AIndex : Integer; AValue : TAnnotationinfo); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure Setposition(AIndex : Integer; AValue : TRangePosition); virtual;
    Procedure Settranscript(AIndex : Integer; AValue : TTranscript); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
    Procedure Setvariant(AIndex : Integer; AValue : TVariantAnnotation); virtual;
  Public
  Published
    Property annotationSetId : string Index 0 Read FannotationSetId Write SetannotationSetId;
    Property id : string Index 8 Read Fid Write Setid;
    Property info : TAnnotationinfo Index 16 Read Finfo Write Setinfo;
    Property name : string Index 24 Read Fname Write Setname;
    Property position : TRangePosition Index 32 Read Fposition Write Setposition;
    Property transcript : TTranscript Index 40 Read Ftranscript Write Settranscript;
    Property _type : string Index 48 Read F_type Write Set_type;
    Property variant : TVariantAnnotation Index 56 Read Fvariant Write Setvariant;
  end;
  TAnnotationClass = Class of TAnnotation;
  
  { --------------------------------------------------------------------
    TAnnotationinfo
    --------------------------------------------------------------------}
  
  TAnnotationinfo = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TAnnotationinfoClass = Class of TAnnotationinfo;
  
  { --------------------------------------------------------------------
    TAnnotationSet
    --------------------------------------------------------------------}
  
  TAnnotationSet = Class(TGoogleBaseObject)
  Private
    FdatasetId : string;
    Fid : string;
    Finfo : TAnnotationSetinfo;
    Fname : string;
    FreferenceSetId : string;
    FsourceUri : string;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetdatasetId(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setinfo(AIndex : Integer; AValue : TAnnotationSetinfo); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetreferenceSetId(AIndex : Integer; AValue : string); virtual;
    Procedure SetsourceUri(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property datasetId : string Index 0 Read FdatasetId Write SetdatasetId;
    Property id : string Index 8 Read Fid Write Setid;
    Property info : TAnnotationSetinfo Index 16 Read Finfo Write Setinfo;
    Property name : string Index 24 Read Fname Write Setname;
    Property referenceSetId : string Index 32 Read FreferenceSetId Write SetreferenceSetId;
    Property sourceUri : string Index 40 Read FsourceUri Write SetsourceUri;
    Property _type : string Index 48 Read F_type Write Set_type;
  end;
  TAnnotationSetClass = Class of TAnnotationSet;
  
  { --------------------------------------------------------------------
    TAnnotationSetinfo
    --------------------------------------------------------------------}
  
  TAnnotationSetinfo = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TAnnotationSetinfoClass = Class of TAnnotationSetinfo;
  
  { --------------------------------------------------------------------
    TBatchAnnotationsResponse
    --------------------------------------------------------------------}
  
  TBatchAnnotationsResponse = Class(TGoogleBaseObject)
  Private
    Fentries : TBatchAnnotationsResponseentries;
  Protected
    //Property setters
    Procedure Setentries(AIndex : Integer; AValue : TBatchAnnotationsResponseentries); virtual;
  Public
  Published
    Property entries : TBatchAnnotationsResponseentries Index 0 Read Fentries Write Setentries;
  end;
  TBatchAnnotationsResponseClass = Class of TBatchAnnotationsResponse;
  
  { --------------------------------------------------------------------
    TBatchAnnotationsResponseentries
    --------------------------------------------------------------------}
  
  TBatchAnnotationsResponseentries = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TBatchAnnotationsResponseentriesClass = Class of TBatchAnnotationsResponseentries;
  
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
    Fmessage : string;
  Protected
    //Property setters
    Procedure Setcode(AIndex : Integer; AValue : integer); virtual;
    Procedure Setmessage(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property code : integer Index 0 Read Fcode Write Setcode;
    Property message : string Index 8 Read Fmessage Write Setmessage;
  end;
  TBatchAnnotationsResponseEntryStatusClass = Class of TBatchAnnotationsResponseEntryStatus;
  
  { --------------------------------------------------------------------
    TBatchCreateAnnotationsRequest
    --------------------------------------------------------------------}
  
  TBatchCreateAnnotationsRequest = Class(TGoogleBaseObject)
  Private
    Fannotations : TBatchCreateAnnotationsRequestannotations;
  Protected
    //Property setters
    Procedure Setannotations(AIndex : Integer; AValue : TBatchCreateAnnotationsRequestannotations); virtual;
  Public
  Published
    Property annotations : TBatchCreateAnnotationsRequestannotations Index 0 Read Fannotations Write Setannotations;
  end;
  TBatchCreateAnnotationsRequestClass = Class of TBatchCreateAnnotationsRequest;
  
  { --------------------------------------------------------------------
    TBatchCreateAnnotationsRequestannotations
    --------------------------------------------------------------------}
  
  TBatchCreateAnnotationsRequestannotations = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TBatchCreateAnnotationsRequestannotationsClass = Class of TBatchCreateAnnotationsRequestannotations;
  
  { --------------------------------------------------------------------
    TCall
    --------------------------------------------------------------------}
  
  TCall = Class(TGoogleBaseObject)
  Private
    FcallSetId : string;
    FcallSetName : string;
    Fgenotype : TCallgenotype;
    FgenotypeLikelihood : TCallgenotypeLikelihood;
    Finfo : TCallinfo;
    Fphaseset : string;
  Protected
    //Property setters
    Procedure SetcallSetId(AIndex : Integer; AValue : string); virtual;
    Procedure SetcallSetName(AIndex : Integer; AValue : string); virtual;
    Procedure Setgenotype(AIndex : Integer; AValue : TCallgenotype); virtual;
    Procedure SetgenotypeLikelihood(AIndex : Integer; AValue : TCallgenotypeLikelihood); virtual;
    Procedure Setinfo(AIndex : Integer; AValue : TCallinfo); virtual;
    Procedure Setphaseset(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property callSetId : string Index 0 Read FcallSetId Write SetcallSetId;
    Property callSetName : string Index 8 Read FcallSetName Write SetcallSetName;
    Property genotype : TCallgenotype Index 16 Read Fgenotype Write Setgenotype;
    Property genotypeLikelihood : TCallgenotypeLikelihood Index 24 Read FgenotypeLikelihood Write SetgenotypeLikelihood;
    Property info : TCallinfo Index 32 Read Finfo Write Setinfo;
    Property phaseset : string Index 40 Read Fphaseset Write Setphaseset;
  end;
  TCallClass = Class of TCall;
  
  { --------------------------------------------------------------------
    TCallgenotype
    --------------------------------------------------------------------}
  
  TCallgenotype = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TCallgenotypeClass = Class of TCallgenotype;
  
  { --------------------------------------------------------------------
    TCallgenotypeLikelihood
    --------------------------------------------------------------------}
  
  TCallgenotypeLikelihood = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TCallgenotypeLikelihoodClass = Class of TCallgenotypeLikelihood;
  
  { --------------------------------------------------------------------
    TCallinfo
    --------------------------------------------------------------------}
  
  TCallinfo = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TCallinfoClass = Class of TCallinfo;
  
  { --------------------------------------------------------------------
    TCallReadGroupSetsRequest
    --------------------------------------------------------------------}
  
  TCallReadGroupSetsRequest = Class(TGoogleBaseObject)
  Private
    FdatasetId : string;
    FreadGroupSetId : string;
    FsourceUris : TCallReadGroupSetsRequestsourceUris;
  Protected
    //Property setters
    Procedure SetdatasetId(AIndex : Integer; AValue : string); virtual;
    Procedure SetreadGroupSetId(AIndex : Integer; AValue : string); virtual;
    Procedure SetsourceUris(AIndex : Integer; AValue : TCallReadGroupSetsRequestsourceUris); virtual;
  Public
  Published
    Property datasetId : string Index 0 Read FdatasetId Write SetdatasetId;
    Property readGroupSetId : string Index 8 Read FreadGroupSetId Write SetreadGroupSetId;
    Property sourceUris : TCallReadGroupSetsRequestsourceUris Index 16 Read FsourceUris Write SetsourceUris;
  end;
  TCallReadGroupSetsRequestClass = Class of TCallReadGroupSetsRequest;
  
  { --------------------------------------------------------------------
    TCallReadGroupSetsRequestsourceUris
    --------------------------------------------------------------------}
  
  TCallReadGroupSetsRequestsourceUris = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TCallReadGroupSetsRequestsourceUrisClass = Class of TCallReadGroupSetsRequestsourceUris;
  
  { --------------------------------------------------------------------
    TCallReadGroupSetsResponse
    --------------------------------------------------------------------}
  
  TCallReadGroupSetsResponse = Class(TGoogleBaseObject)
  Private
    FjobId : string;
  Protected
    //Property setters
    Procedure SetjobId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property jobId : string Index 0 Read FjobId Write SetjobId;
  end;
  TCallReadGroupSetsResponseClass = Class of TCallReadGroupSetsResponse;
  
  { --------------------------------------------------------------------
    TCallSet
    --------------------------------------------------------------------}
  
  TCallSet = Class(TGoogleBaseObject)
  Private
    Fcreated : string;
    Fid : string;
    Finfo : TCallSetinfo;
    Fname : string;
    FsampleId : string;
    FvariantSetIds : TCallSetvariantSetIds;
  Protected
    //Property setters
    Procedure Setcreated(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setinfo(AIndex : Integer; AValue : TCallSetinfo); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetsampleId(AIndex : Integer; AValue : string); virtual;
    Procedure SetvariantSetIds(AIndex : Integer; AValue : TCallSetvariantSetIds); virtual;
  Public
  Published
    Property created : string Index 0 Read Fcreated Write Setcreated;
    Property id : string Index 8 Read Fid Write Setid;
    Property info : TCallSetinfo Index 16 Read Finfo Write Setinfo;
    Property name : string Index 24 Read Fname Write Setname;
    Property sampleId : string Index 32 Read FsampleId Write SetsampleId;
    Property variantSetIds : TCallSetvariantSetIds Index 40 Read FvariantSetIds Write SetvariantSetIds;
  end;
  TCallSetClass = Class of TCallSet;
  
  { --------------------------------------------------------------------
    TCallSetinfo
    --------------------------------------------------------------------}
  
  TCallSetinfo = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TCallSetinfoClass = Class of TCallSetinfo;
  
  { --------------------------------------------------------------------
    TCallSetvariantSetIds
    --------------------------------------------------------------------}
  
  TCallSetvariantSetIds = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TCallSetvariantSetIdsClass = Class of TCallSetvariantSetIds;
  
  { --------------------------------------------------------------------
    TCigarUnit
    --------------------------------------------------------------------}
  
  TCigarUnit = Class(TGoogleBaseObject)
  Private
    Foperation : string;
    FoperationLength : string;
    FreferenceSequence : string;
  Protected
    //Property setters
    Procedure Setoperation(AIndex : Integer; AValue : string); virtual;
    Procedure SetoperationLength(AIndex : Integer; AValue : string); virtual;
    Procedure SetreferenceSequence(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property operation : string Index 0 Read Foperation Write Setoperation;
    Property operationLength : string Index 8 Read FoperationLength Write SetoperationLength;
    Property referenceSequence : string Index 16 Read FreferenceSequence Write SetreferenceSequence;
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
    Fid : string;
    FisPublic : boolean;
    Fname : string;
    FprojectNumber : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure SetisPublic(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetprojectNumber(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property isPublic : boolean Index 8 Read FisPublic Write SetisPublic;
    Property name : string Index 16 Read Fname Write Setname;
    Property projectNumber : string Index 24 Read FprojectNumber Write SetprojectNumber;
  end;
  TDatasetClass = Class of TDataset;
  
  { --------------------------------------------------------------------
    TExperimentalCreateJobRequest
    --------------------------------------------------------------------}
  
  TExperimentalCreateJobRequest = Class(TGoogleBaseObject)
  Private
    Falign : boolean;
    FcallVariants : boolean;
    FgcsOutputPath : string;
    FpairedSourceUris : TExperimentalCreateJobRequestpairedSourceUris;
    FprojectNumber : string;
    FsourceUris : TExperimentalCreateJobRequestsourceUris;
  Protected
    //Property setters
    Procedure Setalign(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetcallVariants(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetgcsOutputPath(AIndex : Integer; AValue : string); virtual;
    Procedure SetpairedSourceUris(AIndex : Integer; AValue : TExperimentalCreateJobRequestpairedSourceUris); virtual;
    Procedure SetprojectNumber(AIndex : Integer; AValue : string); virtual;
    Procedure SetsourceUris(AIndex : Integer; AValue : TExperimentalCreateJobRequestsourceUris); virtual;
  Public
  Published
    Property align : boolean Index 0 Read Falign Write Setalign;
    Property callVariants : boolean Index 8 Read FcallVariants Write SetcallVariants;
    Property gcsOutputPath : string Index 16 Read FgcsOutputPath Write SetgcsOutputPath;
    Property pairedSourceUris : TExperimentalCreateJobRequestpairedSourceUris Index 24 Read FpairedSourceUris Write SetpairedSourceUris;
    Property projectNumber : string Index 32 Read FprojectNumber Write SetprojectNumber;
    Property sourceUris : TExperimentalCreateJobRequestsourceUris Index 40 Read FsourceUris Write SetsourceUris;
  end;
  TExperimentalCreateJobRequestClass = Class of TExperimentalCreateJobRequest;
  
  { --------------------------------------------------------------------
    TExperimentalCreateJobRequestpairedSourceUris
    --------------------------------------------------------------------}
  
  TExperimentalCreateJobRequestpairedSourceUris = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TExperimentalCreateJobRequestpairedSourceUrisClass = Class of TExperimentalCreateJobRequestpairedSourceUris;
  
  { --------------------------------------------------------------------
    TExperimentalCreateJobRequestsourceUris
    --------------------------------------------------------------------}
  
  TExperimentalCreateJobRequestsourceUris = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TExperimentalCreateJobRequestsourceUrisClass = Class of TExperimentalCreateJobRequestsourceUris;
  
  { --------------------------------------------------------------------
    TExperimentalCreateJobResponse
    --------------------------------------------------------------------}
  
  TExperimentalCreateJobResponse = Class(TGoogleBaseObject)
  Private
    FjobId : string;
  Protected
    //Property setters
    Procedure SetjobId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property jobId : string Index 0 Read FjobId Write SetjobId;
  end;
  TExperimentalCreateJobResponseClass = Class of TExperimentalCreateJobResponse;
  
  { --------------------------------------------------------------------
    TExportReadGroupSetsRequest
    --------------------------------------------------------------------}
  
  TExportReadGroupSetsRequest = Class(TGoogleBaseObject)
  Private
    FexportUri : string;
    FprojectNumber : string;
    FreadGroupSetIds : TExportReadGroupSetsRequestreadGroupSetIds;
    FreferenceNames : TExportReadGroupSetsRequestreferenceNames;
  Protected
    //Property setters
    Procedure SetexportUri(AIndex : Integer; AValue : string); virtual;
    Procedure SetprojectNumber(AIndex : Integer; AValue : string); virtual;
    Procedure SetreadGroupSetIds(AIndex : Integer; AValue : TExportReadGroupSetsRequestreadGroupSetIds); virtual;
    Procedure SetreferenceNames(AIndex : Integer; AValue : TExportReadGroupSetsRequestreferenceNames); virtual;
  Public
  Published
    Property exportUri : string Index 0 Read FexportUri Write SetexportUri;
    Property projectNumber : string Index 8 Read FprojectNumber Write SetprojectNumber;
    Property readGroupSetIds : TExportReadGroupSetsRequestreadGroupSetIds Index 16 Read FreadGroupSetIds Write SetreadGroupSetIds;
    Property referenceNames : TExportReadGroupSetsRequestreferenceNames Index 24 Read FreferenceNames Write SetreferenceNames;
  end;
  TExportReadGroupSetsRequestClass = Class of TExportReadGroupSetsRequest;
  
  { --------------------------------------------------------------------
    TExportReadGroupSetsRequestreadGroupSetIds
    --------------------------------------------------------------------}
  
  TExportReadGroupSetsRequestreadGroupSetIds = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TExportReadGroupSetsRequestreadGroupSetIdsClass = Class of TExportReadGroupSetsRequestreadGroupSetIds;
  
  { --------------------------------------------------------------------
    TExportReadGroupSetsRequestreferenceNames
    --------------------------------------------------------------------}
  
  TExportReadGroupSetsRequestreferenceNames = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TExportReadGroupSetsRequestreferenceNamesClass = Class of TExportReadGroupSetsRequestreferenceNames;
  
  { --------------------------------------------------------------------
    TExportReadGroupSetsResponse
    --------------------------------------------------------------------}
  
  TExportReadGroupSetsResponse = Class(TGoogleBaseObject)
  Private
    FjobId : string;
  Protected
    //Property setters
    Procedure SetjobId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property jobId : string Index 0 Read FjobId Write SetjobId;
  end;
  TExportReadGroupSetsResponseClass = Class of TExportReadGroupSetsResponse;
  
  { --------------------------------------------------------------------
    TExportVariantSetRequest
    --------------------------------------------------------------------}
  
  TExportVariantSetRequest = Class(TGoogleBaseObject)
  Private
    FbigqueryDataset : string;
    FbigqueryTable : string;
    FcallSetIds : TExportVariantSetRequestcallSetIds;
    Fformat : string;
    FprojectNumber : string;
  Protected
    //Property setters
    Procedure SetbigqueryDataset(AIndex : Integer; AValue : string); virtual;
    Procedure SetbigqueryTable(AIndex : Integer; AValue : string); virtual;
    Procedure SetcallSetIds(AIndex : Integer; AValue : TExportVariantSetRequestcallSetIds); virtual;
    Procedure Setformat(AIndex : Integer; AValue : string); virtual;
    Procedure SetprojectNumber(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property bigqueryDataset : string Index 0 Read FbigqueryDataset Write SetbigqueryDataset;
    Property bigqueryTable : string Index 8 Read FbigqueryTable Write SetbigqueryTable;
    Property callSetIds : TExportVariantSetRequestcallSetIds Index 16 Read FcallSetIds Write SetcallSetIds;
    Property format : string Index 24 Read Fformat Write Setformat;
    Property projectNumber : string Index 32 Read FprojectNumber Write SetprojectNumber;
  end;
  TExportVariantSetRequestClass = Class of TExportVariantSetRequest;
  
  { --------------------------------------------------------------------
    TExportVariantSetRequestcallSetIds
    --------------------------------------------------------------------}
  
  TExportVariantSetRequestcallSetIds = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TExportVariantSetRequestcallSetIdsClass = Class of TExportVariantSetRequestcallSetIds;
  
  { --------------------------------------------------------------------
    TExportVariantSetResponse
    --------------------------------------------------------------------}
  
  TExportVariantSetResponse = Class(TGoogleBaseObject)
  Private
    FjobId : string;
  Protected
    //Property setters
    Procedure SetjobId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property jobId : string Index 0 Read FjobId Write SetjobId;
  end;
  TExportVariantSetResponseClass = Class of TExportVariantSetResponse;
  
  { --------------------------------------------------------------------
    TExternalId
    --------------------------------------------------------------------}
  
  TExternalId = Class(TGoogleBaseObject)
  Private
    Fid : string;
    FsourceName : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure SetsourceName(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property sourceName : string Index 8 Read FsourceName Write SetsourceName;
  end;
  TExternalIdClass = Class of TExternalId;
  
  { --------------------------------------------------------------------
    TFastqMetadata
    --------------------------------------------------------------------}
  
  TFastqMetadata = Class(TGoogleBaseObject)
  Private
    FlibraryName : string;
    FplatformName : string;
    FplatformUnit : string;
    FreadGroupName : string;
    FsampleName : string;
  Protected
    //Property setters
    Procedure SetlibraryName(AIndex : Integer; AValue : string); virtual;
    Procedure SetplatformName(AIndex : Integer; AValue : string); virtual;
    Procedure SetplatformUnit(AIndex : Integer; AValue : string); virtual;
    Procedure SetreadGroupName(AIndex : Integer; AValue : string); virtual;
    Procedure SetsampleName(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property libraryName : string Index 0 Read FlibraryName Write SetlibraryName;
    Property platformName : string Index 8 Read FplatformName Write SetplatformName;
    Property platformUnit : string Index 16 Read FplatformUnit Write SetplatformUnit;
    Property readGroupName : string Index 24 Read FreadGroupName Write SetreadGroupName;
    Property sampleName : string Index 32 Read FsampleName Write SetsampleName;
  end;
  TFastqMetadataClass = Class of TFastqMetadata;
  
  { --------------------------------------------------------------------
    TImportReadGroupSetsRequest
    --------------------------------------------------------------------}
  
  TImportReadGroupSetsRequest = Class(TGoogleBaseObject)
  Private
    FdatasetId : string;
    FpartitionStrategy : string;
    FreferenceSetId : string;
    FsourceUris : TImportReadGroupSetsRequestsourceUris;
  Protected
    //Property setters
    Procedure SetdatasetId(AIndex : Integer; AValue : string); virtual;
    Procedure SetpartitionStrategy(AIndex : Integer; AValue : string); virtual;
    Procedure SetreferenceSetId(AIndex : Integer; AValue : string); virtual;
    Procedure SetsourceUris(AIndex : Integer; AValue : TImportReadGroupSetsRequestsourceUris); virtual;
  Public
  Published
    Property datasetId : string Index 0 Read FdatasetId Write SetdatasetId;
    Property partitionStrategy : string Index 8 Read FpartitionStrategy Write SetpartitionStrategy;
    Property referenceSetId : string Index 16 Read FreferenceSetId Write SetreferenceSetId;
    Property sourceUris : TImportReadGroupSetsRequestsourceUris Index 24 Read FsourceUris Write SetsourceUris;
  end;
  TImportReadGroupSetsRequestClass = Class of TImportReadGroupSetsRequest;
  
  { --------------------------------------------------------------------
    TImportReadGroupSetsRequestsourceUris
    --------------------------------------------------------------------}
  
  TImportReadGroupSetsRequestsourceUris = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TImportReadGroupSetsRequestsourceUrisClass = Class of TImportReadGroupSetsRequestsourceUris;
  
  { --------------------------------------------------------------------
    TImportReadGroupSetsResponse
    --------------------------------------------------------------------}
  
  TImportReadGroupSetsResponse = Class(TGoogleBaseObject)
  Private
    FjobId : string;
  Protected
    //Property setters
    Procedure SetjobId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property jobId : string Index 0 Read FjobId Write SetjobId;
  end;
  TImportReadGroupSetsResponseClass = Class of TImportReadGroupSetsResponse;
  
  { --------------------------------------------------------------------
    TImportVariantsRequest
    --------------------------------------------------------------------}
  
  TImportVariantsRequest = Class(TGoogleBaseObject)
  Private
    Fformat : string;
    FsourceUris : TImportVariantsRequestsourceUris;
  Protected
    //Property setters
    Procedure Setformat(AIndex : Integer; AValue : string); virtual;
    Procedure SetsourceUris(AIndex : Integer; AValue : TImportVariantsRequestsourceUris); virtual;
  Public
  Published
    Property format : string Index 0 Read Fformat Write Setformat;
    Property sourceUris : TImportVariantsRequestsourceUris Index 8 Read FsourceUris Write SetsourceUris;
  end;
  TImportVariantsRequestClass = Class of TImportVariantsRequest;
  
  { --------------------------------------------------------------------
    TImportVariantsRequestsourceUris
    --------------------------------------------------------------------}
  
  TImportVariantsRequestsourceUris = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TImportVariantsRequestsourceUrisClass = Class of TImportVariantsRequestsourceUris;
  
  { --------------------------------------------------------------------
    TImportVariantsResponse
    --------------------------------------------------------------------}
  
  TImportVariantsResponse = Class(TGoogleBaseObject)
  Private
    FjobId : string;
  Protected
    //Property setters
    Procedure SetjobId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property jobId : string Index 0 Read FjobId Write SetjobId;
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
    FsourceUris : TInterleavedFastqSourcesourceUris;
  Protected
    //Property setters
    Procedure Setmetadata(AIndex : Integer; AValue : TFastqMetadata); virtual;
    Procedure SetsourceUris(AIndex : Integer; AValue : TInterleavedFastqSourcesourceUris); virtual;
  Public
  Published
    Property metadata : TFastqMetadata Index 0 Read Fmetadata Write Setmetadata;
    Property sourceUris : TInterleavedFastqSourcesourceUris Index 8 Read FsourceUris Write SetsourceUris;
  end;
  TInterleavedFastqSourceClass = Class of TInterleavedFastqSource;
  
  { --------------------------------------------------------------------
    TInterleavedFastqSourcesourceUris
    --------------------------------------------------------------------}
  
  TInterleavedFastqSourcesourceUris = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TInterleavedFastqSourcesourceUrisClass = Class of TInterleavedFastqSourcesourceUris;
  
  { --------------------------------------------------------------------
    TJob
    --------------------------------------------------------------------}
  
  TJob = Class(TGoogleBaseObject)
  Private
    Fcreated : string;
    FdetailedStatus : string;
    Ferrors : TJoberrors;
    Fid : string;
    FimportedIds : TJobimportedIds;
    FprojectNumber : string;
    Frequest : TJobRequest;
    Fstatus : string;
    Fwarnings : TJobwarnings;
  Protected
    //Property setters
    Procedure Setcreated(AIndex : Integer; AValue : string); virtual;
    Procedure SetdetailedStatus(AIndex : Integer; AValue : string); virtual;
    Procedure Seterrors(AIndex : Integer; AValue : TJoberrors); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure SetimportedIds(AIndex : Integer; AValue : TJobimportedIds); virtual;
    Procedure SetprojectNumber(AIndex : Integer; AValue : string); virtual;
    Procedure Setrequest(AIndex : Integer; AValue : TJobRequest); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : string); virtual;
    Procedure Setwarnings(AIndex : Integer; AValue : TJobwarnings); virtual;
  Public
  Published
    Property created : string Index 0 Read Fcreated Write Setcreated;
    Property detailedStatus : string Index 8 Read FdetailedStatus Write SetdetailedStatus;
    Property errors : TJoberrors Index 16 Read Ferrors Write Seterrors;
    Property id : string Index 24 Read Fid Write Setid;
    Property importedIds : TJobimportedIds Index 32 Read FimportedIds Write SetimportedIds;
    Property projectNumber : string Index 40 Read FprojectNumber Write SetprojectNumber;
    Property request : TJobRequest Index 48 Read Frequest Write Setrequest;
    Property status : string Index 56 Read Fstatus Write Setstatus;
    Property warnings : TJobwarnings Index 64 Read Fwarnings Write Setwarnings;
  end;
  TJobClass = Class of TJob;
  
  { --------------------------------------------------------------------
    TJoberrors
    --------------------------------------------------------------------}
  
  TJoberrors = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TJoberrorsClass = Class of TJoberrors;
  
  { --------------------------------------------------------------------
    TJobimportedIds
    --------------------------------------------------------------------}
  
  TJobimportedIds = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TJobimportedIdsClass = Class of TJobimportedIds;
  
  { --------------------------------------------------------------------
    TJobwarnings
    --------------------------------------------------------------------}
  
  TJobwarnings = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TJobwarningsClass = Class of TJobwarnings;
  
  { --------------------------------------------------------------------
    TJobRequest
    --------------------------------------------------------------------}
  
  TJobRequest = Class(TGoogleBaseObject)
  Private
    Fdestination : TJobRequestdestination;
    Fsource : TJobRequestsource;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setdestination(AIndex : Integer; AValue : TJobRequestdestination); virtual;
    Procedure Setsource(AIndex : Integer; AValue : TJobRequestsource); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property destination : TJobRequestdestination Index 0 Read Fdestination Write Setdestination;
    Property source : TJobRequestsource Index 8 Read Fsource Write Setsource;
    Property _type : string Index 16 Read F_type Write Set_type;
  end;
  TJobRequestClass = Class of TJobRequest;
  
  { --------------------------------------------------------------------
    TJobRequestdestination
    --------------------------------------------------------------------}
  
  TJobRequestdestination = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TJobRequestdestinationClass = Class of TJobRequestdestination;
  
  { --------------------------------------------------------------------
    TJobRequestsource
    --------------------------------------------------------------------}
  
  TJobRequestsource = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TJobRequestsourceClass = Class of TJobRequestsource;
  
  { --------------------------------------------------------------------
    TLinearAlignment
    --------------------------------------------------------------------}
  
  TLinearAlignment = Class(TGoogleBaseObject)
  Private
    Fcigar : TLinearAlignmentcigar;
    FmappingQuality : integer;
    Fposition : TPosition;
  Protected
    //Property setters
    Procedure Setcigar(AIndex : Integer; AValue : TLinearAlignmentcigar); virtual;
    Procedure SetmappingQuality(AIndex : Integer; AValue : integer); virtual;
    Procedure Setposition(AIndex : Integer; AValue : TPosition); virtual;
  Public
  Published
    Property cigar : TLinearAlignmentcigar Index 0 Read Fcigar Write Setcigar;
    Property mappingQuality : integer Index 8 Read FmappingQuality Write SetmappingQuality;
    Property position : TPosition Index 16 Read Fposition Write Setposition;
  end;
  TLinearAlignmentClass = Class of TLinearAlignment;
  
  { --------------------------------------------------------------------
    TLinearAlignmentcigar
    --------------------------------------------------------------------}
  
  TLinearAlignmentcigar = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TLinearAlignmentcigarClass = Class of TLinearAlignmentcigar;
  
  { --------------------------------------------------------------------
    TListBasesResponse
    --------------------------------------------------------------------}
  
  TListBasesResponse = Class(TGoogleBaseObject)
  Private
    FnextPageToken : string;
    Foffset : string;
    Fsequence : string;
  Protected
    //Property setters
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure Setoffset(AIndex : Integer; AValue : string); virtual;
    Procedure Setsequence(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property nextPageToken : string Index 0 Read FnextPageToken Write SetnextPageToken;
    Property offset : string Index 8 Read Foffset Write Setoffset;
    Property sequence : string Index 16 Read Fsequence Write Setsequence;
  end;
  TListBasesResponseClass = Class of TListBasesResponse;
  
  { --------------------------------------------------------------------
    TListCoverageBucketsResponse
    --------------------------------------------------------------------}
  
  TListCoverageBucketsResponse = Class(TGoogleBaseObject)
  Private
    FbucketWidth : string;
    FcoverageBuckets : TListCoverageBucketsResponsecoverageBuckets;
    FnextPageToken : string;
  Protected
    //Property setters
    Procedure SetbucketWidth(AIndex : Integer; AValue : string); virtual;
    Procedure SetcoverageBuckets(AIndex : Integer; AValue : TListCoverageBucketsResponsecoverageBuckets); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property bucketWidth : string Index 0 Read FbucketWidth Write SetbucketWidth;
    Property coverageBuckets : TListCoverageBucketsResponsecoverageBuckets Index 8 Read FcoverageBuckets Write SetcoverageBuckets;
    Property nextPageToken : string Index 16 Read FnextPageToken Write SetnextPageToken;
  end;
  TListCoverageBucketsResponseClass = Class of TListCoverageBucketsResponse;
  
  { --------------------------------------------------------------------
    TListCoverageBucketsResponsecoverageBuckets
    --------------------------------------------------------------------}
  
  TListCoverageBucketsResponsecoverageBuckets = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TListCoverageBucketsResponsecoverageBucketsClass = Class of TListCoverageBucketsResponsecoverageBuckets;
  
  { --------------------------------------------------------------------
    TListDatasetsResponse
    --------------------------------------------------------------------}
  
  TListDatasetsResponse = Class(TGoogleBaseObject)
  Private
    Fdatasets : TListDatasetsResponsedatasets;
    FnextPageToken : string;
  Protected
    //Property setters
    Procedure Setdatasets(AIndex : Integer; AValue : TListDatasetsResponsedatasets); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property datasets : TListDatasetsResponsedatasets Index 0 Read Fdatasets Write Setdatasets;
    Property nextPageToken : string Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TListDatasetsResponseClass = Class of TListDatasetsResponse;
  
  { --------------------------------------------------------------------
    TListDatasetsResponsedatasets
    --------------------------------------------------------------------}
  
  TListDatasetsResponsedatasets = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TListDatasetsResponsedatasetsClass = Class of TListDatasetsResponsedatasets;
  
  { --------------------------------------------------------------------
    TMergeVariantsRequest
    --------------------------------------------------------------------}
  
  TMergeVariantsRequest = Class(TGoogleBaseObject)
  Private
    Fvariants : TMergeVariantsRequestvariants;
  Protected
    //Property setters
    Procedure Setvariants(AIndex : Integer; AValue : TMergeVariantsRequestvariants); virtual;
  Public
  Published
    Property variants : TMergeVariantsRequestvariants Index 0 Read Fvariants Write Setvariants;
  end;
  TMergeVariantsRequestClass = Class of TMergeVariantsRequest;
  
  { --------------------------------------------------------------------
    TMergeVariantsRequestvariants
    --------------------------------------------------------------------}
  
  TMergeVariantsRequestvariants = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TMergeVariantsRequestvariantsClass = Class of TMergeVariantsRequestvariants;
  
  { --------------------------------------------------------------------
    TMetadata
    --------------------------------------------------------------------}
  
  TMetadata = Class(TGoogleBaseObject)
  Private
    Fdescription : string;
    Fid : string;
    Finfo : TMetadatainfo;
    Fkey : string;
    Fnumber : string;
    F_type : string;
    Fvalue : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setinfo(AIndex : Integer; AValue : TMetadatainfo); virtual;
    Procedure Setkey(AIndex : Integer; AValue : string); virtual;
    Procedure Setnumber(AIndex : Integer; AValue : string); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
    Procedure Setvalue(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property description : string Index 0 Read Fdescription Write Setdescription;
    Property id : string Index 8 Read Fid Write Setid;
    Property info : TMetadatainfo Index 16 Read Finfo Write Setinfo;
    Property key : string Index 24 Read Fkey Write Setkey;
    Property number : string Index 32 Read Fnumber Write Setnumber;
    Property _type : string Index 40 Read F_type Write Set_type;
    Property value : string Index 48 Read Fvalue Write Setvalue;
  end;
  TMetadataClass = Class of TMetadata;
  
  { --------------------------------------------------------------------
    TMetadatainfo
    --------------------------------------------------------------------}
  
  TMetadatainfo = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TMetadatainfoClass = Class of TMetadatainfo;
  
  { --------------------------------------------------------------------
    TPairedFastqSource
    --------------------------------------------------------------------}
  
  TPairedFastqSource = Class(TGoogleBaseObject)
  Private
    FfirstSourceUris : TPairedFastqSourcefirstSourceUris;
    Fmetadata : TFastqMetadata;
    FsecondSourceUris : TPairedFastqSourcesecondSourceUris;
  Protected
    //Property setters
    Procedure SetfirstSourceUris(AIndex : Integer; AValue : TPairedFastqSourcefirstSourceUris); virtual;
    Procedure Setmetadata(AIndex : Integer; AValue : TFastqMetadata); virtual;
    Procedure SetsecondSourceUris(AIndex : Integer; AValue : TPairedFastqSourcesecondSourceUris); virtual;
  Public
  Published
    Property firstSourceUris : TPairedFastqSourcefirstSourceUris Index 0 Read FfirstSourceUris Write SetfirstSourceUris;
    Property metadata : TFastqMetadata Index 8 Read Fmetadata Write Setmetadata;
    Property secondSourceUris : TPairedFastqSourcesecondSourceUris Index 16 Read FsecondSourceUris Write SetsecondSourceUris;
  end;
  TPairedFastqSourceClass = Class of TPairedFastqSource;
  
  { --------------------------------------------------------------------
    TPairedFastqSourcefirstSourceUris
    --------------------------------------------------------------------}
  
  TPairedFastqSourcefirstSourceUris = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TPairedFastqSourcefirstSourceUrisClass = Class of TPairedFastqSourcefirstSourceUris;
  
  { --------------------------------------------------------------------
    TPairedFastqSourcesecondSourceUris
    --------------------------------------------------------------------}
  
  TPairedFastqSourcesecondSourceUris = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TPairedFastqSourcesecondSourceUrisClass = Class of TPairedFastqSourcesecondSourceUris;
  
  { --------------------------------------------------------------------
    TPosition
    --------------------------------------------------------------------}
  
  TPosition = Class(TGoogleBaseObject)
  Private
    Fposition : string;
    FreferenceName : string;
    FreverseStrand : boolean;
  Protected
    //Property setters
    Procedure Setposition(AIndex : Integer; AValue : string); virtual;
    Procedure SetreferenceName(AIndex : Integer; AValue : string); virtual;
    Procedure SetreverseStrand(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property position : string Index 0 Read Fposition Write Setposition;
    Property referenceName : string Index 8 Read FreferenceName Write SetreferenceName;
    Property reverseStrand : boolean Index 16 Read FreverseStrand Write SetreverseStrand;
  end;
  TPositionClass = Class of TPosition;
  
  { --------------------------------------------------------------------
    TQueryRange
    --------------------------------------------------------------------}
  
  TQueryRange = Class(TGoogleBaseObject)
  Private
    F_end : string;
    FreferenceId : string;
    FreferenceName : string;
    Fstart : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_end(AIndex : Integer; AValue : string); virtual;
    Procedure SetreferenceId(AIndex : Integer; AValue : string); virtual;
    Procedure SetreferenceName(AIndex : Integer; AValue : string); virtual;
    Procedure Setstart(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property _end : string Index 0 Read F_end Write Set_end;
    Property referenceId : string Index 8 Read FreferenceId Write SetreferenceId;
    Property referenceName : string Index 16 Read FreferenceName Write SetreferenceName;
    Property start : string Index 24 Read Fstart Write Setstart;
  end;
  TQueryRangeClass = Class of TQueryRange;
  
  { --------------------------------------------------------------------
    TRange
    --------------------------------------------------------------------}
  
  TRange = Class(TGoogleBaseObject)
  Private
    F_end : string;
    FreferenceName : string;
    Fstart : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_end(AIndex : Integer; AValue : string); virtual;
    Procedure SetreferenceName(AIndex : Integer; AValue : string); virtual;
    Procedure Setstart(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property _end : string Index 0 Read F_end Write Set_end;
    Property referenceName : string Index 8 Read FreferenceName Write SetreferenceName;
    Property start : string Index 16 Read Fstart Write Setstart;
  end;
  TRangeClass = Class of TRange;
  
  { --------------------------------------------------------------------
    TRangePosition
    --------------------------------------------------------------------}
  
  TRangePosition = Class(TGoogleBaseObject)
  Private
    F_end : string;
    FreferenceId : string;
    FreferenceName : string;
    FreverseStrand : boolean;
    Fstart : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_end(AIndex : Integer; AValue : string); virtual;
    Procedure SetreferenceId(AIndex : Integer; AValue : string); virtual;
    Procedure SetreferenceName(AIndex : Integer; AValue : string); virtual;
    Procedure SetreverseStrand(AIndex : Integer; AValue : boolean); virtual;
    Procedure Setstart(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property _end : string Index 0 Read F_end Write Set_end;
    Property referenceId : string Index 8 Read FreferenceId Write SetreferenceId;
    Property referenceName : string Index 16 Read FreferenceName Write SetreferenceName;
    Property reverseStrand : boolean Index 24 Read FreverseStrand Write SetreverseStrand;
    Property start : string Index 32 Read Fstart Write Setstart;
  end;
  TRangePositionClass = Class of TRangePosition;
  
  { --------------------------------------------------------------------
    TRead
    --------------------------------------------------------------------}
  
  TRead = Class(TGoogleBaseObject)
  Private
    FalignedQuality : TReadalignedQuality;
    FalignedSequence : string;
    Falignment : TLinearAlignment;
    FduplicateFragment : boolean;
    FfailedVendorQualityChecks : boolean;
    FfragmentLength : integer;
    FfragmentName : string;
    Fid : string;
    Finfo : TReadinfo;
    FnextMatePosition : TPosition;
    FnumberReads : integer;
    FproperPlacement : boolean;
    FreadGroupId : string;
    FreadGroupSetId : string;
    FreadNumber : integer;
    FsecondaryAlignment : boolean;
    FsupplementaryAlignment : boolean;
  Protected
    //Property setters
    Procedure SetalignedQuality(AIndex : Integer; AValue : TReadalignedQuality); virtual;
    Procedure SetalignedSequence(AIndex : Integer; AValue : string); virtual;
    Procedure Setalignment(AIndex : Integer; AValue : TLinearAlignment); virtual;
    Procedure SetduplicateFragment(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetfailedVendorQualityChecks(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetfragmentLength(AIndex : Integer; AValue : integer); virtual;
    Procedure SetfragmentName(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setinfo(AIndex : Integer; AValue : TReadinfo); virtual;
    Procedure SetnextMatePosition(AIndex : Integer; AValue : TPosition); virtual;
    Procedure SetnumberReads(AIndex : Integer; AValue : integer); virtual;
    Procedure SetproperPlacement(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetreadGroupId(AIndex : Integer; AValue : string); virtual;
    Procedure SetreadGroupSetId(AIndex : Integer; AValue : string); virtual;
    Procedure SetreadNumber(AIndex : Integer; AValue : integer); virtual;
    Procedure SetsecondaryAlignment(AIndex : Integer; AValue : boolean); virtual;
    Procedure SetsupplementaryAlignment(AIndex : Integer; AValue : boolean); virtual;
  Public
  Published
    Property alignedQuality : TReadalignedQuality Index 0 Read FalignedQuality Write SetalignedQuality;
    Property alignedSequence : string Index 8 Read FalignedSequence Write SetalignedSequence;
    Property alignment : TLinearAlignment Index 16 Read Falignment Write Setalignment;
    Property duplicateFragment : boolean Index 24 Read FduplicateFragment Write SetduplicateFragment;
    Property failedVendorQualityChecks : boolean Index 32 Read FfailedVendorQualityChecks Write SetfailedVendorQualityChecks;
    Property fragmentLength : integer Index 40 Read FfragmentLength Write SetfragmentLength;
    Property fragmentName : string Index 48 Read FfragmentName Write SetfragmentName;
    Property id : string Index 56 Read Fid Write Setid;
    Property info : TReadinfo Index 64 Read Finfo Write Setinfo;
    Property nextMatePosition : TPosition Index 72 Read FnextMatePosition Write SetnextMatePosition;
    Property numberReads : integer Index 80 Read FnumberReads Write SetnumberReads;
    Property properPlacement : boolean Index 88 Read FproperPlacement Write SetproperPlacement;
    Property readGroupId : string Index 96 Read FreadGroupId Write SetreadGroupId;
    Property readGroupSetId : string Index 104 Read FreadGroupSetId Write SetreadGroupSetId;
    Property readNumber : integer Index 112 Read FreadNumber Write SetreadNumber;
    Property secondaryAlignment : boolean Index 120 Read FsecondaryAlignment Write SetsecondaryAlignment;
    Property supplementaryAlignment : boolean Index 128 Read FsupplementaryAlignment Write SetsupplementaryAlignment;
  end;
  TReadClass = Class of TRead;
  
  { --------------------------------------------------------------------
    TReadalignedQuality
    --------------------------------------------------------------------}
  
  TReadalignedQuality = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TReadalignedQualityClass = Class of TReadalignedQuality;
  
  { --------------------------------------------------------------------
    TReadinfo
    --------------------------------------------------------------------}
  
  TReadinfo = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TReadinfoClass = Class of TReadinfo;
  
  { --------------------------------------------------------------------
    TReadGroup
    --------------------------------------------------------------------}
  
  TReadGroup = Class(TGoogleBaseObject)
  Private
    FdatasetId : string;
    Fdescription : string;
    Fexperiment : TReadGroupExperiment;
    Fid : string;
    Finfo : TReadGroupinfo;
    Fname : string;
    FpredictedInsertSize : integer;
    Fprograms : TReadGroupprograms;
    FreferenceSetId : string;
    FsampleId : string;
  Protected
    //Property setters
    Procedure SetdatasetId(AIndex : Integer; AValue : string); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure Setexperiment(AIndex : Integer; AValue : TReadGroupExperiment); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setinfo(AIndex : Integer; AValue : TReadGroupinfo); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetpredictedInsertSize(AIndex : Integer; AValue : integer); virtual;
    Procedure Setprograms(AIndex : Integer; AValue : TReadGroupprograms); virtual;
    Procedure SetreferenceSetId(AIndex : Integer; AValue : string); virtual;
    Procedure SetsampleId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property datasetId : string Index 0 Read FdatasetId Write SetdatasetId;
    Property description : string Index 8 Read Fdescription Write Setdescription;
    Property experiment : TReadGroupExperiment Index 16 Read Fexperiment Write Setexperiment;
    Property id : string Index 24 Read Fid Write Setid;
    Property info : TReadGroupinfo Index 32 Read Finfo Write Setinfo;
    Property name : string Index 40 Read Fname Write Setname;
    Property predictedInsertSize : integer Index 48 Read FpredictedInsertSize Write SetpredictedInsertSize;
    Property programs : TReadGroupprograms Index 56 Read Fprograms Write Setprograms;
    Property referenceSetId : string Index 64 Read FreferenceSetId Write SetreferenceSetId;
    Property sampleId : string Index 72 Read FsampleId Write SetsampleId;
  end;
  TReadGroupClass = Class of TReadGroup;
  
  { --------------------------------------------------------------------
    TReadGroupinfo
    --------------------------------------------------------------------}
  
  TReadGroupinfo = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TReadGroupinfoClass = Class of TReadGroupinfo;
  
  { --------------------------------------------------------------------
    TReadGroupprograms
    --------------------------------------------------------------------}
  
  TReadGroupprograms = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TReadGroupprogramsClass = Class of TReadGroupprograms;
  
  { --------------------------------------------------------------------
    TReadGroupExperiment
    --------------------------------------------------------------------}
  
  TReadGroupExperiment = Class(TGoogleBaseObject)
  Private
    FinstrumentModel : string;
    FlibraryId : string;
    FplatformUnit : string;
    FsequencingCenter : string;
  Protected
    //Property setters
    Procedure SetinstrumentModel(AIndex : Integer; AValue : string); virtual;
    Procedure SetlibraryId(AIndex : Integer; AValue : string); virtual;
    Procedure SetplatformUnit(AIndex : Integer; AValue : string); virtual;
    Procedure SetsequencingCenter(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property instrumentModel : string Index 0 Read FinstrumentModel Write SetinstrumentModel;
    Property libraryId : string Index 8 Read FlibraryId Write SetlibraryId;
    Property platformUnit : string Index 16 Read FplatformUnit Write SetplatformUnit;
    Property sequencingCenter : string Index 24 Read FsequencingCenter Write SetsequencingCenter;
  end;
  TReadGroupExperimentClass = Class of TReadGroupExperiment;
  
  { --------------------------------------------------------------------
    TReadGroupProgram
    --------------------------------------------------------------------}
  
  TReadGroupProgram = Class(TGoogleBaseObject)
  Private
    FcommandLine : string;
    Fid : string;
    Fname : string;
    FprevProgramId : string;
    Fversion : string;
  Protected
    //Property setters
    Procedure SetcommandLine(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetprevProgramId(AIndex : Integer; AValue : string); virtual;
    Procedure Setversion(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property commandLine : string Index 0 Read FcommandLine Write SetcommandLine;
    Property id : string Index 8 Read Fid Write Setid;
    Property name : string Index 16 Read Fname Write Setname;
    Property prevProgramId : string Index 24 Read FprevProgramId Write SetprevProgramId;
    Property version : string Index 32 Read Fversion Write Setversion;
  end;
  TReadGroupProgramClass = Class of TReadGroupProgram;
  
  { --------------------------------------------------------------------
    TReadGroupSet
    --------------------------------------------------------------------}
  
  TReadGroupSet = Class(TGoogleBaseObject)
  Private
    FdatasetId : string;
    Ffilename : string;
    Fid : string;
    Finfo : TReadGroupSetinfo;
    Fname : string;
    FreadGroups : TReadGroupSetreadGroups;
    FreferenceSetId : string;
  Protected
    //Property setters
    Procedure SetdatasetId(AIndex : Integer; AValue : string); virtual;
    Procedure Setfilename(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setinfo(AIndex : Integer; AValue : TReadGroupSetinfo); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetreadGroups(AIndex : Integer; AValue : TReadGroupSetreadGroups); virtual;
    Procedure SetreferenceSetId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property datasetId : string Index 0 Read FdatasetId Write SetdatasetId;
    Property filename : string Index 8 Read Ffilename Write Setfilename;
    Property id : string Index 16 Read Fid Write Setid;
    Property info : TReadGroupSetinfo Index 24 Read Finfo Write Setinfo;
    Property name : string Index 32 Read Fname Write Setname;
    Property readGroups : TReadGroupSetreadGroups Index 40 Read FreadGroups Write SetreadGroups;
    Property referenceSetId : string Index 48 Read FreferenceSetId Write SetreferenceSetId;
  end;
  TReadGroupSetClass = Class of TReadGroupSet;
  
  { --------------------------------------------------------------------
    TReadGroupSetinfo
    --------------------------------------------------------------------}
  
  TReadGroupSetinfo = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TReadGroupSetinfoClass = Class of TReadGroupSetinfo;
  
  { --------------------------------------------------------------------
    TReadGroupSetreadGroups
    --------------------------------------------------------------------}
  
  TReadGroupSetreadGroups = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TReadGroupSetreadGroupsClass = Class of TReadGroupSetreadGroups;
  
  { --------------------------------------------------------------------
    TReference
    --------------------------------------------------------------------}
  
  TReference = Class(TGoogleBaseObject)
  Private
    Fid : string;
    Flength : string;
    Fmd5checksum : string;
    Fname : string;
    FncbiTaxonId : integer;
    FsourceAccessions : TReferencesourceAccessions;
    FsourceURI : string;
  Protected
    //Property setters
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setlength(AIndex : Integer; AValue : string); virtual;
    Procedure Setmd5checksum(AIndex : Integer; AValue : string); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetncbiTaxonId(AIndex : Integer; AValue : integer); virtual;
    Procedure SetsourceAccessions(AIndex : Integer; AValue : TReferencesourceAccessions); virtual;
    Procedure SetsourceURI(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property id : string Index 0 Read Fid Write Setid;
    Property length : string Index 8 Read Flength Write Setlength;
    Property md5checksum : string Index 16 Read Fmd5checksum Write Setmd5checksum;
    Property name : string Index 24 Read Fname Write Setname;
    Property ncbiTaxonId : integer Index 32 Read FncbiTaxonId Write SetncbiTaxonId;
    Property sourceAccessions : TReferencesourceAccessions Index 40 Read FsourceAccessions Write SetsourceAccessions;
    Property sourceURI : string Index 48 Read FsourceURI Write SetsourceURI;
  end;
  TReferenceClass = Class of TReference;
  
  { --------------------------------------------------------------------
    TReferencesourceAccessions
    --------------------------------------------------------------------}
  
  TReferencesourceAccessions = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TReferencesourceAccessionsClass = Class of TReferencesourceAccessions;
  
  { --------------------------------------------------------------------
    TReferenceBound
    --------------------------------------------------------------------}
  
  TReferenceBound = Class(TGoogleBaseObject)
  Private
    FreferenceName : string;
    FupperBound : string;
  Protected
    //Property setters
    Procedure SetreferenceName(AIndex : Integer; AValue : string); virtual;
    Procedure SetupperBound(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property referenceName : string Index 0 Read FreferenceName Write SetreferenceName;
    Property upperBound : string Index 8 Read FupperBound Write SetupperBound;
  end;
  TReferenceBoundClass = Class of TReferenceBound;
  
  { --------------------------------------------------------------------
    TReferenceSet
    --------------------------------------------------------------------}
  
  TReferenceSet = Class(TGoogleBaseObject)
  Private
    FassemblyId : string;
    Fdescription : string;
    Fid : string;
    Fmd5checksum : string;
    FncbiTaxonId : integer;
    FreferenceIds : TReferenceSetreferenceIds;
    FsourceAccessions : TReferenceSetsourceAccessions;
    FsourceURI : string;
  Protected
    //Property setters
    Procedure SetassemblyId(AIndex : Integer; AValue : string); virtual;
    Procedure Setdescription(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setmd5checksum(AIndex : Integer; AValue : string); virtual;
    Procedure SetncbiTaxonId(AIndex : Integer; AValue : integer); virtual;
    Procedure SetreferenceIds(AIndex : Integer; AValue : TReferenceSetreferenceIds); virtual;
    Procedure SetsourceAccessions(AIndex : Integer; AValue : TReferenceSetsourceAccessions); virtual;
    Procedure SetsourceURI(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property assemblyId : string Index 0 Read FassemblyId Write SetassemblyId;
    Property description : string Index 8 Read Fdescription Write Setdescription;
    Property id : string Index 16 Read Fid Write Setid;
    Property md5checksum : string Index 24 Read Fmd5checksum Write Setmd5checksum;
    Property ncbiTaxonId : integer Index 32 Read FncbiTaxonId Write SetncbiTaxonId;
    Property referenceIds : TReferenceSetreferenceIds Index 40 Read FreferenceIds Write SetreferenceIds;
    Property sourceAccessions : TReferenceSetsourceAccessions Index 48 Read FsourceAccessions Write SetsourceAccessions;
    Property sourceURI : string Index 56 Read FsourceURI Write SetsourceURI;
  end;
  TReferenceSetClass = Class of TReferenceSet;
  
  { --------------------------------------------------------------------
    TReferenceSetreferenceIds
    --------------------------------------------------------------------}
  
  TReferenceSetreferenceIds = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TReferenceSetreferenceIdsClass = Class of TReferenceSetreferenceIds;
  
  { --------------------------------------------------------------------
    TReferenceSetsourceAccessions
    --------------------------------------------------------------------}
  
  TReferenceSetsourceAccessions = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TReferenceSetsourceAccessionsClass = Class of TReferenceSetsourceAccessions;
  
  { --------------------------------------------------------------------
    TSearchAnnotationSetsRequest
    --------------------------------------------------------------------}
  
  TSearchAnnotationSetsRequest = Class(TGoogleBaseObject)
  Private
    FdatasetIds : TSearchAnnotationSetsRequestdatasetIds;
    Fname : string;
    FpageSize : integer;
    FpageToken : string;
    FreferenceSetId : string;
    Ftypes : TSearchAnnotationSetsRequesttypes;
  Protected
    //Property setters
    Procedure SetdatasetIds(AIndex : Integer; AValue : TSearchAnnotationSetsRequestdatasetIds); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetpageSize(AIndex : Integer; AValue : integer); virtual;
    Procedure SetpageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetreferenceSetId(AIndex : Integer; AValue : string); virtual;
    Procedure Settypes(AIndex : Integer; AValue : TSearchAnnotationSetsRequesttypes); virtual;
  Public
  Published
    Property datasetIds : TSearchAnnotationSetsRequestdatasetIds Index 0 Read FdatasetIds Write SetdatasetIds;
    Property name : string Index 8 Read Fname Write Setname;
    Property pageSize : integer Index 16 Read FpageSize Write SetpageSize;
    Property pageToken : string Index 24 Read FpageToken Write SetpageToken;
    Property referenceSetId : string Index 32 Read FreferenceSetId Write SetreferenceSetId;
    Property types : TSearchAnnotationSetsRequesttypes Index 40 Read Ftypes Write Settypes;
  end;
  TSearchAnnotationSetsRequestClass = Class of TSearchAnnotationSetsRequest;
  
  { --------------------------------------------------------------------
    TSearchAnnotationSetsRequestdatasetIds
    --------------------------------------------------------------------}
  
  TSearchAnnotationSetsRequestdatasetIds = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TSearchAnnotationSetsRequestdatasetIdsClass = Class of TSearchAnnotationSetsRequestdatasetIds;
  
  { --------------------------------------------------------------------
    TSearchAnnotationSetsRequesttypes
    --------------------------------------------------------------------}
  
  TSearchAnnotationSetsRequesttypes = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TSearchAnnotationSetsRequesttypesClass = Class of TSearchAnnotationSetsRequesttypes;
  
  { --------------------------------------------------------------------
    TSearchAnnotationSetsResponse
    --------------------------------------------------------------------}
  
  TSearchAnnotationSetsResponse = Class(TGoogleBaseObject)
  Private
    FannotationSets : TSearchAnnotationSetsResponseannotationSets;
    FnextPageToken : string;
  Protected
    //Property setters
    Procedure SetannotationSets(AIndex : Integer; AValue : TSearchAnnotationSetsResponseannotationSets); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property annotationSets : TSearchAnnotationSetsResponseannotationSets Index 0 Read FannotationSets Write SetannotationSets;
    Property nextPageToken : string Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TSearchAnnotationSetsResponseClass = Class of TSearchAnnotationSetsResponse;
  
  { --------------------------------------------------------------------
    TSearchAnnotationSetsResponseannotationSets
    --------------------------------------------------------------------}
  
  TSearchAnnotationSetsResponseannotationSets = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TSearchAnnotationSetsResponseannotationSetsClass = Class of TSearchAnnotationSetsResponseannotationSets;
  
  { --------------------------------------------------------------------
    TSearchAnnotationsRequest
    --------------------------------------------------------------------}
  
  TSearchAnnotationsRequest = Class(TGoogleBaseObject)
  Private
    FannotationSetIds : TSearchAnnotationsRequestannotationSetIds;
    FpageSize : integer;
    FpageToken : string;
    Frange : TQueryRange;
  Protected
    //Property setters
    Procedure SetannotationSetIds(AIndex : Integer; AValue : TSearchAnnotationsRequestannotationSetIds); virtual;
    Procedure SetpageSize(AIndex : Integer; AValue : integer); virtual;
    Procedure SetpageToken(AIndex : Integer; AValue : string); virtual;
    Procedure Setrange(AIndex : Integer; AValue : TQueryRange); virtual;
  Public
  Published
    Property annotationSetIds : TSearchAnnotationsRequestannotationSetIds Index 0 Read FannotationSetIds Write SetannotationSetIds;
    Property pageSize : integer Index 8 Read FpageSize Write SetpageSize;
    Property pageToken : string Index 16 Read FpageToken Write SetpageToken;
    Property range : TQueryRange Index 24 Read Frange Write Setrange;
  end;
  TSearchAnnotationsRequestClass = Class of TSearchAnnotationsRequest;
  
  { --------------------------------------------------------------------
    TSearchAnnotationsRequestannotationSetIds
    --------------------------------------------------------------------}
  
  TSearchAnnotationsRequestannotationSetIds = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TSearchAnnotationsRequestannotationSetIdsClass = Class of TSearchAnnotationsRequestannotationSetIds;
  
  { --------------------------------------------------------------------
    TSearchAnnotationsResponse
    --------------------------------------------------------------------}
  
  TSearchAnnotationsResponse = Class(TGoogleBaseObject)
  Private
    Fannotations : TSearchAnnotationsResponseannotations;
    FnextPageToken : string;
  Protected
    //Property setters
    Procedure Setannotations(AIndex : Integer; AValue : TSearchAnnotationsResponseannotations); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property annotations : TSearchAnnotationsResponseannotations Index 0 Read Fannotations Write Setannotations;
    Property nextPageToken : string Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TSearchAnnotationsResponseClass = Class of TSearchAnnotationsResponse;
  
  { --------------------------------------------------------------------
    TSearchAnnotationsResponseannotations
    --------------------------------------------------------------------}
  
  TSearchAnnotationsResponseannotations = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TSearchAnnotationsResponseannotationsClass = Class of TSearchAnnotationsResponseannotations;
  
  { --------------------------------------------------------------------
    TSearchCallSetsRequest
    --------------------------------------------------------------------}
  
  TSearchCallSetsRequest = Class(TGoogleBaseObject)
  Private
    Fname : string;
    FpageSize : integer;
    FpageToken : string;
    FvariantSetIds : TSearchCallSetsRequestvariantSetIds;
  Protected
    //Property setters
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetpageSize(AIndex : Integer; AValue : integer); virtual;
    Procedure SetpageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetvariantSetIds(AIndex : Integer; AValue : TSearchCallSetsRequestvariantSetIds); virtual;
  Public
  Published
    Property name : string Index 0 Read Fname Write Setname;
    Property pageSize : integer Index 8 Read FpageSize Write SetpageSize;
    Property pageToken : string Index 16 Read FpageToken Write SetpageToken;
    Property variantSetIds : TSearchCallSetsRequestvariantSetIds Index 24 Read FvariantSetIds Write SetvariantSetIds;
  end;
  TSearchCallSetsRequestClass = Class of TSearchCallSetsRequest;
  
  { --------------------------------------------------------------------
    TSearchCallSetsRequestvariantSetIds
    --------------------------------------------------------------------}
  
  TSearchCallSetsRequestvariantSetIds = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TSearchCallSetsRequestvariantSetIdsClass = Class of TSearchCallSetsRequestvariantSetIds;
  
  { --------------------------------------------------------------------
    TSearchCallSetsResponse
    --------------------------------------------------------------------}
  
  TSearchCallSetsResponse = Class(TGoogleBaseObject)
  Private
    FcallSets : TSearchCallSetsResponsecallSets;
    FnextPageToken : string;
  Protected
    //Property setters
    Procedure SetcallSets(AIndex : Integer; AValue : TSearchCallSetsResponsecallSets); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property callSets : TSearchCallSetsResponsecallSets Index 0 Read FcallSets Write SetcallSets;
    Property nextPageToken : string Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TSearchCallSetsResponseClass = Class of TSearchCallSetsResponse;
  
  { --------------------------------------------------------------------
    TSearchCallSetsResponsecallSets
    --------------------------------------------------------------------}
  
  TSearchCallSetsResponsecallSets = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TSearchCallSetsResponsecallSetsClass = Class of TSearchCallSetsResponsecallSets;
  
  { --------------------------------------------------------------------
    TSearchJobsRequest
    --------------------------------------------------------------------}
  
  TSearchJobsRequest = Class(TGoogleBaseObject)
  Private
    FcreatedAfter : string;
    FcreatedBefore : string;
    FpageSize : integer;
    FpageToken : string;
    FprojectNumber : string;
    Fstatus : TSearchJobsRequeststatus;
  Protected
    //Property setters
    Procedure SetcreatedAfter(AIndex : Integer; AValue : string); virtual;
    Procedure SetcreatedBefore(AIndex : Integer; AValue : string); virtual;
    Procedure SetpageSize(AIndex : Integer; AValue : integer); virtual;
    Procedure SetpageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetprojectNumber(AIndex : Integer; AValue : string); virtual;
    Procedure Setstatus(AIndex : Integer; AValue : TSearchJobsRequeststatus); virtual;
  Public
  Published
    Property createdAfter : string Index 0 Read FcreatedAfter Write SetcreatedAfter;
    Property createdBefore : string Index 8 Read FcreatedBefore Write SetcreatedBefore;
    Property pageSize : integer Index 16 Read FpageSize Write SetpageSize;
    Property pageToken : string Index 24 Read FpageToken Write SetpageToken;
    Property projectNumber : string Index 32 Read FprojectNumber Write SetprojectNumber;
    Property status : TSearchJobsRequeststatus Index 40 Read Fstatus Write Setstatus;
  end;
  TSearchJobsRequestClass = Class of TSearchJobsRequest;
  
  { --------------------------------------------------------------------
    TSearchJobsRequeststatus
    --------------------------------------------------------------------}
  
  TSearchJobsRequeststatus = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TSearchJobsRequeststatusClass = Class of TSearchJobsRequeststatus;
  
  { --------------------------------------------------------------------
    TSearchJobsResponse
    --------------------------------------------------------------------}
  
  TSearchJobsResponse = Class(TGoogleBaseObject)
  Private
    Fjobs : TSearchJobsResponsejobs;
    FnextPageToken : string;
  Protected
    //Property setters
    Procedure Setjobs(AIndex : Integer; AValue : TSearchJobsResponsejobs); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property jobs : TSearchJobsResponsejobs Index 0 Read Fjobs Write Setjobs;
    Property nextPageToken : string Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TSearchJobsResponseClass = Class of TSearchJobsResponse;
  
  { --------------------------------------------------------------------
    TSearchJobsResponsejobs
    --------------------------------------------------------------------}
  
  TSearchJobsResponsejobs = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TSearchJobsResponsejobsClass = Class of TSearchJobsResponsejobs;
  
  { --------------------------------------------------------------------
    TSearchReadGroupSetsRequest
    --------------------------------------------------------------------}
  
  TSearchReadGroupSetsRequest = Class(TGoogleBaseObject)
  Private
    FdatasetIds : TSearchReadGroupSetsRequestdatasetIds;
    Fname : string;
    FpageSize : integer;
    FpageToken : string;
  Protected
    //Property setters
    Procedure SetdatasetIds(AIndex : Integer; AValue : TSearchReadGroupSetsRequestdatasetIds); virtual;
    Procedure Setname(AIndex : Integer; AValue : string); virtual;
    Procedure SetpageSize(AIndex : Integer; AValue : integer); virtual;
    Procedure SetpageToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property datasetIds : TSearchReadGroupSetsRequestdatasetIds Index 0 Read FdatasetIds Write SetdatasetIds;
    Property name : string Index 8 Read Fname Write Setname;
    Property pageSize : integer Index 16 Read FpageSize Write SetpageSize;
    Property pageToken : string Index 24 Read FpageToken Write SetpageToken;
  end;
  TSearchReadGroupSetsRequestClass = Class of TSearchReadGroupSetsRequest;
  
  { --------------------------------------------------------------------
    TSearchReadGroupSetsRequestdatasetIds
    --------------------------------------------------------------------}
  
  TSearchReadGroupSetsRequestdatasetIds = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TSearchReadGroupSetsRequestdatasetIdsClass = Class of TSearchReadGroupSetsRequestdatasetIds;
  
  { --------------------------------------------------------------------
    TSearchReadGroupSetsResponse
    --------------------------------------------------------------------}
  
  TSearchReadGroupSetsResponse = Class(TGoogleBaseObject)
  Private
    FnextPageToken : string;
    FreadGroupSets : TSearchReadGroupSetsResponsereadGroupSets;
  Protected
    //Property setters
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetreadGroupSets(AIndex : Integer; AValue : TSearchReadGroupSetsResponsereadGroupSets); virtual;
  Public
  Published
    Property nextPageToken : string Index 0 Read FnextPageToken Write SetnextPageToken;
    Property readGroupSets : TSearchReadGroupSetsResponsereadGroupSets Index 8 Read FreadGroupSets Write SetreadGroupSets;
  end;
  TSearchReadGroupSetsResponseClass = Class of TSearchReadGroupSetsResponse;
  
  { --------------------------------------------------------------------
    TSearchReadGroupSetsResponsereadGroupSets
    --------------------------------------------------------------------}
  
  TSearchReadGroupSetsResponsereadGroupSets = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TSearchReadGroupSetsResponsereadGroupSetsClass = Class of TSearchReadGroupSetsResponsereadGroupSets;
  
  { --------------------------------------------------------------------
    TSearchReadsRequest
    --------------------------------------------------------------------}
  
  TSearchReadsRequest = Class(TGoogleBaseObject)
  Private
    F_end : string;
    FpageSize : integer;
    FpageToken : string;
    FreadGroupIds : TSearchReadsRequestreadGroupIds;
    FreadGroupSetIds : TSearchReadsRequestreadGroupSetIds;
    FreferenceName : string;
    Fstart : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_end(AIndex : Integer; AValue : string); virtual;
    Procedure SetpageSize(AIndex : Integer; AValue : integer); virtual;
    Procedure SetpageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetreadGroupIds(AIndex : Integer; AValue : TSearchReadsRequestreadGroupIds); virtual;
    Procedure SetreadGroupSetIds(AIndex : Integer; AValue : TSearchReadsRequestreadGroupSetIds); virtual;
    Procedure SetreferenceName(AIndex : Integer; AValue : string); virtual;
    Procedure Setstart(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property _end : string Index 0 Read F_end Write Set_end;
    Property pageSize : integer Index 8 Read FpageSize Write SetpageSize;
    Property pageToken : string Index 16 Read FpageToken Write SetpageToken;
    Property readGroupIds : TSearchReadsRequestreadGroupIds Index 24 Read FreadGroupIds Write SetreadGroupIds;
    Property readGroupSetIds : TSearchReadsRequestreadGroupSetIds Index 32 Read FreadGroupSetIds Write SetreadGroupSetIds;
    Property referenceName : string Index 40 Read FreferenceName Write SetreferenceName;
    Property start : string Index 48 Read Fstart Write Setstart;
  end;
  TSearchReadsRequestClass = Class of TSearchReadsRequest;
  
  { --------------------------------------------------------------------
    TSearchReadsRequestreadGroupIds
    --------------------------------------------------------------------}
  
  TSearchReadsRequestreadGroupIds = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TSearchReadsRequestreadGroupIdsClass = Class of TSearchReadsRequestreadGroupIds;
  
  { --------------------------------------------------------------------
    TSearchReadsRequestreadGroupSetIds
    --------------------------------------------------------------------}
  
  TSearchReadsRequestreadGroupSetIds = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TSearchReadsRequestreadGroupSetIdsClass = Class of TSearchReadsRequestreadGroupSetIds;
  
  { --------------------------------------------------------------------
    TSearchReadsResponse
    --------------------------------------------------------------------}
  
  TSearchReadsResponse = Class(TGoogleBaseObject)
  Private
    Falignments : TSearchReadsResponsealignments;
    FnextPageToken : string;
  Protected
    //Property setters
    Procedure Setalignments(AIndex : Integer; AValue : TSearchReadsResponsealignments); virtual;
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property alignments : TSearchReadsResponsealignments Index 0 Read Falignments Write Setalignments;
    Property nextPageToken : string Index 8 Read FnextPageToken Write SetnextPageToken;
  end;
  TSearchReadsResponseClass = Class of TSearchReadsResponse;
  
  { --------------------------------------------------------------------
    TSearchReadsResponsealignments
    --------------------------------------------------------------------}
  
  TSearchReadsResponsealignments = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TSearchReadsResponsealignmentsClass = Class of TSearchReadsResponsealignments;
  
  { --------------------------------------------------------------------
    TSearchReferenceSetsRequest
    --------------------------------------------------------------------}
  
  TSearchReferenceSetsRequest = Class(TGoogleBaseObject)
  Private
    Faccessions : TSearchReferenceSetsRequestaccessions;
    FassemblyId : string;
    Fmd5checksums : TSearchReferenceSetsRequestmd5checksums;
    FpageSize : integer;
    FpageToken : string;
  Protected
    //Property setters
    Procedure Setaccessions(AIndex : Integer; AValue : TSearchReferenceSetsRequestaccessions); virtual;
    Procedure SetassemblyId(AIndex : Integer; AValue : string); virtual;
    Procedure Setmd5checksums(AIndex : Integer; AValue : TSearchReferenceSetsRequestmd5checksums); virtual;
    Procedure SetpageSize(AIndex : Integer; AValue : integer); virtual;
    Procedure SetpageToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property accessions : TSearchReferenceSetsRequestaccessions Index 0 Read Faccessions Write Setaccessions;
    Property assemblyId : string Index 8 Read FassemblyId Write SetassemblyId;
    Property md5checksums : TSearchReferenceSetsRequestmd5checksums Index 16 Read Fmd5checksums Write Setmd5checksums;
    Property pageSize : integer Index 24 Read FpageSize Write SetpageSize;
    Property pageToken : string Index 32 Read FpageToken Write SetpageToken;
  end;
  TSearchReferenceSetsRequestClass = Class of TSearchReferenceSetsRequest;
  
  { --------------------------------------------------------------------
    TSearchReferenceSetsRequestaccessions
    --------------------------------------------------------------------}
  
  TSearchReferenceSetsRequestaccessions = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TSearchReferenceSetsRequestaccessionsClass = Class of TSearchReferenceSetsRequestaccessions;
  
  { --------------------------------------------------------------------
    TSearchReferenceSetsRequestmd5checksums
    --------------------------------------------------------------------}
  
  TSearchReferenceSetsRequestmd5checksums = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TSearchReferenceSetsRequestmd5checksumsClass = Class of TSearchReferenceSetsRequestmd5checksums;
  
  { --------------------------------------------------------------------
    TSearchReferenceSetsResponse
    --------------------------------------------------------------------}
  
  TSearchReferenceSetsResponse = Class(TGoogleBaseObject)
  Private
    FnextPageToken : string;
    FreferenceSets : TSearchReferenceSetsResponsereferenceSets;
  Protected
    //Property setters
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetreferenceSets(AIndex : Integer; AValue : TSearchReferenceSetsResponsereferenceSets); virtual;
  Public
  Published
    Property nextPageToken : string Index 0 Read FnextPageToken Write SetnextPageToken;
    Property referenceSets : TSearchReferenceSetsResponsereferenceSets Index 8 Read FreferenceSets Write SetreferenceSets;
  end;
  TSearchReferenceSetsResponseClass = Class of TSearchReferenceSetsResponse;
  
  { --------------------------------------------------------------------
    TSearchReferenceSetsResponsereferenceSets
    --------------------------------------------------------------------}
  
  TSearchReferenceSetsResponsereferenceSets = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TSearchReferenceSetsResponsereferenceSetsClass = Class of TSearchReferenceSetsResponsereferenceSets;
  
  { --------------------------------------------------------------------
    TSearchReferencesRequest
    --------------------------------------------------------------------}
  
  TSearchReferencesRequest = Class(TGoogleBaseObject)
  Private
    Faccessions : TSearchReferencesRequestaccessions;
    Fmd5checksums : TSearchReferencesRequestmd5checksums;
    FpageSize : integer;
    FpageToken : string;
    FreferenceSetId : string;
  Protected
    //Property setters
    Procedure Setaccessions(AIndex : Integer; AValue : TSearchReferencesRequestaccessions); virtual;
    Procedure Setmd5checksums(AIndex : Integer; AValue : TSearchReferencesRequestmd5checksums); virtual;
    Procedure SetpageSize(AIndex : Integer; AValue : integer); virtual;
    Procedure SetpageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetreferenceSetId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property accessions : TSearchReferencesRequestaccessions Index 0 Read Faccessions Write Setaccessions;
    Property md5checksums : TSearchReferencesRequestmd5checksums Index 8 Read Fmd5checksums Write Setmd5checksums;
    Property pageSize : integer Index 16 Read FpageSize Write SetpageSize;
    Property pageToken : string Index 24 Read FpageToken Write SetpageToken;
    Property referenceSetId : string Index 32 Read FreferenceSetId Write SetreferenceSetId;
  end;
  TSearchReferencesRequestClass = Class of TSearchReferencesRequest;
  
  { --------------------------------------------------------------------
    TSearchReferencesRequestaccessions
    --------------------------------------------------------------------}
  
  TSearchReferencesRequestaccessions = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TSearchReferencesRequestaccessionsClass = Class of TSearchReferencesRequestaccessions;
  
  { --------------------------------------------------------------------
    TSearchReferencesRequestmd5checksums
    --------------------------------------------------------------------}
  
  TSearchReferencesRequestmd5checksums = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TSearchReferencesRequestmd5checksumsClass = Class of TSearchReferencesRequestmd5checksums;
  
  { --------------------------------------------------------------------
    TSearchReferencesResponse
    --------------------------------------------------------------------}
  
  TSearchReferencesResponse = Class(TGoogleBaseObject)
  Private
    FnextPageToken : string;
    Freferences : TSearchReferencesResponsereferences;
  Protected
    //Property setters
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure Setreferences(AIndex : Integer; AValue : TSearchReferencesResponsereferences); virtual;
  Public
  Published
    Property nextPageToken : string Index 0 Read FnextPageToken Write SetnextPageToken;
    Property references : TSearchReferencesResponsereferences Index 8 Read Freferences Write Setreferences;
  end;
  TSearchReferencesResponseClass = Class of TSearchReferencesResponse;
  
  { --------------------------------------------------------------------
    TSearchReferencesResponsereferences
    --------------------------------------------------------------------}
  
  TSearchReferencesResponsereferences = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TSearchReferencesResponsereferencesClass = Class of TSearchReferencesResponsereferences;
  
  { --------------------------------------------------------------------
    TSearchVariantSetsRequest
    --------------------------------------------------------------------}
  
  TSearchVariantSetsRequest = Class(TGoogleBaseObject)
  Private
    FdatasetIds : TSearchVariantSetsRequestdatasetIds;
    FpageSize : integer;
    FpageToken : string;
  Protected
    //Property setters
    Procedure SetdatasetIds(AIndex : Integer; AValue : TSearchVariantSetsRequestdatasetIds); virtual;
    Procedure SetpageSize(AIndex : Integer; AValue : integer); virtual;
    Procedure SetpageToken(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property datasetIds : TSearchVariantSetsRequestdatasetIds Index 0 Read FdatasetIds Write SetdatasetIds;
    Property pageSize : integer Index 8 Read FpageSize Write SetpageSize;
    Property pageToken : string Index 16 Read FpageToken Write SetpageToken;
  end;
  TSearchVariantSetsRequestClass = Class of TSearchVariantSetsRequest;
  
  { --------------------------------------------------------------------
    TSearchVariantSetsRequestdatasetIds
    --------------------------------------------------------------------}
  
  TSearchVariantSetsRequestdatasetIds = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TSearchVariantSetsRequestdatasetIdsClass = Class of TSearchVariantSetsRequestdatasetIds;
  
  { --------------------------------------------------------------------
    TSearchVariantSetsResponse
    --------------------------------------------------------------------}
  
  TSearchVariantSetsResponse = Class(TGoogleBaseObject)
  Private
    FnextPageToken : string;
    FvariantSets : TSearchVariantSetsResponsevariantSets;
  Protected
    //Property setters
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetvariantSets(AIndex : Integer; AValue : TSearchVariantSetsResponsevariantSets); virtual;
  Public
  Published
    Property nextPageToken : string Index 0 Read FnextPageToken Write SetnextPageToken;
    Property variantSets : TSearchVariantSetsResponsevariantSets Index 8 Read FvariantSets Write SetvariantSets;
  end;
  TSearchVariantSetsResponseClass = Class of TSearchVariantSetsResponse;
  
  { --------------------------------------------------------------------
    TSearchVariantSetsResponsevariantSets
    --------------------------------------------------------------------}
  
  TSearchVariantSetsResponsevariantSets = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TSearchVariantSetsResponsevariantSetsClass = Class of TSearchVariantSetsResponsevariantSets;
  
  { --------------------------------------------------------------------
    TSearchVariantsRequest
    --------------------------------------------------------------------}
  
  TSearchVariantsRequest = Class(TGoogleBaseObject)
  Private
    FcallSetIds : TSearchVariantsRequestcallSetIds;
    F_end : string;
    FmaxCalls : integer;
    FpageSize : integer;
    FpageToken : string;
    FreferenceName : string;
    Fstart : string;
    FvariantName : string;
    FvariantSetIds : TSearchVariantsRequestvariantSetIds;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetcallSetIds(AIndex : Integer; AValue : TSearchVariantsRequestcallSetIds); virtual;
    Procedure Set_end(AIndex : Integer; AValue : string); virtual;
    Procedure SetmaxCalls(AIndex : Integer; AValue : integer); virtual;
    Procedure SetpageSize(AIndex : Integer; AValue : integer); virtual;
    Procedure SetpageToken(AIndex : Integer; AValue : string); virtual;
    Procedure SetreferenceName(AIndex : Integer; AValue : string); virtual;
    Procedure Setstart(AIndex : Integer; AValue : string); virtual;
    Procedure SetvariantName(AIndex : Integer; AValue : string); virtual;
    Procedure SetvariantSetIds(AIndex : Integer; AValue : TSearchVariantsRequestvariantSetIds); virtual;
  Public
  Published
    Property callSetIds : TSearchVariantsRequestcallSetIds Index 0 Read FcallSetIds Write SetcallSetIds;
    Property _end : string Index 8 Read F_end Write Set_end;
    Property maxCalls : integer Index 16 Read FmaxCalls Write SetmaxCalls;
    Property pageSize : integer Index 24 Read FpageSize Write SetpageSize;
    Property pageToken : string Index 32 Read FpageToken Write SetpageToken;
    Property referenceName : string Index 40 Read FreferenceName Write SetreferenceName;
    Property start : string Index 48 Read Fstart Write Setstart;
    Property variantName : string Index 56 Read FvariantName Write SetvariantName;
    Property variantSetIds : TSearchVariantsRequestvariantSetIds Index 64 Read FvariantSetIds Write SetvariantSetIds;
  end;
  TSearchVariantsRequestClass = Class of TSearchVariantsRequest;
  
  { --------------------------------------------------------------------
    TSearchVariantsRequestcallSetIds
    --------------------------------------------------------------------}
  
  TSearchVariantsRequestcallSetIds = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TSearchVariantsRequestcallSetIdsClass = Class of TSearchVariantsRequestcallSetIds;
  
  { --------------------------------------------------------------------
    TSearchVariantsRequestvariantSetIds
    --------------------------------------------------------------------}
  
  TSearchVariantsRequestvariantSetIds = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TSearchVariantsRequestvariantSetIdsClass = Class of TSearchVariantsRequestvariantSetIds;
  
  { --------------------------------------------------------------------
    TSearchVariantsResponse
    --------------------------------------------------------------------}
  
  TSearchVariantsResponse = Class(TGoogleBaseObject)
  Private
    FnextPageToken : string;
    Fvariants : TSearchVariantsResponsevariants;
  Protected
    //Property setters
    Procedure SetnextPageToken(AIndex : Integer; AValue : string); virtual;
    Procedure Setvariants(AIndex : Integer; AValue : TSearchVariantsResponsevariants); virtual;
  Public
  Published
    Property nextPageToken : string Index 0 Read FnextPageToken Write SetnextPageToken;
    Property variants : TSearchVariantsResponsevariants Index 8 Read Fvariants Write Setvariants;
  end;
  TSearchVariantsResponseClass = Class of TSearchVariantsResponse;
  
  { --------------------------------------------------------------------
    TSearchVariantsResponsevariants
    --------------------------------------------------------------------}
  
  TSearchVariantsResponsevariants = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TSearchVariantsResponsevariantsClass = Class of TSearchVariantsResponsevariants;
  
  { --------------------------------------------------------------------
    TStreamReadsRequest
    --------------------------------------------------------------------}
  
  TStreamReadsRequest = Class(TGoogleBaseObject)
  Private
    F_end : string;
    FreadGroupSetIds : TStreamReadsRequestreadGroupSetIds;
    FreferenceName : string;
    Fstart : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_end(AIndex : Integer; AValue : string); virtual;
    Procedure SetreadGroupSetIds(AIndex : Integer; AValue : TStreamReadsRequestreadGroupSetIds); virtual;
    Procedure SetreferenceName(AIndex : Integer; AValue : string); virtual;
    Procedure Setstart(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property _end : string Index 0 Read F_end Write Set_end;
    Property readGroupSetIds : TStreamReadsRequestreadGroupSetIds Index 8 Read FreadGroupSetIds Write SetreadGroupSetIds;
    Property referenceName : string Index 16 Read FreferenceName Write SetreferenceName;
    Property start : string Index 24 Read Fstart Write Setstart;
  end;
  TStreamReadsRequestClass = Class of TStreamReadsRequest;
  
  { --------------------------------------------------------------------
    TStreamReadsRequestreadGroupSetIds
    --------------------------------------------------------------------}
  
  TStreamReadsRequestreadGroupSetIds = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TStreamReadsRequestreadGroupSetIdsClass = Class of TStreamReadsRequestreadGroupSetIds;
  
  { --------------------------------------------------------------------
    TStreamReadsResponse
    --------------------------------------------------------------------}
  
  TStreamReadsResponse = Class(TGoogleBaseObject)
  Private
    Falignments : TStreamReadsResponsealignments;
  Protected
    //Property setters
    Procedure Setalignments(AIndex : Integer; AValue : TStreamReadsResponsealignments); virtual;
  Public
  Published
    Property alignments : TStreamReadsResponsealignments Index 0 Read Falignments Write Setalignments;
  end;
  TStreamReadsResponseClass = Class of TStreamReadsResponse;
  
  { --------------------------------------------------------------------
    TStreamReadsResponsealignments
    --------------------------------------------------------------------}
  
  TStreamReadsResponsealignments = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TStreamReadsResponsealignmentsClass = Class of TStreamReadsResponsealignments;
  
  { --------------------------------------------------------------------
    TTranscript
    --------------------------------------------------------------------}
  
  TTranscript = Class(TGoogleBaseObject)
  Private
    FcodingSequence : TTranscriptCodingSequence;
    Fexons : TTranscriptexons;
    FgeneId : string;
  Protected
    //Property setters
    Procedure SetcodingSequence(AIndex : Integer; AValue : TTranscriptCodingSequence); virtual;
    Procedure Setexons(AIndex : Integer; AValue : TTranscriptexons); virtual;
    Procedure SetgeneId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property codingSequence : TTranscriptCodingSequence Index 0 Read FcodingSequence Write SetcodingSequence;
    Property exons : TTranscriptexons Index 8 Read Fexons Write Setexons;
    Property geneId : string Index 16 Read FgeneId Write SetgeneId;
  end;
  TTranscriptClass = Class of TTranscript;
  
  { --------------------------------------------------------------------
    TTranscriptexons
    --------------------------------------------------------------------}
  
  TTranscriptexons = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TTranscriptexonsClass = Class of TTranscriptexons;
  
  { --------------------------------------------------------------------
    TTranscriptCodingSequence
    --------------------------------------------------------------------}
  
  TTranscriptCodingSequence = Class(TGoogleBaseObject)
  Private
    F_end : string;
    Fstart : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_end(AIndex : Integer; AValue : string); virtual;
    Procedure Setstart(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property _end : string Index 0 Read F_end Write Set_end;
    Property start : string Index 8 Read Fstart Write Setstart;
  end;
  TTranscriptCodingSequenceClass = Class of TTranscriptCodingSequence;
  
  { --------------------------------------------------------------------
    TTranscriptExon
    --------------------------------------------------------------------}
  
  TTranscriptExon = Class(TGoogleBaseObject)
  Private
    F_end : string;
    Fframe : TInt32Value;
    Fstart : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure Set_end(AIndex : Integer; AValue : string); virtual;
    Procedure Setframe(AIndex : Integer; AValue : TInt32Value); virtual;
    Procedure Setstart(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property _end : string Index 0 Read F_end Write Set_end;
    Property frame : TInt32Value Index 8 Read Fframe Write Setframe;
    Property start : string Index 16 Read Fstart Write Setstart;
  end;
  TTranscriptExonClass = Class of TTranscriptExon;
  
  { --------------------------------------------------------------------
    TVariant
    --------------------------------------------------------------------}
  
  TVariant = Class(TGoogleBaseObject)
  Private
    FalternateBases : TVariantalternateBases;
    Fcalls : TVariantcalls;
    Fcreated : string;
    F_end : string;
    Ffilter : TVariantfilter;
    Fid : string;
    Finfo : TVariantinfo;
    Fnames : TVariantnames;
    Fquality : double;
    FreferenceBases : string;
    FreferenceName : string;
    Fstart : string;
    FvariantSetId : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetalternateBases(AIndex : Integer; AValue : TVariantalternateBases); virtual;
    Procedure Setcalls(AIndex : Integer; AValue : TVariantcalls); virtual;
    Procedure Setcreated(AIndex : Integer; AValue : string); virtual;
    Procedure Set_end(AIndex : Integer; AValue : string); virtual;
    Procedure Setfilter(AIndex : Integer; AValue : TVariantfilter); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setinfo(AIndex : Integer; AValue : TVariantinfo); virtual;
    Procedure Setnames(AIndex : Integer; AValue : TVariantnames); virtual;
    Procedure Setquality(AIndex : Integer; AValue : double); virtual;
    Procedure SetreferenceBases(AIndex : Integer; AValue : string); virtual;
    Procedure SetreferenceName(AIndex : Integer; AValue : string); virtual;
    Procedure Setstart(AIndex : Integer; AValue : string); virtual;
    Procedure SetvariantSetId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property alternateBases : TVariantalternateBases Index 0 Read FalternateBases Write SetalternateBases;
    Property calls : TVariantcalls Index 8 Read Fcalls Write Setcalls;
    Property created : string Index 16 Read Fcreated Write Setcreated;
    Property _end : string Index 24 Read F_end Write Set_end;
    Property filter : TVariantfilter Index 32 Read Ffilter Write Setfilter;
    Property id : string Index 40 Read Fid Write Setid;
    Property info : TVariantinfo Index 48 Read Finfo Write Setinfo;
    Property names : TVariantnames Index 56 Read Fnames Write Setnames;
    Property quality : double Index 64 Read Fquality Write Setquality;
    Property referenceBases : string Index 72 Read FreferenceBases Write SetreferenceBases;
    Property referenceName : string Index 80 Read FreferenceName Write SetreferenceName;
    Property start : string Index 88 Read Fstart Write Setstart;
    Property variantSetId : string Index 96 Read FvariantSetId Write SetvariantSetId;
  end;
  TVariantClass = Class of TVariant;
  
  { --------------------------------------------------------------------
    TVariantalternateBases
    --------------------------------------------------------------------}
  
  TVariantalternateBases = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TVariantalternateBasesClass = Class of TVariantalternateBases;
  
  { --------------------------------------------------------------------
    TVariantcalls
    --------------------------------------------------------------------}
  
  TVariantcalls = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TVariantcallsClass = Class of TVariantcalls;
  
  { --------------------------------------------------------------------
    TVariantfilter
    --------------------------------------------------------------------}
  
  TVariantfilter = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TVariantfilterClass = Class of TVariantfilter;
  
  { --------------------------------------------------------------------
    TVariantinfo
    --------------------------------------------------------------------}
  
  TVariantinfo = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
    Class Function AllowAdditionalProperties : Boolean; override;
  Published
  end;
  TVariantinfoClass = Class of TVariantinfo;
  
  { --------------------------------------------------------------------
    TVariantnames
    --------------------------------------------------------------------}
  
  TVariantnames = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TVariantnamesClass = Class of TVariantnames;
  
  { --------------------------------------------------------------------
    TVariantAnnotation
    --------------------------------------------------------------------}
  
  TVariantAnnotation = Class(TGoogleBaseObject)
  Private
    FalternateBases : string;
    FclinicalSignificance : string;
    Fconditions : TVariantAnnotationconditions;
    Feffect : string;
    FgeneId : string;
    FtranscriptIds : TVariantAnnotationtranscriptIds;
    F_type : string;
  Protected
    Class Function ExportPropertyName(Const AName : String) : string; override;
    //Property setters
    Procedure SetalternateBases(AIndex : Integer; AValue : string); virtual;
    Procedure SetclinicalSignificance(AIndex : Integer; AValue : string); virtual;
    Procedure Setconditions(AIndex : Integer; AValue : TVariantAnnotationconditions); virtual;
    Procedure Seteffect(AIndex : Integer; AValue : string); virtual;
    Procedure SetgeneId(AIndex : Integer; AValue : string); virtual;
    Procedure SettranscriptIds(AIndex : Integer; AValue : TVariantAnnotationtranscriptIds); virtual;
    Procedure Set_type(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property alternateBases : string Index 0 Read FalternateBases Write SetalternateBases;
    Property clinicalSignificance : string Index 8 Read FclinicalSignificance Write SetclinicalSignificance;
    Property conditions : TVariantAnnotationconditions Index 16 Read Fconditions Write Setconditions;
    Property effect : string Index 24 Read Feffect Write Seteffect;
    Property geneId : string Index 32 Read FgeneId Write SetgeneId;
    Property transcriptIds : TVariantAnnotationtranscriptIds Index 40 Read FtranscriptIds Write SettranscriptIds;
    Property _type : string Index 48 Read F_type Write Set_type;
  end;
  TVariantAnnotationClass = Class of TVariantAnnotation;
  
  { --------------------------------------------------------------------
    TVariantAnnotationconditions
    --------------------------------------------------------------------}
  
  TVariantAnnotationconditions = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TVariantAnnotationconditionsClass = Class of TVariantAnnotationconditions;
  
  { --------------------------------------------------------------------
    TVariantAnnotationtranscriptIds
    --------------------------------------------------------------------}
  
  TVariantAnnotationtranscriptIds = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TVariantAnnotationtranscriptIdsClass = Class of TVariantAnnotationtranscriptIds;
  
  { --------------------------------------------------------------------
    TVariantAnnotationCondition
    --------------------------------------------------------------------}
  
  TVariantAnnotationCondition = Class(TGoogleBaseObject)
  Private
    FconceptId : string;
    FexternalIds : TVariantAnnotationConditionexternalIds;
    Fnames : TVariantAnnotationConditionnames;
    FomimId : string;
  Protected
    //Property setters
    Procedure SetconceptId(AIndex : Integer; AValue : string); virtual;
    Procedure SetexternalIds(AIndex : Integer; AValue : TVariantAnnotationConditionexternalIds); virtual;
    Procedure Setnames(AIndex : Integer; AValue : TVariantAnnotationConditionnames); virtual;
    Procedure SetomimId(AIndex : Integer; AValue : string); virtual;
  Public
  Published
    Property conceptId : string Index 0 Read FconceptId Write SetconceptId;
    Property externalIds : TVariantAnnotationConditionexternalIds Index 8 Read FexternalIds Write SetexternalIds;
    Property names : TVariantAnnotationConditionnames Index 16 Read Fnames Write Setnames;
    Property omimId : string Index 24 Read FomimId Write SetomimId;
  end;
  TVariantAnnotationConditionClass = Class of TVariantAnnotationCondition;
  
  { --------------------------------------------------------------------
    TVariantAnnotationConditionexternalIds
    --------------------------------------------------------------------}
  
  TVariantAnnotationConditionexternalIds = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TVariantAnnotationConditionexternalIdsClass = Class of TVariantAnnotationConditionexternalIds;
  
  { --------------------------------------------------------------------
    TVariantAnnotationConditionnames
    --------------------------------------------------------------------}
  
  TVariantAnnotationConditionnames = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TVariantAnnotationConditionnamesClass = Class of TVariantAnnotationConditionnames;
  
  { --------------------------------------------------------------------
    TVariantSet
    --------------------------------------------------------------------}
  
  TVariantSet = Class(TGoogleBaseObject)
  Private
    FdatasetId : string;
    Fid : string;
    Fmetadata : TVariantSetmetadata;
    FreferenceBounds : TVariantSetreferenceBounds;
  Protected
    //Property setters
    Procedure SetdatasetId(AIndex : Integer; AValue : string); virtual;
    Procedure Setid(AIndex : Integer; AValue : string); virtual;
    Procedure Setmetadata(AIndex : Integer; AValue : TVariantSetmetadata); virtual;
    Procedure SetreferenceBounds(AIndex : Integer; AValue : TVariantSetreferenceBounds); virtual;
  Public
  Published
    Property datasetId : string Index 0 Read FdatasetId Write SetdatasetId;
    Property id : string Index 8 Read Fid Write Setid;
    Property metadata : TVariantSetmetadata Index 16 Read Fmetadata Write Setmetadata;
    Property referenceBounds : TVariantSetreferenceBounds Index 24 Read FreferenceBounds Write SetreferenceBounds;
  end;
  TVariantSetClass = Class of TVariantSet;
  
  { --------------------------------------------------------------------
    TVariantSetmetadata
    --------------------------------------------------------------------}
  
  TVariantSetmetadata = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TVariantSetmetadataClass = Class of TVariantSetmetadata;
  
  { --------------------------------------------------------------------
    TVariantSetreferenceBounds
    --------------------------------------------------------------------}
  
  TVariantSetreferenceBounds = Class(TGoogleBaseObject)
  Private
  Protected
    //Property setters
  Public
  Published
  end;
  TVariantSetreferenceBoundsClass = Class of TVariantSetreferenceBounds;
  
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
    pageToken : string;
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
    TExperimentalResource
    --------------------------------------------------------------------}
  
  TExperimentalResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
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
    TReadgroupsetsResource
    --------------------------------------------------------------------}
  
  TReadgroupsetsResource = Class(TGoogleResource)
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
    TReferencesResource
    --------------------------------------------------------------------}
  
  TReferencesResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Get(referenceId: string) : TReference;
    Function Search(aSearchReferencesRequest : TSearchReferencesRequest) : TSearchReferencesResponse;
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
    TStreamingReadstoreResource
    --------------------------------------------------------------------}
  
  TStreamingReadstoreResource = Class(TGoogleResource)
  Public
    Class Function ResourceName : String; override;
    Class Function DefaultAPI : TGoogleAPIClass; override;
    Function Streamreads(aStreamReadsRequest : TStreamReadsRequest) : TStreamReadsResponse;
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
    FExperimentalInstance : TExperimentalResource;
    FJobsInstance : TJobsResource;
    FReadgroupsetsInstance : TReadgroupsetsResource;
    FReadsInstance : TReadsResource;
    FReferencesInstance : TReferencesResource;
    FReferencesetsInstance : TReferencesetsResource;
    FStreamingReadstoreInstance : TStreamingReadstoreResource;
    FVariantsInstance : TVariantsResource;
    FVariantsetsInstance : TVariantsetsResource;
    Function GetAnnotationSetsInstance : TAnnotationSetsResource;virtual;
    Function GetAnnotationsInstance : TAnnotationsResource;virtual;
    Function GetCallsetsInstance : TCallsetsResource;virtual;
    Function GetDatasetsInstance : TDatasetsResource;virtual;
    Function GetExperimentalInstance : TExperimentalResource;virtual;
    Function GetJobsInstance : TJobsResource;virtual;
    Function GetReadgroupsetsInstance : TReadgroupsetsResource;virtual;
    Function GetReadsInstance : TReadsResource;virtual;
    Function GetReferencesInstance : TReferencesResource;virtual;
    Function GetReferencesetsInstance : TReferencesetsResource;virtual;
    Function GetStreamingReadstoreInstance : TStreamingReadstoreResource;virtual;
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
    Function CreateExperimentalResource(AOwner : TComponent) : TExperimentalResource;virtual;overload;
    Function CreateExperimentalResource : TExperimentalResource;virtual;overload;
    Function CreateJobsResource(AOwner : TComponent) : TJobsResource;virtual;overload;
    Function CreateJobsResource : TJobsResource;virtual;overload;
    Function CreateReadgroupsetsResource(AOwner : TComponent) : TReadgroupsetsResource;virtual;overload;
    Function CreateReadgroupsetsResource : TReadgroupsetsResource;virtual;overload;
    Function CreateReadsResource(AOwner : TComponent) : TReadsResource;virtual;overload;
    Function CreateReadsResource : TReadsResource;virtual;overload;
    Function CreateReferencesResource(AOwner : TComponent) : TReferencesResource;virtual;overload;
    Function CreateReferencesResource : TReferencesResource;virtual;overload;
    Function CreateReferencesetsResource(AOwner : TComponent) : TReferencesetsResource;virtual;overload;
    Function CreateReferencesetsResource : TReferencesetsResource;virtual;overload;
    Function CreateStreamingReadstoreResource(AOwner : TComponent) : TStreamingReadstoreResource;virtual;overload;
    Function CreateStreamingReadstoreResource : TStreamingReadstoreResource;virtual;overload;
    Function CreateVariantsResource(AOwner : TComponent) : TVariantsResource;virtual;overload;
    Function CreateVariantsResource : TVariantsResource;virtual;overload;
    Function CreateVariantsetsResource(AOwner : TComponent) : TVariantsetsResource;virtual;overload;
    Function CreateVariantsetsResource : TVariantsetsResource;virtual;overload;
    //Add default on-demand instances for resources
    Property AnnotationSetsResource : TAnnotationSetsResource Read GetAnnotationSetsInstance;
    Property AnnotationsResource : TAnnotationsResource Read GetAnnotationsInstance;
    Property CallsetsResource : TCallsetsResource Read GetCallsetsInstance;
    Property DatasetsResource : TDatasetsResource Read GetDatasetsInstance;
    Property ExperimentalResource : TExperimentalResource Read GetExperimentalInstance;
    Property JobsResource : TJobsResource Read GetJobsInstance;
    Property ReadgroupsetsResource : TReadgroupsetsResource Read GetReadgroupsetsInstance;
    Property ReadsResource : TReadsResource Read GetReadsInstance;
    Property ReferencesResource : TReferencesResource Read GetReferencesInstance;
    Property ReferencesetsResource : TReferencesetsResource Read GetReferencesetsInstance;
    Property StreamingReadstoreResource : TStreamingReadstoreResource Read GetStreamingReadstoreInstance;
    Property VariantsResource : TVariantsResource Read GetVariantsInstance;
    Property VariantsetsResource : TVariantsetsResource Read GetVariantsetsInstance;
  end;

implementation


{ --------------------------------------------------------------------
  TAlignReadGroupSetsRequest
  --------------------------------------------------------------------}


Procedure TAlignReadGroupSetsRequest.SetbamSourceUris(AIndex : Integer; AValue : TAlignReadGroupSetsRequestbamSourceUris); 

begin
  If (FbamSourceUris=AValue) then exit;
  FbamSourceUris:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAlignReadGroupSetsRequest.SetdatasetId(AIndex : Integer; AValue : string); 

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



Procedure TAlignReadGroupSetsRequest.SetreadGroupSetId(AIndex : Integer; AValue : string); 

begin
  If (FreadGroupSetId=AValue) then exit;
  FreadGroupSetId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAlignReadGroupSetsRequestbamSourceUris
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TAlignReadGroupSetsResponse
  --------------------------------------------------------------------}


Procedure TAlignReadGroupSetsResponse.SetjobId(AIndex : Integer; AValue : string); 

begin
  If (FjobId=AValue) then exit;
  FjobId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TAnnotation
  --------------------------------------------------------------------}


Procedure TAnnotation.SetannotationSetId(AIndex : Integer; AValue : string); 

begin
  If (FannotationSetId=AValue) then exit;
  FannotationSetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotation.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotation.Setinfo(AIndex : Integer; AValue : TAnnotationinfo); 

begin
  If (Finfo=AValue) then exit;
  Finfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotation.Setname(AIndex : Integer; AValue : string); 

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



Procedure TAnnotation.Set_type(AIndex : Integer; AValue : string); 

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
  TAnnotationinfo
  --------------------------------------------------------------------}


Class Function TAnnotationinfo.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TAnnotationSet
  --------------------------------------------------------------------}


Procedure TAnnotationSet.SetdatasetId(AIndex : Integer; AValue : string); 

begin
  If (FdatasetId=AValue) then exit;
  FdatasetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationSet.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationSet.Setinfo(AIndex : Integer; AValue : TAnnotationSetinfo); 

begin
  If (Finfo=AValue) then exit;
  Finfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationSet.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationSet.SetreferenceSetId(AIndex : Integer; AValue : string); 

begin
  If (FreferenceSetId=AValue) then exit;
  FreferenceSetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationSet.SetsourceUri(AIndex : Integer; AValue : string); 

begin
  If (FsourceUri=AValue) then exit;
  FsourceUri:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TAnnotationSet.Set_type(AIndex : Integer; AValue : string); 

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
  TAnnotationSetinfo
  --------------------------------------------------------------------}


Class Function TAnnotationSetinfo.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TBatchAnnotationsResponse
  --------------------------------------------------------------------}


Procedure TBatchAnnotationsResponse.Setentries(AIndex : Integer; AValue : TBatchAnnotationsResponseentries); 

begin
  If (Fentries=AValue) then exit;
  Fentries:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBatchAnnotationsResponseentries
  --------------------------------------------------------------------}




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



Procedure TBatchAnnotationsResponseEntryStatus.Setmessage(AIndex : Integer; AValue : string); 

begin
  If (Fmessage=AValue) then exit;
  Fmessage:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBatchCreateAnnotationsRequest
  --------------------------------------------------------------------}


Procedure TBatchCreateAnnotationsRequest.Setannotations(AIndex : Integer; AValue : TBatchCreateAnnotationsRequestannotations); 

begin
  If (Fannotations=AValue) then exit;
  Fannotations:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TBatchCreateAnnotationsRequestannotations
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TCall
  --------------------------------------------------------------------}


Procedure TCall.SetcallSetId(AIndex : Integer; AValue : string); 

begin
  If (FcallSetId=AValue) then exit;
  FcallSetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCall.SetcallSetName(AIndex : Integer; AValue : string); 

begin
  If (FcallSetName=AValue) then exit;
  FcallSetName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCall.Setgenotype(AIndex : Integer; AValue : TCallgenotype); 

begin
  If (Fgenotype=AValue) then exit;
  Fgenotype:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCall.SetgenotypeLikelihood(AIndex : Integer; AValue : TCallgenotypeLikelihood); 

begin
  If (FgenotypeLikelihood=AValue) then exit;
  FgenotypeLikelihood:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCall.Setinfo(AIndex : Integer; AValue : TCallinfo); 

begin
  If (Finfo=AValue) then exit;
  Finfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCall.Setphaseset(AIndex : Integer; AValue : string); 

begin
  If (Fphaseset=AValue) then exit;
  Fphaseset:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCallgenotype
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TCallgenotypeLikelihood
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TCallinfo
  --------------------------------------------------------------------}


Class Function TCallinfo.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TCallReadGroupSetsRequest
  --------------------------------------------------------------------}


Procedure TCallReadGroupSetsRequest.SetdatasetId(AIndex : Integer; AValue : string); 

begin
  If (FdatasetId=AValue) then exit;
  FdatasetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCallReadGroupSetsRequest.SetreadGroupSetId(AIndex : Integer; AValue : string); 

begin
  If (FreadGroupSetId=AValue) then exit;
  FreadGroupSetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCallReadGroupSetsRequest.SetsourceUris(AIndex : Integer; AValue : TCallReadGroupSetsRequestsourceUris); 

begin
  If (FsourceUris=AValue) then exit;
  FsourceUris:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCallReadGroupSetsRequestsourceUris
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TCallReadGroupSetsResponse
  --------------------------------------------------------------------}


Procedure TCallReadGroupSetsResponse.SetjobId(AIndex : Integer; AValue : string); 

begin
  If (FjobId=AValue) then exit;
  FjobId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCallSet
  --------------------------------------------------------------------}


Procedure TCallSet.Setcreated(AIndex : Integer; AValue : string); 

begin
  If (Fcreated=AValue) then exit;
  Fcreated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCallSet.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCallSet.Setinfo(AIndex : Integer; AValue : TCallSetinfo); 

begin
  If (Finfo=AValue) then exit;
  Finfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCallSet.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCallSet.SetsampleId(AIndex : Integer; AValue : string); 

begin
  If (FsampleId=AValue) then exit;
  FsampleId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCallSet.SetvariantSetIds(AIndex : Integer; AValue : TCallSetvariantSetIds); 

begin
  If (FvariantSetIds=AValue) then exit;
  FvariantSetIds:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TCallSetinfo
  --------------------------------------------------------------------}


Class Function TCallSetinfo.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TCallSetvariantSetIds
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TCigarUnit
  --------------------------------------------------------------------}


Procedure TCigarUnit.Setoperation(AIndex : Integer; AValue : string); 

begin
  If (Foperation=AValue) then exit;
  Foperation:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCigarUnit.SetoperationLength(AIndex : Integer; AValue : string); 

begin
  If (FoperationLength=AValue) then exit;
  FoperationLength:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TCigarUnit.SetreferenceSequence(AIndex : Integer; AValue : string); 

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


Procedure TDataset.Setid(AIndex : Integer; AValue : string); 

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



Procedure TDataset.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TDataset.SetprojectNumber(AIndex : Integer; AValue : string); 

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



Procedure TExperimentalCreateJobRequest.SetgcsOutputPath(AIndex : Integer; AValue : string); 

begin
  If (FgcsOutputPath=AValue) then exit;
  FgcsOutputPath:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperimentalCreateJobRequest.SetpairedSourceUris(AIndex : Integer; AValue : TExperimentalCreateJobRequestpairedSourceUris); 

begin
  If (FpairedSourceUris=AValue) then exit;
  FpairedSourceUris:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperimentalCreateJobRequest.SetprojectNumber(AIndex : Integer; AValue : string); 

begin
  If (FprojectNumber=AValue) then exit;
  FprojectNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExperimentalCreateJobRequest.SetsourceUris(AIndex : Integer; AValue : TExperimentalCreateJobRequestsourceUris); 

begin
  If (FsourceUris=AValue) then exit;
  FsourceUris:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TExperimentalCreateJobRequestpairedSourceUris
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TExperimentalCreateJobRequestsourceUris
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TExperimentalCreateJobResponse
  --------------------------------------------------------------------}


Procedure TExperimentalCreateJobResponse.SetjobId(AIndex : Integer; AValue : string); 

begin
  If (FjobId=AValue) then exit;
  FjobId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TExportReadGroupSetsRequest
  --------------------------------------------------------------------}


Procedure TExportReadGroupSetsRequest.SetexportUri(AIndex : Integer; AValue : string); 

begin
  If (FexportUri=AValue) then exit;
  FexportUri:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExportReadGroupSetsRequest.SetprojectNumber(AIndex : Integer; AValue : string); 

begin
  If (FprojectNumber=AValue) then exit;
  FprojectNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExportReadGroupSetsRequest.SetreadGroupSetIds(AIndex : Integer; AValue : TExportReadGroupSetsRequestreadGroupSetIds); 

begin
  If (FreadGroupSetIds=AValue) then exit;
  FreadGroupSetIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExportReadGroupSetsRequest.SetreferenceNames(AIndex : Integer; AValue : TExportReadGroupSetsRequestreferenceNames); 

begin
  If (FreferenceNames=AValue) then exit;
  FreferenceNames:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TExportReadGroupSetsRequestreadGroupSetIds
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TExportReadGroupSetsRequestreferenceNames
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TExportReadGroupSetsResponse
  --------------------------------------------------------------------}


Procedure TExportReadGroupSetsResponse.SetjobId(AIndex : Integer; AValue : string); 

begin
  If (FjobId=AValue) then exit;
  FjobId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TExportVariantSetRequest
  --------------------------------------------------------------------}


Procedure TExportVariantSetRequest.SetbigqueryDataset(AIndex : Integer; AValue : string); 

begin
  If (FbigqueryDataset=AValue) then exit;
  FbigqueryDataset:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExportVariantSetRequest.SetbigqueryTable(AIndex : Integer; AValue : string); 

begin
  If (FbigqueryTable=AValue) then exit;
  FbigqueryTable:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExportVariantSetRequest.SetcallSetIds(AIndex : Integer; AValue : TExportVariantSetRequestcallSetIds); 

begin
  If (FcallSetIds=AValue) then exit;
  FcallSetIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExportVariantSetRequest.Setformat(AIndex : Integer; AValue : string); 

begin
  If (Fformat=AValue) then exit;
  Fformat:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExportVariantSetRequest.SetprojectNumber(AIndex : Integer; AValue : string); 

begin
  If (FprojectNumber=AValue) then exit;
  FprojectNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TExportVariantSetRequestcallSetIds
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TExportVariantSetResponse
  --------------------------------------------------------------------}


Procedure TExportVariantSetResponse.SetjobId(AIndex : Integer; AValue : string); 

begin
  If (FjobId=AValue) then exit;
  FjobId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TExternalId
  --------------------------------------------------------------------}


Procedure TExternalId.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TExternalId.SetsourceName(AIndex : Integer; AValue : string); 

begin
  If (FsourceName=AValue) then exit;
  FsourceName:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TFastqMetadata
  --------------------------------------------------------------------}


Procedure TFastqMetadata.SetlibraryName(AIndex : Integer; AValue : string); 

begin
  If (FlibraryName=AValue) then exit;
  FlibraryName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFastqMetadata.SetplatformName(AIndex : Integer; AValue : string); 

begin
  If (FplatformName=AValue) then exit;
  FplatformName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFastqMetadata.SetplatformUnit(AIndex : Integer; AValue : string); 

begin
  If (FplatformUnit=AValue) then exit;
  FplatformUnit:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFastqMetadata.SetreadGroupName(AIndex : Integer; AValue : string); 

begin
  If (FreadGroupName=AValue) then exit;
  FreadGroupName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TFastqMetadata.SetsampleName(AIndex : Integer; AValue : string); 

begin
  If (FsampleName=AValue) then exit;
  FsampleName:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TImportReadGroupSetsRequest
  --------------------------------------------------------------------}


Procedure TImportReadGroupSetsRequest.SetdatasetId(AIndex : Integer; AValue : string); 

begin
  If (FdatasetId=AValue) then exit;
  FdatasetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImportReadGroupSetsRequest.SetpartitionStrategy(AIndex : Integer; AValue : string); 

begin
  If (FpartitionStrategy=AValue) then exit;
  FpartitionStrategy:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImportReadGroupSetsRequest.SetreferenceSetId(AIndex : Integer; AValue : string); 

begin
  If (FreferenceSetId=AValue) then exit;
  FreferenceSetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImportReadGroupSetsRequest.SetsourceUris(AIndex : Integer; AValue : TImportReadGroupSetsRequestsourceUris); 

begin
  If (FsourceUris=AValue) then exit;
  FsourceUris:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TImportReadGroupSetsRequestsourceUris
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TImportReadGroupSetsResponse
  --------------------------------------------------------------------}


Procedure TImportReadGroupSetsResponse.SetjobId(AIndex : Integer; AValue : string); 

begin
  If (FjobId=AValue) then exit;
  FjobId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TImportVariantsRequest
  --------------------------------------------------------------------}


Procedure TImportVariantsRequest.Setformat(AIndex : Integer; AValue : string); 

begin
  If (Fformat=AValue) then exit;
  Fformat:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TImportVariantsRequest.SetsourceUris(AIndex : Integer; AValue : TImportVariantsRequestsourceUris); 

begin
  If (FsourceUris=AValue) then exit;
  FsourceUris:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TImportVariantsRequestsourceUris
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TImportVariantsResponse
  --------------------------------------------------------------------}


Procedure TImportVariantsResponse.SetjobId(AIndex : Integer; AValue : string); 

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



Procedure TInterleavedFastqSource.SetsourceUris(AIndex : Integer; AValue : TInterleavedFastqSourcesourceUris); 

begin
  If (FsourceUris=AValue) then exit;
  FsourceUris:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TInterleavedFastqSourcesourceUris
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TJob
  --------------------------------------------------------------------}


Procedure TJob.Setcreated(AIndex : Integer; AValue : string); 

begin
  If (Fcreated=AValue) then exit;
  Fcreated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.SetdetailedStatus(AIndex : Integer; AValue : string); 

begin
  If (FdetailedStatus=AValue) then exit;
  FdetailedStatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.Seterrors(AIndex : Integer; AValue : TJoberrors); 

begin
  If (Ferrors=AValue) then exit;
  Ferrors:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.SetimportedIds(AIndex : Integer; AValue : TJobimportedIds); 

begin
  If (FimportedIds=AValue) then exit;
  FimportedIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.SetprojectNumber(AIndex : Integer; AValue : string); 

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



Procedure TJob.Setstatus(AIndex : Integer; AValue : string); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJob.Setwarnings(AIndex : Integer; AValue : TJobwarnings); 

begin
  If (Fwarnings=AValue) then exit;
  Fwarnings:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TJoberrors
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TJobimportedIds
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TJobwarnings
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TJobRequest
  --------------------------------------------------------------------}


Procedure TJobRequest.Setdestination(AIndex : Integer; AValue : TJobRequestdestination); 

begin
  If (Fdestination=AValue) then exit;
  Fdestination:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobRequest.Setsource(AIndex : Integer; AValue : TJobRequestsource); 

begin
  If (Fsource=AValue) then exit;
  Fsource:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TJobRequest.Set_type(AIndex : Integer; AValue : string); 

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




{ --------------------------------------------------------------------
  TJobRequestdestination
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TJobRequestsource
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TLinearAlignment
  --------------------------------------------------------------------}


Procedure TLinearAlignment.Setcigar(AIndex : Integer; AValue : TLinearAlignmentcigar); 

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





{ --------------------------------------------------------------------
  TLinearAlignmentcigar
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TListBasesResponse
  --------------------------------------------------------------------}


Procedure TListBasesResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListBasesResponse.Setoffset(AIndex : Integer; AValue : string); 

begin
  If (Foffset=AValue) then exit;
  Foffset:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListBasesResponse.Setsequence(AIndex : Integer; AValue : string); 

begin
  If (Fsequence=AValue) then exit;
  Fsequence:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListCoverageBucketsResponse
  --------------------------------------------------------------------}


Procedure TListCoverageBucketsResponse.SetbucketWidth(AIndex : Integer; AValue : string); 

begin
  If (FbucketWidth=AValue) then exit;
  FbucketWidth:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListCoverageBucketsResponse.SetcoverageBuckets(AIndex : Integer; AValue : TListCoverageBucketsResponsecoverageBuckets); 

begin
  If (FcoverageBuckets=AValue) then exit;
  FcoverageBuckets:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListCoverageBucketsResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListCoverageBucketsResponsecoverageBuckets
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TListDatasetsResponse
  --------------------------------------------------------------------}


Procedure TListDatasetsResponse.Setdatasets(AIndex : Integer; AValue : TListDatasetsResponsedatasets); 

begin
  If (Fdatasets=AValue) then exit;
  Fdatasets:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TListDatasetsResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TListDatasetsResponsedatasets
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TMergeVariantsRequest
  --------------------------------------------------------------------}


Procedure TMergeVariantsRequest.Setvariants(AIndex : Integer; AValue : TMergeVariantsRequestvariants); 

begin
  If (Fvariants=AValue) then exit;
  Fvariants:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TMergeVariantsRequestvariants
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TMetadata
  --------------------------------------------------------------------}


Procedure TMetadata.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetadata.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetadata.Setinfo(AIndex : Integer; AValue : TMetadatainfo); 

begin
  If (Finfo=AValue) then exit;
  Finfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetadata.Setkey(AIndex : Integer; AValue : string); 

begin
  If (Fkey=AValue) then exit;
  Fkey:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetadata.Setnumber(AIndex : Integer; AValue : string); 

begin
  If (Fnumber=AValue) then exit;
  Fnumber:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetadata.Set_type(AIndex : Integer; AValue : string); 

begin
  If (F_type=AValue) then exit;
  F_type:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TMetadata.Setvalue(AIndex : Integer; AValue : string); 

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
  TMetadatainfo
  --------------------------------------------------------------------}


Class Function TMetadatainfo.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TPairedFastqSource
  --------------------------------------------------------------------}


Procedure TPairedFastqSource.SetfirstSourceUris(AIndex : Integer; AValue : TPairedFastqSourcefirstSourceUris); 

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



Procedure TPairedFastqSource.SetsecondSourceUris(AIndex : Integer; AValue : TPairedFastqSourcesecondSourceUris); 

begin
  If (FsecondSourceUris=AValue) then exit;
  FsecondSourceUris:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TPairedFastqSourcefirstSourceUris
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TPairedFastqSourcesecondSourceUris
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TPosition
  --------------------------------------------------------------------}


Procedure TPosition.Setposition(AIndex : Integer; AValue : string); 

begin
  If (Fposition=AValue) then exit;
  Fposition:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TPosition.SetreferenceName(AIndex : Integer; AValue : string); 

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


Procedure TQueryRange.Set_end(AIndex : Integer; AValue : string); 

begin
  If (F_end=AValue) then exit;
  F_end:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQueryRange.SetreferenceId(AIndex : Integer; AValue : string); 

begin
  If (FreferenceId=AValue) then exit;
  FreferenceId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQueryRange.SetreferenceName(AIndex : Integer; AValue : string); 

begin
  If (FreferenceName=AValue) then exit;
  FreferenceName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TQueryRange.Setstart(AIndex : Integer; AValue : string); 

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


Procedure TRange.Set_end(AIndex : Integer; AValue : string); 

begin
  If (F_end=AValue) then exit;
  F_end:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRange.SetreferenceName(AIndex : Integer; AValue : string); 

begin
  If (FreferenceName=AValue) then exit;
  FreferenceName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRange.Setstart(AIndex : Integer; AValue : string); 

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


Procedure TRangePosition.Set_end(AIndex : Integer; AValue : string); 

begin
  If (F_end=AValue) then exit;
  F_end:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRangePosition.SetreferenceId(AIndex : Integer; AValue : string); 

begin
  If (FreferenceId=AValue) then exit;
  FreferenceId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRangePosition.SetreferenceName(AIndex : Integer; AValue : string); 

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



Procedure TRangePosition.Setstart(AIndex : Integer; AValue : string); 

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
  TRead
  --------------------------------------------------------------------}


Procedure TRead.SetalignedQuality(AIndex : Integer; AValue : TReadalignedQuality); 

begin
  If (FalignedQuality=AValue) then exit;
  FalignedQuality:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRead.SetalignedSequence(AIndex : Integer; AValue : string); 

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



Procedure TRead.SetfragmentName(AIndex : Integer; AValue : string); 

begin
  If (FfragmentName=AValue) then exit;
  FfragmentName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRead.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRead.Setinfo(AIndex : Integer; AValue : TReadinfo); 

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



Procedure TRead.SetreadGroupId(AIndex : Integer; AValue : string); 

begin
  If (FreadGroupId=AValue) then exit;
  FreadGroupId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TRead.SetreadGroupSetId(AIndex : Integer; AValue : string); 

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





{ --------------------------------------------------------------------
  TReadalignedQuality
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TReadinfo
  --------------------------------------------------------------------}


Class Function TReadinfo.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TReadGroup
  --------------------------------------------------------------------}


Procedure TReadGroup.SetdatasetId(AIndex : Integer; AValue : string); 

begin
  If (FdatasetId=AValue) then exit;
  FdatasetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadGroup.Setdescription(AIndex : Integer; AValue : string); 

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



Procedure TReadGroup.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadGroup.Setinfo(AIndex : Integer; AValue : TReadGroupinfo); 

begin
  If (Finfo=AValue) then exit;
  Finfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadGroup.Setname(AIndex : Integer; AValue : string); 

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



Procedure TReadGroup.Setprograms(AIndex : Integer; AValue : TReadGroupprograms); 

begin
  If (Fprograms=AValue) then exit;
  Fprograms:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadGroup.SetreferenceSetId(AIndex : Integer; AValue : string); 

begin
  If (FreferenceSetId=AValue) then exit;
  FreferenceSetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadGroup.SetsampleId(AIndex : Integer; AValue : string); 

begin
  If (FsampleId=AValue) then exit;
  FsampleId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReadGroupinfo
  --------------------------------------------------------------------}


Class Function TReadGroupinfo.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TReadGroupprograms
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TReadGroupExperiment
  --------------------------------------------------------------------}


Procedure TReadGroupExperiment.SetinstrumentModel(AIndex : Integer; AValue : string); 

begin
  If (FinstrumentModel=AValue) then exit;
  FinstrumentModel:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadGroupExperiment.SetlibraryId(AIndex : Integer; AValue : string); 

begin
  If (FlibraryId=AValue) then exit;
  FlibraryId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadGroupExperiment.SetplatformUnit(AIndex : Integer; AValue : string); 

begin
  If (FplatformUnit=AValue) then exit;
  FplatformUnit:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadGroupExperiment.SetsequencingCenter(AIndex : Integer; AValue : string); 

begin
  If (FsequencingCenter=AValue) then exit;
  FsequencingCenter:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReadGroupProgram
  --------------------------------------------------------------------}


Procedure TReadGroupProgram.SetcommandLine(AIndex : Integer; AValue : string); 

begin
  If (FcommandLine=AValue) then exit;
  FcommandLine:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadGroupProgram.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadGroupProgram.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadGroupProgram.SetprevProgramId(AIndex : Integer; AValue : string); 

begin
  If (FprevProgramId=AValue) then exit;
  FprevProgramId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadGroupProgram.Setversion(AIndex : Integer; AValue : string); 

begin
  If (Fversion=AValue) then exit;
  Fversion:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReadGroupSet
  --------------------------------------------------------------------}


Procedure TReadGroupSet.SetdatasetId(AIndex : Integer; AValue : string); 

begin
  If (FdatasetId=AValue) then exit;
  FdatasetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadGroupSet.Setfilename(AIndex : Integer; AValue : string); 

begin
  If (Ffilename=AValue) then exit;
  Ffilename:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadGroupSet.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadGroupSet.Setinfo(AIndex : Integer; AValue : TReadGroupSetinfo); 

begin
  If (Finfo=AValue) then exit;
  Finfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadGroupSet.Setname(AIndex : Integer; AValue : string); 

begin
  If (Fname=AValue) then exit;
  Fname:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadGroupSet.SetreadGroups(AIndex : Integer; AValue : TReadGroupSetreadGroups); 

begin
  If (FreadGroups=AValue) then exit;
  FreadGroups:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReadGroupSet.SetreferenceSetId(AIndex : Integer; AValue : string); 

begin
  If (FreferenceSetId=AValue) then exit;
  FreferenceSetId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReadGroupSetinfo
  --------------------------------------------------------------------}


Class Function TReadGroupSetinfo.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TReadGroupSetreadGroups
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TReference
  --------------------------------------------------------------------}


Procedure TReference.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReference.Setlength(AIndex : Integer; AValue : string); 

begin
  If (Flength=AValue) then exit;
  Flength:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReference.Setmd5checksum(AIndex : Integer; AValue : string); 

begin
  If (Fmd5checksum=AValue) then exit;
  Fmd5checksum:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReference.Setname(AIndex : Integer; AValue : string); 

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



Procedure TReference.SetsourceAccessions(AIndex : Integer; AValue : TReferencesourceAccessions); 

begin
  If (FsourceAccessions=AValue) then exit;
  FsourceAccessions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReference.SetsourceURI(AIndex : Integer; AValue : string); 

begin
  If (FsourceURI=AValue) then exit;
  FsourceURI:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReferencesourceAccessions
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TReferenceBound
  --------------------------------------------------------------------}


Procedure TReferenceBound.SetreferenceName(AIndex : Integer; AValue : string); 

begin
  If (FreferenceName=AValue) then exit;
  FreferenceName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReferenceBound.SetupperBound(AIndex : Integer; AValue : string); 

begin
  If (FupperBound=AValue) then exit;
  FupperBound:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReferenceSet
  --------------------------------------------------------------------}


Procedure TReferenceSet.SetassemblyId(AIndex : Integer; AValue : string); 

begin
  If (FassemblyId=AValue) then exit;
  FassemblyId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReferenceSet.Setdescription(AIndex : Integer; AValue : string); 

begin
  If (Fdescription=AValue) then exit;
  Fdescription:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReferenceSet.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReferenceSet.Setmd5checksum(AIndex : Integer; AValue : string); 

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



Procedure TReferenceSet.SetreferenceIds(AIndex : Integer; AValue : TReferenceSetreferenceIds); 

begin
  If (FreferenceIds=AValue) then exit;
  FreferenceIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReferenceSet.SetsourceAccessions(AIndex : Integer; AValue : TReferenceSetsourceAccessions); 

begin
  If (FsourceAccessions=AValue) then exit;
  FsourceAccessions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TReferenceSet.SetsourceURI(AIndex : Integer; AValue : string); 

begin
  If (FsourceURI=AValue) then exit;
  FsourceURI:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TReferenceSetreferenceIds
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TReferenceSetsourceAccessions
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TSearchAnnotationSetsRequest
  --------------------------------------------------------------------}


Procedure TSearchAnnotationSetsRequest.SetdatasetIds(AIndex : Integer; AValue : TSearchAnnotationSetsRequestdatasetIds); 

begin
  If (FdatasetIds=AValue) then exit;
  FdatasetIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchAnnotationSetsRequest.Setname(AIndex : Integer; AValue : string); 

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



Procedure TSearchAnnotationSetsRequest.SetpageToken(AIndex : Integer; AValue : string); 

begin
  If (FpageToken=AValue) then exit;
  FpageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchAnnotationSetsRequest.SetreferenceSetId(AIndex : Integer; AValue : string); 

begin
  If (FreferenceSetId=AValue) then exit;
  FreferenceSetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchAnnotationSetsRequest.Settypes(AIndex : Integer; AValue : TSearchAnnotationSetsRequesttypes); 

begin
  If (Ftypes=AValue) then exit;
  Ftypes:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSearchAnnotationSetsRequestdatasetIds
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TSearchAnnotationSetsRequesttypes
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TSearchAnnotationSetsResponse
  --------------------------------------------------------------------}


Procedure TSearchAnnotationSetsResponse.SetannotationSets(AIndex : Integer; AValue : TSearchAnnotationSetsResponseannotationSets); 

begin
  If (FannotationSets=AValue) then exit;
  FannotationSets:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchAnnotationSetsResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSearchAnnotationSetsResponseannotationSets
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TSearchAnnotationsRequest
  --------------------------------------------------------------------}


Procedure TSearchAnnotationsRequest.SetannotationSetIds(AIndex : Integer; AValue : TSearchAnnotationsRequestannotationSetIds); 

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



Procedure TSearchAnnotationsRequest.SetpageToken(AIndex : Integer; AValue : string); 

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





{ --------------------------------------------------------------------
  TSearchAnnotationsRequestannotationSetIds
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TSearchAnnotationsResponse
  --------------------------------------------------------------------}


Procedure TSearchAnnotationsResponse.Setannotations(AIndex : Integer; AValue : TSearchAnnotationsResponseannotations); 

begin
  If (Fannotations=AValue) then exit;
  Fannotations:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchAnnotationsResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSearchAnnotationsResponseannotations
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TSearchCallSetsRequest
  --------------------------------------------------------------------}


Procedure TSearchCallSetsRequest.Setname(AIndex : Integer; AValue : string); 

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



Procedure TSearchCallSetsRequest.SetpageToken(AIndex : Integer; AValue : string); 

begin
  If (FpageToken=AValue) then exit;
  FpageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchCallSetsRequest.SetvariantSetIds(AIndex : Integer; AValue : TSearchCallSetsRequestvariantSetIds); 

begin
  If (FvariantSetIds=AValue) then exit;
  FvariantSetIds:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSearchCallSetsRequestvariantSetIds
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TSearchCallSetsResponse
  --------------------------------------------------------------------}


Procedure TSearchCallSetsResponse.SetcallSets(AIndex : Integer; AValue : TSearchCallSetsResponsecallSets); 

begin
  If (FcallSets=AValue) then exit;
  FcallSets:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchCallSetsResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSearchCallSetsResponsecallSets
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TSearchJobsRequest
  --------------------------------------------------------------------}


Procedure TSearchJobsRequest.SetcreatedAfter(AIndex : Integer; AValue : string); 

begin
  If (FcreatedAfter=AValue) then exit;
  FcreatedAfter:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchJobsRequest.SetcreatedBefore(AIndex : Integer; AValue : string); 

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



Procedure TSearchJobsRequest.SetpageToken(AIndex : Integer; AValue : string); 

begin
  If (FpageToken=AValue) then exit;
  FpageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchJobsRequest.SetprojectNumber(AIndex : Integer; AValue : string); 

begin
  If (FprojectNumber=AValue) then exit;
  FprojectNumber:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchJobsRequest.Setstatus(AIndex : Integer; AValue : TSearchJobsRequeststatus); 

begin
  If (Fstatus=AValue) then exit;
  Fstatus:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSearchJobsRequeststatus
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TSearchJobsResponse
  --------------------------------------------------------------------}


Procedure TSearchJobsResponse.Setjobs(AIndex : Integer; AValue : TSearchJobsResponsejobs); 

begin
  If (Fjobs=AValue) then exit;
  Fjobs:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchJobsResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSearchJobsResponsejobs
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TSearchReadGroupSetsRequest
  --------------------------------------------------------------------}


Procedure TSearchReadGroupSetsRequest.SetdatasetIds(AIndex : Integer; AValue : TSearchReadGroupSetsRequestdatasetIds); 

begin
  If (FdatasetIds=AValue) then exit;
  FdatasetIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchReadGroupSetsRequest.Setname(AIndex : Integer; AValue : string); 

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



Procedure TSearchReadGroupSetsRequest.SetpageToken(AIndex : Integer; AValue : string); 

begin
  If (FpageToken=AValue) then exit;
  FpageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSearchReadGroupSetsRequestdatasetIds
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TSearchReadGroupSetsResponse
  --------------------------------------------------------------------}


Procedure TSearchReadGroupSetsResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchReadGroupSetsResponse.SetreadGroupSets(AIndex : Integer; AValue : TSearchReadGroupSetsResponsereadGroupSets); 

begin
  If (FreadGroupSets=AValue) then exit;
  FreadGroupSets:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSearchReadGroupSetsResponsereadGroupSets
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TSearchReadsRequest
  --------------------------------------------------------------------}


Procedure TSearchReadsRequest.Set_end(AIndex : Integer; AValue : string); 

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



Procedure TSearchReadsRequest.SetpageToken(AIndex : Integer; AValue : string); 

begin
  If (FpageToken=AValue) then exit;
  FpageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchReadsRequest.SetreadGroupIds(AIndex : Integer; AValue : TSearchReadsRequestreadGroupIds); 

begin
  If (FreadGroupIds=AValue) then exit;
  FreadGroupIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchReadsRequest.SetreadGroupSetIds(AIndex : Integer; AValue : TSearchReadsRequestreadGroupSetIds); 

begin
  If (FreadGroupSetIds=AValue) then exit;
  FreadGroupSetIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchReadsRequest.SetreferenceName(AIndex : Integer; AValue : string); 

begin
  If (FreferenceName=AValue) then exit;
  FreferenceName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchReadsRequest.Setstart(AIndex : Integer; AValue : string); 

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




{ --------------------------------------------------------------------
  TSearchReadsRequestreadGroupIds
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TSearchReadsRequestreadGroupSetIds
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TSearchReadsResponse
  --------------------------------------------------------------------}


Procedure TSearchReadsResponse.Setalignments(AIndex : Integer; AValue : TSearchReadsResponsealignments); 

begin
  If (Falignments=AValue) then exit;
  Falignments:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchReadsResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSearchReadsResponsealignments
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TSearchReferenceSetsRequest
  --------------------------------------------------------------------}


Procedure TSearchReferenceSetsRequest.Setaccessions(AIndex : Integer; AValue : TSearchReferenceSetsRequestaccessions); 

begin
  If (Faccessions=AValue) then exit;
  Faccessions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchReferenceSetsRequest.SetassemblyId(AIndex : Integer; AValue : string); 

begin
  If (FassemblyId=AValue) then exit;
  FassemblyId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchReferenceSetsRequest.Setmd5checksums(AIndex : Integer; AValue : TSearchReferenceSetsRequestmd5checksums); 

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



Procedure TSearchReferenceSetsRequest.SetpageToken(AIndex : Integer; AValue : string); 

begin
  If (FpageToken=AValue) then exit;
  FpageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSearchReferenceSetsRequestaccessions
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TSearchReferenceSetsRequestmd5checksums
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TSearchReferenceSetsResponse
  --------------------------------------------------------------------}


Procedure TSearchReferenceSetsResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchReferenceSetsResponse.SetreferenceSets(AIndex : Integer; AValue : TSearchReferenceSetsResponsereferenceSets); 

begin
  If (FreferenceSets=AValue) then exit;
  FreferenceSets:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSearchReferenceSetsResponsereferenceSets
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TSearchReferencesRequest
  --------------------------------------------------------------------}


Procedure TSearchReferencesRequest.Setaccessions(AIndex : Integer; AValue : TSearchReferencesRequestaccessions); 

begin
  If (Faccessions=AValue) then exit;
  Faccessions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchReferencesRequest.Setmd5checksums(AIndex : Integer; AValue : TSearchReferencesRequestmd5checksums); 

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



Procedure TSearchReferencesRequest.SetpageToken(AIndex : Integer; AValue : string); 

begin
  If (FpageToken=AValue) then exit;
  FpageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchReferencesRequest.SetreferenceSetId(AIndex : Integer; AValue : string); 

begin
  If (FreferenceSetId=AValue) then exit;
  FreferenceSetId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSearchReferencesRequestaccessions
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TSearchReferencesRequestmd5checksums
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TSearchReferencesResponse
  --------------------------------------------------------------------}


Procedure TSearchReferencesResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchReferencesResponse.Setreferences(AIndex : Integer; AValue : TSearchReferencesResponsereferences); 

begin
  If (Freferences=AValue) then exit;
  Freferences:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSearchReferencesResponsereferences
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TSearchVariantSetsRequest
  --------------------------------------------------------------------}


Procedure TSearchVariantSetsRequest.SetdatasetIds(AIndex : Integer; AValue : TSearchVariantSetsRequestdatasetIds); 

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



Procedure TSearchVariantSetsRequest.SetpageToken(AIndex : Integer; AValue : string); 

begin
  If (FpageToken=AValue) then exit;
  FpageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSearchVariantSetsRequestdatasetIds
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TSearchVariantSetsResponse
  --------------------------------------------------------------------}


Procedure TSearchVariantSetsResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchVariantSetsResponse.SetvariantSets(AIndex : Integer; AValue : TSearchVariantSetsResponsevariantSets); 

begin
  If (FvariantSets=AValue) then exit;
  FvariantSets:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSearchVariantSetsResponsevariantSets
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TSearchVariantsRequest
  --------------------------------------------------------------------}


Procedure TSearchVariantsRequest.SetcallSetIds(AIndex : Integer; AValue : TSearchVariantsRequestcallSetIds); 

begin
  If (FcallSetIds=AValue) then exit;
  FcallSetIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchVariantsRequest.Set_end(AIndex : Integer; AValue : string); 

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



Procedure TSearchVariantsRequest.SetpageToken(AIndex : Integer; AValue : string); 

begin
  If (FpageToken=AValue) then exit;
  FpageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchVariantsRequest.SetreferenceName(AIndex : Integer; AValue : string); 

begin
  If (FreferenceName=AValue) then exit;
  FreferenceName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchVariantsRequest.Setstart(AIndex : Integer; AValue : string); 

begin
  If (Fstart=AValue) then exit;
  Fstart:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchVariantsRequest.SetvariantName(AIndex : Integer; AValue : string); 

begin
  If (FvariantName=AValue) then exit;
  FvariantName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchVariantsRequest.SetvariantSetIds(AIndex : Integer; AValue : TSearchVariantsRequestvariantSetIds); 

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




{ --------------------------------------------------------------------
  TSearchVariantsRequestcallSetIds
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TSearchVariantsRequestvariantSetIds
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TSearchVariantsResponse
  --------------------------------------------------------------------}


Procedure TSearchVariantsResponse.SetnextPageToken(AIndex : Integer; AValue : string); 

begin
  If (FnextPageToken=AValue) then exit;
  FnextPageToken:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TSearchVariantsResponse.Setvariants(AIndex : Integer; AValue : TSearchVariantsResponsevariants); 

begin
  If (Fvariants=AValue) then exit;
  Fvariants:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TSearchVariantsResponsevariants
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TStreamReadsRequest
  --------------------------------------------------------------------}


Procedure TStreamReadsRequest.Set_end(AIndex : Integer; AValue : string); 

begin
  If (F_end=AValue) then exit;
  F_end:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStreamReadsRequest.SetreadGroupSetIds(AIndex : Integer; AValue : TStreamReadsRequestreadGroupSetIds); 

begin
  If (FreadGroupSetIds=AValue) then exit;
  FreadGroupSetIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStreamReadsRequest.SetreferenceName(AIndex : Integer; AValue : string); 

begin
  If (FreferenceName=AValue) then exit;
  FreferenceName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TStreamReadsRequest.Setstart(AIndex : Integer; AValue : string); 

begin
  If (Fstart=AValue) then exit;
  Fstart:=AValue;
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
  TStreamReadsRequestreadGroupSetIds
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TStreamReadsResponse
  --------------------------------------------------------------------}


Procedure TStreamReadsResponse.Setalignments(AIndex : Integer; AValue : TStreamReadsResponsealignments); 

begin
  If (Falignments=AValue) then exit;
  Falignments:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TStreamReadsResponsealignments
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TTranscript
  --------------------------------------------------------------------}


Procedure TTranscript.SetcodingSequence(AIndex : Integer; AValue : TTranscriptCodingSequence); 

begin
  If (FcodingSequence=AValue) then exit;
  FcodingSequence:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTranscript.Setexons(AIndex : Integer; AValue : TTranscriptexons); 

begin
  If (Fexons=AValue) then exit;
  Fexons:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTranscript.SetgeneId(AIndex : Integer; AValue : string); 

begin
  If (FgeneId=AValue) then exit;
  FgeneId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TTranscriptexons
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TTranscriptCodingSequence
  --------------------------------------------------------------------}


Procedure TTranscriptCodingSequence.Set_end(AIndex : Integer; AValue : string); 

begin
  If (F_end=AValue) then exit;
  F_end:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TTranscriptCodingSequence.Setstart(AIndex : Integer; AValue : string); 

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


Procedure TTranscriptExon.Set_end(AIndex : Integer; AValue : string); 

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



Procedure TTranscriptExon.Setstart(AIndex : Integer; AValue : string); 

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
  TVariant
  --------------------------------------------------------------------}


Procedure TVariant.SetalternateBases(AIndex : Integer; AValue : TVariantalternateBases); 

begin
  If (FalternateBases=AValue) then exit;
  FalternateBases:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariant.Setcalls(AIndex : Integer; AValue : TVariantcalls); 

begin
  If (Fcalls=AValue) then exit;
  Fcalls:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariant.Setcreated(AIndex : Integer; AValue : string); 

begin
  If (Fcreated=AValue) then exit;
  Fcreated:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariant.Set_end(AIndex : Integer; AValue : string); 

begin
  If (F_end=AValue) then exit;
  F_end:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariant.Setfilter(AIndex : Integer; AValue : TVariantfilter); 

begin
  If (Ffilter=AValue) then exit;
  Ffilter:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariant.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariant.Setinfo(AIndex : Integer; AValue : TVariantinfo); 

begin
  If (Finfo=AValue) then exit;
  Finfo:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariant.Setnames(AIndex : Integer; AValue : TVariantnames); 

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



Procedure TVariant.SetreferenceBases(AIndex : Integer; AValue : string); 

begin
  If (FreferenceBases=AValue) then exit;
  FreferenceBases:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariant.SetreferenceName(AIndex : Integer; AValue : string); 

begin
  If (FreferenceName=AValue) then exit;
  FreferenceName:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariant.Setstart(AIndex : Integer; AValue : string); 

begin
  If (Fstart=AValue) then exit;
  Fstart:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariant.SetvariantSetId(AIndex : Integer; AValue : string); 

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




{ --------------------------------------------------------------------
  TVariantalternateBases
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TVariantcalls
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TVariantfilter
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TVariantinfo
  --------------------------------------------------------------------}


Class Function TVariantinfo.AllowAdditionalProperties : Boolean;

begin
  Result:=True;
end;



{ --------------------------------------------------------------------
  TVariantnames
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TVariantAnnotation
  --------------------------------------------------------------------}


Procedure TVariantAnnotation.SetalternateBases(AIndex : Integer; AValue : string); 

begin
  If (FalternateBases=AValue) then exit;
  FalternateBases:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariantAnnotation.SetclinicalSignificance(AIndex : Integer; AValue : string); 

begin
  If (FclinicalSignificance=AValue) then exit;
  FclinicalSignificance:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariantAnnotation.Setconditions(AIndex : Integer; AValue : TVariantAnnotationconditions); 

begin
  If (Fconditions=AValue) then exit;
  Fconditions:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariantAnnotation.Seteffect(AIndex : Integer; AValue : string); 

begin
  If (Feffect=AValue) then exit;
  Feffect:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariantAnnotation.SetgeneId(AIndex : Integer; AValue : string); 

begin
  If (FgeneId=AValue) then exit;
  FgeneId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariantAnnotation.SettranscriptIds(AIndex : Integer; AValue : TVariantAnnotationtranscriptIds); 

begin
  If (FtranscriptIds=AValue) then exit;
  FtranscriptIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariantAnnotation.Set_type(AIndex : Integer; AValue : string); 

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




{ --------------------------------------------------------------------
  TVariantAnnotationconditions
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TVariantAnnotationtranscriptIds
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TVariantAnnotationCondition
  --------------------------------------------------------------------}


Procedure TVariantAnnotationCondition.SetconceptId(AIndex : Integer; AValue : string); 

begin
  If (FconceptId=AValue) then exit;
  FconceptId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariantAnnotationCondition.SetexternalIds(AIndex : Integer; AValue : TVariantAnnotationConditionexternalIds); 

begin
  If (FexternalIds=AValue) then exit;
  FexternalIds:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariantAnnotationCondition.Setnames(AIndex : Integer; AValue : TVariantAnnotationConditionnames); 

begin
  If (Fnames=AValue) then exit;
  Fnames:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariantAnnotationCondition.SetomimId(AIndex : Integer; AValue : string); 

begin
  If (FomimId=AValue) then exit;
  FomimId:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVariantAnnotationConditionexternalIds
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TVariantAnnotationConditionnames
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TVariantSet
  --------------------------------------------------------------------}


Procedure TVariantSet.SetdatasetId(AIndex : Integer; AValue : string); 

begin
  If (FdatasetId=AValue) then exit;
  FdatasetId:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariantSet.Setid(AIndex : Integer; AValue : string); 

begin
  If (Fid=AValue) then exit;
  Fid:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariantSet.Setmetadata(AIndex : Integer; AValue : TVariantSetmetadata); 

begin
  If (Fmetadata=AValue) then exit;
  Fmetadata:=AValue;
  MarkPropertyChanged(AIndex);
end;



Procedure TVariantSet.SetreferenceBounds(AIndex : Integer; AValue : TVariantSetreferenceBounds); 

begin
  If (FreferenceBounds=AValue) then exit;
  FreferenceBounds:=AValue;
  MarkPropertyChanged(AIndex);
end;





{ --------------------------------------------------------------------
  TVariantSetmetadata
  --------------------------------------------------------------------}




{ --------------------------------------------------------------------
  TVariantSetreferenceBounds
  --------------------------------------------------------------------}




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
  TStreamingReadstoreResource
  --------------------------------------------------------------------}


Class Function TStreamingReadstoreResource.ResourceName : String;

begin
  Result:='streamingReadstore';
end;

Class Function TStreamingReadstoreResource.DefaultAPI : TGoogleAPIClass;

begin
  Result:=TgenomicsAPI;
end;

Function TStreamingReadstoreResource.Streamreads(aStreamReadsRequest : TStreamReadsRequest) : TStreamReadsResponse;

Const
  _HTTPMethod = 'POST';
  _Path       = 'streamingReadstore/streamreads';
  _Methodid   = 'genomics.streamingReadstore.streamreads';

begin
  Result:=ServiceCall(_HTTPMethod,_Path,'',aStreamReadsRequest,TStreamReadsResponse) as TStreamReadsResponse;
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
  Result:='20150415';
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
  Result:='https://www.googleapis.com/';
end;

Class Function TGenomicsAPI.APIbasePath : string;

begin
  Result:='/genomics/v1beta2/';
end;

Class Function TGenomicsAPI.APIbaseURL : String;

begin
  Result:='https://www.googleapis.com/genomics/v1beta2/';
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
  SetLength(Result,4);
  Result[0].Name:='https://www.googleapis.com/auth/bigquery';
  Result[0].Description:='View and manage your data in Google BigQuery';
  Result[1].Name:='https://www.googleapis.com/auth/devstorage.read_write';
  Result[1].Description:='Manage your data in Google Cloud Storage';
  Result[2].Name:='https://www.googleapis.com/auth/genomics';
  Result[2].Description:='View and manage Genomics data';
  Result[3].Name:='https://www.googleapis.com/auth/genomics.readonly';
  Result[3].Description:='View Genomics data';
  
end;

Class Function TGenomicsAPI.APINeedsAuth : Boolean;

begin
  Result:=True;
end;

Class Procedure TGenomicsAPI.RegisterAPIResources;

begin
  TAlignReadGroupSetsRequest.RegisterObject;
  TAlignReadGroupSetsRequestbamSourceUris.RegisterObject;
  TAlignReadGroupSetsResponse.RegisterObject;
  TAnnotation.RegisterObject;
  TAnnotationinfo.RegisterObject;
  TAnnotationSet.RegisterObject;
  TAnnotationSetinfo.RegisterObject;
  TBatchAnnotationsResponse.RegisterObject;
  TBatchAnnotationsResponseentries.RegisterObject;
  TBatchAnnotationsResponseEntry.RegisterObject;
  TBatchAnnotationsResponseEntryStatus.RegisterObject;
  TBatchCreateAnnotationsRequest.RegisterObject;
  TBatchCreateAnnotationsRequestannotations.RegisterObject;
  TCall.RegisterObject;
  TCallgenotype.RegisterObject;
  TCallgenotypeLikelihood.RegisterObject;
  TCallinfo.RegisterObject;
  TCallReadGroupSetsRequest.RegisterObject;
  TCallReadGroupSetsRequestsourceUris.RegisterObject;
  TCallReadGroupSetsResponse.RegisterObject;
  TCallSet.RegisterObject;
  TCallSetinfo.RegisterObject;
  TCallSetvariantSetIds.RegisterObject;
  TCigarUnit.RegisterObject;
  TCoverageBucket.RegisterObject;
  TDataset.RegisterObject;
  TExperimentalCreateJobRequest.RegisterObject;
  TExperimentalCreateJobRequestpairedSourceUris.RegisterObject;
  TExperimentalCreateJobRequestsourceUris.RegisterObject;
  TExperimentalCreateJobResponse.RegisterObject;
  TExportReadGroupSetsRequest.RegisterObject;
  TExportReadGroupSetsRequestreadGroupSetIds.RegisterObject;
  TExportReadGroupSetsRequestreferenceNames.RegisterObject;
  TExportReadGroupSetsResponse.RegisterObject;
  TExportVariantSetRequest.RegisterObject;
  TExportVariantSetRequestcallSetIds.RegisterObject;
  TExportVariantSetResponse.RegisterObject;
  TExternalId.RegisterObject;
  TFastqMetadata.RegisterObject;
  TImportReadGroupSetsRequest.RegisterObject;
  TImportReadGroupSetsRequestsourceUris.RegisterObject;
  TImportReadGroupSetsResponse.RegisterObject;
  TImportVariantsRequest.RegisterObject;
  TImportVariantsRequestsourceUris.RegisterObject;
  TImportVariantsResponse.RegisterObject;
  TInt32Value.RegisterObject;
  TInterleavedFastqSource.RegisterObject;
  TInterleavedFastqSourcesourceUris.RegisterObject;
  TJob.RegisterObject;
  TJoberrors.RegisterObject;
  TJobimportedIds.RegisterObject;
  TJobwarnings.RegisterObject;
  TJobRequest.RegisterObject;
  TJobRequestdestination.RegisterObject;
  TJobRequestsource.RegisterObject;
  TLinearAlignment.RegisterObject;
  TLinearAlignmentcigar.RegisterObject;
  TListBasesResponse.RegisterObject;
  TListCoverageBucketsResponse.RegisterObject;
  TListCoverageBucketsResponsecoverageBuckets.RegisterObject;
  TListDatasetsResponse.RegisterObject;
  TListDatasetsResponsedatasets.RegisterObject;
  TMergeVariantsRequest.RegisterObject;
  TMergeVariantsRequestvariants.RegisterObject;
  TMetadata.RegisterObject;
  TMetadatainfo.RegisterObject;
  TPairedFastqSource.RegisterObject;
  TPairedFastqSourcefirstSourceUris.RegisterObject;
  TPairedFastqSourcesecondSourceUris.RegisterObject;
  TPosition.RegisterObject;
  TQueryRange.RegisterObject;
  TRange.RegisterObject;
  TRangePosition.RegisterObject;
  TRead.RegisterObject;
  TReadalignedQuality.RegisterObject;
  TReadinfo.RegisterObject;
  TReadGroup.RegisterObject;
  TReadGroupinfo.RegisterObject;
  TReadGroupprograms.RegisterObject;
  TReadGroupExperiment.RegisterObject;
  TReadGroupProgram.RegisterObject;
  TReadGroupSet.RegisterObject;
  TReadGroupSetinfo.RegisterObject;
  TReadGroupSetreadGroups.RegisterObject;
  TReference.RegisterObject;
  TReferencesourceAccessions.RegisterObject;
  TReferenceBound.RegisterObject;
  TReferenceSet.RegisterObject;
  TReferenceSetreferenceIds.RegisterObject;
  TReferenceSetsourceAccessions.RegisterObject;
  TSearchAnnotationSetsRequest.RegisterObject;
  TSearchAnnotationSetsRequestdatasetIds.RegisterObject;
  TSearchAnnotationSetsRequesttypes.RegisterObject;
  TSearchAnnotationSetsResponse.RegisterObject;
  TSearchAnnotationSetsResponseannotationSets.RegisterObject;
  TSearchAnnotationsRequest.RegisterObject;
  TSearchAnnotationsRequestannotationSetIds.RegisterObject;
  TSearchAnnotationsResponse.RegisterObject;
  TSearchAnnotationsResponseannotations.RegisterObject;
  TSearchCallSetsRequest.RegisterObject;
  TSearchCallSetsRequestvariantSetIds.RegisterObject;
  TSearchCallSetsResponse.RegisterObject;
  TSearchCallSetsResponsecallSets.RegisterObject;
  TSearchJobsRequest.RegisterObject;
  TSearchJobsRequeststatus.RegisterObject;
  TSearchJobsResponse.RegisterObject;
  TSearchJobsResponsejobs.RegisterObject;
  TSearchReadGroupSetsRequest.RegisterObject;
  TSearchReadGroupSetsRequestdatasetIds.RegisterObject;
  TSearchReadGroupSetsResponse.RegisterObject;
  TSearchReadGroupSetsResponsereadGroupSets.RegisterObject;
  TSearchReadsRequest.RegisterObject;
  TSearchReadsRequestreadGroupIds.RegisterObject;
  TSearchReadsRequestreadGroupSetIds.RegisterObject;
  TSearchReadsResponse.RegisterObject;
  TSearchReadsResponsealignments.RegisterObject;
  TSearchReferenceSetsRequest.RegisterObject;
  TSearchReferenceSetsRequestaccessions.RegisterObject;
  TSearchReferenceSetsRequestmd5checksums.RegisterObject;
  TSearchReferenceSetsResponse.RegisterObject;
  TSearchReferenceSetsResponsereferenceSets.RegisterObject;
  TSearchReferencesRequest.RegisterObject;
  TSearchReferencesRequestaccessions.RegisterObject;
  TSearchReferencesRequestmd5checksums.RegisterObject;
  TSearchReferencesResponse.RegisterObject;
  TSearchReferencesResponsereferences.RegisterObject;
  TSearchVariantSetsRequest.RegisterObject;
  TSearchVariantSetsRequestdatasetIds.RegisterObject;
  TSearchVariantSetsResponse.RegisterObject;
  TSearchVariantSetsResponsevariantSets.RegisterObject;
  TSearchVariantsRequest.RegisterObject;
  TSearchVariantsRequestcallSetIds.RegisterObject;
  TSearchVariantsRequestvariantSetIds.RegisterObject;
  TSearchVariantsResponse.RegisterObject;
  TSearchVariantsResponsevariants.RegisterObject;
  TStreamReadsRequest.RegisterObject;
  TStreamReadsRequestreadGroupSetIds.RegisterObject;
  TStreamReadsResponse.RegisterObject;
  TStreamReadsResponsealignments.RegisterObject;
  TTranscript.RegisterObject;
  TTranscriptexons.RegisterObject;
  TTranscriptCodingSequence.RegisterObject;
  TTranscriptExon.RegisterObject;
  TVariant.RegisterObject;
  TVariantalternateBases.RegisterObject;
  TVariantcalls.RegisterObject;
  TVariantfilter.RegisterObject;
  TVariantinfo.RegisterObject;
  TVariantnames.RegisterObject;
  TVariantAnnotation.RegisterObject;
  TVariantAnnotationconditions.RegisterObject;
  TVariantAnnotationtranscriptIds.RegisterObject;
  TVariantAnnotationCondition.RegisterObject;
  TVariantAnnotationConditionexternalIds.RegisterObject;
  TVariantAnnotationConditionnames.RegisterObject;
  TVariantSet.RegisterObject;
  TVariantSetmetadata.RegisterObject;
  TVariantSetreferenceBounds.RegisterObject;
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
  Result.API:=Self;
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
  Result.API:=Self;
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
  Result.API:=Self;
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
  Result.API:=Self;
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
  Result.API:=Self;
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
  Result.API:=Self;
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
  Result.API:=Self;
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
  Result.API:=Self;
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
  Result.API:=Self;
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
  Result.API:=Self;
end;



Function TGenomicsAPI.GetStreamingReadstoreInstance : TStreamingReadstoreResource;

begin
  if (FStreamingReadstoreInstance=Nil) then
    FStreamingReadstoreInstance:=CreateStreamingReadstoreResource;
  Result:=FStreamingReadstoreInstance;
end;

Function TGenomicsAPI.CreateStreamingReadstoreResource : TStreamingReadstoreResource;

begin
  Result:=CreateStreamingReadstoreResource(Self);
end;


Function TGenomicsAPI.CreateStreamingReadstoreResource(AOwner : TComponent) : TStreamingReadstoreResource;

begin
  Result:=TStreamingReadstoreResource.Create(AOwner);
  Result.API:=Self;
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
  Result.API:=Self;
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
  Result.API:=Self;
end;



initialization
  TGenomicsAPI.RegisterAPI;
end.
